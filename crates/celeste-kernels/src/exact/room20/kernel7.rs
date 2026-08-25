// GENERATED from a TRACED frame (shape 7). Do not edit.
//
// One input shape, 5 output shapes, 90 distinct button
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
pub const SHAPE: u64 = 359672449374052638;

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
    pub c339: P8,
    pub c340: P8,
    pub c341: P8,
    pub c342: P8,
    pub c349: P8,
    pub c350: P8,
    pub c351: P8,
    pub c352: P8,
    pub c363: P8,
    pub c364: P8,
    pub c365: P8,
    pub c366: P8,
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
    pub c337: u16,
    pub c338: u16,
    pub c242: ZN,
    pub c243: ZN,
    pub c343: ZN,
    pub c344: ZN,
    pub c251: u16,
    pub c345: ZN,
    pub c346: ZN,
    pub c253: ZN,
    pub c255: ZN,
    pub c256: ZN,
    pub c259: u16,
    pub c260: ZN,
    pub c347: u16,
    pub c348: u16,
    pub c262: ZN,
    pub c263: ZN,
    pub c353: ZN,
    pub c354: ZN,
    pub c271: u16,
    pub c355: ZN,
    pub c356: ZN,
    pub c273: ZN,
    pub c275: ZN,
    pub c276: ZN,
    pub c279: u16,
    pub c357: ZN,
    pub c358: ZN,
    pub c281: ZN,
    pub c359: ZN,
    pub c360: ZN,
    pub c283: ZN,
    pub c284: ZN,
    pub c361: u16,
    pub c362: u16,
    pub c286: ZN,
    pub c293: u16,
    pub c294: u16,
    pub c367: ZN,
    pub c368: ZN,
    pub c296: u16,
    pub c369: ZN,
    pub c370: ZN,
    pub c300: ZN,
    pub c301: ZN,
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
    pub c337: u32,
    pub c338: u32,
    pub c242: u32,
    pub c243: u32,
    pub c343: u32,
    pub c344: u32,
    pub c251: u32,
    pub c345: u32,
    pub c346: u32,
    pub c253: u32,
    pub c255: u32,
    pub c256: u32,
    pub c259: u32,
    pub c260: u32,
    pub c347: u32,
    pub c348: u32,
    pub c262: u32,
    pub c263: u32,
    pub c353: u32,
    pub c354: u32,
    pub c271: u32,
    pub c355: u32,
    pub c356: u32,
    pub c273: u32,
    pub c275: u32,
    pub c276: u32,
    pub c279: u32,
    pub c357: u32,
    pub c358: u32,
    pub c281: u32,
    pub c359: u32,
    pub c360: u32,
    pub c283: u32,
    pub c284: u32,
    pub c361: u32,
    pub c362: u32,
    pub c286: u32,
    pub c293: u32,
    pub c294: u32,
    pub c367: u32,
    pub c368: u32,
    pub c296: u32,
    pub c369: u32,
    pub c370: u32,
    pub c300: u32,
    pub c301: u32,
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
        c339: match &b.cols[cell("objects[0].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c340: match &b.cols[cell("objects[0].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c341: match &b.cols[cell("objects[0].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c342: match &b.cols[cell("objects[0].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c349: match &b.cols[cell("objects[1].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c350: match &b.cols[cell("objects[1].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c351: match &b.cols[cell("objects[1].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c352: match &b.cols[cell("objects[1].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c363: match &b.cols[cell("objects[2].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c364: match &b.cols[cell("objects[2].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c365: match &b.cols[cell("objects[2].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c366: match &b.cols[cell("objects[2].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
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
        c337: cell("objects[0].flip.x")?,
        c338: cell("objects[0].flip.y")?,
        c242: cell("objects[0].hide_for")?,
        c243: cell("objects[0].hide_in")?,
        c343: cell("objects[0].rem.x")?,
        c344: cell("objects[0].rem.y")?,
        c251: cell("objects[0].solids")?,
        c345: cell("objects[0].spd.x")?,
        c346: cell("objects[0].spd.y")?,
        c253: cell("objects[0].spr")?,
        c255: cell("objects[0].x")?,
        c256: cell("objects[0].y")?,
        c259: cell("objects[1].collideable")?,
        c260: cell("objects[1].delay")?,
        c347: cell("objects[1].flip.x")?,
        c348: cell("objects[1].flip.y")?,
        c262: cell("objects[1].hide_for")?,
        c263: cell("objects[1].hide_in")?,
        c353: cell("objects[1].rem.x")?,
        c354: cell("objects[1].rem.y")?,
        c271: cell("objects[1].solids")?,
        c355: cell("objects[1].spd.x")?,
        c356: cell("objects[1].spd.y")?,
        c273: cell("objects[1].spr")?,
        c275: cell("objects[1].x")?,
        c276: cell("objects[1].y")?,
        c279: cell("objects[2].collideable")?,
        c357: cell("objects[2].dash_accel.x")?,
        c358: cell("objects[2].dash_accel.y")?,
        c281: cell("objects[2].dash_effect_time")?,
        c359: cell("objects[2].dash_target.x")?,
        c360: cell("objects[2].dash_target.y")?,
        c283: cell("objects[2].dash_time")?,
        c284: cell("objects[2].djump")?,
        c361: cell("objects[2].flip.x")?,
        c362: cell("objects[2].flip.y")?,
        c286: cell("objects[2].grace")?,
        c293: cell("objects[2].p_dash")?,
        c294: cell("objects[2].p_jump")?,
        c367: cell("objects[2].rem.x")?,
        c368: cell("objects[2].rem.y")?,
        c296: cell("objects[2].solids")?,
        c369: cell("objects[2].spd.x")?,
        c370: cell("objects[2].spd.y")?,
        c300: cell("objects[2].x")?,
        c301: cell("objects[2].y")?,
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
        c338: match &b.cols[s.c338 as usize] {
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
        c343: match &b.cols[s.c343 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c344: match &b.cols[s.c344 as usize] {
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
        c345: match &b.cols[s.c345 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c346: match &b.cols[s.c346 as usize] {
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
        c260: match &b.cols[s.c260 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
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
        c348: match &b.cols[s.c348 as usize] {
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
        c262: match &b.cols[s.c262 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c263: match &b.cols[s.c263 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c353: match &b.cols[s.c353 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c354: match &b.cols[s.c354 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c271: match &b.cols[s.c271 as usize] {
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
        c355: match &b.cols[s.c355 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c356: match &b.cols[s.c356 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c273: match &b.cols[s.c273 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c275: match &b.cols[s.c275 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c276: match &b.cols[s.c276 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c279: match &b.cols[s.c279 as usize] {
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
        c357: match &b.cols[s.c357 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c358: match &b.cols[s.c358 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c281: match &b.cols[s.c281 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c359: match &b.cols[s.c359 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c360: match &b.cols[s.c360 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c283: match &b.cols[s.c283 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c284: match &b.cols[s.c284 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
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
        c362: match &b.cols[s.c362 as usize] {
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
        c286: match &b.cols[s.c286 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
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
        c294: match &b.cols[s.c294 as usize] {
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
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c368: match &b.cols[s.c368 as usize] {
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
        c369: match &b.cols[s.c369 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c370: match &b.cols[s.c370 as usize] {
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

/// Outcome 2's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared2 {
    pub c87: ZN,
    pub c84: ZN,
    pub c86: ZN,
    pub c259: ZN,
    pub c272: ZN,
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
        if let Col::N(v) = &mut acc.cols[259] { v.push(sh.c259.lane(i)); }
        if let Col::N(v) = &mut acc.cols[272] { v.push(sh.c272.lane(i)); }
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

pub const OUTCOMES: usize = 5;

pub fn acc(i: usize, cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    match i {
        0 => acc0(cart, cache),
        1 => acc1(cart, cache),
        2 => acc2(cart, cache),
        3 => acc3(cart, cache),
        4 => acc4(cart, cache),
        _ => panic!("outcome {} of 5", i),
    }
}

pub fn out_slots(i: usize) -> &'static [(u32, &'static str)] {
    match i {
        0 => OUT_SLOTS_0,
        1 => OUT_SLOTS_1,
        2 => OUT_SLOTS_2,
        3 => OUT_SLOTS_3,
        4 => OUT_SLOTS_4,
        _ => panic!("outcome {} of 5", i),
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
    let r_c260: ZN = rin.c260;
    let r_c262: ZN = rin.c262;
    let r_c263: ZN = rin.c263;
    let r_c271: ZB = ZB { val: rin.c271, known: ALL };
    let r_c273: ZN = rin.c273;
    let r_c275: ZN = rin.c275;
    let r_c276: ZN = rin.c276;
    let r_c279: ZB = ZB { val: rin.c279, known: ALL };
    let r_c281: ZN = rin.c281;
    let r_c283: ZN = rin.c283;
    let r_c284: ZN = rin.c284;
    let r_c286: ZN = rin.c286;
    let r_c293: ZB = ZB { val: rin.c293, known: ALL };
    let r_c294: ZB = ZB { val: rin.c294, known: ALL };
    let r_c296: ZB = ZB { val: rin.c296, known: ALL };
    let r_c300: ZN = rin.c300;
    let r_c301: ZN = rin.c301;
    let r_c337: ZB = ZB { val: rin.c337, known: ALL };
    let r_c338: ZB = ZB { val: rin.c338, known: ALL };
    let r_c343: ZN = rin.c343;
    let r_c344: ZN = rin.c344;
    let r_c345: ZN = rin.c345;
    let r_c346: ZN = rin.c346;
    let r_c347: ZB = ZB { val: rin.c347, known: ALL };
    let r_c348: ZB = ZB { val: rin.c348, known: ALL };
    let r_c353: ZN = rin.c353;
    let r_c354: ZN = rin.c354;
    let r_c355: ZN = rin.c355;
    let r_c356: ZN = rin.c356;
    let r_c357: ZN = rin.c357;
    let r_c358: ZN = rin.c358;
    let r_c359: ZN = rin.c359;
    let r_c360: ZN = rin.c360;
    let r_c361: ZB = ZB { val: rin.c361, known: ALL };
    let r_c362: ZB = ZB { val: rin.c362, known: ALL };
    let r_c367: ZN = rin.c367;
    let r_c368: ZN = rin.c368;
    let r_c369: ZN = rin.c369;
    let r_c370: ZN = rin.c370;
    let n101: ZB = zb_not(r_c337);
    let n102: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c243);
    let n103: bool = P8::from_raw(0i32) == u.c341;
    let n104: bool = P8::from_raw(0i32) == u.c342;
    let n105: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c344);
    let n106: ZB = zb_not(r_c347);
    let n107: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c263);
    let n108: bool = P8::from_raw(0i32) == u.c351;
    let n109: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c353);
    let n110: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c355);
    let n114: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c84);
    let n115: ZN = zn_rem(n114, zn_splat(P8::from_raw(1966080i32)));
    let n116: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n115);
    let n131: bool = P8::from_raw(0i32) == u.c352;
    let n132: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c354);
    let n133: ZB = zb_not(r_c293);
    let n134: ZB = zn_le(r_c20, zn_splat(P8::from_raw(0i32)));
    let n135: ZB = zb_not(r_c43);
    let n140: ZB = zb_not(r_c294);
    let n141: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c262);
    let n142: bool = P8::from_raw(524288i32) == u.c349;
    let n143: ZB = zb_not(r_c42);
    let n144: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), r_c88);
    let n145: ZB = zb_not(r_c338);
    let n146: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c242);
    let n147: bool = P8::from_raw(524288i32) == u.c339;
    let n148: bool = P8::from_raw(524288i32) == u.c340;
    let n149: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c343);
    let n150: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c345);
    let n151: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c346);
    let n152: ZB = zn_eq(zn_splat(P8::from_raw(1179648i32)), r_c253);
    let n153: ZB = zn_eq(zn_splat(P8::from_raw(2621440i32)), r_c255);
    let n154: ZB = zn_eq(zn_splat(P8::from_raw(7340032i32)), r_c256);
    let n155: ZB = zb_not(r_c348);
    let n156: bool = P8::from_raw(524288i32) == u.c350;
    let n157: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c356);
    let n158: ZB = zn_eq(zn_splat(P8::from_raw(6815744i32)), r_c275);
    let n159: ZB = zn_eq(zn_splat(P8::from_raw(7340032i32)), r_c276);
    let n160: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c86);
    let n194: ZB = zn_gt(r_c260, zn_splat(P8::from_raw(0i32)));
    let n195: ZB = zn_le(r_c260, zn_splat(P8::from_raw(0i32)));
    let n196: ZN = zn_sub(r_c260, zn_splat(P8::from_raw(65536i32)));
    let n197: ZB = zn_le(n196, zn_splat(P8::from_raw(0i32)));
    let n198: ZB = zn_gt(n196, zn_splat(P8::from_raw(0i32)));
    let n199: ZN = zsel_n(n194, n196, r_c260);
    let n202: ZB = zn_eq(zn_splat(P8::from_raw(1179648i32)), r_c273);
    let n203: ZB = zb_not(n202);
    let n204: ZN = zsel_n(n197, zn_splat(P8::from_raw(1179648i32)), r_c273);
    let n205: ZN = zsel_n(n194, n204, r_c273);
    let n206: ZB = zb_not(r_c38);
    let n207: ZB = zb_not(r_c362);
    let n208: bool = P8::from_raw(327680i32) == u.c363;
    let n209: bool = P8::from_raw(393216i32) == u.c364;
    let n210: bool = P8::from_raw(65536i32) == u.c365;
    let n211: bool = P8::from_raw(196608i32) == u.c366;
    let n212: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c85);
    let n213: ZN = zn_rem(n212, zn_splat(P8::from_raw(3932160i32)));
    let n214: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n213);
    let n215: ZN = zsel_n(n214, n160, r_c86);
    let n216: ZN = zsel_n(n116, n215, r_c86);
    let n217: ZN = zsel_n(n116, n213, r_c85);
    let n218: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c300);
    let n219: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n218);
    let n220: ZB = zn_gt(n219, zn_splat(P8::from_raw(2621440i32)));
    let n221: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), r_c301);
    let n222: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n221);
    let n223: ZB = zn_gt(n222, zn_splat(P8::from_raw(7340032i32)));
    let n224: ZB = zb_and(n220, n223);
    let n225: ZB = zn_lt(n218, zn_splat(P8::from_raw(3145728i32)));
    let n226: ZB = zb_and(n224, n225);
    let n227: ZB = zn_lt(n221, zn_splat(P8::from_raw(7864320i32)));
    let n228: ZB = zb_and(n226, n227);
    let n229: ZB = zb_and(n134, n228);
    let n230: ZB = zn_ge(r_c370, zn_splat(P8::from_raw(0i32)));
    let n231: ZB = zb_and(n229, n230);
    let n232: ZN = zn_mul(r_c369, zn_splat(P8::from_raw(13107i32)));
    let n233: ZB = zb_and(n202, n231);
    let n234: ZB = zb_and(n203, n231);
    let n235: ZB = zn_gt(n219, zn_splat(P8::from_raw(6815744i32)));
    let n236: ZB = zn_le(n219, zn_splat(P8::from_raw(6815744i32)));
    let n237: ZB = zb_and(n233, n235);
    let n238: ZB = zb_and(n233, n236);
    let n239: ZB = zb_or(n237, n238);
    let n240: ZB = zb_and(n235, n239);
    let n241: ZB = zb_and(n236, n239);
    let n242: ZB = zn_lt(n218, zn_splat(P8::from_raw(7340032i32)));
    let n243: ZB = zb_or(n240, n241);
    let n244: ZB = zb_and(n235, n242);
    let n245: ZB = zb_not(n244);
    let n246: ZB = zb_and(n243, n244);
    let n247: ZB = zb_and(n243, n245);
    let n248: ZB = zb_or(n246, n247);
    let n249: ZB = zb_and(n244, n248);
    let n250: ZB = zb_and(n245, n248);
    let n251: ZB = zb_and(n194, n234);
    let n252: ZB = zb_and(n195, n234);
    let n253: ZB = zb_and(n197, n251);
    let n254: ZB = zb_and(n198, n251);
    let n255: ZB = zb_or(n253, n254);
    let n256: ZB = zb_or(n252, n255);
    let n257: ZB = zb_or(n249, n250);
    let n258: ZN = zsel_n(n203, n199, r_c260);
    let n259: ZN = zsel_n(n203, n205, r_c273);
    let n260: ZB = zb_or(n256, n257);
    let n261: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n232);
    let n262: ZB = zb_not(n261);
    let n263: ZB = zb_and(n260, n261);
    let n264: ZB = zb_and(n260, n262);
    let n265: ZB = zb_or(n263, n264);
    let n266: ZN = zn_add(r_c367, n232);
    let n267: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n266);
    let n268: ZN = zn_flr(n267);
    let n269: ZN = zn_sub(n267, zn_splat(P8::from_raw(32768i32)));
    let n270: ZN = zn_sub(n269, n268);
    let n271: ZB = zn_gt(n268, zn_splat(P8::from_raw(0i32)));
    let n272: ZB = zn_le(n268, zn_splat(P8::from_raw(0i32)));
    let n273: ZB = zb_and(n265, n271);
    let n274: ZB = zb_and(n265, n272);
    let n275: ZB = zn_lt(n268, zn_splat(P8::from_raw(0i32)));
    let n276: ZB = zn_ge(n268, zn_splat(P8::from_raw(0i32)));
    let n277: ZB = zb_and(n274, n275);
    let n278: ZB = zb_and(n274, n276);
    let n279: ZN = zsel_n(n275, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n280: ZB = zb_or(n277, n278);
    let n281: ZN = zsel_n(n271, zn_splat(P8::from_raw(65536i32)), n279);
    let n282: ZB = zb_or(n273, n280);
    let n283: ZN = zn_abs(n268);
    let n284: ZN = zn_add(n218, n281);
    let n285: ZB = zn_tile_flag_at(g.cache, g.cart, n284, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n286: ZB = zb_not(n285);
    let n287: ZB = zb_and(n282, n286);
    let n288: ZB = zb_and(n282, n285);
    let n289: ZB = zb_or(n287, n288);
    let n290: ZB = zb_and(n286, n289);
    let n291: ZB = zb_and(n285, n289);
    let n292: ZB = zb_or(n290, n291);
    let n293: ZB = zb_and(n286, n292);
    let n294: ZB = zb_and(n285, n292);
    let n295: ZN = zn_add(r_c300, n281);
    let n296: ZB = zn_le(zn_splat(P8::from_raw(65536i32)), n283);
    let n297: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n283);
    let n298: ZB = zb_and(n293, n296);
    let n299: ZB = zb_and(n293, n297);
    let n300: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n295);
    let n301: ZN = zn_add(n281, n300);
    let n302: ZB = zn_tile_flag_at(g.cache, g.cart, n301, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n303: ZB = zb_not(n302);
    let n304: ZB = zb_and(n298, n303);
    let n305: ZB = zb_and(n298, n302);
    let n306: ZB = zb_or(n304, n305);
    let n307: ZB = zb_and(n303, n306);
    let n308: ZB = zb_and(n302, n306);
    let n309: ZB = zb_or(n307, n308);
    let n310: ZB = zb_and(n303, n309);
    let n311: ZB = zb_and(n302, n309);
    let n312: ZN = zn_add(n281, n295);
    let n313: ZB = zn_le(zn_splat(P8::from_raw(131072i32)), n283);
    let n314: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n283);
    let n315: ZB = zb_and(n310, n313);
    let n316: ZB = zb_and(n310, n314);
    let n317: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n312);
    let n318: ZN = zn_add(n281, n317);
    let n319: ZB = zn_tile_flag_at(g.cache, g.cart, n318, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n320: ZB = zb_not(n319);
    let n321: ZB = zb_and(n315, n320);
    let n322: ZB = zb_and(n315, n319);
    let n323: ZB = zb_or(n321, n322);
    let n324: ZB = zb_and(n320, n323);
    let n325: ZB = zb_and(n319, n323);
    let n326: ZB = zb_or(n324, n325);
    let n327: ZB = zb_and(n320, n326);
    let n328: ZB = zb_and(n319, n326);
    let n329: ZN = zn_add(n281, n312);
    let n330: ZB = zn_le(zn_splat(P8::from_raw(196608i32)), n283);
    let n331: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n283);
    let n332: ZB = zb_and(n327, n330);
    let n333: ZB = zb_and(n327, n331);
    let n334: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n329);
    let n335: ZN = zn_add(n281, n334);
    let n336: ZB = zn_tile_flag_at(g.cache, g.cart, n335, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n337: ZB = zb_not(n336);
    let n338: ZB = zb_and(n332, n337);
    let n339: ZB = zb_and(n332, n336);
    let n340: ZB = zb_or(n338, n339);
    let n341: ZB = zb_and(n337, n340);
    let n342: ZB = zb_and(n336, n340);
    let n343: ZB = zb_or(n341, n342);
    let n344: ZB = zb_and(n337, n343);
    let n345: ZB = zb_and(n336, n343);
    let n346: ZN = zn_add(n281, n329);
    let n347: ZB = zn_le(zn_splat(P8::from_raw(262144i32)), n283);
    let n348: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n283);
    let n349: ZB = zb_and(n344, n347);
    let n350: ZB = zb_and(n344, n348);
    let n351: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n346);
    let n352: ZN = zn_add(n281, n351);
    let n353: ZB = zn_tile_flag_at(g.cache, g.cart, n352, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n354: ZB = zb_not(n353);
    let n355: ZB = zb_and(n349, n354);
    let n356: ZB = zb_and(n349, n353);
    let n357: ZB = zb_or(n355, n356);
    let n358: ZB = zb_and(n354, n357);
    let n359: ZB = zb_and(n353, n357);
    let n360: ZB = zb_or(n358, n359);
    let n361: ZB = zb_and(n354, n360);
    let n362: ZB = zb_and(n353, n360);
    let n363: ZN = zn_add(n281, n346);
    let n364: ZB = zn_le(zn_splat(P8::from_raw(327680i32)), n283);
    let n365: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n283);
    let n366: ZB = zb_and(n361, n364);
    let n367: ZB = zb_and(n361, n365);
    let n368: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n363);
    let n369: ZN = zn_add(n281, n368);
    let n370: ZB = zn_tile_flag_at(g.cache, g.cart, n369, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n371: ZB = zb_not(n370);
    let n372: ZB = zb_and(n366, n371);
    let n373: ZB = zb_and(n366, n370);
    let n374: ZB = zb_or(n372, n373);
    let n375: ZB = zb_and(n371, n374);
    let n376: ZB = zb_and(n370, n374);
    let n377: ZB = zb_or(n375, n376);
    let n378: ZB = zb_and(n371, n377);
    let n379: ZB = zb_and(n370, n377);
    let n380: ZN = zn_add(n281, n363);
    let n381: ZB = zn_le(zn_splat(P8::from_raw(393216i32)), n283);
    let n382: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n283);
    let n383: ZB = zb_and(n378, n381);
    let n384: ZB = zb_and(n378, n382);
    let n385: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n380);
    let n386: ZN = zn_add(n281, n385);
    let n387: ZB = zn_tile_flag_at(g.cache, g.cart, n386, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n388: ZB = zb_not(n387);
    let n389: ZB = zb_and(n383, n388);
    let n390: ZB = zb_and(n383, n387);
    let n391: ZB = zb_or(n389, n390);
    let n392: ZB = zb_and(n388, n391);
    let n393: ZB = zb_and(n387, n391);
    let n394: ZB = zb_or(n392, n393);
    let n395: ZB = zb_and(n388, n394);
    let n396: ZB = zb_and(n387, n394);
    let n397: ZN = zn_add(n281, n380);
    let n398: ZB = zn_le(zn_splat(P8::from_raw(458752i32)), n283);
    let n399: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n283);
    let n400: ZB = zb_and(n395, n398);
    let n401: ZB = zb_and(n395, n399);
    let n402: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n397);
    let n403: ZN = zn_add(n281, n402);
    let n404: ZB = zn_tile_flag_at(g.cache, g.cart, n403, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n405: ZB = zb_not(n404);
    let n406: ZB = zb_and(n400, n405);
    let n407: ZB = zb_and(n400, n404);
    let n408: ZB = zb_or(n406, n407);
    let n409: ZB = zb_and(n405, n408);
    let n410: ZB = zb_and(n404, n408);
    let n411: ZB = zb_or(n409, n410);
    let n412: ZB = zb_and(n405, n411);
    let n413: ZB = zb_and(n404, n411);
    let n414: ZN = zn_add(n281, n397);
    let n415: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n283);
    let n416: ZN = zsel_n(n404, n397, n414);
    let n417: ZN = zsel_n(n404, zn_splat(P8::from_raw(0i32)), n270);
    let n418: ZN = zsel_n(n404, zn_splat(P8::from_raw(0i32)), n232);
    let n419: ZB = zb_or(n412, n413);
    let n420: ZB = zb_or(n404, n415);
    let n421: ZN = zsel_n(n399, n397, n416);
    let n422: ZN = zsel_n(n399, n270, n417);
    let n423: ZN = zsel_n(n399, n232, n418);
    let n424: ZB = zb_or(n401, n419);
    let n425: ZB = zb_or(n399, n420);
    let n426: ZN = zsel_n(n387, n380, n421);
    let n427: ZN = zsel_n(n387, zn_splat(P8::from_raw(0i32)), n422);
    let n428: ZN = zsel_n(n387, zn_splat(P8::from_raw(0i32)), n423);
    let n429: ZB = zb_or(n396, n424);
    let n430: ZB = zb_or(n387, n425);
    let n431: ZN = zsel_n(n382, n380, n426);
    let n432: ZN = zsel_n(n382, n270, n427);
    let n433: ZN = zsel_n(n382, n232, n428);
    let n434: ZB = zb_or(n384, n429);
    let n435: ZB = zb_or(n382, n430);
    let n436: ZN = zsel_n(n370, n363, n431);
    let n437: ZN = zsel_n(n370, zn_splat(P8::from_raw(0i32)), n432);
    let n438: ZN = zsel_n(n370, zn_splat(P8::from_raw(0i32)), n433);
    let n439: ZB = zb_or(n379, n434);
    let n440: ZB = zb_or(n370, n435);
    let n441: ZN = zsel_n(n365, n363, n436);
    let n442: ZN = zsel_n(n365, n270, n437);
    let n443: ZN = zsel_n(n365, n232, n438);
    let n444: ZB = zb_or(n367, n439);
    let n445: ZB = zb_or(n365, n440);
    let n446: ZN = zsel_n(n353, n346, n441);
    let n447: ZN = zsel_n(n353, zn_splat(P8::from_raw(0i32)), n442);
    let n448: ZN = zsel_n(n353, zn_splat(P8::from_raw(0i32)), n443);
    let n449: ZB = zb_or(n362, n444);
    let n450: ZB = zb_or(n353, n445);
    let n451: ZN = zsel_n(n348, n346, n446);
    let n452: ZN = zsel_n(n348, n270, n447);
    let n453: ZN = zsel_n(n348, n232, n448);
    let n454: ZB = zb_or(n350, n449);
    let n455: ZB = zb_or(n348, n450);
    let n456: ZN = zsel_n(n336, n329, n451);
    let n457: ZN = zsel_n(n336, zn_splat(P8::from_raw(0i32)), n452);
    let n458: ZN = zsel_n(n336, zn_splat(P8::from_raw(0i32)), n453);
    let n459: ZB = zb_or(n345, n454);
    let n460: ZB = zb_or(n336, n455);
    let n461: ZN = zsel_n(n331, n329, n456);
    let n462: ZN = zsel_n(n331, n270, n457);
    let n463: ZN = zsel_n(n331, n232, n458);
    let n464: ZB = zb_or(n333, n459);
    let n465: ZB = zb_or(n331, n460);
    let n466: ZN = zsel_n(n319, n312, n461);
    let n467: ZN = zsel_n(n319, zn_splat(P8::from_raw(0i32)), n462);
    let n468: ZN = zsel_n(n319, zn_splat(P8::from_raw(0i32)), n463);
    let n469: ZB = zb_or(n328, n464);
    let n470: ZB = zb_or(n319, n465);
    let n471: ZN = zsel_n(n314, n312, n466);
    let n472: ZN = zsel_n(n314, n270, n467);
    let n473: ZN = zsel_n(n314, n232, n468);
    let n474: ZB = zb_or(n316, n469);
    let n475: ZB = zb_or(n314, n470);
    let n476: ZN = zsel_n(n302, n295, n471);
    let n477: ZN = zsel_n(n302, zn_splat(P8::from_raw(0i32)), n472);
    let n478: ZN = zsel_n(n302, zn_splat(P8::from_raw(0i32)), n473);
    let n479: ZB = zb_or(n311, n474);
    let n480: ZB = zb_or(n302, n475);
    let n481: ZN = zsel_n(n297, n295, n476);
    let n482: ZN = zsel_n(n297, n270, n477);
    let n483: ZN = zsel_n(n297, n232, n478);
    let n484: ZB = zb_or(n299, n479);
    let n485: ZB = zb_or(n297, n480);
    let n486: ZN = zsel_n(n285, r_c300, n481);
    let n487: ZN = zsel_n(n285, zn_splat(P8::from_raw(0i32)), n482);
    let n488: ZN = zsel_n(n285, zn_splat(P8::from_raw(0i32)), n483);
    let n489: ZB = zb_or(n294, n484);
    let n490: ZB = zb_or(n285, n485);
    let n491: ZN = zn_add(r_c368, zn_splat(P8::from_raw(-196608i32)));
    let n492: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n491);
    let n493: ZN = zn_flr(n492);
    let n494: ZN = zn_sub(n492, zn_splat(P8::from_raw(32768i32)));
    let n495: ZN = zn_sub(n494, n493);
    let n496: ZB = zn_gt(n493, zn_splat(P8::from_raw(0i32)));
    let n497: ZB = zn_le(n493, zn_splat(P8::from_raw(0i32)));
    let n498: ZB = zb_and(n489, n496);
    let n499: ZB = zb_and(n489, n497);
    let n500: ZB = zn_lt(n493, zn_splat(P8::from_raw(0i32)));
    let n501: ZB = zn_ge(n493, zn_splat(P8::from_raw(0i32)));
    let n502: ZB = zb_and(n499, n500);
    let n503: ZB = zb_and(n499, n501);
    let n504: ZN = zsel_n(n500, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n505: ZB = zb_or(n502, n503);
    let n506: ZN = zsel_n(n496, zn_splat(P8::from_raw(65536i32)), n504);
    let n507: ZB = zb_or(n498, n505);
    let n508: ZN = zn_abs(n493);
    let n509: ZB = zn_gt(n506, zn_splat(P8::from_raw(0i32)));
    let n510: ZB = zn_le(n506, zn_splat(P8::from_raw(0i32)));
    let n511: ZB = zb_and(n507, n509);
    let n512: ZB = zb_and(n507, n510);
    let n513: ZB = zb_or(n511, n512);
    let n514: ZB = zb_and(n509, n513);
    let n515: ZB = zb_and(n510, n513);
    let n516: ZB = zb_or(n514, n515);
    let n517: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n486);
    let n518: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n517);
    let n519: ZN = zn_add(zn_splat(P8::from_raw(7274496i32)), n506);
    let n520: ZB = zn_tile_flag_at(g.cache, g.cart, n518, n519, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n521: ZB = zb_not(n520);
    let n522: ZB = zb_and(n516, n521);
    let n523: ZB = zb_and(n516, n520);
    let n524: ZB = zb_or(n522, n523);
    let n525: ZB = zb_and(n521, n524);
    let n526: ZB = zb_and(n520, n524);
    let n527: ZB = zb_or(n525, n526);
    let n528: ZB = zb_and(n521, n527);
    let n529: ZB = zb_and(n520, n527);
    let n530: ZN = zn_add(zn_splat(P8::from_raw(7077888i32)), n506);
    let n531: ZB = zn_le(zn_splat(P8::from_raw(65536i32)), n508);
    let n532: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n508);
    let n533: ZB = zb_and(n528, n531);
    let n534: ZB = zb_and(n528, n532);
    let n535: ZB = zb_and(n509, n533);
    let n536: ZB = zb_and(n510, n533);
    let n537: ZB = zb_or(n535, n536);
    let n538: ZB = zb_and(n509, n537);
    let n539: ZB = zb_and(n510, n537);
    let n540: ZB = zb_or(n538, n539);
    let n541: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n530);
    let n542: ZN = zn_add(n506, n541);
    let n543: ZB = zn_tile_flag_at(g.cache, g.cart, n518, n542, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n544: ZB = zb_not(n543);
    let n545: ZB = zb_and(n540, n544);
    let n546: ZB = zb_and(n540, n543);
    let n547: ZB = zb_or(n545, n546);
    let n548: ZB = zb_and(n544, n547);
    let n549: ZB = zb_and(n543, n547);
    let n550: ZB = zb_or(n548, n549);
    let n551: ZB = zb_and(n544, n550);
    let n552: ZB = zb_and(n543, n550);
    let n553: ZN = zn_add(n506, n530);
    let n554: ZB = zn_le(zn_splat(P8::from_raw(131072i32)), n508);
    let n555: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n508);
    let n556: ZB = zb_and(n551, n554);
    let n557: ZB = zb_and(n551, n555);
    let n558: ZB = zb_and(n509, n556);
    let n559: ZB = zb_and(n510, n556);
    let n560: ZB = zb_or(n558, n559);
    let n561: ZB = zb_and(n509, n560);
    let n562: ZB = zb_and(n510, n560);
    let n563: ZB = zb_or(n561, n562);
    let n564: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n553);
    let n565: ZN = zn_add(n506, n564);
    let n566: ZB = zn_tile_flag_at(g.cache, g.cart, n518, n565, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n567: ZB = zb_not(n566);
    let n568: ZB = zb_and(n563, n567);
    let n569: ZB = zb_and(n563, n566);
    let n570: ZB = zb_or(n568, n569);
    let n571: ZB = zb_and(n567, n570);
    let n572: ZB = zb_and(n566, n570);
    let n573: ZB = zb_or(n571, n572);
    let n574: ZB = zb_and(n567, n573);
    let n575: ZB = zb_and(n566, n573);
    let n576: ZN = zn_add(n506, n553);
    let n577: ZB = zn_le(zn_splat(P8::from_raw(196608i32)), n508);
    let n578: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n508);
    let n579: ZB = zb_and(n574, n577);
    let n580: ZB = zb_and(n574, n578);
    let n581: ZB = zb_and(n509, n579);
    let n582: ZB = zb_and(n510, n579);
    let n583: ZB = zb_or(n581, n582);
    let n584: ZB = zb_and(n509, n583);
    let n585: ZB = zb_and(n510, n583);
    let n586: ZB = zb_or(n584, n585);
    let n587: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n576);
    let n588: ZN = zn_add(n506, n587);
    let n589: ZB = zn_tile_flag_at(g.cache, g.cart, n518, n588, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n590: ZB = zb_not(n589);
    let n591: ZB = zb_and(n586, n590);
    let n592: ZB = zb_and(n586, n589);
    let n593: ZB = zb_or(n591, n592);
    let n594: ZB = zb_and(n590, n593);
    let n595: ZB = zb_and(n589, n593);
    let n596: ZB = zb_or(n594, n595);
    let n597: ZB = zb_and(n590, n596);
    let n598: ZB = zb_and(n589, n596);
    let n599: ZN = zn_add(n506, n576);
    let n600: ZB = zn_le(zn_splat(P8::from_raw(262144i32)), n508);
    let n601: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n508);
    let n602: ZB = zb_and(n597, n600);
    let n603: ZB = zb_and(n597, n601);
    let n604: ZB = zb_and(n509, n602);
    let n605: ZB = zb_and(n510, n602);
    let n606: ZB = zb_or(n604, n605);
    let n607: ZB = zb_and(n509, n606);
    let n608: ZB = zb_and(n510, n606);
    let n609: ZB = zb_or(n607, n608);
    let n610: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n599);
    let n611: ZN = zn_add(n506, n610);
    let n612: ZB = zn_tile_flag_at(g.cache, g.cart, n518, n611, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n613: ZB = zb_not(n612);
    let n614: ZB = zb_and(n609, n613);
    let n615: ZB = zb_and(n609, n612);
    let n616: ZB = zb_or(n614, n615);
    let n617: ZB = zb_and(n613, n616);
    let n618: ZB = zb_and(n612, n616);
    let n619: ZB = zb_or(n617, n618);
    let n620: ZB = zb_and(n613, n619);
    let n621: ZB = zb_and(n612, n619);
    let n622: ZN = zn_add(n506, n599);
    let n623: ZB = zn_le(zn_splat(P8::from_raw(327680i32)), n508);
    let n624: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n508);
    let n625: ZB = zb_and(n620, n623);
    let n626: ZB = zb_and(n620, n624);
    let n627: ZB = zb_and(n509, n625);
    let n628: ZB = zb_and(n510, n625);
    let n629: ZB = zb_or(n627, n628);
    let n630: ZB = zb_and(n509, n629);
    let n631: ZB = zb_and(n510, n629);
    let n632: ZB = zb_or(n630, n631);
    let n633: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n622);
    let n634: ZN = zn_add(n506, n633);
    let n635: ZB = zn_tile_flag_at(g.cache, g.cart, n518, n634, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n636: ZB = zb_not(n635);
    let n637: ZB = zb_and(n632, n636);
    let n638: ZB = zb_and(n632, n635);
    let n639: ZB = zb_or(n637, n638);
    let n640: ZB = zb_and(n636, n639);
    let n641: ZB = zb_and(n635, n639);
    let n642: ZB = zb_or(n640, n641);
    let n643: ZB = zb_and(n636, n642);
    let n644: ZB = zb_and(n635, n642);
    let n645: ZN = zn_add(n506, n622);
    let n646: ZB = zn_le(zn_splat(P8::from_raw(393216i32)), n508);
    let n647: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n508);
    let n648: ZB = zb_and(n643, n646);
    let n649: ZB = zb_and(n643, n647);
    let n650: ZB = zb_and(n509, n648);
    let n651: ZB = zb_and(n510, n648);
    let n652: ZB = zb_or(n650, n651);
    let n653: ZB = zb_and(n509, n652);
    let n654: ZB = zb_and(n510, n652);
    let n655: ZB = zb_or(n653, n654);
    let n656: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n645);
    let n657: ZN = zn_add(n506, n656);
    let n658: ZB = zn_tile_flag_at(g.cache, g.cart, n518, n657, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n659: ZB = zb_not(n658);
    let n660: ZB = zb_and(n655, n659);
    let n661: ZB = zb_and(n655, n658);
    let n662: ZB = zb_or(n660, n661);
    let n663: ZB = zb_and(n659, n662);
    let n664: ZB = zb_and(n658, n662);
    let n665: ZB = zb_or(n663, n664);
    let n666: ZB = zb_and(n659, n665);
    let n667: ZB = zb_and(n658, n665);
    let n668: ZN = zn_add(n506, n645);
    let n669: ZB = zn_le(zn_splat(P8::from_raw(458752i32)), n508);
    let n670: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n508);
    let n671: ZB = zb_and(n666, n669);
    let n672: ZB = zb_and(n666, n670);
    let n673: ZB = zb_and(n509, n671);
    let n674: ZB = zb_and(n510, n671);
    let n675: ZB = zb_or(n673, n674);
    let n676: ZB = zb_and(n509, n675);
    let n677: ZB = zb_and(n510, n675);
    let n678: ZB = zb_or(n676, n677);
    let n679: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n668);
    let n680: ZN = zn_add(n506, n679);
    let n681: ZB = zn_tile_flag_at(g.cache, g.cart, n518, n680, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n682: ZB = zb_not(n681);
    let n683: ZB = zb_and(n678, n682);
    let n684: ZB = zb_and(n678, n681);
    let n685: ZB = zb_or(n683, n684);
    let n686: ZB = zb_and(n682, n685);
    let n687: ZB = zb_and(n681, n685);
    let n688: ZB = zb_or(n686, n687);
    let n689: ZB = zb_and(n682, n688);
    let n690: ZB = zb_and(n681, n688);
    let n691: ZN = zn_add(n506, n668);
    let n692: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n508);
    let n693: ZB = zb_and(n490, n692);
    let n694: ZN = zsel_n(n681, n668, n691);
    let n695: ZN = zsel_n(n681, zn_splat(P8::from_raw(0i32)), n495);
    let n696: ZN = zsel_n(n681, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(-196608i32)));
    let n697: ZB = zb_or(n689, n690);
    let n698: ZB = zsel_b(n681, n490, n693);
    let n699: ZN = zsel_n(n670, n668, n694);
    let n700: ZN = zsel_n(n670, n495, n695);
    let n701: ZN = zsel_n(n670, zn_splat(P8::from_raw(-196608i32)), n696);
    let n702: ZB = zb_or(n672, n697);
    let n703: ZB = zsel_b(n670, n490, n698);
    let n704: ZN = zsel_n(n658, n645, n699);
    let n705: ZN = zsel_n(n658, zn_splat(P8::from_raw(0i32)), n700);
    let n706: ZN = zsel_n(n658, zn_splat(P8::from_raw(0i32)), n701);
    let n707: ZB = zb_or(n667, n702);
    let n708: ZB = zsel_b(n658, n490, n703);
    let n709: ZN = zsel_n(n647, n645, n704);
    let n710: ZN = zsel_n(n647, n495, n705);
    let n711: ZN = zsel_n(n647, zn_splat(P8::from_raw(-196608i32)), n706);
    let n712: ZB = zb_or(n649, n707);
    let n713: ZB = zsel_b(n647, n490, n708);
    let n714: ZN = zsel_n(n635, n622, n709);
    let n715: ZN = zsel_n(n635, zn_splat(P8::from_raw(0i32)), n710);
    let n716: ZN = zsel_n(n635, zn_splat(P8::from_raw(0i32)), n711);
    let n717: ZB = zb_or(n644, n712);
    let n718: ZB = zsel_b(n635, n490, n713);
    let n719: ZN = zsel_n(n624, n622, n714);
    let n720: ZN = zsel_n(n624, n495, n715);
    let n721: ZN = zsel_n(n624, zn_splat(P8::from_raw(-196608i32)), n716);
    let n722: ZB = zb_or(n626, n717);
    let n723: ZB = zsel_b(n624, n490, n718);
    let n724: ZN = zsel_n(n612, n599, n719);
    let n725: ZN = zsel_n(n612, zn_splat(P8::from_raw(0i32)), n720);
    let n726: ZN = zsel_n(n612, zn_splat(P8::from_raw(0i32)), n721);
    let n727: ZB = zb_or(n621, n722);
    let n728: ZB = zsel_b(n612, n490, n723);
    let n729: ZN = zsel_n(n601, n599, n724);
    let n730: ZN = zsel_n(n601, n495, n725);
    let n731: ZN = zsel_n(n601, zn_splat(P8::from_raw(-196608i32)), n726);
    let n732: ZB = zb_or(n603, n727);
    let n733: ZB = zsel_b(n601, n490, n728);
    let n734: ZN = zsel_n(n589, n576, n729);
    let n735: ZN = zsel_n(n589, zn_splat(P8::from_raw(0i32)), n730);
    let n736: ZN = zsel_n(n589, zn_splat(P8::from_raw(0i32)), n731);
    let n737: ZB = zb_or(n598, n732);
    let n738: ZB = zsel_b(n589, n490, n733);
    let n739: ZN = zsel_n(n578, n576, n734);
    let n740: ZN = zsel_n(n578, n495, n735);
    let n741: ZN = zsel_n(n578, zn_splat(P8::from_raw(-196608i32)), n736);
    let n742: ZB = zb_or(n580, n737);
    let n743: ZB = zsel_b(n578, n490, n738);
    let n744: ZN = zsel_n(n566, n553, n739);
    let n745: ZN = zsel_n(n566, zn_splat(P8::from_raw(0i32)), n740);
    let n746: ZN = zsel_n(n566, zn_splat(P8::from_raw(0i32)), n741);
    let n747: ZB = zb_or(n575, n742);
    let n748: ZB = zsel_b(n566, n490, n743);
    let n749: ZN = zsel_n(n555, n553, n744);
    let n750: ZN = zsel_n(n555, n495, n745);
    let n751: ZN = zsel_n(n555, zn_splat(P8::from_raw(-196608i32)), n746);
    let n752: ZB = zb_or(n557, n747);
    let n753: ZB = zsel_b(n555, n490, n748);
    let n754: ZN = zsel_n(n543, n530, n749);
    let n755: ZN = zsel_n(n543, zn_splat(P8::from_raw(0i32)), n750);
    let n756: ZN = zsel_n(n543, zn_splat(P8::from_raw(0i32)), n751);
    let n757: ZB = zb_or(n552, n752);
    let n758: ZB = zsel_b(n543, n490, n753);
    let n759: ZN = zsel_n(n532, n530, n754);
    let n760: ZN = zsel_n(n532, n495, n755);
    let n761: ZN = zsel_n(n532, zn_splat(P8::from_raw(-196608i32)), n756);
    let n762: ZB = zb_or(n534, n757);
    let n763: ZB = zsel_b(n532, n490, n758);
    let n764: ZN = zsel_n(n520, zn_splat(P8::from_raw(7077888i32)), n759);
    let n765: ZN = zsel_n(n520, zn_splat(P8::from_raw(0i32)), n760);
    let n766: ZN = zsel_n(n520, zn_splat(P8::from_raw(0i32)), n761);
    let n767: ZB = zb_or(n529, n762);
    let n768: ZB = zsel_b(n520, n490, n763);
    let n769: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n764);
    let n770: ZN = zn_div(n517, zn_splat(P8::from_raw(524288i32)));
    let n771: ZN = zn_flr(n770);
    let n772: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n771);
    let n773: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n517);
    let n774: ZN = zn_sub(n773, zn_splat(P8::from_raw(65536i32)));
    let n775: ZN = zn_div(n774, zn_splat(P8::from_raw(524288i32)));
    let n776: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n775);
    let n777: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n772);
    let n778: ZB = zn_le(n777, n776);
    let n779: ZB = zn_gt(n777, n776);
    let n780: ZB = zb_and(n767, n778);
    let n781: ZB = zb_and(n767, n779);
    let n782: ZN = zn_div(n769, zn_splat(P8::from_raw(524288i32)));
    let n783: ZN = zn_flr(n782);
    let n784: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n783);
    let n785: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n769);
    let n786: ZN = zn_sub(n785, zn_splat(P8::from_raw(65536i32)));
    let n787: ZN = zn_div(n786, zn_splat(P8::from_raw(524288i32)));
    let n788: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n787);
    let n789: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n784);
    let n790: ZB = zn_le(n789, n788);
    let n791: ZB = zn_gt(n789, n788);
    let n792: ZB = zb_and(n780, n790);
    let n793: ZB = zb_and(n780, n791);
    let n794: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n777);
    let n795: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n789);
    let n796: ZN = zn_mget(g.cart, n794, n795);
    let n797: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n796);
    let n798: ZB = zb_not(n797);
    let n799: ZB = zb_and(n792, n797);
    let n800: ZB = zb_and(n792, n798);
    let n801: ZN = zn_rem(n786, zn_splat(P8::from_raw(524288i32)));
    let n802: ZB = zn_ge(n801, zn_splat(P8::from_raw(393216i32)));
    let n803: ZB = zn_lt(n801, zn_splat(P8::from_raw(393216i32)));
    let n804: ZB = zb_and(n799, n803);
    let n805: ZB = zb_and(n799, n802);
    let n806: ZN = zn_mul(n789, zn_splat(P8::from_raw(524288i32)));
    let n807: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n806);
    let n808: ZB = zn_eq(n785, n807);
    let n809: ZB = zb_or(n804, n805);
    let n810: ZB = zb_or(n802, n808);
    let n811: ZB = zb_or(n800, n809);
    let n812: ZB = zb_and(n797, n810);
    let n813: ZB = zb_not(n812);
    let n814: ZB = zb_and(n811, n812);
    let n815: ZB = zb_and(n811, n813);
    let n816: ZB = zn_ge(n766, zn_splat(P8::from_raw(0i32)));
    let n817: ZB = zb_or(n814, n815);
    let n818: ZB = zb_and(n812, n816);
    let n819: ZB = zb_not(n818);
    let n820: ZB = zb_and(n817, n819);
    let n821: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n796);
    let n822: ZB = zb_not(n821);
    let n823: ZB = zb_and(n820, n821);
    let n824: ZB = zb_and(n820, n822);
    let n825: ZN = zn_rem(n769, zn_splat(P8::from_raw(524288i32)));
    let n826: ZB = zn_le(n825, zn_splat(P8::from_raw(131072i32)));
    let n827: ZB = zb_or(n823, n824);
    let n828: ZB = zb_and(n821, n826);
    let n829: ZB = zb_not(n828);
    let n830: ZB = zb_and(n827, n828);
    let n831: ZB = zb_and(n827, n829);
    let n832: ZB = zb_or(n830, n831);
    let n833: ZB = zb_and(n829, n832);
    let n834: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n796);
    let n835: ZB = zb_not(n834);
    let n836: ZB = zb_and(n833, n834);
    let n837: ZB = zb_and(n833, n835);
    let n838: ZN = zn_rem(n517, zn_splat(P8::from_raw(524288i32)));
    let n839: ZB = zn_le(n838, zn_splat(P8::from_raw(131072i32)));
    let n840: ZB = zb_or(n836, n837);
    let n841: ZB = zb_and(n834, n839);
    let n842: ZB = zb_not(n841);
    let n843: ZB = zb_and(n840, n841);
    let n844: ZB = zb_and(n840, n842);
    let n845: ZB = zn_le(n488, zn_splat(P8::from_raw(0i32)));
    let n846: ZB = zb_or(n843, n844);
    let n847: ZB = zb_and(n841, n845);
    let n848: ZB = zb_not(n847);
    let n849: ZB = zb_and(n846, n848);
    let n850: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n796);
    let n851: ZB = zb_not(n850);
    let n852: ZB = zb_and(n849, n850);
    let n853: ZB = zb_and(n849, n851);
    let n854: ZN = zn_rem(n774, zn_splat(P8::from_raw(524288i32)));
    let n855: ZB = zn_ge(n854, zn_splat(P8::from_raw(393216i32)));
    let n856: ZB = zn_lt(n854, zn_splat(P8::from_raw(393216i32)));
    let n857: ZB = zb_and(n852, n856);
    let n858: ZB = zb_and(n852, n855);
    let n859: ZN = zn_mul(n777, zn_splat(P8::from_raw(524288i32)));
    let n860: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n859);
    let n861: ZB = zn_eq(n773, n860);
    let n862: ZB = zb_or(n857, n858);
    let n863: ZB = zb_or(n855, n861);
    let n864: ZB = zb_or(n853, n862);
    let n865: ZB = zb_and(n850, n863);
    let n866: ZB = zb_not(n865);
    let n867: ZB = zb_and(n864, n865);
    let n868: ZB = zb_and(n864, n866);
    let n869: ZB = zn_ge(n488, zn_splat(P8::from_raw(0i32)));
    let n870: ZB = zb_or(n867, n868);
    let n871: ZB = zb_and(n865, n869);
    let n872: ZB = zb_not(n871);
    let n873: ZB = zb_and(n870, n872);
    let n874: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n784);
    let n875: ZB = zn_le(n874, n788);
    let n876: ZB = zn_gt(n874, n788);
    let n877: ZB = zb_and(n873, n875);
    let n878: ZB = zb_and(n873, n876);
    let n879: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n874);
    let n880: ZN = zn_mget(g.cart, n794, n879);
    let n881: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n880);
    let n882: ZB = zb_not(n881);
    let n883: ZB = zb_and(n877, n881);
    let n884: ZB = zb_and(n877, n882);
    let n885: ZB = zb_and(n803, n883);
    let n886: ZB = zb_and(n802, n883);
    let n887: ZN = zn_mul(n874, zn_splat(P8::from_raw(524288i32)));
    let n888: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n887);
    let n889: ZB = zn_eq(n785, n888);
    let n890: ZB = zb_or(n885, n886);
    let n891: ZB = zb_or(n802, n889);
    let n892: ZB = zb_or(n884, n890);
    let n893: ZB = zb_and(n881, n891);
    let n894: ZB = zb_not(n893);
    let n895: ZB = zb_and(n892, n893);
    let n896: ZB = zb_and(n892, n894);
    let n897: ZB = zb_or(n895, n896);
    let n898: ZB = zb_and(n816, n893);
    let n899: ZB = zb_not(n898);
    let n900: ZB = zb_and(n897, n899);
    let n901: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n880);
    let n902: ZB = zb_not(n901);
    let n903: ZB = zb_and(n900, n901);
    let n904: ZB = zb_and(n900, n902);
    let n905: ZB = zb_or(n903, n904);
    let n906: ZB = zb_and(n826, n901);
    let n907: ZB = zb_not(n906);
    let n908: ZB = zb_and(n905, n906);
    let n909: ZB = zb_and(n905, n907);
    let n910: ZB = zb_or(n908, n909);
    let n911: ZB = zb_and(n907, n910);
    let n912: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n880);
    let n913: ZB = zb_not(n912);
    let n914: ZB = zb_and(n911, n912);
    let n915: ZB = zb_and(n911, n913);
    let n916: ZB = zb_or(n914, n915);
    let n917: ZB = zb_and(n839, n912);
    let n918: ZB = zb_not(n917);
    let n919: ZB = zb_and(n916, n917);
    let n920: ZB = zb_and(n916, n918);
    let n921: ZB = zb_or(n919, n920);
    let n922: ZB = zb_and(n845, n917);
    let n923: ZB = zb_not(n922);
    let n924: ZB = zb_and(n921, n923);
    let n925: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n880);
    let n926: ZB = zb_not(n925);
    let n927: ZB = zb_and(n924, n925);
    let n928: ZB = zb_and(n924, n926);
    let n929: ZB = zb_and(n856, n927);
    let n930: ZB = zb_and(n855, n927);
    let n931: ZB = zb_or(n929, n930);
    let n932: ZB = zb_or(n928, n931);
    let n933: ZB = zb_and(n863, n925);
    let n934: ZB = zb_not(n933);
    let n935: ZB = zb_and(n932, n933);
    let n936: ZB = zb_and(n932, n934);
    let n937: ZB = zb_or(n935, n936);
    let n938: ZB = zb_and(n869, n933);
    let n939: ZB = zb_not(n938);
    let n940: ZB = zb_and(n937, n939);
    let n941: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n784);
    let n942: ZB = zn_le(n941, n788);
    let n943: ZB = zn_gt(n941, n788);
    let n944: ZB = zb_and(n940, n942);
    let n945: ZB = zb_and(n940, n943);
    let n946: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n941);
    let n947: ZN = zn_mget(g.cart, n794, n946);
    let n948: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n947);
    let n949: ZB = zb_not(n948);
    let n950: ZB = zb_and(n944, n948);
    let n951: ZB = zb_and(n944, n949);
    let n952: ZB = zb_and(n803, n950);
    let n953: ZB = zb_and(n802, n950);
    let n954: ZN = zn_mul(n941, zn_splat(P8::from_raw(524288i32)));
    let n955: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n954);
    let n956: ZB = zn_eq(n785, n955);
    let n957: ZB = zb_or(n952, n953);
    let n958: ZB = zb_or(n802, n956);
    let n959: ZB = zb_or(n951, n957);
    let n960: ZB = zb_and(n948, n958);
    let n961: ZB = zb_not(n960);
    let n962: ZB = zb_and(n959, n960);
    let n963: ZB = zb_and(n959, n961);
    let n964: ZB = zb_or(n962, n963);
    let n965: ZB = zb_and(n816, n960);
    let n966: ZB = zb_not(n965);
    let n967: ZB = zb_and(n964, n966);
    let n968: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n947);
    let n969: ZB = zb_not(n968);
    let n970: ZB = zb_and(n967, n968);
    let n971: ZB = zb_and(n967, n969);
    let n972: ZB = zb_or(n970, n971);
    let n973: ZB = zb_and(n826, n968);
    let n974: ZB = zb_not(n973);
    let n975: ZB = zb_and(n972, n973);
    let n976: ZB = zb_and(n972, n974);
    let n977: ZB = zb_or(n975, n976);
    let n978: ZB = zb_and(n974, n977);
    let n979: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n947);
    let n980: ZB = zb_not(n979);
    let n981: ZB = zb_and(n978, n979);
    let n982: ZB = zb_and(n978, n980);
    let n983: ZB = zb_or(n981, n982);
    let n984: ZB = zb_and(n839, n979);
    let n985: ZB = zb_not(n984);
    let n986: ZB = zb_and(n983, n984);
    let n987: ZB = zb_and(n983, n985);
    let n988: ZB = zb_or(n986, n987);
    let n989: ZB = zb_and(n845, n984);
    let n990: ZB = zb_not(n989);
    let n991: ZB = zb_and(n988, n990);
    let n992: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n947);
    let n993: ZB = zb_not(n992);
    let n994: ZB = zb_and(n991, n992);
    let n995: ZB = zb_and(n991, n993);
    let n996: ZB = zb_and(n856, n994);
    let n997: ZB = zb_and(n855, n994);
    let n998: ZB = zb_or(n996, n997);
    let n999: ZB = zb_or(n995, n998);
    let n1000: ZB = zb_and(n863, n992);
    let n1001: ZB = zb_not(n1000);
    let n1002: ZB = zb_and(n999, n1000);
    let n1003: ZB = zb_and(n999, n1001);
    let n1004: ZB = zb_or(n1002, n1003);
    let n1005: ZB = zb_and(n869, n1000);
    let n1006: ZB = zb_not(n1005);
    let n1007: ZB = zb_and(n1004, n1006);
    let n1008: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n784);
    let n1009: ZB = zn_gt(n1008, n788);
    let n1010: ZB = zb_and(n768, n1009);
    let n1011: ZB = zb_or(n945, n1007);
    let n1012: ZB = zsel_b(n943, n768, n1010);
    let n1013: ZB = zb_or(n878, n1011);
    let n1014: ZB = zsel_b(n876, n768, n1012);
    let n1015: ZB = zb_or(n793, n1013);
    let n1016: ZB = zsel_b(n791, n768, n1014);
    let n1017: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n772);
    let n1018: ZB = zn_le(n1017, n776);
    let n1019: ZB = zn_gt(n1017, n776);
    let n1020: ZB = zb_and(n1015, n1018);
    let n1021: ZB = zb_and(n1015, n1019);
    let n1022: ZB = zb_and(n790, n1020);
    let n1023: ZB = zb_and(n791, n1020);
    let n1024: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n1017);
    let n1025: ZN = zn_mget(g.cart, n1024, n795);
    let n1026: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1025);
    let n1027: ZB = zb_not(n1026);
    let n1028: ZB = zb_and(n1022, n1026);
    let n1029: ZB = zb_and(n1022, n1027);
    let n1030: ZB = zb_and(n803, n1028);
    let n1031: ZB = zb_and(n802, n1028);
    let n1032: ZB = zb_or(n1030, n1031);
    let n1033: ZB = zb_or(n1029, n1032);
    let n1034: ZB = zb_and(n810, n1026);
    let n1035: ZB = zb_not(n1034);
    let n1036: ZB = zb_and(n1033, n1034);
    let n1037: ZB = zb_and(n1033, n1035);
    let n1038: ZB = zb_or(n1036, n1037);
    let n1039: ZB = zb_and(n816, n1034);
    let n1040: ZB = zb_not(n1039);
    let n1041: ZB = zb_and(n1038, n1040);
    let n1042: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1025);
    let n1043: ZB = zb_not(n1042);
    let n1044: ZB = zb_and(n1041, n1042);
    let n1045: ZB = zb_and(n1041, n1043);
    let n1046: ZB = zb_or(n1044, n1045);
    let n1047: ZB = zb_and(n826, n1042);
    let n1048: ZB = zb_not(n1047);
    let n1049: ZB = zb_and(n1046, n1047);
    let n1050: ZB = zb_and(n1046, n1048);
    let n1051: ZB = zb_or(n1049, n1050);
    let n1052: ZB = zb_and(n1048, n1051);
    let n1053: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1025);
    let n1054: ZB = zb_not(n1053);
    let n1055: ZB = zb_and(n1052, n1053);
    let n1056: ZB = zb_and(n1052, n1054);
    let n1057: ZB = zb_or(n1055, n1056);
    let n1058: ZB = zb_and(n839, n1053);
    let n1059: ZB = zb_not(n1058);
    let n1060: ZB = zb_and(n1057, n1058);
    let n1061: ZB = zb_and(n1057, n1059);
    let n1062: ZB = zb_or(n1060, n1061);
    let n1063: ZB = zb_and(n845, n1058);
    let n1064: ZB = zb_not(n1063);
    let n1065: ZB = zb_and(n1062, n1064);
    let n1066: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1025);
    let n1067: ZB = zb_not(n1066);
    let n1068: ZB = zb_and(n1065, n1066);
    let n1069: ZB = zb_and(n1065, n1067);
    let n1070: ZB = zb_and(n856, n1068);
    let n1071: ZB = zb_and(n855, n1068);
    let n1072: ZN = zn_mul(n1017, zn_splat(P8::from_raw(524288i32)));
    let n1073: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1072);
    let n1074: ZB = zn_eq(n773, n1073);
    let n1075: ZB = zb_or(n1070, n1071);
    let n1076: ZB = zb_or(n855, n1074);
    let n1077: ZB = zb_or(n1069, n1075);
    let n1078: ZB = zb_and(n1066, n1076);
    let n1079: ZB = zb_not(n1078);
    let n1080: ZB = zb_and(n1077, n1078);
    let n1081: ZB = zb_and(n1077, n1079);
    let n1082: ZB = zb_or(n1080, n1081);
    let n1083: ZB = zb_and(n869, n1078);
    let n1084: ZB = zb_not(n1083);
    let n1085: ZB = zb_and(n1082, n1084);
    let n1086: ZB = zb_and(n875, n1085);
    let n1087: ZB = zb_and(n876, n1085);
    let n1088: ZN = zn_mget(g.cart, n1024, n879);
    let n1089: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1088);
    let n1090: ZB = zb_not(n1089);
    let n1091: ZB = zb_and(n1086, n1089);
    let n1092: ZB = zb_and(n1086, n1090);
    let n1093: ZB = zb_and(n803, n1091);
    let n1094: ZB = zb_and(n802, n1091);
    let n1095: ZB = zb_or(n1093, n1094);
    let n1096: ZB = zb_or(n1092, n1095);
    let n1097: ZB = zb_and(n891, n1089);
    let n1098: ZB = zb_not(n1097);
    let n1099: ZB = zb_and(n1096, n1097);
    let n1100: ZB = zb_and(n1096, n1098);
    let n1101: ZB = zb_or(n1099, n1100);
    let n1102: ZB = zb_and(n816, n1097);
    let n1103: ZB = zb_not(n1102);
    let n1104: ZB = zb_and(n1101, n1103);
    let n1105: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1088);
    let n1106: ZB = zb_not(n1105);
    let n1107: ZB = zb_and(n1104, n1105);
    let n1108: ZB = zb_and(n1104, n1106);
    let n1109: ZB = zb_or(n1107, n1108);
    let n1110: ZB = zb_and(n826, n1105);
    let n1111: ZB = zb_not(n1110);
    let n1112: ZB = zb_and(n1109, n1110);
    let n1113: ZB = zb_and(n1109, n1111);
    let n1114: ZB = zb_or(n1112, n1113);
    let n1115: ZB = zb_and(n1111, n1114);
    let n1116: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1088);
    let n1117: ZB = zb_not(n1116);
    let n1118: ZB = zb_and(n1115, n1116);
    let n1119: ZB = zb_and(n1115, n1117);
    let n1120: ZB = zb_or(n1118, n1119);
    let n1121: ZB = zb_and(n839, n1116);
    let n1122: ZB = zb_not(n1121);
    let n1123: ZB = zb_and(n1120, n1121);
    let n1124: ZB = zb_and(n1120, n1122);
    let n1125: ZB = zb_or(n1123, n1124);
    let n1126: ZB = zb_and(n845, n1121);
    let n1127: ZB = zb_not(n1126);
    let n1128: ZB = zb_and(n1125, n1127);
    let n1129: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1088);
    let n1130: ZB = zb_not(n1129);
    let n1131: ZB = zb_and(n1128, n1129);
    let n1132: ZB = zb_and(n1128, n1130);
    let n1133: ZB = zb_and(n856, n1131);
    let n1134: ZB = zb_and(n855, n1131);
    let n1135: ZB = zb_or(n1133, n1134);
    let n1136: ZB = zb_or(n1132, n1135);
    let n1137: ZB = zb_and(n1076, n1129);
    let n1138: ZB = zb_not(n1137);
    let n1139: ZB = zb_and(n1136, n1137);
    let n1140: ZB = zb_and(n1136, n1138);
    let n1141: ZB = zb_or(n1139, n1140);
    let n1142: ZB = zb_and(n869, n1137);
    let n1143: ZB = zb_not(n1142);
    let n1144: ZB = zb_and(n1141, n1143);
    let n1145: ZB = zb_and(n942, n1144);
    let n1146: ZB = zb_and(n943, n1144);
    let n1147: ZN = zn_mget(g.cart, n1024, n946);
    let n1148: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1147);
    let n1149: ZB = zb_not(n1148);
    let n1150: ZB = zb_and(n1145, n1148);
    let n1151: ZB = zb_and(n1145, n1149);
    let n1152: ZB = zb_and(n803, n1150);
    let n1153: ZB = zb_and(n802, n1150);
    let n1154: ZB = zb_or(n1152, n1153);
    let n1155: ZB = zb_or(n1151, n1154);
    let n1156: ZB = zb_and(n958, n1148);
    let n1157: ZB = zb_not(n1156);
    let n1158: ZB = zb_and(n1155, n1156);
    let n1159: ZB = zb_and(n1155, n1157);
    let n1160: ZB = zb_or(n1158, n1159);
    let n1161: ZB = zb_and(n816, n1156);
    let n1162: ZB = zb_not(n1161);
    let n1163: ZB = zb_and(n1160, n1162);
    let n1164: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1147);
    let n1165: ZB = zb_not(n1164);
    let n1166: ZB = zb_and(n1163, n1164);
    let n1167: ZB = zb_and(n1163, n1165);
    let n1168: ZB = zb_or(n1166, n1167);
    let n1169: ZB = zb_and(n826, n1164);
    let n1170: ZB = zb_not(n1169);
    let n1171: ZB = zb_and(n1168, n1169);
    let n1172: ZB = zb_and(n1168, n1170);
    let n1173: ZB = zb_or(n1171, n1172);
    let n1174: ZB = zb_and(n1170, n1173);
    let n1175: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1147);
    let n1176: ZB = zb_not(n1175);
    let n1177: ZB = zb_and(n1174, n1175);
    let n1178: ZB = zb_and(n1174, n1176);
    let n1179: ZB = zb_or(n1177, n1178);
    let n1180: ZB = zb_and(n839, n1175);
    let n1181: ZB = zb_not(n1180);
    let n1182: ZB = zb_and(n1179, n1180);
    let n1183: ZB = zb_and(n1179, n1181);
    let n1184: ZB = zb_or(n1182, n1183);
    let n1185: ZB = zb_and(n845, n1180);
    let n1186: ZB = zb_not(n1185);
    let n1187: ZB = zb_and(n1184, n1186);
    let n1188: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1147);
    let n1189: ZB = zb_not(n1188);
    let n1190: ZB = zb_and(n1187, n1188);
    let n1191: ZB = zb_and(n1187, n1189);
    let n1192: ZB = zb_and(n856, n1190);
    let n1193: ZB = zb_and(n855, n1190);
    let n1194: ZB = zb_or(n1192, n1193);
    let n1195: ZB = zb_or(n1191, n1194);
    let n1196: ZB = zb_and(n1076, n1188);
    let n1197: ZB = zb_not(n1196);
    let n1198: ZB = zb_and(n1195, n1196);
    let n1199: ZB = zb_and(n1195, n1197);
    let n1200: ZB = zb_or(n1198, n1199);
    let n1201: ZB = zb_and(n869, n1196);
    let n1202: ZB = zb_not(n1201);
    let n1203: ZB = zb_and(n1200, n1202);
    let n1204: ZB = zb_and(n1009, n1016);
    let n1205: ZB = zb_or(n1146, n1203);
    let n1206: ZB = zsel_b(n943, n1016, n1204);
    let n1207: ZB = zb_or(n1087, n1205);
    let n1208: ZB = zsel_b(n876, n1016, n1206);
    let n1209: ZB = zb_or(n1023, n1207);
    let n1210: ZB = zsel_b(n791, n1016, n1208);
    let n1211: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n772);
    let n1212: ZB = zn_le(n1211, n776);
    let n1213: ZB = zn_gt(n1211, n776);
    let n1214: ZB = zb_and(n1209, n1212);
    let n1215: ZB = zb_and(n1209, n1213);
    let n1216: ZB = zb_and(n790, n1214);
    let n1217: ZB = zb_and(n791, n1214);
    let n1218: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n1211);
    let n1219: ZN = zn_mget(g.cart, n1218, n795);
    let n1220: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1219);
    let n1221: ZB = zb_not(n1220);
    let n1222: ZB = zb_and(n1216, n1220);
    let n1223: ZB = zb_and(n1216, n1221);
    let n1224: ZB = zb_and(n803, n1222);
    let n1225: ZB = zb_and(n802, n1222);
    let n1226: ZB = zb_or(n1224, n1225);
    let n1227: ZB = zb_or(n1223, n1226);
    let n1228: ZB = zb_and(n810, n1220);
    let n1229: ZB = zb_not(n1228);
    let n1230: ZB = zb_and(n1227, n1228);
    let n1231: ZB = zb_and(n1227, n1229);
    let n1232: ZB = zb_or(n1230, n1231);
    let n1233: ZB = zb_and(n816, n1228);
    let n1234: ZB = zb_not(n1233);
    let n1235: ZB = zb_and(n1232, n1234);
    let n1236: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1219);
    let n1237: ZB = zb_not(n1236);
    let n1238: ZB = zb_and(n1235, n1236);
    let n1239: ZB = zb_and(n1235, n1237);
    let n1240: ZB = zb_or(n1238, n1239);
    let n1241: ZB = zb_and(n826, n1236);
    let n1242: ZB = zb_not(n1241);
    let n1243: ZB = zb_and(n1240, n1241);
    let n1244: ZB = zb_and(n1240, n1242);
    let n1245: ZB = zb_or(n1243, n1244);
    let n1246: ZB = zb_and(n1242, n1245);
    let n1247: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1219);
    let n1248: ZB = zb_not(n1247);
    let n1249: ZB = zb_and(n1246, n1247);
    let n1250: ZB = zb_and(n1246, n1248);
    let n1251: ZB = zb_or(n1249, n1250);
    let n1252: ZB = zb_and(n839, n1247);
    let n1253: ZB = zb_not(n1252);
    let n1254: ZB = zb_and(n1251, n1252);
    let n1255: ZB = zb_and(n1251, n1253);
    let n1256: ZB = zb_or(n1254, n1255);
    let n1257: ZB = zb_and(n845, n1252);
    let n1258: ZB = zb_not(n1257);
    let n1259: ZB = zb_and(n1256, n1258);
    let n1260: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1219);
    let n1261: ZB = zb_not(n1260);
    let n1262: ZB = zb_and(n1259, n1260);
    let n1263: ZB = zb_and(n1259, n1261);
    let n1264: ZB = zb_and(n856, n1262);
    let n1265: ZB = zb_and(n855, n1262);
    let n1266: ZN = zn_mul(n1211, zn_splat(P8::from_raw(524288i32)));
    let n1267: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1266);
    let n1268: ZB = zn_eq(n773, n1267);
    let n1269: ZB = zb_or(n1264, n1265);
    let n1270: ZB = zb_or(n855, n1268);
    let n1271: ZB = zb_or(n1263, n1269);
    let n1272: ZB = zb_and(n1260, n1270);
    let n1273: ZB = zb_not(n1272);
    let n1274: ZB = zb_and(n1271, n1272);
    let n1275: ZB = zb_and(n1271, n1273);
    let n1276: ZB = zb_or(n1274, n1275);
    let n1277: ZB = zb_and(n869, n1272);
    let n1278: ZB = zb_not(n1277);
    let n1279: ZB = zb_and(n1276, n1278);
    let n1280: ZB = zb_and(n875, n1279);
    let n1281: ZB = zb_and(n876, n1279);
    let n1282: ZN = zn_mget(g.cart, n1218, n879);
    let n1283: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1282);
    let n1284: ZB = zb_not(n1283);
    let n1285: ZB = zb_and(n1280, n1283);
    let n1286: ZB = zb_and(n1280, n1284);
    let n1287: ZB = zb_and(n803, n1285);
    let n1288: ZB = zb_and(n802, n1285);
    let n1289: ZB = zb_or(n1287, n1288);
    let n1290: ZB = zb_or(n1286, n1289);
    let n1291: ZB = zb_and(n891, n1283);
    let n1292: ZB = zb_not(n1291);
    let n1293: ZB = zb_and(n1290, n1291);
    let n1294: ZB = zb_and(n1290, n1292);
    let n1295: ZB = zb_or(n1293, n1294);
    let n1296: ZB = zb_and(n816, n1291);
    let n1297: ZB = zb_not(n1296);
    let n1298: ZB = zb_and(n1295, n1297);
    let n1299: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1282);
    let n1300: ZB = zb_not(n1299);
    let n1301: ZB = zb_and(n1298, n1299);
    let n1302: ZB = zb_and(n1298, n1300);
    let n1303: ZB = zb_or(n1301, n1302);
    let n1304: ZB = zb_and(n826, n1299);
    let n1305: ZB = zb_not(n1304);
    let n1306: ZB = zb_and(n1303, n1304);
    let n1307: ZB = zb_and(n1303, n1305);
    let n1308: ZB = zb_or(n1306, n1307);
    let n1309: ZB = zb_and(n1305, n1308);
    let n1310: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1282);
    let n1311: ZB = zb_not(n1310);
    let n1312: ZB = zb_and(n1309, n1310);
    let n1313: ZB = zb_and(n1309, n1311);
    let n1314: ZB = zb_or(n1312, n1313);
    let n1315: ZB = zb_and(n839, n1310);
    let n1316: ZB = zb_not(n1315);
    let n1317: ZB = zb_and(n1314, n1315);
    let n1318: ZB = zb_and(n1314, n1316);
    let n1319: ZB = zb_or(n1317, n1318);
    let n1320: ZB = zb_and(n845, n1315);
    let n1321: ZB = zb_not(n1320);
    let n1322: ZB = zb_and(n1319, n1321);
    let n1323: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1282);
    let n1324: ZB = zb_not(n1323);
    let n1325: ZB = zb_and(n1322, n1323);
    let n1326: ZB = zb_and(n1322, n1324);
    let n1327: ZB = zb_and(n856, n1325);
    let n1328: ZB = zb_and(n855, n1325);
    let n1329: ZB = zb_or(n1327, n1328);
    let n1330: ZB = zb_or(n1326, n1329);
    let n1331: ZB = zb_and(n1270, n1323);
    let n1332: ZB = zb_not(n1331);
    let n1333: ZB = zb_and(n1330, n1331);
    let n1334: ZB = zb_and(n1330, n1332);
    let n1335: ZB = zb_or(n1333, n1334);
    let n1336: ZB = zb_and(n869, n1331);
    let n1337: ZB = zb_not(n1336);
    let n1338: ZB = zb_and(n1335, n1337);
    let n1339: ZB = zb_and(n942, n1338);
    let n1340: ZB = zb_and(n943, n1338);
    let n1341: ZN = zn_mget(g.cart, n1218, n946);
    let n1342: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1341);
    let n1343: ZB = zb_not(n1342);
    let n1344: ZB = zb_and(n1339, n1342);
    let n1345: ZB = zb_and(n1339, n1343);
    let n1346: ZB = zb_and(n803, n1344);
    let n1347: ZB = zb_and(n802, n1344);
    let n1348: ZB = zb_or(n1346, n1347);
    let n1349: ZB = zb_or(n1345, n1348);
    let n1350: ZB = zb_and(n958, n1342);
    let n1351: ZB = zb_not(n1350);
    let n1352: ZB = zb_and(n1349, n1350);
    let n1353: ZB = zb_and(n1349, n1351);
    let n1354: ZB = zb_or(n1352, n1353);
    let n1355: ZB = zb_and(n816, n1350);
    let n1356: ZB = zb_not(n1355);
    let n1357: ZB = zb_and(n1354, n1356);
    let n1358: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1341);
    let n1359: ZB = zb_not(n1358);
    let n1360: ZB = zb_and(n1357, n1358);
    let n1361: ZB = zb_and(n1357, n1359);
    let n1362: ZB = zb_or(n1360, n1361);
    let n1363: ZB = zb_and(n826, n1358);
    let n1364: ZB = zb_not(n1363);
    let n1365: ZB = zb_and(n1362, n1363);
    let n1366: ZB = zb_and(n1362, n1364);
    let n1367: ZB = zb_or(n1365, n1366);
    let n1368: ZB = zb_and(n1364, n1367);
    let n1369: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1341);
    let n1370: ZB = zb_not(n1369);
    let n1371: ZB = zb_and(n1368, n1369);
    let n1372: ZB = zb_and(n1368, n1370);
    let n1373: ZB = zb_or(n1371, n1372);
    let n1374: ZB = zb_and(n839, n1369);
    let n1375: ZB = zb_not(n1374);
    let n1376: ZB = zb_and(n1373, n1374);
    let n1377: ZB = zb_and(n1373, n1375);
    let n1378: ZB = zb_or(n1376, n1377);
    let n1379: ZB = zb_and(n845, n1374);
    let n1380: ZB = zb_not(n1379);
    let n1381: ZB = zb_and(n1378, n1380);
    let n1382: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1341);
    let n1383: ZB = zb_not(n1382);
    let n1384: ZB = zb_and(n1381, n1382);
    let n1385: ZB = zb_and(n1381, n1383);
    let n1386: ZB = zb_and(n856, n1384);
    let n1387: ZB = zb_and(n855, n1384);
    let n1388: ZB = zb_or(n1386, n1387);
    let n1389: ZB = zb_or(n1385, n1388);
    let n1390: ZB = zb_and(n1270, n1382);
    let n1391: ZB = zb_not(n1390);
    let n1392: ZB = zb_and(n1389, n1390);
    let n1393: ZB = zb_and(n1389, n1391);
    let n1394: ZB = zb_or(n1392, n1393);
    let n1395: ZB = zb_and(n869, n1390);
    let n1396: ZB = zb_not(n1395);
    let n1397: ZB = zb_and(n1394, n1396);
    let n1398: ZB = zb_and(n1009, n1210);
    let n1399: ZB = zb_or(n1340, n1397);
    let n1400: ZB = zsel_b(n943, n1210, n1398);
    let n1401: ZB = zb_or(n1281, n1399);
    let n1402: ZB = zsel_b(n876, n1210, n1400);
    let n1403: ZB = zb_or(n1217, n1401);
    let n1404: ZB = zsel_b(n791, n1210, n1402);
    let n1405: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n772);
    let n1406: ZB = zn_gt(n1405, n776);
    let n1407: ZB = zb_and(n1404, n1406);
    let n1408: ZB = zb_or(n1215, n1403);
    let n1409: ZB = zsel_b(n1213, n1210, n1407);
    let n1410: ZB = zb_or(n1021, n1408);
    let n1411: ZB = zsel_b(n1019, n1016, n1409);
    let n1412: ZB = zb_or(n781, n1410);
    let n1413: ZB = zsel_b(n779, n768, n1411);
    let n1414: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n769);
    let n1415: ZB = zn_tile_flag_at(g.cache, g.cart, n518, n1414, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1416: ZB = zb_not(n1415);
    let n1417: ZB = zb_and(n1412, n1416);
    let n1418: ZB = zb_and(n1412, n1415);
    let n1419: ZB = zb_or(n1417, n1418);
    let n1420: ZB = zb_and(n1416, n1419);
    let n1421: ZB = zb_and(n1415, n1419);
    let n1422: ZB = zb_or(n1420, n1421);
    let n1423: ZB = zb_and(n1415, n1422);
    let n1424: ZB = zb_and(n1416, n1422);
    let n1425: ZB = zn_gt(r_c286, zn_splat(P8::from_raw(0i32)));
    let n1426: ZB = zn_le(r_c286, zn_splat(P8::from_raw(0i32)));
    let n1427: ZB = zb_and(n1424, n1425);
    let n1428: ZB = zb_and(n1424, n1426);
    let n1429: ZN = zn_sub(r_c286, zn_splat(P8::from_raw(65536i32)));
    let n1430: ZN = zsel_n(n1425, n1429, r_c286);
    let n1431: ZB = zb_or(n1427, n1428);
    let n1432: ZN = zsel_n(n1415, zn_splat(P8::from_raw(393216i32)), n1430);
    let n1433: ZB = zb_or(n1423, n1431);
    let n1434: ZN = zn_sub(r_c281, zn_splat(P8::from_raw(65536i32)));
    let n1435: ZB = zn_gt(r_c283, zn_splat(P8::from_raw(0i32)));
    let n1436: ZB = zn_le(r_c283, zn_splat(P8::from_raw(0i32)));
    let n1437: ZB = zb_and(n1433, n1435);
    let n1438: ZB = zb_and(n1433, n1436);
    let n1439: ZN = zn_sub(r_c283, zn_splat(P8::from_raw(65536i32)));
    let n1440: ZB = zn_gt(n488, r_c359);
    let n1441: ZB = zn_le(n488, r_c359);
    let n1442: ZB = zb_and(n1437, n1440);
    let n1443: ZB = zb_and(n1437, n1441);
    let n1444: ZN = zn_sub(n488, r_c357);
    let n1445: ZN = zn_max(r_c359, n1444);
    let n1446: ZN = zn_add(r_c357, n488);
    let n1447: ZN = zn_min(r_c359, n1446);
    let n1448: ZN = zsel_n(n1440, n1445, n1447);
    let n1449: ZB = zb_or(n1442, n1443);
    let n1450: ZB = zn_gt(n766, r_c360);
    let n1451: ZB = zn_le(n766, r_c360);
    let n1452: ZB = zb_and(n1449, n1450);
    let n1453: ZB = zb_and(n1449, n1451);
    let n1454: ZN = zn_sub(n766, r_c358);
    let n1455: ZN = zn_max(r_c360, n1454);
    let n1456: ZN = zn_add(r_c358, n766);
    let n1457: ZN = zn_min(r_c360, n1456);
    let n1458: ZN = zsel_n(n1450, n1455, n1457);
    let n1459: ZB = zb_or(n1452, n1453);
    let n1460: ZB = zb_and(n1416, n1438);
    let n1461: ZB = zb_and(n1415, n1438);
    let n1462: ZN = zsel_n(n1416, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n1463: ZB = zb_or(n1460, n1461);
    let n1464: ZN = zn_abs(n488);
    let n1465: ZB = zn_gt(n1464, zn_splat(P8::from_raw(65536i32)));
    let n1466: ZB = zn_le(n1464, zn_splat(P8::from_raw(65536i32)));
    let n1467: ZB = zb_and(n1463, n1465);
    let n1468: ZB = zb_and(n1463, n1466);
    let n1469: ZB = zn_gt(n488, zn_splat(P8::from_raw(0i32)));
    let n1470: ZB = zb_and(n1467, n1469);
    let n1471: ZB = zb_and(n845, n1467);
    let n1472: ZB = zn_lt(n488, zn_splat(P8::from_raw(0i32)));
    let n1473: ZB = zb_and(n1471, n1472);
    let n1474: ZB = zb_and(n869, n1471);
    let n1475: ZB = zn_gt(n488, zn_splat(P8::from_raw(65536i32)));
    let n1476: ZB = zn_le(n488, zn_splat(P8::from_raw(65536i32)));
    let n1477: ZB = zb_and(n1470, n1475);
    let n1478: ZB = zb_and(n1470, n1476);
    let n1479: ZN = zn_sub(n488, zn_splat(P8::from_raw(9830i32)));
    let n1480: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n1479);
    let n1481: ZN = zn_add(zn_splat(P8::from_raw(9830i32)), n488);
    let n1482: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n1481);
    let n1483: ZB = zn_gt(n488, zn_splat(P8::from_raw(-65536i32)));
    let n1484: ZB = zn_le(n488, zn_splat(P8::from_raw(-65536i32)));
    let n1485: ZB = zb_and(n1473, n1483);
    let n1486: ZB = zb_and(n1473, n1484);
    let n1487: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1479);
    let n1488: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n1481);
    let n1489: ZB = zb_and(n845, n1474);
    let n1490: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1479);
    let n1491: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n1481);
    let n1492: ZN = zsel_n(n1483, n1487, n1488);
    let n1493: ZB = zb_or(n1485, n1486);
    let n1494: ZN = zsel_n(n1469, n1490, n1491);
    let n1495: ZN = zsel_n(n1475, n1480, n1482);
    let n1496: ZB = zb_or(n1477, n1478);
    let n1497: ZN = zsel_n(n1472, n1492, n1494);
    let n1498: ZB = zb_or(n1489, n1493);
    let n1499: ZN = zsel_n(n1469, n1495, n1497);
    let n1500: ZB = zb_or(n1496, n1498);
    let n1501: ZB = zb_and(n1468, n1469);
    let n1502: ZB = zb_and(n845, n1468);
    let n1503: ZN = zn_sub(n488, n1462);
    let n1504: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1503);
    let n1505: ZN = zn_add(n488, n1462);
    let n1506: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n1505);
    let n1507: ZN = zsel_n(n1469, n1504, n1506);
    let n1508: ZB = zb_or(n1501, n1502);
    let n1509: ZN = zsel_n(n1465, n1499, n1507);
    let n1510: ZB = zb_or(n1500, n1508);
    let n1511: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1509);
    let n1512: ZB = zb_not(n1511);
    let n1513: ZB = zb_and(n1510, n1512);
    let n1514: ZB = zb_and(n1510, n1511);
    let n1515: ZB = zn_lt(n1509, zn_splat(P8::from_raw(0i32)));
    let n1516: ZB = zsel_b(n1512, n1515, r_c361);
    let n1517: ZB = zb_or(n1513, n1514);
    let n1518: ZN = zn_abs(n766);
    let n1519: ZB = zn_le(n1518, zn_splat(P8::from_raw(9830i32)));
    let n1520: ZB = zn_gt(n1518, zn_splat(P8::from_raw(9830i32)));
    let n1521: ZB = zb_and(n1517, n1519);
    let n1522: ZB = zb_and(n1517, n1520);
    let n1523: ZN = zsel_n(n1519, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(13762i32)));
    let n1524: ZB = zb_or(n1521, n1522);
    let n1525: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n769);
    let n1526: ZB = zb_and(n1416, n1524);
    let n1527: ZB = zb_and(n1415, n1524);
    let n1528: ZN = zn_add(n766, n1523);
    let n1529: ZN = zn_min(zn_splat(P8::from_raw(131072i32)), n1528);
    let n1530: ZN = zsel_n(n1416, n1529, n766);
    let n1531: ZB = zb_or(n1526, n1527);
    let n1532: ZB = zn_gt(n1432, zn_splat(P8::from_raw(0i32)));
    let n1533: ZB = zn_le(n1432, zn_splat(P8::from_raw(0i32)));
    let n1534: ZN = zn_add(zn_splat(P8::from_raw(-196608i32)), n517);
    let n1535: ZB = zn_tile_flag_at(g.cache, g.cart, n1534, n1525, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1536: ZB = zb_not(n1535);
    let n1537: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n517);
    let n1538: ZB = zn_tile_flag_at(g.cache, g.cart, n1537, n1525, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1539: ZB = zb_not(n1538);
    let n1540: ZN = zsel_n(n1538, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n1541: ZN = zsel_n(n1535, zn_splat(P8::from_raw(-65536i32)), n1540);
    let n1542: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1541);
    let n1543: ZB = zb_not(n1542);
    let n1544: ZN = zn_neg(n1541);
    let n1545: ZN = zn_mul(n1544, zn_splat(P8::from_raw(131072i32)));
    let n1546: ZN = zsel_n(n1543, n1545, n1509);
    let n1547: ZN = zsel_n(n1543, zn_splat(P8::from_raw(-131072i32)), n1530);
    let n1548: ZN = zsel_n(n1532, zn_splat(P8::from_raw(0i32)), n1432);
    let n1549: ZN = zsel_n(n1532, n1509, n1546);
    let n1550: ZN = zsel_n(n1532, zn_splat(P8::from_raw(-131072i32)), n1547);
    let n1551: ZB = zb_not(n1516);
    let n1552: ZN = zsel_n(n1516, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n1553: ZB = zn_gt(n1552, zn_splat(P8::from_raw(0i32)));
    let n1554: ZB = zn_le(n1552, zn_splat(P8::from_raw(0i32)));
    let n1555: ZB = zn_lt(n1552, zn_splat(P8::from_raw(0i32)));
    let n1556: ZB = zn_ge(n1552, zn_splat(P8::from_raw(0i32)));
    let n1557: ZN = zsel_n(n1555, zn_splat(P8::from_raw(-131072i32)), zn_splat(P8::from_raw(0i32)));
    let n1558: ZN = zsel_n(n1553, zn_splat(P8::from_raw(131072i32)), n1557);
    let n1559: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1552);
    let n1560: ZB = zb_not(n1559);
    let n1561: ZN = zsel_n(n1560, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n1562: ZN = zsel_n(n1435, n1439, r_c283);
    let n1563: ZB = zsel_b(n1435, r_c361, n1516);
    let n1564: ZN = zsel_n(n1435, n1448, n1509);
    let n1565: ZN = zsel_n(n1435, n1458, n1530);
    let n1566: ZB = zb_or(n1459, n1531);
    let n1567: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n1568: ZB = zb_and(n134, n1566);
    let n1569: ZB = zn_lt(n486, zn_splat(P8::from_raw(-65536i32)));
    let n1570: ZB = zn_ge(n486, zn_splat(P8::from_raw(-65536i32)));
    let n1571: ZB = zb_and(n1568, n1570);
    let n1572: ZB = zb_and(n1568, n1569);
    let n1573: ZB = zn_gt(n486, zn_splat(P8::from_raw(7929856i32)));
    let n1574: ZB = zb_or(n1571, n1572);
    let n1575: ZB = zb_or(n1569, n1573);
    let n1576: ZB = zb_not(n1575);
    let n1577: ZB = zb_and(n1574, n1575);
    let n1578: ZB = zb_and(n1574, n1576);
    let n1579: ZN = zn_min(zn_splat(P8::from_raw(7929856i32)), n486);
    let n1580: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1579);
    let n1581: ZN = zsel_n(n1575, n1580, n486);
    let n1582: ZN = zsel_n(n1575, zn_splat(P8::from_raw(0i32)), n1564);
    let n1583: ZB = zb_or(n1577, n1578);
    let n1584: ZN = zsel_n(n1567, n486, n1581);
    let n1585: ZN = zsel_n(n1567, n1564, n1582);
    let n1598: ZB = zb_and(n1468, n1483);
    let n1599: ZB = zb_and(n1468, n1484);
    let n1600: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1503);
    let n1601: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n1505);
    let n1602: ZN = zsel_n(n1483, n1600, n1601);
    let n1603: ZB = zb_or(n1598, n1599);
    let n1604: ZN = zsel_n(n1465, n1499, n1602);
    let n1605: ZB = zb_or(n1500, n1603);
    let n1606: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1604);
    let n1607: ZB = zb_not(n1606);
    let n1608: ZB = zb_and(n1605, n1607);
    let n1609: ZB = zb_and(n1605, n1606);
    let n1610: ZB = zn_lt(n1604, zn_splat(P8::from_raw(0i32)));
    let n1611: ZB = zsel_b(n1607, n1610, r_c361);
    let n1612: ZB = zb_or(n1608, n1609);
    let n1613: ZB = zb_and(n1519, n1612);
    let n1614: ZB = zb_and(n1520, n1612);
    let n1615: ZB = zb_or(n1613, n1614);
    let n1616: ZN = zn_add(zn_splat(P8::from_raw(-65536i32)), n517);
    let n1617: ZB = zn_tile_flag_at(g.cache, g.cart, n1616, n1525, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1618: ZB = zb_not(n1617);
    let n1619: ZB = zb_and(n1615, n1618);
    let n1620: ZB = zb_and(n1615, n1617);
    let n1621: ZB = zb_or(n1619, n1620);
    let n1622: ZB = zb_and(n1618, n1621);
    let n1623: ZB = zb_and(n1617, n1621);
    let n1624: ZB = zb_or(n1622, n1623);
    let n1625: ZB = zb_and(n1617, n1624);
    let n1626: ZB = zb_and(n1618, n1624);
    let n1627: ZB = zb_or(n1625, n1626);
    let n1628: ZB = zb_and(n1617, n1627);
    let n1629: ZB = zb_and(n1618, n1627);
    let n1630: ZN = zsel_n(n1617, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n1631: ZB = zb_or(n1628, n1629);
    let n1632: ZB = zb_and(n1416, n1631);
    let n1633: ZB = zb_and(n1415, n1631);
    let n1634: ZN = zn_min(n1528, n1630);
    let n1635: ZN = zsel_n(n1416, n1634, n766);
    let n1636: ZB = zb_or(n1632, n1633);
    let n1637: ZN = zsel_n(n1543, n1545, n1604);
    let n1638: ZN = zsel_n(n1543, zn_splat(P8::from_raw(-131072i32)), n1635);
    let n1639: ZN = zsel_n(n1532, n1604, n1637);
    let n1640: ZN = zsel_n(n1532, zn_splat(P8::from_raw(-131072i32)), n1638);
    let n1641: ZB = zsel_b(n1435, r_c361, n1611);
    let n1642: ZN = zsel_n(n1435, n1448, n1604);
    let n1643: ZN = zsel_n(n1435, n1458, n1635);
    let n1644: ZB = zb_or(n1459, n1636);
    let n1645: ZB = zb_and(n134, n1644);
    let n1646: ZB = zb_and(n1570, n1645);
    let n1647: ZB = zb_and(n1569, n1645);
    let n1648: ZB = zb_or(n1646, n1647);
    let n1649: ZB = zb_and(n1575, n1648);
    let n1650: ZB = zb_and(n1576, n1648);
    let n1651: ZN = zsel_n(n1575, zn_splat(P8::from_raw(0i32)), n1642);
    let n1652: ZB = zb_or(n1649, n1650);
    let n1653: ZN = zsel_n(n1567, n1642, n1651);
    let n1654: ZB = zb_and(n1468, n1475);
    let n1655: ZB = zb_and(n1468, n1476);
    let n1656: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n1503);
    let n1657: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n1505);
    let n1658: ZN = zsel_n(n1475, n1656, n1657);
    let n1659: ZB = zb_or(n1654, n1655);
    let n1660: ZN = zsel_n(n1465, n1499, n1658);
    let n1661: ZB = zb_or(n1500, n1659);
    let n1662: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1660);
    let n1663: ZB = zb_not(n1662);
    let n1664: ZB = zb_and(n1661, n1663);
    let n1665: ZB = zb_and(n1661, n1662);
    let n1666: ZB = zn_lt(n1660, zn_splat(P8::from_raw(0i32)));
    let n1667: ZB = zsel_b(n1663, n1666, r_c361);
    let n1668: ZB = zb_or(n1664, n1665);
    let n1669: ZB = zb_and(n1519, n1668);
    let n1670: ZB = zb_and(n1520, n1668);
    let n1671: ZB = zb_or(n1669, n1670);
    let n1672: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n517);
    let n1673: ZB = zn_tile_flag_at(g.cache, g.cart, n1672, n1525, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1674: ZB = zb_not(n1673);
    let n1675: ZB = zb_and(n1671, n1674);
    let n1676: ZB = zb_and(n1671, n1673);
    let n1677: ZB = zb_or(n1675, n1676);
    let n1678: ZB = zb_and(n1674, n1677);
    let n1679: ZB = zb_and(n1673, n1677);
    let n1680: ZB = zb_or(n1678, n1679);
    let n1681: ZB = zb_and(n1673, n1680);
    let n1682: ZB = zb_and(n1674, n1680);
    let n1683: ZB = zb_or(n1681, n1682);
    let n1684: ZB = zb_and(n1673, n1683);
    let n1685: ZB = zb_and(n1674, n1683);
    let n1686: ZN = zsel_n(n1673, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n1687: ZB = zb_or(n1684, n1685);
    let n1688: ZB = zb_and(n1416, n1687);
    let n1689: ZB = zb_and(n1415, n1687);
    let n1690: ZN = zn_min(n1528, n1686);
    let n1691: ZN = zsel_n(n1416, n1690, n766);
    let n1692: ZB = zb_or(n1688, n1689);
    let n1693: ZN = zsel_n(n1543, n1545, n1660);
    let n1694: ZN = zsel_n(n1543, zn_splat(P8::from_raw(-131072i32)), n1691);
    let n1695: ZN = zsel_n(n1532, n1660, n1693);
    let n1696: ZN = zsel_n(n1532, zn_splat(P8::from_raw(-131072i32)), n1694);
    let n1697: ZB = zsel_b(n1435, r_c361, n1667);
    let n1698: ZN = zsel_n(n1435, n1448, n1660);
    let n1699: ZN = zsel_n(n1435, n1458, n1691);
    let n1700: ZB = zb_or(n1459, n1692);
    let n1701: ZB = zb_and(n134, n1700);
    let n1702: ZB = zb_and(n1570, n1701);
    let n1703: ZB = zb_and(n1569, n1701);
    let n1704: ZB = zb_or(n1702, n1703);
    let n1705: ZB = zb_and(n1575, n1704);
    let n1706: ZB = zb_and(n1576, n1704);
    let n1707: ZN = zsel_n(n1575, zn_splat(P8::from_raw(0i32)), n1698);
    let n1708: ZB = zb_or(n1705, n1706);
    let n1709: ZN = zsel_n(n1567, n1698, n1707);
    let n1710: ZB = zb_and(n140, n1531);
    let n1711: ZB = zb_and(r_c294, n1531);
    let n1712: ZB = zb_and(n1532, n1710);
    let n1713: ZB = zb_and(n1533, n1710);
    let n1714: ZB = zb_and(n1536, n1713);
    let n1715: ZB = zb_and(n1535, n1713);
    let n1716: ZB = zb_or(n1714, n1715);
    let n1717: ZB = zb_and(n1536, n1716);
    let n1718: ZB = zb_and(n1535, n1716);
    let n1719: ZB = zb_or(n1717, n1718);
    let n1720: ZB = zb_and(n1535, n1719);
    let n1721: ZB = zb_and(n1536, n1719);
    let n1722: ZB = zb_and(n1539, n1721);
    let n1723: ZB = zb_and(n1538, n1721);
    let n1724: ZB = zb_or(n1722, n1723);
    let n1725: ZB = zb_and(n1539, n1724);
    let n1726: ZB = zb_and(n1538, n1724);
    let n1727: ZB = zb_or(n1725, n1726);
    let n1728: ZB = zb_and(n1538, n1727);
    let n1729: ZB = zb_and(n1539, n1727);
    let n1730: ZB = zb_or(n1728, n1729);
    let n1731: ZB = zb_or(n1720, n1730);
    let n1732: ZB = zb_and(n1543, n1731);
    let n1733: ZB = zb_and(n1542, n1731);
    let n1734: ZB = zb_or(n1732, n1733);
    let n1735: ZB = zb_or(n1712, n1734);
    let n1736: ZN = zsel_n(n140, n1548, n1432);
    let n1737: ZN = zsel_n(n140, n1549, n1509);
    let n1738: ZN = zsel_n(n140, n1550, n1530);
    let n1739: ZB = zb_or(n1711, n1735);
    let n1740: ZN = zsel_n(n1435, n1432, n1736);
    let n1741: ZN = zsel_n(n1435, n1448, n1737);
    let n1742: ZN = zsel_n(n1435, n1458, n1738);
    let n1743: ZB = zb_or(n1459, n1739);
    let n1744: ZB = zb_and(n134, n1743);
    let n1745: ZB = zb_and(n1570, n1744);
    let n1746: ZB = zb_and(n1569, n1744);
    let n1747: ZB = zb_or(n1745, n1746);
    let n1748: ZB = zb_and(n1575, n1747);
    let n1749: ZB = zb_and(n1576, n1747);
    let n1750: ZN = zsel_n(n1575, zn_splat(P8::from_raw(0i32)), n1741);
    let n1751: ZB = zb_or(n1748, n1749);
    let n1752: ZN = zsel_n(n1567, n1741, n1750);
    let n1753: ZB = zb_and(n140, n1636);
    let n1754: ZB = zb_and(r_c294, n1636);
    let n1755: ZB = zb_and(n1532, n1753);
    let n1756: ZB = zb_and(n1533, n1753);
    let n1757: ZB = zb_and(n1536, n1756);
    let n1758: ZB = zb_and(n1535, n1756);
    let n1759: ZB = zb_or(n1757, n1758);
    let n1760: ZB = zb_and(n1536, n1759);
    let n1761: ZB = zb_and(n1535, n1759);
    let n1762: ZB = zb_or(n1760, n1761);
    let n1763: ZB = zb_and(n1535, n1762);
    let n1764: ZB = zb_and(n1536, n1762);
    let n1765: ZB = zb_and(n1539, n1764);
    let n1766: ZB = zb_and(n1538, n1764);
    let n1767: ZB = zb_or(n1765, n1766);
    let n1768: ZB = zb_and(n1539, n1767);
    let n1769: ZB = zb_and(n1538, n1767);
    let n1770: ZB = zb_or(n1768, n1769);
    let n1771: ZB = zb_and(n1538, n1770);
    let n1772: ZB = zb_and(n1539, n1770);
    let n1773: ZB = zb_or(n1771, n1772);
    let n1774: ZB = zb_or(n1763, n1773);
    let n1775: ZB = zb_and(n1543, n1774);
    let n1776: ZB = zb_and(n1542, n1774);
    let n1777: ZB = zb_or(n1775, n1776);
    let n1778: ZB = zb_or(n1755, n1777);
    let n1779: ZN = zsel_n(n140, n1639, n1604);
    let n1780: ZN = zsel_n(n140, n1640, n1635);
    let n1781: ZB = zb_or(n1754, n1778);
    let n1782: ZN = zsel_n(n1435, n1448, n1779);
    let n1783: ZN = zsel_n(n1435, n1458, n1780);
    let n1784: ZB = zb_or(n1459, n1781);
    let n1785: ZB = zb_and(n134, n1784);
    let n1786: ZB = zb_and(n1570, n1785);
    let n1787: ZB = zb_and(n1569, n1785);
    let n1788: ZB = zb_or(n1786, n1787);
    let n1789: ZB = zb_and(n1575, n1788);
    let n1790: ZB = zb_and(n1576, n1788);
    let n1791: ZN = zsel_n(n1575, zn_splat(P8::from_raw(0i32)), n1782);
    let n1792: ZB = zb_or(n1789, n1790);
    let n1793: ZN = zsel_n(n1567, n1782, n1791);
    let n1794: ZB = zb_and(n140, n1692);
    let n1795: ZB = zb_and(r_c294, n1692);
    let n1796: ZB = zb_and(n1532, n1794);
    let n1797: ZB = zb_and(n1533, n1794);
    let n1798: ZB = zb_and(n1536, n1797);
    let n1799: ZB = zb_and(n1535, n1797);
    let n1800: ZB = zb_or(n1798, n1799);
    let n1801: ZB = zb_and(n1536, n1800);
    let n1802: ZB = zb_and(n1535, n1800);
    let n1803: ZB = zb_or(n1801, n1802);
    let n1804: ZB = zb_and(n1535, n1803);
    let n1805: ZB = zb_and(n1536, n1803);
    let n1806: ZB = zb_and(n1539, n1805);
    let n1807: ZB = zb_and(n1538, n1805);
    let n1808: ZB = zb_or(n1806, n1807);
    let n1809: ZB = zb_and(n1539, n1808);
    let n1810: ZB = zb_and(n1538, n1808);
    let n1811: ZB = zb_or(n1809, n1810);
    let n1812: ZB = zb_and(n1538, n1811);
    let n1813: ZB = zb_and(n1539, n1811);
    let n1814: ZB = zb_or(n1812, n1813);
    let n1815: ZB = zb_or(n1804, n1814);
    let n1816: ZB = zb_and(n1543, n1815);
    let n1817: ZB = zb_and(n1542, n1815);
    let n1818: ZB = zb_or(n1816, n1817);
    let n1819: ZB = zb_or(n1796, n1818);
    let n1820: ZN = zsel_n(n140, n1695, n1660);
    let n1821: ZN = zsel_n(n140, n1696, n1691);
    let n1822: ZB = zb_or(n1795, n1819);
    let n1823: ZN = zsel_n(n1435, n1448, n1820);
    let n1824: ZN = zsel_n(n1435, n1458, n1821);
    let n1825: ZB = zb_or(n1459, n1822);
    let n1826: ZB = zb_and(n134, n1825);
    let n1827: ZB = zb_and(n1570, n1826);
    let n1828: ZB = zb_and(n1569, n1826);
    let n1829: ZB = zb_or(n1827, n1828);
    let n1830: ZB = zb_and(n1575, n1829);
    let n1831: ZB = zb_and(n1576, n1829);
    let n1832: ZN = zsel_n(n1575, zn_splat(P8::from_raw(0i32)), n1823);
    let n1833: ZB = zb_or(n1830, n1831);
    let n1834: ZN = zsel_n(n1567, n1823, n1832);
    let n1835: ZB = zb_and(n133, n1531);
    let n1836: ZB = zb_and(r_c293, n1531);
    let n1837: ZB = zb_and(n1516, n1835);
    let n1838: ZB = zb_and(n1551, n1835);
    let n1839: ZB = zb_or(n1837, n1838);
    let n1840: ZB = zb_and(n1553, n1839);
    let n1841: ZB = zb_and(n1554, n1839);
    let n1842: ZB = zb_and(n1555, n1841);
    let n1843: ZB = zb_and(n1556, n1841);
    let n1844: ZB = zb_or(n1842, n1843);
    let n1845: ZB = zb_or(n1840, n1844);
    let n1846: ZB = zb_and(n1560, n1845);
    let n1847: ZB = zb_and(n1559, n1845);
    let n1848: ZB = zb_or(n1846, n1847);
    let n1849: ZN = zsel_n(n133, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n1850: ZB = zb_or(r_c41, n133);
    let n1851: ZN = zsel_n(n133, zn_splat(P8::from_raw(655360i32)), n1434);
    let n1852: ZN = zsel_n(n133, zn_splat(P8::from_raw(262144i32)), r_c283);
    let n1853: ZN = zsel_n(n133, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n1854: ZN = zsel_n(n133, zn_splat(P8::from_raw(98304i32)), r_c357);
    let n1855: ZN = zsel_n(n133, n1561, r_c358);
    let n1856: ZN = zsel_n(n133, n1558, r_c359);
    let n1857: ZN = zsel_n(n133, zn_splat(P8::from_raw(0i32)), r_c360);
    let n1858: ZN = zsel_n(n133, n1552, n1509);
    let n1859: ZN = zsel_n(n133, zn_splat(P8::from_raw(0i32)), n1530);
    let n1860: ZB = zb_or(n1836, n1848);
    let n1861: ZN = zsel_n(n1435, r_c20, n1849);
    let n1862: ZB = zsel_b(n1435, r_c41, n1850);
    let n1863: ZN = zsel_n(n1435, n1434, n1851);
    let n1864: ZN = zsel_n(n1435, n1439, n1852);
    let n1865: ZN = zsel_n(n1435, zn_splat(P8::from_raw(65536i32)), n1853);
    let n1866: ZN = zsel_n(n1435, r_c357, n1854);
    let n1867: ZN = zsel_n(n1435, r_c358, n1855);
    let n1868: ZN = zsel_n(n1435, r_c359, n1856);
    let n1869: ZN = zsel_n(n1435, r_c360, n1857);
    let n1870: ZN = zsel_n(n1435, n1448, n1858);
    let n1871: ZN = zsel_n(n1435, n1458, n1859);
    let n1872: ZB = zb_or(n1459, n1860);
    let n1873: ZB = zn_gt(n1861, zn_splat(P8::from_raw(0i32)));
    let n1874: ZB = zn_le(n1861, zn_splat(P8::from_raw(0i32)));
    let n1875: ZB = zb_and(n1872, n1873);
    let n1876: ZB = zb_and(n1872, n1874);
    let n1877: ZB = zb_and(n1570, n1876);
    let n1878: ZB = zb_and(n1569, n1876);
    let n1879: ZB = zb_or(n1877, n1878);
    let n1880: ZB = zb_and(n1575, n1879);
    let n1881: ZB = zb_and(n1576, n1879);
    let n1882: ZN = zsel_n(n1575, zn_splat(P8::from_raw(0i32)), n1870);
    let n1883: ZB = zb_or(n1880, n1881);
    let n1884: ZN = zsel_n(n1873, n486, n1581);
    let n1885: ZN = zsel_n(n1873, n1870, n1882);
    let n1886: ZB = zb_or(n1875, n1883);
    let n1887: ZB = zb_and(n133, n1636);
    let n1888: ZB = zb_and(r_c293, n1636);
    let n1889: ZN = zsel_n(n133, zn_splat(P8::from_raw(69510i32)), r_c358);
    let n1890: ZN = zsel_n(n133, zn_splat(P8::from_raw(-131072i32)), r_c359);
    let n1891: ZN = zsel_n(n133, zn_splat(P8::from_raw(-327680i32)), n1604);
    let n1892: ZN = zsel_n(n133, zn_splat(P8::from_raw(0i32)), n1635);
    let n1893: ZB = zb_or(n1887, n1888);
    let n1894: ZN = zsel_n(n1435, r_c358, n1889);
    let n1895: ZN = zsel_n(n1435, r_c359, n1890);
    let n1896: ZN = zsel_n(n1435, n1448, n1891);
    let n1897: ZN = zsel_n(n1435, n1458, n1892);
    let n1898: ZB = zb_or(n1459, n1893);
    let n1899: ZB = zb_and(n1873, n1898);
    let n1900: ZB = zb_and(n1874, n1898);
    let n1901: ZB = zb_and(n1570, n1900);
    let n1902: ZB = zb_and(n1569, n1900);
    let n1903: ZB = zb_or(n1901, n1902);
    let n1904: ZB = zb_and(n1575, n1903);
    let n1905: ZB = zb_and(n1576, n1903);
    let n1906: ZN = zsel_n(n1575, zn_splat(P8::from_raw(0i32)), n1896);
    let n1907: ZB = zb_or(n1904, n1905);
    let n1908: ZN = zsel_n(n1873, n1896, n1906);
    let n1909: ZB = zb_or(n1899, n1907);
    let n1910: ZB = zb_and(n133, n1692);
    let n1911: ZB = zb_and(r_c293, n1692);
    let n1912: ZN = zsel_n(n133, zn_splat(P8::from_raw(131072i32)), r_c359);
    let n1913: ZN = zsel_n(n133, zn_splat(P8::from_raw(327680i32)), n1660);
    let n1914: ZN = zsel_n(n133, zn_splat(P8::from_raw(0i32)), n1691);
    let n1915: ZB = zb_or(n1910, n1911);
    let n1916: ZN = zsel_n(n1435, r_c359, n1912);
    let n1917: ZN = zsel_n(n1435, n1448, n1913);
    let n1918: ZN = zsel_n(n1435, n1458, n1914);
    let n1919: ZB = zb_or(n1459, n1915);
    let n1920: ZB = zb_and(n1873, n1919);
    let n1921: ZB = zb_and(n1874, n1919);
    let n1922: ZB = zb_and(n1570, n1921);
    let n1923: ZB = zb_and(n1569, n1921);
    let n1924: ZB = zb_or(n1922, n1923);
    let n1925: ZB = zb_and(n1575, n1924);
    let n1926: ZB = zb_and(n1576, n1924);
    let n1927: ZN = zsel_n(n1575, zn_splat(P8::from_raw(0i32)), n1917);
    let n1928: ZB = zb_or(n1925, n1926);
    let n1929: ZN = zsel_n(n1873, n1917, n1927);
    let n1930: ZB = zb_or(n1920, n1928);
    let n1932: ZN = zsel_n(n133, zn_splat(P8::from_raw(69510i32)), r_c357);
    let n1933: ZN = zsel_n(n133, zn_splat(P8::from_raw(98304i32)), r_c358);
    let n1934: ZN = zsel_n(n133, zn_splat(P8::from_raw(0i32)), r_c359);
    let n1935: ZN = zsel_n(n133, zn_splat(P8::from_raw(-98304i32)), r_c360);
    let n1936: ZN = zsel_n(n133, zn_splat(P8::from_raw(0i32)), n1509);
    let n1937: ZN = zsel_n(n133, zn_splat(P8::from_raw(-327680i32)), n1530);
    let n1938: ZB = zb_or(n1835, n1836);
    let n1939: ZN = zsel_n(n1435, r_c357, n1932);
    let n1940: ZN = zsel_n(n1435, r_c358, n1933);
    let n1941: ZN = zsel_n(n1435, r_c359, n1934);
    let n1942: ZN = zsel_n(n1435, r_c360, n1935);
    let n1943: ZN = zsel_n(n1435, n1448, n1936);
    let n1944: ZN = zsel_n(n1435, n1458, n1937);
    let n1945: ZB = zb_or(n1459, n1938);
    let n1946: ZB = zb_and(n1873, n1945);
    let n1947: ZB = zb_and(n1874, n1945);
    let n1948: ZB = zb_and(n1570, n1947);
    let n1949: ZB = zb_and(n1569, n1947);
    let n1950: ZB = zb_or(n1948, n1949);
    let n1951: ZB = zb_and(n1575, n1950);
    let n1952: ZB = zb_and(n1576, n1950);
    let n1953: ZN = zsel_n(n1575, zn_splat(P8::from_raw(0i32)), n1943);
    let n1954: ZB = zb_or(n1951, n1952);
    let n1955: ZN = zsel_n(n1873, n1943, n1953);
    let n1956: ZB = zb_or(n1946, n1954);
    let n1957: ZN = zsel_n(n133, zn_splat(P8::from_raw(-231700i32)), n1604);
    let n1958: ZN = zsel_n(n133, zn_splat(P8::from_raw(-231700i32)), n1635);
    let n1959: ZN = zsel_n(n1435, n1448, n1957);
    let n1960: ZN = zsel_n(n1435, n1458, n1958);
    let n1961: ZN = zsel_n(n1575, zn_splat(P8::from_raw(0i32)), n1959);
    let n1962: ZN = zsel_n(n1873, n1959, n1961);
    let n1963: ZN = zsel_n(n133, zn_splat(P8::from_raw(231700i32)), n1660);
    let n1964: ZN = zsel_n(n133, zn_splat(P8::from_raw(-231700i32)), n1691);
    let n1965: ZN = zsel_n(n1435, n1448, n1963);
    let n1966: ZN = zsel_n(n1435, n1458, n1964);
    let n1967: ZN = zsel_n(n1575, zn_splat(P8::from_raw(0i32)), n1965);
    let n1968: ZN = zsel_n(n1873, n1965, n1967);
    let n1969: ZN = zsel_n(n133, zn_splat(P8::from_raw(131072i32)), r_c360);
    let n1970: ZN = zsel_n(n133, zn_splat(P8::from_raw(327680i32)), n1530);
    let n1971: ZN = zsel_n(n1435, r_c360, n1969);
    let n1972: ZN = zsel_n(n1435, n1458, n1970);
    let n1973: ZN = zsel_n(n133, zn_splat(P8::from_raw(231700i32)), n1635);
    let n1974: ZN = zsel_n(n1435, n1458, n1973);
    let n1975: ZN = zsel_n(n133, zn_splat(P8::from_raw(231700i32)), n1691);
    let n1976: ZN = zsel_n(n1435, n1458, n1975);
    let n1977: ZB = zb_and(n133, n1739);
    let n1978: ZB = zb_and(r_c293, n1739);
    let n1979: ZB = zb_and(n1516, n1977);
    let n1980: ZB = zb_and(n1551, n1977);
    let n1981: ZB = zb_or(n1979, n1980);
    let n1982: ZB = zb_and(n1553, n1981);
    let n1983: ZB = zb_and(n1554, n1981);
    let n1984: ZB = zb_and(n1555, n1983);
    let n1985: ZB = zb_and(n1556, n1983);
    let n1986: ZB = zb_or(n1984, n1985);
    let n1987: ZB = zb_or(n1982, n1986);
    let n1988: ZB = zb_and(n1560, n1987);
    let n1989: ZB = zb_and(n1559, n1987);
    let n1990: ZB = zb_or(n1988, n1989);
    let n1991: ZN = zsel_n(n133, n1552, n1737);
    let n1992: ZN = zsel_n(n133, zn_splat(P8::from_raw(0i32)), n1738);
    let n1993: ZB = zb_or(n1978, n1990);
    let n1994: ZN = zsel_n(n1435, n1448, n1991);
    let n1995: ZN = zsel_n(n1435, n1458, n1992);
    let n1996: ZB = zb_or(n1459, n1993);
    let n1997: ZB = zb_and(n1873, n1996);
    let n1998: ZB = zb_and(n1874, n1996);
    let n1999: ZB = zb_and(n1570, n1998);
    let n2000: ZB = zb_and(n1569, n1998);
    let n2001: ZB = zb_or(n1999, n2000);
    let n2002: ZB = zb_and(n1575, n2001);
    let n2003: ZB = zb_and(n1576, n2001);
    let n2004: ZN = zsel_n(n1575, zn_splat(P8::from_raw(0i32)), n1994);
    let n2005: ZB = zb_or(n2002, n2003);
    let n2006: ZN = zsel_n(n1873, n1994, n2004);
    let n2007: ZB = zb_or(n1997, n2005);
    let n2008: ZB = zb_and(n133, n1781);
    let n2009: ZB = zb_and(r_c293, n1781);
    let n2010: ZN = zsel_n(n133, zn_splat(P8::from_raw(-327680i32)), n1779);
    let n2011: ZN = zsel_n(n133, zn_splat(P8::from_raw(0i32)), n1780);
    let n2012: ZB = zb_or(n2008, n2009);
    let n2013: ZN = zsel_n(n1435, n1448, n2010);
    let n2014: ZN = zsel_n(n1435, n1458, n2011);
    let n2015: ZB = zb_or(n1459, n2012);
    let n2016: ZB = zb_and(n1873, n2015);
    let n2017: ZB = zb_and(n1874, n2015);
    let n2018: ZB = zb_and(n1570, n2017);
    let n2019: ZB = zb_and(n1569, n2017);
    let n2020: ZB = zb_or(n2018, n2019);
    let n2021: ZB = zb_and(n1575, n2020);
    let n2022: ZB = zb_and(n1576, n2020);
    let n2023: ZN = zsel_n(n1575, zn_splat(P8::from_raw(0i32)), n2013);
    let n2024: ZB = zb_or(n2021, n2022);
    let n2025: ZN = zsel_n(n1873, n2013, n2023);
    let n2026: ZB = zb_or(n2016, n2024);
    let n2027: ZB = zb_and(n133, n1822);
    let n2028: ZB = zb_and(r_c293, n1822);
    let n2029: ZN = zsel_n(n133, zn_splat(P8::from_raw(327680i32)), n1820);
    let n2030: ZN = zsel_n(n133, zn_splat(P8::from_raw(0i32)), n1821);
    let n2031: ZB = zb_or(n2027, n2028);
    let n2032: ZN = zsel_n(n1435, n1448, n2029);
    let n2033: ZN = zsel_n(n1435, n1458, n2030);
    let n2034: ZB = zb_or(n1459, n2031);
    let n2035: ZB = zb_and(n1873, n2034);
    let n2036: ZB = zb_and(n1874, n2034);
    let n2037: ZB = zb_and(n1570, n2036);
    let n2038: ZB = zb_and(n1569, n2036);
    let n2039: ZB = zb_or(n2037, n2038);
    let n2040: ZB = zb_and(n1575, n2039);
    let n2041: ZB = zb_and(n1576, n2039);
    let n2042: ZN = zsel_n(n1575, zn_splat(P8::from_raw(0i32)), n2032);
    let n2043: ZB = zb_or(n2040, n2041);
    let n2044: ZN = zsel_n(n1873, n2032, n2042);
    let n2045: ZB = zb_or(n2035, n2043);
    let n2046: ZN = zsel_n(n133, zn_splat(P8::from_raw(0i32)), n1737);
    let n2047: ZN = zsel_n(n133, zn_splat(P8::from_raw(-327680i32)), n1738);
    let n2048: ZB = zb_or(n1977, n1978);
    let n2049: ZN = zsel_n(n1435, n1448, n2046);
    let n2050: ZN = zsel_n(n1435, n1458, n2047);
    let n2051: ZB = zb_or(n1459, n2048);
    let n2052: ZB = zb_and(n1873, n2051);
    let n2053: ZB = zb_and(n1874, n2051);
    let n2054: ZB = zb_and(n1570, n2053);
    let n2055: ZB = zb_and(n1569, n2053);
    let n2056: ZB = zb_or(n2054, n2055);
    let n2057: ZB = zb_and(n1575, n2056);
    let n2058: ZB = zb_and(n1576, n2056);
    let n2059: ZN = zsel_n(n1575, zn_splat(P8::from_raw(0i32)), n2049);
    let n2060: ZB = zb_or(n2057, n2058);
    let n2061: ZN = zsel_n(n1873, n2049, n2059);
    let n2062: ZB = zb_or(n2052, n2060);
    let n2063: ZN = zsel_n(n133, zn_splat(P8::from_raw(-231700i32)), n1779);
    let n2064: ZN = zsel_n(n133, zn_splat(P8::from_raw(-231700i32)), n1780);
    let n2065: ZN = zsel_n(n1435, n1448, n2063);
    let n2066: ZN = zsel_n(n1435, n1458, n2064);
    let n2067: ZN = zsel_n(n1575, zn_splat(P8::from_raw(0i32)), n2065);
    let n2068: ZN = zsel_n(n1873, n2065, n2067);
    let n2069: ZN = zsel_n(n133, zn_splat(P8::from_raw(231700i32)), n1820);
    let n2070: ZN = zsel_n(n133, zn_splat(P8::from_raw(-231700i32)), n1821);
    let n2071: ZN = zsel_n(n1435, n1448, n2069);
    let n2072: ZN = zsel_n(n1435, n1458, n2070);
    let n2073: ZN = zsel_n(n1575, zn_splat(P8::from_raw(0i32)), n2071);
    let n2074: ZN = zsel_n(n1873, n2071, n2073);
    let n2075: ZN = zsel_n(n133, zn_splat(P8::from_raw(327680i32)), n1738);
    let n2076: ZN = zsel_n(n1435, n1458, n2075);
    let n2077: ZN = zsel_n(n133, zn_splat(P8::from_raw(231700i32)), n1780);
    let n2078: ZN = zsel_n(n1435, n1458, n2077);
    let n2079: ZN = zsel_n(n133, zn_splat(P8::from_raw(231700i32)), n1821);
    let n2080: ZN = zsel_n(n1435, n1458, n2079);
    let n2081: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c87);
    let n2082: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n2081);
    let n2083: ZB = zb_and(n817, n818);
    let n2084: ZB = zb_and(n828, n832);
    let n2085: ZB = zb_and(n846, n847);
    let n2086: ZB = zb_and(n870, n871);
    let n2087: ZB = zb_or(n2085, n2086);
    let n2088: ZB = zb_or(n2084, n2087);
    let n2089: ZB = zb_or(n2083, n2088);
    let n2090: ZB = zb_and(n897, n898);
    let n2091: ZB = zb_and(n906, n910);
    let n2092: ZB = zb_and(n921, n922);
    let n2093: ZB = zb_and(n937, n938);
    let n2094: ZB = zb_or(n2092, n2093);
    let n2095: ZB = zb_or(n2091, n2094);
    let n2096: ZB = zb_or(n2090, n2095);
    let n2097: ZB = zb_and(n964, n965);
    let n2098: ZB = zb_and(n973, n977);
    let n2099: ZB = zb_and(n988, n989);
    let n2100: ZB = zb_and(n1004, n1005);
    let n2101: ZB = zb_or(n2099, n2100);
    let n2102: ZB = zb_or(n2098, n2101);
    let n2103: ZB = zb_or(n2097, n2102);
    let n2104: ZB = zb_or(n2096, n2103);
    let n2105: ZB = zb_or(n2089, n2104);
    let n2106: ZB = zb_and(n1038, n1039);
    let n2107: ZB = zb_and(n1047, n1051);
    let n2108: ZB = zb_and(n1062, n1063);
    let n2109: ZB = zb_and(n1082, n1083);
    let n2110: ZB = zb_or(n2108, n2109);
    let n2111: ZB = zb_or(n2107, n2110);
    let n2112: ZB = zb_or(n2106, n2111);
    let n2113: ZB = zb_and(n1101, n1102);
    let n2114: ZB = zb_and(n1110, n1114);
    let n2115: ZB = zb_and(n1125, n1126);
    let n2116: ZB = zb_and(n1141, n1142);
    let n2117: ZB = zb_or(n2115, n2116);
    let n2118: ZB = zb_or(n2114, n2117);
    let n2119: ZB = zb_or(n2113, n2118);
    let n2120: ZB = zb_and(n1160, n1161);
    let n2121: ZB = zb_and(n1169, n1173);
    let n2122: ZB = zb_and(n1184, n1185);
    let n2123: ZB = zb_and(n1200, n1201);
    let n2124: ZB = zb_or(n2122, n2123);
    let n2125: ZB = zb_or(n2121, n2124);
    let n2126: ZB = zb_or(n2120, n2125);
    let n2127: ZB = zb_or(n2119, n2126);
    let n2128: ZB = zb_or(n2112, n2127);
    let n2129: ZB = zb_and(n1232, n1233);
    let n2130: ZB = zb_and(n1241, n1245);
    let n2131: ZB = zb_and(n1256, n1257);
    let n2132: ZB = zb_and(n1276, n1277);
    let n2133: ZB = zb_or(n2131, n2132);
    let n2134: ZB = zb_or(n2130, n2133);
    let n2135: ZB = zb_or(n2129, n2134);
    let n2136: ZB = zb_and(n1295, n1296);
    let n2137: ZB = zb_and(n1304, n1308);
    let n2138: ZB = zb_and(n1319, n1320);
    let n2139: ZB = zb_and(n1335, n1336);
    let n2140: ZB = zb_or(n2138, n2139);
    let n2141: ZB = zb_or(n2137, n2140);
    let n2142: ZB = zb_or(n2136, n2141);
    let n2143: ZB = zb_and(n1354, n1355);
    let n2144: ZB = zb_and(n1363, n1367);
    let n2145: ZB = zb_and(n1378, n1379);
    let n2146: ZB = zb_and(n1394, n1395);
    let n2147: ZB = zb_or(n2145, n2146);
    let n2148: ZB = zb_or(n2144, n2147);
    let n2149: ZB = zb_or(n2143, n2148);
    let n2150: ZB = zb_or(n2142, n2149);
    let n2151: ZB = zb_or(n2135, n2150);
    let n2152: ZB = zb_or(n2128, n2151);
    let n2153: ZB = zsel_b(n2128, n1016, n1210);
    let n2154: ZB = zb_or(n2105, n2152);
    let n2155: ZB = zsel_b(n2105, n768, n2153);
    let n2156: ZB = zsel_b(n2154, n2155, n1413);
    let n2157: ZB = zb_and(n1416, n2154);
    let n2158: ZB = zb_and(n1415, n2154);
    let n2159: ZB = zb_or(n2157, n2158);
    let n2160: ZB = zb_and(n1416, n2159);
    let n2161: ZB = zb_and(n1415, n2159);
    let n2162: ZB = zb_or(n2160, n2161);
    let n2163: ZB = zb_and(n1415, n2162);
    let n2164: ZB = zb_and(n1416, n2162);
    let n2165: ZB = zb_and(n1425, n2164);
    let n2166: ZB = zb_and(n1426, n2164);
    let n2167: ZB = zb_or(n2165, n2166);
    let n2168: ZB = zb_or(n2163, n2167);
    let n2169: ZB = zb_and(n1435, n2168);
    let n2170: ZB = zb_and(n1436, n2168);
    let n2171: ZB = zb_and(n1440, n2169);
    let n2172: ZB = zb_and(n1441, n2169);
    let n2173: ZB = zb_or(n2171, n2172);
    let n2174: ZB = zb_and(n1450, n2173);
    let n2175: ZB = zb_and(n1451, n2173);
    let n2176: ZB = zb_or(n2174, n2175);
    let n2177: ZB = zb_and(n1416, n2170);
    let n2178: ZB = zb_and(n1415, n2170);
    let n2179: ZB = zb_or(n2177, n2178);
    let n2180: ZB = zb_and(n1465, n2179);
    let n2181: ZB = zb_and(n1466, n2179);
    let n2182: ZB = zb_and(n1469, n2180);
    let n2183: ZB = zb_and(n845, n2180);
    let n2184: ZB = zb_and(n1472, n2183);
    let n2185: ZB = zb_and(n869, n2183);
    let n2186: ZB = zb_and(n1475, n2182);
    let n2187: ZB = zb_and(n1476, n2182);
    let n2188: ZB = zb_and(n1483, n2184);
    let n2189: ZB = zb_and(n1484, n2184);
    let n2190: ZB = zb_and(n845, n2185);
    let n2191: ZB = zb_or(n2188, n2189);
    let n2192: ZB = zb_or(n2186, n2187);
    let n2193: ZB = zb_or(n2190, n2191);
    let n2194: ZB = zb_or(n2192, n2193);
    let n2195: ZB = zb_and(n1469, n2181);
    let n2196: ZB = zb_and(n845, n2181);
    let n2197: ZB = zb_or(n2195, n2196);
    let n2198: ZB = zb_or(n2194, n2197);
    let n2199: ZB = zb_and(n1512, n2198);
    let n2200: ZB = zb_and(n1511, n2198);
    let n2201: ZB = zb_or(n2199, n2200);
    let n2202: ZB = zb_and(n1519, n2201);
    let n2203: ZB = zb_and(n1520, n2201);
    let n2204: ZB = zb_or(n2202, n2203);
    let n2205: ZB = zb_and(n1416, n2204);
    let n2206: ZB = zb_and(n1415, n2204);
    let n2207: ZB = zb_or(n2205, n2206);
    let n2208: ZB = zb_or(n2176, n2207);
    let n2211: ZB = zb_and(n1483, n2181);
    let n2212: ZB = zb_and(n1484, n2181);
    let n2213: ZB = zb_or(n2211, n2212);
    let n2214: ZB = zb_or(n2194, n2213);
    let n2215: ZB = zb_and(n1607, n2214);
    let n2216: ZB = zb_and(n1606, n2214);
    let n2217: ZB = zb_or(n2215, n2216);
    let n2218: ZB = zb_and(n1519, n2217);
    let n2219: ZB = zb_and(n1520, n2217);
    let n2220: ZB = zb_or(n2218, n2219);
    let n2221: ZB = zb_and(n1618, n2220);
    let n2222: ZB = zb_and(n1617, n2220);
    let n2223: ZB = zb_or(n2221, n2222);
    let n2224: ZB = zb_and(n1618, n2223);
    let n2225: ZB = zb_and(n1617, n2223);
    let n2226: ZB = zb_or(n2224, n2225);
    let n2227: ZB = zb_and(n1617, n2226);
    let n2228: ZB = zb_and(n1618, n2226);
    let n2229: ZB = zb_or(n2227, n2228);
    let n2230: ZB = zb_and(n1617, n2229);
    let n2231: ZB = zb_and(n1618, n2229);
    let n2232: ZB = zb_or(n2230, n2231);
    let n2233: ZB = zb_and(n1416, n2232);
    let n2234: ZB = zb_and(n1415, n2232);
    let n2235: ZB = zb_or(n2233, n2234);
    let n2236: ZB = zb_or(n2176, n2235);
    let n2238: ZB = zb_and(n1475, n2181);
    let n2239: ZB = zb_and(n1476, n2181);
    let n2240: ZB = zb_or(n2238, n2239);
    let n2241: ZB = zb_or(n2194, n2240);
    let n2242: ZB = zb_and(n1663, n2241);
    let n2243: ZB = zb_and(n1662, n2241);
    let n2244: ZB = zb_or(n2242, n2243);
    let n2245: ZB = zb_and(n1519, n2244);
    let n2246: ZB = zb_and(n1520, n2244);
    let n2247: ZB = zb_or(n2245, n2246);
    let n2248: ZB = zb_and(n1674, n2247);
    let n2249: ZB = zb_and(n1673, n2247);
    let n2250: ZB = zb_or(n2248, n2249);
    let n2251: ZB = zb_and(n1674, n2250);
    let n2252: ZB = zb_and(n1673, n2250);
    let n2253: ZB = zb_or(n2251, n2252);
    let n2254: ZB = zb_and(n1673, n2253);
    let n2255: ZB = zb_and(n1674, n2253);
    let n2256: ZB = zb_or(n2254, n2255);
    let n2257: ZB = zb_and(n1673, n2256);
    let n2258: ZB = zb_and(n1674, n2256);
    let n2259: ZB = zb_or(n2257, n2258);
    let n2260: ZB = zb_and(n1416, n2259);
    let n2261: ZB = zb_and(n1415, n2259);
    let n2262: ZB = zb_or(n2260, n2261);
    let n2263: ZB = zb_or(n2176, n2262);
    let n2265: ZB = zb_and(n140, n2207);
    let n2266: ZB = zb_and(r_c294, n2207);
    let n2267: ZB = zb_and(n1532, n2265);
    let n2268: ZB = zb_and(n1533, n2265);
    let n2269: ZB = zb_and(n1536, n2268);
    let n2270: ZB = zb_and(n1535, n2268);
    let n2271: ZB = zb_or(n2269, n2270);
    let n2272: ZB = zb_and(n1536, n2271);
    let n2273: ZB = zb_and(n1535, n2271);
    let n2274: ZB = zb_or(n2272, n2273);
    let n2275: ZB = zb_and(n1535, n2274);
    let n2276: ZB = zb_and(n1536, n2274);
    let n2277: ZB = zb_and(n1539, n2276);
    let n2278: ZB = zb_and(n1538, n2276);
    let n2279: ZB = zb_or(n2277, n2278);
    let n2280: ZB = zb_and(n1539, n2279);
    let n2281: ZB = zb_and(n1538, n2279);
    let n2282: ZB = zb_or(n2280, n2281);
    let n2283: ZB = zb_and(n1538, n2282);
    let n2284: ZB = zb_and(n1539, n2282);
    let n2285: ZB = zb_or(n2283, n2284);
    let n2286: ZB = zb_or(n2275, n2285);
    let n2287: ZB = zb_and(n1543, n2286);
    let n2288: ZB = zb_and(n1542, n2286);
    let n2289: ZB = zb_or(n2287, n2288);
    let n2290: ZB = zb_or(n2267, n2289);
    let n2291: ZB = zb_or(n2266, n2290);
    let n2292: ZB = zb_or(n2176, n2291);
    let n2294: ZB = zb_and(n140, n2235);
    let n2295: ZB = zb_and(r_c294, n2235);
    let n2296: ZB = zb_and(n1532, n2294);
    let n2297: ZB = zb_and(n1533, n2294);
    let n2298: ZB = zb_and(n1536, n2297);
    let n2299: ZB = zb_and(n1535, n2297);
    let n2300: ZB = zb_or(n2298, n2299);
    let n2301: ZB = zb_and(n1536, n2300);
    let n2302: ZB = zb_and(n1535, n2300);
    let n2303: ZB = zb_or(n2301, n2302);
    let n2304: ZB = zb_and(n1535, n2303);
    let n2305: ZB = zb_and(n1536, n2303);
    let n2306: ZB = zb_and(n1539, n2305);
    let n2307: ZB = zb_and(n1538, n2305);
    let n2308: ZB = zb_or(n2306, n2307);
    let n2309: ZB = zb_and(n1539, n2308);
    let n2310: ZB = zb_and(n1538, n2308);
    let n2311: ZB = zb_or(n2309, n2310);
    let n2312: ZB = zb_and(n1538, n2311);
    let n2313: ZB = zb_and(n1539, n2311);
    let n2314: ZB = zb_or(n2312, n2313);
    let n2315: ZB = zb_or(n2304, n2314);
    let n2316: ZB = zb_and(n1543, n2315);
    let n2317: ZB = zb_and(n1542, n2315);
    let n2318: ZB = zb_or(n2316, n2317);
    let n2319: ZB = zb_or(n2296, n2318);
    let n2320: ZB = zb_or(n2295, n2319);
    let n2321: ZB = zb_or(n2176, n2320);
    let n2323: ZB = zb_and(n140, n2262);
    let n2324: ZB = zb_and(r_c294, n2262);
    let n2325: ZB = zb_and(n1532, n2323);
    let n2326: ZB = zb_and(n1533, n2323);
    let n2327: ZB = zb_and(n1536, n2326);
    let n2328: ZB = zb_and(n1535, n2326);
    let n2329: ZB = zb_or(n2327, n2328);
    let n2330: ZB = zb_and(n1536, n2329);
    let n2331: ZB = zb_and(n1535, n2329);
    let n2332: ZB = zb_or(n2330, n2331);
    let n2333: ZB = zb_and(n1535, n2332);
    let n2334: ZB = zb_and(n1536, n2332);
    let n2335: ZB = zb_and(n1539, n2334);
    let n2336: ZB = zb_and(n1538, n2334);
    let n2337: ZB = zb_or(n2335, n2336);
    let n2338: ZB = zb_and(n1539, n2337);
    let n2339: ZB = zb_and(n1538, n2337);
    let n2340: ZB = zb_or(n2338, n2339);
    let n2341: ZB = zb_and(n1538, n2340);
    let n2342: ZB = zb_and(n1539, n2340);
    let n2343: ZB = zb_or(n2341, n2342);
    let n2344: ZB = zb_or(n2333, n2343);
    let n2345: ZB = zb_and(n1543, n2344);
    let n2346: ZB = zb_and(n1542, n2344);
    let n2347: ZB = zb_or(n2345, n2346);
    let n2348: ZB = zb_or(n2325, n2347);
    let n2349: ZB = zb_or(n2324, n2348);
    let n2350: ZB = zb_or(n2176, n2349);
    let n2352: ZB = zb_and(n133, n2207);
    let n2353: ZB = zb_and(r_c293, n2207);
    let n2354: ZB = zb_and(n1516, n2352);
    let n2355: ZB = zb_and(n1551, n2352);
    let n2356: ZB = zb_or(n2354, n2355);
    let n2357: ZB = zb_and(n1553, n2356);
    let n2358: ZB = zb_and(n1554, n2356);
    let n2359: ZB = zb_and(n1555, n2358);
    let n2360: ZB = zb_and(n1556, n2358);
    let n2361: ZB = zb_or(n2359, n2360);
    let n2362: ZB = zb_or(n2357, n2361);
    let n2363: ZB = zb_and(n1560, n2362);
    let n2364: ZB = zb_and(n1559, n2362);
    let n2365: ZB = zb_or(n2363, n2364);
    let n2366: ZB = zb_or(n2353, n2365);
    let n2367: ZB = zb_or(n2176, n2366);
    let n2368: ZB = zb_and(n1873, n2367);
    let n2369: ZB = zb_and(n1874, n2367);
    let n2370: ZB = zb_or(n2368, n2369);
    let n2371: ZB = zb_and(n133, n2235);
    let n2372: ZB = zb_and(r_c293, n2235);
    let n2373: ZB = zb_or(n2371, n2372);
    let n2374: ZB = zb_or(n2176, n2373);
    let n2375: ZB = zb_and(n1873, n2374);
    let n2376: ZB = zb_and(n1874, n2374);
    let n2377: ZB = zb_or(n2375, n2376);
    let n2378: ZB = zb_and(n133, n2262);
    let n2379: ZB = zb_and(r_c293, n2262);
    let n2380: ZB = zb_or(n2378, n2379);
    let n2381: ZB = zb_or(n2176, n2380);
    let n2382: ZB = zb_and(n1873, n2381);
    let n2383: ZB = zb_and(n1874, n2381);
    let n2384: ZB = zb_or(n2382, n2383);
    let n2385: ZB = zb_or(n2352, n2353);
    let n2386: ZB = zb_or(n2176, n2385);
    let n2387: ZB = zb_and(n1873, n2386);
    let n2388: ZB = zb_and(n1874, n2386);
    let n2389: ZB = zb_or(n2387, n2388);
    let n2390: ZB = zb_and(n133, n2291);
    let n2391: ZB = zb_and(r_c293, n2291);
    let n2392: ZB = zb_and(n1516, n2390);
    let n2393: ZB = zb_and(n1551, n2390);
    let n2394: ZB = zb_or(n2392, n2393);
    let n2395: ZB = zb_and(n1553, n2394);
    let n2396: ZB = zb_and(n1554, n2394);
    let n2397: ZB = zb_and(n1555, n2396);
    let n2398: ZB = zb_and(n1556, n2396);
    let n2399: ZB = zb_or(n2397, n2398);
    let n2400: ZB = zb_or(n2395, n2399);
    let n2401: ZB = zb_and(n1560, n2400);
    let n2402: ZB = zb_and(n1559, n2400);
    let n2403: ZB = zb_or(n2401, n2402);
    let n2404: ZB = zb_or(n2391, n2403);
    let n2405: ZB = zb_or(n2176, n2404);
    let n2406: ZB = zb_and(n1873, n2405);
    let n2407: ZB = zb_and(n1874, n2405);
    let n2408: ZB = zb_or(n2406, n2407);
    let n2409: ZB = zb_and(n133, n2320);
    let n2410: ZB = zb_and(r_c293, n2320);
    let n2411: ZB = zb_or(n2409, n2410);
    let n2412: ZB = zb_or(n2176, n2411);
    let n2413: ZB = zb_and(n1873, n2412);
    let n2414: ZB = zb_and(n1874, n2412);
    let n2415: ZB = zb_or(n2413, n2414);
    let n2416: ZB = zb_and(n133, n2349);
    let n2417: ZB = zb_and(r_c293, n2349);
    let n2418: ZB = zb_or(n2416, n2417);
    let n2419: ZB = zb_or(n2176, n2418);
    let n2420: ZB = zb_and(n1873, n2419);
    let n2421: ZB = zb_and(n1874, n2419);
    let n2422: ZB = zb_or(n2420, n2421);
    let n2423: ZB = zb_or(n2390, n2391);
    let n2424: ZB = zb_or(n2176, n2423);
    let n2425: ZB = zb_and(n1873, n2424);
    let n2426: ZB = zb_and(n1874, n2424);
    let n2427: ZB = zb_or(n2425, n2426);
    let n2429: ZB = zb_not(n228);
    let n2430: ZB = zb_and(n134, n2429);
    let n2431: ZB = zn_lt(r_c370, zn_splat(P8::from_raw(0i32)));
    let n2432: ZB = zb_and(n229, n2431);
    let n2433: ZB = zb_or(n2430, n2432);
    let n2434: ZB = zb_and(n202, n2433);
    let n2435: ZB = zb_and(n203, n2433);
    let n2436: ZB = zb_and(n235, n2434);
    let n2437: ZB = zb_and(n236, n2434);
    let n2438: ZB = zb_or(n2436, n2437);
    let n2439: ZB = zb_and(n223, n235);
    let n2440: ZB = zb_not(n2439);
    let n2441: ZB = zb_and(n2438, n2439);
    let n2442: ZB = zb_and(n2438, n2440);
    let n2443: ZB = zb_or(n2441, n2442);
    let n2444: ZB = zb_and(n242, n2439);
    let n2445: ZB = zb_not(n2444);
    let n2446: ZB = zb_and(n2443, n2444);
    let n2447: ZB = zb_and(n2443, n2445);
    let n2448: ZB = zb_or(n2446, n2447);
    let n2449: ZB = zb_and(n227, n2444);
    let n2450: ZB = zb_not(n2449);
    let n2451: ZB = zb_and(n2448, n2449);
    let n2452: ZB = zb_and(n2448, n2450);
    let n2453: ZB = zb_and(n230, n2451);
    let n2454: ZB = zb_and(n2431, n2451);
    let n2455: ZN = zsel_n(n230, zn_splat(P8::from_raw(65536i32)), r_c284);
    let n2456: ZN = zsel_n(n230, zn_splat(P8::from_raw(7077888i32)), r_c301);
    let n2457: ZN = zsel_n(n230, zn_splat(P8::from_raw(655360i32)), r_c260);
    let n2458: ZN = zsel_n(n230, zn_splat(P8::from_raw(1245184i32)), r_c273);
    let n2459: ZN = zsel_n(n230, n232, r_c369);
    let n2460: ZN = zsel_n(n230, zn_splat(P8::from_raw(-196608i32)), r_c370);
    let n2461: ZB = zb_or(n2453, n2454);
    let n2462: ZB = zb_and(n194, n2435);
    let n2463: ZB = zb_and(n195, n2435);
    let n2464: ZB = zb_and(n197, n2462);
    let n2465: ZB = zb_and(n198, n2462);
    let n2466: ZB = zb_or(n2464, n2465);
    let n2467: ZB = zb_or(n2463, n2466);
    let n2468: ZN = zsel_n(n2449, n2457, r_c260);
    let n2469: ZN = zsel_n(n2449, n2458, r_c273);
    let n2470: ZN = zsel_n(n2449, n2455, r_c284);
    let n2471: ZN = zsel_n(n2449, n2456, r_c301);
    let n2472: ZN = zsel_n(n2449, n2459, r_c369);
    let n2473: ZN = zsel_n(n2449, n2460, r_c370);
    let n2474: ZB = zb_or(n2452, n2461);
    let n2475: ZN = zsel_n(n203, n199, n2468);
    let n2476: ZN = zsel_n(n203, n205, n2469);
    let n2477: ZN = zsel_n(n203, r_c284, n2470);
    let n2478: ZN = zsel_n(n203, r_c301, n2471);
    let n2479: ZN = zsel_n(n203, r_c369, n2472);
    let n2480: ZN = zsel_n(n203, r_c370, n2473);
    let n2481: ZB = zb_or(n2467, n2474);
    let n2482: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2479);
    let n2483: ZB = zb_not(n2482);
    let n2484: ZB = zb_and(n2481, n2482);
    let n2485: ZB = zb_and(n2481, n2483);
    let n2486: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2480);
    let n2487: ZB = zb_not(n2486);
    let n2488: ZB = zb_or(n2484, n2485);
    let n2489: ZB = zb_or(n2483, n2487);
    let n2490: ZB = zb_not(n2489);
    let n2491: ZB = zb_and(n2488, n2489);
    let n2492: ZB = zb_and(n2488, n2490);
    let n2493: ZN = zn_add(r_c367, n2479);
    let n2494: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n2493);
    let n2495: ZN = zn_flr(n2494);
    let n2496: ZB = zn_gt(n2495, zn_splat(P8::from_raw(0i32)));
    let n2497: ZB = zn_le(n2495, zn_splat(P8::from_raw(0i32)));
    let n2498: ZB = zb_and(n2491, n2496);
    let n2499: ZB = zb_and(n2491, n2497);
    let n2500: ZB = zn_lt(n2495, zn_splat(P8::from_raw(0i32)));
    let n2501: ZB = zn_ge(n2495, zn_splat(P8::from_raw(0i32)));
    let n2502: ZB = zb_and(n2499, n2500);
    let n2503: ZB = zb_and(n2499, n2501);
    let n2504: ZN = zsel_n(n2500, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n2505: ZB = zb_or(n2502, n2503);
    let n2506: ZN = zsel_n(n2496, zn_splat(P8::from_raw(65536i32)), n2504);
    let n2507: ZB = zb_or(n2498, n2505);
    let n2508: ZN = zn_abs(n2495);
    let n2509: ZN = zn_add(n218, n2506);
    let n2510: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n2478);
    let n2511: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n2510);
    let n2512: ZB = zn_tile_flag_at(g.cache, g.cart, n2509, n2511, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2513: ZB = zb_not(n2512);
    let n2514: ZB = zb_and(n2507, n2513);
    let n2515: ZB = zb_and(n2507, n2512);
    let n2516: ZB = zb_or(n2514, n2515);
    let n2517: ZB = zb_and(n2513, n2516);
    let n2518: ZB = zb_and(n2512, n2516);
    let n2519: ZB = zb_or(n2517, n2518);
    let n2520: ZB = zb_and(n2513, n2519);
    let n2521: ZB = zb_and(n2512, n2519);
    let n2522: ZN = zn_add(r_c300, n2506);
    let n2523: ZB = zn_le(zn_splat(P8::from_raw(65536i32)), n2508);
    let n2524: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n2508);
    let n2525: ZB = zb_and(n2520, n2523);
    let n2526: ZB = zb_and(n2520, n2524);
    let n2527: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n2522);
    let n2528: ZN = zn_add(n2506, n2527);
    let n2529: ZB = zn_tile_flag_at(g.cache, g.cart, n2528, n2511, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2530: ZB = zb_not(n2529);
    let n2531: ZB = zb_and(n2525, n2530);
    let n2532: ZB = zb_and(n2525, n2529);
    let n2533: ZB = zb_or(n2531, n2532);
    let n2534: ZB = zb_and(n2530, n2533);
    let n2535: ZB = zb_and(n2529, n2533);
    let n2536: ZB = zb_or(n2534, n2535);
    let n2537: ZB = zb_and(n2530, n2536);
    let n2538: ZB = zb_and(n2529, n2536);
    let n2539: ZN = zn_add(n2506, n2522);
    let n2540: ZB = zn_le(zn_splat(P8::from_raw(131072i32)), n2508);
    let n2541: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n2508);
    let n2542: ZB = zb_and(n2537, n2540);
    let n2543: ZB = zb_and(n2537, n2541);
    let n2544: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n2539);
    let n2545: ZN = zn_add(n2506, n2544);
    let n2546: ZB = zn_tile_flag_at(g.cache, g.cart, n2545, n2511, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2547: ZB = zb_not(n2546);
    let n2548: ZB = zb_and(n2542, n2547);
    let n2549: ZB = zb_and(n2542, n2546);
    let n2550: ZB = zb_or(n2548, n2549);
    let n2551: ZB = zb_and(n2547, n2550);
    let n2552: ZB = zb_and(n2546, n2550);
    let n2553: ZB = zb_or(n2551, n2552);
    let n2554: ZB = zb_and(n2547, n2553);
    let n2555: ZB = zb_and(n2546, n2553);
    let n2556: ZN = zn_add(n2506, n2539);
    let n2557: ZB = zn_le(zn_splat(P8::from_raw(196608i32)), n2508);
    let n2558: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n2508);
    let n2559: ZB = zb_and(n2554, n2557);
    let n2560: ZB = zb_and(n2554, n2558);
    let n2561: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n2556);
    let n2562: ZN = zn_add(n2506, n2561);
    let n2563: ZB = zn_tile_flag_at(g.cache, g.cart, n2562, n2511, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2564: ZB = zb_not(n2563);
    let n2565: ZB = zb_and(n2559, n2564);
    let n2566: ZB = zb_and(n2559, n2563);
    let n2567: ZB = zb_or(n2565, n2566);
    let n2568: ZB = zb_and(n2564, n2567);
    let n2569: ZB = zb_and(n2563, n2567);
    let n2570: ZB = zb_or(n2568, n2569);
    let n2571: ZB = zb_and(n2564, n2570);
    let n2572: ZB = zb_and(n2563, n2570);
    let n2573: ZN = zn_add(n2506, n2556);
    let n2574: ZB = zn_le(zn_splat(P8::from_raw(262144i32)), n2508);
    let n2575: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n2508);
    let n2576: ZB = zb_and(n2571, n2574);
    let n2577: ZB = zb_and(n2571, n2575);
    let n2578: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n2573);
    let n2579: ZN = zn_add(n2506, n2578);
    let n2580: ZB = zn_tile_flag_at(g.cache, g.cart, n2579, n2511, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2581: ZB = zb_not(n2580);
    let n2582: ZB = zb_and(n2576, n2581);
    let n2583: ZB = zb_and(n2576, n2580);
    let n2584: ZB = zb_or(n2582, n2583);
    let n2585: ZB = zb_and(n2581, n2584);
    let n2586: ZB = zb_and(n2580, n2584);
    let n2587: ZB = zb_or(n2585, n2586);
    let n2588: ZB = zb_and(n2581, n2587);
    let n2589: ZB = zb_and(n2580, n2587);
    let n2590: ZN = zn_add(n2506, n2573);
    let n2591: ZB = zn_le(zn_splat(P8::from_raw(327680i32)), n2508);
    let n2592: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n2508);
    let n2593: ZB = zb_and(n2588, n2591);
    let n2594: ZB = zb_and(n2588, n2592);
    let n2595: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n2590);
    let n2596: ZN = zn_add(n2506, n2595);
    let n2597: ZB = zn_tile_flag_at(g.cache, g.cart, n2596, n2511, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2598: ZB = zb_not(n2597);
    let n2599: ZB = zb_and(n2593, n2598);
    let n2600: ZB = zb_and(n2593, n2597);
    let n2601: ZB = zb_or(n2599, n2600);
    let n2602: ZB = zb_and(n2598, n2601);
    let n2603: ZB = zb_and(n2597, n2601);
    let n2604: ZB = zb_or(n2602, n2603);
    let n2605: ZB = zb_and(n2598, n2604);
    let n2606: ZB = zb_and(n2597, n2604);
    let n2607: ZN = zn_add(n2506, n2590);
    let n2608: ZB = zn_le(zn_splat(P8::from_raw(393216i32)), n2508);
    let n2609: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n2508);
    let n2610: ZB = zb_and(n2605, n2608);
    let n2611: ZB = zb_and(n2605, n2609);
    let n2612: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n2607);
    let n2613: ZN = zn_add(n2506, n2612);
    let n2614: ZB = zn_tile_flag_at(g.cache, g.cart, n2613, n2511, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2615: ZB = zb_not(n2614);
    let n2616: ZB = zb_and(n2610, n2615);
    let n2617: ZB = zb_and(n2610, n2614);
    let n2618: ZB = zb_or(n2616, n2617);
    let n2619: ZB = zb_and(n2615, n2618);
    let n2620: ZB = zb_and(n2614, n2618);
    let n2621: ZB = zb_or(n2619, n2620);
    let n2622: ZB = zb_and(n2615, n2621);
    let n2623: ZB = zb_and(n2614, n2621);
    let n2624: ZN = zn_add(n2506, n2607);
    let n2625: ZB = zn_le(zn_splat(P8::from_raw(458752i32)), n2508);
    let n2626: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n2508);
    let n2627: ZB = zb_and(n2622, n2625);
    let n2628: ZB = zb_and(n2622, n2626);
    let n2629: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n2624);
    let n2630: ZN = zn_add(n2506, n2629);
    let n2631: ZB = zn_tile_flag_at(g.cache, g.cart, n2630, n2511, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2632: ZB = zb_not(n2631);
    let n2633: ZB = zb_and(n2627, n2632);
    let n2634: ZB = zb_and(n2627, n2631);
    let n2635: ZB = zb_or(n2633, n2634);
    let n2636: ZB = zb_and(n2632, n2635);
    let n2637: ZB = zb_and(n2631, n2635);
    let n2638: ZB = zb_or(n2636, n2637);
    let n2639: ZB = zb_and(n2632, n2638);
    let n2640: ZB = zb_and(n2631, n2638);
    let n2641: ZN = zn_add(n2506, n2624);
    let n2642: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n2508);
    let n2643: ZN = zsel_n(n2631, n2624, n2641);
    let n2644: ZN = zsel_n(n2631, zn_splat(P8::from_raw(0i32)), n2479);
    let n2645: ZB = zb_or(n2639, n2640);
    let n2646: ZB = zb_or(n2631, n2642);
    let n2647: ZN = zsel_n(n2626, n2624, n2643);
    let n2648: ZN = zsel_n(n2626, n2479, n2644);
    let n2649: ZB = zb_or(n2628, n2645);
    let n2650: ZB = zb_or(n2626, n2646);
    let n2651: ZN = zsel_n(n2614, n2607, n2647);
    let n2652: ZN = zsel_n(n2614, zn_splat(P8::from_raw(0i32)), n2648);
    let n2653: ZB = zb_or(n2623, n2649);
    let n2654: ZB = zb_or(n2614, n2650);
    let n2655: ZN = zsel_n(n2609, n2607, n2651);
    let n2656: ZN = zsel_n(n2609, n2479, n2652);
    let n2657: ZB = zb_or(n2611, n2653);
    let n2658: ZB = zb_or(n2609, n2654);
    let n2659: ZN = zsel_n(n2597, n2590, n2655);
    let n2660: ZN = zsel_n(n2597, zn_splat(P8::from_raw(0i32)), n2656);
    let n2661: ZB = zb_or(n2606, n2657);
    let n2662: ZB = zb_or(n2597, n2658);
    let n2663: ZN = zsel_n(n2592, n2590, n2659);
    let n2664: ZN = zsel_n(n2592, n2479, n2660);
    let n2665: ZB = zb_or(n2594, n2661);
    let n2666: ZB = zb_or(n2592, n2662);
    let n2667: ZN = zsel_n(n2580, n2573, n2663);
    let n2668: ZN = zsel_n(n2580, zn_splat(P8::from_raw(0i32)), n2664);
    let n2669: ZB = zb_or(n2589, n2665);
    let n2670: ZB = zb_or(n2580, n2666);
    let n2671: ZN = zsel_n(n2575, n2573, n2667);
    let n2672: ZN = zsel_n(n2575, n2479, n2668);
    let n2673: ZB = zb_or(n2577, n2669);
    let n2674: ZB = zb_or(n2575, n2670);
    let n2675: ZN = zsel_n(n2563, n2556, n2671);
    let n2676: ZN = zsel_n(n2563, zn_splat(P8::from_raw(0i32)), n2672);
    let n2677: ZB = zb_or(n2572, n2673);
    let n2678: ZB = zb_or(n2563, n2674);
    let n2679: ZN = zsel_n(n2558, n2556, n2675);
    let n2680: ZN = zsel_n(n2558, n2479, n2676);
    let n2681: ZB = zb_or(n2560, n2677);
    let n2682: ZB = zb_or(n2558, n2678);
    let n2683: ZN = zsel_n(n2546, n2539, n2679);
    let n2684: ZN = zsel_n(n2546, zn_splat(P8::from_raw(0i32)), n2680);
    let n2685: ZB = zb_or(n2555, n2681);
    let n2686: ZB = zb_or(n2546, n2682);
    let n2687: ZN = zsel_n(n2541, n2539, n2683);
    let n2688: ZN = zsel_n(n2541, n2479, n2684);
    let n2689: ZB = zb_or(n2543, n2685);
    let n2690: ZB = zb_or(n2541, n2686);
    let n2691: ZN = zsel_n(n2529, n2522, n2687);
    let n2692: ZN = zsel_n(n2529, zn_splat(P8::from_raw(0i32)), n2688);
    let n2693: ZB = zb_or(n2538, n2689);
    let n2694: ZB = zb_or(n2529, n2690);
    let n2695: ZN = zsel_n(n2524, n2522, n2691);
    let n2696: ZN = zsel_n(n2524, n2479, n2692);
    let n2697: ZB = zb_or(n2526, n2693);
    let n2698: ZB = zb_or(n2524, n2694);
    let n2699: ZN = zsel_n(n2512, r_c300, n2695);
    let n2700: ZN = zsel_n(n2512, zn_splat(P8::from_raw(0i32)), n2696);
    let n2701: ZB = zb_or(n2521, n2697);
    let n2702: ZB = zb_or(n2512, n2698);
    let n2703: ZN = zn_add(r_c368, n2480);
    let n2704: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n2703);
    let n2705: ZN = zn_flr(n2704);
    let n2706: ZB = zn_gt(n2705, zn_splat(P8::from_raw(0i32)));
    let n2707: ZB = zn_le(n2705, zn_splat(P8::from_raw(0i32)));
    let n2708: ZB = zb_and(n2701, n2706);
    let n2709: ZB = zb_and(n2701, n2707);
    let n2710: ZB = zn_lt(n2705, zn_splat(P8::from_raw(0i32)));
    let n2711: ZB = zn_ge(n2705, zn_splat(P8::from_raw(0i32)));
    let n2712: ZB = zb_and(n2709, n2710);
    let n2713: ZB = zb_and(n2709, n2711);
    let n2714: ZN = zsel_n(n2710, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n2715: ZB = zb_or(n2712, n2713);
    let n2716: ZN = zsel_n(n2706, zn_splat(P8::from_raw(65536i32)), n2714);
    let n2717: ZB = zb_or(n2708, n2715);
    let n2718: ZN = zn_abs(n2705);
    let n2719: ZB = zn_gt(n2716, zn_splat(P8::from_raw(0i32)));
    let n2720: ZB = zn_le(n2716, zn_splat(P8::from_raw(0i32)));
    let n2721: ZB = zb_and(n2717, n2719);
    let n2722: ZB = zb_and(n2717, n2720);
    let n2723: ZB = zb_or(n2721, n2722);
    let n2724: ZB = zb_and(n2719, n2723);
    let n2725: ZB = zb_and(n2720, n2723);
    let n2726: ZB = zb_or(n2724, n2725);
    let n2727: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n2699);
    let n2728: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n2727);
    let n2729: ZN = zn_add(n2510, n2716);
    let n2730: ZB = zn_tile_flag_at(g.cache, g.cart, n2728, n2729, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2731: ZB = zb_not(n2730);
    let n2732: ZB = zb_and(n2726, n2731);
    let n2733: ZB = zb_and(n2726, n2730);
    let n2734: ZB = zb_or(n2732, n2733);
    let n2735: ZB = zb_and(n2731, n2734);
    let n2736: ZB = zb_and(n2730, n2734);
    let n2737: ZB = zb_or(n2735, n2736);
    let n2738: ZB = zb_and(n2731, n2737);
    let n2739: ZB = zb_and(n2730, n2737);
    let n2740: ZN = zn_add(n2478, n2716);
    let n2741: ZB = zn_le(zn_splat(P8::from_raw(65536i32)), n2718);
    let n2742: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n2718);
    let n2743: ZB = zb_and(n2738, n2741);
    let n2744: ZB = zb_and(n2738, n2742);
    let n2745: ZB = zb_and(n2719, n2743);
    let n2746: ZB = zb_and(n2720, n2743);
    let n2747: ZB = zb_or(n2745, n2746);
    let n2748: ZB = zb_and(n2719, n2747);
    let n2749: ZB = zb_and(n2720, n2747);
    let n2750: ZB = zb_or(n2748, n2749);
    let n2751: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n2740);
    let n2752: ZN = zn_add(n2716, n2751);
    let n2753: ZB = zn_tile_flag_at(g.cache, g.cart, n2728, n2752, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2754: ZB = zb_not(n2753);
    let n2755: ZB = zb_and(n2750, n2754);
    let n2756: ZB = zb_and(n2750, n2753);
    let n2757: ZB = zb_or(n2755, n2756);
    let n2758: ZB = zb_and(n2754, n2757);
    let n2759: ZB = zb_and(n2753, n2757);
    let n2760: ZB = zb_or(n2758, n2759);
    let n2761: ZB = zb_and(n2754, n2760);
    let n2762: ZB = zb_and(n2753, n2760);
    let n2763: ZN = zn_add(n2716, n2740);
    let n2764: ZB = zn_le(zn_splat(P8::from_raw(131072i32)), n2718);
    let n2765: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n2718);
    let n2766: ZB = zb_and(n2761, n2764);
    let n2767: ZB = zb_and(n2761, n2765);
    let n2768: ZB = zb_and(n2719, n2766);
    let n2769: ZB = zb_and(n2720, n2766);
    let n2770: ZB = zb_or(n2768, n2769);
    let n2771: ZB = zb_and(n2719, n2770);
    let n2772: ZB = zb_and(n2720, n2770);
    let n2773: ZB = zb_or(n2771, n2772);
    let n2774: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n2763);
    let n2775: ZN = zn_add(n2716, n2774);
    let n2776: ZB = zn_tile_flag_at(g.cache, g.cart, n2728, n2775, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2777: ZB = zb_not(n2776);
    let n2778: ZB = zb_and(n2773, n2777);
    let n2779: ZB = zb_and(n2773, n2776);
    let n2780: ZB = zb_or(n2778, n2779);
    let n2781: ZB = zb_and(n2777, n2780);
    let n2782: ZB = zb_and(n2776, n2780);
    let n2783: ZB = zb_or(n2781, n2782);
    let n2784: ZB = zb_and(n2777, n2783);
    let n2785: ZB = zb_and(n2776, n2783);
    let n2786: ZN = zn_add(n2716, n2763);
    let n2787: ZB = zn_le(zn_splat(P8::from_raw(196608i32)), n2718);
    let n2788: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n2718);
    let n2789: ZB = zb_and(n2784, n2787);
    let n2790: ZB = zb_and(n2784, n2788);
    let n2791: ZB = zb_and(n2719, n2789);
    let n2792: ZB = zb_and(n2720, n2789);
    let n2793: ZB = zb_or(n2791, n2792);
    let n2794: ZB = zb_and(n2719, n2793);
    let n2795: ZB = zb_and(n2720, n2793);
    let n2796: ZB = zb_or(n2794, n2795);
    let n2797: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n2786);
    let n2798: ZN = zn_add(n2716, n2797);
    let n2799: ZB = zn_tile_flag_at(g.cache, g.cart, n2728, n2798, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2800: ZB = zb_not(n2799);
    let n2801: ZB = zb_and(n2796, n2800);
    let n2802: ZB = zb_and(n2796, n2799);
    let n2803: ZB = zb_or(n2801, n2802);
    let n2804: ZB = zb_and(n2800, n2803);
    let n2805: ZB = zb_and(n2799, n2803);
    let n2806: ZB = zb_or(n2804, n2805);
    let n2807: ZB = zb_and(n2800, n2806);
    let n2808: ZB = zb_and(n2799, n2806);
    let n2809: ZN = zn_add(n2716, n2786);
    let n2810: ZB = zn_le(zn_splat(P8::from_raw(262144i32)), n2718);
    let n2811: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n2718);
    let n2812: ZB = zb_and(n2807, n2810);
    let n2813: ZB = zb_and(n2807, n2811);
    let n2814: ZB = zb_and(n2719, n2812);
    let n2815: ZB = zb_and(n2720, n2812);
    let n2816: ZB = zb_or(n2814, n2815);
    let n2817: ZB = zb_and(n2719, n2816);
    let n2818: ZB = zb_and(n2720, n2816);
    let n2819: ZB = zb_or(n2817, n2818);
    let n2820: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n2809);
    let n2821: ZN = zn_add(n2716, n2820);
    let n2822: ZB = zn_tile_flag_at(g.cache, g.cart, n2728, n2821, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2823: ZB = zb_not(n2822);
    let n2824: ZB = zb_and(n2819, n2823);
    let n2825: ZB = zb_and(n2819, n2822);
    let n2826: ZB = zb_or(n2824, n2825);
    let n2827: ZB = zb_and(n2823, n2826);
    let n2828: ZB = zb_and(n2822, n2826);
    let n2829: ZB = zb_or(n2827, n2828);
    let n2830: ZB = zb_and(n2823, n2829);
    let n2831: ZB = zb_and(n2822, n2829);
    let n2832: ZN = zn_add(n2716, n2809);
    let n2833: ZB = zn_le(zn_splat(P8::from_raw(327680i32)), n2718);
    let n2834: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n2718);
    let n2835: ZB = zb_and(n2830, n2833);
    let n2836: ZB = zb_and(n2830, n2834);
    let n2837: ZB = zb_and(n2719, n2835);
    let n2838: ZB = zb_and(n2720, n2835);
    let n2839: ZB = zb_or(n2837, n2838);
    let n2840: ZB = zb_and(n2719, n2839);
    let n2841: ZB = zb_and(n2720, n2839);
    let n2842: ZB = zb_or(n2840, n2841);
    let n2843: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n2832);
    let n2844: ZN = zn_add(n2716, n2843);
    let n2845: ZB = zn_tile_flag_at(g.cache, g.cart, n2728, n2844, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2846: ZB = zb_not(n2845);
    let n2847: ZB = zb_and(n2842, n2846);
    let n2848: ZB = zb_and(n2842, n2845);
    let n2849: ZB = zb_or(n2847, n2848);
    let n2850: ZB = zb_and(n2846, n2849);
    let n2851: ZB = zb_and(n2845, n2849);
    let n2852: ZB = zb_or(n2850, n2851);
    let n2853: ZB = zb_and(n2846, n2852);
    let n2854: ZB = zb_and(n2845, n2852);
    let n2855: ZN = zn_add(n2716, n2832);
    let n2856: ZB = zn_le(zn_splat(P8::from_raw(393216i32)), n2718);
    let n2857: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n2718);
    let n2858: ZB = zb_and(n2853, n2856);
    let n2859: ZB = zb_and(n2853, n2857);
    let n2860: ZB = zb_and(n2719, n2858);
    let n2861: ZB = zb_and(n2720, n2858);
    let n2862: ZB = zb_or(n2860, n2861);
    let n2863: ZB = zb_and(n2719, n2862);
    let n2864: ZB = zb_and(n2720, n2862);
    let n2865: ZB = zb_or(n2863, n2864);
    let n2866: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n2855);
    let n2867: ZN = zn_add(n2716, n2866);
    let n2868: ZB = zn_tile_flag_at(g.cache, g.cart, n2728, n2867, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2869: ZB = zb_not(n2868);
    let n2870: ZB = zb_and(n2865, n2869);
    let n2871: ZB = zb_and(n2865, n2868);
    let n2872: ZB = zb_or(n2870, n2871);
    let n2873: ZB = zb_and(n2869, n2872);
    let n2874: ZB = zb_and(n2868, n2872);
    let n2875: ZB = zb_or(n2873, n2874);
    let n2876: ZB = zb_and(n2869, n2875);
    let n2877: ZB = zb_and(n2868, n2875);
    let n2878: ZN = zn_add(n2716, n2855);
    let n2879: ZB = zn_le(zn_splat(P8::from_raw(458752i32)), n2718);
    let n2880: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n2718);
    let n2881: ZB = zb_and(n2876, n2879);
    let n2882: ZB = zb_and(n2876, n2880);
    let n2883: ZB = zb_and(n2719, n2881);
    let n2884: ZB = zb_and(n2720, n2881);
    let n2885: ZB = zb_or(n2883, n2884);
    let n2886: ZB = zb_and(n2719, n2885);
    let n2887: ZB = zb_and(n2720, n2885);
    let n2888: ZB = zb_or(n2886, n2887);
    let n2889: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n2878);
    let n2890: ZN = zn_add(n2716, n2889);
    let n2891: ZB = zn_tile_flag_at(g.cache, g.cart, n2728, n2890, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2892: ZB = zb_not(n2891);
    let n2893: ZB = zb_and(n2888, n2892);
    let n2894: ZB = zb_and(n2888, n2891);
    let n2895: ZB = zb_or(n2893, n2894);
    let n2896: ZB = zb_and(n2892, n2895);
    let n2897: ZB = zb_and(n2891, n2895);
    let n2898: ZB = zb_or(n2896, n2897);
    let n2899: ZB = zb_and(n2892, n2898);
    let n2900: ZB = zb_and(n2891, n2898);
    let n2901: ZN = zn_add(n2716, n2878);
    let n2902: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n2718);
    let n2903: ZB = zb_and(n2702, n2902);
    let n2904: ZN = zsel_n(n2891, n2878, n2901);
    let n2905: ZN = zsel_n(n2891, zn_splat(P8::from_raw(0i32)), n2480);
    let n2906: ZB = zb_or(n2899, n2900);
    let n2907: ZB = zsel_b(n2891, n2702, n2903);
    let n2908: ZN = zsel_n(n2880, n2878, n2904);
    let n2909: ZN = zsel_n(n2880, n2480, n2905);
    let n2910: ZB = zb_or(n2882, n2906);
    let n2911: ZB = zsel_b(n2880, n2702, n2907);
    let n2912: ZN = zsel_n(n2868, n2855, n2908);
    let n2913: ZN = zsel_n(n2868, zn_splat(P8::from_raw(0i32)), n2909);
    let n2914: ZB = zb_or(n2877, n2910);
    let n2915: ZB = zsel_b(n2868, n2702, n2911);
    let n2916: ZN = zsel_n(n2857, n2855, n2912);
    let n2917: ZN = zsel_n(n2857, n2480, n2913);
    let n2918: ZB = zb_or(n2859, n2914);
    let n2919: ZB = zsel_b(n2857, n2702, n2915);
    let n2920: ZN = zsel_n(n2845, n2832, n2916);
    let n2921: ZN = zsel_n(n2845, zn_splat(P8::from_raw(0i32)), n2917);
    let n2922: ZB = zb_or(n2854, n2918);
    let n2923: ZB = zsel_b(n2845, n2702, n2919);
    let n2924: ZN = zsel_n(n2834, n2832, n2920);
    let n2925: ZN = zsel_n(n2834, n2480, n2921);
    let n2926: ZB = zb_or(n2836, n2922);
    let n2927: ZB = zsel_b(n2834, n2702, n2923);
    let n2928: ZN = zsel_n(n2822, n2809, n2924);
    let n2929: ZN = zsel_n(n2822, zn_splat(P8::from_raw(0i32)), n2925);
    let n2930: ZB = zb_or(n2831, n2926);
    let n2931: ZB = zsel_b(n2822, n2702, n2927);
    let n2932: ZN = zsel_n(n2811, n2809, n2928);
    let n2933: ZN = zsel_n(n2811, n2480, n2929);
    let n2934: ZB = zb_or(n2813, n2930);
    let n2935: ZB = zsel_b(n2811, n2702, n2931);
    let n2936: ZN = zsel_n(n2799, n2786, n2932);
    let n2937: ZN = zsel_n(n2799, zn_splat(P8::from_raw(0i32)), n2933);
    let n2938: ZB = zb_or(n2808, n2934);
    let n2939: ZB = zsel_b(n2799, n2702, n2935);
    let n2940: ZN = zsel_n(n2788, n2786, n2936);
    let n2941: ZN = zsel_n(n2788, n2480, n2937);
    let n2942: ZB = zb_or(n2790, n2938);
    let n2943: ZB = zsel_b(n2788, n2702, n2939);
    let n2944: ZN = zsel_n(n2776, n2763, n2940);
    let n2945: ZN = zsel_n(n2776, zn_splat(P8::from_raw(0i32)), n2941);
    let n2946: ZB = zb_or(n2785, n2942);
    let n2947: ZB = zsel_b(n2776, n2702, n2943);
    let n2948: ZN = zsel_n(n2765, n2763, n2944);
    let n2949: ZN = zsel_n(n2765, n2480, n2945);
    let n2950: ZB = zb_or(n2767, n2946);
    let n2951: ZB = zsel_b(n2765, n2702, n2947);
    let n2952: ZN = zsel_n(n2753, n2740, n2948);
    let n2953: ZN = zsel_n(n2753, zn_splat(P8::from_raw(0i32)), n2949);
    let n2954: ZB = zb_or(n2762, n2950);
    let n2955: ZB = zsel_b(n2753, n2702, n2951);
    let n2956: ZN = zsel_n(n2742, n2740, n2952);
    let n2957: ZN = zsel_n(n2742, n2480, n2953);
    let n2958: ZB = zb_or(n2744, n2954);
    let n2959: ZB = zsel_b(n2742, n2702, n2955);
    let n2960: ZN = zsel_n(n2730, n2478, n2956);
    let n2961: ZN = zsel_n(n2730, zn_splat(P8::from_raw(0i32)), n2957);
    let n2962: ZB = zb_or(n2739, n2958);
    let n2963: ZB = zsel_b(n2730, n2702, n2959);
    let n2964: ZN = zsel_n(n2489, n2699, r_c300);
    let n2965: ZN = zsel_n(n2489, n2960, n2478);
    let n2966: ZN = zsel_n(n2489, n2700, n2479);
    let n2967: ZN = zsel_n(n2489, n2961, n2480);
    let n2968: ZB = zb_or(n2492, n2962);
    let n2969: ZB = zb_or(n2490, n2963);
    let n2970: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n2964);
    let n2971: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n2965);
    let n2972: ZN = zn_div(n2970, zn_splat(P8::from_raw(524288i32)));
    let n2973: ZN = zn_flr(n2972);
    let n2974: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n2973);
    let n2975: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n2970);
    let n2976: ZN = zn_sub(n2975, zn_splat(P8::from_raw(65536i32)));
    let n2977: ZN = zn_div(n2976, zn_splat(P8::from_raw(524288i32)));
    let n2978: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n2977);
    let n2979: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n2974);
    let n2980: ZB = zn_le(n2979, n2978);
    let n2981: ZB = zn_gt(n2979, n2978);
    let n2982: ZB = zb_and(n2968, n2980);
    let n2983: ZB = zb_and(n2968, n2981);
    let n2984: ZN = zn_div(n2971, zn_splat(P8::from_raw(524288i32)));
    let n2985: ZN = zn_flr(n2984);
    let n2986: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n2985);
    let n2987: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n2971);
    let n2988: ZN = zn_sub(n2987, zn_splat(P8::from_raw(65536i32)));
    let n2989: ZN = zn_div(n2988, zn_splat(P8::from_raw(524288i32)));
    let n2990: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n2989);
    let n2991: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n2986);
    let n2992: ZB = zn_le(n2991, n2990);
    let n2993: ZB = zn_gt(n2991, n2990);
    let n2994: ZB = zb_and(n2982, n2992);
    let n2995: ZB = zb_and(n2982, n2993);
    let n2996: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n2979);
    let n2997: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n2991);
    let n2998: ZN = zn_mget(g.cart, n2996, n2997);
    let n2999: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n2998);
    let n3000: ZB = zb_not(n2999);
    let n3001: ZB = zb_and(n2994, n2999);
    let n3002: ZB = zb_and(n2994, n3000);
    let n3003: ZN = zn_rem(n2988, zn_splat(P8::from_raw(524288i32)));
    let n3004: ZB = zn_ge(n3003, zn_splat(P8::from_raw(393216i32)));
    let n3005: ZB = zn_lt(n3003, zn_splat(P8::from_raw(393216i32)));
    let n3006: ZB = zb_and(n3001, n3005);
    let n3007: ZB = zb_and(n3001, n3004);
    let n3008: ZN = zn_mul(n2991, zn_splat(P8::from_raw(524288i32)));
    let n3009: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n3008);
    let n3010: ZB = zn_eq(n2987, n3009);
    let n3011: ZB = zb_or(n3006, n3007);
    let n3012: ZB = zb_or(n3004, n3010);
    let n3013: ZB = zb_or(n3002, n3011);
    let n3014: ZB = zb_and(n2999, n3012);
    let n3015: ZB = zb_not(n3014);
    let n3016: ZB = zb_and(n3013, n3014);
    let n3017: ZB = zb_and(n3013, n3015);
    let n3018: ZB = zn_ge(n2967, zn_splat(P8::from_raw(0i32)));
    let n3019: ZB = zb_or(n3016, n3017);
    let n3020: ZB = zb_and(n3014, n3018);
    let n3021: ZB = zb_not(n3020);
    let n3022: ZB = zb_and(n3019, n3020);
    let n3023: ZB = zb_and(n3019, n3021);
    let n3024: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n2998);
    let n3025: ZB = zb_not(n3024);
    let n3026: ZB = zb_and(n3023, n3024);
    let n3027: ZB = zb_and(n3023, n3025);
    let n3028: ZN = zn_rem(n2971, zn_splat(P8::from_raw(524288i32)));
    let n3029: ZB = zn_le(n3028, zn_splat(P8::from_raw(131072i32)));
    let n3030: ZB = zb_or(n3026, n3027);
    let n3031: ZB = zb_and(n3024, n3029);
    let n3032: ZB = zb_not(n3031);
    let n3033: ZB = zb_and(n3030, n3031);
    let n3034: ZB = zb_and(n3030, n3032);
    let n3035: ZB = zn_le(n2967, zn_splat(P8::from_raw(0i32)));
    let n3036: ZB = zb_or(n3033, n3034);
    let n3037: ZB = zb_and(n3031, n3035);
    let n3038: ZB = zb_not(n3037);
    let n3039: ZB = zb_and(n3036, n3037);
    let n3040: ZB = zb_and(n3036, n3038);
    let n3041: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n2998);
    let n3042: ZB = zb_not(n3041);
    let n3043: ZB = zb_and(n3040, n3041);
    let n3044: ZB = zb_and(n3040, n3042);
    let n3045: ZN = zn_rem(n2970, zn_splat(P8::from_raw(524288i32)));
    let n3046: ZB = zn_le(n3045, zn_splat(P8::from_raw(131072i32)));
    let n3047: ZB = zb_or(n3043, n3044);
    let n3048: ZB = zb_and(n3041, n3046);
    let n3049: ZB = zb_not(n3048);
    let n3050: ZB = zb_and(n3047, n3048);
    let n3051: ZB = zb_and(n3047, n3049);
    let n3052: ZB = zn_le(n2966, zn_splat(P8::from_raw(0i32)));
    let n3053: ZB = zb_or(n3050, n3051);
    let n3054: ZB = zb_and(n3048, n3052);
    let n3055: ZB = zb_not(n3054);
    let n3056: ZB = zb_and(n3053, n3054);
    let n3057: ZB = zb_and(n3053, n3055);
    let n3058: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n2998);
    let n3059: ZB = zb_not(n3058);
    let n3060: ZB = zb_and(n3057, n3058);
    let n3061: ZB = zb_and(n3057, n3059);
    let n3062: ZN = zn_rem(n2976, zn_splat(P8::from_raw(524288i32)));
    let n3063: ZB = zn_ge(n3062, zn_splat(P8::from_raw(393216i32)));
    let n3064: ZB = zn_lt(n3062, zn_splat(P8::from_raw(393216i32)));
    let n3065: ZB = zb_and(n3060, n3064);
    let n3066: ZB = zb_and(n3060, n3063);
    let n3067: ZN = zn_mul(n2979, zn_splat(P8::from_raw(524288i32)));
    let n3068: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n3067);
    let n3069: ZB = zn_eq(n2975, n3068);
    let n3070: ZB = zb_or(n3065, n3066);
    let n3071: ZB = zb_or(n3063, n3069);
    let n3072: ZB = zb_or(n3061, n3070);
    let n3073: ZB = zb_and(n3058, n3071);
    let n3074: ZB = zb_not(n3073);
    let n3075: ZB = zb_and(n3072, n3073);
    let n3076: ZB = zb_and(n3072, n3074);
    let n3077: ZB = zn_ge(n2966, zn_splat(P8::from_raw(0i32)));
    let n3078: ZB = zb_or(n3075, n3076);
    let n3079: ZB = zb_and(n3073, n3077);
    let n3080: ZB = zb_not(n3079);
    let n3081: ZB = zb_and(n3078, n3079);
    let n3082: ZB = zb_and(n3078, n3080);
    let n3083: ZB = zb_or(n3056, n3081);
    let n3084: ZB = zb_or(n3039, n3083);
    let n3085: ZB = zb_or(n3022, n3084);
    let n3086: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n2986);
    let n3087: ZB = zn_le(n3086, n2990);
    let n3088: ZB = zn_gt(n3086, n2990);
    let n3089: ZB = zb_and(n3082, n3087);
    let n3090: ZB = zb_and(n3082, n3088);
    let n3091: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n3086);
    let n3092: ZN = zn_mget(g.cart, n2996, n3091);
    let n3093: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n3092);
    let n3094: ZB = zb_not(n3093);
    let n3095: ZB = zb_and(n3089, n3093);
    let n3096: ZB = zb_and(n3089, n3094);
    let n3097: ZB = zb_and(n3005, n3095);
    let n3098: ZB = zb_and(n3004, n3095);
    let n3099: ZN = zn_mul(n3086, zn_splat(P8::from_raw(524288i32)));
    let n3100: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n3099);
    let n3101: ZB = zn_eq(n2987, n3100);
    let n3102: ZB = zb_or(n3097, n3098);
    let n3103: ZB = zb_or(n3004, n3101);
    let n3104: ZB = zb_or(n3096, n3102);
    let n3105: ZB = zb_and(n3093, n3103);
    let n3106: ZB = zb_not(n3105);
    let n3107: ZB = zb_and(n3104, n3105);
    let n3108: ZB = zb_and(n3104, n3106);
    let n3109: ZB = zb_or(n3107, n3108);
    let n3110: ZB = zb_and(n3018, n3105);
    let n3111: ZB = zb_not(n3110);
    let n3112: ZB = zb_and(n3109, n3110);
    let n3113: ZB = zb_and(n3109, n3111);
    let n3114: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n3092);
    let n3115: ZB = zb_not(n3114);
    let n3116: ZB = zb_and(n3113, n3114);
    let n3117: ZB = zb_and(n3113, n3115);
    let n3118: ZB = zb_or(n3116, n3117);
    let n3119: ZB = zb_and(n3029, n3114);
    let n3120: ZB = zb_not(n3119);
    let n3121: ZB = zb_and(n3118, n3119);
    let n3122: ZB = zb_and(n3118, n3120);
    let n3123: ZB = zb_or(n3121, n3122);
    let n3124: ZB = zb_and(n3035, n3119);
    let n3125: ZB = zb_not(n3124);
    let n3126: ZB = zb_and(n3123, n3124);
    let n3127: ZB = zb_and(n3123, n3125);
    let n3128: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n3092);
    let n3129: ZB = zb_not(n3128);
    let n3130: ZB = zb_and(n3127, n3128);
    let n3131: ZB = zb_and(n3127, n3129);
    let n3132: ZB = zb_or(n3130, n3131);
    let n3133: ZB = zb_and(n3046, n3128);
    let n3134: ZB = zb_not(n3133);
    let n3135: ZB = zb_and(n3132, n3133);
    let n3136: ZB = zb_and(n3132, n3134);
    let n3137: ZB = zb_or(n3135, n3136);
    let n3138: ZB = zb_and(n3052, n3133);
    let n3139: ZB = zb_not(n3138);
    let n3140: ZB = zb_and(n3137, n3138);
    let n3141: ZB = zb_and(n3137, n3139);
    let n3142: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n3092);
    let n3143: ZB = zb_not(n3142);
    let n3144: ZB = zb_and(n3141, n3142);
    let n3145: ZB = zb_and(n3141, n3143);
    let n3146: ZB = zb_and(n3064, n3144);
    let n3147: ZB = zb_and(n3063, n3144);
    let n3148: ZB = zb_or(n3146, n3147);
    let n3149: ZB = zb_or(n3145, n3148);
    let n3150: ZB = zb_and(n3071, n3142);
    let n3151: ZB = zb_not(n3150);
    let n3152: ZB = zb_and(n3149, n3150);
    let n3153: ZB = zb_and(n3149, n3151);
    let n3154: ZB = zb_or(n3152, n3153);
    let n3155: ZB = zb_and(n3077, n3150);
    let n3156: ZB = zb_not(n3155);
    let n3157: ZB = zb_and(n3154, n3155);
    let n3158: ZB = zb_and(n3154, n3156);
    let n3159: ZB = zb_or(n3140, n3157);
    let n3160: ZB = zb_or(n3126, n3159);
    let n3161: ZB = zb_or(n3112, n3160);
    let n3162: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n2986);
    let n3163: ZB = zn_le(n3162, n2990);
    let n3164: ZB = zn_gt(n3162, n2990);
    let n3165: ZB = zb_and(n3158, n3163);
    let n3166: ZB = zb_and(n3158, n3164);
    let n3167: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n3162);
    let n3168: ZN = zn_mget(g.cart, n2996, n3167);
    let n3169: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n3168);
    let n3170: ZB = zb_not(n3169);
    let n3171: ZB = zb_and(n3165, n3169);
    let n3172: ZB = zb_and(n3165, n3170);
    let n3173: ZB = zb_and(n3005, n3171);
    let n3174: ZB = zb_and(n3004, n3171);
    let n3175: ZN = zn_mul(n3162, zn_splat(P8::from_raw(524288i32)));
    let n3176: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n3175);
    let n3177: ZB = zn_eq(n2987, n3176);
    let n3178: ZB = zb_or(n3173, n3174);
    let n3179: ZB = zb_or(n3004, n3177);
    let n3180: ZB = zb_or(n3172, n3178);
    let n3181: ZB = zb_and(n3169, n3179);
    let n3182: ZB = zb_not(n3181);
    let n3183: ZB = zb_and(n3180, n3181);
    let n3184: ZB = zb_and(n3180, n3182);
    let n3185: ZB = zb_or(n3183, n3184);
    let n3186: ZB = zb_and(n3018, n3181);
    let n3187: ZB = zb_not(n3186);
    let n3188: ZB = zb_and(n3185, n3186);
    let n3189: ZB = zb_and(n3185, n3187);
    let n3190: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n3168);
    let n3191: ZB = zb_not(n3190);
    let n3192: ZB = zb_and(n3189, n3190);
    let n3193: ZB = zb_and(n3189, n3191);
    let n3194: ZB = zb_or(n3192, n3193);
    let n3195: ZB = zb_and(n3029, n3190);
    let n3196: ZB = zb_not(n3195);
    let n3197: ZB = zb_and(n3194, n3195);
    let n3198: ZB = zb_and(n3194, n3196);
    let n3199: ZB = zb_or(n3197, n3198);
    let n3200: ZB = zb_and(n3035, n3195);
    let n3201: ZB = zb_not(n3200);
    let n3202: ZB = zb_and(n3199, n3200);
    let n3203: ZB = zb_and(n3199, n3201);
    let n3204: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n3168);
    let n3205: ZB = zb_not(n3204);
    let n3206: ZB = zb_and(n3203, n3204);
    let n3207: ZB = zb_and(n3203, n3205);
    let n3208: ZB = zb_or(n3206, n3207);
    let n3209: ZB = zb_and(n3046, n3204);
    let n3210: ZB = zb_not(n3209);
    let n3211: ZB = zb_and(n3208, n3209);
    let n3212: ZB = zb_and(n3208, n3210);
    let n3213: ZB = zb_or(n3211, n3212);
    let n3214: ZB = zb_and(n3052, n3209);
    let n3215: ZB = zb_not(n3214);
    let n3216: ZB = zb_and(n3213, n3214);
    let n3217: ZB = zb_and(n3213, n3215);
    let n3218: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n3168);
    let n3219: ZB = zb_not(n3218);
    let n3220: ZB = zb_and(n3217, n3218);
    let n3221: ZB = zb_and(n3217, n3219);
    let n3222: ZB = zb_and(n3064, n3220);
    let n3223: ZB = zb_and(n3063, n3220);
    let n3224: ZB = zb_or(n3222, n3223);
    let n3225: ZB = zb_or(n3221, n3224);
    let n3226: ZB = zb_and(n3071, n3218);
    let n3227: ZB = zb_not(n3226);
    let n3228: ZB = zb_and(n3225, n3226);
    let n3229: ZB = zb_and(n3225, n3227);
    let n3230: ZB = zb_or(n3228, n3229);
    let n3231: ZB = zb_and(n3077, n3226);
    let n3232: ZB = zb_not(n3231);
    let n3233: ZB = zb_and(n3230, n3231);
    let n3234: ZB = zb_and(n3230, n3232);
    let n3235: ZB = zb_or(n3216, n3233);
    let n3236: ZB = zb_or(n3202, n3235);
    let n3237: ZB = zb_or(n3188, n3236);
    let n3238: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n2986);
    let n3239: ZB = zn_gt(n3238, n2990);
    let n3240: ZB = zb_and(n2969, n3239);
    let n3241: ZB = zb_or(n3166, n3234);
    let n3242: ZB = zsel_b(n3164, n2969, n3240);
    let n3243: ZB = zb_or(n3161, n3237);
    let n3244: ZB = zb_or(n3090, n3241);
    let n3245: ZB = zsel_b(n3088, n2969, n3242);
    let n3246: ZB = zb_or(n3085, n3243);
    let n3247: ZB = zb_or(n2995, n3244);
    let n3248: ZB = zsel_b(n2993, n2969, n3245);
    let n3249: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n2974);
    let n3250: ZB = zn_le(n3249, n2978);
    let n3251: ZB = zn_gt(n3249, n2978);
    let n3252: ZB = zb_and(n3247, n3250);
    let n3253: ZB = zb_and(n3247, n3251);
    let n3254: ZB = zb_and(n2992, n3252);
    let n3255: ZB = zb_and(n2993, n3252);
    let n3256: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n3249);
    let n3257: ZN = zn_mget(g.cart, n3256, n2997);
    let n3258: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n3257);
    let n3259: ZB = zb_not(n3258);
    let n3260: ZB = zb_and(n3254, n3258);
    let n3261: ZB = zb_and(n3254, n3259);
    let n3262: ZB = zb_and(n3005, n3260);
    let n3263: ZB = zb_and(n3004, n3260);
    let n3264: ZB = zb_or(n3262, n3263);
    let n3265: ZB = zb_or(n3261, n3264);
    let n3266: ZB = zb_and(n3012, n3258);
    let n3267: ZB = zb_not(n3266);
    let n3268: ZB = zb_and(n3265, n3266);
    let n3269: ZB = zb_and(n3265, n3267);
    let n3270: ZB = zb_or(n3268, n3269);
    let n3271: ZB = zb_and(n3018, n3266);
    let n3272: ZB = zb_not(n3271);
    let n3273: ZB = zb_and(n3270, n3271);
    let n3274: ZB = zb_and(n3270, n3272);
    let n3275: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n3257);
    let n3276: ZB = zb_not(n3275);
    let n3277: ZB = zb_and(n3274, n3275);
    let n3278: ZB = zb_and(n3274, n3276);
    let n3279: ZB = zb_or(n3277, n3278);
    let n3280: ZB = zb_and(n3029, n3275);
    let n3281: ZB = zb_not(n3280);
    let n3282: ZB = zb_and(n3279, n3280);
    let n3283: ZB = zb_and(n3279, n3281);
    let n3284: ZB = zb_or(n3282, n3283);
    let n3285: ZB = zb_and(n3035, n3280);
    let n3286: ZB = zb_not(n3285);
    let n3287: ZB = zb_and(n3284, n3285);
    let n3288: ZB = zb_and(n3284, n3286);
    let n3289: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n3257);
    let n3290: ZB = zb_not(n3289);
    let n3291: ZB = zb_and(n3288, n3289);
    let n3292: ZB = zb_and(n3288, n3290);
    let n3293: ZB = zb_or(n3291, n3292);
    let n3294: ZB = zb_and(n3046, n3289);
    let n3295: ZB = zb_not(n3294);
    let n3296: ZB = zb_and(n3293, n3294);
    let n3297: ZB = zb_and(n3293, n3295);
    let n3298: ZB = zb_or(n3296, n3297);
    let n3299: ZB = zb_and(n3052, n3294);
    let n3300: ZB = zb_not(n3299);
    let n3301: ZB = zb_and(n3298, n3299);
    let n3302: ZB = zb_and(n3298, n3300);
    let n3303: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n3257);
    let n3304: ZB = zb_not(n3303);
    let n3305: ZB = zb_and(n3302, n3303);
    let n3306: ZB = zb_and(n3302, n3304);
    let n3307: ZB = zb_and(n3064, n3305);
    let n3308: ZB = zb_and(n3063, n3305);
    let n3309: ZN = zn_mul(n3249, zn_splat(P8::from_raw(524288i32)));
    let n3310: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n3309);
    let n3311: ZB = zn_eq(n2975, n3310);
    let n3312: ZB = zb_or(n3307, n3308);
    let n3313: ZB = zb_or(n3063, n3311);
    let n3314: ZB = zb_or(n3306, n3312);
    let n3315: ZB = zb_and(n3303, n3313);
    let n3316: ZB = zb_not(n3315);
    let n3317: ZB = zb_and(n3314, n3315);
    let n3318: ZB = zb_and(n3314, n3316);
    let n3319: ZB = zb_or(n3317, n3318);
    let n3320: ZB = zb_and(n3077, n3315);
    let n3321: ZB = zb_not(n3320);
    let n3322: ZB = zb_and(n3319, n3320);
    let n3323: ZB = zb_and(n3319, n3321);
    let n3324: ZB = zb_or(n3301, n3322);
    let n3325: ZB = zb_or(n3287, n3324);
    let n3326: ZB = zb_or(n3273, n3325);
    let n3327: ZB = zb_and(n3087, n3323);
    let n3328: ZB = zb_and(n3088, n3323);
    let n3329: ZN = zn_mget(g.cart, n3256, n3091);
    let n3330: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n3329);
    let n3331: ZB = zb_not(n3330);
    let n3332: ZB = zb_and(n3327, n3330);
    let n3333: ZB = zb_and(n3327, n3331);
    let n3334: ZB = zb_and(n3005, n3332);
    let n3335: ZB = zb_and(n3004, n3332);
    let n3336: ZB = zb_or(n3334, n3335);
    let n3337: ZB = zb_or(n3333, n3336);
    let n3338: ZB = zb_and(n3103, n3330);
    let n3339: ZB = zb_not(n3338);
    let n3340: ZB = zb_and(n3337, n3338);
    let n3341: ZB = zb_and(n3337, n3339);
    let n3342: ZB = zb_or(n3340, n3341);
    let n3343: ZB = zb_and(n3018, n3338);
    let n3344: ZB = zb_not(n3343);
    let n3345: ZB = zb_and(n3342, n3343);
    let n3346: ZB = zb_and(n3342, n3344);
    let n3347: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n3329);
    let n3348: ZB = zb_not(n3347);
    let n3349: ZB = zb_and(n3346, n3347);
    let n3350: ZB = zb_and(n3346, n3348);
    let n3351: ZB = zb_or(n3349, n3350);
    let n3352: ZB = zb_and(n3029, n3347);
    let n3353: ZB = zb_not(n3352);
    let n3354: ZB = zb_and(n3351, n3352);
    let n3355: ZB = zb_and(n3351, n3353);
    let n3356: ZB = zb_or(n3354, n3355);
    let n3357: ZB = zb_and(n3035, n3352);
    let n3358: ZB = zb_not(n3357);
    let n3359: ZB = zb_and(n3356, n3357);
    let n3360: ZB = zb_and(n3356, n3358);
    let n3361: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n3329);
    let n3362: ZB = zb_not(n3361);
    let n3363: ZB = zb_and(n3360, n3361);
    let n3364: ZB = zb_and(n3360, n3362);
    let n3365: ZB = zb_or(n3363, n3364);
    let n3366: ZB = zb_and(n3046, n3361);
    let n3367: ZB = zb_not(n3366);
    let n3368: ZB = zb_and(n3365, n3366);
    let n3369: ZB = zb_and(n3365, n3367);
    let n3370: ZB = zb_or(n3368, n3369);
    let n3371: ZB = zb_and(n3052, n3366);
    let n3372: ZB = zb_not(n3371);
    let n3373: ZB = zb_and(n3370, n3371);
    let n3374: ZB = zb_and(n3370, n3372);
    let n3375: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n3329);
    let n3376: ZB = zb_not(n3375);
    let n3377: ZB = zb_and(n3374, n3375);
    let n3378: ZB = zb_and(n3374, n3376);
    let n3379: ZB = zb_and(n3064, n3377);
    let n3380: ZB = zb_and(n3063, n3377);
    let n3381: ZB = zb_or(n3379, n3380);
    let n3382: ZB = zb_or(n3378, n3381);
    let n3383: ZB = zb_and(n3313, n3375);
    let n3384: ZB = zb_not(n3383);
    let n3385: ZB = zb_and(n3382, n3383);
    let n3386: ZB = zb_and(n3382, n3384);
    let n3387: ZB = zb_or(n3385, n3386);
    let n3388: ZB = zb_and(n3077, n3383);
    let n3389: ZB = zb_not(n3388);
    let n3390: ZB = zb_and(n3387, n3388);
    let n3391: ZB = zb_and(n3387, n3389);
    let n3392: ZB = zb_or(n3373, n3390);
    let n3393: ZB = zb_or(n3359, n3392);
    let n3394: ZB = zb_or(n3345, n3393);
    let n3395: ZB = zb_and(n3163, n3391);
    let n3396: ZB = zb_and(n3164, n3391);
    let n3397: ZN = zn_mget(g.cart, n3256, n3167);
    let n3398: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n3397);
    let n3399: ZB = zb_not(n3398);
    let n3400: ZB = zb_and(n3395, n3398);
    let n3401: ZB = zb_and(n3395, n3399);
    let n3402: ZB = zb_and(n3005, n3400);
    let n3403: ZB = zb_and(n3004, n3400);
    let n3404: ZB = zb_or(n3402, n3403);
    let n3405: ZB = zb_or(n3401, n3404);
    let n3406: ZB = zb_and(n3179, n3398);
    let n3407: ZB = zb_not(n3406);
    let n3408: ZB = zb_and(n3405, n3406);
    let n3409: ZB = zb_and(n3405, n3407);
    let n3410: ZB = zb_or(n3408, n3409);
    let n3411: ZB = zb_and(n3018, n3406);
    let n3412: ZB = zb_not(n3411);
    let n3413: ZB = zb_and(n3410, n3411);
    let n3414: ZB = zb_and(n3410, n3412);
    let n3415: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n3397);
    let n3416: ZB = zb_not(n3415);
    let n3417: ZB = zb_and(n3414, n3415);
    let n3418: ZB = zb_and(n3414, n3416);
    let n3419: ZB = zb_or(n3417, n3418);
    let n3420: ZB = zb_and(n3029, n3415);
    let n3421: ZB = zb_not(n3420);
    let n3422: ZB = zb_and(n3419, n3420);
    let n3423: ZB = zb_and(n3419, n3421);
    let n3424: ZB = zb_or(n3422, n3423);
    let n3425: ZB = zb_and(n3035, n3420);
    let n3426: ZB = zb_not(n3425);
    let n3427: ZB = zb_and(n3424, n3425);
    let n3428: ZB = zb_and(n3424, n3426);
    let n3429: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n3397);
    let n3430: ZB = zb_not(n3429);
    let n3431: ZB = zb_and(n3428, n3429);
    let n3432: ZB = zb_and(n3428, n3430);
    let n3433: ZB = zb_or(n3431, n3432);
    let n3434: ZB = zb_and(n3046, n3429);
    let n3435: ZB = zb_not(n3434);
    let n3436: ZB = zb_and(n3433, n3434);
    let n3437: ZB = zb_and(n3433, n3435);
    let n3438: ZB = zb_or(n3436, n3437);
    let n3439: ZB = zb_and(n3052, n3434);
    let n3440: ZB = zb_not(n3439);
    let n3441: ZB = zb_and(n3438, n3439);
    let n3442: ZB = zb_and(n3438, n3440);
    let n3443: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n3397);
    let n3444: ZB = zb_not(n3443);
    let n3445: ZB = zb_and(n3442, n3443);
    let n3446: ZB = zb_and(n3442, n3444);
    let n3447: ZB = zb_and(n3064, n3445);
    let n3448: ZB = zb_and(n3063, n3445);
    let n3449: ZB = zb_or(n3447, n3448);
    let n3450: ZB = zb_or(n3446, n3449);
    let n3451: ZB = zb_and(n3313, n3443);
    let n3452: ZB = zb_not(n3451);
    let n3453: ZB = zb_and(n3450, n3451);
    let n3454: ZB = zb_and(n3450, n3452);
    let n3455: ZB = zb_or(n3453, n3454);
    let n3456: ZB = zb_and(n3077, n3451);
    let n3457: ZB = zb_not(n3456);
    let n3458: ZB = zb_and(n3455, n3456);
    let n3459: ZB = zb_and(n3455, n3457);
    let n3460: ZB = zb_or(n3441, n3458);
    let n3461: ZB = zb_or(n3427, n3460);
    let n3462: ZB = zb_or(n3413, n3461);
    let n3463: ZB = zb_and(n3239, n3248);
    let n3464: ZB = zb_or(n3396, n3459);
    let n3465: ZB = zsel_b(n3164, n3248, n3463);
    let n3466: ZB = zb_or(n3394, n3462);
    let n3467: ZB = zb_or(n3328, n3464);
    let n3468: ZB = zsel_b(n3088, n3248, n3465);
    let n3469: ZB = zb_or(n3326, n3466);
    let n3470: ZB = zb_or(n3255, n3467);
    let n3471: ZB = zsel_b(n2993, n3248, n3468);
    let n3472: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n2974);
    let n3473: ZB = zn_le(n3472, n2978);
    let n3474: ZB = zn_gt(n3472, n2978);
    let n3475: ZB = zb_and(n3470, n3473);
    let n3476: ZB = zb_and(n3470, n3474);
    let n3477: ZB = zb_and(n2992, n3475);
    let n3478: ZB = zb_and(n2993, n3475);
    let n3479: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n3472);
    let n3480: ZN = zn_mget(g.cart, n3479, n2997);
    let n3481: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n3480);
    let n3482: ZB = zb_not(n3481);
    let n3483: ZB = zb_and(n3477, n3481);
    let n3484: ZB = zb_and(n3477, n3482);
    let n3485: ZB = zb_and(n3005, n3483);
    let n3486: ZB = zb_and(n3004, n3483);
    let n3487: ZB = zb_or(n3485, n3486);
    let n3488: ZB = zb_or(n3484, n3487);
    let n3489: ZB = zb_and(n3012, n3481);
    let n3490: ZB = zb_not(n3489);
    let n3491: ZB = zb_and(n3488, n3489);
    let n3492: ZB = zb_and(n3488, n3490);
    let n3493: ZB = zb_or(n3491, n3492);
    let n3494: ZB = zb_and(n3018, n3489);
    let n3495: ZB = zb_not(n3494);
    let n3496: ZB = zb_and(n3493, n3494);
    let n3497: ZB = zb_and(n3493, n3495);
    let n3498: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n3480);
    let n3499: ZB = zb_not(n3498);
    let n3500: ZB = zb_and(n3497, n3498);
    let n3501: ZB = zb_and(n3497, n3499);
    let n3502: ZB = zb_or(n3500, n3501);
    let n3503: ZB = zb_and(n3029, n3498);
    let n3504: ZB = zb_not(n3503);
    let n3505: ZB = zb_and(n3502, n3503);
    let n3506: ZB = zb_and(n3502, n3504);
    let n3507: ZB = zb_or(n3505, n3506);
    let n3508: ZB = zb_and(n3035, n3503);
    let n3509: ZB = zb_not(n3508);
    let n3510: ZB = zb_and(n3507, n3508);
    let n3511: ZB = zb_and(n3507, n3509);
    let n3512: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n3480);
    let n3513: ZB = zb_not(n3512);
    let n3514: ZB = zb_and(n3511, n3512);
    let n3515: ZB = zb_and(n3511, n3513);
    let n3516: ZB = zb_or(n3514, n3515);
    let n3517: ZB = zb_and(n3046, n3512);
    let n3518: ZB = zb_not(n3517);
    let n3519: ZB = zb_and(n3516, n3517);
    let n3520: ZB = zb_and(n3516, n3518);
    let n3521: ZB = zb_or(n3519, n3520);
    let n3522: ZB = zb_and(n3052, n3517);
    let n3523: ZB = zb_not(n3522);
    let n3524: ZB = zb_and(n3521, n3522);
    let n3525: ZB = zb_and(n3521, n3523);
    let n3526: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n3480);
    let n3527: ZB = zb_not(n3526);
    let n3528: ZB = zb_and(n3525, n3526);
    let n3529: ZB = zb_and(n3525, n3527);
    let n3530: ZB = zb_and(n3064, n3528);
    let n3531: ZB = zb_and(n3063, n3528);
    let n3532: ZN = zn_mul(n3472, zn_splat(P8::from_raw(524288i32)));
    let n3533: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n3532);
    let n3534: ZB = zn_eq(n2975, n3533);
    let n3535: ZB = zb_or(n3530, n3531);
    let n3536: ZB = zb_or(n3063, n3534);
    let n3537: ZB = zb_or(n3529, n3535);
    let n3538: ZB = zb_and(n3526, n3536);
    let n3539: ZB = zb_not(n3538);
    let n3540: ZB = zb_and(n3537, n3538);
    let n3541: ZB = zb_and(n3537, n3539);
    let n3542: ZB = zb_or(n3540, n3541);
    let n3543: ZB = zb_and(n3077, n3538);
    let n3544: ZB = zb_not(n3543);
    let n3545: ZB = zb_and(n3542, n3543);
    let n3546: ZB = zb_and(n3542, n3544);
    let n3547: ZB = zb_or(n3524, n3545);
    let n3548: ZB = zb_or(n3510, n3547);
    let n3549: ZB = zb_or(n3496, n3548);
    let n3550: ZB = zb_and(n3087, n3546);
    let n3551: ZB = zb_and(n3088, n3546);
    let n3552: ZN = zn_mget(g.cart, n3479, n3091);
    let n3553: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n3552);
    let n3554: ZB = zb_not(n3553);
    let n3555: ZB = zb_and(n3550, n3553);
    let n3556: ZB = zb_and(n3550, n3554);
    let n3557: ZB = zb_and(n3005, n3555);
    let n3558: ZB = zb_and(n3004, n3555);
    let n3559: ZB = zb_or(n3557, n3558);
    let n3560: ZB = zb_or(n3556, n3559);
    let n3561: ZB = zb_and(n3103, n3553);
    let n3562: ZB = zb_not(n3561);
    let n3563: ZB = zb_and(n3560, n3561);
    let n3564: ZB = zb_and(n3560, n3562);
    let n3565: ZB = zb_or(n3563, n3564);
    let n3566: ZB = zb_and(n3018, n3561);
    let n3567: ZB = zb_not(n3566);
    let n3568: ZB = zb_and(n3565, n3566);
    let n3569: ZB = zb_and(n3565, n3567);
    let n3570: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n3552);
    let n3571: ZB = zb_not(n3570);
    let n3572: ZB = zb_and(n3569, n3570);
    let n3573: ZB = zb_and(n3569, n3571);
    let n3574: ZB = zb_or(n3572, n3573);
    let n3575: ZB = zb_and(n3029, n3570);
    let n3576: ZB = zb_not(n3575);
    let n3577: ZB = zb_and(n3574, n3575);
    let n3578: ZB = zb_and(n3574, n3576);
    let n3579: ZB = zb_or(n3577, n3578);
    let n3580: ZB = zb_and(n3035, n3575);
    let n3581: ZB = zb_not(n3580);
    let n3582: ZB = zb_and(n3579, n3580);
    let n3583: ZB = zb_and(n3579, n3581);
    let n3584: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n3552);
    let n3585: ZB = zb_not(n3584);
    let n3586: ZB = zb_and(n3583, n3584);
    let n3587: ZB = zb_and(n3583, n3585);
    let n3588: ZB = zb_or(n3586, n3587);
    let n3589: ZB = zb_and(n3046, n3584);
    let n3590: ZB = zb_not(n3589);
    let n3591: ZB = zb_and(n3588, n3589);
    let n3592: ZB = zb_and(n3588, n3590);
    let n3593: ZB = zb_or(n3591, n3592);
    let n3594: ZB = zb_and(n3052, n3589);
    let n3595: ZB = zb_not(n3594);
    let n3596: ZB = zb_and(n3593, n3594);
    let n3597: ZB = zb_and(n3593, n3595);
    let n3598: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n3552);
    let n3599: ZB = zb_not(n3598);
    let n3600: ZB = zb_and(n3597, n3598);
    let n3601: ZB = zb_and(n3597, n3599);
    let n3602: ZB = zb_and(n3064, n3600);
    let n3603: ZB = zb_and(n3063, n3600);
    let n3604: ZB = zb_or(n3602, n3603);
    let n3605: ZB = zb_or(n3601, n3604);
    let n3606: ZB = zb_and(n3536, n3598);
    let n3607: ZB = zb_not(n3606);
    let n3608: ZB = zb_and(n3605, n3606);
    let n3609: ZB = zb_and(n3605, n3607);
    let n3610: ZB = zb_or(n3608, n3609);
    let n3611: ZB = zb_and(n3077, n3606);
    let n3612: ZB = zb_not(n3611);
    let n3613: ZB = zb_and(n3610, n3611);
    let n3614: ZB = zb_and(n3610, n3612);
    let n3615: ZB = zb_or(n3596, n3613);
    let n3616: ZB = zb_or(n3582, n3615);
    let n3617: ZB = zb_or(n3568, n3616);
    let n3618: ZB = zb_and(n3163, n3614);
    let n3619: ZB = zb_and(n3164, n3614);
    let n3620: ZN = zn_mget(g.cart, n3479, n3167);
    let n3621: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n3620);
    let n3622: ZB = zb_not(n3621);
    let n3623: ZB = zb_and(n3618, n3621);
    let n3624: ZB = zb_and(n3618, n3622);
    let n3625: ZB = zb_and(n3005, n3623);
    let n3626: ZB = zb_and(n3004, n3623);
    let n3627: ZB = zb_or(n3625, n3626);
    let n3628: ZB = zb_or(n3624, n3627);
    let n3629: ZB = zb_and(n3179, n3621);
    let n3630: ZB = zb_not(n3629);
    let n3631: ZB = zb_and(n3628, n3629);
    let n3632: ZB = zb_and(n3628, n3630);
    let n3633: ZB = zb_or(n3631, n3632);
    let n3634: ZB = zb_and(n3018, n3629);
    let n3635: ZB = zb_not(n3634);
    let n3636: ZB = zb_and(n3633, n3634);
    let n3637: ZB = zb_and(n3633, n3635);
    let n3638: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n3620);
    let n3639: ZB = zb_not(n3638);
    let n3640: ZB = zb_and(n3637, n3638);
    let n3641: ZB = zb_and(n3637, n3639);
    let n3642: ZB = zb_or(n3640, n3641);
    let n3643: ZB = zb_and(n3029, n3638);
    let n3644: ZB = zb_not(n3643);
    let n3645: ZB = zb_and(n3642, n3643);
    let n3646: ZB = zb_and(n3642, n3644);
    let n3647: ZB = zb_or(n3645, n3646);
    let n3648: ZB = zb_and(n3035, n3643);
    let n3649: ZB = zb_not(n3648);
    let n3650: ZB = zb_and(n3647, n3648);
    let n3651: ZB = zb_and(n3647, n3649);
    let n3652: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n3620);
    let n3653: ZB = zb_not(n3652);
    let n3654: ZB = zb_and(n3651, n3652);
    let n3655: ZB = zb_and(n3651, n3653);
    let n3656: ZB = zb_or(n3654, n3655);
    let n3657: ZB = zb_and(n3046, n3652);
    let n3658: ZB = zb_not(n3657);
    let n3659: ZB = zb_and(n3656, n3657);
    let n3660: ZB = zb_and(n3656, n3658);
    let n3661: ZB = zb_or(n3659, n3660);
    let n3662: ZB = zb_and(n3052, n3657);
    let n3663: ZB = zb_not(n3662);
    let n3664: ZB = zb_and(n3661, n3662);
    let n3665: ZB = zb_and(n3661, n3663);
    let n3666: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n3620);
    let n3667: ZB = zb_not(n3666);
    let n3668: ZB = zb_and(n3665, n3666);
    let n3669: ZB = zb_and(n3665, n3667);
    let n3670: ZB = zb_and(n3064, n3668);
    let n3671: ZB = zb_and(n3063, n3668);
    let n3672: ZB = zb_or(n3670, n3671);
    let n3673: ZB = zb_or(n3669, n3672);
    let n3674: ZB = zb_and(n3536, n3666);
    let n3675: ZB = zb_not(n3674);
    let n3676: ZB = zb_and(n3673, n3674);
    let n3677: ZB = zb_and(n3673, n3675);
    let n3678: ZB = zb_or(n3676, n3677);
    let n3679: ZB = zb_and(n3077, n3674);
    let n3680: ZB = zb_not(n3679);
    let n3681: ZB = zb_and(n3678, n3679);
    let n3682: ZB = zb_and(n3678, n3680);
    let n3683: ZB = zb_or(n3664, n3681);
    let n3684: ZB = zb_or(n3650, n3683);
    let n3685: ZB = zb_or(n3636, n3684);
    let n3686: ZB = zb_and(n3239, n3471);
    let n3687: ZB = zb_or(n3619, n3682);
    let n3688: ZB = zsel_b(n3164, n3471, n3686);
    let n3689: ZB = zb_or(n3617, n3685);
    let n3690: ZB = zb_or(n3551, n3687);
    let n3691: ZB = zsel_b(n3088, n3471, n3688);
    let n3692: ZB = zb_or(n3549, n3689);
    let n3693: ZB = zb_or(n3478, n3690);
    let n3694: ZB = zsel_b(n2993, n3471, n3691);
    let n3695: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n2974);
    let n3696: ZB = zn_gt(n3695, n2978);
    let n3697: ZB = zb_and(n3694, n3696);
    let n3698: ZB = zb_or(n3469, n3692);
    let n3699: ZB = zsel_b(n3469, n3248, n3471);
    let n3700: ZB = zb_or(n3476, n3693);
    let n3701: ZB = zsel_b(n3474, n3471, n3697);
    let n3702: ZB = zb_or(n3246, n3698);
    let n3703: ZB = zsel_b(n3246, n2969, n3699);
    let n3704: ZB = zb_or(n3253, n3700);
    let n3705: ZB = zsel_b(n3251, n3248, n3701);
    let n3706: ZB = zb_or(n2983, n3704);
    let n3707: ZB = zsel_b(n2981, n2969, n3705);
    let n3708: ZB = zn_gt(n2965, zn_splat(P8::from_raw(8388608i32)));
    let n3709: ZB = zn_le(n2965, zn_splat(P8::from_raw(8388608i32)));
    let n3710: ZB = zb_and(n3702, n3708);
    let n3711: ZB = zb_and(n3702, n3709);
    let n3712: ZB = zb_or(n3710, n3711);
    let n3713: ZB = zb_and(n3706, n3708);
    let n3714: ZB = zb_or(n3712, n3713);
    let n3715: ZB = zsel_b(n3712, n3703, n3707);
    let n3716: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n2970);
    let n3717: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n2971);
    let n3718: ZB = zn_tile_flag_at(g.cache, g.cart, n3716, n3717, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n3719: ZB = zb_not(n3718);
    let n3720: ZB = zb_and(n3714, n3719);
    let n3721: ZB = zb_and(n3714, n3718);
    let n3722: ZB = zb_or(n3720, n3721);
    let n3723: ZB = zb_and(n3719, n3722);
    let n3724: ZB = zb_and(n3718, n3722);
    let n3725: ZB = zb_or(n3723, n3724);
    let n3726: ZB = zn_lt(n2477, zn_splat(P8::from_raw(65536i32)));
    let n3727: ZB = zn_ge(n2477, zn_splat(P8::from_raw(65536i32)));
    let n3728: ZN = zsel_n(n3726, zn_splat(P8::from_raw(65536i32)), n2477);
    let n3729: ZN = zsel_n(n3718, n3728, n2477);
    let n3730: ZN = zsel_n(n3718, zn_splat(P8::from_raw(393216i32)), n1430);
    let n3731: ZB = zb_and(n3718, n3725);
    let n3732: ZB = zb_and(n3719, n3725);
    let n3733: ZB = zb_and(n3726, n3731);
    let n3734: ZB = zb_and(n3727, n3731);
    let n3735: ZB = zb_or(n3733, n3734);
    let n3736: ZB = zb_and(n1425, n3732);
    let n3737: ZB = zb_and(n1426, n3732);
    let n3738: ZB = zb_or(n3736, n3737);
    let n3739: ZB = zb_or(n3735, n3738);
    let n3740: ZB = zn_gt(n2966, r_c359);
    let n3741: ZB = zn_le(n2966, r_c359);
    let n3742: ZB = zn_gt(n2967, r_c360);
    let n3743: ZB = zn_le(n2967, r_c360);
    let n3744: ZN = zsel_n(n3719, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n3745: ZN = zn_abs(n2966);
    let n3746: ZB = zn_gt(n3745, zn_splat(P8::from_raw(65536i32)));
    let n3747: ZB = zn_le(n3745, zn_splat(P8::from_raw(65536i32)));
    let n3748: ZB = zn_gt(n2966, zn_splat(P8::from_raw(0i32)));
    let n3749: ZB = zn_lt(n2966, zn_splat(P8::from_raw(0i32)));
    let n3750: ZB = zn_gt(n2966, zn_splat(P8::from_raw(65536i32)));
    let n3751: ZB = zn_le(n2966, zn_splat(P8::from_raw(65536i32)));
    let n3752: ZN = zn_sub(n2966, zn_splat(P8::from_raw(9830i32)));
    let n3753: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n3752);
    let n3754: ZN = zn_add(zn_splat(P8::from_raw(9830i32)), n2966);
    let n3755: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n3754);
    let n3756: ZB = zn_gt(n2966, zn_splat(P8::from_raw(-65536i32)));
    let n3757: ZB = zn_le(n2966, zn_splat(P8::from_raw(-65536i32)));
    let n3758: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n3752);
    let n3759: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n3754);
    let n3760: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n3752);
    let n3761: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n3754);
    let n3762: ZN = zsel_n(n3756, n3758, n3759);
    let n3763: ZN = zsel_n(n3748, n3760, n3761);
    let n3764: ZN = zsel_n(n3750, n3753, n3755);
    let n3765: ZN = zsel_n(n3749, n3762, n3763);
    let n3766: ZN = zsel_n(n3748, n3764, n3765);
    let n3767: ZN = zn_sub(n2966, n3744);
    let n3768: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n3767);
    let n3769: ZN = zn_add(n2966, n3744);
    let n3770: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n3769);
    let n3771: ZN = zsel_n(n3748, n3768, n3770);
    let n3772: ZN = zsel_n(n3746, n3766, n3771);
    let n3773: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3772);
    let n3774: ZB = zb_not(n3773);
    let n3775: ZB = zn_lt(n3772, zn_splat(P8::from_raw(0i32)));
    let n3776: ZB = zsel_b(n3774, n3775, r_c361);
    let n3777: ZN = zn_abs(n2967);
    let n3778: ZB = zn_le(n3777, zn_splat(P8::from_raw(9830i32)));
    let n3779: ZB = zn_gt(n3777, zn_splat(P8::from_raw(9830i32)));
    let n3780: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n2971);
    let n3781: ZB = zn_gt(n2967, zn_splat(P8::from_raw(131072i32)));
    let n3782: ZB = zn_le(n2967, zn_splat(P8::from_raw(131072i32)));
    let n3783: ZB = zn_gt(n3730, zn_splat(P8::from_raw(0i32)));
    let n3784: ZB = zn_le(n3730, zn_splat(P8::from_raw(0i32)));
    let n3785: ZN = zn_add(zn_splat(P8::from_raw(-196608i32)), n2970);
    let n3786: ZB = zn_tile_flag_at(g.cache, g.cart, n3785, n3780, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n3787: ZB = zb_not(n3786);
    let n3788: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n2970);
    let n3789: ZB = zn_tile_flag_at(g.cache, g.cart, n3788, n3780, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n3790: ZB = zb_not(n3789);
    let n3791: ZN = zsel_n(n3789, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n3792: ZN = zsel_n(n3786, zn_splat(P8::from_raw(-65536i32)), n3791);
    let n3793: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3792);
    let n3794: ZB = zb_not(n3793);
    let n3795: ZB = zn_gt(n3729, zn_splat(P8::from_raw(0i32)));
    let n3796: ZB = zn_le(n3729, zn_splat(P8::from_raw(0i32)));
    let n3797: ZB = zb_not(n3776);
    let n3798: ZN = zsel_n(n3776, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n3799: ZB = zn_gt(n3798, zn_splat(P8::from_raw(0i32)));
    let n3800: ZB = zn_le(n3798, zn_splat(P8::from_raw(0i32)));
    let n3801: ZB = zn_lt(n3798, zn_splat(P8::from_raw(0i32)));
    let n3802: ZB = zn_ge(n3798, zn_splat(P8::from_raw(0i32)));
    let n3803: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3798);
    let n3804: ZB = zb_not(n3803);
    let n3805: ZB = zb_and(n1435, n3739);
    let n3806: ZB = zb_and(n1436, n3739);
    let n3807: ZB = zb_and(n3740, n3805);
    let n3808: ZB = zb_and(n3741, n3805);
    let n3809: ZB = zb_or(n3807, n3808);
    let n3810: ZB = zb_and(n3742, n3809);
    let n3811: ZB = zb_and(n3743, n3809);
    let n3812: ZB = zb_or(n3810, n3811);
    let n3813: ZB = zb_and(n3719, n3806);
    let n3814: ZB = zb_and(n3718, n3806);
    let n3815: ZB = zb_or(n3813, n3814);
    let n3816: ZB = zb_and(n3746, n3815);
    let n3817: ZB = zb_and(n3747, n3815);
    let n3818: ZB = zb_and(n3748, n3816);
    let n3819: ZB = zb_and(n3052, n3816);
    let n3820: ZB = zb_and(n3749, n3819);
    let n3821: ZB = zb_and(n3077, n3819);
    let n3822: ZB = zb_and(n3750, n3818);
    let n3823: ZB = zb_and(n3751, n3818);
    let n3824: ZB = zb_and(n3756, n3820);
    let n3825: ZB = zb_and(n3757, n3820);
    let n3826: ZB = zb_and(n3052, n3821);
    let n3827: ZB = zb_or(n3824, n3825);
    let n3828: ZB = zb_or(n3822, n3823);
    let n3829: ZB = zb_or(n3826, n3827);
    let n3830: ZB = zb_or(n3828, n3829);
    let n3831: ZB = zb_and(n3748, n3817);
    let n3832: ZB = zb_and(n3052, n3817);
    let n3833: ZB = zb_or(n3831, n3832);
    let n3834: ZB = zb_or(n3830, n3833);
    let n3835: ZB = zb_and(n3774, n3834);
    let n3836: ZB = zb_and(n3773, n3834);
    let n3837: ZB = zb_or(n3835, n3836);
    let n3838: ZB = zb_and(n3778, n3837);
    let n3839: ZB = zb_and(n3779, n3837);
    let n3840: ZB = zb_or(n3838, n3839);
    let n3841: ZB = zb_and(n3719, n3840);
    let n3842: ZB = zb_and(n3718, n3840);
    let n3843: ZB = zb_and(n3781, n3841);
    let n3844: ZB = zb_and(n3782, n3841);
    let n3845: ZB = zb_or(n3843, n3844);
    let n3846: ZB = zb_or(n3842, n3845);
    let n3847: ZB = zb_and(n3795, n3846);
    let n3848: ZB = zb_and(n3796, n3846);
    let n3849: ZB = zb_or(n3847, n3848);
    let n3850: ZB = zb_or(n3812, n3849);
    let n3851: ZB = zn_lt(n2965, zn_splat(P8::from_raw(-262144i32)));
    let n3852: ZB = zn_ge(n2965, zn_splat(P8::from_raw(-262144i32)));
    let n3853: ZB = zb_and(n3850, n3851);
    let n3854: ZB = zb_and(n3850, n3852);
    let n3855: ZB = zb_or(n3853, n3854);
    let n3858: ZN = zsel_n(n3708, n2082, n2081);
    let n3859: ZN = zsel_n(n3712, n3858, n2081);
    let n3861: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n3767);
    let n3862: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n3769);
    let n3863: ZN = zsel_n(n3756, n3861, n3862);
    let n3864: ZN = zsel_n(n3746, n3766, n3863);
    let n3865: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3864);
    let n3866: ZB = zb_not(n3865);
    let n3867: ZB = zn_lt(n3864, zn_splat(P8::from_raw(0i32)));
    let n3868: ZB = zsel_b(n3866, n3867, r_c361);
    let n3869: ZN = zn_add(zn_splat(P8::from_raw(-65536i32)), n2970);
    let n3870: ZB = zn_tile_flag_at(g.cache, g.cart, n3869, n3780, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n3871: ZB = zb_not(n3870);
    let n3872: ZN = zsel_n(n3870, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n3873: ZB = zn_gt(n2967, n3872);
    let n3874: ZB = zn_le(n2967, n3872);
    let n3875: ZB = zb_and(n3756, n3817);
    let n3876: ZB = zb_and(n3757, n3817);
    let n3877: ZB = zb_or(n3875, n3876);
    let n3878: ZB = zb_or(n3830, n3877);
    let n3879: ZB = zb_and(n3866, n3878);
    let n3880: ZB = zb_and(n3865, n3878);
    let n3881: ZB = zb_or(n3879, n3880);
    let n3882: ZB = zb_and(n3778, n3881);
    let n3883: ZB = zb_and(n3779, n3881);
    let n3884: ZB = zb_or(n3882, n3883);
    let n3885: ZB = zb_and(n3871, n3884);
    let n3886: ZB = zb_and(n3870, n3884);
    let n3887: ZB = zb_or(n3885, n3886);
    let n3888: ZB = zb_and(n3871, n3887);
    let n3889: ZB = zb_and(n3870, n3887);
    let n3890: ZB = zb_or(n3888, n3889);
    let n3891: ZB = zb_and(n3870, n3890);
    let n3892: ZB = zb_and(n3871, n3890);
    let n3893: ZB = zb_or(n3891, n3892);
    let n3894: ZB = zb_and(n3870, n3893);
    let n3895: ZB = zb_and(n3871, n3893);
    let n3896: ZB = zb_or(n3894, n3895);
    let n3897: ZB = zb_and(n3719, n3896);
    let n3898: ZB = zb_and(n3718, n3896);
    let n3899: ZB = zb_and(n3873, n3897);
    let n3900: ZB = zb_and(n3874, n3897);
    let n3901: ZB = zb_or(n3899, n3900);
    let n3902: ZB = zb_or(n3898, n3901);
    let n3903: ZB = zb_and(n3795, n3902);
    let n3904: ZB = zb_and(n3796, n3902);
    let n3905: ZB = zb_or(n3903, n3904);
    let n3906: ZB = zb_or(n3812, n3905);
    let n3907: ZB = zb_and(n3851, n3906);
    let n3908: ZB = zb_and(n3852, n3906);
    let n3909: ZB = zb_or(n3907, n3908);
    let n3912: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n3767);
    let n3913: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n3769);
    let n3914: ZN = zsel_n(n3750, n3912, n3913);
    let n3915: ZN = zsel_n(n3746, n3766, n3914);
    let n3916: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3915);
    let n3917: ZB = zb_not(n3916);
    let n3918: ZB = zn_lt(n3915, zn_splat(P8::from_raw(0i32)));
    let n3919: ZB = zsel_b(n3917, n3918, r_c361);
    let n3920: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n2970);
    let n3921: ZB = zn_tile_flag_at(g.cache, g.cart, n3920, n3780, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n3922: ZB = zb_not(n3921);
    let n3923: ZN = zsel_n(n3921, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n3924: ZB = zn_gt(n2967, n3923);
    let n3925: ZB = zn_le(n2967, n3923);
    let n3926: ZB = zb_and(n3750, n3817);
    let n3927: ZB = zb_and(n3751, n3817);
    let n3928: ZB = zb_or(n3926, n3927);
    let n3929: ZB = zb_or(n3830, n3928);
    let n3930: ZB = zb_and(n3917, n3929);
    let n3931: ZB = zb_and(n3916, n3929);
    let n3932: ZB = zb_or(n3930, n3931);
    let n3933: ZB = zb_and(n3778, n3932);
    let n3934: ZB = zb_and(n3779, n3932);
    let n3935: ZB = zb_or(n3933, n3934);
    let n3936: ZB = zb_and(n3922, n3935);
    let n3937: ZB = zb_and(n3921, n3935);
    let n3938: ZB = zb_or(n3936, n3937);
    let n3939: ZB = zb_and(n3922, n3938);
    let n3940: ZB = zb_and(n3921, n3938);
    let n3941: ZB = zb_or(n3939, n3940);
    let n3942: ZB = zb_and(n3921, n3941);
    let n3943: ZB = zb_and(n3922, n3941);
    let n3944: ZB = zb_or(n3942, n3943);
    let n3945: ZB = zb_and(n3921, n3944);
    let n3946: ZB = zb_and(n3922, n3944);
    let n3947: ZB = zb_or(n3945, n3946);
    let n3948: ZB = zb_and(n3719, n3947);
    let n3949: ZB = zb_and(n3718, n3947);
    let n3950: ZB = zb_and(n3924, n3948);
    let n3951: ZB = zb_and(n3925, n3948);
    let n3952: ZB = zb_or(n3950, n3951);
    let n3953: ZB = zb_or(n3949, n3952);
    let n3954: ZB = zb_and(n3795, n3953);
    let n3955: ZB = zb_and(n3796, n3953);
    let n3956: ZB = zb_or(n3954, n3955);
    let n3957: ZB = zb_or(n3812, n3956);
    let n3958: ZB = zb_and(n3851, n3957);
    let n3959: ZB = zb_and(n3852, n3957);
    let n3960: ZB = zb_or(n3958, n3959);
    let n3963: ZB = zb_and(n140, n3846);
    let n3964: ZB = zb_and(r_c294, n3846);
    let n3965: ZB = zb_and(n3783, n3963);
    let n3966: ZB = zb_and(n3784, n3963);
    let n3967: ZB = zb_and(n3787, n3966);
    let n3968: ZB = zb_and(n3786, n3966);
    let n3969: ZB = zb_or(n3967, n3968);
    let n3970: ZB = zb_and(n3787, n3969);
    let n3971: ZB = zb_and(n3786, n3969);
    let n3972: ZB = zb_or(n3970, n3971);
    let n3973: ZB = zb_and(n3786, n3972);
    let n3974: ZB = zb_and(n3787, n3972);
    let n3975: ZB = zb_and(n3790, n3974);
    let n3976: ZB = zb_and(n3789, n3974);
    let n3977: ZB = zb_or(n3975, n3976);
    let n3978: ZB = zb_and(n3790, n3977);
    let n3979: ZB = zb_and(n3789, n3977);
    let n3980: ZB = zb_or(n3978, n3979);
    let n3981: ZB = zb_and(n3789, n3980);
    let n3982: ZB = zb_and(n3790, n3980);
    let n3983: ZB = zb_or(n3981, n3982);
    let n3984: ZB = zb_or(n3973, n3983);
    let n3985: ZB = zb_and(n3794, n3984);
    let n3986: ZB = zb_and(n3793, n3984);
    let n3987: ZB = zb_or(n3985, n3986);
    let n3988: ZB = zb_or(n3965, n3987);
    let n3989: ZB = zb_or(n3964, n3988);
    let n3990: ZB = zb_and(n3795, n3989);
    let n3991: ZB = zb_and(n3796, n3989);
    let n3992: ZB = zb_or(n3990, n3991);
    let n3993: ZB = zb_or(n3812, n3992);
    let n3994: ZB = zb_and(n3851, n3993);
    let n3995: ZB = zb_and(n3852, n3993);
    let n3996: ZB = zb_or(n3994, n3995);
    let n3999: ZB = zb_and(n140, n3902);
    let n4000: ZB = zb_and(r_c294, n3902);
    let n4001: ZB = zb_and(n3783, n3999);
    let n4002: ZB = zb_and(n3784, n3999);
    let n4003: ZB = zb_and(n3787, n4002);
    let n4004: ZB = zb_and(n3786, n4002);
    let n4005: ZB = zb_or(n4003, n4004);
    let n4006: ZB = zb_and(n3787, n4005);
    let n4007: ZB = zb_and(n3786, n4005);
    let n4008: ZB = zb_or(n4006, n4007);
    let n4009: ZB = zb_and(n3786, n4008);
    let n4010: ZB = zb_and(n3787, n4008);
    let n4011: ZB = zb_and(n3790, n4010);
    let n4012: ZB = zb_and(n3789, n4010);
    let n4013: ZB = zb_or(n4011, n4012);
    let n4014: ZB = zb_and(n3790, n4013);
    let n4015: ZB = zb_and(n3789, n4013);
    let n4016: ZB = zb_or(n4014, n4015);
    let n4017: ZB = zb_and(n3789, n4016);
    let n4018: ZB = zb_and(n3790, n4016);
    let n4019: ZB = zb_or(n4017, n4018);
    let n4020: ZB = zb_or(n4009, n4019);
    let n4021: ZB = zb_and(n3794, n4020);
    let n4022: ZB = zb_and(n3793, n4020);
    let n4023: ZB = zb_or(n4021, n4022);
    let n4024: ZB = zb_or(n4001, n4023);
    let n4025: ZB = zb_or(n4000, n4024);
    let n4026: ZB = zb_and(n3795, n4025);
    let n4027: ZB = zb_and(n3796, n4025);
    let n4028: ZB = zb_or(n4026, n4027);
    let n4029: ZB = zb_or(n3812, n4028);
    let n4030: ZB = zb_and(n3851, n4029);
    let n4031: ZB = zb_and(n3852, n4029);
    let n4032: ZB = zb_or(n4030, n4031);
    let n4035: ZB = zb_and(n140, n3953);
    let n4036: ZB = zb_and(r_c294, n3953);
    let n4037: ZB = zb_and(n3783, n4035);
    let n4038: ZB = zb_and(n3784, n4035);
    let n4039: ZB = zb_and(n3787, n4038);
    let n4040: ZB = zb_and(n3786, n4038);
    let n4041: ZB = zb_or(n4039, n4040);
    let n4042: ZB = zb_and(n3787, n4041);
    let n4043: ZB = zb_and(n3786, n4041);
    let n4044: ZB = zb_or(n4042, n4043);
    let n4045: ZB = zb_and(n3786, n4044);
    let n4046: ZB = zb_and(n3787, n4044);
    let n4047: ZB = zb_and(n3790, n4046);
    let n4048: ZB = zb_and(n3789, n4046);
    let n4049: ZB = zb_or(n4047, n4048);
    let n4050: ZB = zb_and(n3790, n4049);
    let n4051: ZB = zb_and(n3789, n4049);
    let n4052: ZB = zb_or(n4050, n4051);
    let n4053: ZB = zb_and(n3789, n4052);
    let n4054: ZB = zb_and(n3790, n4052);
    let n4055: ZB = zb_or(n4053, n4054);
    let n4056: ZB = zb_or(n4045, n4055);
    let n4057: ZB = zb_and(n3794, n4056);
    let n4058: ZB = zb_and(n3793, n4056);
    let n4059: ZB = zb_or(n4057, n4058);
    let n4060: ZB = zb_or(n4037, n4059);
    let n4061: ZB = zb_or(n4036, n4060);
    let n4062: ZB = zb_and(n3795, n4061);
    let n4063: ZB = zb_and(n3796, n4061);
    let n4064: ZB = zb_or(n4062, n4063);
    let n4065: ZB = zb_or(n3812, n4064);
    let n4066: ZB = zb_and(n3851, n4065);
    let n4067: ZB = zb_and(n3852, n4065);
    let n4068: ZB = zb_or(n4066, n4067);
    let n4071: ZB = zb_and(n133, n3795);
    let n4072: ZB = zb_not(n4071);
    let n4073: ZN = zsel_n(n4071, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n4074: ZB = zb_or(r_c41, n4071);
    let n4075: ZN = zsel_n(n1435, r_c20, n4073);
    let n4076: ZB = zsel_b(n1435, r_c41, n4074);
    let n4077: ZB = zb_and(n3849, n4071);
    let n4078: ZB = zb_and(n3849, n4072);
    let n4079: ZB = zb_and(n3776, n4077);
    let n4080: ZB = zb_and(n3797, n4077);
    let n4081: ZB = zb_or(n4079, n4080);
    let n4082: ZB = zb_and(n3799, n4081);
    let n4083: ZB = zb_and(n3800, n4081);
    let n4084: ZB = zb_and(n3801, n4083);
    let n4085: ZB = zb_and(n3802, n4083);
    let n4086: ZB = zb_or(n4084, n4085);
    let n4087: ZB = zb_or(n4082, n4086);
    let n4088: ZB = zb_and(n3804, n4087);
    let n4089: ZB = zb_and(n3803, n4087);
    let n4090: ZB = zb_or(n4088, n4089);
    let n4091: ZB = zb_or(n4078, n4090);
    let n4092: ZB = zb_or(n3812, n4091);
    let n4093: ZB = zb_and(n3851, n4092);
    let n4094: ZB = zb_and(n3852, n4092);
    let n4095: ZB = zb_or(n4093, n4094);
    let n4096: ZB = zb_and(n3852, n4095);
    let n4097: ZB = zn_gt(n4075, zn_splat(P8::from_raw(0i32)));
    let n4098: ZB = zn_le(n4075, zn_splat(P8::from_raw(0i32)));
    let n4099: ZB = zb_and(n4096, n4097);
    let n4100: ZB = zb_and(n4096, n4098);
    let n4101: ZB = zb_or(n4099, n4100);
    let n4102: ZB = zb_and(n3905, n4071);
    let n4103: ZB = zb_and(n3905, n4072);
    let n4104: ZB = zb_or(n4102, n4103);
    let n4105: ZB = zb_or(n3812, n4104);
    let n4106: ZB = zb_and(n3851, n4105);
    let n4107: ZB = zb_and(n3852, n4105);
    let n4108: ZB = zb_or(n4106, n4107);
    let n4109: ZB = zb_and(n3852, n4108);
    let n4110: ZB = zb_and(n4097, n4109);
    let n4111: ZB = zb_and(n4098, n4109);
    let n4112: ZB = zb_or(n4110, n4111);
    let n4113: ZB = zb_and(n3956, n4071);
    let n4114: ZB = zb_and(n3956, n4072);
    let n4115: ZB = zb_or(n4113, n4114);
    let n4116: ZB = zb_or(n3812, n4115);
    let n4117: ZB = zb_and(n3851, n4116);
    let n4118: ZB = zb_and(n3852, n4116);
    let n4119: ZB = zb_or(n4117, n4118);
    let n4120: ZB = zb_and(n3852, n4119);
    let n4121: ZB = zb_and(n4097, n4120);
    let n4122: ZB = zb_and(n4098, n4120);
    let n4123: ZB = zb_or(n4121, n4122);
    let n4124: ZB = zb_or(n4077, n4078);
    let n4125: ZB = zb_or(n3812, n4124);
    let n4126: ZB = zb_and(n3851, n4125);
    let n4127: ZB = zb_and(n3852, n4125);
    let n4128: ZB = zb_or(n4126, n4127);
    let n4129: ZB = zb_and(n3852, n4128);
    let n4130: ZB = zb_and(n4097, n4129);
    let n4131: ZB = zb_and(n4098, n4129);
    let n4132: ZB = zb_or(n4130, n4131);
    let n4133: ZB = zb_and(n3992, n4071);
    let n4134: ZB = zb_and(n3992, n4072);
    let n4135: ZB = zb_and(n3776, n4133);
    let n4136: ZB = zb_and(n3797, n4133);
    let n4137: ZB = zb_or(n4135, n4136);
    let n4138: ZB = zb_and(n3799, n4137);
    let n4139: ZB = zb_and(n3800, n4137);
    let n4140: ZB = zb_and(n3801, n4139);
    let n4141: ZB = zb_and(n3802, n4139);
    let n4142: ZB = zb_or(n4140, n4141);
    let n4143: ZB = zb_or(n4138, n4142);
    let n4144: ZB = zb_and(n3804, n4143);
    let n4145: ZB = zb_and(n3803, n4143);
    let n4146: ZB = zb_or(n4144, n4145);
    let n4147: ZB = zb_or(n4134, n4146);
    let n4148: ZB = zb_or(n3812, n4147);
    let n4149: ZB = zb_and(n3851, n4148);
    let n4150: ZB = zb_and(n3852, n4148);
    let n4151: ZB = zb_or(n4149, n4150);
    let n4152: ZB = zb_and(n3852, n4151);
    let n4153: ZB = zb_and(n4097, n4152);
    let n4154: ZB = zb_and(n4098, n4152);
    let n4155: ZB = zb_or(n4153, n4154);
    let n4156: ZB = zb_and(n4028, n4071);
    let n4157: ZB = zb_and(n4028, n4072);
    let n4158: ZB = zb_or(n4156, n4157);
    let n4159: ZB = zb_or(n3812, n4158);
    let n4160: ZB = zb_and(n3851, n4159);
    let n4161: ZB = zb_and(n3852, n4159);
    let n4162: ZB = zb_or(n4160, n4161);
    let n4163: ZB = zb_and(n3852, n4162);
    let n4164: ZB = zb_and(n4097, n4163);
    let n4165: ZB = zb_and(n4098, n4163);
    let n4166: ZB = zb_or(n4164, n4165);
    let n4167: ZB = zb_and(n4064, n4071);
    let n4168: ZB = zb_and(n4064, n4072);
    let n4169: ZB = zb_or(n4167, n4168);
    let n4170: ZB = zb_or(n3812, n4169);
    let n4171: ZB = zb_and(n3851, n4170);
    let n4172: ZB = zb_and(n3852, n4170);
    let n4173: ZB = zb_or(n4171, n4172);
    let n4174: ZB = zb_and(n3852, n4173);
    let n4175: ZB = zb_and(n4097, n4174);
    let n4176: ZB = zb_and(n4098, n4174);
    let n4177: ZB = zb_or(n4175, n4176);
    let n4178: ZB = zb_or(n4133, n4134);
    let n4179: ZB = zb_or(n3812, n4178);
    let n4180: ZB = zb_and(n3851, n4179);
    let n4181: ZB = zb_and(n3852, n4179);
    let n4182: ZB = zb_or(n4180, n4181);
    let n4183: ZB = zb_and(n3852, n4182);
    let n4184: ZB = zb_and(n4097, n4183);
    let n4185: ZB = zb_and(n4098, n4183);
    let n4186: ZB = zb_or(n4184, n4185);
    let n4190: ZB = zb_and(n3706, n3709);
    let n4191: ZB = zb_and(n3719, n4190);
    let n4192: ZB = zb_and(n3718, n4190);
    let n4193: ZB = zb_or(n4191, n4192);
    let n4194: ZB = zb_and(n3719, n4193);
    let n4195: ZB = zb_and(n3718, n4193);
    let n4196: ZB = zb_or(n4194, n4195);
    let n4197: ZB = zb_and(n3718, n4196);
    let n4198: ZB = zb_and(n3719, n4196);
    let n4199: ZB = zb_and(n3726, n4197);
    let n4200: ZB = zb_and(n3727, n4197);
    let n4201: ZB = zb_or(n4199, n4200);
    let n4202: ZB = zb_and(n1425, n4198);
    let n4203: ZB = zb_and(n1426, n4198);
    let n4204: ZB = zb_or(n4202, n4203);
    let n4205: ZB = zb_or(n4201, n4204);
    let n4206: ZB = zb_and(n1435, n4205);
    let n4207: ZB = zb_and(n1436, n4205);
    let n4208: ZB = zb_and(n3740, n4206);
    let n4209: ZB = zb_and(n3741, n4206);
    let n4210: ZB = zb_or(n4208, n4209);
    let n4211: ZB = zb_and(n3742, n4210);
    let n4212: ZB = zb_and(n3743, n4210);
    let n4213: ZB = zb_or(n4211, n4212);
    let n4214: ZB = zb_and(n3719, n4207);
    let n4215: ZB = zb_and(n3718, n4207);
    let n4216: ZB = zb_or(n4214, n4215);
    let n4217: ZB = zb_and(n3746, n4216);
    let n4218: ZB = zb_and(n3747, n4216);
    let n4219: ZB = zb_and(n3748, n4217);
    let n4220: ZB = zb_and(n3052, n4217);
    let n4221: ZB = zb_and(n3749, n4220);
    let n4222: ZB = zb_and(n3077, n4220);
    let n4223: ZB = zb_and(n3750, n4219);
    let n4224: ZB = zb_and(n3751, n4219);
    let n4225: ZB = zb_and(n3756, n4221);
    let n4226: ZB = zb_and(n3757, n4221);
    let n4227: ZB = zb_and(n3052, n4222);
    let n4228: ZB = zb_or(n4225, n4226);
    let n4229: ZB = zb_or(n4223, n4224);
    let n4230: ZB = zb_or(n4227, n4228);
    let n4231: ZB = zb_or(n4229, n4230);
    let n4232: ZB = zb_and(n3748, n4218);
    let n4233: ZB = zb_and(n3052, n4218);
    let n4234: ZB = zb_or(n4232, n4233);
    let n4235: ZB = zb_or(n4231, n4234);
    let n4236: ZB = zb_and(n3774, n4235);
    let n4237: ZB = zb_and(n3773, n4235);
    let n4238: ZB = zb_or(n4236, n4237);
    let n4239: ZB = zb_and(n3778, n4238);
    let n4240: ZB = zb_and(n3779, n4238);
    let n4241: ZB = zb_or(n4239, n4240);
    let n4242: ZB = zb_and(n3719, n4241);
    let n4243: ZB = zb_and(n3718, n4241);
    let n4244: ZB = zb_and(n3781, n4242);
    let n4245: ZB = zb_and(n3782, n4242);
    let n4246: ZB = zb_or(n4244, n4245);
    let n4247: ZB = zb_or(n4243, n4246);
    let n4248: ZB = zb_and(n3795, n4247);
    let n4249: ZB = zb_and(n3796, n4247);
    let n4250: ZB = zb_or(n4248, n4249);
    let n4251: ZB = zb_or(n4213, n4250);
    let n4252: ZB = zb_and(n3851, n4251);
    let n4253: ZB = zb_and(n3852, n4251);
    let n4254: ZB = zb_or(n4252, n4253);
    let n4255: ZB = zb_and(n3851, n4254);
    let n4256: ZB = zb_and(n3851, n3855);
    let n4257: ZB = zb_not(n4255);
    let n4258: ZB = zb_or(n4255, n4256);
    let n4259: ZB = zsel_b(n4255, n3707, n3715);
    let n4261: ZN = zsel_n(n4255, r_c87, n3859);
    let n4262: ZN = zsel_n(n4255, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4264: ZB = zb_and(n3756, n4218);
    let n4265: ZB = zb_and(n3757, n4218);
    let n4266: ZB = zb_or(n4264, n4265);
    let n4267: ZB = zb_or(n4231, n4266);
    let n4268: ZB = zb_and(n3866, n4267);
    let n4269: ZB = zb_and(n3865, n4267);
    let n4270: ZB = zb_or(n4268, n4269);
    let n4271: ZB = zb_and(n3778, n4270);
    let n4272: ZB = zb_and(n3779, n4270);
    let n4273: ZB = zb_or(n4271, n4272);
    let n4274: ZB = zb_and(n3871, n4273);
    let n4275: ZB = zb_and(n3870, n4273);
    let n4276: ZB = zb_or(n4274, n4275);
    let n4277: ZB = zb_and(n3871, n4276);
    let n4278: ZB = zb_and(n3870, n4276);
    let n4279: ZB = zb_or(n4277, n4278);
    let n4280: ZB = zb_and(n3870, n4279);
    let n4281: ZB = zb_and(n3871, n4279);
    let n4282: ZB = zb_or(n4280, n4281);
    let n4283: ZB = zb_and(n3870, n4282);
    let n4284: ZB = zb_and(n3871, n4282);
    let n4285: ZB = zb_or(n4283, n4284);
    let n4286: ZB = zb_and(n3719, n4285);
    let n4287: ZB = zb_and(n3718, n4285);
    let n4288: ZB = zb_and(n3873, n4286);
    let n4289: ZB = zb_and(n3874, n4286);
    let n4290: ZB = zb_or(n4288, n4289);
    let n4291: ZB = zb_or(n4287, n4290);
    let n4292: ZB = zb_and(n3795, n4291);
    let n4293: ZB = zb_and(n3796, n4291);
    let n4294: ZB = zb_or(n4292, n4293);
    let n4295: ZB = zb_or(n4213, n4294);
    let n4296: ZB = zb_and(n3851, n4295);
    let n4297: ZB = zb_and(n3852, n4295);
    let n4298: ZB = zb_or(n4296, n4297);
    let n4299: ZB = zb_and(n3851, n4298);
    let n4300: ZB = zb_and(n3851, n3909);
    let n4301: ZB = zb_not(n4299);
    let n4302: ZB = zb_or(n4299, n4300);
    let n4303: ZB = zsel_b(n4299, n3707, n3715);
    let n4305: ZN = zsel_n(n4299, r_c87, n3859);
    let n4306: ZN = zsel_n(n4299, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4308: ZB = zb_and(n3750, n4218);
    let n4309: ZB = zb_and(n3751, n4218);
    let n4310: ZB = zb_or(n4308, n4309);
    let n4311: ZB = zb_or(n4231, n4310);
    let n4312: ZB = zb_and(n3917, n4311);
    let n4313: ZB = zb_and(n3916, n4311);
    let n4314: ZB = zb_or(n4312, n4313);
    let n4315: ZB = zb_and(n3778, n4314);
    let n4316: ZB = zb_and(n3779, n4314);
    let n4317: ZB = zb_or(n4315, n4316);
    let n4318: ZB = zb_and(n3922, n4317);
    let n4319: ZB = zb_and(n3921, n4317);
    let n4320: ZB = zb_or(n4318, n4319);
    let n4321: ZB = zb_and(n3922, n4320);
    let n4322: ZB = zb_and(n3921, n4320);
    let n4323: ZB = zb_or(n4321, n4322);
    let n4324: ZB = zb_and(n3921, n4323);
    let n4325: ZB = zb_and(n3922, n4323);
    let n4326: ZB = zb_or(n4324, n4325);
    let n4327: ZB = zb_and(n3921, n4326);
    let n4328: ZB = zb_and(n3922, n4326);
    let n4329: ZB = zb_or(n4327, n4328);
    let n4330: ZB = zb_and(n3719, n4329);
    let n4331: ZB = zb_and(n3718, n4329);
    let n4332: ZB = zb_and(n3924, n4330);
    let n4333: ZB = zb_and(n3925, n4330);
    let n4334: ZB = zb_or(n4332, n4333);
    let n4335: ZB = zb_or(n4331, n4334);
    let n4336: ZB = zb_and(n3795, n4335);
    let n4337: ZB = zb_and(n3796, n4335);
    let n4338: ZB = zb_or(n4336, n4337);
    let n4339: ZB = zb_or(n4213, n4338);
    let n4340: ZB = zb_and(n3851, n4339);
    let n4341: ZB = zb_and(n3852, n4339);
    let n4342: ZB = zb_or(n4340, n4341);
    let n4343: ZB = zb_and(n3851, n4342);
    let n4344: ZB = zb_and(n3851, n3960);
    let n4345: ZB = zb_not(n4343);
    let n4346: ZB = zb_or(n4343, n4344);
    let n4347: ZB = zsel_b(n4343, n3707, n3715);
    let n4349: ZN = zsel_n(n4343, r_c87, n3859);
    let n4350: ZN = zsel_n(n4343, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4352: ZB = zb_and(n140, n4247);
    let n4353: ZB = zb_and(r_c294, n4247);
    let n4354: ZB = zb_and(n3783, n4352);
    let n4355: ZB = zb_and(n3784, n4352);
    let n4356: ZB = zb_and(n3787, n4355);
    let n4357: ZB = zb_and(n3786, n4355);
    let n4358: ZB = zb_or(n4356, n4357);
    let n4359: ZB = zb_and(n3787, n4358);
    let n4360: ZB = zb_and(n3786, n4358);
    let n4361: ZB = zb_or(n4359, n4360);
    let n4362: ZB = zb_and(n3786, n4361);
    let n4363: ZB = zb_and(n3787, n4361);
    let n4364: ZB = zb_and(n3790, n4363);
    let n4365: ZB = zb_and(n3789, n4363);
    let n4366: ZB = zb_or(n4364, n4365);
    let n4367: ZB = zb_and(n3790, n4366);
    let n4368: ZB = zb_and(n3789, n4366);
    let n4369: ZB = zb_or(n4367, n4368);
    let n4370: ZB = zb_and(n3789, n4369);
    let n4371: ZB = zb_and(n3790, n4369);
    let n4372: ZB = zb_or(n4370, n4371);
    let n4373: ZB = zb_or(n4362, n4372);
    let n4374: ZB = zb_and(n3794, n4373);
    let n4375: ZB = zb_and(n3793, n4373);
    let n4376: ZB = zb_or(n4374, n4375);
    let n4377: ZB = zb_or(n4354, n4376);
    let n4378: ZB = zb_or(n4353, n4377);
    let n4379: ZB = zb_and(n3795, n4378);
    let n4380: ZB = zb_and(n3796, n4378);
    let n4381: ZB = zb_or(n4379, n4380);
    let n4382: ZB = zb_or(n4213, n4381);
    let n4383: ZB = zb_and(n3851, n4382);
    let n4384: ZB = zb_and(n3852, n4382);
    let n4385: ZB = zb_or(n4383, n4384);
    let n4386: ZB = zb_and(n3851, n4385);
    let n4387: ZB = zb_and(n3851, n3996);
    let n4388: ZB = zb_not(n4386);
    let n4389: ZB = zb_or(n4386, n4387);
    let n4390: ZB = zsel_b(n4386, n3707, n3715);
    let n4392: ZN = zsel_n(n4386, r_c87, n3859);
    let n4393: ZN = zsel_n(n4386, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4395: ZB = zb_and(n140, n4291);
    let n4396: ZB = zb_and(r_c294, n4291);
    let n4397: ZB = zb_and(n3783, n4395);
    let n4398: ZB = zb_and(n3784, n4395);
    let n4399: ZB = zb_and(n3787, n4398);
    let n4400: ZB = zb_and(n3786, n4398);
    let n4401: ZB = zb_or(n4399, n4400);
    let n4402: ZB = zb_and(n3787, n4401);
    let n4403: ZB = zb_and(n3786, n4401);
    let n4404: ZB = zb_or(n4402, n4403);
    let n4405: ZB = zb_and(n3786, n4404);
    let n4406: ZB = zb_and(n3787, n4404);
    let n4407: ZB = zb_and(n3790, n4406);
    let n4408: ZB = zb_and(n3789, n4406);
    let n4409: ZB = zb_or(n4407, n4408);
    let n4410: ZB = zb_and(n3790, n4409);
    let n4411: ZB = zb_and(n3789, n4409);
    let n4412: ZB = zb_or(n4410, n4411);
    let n4413: ZB = zb_and(n3789, n4412);
    let n4414: ZB = zb_and(n3790, n4412);
    let n4415: ZB = zb_or(n4413, n4414);
    let n4416: ZB = zb_or(n4405, n4415);
    let n4417: ZB = zb_and(n3794, n4416);
    let n4418: ZB = zb_and(n3793, n4416);
    let n4419: ZB = zb_or(n4417, n4418);
    let n4420: ZB = zb_or(n4397, n4419);
    let n4421: ZB = zb_or(n4396, n4420);
    let n4422: ZB = zb_and(n3795, n4421);
    let n4423: ZB = zb_and(n3796, n4421);
    let n4424: ZB = zb_or(n4422, n4423);
    let n4425: ZB = zb_or(n4213, n4424);
    let n4426: ZB = zb_and(n3851, n4425);
    let n4427: ZB = zb_and(n3852, n4425);
    let n4428: ZB = zb_or(n4426, n4427);
    let n4429: ZB = zb_and(n3851, n4428);
    let n4430: ZB = zb_and(n3851, n4032);
    let n4431: ZB = zb_not(n4429);
    let n4432: ZB = zb_or(n4429, n4430);
    let n4433: ZB = zsel_b(n4429, n3707, n3715);
    let n4435: ZN = zsel_n(n4429, r_c87, n3859);
    let n4436: ZN = zsel_n(n4429, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4438: ZB = zb_and(n140, n4335);
    let n4439: ZB = zb_and(r_c294, n4335);
    let n4440: ZB = zb_and(n3783, n4438);
    let n4441: ZB = zb_and(n3784, n4438);
    let n4442: ZB = zb_and(n3787, n4441);
    let n4443: ZB = zb_and(n3786, n4441);
    let n4444: ZB = zb_or(n4442, n4443);
    let n4445: ZB = zb_and(n3787, n4444);
    let n4446: ZB = zb_and(n3786, n4444);
    let n4447: ZB = zb_or(n4445, n4446);
    let n4448: ZB = zb_and(n3786, n4447);
    let n4449: ZB = zb_and(n3787, n4447);
    let n4450: ZB = zb_and(n3790, n4449);
    let n4451: ZB = zb_and(n3789, n4449);
    let n4452: ZB = zb_or(n4450, n4451);
    let n4453: ZB = zb_and(n3790, n4452);
    let n4454: ZB = zb_and(n3789, n4452);
    let n4455: ZB = zb_or(n4453, n4454);
    let n4456: ZB = zb_and(n3789, n4455);
    let n4457: ZB = zb_and(n3790, n4455);
    let n4458: ZB = zb_or(n4456, n4457);
    let n4459: ZB = zb_or(n4448, n4458);
    let n4460: ZB = zb_and(n3794, n4459);
    let n4461: ZB = zb_and(n3793, n4459);
    let n4462: ZB = zb_or(n4460, n4461);
    let n4463: ZB = zb_or(n4440, n4462);
    let n4464: ZB = zb_or(n4439, n4463);
    let n4465: ZB = zb_and(n3795, n4464);
    let n4466: ZB = zb_and(n3796, n4464);
    let n4467: ZB = zb_or(n4465, n4466);
    let n4468: ZB = zb_or(n4213, n4467);
    let n4469: ZB = zb_and(n3851, n4468);
    let n4470: ZB = zb_and(n3852, n4468);
    let n4471: ZB = zb_or(n4469, n4470);
    let n4472: ZB = zb_and(n3851, n4471);
    let n4473: ZB = zb_and(n3851, n4068);
    let n4474: ZB = zb_not(n4472);
    let n4475: ZB = zb_or(n4472, n4473);
    let n4476: ZB = zsel_b(n4472, n3707, n3715);
    let n4478: ZN = zsel_n(n4472, r_c87, n3859);
    let n4479: ZN = zsel_n(n4472, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4481: ZB = zb_and(n4071, n4250);
    let n4482: ZB = zb_and(n4072, n4250);
    let n4483: ZB = zb_and(n3776, n4481);
    let n4484: ZB = zb_and(n3797, n4481);
    let n4485: ZB = zb_or(n4483, n4484);
    let n4486: ZB = zb_and(n3799, n4485);
    let n4487: ZB = zb_and(n3800, n4485);
    let n4488: ZB = zb_and(n3801, n4487);
    let n4489: ZB = zb_and(n3802, n4487);
    let n4490: ZB = zb_or(n4488, n4489);
    let n4491: ZB = zb_or(n4486, n4490);
    let n4492: ZB = zb_and(n3804, n4491);
    let n4493: ZB = zb_and(n3803, n4491);
    let n4494: ZB = zb_or(n4492, n4493);
    let n4495: ZB = zb_or(n4482, n4494);
    let n4496: ZB = zb_or(n4213, n4495);
    let n4497: ZB = zb_and(n3851, n4496);
    let n4498: ZB = zb_and(n3852, n4496);
    let n4499: ZB = zb_or(n4497, n4498);
    let n4500: ZB = zb_and(n3851, n4499);
    let n4501: ZB = zb_and(n3851, n4095);
    let n4502: ZB = zb_not(n4500);
    let n4503: ZB = zb_or(n4500, n4501);
    let n4504: ZB = zsel_b(n4500, n3707, n3715);
    let n4505: ZB = zb_and(n4097, n4503);
    let n4506: ZB = zb_and(n4098, n4503);
    let n4507: ZB = zb_or(n4505, n4506);
    let n4508: ZN = zsel_n(n4500, r_c87, n3859);
    let n4509: ZN = zsel_n(n4500, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4511: ZB = zb_and(n4071, n4294);
    let n4512: ZB = zb_and(n4072, n4294);
    let n4513: ZB = zb_or(n4511, n4512);
    let n4514: ZB = zb_or(n4213, n4513);
    let n4515: ZB = zb_and(n3851, n4514);
    let n4516: ZB = zb_and(n3852, n4514);
    let n4517: ZB = zb_or(n4515, n4516);
    let n4518: ZB = zb_and(n3851, n4517);
    let n4519: ZB = zb_and(n3851, n4108);
    let n4520: ZB = zb_not(n4518);
    let n4521: ZB = zb_or(n4518, n4519);
    let n4522: ZB = zsel_b(n4518, n3707, n3715);
    let n4523: ZB = zb_and(n4097, n4521);
    let n4524: ZB = zb_and(n4098, n4521);
    let n4525: ZB = zb_or(n4523, n4524);
    let n4526: ZN = zsel_n(n4518, r_c87, n3859);
    let n4527: ZN = zsel_n(n4518, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4529: ZB = zb_and(n4071, n4338);
    let n4530: ZB = zb_and(n4072, n4338);
    let n4531: ZB = zb_or(n4529, n4530);
    let n4532: ZB = zb_or(n4213, n4531);
    let n4533: ZB = zb_and(n3851, n4532);
    let n4534: ZB = zb_and(n3852, n4532);
    let n4535: ZB = zb_or(n4533, n4534);
    let n4536: ZB = zb_and(n3851, n4535);
    let n4537: ZB = zb_and(n3851, n4119);
    let n4538: ZB = zb_not(n4536);
    let n4539: ZB = zb_or(n4536, n4537);
    let n4540: ZB = zsel_b(n4536, n3707, n3715);
    let n4541: ZB = zb_and(n4097, n4539);
    let n4542: ZB = zb_and(n4098, n4539);
    let n4543: ZB = zb_or(n4541, n4542);
    let n4544: ZN = zsel_n(n4536, r_c87, n3859);
    let n4545: ZN = zsel_n(n4536, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4547: ZB = zb_or(n4481, n4482);
    let n4548: ZB = zb_or(n4213, n4547);
    let n4549: ZB = zb_and(n3851, n4548);
    let n4550: ZB = zb_and(n3852, n4548);
    let n4551: ZB = zb_or(n4549, n4550);
    let n4552: ZB = zb_and(n3851, n4551);
    let n4553: ZB = zb_and(n3851, n4128);
    let n4554: ZB = zb_not(n4552);
    let n4555: ZB = zb_or(n4552, n4553);
    let n4556: ZB = zsel_b(n4552, n3707, n3715);
    let n4557: ZB = zb_and(n4097, n4555);
    let n4558: ZB = zb_and(n4098, n4555);
    let n4559: ZB = zb_or(n4557, n4558);
    let n4560: ZN = zsel_n(n4552, r_c87, n3859);
    let n4561: ZN = zsel_n(n4552, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4563: ZB = zb_and(n4071, n4381);
    let n4564: ZB = zb_and(n4072, n4381);
    let n4565: ZB = zb_and(n3776, n4563);
    let n4566: ZB = zb_and(n3797, n4563);
    let n4567: ZB = zb_or(n4565, n4566);
    let n4568: ZB = zb_and(n3799, n4567);
    let n4569: ZB = zb_and(n3800, n4567);
    let n4570: ZB = zb_and(n3801, n4569);
    let n4571: ZB = zb_and(n3802, n4569);
    let n4572: ZB = zb_or(n4570, n4571);
    let n4573: ZB = zb_or(n4568, n4572);
    let n4574: ZB = zb_and(n3804, n4573);
    let n4575: ZB = zb_and(n3803, n4573);
    let n4576: ZB = zb_or(n4574, n4575);
    let n4577: ZB = zb_or(n4564, n4576);
    let n4578: ZB = zb_or(n4213, n4577);
    let n4579: ZB = zb_and(n3851, n4578);
    let n4580: ZB = zb_and(n3852, n4578);
    let n4581: ZB = zb_or(n4579, n4580);
    let n4582: ZB = zb_and(n3851, n4581);
    let n4583: ZB = zb_and(n3851, n4151);
    let n4584: ZB = zb_not(n4582);
    let n4585: ZB = zb_or(n4582, n4583);
    let n4586: ZB = zsel_b(n4582, n3707, n3715);
    let n4587: ZB = zb_and(n4097, n4585);
    let n4588: ZB = zb_and(n4098, n4585);
    let n4589: ZB = zb_or(n4587, n4588);
    let n4590: ZN = zsel_n(n4582, r_c87, n3859);
    let n4591: ZN = zsel_n(n4582, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4593: ZB = zb_and(n4071, n4424);
    let n4594: ZB = zb_and(n4072, n4424);
    let n4595: ZB = zb_or(n4593, n4594);
    let n4596: ZB = zb_or(n4213, n4595);
    let n4597: ZB = zb_and(n3851, n4596);
    let n4598: ZB = zb_and(n3852, n4596);
    let n4599: ZB = zb_or(n4597, n4598);
    let n4600: ZB = zb_and(n3851, n4599);
    let n4601: ZB = zb_and(n3851, n4162);
    let n4602: ZB = zb_not(n4600);
    let n4603: ZB = zb_or(n4600, n4601);
    let n4604: ZB = zsel_b(n4600, n3707, n3715);
    let n4605: ZB = zb_and(n4097, n4603);
    let n4606: ZB = zb_and(n4098, n4603);
    let n4607: ZB = zb_or(n4605, n4606);
    let n4608: ZN = zsel_n(n4600, r_c87, n3859);
    let n4609: ZN = zsel_n(n4600, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4611: ZB = zb_and(n4071, n4467);
    let n4612: ZB = zb_and(n4072, n4467);
    let n4613: ZB = zb_or(n4611, n4612);
    let n4614: ZB = zb_or(n4213, n4613);
    let n4615: ZB = zb_and(n3851, n4614);
    let n4616: ZB = zb_and(n3852, n4614);
    let n4617: ZB = zb_or(n4615, n4616);
    let n4618: ZB = zb_and(n3851, n4617);
    let n4619: ZB = zb_and(n3851, n4173);
    let n4620: ZB = zb_not(n4618);
    let n4621: ZB = zb_or(n4618, n4619);
    let n4622: ZB = zsel_b(n4618, n3707, n3715);
    let n4623: ZB = zb_and(n4097, n4621);
    let n4624: ZB = zb_and(n4098, n4621);
    let n4625: ZB = zb_or(n4623, n4624);
    let n4626: ZN = zsel_n(n4618, r_c87, n3859);
    let n4627: ZN = zsel_n(n4618, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4629: ZB = zb_or(n4563, n4564);
    let n4630: ZB = zb_or(n4213, n4629);
    let n4631: ZB = zb_and(n3851, n4630);
    let n4632: ZB = zb_and(n3852, n4630);
    let n4633: ZB = zb_or(n4631, n4632);
    let n4634: ZB = zb_and(n3851, n4633);
    let n4635: ZB = zb_and(n3851, n4182);
    let n4636: ZB = zb_not(n4634);
    let n4637: ZB = zb_or(n4634, n4635);
    let n4638: ZB = zsel_b(n4634, n3707, n3715);
    let n4639: ZB = zb_and(n4097, n4637);
    let n4640: ZB = zb_and(n4098, n4637);
    let n4641: ZB = zb_or(n4639, n4640);
    let n4642: ZN = zsel_n(n4634, r_c87, n3859);
    let n4643: ZN = zsel_n(n4634, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4645: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n4646: ZN = zn_sub(n2494, zn_splat(P8::from_raw(32768i32)));
    let n4647: ZN = zn_sub(n4646, n2495);
    let n4648: ZN = zsel_n(n2631, zn_splat(P8::from_raw(0i32)), n4647);
    let n4649: ZN = zsel_n(n2626, n4647, n4648);
    let n4650: ZN = zsel_n(n2614, zn_splat(P8::from_raw(0i32)), n4649);
    let n4651: ZN = zsel_n(n2609, n4647, n4650);
    let n4652: ZN = zsel_n(n2597, zn_splat(P8::from_raw(0i32)), n4651);
    let n4653: ZN = zsel_n(n2592, n4647, n4652);
    let n4654: ZN = zsel_n(n2580, zn_splat(P8::from_raw(0i32)), n4653);
    let n4655: ZN = zsel_n(n2575, n4647, n4654);
    let n4656: ZN = zsel_n(n2563, zn_splat(P8::from_raw(0i32)), n4655);
    let n4657: ZN = zsel_n(n2558, n4647, n4656);
    let n4658: ZN = zsel_n(n2546, zn_splat(P8::from_raw(0i32)), n4657);
    let n4659: ZN = zsel_n(n2541, n4647, n4658);
    let n4660: ZN = zsel_n(n2529, zn_splat(P8::from_raw(0i32)), n4659);
    let n4661: ZN = zsel_n(n2524, n4647, n4660);
    let n4662: ZN = zsel_n(n2512, zn_splat(P8::from_raw(0i32)), n4661);
    let n4663: ZN = zn_sub(n2704, zn_splat(P8::from_raw(32768i32)));
    let n4664: ZN = zn_sub(n4663, n2705);
    let n4665: ZN = zsel_n(n2891, zn_splat(P8::from_raw(0i32)), n4664);
    let n4666: ZN = zsel_n(n2880, n4664, n4665);
    let n4667: ZN = zsel_n(n2868, zn_splat(P8::from_raw(0i32)), n4666);
    let n4668: ZN = zsel_n(n2857, n4664, n4667);
    let n4669: ZN = zsel_n(n2845, zn_splat(P8::from_raw(0i32)), n4668);
    let n4670: ZN = zsel_n(n2834, n4664, n4669);
    let n4671: ZN = zsel_n(n2822, zn_splat(P8::from_raw(0i32)), n4670);
    let n4672: ZN = zsel_n(n2811, n4664, n4671);
    let n4673: ZN = zsel_n(n2799, zn_splat(P8::from_raw(0i32)), n4672);
    let n4674: ZN = zsel_n(n2788, n4664, n4673);
    let n4675: ZN = zsel_n(n2776, zn_splat(P8::from_raw(0i32)), n4674);
    let n4676: ZN = zsel_n(n2765, n4664, n4675);
    let n4677: ZN = zsel_n(n2753, zn_splat(P8::from_raw(0i32)), n4676);
    let n4678: ZN = zsel_n(n2742, n4664, n4677);
    let n4679: ZN = zsel_n(n2730, zn_splat(P8::from_raw(0i32)), n4678);
    let n4680: ZN = zsel_n(n2489, n4662, r_c367);
    let n4681: ZN = zsel_n(n2489, n4679, r_c368);
    let n4682: ZN = zn_sub(n2966, r_c357);
    let n4683: ZN = zn_max(r_c359, n4682);
    let n4684: ZN = zn_add(r_c357, n2966);
    let n4685: ZN = zn_min(r_c359, n4684);
    let n4686: ZN = zsel_n(n3740, n4683, n4685);
    let n4687: ZN = zn_sub(n2967, r_c358);
    let n4688: ZN = zn_max(r_c360, n4687);
    let n4689: ZN = zn_add(r_c358, n2967);
    let n4690: ZN = zn_min(r_c360, n4689);
    let n4691: ZN = zsel_n(n3742, n4688, n4690);
    let n4692: ZN = zsel_n(n3778, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(13762i32)));
    let n4693: ZN = zn_sub(n2967, n4692);
    let n4694: ZN = zn_max(zn_splat(P8::from_raw(131072i32)), n4693);
    let n4695: ZN = zn_add(n2967, n4692);
    let n4696: ZN = zn_min(zn_splat(P8::from_raw(131072i32)), n4695);
    let n4697: ZN = zsel_n(n3781, n4694, n4696);
    let n4698: ZN = zsel_n(n3719, n4697, n2967);
    let n4699: ZN = zn_neg(n3792);
    let n4700: ZN = zn_mul(n4699, zn_splat(P8::from_raw(131072i32)));
    let n4701: ZN = zsel_n(n3794, n4700, n3772);
    let n4702: ZN = zsel_n(n3794, zn_splat(P8::from_raw(-131072i32)), n4698);
    let n4703: ZN = zsel_n(n3783, zn_splat(P8::from_raw(0i32)), n3730);
    let n4704: ZN = zsel_n(n3783, n3772, n4701);
    let n4705: ZN = zsel_n(n3783, zn_splat(P8::from_raw(-131072i32)), n4702);
    let n4706: ZN = zn_sub(n3729, zn_splat(P8::from_raw(65536i32)));
    let n4707: ZN = zsel_n(n3801, zn_splat(P8::from_raw(-131072i32)), zn_splat(P8::from_raw(0i32)));
    let n4708: ZN = zsel_n(n3799, zn_splat(P8::from_raw(131072i32)), n4707);
    let n4709: ZN = zsel_n(n3804, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n4710: ZB = zsel_b(n1435, r_c361, n3776);
    let n4711: ZN = zsel_n(n1435, n4686, n3772);
    let n4712: ZN = zsel_n(n1435, n4691, n4698);
    let n4713: ZB = zb_and(n3852, n4254);
    let n4714: ZN = zsel_n(n1567, n4645, r_c20);
    let n4715: ZN = zsel_n(n1567, r_c260, n2475);
    let n4716: ZN = zsel_n(n1567, r_c273, n2476);
    let n4717: ZN = zsel_n(n1567, r_c281, n1434);
    let n4718: ZN = zsel_n(n1567, r_c283, n1562);
    let n4719: ZN = zsel_n(n1567, r_c284, n3729);
    let n4720: ZN = zsel_n(n1567, r_c286, n3730);
    let n4721: ZB = zb_and(r_c293, n1567);
    let n4722: ZB = zb_and(r_c294, n1567);
    let n4723: ZN = zsel_n(n1567, r_c300, n2964);
    let n4724: ZN = zsel_n(n1567, r_c301, n2965);
    let n4725: ZB = zsel_b(n1567, r_c361, n4710);
    let n4726: ZN = zsel_n(n1567, r_c367, n4680);
    let n4727: ZN = zsel_n(n1567, r_c368, n4681);
    let n4728: ZN = zsel_n(n1567, r_c369, n4711);
    let n4729: ZN = zsel_n(n1567, r_c370, n4712);
    let n4730: ZB = zb_or(n1567, n4713);
    let n4731: ZB = zb_or(n1567, n3707);
    let n4732: ZB = zn_gt(n4714, zn_splat(P8::from_raw(0i32)));
    let n4733: ZB = zn_le(n4714, zn_splat(P8::from_raw(0i32)));
    let n4734: ZB = zb_and(n4730, n4732);
    let n4735: ZB = zb_and(n4730, n4733);
    let n4736: ZB = zn_lt(n4723, zn_splat(P8::from_raw(-65536i32)));
    let n4737: ZB = zn_ge(n4723, zn_splat(P8::from_raw(-65536i32)));
    let n4738: ZB = zb_and(n4735, n4737);
    let n4739: ZB = zb_and(n4735, n4736);
    let n4740: ZB = zn_gt(n4723, zn_splat(P8::from_raw(7929856i32)));
    let n4741: ZB = zb_or(n4738, n4739);
    let n4742: ZB = zb_or(n4736, n4740);
    let n4743: ZB = zb_not(n4742);
    let n4744: ZB = zb_and(n4741, n4742);
    let n4745: ZB = zb_and(n4741, n4743);
    let n4746: ZN = zn_min(zn_splat(P8::from_raw(7929856i32)), n4723);
    let n4747: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n4746);
    let n4748: ZN = zsel_n(n4742, n4747, n4723);
    let n4749: ZN = zsel_n(n4742, zn_splat(P8::from_raw(0i32)), n4728);
    let n4750: ZB = zb_or(n4744, n4745);
    let n4751: ZN = zsel_n(n4732, n4723, n4748);
    let n4752: ZN = zsel_n(n4732, n4728, n4749);
    let n4753: ZB = zb_or(n4734, n4750);
    let n4755: ZN = zn_max(n3872, n4693);
    let n4756: ZN = zn_min(n3872, n4695);
    let n4757: ZN = zsel_n(n3873, n4755, n4756);
    let n4758: ZN = zsel_n(n3719, n4757, n2967);
    let n4759: ZN = zsel_n(n3794, n4700, n3864);
    let n4760: ZN = zsel_n(n3794, zn_splat(P8::from_raw(-131072i32)), n4758);
    let n4761: ZN = zsel_n(n3783, n3864, n4759);
    let n4762: ZN = zsel_n(n3783, zn_splat(P8::from_raw(-131072i32)), n4760);
    let n4763: ZB = zsel_b(n1435, r_c361, n3868);
    let n4764: ZN = zsel_n(n1435, n4686, n3864);
    let n4765: ZN = zsel_n(n1435, n4691, n4758);
    let n4766: ZB = zb_and(n3852, n4298);
    let n4767: ZB = zsel_b(n1567, r_c361, n4763);
    let n4768: ZN = zsel_n(n1567, r_c369, n4764);
    let n4769: ZN = zsel_n(n1567, r_c370, n4765);
    let n4770: ZB = zb_or(n1567, n4766);
    let n4771: ZB = zb_and(n4732, n4770);
    let n4772: ZB = zb_and(n4733, n4770);
    let n4773: ZB = zb_and(n4737, n4772);
    let n4774: ZB = zb_and(n4736, n4772);
    let n4775: ZB = zb_or(n4773, n4774);
    let n4776: ZB = zb_and(n4742, n4775);
    let n4777: ZB = zb_and(n4743, n4775);
    let n4778: ZN = zsel_n(n4742, zn_splat(P8::from_raw(0i32)), n4768);
    let n4779: ZB = zb_or(n4776, n4777);
    let n4780: ZN = zsel_n(n4732, n4768, n4778);
    let n4781: ZB = zb_or(n4771, n4779);
    let n4782: ZN = zn_max(n3923, n4693);
    let n4783: ZN = zn_min(n3923, n4695);
    let n4784: ZN = zsel_n(n3924, n4782, n4783);
    let n4785: ZN = zsel_n(n3719, n4784, n2967);
    let n4786: ZN = zsel_n(n3794, n4700, n3915);
    let n4787: ZN = zsel_n(n3794, zn_splat(P8::from_raw(-131072i32)), n4785);
    let n4788: ZN = zsel_n(n3783, n3915, n4786);
    let n4789: ZN = zsel_n(n3783, zn_splat(P8::from_raw(-131072i32)), n4787);
    let n4790: ZB = zsel_b(n1435, r_c361, n3919);
    let n4791: ZN = zsel_n(n1435, n4686, n3915);
    let n4792: ZN = zsel_n(n1435, n4691, n4785);
    let n4793: ZB = zb_and(n3852, n4342);
    let n4794: ZB = zsel_b(n1567, r_c361, n4790);
    let n4795: ZN = zsel_n(n1567, r_c369, n4791);
    let n4796: ZN = zsel_n(n1567, r_c370, n4792);
    let n4797: ZB = zb_or(n1567, n4793);
    let n4798: ZB = zb_and(n4732, n4797);
    let n4799: ZB = zb_and(n4733, n4797);
    let n4800: ZB = zb_and(n4737, n4799);
    let n4801: ZB = zb_and(n4736, n4799);
    let n4802: ZB = zb_or(n4800, n4801);
    let n4803: ZB = zb_and(n4742, n4802);
    let n4804: ZB = zb_and(n4743, n4802);
    let n4805: ZN = zsel_n(n4742, zn_splat(P8::from_raw(0i32)), n4795);
    let n4806: ZB = zb_or(n4803, n4804);
    let n4807: ZN = zsel_n(n4732, n4795, n4805);
    let n4808: ZB = zb_or(n4798, n4806);
    let n4809: ZN = zsel_n(n140, n4703, n3730);
    let n4810: ZN = zsel_n(n140, n4704, n3772);
    let n4811: ZN = zsel_n(n140, n4705, n4698);
    let n4812: ZN = zsel_n(n1435, n3730, n4809);
    let n4813: ZN = zsel_n(n1435, n4686, n4810);
    let n4814: ZN = zsel_n(n1435, n4691, n4811);
    let n4815: ZB = zb_and(n3852, n4385);
    let n4816: ZN = zsel_n(n1567, r_c286, n4812);
    let n4817: ZB = zb_or(r_c294, n134);
    let n4818: ZN = zsel_n(n1567, r_c369, n4813);
    let n4819: ZN = zsel_n(n1567, r_c370, n4814);
    let n4820: ZB = zb_or(n1567, n4815);
    let n4821: ZB = zb_and(n4732, n4820);
    let n4822: ZB = zb_and(n4733, n4820);
    let n4823: ZB = zb_and(n4737, n4822);
    let n4824: ZB = zb_and(n4736, n4822);
    let n4825: ZB = zb_or(n4823, n4824);
    let n4826: ZB = zb_and(n4742, n4825);
    let n4827: ZB = zb_and(n4743, n4825);
    let n4828: ZN = zsel_n(n4742, zn_splat(P8::from_raw(0i32)), n4818);
    let n4829: ZB = zb_or(n4826, n4827);
    let n4830: ZN = zsel_n(n4732, n4818, n4828);
    let n4831: ZB = zb_or(n4821, n4829);
    let n4832: ZN = zsel_n(n140, n4761, n3864);
    let n4833: ZN = zsel_n(n140, n4762, n4758);
    let n4834: ZN = zsel_n(n1435, n4686, n4832);
    let n4835: ZN = zsel_n(n1435, n4691, n4833);
    let n4836: ZB = zb_and(n3852, n4428);
    let n4837: ZN = zsel_n(n1567, r_c369, n4834);
    let n4838: ZN = zsel_n(n1567, r_c370, n4835);
    let n4839: ZB = zb_or(n1567, n4836);
    let n4840: ZB = zb_and(n4732, n4839);
    let n4841: ZB = zb_and(n4733, n4839);
    let n4842: ZB = zb_and(n4737, n4841);
    let n4843: ZB = zb_and(n4736, n4841);
    let n4844: ZB = zb_or(n4842, n4843);
    let n4845: ZB = zb_and(n4742, n4844);
    let n4846: ZB = zb_and(n4743, n4844);
    let n4847: ZN = zsel_n(n4742, zn_splat(P8::from_raw(0i32)), n4837);
    let n4848: ZB = zb_or(n4845, n4846);
    let n4849: ZN = zsel_n(n4732, n4837, n4847);
    let n4850: ZB = zb_or(n4840, n4848);
    let n4851: ZN = zsel_n(n140, n4788, n3915);
    let n4852: ZN = zsel_n(n140, n4789, n4785);
    let n4853: ZN = zsel_n(n1435, n4686, n4851);
    let n4854: ZN = zsel_n(n1435, n4691, n4852);
    let n4855: ZB = zb_and(n3852, n4471);
    let n4856: ZN = zsel_n(n1567, r_c369, n4853);
    let n4857: ZN = zsel_n(n1567, r_c370, n4854);
    let n4858: ZB = zb_or(n1567, n4855);
    let n4859: ZB = zb_and(n4732, n4858);
    let n4860: ZB = zb_and(n4733, n4858);
    let n4861: ZB = zb_and(n4737, n4860);
    let n4862: ZB = zb_and(n4736, n4860);
    let n4863: ZB = zb_or(n4861, n4862);
    let n4864: ZB = zb_and(n4742, n4863);
    let n4865: ZB = zb_and(n4743, n4863);
    let n4866: ZN = zsel_n(n4742, zn_splat(P8::from_raw(0i32)), n4856);
    let n4867: ZB = zb_or(n4864, n4865);
    let n4868: ZN = zsel_n(n4732, n4856, n4866);
    let n4869: ZB = zb_or(n4859, n4867);
    let n4870: ZN = zsel_n(n4071, zn_splat(P8::from_raw(655360i32)), n1434);
    let n4871: ZN = zsel_n(n4071, zn_splat(P8::from_raw(262144i32)), r_c283);
    let n4872: ZN = zsel_n(n4071, n4706, n3729);
    let n4873: ZN = zsel_n(n4071, zn_splat(P8::from_raw(98304i32)), r_c357);
    let n4874: ZN = zsel_n(n4071, n4709, r_c358);
    let n4875: ZN = zsel_n(n4071, n4708, r_c359);
    let n4876: ZN = zsel_n(n4071, zn_splat(P8::from_raw(0i32)), r_c360);
    let n4877: ZN = zsel_n(n4071, n3798, n3772);
    let n4878: ZN = zsel_n(n4071, zn_splat(P8::from_raw(0i32)), n4698);
    let n4879: ZN = zsel_n(n1435, n1434, n4870);
    let n4880: ZN = zsel_n(n1435, n1439, n4871);
    let n4881: ZN = zsel_n(n1435, n3729, n4872);
    let n4882: ZN = zsel_n(n1435, r_c357, n4873);
    let n4883: ZN = zsel_n(n1435, r_c358, n4874);
    let n4884: ZN = zsel_n(n1435, r_c359, n4875);
    let n4885: ZN = zsel_n(n1435, r_c360, n4876);
    let n4886: ZN = zsel_n(n1435, n4686, n4877);
    let n4887: ZN = zsel_n(n1435, n4691, n4878);
    let n4888: ZB = zb_and(n3852, n4499);
    let n4889: ZN = zsel_n(n1567, n4645, n4075);
    let n4890: ZB = zsel_b(n1567, r_c41, n4076);
    let n4891: ZN = zsel_n(n1567, r_c281, n4879);
    let n4892: ZN = zsel_n(n1567, r_c283, n4880);
    let n4893: ZN = zsel_n(n1567, r_c284, n4881);
    let n4894: ZB = zb_or(r_c293, n134);
    let n4895: ZN = zsel_n(n1567, r_c357, n4882);
    let n4896: ZN = zsel_n(n1567, r_c358, n4883);
    let n4897: ZN = zsel_n(n1567, r_c359, n4884);
    let n4898: ZN = zsel_n(n1567, r_c360, n4885);
    let n4899: ZN = zsel_n(n1567, r_c369, n4886);
    let n4900: ZN = zsel_n(n1567, r_c370, n4887);
    let n4901: ZB = zb_or(n1567, n4888);
    let n4902: ZB = zn_gt(n4889, zn_splat(P8::from_raw(0i32)));
    let n4903: ZB = zn_le(n4889, zn_splat(P8::from_raw(0i32)));
    let n4904: ZB = zb_and(n4901, n4902);
    let n4905: ZB = zb_and(n4901, n4903);
    let n4906: ZB = zb_and(n4737, n4905);
    let n4907: ZB = zb_and(n4736, n4905);
    let n4908: ZB = zb_or(n4906, n4907);
    let n4909: ZB = zb_and(n4742, n4908);
    let n4910: ZB = zb_and(n4743, n4908);
    let n4911: ZN = zsel_n(n4742, zn_splat(P8::from_raw(0i32)), n4899);
    let n4912: ZB = zb_or(n4909, n4910);
    let n4913: ZN = zsel_n(n4902, n4723, n4748);
    let n4914: ZN = zsel_n(n4902, n4899, n4911);
    let n4915: ZB = zb_or(n4904, n4912);
    let n4916: ZN = zsel_n(n4071, zn_splat(P8::from_raw(69510i32)), r_c358);
    let n4917: ZN = zsel_n(n4071, zn_splat(P8::from_raw(-131072i32)), r_c359);
    let n4918: ZN = zsel_n(n4071, zn_splat(P8::from_raw(-327680i32)), n3864);
    let n4919: ZN = zsel_n(n4071, zn_splat(P8::from_raw(0i32)), n4758);
    let n4920: ZN = zsel_n(n1435, r_c358, n4916);
    let n4921: ZN = zsel_n(n1435, r_c359, n4917);
    let n4922: ZN = zsel_n(n1435, n4686, n4918);
    let n4923: ZN = zsel_n(n1435, n4691, n4919);
    let n4924: ZB = zb_and(n3852, n4517);
    let n4925: ZN = zsel_n(n1567, r_c358, n4920);
    let n4926: ZN = zsel_n(n1567, r_c359, n4921);
    let n4927: ZN = zsel_n(n1567, r_c369, n4922);
    let n4928: ZN = zsel_n(n1567, r_c370, n4923);
    let n4929: ZB = zb_or(n1567, n4924);
    let n4930: ZB = zb_and(n4902, n4929);
    let n4931: ZB = zb_and(n4903, n4929);
    let n4932: ZB = zb_and(n4737, n4931);
    let n4933: ZB = zb_and(n4736, n4931);
    let n4934: ZB = zb_or(n4932, n4933);
    let n4935: ZB = zb_and(n4742, n4934);
    let n4936: ZB = zb_and(n4743, n4934);
    let n4937: ZN = zsel_n(n4742, zn_splat(P8::from_raw(0i32)), n4927);
    let n4938: ZB = zb_or(n4935, n4936);
    let n4939: ZN = zsel_n(n4902, n4927, n4937);
    let n4940: ZB = zb_or(n4930, n4938);
    let n4941: ZN = zsel_n(n4071, zn_splat(P8::from_raw(131072i32)), r_c359);
    let n4942: ZN = zsel_n(n4071, zn_splat(P8::from_raw(327680i32)), n3915);
    let n4943: ZN = zsel_n(n4071, zn_splat(P8::from_raw(0i32)), n4785);
    let n4944: ZN = zsel_n(n1435, r_c359, n4941);
    let n4945: ZN = zsel_n(n1435, n4686, n4942);
    let n4946: ZN = zsel_n(n1435, n4691, n4943);
    let n4947: ZB = zb_and(n3852, n4535);
    let n4948: ZN = zsel_n(n1567, r_c359, n4944);
    let n4949: ZN = zsel_n(n1567, r_c369, n4945);
    let n4950: ZN = zsel_n(n1567, r_c370, n4946);
    let n4951: ZB = zb_or(n1567, n4947);
    let n4952: ZB = zb_and(n4902, n4951);
    let n4953: ZB = zb_and(n4903, n4951);
    let n4954: ZB = zb_and(n4737, n4953);
    let n4955: ZB = zb_and(n4736, n4953);
    let n4956: ZB = zb_or(n4954, n4955);
    let n4957: ZB = zb_and(n4742, n4956);
    let n4958: ZB = zb_and(n4743, n4956);
    let n4959: ZN = zsel_n(n4742, zn_splat(P8::from_raw(0i32)), n4949);
    let n4960: ZB = zb_or(n4957, n4958);
    let n4961: ZN = zsel_n(n4902, n4949, n4959);
    let n4962: ZB = zb_or(n4952, n4960);
    let n4963: ZN = zsel_n(n4071, zn_splat(P8::from_raw(69510i32)), r_c357);
    let n4964: ZN = zsel_n(n4071, zn_splat(P8::from_raw(98304i32)), r_c358);
    let n4965: ZN = zsel_n(n4071, zn_splat(P8::from_raw(0i32)), r_c359);
    let n4966: ZN = zsel_n(n4071, zn_splat(P8::from_raw(-98304i32)), r_c360);
    let n4967: ZN = zsel_n(n4071, zn_splat(P8::from_raw(0i32)), n3772);
    let n4968: ZN = zsel_n(n4071, zn_splat(P8::from_raw(-327680i32)), n4698);
    let n4969: ZN = zsel_n(n1435, r_c357, n4963);
    let n4970: ZN = zsel_n(n1435, r_c358, n4964);
    let n4971: ZN = zsel_n(n1435, r_c359, n4965);
    let n4972: ZN = zsel_n(n1435, r_c360, n4966);
    let n4973: ZN = zsel_n(n1435, n4686, n4967);
    let n4974: ZN = zsel_n(n1435, n4691, n4968);
    let n4975: ZB = zb_and(n3852, n4551);
    let n4976: ZN = zsel_n(n1567, r_c357, n4969);
    let n4977: ZN = zsel_n(n1567, r_c358, n4970);
    let n4978: ZN = zsel_n(n1567, r_c359, n4971);
    let n4979: ZN = zsel_n(n1567, r_c360, n4972);
    let n4980: ZN = zsel_n(n1567, r_c369, n4973);
    let n4981: ZN = zsel_n(n1567, r_c370, n4974);
    let n4982: ZB = zb_or(n1567, n4975);
    let n4983: ZB = zb_and(n4902, n4982);
    let n4984: ZB = zb_and(n4903, n4982);
    let n4985: ZB = zb_and(n4737, n4984);
    let n4986: ZB = zb_and(n4736, n4984);
    let n4987: ZB = zb_or(n4985, n4986);
    let n4988: ZB = zb_and(n4742, n4987);
    let n4989: ZB = zb_and(n4743, n4987);
    let n4990: ZN = zsel_n(n4742, zn_splat(P8::from_raw(0i32)), n4980);
    let n4991: ZB = zb_or(n4988, n4989);
    let n4992: ZN = zsel_n(n4902, n4980, n4990);
    let n4993: ZB = zb_or(n4983, n4991);
    let n4994: ZN = zsel_n(n4071, zn_splat(P8::from_raw(-231700i32)), n3864);
    let n4995: ZN = zsel_n(n4071, zn_splat(P8::from_raw(-231700i32)), n4758);
    let n4996: ZN = zsel_n(n1435, n4686, n4994);
    let n4997: ZN = zsel_n(n1435, n4691, n4995);
    let n4998: ZN = zsel_n(n1567, r_c369, n4996);
    let n4999: ZN = zsel_n(n1567, r_c370, n4997);
    let n5000: ZN = zsel_n(n4742, zn_splat(P8::from_raw(0i32)), n4998);
    let n5001: ZN = zsel_n(n4902, n4998, n5000);
    let n5002: ZN = zsel_n(n4071, zn_splat(P8::from_raw(231700i32)), n3915);
    let n5003: ZN = zsel_n(n4071, zn_splat(P8::from_raw(-231700i32)), n4785);
    let n5004: ZN = zsel_n(n1435, n4686, n5002);
    let n5005: ZN = zsel_n(n1435, n4691, n5003);
    let n5006: ZN = zsel_n(n1567, r_c369, n5004);
    let n5007: ZN = zsel_n(n1567, r_c370, n5005);
    let n5008: ZN = zsel_n(n4742, zn_splat(P8::from_raw(0i32)), n5006);
    let n5009: ZN = zsel_n(n4902, n5006, n5008);
    let n5010: ZN = zsel_n(n4071, zn_splat(P8::from_raw(131072i32)), r_c360);
    let n5011: ZN = zsel_n(n4071, zn_splat(P8::from_raw(327680i32)), n4698);
    let n5012: ZN = zsel_n(n1435, r_c360, n5010);
    let n5013: ZN = zsel_n(n1435, n4691, n5011);
    let n5014: ZN = zsel_n(n1567, r_c360, n5012);
    let n5015: ZN = zsel_n(n1567, r_c370, n5013);
    let n5016: ZN = zsel_n(n4071, zn_splat(P8::from_raw(231700i32)), n4758);
    let n5017: ZN = zsel_n(n1435, n4691, n5016);
    let n5018: ZN = zsel_n(n1567, r_c370, n5017);
    let n5019: ZN = zsel_n(n4071, zn_splat(P8::from_raw(231700i32)), n4785);
    let n5020: ZN = zsel_n(n1435, n4691, n5019);
    let n5021: ZN = zsel_n(n1567, r_c370, n5020);
    let n5022: ZN = zsel_n(n4071, n3798, n4810);
    let n5023: ZN = zsel_n(n4071, zn_splat(P8::from_raw(0i32)), n4811);
    let n5024: ZN = zsel_n(n1435, n4686, n5022);
    let n5025: ZN = zsel_n(n1435, n4691, n5023);
    let n5026: ZB = zb_and(n3852, n4581);
    let n5027: ZN = zsel_n(n1567, r_c369, n5024);
    let n5028: ZN = zsel_n(n1567, r_c370, n5025);
    let n5029: ZB = zb_or(n1567, n5026);
    let n5030: ZB = zb_and(n4902, n5029);
    let n5031: ZB = zb_and(n4903, n5029);
    let n5032: ZB = zb_and(n4737, n5031);
    let n5033: ZB = zb_and(n4736, n5031);
    let n5034: ZB = zb_or(n5032, n5033);
    let n5035: ZB = zb_and(n4742, n5034);
    let n5036: ZB = zb_and(n4743, n5034);
    let n5037: ZN = zsel_n(n4742, zn_splat(P8::from_raw(0i32)), n5027);
    let n5038: ZB = zb_or(n5035, n5036);
    let n5039: ZN = zsel_n(n4902, n5027, n5037);
    let n5040: ZB = zb_or(n5030, n5038);
    let n5041: ZN = zsel_n(n4071, zn_splat(P8::from_raw(-327680i32)), n4832);
    let n5042: ZN = zsel_n(n4071, zn_splat(P8::from_raw(0i32)), n4833);
    let n5043: ZN = zsel_n(n1435, n4686, n5041);
    let n5044: ZN = zsel_n(n1435, n4691, n5042);
    let n5045: ZB = zb_and(n3852, n4599);
    let n5046: ZN = zsel_n(n1567, r_c369, n5043);
    let n5047: ZN = zsel_n(n1567, r_c370, n5044);
    let n5048: ZB = zb_or(n1567, n5045);
    let n5049: ZB = zb_and(n4902, n5048);
    let n5050: ZB = zb_and(n4903, n5048);
    let n5051: ZB = zb_and(n4737, n5050);
    let n5052: ZB = zb_and(n4736, n5050);
    let n5053: ZB = zb_or(n5051, n5052);
    let n5054: ZB = zb_and(n4742, n5053);
    let n5055: ZB = zb_and(n4743, n5053);
    let n5056: ZN = zsel_n(n4742, zn_splat(P8::from_raw(0i32)), n5046);
    let n5057: ZB = zb_or(n5054, n5055);
    let n5058: ZN = zsel_n(n4902, n5046, n5056);
    let n5059: ZB = zb_or(n5049, n5057);
    let n5060: ZN = zsel_n(n4071, zn_splat(P8::from_raw(327680i32)), n4851);
    let n5061: ZN = zsel_n(n4071, zn_splat(P8::from_raw(0i32)), n4852);
    let n5062: ZN = zsel_n(n1435, n4686, n5060);
    let n5063: ZN = zsel_n(n1435, n4691, n5061);
    let n5064: ZB = zb_and(n3852, n4617);
    let n5065: ZN = zsel_n(n1567, r_c369, n5062);
    let n5066: ZN = zsel_n(n1567, r_c370, n5063);
    let n5067: ZB = zb_or(n1567, n5064);
    let n5068: ZB = zb_and(n4902, n5067);
    let n5069: ZB = zb_and(n4903, n5067);
    let n5070: ZB = zb_and(n4737, n5069);
    let n5071: ZB = zb_and(n4736, n5069);
    let n5072: ZB = zb_or(n5070, n5071);
    let n5073: ZB = zb_and(n4742, n5072);
    let n5074: ZB = zb_and(n4743, n5072);
    let n5075: ZN = zsel_n(n4742, zn_splat(P8::from_raw(0i32)), n5065);
    let n5076: ZB = zb_or(n5073, n5074);
    let n5077: ZN = zsel_n(n4902, n5065, n5075);
    let n5078: ZB = zb_or(n5068, n5076);
    let n5079: ZN = zsel_n(n4071, zn_splat(P8::from_raw(0i32)), n4810);
    let n5080: ZN = zsel_n(n4071, zn_splat(P8::from_raw(-327680i32)), n4811);
    let n5081: ZN = zsel_n(n1435, n4686, n5079);
    let n5082: ZN = zsel_n(n1435, n4691, n5080);
    let n5083: ZB = zb_and(n3852, n4633);
    let n5084: ZN = zsel_n(n1567, r_c369, n5081);
    let n5085: ZN = zsel_n(n1567, r_c370, n5082);
    let n5086: ZB = zb_or(n1567, n5083);
    let n5087: ZB = zb_and(n4902, n5086);
    let n5088: ZB = zb_and(n4903, n5086);
    let n5089: ZB = zb_and(n4737, n5088);
    let n5090: ZB = zb_and(n4736, n5088);
    let n5091: ZB = zb_or(n5089, n5090);
    let n5092: ZB = zb_and(n4742, n5091);
    let n5093: ZB = zb_and(n4743, n5091);
    let n5094: ZN = zsel_n(n4742, zn_splat(P8::from_raw(0i32)), n5084);
    let n5095: ZB = zb_or(n5092, n5093);
    let n5096: ZN = zsel_n(n4902, n5084, n5094);
    let n5097: ZB = zb_or(n5087, n5095);
    let n5098: ZN = zsel_n(n4071, zn_splat(P8::from_raw(-231700i32)), n4832);
    let n5099: ZN = zsel_n(n4071, zn_splat(P8::from_raw(-231700i32)), n4833);
    let n5100: ZN = zsel_n(n1435, n4686, n5098);
    let n5101: ZN = zsel_n(n1435, n4691, n5099);
    let n5102: ZN = zsel_n(n1567, r_c369, n5100);
    let n5103: ZN = zsel_n(n1567, r_c370, n5101);
    let n5104: ZN = zsel_n(n4742, zn_splat(P8::from_raw(0i32)), n5102);
    let n5105: ZN = zsel_n(n4902, n5102, n5104);
    let n5106: ZN = zsel_n(n4071, zn_splat(P8::from_raw(231700i32)), n4851);
    let n5107: ZN = zsel_n(n4071, zn_splat(P8::from_raw(-231700i32)), n4852);
    let n5108: ZN = zsel_n(n1435, n4686, n5106);
    let n5109: ZN = zsel_n(n1435, n4691, n5107);
    let n5110: ZN = zsel_n(n1567, r_c369, n5108);
    let n5111: ZN = zsel_n(n1567, r_c370, n5109);
    let n5112: ZN = zsel_n(n4742, zn_splat(P8::from_raw(0i32)), n5110);
    let n5113: ZN = zsel_n(n4902, n5110, n5112);
    let n5114: ZN = zsel_n(n4071, zn_splat(P8::from_raw(327680i32)), n4811);
    let n5115: ZN = zsel_n(n1435, n4691, n5114);
    let n5116: ZN = zsel_n(n1567, r_c370, n5115);
    let n5117: ZN = zsel_n(n4071, zn_splat(P8::from_raw(231700i32)), n4833);
    let n5118: ZN = zsel_n(n1435, n4691, n5117);
    let n5119: ZN = zsel_n(n1567, r_c370, n5118);
    let n5120: ZN = zsel_n(n4071, zn_splat(P8::from_raw(231700i32)), n4852);
    let n5121: ZN = zsel_n(n1435, n4691, n5120);
    let n5122: ZN = zsel_n(n1567, r_c370, n5121);
    let n5125: ZW = zw_bits_n(r_c39);
    let n5126: ZW = zw_mix1(zw_splat(11400714819323198485u64), n5125, 39u64);
    let n5127: ZW = zw_mix2(zw_splat(11562461410679940143u64), n5125, 39u64);
    let n5128: ZW = zw_bits_n(n115);
    let n5129: ZW = zw_mix1(n5126, n5128, 84u64);
    let n5130: ZW = zw_mix2(n5127, n5128, 84u64);
    let n5131: ZW = zw_bits_n(n217);
    let n5132: ZW = zw_mix1(n5129, n5131, 85u64);
    let n5133: ZW = zw_mix2(n5130, n5131, 85u64);
    let n5134: ZW = zw_bits_n(n216);
    let n5135: ZW = zw_mix1(n5132, n5134, 86u64);
    let n5136: ZW = zw_mix2(n5133, n5134, 86u64);
    let n5137: ZW = zw_bits_n(r_c87);
    let n5138: ZW = zw_mix1(n5135, n5137, 87u64);
    let n5139: ZW = zw_mix2(n5136, n5137, 87u64);
    let n5140: ZW = zw_bits_n(n258);
    let n5141: ZW = zw_mix1(n5138, n5140, 261u64);
    let n5142: ZW = zw_mix2(n5139, n5140, 261u64);
    let n5143: ZW = zw_bits_n(n259);
    let n5144: ZW = zw_mix1(n5141, n5143, 274u64);
    let n5145: ZW = zw_mix2(n5142, n5143, 274u64);
    let n5146: ZW = zw_bits_n(n764);
    let n5147: ZW = zw_mix1(n5144, n5146, 302u64);
    let n5148: ZW = zw_mix2(n5145, n5146, 302u64);
    let n5149: ZW = zw_bits_n(n487);
    let n5150: ZW = zw_mix1(n5147, n5149, 368u64);
    let n5151: ZW = zw_mix2(n5148, n5149, 368u64);
    let n5152: ZW = zw_bits_n(n765);
    let n5153: ZW = zw_mix1(n5150, n5152, 369u64);
    let n5154: ZW = zw_mix2(n5151, n5152, 369u64);
    let n5155: ZW = zw_bits_n(r_c20);
    let n5156: ZW = zw_mix1(n5153, n5155, 20u64);
    let n5157: ZW = zw_mix2(n5154, n5155, 20u64);
    let n5158: ZW = zw_bits_b(r_c41);
    let n5159: ZW = zw_mix1(n5156, n5158, 41u64);
    let n5160: ZW = zw_mix2(n5157, n5158, 41u64);
    let n5161: ZW = zw_bits_n(n1434);
    let n5162: ZW = zw_mix1(n5159, n5161, 282u64);
    let n5163: ZW = zw_mix2(n5160, n5161, 282u64);
    let n5164: ZW = zw_bits_n(n1562);
    let n5165: ZW = zw_mix1(n5162, n5164, 284u64);
    let n5166: ZW = zw_mix2(n5163, n5164, 284u64);
    let n5167: u64 = P8::from_raw(65536i32).as_raw_u32() as u64;
    let n5168: ZW = zw_mix1(n5165, zw_splat(n5167), 285u64);
    let n5169: ZW = zw_mix2(n5166, zw_splat(n5167), 285u64);
    let n5170: ZW = zw_bits_n(n1432);
    let n5171: ZW = zw_mix1(n5168, n5170, 287u64);
    let n5172: ZW = zw_mix2(n5169, n5170, 287u64);
    let n5173: u64 = false as u64;
    let n5174: ZW = zw_mix1(n5171, zw_splat(n5173), 294u64);
    let n5175: ZW = zw_mix2(n5172, zw_splat(n5173), 294u64);
    let n5176: ZW = zw_mix1(n5174, zw_splat(n5173), 295u64);
    let n5177: ZW = zw_mix2(n5175, zw_splat(n5173), 295u64);
    let n5178: ZW = zw_bits_n(n1584);
    let n5179: ZW = zw_mix1(n5176, n5178, 301u64);
    let n5180: ZW = zw_mix2(n5177, n5178, 301u64);
    let n5181: ZW = zw_bits_n(r_c357);
    let n5182: ZW = zw_mix1(n5179, n5181, 358u64);
    let n5183: ZW = zw_mix2(n5180, n5181, 358u64);
    let n5184: ZW = zw_bits_n(r_c358);
    let n5185: ZW = zw_mix1(n5182, n5184, 359u64);
    let n5186: ZW = zw_mix2(n5183, n5184, 359u64);
    let n5187: ZW = zw_bits_n(r_c359);
    let n5188: ZW = zw_mix1(n5185, n5187, 360u64);
    let n5189: ZW = zw_mix2(n5186, n5187, 360u64);
    let n5190: ZW = zw_bits_n(r_c360);
    let n5191: ZW = zw_mix1(n5188, n5190, 361u64);
    let n5192: ZW = zw_mix2(n5189, n5190, 361u64);
    let n5193: ZW = zw_bits_b(n1563);
    let n5194: ZW = zw_mix1(n5191, n5193, 362u64);
    let n5195: ZW = zw_mix2(n5192, n5193, 362u64);
    let n5196: ZW = zw_bits_n(n1585);
    let n5197: ZW = zw_mix1(n5194, n5196, 370u64);
    let n5198: ZW = zw_mix2(n5195, n5196, 370u64);
    let n5199: ZW = zw_bits_n(n1565);
    let n5200: ZW = zw_mix1(n5197, n5199, 371u64);
    let n5201: ZW = zw_mix2(n5198, n5199, 371u64);
    let n5202: ZW = zw_bits_b(n1641);
    let n5203: ZW = zw_mix1(n5191, n5202, 362u64);
    let n5204: ZW = zw_mix2(n5192, n5202, 362u64);
    let n5205: ZW = zw_bits_n(n1653);
    let n5206: ZW = zw_mix1(n5203, n5205, 370u64);
    let n5207: ZW = zw_mix2(n5204, n5205, 370u64);
    let n5208: ZW = zw_bits_n(n1643);
    let n5209: ZW = zw_mix1(n5206, n5208, 371u64);
    let n5210: ZW = zw_mix2(n5207, n5208, 371u64);
    let n5211: ZW = zw_bits_b(n1697);
    let n5212: ZW = zw_mix1(n5191, n5211, 362u64);
    let n5213: ZW = zw_mix2(n5192, n5211, 362u64);
    let n5214: ZW = zw_bits_n(n1709);
    let n5215: ZW = zw_mix1(n5212, n5214, 370u64);
    let n5216: ZW = zw_mix2(n5213, n5214, 370u64);
    let n5217: ZW = zw_bits_n(n1699);
    let n5218: ZW = zw_mix1(n5215, n5217, 371u64);
    let n5219: ZW = zw_mix2(n5216, n5217, 371u64);
    let n5220: ZW = zw_bits_n(n1740);
    let n5221: ZW = zw_mix1(n5168, n5220, 287u64);
    let n5222: ZW = zw_mix2(n5169, n5220, 287u64);
    let n5223: ZW = zw_mix1(n5221, zw_splat(n5173), 294u64);
    let n5224: ZW = zw_mix2(n5222, zw_splat(n5173), 294u64);
    let n5225: u64 = true as u64;
    let n5226: ZW = zw_mix1(n5223, zw_splat(n5225), 295u64);
    let n5227: ZW = zw_mix2(n5224, zw_splat(n5225), 295u64);
    let n5228: ZW = zw_mix1(n5226, n5178, 301u64);
    let n5229: ZW = zw_mix2(n5227, n5178, 301u64);
    let n5230: ZW = zw_mix1(n5228, n5181, 358u64);
    let n5231: ZW = zw_mix2(n5229, n5181, 358u64);
    let n5232: ZW = zw_mix1(n5230, n5184, 359u64);
    let n5233: ZW = zw_mix2(n5231, n5184, 359u64);
    let n5234: ZW = zw_mix1(n5232, n5187, 360u64);
    let n5235: ZW = zw_mix2(n5233, n5187, 360u64);
    let n5236: ZW = zw_mix1(n5234, n5190, 361u64);
    let n5237: ZW = zw_mix2(n5235, n5190, 361u64);
    let n5238: ZW = zw_mix1(n5236, n5193, 362u64);
    let n5239: ZW = zw_mix2(n5237, n5193, 362u64);
    let n5240: ZW = zw_bits_n(n1752);
    let n5241: ZW = zw_mix1(n5238, n5240, 370u64);
    let n5242: ZW = zw_mix2(n5239, n5240, 370u64);
    let n5243: ZW = zw_bits_n(n1742);
    let n5244: ZW = zw_mix1(n5241, n5243, 371u64);
    let n5245: ZW = zw_mix2(n5242, n5243, 371u64);
    let n5246: ZW = zw_mix1(n5236, n5202, 362u64);
    let n5247: ZW = zw_mix2(n5237, n5202, 362u64);
    let n5248: ZW = zw_bits_n(n1793);
    let n5249: ZW = zw_mix1(n5246, n5248, 370u64);
    let n5250: ZW = zw_mix2(n5247, n5248, 370u64);
    let n5251: ZW = zw_bits_n(n1783);
    let n5252: ZW = zw_mix1(n5249, n5251, 371u64);
    let n5253: ZW = zw_mix2(n5250, n5251, 371u64);
    let n5254: ZW = zw_mix1(n5236, n5211, 362u64);
    let n5255: ZW = zw_mix2(n5237, n5211, 362u64);
    let n5256: ZW = zw_bits_n(n1834);
    let n5257: ZW = zw_mix1(n5254, n5256, 370u64);
    let n5258: ZW = zw_mix2(n5255, n5256, 370u64);
    let n5259: ZW = zw_bits_n(n1824);
    let n5260: ZW = zw_mix1(n5257, n5259, 371u64);
    let n5261: ZW = zw_mix2(n5258, n5259, 371u64);
    let n5262: ZW = zw_bits_n(n1861);
    let n5263: ZW = zw_mix1(n5153, n5262, 20u64);
    let n5264: ZW = zw_mix2(n5154, n5262, 20u64);
    let n5265: ZW = zw_bits_b(n1862);
    let n5266: ZW = zw_mix1(n5263, n5265, 41u64);
    let n5267: ZW = zw_mix2(n5264, n5265, 41u64);
    let n5268: ZW = zw_bits_n(n1863);
    let n5269: ZW = zw_mix1(n5266, n5268, 282u64);
    let n5270: ZW = zw_mix2(n5267, n5268, 282u64);
    let n5271: ZW = zw_bits_n(n1864);
    let n5272: ZW = zw_mix1(n5269, n5271, 284u64);
    let n5273: ZW = zw_mix2(n5270, n5271, 284u64);
    let n5274: ZW = zw_bits_n(n1865);
    let n5275: ZW = zw_mix1(n5272, n5274, 285u64);
    let n5276: ZW = zw_mix2(n5273, n5274, 285u64);
    let n5277: ZW = zw_mix1(n5275, n5170, 287u64);
    let n5278: ZW = zw_mix2(n5276, n5170, 287u64);
    let n5279: ZW = zw_mix1(n5277, zw_splat(n5225), 294u64);
    let n5280: ZW = zw_mix2(n5278, zw_splat(n5225), 294u64);
    let n5281: ZW = zw_mix1(n5279, zw_splat(n5173), 295u64);
    let n5282: ZW = zw_mix2(n5280, zw_splat(n5173), 295u64);
    let n5283: ZW = zw_bits_n(n1884);
    let n5284: ZW = zw_mix1(n5281, n5283, 301u64);
    let n5285: ZW = zw_mix2(n5282, n5283, 301u64);
    let n5286: ZW = zw_bits_n(n1866);
    let n5287: ZW = zw_mix1(n5284, n5286, 358u64);
    let n5288: ZW = zw_mix2(n5285, n5286, 358u64);
    let n5289: ZW = zw_bits_n(n1867);
    let n5290: ZW = zw_mix1(n5287, n5289, 359u64);
    let n5291: ZW = zw_mix2(n5288, n5289, 359u64);
    let n5292: ZW = zw_bits_n(n1868);
    let n5293: ZW = zw_mix1(n5290, n5292, 360u64);
    let n5294: ZW = zw_mix2(n5291, n5292, 360u64);
    let n5295: ZW = zw_bits_n(n1869);
    let n5296: ZW = zw_mix1(n5293, n5295, 361u64);
    let n5297: ZW = zw_mix2(n5294, n5295, 361u64);
    let n5298: ZW = zw_mix1(n5296, n5193, 362u64);
    let n5299: ZW = zw_mix2(n5297, n5193, 362u64);
    let n5300: ZW = zw_bits_n(n1885);
    let n5301: ZW = zw_mix1(n5298, n5300, 370u64);
    let n5302: ZW = zw_mix2(n5299, n5300, 370u64);
    let n5303: ZW = zw_bits_n(n1871);
    let n5304: ZW = zw_mix1(n5301, n5303, 371u64);
    let n5305: ZW = zw_mix2(n5302, n5303, 371u64);
    let n5306: ZW = zw_bits_n(n1894);
    let n5307: ZW = zw_mix1(n5287, n5306, 359u64);
    let n5308: ZW = zw_mix2(n5288, n5306, 359u64);
    let n5309: ZW = zw_bits_n(n1895);
    let n5310: ZW = zw_mix1(n5307, n5309, 360u64);
    let n5311: ZW = zw_mix2(n5308, n5309, 360u64);
    let n5312: ZW = zw_mix1(n5310, n5295, 361u64);
    let n5313: ZW = zw_mix2(n5311, n5295, 361u64);
    let n5314: ZW = zw_mix1(n5312, n5202, 362u64);
    let n5315: ZW = zw_mix2(n5313, n5202, 362u64);
    let n5316: ZW = zw_bits_n(n1908);
    let n5317: ZW = zw_mix1(n5314, n5316, 370u64);
    let n5318: ZW = zw_mix2(n5315, n5316, 370u64);
    let n5319: ZW = zw_bits_n(n1897);
    let n5320: ZW = zw_mix1(n5317, n5319, 371u64);
    let n5321: ZW = zw_mix2(n5318, n5319, 371u64);
    let n5322: ZW = zw_bits_n(n1916);
    let n5323: ZW = zw_mix1(n5307, n5322, 360u64);
    let n5324: ZW = zw_mix2(n5308, n5322, 360u64);
    let n5325: ZW = zw_mix1(n5323, n5295, 361u64);
    let n5326: ZW = zw_mix2(n5324, n5295, 361u64);
    let n5327: ZW = zw_mix1(n5325, n5211, 362u64);
    let n5328: ZW = zw_mix2(n5326, n5211, 362u64);
    let n5329: ZW = zw_bits_n(n1929);
    let n5330: ZW = zw_mix1(n5327, n5329, 370u64);
    let n5331: ZW = zw_mix2(n5328, n5329, 370u64);
    let n5332: ZW = zw_bits_n(n1918);
    let n5333: ZW = zw_mix1(n5330, n5332, 371u64);
    let n5334: ZW = zw_mix2(n5331, n5332, 371u64);
    let n5335: ZW = zw_bits_n(n1939);
    let n5336: ZW = zw_mix1(n5284, n5335, 358u64);
    let n5337: ZW = zw_mix2(n5285, n5335, 358u64);
    let n5338: ZW = zw_bits_n(n1940);
    let n5339: ZW = zw_mix1(n5336, n5338, 359u64);
    let n5340: ZW = zw_mix2(n5337, n5338, 359u64);
    let n5341: ZW = zw_bits_n(n1941);
    let n5342: ZW = zw_mix1(n5339, n5341, 360u64);
    let n5343: ZW = zw_mix2(n5340, n5341, 360u64);
    let n5344: ZW = zw_bits_n(n1942);
    let n5345: ZW = zw_mix1(n5342, n5344, 361u64);
    let n5346: ZW = zw_mix2(n5343, n5344, 361u64);
    let n5347: ZW = zw_mix1(n5345, n5193, 362u64);
    let n5348: ZW = zw_mix2(n5346, n5193, 362u64);
    let n5349: ZW = zw_bits_n(n1955);
    let n5350: ZW = zw_mix1(n5347, n5349, 370u64);
    let n5351: ZW = zw_mix2(n5348, n5349, 370u64);
    let n5352: ZW = zw_bits_n(n1944);
    let n5353: ZW = zw_mix1(n5350, n5352, 371u64);
    let n5354: ZW = zw_mix2(n5351, n5352, 371u64);
    let n5355: ZW = zw_mix1(n5336, n5306, 359u64);
    let n5356: ZW = zw_mix2(n5337, n5306, 359u64);
    let n5357: ZW = zw_mix1(n5355, n5309, 360u64);
    let n5358: ZW = zw_mix2(n5356, n5309, 360u64);
    let n5359: ZW = zw_mix1(n5357, n5344, 361u64);
    let n5360: ZW = zw_mix2(n5358, n5344, 361u64);
    let n5361: ZW = zw_mix1(n5359, n5202, 362u64);
    let n5362: ZW = zw_mix2(n5360, n5202, 362u64);
    let n5363: ZW = zw_bits_n(n1962);
    let n5364: ZW = zw_mix1(n5361, n5363, 370u64);
    let n5365: ZW = zw_mix2(n5362, n5363, 370u64);
    let n5366: ZW = zw_bits_n(n1960);
    let n5367: ZW = zw_mix1(n5364, n5366, 371u64);
    let n5368: ZW = zw_mix2(n5365, n5366, 371u64);
    let n5369: ZW = zw_mix1(n5355, n5322, 360u64);
    let n5370: ZW = zw_mix2(n5356, n5322, 360u64);
    let n5371: ZW = zw_mix1(n5369, n5344, 361u64);
    let n5372: ZW = zw_mix2(n5370, n5344, 361u64);
    let n5373: ZW = zw_mix1(n5371, n5211, 362u64);
    let n5374: ZW = zw_mix2(n5372, n5211, 362u64);
    let n5375: ZW = zw_bits_n(n1968);
    let n5376: ZW = zw_mix1(n5373, n5375, 370u64);
    let n5377: ZW = zw_mix2(n5374, n5375, 370u64);
    let n5378: ZW = zw_bits_n(n1966);
    let n5379: ZW = zw_mix1(n5376, n5378, 371u64);
    let n5380: ZW = zw_mix2(n5377, n5378, 371u64);
    let n5381: ZW = zw_bits_n(n1971);
    let n5382: ZW = zw_mix1(n5342, n5381, 361u64);
    let n5383: ZW = zw_mix2(n5343, n5381, 361u64);
    let n5384: ZW = zw_mix1(n5382, n5193, 362u64);
    let n5385: ZW = zw_mix2(n5383, n5193, 362u64);
    let n5386: ZW = zw_mix1(n5384, n5349, 370u64);
    let n5387: ZW = zw_mix2(n5385, n5349, 370u64);
    let n5388: ZW = zw_bits_n(n1972);
    let n5389: ZW = zw_mix1(n5386, n5388, 371u64);
    let n5390: ZW = zw_mix2(n5387, n5388, 371u64);
    let n5391: ZW = zw_mix1(n5357, n5381, 361u64);
    let n5392: ZW = zw_mix2(n5358, n5381, 361u64);
    let n5393: ZW = zw_mix1(n5391, n5202, 362u64);
    let n5394: ZW = zw_mix2(n5392, n5202, 362u64);
    let n5395: ZW = zw_mix1(n5393, n5363, 370u64);
    let n5396: ZW = zw_mix2(n5394, n5363, 370u64);
    let n5397: ZW = zw_bits_n(n1974);
    let n5398: ZW = zw_mix1(n5395, n5397, 371u64);
    let n5399: ZW = zw_mix2(n5396, n5397, 371u64);
    let n5400: ZW = zw_mix1(n5369, n5381, 361u64);
    let n5401: ZW = zw_mix2(n5370, n5381, 361u64);
    let n5402: ZW = zw_mix1(n5400, n5211, 362u64);
    let n5403: ZW = zw_mix2(n5401, n5211, 362u64);
    let n5404: ZW = zw_mix1(n5402, n5375, 370u64);
    let n5405: ZW = zw_mix2(n5403, n5375, 370u64);
    let n5406: ZW = zw_bits_n(n1976);
    let n5407: ZW = zw_mix1(n5404, n5406, 371u64);
    let n5408: ZW = zw_mix2(n5405, n5406, 371u64);
    let n5409: ZW = zw_mix1(n5275, n5220, 287u64);
    let n5410: ZW = zw_mix2(n5276, n5220, 287u64);
    let n5411: ZW = zw_mix1(n5409, zw_splat(n5225), 294u64);
    let n5412: ZW = zw_mix2(n5410, zw_splat(n5225), 294u64);
    let n5413: ZW = zw_mix1(n5411, zw_splat(n5225), 295u64);
    let n5414: ZW = zw_mix2(n5412, zw_splat(n5225), 295u64);
    let n5415: ZW = zw_mix1(n5413, n5283, 301u64);
    let n5416: ZW = zw_mix2(n5414, n5283, 301u64);
    let n5417: ZW = zw_mix1(n5415, n5286, 358u64);
    let n5418: ZW = zw_mix2(n5416, n5286, 358u64);
    let n5419: ZW = zw_mix1(n5417, n5289, 359u64);
    let n5420: ZW = zw_mix2(n5418, n5289, 359u64);
    let n5421: ZW = zw_mix1(n5419, n5292, 360u64);
    let n5422: ZW = zw_mix2(n5420, n5292, 360u64);
    let n5423: ZW = zw_mix1(n5421, n5295, 361u64);
    let n5424: ZW = zw_mix2(n5422, n5295, 361u64);
    let n5425: ZW = zw_mix1(n5423, n5193, 362u64);
    let n5426: ZW = zw_mix2(n5424, n5193, 362u64);
    let n5427: ZW = zw_bits_n(n2006);
    let n5428: ZW = zw_mix1(n5425, n5427, 370u64);
    let n5429: ZW = zw_mix2(n5426, n5427, 370u64);
    let n5430: ZW = zw_bits_n(n1995);
    let n5431: ZW = zw_mix1(n5428, n5430, 371u64);
    let n5432: ZW = zw_mix2(n5429, n5430, 371u64);
    let n5433: ZW = zw_mix1(n5417, n5306, 359u64);
    let n5434: ZW = zw_mix2(n5418, n5306, 359u64);
    let n5435: ZW = zw_mix1(n5433, n5309, 360u64);
    let n5436: ZW = zw_mix2(n5434, n5309, 360u64);
    let n5437: ZW = zw_mix1(n5435, n5295, 361u64);
    let n5438: ZW = zw_mix2(n5436, n5295, 361u64);
    let n5439: ZW = zw_mix1(n5437, n5202, 362u64);
    let n5440: ZW = zw_mix2(n5438, n5202, 362u64);
    let n5441: ZW = zw_bits_n(n2025);
    let n5442: ZW = zw_mix1(n5439, n5441, 370u64);
    let n5443: ZW = zw_mix2(n5440, n5441, 370u64);
    let n5444: ZW = zw_bits_n(n2014);
    let n5445: ZW = zw_mix1(n5442, n5444, 371u64);
    let n5446: ZW = zw_mix2(n5443, n5444, 371u64);
    let n5447: ZW = zw_mix1(n5433, n5322, 360u64);
    let n5448: ZW = zw_mix2(n5434, n5322, 360u64);
    let n5449: ZW = zw_mix1(n5447, n5295, 361u64);
    let n5450: ZW = zw_mix2(n5448, n5295, 361u64);
    let n5451: ZW = zw_mix1(n5449, n5211, 362u64);
    let n5452: ZW = zw_mix2(n5450, n5211, 362u64);
    let n5453: ZW = zw_bits_n(n2044);
    let n5454: ZW = zw_mix1(n5451, n5453, 370u64);
    let n5455: ZW = zw_mix2(n5452, n5453, 370u64);
    let n5456: ZW = zw_bits_n(n2033);
    let n5457: ZW = zw_mix1(n5454, n5456, 371u64);
    let n5458: ZW = zw_mix2(n5455, n5456, 371u64);
    let n5459: ZW = zw_mix1(n5415, n5335, 358u64);
    let n5460: ZW = zw_mix2(n5416, n5335, 358u64);
    let n5461: ZW = zw_mix1(n5459, n5338, 359u64);
    let n5462: ZW = zw_mix2(n5460, n5338, 359u64);
    let n5463: ZW = zw_mix1(n5461, n5341, 360u64);
    let n5464: ZW = zw_mix2(n5462, n5341, 360u64);
    let n5465: ZW = zw_mix1(n5463, n5344, 361u64);
    let n5466: ZW = zw_mix2(n5464, n5344, 361u64);
    let n5467: ZW = zw_mix1(n5465, n5193, 362u64);
    let n5468: ZW = zw_mix2(n5466, n5193, 362u64);
    let n5469: ZW = zw_bits_n(n2061);
    let n5470: ZW = zw_mix1(n5467, n5469, 370u64);
    let n5471: ZW = zw_mix2(n5468, n5469, 370u64);
    let n5472: ZW = zw_bits_n(n2050);
    let n5473: ZW = zw_mix1(n5470, n5472, 371u64);
    let n5474: ZW = zw_mix2(n5471, n5472, 371u64);
    let n5475: ZW = zw_mix1(n5459, n5306, 359u64);
    let n5476: ZW = zw_mix2(n5460, n5306, 359u64);
    let n5477: ZW = zw_mix1(n5475, n5309, 360u64);
    let n5478: ZW = zw_mix2(n5476, n5309, 360u64);
    let n5479: ZW = zw_mix1(n5477, n5344, 361u64);
    let n5480: ZW = zw_mix2(n5478, n5344, 361u64);
    let n5481: ZW = zw_mix1(n5479, n5202, 362u64);
    let n5482: ZW = zw_mix2(n5480, n5202, 362u64);
    let n5483: ZW = zw_bits_n(n2068);
    let n5484: ZW = zw_mix1(n5481, n5483, 370u64);
    let n5485: ZW = zw_mix2(n5482, n5483, 370u64);
    let n5486: ZW = zw_bits_n(n2066);
    let n5487: ZW = zw_mix1(n5484, n5486, 371u64);
    let n5488: ZW = zw_mix2(n5485, n5486, 371u64);
    let n5489: ZW = zw_mix1(n5475, n5322, 360u64);
    let n5490: ZW = zw_mix2(n5476, n5322, 360u64);
    let n5491: ZW = zw_mix1(n5489, n5344, 361u64);
    let n5492: ZW = zw_mix2(n5490, n5344, 361u64);
    let n5493: ZW = zw_mix1(n5491, n5211, 362u64);
    let n5494: ZW = zw_mix2(n5492, n5211, 362u64);
    let n5495: ZW = zw_bits_n(n2074);
    let n5496: ZW = zw_mix1(n5493, n5495, 370u64);
    let n5497: ZW = zw_mix2(n5494, n5495, 370u64);
    let n5498: ZW = zw_bits_n(n2072);
    let n5499: ZW = zw_mix1(n5496, n5498, 371u64);
    let n5500: ZW = zw_mix2(n5497, n5498, 371u64);
    let n5501: ZW = zw_mix1(n5463, n5381, 361u64);
    let n5502: ZW = zw_mix2(n5464, n5381, 361u64);
    let n5503: ZW = zw_mix1(n5501, n5193, 362u64);
    let n5504: ZW = zw_mix2(n5502, n5193, 362u64);
    let n5505: ZW = zw_mix1(n5503, n5469, 370u64);
    let n5506: ZW = zw_mix2(n5504, n5469, 370u64);
    let n5507: ZW = zw_bits_n(n2076);
    let n5508: ZW = zw_mix1(n5505, n5507, 371u64);
    let n5509: ZW = zw_mix2(n5506, n5507, 371u64);
    let n5510: ZW = zw_mix1(n5477, n5381, 361u64);
    let n5511: ZW = zw_mix2(n5478, n5381, 361u64);
    let n5512: ZW = zw_mix1(n5510, n5202, 362u64);
    let n5513: ZW = zw_mix2(n5511, n5202, 362u64);
    let n5514: ZW = zw_mix1(n5512, n5483, 370u64);
    let n5515: ZW = zw_mix2(n5513, n5483, 370u64);
    let n5516: ZW = zw_bits_n(n2078);
    let n5517: ZW = zw_mix1(n5514, n5516, 371u64);
    let n5518: ZW = zw_mix2(n5515, n5516, 371u64);
    let n5519: ZW = zw_mix1(n5489, n5381, 361u64);
    let n5520: ZW = zw_mix2(n5490, n5381, 361u64);
    let n5521: ZW = zw_mix1(n5519, n5211, 362u64);
    let n5522: ZW = zw_mix2(n5520, n5211, 362u64);
    let n5523: ZW = zw_mix1(n5521, n5495, 370u64);
    let n5524: ZW = zw_mix2(n5522, n5495, 370u64);
    let n5525: ZW = zw_bits_n(n2080);
    let n5526: ZW = zw_mix1(n5523, n5525, 371u64);
    let n5527: ZW = zw_mix2(n5524, n5525, 371u64);
    let n5528: ZW = zw_mix1(zw_splat(11400714819323198485u64), n5128, 84u64);
    let n5529: ZW = zw_mix2(zw_splat(11562461410679940143u64), n5128, 84u64);
    let n5530: ZW = zw_mix1(n5528, n5131, 85u64);
    let n5531: ZW = zw_mix2(n5529, n5131, 85u64);
    let n5532: ZW = zw_mix1(n5530, n5134, 86u64);
    let n5533: ZW = zw_mix2(n5531, n5134, 86u64);
    let n5534: ZW = zw_bits_n(n2081);
    let n5535: ZW = zw_mix1(n5532, n5534, 87u64);
    let n5536: ZW = zw_mix2(n5533, n5534, 87u64);
    let n5537: ZW = zw_mix1(n5535, n5140, 260u64);
    let n5538: ZW = zw_mix2(n5536, n5140, 260u64);
    let n5539: ZW = zw_mix1(n5537, n5143, 273u64);
    let n5540: ZW = zw_mix2(n5538, n5143, 273u64);
    let n5541: ZW = zw_mix1(n5539, n5155, 20u64);
    let n5542: ZW = zw_mix2(n5540, n5155, 20u64);
    let n5543: ZW = zw_mix1(n5541, n5158, 41u64);
    let n5544: ZW = zw_mix2(n5542, n5158, 41u64);
    let n5545: ZW = zw_mix1(n5539, n5262, 20u64);
    let n5546: ZW = zw_mix2(n5540, n5262, 20u64);
    let n5547: ZW = zw_mix1(n5545, n5265, 41u64);
    let n5548: ZW = zw_mix2(n5546, n5265, 41u64);
    let n5549: ZW = zw_bits_n(n3859);
    let n5550: ZW = zw_mix1(n5532, n5549, 87u64);
    let n5551: ZW = zw_mix2(n5533, n5549, 87u64);
    let n5552: ZW = zw_bits_n(n2475);
    let n5553: ZW = zw_mix1(n5550, n5552, 259u64);
    let n5554: ZW = zw_mix2(n5551, n5552, 259u64);
    let n5555: ZW = zw_bits_n(n2476);
    let n5556: ZW = zw_mix1(n5553, n5555, 272u64);
    let n5557: ZW = zw_mix2(n5554, n5555, 272u64);
    let n5558: ZW = zw_mix1(n5556, n5155, 20u64);
    let n5559: ZW = zw_mix2(n5557, n5155, 20u64);
    let n5560: ZW = zw_mix1(n5558, n5158, 41u64);
    let n5561: ZW = zw_mix2(n5559, n5158, 41u64);
    let n5562: ZW = zw_bits_n(n4075);
    let n5563: ZW = zw_mix1(n5556, n5562, 20u64);
    let n5564: ZW = zw_mix2(n5557, n5562, 20u64);
    let n5565: ZW = zw_bits_b(n4076);
    let n5566: ZW = zw_mix1(n5563, n5565, 41u64);
    let n5567: ZW = zw_mix2(n5564, n5565, 41u64);
    let n5568: ZW = zw_mix1(n5532, n5155, 20u64);
    let n5569: ZW = zw_mix2(n5533, n5155, 20u64);
    let n5570: ZW = zw_bits_b(n4257);
    let n5571: ZW = zw_mix1(n5568, n5570, 38u64);
    let n5572: ZW = zw_mix2(n5569, n5570, 38u64);
    let n5573: ZW = zw_bits_n(n4262);
    let n5574: ZW = zw_mix1(n5571, n5573, 39u64);
    let n5575: ZW = zw_mix2(n5572, n5573, 39u64);
    let n5576: ZW = zw_bits_n(n4261);
    let n5577: ZW = zw_mix1(n5574, n5576, 87u64);
    let n5578: ZW = zw_mix2(n5575, n5576, 87u64);
    let n5579: ZW = zw_bits_b(n4301);
    let n5580: ZW = zw_mix1(n5568, n5579, 38u64);
    let n5581: ZW = zw_mix2(n5569, n5579, 38u64);
    let n5582: ZW = zw_bits_n(n4306);
    let n5583: ZW = zw_mix1(n5580, n5582, 39u64);
    let n5584: ZW = zw_mix2(n5581, n5582, 39u64);
    let n5585: ZW = zw_bits_n(n4305);
    let n5586: ZW = zw_mix1(n5583, n5585, 87u64);
    let n5587: ZW = zw_mix2(n5584, n5585, 87u64);
    let n5588: ZW = zw_bits_b(n4345);
    let n5589: ZW = zw_mix1(n5568, n5588, 38u64);
    let n5590: ZW = zw_mix2(n5569, n5588, 38u64);
    let n5591: ZW = zw_bits_n(n4350);
    let n5592: ZW = zw_mix1(n5589, n5591, 39u64);
    let n5593: ZW = zw_mix2(n5590, n5591, 39u64);
    let n5594: ZW = zw_bits_n(n4349);
    let n5595: ZW = zw_mix1(n5592, n5594, 87u64);
    let n5596: ZW = zw_mix2(n5593, n5594, 87u64);
    let n5597: ZW = zw_bits_b(n4388);
    let n5598: ZW = zw_mix1(n5568, n5597, 38u64);
    let n5599: ZW = zw_mix2(n5569, n5597, 38u64);
    let n5600: ZW = zw_bits_n(n4393);
    let n5601: ZW = zw_mix1(n5598, n5600, 39u64);
    let n5602: ZW = zw_mix2(n5599, n5600, 39u64);
    let n5603: ZW = zw_bits_n(n4392);
    let n5604: ZW = zw_mix1(n5601, n5603, 87u64);
    let n5605: ZW = zw_mix2(n5602, n5603, 87u64);
    let n5606: ZW = zw_bits_b(n4431);
    let n5607: ZW = zw_mix1(n5568, n5606, 38u64);
    let n5608: ZW = zw_mix2(n5569, n5606, 38u64);
    let n5609: ZW = zw_bits_n(n4436);
    let n5610: ZW = zw_mix1(n5607, n5609, 39u64);
    let n5611: ZW = zw_mix2(n5608, n5609, 39u64);
    let n5612: ZW = zw_bits_n(n4435);
    let n5613: ZW = zw_mix1(n5610, n5612, 87u64);
    let n5614: ZW = zw_mix2(n5611, n5612, 87u64);
    let n5615: ZW = zw_bits_b(n4474);
    let n5616: ZW = zw_mix1(n5568, n5615, 38u64);
    let n5617: ZW = zw_mix2(n5569, n5615, 38u64);
    let n5618: ZW = zw_bits_n(n4479);
    let n5619: ZW = zw_mix1(n5616, n5618, 39u64);
    let n5620: ZW = zw_mix2(n5617, n5618, 39u64);
    let n5621: ZW = zw_bits_n(n4478);
    let n5622: ZW = zw_mix1(n5619, n5621, 87u64);
    let n5623: ZW = zw_mix2(n5620, n5621, 87u64);
    let n5624: ZW = zw_mix1(n5532, n5562, 20u64);
    let n5625: ZW = zw_mix2(n5533, n5562, 20u64);
    let n5626: ZW = zw_bits_b(n4502);
    let n5627: ZW = zw_mix1(n5624, n5626, 38u64);
    let n5628: ZW = zw_mix2(n5625, n5626, 38u64);
    let n5629: ZW = zw_bits_n(n4509);
    let n5630: ZW = zw_mix1(n5627, n5629, 39u64);
    let n5631: ZW = zw_mix2(n5628, n5629, 39u64);
    let n5632: ZW = zw_bits_n(n4508);
    let n5633: ZW = zw_mix1(n5630, n5632, 87u64);
    let n5634: ZW = zw_mix2(n5631, n5632, 87u64);
    let n5635: ZW = zw_bits_b(n4520);
    let n5636: ZW = zw_mix1(n5624, n5635, 38u64);
    let n5637: ZW = zw_mix2(n5625, n5635, 38u64);
    let n5638: ZW = zw_bits_n(n4527);
    let n5639: ZW = zw_mix1(n5636, n5638, 39u64);
    let n5640: ZW = zw_mix2(n5637, n5638, 39u64);
    let n5641: ZW = zw_bits_n(n4526);
    let n5642: ZW = zw_mix1(n5639, n5641, 87u64);
    let n5643: ZW = zw_mix2(n5640, n5641, 87u64);
    let n5644: ZW = zw_bits_b(n4538);
    let n5645: ZW = zw_mix1(n5624, n5644, 38u64);
    let n5646: ZW = zw_mix2(n5625, n5644, 38u64);
    let n5647: ZW = zw_bits_n(n4545);
    let n5648: ZW = zw_mix1(n5645, n5647, 39u64);
    let n5649: ZW = zw_mix2(n5646, n5647, 39u64);
    let n5650: ZW = zw_bits_n(n4544);
    let n5651: ZW = zw_mix1(n5648, n5650, 87u64);
    let n5652: ZW = zw_mix2(n5649, n5650, 87u64);
    let n5653: ZW = zw_bits_b(n4554);
    let n5654: ZW = zw_mix1(n5624, n5653, 38u64);
    let n5655: ZW = zw_mix2(n5625, n5653, 38u64);
    let n5656: ZW = zw_bits_n(n4561);
    let n5657: ZW = zw_mix1(n5654, n5656, 39u64);
    let n5658: ZW = zw_mix2(n5655, n5656, 39u64);
    let n5659: ZW = zw_bits_n(n4560);
    let n5660: ZW = zw_mix1(n5657, n5659, 87u64);
    let n5661: ZW = zw_mix2(n5658, n5659, 87u64);
    let n5662: ZW = zw_bits_b(n4584);
    let n5663: ZW = zw_mix1(n5624, n5662, 38u64);
    let n5664: ZW = zw_mix2(n5625, n5662, 38u64);
    let n5665: ZW = zw_bits_n(n4591);
    let n5666: ZW = zw_mix1(n5663, n5665, 39u64);
    let n5667: ZW = zw_mix2(n5664, n5665, 39u64);
    let n5668: ZW = zw_bits_n(n4590);
    let n5669: ZW = zw_mix1(n5666, n5668, 87u64);
    let n5670: ZW = zw_mix2(n5667, n5668, 87u64);
    let n5671: ZW = zw_bits_b(n4602);
    let n5672: ZW = zw_mix1(n5624, n5671, 38u64);
    let n5673: ZW = zw_mix2(n5625, n5671, 38u64);
    let n5674: ZW = zw_bits_n(n4609);
    let n5675: ZW = zw_mix1(n5672, n5674, 39u64);
    let n5676: ZW = zw_mix2(n5673, n5674, 39u64);
    let n5677: ZW = zw_bits_n(n4608);
    let n5678: ZW = zw_mix1(n5675, n5677, 87u64);
    let n5679: ZW = zw_mix2(n5676, n5677, 87u64);
    let n5680: ZW = zw_bits_b(n4620);
    let n5681: ZW = zw_mix1(n5624, n5680, 38u64);
    let n5682: ZW = zw_mix2(n5625, n5680, 38u64);
    let n5683: ZW = zw_bits_n(n4627);
    let n5684: ZW = zw_mix1(n5681, n5683, 39u64);
    let n5685: ZW = zw_mix2(n5682, n5683, 39u64);
    let n5686: ZW = zw_bits_n(n4626);
    let n5687: ZW = zw_mix1(n5684, n5686, 87u64);
    let n5688: ZW = zw_mix2(n5685, n5686, 87u64);
    let n5689: ZW = zw_bits_b(n4636);
    let n5690: ZW = zw_mix1(n5624, n5689, 38u64);
    let n5691: ZW = zw_mix2(n5625, n5689, 38u64);
    let n5692: ZW = zw_bits_n(n4643);
    let n5693: ZW = zw_mix1(n5690, n5692, 39u64);
    let n5694: ZW = zw_mix2(n5691, n5692, 39u64);
    let n5695: ZW = zw_bits_n(n4642);
    let n5696: ZW = zw_mix1(n5693, n5695, 87u64);
    let n5697: ZW = zw_mix2(n5694, n5695, 87u64);
    let n5698: ZW = zw_bits_n(n4715);
    let n5699: ZW = zw_mix1(n5138, n5698, 260u64);
    let n5700: ZW = zw_mix2(n5139, n5698, 260u64);
    let n5701: ZW = zw_bits_n(n4716);
    let n5702: ZW = zw_mix1(n5699, n5701, 273u64);
    let n5703: ZW = zw_mix2(n5700, n5701, 273u64);
    let n5704: ZW = zw_bits_n(n4724);
    let n5705: ZW = zw_mix1(n5702, n5704, 301u64);
    let n5706: ZW = zw_mix2(n5703, n5704, 301u64);
    let n5707: ZW = zw_bits_n(n4726);
    let n5708: ZW = zw_mix1(n5705, n5707, 367u64);
    let n5709: ZW = zw_mix2(n5706, n5707, 367u64);
    let n5710: ZW = zw_bits_n(n4727);
    let n5711: ZW = zw_mix1(n5708, n5710, 368u64);
    let n5712: ZW = zw_mix2(n5709, n5710, 368u64);
    let n5713: ZW = zw_bits_n(n4714);
    let n5714: ZW = zw_mix1(n5711, n5713, 20u64);
    let n5715: ZW = zw_mix2(n5712, n5713, 20u64);
    let n5716: ZW = zw_mix1(n5714, n5158, 41u64);
    let n5717: ZW = zw_mix2(n5715, n5158, 41u64);
    let n5718: ZW = zw_bits_n(n4717);
    let n5719: ZW = zw_mix1(n5716, n5718, 281u64);
    let n5720: ZW = zw_mix2(n5717, n5718, 281u64);
    let n5721: ZW = zw_bits_n(n4718);
    let n5722: ZW = zw_mix1(n5719, n5721, 283u64);
    let n5723: ZW = zw_mix2(n5720, n5721, 283u64);
    let n5724: ZW = zw_bits_n(n4719);
    let n5725: ZW = zw_mix1(n5722, n5724, 284u64);
    let n5726: ZW = zw_mix2(n5723, n5724, 284u64);
    let n5727: ZW = zw_bits_n(n4720);
    let n5728: ZW = zw_mix1(n5725, n5727, 286u64);
    let n5729: ZW = zw_mix2(n5726, n5727, 286u64);
    let n5730: ZW = zw_bits_b(n4721);
    let n5731: ZW = zw_mix1(n5728, n5730, 293u64);
    let n5732: ZW = zw_mix2(n5729, n5730, 293u64);
    let n5733: ZW = zw_bits_b(n4722);
    let n5734: ZW = zw_mix1(n5731, n5733, 294u64);
    let n5735: ZW = zw_mix2(n5732, n5733, 294u64);
    let n5736: ZW = zw_bits_n(n4751);
    let n5737: ZW = zw_mix1(n5734, n5736, 300u64);
    let n5738: ZW = zw_mix2(n5735, n5736, 300u64);
    let n5739: ZW = zw_mix1(n5737, n5181, 357u64);
    let n5740: ZW = zw_mix2(n5738, n5181, 357u64);
    let n5741: ZW = zw_mix1(n5739, n5184, 358u64);
    let n5742: ZW = zw_mix2(n5740, n5184, 358u64);
    let n5743: ZW = zw_mix1(n5741, n5187, 359u64);
    let n5744: ZW = zw_mix2(n5742, n5187, 359u64);
    let n5745: ZW = zw_mix1(n5743, n5190, 360u64);
    let n5746: ZW = zw_mix2(n5744, n5190, 360u64);
    let n5747: ZW = zw_bits_b(n4725);
    let n5748: ZW = zw_mix1(n5745, n5747, 361u64);
    let n5749: ZW = zw_mix2(n5746, n5747, 361u64);
    let n5750: ZW = zw_bits_n(n4752);
    let n5751: ZW = zw_mix1(n5748, n5750, 369u64);
    let n5752: ZW = zw_mix2(n5749, n5750, 369u64);
    let n5753: ZW = zw_bits_n(n4729);
    let n5754: ZW = zw_mix1(n5751, n5753, 370u64);
    let n5755: ZW = zw_mix2(n5752, n5753, 370u64);
    let n5756: ZW = zw_bits_b(n4767);
    let n5757: ZW = zw_mix1(n5745, n5756, 361u64);
    let n5758: ZW = zw_mix2(n5746, n5756, 361u64);
    let n5759: ZW = zw_bits_n(n4780);
    let n5760: ZW = zw_mix1(n5757, n5759, 369u64);
    let n5761: ZW = zw_mix2(n5758, n5759, 369u64);
    let n5762: ZW = zw_bits_n(n4769);
    let n5763: ZW = zw_mix1(n5760, n5762, 370u64);
    let n5764: ZW = zw_mix2(n5761, n5762, 370u64);
    let n5765: ZW = zw_bits_b(n4794);
    let n5766: ZW = zw_mix1(n5745, n5765, 361u64);
    let n5767: ZW = zw_mix2(n5746, n5765, 361u64);
    let n5768: ZW = zw_bits_n(n4807);
    let n5769: ZW = zw_mix1(n5766, n5768, 369u64);
    let n5770: ZW = zw_mix2(n5767, n5768, 369u64);
    let n5771: ZW = zw_bits_n(n4796);
    let n5772: ZW = zw_mix1(n5769, n5771, 370u64);
    let n5773: ZW = zw_mix2(n5770, n5771, 370u64);
    let n5774: ZW = zw_bits_n(n4816);
    let n5775: ZW = zw_mix1(n5725, n5774, 286u64);
    let n5776: ZW = zw_mix2(n5726, n5774, 286u64);
    let n5777: ZW = zw_mix1(n5775, n5730, 293u64);
    let n5778: ZW = zw_mix2(n5776, n5730, 293u64);
    let n5779: ZW = zw_bits_b(n4817);
    let n5780: ZW = zw_mix1(n5777, n5779, 294u64);
    let n5781: ZW = zw_mix2(n5778, n5779, 294u64);
    let n5782: ZW = zw_mix1(n5780, n5736, 300u64);
    let n5783: ZW = zw_mix2(n5781, n5736, 300u64);
    let n5784: ZW = zw_mix1(n5782, n5181, 357u64);
    let n5785: ZW = zw_mix2(n5783, n5181, 357u64);
    let n5786: ZW = zw_mix1(n5784, n5184, 358u64);
    let n5787: ZW = zw_mix2(n5785, n5184, 358u64);
    let n5788: ZW = zw_mix1(n5786, n5187, 359u64);
    let n5789: ZW = zw_mix2(n5787, n5187, 359u64);
    let n5790: ZW = zw_mix1(n5788, n5190, 360u64);
    let n5791: ZW = zw_mix2(n5789, n5190, 360u64);
    let n5792: ZW = zw_mix1(n5790, n5747, 361u64);
    let n5793: ZW = zw_mix2(n5791, n5747, 361u64);
    let n5794: ZW = zw_bits_n(n4830);
    let n5795: ZW = zw_mix1(n5792, n5794, 369u64);
    let n5796: ZW = zw_mix2(n5793, n5794, 369u64);
    let n5797: ZW = zw_bits_n(n4819);
    let n5798: ZW = zw_mix1(n5795, n5797, 370u64);
    let n5799: ZW = zw_mix2(n5796, n5797, 370u64);
    let n5800: ZW = zw_mix1(n5790, n5756, 361u64);
    let n5801: ZW = zw_mix2(n5791, n5756, 361u64);
    let n5802: ZW = zw_bits_n(n4849);
    let n5803: ZW = zw_mix1(n5800, n5802, 369u64);
    let n5804: ZW = zw_mix2(n5801, n5802, 369u64);
    let n5805: ZW = zw_bits_n(n4838);
    let n5806: ZW = zw_mix1(n5803, n5805, 370u64);
    let n5807: ZW = zw_mix2(n5804, n5805, 370u64);
    let n5808: ZW = zw_mix1(n5790, n5765, 361u64);
    let n5809: ZW = zw_mix2(n5791, n5765, 361u64);
    let n5810: ZW = zw_bits_n(n4868);
    let n5811: ZW = zw_mix1(n5808, n5810, 369u64);
    let n5812: ZW = zw_mix2(n5809, n5810, 369u64);
    let n5813: ZW = zw_bits_n(n4857);
    let n5814: ZW = zw_mix1(n5811, n5813, 370u64);
    let n5815: ZW = zw_mix2(n5812, n5813, 370u64);
    let n5816: ZW = zw_bits_n(n4889);
    let n5817: ZW = zw_mix1(n5711, n5816, 20u64);
    let n5818: ZW = zw_mix2(n5712, n5816, 20u64);
    let n5819: ZW = zw_bits_b(n4890);
    let n5820: ZW = zw_mix1(n5817, n5819, 41u64);
    let n5821: ZW = zw_mix2(n5818, n5819, 41u64);
    let n5822: ZW = zw_bits_n(n4891);
    let n5823: ZW = zw_mix1(n5820, n5822, 281u64);
    let n5824: ZW = zw_mix2(n5821, n5822, 281u64);
    let n5825: ZW = zw_bits_n(n4892);
    let n5826: ZW = zw_mix1(n5823, n5825, 283u64);
    let n5827: ZW = zw_mix2(n5824, n5825, 283u64);
    let n5828: ZW = zw_bits_n(n4893);
    let n5829: ZW = zw_mix1(n5826, n5828, 284u64);
    let n5830: ZW = zw_mix2(n5827, n5828, 284u64);
    let n5831: ZW = zw_mix1(n5829, n5727, 286u64);
    let n5832: ZW = zw_mix2(n5830, n5727, 286u64);
    let n5833: ZW = zw_bits_b(n4894);
    let n5834: ZW = zw_mix1(n5831, n5833, 293u64);
    let n5835: ZW = zw_mix2(n5832, n5833, 293u64);
    let n5836: ZW = zw_mix1(n5834, n5733, 294u64);
    let n5837: ZW = zw_mix2(n5835, n5733, 294u64);
    let n5838: ZW = zw_bits_n(n4913);
    let n5839: ZW = zw_mix1(n5836, n5838, 300u64);
    let n5840: ZW = zw_mix2(n5837, n5838, 300u64);
    let n5841: ZW = zw_bits_n(n4895);
    let n5842: ZW = zw_mix1(n5839, n5841, 357u64);
    let n5843: ZW = zw_mix2(n5840, n5841, 357u64);
    let n5844: ZW = zw_bits_n(n4896);
    let n5845: ZW = zw_mix1(n5842, n5844, 358u64);
    let n5846: ZW = zw_mix2(n5843, n5844, 358u64);
    let n5847: ZW = zw_bits_n(n4897);
    let n5848: ZW = zw_mix1(n5845, n5847, 359u64);
    let n5849: ZW = zw_mix2(n5846, n5847, 359u64);
    let n5850: ZW = zw_bits_n(n4898);
    let n5851: ZW = zw_mix1(n5848, n5850, 360u64);
    let n5852: ZW = zw_mix2(n5849, n5850, 360u64);
    let n5853: ZW = zw_mix1(n5851, n5747, 361u64);
    let n5854: ZW = zw_mix2(n5852, n5747, 361u64);
    let n5855: ZW = zw_bits_n(n4914);
    let n5856: ZW = zw_mix1(n5853, n5855, 369u64);
    let n5857: ZW = zw_mix2(n5854, n5855, 369u64);
    let n5858: ZW = zw_bits_n(n4900);
    let n5859: ZW = zw_mix1(n5856, n5858, 370u64);
    let n5860: ZW = zw_mix2(n5857, n5858, 370u64);
    let n5861: ZW = zw_bits_n(n4925);
    let n5862: ZW = zw_mix1(n5842, n5861, 358u64);
    let n5863: ZW = zw_mix2(n5843, n5861, 358u64);
    let n5864: ZW = zw_bits_n(n4926);
    let n5865: ZW = zw_mix1(n5862, n5864, 359u64);
    let n5866: ZW = zw_mix2(n5863, n5864, 359u64);
    let n5867: ZW = zw_mix1(n5865, n5850, 360u64);
    let n5868: ZW = zw_mix2(n5866, n5850, 360u64);
    let n5869: ZW = zw_mix1(n5867, n5756, 361u64);
    let n5870: ZW = zw_mix2(n5868, n5756, 361u64);
    let n5871: ZW = zw_bits_n(n4939);
    let n5872: ZW = zw_mix1(n5869, n5871, 369u64);
    let n5873: ZW = zw_mix2(n5870, n5871, 369u64);
    let n5874: ZW = zw_bits_n(n4928);
    let n5875: ZW = zw_mix1(n5872, n5874, 370u64);
    let n5876: ZW = zw_mix2(n5873, n5874, 370u64);
    let n5877: ZW = zw_bits_n(n4948);
    let n5878: ZW = zw_mix1(n5862, n5877, 359u64);
    let n5879: ZW = zw_mix2(n5863, n5877, 359u64);
    let n5880: ZW = zw_mix1(n5878, n5850, 360u64);
    let n5881: ZW = zw_mix2(n5879, n5850, 360u64);
    let n5882: ZW = zw_mix1(n5880, n5765, 361u64);
    let n5883: ZW = zw_mix2(n5881, n5765, 361u64);
    let n5884: ZW = zw_bits_n(n4961);
    let n5885: ZW = zw_mix1(n5882, n5884, 369u64);
    let n5886: ZW = zw_mix2(n5883, n5884, 369u64);
    let n5887: ZW = zw_bits_n(n4950);
    let n5888: ZW = zw_mix1(n5885, n5887, 370u64);
    let n5889: ZW = zw_mix2(n5886, n5887, 370u64);
    let n5890: ZW = zw_bits_n(n4976);
    let n5891: ZW = zw_mix1(n5839, n5890, 357u64);
    let n5892: ZW = zw_mix2(n5840, n5890, 357u64);
    let n5893: ZW = zw_bits_n(n4977);
    let n5894: ZW = zw_mix1(n5891, n5893, 358u64);
    let n5895: ZW = zw_mix2(n5892, n5893, 358u64);
    let n5896: ZW = zw_bits_n(n4978);
    let n5897: ZW = zw_mix1(n5894, n5896, 359u64);
    let n5898: ZW = zw_mix2(n5895, n5896, 359u64);
    let n5899: ZW = zw_bits_n(n4979);
    let n5900: ZW = zw_mix1(n5897, n5899, 360u64);
    let n5901: ZW = zw_mix2(n5898, n5899, 360u64);
    let n5902: ZW = zw_mix1(n5900, n5747, 361u64);
    let n5903: ZW = zw_mix2(n5901, n5747, 361u64);
    let n5904: ZW = zw_bits_n(n4992);
    let n5905: ZW = zw_mix1(n5902, n5904, 369u64);
    let n5906: ZW = zw_mix2(n5903, n5904, 369u64);
    let n5907: ZW = zw_bits_n(n4981);
    let n5908: ZW = zw_mix1(n5905, n5907, 370u64);
    let n5909: ZW = zw_mix2(n5906, n5907, 370u64);
    let n5910: ZW = zw_mix1(n5891, n5861, 358u64);
    let n5911: ZW = zw_mix2(n5892, n5861, 358u64);
    let n5912: ZW = zw_mix1(n5910, n5864, 359u64);
    let n5913: ZW = zw_mix2(n5911, n5864, 359u64);
    let n5914: ZW = zw_mix1(n5912, n5899, 360u64);
    let n5915: ZW = zw_mix2(n5913, n5899, 360u64);
    let n5916: ZW = zw_mix1(n5914, n5756, 361u64);
    let n5917: ZW = zw_mix2(n5915, n5756, 361u64);
    let n5918: ZW = zw_bits_n(n5001);
    let n5919: ZW = zw_mix1(n5916, n5918, 369u64);
    let n5920: ZW = zw_mix2(n5917, n5918, 369u64);
    let n5921: ZW = zw_bits_n(n4999);
    let n5922: ZW = zw_mix1(n5919, n5921, 370u64);
    let n5923: ZW = zw_mix2(n5920, n5921, 370u64);
    let n5924: ZW = zw_mix1(n5910, n5877, 359u64);
    let n5925: ZW = zw_mix2(n5911, n5877, 359u64);
    let n5926: ZW = zw_mix1(n5924, n5899, 360u64);
    let n5927: ZW = zw_mix2(n5925, n5899, 360u64);
    let n5928: ZW = zw_mix1(n5926, n5765, 361u64);
    let n5929: ZW = zw_mix2(n5927, n5765, 361u64);
    let n5930: ZW = zw_bits_n(n5009);
    let n5931: ZW = zw_mix1(n5928, n5930, 369u64);
    let n5932: ZW = zw_mix2(n5929, n5930, 369u64);
    let n5933: ZW = zw_bits_n(n5007);
    let n5934: ZW = zw_mix1(n5931, n5933, 370u64);
    let n5935: ZW = zw_mix2(n5932, n5933, 370u64);
    let n5936: ZW = zw_bits_n(n5014);
    let n5937: ZW = zw_mix1(n5897, n5936, 360u64);
    let n5938: ZW = zw_mix2(n5898, n5936, 360u64);
    let n5939: ZW = zw_mix1(n5937, n5747, 361u64);
    let n5940: ZW = zw_mix2(n5938, n5747, 361u64);
    let n5941: ZW = zw_mix1(n5939, n5904, 369u64);
    let n5942: ZW = zw_mix2(n5940, n5904, 369u64);
    let n5943: ZW = zw_bits_n(n5015);
    let n5944: ZW = zw_mix1(n5941, n5943, 370u64);
    let n5945: ZW = zw_mix2(n5942, n5943, 370u64);
    let n5946: ZW = zw_mix1(n5912, n5936, 360u64);
    let n5947: ZW = zw_mix2(n5913, n5936, 360u64);
    let n5948: ZW = zw_mix1(n5946, n5756, 361u64);
    let n5949: ZW = zw_mix2(n5947, n5756, 361u64);
    let n5950: ZW = zw_mix1(n5948, n5918, 369u64);
    let n5951: ZW = zw_mix2(n5949, n5918, 369u64);
    let n5952: ZW = zw_bits_n(n5018);
    let n5953: ZW = zw_mix1(n5950, n5952, 370u64);
    let n5954: ZW = zw_mix2(n5951, n5952, 370u64);
    let n5955: ZW = zw_mix1(n5924, n5936, 360u64);
    let n5956: ZW = zw_mix2(n5925, n5936, 360u64);
    let n5957: ZW = zw_mix1(n5955, n5765, 361u64);
    let n5958: ZW = zw_mix2(n5956, n5765, 361u64);
    let n5959: ZW = zw_mix1(n5957, n5930, 369u64);
    let n5960: ZW = zw_mix2(n5958, n5930, 369u64);
    let n5961: ZW = zw_bits_n(n5021);
    let n5962: ZW = zw_mix1(n5959, n5961, 370u64);
    let n5963: ZW = zw_mix2(n5960, n5961, 370u64);
    let n5964: ZW = zw_mix1(n5829, n5774, 286u64);
    let n5965: ZW = zw_mix2(n5830, n5774, 286u64);
    let n5966: ZW = zw_mix1(n5964, n5833, 293u64);
    let n5967: ZW = zw_mix2(n5965, n5833, 293u64);
    let n5968: ZW = zw_mix1(n5966, n5779, 294u64);
    let n5969: ZW = zw_mix2(n5967, n5779, 294u64);
    let n5970: ZW = zw_mix1(n5968, n5838, 300u64);
    let n5971: ZW = zw_mix2(n5969, n5838, 300u64);
    let n5972: ZW = zw_mix1(n5970, n5841, 357u64);
    let n5973: ZW = zw_mix2(n5971, n5841, 357u64);
    let n5974: ZW = zw_mix1(n5972, n5844, 358u64);
    let n5975: ZW = zw_mix2(n5973, n5844, 358u64);
    let n5976: ZW = zw_mix1(n5974, n5847, 359u64);
    let n5977: ZW = zw_mix2(n5975, n5847, 359u64);
    let n5978: ZW = zw_mix1(n5976, n5850, 360u64);
    let n5979: ZW = zw_mix2(n5977, n5850, 360u64);
    let n5980: ZW = zw_mix1(n5978, n5747, 361u64);
    let n5981: ZW = zw_mix2(n5979, n5747, 361u64);
    let n5982: ZW = zw_bits_n(n5039);
    let n5983: ZW = zw_mix1(n5980, n5982, 369u64);
    let n5984: ZW = zw_mix2(n5981, n5982, 369u64);
    let n5985: ZW = zw_bits_n(n5028);
    let n5986: ZW = zw_mix1(n5983, n5985, 370u64);
    let n5987: ZW = zw_mix2(n5984, n5985, 370u64);
    let n5988: ZW = zw_mix1(n5972, n5861, 358u64);
    let n5989: ZW = zw_mix2(n5973, n5861, 358u64);
    let n5990: ZW = zw_mix1(n5988, n5864, 359u64);
    let n5991: ZW = zw_mix2(n5989, n5864, 359u64);
    let n5992: ZW = zw_mix1(n5990, n5850, 360u64);
    let n5993: ZW = zw_mix2(n5991, n5850, 360u64);
    let n5994: ZW = zw_mix1(n5992, n5756, 361u64);
    let n5995: ZW = zw_mix2(n5993, n5756, 361u64);
    let n5996: ZW = zw_bits_n(n5058);
    let n5997: ZW = zw_mix1(n5994, n5996, 369u64);
    let n5998: ZW = zw_mix2(n5995, n5996, 369u64);
    let n5999: ZW = zw_bits_n(n5047);
    let n6000: ZW = zw_mix1(n5997, n5999, 370u64);
    let n6001: ZW = zw_mix2(n5998, n5999, 370u64);
    let n6002: ZW = zw_mix1(n5988, n5877, 359u64);
    let n6003: ZW = zw_mix2(n5989, n5877, 359u64);
    let n6004: ZW = zw_mix1(n6002, n5850, 360u64);
    let n6005: ZW = zw_mix2(n6003, n5850, 360u64);
    let n6006: ZW = zw_mix1(n6004, n5765, 361u64);
    let n6007: ZW = zw_mix2(n6005, n5765, 361u64);
    let n6008: ZW = zw_bits_n(n5077);
    let n6009: ZW = zw_mix1(n6006, n6008, 369u64);
    let n6010: ZW = zw_mix2(n6007, n6008, 369u64);
    let n6011: ZW = zw_bits_n(n5066);
    let n6012: ZW = zw_mix1(n6009, n6011, 370u64);
    let n6013: ZW = zw_mix2(n6010, n6011, 370u64);
    let n6014: ZW = zw_mix1(n5970, n5890, 357u64);
    let n6015: ZW = zw_mix2(n5971, n5890, 357u64);
    let n6016: ZW = zw_mix1(n6014, n5893, 358u64);
    let n6017: ZW = zw_mix2(n6015, n5893, 358u64);
    let n6018: ZW = zw_mix1(n6016, n5896, 359u64);
    let n6019: ZW = zw_mix2(n6017, n5896, 359u64);
    let n6020: ZW = zw_mix1(n6018, n5899, 360u64);
    let n6021: ZW = zw_mix2(n6019, n5899, 360u64);
    let n6022: ZW = zw_mix1(n6020, n5747, 361u64);
    let n6023: ZW = zw_mix2(n6021, n5747, 361u64);
    let n6024: ZW = zw_bits_n(n5096);
    let n6025: ZW = zw_mix1(n6022, n6024, 369u64);
    let n6026: ZW = zw_mix2(n6023, n6024, 369u64);
    let n6027: ZW = zw_bits_n(n5085);
    let n6028: ZW = zw_mix1(n6025, n6027, 370u64);
    let n6029: ZW = zw_mix2(n6026, n6027, 370u64);
    let n6030: ZW = zw_mix1(n6014, n5861, 358u64);
    let n6031: ZW = zw_mix2(n6015, n5861, 358u64);
    let n6032: ZW = zw_mix1(n6030, n5864, 359u64);
    let n6033: ZW = zw_mix2(n6031, n5864, 359u64);
    let n6034: ZW = zw_mix1(n6032, n5899, 360u64);
    let n6035: ZW = zw_mix2(n6033, n5899, 360u64);
    let n6036: ZW = zw_mix1(n6034, n5756, 361u64);
    let n6037: ZW = zw_mix2(n6035, n5756, 361u64);
    let n6038: ZW = zw_bits_n(n5105);
    let n6039: ZW = zw_mix1(n6036, n6038, 369u64);
    let n6040: ZW = zw_mix2(n6037, n6038, 369u64);
    let n6041: ZW = zw_bits_n(n5103);
    let n6042: ZW = zw_mix1(n6039, n6041, 370u64);
    let n6043: ZW = zw_mix2(n6040, n6041, 370u64);
    let n6044: ZW = zw_mix1(n6030, n5877, 359u64);
    let n6045: ZW = zw_mix2(n6031, n5877, 359u64);
    let n6046: ZW = zw_mix1(n6044, n5899, 360u64);
    let n6047: ZW = zw_mix2(n6045, n5899, 360u64);
    let n6048: ZW = zw_mix1(n6046, n5765, 361u64);
    let n6049: ZW = zw_mix2(n6047, n5765, 361u64);
    let n6050: ZW = zw_bits_n(n5113);
    let n6051: ZW = zw_mix1(n6048, n6050, 369u64);
    let n6052: ZW = zw_mix2(n6049, n6050, 369u64);
    let n6053: ZW = zw_bits_n(n5111);
    let n6054: ZW = zw_mix1(n6051, n6053, 370u64);
    let n6055: ZW = zw_mix2(n6052, n6053, 370u64);
    let n6056: ZW = zw_mix1(n6018, n5936, 360u64);
    let n6057: ZW = zw_mix2(n6019, n5936, 360u64);
    let n6058: ZW = zw_mix1(n6056, n5747, 361u64);
    let n6059: ZW = zw_mix2(n6057, n5747, 361u64);
    let n6060: ZW = zw_mix1(n6058, n6024, 369u64);
    let n6061: ZW = zw_mix2(n6059, n6024, 369u64);
    let n6062: ZW = zw_bits_n(n5116);
    let n6063: ZW = zw_mix1(n6060, n6062, 370u64);
    let n6064: ZW = zw_mix2(n6061, n6062, 370u64);
    let n6065: ZW = zw_mix1(n6032, n5936, 360u64);
    let n6066: ZW = zw_mix2(n6033, n5936, 360u64);
    let n6067: ZW = zw_mix1(n6065, n5756, 361u64);
    let n6068: ZW = zw_mix2(n6066, n5756, 361u64);
    let n6069: ZW = zw_mix1(n6067, n6038, 369u64);
    let n6070: ZW = zw_mix2(n6068, n6038, 369u64);
    let n6071: ZW = zw_bits_n(n5119);
    let n6072: ZW = zw_mix1(n6069, n6071, 370u64);
    let n6073: ZW = zw_mix2(n6070, n6071, 370u64);
    let n6074: ZW = zw_mix1(n6044, n5936, 360u64);
    let n6075: ZW = zw_mix2(n6045, n5936, 360u64);
    let n6076: ZW = zw_mix1(n6074, n5765, 361u64);
    let n6077: ZW = zw_mix2(n6075, n5765, 361u64);
    let n6078: ZW = zw_mix1(n6076, n6050, 369u64);
    let n6079: ZW = zw_mix2(n6077, n6050, 369u64);
    let n6080: ZW = zw_bits_n(n5122);
    let n6081: ZW = zw_mix1(n6078, n6080, 370u64);
    let n6082: ZW = zw_mix2(n6079, n6080, 370u64);
    let ok_v0_b0: u16 = ALL & zb_holds(n1413) & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v0_b0: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v0_b0: u16 = ALL & zb_holds(n1583);
    let ok_v1_b1: u16 = ALL & zb_holds(n1413) & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v1_b1: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v1_b1: u16 = ALL & zb_holds(n1652);
    let ok_v2_b2: u16 = ALL & zb_holds(n1413) & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v2_b2: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v2_b2: u16 = ALL & zb_holds(n1708);
    let ok_v16_b3: u16 = ALL & zb_holds(n1413) & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v16_b3: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v16_b3: u16 = ALL & zb_holds(n1751);
    let ok_v17_b4: u16 = ALL & zb_holds(n1413) & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v17_b4: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v17_b4: u16 = ALL & zb_holds(n1792);
    let ok_v18_b5: u16 = ALL & zb_holds(n1413) & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v18_b5: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v18_b5: u16 = ALL & zb_holds(n1833);
    let ok_v32_b6: u16 = ALL & zb_holds(n1413) & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v32_b6: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v32_b6: u16 = ALL & zb_holds(n1886);
    let ok_v33_b7: u16 = ALL & zb_holds(n1413) & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v33_b7: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v33_b7: u16 = ALL & zb_holds(n1909);
    let ok_v34_b8: u16 = ALL & zb_holds(n1413) & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v34_b8: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v34_b8: u16 = ALL & zb_holds(n1930);
    let ok_v36_b9: u16 = ALL & zb_holds(n1413) & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v36_b9: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v36_b9: u16 = ALL & zb_holds(n1956);
    let ok_v37_b10: u16 = ALL & zb_holds(n1413) & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v37_b10: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v37_b10: u16 = ALL & zb_holds(n1909);
    let ok_v38_b11: u16 = ALL & zb_holds(n1413) & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v38_b11: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v38_b11: u16 = ALL & zb_holds(n1930);
    let ok_v40_b12: u16 = ALL & zb_holds(n1413) & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v40_b12: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v40_b12: u16 = ALL & zb_holds(n1956);
    let ok_v41_b13: u16 = ALL & zb_holds(n1413) & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v41_b13: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v41_b13: u16 = ALL & zb_holds(n1909);
    let ok_v42_b14: u16 = ALL & zb_holds(n1413) & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v42_b14: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v42_b14: u16 = ALL & zb_holds(n1930);
    let ok_v48_b15: u16 = ALL & zb_holds(n1413) & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v48_b15: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v48_b15: u16 = ALL & zb_holds(n2007);
    let ok_v49_b16: u16 = ALL & zb_holds(n1413) & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v49_b16: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v49_b16: u16 = ALL & zb_holds(n2026);
    let ok_v50_b17: u16 = ALL & zb_holds(n1413) & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v50_b17: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v50_b17: u16 = ALL & zb_holds(n2045);
    let ok_v52_b18: u16 = ALL & zb_holds(n1413) & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v52_b18: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v52_b18: u16 = ALL & zb_holds(n2062);
    let ok_v53_b19: u16 = ALL & zb_holds(n1413) & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v53_b19: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v53_b19: u16 = ALL & zb_holds(n2026);
    let ok_v54_b20: u16 = ALL & zb_holds(n1413) & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v54_b20: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v54_b20: u16 = ALL & zb_holds(n2045);
    let ok_v56_b21: u16 = ALL & zb_holds(n1413) & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v56_b21: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v56_b21: u16 = ALL & zb_holds(n2062);
    let ok_v57_b22: u16 = ALL & zb_holds(n1413) & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v57_b22: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v57_b22: u16 = ALL & zb_holds(n2026);
    let ok_v58_b23: u16 = ALL & zb_holds(n1413) & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v58_b23: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v58_b23: u16 = ALL & zb_holds(n2045);
    let ok_v0_b24: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n2156);
    let bd_v0_b24: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v0_b24: u16 = ALL & zb_holds(n134) & zb_holds(n2208);
    let ok_v1_b25: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n2156);
    let bd_v1_b25: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v1_b25: u16 = ALL & zb_holds(n134) & zb_holds(n2236);
    let ok_v2_b26: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n2156);
    let bd_v2_b26: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v2_b26: u16 = ALL & zb_holds(n134) & zb_holds(n2263);
    let ok_v16_b27: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n2156);
    let bd_v16_b27: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v16_b27: u16 = ALL & zb_holds(n134) & zb_holds(n2292);
    let ok_v17_b28: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n2156);
    let bd_v17_b28: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v17_b28: u16 = ALL & zb_holds(n134) & zb_holds(n2321);
    let ok_v18_b29: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n2156);
    let bd_v18_b29: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v18_b29: u16 = ALL & zb_holds(n134) & zb_holds(n2350);
    let ok_v32_b30: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n2156);
    let bd_v32_b30: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v32_b30: u16 = ALL & zb_holds(n2370);
    let ok_v33_b31: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n2156);
    let bd_v33_b31: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v33_b31: u16 = ALL & zb_holds(n2377);
    let ok_v34_b32: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n2156);
    let bd_v34_b32: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v34_b32: u16 = ALL & zb_holds(n2384);
    let ok_v36_b33: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n2156);
    let bd_v36_b33: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v36_b33: u16 = ALL & zb_holds(n2389);
    let ok_v48_b34: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n2156);
    let bd_v48_b34: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v48_b34: u16 = ALL & zb_holds(n2408);
    let ok_v49_b35: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n2156);
    let bd_v49_b35: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v49_b35: u16 = ALL & zb_holds(n2415);
    let ok_v50_b36: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n2156);
    let bd_v50_b36: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v50_b36: u16 = ALL & zb_holds(n2422);
    let ok_v52_b37: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n2156);
    let bd_v52_b37: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v52_b37: u16 = ALL & zb_holds(n2427);
    let ok_v0_b38: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n3715);
    let bd_v0_b38: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v0_b38: u16 = ALL & zb_holds(n134) & zb_holds(n3852) & zb_holds(n3855);
    let ok_v1_b39: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n3715);
    let bd_v1_b39: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v1_b39: u16 = ALL & zb_holds(n134) & zb_holds(n3852) & zb_holds(n3909);
    let ok_v2_b40: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n3715);
    let bd_v2_b40: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v2_b40: u16 = ALL & zb_holds(n134) & zb_holds(n3852) & zb_holds(n3960);
    let ok_v16_b41: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n3715);
    let bd_v16_b41: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v16_b41: u16 = ALL & zb_holds(n134) & zb_holds(n3852) & zb_holds(n3996);
    let ok_v17_b42: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n3715);
    let bd_v17_b42: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v17_b42: u16 = ALL & zb_holds(n134) & zb_holds(n3852) & zb_holds(n4032);
    let ok_v18_b43: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n3715);
    let bd_v18_b43: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v18_b43: u16 = ALL & zb_holds(n134) & zb_holds(n3852) & zb_holds(n4068);
    let ok_v32_b44: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n3715);
    let bd_v32_b44: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v32_b44: u16 = ALL & zb_holds(n4101);
    let ok_v33_b45: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n3715);
    let bd_v33_b45: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v33_b45: u16 = ALL & zb_holds(n4112);
    let ok_v34_b46: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n3715);
    let bd_v34_b46: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v34_b46: u16 = ALL & zb_holds(n4123);
    let ok_v36_b47: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n3715);
    let bd_v36_b47: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v36_b47: u16 = ALL & zb_holds(n4132);
    let ok_v48_b48: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n3715);
    let bd_v48_b48: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v48_b48: u16 = ALL & zb_holds(n4155);
    let ok_v49_b49: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n3715);
    let bd_v49_b49: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v49_b49: u16 = ALL & zb_holds(n4166);
    let ok_v50_b50: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n3715);
    let bd_v50_b50: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v50_b50: u16 = ALL & zb_holds(n4177);
    let ok_v52_b51: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n3715);
    let bd_v52_b51: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v52_b51: u16 = ALL & zb_holds(n4186);
    let ok_v0_b52: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4259);
    let bd_v0_b52: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v0_b52: u16 = ALL & zb_holds(n134) & zb_holds(n4258);
    let ok_v1_b53: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4303);
    let bd_v1_b53: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v1_b53: u16 = ALL & zb_holds(n134) & zb_holds(n4302);
    let ok_v2_b54: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4347);
    let bd_v2_b54: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v2_b54: u16 = ALL & zb_holds(n134) & zb_holds(n4346);
    let ok_v16_b55: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4390);
    let bd_v16_b55: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v16_b55: u16 = ALL & zb_holds(n134) & zb_holds(n4389);
    let ok_v17_b56: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4433);
    let bd_v17_b56: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v17_b56: u16 = ALL & zb_holds(n134) & zb_holds(n4432);
    let ok_v18_b57: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4476);
    let bd_v18_b57: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v18_b57: u16 = ALL & zb_holds(n134) & zb_holds(n4475);
    let ok_v32_b58: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4504);
    let bd_v32_b58: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v32_b58: u16 = ALL & zb_holds(n4507);
    let ok_v33_b59: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4522);
    let bd_v33_b59: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v33_b59: u16 = ALL & zb_holds(n4525);
    let ok_v34_b60: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4540);
    let bd_v34_b60: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v34_b60: u16 = ALL & zb_holds(n4543);
    let ok_v36_b61: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4556);
    let bd_v36_b61: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v36_b61: u16 = ALL & zb_holds(n4559);
    let ok_v48_b62: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4586);
    let bd_v48_b62: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v48_b62: u16 = ALL & zb_holds(n4589);
    let ok_v49_b63: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4604);
    let bd_v49_b63: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v49_b63: u16 = ALL & zb_holds(n4607);
    let ok_v50_b64: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4622);
    let bd_v50_b64: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v50_b64: u16 = ALL & zb_holds(n4625);
    let ok_v52_b65: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4638);
    let bd_v52_b65: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v52_b65: u16 = ALL & zb_holds(n4641);
    let ok_v0_b66: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4731);
    let bd_v0_b66: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v0_b66: u16 = ALL & zb_holds(n4753);
    let ok_v1_b67: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4731);
    let bd_v1_b67: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v1_b67: u16 = ALL & zb_holds(n4781);
    let ok_v2_b68: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4731);
    let bd_v2_b68: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v2_b68: u16 = ALL & zb_holds(n4808);
    let ok_v16_b69: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4731);
    let bd_v16_b69: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v16_b69: u16 = ALL & zb_holds(n4831);
    let ok_v17_b70: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4731);
    let bd_v17_b70: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v17_b70: u16 = ALL & zb_holds(n4850);
    let ok_v18_b71: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4731);
    let bd_v18_b71: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v18_b71: u16 = ALL & zb_holds(n4869);
    let ok_v32_b72: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4731);
    let bd_v32_b72: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v32_b72: u16 = ALL & zb_holds(n4915);
    let ok_v33_b73: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4731);
    let bd_v33_b73: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v33_b73: u16 = ALL & zb_holds(n4940);
    let ok_v34_b74: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4731);
    let bd_v34_b74: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v34_b74: u16 = ALL & zb_holds(n4962);
    let ok_v36_b75: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4731);
    let bd_v36_b75: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v36_b75: u16 = ALL & zb_holds(n4993);
    let ok_v37_b76: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4731);
    let bd_v37_b76: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v37_b76: u16 = ALL & zb_holds(n4940);
    let ok_v38_b77: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4731);
    let bd_v38_b77: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v38_b77: u16 = ALL & zb_holds(n4962);
    let ok_v40_b78: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4731);
    let bd_v40_b78: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v40_b78: u16 = ALL & zb_holds(n4993);
    let ok_v41_b79: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4731);
    let bd_v41_b79: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v41_b79: u16 = ALL & zb_holds(n4940);
    let ok_v42_b80: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4731);
    let bd_v42_b80: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v42_b80: u16 = ALL & zb_holds(n4962);
    let ok_v48_b81: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4731);
    let bd_v48_b81: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v48_b81: u16 = ALL & zb_holds(n5040);
    let ok_v49_b82: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4731);
    let bd_v49_b82: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v49_b82: u16 = ALL & zb_holds(n5059);
    let ok_v50_b83: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4731);
    let bd_v50_b83: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v50_b83: u16 = ALL & zb_holds(n5078);
    let ok_v52_b84: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4731);
    let bd_v52_b84: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v52_b84: u16 = ALL & zb_holds(n5097);
    let ok_v53_b85: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4731);
    let bd_v53_b85: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v53_b85: u16 = ALL & zb_holds(n5059);
    let ok_v54_b86: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4731);
    let bd_v54_b86: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v54_b86: u16 = ALL & zb_holds(n5078);
    let ok_v56_b87: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4731);
    let bd_v56_b87: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v56_b87: u16 = ALL & zb_holds(n5097);
    let ok_v57_b88: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4731);
    let bd_v57_b88: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v57_b88: u16 = ALL & zb_holds(n5059);
    let ok_v58_b89: u16 = ALL & zb_holds(n206) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n207) & zb_holds(r_c279) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n155) & zb_holds(n106) & zb_holds(r_c259) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(n150) & zb_holds(r_c251) & zb_holds(n105) & zb_holds(n149) & zb_holds(n102) & zb_holds(n146) & zb_holds(n145) & zb_holds(n101) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4731);
    let bd_v58_b89: bool = !n211 || !n210 || !n209 || !n208 || !n131 || !n108 || !n156 || !n142 || !n104 || !n103 || !n148 || !n147;
    let live_v58_b89: u16 = ALL & zb_holds(n5078);
    let sh0 = KShared0 {
        c87: r_c87,
        c39: r_c39,
        c84: n115,
        c86: n216,
        c261: n258,
        c274: n259,
        c368: n487,
        c369: n765,
        c302: n764,
        c85: n217,
    };
    let sh1 = KShared1 {
        c87: n2081,
        c84: n115,
        c86: n216,
        c260: n258,
        c273: n259,
        c85: n217,
    };
    let sh2 = KShared2 {
        c87: n3859,
        c84: n115,
        c86: n216,
        c259: n2475,
        c272: n2476,
        c85: n217,
    };
    let sh3 = KShared3 {
        c84: n115,
        c86: n216,
        c85: n217,
    };
    let sh4 = KShared4 {
        c87: r_c87,
        c39: r_c39,
        c84: n115,
        c86: n216,
        c260: n4715,
        c273: n4716,
        c367: n4726,
        c368: n4727,
        c301: n4724,
        c85: n217,
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
    // 90 distinct button assignments; per outcome they fall
    // into [24, 2, 2, 14, 24] groups that write identical values.
    declined |= live_v0_b0 & (if bd_v0_b0 { ALL } else { !ok_v0_b0 });
    take_0_0 |= live_v0_b0 & ok_v0_b0 & (if bd_v0_b0 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c358: r_c357,
        c359: r_c358,
        c282: n1434,
        c360: r_c359,
        c361: r_c360,
        c284: n1562,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n1563,
        c287: n1432,
        c294: zb_splat(false),
        c295: zb_splat(false),
        c370: n1585,
        c371: n1565,
        c301: n1584,
        h1: n5200, h2: n5201,
    };
    // body 0: buttons 0x00, forks 0x0
    sink.o0(0, take_0_0, &sh0, &o0);
    declined |= live_v1_b1 & (if bd_v1_b1 { ALL } else { !ok_v1_b1 });
    take_0_1 |= live_v1_b1 & ok_v1_b1 & (if bd_v1_b1 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c358: r_c357,
        c359: r_c358,
        c282: n1434,
        c360: r_c359,
        c361: r_c360,
        c284: n1562,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n1641,
        c287: n1432,
        c294: zb_splat(false),
        c295: zb_splat(false),
        c370: n1653,
        c371: n1643,
        c301: n1584,
        h1: n5209, h2: n5210,
    };
    // body 1: buttons 0x01, forks 0x0
    sink.o0(1, take_0_1, &sh0, &o0);
    declined |= live_v2_b2 & (if bd_v2_b2 { ALL } else { !ok_v2_b2 });
    take_0_2 |= live_v2_b2 & ok_v2_b2 & (if bd_v2_b2 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c358: r_c357,
        c359: r_c358,
        c282: n1434,
        c360: r_c359,
        c361: r_c360,
        c284: n1562,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n1697,
        c287: n1432,
        c294: zb_splat(false),
        c295: zb_splat(false),
        c370: n1709,
        c371: n1699,
        c301: n1584,
        h1: n5218, h2: n5219,
    };
    // body 2: buttons 0x02, forks 0x0
    sink.o0(2, take_0_2, &sh0, &o0);
    declined |= live_v16_b3 & (if bd_v16_b3 { ALL } else { !ok_v16_b3 });
    take_0_3 |= live_v16_b3 & ok_v16_b3 & (if bd_v16_b3 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c358: r_c357,
        c359: r_c358,
        c282: n1434,
        c360: r_c359,
        c361: r_c360,
        c284: n1562,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n1563,
        c287: n1740,
        c294: zb_splat(false),
        c295: zb_splat(true),
        c370: n1752,
        c371: n1742,
        c301: n1584,
        h1: n5244, h2: n5245,
    };
    // body 3: buttons 0x10, forks 0x0
    sink.o0(16, take_0_3, &sh0, &o0);
    declined |= live_v17_b4 & (if bd_v17_b4 { ALL } else { !ok_v17_b4 });
    take_0_4 |= live_v17_b4 & ok_v17_b4 & (if bd_v17_b4 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c358: r_c357,
        c359: r_c358,
        c282: n1434,
        c360: r_c359,
        c361: r_c360,
        c284: n1562,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n1641,
        c287: n1740,
        c294: zb_splat(false),
        c295: zb_splat(true),
        c370: n1793,
        c371: n1783,
        c301: n1584,
        h1: n5252, h2: n5253,
    };
    // body 4: buttons 0x11, forks 0x0
    sink.o0(17, take_0_4, &sh0, &o0);
    declined |= live_v18_b5 & (if bd_v18_b5 { ALL } else { !ok_v18_b5 });
    take_0_5 |= live_v18_b5 & ok_v18_b5 & (if bd_v18_b5 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c358: r_c357,
        c359: r_c358,
        c282: n1434,
        c360: r_c359,
        c361: r_c360,
        c284: n1562,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n1697,
        c287: n1740,
        c294: zb_splat(false),
        c295: zb_splat(true),
        c370: n1834,
        c371: n1824,
        c301: n1584,
        h1: n5260, h2: n5261,
    };
    // body 5: buttons 0x12, forks 0x0
    sink.o0(18, take_0_5, &sh0, &o0);
    declined |= live_v32_b6 & (if bd_v32_b6 { ALL } else { !ok_v32_b6 });
    take_0_6 |= live_v32_b6 & ok_v32_b6 & (if bd_v32_b6 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1861,
        c41: n1862,
        c358: n1866,
        c359: n1867,
        c282: n1863,
        c360: n1868,
        c361: n1869,
        c284: n1864,
        c285: n1865,
        c362: n1563,
        c287: n1432,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1885,
        c371: n1871,
        c301: n1884,
        h1: n5304, h2: n5305,
    };
    // body 6: buttons 0x20, forks 0x0
    sink.o0(32, take_0_6, &sh0, &o0);
    declined |= live_v33_b7 & (if bd_v33_b7 { ALL } else { !ok_v33_b7 });
    take_0_7 |= live_v33_b7 & ok_v33_b7 & (if bd_v33_b7 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1861,
        c41: n1862,
        c358: n1866,
        c359: n1894,
        c282: n1863,
        c360: n1895,
        c361: n1869,
        c284: n1864,
        c285: n1865,
        c362: n1641,
        c287: n1432,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1908,
        c371: n1897,
        c301: n1884,
        h1: n5320, h2: n5321,
    };
    // body 7: buttons 0x21, forks 0x0
    sink.o0(33, take_0_7, &sh0, &o0);
    declined |= live_v34_b8 & (if bd_v34_b8 { ALL } else { !ok_v34_b8 });
    take_0_8 |= live_v34_b8 & ok_v34_b8 & (if bd_v34_b8 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1861,
        c41: n1862,
        c358: n1866,
        c359: n1894,
        c282: n1863,
        c360: n1916,
        c361: n1869,
        c284: n1864,
        c285: n1865,
        c362: n1697,
        c287: n1432,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1929,
        c371: n1918,
        c301: n1884,
        h1: n5333, h2: n5334,
    };
    // body 8: buttons 0x22, forks 0x0
    sink.o0(34, take_0_8, &sh0, &o0);
    declined |= live_v36_b9 & (if bd_v36_b9 { ALL } else { !ok_v36_b9 });
    take_0_9 |= live_v36_b9 & ok_v36_b9 & (if bd_v36_b9 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1861,
        c41: n1862,
        c358: n1939,
        c359: n1940,
        c282: n1863,
        c360: n1941,
        c361: n1942,
        c284: n1864,
        c285: n1865,
        c362: n1563,
        c287: n1432,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1955,
        c371: n1944,
        c301: n1884,
        h1: n5353, h2: n5354,
    };
    // body 9: buttons 0x24, forks 0x0
    sink.o0(36, take_0_9, &sh0, &o0);
    declined |= live_v37_b10 & (if bd_v37_b10 { ALL } else { !ok_v37_b10 });
    take_0_10 |= live_v37_b10 & ok_v37_b10 & (if bd_v37_b10 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1861,
        c41: n1862,
        c358: n1939,
        c359: n1894,
        c282: n1863,
        c360: n1895,
        c361: n1942,
        c284: n1864,
        c285: n1865,
        c362: n1641,
        c287: n1432,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1962,
        c371: n1960,
        c301: n1884,
        h1: n5367, h2: n5368,
    };
    // body 10: buttons 0x25, forks 0x0
    sink.o0(37, take_0_10, &sh0, &o0);
    declined |= live_v38_b11 & (if bd_v38_b11 { ALL } else { !ok_v38_b11 });
    take_0_11 |= live_v38_b11 & ok_v38_b11 & (if bd_v38_b11 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1861,
        c41: n1862,
        c358: n1939,
        c359: n1894,
        c282: n1863,
        c360: n1916,
        c361: n1942,
        c284: n1864,
        c285: n1865,
        c362: n1697,
        c287: n1432,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1968,
        c371: n1966,
        c301: n1884,
        h1: n5379, h2: n5380,
    };
    // body 11: buttons 0x26, forks 0x0
    sink.o0(38, take_0_11, &sh0, &o0);
    declined |= live_v40_b12 & (if bd_v40_b12 { ALL } else { !ok_v40_b12 });
    take_0_12 |= live_v40_b12 & ok_v40_b12 & (if bd_v40_b12 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1861,
        c41: n1862,
        c358: n1939,
        c359: n1940,
        c282: n1863,
        c360: n1941,
        c361: n1971,
        c284: n1864,
        c285: n1865,
        c362: n1563,
        c287: n1432,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1955,
        c371: n1972,
        c301: n1884,
        h1: n5389, h2: n5390,
    };
    // body 12: buttons 0x28, forks 0x0
    sink.o0(40, take_0_12, &sh0, &o0);
    declined |= live_v41_b13 & (if bd_v41_b13 { ALL } else { !ok_v41_b13 });
    take_0_13 |= live_v41_b13 & ok_v41_b13 & (if bd_v41_b13 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1861,
        c41: n1862,
        c358: n1939,
        c359: n1894,
        c282: n1863,
        c360: n1895,
        c361: n1971,
        c284: n1864,
        c285: n1865,
        c362: n1641,
        c287: n1432,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1962,
        c371: n1974,
        c301: n1884,
        h1: n5398, h2: n5399,
    };
    // body 13: buttons 0x29, forks 0x0
    sink.o0(41, take_0_13, &sh0, &o0);
    declined |= live_v42_b14 & (if bd_v42_b14 { ALL } else { !ok_v42_b14 });
    take_0_14 |= live_v42_b14 & ok_v42_b14 & (if bd_v42_b14 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1861,
        c41: n1862,
        c358: n1939,
        c359: n1894,
        c282: n1863,
        c360: n1916,
        c361: n1971,
        c284: n1864,
        c285: n1865,
        c362: n1697,
        c287: n1432,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1968,
        c371: n1976,
        c301: n1884,
        h1: n5407, h2: n5408,
    };
    // body 14: buttons 0x2a, forks 0x0
    sink.o0(42, take_0_14, &sh0, &o0);
    declined |= live_v48_b15 & (if bd_v48_b15 { ALL } else { !ok_v48_b15 });
    take_0_15 |= live_v48_b15 & ok_v48_b15 & (if bd_v48_b15 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1861,
        c41: n1862,
        c358: n1866,
        c359: n1867,
        c282: n1863,
        c360: n1868,
        c361: n1869,
        c284: n1864,
        c285: n1865,
        c362: n1563,
        c287: n1740,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n2006,
        c371: n1995,
        c301: n1884,
        h1: n5431, h2: n5432,
    };
    // body 15: buttons 0x30, forks 0x0
    sink.o0(48, take_0_15, &sh0, &o0);
    declined |= live_v49_b16 & (if bd_v49_b16 { ALL } else { !ok_v49_b16 });
    take_0_16 |= live_v49_b16 & ok_v49_b16 & (if bd_v49_b16 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1861,
        c41: n1862,
        c358: n1866,
        c359: n1894,
        c282: n1863,
        c360: n1895,
        c361: n1869,
        c284: n1864,
        c285: n1865,
        c362: n1641,
        c287: n1740,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n2025,
        c371: n2014,
        c301: n1884,
        h1: n5445, h2: n5446,
    };
    // body 16: buttons 0x31, forks 0x0
    sink.o0(49, take_0_16, &sh0, &o0);
    declined |= live_v50_b17 & (if bd_v50_b17 { ALL } else { !ok_v50_b17 });
    take_0_17 |= live_v50_b17 & ok_v50_b17 & (if bd_v50_b17 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1861,
        c41: n1862,
        c358: n1866,
        c359: n1894,
        c282: n1863,
        c360: n1916,
        c361: n1869,
        c284: n1864,
        c285: n1865,
        c362: n1697,
        c287: n1740,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n2044,
        c371: n2033,
        c301: n1884,
        h1: n5457, h2: n5458,
    };
    // body 17: buttons 0x32, forks 0x0
    sink.o0(50, take_0_17, &sh0, &o0);
    declined |= live_v52_b18 & (if bd_v52_b18 { ALL } else { !ok_v52_b18 });
    take_0_18 |= live_v52_b18 & ok_v52_b18 & (if bd_v52_b18 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1861,
        c41: n1862,
        c358: n1939,
        c359: n1940,
        c282: n1863,
        c360: n1941,
        c361: n1942,
        c284: n1864,
        c285: n1865,
        c362: n1563,
        c287: n1740,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n2061,
        c371: n2050,
        c301: n1884,
        h1: n5473, h2: n5474,
    };
    // body 18: buttons 0x34, forks 0x0
    sink.o0(52, take_0_18, &sh0, &o0);
    declined |= live_v53_b19 & (if bd_v53_b19 { ALL } else { !ok_v53_b19 });
    take_0_19 |= live_v53_b19 & ok_v53_b19 & (if bd_v53_b19 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1861,
        c41: n1862,
        c358: n1939,
        c359: n1894,
        c282: n1863,
        c360: n1895,
        c361: n1942,
        c284: n1864,
        c285: n1865,
        c362: n1641,
        c287: n1740,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n2068,
        c371: n2066,
        c301: n1884,
        h1: n5487, h2: n5488,
    };
    // body 19: buttons 0x35, forks 0x0
    sink.o0(53, take_0_19, &sh0, &o0);
    declined |= live_v54_b20 & (if bd_v54_b20 { ALL } else { !ok_v54_b20 });
    take_0_20 |= live_v54_b20 & ok_v54_b20 & (if bd_v54_b20 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1861,
        c41: n1862,
        c358: n1939,
        c359: n1894,
        c282: n1863,
        c360: n1916,
        c361: n1942,
        c284: n1864,
        c285: n1865,
        c362: n1697,
        c287: n1740,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n2074,
        c371: n2072,
        c301: n1884,
        h1: n5499, h2: n5500,
    };
    // body 20: buttons 0x36, forks 0x0
    sink.o0(54, take_0_20, &sh0, &o0);
    declined |= live_v56_b21 & (if bd_v56_b21 { ALL } else { !ok_v56_b21 });
    take_0_21 |= live_v56_b21 & ok_v56_b21 & (if bd_v56_b21 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1861,
        c41: n1862,
        c358: n1939,
        c359: n1940,
        c282: n1863,
        c360: n1941,
        c361: n1971,
        c284: n1864,
        c285: n1865,
        c362: n1563,
        c287: n1740,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n2061,
        c371: n2076,
        c301: n1884,
        h1: n5508, h2: n5509,
    };
    // body 21: buttons 0x38, forks 0x0
    sink.o0(56, take_0_21, &sh0, &o0);
    declined |= live_v57_b22 & (if bd_v57_b22 { ALL } else { !ok_v57_b22 });
    take_0_22 |= live_v57_b22 & ok_v57_b22 & (if bd_v57_b22 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1861,
        c41: n1862,
        c358: n1939,
        c359: n1894,
        c282: n1863,
        c360: n1895,
        c361: n1971,
        c284: n1864,
        c285: n1865,
        c362: n1641,
        c287: n1740,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n2068,
        c371: n2078,
        c301: n1884,
        h1: n5517, h2: n5518,
    };
    // body 22: buttons 0x39, forks 0x0
    sink.o0(57, take_0_22, &sh0, &o0);
    declined |= live_v58_b23 & (if bd_v58_b23 { ALL } else { !ok_v58_b23 });
    take_0_23 |= live_v58_b23 & ok_v58_b23 & (if bd_v58_b23 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1861,
        c41: n1862,
        c358: n1939,
        c359: n1894,
        c282: n1863,
        c360: n1916,
        c361: n1971,
        c284: n1864,
        c285: n1865,
        c362: n1697,
        c287: n1740,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n2074,
        c371: n2080,
        c301: n1884,
        h1: n5526, h2: n5527,
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
        h1: n5543, h2: n5544,
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
        c20: n1861,
        c41: n1862,
        h1: n5547, h2: n5548,
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
        h1: n5560, h2: n5561,
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
        c20: n4075,
        c41: n4076,
        h1: n5566, h2: n5567,
    };
    // body 51: buttons 0x34, forks 0x0
    sink.o2(52, take_2_1, &sh2, &o2);
    declined |= live_v0_b52 & (if bd_v0_b52 { ALL } else { !ok_v0_b52 });
    take_3_0 |= live_v0_b52 & ok_v0_b52 & (if bd_v0_b52 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4261,
        c39: n4262,
        c20: r_c20,
        c38: n4257,
        h1: n5577, h2: n5578,
    };
    // body 52: buttons 0x00, forks 0x0
    sink.o3(0, take_3_0, &sh3, &o3);
    declined |= live_v1_b53 & (if bd_v1_b53 { ALL } else { !ok_v1_b53 });
    take_3_1 |= live_v1_b53 & ok_v1_b53 & (if bd_v1_b53 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4305,
        c39: n4306,
        c20: r_c20,
        c38: n4301,
        h1: n5586, h2: n5587,
    };
    // body 53: buttons 0x01, forks 0x0
    sink.o3(1, take_3_1, &sh3, &o3);
    declined |= live_v2_b54 & (if bd_v2_b54 { ALL } else { !ok_v2_b54 });
    take_3_2 |= live_v2_b54 & ok_v2_b54 & (if bd_v2_b54 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4349,
        c39: n4350,
        c20: r_c20,
        c38: n4345,
        h1: n5595, h2: n5596,
    };
    // body 54: buttons 0x02, forks 0x0
    sink.o3(2, take_3_2, &sh3, &o3);
    declined |= live_v16_b55 & (if bd_v16_b55 { ALL } else { !ok_v16_b55 });
    take_3_3 |= live_v16_b55 & ok_v16_b55 & (if bd_v16_b55 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4392,
        c39: n4393,
        c20: r_c20,
        c38: n4388,
        h1: n5604, h2: n5605,
    };
    // body 55: buttons 0x10, forks 0x0
    sink.o3(16, take_3_3, &sh3, &o3);
    declined |= live_v17_b56 & (if bd_v17_b56 { ALL } else { !ok_v17_b56 });
    take_3_4 |= live_v17_b56 & ok_v17_b56 & (if bd_v17_b56 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4435,
        c39: n4436,
        c20: r_c20,
        c38: n4431,
        h1: n5613, h2: n5614,
    };
    // body 56: buttons 0x11, forks 0x0
    sink.o3(17, take_3_4, &sh3, &o3);
    declined |= live_v18_b57 & (if bd_v18_b57 { ALL } else { !ok_v18_b57 });
    take_3_5 |= live_v18_b57 & ok_v18_b57 & (if bd_v18_b57 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4478,
        c39: n4479,
        c20: r_c20,
        c38: n4474,
        h1: n5622, h2: n5623,
    };
    // body 57: buttons 0x12, forks 0x0
    sink.o3(18, take_3_5, &sh3, &o3);
    declined |= live_v32_b58 & (if bd_v32_b58 { ALL } else { !ok_v32_b58 });
    take_3_6 |= live_v32_b58 & ok_v32_b58 & (if bd_v32_b58 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4508,
        c39: n4509,
        c20: n4075,
        c38: n4502,
        h1: n5633, h2: n5634,
    };
    // body 58: buttons 0x20, forks 0x0
    sink.o3(32, take_3_6, &sh3, &o3);
    declined |= live_v33_b59 & (if bd_v33_b59 { ALL } else { !ok_v33_b59 });
    take_3_7 |= live_v33_b59 & ok_v33_b59 & (if bd_v33_b59 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4526,
        c39: n4527,
        c20: n4075,
        c38: n4520,
        h1: n5642, h2: n5643,
    };
    // body 59: buttons 0x21, forks 0x0
    sink.o3(33, take_3_7, &sh3, &o3);
    declined |= live_v34_b60 & (if bd_v34_b60 { ALL } else { !ok_v34_b60 });
    take_3_8 |= live_v34_b60 & ok_v34_b60 & (if bd_v34_b60 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4544,
        c39: n4545,
        c20: n4075,
        c38: n4538,
        h1: n5651, h2: n5652,
    };
    // body 60: buttons 0x22, forks 0x0
    sink.o3(34, take_3_8, &sh3, &o3);
    declined |= live_v36_b61 & (if bd_v36_b61 { ALL } else { !ok_v36_b61 });
    take_3_9 |= live_v36_b61 & ok_v36_b61 & (if bd_v36_b61 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4560,
        c39: n4561,
        c20: n4075,
        c38: n4554,
        h1: n5660, h2: n5661,
    };
    // body 61: buttons 0x24, forks 0x0
    sink.o3(36, take_3_9, &sh3, &o3);
    declined |= live_v48_b62 & (if bd_v48_b62 { ALL } else { !ok_v48_b62 });
    take_3_10 |= live_v48_b62 & ok_v48_b62 & (if bd_v48_b62 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4590,
        c39: n4591,
        c20: n4075,
        c38: n4584,
        h1: n5669, h2: n5670,
    };
    // body 62: buttons 0x30, forks 0x0
    sink.o3(48, take_3_10, &sh3, &o3);
    declined |= live_v49_b63 & (if bd_v49_b63 { ALL } else { !ok_v49_b63 });
    take_3_11 |= live_v49_b63 & ok_v49_b63 & (if bd_v49_b63 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4608,
        c39: n4609,
        c20: n4075,
        c38: n4602,
        h1: n5678, h2: n5679,
    };
    // body 63: buttons 0x31, forks 0x0
    sink.o3(49, take_3_11, &sh3, &o3);
    declined |= live_v50_b64 & (if bd_v50_b64 { ALL } else { !ok_v50_b64 });
    take_3_12 |= live_v50_b64 & ok_v50_b64 & (if bd_v50_b64 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4626,
        c39: n4627,
        c20: n4075,
        c38: n4620,
        h1: n5687, h2: n5688,
    };
    // body 64: buttons 0x32, forks 0x0
    sink.o3(50, take_3_12, &sh3, &o3);
    declined |= live_v52_b65 & (if bd_v52_b65 { ALL } else { !ok_v52_b65 });
    take_3_13 |= live_v52_b65 & ok_v52_b65 & (if bd_v52_b65 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4642,
        c39: n4643,
        c20: n4075,
        c38: n4636,
        h1: n5696, h2: n5697,
    };
    // body 65: buttons 0x34, forks 0x0
    sink.o3(52, take_3_13, &sh3, &o3);
    declined |= live_v0_b66 & (if bd_v0_b66 { ALL } else { !ok_v0_b66 });
    take_4_0 |= live_v0_b66 & ok_v0_b66 & (if bd_v0_b66 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4714,
        c41: r_c41,
        c357: r_c357,
        c358: r_c358,
        c281: n4717,
        c359: r_c359,
        c360: r_c360,
        c283: n4718,
        c284: n4719,
        c361: n4725,
        c286: n4720,
        c293: n4721,
        c294: n4722,
        c369: n4752,
        c370: n4729,
        c300: n4751,
        h1: n5754, h2: n5755,
    };
    // body 66: buttons 0x00, forks 0x0
    sink.o4(0, take_4_0, &sh4, &o4);
    declined |= live_v1_b67 & (if bd_v1_b67 { ALL } else { !ok_v1_b67 });
    take_4_1 |= live_v1_b67 & ok_v1_b67 & (if bd_v1_b67 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4714,
        c41: r_c41,
        c357: r_c357,
        c358: r_c358,
        c281: n4717,
        c359: r_c359,
        c360: r_c360,
        c283: n4718,
        c284: n4719,
        c361: n4767,
        c286: n4720,
        c293: n4721,
        c294: n4722,
        c369: n4780,
        c370: n4769,
        c300: n4751,
        h1: n5763, h2: n5764,
    };
    // body 67: buttons 0x01, forks 0x0
    sink.o4(1, take_4_1, &sh4, &o4);
    declined |= live_v2_b68 & (if bd_v2_b68 { ALL } else { !ok_v2_b68 });
    take_4_2 |= live_v2_b68 & ok_v2_b68 & (if bd_v2_b68 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4714,
        c41: r_c41,
        c357: r_c357,
        c358: r_c358,
        c281: n4717,
        c359: r_c359,
        c360: r_c360,
        c283: n4718,
        c284: n4719,
        c361: n4794,
        c286: n4720,
        c293: n4721,
        c294: n4722,
        c369: n4807,
        c370: n4796,
        c300: n4751,
        h1: n5772, h2: n5773,
    };
    // body 68: buttons 0x02, forks 0x0
    sink.o4(2, take_4_2, &sh4, &o4);
    declined |= live_v16_b69 & (if bd_v16_b69 { ALL } else { !ok_v16_b69 });
    take_4_3 |= live_v16_b69 & ok_v16_b69 & (if bd_v16_b69 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4714,
        c41: r_c41,
        c357: r_c357,
        c358: r_c358,
        c281: n4717,
        c359: r_c359,
        c360: r_c360,
        c283: n4718,
        c284: n4719,
        c361: n4725,
        c286: n4816,
        c293: n4721,
        c294: n4817,
        c369: n4830,
        c370: n4819,
        c300: n4751,
        h1: n5798, h2: n5799,
    };
    // body 69: buttons 0x10, forks 0x0
    sink.o4(16, take_4_3, &sh4, &o4);
    declined |= live_v17_b70 & (if bd_v17_b70 { ALL } else { !ok_v17_b70 });
    take_4_4 |= live_v17_b70 & ok_v17_b70 & (if bd_v17_b70 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4714,
        c41: r_c41,
        c357: r_c357,
        c358: r_c358,
        c281: n4717,
        c359: r_c359,
        c360: r_c360,
        c283: n4718,
        c284: n4719,
        c361: n4767,
        c286: n4816,
        c293: n4721,
        c294: n4817,
        c369: n4849,
        c370: n4838,
        c300: n4751,
        h1: n5806, h2: n5807,
    };
    // body 70: buttons 0x11, forks 0x0
    sink.o4(17, take_4_4, &sh4, &o4);
    declined |= live_v18_b71 & (if bd_v18_b71 { ALL } else { !ok_v18_b71 });
    take_4_5 |= live_v18_b71 & ok_v18_b71 & (if bd_v18_b71 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4714,
        c41: r_c41,
        c357: r_c357,
        c358: r_c358,
        c281: n4717,
        c359: r_c359,
        c360: r_c360,
        c283: n4718,
        c284: n4719,
        c361: n4794,
        c286: n4816,
        c293: n4721,
        c294: n4817,
        c369: n4868,
        c370: n4857,
        c300: n4751,
        h1: n5814, h2: n5815,
    };
    // body 71: buttons 0x12, forks 0x0
    sink.o4(18, take_4_5, &sh4, &o4);
    declined |= live_v32_b72 & (if bd_v32_b72 { ALL } else { !ok_v32_b72 });
    take_4_6 |= live_v32_b72 & ok_v32_b72 & (if bd_v32_b72 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4889,
        c41: n4890,
        c357: n4895,
        c358: n4896,
        c281: n4891,
        c359: n4897,
        c360: n4898,
        c283: n4892,
        c284: n4893,
        c361: n4725,
        c286: n4720,
        c293: n4894,
        c294: n4722,
        c369: n4914,
        c370: n4900,
        c300: n4913,
        h1: n5859, h2: n5860,
    };
    // body 72: buttons 0x20, forks 0x0
    sink.o4(32, take_4_6, &sh4, &o4);
    declined |= live_v33_b73 & (if bd_v33_b73 { ALL } else { !ok_v33_b73 });
    take_4_7 |= live_v33_b73 & ok_v33_b73 & (if bd_v33_b73 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4889,
        c41: n4890,
        c357: n4895,
        c358: n4925,
        c281: n4891,
        c359: n4926,
        c360: n4898,
        c283: n4892,
        c284: n4893,
        c361: n4767,
        c286: n4720,
        c293: n4894,
        c294: n4722,
        c369: n4939,
        c370: n4928,
        c300: n4913,
        h1: n5875, h2: n5876,
    };
    // body 73: buttons 0x21, forks 0x0
    sink.o4(33, take_4_7, &sh4, &o4);
    declined |= live_v34_b74 & (if bd_v34_b74 { ALL } else { !ok_v34_b74 });
    take_4_8 |= live_v34_b74 & ok_v34_b74 & (if bd_v34_b74 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4889,
        c41: n4890,
        c357: n4895,
        c358: n4925,
        c281: n4891,
        c359: n4948,
        c360: n4898,
        c283: n4892,
        c284: n4893,
        c361: n4794,
        c286: n4720,
        c293: n4894,
        c294: n4722,
        c369: n4961,
        c370: n4950,
        c300: n4913,
        h1: n5888, h2: n5889,
    };
    // body 74: buttons 0x22, forks 0x0
    sink.o4(34, take_4_8, &sh4, &o4);
    declined |= live_v36_b75 & (if bd_v36_b75 { ALL } else { !ok_v36_b75 });
    take_4_9 |= live_v36_b75 & ok_v36_b75 & (if bd_v36_b75 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4889,
        c41: n4890,
        c357: n4976,
        c358: n4977,
        c281: n4891,
        c359: n4978,
        c360: n4979,
        c283: n4892,
        c284: n4893,
        c361: n4725,
        c286: n4720,
        c293: n4894,
        c294: n4722,
        c369: n4992,
        c370: n4981,
        c300: n4913,
        h1: n5908, h2: n5909,
    };
    // body 75: buttons 0x24, forks 0x0
    sink.o4(36, take_4_9, &sh4, &o4);
    declined |= live_v37_b76 & (if bd_v37_b76 { ALL } else { !ok_v37_b76 });
    take_4_10 |= live_v37_b76 & ok_v37_b76 & (if bd_v37_b76 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4889,
        c41: n4890,
        c357: n4976,
        c358: n4925,
        c281: n4891,
        c359: n4926,
        c360: n4979,
        c283: n4892,
        c284: n4893,
        c361: n4767,
        c286: n4720,
        c293: n4894,
        c294: n4722,
        c369: n5001,
        c370: n4999,
        c300: n4913,
        h1: n5922, h2: n5923,
    };
    // body 76: buttons 0x25, forks 0x0
    sink.o4(37, take_4_10, &sh4, &o4);
    declined |= live_v38_b77 & (if bd_v38_b77 { ALL } else { !ok_v38_b77 });
    take_4_11 |= live_v38_b77 & ok_v38_b77 & (if bd_v38_b77 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4889,
        c41: n4890,
        c357: n4976,
        c358: n4925,
        c281: n4891,
        c359: n4948,
        c360: n4979,
        c283: n4892,
        c284: n4893,
        c361: n4794,
        c286: n4720,
        c293: n4894,
        c294: n4722,
        c369: n5009,
        c370: n5007,
        c300: n4913,
        h1: n5934, h2: n5935,
    };
    // body 77: buttons 0x26, forks 0x0
    sink.o4(38, take_4_11, &sh4, &o4);
    declined |= live_v40_b78 & (if bd_v40_b78 { ALL } else { !ok_v40_b78 });
    take_4_12 |= live_v40_b78 & ok_v40_b78 & (if bd_v40_b78 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4889,
        c41: n4890,
        c357: n4976,
        c358: n4977,
        c281: n4891,
        c359: n4978,
        c360: n5014,
        c283: n4892,
        c284: n4893,
        c361: n4725,
        c286: n4720,
        c293: n4894,
        c294: n4722,
        c369: n4992,
        c370: n5015,
        c300: n4913,
        h1: n5944, h2: n5945,
    };
    // body 78: buttons 0x28, forks 0x0
    sink.o4(40, take_4_12, &sh4, &o4);
    declined |= live_v41_b79 & (if bd_v41_b79 { ALL } else { !ok_v41_b79 });
    take_4_13 |= live_v41_b79 & ok_v41_b79 & (if bd_v41_b79 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4889,
        c41: n4890,
        c357: n4976,
        c358: n4925,
        c281: n4891,
        c359: n4926,
        c360: n5014,
        c283: n4892,
        c284: n4893,
        c361: n4767,
        c286: n4720,
        c293: n4894,
        c294: n4722,
        c369: n5001,
        c370: n5018,
        c300: n4913,
        h1: n5953, h2: n5954,
    };
    // body 79: buttons 0x29, forks 0x0
    sink.o4(41, take_4_13, &sh4, &o4);
    declined |= live_v42_b80 & (if bd_v42_b80 { ALL } else { !ok_v42_b80 });
    take_4_14 |= live_v42_b80 & ok_v42_b80 & (if bd_v42_b80 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4889,
        c41: n4890,
        c357: n4976,
        c358: n4925,
        c281: n4891,
        c359: n4948,
        c360: n5014,
        c283: n4892,
        c284: n4893,
        c361: n4794,
        c286: n4720,
        c293: n4894,
        c294: n4722,
        c369: n5009,
        c370: n5021,
        c300: n4913,
        h1: n5962, h2: n5963,
    };
    // body 80: buttons 0x2a, forks 0x0
    sink.o4(42, take_4_14, &sh4, &o4);
    declined |= live_v48_b81 & (if bd_v48_b81 { ALL } else { !ok_v48_b81 });
    take_4_15 |= live_v48_b81 & ok_v48_b81 & (if bd_v48_b81 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4889,
        c41: n4890,
        c357: n4895,
        c358: n4896,
        c281: n4891,
        c359: n4897,
        c360: n4898,
        c283: n4892,
        c284: n4893,
        c361: n4725,
        c286: n4816,
        c293: n4894,
        c294: n4817,
        c369: n5039,
        c370: n5028,
        c300: n4913,
        h1: n5986, h2: n5987,
    };
    // body 81: buttons 0x30, forks 0x0
    sink.o4(48, take_4_15, &sh4, &o4);
    declined |= live_v49_b82 & (if bd_v49_b82 { ALL } else { !ok_v49_b82 });
    take_4_16 |= live_v49_b82 & ok_v49_b82 & (if bd_v49_b82 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4889,
        c41: n4890,
        c357: n4895,
        c358: n4925,
        c281: n4891,
        c359: n4926,
        c360: n4898,
        c283: n4892,
        c284: n4893,
        c361: n4767,
        c286: n4816,
        c293: n4894,
        c294: n4817,
        c369: n5058,
        c370: n5047,
        c300: n4913,
        h1: n6000, h2: n6001,
    };
    // body 82: buttons 0x31, forks 0x0
    sink.o4(49, take_4_16, &sh4, &o4);
    declined |= live_v50_b83 & (if bd_v50_b83 { ALL } else { !ok_v50_b83 });
    take_4_17 |= live_v50_b83 & ok_v50_b83 & (if bd_v50_b83 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4889,
        c41: n4890,
        c357: n4895,
        c358: n4925,
        c281: n4891,
        c359: n4948,
        c360: n4898,
        c283: n4892,
        c284: n4893,
        c361: n4794,
        c286: n4816,
        c293: n4894,
        c294: n4817,
        c369: n5077,
        c370: n5066,
        c300: n4913,
        h1: n6012, h2: n6013,
    };
    // body 83: buttons 0x32, forks 0x0
    sink.o4(50, take_4_17, &sh4, &o4);
    declined |= live_v52_b84 & (if bd_v52_b84 { ALL } else { !ok_v52_b84 });
    take_4_18 |= live_v52_b84 & ok_v52_b84 & (if bd_v52_b84 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4889,
        c41: n4890,
        c357: n4976,
        c358: n4977,
        c281: n4891,
        c359: n4978,
        c360: n4979,
        c283: n4892,
        c284: n4893,
        c361: n4725,
        c286: n4816,
        c293: n4894,
        c294: n4817,
        c369: n5096,
        c370: n5085,
        c300: n4913,
        h1: n6028, h2: n6029,
    };
    // body 84: buttons 0x34, forks 0x0
    sink.o4(52, take_4_18, &sh4, &o4);
    declined |= live_v53_b85 & (if bd_v53_b85 { ALL } else { !ok_v53_b85 });
    take_4_19 |= live_v53_b85 & ok_v53_b85 & (if bd_v53_b85 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4889,
        c41: n4890,
        c357: n4976,
        c358: n4925,
        c281: n4891,
        c359: n4926,
        c360: n4979,
        c283: n4892,
        c284: n4893,
        c361: n4767,
        c286: n4816,
        c293: n4894,
        c294: n4817,
        c369: n5105,
        c370: n5103,
        c300: n4913,
        h1: n6042, h2: n6043,
    };
    // body 85: buttons 0x35, forks 0x0
    sink.o4(53, take_4_19, &sh4, &o4);
    declined |= live_v54_b86 & (if bd_v54_b86 { ALL } else { !ok_v54_b86 });
    take_4_20 |= live_v54_b86 & ok_v54_b86 & (if bd_v54_b86 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4889,
        c41: n4890,
        c357: n4976,
        c358: n4925,
        c281: n4891,
        c359: n4948,
        c360: n4979,
        c283: n4892,
        c284: n4893,
        c361: n4794,
        c286: n4816,
        c293: n4894,
        c294: n4817,
        c369: n5113,
        c370: n5111,
        c300: n4913,
        h1: n6054, h2: n6055,
    };
    // body 86: buttons 0x36, forks 0x0
    sink.o4(54, take_4_20, &sh4, &o4);
    declined |= live_v56_b87 & (if bd_v56_b87 { ALL } else { !ok_v56_b87 });
    take_4_21 |= live_v56_b87 & ok_v56_b87 & (if bd_v56_b87 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4889,
        c41: n4890,
        c357: n4976,
        c358: n4977,
        c281: n4891,
        c359: n4978,
        c360: n5014,
        c283: n4892,
        c284: n4893,
        c361: n4725,
        c286: n4816,
        c293: n4894,
        c294: n4817,
        c369: n5096,
        c370: n5116,
        c300: n4913,
        h1: n6063, h2: n6064,
    };
    // body 87: buttons 0x38, forks 0x0
    sink.o4(56, take_4_21, &sh4, &o4);
    declined |= live_v57_b88 & (if bd_v57_b88 { ALL } else { !ok_v57_b88 });
    take_4_22 |= live_v57_b88 & ok_v57_b88 & (if bd_v57_b88 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4889,
        c41: n4890,
        c357: n4976,
        c358: n4925,
        c281: n4891,
        c359: n4926,
        c360: n5014,
        c283: n4892,
        c284: n4893,
        c361: n4767,
        c286: n4816,
        c293: n4894,
        c294: n4817,
        c369: n5105,
        c370: n5119,
        c300: n4913,
        h1: n6072, h2: n6073,
    };
    // body 88: buttons 0x39, forks 0x0
    sink.o4(57, take_4_22, &sh4, &o4);
    declined |= live_v58_b89 & (if bd_v58_b89 { ALL } else { !ok_v58_b89 });
    take_4_23 |= live_v58_b89 & ok_v58_b89 & (if bd_v58_b89 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4889,
        c41: n4890,
        c357: n4976,
        c358: n4925,
        c281: n4891,
        c359: n4948,
        c360: n5014,
        c283: n4892,
        c284: n4893,
        c361: n4794,
        c286: n4816,
        c293: n4894,
        c294: n4817,
        c369: n5113,
        c370: n5122,
        c300: n4913,
        h1: n6081, h2: n6082,
    };
    // body 89: buttons 0x3a, forks 0x0
    sink.o4(58, take_4_23, &sh4, &o4);
    declined
}
