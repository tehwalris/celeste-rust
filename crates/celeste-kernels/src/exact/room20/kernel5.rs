// GENERATED from a TRACED frame (shape 5). Do not edit.
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
pub const SHAPE: u64 = 2559296174155201406;

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
    ("objects[0].delay", "num"),
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
    pub c241: ZN,
    pub c337: u16,
    pub c338: u16,
    pub c243: ZN,
    pub c244: ZN,
    pub c343: ZN,
    pub c344: ZN,
    pub c252: u16,
    pub c345: ZN,
    pub c346: ZN,
    pub c254: ZN,
    pub c256: ZN,
    pub c257: ZN,
    pub c260: u16,
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
    pub c241: u32,
    pub c337: u32,
    pub c338: u32,
    pub c243: u32,
    pub c244: u32,
    pub c343: u32,
    pub c344: u32,
    pub c252: u32,
    pub c345: u32,
    pub c346: u32,
    pub c254: u32,
    pub c256: u32,
    pub c257: u32,
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
        c241: cell("objects[0].delay")?,
        c337: cell("objects[0].flip.x")?,
        c338: cell("objects[0].flip.y")?,
        c243: cell("objects[0].hide_for")?,
        c244: cell("objects[0].hide_in")?,
        c343: cell("objects[0].rem.x")?,
        c344: cell("objects[0].rem.y")?,
        c252: cell("objects[0].solids")?,
        c345: cell("objects[0].spd.x")?,
        c346: cell("objects[0].spd.y")?,
        c254: cell("objects[0].spr")?,
        c256: cell("objects[0].x")?,
        c257: cell("objects[0].y")?,
        c260: cell("objects[1].collideable")?,
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
        c241: match &b.cols[s.c241 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
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
        c243: match &b.cols[s.c243 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c244: match &b.cols[s.c244 as usize] {
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
        c252: match &b.cols[s.c252 as usize] {
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
        c254: match &b.cols[s.c254 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c256: match &b.cols[s.c256 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c257: match &b.cols[s.c257 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c260: match &b.cols[s.c260 as usize] {
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
    pub c241: ZN,
    pub c254: ZN,
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
    pub c240: ZN,
    pub c253: ZN,
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
    pub c241: ZN,
    pub c254: ZN,
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
    b.cols[261] = Col::U(AV::Num(P8::from_raw(655360i32)));
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
    b.cols[274] = Col::U(AV::Num(P8::from_raw(1245184i32)));
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
    b.cols[260] = Col::U(AV::Num(P8::from_raw(655360i32)));
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
    b.cols[273] = Col::U(AV::Num(P8::from_raw(1245184i32)));
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
    b.cols[240] = Col::N(Vec::new());
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
    b.cols[253] = Col::N(Vec::new());
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
        if let Col::N(v) = &mut acc.cols[240] { v.push(sh.c240.lane(i)); }
        if let Col::N(v) = &mut acc.cols[253] { v.push(sh.c253.lane(i)); }
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
    b.cols[241] = Col::N(Vec::new());
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
    b.cols[254] = Col::N(Vec::new());
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
        if let Col::N(v) = &mut acc.cols[241] { v.push(sh.c241.lane(i)); }
        if let Col::N(v) = &mut acc.cols[254] { v.push(sh.c254.lane(i)); }
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
    let r_c241: ZN = rin.c241;
    let r_c243: ZN = rin.c243;
    let r_c244: ZN = rin.c244;
    let r_c252: ZB = ZB { val: rin.c252, known: ALL };
    let r_c254: ZN = rin.c254;
    let r_c256: ZN = rin.c256;
    let r_c257: ZN = rin.c257;
    let r_c260: ZB = ZB { val: rin.c260, known: ALL };
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
    let n101: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c243);
    let n102: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c244);
    let n103: bool = P8::from_raw(0i32) == u.c341;
    let n104: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c343);
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
    let n145: ZB = zb_not(r_c337);
    let n146: bool = P8::from_raw(524288i32) == u.c339;
    let n147: bool = P8::from_raw(0i32) == u.c342;
    let n148: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c345);
    let n149: ZB = zb_not(r_c348);
    let n150: bool = P8::from_raw(524288i32) == u.c350;
    let n151: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c356);
    let n152: ZB = zn_eq(zn_splat(P8::from_raw(6815744i32)), r_c275);
    let n153: ZB = zn_eq(zn_splat(P8::from_raw(7340032i32)), r_c276);
    let n154: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c86);
    let n158: ZB = zn_eq(zn_splat(P8::from_raw(1179648i32)), r_c273);
    let n159: ZB = zb_not(r_c338);
    let n160: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c346);
    let n161: ZN = zn_sub(r_c241, zn_splat(P8::from_raw(65536i32)));
    let n162: ZB = zn_gt(r_c241, zn_splat(P8::from_raw(0i32)));
    let n163: bool = P8::from_raw(524288i32) == u.c340;
    let n164: ZB = zn_eq(zn_splat(P8::from_raw(2621440i32)), r_c256);
    let n165: ZB = zn_eq(zn_splat(P8::from_raw(7340032i32)), r_c257);
    let n179: ZB = zn_le(n161, zn_splat(P8::from_raw(0i32)));
    let n180: ZN = zsel_n(n162, n161, r_c241);
    let n200: ZB = zn_eq(zn_splat(P8::from_raw(1179648i32)), r_c254);
    let n201: ZB = zb_not(n200);
    let n202: ZN = zsel_n(n179, zn_splat(P8::from_raw(1179648i32)), r_c254);
    let n203: ZN = zsel_n(n162, n202, r_c254);
    let n204: ZB = zb_not(r_c38);
    let n205: ZB = zb_not(r_c362);
    let n206: bool = P8::from_raw(327680i32) == u.c363;
    let n207: bool = P8::from_raw(393216i32) == u.c364;
    let n208: bool = P8::from_raw(65536i32) == u.c365;
    let n209: bool = P8::from_raw(196608i32) == u.c366;
    let n210: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c85);
    let n211: ZN = zn_rem(n210, zn_splat(P8::from_raw(3932160i32)));
    let n212: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n211);
    let n213: ZN = zsel_n(n212, n154, r_c86);
    let n214: ZN = zsel_n(n116, n213, r_c86);
    let n215: ZN = zsel_n(n116, n211, r_c85);
    let n216: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c300);
    let n217: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n216);
    let n218: ZB = zn_gt(n217, zn_splat(P8::from_raw(2621440i32)));
    let n219: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), r_c301);
    let n220: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n219);
    let n221: ZB = zn_gt(n220, zn_splat(P8::from_raw(7340032i32)));
    let n222: ZB = zb_and(n218, n221);
    let n223: ZB = zn_lt(n216, zn_splat(P8::from_raw(3145728i32)));
    let n224: ZB = zb_and(n222, n223);
    let n225: ZB = zn_lt(n219, zn_splat(P8::from_raw(7864320i32)));
    let n226: ZB = zb_and(n224, n225);
    let n227: ZB = zn_ge(r_c370, zn_splat(P8::from_raw(0i32)));
    let n228: ZN = zn_mul(r_c369, zn_splat(P8::from_raw(13107i32)));
    let n229: ZB = zn_gt(n217, zn_splat(P8::from_raw(6815744i32)));
    let n230: ZB = zn_lt(n216, zn_splat(P8::from_raw(7340032i32)));
    let n231: ZN = zsel_n(n227, zn_splat(P8::from_raw(7077888i32)), r_c301);
    let n232: ZN = zsel_n(n227, n228, r_c369);
    let n233: ZN = zsel_n(n227, zn_splat(P8::from_raw(-196608i32)), r_c370);
    let n234: ZN = zn_add(r_c368, zn_splat(P8::from_raw(-196608i32)));
    let n235: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n234);
    let n236: ZN = zn_flr(n235);
    let n237: ZN = zn_sub(n235, zn_splat(P8::from_raw(32768i32)));
    let n238: ZN = zn_sub(n237, n236);
    let n239: ZB = zn_gt(n236, zn_splat(P8::from_raw(0i32)));
    let n240: ZB = zn_le(n236, zn_splat(P8::from_raw(0i32)));
    let n241: ZB = zn_lt(n236, zn_splat(P8::from_raw(0i32)));
    let n242: ZB = zn_ge(n236, zn_splat(P8::from_raw(0i32)));
    let n243: ZN = zsel_n(n241, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n244: ZN = zsel_n(n239, zn_splat(P8::from_raw(65536i32)), n243);
    let n245: ZN = zn_abs(n236);
    let n246: ZB = zn_gt(n244, zn_splat(P8::from_raw(0i32)));
    let n247: ZB = zn_le(n244, zn_splat(P8::from_raw(0i32)));
    let n248: ZN = zn_add(zn_splat(P8::from_raw(7274496i32)), n244);
    let n249: ZN = zn_add(zn_splat(P8::from_raw(7077888i32)), n244);
    let n250: ZB = zn_le(zn_splat(P8::from_raw(65536i32)), n245);
    let n251: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n245);
    let n252: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n249);
    let n253: ZN = zn_add(n244, n252);
    let n254: ZN = zn_add(n244, n249);
    let n255: ZB = zn_le(zn_splat(P8::from_raw(131072i32)), n245);
    let n256: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n245);
    let n257: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n254);
    let n258: ZN = zn_add(n244, n257);
    let n259: ZN = zn_add(n244, n254);
    let n260: ZB = zn_le(zn_splat(P8::from_raw(196608i32)), n245);
    let n261: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n245);
    let n262: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n259);
    let n263: ZN = zn_add(n244, n262);
    let n264: ZN = zn_add(n244, n259);
    let n265: ZB = zn_le(zn_splat(P8::from_raw(262144i32)), n245);
    let n266: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n245);
    let n267: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n264);
    let n268: ZN = zn_add(n244, n267);
    let n269: ZN = zn_add(n244, n264);
    let n270: ZB = zn_le(zn_splat(P8::from_raw(327680i32)), n245);
    let n271: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n245);
    let n272: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n269);
    let n273: ZN = zn_add(n244, n272);
    let n274: ZN = zn_add(n244, n269);
    let n275: ZB = zn_le(zn_splat(P8::from_raw(393216i32)), n245);
    let n276: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n245);
    let n277: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n274);
    let n278: ZN = zn_add(n244, n277);
    let n279: ZN = zn_add(n244, n274);
    let n280: ZB = zn_le(zn_splat(P8::from_raw(458752i32)), n245);
    let n281: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n245);
    let n282: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n279);
    let n283: ZN = zn_add(n244, n282);
    let n284: ZN = zn_add(n244, n279);
    let n285: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n245);
    let n286: ZB = zn_gt(r_c286, zn_splat(P8::from_raw(0i32)));
    let n287: ZB = zn_le(r_c286, zn_splat(P8::from_raw(0i32)));
    let n288: ZN = zn_sub(r_c286, zn_splat(P8::from_raw(65536i32)));
    let n289: ZN = zsel_n(n286, n288, r_c286);
    let n290: ZN = zn_sub(r_c281, zn_splat(P8::from_raw(65536i32)));
    let n291: ZB = zn_gt(r_c283, zn_splat(P8::from_raw(0i32)));
    let n292: ZB = zn_le(r_c283, zn_splat(P8::from_raw(0i32)));
    let n293: ZN = zn_sub(r_c283, zn_splat(P8::from_raw(65536i32)));
    let n294: ZN = zsel_n(n291, n293, r_c283);
    let n295: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n296: ZN = zsel_n(n227, zn_splat(P8::from_raw(655360i32)), r_c241);
    let n297: ZN = zsel_n(n227, zn_splat(P8::from_raw(1245184i32)), r_c254);
    let n298: ZN = zsel_n(n226, n296, r_c241);
    let n299: ZN = zsel_n(n226, n297, r_c254);
    let n300: ZN = zsel_n(n226, n231, r_c301);
    let n301: ZN = zsel_n(n226, n232, r_c369);
    let n302: ZN = zsel_n(n226, n233, r_c370);
    let n303: ZN = zsel_n(n201, n180, n298);
    let n304: ZN = zsel_n(n201, n203, n299);
    let n305: ZN = zsel_n(n201, r_c301, n300);
    let n306: ZN = zsel_n(n201, r_c369, n301);
    let n307: ZN = zsel_n(n201, r_c370, n302);
    let n308: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n305);
    let n309: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n308);
    let n310: ZB = zn_gt(n309, zn_splat(P8::from_raw(7340032i32)));
    let n311: ZB = zb_and(n229, n310);
    let n312: ZB = zb_and(n230, n311);
    let n313: ZB = zn_lt(n308, zn_splat(P8::from_raw(7864320i32)));
    let n314: ZB = zb_and(n312, n313);
    let n315: ZB = zb_and(n134, n314);
    let n316: ZB = zn_ge(n307, zn_splat(P8::from_raw(0i32)));
    let n317: ZB = zb_and(n315, n316);
    let n318: ZN = zn_mul(n306, zn_splat(P8::from_raw(13107i32)));
    let n319: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n318);
    let n320: ZB = zb_not(n319);
    let n321: ZB = zb_and(n317, n319);
    let n322: ZB = zb_and(n317, n320);
    let n323: ZB = zb_or(n321, n322);
    let n324: ZN = zn_add(r_c367, n318);
    let n325: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n324);
    let n326: ZN = zn_flr(n325);
    let n327: ZN = zn_sub(n325, zn_splat(P8::from_raw(32768i32)));
    let n328: ZN = zn_sub(n327, n326);
    let n329: ZB = zn_gt(n326, zn_splat(P8::from_raw(0i32)));
    let n330: ZB = zn_le(n326, zn_splat(P8::from_raw(0i32)));
    let n331: ZB = zb_and(n323, n329);
    let n332: ZB = zb_and(n323, n330);
    let n333: ZB = zn_lt(n326, zn_splat(P8::from_raw(0i32)));
    let n334: ZB = zn_ge(n326, zn_splat(P8::from_raw(0i32)));
    let n335: ZB = zb_and(n332, n333);
    let n336: ZB = zb_and(n332, n334);
    let n337: ZN = zsel_n(n333, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n338: ZB = zb_or(n335, n336);
    let n339: ZN = zsel_n(n329, zn_splat(P8::from_raw(65536i32)), n337);
    let n340: ZB = zb_or(n331, n338);
    let n341: ZN = zn_abs(n326);
    let n342: ZN = zn_add(n216, n339);
    let n343: ZB = zn_tile_flag_at(g.cache, g.cart, n342, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n344: ZB = zb_not(n343);
    let n345: ZB = zb_and(n340, n344);
    let n346: ZB = zb_and(n340, n343);
    let n347: ZB = zb_or(n345, n346);
    let n348: ZB = zb_and(n344, n347);
    let n349: ZB = zb_and(n343, n347);
    let n350: ZB = zb_or(n348, n349);
    let n351: ZB = zb_and(n344, n350);
    let n352: ZB = zb_and(n343, n350);
    let n353: ZN = zn_add(r_c300, n339);
    let n354: ZB = zn_le(zn_splat(P8::from_raw(65536i32)), n341);
    let n355: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n341);
    let n356: ZB = zb_and(n351, n354);
    let n357: ZB = zb_and(n351, n355);
    let n358: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n353);
    let n359: ZN = zn_add(n339, n358);
    let n360: ZB = zn_tile_flag_at(g.cache, g.cart, n359, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n361: ZB = zb_not(n360);
    let n362: ZB = zb_and(n356, n361);
    let n363: ZB = zb_and(n356, n360);
    let n364: ZB = zb_or(n362, n363);
    let n365: ZB = zb_and(n361, n364);
    let n366: ZB = zb_and(n360, n364);
    let n367: ZB = zb_or(n365, n366);
    let n368: ZB = zb_and(n361, n367);
    let n369: ZB = zb_and(n360, n367);
    let n370: ZN = zn_add(n339, n353);
    let n371: ZB = zn_le(zn_splat(P8::from_raw(131072i32)), n341);
    let n372: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n341);
    let n373: ZB = zb_and(n368, n371);
    let n374: ZB = zb_and(n368, n372);
    let n375: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n370);
    let n376: ZN = zn_add(n339, n375);
    let n377: ZB = zn_tile_flag_at(g.cache, g.cart, n376, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n378: ZB = zb_not(n377);
    let n379: ZB = zb_and(n373, n378);
    let n380: ZB = zb_and(n373, n377);
    let n381: ZB = zb_or(n379, n380);
    let n382: ZB = zb_and(n378, n381);
    let n383: ZB = zb_and(n377, n381);
    let n384: ZB = zb_or(n382, n383);
    let n385: ZB = zb_and(n378, n384);
    let n386: ZB = zb_and(n377, n384);
    let n387: ZN = zn_add(n339, n370);
    let n388: ZB = zn_le(zn_splat(P8::from_raw(196608i32)), n341);
    let n389: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n341);
    let n390: ZB = zb_and(n385, n388);
    let n391: ZB = zb_and(n385, n389);
    let n392: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n387);
    let n393: ZN = zn_add(n339, n392);
    let n394: ZB = zn_tile_flag_at(g.cache, g.cart, n393, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n395: ZB = zb_not(n394);
    let n396: ZB = zb_and(n390, n395);
    let n397: ZB = zb_and(n390, n394);
    let n398: ZB = zb_or(n396, n397);
    let n399: ZB = zb_and(n395, n398);
    let n400: ZB = zb_and(n394, n398);
    let n401: ZB = zb_or(n399, n400);
    let n402: ZB = zb_and(n395, n401);
    let n403: ZB = zb_and(n394, n401);
    let n404: ZN = zn_add(n339, n387);
    let n405: ZB = zn_le(zn_splat(P8::from_raw(262144i32)), n341);
    let n406: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n341);
    let n407: ZB = zb_and(n402, n405);
    let n408: ZB = zb_and(n402, n406);
    let n409: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n404);
    let n410: ZN = zn_add(n339, n409);
    let n411: ZB = zn_tile_flag_at(g.cache, g.cart, n410, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n412: ZB = zb_not(n411);
    let n413: ZB = zb_and(n407, n412);
    let n414: ZB = zb_and(n407, n411);
    let n415: ZB = zb_or(n413, n414);
    let n416: ZB = zb_and(n412, n415);
    let n417: ZB = zb_and(n411, n415);
    let n418: ZB = zb_or(n416, n417);
    let n419: ZB = zb_and(n412, n418);
    let n420: ZB = zb_and(n411, n418);
    let n421: ZN = zn_add(n339, n404);
    let n422: ZB = zn_le(zn_splat(P8::from_raw(327680i32)), n341);
    let n423: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n341);
    let n424: ZB = zb_and(n419, n422);
    let n425: ZB = zb_and(n419, n423);
    let n426: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n421);
    let n427: ZN = zn_add(n339, n426);
    let n428: ZB = zn_tile_flag_at(g.cache, g.cart, n427, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n429: ZB = zb_not(n428);
    let n430: ZB = zb_and(n424, n429);
    let n431: ZB = zb_and(n424, n428);
    let n432: ZB = zb_or(n430, n431);
    let n433: ZB = zb_and(n429, n432);
    let n434: ZB = zb_and(n428, n432);
    let n435: ZB = zb_or(n433, n434);
    let n436: ZB = zb_and(n429, n435);
    let n437: ZB = zb_and(n428, n435);
    let n438: ZN = zn_add(n339, n421);
    let n439: ZB = zn_le(zn_splat(P8::from_raw(393216i32)), n341);
    let n440: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n341);
    let n441: ZB = zb_and(n436, n439);
    let n442: ZB = zb_and(n436, n440);
    let n443: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n438);
    let n444: ZN = zn_add(n339, n443);
    let n445: ZB = zn_tile_flag_at(g.cache, g.cart, n444, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n446: ZB = zb_not(n445);
    let n447: ZB = zb_and(n441, n446);
    let n448: ZB = zb_and(n441, n445);
    let n449: ZB = zb_or(n447, n448);
    let n450: ZB = zb_and(n446, n449);
    let n451: ZB = zb_and(n445, n449);
    let n452: ZB = zb_or(n450, n451);
    let n453: ZB = zb_and(n446, n452);
    let n454: ZB = zb_and(n445, n452);
    let n455: ZN = zn_add(n339, n438);
    let n456: ZB = zn_le(zn_splat(P8::from_raw(458752i32)), n341);
    let n457: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n341);
    let n458: ZB = zb_and(n453, n456);
    let n459: ZB = zb_and(n453, n457);
    let n460: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n455);
    let n461: ZN = zn_add(n339, n460);
    let n462: ZB = zn_tile_flag_at(g.cache, g.cart, n461, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n463: ZB = zb_not(n462);
    let n464: ZB = zb_and(n458, n463);
    let n465: ZB = zb_and(n458, n462);
    let n466: ZB = zb_or(n464, n465);
    let n467: ZB = zb_and(n463, n466);
    let n468: ZB = zb_and(n462, n466);
    let n469: ZB = zb_or(n467, n468);
    let n470: ZB = zb_and(n463, n469);
    let n471: ZB = zb_and(n462, n469);
    let n472: ZN = zn_add(n339, n455);
    let n473: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n341);
    let n474: ZN = zsel_n(n462, n455, n472);
    let n475: ZN = zsel_n(n462, zn_splat(P8::from_raw(0i32)), n328);
    let n476: ZN = zsel_n(n462, zn_splat(P8::from_raw(0i32)), n318);
    let n477: ZB = zb_or(n470, n471);
    let n478: ZB = zb_or(n462, n473);
    let n479: ZN = zsel_n(n457, n455, n474);
    let n480: ZN = zsel_n(n457, n328, n475);
    let n481: ZN = zsel_n(n457, n318, n476);
    let n482: ZB = zb_or(n459, n477);
    let n483: ZB = zb_or(n457, n478);
    let n484: ZN = zsel_n(n445, n438, n479);
    let n485: ZN = zsel_n(n445, zn_splat(P8::from_raw(0i32)), n480);
    let n486: ZN = zsel_n(n445, zn_splat(P8::from_raw(0i32)), n481);
    let n487: ZB = zb_or(n454, n482);
    let n488: ZB = zb_or(n445, n483);
    let n489: ZN = zsel_n(n440, n438, n484);
    let n490: ZN = zsel_n(n440, n328, n485);
    let n491: ZN = zsel_n(n440, n318, n486);
    let n492: ZB = zb_or(n442, n487);
    let n493: ZB = zb_or(n440, n488);
    let n494: ZN = zsel_n(n428, n421, n489);
    let n495: ZN = zsel_n(n428, zn_splat(P8::from_raw(0i32)), n490);
    let n496: ZN = zsel_n(n428, zn_splat(P8::from_raw(0i32)), n491);
    let n497: ZB = zb_or(n437, n492);
    let n498: ZB = zb_or(n428, n493);
    let n499: ZN = zsel_n(n423, n421, n494);
    let n500: ZN = zsel_n(n423, n328, n495);
    let n501: ZN = zsel_n(n423, n318, n496);
    let n502: ZB = zb_or(n425, n497);
    let n503: ZB = zb_or(n423, n498);
    let n504: ZN = zsel_n(n411, n404, n499);
    let n505: ZN = zsel_n(n411, zn_splat(P8::from_raw(0i32)), n500);
    let n506: ZN = zsel_n(n411, zn_splat(P8::from_raw(0i32)), n501);
    let n507: ZB = zb_or(n420, n502);
    let n508: ZB = zb_or(n411, n503);
    let n509: ZN = zsel_n(n406, n404, n504);
    let n510: ZN = zsel_n(n406, n328, n505);
    let n511: ZN = zsel_n(n406, n318, n506);
    let n512: ZB = zb_or(n408, n507);
    let n513: ZB = zb_or(n406, n508);
    let n514: ZN = zsel_n(n394, n387, n509);
    let n515: ZN = zsel_n(n394, zn_splat(P8::from_raw(0i32)), n510);
    let n516: ZN = zsel_n(n394, zn_splat(P8::from_raw(0i32)), n511);
    let n517: ZB = zb_or(n403, n512);
    let n518: ZB = zb_or(n394, n513);
    let n519: ZN = zsel_n(n389, n387, n514);
    let n520: ZN = zsel_n(n389, n328, n515);
    let n521: ZN = zsel_n(n389, n318, n516);
    let n522: ZB = zb_or(n391, n517);
    let n523: ZB = zb_or(n389, n518);
    let n524: ZN = zsel_n(n377, n370, n519);
    let n525: ZN = zsel_n(n377, zn_splat(P8::from_raw(0i32)), n520);
    let n526: ZN = zsel_n(n377, zn_splat(P8::from_raw(0i32)), n521);
    let n527: ZB = zb_or(n386, n522);
    let n528: ZB = zb_or(n377, n523);
    let n529: ZN = zsel_n(n372, n370, n524);
    let n530: ZN = zsel_n(n372, n328, n525);
    let n531: ZN = zsel_n(n372, n318, n526);
    let n532: ZB = zb_or(n374, n527);
    let n533: ZB = zb_or(n372, n528);
    let n534: ZN = zsel_n(n360, n353, n529);
    let n535: ZN = zsel_n(n360, zn_splat(P8::from_raw(0i32)), n530);
    let n536: ZN = zsel_n(n360, zn_splat(P8::from_raw(0i32)), n531);
    let n537: ZB = zb_or(n369, n532);
    let n538: ZB = zb_or(n360, n533);
    let n539: ZN = zsel_n(n355, n353, n534);
    let n540: ZN = zsel_n(n355, n328, n535);
    let n541: ZN = zsel_n(n355, n318, n536);
    let n542: ZB = zb_or(n357, n537);
    let n543: ZB = zb_or(n355, n538);
    let n544: ZN = zsel_n(n343, r_c300, n539);
    let n545: ZN = zsel_n(n343, zn_splat(P8::from_raw(0i32)), n540);
    let n546: ZN = zsel_n(n343, zn_splat(P8::from_raw(0i32)), n541);
    let n547: ZB = zb_or(n352, n542);
    let n548: ZB = zb_or(n343, n543);
    let n549: ZB = zb_and(n239, n547);
    let n550: ZB = zb_and(n240, n547);
    let n551: ZB = zb_and(n241, n550);
    let n552: ZB = zb_and(n242, n550);
    let n553: ZB = zb_or(n551, n552);
    let n554: ZB = zb_or(n549, n553);
    let n555: ZB = zb_and(n246, n554);
    let n556: ZB = zb_and(n247, n554);
    let n557: ZB = zb_or(n555, n556);
    let n558: ZB = zb_and(n246, n557);
    let n559: ZB = zb_and(n247, n557);
    let n560: ZB = zb_or(n558, n559);
    let n561: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n544);
    let n562: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n561);
    let n563: ZB = zn_tile_flag_at(g.cache, g.cart, n562, n248, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n564: ZB = zb_not(n563);
    let n565: ZB = zb_and(n560, n564);
    let n566: ZB = zb_and(n560, n563);
    let n567: ZB = zb_or(n565, n566);
    let n568: ZB = zb_and(n564, n567);
    let n569: ZB = zb_and(n563, n567);
    let n570: ZB = zb_or(n568, n569);
    let n571: ZB = zb_and(n564, n570);
    let n572: ZB = zb_and(n563, n570);
    let n573: ZB = zb_and(n250, n571);
    let n574: ZB = zb_and(n251, n571);
    let n575: ZB = zb_and(n246, n573);
    let n576: ZB = zb_and(n247, n573);
    let n577: ZB = zb_or(n575, n576);
    let n578: ZB = zb_and(n246, n577);
    let n579: ZB = zb_and(n247, n577);
    let n580: ZB = zb_or(n578, n579);
    let n581: ZB = zn_tile_flag_at(g.cache, g.cart, n562, n253, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n582: ZB = zb_not(n581);
    let n583: ZB = zb_and(n580, n582);
    let n584: ZB = zb_and(n580, n581);
    let n585: ZB = zb_or(n583, n584);
    let n586: ZB = zb_and(n582, n585);
    let n587: ZB = zb_and(n581, n585);
    let n588: ZB = zb_or(n586, n587);
    let n589: ZB = zb_and(n582, n588);
    let n590: ZB = zb_and(n581, n588);
    let n591: ZB = zb_and(n255, n589);
    let n592: ZB = zb_and(n256, n589);
    let n593: ZB = zb_and(n246, n591);
    let n594: ZB = zb_and(n247, n591);
    let n595: ZB = zb_or(n593, n594);
    let n596: ZB = zb_and(n246, n595);
    let n597: ZB = zb_and(n247, n595);
    let n598: ZB = zb_or(n596, n597);
    let n599: ZB = zn_tile_flag_at(g.cache, g.cart, n562, n258, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n600: ZB = zb_not(n599);
    let n601: ZB = zb_and(n598, n600);
    let n602: ZB = zb_and(n598, n599);
    let n603: ZB = zb_or(n601, n602);
    let n604: ZB = zb_and(n600, n603);
    let n605: ZB = zb_and(n599, n603);
    let n606: ZB = zb_or(n604, n605);
    let n607: ZB = zb_and(n600, n606);
    let n608: ZB = zb_and(n599, n606);
    let n609: ZB = zb_and(n260, n607);
    let n610: ZB = zb_and(n261, n607);
    let n611: ZB = zb_and(n246, n609);
    let n612: ZB = zb_and(n247, n609);
    let n613: ZB = zb_or(n611, n612);
    let n614: ZB = zb_and(n246, n613);
    let n615: ZB = zb_and(n247, n613);
    let n616: ZB = zb_or(n614, n615);
    let n617: ZB = zn_tile_flag_at(g.cache, g.cart, n562, n263, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n618: ZB = zb_not(n617);
    let n619: ZB = zb_and(n616, n618);
    let n620: ZB = zb_and(n616, n617);
    let n621: ZB = zb_or(n619, n620);
    let n622: ZB = zb_and(n618, n621);
    let n623: ZB = zb_and(n617, n621);
    let n624: ZB = zb_or(n622, n623);
    let n625: ZB = zb_and(n618, n624);
    let n626: ZB = zb_and(n617, n624);
    let n627: ZB = zb_and(n265, n625);
    let n628: ZB = zb_and(n266, n625);
    let n629: ZB = zb_and(n246, n627);
    let n630: ZB = zb_and(n247, n627);
    let n631: ZB = zb_or(n629, n630);
    let n632: ZB = zb_and(n246, n631);
    let n633: ZB = zb_and(n247, n631);
    let n634: ZB = zb_or(n632, n633);
    let n635: ZB = zn_tile_flag_at(g.cache, g.cart, n562, n268, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n636: ZB = zb_not(n635);
    let n637: ZB = zb_and(n634, n636);
    let n638: ZB = zb_and(n634, n635);
    let n639: ZB = zb_or(n637, n638);
    let n640: ZB = zb_and(n636, n639);
    let n641: ZB = zb_and(n635, n639);
    let n642: ZB = zb_or(n640, n641);
    let n643: ZB = zb_and(n636, n642);
    let n644: ZB = zb_and(n635, n642);
    let n645: ZB = zb_and(n270, n643);
    let n646: ZB = zb_and(n271, n643);
    let n647: ZB = zb_and(n246, n645);
    let n648: ZB = zb_and(n247, n645);
    let n649: ZB = zb_or(n647, n648);
    let n650: ZB = zb_and(n246, n649);
    let n651: ZB = zb_and(n247, n649);
    let n652: ZB = zb_or(n650, n651);
    let n653: ZB = zn_tile_flag_at(g.cache, g.cart, n562, n273, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n654: ZB = zb_not(n653);
    let n655: ZB = zb_and(n652, n654);
    let n656: ZB = zb_and(n652, n653);
    let n657: ZB = zb_or(n655, n656);
    let n658: ZB = zb_and(n654, n657);
    let n659: ZB = zb_and(n653, n657);
    let n660: ZB = zb_or(n658, n659);
    let n661: ZB = zb_and(n654, n660);
    let n662: ZB = zb_and(n653, n660);
    let n663: ZB = zb_and(n275, n661);
    let n664: ZB = zb_and(n276, n661);
    let n665: ZB = zb_and(n246, n663);
    let n666: ZB = zb_and(n247, n663);
    let n667: ZB = zb_or(n665, n666);
    let n668: ZB = zb_and(n246, n667);
    let n669: ZB = zb_and(n247, n667);
    let n670: ZB = zb_or(n668, n669);
    let n671: ZB = zn_tile_flag_at(g.cache, g.cart, n562, n278, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n672: ZB = zb_not(n671);
    let n673: ZB = zb_and(n670, n672);
    let n674: ZB = zb_and(n670, n671);
    let n675: ZB = zb_or(n673, n674);
    let n676: ZB = zb_and(n672, n675);
    let n677: ZB = zb_and(n671, n675);
    let n678: ZB = zb_or(n676, n677);
    let n679: ZB = zb_and(n672, n678);
    let n680: ZB = zb_and(n671, n678);
    let n681: ZB = zb_and(n280, n679);
    let n682: ZB = zb_and(n281, n679);
    let n683: ZB = zb_and(n246, n681);
    let n684: ZB = zb_and(n247, n681);
    let n685: ZB = zb_or(n683, n684);
    let n686: ZB = zb_and(n246, n685);
    let n687: ZB = zb_and(n247, n685);
    let n688: ZB = zb_or(n686, n687);
    let n689: ZB = zn_tile_flag_at(g.cache, g.cart, n562, n283, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n690: ZB = zb_not(n689);
    let n691: ZB = zb_and(n688, n690);
    let n692: ZB = zb_and(n688, n689);
    let n693: ZB = zb_or(n691, n692);
    let n694: ZB = zb_and(n690, n693);
    let n695: ZB = zb_and(n689, n693);
    let n696: ZB = zb_or(n694, n695);
    let n697: ZB = zb_and(n690, n696);
    let n698: ZB = zb_and(n689, n696);
    let n699: ZB = zb_and(n285, n548);
    let n700: ZN = zsel_n(n689, n279, n284);
    let n701: ZN = zsel_n(n689, zn_splat(P8::from_raw(0i32)), n238);
    let n702: ZN = zsel_n(n689, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(-196608i32)));
    let n703: ZB = zb_or(n697, n698);
    let n704: ZB = zsel_b(n689, n548, n699);
    let n705: ZN = zsel_n(n281, n279, n700);
    let n706: ZN = zsel_n(n281, n238, n701);
    let n707: ZN = zsel_n(n281, zn_splat(P8::from_raw(-196608i32)), n702);
    let n708: ZB = zb_or(n682, n703);
    let n709: ZB = zsel_b(n281, n548, n704);
    let n710: ZN = zsel_n(n671, n274, n705);
    let n711: ZN = zsel_n(n671, zn_splat(P8::from_raw(0i32)), n706);
    let n712: ZN = zsel_n(n671, zn_splat(P8::from_raw(0i32)), n707);
    let n713: ZB = zb_or(n680, n708);
    let n714: ZB = zsel_b(n671, n548, n709);
    let n715: ZN = zsel_n(n276, n274, n710);
    let n716: ZN = zsel_n(n276, n238, n711);
    let n717: ZN = zsel_n(n276, zn_splat(P8::from_raw(-196608i32)), n712);
    let n718: ZB = zb_or(n664, n713);
    let n719: ZB = zsel_b(n276, n548, n714);
    let n720: ZN = zsel_n(n653, n269, n715);
    let n721: ZN = zsel_n(n653, zn_splat(P8::from_raw(0i32)), n716);
    let n722: ZN = zsel_n(n653, zn_splat(P8::from_raw(0i32)), n717);
    let n723: ZB = zb_or(n662, n718);
    let n724: ZB = zsel_b(n653, n548, n719);
    let n725: ZN = zsel_n(n271, n269, n720);
    let n726: ZN = zsel_n(n271, n238, n721);
    let n727: ZN = zsel_n(n271, zn_splat(P8::from_raw(-196608i32)), n722);
    let n728: ZB = zb_or(n646, n723);
    let n729: ZB = zsel_b(n271, n548, n724);
    let n730: ZN = zsel_n(n635, n264, n725);
    let n731: ZN = zsel_n(n635, zn_splat(P8::from_raw(0i32)), n726);
    let n732: ZN = zsel_n(n635, zn_splat(P8::from_raw(0i32)), n727);
    let n733: ZB = zb_or(n644, n728);
    let n734: ZB = zsel_b(n635, n548, n729);
    let n735: ZN = zsel_n(n266, n264, n730);
    let n736: ZN = zsel_n(n266, n238, n731);
    let n737: ZN = zsel_n(n266, zn_splat(P8::from_raw(-196608i32)), n732);
    let n738: ZB = zb_or(n628, n733);
    let n739: ZB = zsel_b(n266, n548, n734);
    let n740: ZN = zsel_n(n617, n259, n735);
    let n741: ZN = zsel_n(n617, zn_splat(P8::from_raw(0i32)), n736);
    let n742: ZN = zsel_n(n617, zn_splat(P8::from_raw(0i32)), n737);
    let n743: ZB = zb_or(n626, n738);
    let n744: ZB = zsel_b(n617, n548, n739);
    let n745: ZN = zsel_n(n261, n259, n740);
    let n746: ZN = zsel_n(n261, n238, n741);
    let n747: ZN = zsel_n(n261, zn_splat(P8::from_raw(-196608i32)), n742);
    let n748: ZB = zb_or(n610, n743);
    let n749: ZB = zsel_b(n261, n548, n744);
    let n750: ZN = zsel_n(n599, n254, n745);
    let n751: ZN = zsel_n(n599, zn_splat(P8::from_raw(0i32)), n746);
    let n752: ZN = zsel_n(n599, zn_splat(P8::from_raw(0i32)), n747);
    let n753: ZB = zb_or(n608, n748);
    let n754: ZB = zsel_b(n599, n548, n749);
    let n755: ZN = zsel_n(n256, n254, n750);
    let n756: ZN = zsel_n(n256, n238, n751);
    let n757: ZN = zsel_n(n256, zn_splat(P8::from_raw(-196608i32)), n752);
    let n758: ZB = zb_or(n592, n753);
    let n759: ZB = zsel_b(n256, n548, n754);
    let n760: ZN = zsel_n(n581, n249, n755);
    let n761: ZN = zsel_n(n581, zn_splat(P8::from_raw(0i32)), n756);
    let n762: ZN = zsel_n(n581, zn_splat(P8::from_raw(0i32)), n757);
    let n763: ZB = zb_or(n590, n758);
    let n764: ZB = zsel_b(n581, n548, n759);
    let n765: ZN = zsel_n(n251, n249, n760);
    let n766: ZN = zsel_n(n251, n238, n761);
    let n767: ZN = zsel_n(n251, zn_splat(P8::from_raw(-196608i32)), n762);
    let n768: ZB = zb_or(n574, n763);
    let n769: ZB = zsel_b(n251, n548, n764);
    let n770: ZN = zsel_n(n563, zn_splat(P8::from_raw(7077888i32)), n765);
    let n771: ZN = zsel_n(n563, zn_splat(P8::from_raw(0i32)), n766);
    let n772: ZN = zsel_n(n563, zn_splat(P8::from_raw(0i32)), n767);
    let n773: ZB = zb_or(n572, n768);
    let n774: ZB = zsel_b(n563, n548, n769);
    let n775: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n770);
    let n776: ZN = zn_div(n561, zn_splat(P8::from_raw(524288i32)));
    let n777: ZN = zn_flr(n776);
    let n778: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n777);
    let n779: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n561);
    let n780: ZN = zn_sub(n779, zn_splat(P8::from_raw(65536i32)));
    let n781: ZN = zn_div(n780, zn_splat(P8::from_raw(524288i32)));
    let n782: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n781);
    let n783: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n778);
    let n784: ZB = zn_le(n783, n782);
    let n785: ZB = zn_gt(n783, n782);
    let n786: ZB = zb_and(n773, n784);
    let n787: ZB = zb_and(n773, n785);
    let n788: ZN = zn_div(n775, zn_splat(P8::from_raw(524288i32)));
    let n789: ZN = zn_flr(n788);
    let n790: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n789);
    let n791: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n775);
    let n792: ZN = zn_sub(n791, zn_splat(P8::from_raw(65536i32)));
    let n793: ZN = zn_div(n792, zn_splat(P8::from_raw(524288i32)));
    let n794: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n793);
    let n795: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n790);
    let n796: ZB = zn_le(n795, n794);
    let n797: ZB = zn_gt(n795, n794);
    let n798: ZB = zb_and(n786, n796);
    let n799: ZB = zb_and(n786, n797);
    let n800: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n783);
    let n801: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n795);
    let n802: ZN = zn_mget(g.cart, n800, n801);
    let n803: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n802);
    let n804: ZB = zb_not(n803);
    let n805: ZB = zb_and(n798, n803);
    let n806: ZB = zb_and(n798, n804);
    let n807: ZN = zn_rem(n792, zn_splat(P8::from_raw(524288i32)));
    let n808: ZB = zn_ge(n807, zn_splat(P8::from_raw(393216i32)));
    let n809: ZB = zn_lt(n807, zn_splat(P8::from_raw(393216i32)));
    let n810: ZB = zb_and(n805, n809);
    let n811: ZB = zb_and(n805, n808);
    let n812: ZN = zn_mul(n795, zn_splat(P8::from_raw(524288i32)));
    let n813: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n812);
    let n814: ZB = zn_eq(n791, n813);
    let n815: ZB = zb_or(n810, n811);
    let n816: ZB = zb_or(n808, n814);
    let n817: ZB = zb_or(n806, n815);
    let n818: ZB = zb_and(n803, n816);
    let n819: ZB = zb_not(n818);
    let n820: ZB = zb_and(n817, n818);
    let n821: ZB = zb_and(n817, n819);
    let n822: ZB = zn_ge(n772, zn_splat(P8::from_raw(0i32)));
    let n823: ZB = zb_or(n820, n821);
    let n824: ZB = zb_and(n818, n822);
    let n825: ZB = zb_not(n824);
    let n826: ZB = zb_and(n823, n825);
    let n827: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n802);
    let n828: ZB = zb_not(n827);
    let n829: ZB = zb_and(n826, n827);
    let n830: ZB = zb_and(n826, n828);
    let n831: ZN = zn_rem(n775, zn_splat(P8::from_raw(524288i32)));
    let n832: ZB = zn_le(n831, zn_splat(P8::from_raw(131072i32)));
    let n833: ZB = zb_or(n829, n830);
    let n834: ZB = zb_and(n827, n832);
    let n835: ZB = zb_not(n834);
    let n836: ZB = zb_and(n833, n834);
    let n837: ZB = zb_and(n833, n835);
    let n838: ZB = zb_or(n836, n837);
    let n839: ZB = zb_and(n835, n838);
    let n840: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n802);
    let n841: ZB = zb_not(n840);
    let n842: ZB = zb_and(n839, n840);
    let n843: ZB = zb_and(n839, n841);
    let n844: ZN = zn_rem(n561, zn_splat(P8::from_raw(524288i32)));
    let n845: ZB = zn_le(n844, zn_splat(P8::from_raw(131072i32)));
    let n846: ZB = zb_or(n842, n843);
    let n847: ZB = zb_and(n840, n845);
    let n848: ZB = zb_not(n847);
    let n849: ZB = zb_and(n846, n847);
    let n850: ZB = zb_and(n846, n848);
    let n851: ZB = zn_le(n546, zn_splat(P8::from_raw(0i32)));
    let n852: ZB = zb_or(n849, n850);
    let n853: ZB = zb_and(n847, n851);
    let n854: ZB = zb_not(n853);
    let n855: ZB = zb_and(n852, n854);
    let n856: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n802);
    let n857: ZB = zb_not(n856);
    let n858: ZB = zb_and(n855, n856);
    let n859: ZB = zb_and(n855, n857);
    let n860: ZN = zn_rem(n780, zn_splat(P8::from_raw(524288i32)));
    let n861: ZB = zn_ge(n860, zn_splat(P8::from_raw(393216i32)));
    let n862: ZB = zn_lt(n860, zn_splat(P8::from_raw(393216i32)));
    let n863: ZB = zb_and(n858, n862);
    let n864: ZB = zb_and(n858, n861);
    let n865: ZN = zn_mul(n783, zn_splat(P8::from_raw(524288i32)));
    let n866: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n865);
    let n867: ZB = zn_eq(n779, n866);
    let n868: ZB = zb_or(n863, n864);
    let n869: ZB = zb_or(n861, n867);
    let n870: ZB = zb_or(n859, n868);
    let n871: ZB = zb_and(n856, n869);
    let n872: ZB = zb_not(n871);
    let n873: ZB = zb_and(n870, n871);
    let n874: ZB = zb_and(n870, n872);
    let n875: ZB = zn_ge(n546, zn_splat(P8::from_raw(0i32)));
    let n876: ZB = zb_or(n873, n874);
    let n877: ZB = zb_and(n871, n875);
    let n878: ZB = zb_not(n877);
    let n879: ZB = zb_and(n876, n878);
    let n880: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n790);
    let n881: ZB = zn_le(n880, n794);
    let n882: ZB = zn_gt(n880, n794);
    let n883: ZB = zb_and(n879, n881);
    let n884: ZB = zb_and(n879, n882);
    let n885: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n880);
    let n886: ZN = zn_mget(g.cart, n800, n885);
    let n887: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n886);
    let n888: ZB = zb_not(n887);
    let n889: ZB = zb_and(n883, n887);
    let n890: ZB = zb_and(n883, n888);
    let n891: ZB = zb_and(n809, n889);
    let n892: ZB = zb_and(n808, n889);
    let n893: ZN = zn_mul(n880, zn_splat(P8::from_raw(524288i32)));
    let n894: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n893);
    let n895: ZB = zn_eq(n791, n894);
    let n896: ZB = zb_or(n891, n892);
    let n897: ZB = zb_or(n808, n895);
    let n898: ZB = zb_or(n890, n896);
    let n899: ZB = zb_and(n887, n897);
    let n900: ZB = zb_not(n899);
    let n901: ZB = zb_and(n898, n899);
    let n902: ZB = zb_and(n898, n900);
    let n903: ZB = zb_or(n901, n902);
    let n904: ZB = zb_and(n822, n899);
    let n905: ZB = zb_not(n904);
    let n906: ZB = zb_and(n903, n905);
    let n907: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n886);
    let n908: ZB = zb_not(n907);
    let n909: ZB = zb_and(n906, n907);
    let n910: ZB = zb_and(n906, n908);
    let n911: ZB = zb_or(n909, n910);
    let n912: ZB = zb_and(n832, n907);
    let n913: ZB = zb_not(n912);
    let n914: ZB = zb_and(n911, n912);
    let n915: ZB = zb_and(n911, n913);
    let n916: ZB = zb_or(n914, n915);
    let n917: ZB = zb_and(n913, n916);
    let n918: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n886);
    let n919: ZB = zb_not(n918);
    let n920: ZB = zb_and(n917, n918);
    let n921: ZB = zb_and(n917, n919);
    let n922: ZB = zb_or(n920, n921);
    let n923: ZB = zb_and(n845, n918);
    let n924: ZB = zb_not(n923);
    let n925: ZB = zb_and(n922, n923);
    let n926: ZB = zb_and(n922, n924);
    let n927: ZB = zb_or(n925, n926);
    let n928: ZB = zb_and(n851, n923);
    let n929: ZB = zb_not(n928);
    let n930: ZB = zb_and(n927, n929);
    let n931: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n886);
    let n932: ZB = zb_not(n931);
    let n933: ZB = zb_and(n930, n931);
    let n934: ZB = zb_and(n930, n932);
    let n935: ZB = zb_and(n862, n933);
    let n936: ZB = zb_and(n861, n933);
    let n937: ZB = zb_or(n935, n936);
    let n938: ZB = zb_or(n934, n937);
    let n939: ZB = zb_and(n869, n931);
    let n940: ZB = zb_not(n939);
    let n941: ZB = zb_and(n938, n939);
    let n942: ZB = zb_and(n938, n940);
    let n943: ZB = zb_or(n941, n942);
    let n944: ZB = zb_and(n875, n939);
    let n945: ZB = zb_not(n944);
    let n946: ZB = zb_and(n943, n945);
    let n947: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n790);
    let n948: ZB = zn_le(n947, n794);
    let n949: ZB = zn_gt(n947, n794);
    let n950: ZB = zb_and(n946, n948);
    let n951: ZB = zb_and(n946, n949);
    let n952: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n947);
    let n953: ZN = zn_mget(g.cart, n800, n952);
    let n954: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n953);
    let n955: ZB = zb_not(n954);
    let n956: ZB = zb_and(n950, n954);
    let n957: ZB = zb_and(n950, n955);
    let n958: ZB = zb_and(n809, n956);
    let n959: ZB = zb_and(n808, n956);
    let n960: ZN = zn_mul(n947, zn_splat(P8::from_raw(524288i32)));
    let n961: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n960);
    let n962: ZB = zn_eq(n791, n961);
    let n963: ZB = zb_or(n958, n959);
    let n964: ZB = zb_or(n808, n962);
    let n965: ZB = zb_or(n957, n963);
    let n966: ZB = zb_and(n954, n964);
    let n967: ZB = zb_not(n966);
    let n968: ZB = zb_and(n965, n966);
    let n969: ZB = zb_and(n965, n967);
    let n970: ZB = zb_or(n968, n969);
    let n971: ZB = zb_and(n822, n966);
    let n972: ZB = zb_not(n971);
    let n973: ZB = zb_and(n970, n972);
    let n974: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n953);
    let n975: ZB = zb_not(n974);
    let n976: ZB = zb_and(n973, n974);
    let n977: ZB = zb_and(n973, n975);
    let n978: ZB = zb_or(n976, n977);
    let n979: ZB = zb_and(n832, n974);
    let n980: ZB = zb_not(n979);
    let n981: ZB = zb_and(n978, n979);
    let n982: ZB = zb_and(n978, n980);
    let n983: ZB = zb_or(n981, n982);
    let n984: ZB = zb_and(n980, n983);
    let n985: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n953);
    let n986: ZB = zb_not(n985);
    let n987: ZB = zb_and(n984, n985);
    let n988: ZB = zb_and(n984, n986);
    let n989: ZB = zb_or(n987, n988);
    let n990: ZB = zb_and(n845, n985);
    let n991: ZB = zb_not(n990);
    let n992: ZB = zb_and(n989, n990);
    let n993: ZB = zb_and(n989, n991);
    let n994: ZB = zb_or(n992, n993);
    let n995: ZB = zb_and(n851, n990);
    let n996: ZB = zb_not(n995);
    let n997: ZB = zb_and(n994, n996);
    let n998: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n953);
    let n999: ZB = zb_not(n998);
    let n1000: ZB = zb_and(n997, n998);
    let n1001: ZB = zb_and(n997, n999);
    let n1002: ZB = zb_and(n862, n1000);
    let n1003: ZB = zb_and(n861, n1000);
    let n1004: ZB = zb_or(n1002, n1003);
    let n1005: ZB = zb_or(n1001, n1004);
    let n1006: ZB = zb_and(n869, n998);
    let n1007: ZB = zb_not(n1006);
    let n1008: ZB = zb_and(n1005, n1006);
    let n1009: ZB = zb_and(n1005, n1007);
    let n1010: ZB = zb_or(n1008, n1009);
    let n1011: ZB = zb_and(n875, n1006);
    let n1012: ZB = zb_not(n1011);
    let n1013: ZB = zb_and(n1010, n1012);
    let n1014: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n790);
    let n1015: ZB = zn_gt(n1014, n794);
    let n1016: ZB = zb_and(n774, n1015);
    let n1017: ZB = zb_or(n951, n1013);
    let n1018: ZB = zsel_b(n949, n774, n1016);
    let n1019: ZB = zb_or(n884, n1017);
    let n1020: ZB = zsel_b(n882, n774, n1018);
    let n1021: ZB = zb_or(n799, n1019);
    let n1022: ZB = zsel_b(n797, n774, n1020);
    let n1023: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n778);
    let n1024: ZB = zn_le(n1023, n782);
    let n1025: ZB = zn_gt(n1023, n782);
    let n1026: ZB = zb_and(n1021, n1024);
    let n1027: ZB = zb_and(n1021, n1025);
    let n1028: ZB = zb_and(n796, n1026);
    let n1029: ZB = zb_and(n797, n1026);
    let n1030: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n1023);
    let n1031: ZN = zn_mget(g.cart, n1030, n801);
    let n1032: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1031);
    let n1033: ZB = zb_not(n1032);
    let n1034: ZB = zb_and(n1028, n1032);
    let n1035: ZB = zb_and(n1028, n1033);
    let n1036: ZB = zb_and(n809, n1034);
    let n1037: ZB = zb_and(n808, n1034);
    let n1038: ZB = zb_or(n1036, n1037);
    let n1039: ZB = zb_or(n1035, n1038);
    let n1040: ZB = zb_and(n816, n1032);
    let n1041: ZB = zb_not(n1040);
    let n1042: ZB = zb_and(n1039, n1040);
    let n1043: ZB = zb_and(n1039, n1041);
    let n1044: ZB = zb_or(n1042, n1043);
    let n1045: ZB = zb_and(n822, n1040);
    let n1046: ZB = zb_not(n1045);
    let n1047: ZB = zb_and(n1044, n1046);
    let n1048: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1031);
    let n1049: ZB = zb_not(n1048);
    let n1050: ZB = zb_and(n1047, n1048);
    let n1051: ZB = zb_and(n1047, n1049);
    let n1052: ZB = zb_or(n1050, n1051);
    let n1053: ZB = zb_and(n832, n1048);
    let n1054: ZB = zb_not(n1053);
    let n1055: ZB = zb_and(n1052, n1053);
    let n1056: ZB = zb_and(n1052, n1054);
    let n1057: ZB = zb_or(n1055, n1056);
    let n1058: ZB = zb_and(n1054, n1057);
    let n1059: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1031);
    let n1060: ZB = zb_not(n1059);
    let n1061: ZB = zb_and(n1058, n1059);
    let n1062: ZB = zb_and(n1058, n1060);
    let n1063: ZB = zb_or(n1061, n1062);
    let n1064: ZB = zb_and(n845, n1059);
    let n1065: ZB = zb_not(n1064);
    let n1066: ZB = zb_and(n1063, n1064);
    let n1067: ZB = zb_and(n1063, n1065);
    let n1068: ZB = zb_or(n1066, n1067);
    let n1069: ZB = zb_and(n851, n1064);
    let n1070: ZB = zb_not(n1069);
    let n1071: ZB = zb_and(n1068, n1070);
    let n1072: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1031);
    let n1073: ZB = zb_not(n1072);
    let n1074: ZB = zb_and(n1071, n1072);
    let n1075: ZB = zb_and(n1071, n1073);
    let n1076: ZB = zb_and(n862, n1074);
    let n1077: ZB = zb_and(n861, n1074);
    let n1078: ZN = zn_mul(n1023, zn_splat(P8::from_raw(524288i32)));
    let n1079: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1078);
    let n1080: ZB = zn_eq(n779, n1079);
    let n1081: ZB = zb_or(n1076, n1077);
    let n1082: ZB = zb_or(n861, n1080);
    let n1083: ZB = zb_or(n1075, n1081);
    let n1084: ZB = zb_and(n1072, n1082);
    let n1085: ZB = zb_not(n1084);
    let n1086: ZB = zb_and(n1083, n1084);
    let n1087: ZB = zb_and(n1083, n1085);
    let n1088: ZB = zb_or(n1086, n1087);
    let n1089: ZB = zb_and(n875, n1084);
    let n1090: ZB = zb_not(n1089);
    let n1091: ZB = zb_and(n1088, n1090);
    let n1092: ZB = zb_and(n881, n1091);
    let n1093: ZB = zb_and(n882, n1091);
    let n1094: ZN = zn_mget(g.cart, n1030, n885);
    let n1095: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1094);
    let n1096: ZB = zb_not(n1095);
    let n1097: ZB = zb_and(n1092, n1095);
    let n1098: ZB = zb_and(n1092, n1096);
    let n1099: ZB = zb_and(n809, n1097);
    let n1100: ZB = zb_and(n808, n1097);
    let n1101: ZB = zb_or(n1099, n1100);
    let n1102: ZB = zb_or(n1098, n1101);
    let n1103: ZB = zb_and(n897, n1095);
    let n1104: ZB = zb_not(n1103);
    let n1105: ZB = zb_and(n1102, n1103);
    let n1106: ZB = zb_and(n1102, n1104);
    let n1107: ZB = zb_or(n1105, n1106);
    let n1108: ZB = zb_and(n822, n1103);
    let n1109: ZB = zb_not(n1108);
    let n1110: ZB = zb_and(n1107, n1109);
    let n1111: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1094);
    let n1112: ZB = zb_not(n1111);
    let n1113: ZB = zb_and(n1110, n1111);
    let n1114: ZB = zb_and(n1110, n1112);
    let n1115: ZB = zb_or(n1113, n1114);
    let n1116: ZB = zb_and(n832, n1111);
    let n1117: ZB = zb_not(n1116);
    let n1118: ZB = zb_and(n1115, n1116);
    let n1119: ZB = zb_and(n1115, n1117);
    let n1120: ZB = zb_or(n1118, n1119);
    let n1121: ZB = zb_and(n1117, n1120);
    let n1122: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1094);
    let n1123: ZB = zb_not(n1122);
    let n1124: ZB = zb_and(n1121, n1122);
    let n1125: ZB = zb_and(n1121, n1123);
    let n1126: ZB = zb_or(n1124, n1125);
    let n1127: ZB = zb_and(n845, n1122);
    let n1128: ZB = zb_not(n1127);
    let n1129: ZB = zb_and(n1126, n1127);
    let n1130: ZB = zb_and(n1126, n1128);
    let n1131: ZB = zb_or(n1129, n1130);
    let n1132: ZB = zb_and(n851, n1127);
    let n1133: ZB = zb_not(n1132);
    let n1134: ZB = zb_and(n1131, n1133);
    let n1135: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1094);
    let n1136: ZB = zb_not(n1135);
    let n1137: ZB = zb_and(n1134, n1135);
    let n1138: ZB = zb_and(n1134, n1136);
    let n1139: ZB = zb_and(n862, n1137);
    let n1140: ZB = zb_and(n861, n1137);
    let n1141: ZB = zb_or(n1139, n1140);
    let n1142: ZB = zb_or(n1138, n1141);
    let n1143: ZB = zb_and(n1082, n1135);
    let n1144: ZB = zb_not(n1143);
    let n1145: ZB = zb_and(n1142, n1143);
    let n1146: ZB = zb_and(n1142, n1144);
    let n1147: ZB = zb_or(n1145, n1146);
    let n1148: ZB = zb_and(n875, n1143);
    let n1149: ZB = zb_not(n1148);
    let n1150: ZB = zb_and(n1147, n1149);
    let n1151: ZB = zb_and(n948, n1150);
    let n1152: ZB = zb_and(n949, n1150);
    let n1153: ZN = zn_mget(g.cart, n1030, n952);
    let n1154: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1153);
    let n1155: ZB = zb_not(n1154);
    let n1156: ZB = zb_and(n1151, n1154);
    let n1157: ZB = zb_and(n1151, n1155);
    let n1158: ZB = zb_and(n809, n1156);
    let n1159: ZB = zb_and(n808, n1156);
    let n1160: ZB = zb_or(n1158, n1159);
    let n1161: ZB = zb_or(n1157, n1160);
    let n1162: ZB = zb_and(n964, n1154);
    let n1163: ZB = zb_not(n1162);
    let n1164: ZB = zb_and(n1161, n1162);
    let n1165: ZB = zb_and(n1161, n1163);
    let n1166: ZB = zb_or(n1164, n1165);
    let n1167: ZB = zb_and(n822, n1162);
    let n1168: ZB = zb_not(n1167);
    let n1169: ZB = zb_and(n1166, n1168);
    let n1170: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1153);
    let n1171: ZB = zb_not(n1170);
    let n1172: ZB = zb_and(n1169, n1170);
    let n1173: ZB = zb_and(n1169, n1171);
    let n1174: ZB = zb_or(n1172, n1173);
    let n1175: ZB = zb_and(n832, n1170);
    let n1176: ZB = zb_not(n1175);
    let n1177: ZB = zb_and(n1174, n1175);
    let n1178: ZB = zb_and(n1174, n1176);
    let n1179: ZB = zb_or(n1177, n1178);
    let n1180: ZB = zb_and(n1176, n1179);
    let n1181: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1153);
    let n1182: ZB = zb_not(n1181);
    let n1183: ZB = zb_and(n1180, n1181);
    let n1184: ZB = zb_and(n1180, n1182);
    let n1185: ZB = zb_or(n1183, n1184);
    let n1186: ZB = zb_and(n845, n1181);
    let n1187: ZB = zb_not(n1186);
    let n1188: ZB = zb_and(n1185, n1186);
    let n1189: ZB = zb_and(n1185, n1187);
    let n1190: ZB = zb_or(n1188, n1189);
    let n1191: ZB = zb_and(n851, n1186);
    let n1192: ZB = zb_not(n1191);
    let n1193: ZB = zb_and(n1190, n1192);
    let n1194: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1153);
    let n1195: ZB = zb_not(n1194);
    let n1196: ZB = zb_and(n1193, n1194);
    let n1197: ZB = zb_and(n1193, n1195);
    let n1198: ZB = zb_and(n862, n1196);
    let n1199: ZB = zb_and(n861, n1196);
    let n1200: ZB = zb_or(n1198, n1199);
    let n1201: ZB = zb_or(n1197, n1200);
    let n1202: ZB = zb_and(n1082, n1194);
    let n1203: ZB = zb_not(n1202);
    let n1204: ZB = zb_and(n1201, n1202);
    let n1205: ZB = zb_and(n1201, n1203);
    let n1206: ZB = zb_or(n1204, n1205);
    let n1207: ZB = zb_and(n875, n1202);
    let n1208: ZB = zb_not(n1207);
    let n1209: ZB = zb_and(n1206, n1208);
    let n1210: ZB = zb_and(n1015, n1022);
    let n1211: ZB = zb_or(n1152, n1209);
    let n1212: ZB = zsel_b(n949, n1022, n1210);
    let n1213: ZB = zb_or(n1093, n1211);
    let n1214: ZB = zsel_b(n882, n1022, n1212);
    let n1215: ZB = zb_or(n1029, n1213);
    let n1216: ZB = zsel_b(n797, n1022, n1214);
    let n1217: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n778);
    let n1218: ZB = zn_le(n1217, n782);
    let n1219: ZB = zn_gt(n1217, n782);
    let n1220: ZB = zb_and(n1215, n1218);
    let n1221: ZB = zb_and(n1215, n1219);
    let n1222: ZB = zb_and(n796, n1220);
    let n1223: ZB = zb_and(n797, n1220);
    let n1224: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n1217);
    let n1225: ZN = zn_mget(g.cart, n1224, n801);
    let n1226: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1225);
    let n1227: ZB = zb_not(n1226);
    let n1228: ZB = zb_and(n1222, n1226);
    let n1229: ZB = zb_and(n1222, n1227);
    let n1230: ZB = zb_and(n809, n1228);
    let n1231: ZB = zb_and(n808, n1228);
    let n1232: ZB = zb_or(n1230, n1231);
    let n1233: ZB = zb_or(n1229, n1232);
    let n1234: ZB = zb_and(n816, n1226);
    let n1235: ZB = zb_not(n1234);
    let n1236: ZB = zb_and(n1233, n1234);
    let n1237: ZB = zb_and(n1233, n1235);
    let n1238: ZB = zb_or(n1236, n1237);
    let n1239: ZB = zb_and(n822, n1234);
    let n1240: ZB = zb_not(n1239);
    let n1241: ZB = zb_and(n1238, n1240);
    let n1242: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1225);
    let n1243: ZB = zb_not(n1242);
    let n1244: ZB = zb_and(n1241, n1242);
    let n1245: ZB = zb_and(n1241, n1243);
    let n1246: ZB = zb_or(n1244, n1245);
    let n1247: ZB = zb_and(n832, n1242);
    let n1248: ZB = zb_not(n1247);
    let n1249: ZB = zb_and(n1246, n1247);
    let n1250: ZB = zb_and(n1246, n1248);
    let n1251: ZB = zb_or(n1249, n1250);
    let n1252: ZB = zb_and(n1248, n1251);
    let n1253: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1225);
    let n1254: ZB = zb_not(n1253);
    let n1255: ZB = zb_and(n1252, n1253);
    let n1256: ZB = zb_and(n1252, n1254);
    let n1257: ZB = zb_or(n1255, n1256);
    let n1258: ZB = zb_and(n845, n1253);
    let n1259: ZB = zb_not(n1258);
    let n1260: ZB = zb_and(n1257, n1258);
    let n1261: ZB = zb_and(n1257, n1259);
    let n1262: ZB = zb_or(n1260, n1261);
    let n1263: ZB = zb_and(n851, n1258);
    let n1264: ZB = zb_not(n1263);
    let n1265: ZB = zb_and(n1262, n1264);
    let n1266: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1225);
    let n1267: ZB = zb_not(n1266);
    let n1268: ZB = zb_and(n1265, n1266);
    let n1269: ZB = zb_and(n1265, n1267);
    let n1270: ZB = zb_and(n862, n1268);
    let n1271: ZB = zb_and(n861, n1268);
    let n1272: ZN = zn_mul(n1217, zn_splat(P8::from_raw(524288i32)));
    let n1273: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1272);
    let n1274: ZB = zn_eq(n779, n1273);
    let n1275: ZB = zb_or(n1270, n1271);
    let n1276: ZB = zb_or(n861, n1274);
    let n1277: ZB = zb_or(n1269, n1275);
    let n1278: ZB = zb_and(n1266, n1276);
    let n1279: ZB = zb_not(n1278);
    let n1280: ZB = zb_and(n1277, n1278);
    let n1281: ZB = zb_and(n1277, n1279);
    let n1282: ZB = zb_or(n1280, n1281);
    let n1283: ZB = zb_and(n875, n1278);
    let n1284: ZB = zb_not(n1283);
    let n1285: ZB = zb_and(n1282, n1284);
    let n1286: ZB = zb_and(n881, n1285);
    let n1287: ZB = zb_and(n882, n1285);
    let n1288: ZN = zn_mget(g.cart, n1224, n885);
    let n1289: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1288);
    let n1290: ZB = zb_not(n1289);
    let n1291: ZB = zb_and(n1286, n1289);
    let n1292: ZB = zb_and(n1286, n1290);
    let n1293: ZB = zb_and(n809, n1291);
    let n1294: ZB = zb_and(n808, n1291);
    let n1295: ZB = zb_or(n1293, n1294);
    let n1296: ZB = zb_or(n1292, n1295);
    let n1297: ZB = zb_and(n897, n1289);
    let n1298: ZB = zb_not(n1297);
    let n1299: ZB = zb_and(n1296, n1297);
    let n1300: ZB = zb_and(n1296, n1298);
    let n1301: ZB = zb_or(n1299, n1300);
    let n1302: ZB = zb_and(n822, n1297);
    let n1303: ZB = zb_not(n1302);
    let n1304: ZB = zb_and(n1301, n1303);
    let n1305: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1288);
    let n1306: ZB = zb_not(n1305);
    let n1307: ZB = zb_and(n1304, n1305);
    let n1308: ZB = zb_and(n1304, n1306);
    let n1309: ZB = zb_or(n1307, n1308);
    let n1310: ZB = zb_and(n832, n1305);
    let n1311: ZB = zb_not(n1310);
    let n1312: ZB = zb_and(n1309, n1310);
    let n1313: ZB = zb_and(n1309, n1311);
    let n1314: ZB = zb_or(n1312, n1313);
    let n1315: ZB = zb_and(n1311, n1314);
    let n1316: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1288);
    let n1317: ZB = zb_not(n1316);
    let n1318: ZB = zb_and(n1315, n1316);
    let n1319: ZB = zb_and(n1315, n1317);
    let n1320: ZB = zb_or(n1318, n1319);
    let n1321: ZB = zb_and(n845, n1316);
    let n1322: ZB = zb_not(n1321);
    let n1323: ZB = zb_and(n1320, n1321);
    let n1324: ZB = zb_and(n1320, n1322);
    let n1325: ZB = zb_or(n1323, n1324);
    let n1326: ZB = zb_and(n851, n1321);
    let n1327: ZB = zb_not(n1326);
    let n1328: ZB = zb_and(n1325, n1327);
    let n1329: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1288);
    let n1330: ZB = zb_not(n1329);
    let n1331: ZB = zb_and(n1328, n1329);
    let n1332: ZB = zb_and(n1328, n1330);
    let n1333: ZB = zb_and(n862, n1331);
    let n1334: ZB = zb_and(n861, n1331);
    let n1335: ZB = zb_or(n1333, n1334);
    let n1336: ZB = zb_or(n1332, n1335);
    let n1337: ZB = zb_and(n1276, n1329);
    let n1338: ZB = zb_not(n1337);
    let n1339: ZB = zb_and(n1336, n1337);
    let n1340: ZB = zb_and(n1336, n1338);
    let n1341: ZB = zb_or(n1339, n1340);
    let n1342: ZB = zb_and(n875, n1337);
    let n1343: ZB = zb_not(n1342);
    let n1344: ZB = zb_and(n1341, n1343);
    let n1345: ZB = zb_and(n948, n1344);
    let n1346: ZB = zb_and(n949, n1344);
    let n1347: ZN = zn_mget(g.cart, n1224, n952);
    let n1348: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1347);
    let n1349: ZB = zb_not(n1348);
    let n1350: ZB = zb_and(n1345, n1348);
    let n1351: ZB = zb_and(n1345, n1349);
    let n1352: ZB = zb_and(n809, n1350);
    let n1353: ZB = zb_and(n808, n1350);
    let n1354: ZB = zb_or(n1352, n1353);
    let n1355: ZB = zb_or(n1351, n1354);
    let n1356: ZB = zb_and(n964, n1348);
    let n1357: ZB = zb_not(n1356);
    let n1358: ZB = zb_and(n1355, n1356);
    let n1359: ZB = zb_and(n1355, n1357);
    let n1360: ZB = zb_or(n1358, n1359);
    let n1361: ZB = zb_and(n822, n1356);
    let n1362: ZB = zb_not(n1361);
    let n1363: ZB = zb_and(n1360, n1362);
    let n1364: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1347);
    let n1365: ZB = zb_not(n1364);
    let n1366: ZB = zb_and(n1363, n1364);
    let n1367: ZB = zb_and(n1363, n1365);
    let n1368: ZB = zb_or(n1366, n1367);
    let n1369: ZB = zb_and(n832, n1364);
    let n1370: ZB = zb_not(n1369);
    let n1371: ZB = zb_and(n1368, n1369);
    let n1372: ZB = zb_and(n1368, n1370);
    let n1373: ZB = zb_or(n1371, n1372);
    let n1374: ZB = zb_and(n1370, n1373);
    let n1375: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1347);
    let n1376: ZB = zb_not(n1375);
    let n1377: ZB = zb_and(n1374, n1375);
    let n1378: ZB = zb_and(n1374, n1376);
    let n1379: ZB = zb_or(n1377, n1378);
    let n1380: ZB = zb_and(n845, n1375);
    let n1381: ZB = zb_not(n1380);
    let n1382: ZB = zb_and(n1379, n1380);
    let n1383: ZB = zb_and(n1379, n1381);
    let n1384: ZB = zb_or(n1382, n1383);
    let n1385: ZB = zb_and(n851, n1380);
    let n1386: ZB = zb_not(n1385);
    let n1387: ZB = zb_and(n1384, n1386);
    let n1388: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1347);
    let n1389: ZB = zb_not(n1388);
    let n1390: ZB = zb_and(n1387, n1388);
    let n1391: ZB = zb_and(n1387, n1389);
    let n1392: ZB = zb_and(n862, n1390);
    let n1393: ZB = zb_and(n861, n1390);
    let n1394: ZB = zb_or(n1392, n1393);
    let n1395: ZB = zb_or(n1391, n1394);
    let n1396: ZB = zb_and(n1276, n1388);
    let n1397: ZB = zb_not(n1396);
    let n1398: ZB = zb_and(n1395, n1396);
    let n1399: ZB = zb_and(n1395, n1397);
    let n1400: ZB = zb_or(n1398, n1399);
    let n1401: ZB = zb_and(n875, n1396);
    let n1402: ZB = zb_not(n1401);
    let n1403: ZB = zb_and(n1400, n1402);
    let n1404: ZB = zb_and(n1015, n1216);
    let n1405: ZB = zb_or(n1346, n1403);
    let n1406: ZB = zsel_b(n949, n1216, n1404);
    let n1407: ZB = zb_or(n1287, n1405);
    let n1408: ZB = zsel_b(n882, n1216, n1406);
    let n1409: ZB = zb_or(n1223, n1407);
    let n1410: ZB = zsel_b(n797, n1216, n1408);
    let n1411: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n778);
    let n1412: ZB = zn_gt(n1411, n782);
    let n1413: ZB = zb_and(n1410, n1412);
    let n1414: ZB = zb_or(n1221, n1409);
    let n1415: ZB = zsel_b(n1219, n1216, n1413);
    let n1416: ZB = zb_or(n1027, n1414);
    let n1417: ZB = zsel_b(n1025, n1022, n1415);
    let n1418: ZB = zb_or(n787, n1416);
    let n1419: ZB = zsel_b(n785, n774, n1417);
    let n1420: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n775);
    let n1421: ZB = zn_tile_flag_at(g.cache, g.cart, n562, n1420, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1422: ZB = zb_not(n1421);
    let n1423: ZB = zb_and(n1418, n1422);
    let n1424: ZB = zb_and(n1418, n1421);
    let n1425: ZB = zb_or(n1423, n1424);
    let n1426: ZB = zb_and(n1422, n1425);
    let n1427: ZB = zb_and(n1421, n1425);
    let n1428: ZB = zb_or(n1426, n1427);
    let n1429: ZB = zb_and(n1421, n1428);
    let n1430: ZB = zb_and(n1422, n1428);
    let n1431: ZB = zb_and(n286, n1430);
    let n1432: ZB = zb_and(n287, n1430);
    let n1433: ZB = zb_or(n1431, n1432);
    let n1434: ZN = zsel_n(n1421, zn_splat(P8::from_raw(393216i32)), n289);
    let n1435: ZB = zb_or(n1429, n1433);
    let n1436: ZB = zb_and(n291, n1435);
    let n1437: ZB = zb_and(n292, n1435);
    let n1438: ZB = zn_gt(n546, r_c359);
    let n1439: ZB = zn_le(n546, r_c359);
    let n1440: ZB = zb_and(n1436, n1438);
    let n1441: ZB = zb_and(n1436, n1439);
    let n1442: ZN = zn_sub(n546, r_c357);
    let n1443: ZN = zn_max(r_c359, n1442);
    let n1444: ZN = zn_add(r_c357, n546);
    let n1445: ZN = zn_min(r_c359, n1444);
    let n1446: ZN = zsel_n(n1438, n1443, n1445);
    let n1447: ZB = zb_or(n1440, n1441);
    let n1448: ZB = zn_gt(n772, r_c360);
    let n1449: ZB = zn_le(n772, r_c360);
    let n1450: ZB = zb_and(n1447, n1448);
    let n1451: ZB = zb_and(n1447, n1449);
    let n1452: ZN = zn_sub(n772, r_c358);
    let n1453: ZN = zn_max(r_c360, n1452);
    let n1454: ZN = zn_add(r_c358, n772);
    let n1455: ZN = zn_min(r_c360, n1454);
    let n1456: ZN = zsel_n(n1448, n1453, n1455);
    let n1457: ZB = zb_or(n1450, n1451);
    let n1458: ZB = zb_and(n1422, n1437);
    let n1459: ZB = zb_and(n1421, n1437);
    let n1460: ZN = zsel_n(n1422, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n1461: ZB = zb_or(n1458, n1459);
    let n1462: ZN = zn_abs(n546);
    let n1463: ZB = zn_gt(n1462, zn_splat(P8::from_raw(65536i32)));
    let n1464: ZB = zn_le(n1462, zn_splat(P8::from_raw(65536i32)));
    let n1465: ZB = zb_and(n1461, n1463);
    let n1466: ZB = zb_and(n1461, n1464);
    let n1467: ZB = zn_gt(n546, zn_splat(P8::from_raw(0i32)));
    let n1468: ZB = zb_and(n1465, n1467);
    let n1469: ZB = zb_and(n851, n1465);
    let n1470: ZB = zn_lt(n546, zn_splat(P8::from_raw(0i32)));
    let n1471: ZB = zb_and(n1469, n1470);
    let n1472: ZB = zb_and(n875, n1469);
    let n1473: ZB = zn_gt(n546, zn_splat(P8::from_raw(65536i32)));
    let n1474: ZB = zn_le(n546, zn_splat(P8::from_raw(65536i32)));
    let n1475: ZB = zb_and(n1468, n1473);
    let n1476: ZB = zb_and(n1468, n1474);
    let n1477: ZN = zn_sub(n546, zn_splat(P8::from_raw(9830i32)));
    let n1478: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n1477);
    let n1479: ZN = zn_add(zn_splat(P8::from_raw(9830i32)), n546);
    let n1480: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n1479);
    let n1481: ZB = zn_gt(n546, zn_splat(P8::from_raw(-65536i32)));
    let n1482: ZB = zn_le(n546, zn_splat(P8::from_raw(-65536i32)));
    let n1483: ZB = zb_and(n1471, n1481);
    let n1484: ZB = zb_and(n1471, n1482);
    let n1485: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1477);
    let n1486: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n1479);
    let n1487: ZB = zb_and(n851, n1472);
    let n1488: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1477);
    let n1489: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n1479);
    let n1490: ZN = zsel_n(n1481, n1485, n1486);
    let n1491: ZB = zb_or(n1483, n1484);
    let n1492: ZN = zsel_n(n1467, n1488, n1489);
    let n1493: ZN = zsel_n(n1473, n1478, n1480);
    let n1494: ZB = zb_or(n1475, n1476);
    let n1495: ZN = zsel_n(n1470, n1490, n1492);
    let n1496: ZB = zb_or(n1487, n1491);
    let n1497: ZN = zsel_n(n1467, n1493, n1495);
    let n1498: ZB = zb_or(n1494, n1496);
    let n1499: ZB = zb_and(n1466, n1467);
    let n1500: ZB = zb_and(n851, n1466);
    let n1501: ZN = zn_sub(n546, n1460);
    let n1502: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1501);
    let n1503: ZN = zn_add(n546, n1460);
    let n1504: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n1503);
    let n1505: ZN = zsel_n(n1467, n1502, n1504);
    let n1506: ZB = zb_or(n1499, n1500);
    let n1507: ZN = zsel_n(n1463, n1497, n1505);
    let n1508: ZB = zb_or(n1498, n1506);
    let n1509: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1507);
    let n1510: ZB = zb_not(n1509);
    let n1511: ZB = zb_and(n1508, n1510);
    let n1512: ZB = zb_and(n1508, n1509);
    let n1513: ZB = zn_lt(n1507, zn_splat(P8::from_raw(0i32)));
    let n1514: ZB = zsel_b(n1510, n1513, r_c361);
    let n1515: ZB = zb_or(n1511, n1512);
    let n1516: ZN = zn_abs(n772);
    let n1517: ZB = zn_le(n1516, zn_splat(P8::from_raw(9830i32)));
    let n1518: ZB = zn_gt(n1516, zn_splat(P8::from_raw(9830i32)));
    let n1519: ZB = zb_and(n1515, n1517);
    let n1520: ZB = zb_and(n1515, n1518);
    let n1521: ZN = zsel_n(n1517, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(13762i32)));
    let n1522: ZB = zb_or(n1519, n1520);
    let n1523: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n775);
    let n1524: ZB = zb_and(n1422, n1522);
    let n1525: ZB = zb_and(n1421, n1522);
    let n1526: ZN = zn_add(n772, n1521);
    let n1527: ZN = zn_min(zn_splat(P8::from_raw(131072i32)), n1526);
    let n1528: ZN = zsel_n(n1422, n1527, n772);
    let n1529: ZB = zb_or(n1524, n1525);
    let n1530: ZB = zn_gt(n1434, zn_splat(P8::from_raw(0i32)));
    let n1531: ZB = zn_le(n1434, zn_splat(P8::from_raw(0i32)));
    let n1532: ZN = zn_add(zn_splat(P8::from_raw(-196608i32)), n561);
    let n1533: ZB = zn_tile_flag_at(g.cache, g.cart, n1532, n1523, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1534: ZB = zb_not(n1533);
    let n1535: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n561);
    let n1536: ZB = zn_tile_flag_at(g.cache, g.cart, n1535, n1523, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1537: ZB = zb_not(n1536);
    let n1538: ZN = zsel_n(n1536, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n1539: ZN = zsel_n(n1533, zn_splat(P8::from_raw(-65536i32)), n1538);
    let n1540: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1539);
    let n1541: ZB = zb_not(n1540);
    let n1542: ZN = zn_neg(n1539);
    let n1543: ZN = zn_mul(n1542, zn_splat(P8::from_raw(131072i32)));
    let n1544: ZN = zsel_n(n1541, n1543, n1507);
    let n1545: ZN = zsel_n(n1541, zn_splat(P8::from_raw(-131072i32)), n1528);
    let n1546: ZN = zsel_n(n1530, zn_splat(P8::from_raw(0i32)), n1434);
    let n1547: ZN = zsel_n(n1530, n1507, n1544);
    let n1548: ZN = zsel_n(n1530, zn_splat(P8::from_raw(-131072i32)), n1545);
    let n1549: ZB = zb_not(n1514);
    let n1550: ZN = zsel_n(n1514, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n1551: ZB = zn_gt(n1550, zn_splat(P8::from_raw(0i32)));
    let n1552: ZB = zn_le(n1550, zn_splat(P8::from_raw(0i32)));
    let n1553: ZB = zn_lt(n1550, zn_splat(P8::from_raw(0i32)));
    let n1554: ZB = zn_ge(n1550, zn_splat(P8::from_raw(0i32)));
    let n1555: ZN = zsel_n(n1553, zn_splat(P8::from_raw(-131072i32)), zn_splat(P8::from_raw(0i32)));
    let n1556: ZN = zsel_n(n1551, zn_splat(P8::from_raw(131072i32)), n1555);
    let n1557: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1550);
    let n1558: ZB = zb_not(n1557);
    let n1559: ZN = zsel_n(n1558, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n1560: ZB = zsel_b(n291, r_c361, n1514);
    let n1561: ZN = zsel_n(n291, n1446, n1507);
    let n1562: ZN = zsel_n(n291, n1456, n1528);
    let n1563: ZB = zb_or(n1457, n1529);
    let n1564: ZB = zb_and(n134, n1563);
    let n1565: ZB = zn_lt(n544, zn_splat(P8::from_raw(-65536i32)));
    let n1566: ZB = zn_ge(n544, zn_splat(P8::from_raw(-65536i32)));
    let n1567: ZB = zb_and(n1564, n1566);
    let n1568: ZB = zb_and(n1564, n1565);
    let n1569: ZB = zn_gt(n544, zn_splat(P8::from_raw(7929856i32)));
    let n1570: ZB = zb_or(n1567, n1568);
    let n1571: ZB = zb_or(n1565, n1569);
    let n1572: ZB = zb_not(n1571);
    let n1573: ZB = zb_and(n1570, n1571);
    let n1574: ZB = zb_and(n1570, n1572);
    let n1575: ZN = zn_min(zn_splat(P8::from_raw(7929856i32)), n544);
    let n1576: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1575);
    let n1577: ZN = zsel_n(n1571, n1576, n544);
    let n1578: ZN = zsel_n(n1571, zn_splat(P8::from_raw(0i32)), n1561);
    let n1579: ZB = zb_or(n1573, n1574);
    let n1580: ZN = zsel_n(n295, n544, n1577);
    let n1581: ZN = zsel_n(n295, n1561, n1578);
    let n1594: ZB = zb_and(n1466, n1481);
    let n1595: ZB = zb_and(n1466, n1482);
    let n1596: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1501);
    let n1597: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n1503);
    let n1598: ZN = zsel_n(n1481, n1596, n1597);
    let n1599: ZB = zb_or(n1594, n1595);
    let n1600: ZN = zsel_n(n1463, n1497, n1598);
    let n1601: ZB = zb_or(n1498, n1599);
    let n1602: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1600);
    let n1603: ZB = zb_not(n1602);
    let n1604: ZB = zb_and(n1601, n1603);
    let n1605: ZB = zb_and(n1601, n1602);
    let n1606: ZB = zn_lt(n1600, zn_splat(P8::from_raw(0i32)));
    let n1607: ZB = zsel_b(n1603, n1606, r_c361);
    let n1608: ZB = zb_or(n1604, n1605);
    let n1609: ZB = zb_and(n1517, n1608);
    let n1610: ZB = zb_and(n1518, n1608);
    let n1611: ZB = zb_or(n1609, n1610);
    let n1612: ZN = zn_add(zn_splat(P8::from_raw(-65536i32)), n561);
    let n1613: ZB = zn_tile_flag_at(g.cache, g.cart, n1612, n1523, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1614: ZB = zb_not(n1613);
    let n1615: ZB = zb_and(n1611, n1614);
    let n1616: ZB = zb_and(n1611, n1613);
    let n1617: ZB = zb_or(n1615, n1616);
    let n1618: ZB = zb_and(n1614, n1617);
    let n1619: ZB = zb_and(n1613, n1617);
    let n1620: ZB = zb_or(n1618, n1619);
    let n1621: ZB = zb_and(n1613, n1620);
    let n1622: ZB = zb_and(n1614, n1620);
    let n1623: ZB = zb_or(n1621, n1622);
    let n1624: ZB = zb_and(n1613, n1623);
    let n1625: ZB = zb_and(n1614, n1623);
    let n1626: ZN = zsel_n(n1613, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n1627: ZB = zb_or(n1624, n1625);
    let n1628: ZB = zb_and(n1422, n1627);
    let n1629: ZB = zb_and(n1421, n1627);
    let n1630: ZN = zn_min(n1526, n1626);
    let n1631: ZN = zsel_n(n1422, n1630, n772);
    let n1632: ZB = zb_or(n1628, n1629);
    let n1633: ZN = zsel_n(n1541, n1543, n1600);
    let n1634: ZN = zsel_n(n1541, zn_splat(P8::from_raw(-131072i32)), n1631);
    let n1635: ZN = zsel_n(n1530, n1600, n1633);
    let n1636: ZN = zsel_n(n1530, zn_splat(P8::from_raw(-131072i32)), n1634);
    let n1637: ZB = zsel_b(n291, r_c361, n1607);
    let n1638: ZN = zsel_n(n291, n1446, n1600);
    let n1639: ZN = zsel_n(n291, n1456, n1631);
    let n1640: ZB = zb_or(n1457, n1632);
    let n1641: ZB = zb_and(n134, n1640);
    let n1642: ZB = zb_and(n1566, n1641);
    let n1643: ZB = zb_and(n1565, n1641);
    let n1644: ZB = zb_or(n1642, n1643);
    let n1645: ZB = zb_and(n1571, n1644);
    let n1646: ZB = zb_and(n1572, n1644);
    let n1647: ZN = zsel_n(n1571, zn_splat(P8::from_raw(0i32)), n1638);
    let n1648: ZB = zb_or(n1645, n1646);
    let n1649: ZN = zsel_n(n295, n1638, n1647);
    let n1650: ZB = zb_and(n1466, n1473);
    let n1651: ZB = zb_and(n1466, n1474);
    let n1652: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n1501);
    let n1653: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n1503);
    let n1654: ZN = zsel_n(n1473, n1652, n1653);
    let n1655: ZB = zb_or(n1650, n1651);
    let n1656: ZN = zsel_n(n1463, n1497, n1654);
    let n1657: ZB = zb_or(n1498, n1655);
    let n1658: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1656);
    let n1659: ZB = zb_not(n1658);
    let n1660: ZB = zb_and(n1657, n1659);
    let n1661: ZB = zb_and(n1657, n1658);
    let n1662: ZB = zn_lt(n1656, zn_splat(P8::from_raw(0i32)));
    let n1663: ZB = zsel_b(n1659, n1662, r_c361);
    let n1664: ZB = zb_or(n1660, n1661);
    let n1665: ZB = zb_and(n1517, n1664);
    let n1666: ZB = zb_and(n1518, n1664);
    let n1667: ZB = zb_or(n1665, n1666);
    let n1668: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n561);
    let n1669: ZB = zn_tile_flag_at(g.cache, g.cart, n1668, n1523, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1670: ZB = zb_not(n1669);
    let n1671: ZB = zb_and(n1667, n1670);
    let n1672: ZB = zb_and(n1667, n1669);
    let n1673: ZB = zb_or(n1671, n1672);
    let n1674: ZB = zb_and(n1670, n1673);
    let n1675: ZB = zb_and(n1669, n1673);
    let n1676: ZB = zb_or(n1674, n1675);
    let n1677: ZB = zb_and(n1669, n1676);
    let n1678: ZB = zb_and(n1670, n1676);
    let n1679: ZB = zb_or(n1677, n1678);
    let n1680: ZB = zb_and(n1669, n1679);
    let n1681: ZB = zb_and(n1670, n1679);
    let n1682: ZN = zsel_n(n1669, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n1683: ZB = zb_or(n1680, n1681);
    let n1684: ZB = zb_and(n1422, n1683);
    let n1685: ZB = zb_and(n1421, n1683);
    let n1686: ZN = zn_min(n1526, n1682);
    let n1687: ZN = zsel_n(n1422, n1686, n772);
    let n1688: ZB = zb_or(n1684, n1685);
    let n1689: ZN = zsel_n(n1541, n1543, n1656);
    let n1690: ZN = zsel_n(n1541, zn_splat(P8::from_raw(-131072i32)), n1687);
    let n1691: ZN = zsel_n(n1530, n1656, n1689);
    let n1692: ZN = zsel_n(n1530, zn_splat(P8::from_raw(-131072i32)), n1690);
    let n1693: ZB = zsel_b(n291, r_c361, n1663);
    let n1694: ZN = zsel_n(n291, n1446, n1656);
    let n1695: ZN = zsel_n(n291, n1456, n1687);
    let n1696: ZB = zb_or(n1457, n1688);
    let n1697: ZB = zb_and(n134, n1696);
    let n1698: ZB = zb_and(n1566, n1697);
    let n1699: ZB = zb_and(n1565, n1697);
    let n1700: ZB = zb_or(n1698, n1699);
    let n1701: ZB = zb_and(n1571, n1700);
    let n1702: ZB = zb_and(n1572, n1700);
    let n1703: ZN = zsel_n(n1571, zn_splat(P8::from_raw(0i32)), n1694);
    let n1704: ZB = zb_or(n1701, n1702);
    let n1705: ZN = zsel_n(n295, n1694, n1703);
    let n1706: ZB = zb_and(n140, n1529);
    let n1707: ZB = zb_and(r_c294, n1529);
    let n1708: ZB = zb_and(n1530, n1706);
    let n1709: ZB = zb_and(n1531, n1706);
    let n1710: ZB = zb_and(n1534, n1709);
    let n1711: ZB = zb_and(n1533, n1709);
    let n1712: ZB = zb_or(n1710, n1711);
    let n1713: ZB = zb_and(n1534, n1712);
    let n1714: ZB = zb_and(n1533, n1712);
    let n1715: ZB = zb_or(n1713, n1714);
    let n1716: ZB = zb_and(n1533, n1715);
    let n1717: ZB = zb_and(n1534, n1715);
    let n1718: ZB = zb_and(n1537, n1717);
    let n1719: ZB = zb_and(n1536, n1717);
    let n1720: ZB = zb_or(n1718, n1719);
    let n1721: ZB = zb_and(n1537, n1720);
    let n1722: ZB = zb_and(n1536, n1720);
    let n1723: ZB = zb_or(n1721, n1722);
    let n1724: ZB = zb_and(n1536, n1723);
    let n1725: ZB = zb_and(n1537, n1723);
    let n1726: ZB = zb_or(n1724, n1725);
    let n1727: ZB = zb_or(n1716, n1726);
    let n1728: ZB = zb_and(n1541, n1727);
    let n1729: ZB = zb_and(n1540, n1727);
    let n1730: ZB = zb_or(n1728, n1729);
    let n1731: ZB = zb_or(n1708, n1730);
    let n1732: ZN = zsel_n(n140, n1546, n1434);
    let n1733: ZN = zsel_n(n140, n1547, n1507);
    let n1734: ZN = zsel_n(n140, n1548, n1528);
    let n1735: ZB = zb_or(n1707, n1731);
    let n1736: ZN = zsel_n(n291, n1434, n1732);
    let n1737: ZN = zsel_n(n291, n1446, n1733);
    let n1738: ZN = zsel_n(n291, n1456, n1734);
    let n1739: ZB = zb_or(n1457, n1735);
    let n1740: ZB = zb_and(n134, n1739);
    let n1741: ZB = zb_and(n1566, n1740);
    let n1742: ZB = zb_and(n1565, n1740);
    let n1743: ZB = zb_or(n1741, n1742);
    let n1744: ZB = zb_and(n1571, n1743);
    let n1745: ZB = zb_and(n1572, n1743);
    let n1746: ZN = zsel_n(n1571, zn_splat(P8::from_raw(0i32)), n1737);
    let n1747: ZB = zb_or(n1744, n1745);
    let n1748: ZN = zsel_n(n295, n1737, n1746);
    let n1749: ZB = zb_and(n140, n1632);
    let n1750: ZB = zb_and(r_c294, n1632);
    let n1751: ZB = zb_and(n1530, n1749);
    let n1752: ZB = zb_and(n1531, n1749);
    let n1753: ZB = zb_and(n1534, n1752);
    let n1754: ZB = zb_and(n1533, n1752);
    let n1755: ZB = zb_or(n1753, n1754);
    let n1756: ZB = zb_and(n1534, n1755);
    let n1757: ZB = zb_and(n1533, n1755);
    let n1758: ZB = zb_or(n1756, n1757);
    let n1759: ZB = zb_and(n1533, n1758);
    let n1760: ZB = zb_and(n1534, n1758);
    let n1761: ZB = zb_and(n1537, n1760);
    let n1762: ZB = zb_and(n1536, n1760);
    let n1763: ZB = zb_or(n1761, n1762);
    let n1764: ZB = zb_and(n1537, n1763);
    let n1765: ZB = zb_and(n1536, n1763);
    let n1766: ZB = zb_or(n1764, n1765);
    let n1767: ZB = zb_and(n1536, n1766);
    let n1768: ZB = zb_and(n1537, n1766);
    let n1769: ZB = zb_or(n1767, n1768);
    let n1770: ZB = zb_or(n1759, n1769);
    let n1771: ZB = zb_and(n1541, n1770);
    let n1772: ZB = zb_and(n1540, n1770);
    let n1773: ZB = zb_or(n1771, n1772);
    let n1774: ZB = zb_or(n1751, n1773);
    let n1775: ZN = zsel_n(n140, n1635, n1600);
    let n1776: ZN = zsel_n(n140, n1636, n1631);
    let n1777: ZB = zb_or(n1750, n1774);
    let n1778: ZN = zsel_n(n291, n1446, n1775);
    let n1779: ZN = zsel_n(n291, n1456, n1776);
    let n1780: ZB = zb_or(n1457, n1777);
    let n1781: ZB = zb_and(n134, n1780);
    let n1782: ZB = zb_and(n1566, n1781);
    let n1783: ZB = zb_and(n1565, n1781);
    let n1784: ZB = zb_or(n1782, n1783);
    let n1785: ZB = zb_and(n1571, n1784);
    let n1786: ZB = zb_and(n1572, n1784);
    let n1787: ZN = zsel_n(n1571, zn_splat(P8::from_raw(0i32)), n1778);
    let n1788: ZB = zb_or(n1785, n1786);
    let n1789: ZN = zsel_n(n295, n1778, n1787);
    let n1790: ZB = zb_and(n140, n1688);
    let n1791: ZB = zb_and(r_c294, n1688);
    let n1792: ZB = zb_and(n1530, n1790);
    let n1793: ZB = zb_and(n1531, n1790);
    let n1794: ZB = zb_and(n1534, n1793);
    let n1795: ZB = zb_and(n1533, n1793);
    let n1796: ZB = zb_or(n1794, n1795);
    let n1797: ZB = zb_and(n1534, n1796);
    let n1798: ZB = zb_and(n1533, n1796);
    let n1799: ZB = zb_or(n1797, n1798);
    let n1800: ZB = zb_and(n1533, n1799);
    let n1801: ZB = zb_and(n1534, n1799);
    let n1802: ZB = zb_and(n1537, n1801);
    let n1803: ZB = zb_and(n1536, n1801);
    let n1804: ZB = zb_or(n1802, n1803);
    let n1805: ZB = zb_and(n1537, n1804);
    let n1806: ZB = zb_and(n1536, n1804);
    let n1807: ZB = zb_or(n1805, n1806);
    let n1808: ZB = zb_and(n1536, n1807);
    let n1809: ZB = zb_and(n1537, n1807);
    let n1810: ZB = zb_or(n1808, n1809);
    let n1811: ZB = zb_or(n1800, n1810);
    let n1812: ZB = zb_and(n1541, n1811);
    let n1813: ZB = zb_and(n1540, n1811);
    let n1814: ZB = zb_or(n1812, n1813);
    let n1815: ZB = zb_or(n1792, n1814);
    let n1816: ZN = zsel_n(n140, n1691, n1656);
    let n1817: ZN = zsel_n(n140, n1692, n1687);
    let n1818: ZB = zb_or(n1791, n1815);
    let n1819: ZN = zsel_n(n291, n1446, n1816);
    let n1820: ZN = zsel_n(n291, n1456, n1817);
    let n1821: ZB = zb_or(n1457, n1818);
    let n1822: ZB = zb_and(n134, n1821);
    let n1823: ZB = zb_and(n1566, n1822);
    let n1824: ZB = zb_and(n1565, n1822);
    let n1825: ZB = zb_or(n1823, n1824);
    let n1826: ZB = zb_and(n1571, n1825);
    let n1827: ZB = zb_and(n1572, n1825);
    let n1828: ZN = zsel_n(n1571, zn_splat(P8::from_raw(0i32)), n1819);
    let n1829: ZB = zb_or(n1826, n1827);
    let n1830: ZN = zsel_n(n295, n1819, n1828);
    let n1831: ZN = zsel_n(n133, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n1832: ZB = zb_or(r_c41, n133);
    let n1833: ZN = zsel_n(n133, zn_splat(P8::from_raw(655360i32)), n290);
    let n1834: ZN = zsel_n(n133, zn_splat(P8::from_raw(262144i32)), r_c283);
    let n1835: ZN = zsel_n(n133, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n1836: ZN = zsel_n(n133, zn_splat(P8::from_raw(98304i32)), r_c357);
    let n1837: ZN = zsel_n(n133, zn_splat(P8::from_raw(0i32)), r_c360);
    let n1838: ZN = zsel_n(n291, r_c20, n1831);
    let n1839: ZB = zsel_b(n291, r_c41, n1832);
    let n1840: ZN = zsel_n(n291, n290, n1833);
    let n1841: ZN = zsel_n(n291, n293, n1834);
    let n1842: ZN = zsel_n(n291, zn_splat(P8::from_raw(65536i32)), n1835);
    let n1843: ZN = zsel_n(n291, r_c357, n1836);
    let n1844: ZN = zsel_n(n291, r_c360, n1837);
    let n1845: ZB = zn_gt(n1838, zn_splat(P8::from_raw(0i32)));
    let n1846: ZB = zn_le(n1838, zn_splat(P8::from_raw(0i32)));
    let n1847: ZB = zb_and(n133, n1529);
    let n1848: ZB = zb_and(r_c293, n1529);
    let n1849: ZB = zb_and(n1514, n1847);
    let n1850: ZB = zb_and(n1549, n1847);
    let n1851: ZB = zb_or(n1849, n1850);
    let n1852: ZB = zb_and(n1551, n1851);
    let n1853: ZB = zb_and(n1552, n1851);
    let n1854: ZB = zb_and(n1553, n1853);
    let n1855: ZB = zb_and(n1554, n1853);
    let n1856: ZB = zb_or(n1854, n1855);
    let n1857: ZB = zb_or(n1852, n1856);
    let n1858: ZB = zb_and(n1558, n1857);
    let n1859: ZB = zb_and(n1557, n1857);
    let n1860: ZB = zb_or(n1858, n1859);
    let n1861: ZN = zsel_n(n133, n1559, r_c358);
    let n1862: ZN = zsel_n(n133, n1556, r_c359);
    let n1863: ZN = zsel_n(n133, n1550, n1507);
    let n1864: ZN = zsel_n(n133, zn_splat(P8::from_raw(0i32)), n1528);
    let n1865: ZB = zb_or(n1848, n1860);
    let n1866: ZN = zsel_n(n291, r_c358, n1861);
    let n1867: ZN = zsel_n(n291, r_c359, n1862);
    let n1868: ZN = zsel_n(n291, n1446, n1863);
    let n1869: ZN = zsel_n(n291, n1456, n1864);
    let n1870: ZB = zb_or(n1457, n1865);
    let n1871: ZB = zb_and(n1845, n1870);
    let n1872: ZB = zb_and(n1846, n1870);
    let n1873: ZB = zb_and(n1566, n1872);
    let n1874: ZB = zb_and(n1565, n1872);
    let n1875: ZB = zb_or(n1873, n1874);
    let n1876: ZB = zb_and(n1571, n1875);
    let n1877: ZB = zb_and(n1572, n1875);
    let n1878: ZN = zsel_n(n1571, zn_splat(P8::from_raw(0i32)), n1868);
    let n1879: ZB = zb_or(n1876, n1877);
    let n1880: ZN = zsel_n(n1845, n544, n1577);
    let n1881: ZN = zsel_n(n1845, n1868, n1878);
    let n1882: ZB = zb_or(n1871, n1879);
    let n1883: ZB = zb_and(n133, n1632);
    let n1884: ZB = zb_and(r_c293, n1632);
    let n1885: ZN = zsel_n(n133, zn_splat(P8::from_raw(69510i32)), r_c358);
    let n1886: ZN = zsel_n(n133, zn_splat(P8::from_raw(-131072i32)), r_c359);
    let n1887: ZN = zsel_n(n133, zn_splat(P8::from_raw(-327680i32)), n1600);
    let n1888: ZN = zsel_n(n133, zn_splat(P8::from_raw(0i32)), n1631);
    let n1889: ZB = zb_or(n1883, n1884);
    let n1890: ZN = zsel_n(n291, r_c358, n1885);
    let n1891: ZN = zsel_n(n291, r_c359, n1886);
    let n1892: ZN = zsel_n(n291, n1446, n1887);
    let n1893: ZN = zsel_n(n291, n1456, n1888);
    let n1894: ZB = zb_or(n1457, n1889);
    let n1895: ZB = zb_and(n1845, n1894);
    let n1896: ZB = zb_and(n1846, n1894);
    let n1897: ZB = zb_and(n1566, n1896);
    let n1898: ZB = zb_and(n1565, n1896);
    let n1899: ZB = zb_or(n1897, n1898);
    let n1900: ZB = zb_and(n1571, n1899);
    let n1901: ZB = zb_and(n1572, n1899);
    let n1902: ZN = zsel_n(n1571, zn_splat(P8::from_raw(0i32)), n1892);
    let n1903: ZB = zb_or(n1900, n1901);
    let n1904: ZN = zsel_n(n1845, n1892, n1902);
    let n1905: ZB = zb_or(n1895, n1903);
    let n1906: ZB = zb_and(n133, n1688);
    let n1907: ZB = zb_and(r_c293, n1688);
    let n1908: ZN = zsel_n(n133, zn_splat(P8::from_raw(131072i32)), r_c359);
    let n1909: ZN = zsel_n(n133, zn_splat(P8::from_raw(327680i32)), n1656);
    let n1910: ZN = zsel_n(n133, zn_splat(P8::from_raw(0i32)), n1687);
    let n1911: ZB = zb_or(n1906, n1907);
    let n1912: ZN = zsel_n(n291, r_c359, n1908);
    let n1913: ZN = zsel_n(n291, n1446, n1909);
    let n1914: ZN = zsel_n(n291, n1456, n1910);
    let n1915: ZB = zb_or(n1457, n1911);
    let n1916: ZB = zb_and(n1845, n1915);
    let n1917: ZB = zb_and(n1846, n1915);
    let n1918: ZB = zb_and(n1566, n1917);
    let n1919: ZB = zb_and(n1565, n1917);
    let n1920: ZB = zb_or(n1918, n1919);
    let n1921: ZB = zb_and(n1571, n1920);
    let n1922: ZB = zb_and(n1572, n1920);
    let n1923: ZN = zsel_n(n1571, zn_splat(P8::from_raw(0i32)), n1913);
    let n1924: ZB = zb_or(n1921, n1922);
    let n1925: ZN = zsel_n(n1845, n1913, n1923);
    let n1926: ZB = zb_or(n1916, n1924);
    let n1928: ZN = zsel_n(n133, zn_splat(P8::from_raw(69510i32)), r_c357);
    let n1929: ZN = zsel_n(n133, zn_splat(P8::from_raw(-98304i32)), r_c360);
    let n1930: ZN = zsel_n(n291, r_c357, n1928);
    let n1931: ZN = zsel_n(n291, r_c360, n1929);
    let n1932: ZN = zsel_n(n133, zn_splat(P8::from_raw(98304i32)), r_c358);
    let n1933: ZN = zsel_n(n133, zn_splat(P8::from_raw(0i32)), r_c359);
    let n1934: ZN = zsel_n(n133, zn_splat(P8::from_raw(0i32)), n1507);
    let n1935: ZN = zsel_n(n133, zn_splat(P8::from_raw(-327680i32)), n1528);
    let n1936: ZB = zb_or(n1847, n1848);
    let n1937: ZN = zsel_n(n291, r_c358, n1932);
    let n1938: ZN = zsel_n(n291, r_c359, n1933);
    let n1939: ZN = zsel_n(n291, n1446, n1934);
    let n1940: ZN = zsel_n(n291, n1456, n1935);
    let n1941: ZB = zb_or(n1457, n1936);
    let n1942: ZB = zb_and(n1845, n1941);
    let n1943: ZB = zb_and(n1846, n1941);
    let n1944: ZB = zb_and(n1566, n1943);
    let n1945: ZB = zb_and(n1565, n1943);
    let n1946: ZB = zb_or(n1944, n1945);
    let n1947: ZB = zb_and(n1571, n1946);
    let n1948: ZB = zb_and(n1572, n1946);
    let n1949: ZN = zsel_n(n1571, zn_splat(P8::from_raw(0i32)), n1939);
    let n1950: ZB = zb_or(n1947, n1948);
    let n1951: ZN = zsel_n(n1845, n1939, n1949);
    let n1952: ZB = zb_or(n1942, n1950);
    let n1953: ZN = zsel_n(n133, zn_splat(P8::from_raw(-231700i32)), n1600);
    let n1954: ZN = zsel_n(n133, zn_splat(P8::from_raw(-231700i32)), n1631);
    let n1955: ZN = zsel_n(n291, n1446, n1953);
    let n1956: ZN = zsel_n(n291, n1456, n1954);
    let n1957: ZN = zsel_n(n1571, zn_splat(P8::from_raw(0i32)), n1955);
    let n1958: ZN = zsel_n(n1845, n1955, n1957);
    let n1959: ZN = zsel_n(n133, zn_splat(P8::from_raw(231700i32)), n1656);
    let n1960: ZN = zsel_n(n133, zn_splat(P8::from_raw(-231700i32)), n1687);
    let n1961: ZN = zsel_n(n291, n1446, n1959);
    let n1962: ZN = zsel_n(n291, n1456, n1960);
    let n1963: ZN = zsel_n(n1571, zn_splat(P8::from_raw(0i32)), n1961);
    let n1964: ZN = zsel_n(n1845, n1961, n1963);
    let n1965: ZN = zsel_n(n133, zn_splat(P8::from_raw(131072i32)), r_c360);
    let n1966: ZN = zsel_n(n291, r_c360, n1965);
    let n1967: ZN = zsel_n(n133, zn_splat(P8::from_raw(327680i32)), n1528);
    let n1968: ZN = zsel_n(n291, n1456, n1967);
    let n1969: ZN = zsel_n(n133, zn_splat(P8::from_raw(231700i32)), n1631);
    let n1970: ZN = zsel_n(n291, n1456, n1969);
    let n1971: ZN = zsel_n(n133, zn_splat(P8::from_raw(231700i32)), n1687);
    let n1972: ZN = zsel_n(n291, n1456, n1971);
    let n1973: ZB = zb_and(n133, n1735);
    let n1974: ZB = zb_and(r_c293, n1735);
    let n1975: ZB = zb_and(n1514, n1973);
    let n1976: ZB = zb_and(n1549, n1973);
    let n1977: ZB = zb_or(n1975, n1976);
    let n1978: ZB = zb_and(n1551, n1977);
    let n1979: ZB = zb_and(n1552, n1977);
    let n1980: ZB = zb_and(n1553, n1979);
    let n1981: ZB = zb_and(n1554, n1979);
    let n1982: ZB = zb_or(n1980, n1981);
    let n1983: ZB = zb_or(n1978, n1982);
    let n1984: ZB = zb_and(n1558, n1983);
    let n1985: ZB = zb_and(n1557, n1983);
    let n1986: ZB = zb_or(n1984, n1985);
    let n1987: ZN = zsel_n(n133, n1550, n1733);
    let n1988: ZN = zsel_n(n133, zn_splat(P8::from_raw(0i32)), n1734);
    let n1989: ZB = zb_or(n1974, n1986);
    let n1990: ZN = zsel_n(n291, n1446, n1987);
    let n1991: ZN = zsel_n(n291, n1456, n1988);
    let n1992: ZB = zb_or(n1457, n1989);
    let n1993: ZB = zb_and(n1845, n1992);
    let n1994: ZB = zb_and(n1846, n1992);
    let n1995: ZB = zb_and(n1566, n1994);
    let n1996: ZB = zb_and(n1565, n1994);
    let n1997: ZB = zb_or(n1995, n1996);
    let n1998: ZB = zb_and(n1571, n1997);
    let n1999: ZB = zb_and(n1572, n1997);
    let n2000: ZN = zsel_n(n1571, zn_splat(P8::from_raw(0i32)), n1990);
    let n2001: ZB = zb_or(n1998, n1999);
    let n2002: ZN = zsel_n(n1845, n1990, n2000);
    let n2003: ZB = zb_or(n1993, n2001);
    let n2004: ZB = zb_and(n133, n1777);
    let n2005: ZB = zb_and(r_c293, n1777);
    let n2006: ZN = zsel_n(n133, zn_splat(P8::from_raw(-327680i32)), n1775);
    let n2007: ZN = zsel_n(n133, zn_splat(P8::from_raw(0i32)), n1776);
    let n2008: ZB = zb_or(n2004, n2005);
    let n2009: ZN = zsel_n(n291, n1446, n2006);
    let n2010: ZN = zsel_n(n291, n1456, n2007);
    let n2011: ZB = zb_or(n1457, n2008);
    let n2012: ZB = zb_and(n1845, n2011);
    let n2013: ZB = zb_and(n1846, n2011);
    let n2014: ZB = zb_and(n1566, n2013);
    let n2015: ZB = zb_and(n1565, n2013);
    let n2016: ZB = zb_or(n2014, n2015);
    let n2017: ZB = zb_and(n1571, n2016);
    let n2018: ZB = zb_and(n1572, n2016);
    let n2019: ZN = zsel_n(n1571, zn_splat(P8::from_raw(0i32)), n2009);
    let n2020: ZB = zb_or(n2017, n2018);
    let n2021: ZN = zsel_n(n1845, n2009, n2019);
    let n2022: ZB = zb_or(n2012, n2020);
    let n2023: ZB = zb_and(n133, n1818);
    let n2024: ZB = zb_and(r_c293, n1818);
    let n2025: ZN = zsel_n(n133, zn_splat(P8::from_raw(327680i32)), n1816);
    let n2026: ZN = zsel_n(n133, zn_splat(P8::from_raw(0i32)), n1817);
    let n2027: ZB = zb_or(n2023, n2024);
    let n2028: ZN = zsel_n(n291, n1446, n2025);
    let n2029: ZN = zsel_n(n291, n1456, n2026);
    let n2030: ZB = zb_or(n1457, n2027);
    let n2031: ZB = zb_and(n1845, n2030);
    let n2032: ZB = zb_and(n1846, n2030);
    let n2033: ZB = zb_and(n1566, n2032);
    let n2034: ZB = zb_and(n1565, n2032);
    let n2035: ZB = zb_or(n2033, n2034);
    let n2036: ZB = zb_and(n1571, n2035);
    let n2037: ZB = zb_and(n1572, n2035);
    let n2038: ZN = zsel_n(n1571, zn_splat(P8::from_raw(0i32)), n2028);
    let n2039: ZB = zb_or(n2036, n2037);
    let n2040: ZN = zsel_n(n1845, n2028, n2038);
    let n2041: ZB = zb_or(n2031, n2039);
    let n2042: ZN = zsel_n(n133, zn_splat(P8::from_raw(0i32)), n1733);
    let n2043: ZN = zsel_n(n133, zn_splat(P8::from_raw(-327680i32)), n1734);
    let n2044: ZB = zb_or(n1973, n1974);
    let n2045: ZN = zsel_n(n291, n1446, n2042);
    let n2046: ZN = zsel_n(n291, n1456, n2043);
    let n2047: ZB = zb_or(n1457, n2044);
    let n2048: ZB = zb_and(n1845, n2047);
    let n2049: ZB = zb_and(n1846, n2047);
    let n2050: ZB = zb_and(n1566, n2049);
    let n2051: ZB = zb_and(n1565, n2049);
    let n2052: ZB = zb_or(n2050, n2051);
    let n2053: ZB = zb_and(n1571, n2052);
    let n2054: ZB = zb_and(n1572, n2052);
    let n2055: ZN = zsel_n(n1571, zn_splat(P8::from_raw(0i32)), n2045);
    let n2056: ZB = zb_or(n2053, n2054);
    let n2057: ZN = zsel_n(n1845, n2045, n2055);
    let n2058: ZB = zb_or(n2048, n2056);
    let n2059: ZN = zsel_n(n133, zn_splat(P8::from_raw(-231700i32)), n1775);
    let n2060: ZN = zsel_n(n133, zn_splat(P8::from_raw(-231700i32)), n1776);
    let n2061: ZN = zsel_n(n291, n1446, n2059);
    let n2062: ZN = zsel_n(n291, n1456, n2060);
    let n2063: ZN = zsel_n(n1571, zn_splat(P8::from_raw(0i32)), n2061);
    let n2064: ZN = zsel_n(n1845, n2061, n2063);
    let n2065: ZN = zsel_n(n133, zn_splat(P8::from_raw(231700i32)), n1816);
    let n2066: ZN = zsel_n(n133, zn_splat(P8::from_raw(-231700i32)), n1817);
    let n2067: ZN = zsel_n(n291, n1446, n2065);
    let n2068: ZN = zsel_n(n291, n1456, n2066);
    let n2069: ZN = zsel_n(n1571, zn_splat(P8::from_raw(0i32)), n2067);
    let n2070: ZN = zsel_n(n1845, n2067, n2069);
    let n2071: ZN = zsel_n(n133, zn_splat(P8::from_raw(327680i32)), n1734);
    let n2072: ZN = zsel_n(n291, n1456, n2071);
    let n2073: ZN = zsel_n(n133, zn_splat(P8::from_raw(231700i32)), n1776);
    let n2074: ZN = zsel_n(n291, n1456, n2073);
    let n2075: ZN = zsel_n(n133, zn_splat(P8::from_raw(231700i32)), n1817);
    let n2076: ZN = zsel_n(n291, n1456, n2075);
    let n2077: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c87);
    let n2078: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n2077);
    let n2079: ZB = zb_and(n823, n824);
    let n2080: ZB = zb_and(n834, n838);
    let n2081: ZB = zb_and(n852, n853);
    let n2082: ZB = zb_and(n876, n877);
    let n2083: ZB = zb_or(n2081, n2082);
    let n2084: ZB = zb_or(n2080, n2083);
    let n2085: ZB = zb_or(n2079, n2084);
    let n2086: ZB = zb_and(n903, n904);
    let n2087: ZB = zb_and(n912, n916);
    let n2088: ZB = zb_and(n927, n928);
    let n2089: ZB = zb_and(n943, n944);
    let n2090: ZB = zb_or(n2088, n2089);
    let n2091: ZB = zb_or(n2087, n2090);
    let n2092: ZB = zb_or(n2086, n2091);
    let n2093: ZB = zb_and(n970, n971);
    let n2094: ZB = zb_and(n979, n983);
    let n2095: ZB = zb_and(n994, n995);
    let n2096: ZB = zb_and(n1010, n1011);
    let n2097: ZB = zb_or(n2095, n2096);
    let n2098: ZB = zb_or(n2094, n2097);
    let n2099: ZB = zb_or(n2093, n2098);
    let n2100: ZB = zb_or(n2092, n2099);
    let n2101: ZB = zb_or(n2085, n2100);
    let n2102: ZB = zb_and(n1044, n1045);
    let n2103: ZB = zb_and(n1053, n1057);
    let n2104: ZB = zb_and(n1068, n1069);
    let n2105: ZB = zb_and(n1088, n1089);
    let n2106: ZB = zb_or(n2104, n2105);
    let n2107: ZB = zb_or(n2103, n2106);
    let n2108: ZB = zb_or(n2102, n2107);
    let n2109: ZB = zb_and(n1107, n1108);
    let n2110: ZB = zb_and(n1116, n1120);
    let n2111: ZB = zb_and(n1131, n1132);
    let n2112: ZB = zb_and(n1147, n1148);
    let n2113: ZB = zb_or(n2111, n2112);
    let n2114: ZB = zb_or(n2110, n2113);
    let n2115: ZB = zb_or(n2109, n2114);
    let n2116: ZB = zb_and(n1166, n1167);
    let n2117: ZB = zb_and(n1175, n1179);
    let n2118: ZB = zb_and(n1190, n1191);
    let n2119: ZB = zb_and(n1206, n1207);
    let n2120: ZB = zb_or(n2118, n2119);
    let n2121: ZB = zb_or(n2117, n2120);
    let n2122: ZB = zb_or(n2116, n2121);
    let n2123: ZB = zb_or(n2115, n2122);
    let n2124: ZB = zb_or(n2108, n2123);
    let n2125: ZB = zb_and(n1238, n1239);
    let n2126: ZB = zb_and(n1247, n1251);
    let n2127: ZB = zb_and(n1262, n1263);
    let n2128: ZB = zb_and(n1282, n1283);
    let n2129: ZB = zb_or(n2127, n2128);
    let n2130: ZB = zb_or(n2126, n2129);
    let n2131: ZB = zb_or(n2125, n2130);
    let n2132: ZB = zb_and(n1301, n1302);
    let n2133: ZB = zb_and(n1310, n1314);
    let n2134: ZB = zb_and(n1325, n1326);
    let n2135: ZB = zb_and(n1341, n1342);
    let n2136: ZB = zb_or(n2134, n2135);
    let n2137: ZB = zb_or(n2133, n2136);
    let n2138: ZB = zb_or(n2132, n2137);
    let n2139: ZB = zb_and(n1360, n1361);
    let n2140: ZB = zb_and(n1369, n1373);
    let n2141: ZB = zb_and(n1384, n1385);
    let n2142: ZB = zb_and(n1400, n1401);
    let n2143: ZB = zb_or(n2141, n2142);
    let n2144: ZB = zb_or(n2140, n2143);
    let n2145: ZB = zb_or(n2139, n2144);
    let n2146: ZB = zb_or(n2138, n2145);
    let n2147: ZB = zb_or(n2131, n2146);
    let n2148: ZB = zb_or(n2124, n2147);
    let n2149: ZB = zsel_b(n2124, n1022, n1216);
    let n2150: ZB = zb_or(n2101, n2148);
    let n2151: ZB = zsel_b(n2101, n774, n2149);
    let n2152: ZB = zsel_b(n2150, n2151, n1419);
    let n2153: ZB = zb_and(n1422, n2150);
    let n2154: ZB = zb_and(n1421, n2150);
    let n2155: ZB = zb_or(n2153, n2154);
    let n2156: ZB = zb_and(n1422, n2155);
    let n2157: ZB = zb_and(n1421, n2155);
    let n2158: ZB = zb_or(n2156, n2157);
    let n2159: ZB = zb_and(n1421, n2158);
    let n2160: ZB = zb_and(n1422, n2158);
    let n2161: ZB = zb_and(n286, n2160);
    let n2162: ZB = zb_and(n287, n2160);
    let n2163: ZB = zb_or(n2161, n2162);
    let n2164: ZB = zb_or(n2159, n2163);
    let n2165: ZB = zb_and(n291, n2164);
    let n2166: ZB = zb_and(n292, n2164);
    let n2167: ZB = zb_and(n1438, n2165);
    let n2168: ZB = zb_and(n1439, n2165);
    let n2169: ZB = zb_or(n2167, n2168);
    let n2170: ZB = zb_and(n1448, n2169);
    let n2171: ZB = zb_and(n1449, n2169);
    let n2172: ZB = zb_or(n2170, n2171);
    let n2173: ZB = zb_and(n1422, n2166);
    let n2174: ZB = zb_and(n1421, n2166);
    let n2175: ZB = zb_or(n2173, n2174);
    let n2176: ZB = zb_and(n1463, n2175);
    let n2177: ZB = zb_and(n1464, n2175);
    let n2178: ZB = zb_and(n1467, n2176);
    let n2179: ZB = zb_and(n851, n2176);
    let n2180: ZB = zb_and(n1470, n2179);
    let n2181: ZB = zb_and(n875, n2179);
    let n2182: ZB = zb_and(n1473, n2178);
    let n2183: ZB = zb_and(n1474, n2178);
    let n2184: ZB = zb_and(n1481, n2180);
    let n2185: ZB = zb_and(n1482, n2180);
    let n2186: ZB = zb_and(n851, n2181);
    let n2187: ZB = zb_or(n2184, n2185);
    let n2188: ZB = zb_or(n2182, n2183);
    let n2189: ZB = zb_or(n2186, n2187);
    let n2190: ZB = zb_or(n2188, n2189);
    let n2191: ZB = zb_and(n1467, n2177);
    let n2192: ZB = zb_and(n851, n2177);
    let n2193: ZB = zb_or(n2191, n2192);
    let n2194: ZB = zb_or(n2190, n2193);
    let n2195: ZB = zb_and(n1510, n2194);
    let n2196: ZB = zb_and(n1509, n2194);
    let n2197: ZB = zb_or(n2195, n2196);
    let n2198: ZB = zb_and(n1517, n2197);
    let n2199: ZB = zb_and(n1518, n2197);
    let n2200: ZB = zb_or(n2198, n2199);
    let n2201: ZB = zb_and(n1422, n2200);
    let n2202: ZB = zb_and(n1421, n2200);
    let n2203: ZB = zb_or(n2201, n2202);
    let n2204: ZB = zb_or(n2172, n2203);
    let n2207: ZB = zb_and(n1481, n2177);
    let n2208: ZB = zb_and(n1482, n2177);
    let n2209: ZB = zb_or(n2207, n2208);
    let n2210: ZB = zb_or(n2190, n2209);
    let n2211: ZB = zb_and(n1603, n2210);
    let n2212: ZB = zb_and(n1602, n2210);
    let n2213: ZB = zb_or(n2211, n2212);
    let n2214: ZB = zb_and(n1517, n2213);
    let n2215: ZB = zb_and(n1518, n2213);
    let n2216: ZB = zb_or(n2214, n2215);
    let n2217: ZB = zb_and(n1614, n2216);
    let n2218: ZB = zb_and(n1613, n2216);
    let n2219: ZB = zb_or(n2217, n2218);
    let n2220: ZB = zb_and(n1614, n2219);
    let n2221: ZB = zb_and(n1613, n2219);
    let n2222: ZB = zb_or(n2220, n2221);
    let n2223: ZB = zb_and(n1613, n2222);
    let n2224: ZB = zb_and(n1614, n2222);
    let n2225: ZB = zb_or(n2223, n2224);
    let n2226: ZB = zb_and(n1613, n2225);
    let n2227: ZB = zb_and(n1614, n2225);
    let n2228: ZB = zb_or(n2226, n2227);
    let n2229: ZB = zb_and(n1422, n2228);
    let n2230: ZB = zb_and(n1421, n2228);
    let n2231: ZB = zb_or(n2229, n2230);
    let n2232: ZB = zb_or(n2172, n2231);
    let n2234: ZB = zb_and(n1473, n2177);
    let n2235: ZB = zb_and(n1474, n2177);
    let n2236: ZB = zb_or(n2234, n2235);
    let n2237: ZB = zb_or(n2190, n2236);
    let n2238: ZB = zb_and(n1659, n2237);
    let n2239: ZB = zb_and(n1658, n2237);
    let n2240: ZB = zb_or(n2238, n2239);
    let n2241: ZB = zb_and(n1517, n2240);
    let n2242: ZB = zb_and(n1518, n2240);
    let n2243: ZB = zb_or(n2241, n2242);
    let n2244: ZB = zb_and(n1670, n2243);
    let n2245: ZB = zb_and(n1669, n2243);
    let n2246: ZB = zb_or(n2244, n2245);
    let n2247: ZB = zb_and(n1670, n2246);
    let n2248: ZB = zb_and(n1669, n2246);
    let n2249: ZB = zb_or(n2247, n2248);
    let n2250: ZB = zb_and(n1669, n2249);
    let n2251: ZB = zb_and(n1670, n2249);
    let n2252: ZB = zb_or(n2250, n2251);
    let n2253: ZB = zb_and(n1669, n2252);
    let n2254: ZB = zb_and(n1670, n2252);
    let n2255: ZB = zb_or(n2253, n2254);
    let n2256: ZB = zb_and(n1422, n2255);
    let n2257: ZB = zb_and(n1421, n2255);
    let n2258: ZB = zb_or(n2256, n2257);
    let n2259: ZB = zb_or(n2172, n2258);
    let n2261: ZB = zb_and(n140, n2203);
    let n2262: ZB = zb_and(r_c294, n2203);
    let n2263: ZB = zb_and(n1530, n2261);
    let n2264: ZB = zb_and(n1531, n2261);
    let n2265: ZB = zb_and(n1534, n2264);
    let n2266: ZB = zb_and(n1533, n2264);
    let n2267: ZB = zb_or(n2265, n2266);
    let n2268: ZB = zb_and(n1534, n2267);
    let n2269: ZB = zb_and(n1533, n2267);
    let n2270: ZB = zb_or(n2268, n2269);
    let n2271: ZB = zb_and(n1533, n2270);
    let n2272: ZB = zb_and(n1534, n2270);
    let n2273: ZB = zb_and(n1537, n2272);
    let n2274: ZB = zb_and(n1536, n2272);
    let n2275: ZB = zb_or(n2273, n2274);
    let n2276: ZB = zb_and(n1537, n2275);
    let n2277: ZB = zb_and(n1536, n2275);
    let n2278: ZB = zb_or(n2276, n2277);
    let n2279: ZB = zb_and(n1536, n2278);
    let n2280: ZB = zb_and(n1537, n2278);
    let n2281: ZB = zb_or(n2279, n2280);
    let n2282: ZB = zb_or(n2271, n2281);
    let n2283: ZB = zb_and(n1541, n2282);
    let n2284: ZB = zb_and(n1540, n2282);
    let n2285: ZB = zb_or(n2283, n2284);
    let n2286: ZB = zb_or(n2263, n2285);
    let n2287: ZB = zb_or(n2262, n2286);
    let n2288: ZB = zb_or(n2172, n2287);
    let n2290: ZB = zb_and(n140, n2231);
    let n2291: ZB = zb_and(r_c294, n2231);
    let n2292: ZB = zb_and(n1530, n2290);
    let n2293: ZB = zb_and(n1531, n2290);
    let n2294: ZB = zb_and(n1534, n2293);
    let n2295: ZB = zb_and(n1533, n2293);
    let n2296: ZB = zb_or(n2294, n2295);
    let n2297: ZB = zb_and(n1534, n2296);
    let n2298: ZB = zb_and(n1533, n2296);
    let n2299: ZB = zb_or(n2297, n2298);
    let n2300: ZB = zb_and(n1533, n2299);
    let n2301: ZB = zb_and(n1534, n2299);
    let n2302: ZB = zb_and(n1537, n2301);
    let n2303: ZB = zb_and(n1536, n2301);
    let n2304: ZB = zb_or(n2302, n2303);
    let n2305: ZB = zb_and(n1537, n2304);
    let n2306: ZB = zb_and(n1536, n2304);
    let n2307: ZB = zb_or(n2305, n2306);
    let n2308: ZB = zb_and(n1536, n2307);
    let n2309: ZB = zb_and(n1537, n2307);
    let n2310: ZB = zb_or(n2308, n2309);
    let n2311: ZB = zb_or(n2300, n2310);
    let n2312: ZB = zb_and(n1541, n2311);
    let n2313: ZB = zb_and(n1540, n2311);
    let n2314: ZB = zb_or(n2312, n2313);
    let n2315: ZB = zb_or(n2292, n2314);
    let n2316: ZB = zb_or(n2291, n2315);
    let n2317: ZB = zb_or(n2172, n2316);
    let n2319: ZB = zb_and(n140, n2258);
    let n2320: ZB = zb_and(r_c294, n2258);
    let n2321: ZB = zb_and(n1530, n2319);
    let n2322: ZB = zb_and(n1531, n2319);
    let n2323: ZB = zb_and(n1534, n2322);
    let n2324: ZB = zb_and(n1533, n2322);
    let n2325: ZB = zb_or(n2323, n2324);
    let n2326: ZB = zb_and(n1534, n2325);
    let n2327: ZB = zb_and(n1533, n2325);
    let n2328: ZB = zb_or(n2326, n2327);
    let n2329: ZB = zb_and(n1533, n2328);
    let n2330: ZB = zb_and(n1534, n2328);
    let n2331: ZB = zb_and(n1537, n2330);
    let n2332: ZB = zb_and(n1536, n2330);
    let n2333: ZB = zb_or(n2331, n2332);
    let n2334: ZB = zb_and(n1537, n2333);
    let n2335: ZB = zb_and(n1536, n2333);
    let n2336: ZB = zb_or(n2334, n2335);
    let n2337: ZB = zb_and(n1536, n2336);
    let n2338: ZB = zb_and(n1537, n2336);
    let n2339: ZB = zb_or(n2337, n2338);
    let n2340: ZB = zb_or(n2329, n2339);
    let n2341: ZB = zb_and(n1541, n2340);
    let n2342: ZB = zb_and(n1540, n2340);
    let n2343: ZB = zb_or(n2341, n2342);
    let n2344: ZB = zb_or(n2321, n2343);
    let n2345: ZB = zb_or(n2320, n2344);
    let n2346: ZB = zb_or(n2172, n2345);
    let n2348: ZB = zb_and(n133, n2203);
    let n2349: ZB = zb_and(r_c293, n2203);
    let n2350: ZB = zb_and(n1514, n2348);
    let n2351: ZB = zb_and(n1549, n2348);
    let n2352: ZB = zb_or(n2350, n2351);
    let n2353: ZB = zb_and(n1551, n2352);
    let n2354: ZB = zb_and(n1552, n2352);
    let n2355: ZB = zb_and(n1553, n2354);
    let n2356: ZB = zb_and(n1554, n2354);
    let n2357: ZB = zb_or(n2355, n2356);
    let n2358: ZB = zb_or(n2353, n2357);
    let n2359: ZB = zb_and(n1558, n2358);
    let n2360: ZB = zb_and(n1557, n2358);
    let n2361: ZB = zb_or(n2359, n2360);
    let n2362: ZB = zb_or(n2349, n2361);
    let n2363: ZB = zb_or(n2172, n2362);
    let n2364: ZB = zb_and(n1845, n2363);
    let n2365: ZB = zb_and(n1846, n2363);
    let n2366: ZB = zb_or(n2364, n2365);
    let n2367: ZB = zb_and(n133, n2231);
    let n2368: ZB = zb_and(r_c293, n2231);
    let n2369: ZB = zb_or(n2367, n2368);
    let n2370: ZB = zb_or(n2172, n2369);
    let n2371: ZB = zb_and(n1845, n2370);
    let n2372: ZB = zb_and(n1846, n2370);
    let n2373: ZB = zb_or(n2371, n2372);
    let n2374: ZB = zb_and(n133, n2258);
    let n2375: ZB = zb_and(r_c293, n2258);
    let n2376: ZB = zb_or(n2374, n2375);
    let n2377: ZB = zb_or(n2172, n2376);
    let n2378: ZB = zb_and(n1845, n2377);
    let n2379: ZB = zb_and(n1846, n2377);
    let n2380: ZB = zb_or(n2378, n2379);
    let n2381: ZB = zb_or(n2348, n2349);
    let n2382: ZB = zb_or(n2172, n2381);
    let n2383: ZB = zb_and(n1845, n2382);
    let n2384: ZB = zb_and(n1846, n2382);
    let n2385: ZB = zb_or(n2383, n2384);
    let n2386: ZB = zb_and(n133, n2287);
    let n2387: ZB = zb_and(r_c293, n2287);
    let n2388: ZB = zb_and(n1514, n2386);
    let n2389: ZB = zb_and(n1549, n2386);
    let n2390: ZB = zb_or(n2388, n2389);
    let n2391: ZB = zb_and(n1551, n2390);
    let n2392: ZB = zb_and(n1552, n2390);
    let n2393: ZB = zb_and(n1553, n2392);
    let n2394: ZB = zb_and(n1554, n2392);
    let n2395: ZB = zb_or(n2393, n2394);
    let n2396: ZB = zb_or(n2391, n2395);
    let n2397: ZB = zb_and(n1558, n2396);
    let n2398: ZB = zb_and(n1557, n2396);
    let n2399: ZB = zb_or(n2397, n2398);
    let n2400: ZB = zb_or(n2387, n2399);
    let n2401: ZB = zb_or(n2172, n2400);
    let n2402: ZB = zb_and(n1845, n2401);
    let n2403: ZB = zb_and(n1846, n2401);
    let n2404: ZB = zb_or(n2402, n2403);
    let n2405: ZB = zb_and(n133, n2316);
    let n2406: ZB = zb_and(r_c293, n2316);
    let n2407: ZB = zb_or(n2405, n2406);
    let n2408: ZB = zb_or(n2172, n2407);
    let n2409: ZB = zb_and(n1845, n2408);
    let n2410: ZB = zb_and(n1846, n2408);
    let n2411: ZB = zb_or(n2409, n2410);
    let n2412: ZB = zb_and(n133, n2345);
    let n2413: ZB = zb_and(r_c293, n2345);
    let n2414: ZB = zb_or(n2412, n2413);
    let n2415: ZB = zb_or(n2172, n2414);
    let n2416: ZB = zb_and(n1845, n2415);
    let n2417: ZB = zb_and(n1846, n2415);
    let n2418: ZB = zb_or(n2416, n2417);
    let n2419: ZB = zb_or(n2386, n2387);
    let n2420: ZB = zb_or(n2172, n2419);
    let n2421: ZB = zb_and(n1845, n2420);
    let n2422: ZB = zb_and(n1846, n2420);
    let n2423: ZB = zb_or(n2421, n2422);
    let n2425: ZN = zsel_n(n227, zn_splat(P8::from_raw(65536i32)), r_c284);
    let n2426: ZN = zsel_n(n226, n2425, r_c284);
    let n2427: ZN = zsel_n(n201, r_c284, n2426);
    let n2428: ZB = zb_not(n314);
    let n2429: ZB = zb_and(n134, n2428);
    let n2430: ZB = zn_lt(n307, zn_splat(P8::from_raw(0i32)));
    let n2431: ZB = zb_and(n315, n2430);
    let n2432: ZB = zb_or(n2429, n2431);
    let n2433: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n306);
    let n2434: ZB = zb_not(n2433);
    let n2435: ZB = zb_and(n2432, n2433);
    let n2436: ZB = zb_and(n2432, n2434);
    let n2437: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n307);
    let n2438: ZB = zb_not(n2437);
    let n2439: ZB = zb_or(n2435, n2436);
    let n2440: ZB = zb_or(n2434, n2438);
    let n2441: ZB = zb_not(n2440);
    let n2442: ZB = zb_and(n2439, n2440);
    let n2443: ZB = zb_and(n2439, n2441);
    let n2444: ZN = zn_add(r_c367, n306);
    let n2445: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n2444);
    let n2446: ZN = zn_flr(n2445);
    let n2447: ZB = zn_gt(n2446, zn_splat(P8::from_raw(0i32)));
    let n2448: ZB = zn_le(n2446, zn_splat(P8::from_raw(0i32)));
    let n2449: ZB = zb_and(n2442, n2447);
    let n2450: ZB = zb_and(n2442, n2448);
    let n2451: ZB = zn_lt(n2446, zn_splat(P8::from_raw(0i32)));
    let n2452: ZB = zn_ge(n2446, zn_splat(P8::from_raw(0i32)));
    let n2453: ZB = zb_and(n2450, n2451);
    let n2454: ZB = zb_and(n2450, n2452);
    let n2455: ZN = zsel_n(n2451, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n2456: ZB = zb_or(n2453, n2454);
    let n2457: ZN = zsel_n(n2447, zn_splat(P8::from_raw(65536i32)), n2455);
    let n2458: ZB = zb_or(n2449, n2456);
    let n2459: ZN = zn_abs(n2446);
    let n2460: ZN = zn_add(n216, n2457);
    let n2461: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n308);
    let n2462: ZB = zn_tile_flag_at(g.cache, g.cart, n2460, n2461, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2463: ZB = zb_not(n2462);
    let n2464: ZB = zb_and(n2458, n2463);
    let n2465: ZB = zb_and(n2458, n2462);
    let n2466: ZB = zb_or(n2464, n2465);
    let n2467: ZB = zb_and(n2463, n2466);
    let n2468: ZB = zb_and(n2462, n2466);
    let n2469: ZB = zb_or(n2467, n2468);
    let n2470: ZB = zb_and(n2463, n2469);
    let n2471: ZB = zb_and(n2462, n2469);
    let n2472: ZN = zn_add(r_c300, n2457);
    let n2473: ZB = zn_le(zn_splat(P8::from_raw(65536i32)), n2459);
    let n2474: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n2459);
    let n2475: ZB = zb_and(n2470, n2473);
    let n2476: ZB = zb_and(n2470, n2474);
    let n2477: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n2472);
    let n2478: ZN = zn_add(n2457, n2477);
    let n2479: ZB = zn_tile_flag_at(g.cache, g.cart, n2478, n2461, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2480: ZB = zb_not(n2479);
    let n2481: ZB = zb_and(n2475, n2480);
    let n2482: ZB = zb_and(n2475, n2479);
    let n2483: ZB = zb_or(n2481, n2482);
    let n2484: ZB = zb_and(n2480, n2483);
    let n2485: ZB = zb_and(n2479, n2483);
    let n2486: ZB = zb_or(n2484, n2485);
    let n2487: ZB = zb_and(n2480, n2486);
    let n2488: ZB = zb_and(n2479, n2486);
    let n2489: ZN = zn_add(n2457, n2472);
    let n2490: ZB = zn_le(zn_splat(P8::from_raw(131072i32)), n2459);
    let n2491: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n2459);
    let n2492: ZB = zb_and(n2487, n2490);
    let n2493: ZB = zb_and(n2487, n2491);
    let n2494: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n2489);
    let n2495: ZN = zn_add(n2457, n2494);
    let n2496: ZB = zn_tile_flag_at(g.cache, g.cart, n2495, n2461, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2497: ZB = zb_not(n2496);
    let n2498: ZB = zb_and(n2492, n2497);
    let n2499: ZB = zb_and(n2492, n2496);
    let n2500: ZB = zb_or(n2498, n2499);
    let n2501: ZB = zb_and(n2497, n2500);
    let n2502: ZB = zb_and(n2496, n2500);
    let n2503: ZB = zb_or(n2501, n2502);
    let n2504: ZB = zb_and(n2497, n2503);
    let n2505: ZB = zb_and(n2496, n2503);
    let n2506: ZN = zn_add(n2457, n2489);
    let n2507: ZB = zn_le(zn_splat(P8::from_raw(196608i32)), n2459);
    let n2508: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n2459);
    let n2509: ZB = zb_and(n2504, n2507);
    let n2510: ZB = zb_and(n2504, n2508);
    let n2511: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n2506);
    let n2512: ZN = zn_add(n2457, n2511);
    let n2513: ZB = zn_tile_flag_at(g.cache, g.cart, n2512, n2461, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2514: ZB = zb_not(n2513);
    let n2515: ZB = zb_and(n2509, n2514);
    let n2516: ZB = zb_and(n2509, n2513);
    let n2517: ZB = zb_or(n2515, n2516);
    let n2518: ZB = zb_and(n2514, n2517);
    let n2519: ZB = zb_and(n2513, n2517);
    let n2520: ZB = zb_or(n2518, n2519);
    let n2521: ZB = zb_and(n2514, n2520);
    let n2522: ZB = zb_and(n2513, n2520);
    let n2523: ZN = zn_add(n2457, n2506);
    let n2524: ZB = zn_le(zn_splat(P8::from_raw(262144i32)), n2459);
    let n2525: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n2459);
    let n2526: ZB = zb_and(n2521, n2524);
    let n2527: ZB = zb_and(n2521, n2525);
    let n2528: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n2523);
    let n2529: ZN = zn_add(n2457, n2528);
    let n2530: ZB = zn_tile_flag_at(g.cache, g.cart, n2529, n2461, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2531: ZB = zb_not(n2530);
    let n2532: ZB = zb_and(n2526, n2531);
    let n2533: ZB = zb_and(n2526, n2530);
    let n2534: ZB = zb_or(n2532, n2533);
    let n2535: ZB = zb_and(n2531, n2534);
    let n2536: ZB = zb_and(n2530, n2534);
    let n2537: ZB = zb_or(n2535, n2536);
    let n2538: ZB = zb_and(n2531, n2537);
    let n2539: ZB = zb_and(n2530, n2537);
    let n2540: ZN = zn_add(n2457, n2523);
    let n2541: ZB = zn_le(zn_splat(P8::from_raw(327680i32)), n2459);
    let n2542: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n2459);
    let n2543: ZB = zb_and(n2538, n2541);
    let n2544: ZB = zb_and(n2538, n2542);
    let n2545: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n2540);
    let n2546: ZN = zn_add(n2457, n2545);
    let n2547: ZB = zn_tile_flag_at(g.cache, g.cart, n2546, n2461, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2548: ZB = zb_not(n2547);
    let n2549: ZB = zb_and(n2543, n2548);
    let n2550: ZB = zb_and(n2543, n2547);
    let n2551: ZB = zb_or(n2549, n2550);
    let n2552: ZB = zb_and(n2548, n2551);
    let n2553: ZB = zb_and(n2547, n2551);
    let n2554: ZB = zb_or(n2552, n2553);
    let n2555: ZB = zb_and(n2548, n2554);
    let n2556: ZB = zb_and(n2547, n2554);
    let n2557: ZN = zn_add(n2457, n2540);
    let n2558: ZB = zn_le(zn_splat(P8::from_raw(393216i32)), n2459);
    let n2559: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n2459);
    let n2560: ZB = zb_and(n2555, n2558);
    let n2561: ZB = zb_and(n2555, n2559);
    let n2562: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n2557);
    let n2563: ZN = zn_add(n2457, n2562);
    let n2564: ZB = zn_tile_flag_at(g.cache, g.cart, n2563, n2461, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2565: ZB = zb_not(n2564);
    let n2566: ZB = zb_and(n2560, n2565);
    let n2567: ZB = zb_and(n2560, n2564);
    let n2568: ZB = zb_or(n2566, n2567);
    let n2569: ZB = zb_and(n2565, n2568);
    let n2570: ZB = zb_and(n2564, n2568);
    let n2571: ZB = zb_or(n2569, n2570);
    let n2572: ZB = zb_and(n2565, n2571);
    let n2573: ZB = zb_and(n2564, n2571);
    let n2574: ZN = zn_add(n2457, n2557);
    let n2575: ZB = zn_le(zn_splat(P8::from_raw(458752i32)), n2459);
    let n2576: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n2459);
    let n2577: ZB = zb_and(n2572, n2575);
    let n2578: ZB = zb_and(n2572, n2576);
    let n2579: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n2574);
    let n2580: ZN = zn_add(n2457, n2579);
    let n2581: ZB = zn_tile_flag_at(g.cache, g.cart, n2580, n2461, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2582: ZB = zb_not(n2581);
    let n2583: ZB = zb_and(n2577, n2582);
    let n2584: ZB = zb_and(n2577, n2581);
    let n2585: ZB = zb_or(n2583, n2584);
    let n2586: ZB = zb_and(n2582, n2585);
    let n2587: ZB = zb_and(n2581, n2585);
    let n2588: ZB = zb_or(n2586, n2587);
    let n2589: ZB = zb_and(n2582, n2588);
    let n2590: ZB = zb_and(n2581, n2588);
    let n2591: ZN = zn_add(n2457, n2574);
    let n2592: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n2459);
    let n2593: ZN = zsel_n(n2581, n2574, n2591);
    let n2594: ZN = zsel_n(n2581, zn_splat(P8::from_raw(0i32)), n306);
    let n2595: ZB = zb_or(n2589, n2590);
    let n2596: ZB = zb_or(n2581, n2592);
    let n2597: ZN = zsel_n(n2576, n2574, n2593);
    let n2598: ZN = zsel_n(n2576, n306, n2594);
    let n2599: ZB = zb_or(n2578, n2595);
    let n2600: ZB = zb_or(n2576, n2596);
    let n2601: ZN = zsel_n(n2564, n2557, n2597);
    let n2602: ZN = zsel_n(n2564, zn_splat(P8::from_raw(0i32)), n2598);
    let n2603: ZB = zb_or(n2573, n2599);
    let n2604: ZB = zb_or(n2564, n2600);
    let n2605: ZN = zsel_n(n2559, n2557, n2601);
    let n2606: ZN = zsel_n(n2559, n306, n2602);
    let n2607: ZB = zb_or(n2561, n2603);
    let n2608: ZB = zb_or(n2559, n2604);
    let n2609: ZN = zsel_n(n2547, n2540, n2605);
    let n2610: ZN = zsel_n(n2547, zn_splat(P8::from_raw(0i32)), n2606);
    let n2611: ZB = zb_or(n2556, n2607);
    let n2612: ZB = zb_or(n2547, n2608);
    let n2613: ZN = zsel_n(n2542, n2540, n2609);
    let n2614: ZN = zsel_n(n2542, n306, n2610);
    let n2615: ZB = zb_or(n2544, n2611);
    let n2616: ZB = zb_or(n2542, n2612);
    let n2617: ZN = zsel_n(n2530, n2523, n2613);
    let n2618: ZN = zsel_n(n2530, zn_splat(P8::from_raw(0i32)), n2614);
    let n2619: ZB = zb_or(n2539, n2615);
    let n2620: ZB = zb_or(n2530, n2616);
    let n2621: ZN = zsel_n(n2525, n2523, n2617);
    let n2622: ZN = zsel_n(n2525, n306, n2618);
    let n2623: ZB = zb_or(n2527, n2619);
    let n2624: ZB = zb_or(n2525, n2620);
    let n2625: ZN = zsel_n(n2513, n2506, n2621);
    let n2626: ZN = zsel_n(n2513, zn_splat(P8::from_raw(0i32)), n2622);
    let n2627: ZB = zb_or(n2522, n2623);
    let n2628: ZB = zb_or(n2513, n2624);
    let n2629: ZN = zsel_n(n2508, n2506, n2625);
    let n2630: ZN = zsel_n(n2508, n306, n2626);
    let n2631: ZB = zb_or(n2510, n2627);
    let n2632: ZB = zb_or(n2508, n2628);
    let n2633: ZN = zsel_n(n2496, n2489, n2629);
    let n2634: ZN = zsel_n(n2496, zn_splat(P8::from_raw(0i32)), n2630);
    let n2635: ZB = zb_or(n2505, n2631);
    let n2636: ZB = zb_or(n2496, n2632);
    let n2637: ZN = zsel_n(n2491, n2489, n2633);
    let n2638: ZN = zsel_n(n2491, n306, n2634);
    let n2639: ZB = zb_or(n2493, n2635);
    let n2640: ZB = zb_or(n2491, n2636);
    let n2641: ZN = zsel_n(n2479, n2472, n2637);
    let n2642: ZN = zsel_n(n2479, zn_splat(P8::from_raw(0i32)), n2638);
    let n2643: ZB = zb_or(n2488, n2639);
    let n2644: ZB = zb_or(n2479, n2640);
    let n2645: ZN = zsel_n(n2474, n2472, n2641);
    let n2646: ZN = zsel_n(n2474, n306, n2642);
    let n2647: ZB = zb_or(n2476, n2643);
    let n2648: ZB = zb_or(n2474, n2644);
    let n2649: ZN = zsel_n(n2462, r_c300, n2645);
    let n2650: ZN = zsel_n(n2462, zn_splat(P8::from_raw(0i32)), n2646);
    let n2651: ZB = zb_or(n2471, n2647);
    let n2652: ZB = zb_or(n2462, n2648);
    let n2653: ZN = zn_add(r_c368, n307);
    let n2654: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n2653);
    let n2655: ZN = zn_flr(n2654);
    let n2656: ZB = zn_gt(n2655, zn_splat(P8::from_raw(0i32)));
    let n2657: ZB = zn_le(n2655, zn_splat(P8::from_raw(0i32)));
    let n2658: ZB = zb_and(n2651, n2656);
    let n2659: ZB = zb_and(n2651, n2657);
    let n2660: ZB = zn_lt(n2655, zn_splat(P8::from_raw(0i32)));
    let n2661: ZB = zn_ge(n2655, zn_splat(P8::from_raw(0i32)));
    let n2662: ZB = zb_and(n2659, n2660);
    let n2663: ZB = zb_and(n2659, n2661);
    let n2664: ZN = zsel_n(n2660, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n2665: ZB = zb_or(n2662, n2663);
    let n2666: ZN = zsel_n(n2656, zn_splat(P8::from_raw(65536i32)), n2664);
    let n2667: ZB = zb_or(n2658, n2665);
    let n2668: ZN = zn_abs(n2655);
    let n2669: ZB = zn_gt(n2666, zn_splat(P8::from_raw(0i32)));
    let n2670: ZB = zn_le(n2666, zn_splat(P8::from_raw(0i32)));
    let n2671: ZB = zb_and(n2667, n2669);
    let n2672: ZB = zb_and(n2667, n2670);
    let n2673: ZB = zb_or(n2671, n2672);
    let n2674: ZB = zb_and(n2669, n2673);
    let n2675: ZB = zb_and(n2670, n2673);
    let n2676: ZB = zb_or(n2674, n2675);
    let n2677: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n2649);
    let n2678: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n2677);
    let n2679: ZN = zn_add(n308, n2666);
    let n2680: ZB = zn_tile_flag_at(g.cache, g.cart, n2678, n2679, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2681: ZB = zb_not(n2680);
    let n2682: ZB = zb_and(n2676, n2681);
    let n2683: ZB = zb_and(n2676, n2680);
    let n2684: ZB = zb_or(n2682, n2683);
    let n2685: ZB = zb_and(n2681, n2684);
    let n2686: ZB = zb_and(n2680, n2684);
    let n2687: ZB = zb_or(n2685, n2686);
    let n2688: ZB = zb_and(n2681, n2687);
    let n2689: ZB = zb_and(n2680, n2687);
    let n2690: ZN = zn_add(n305, n2666);
    let n2691: ZB = zn_le(zn_splat(P8::from_raw(65536i32)), n2668);
    let n2692: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n2668);
    let n2693: ZB = zb_and(n2688, n2691);
    let n2694: ZB = zb_and(n2688, n2692);
    let n2695: ZB = zb_and(n2669, n2693);
    let n2696: ZB = zb_and(n2670, n2693);
    let n2697: ZB = zb_or(n2695, n2696);
    let n2698: ZB = zb_and(n2669, n2697);
    let n2699: ZB = zb_and(n2670, n2697);
    let n2700: ZB = zb_or(n2698, n2699);
    let n2701: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n2690);
    let n2702: ZN = zn_add(n2666, n2701);
    let n2703: ZB = zn_tile_flag_at(g.cache, g.cart, n2678, n2702, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2704: ZB = zb_not(n2703);
    let n2705: ZB = zb_and(n2700, n2704);
    let n2706: ZB = zb_and(n2700, n2703);
    let n2707: ZB = zb_or(n2705, n2706);
    let n2708: ZB = zb_and(n2704, n2707);
    let n2709: ZB = zb_and(n2703, n2707);
    let n2710: ZB = zb_or(n2708, n2709);
    let n2711: ZB = zb_and(n2704, n2710);
    let n2712: ZB = zb_and(n2703, n2710);
    let n2713: ZN = zn_add(n2666, n2690);
    let n2714: ZB = zn_le(zn_splat(P8::from_raw(131072i32)), n2668);
    let n2715: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n2668);
    let n2716: ZB = zb_and(n2711, n2714);
    let n2717: ZB = zb_and(n2711, n2715);
    let n2718: ZB = zb_and(n2669, n2716);
    let n2719: ZB = zb_and(n2670, n2716);
    let n2720: ZB = zb_or(n2718, n2719);
    let n2721: ZB = zb_and(n2669, n2720);
    let n2722: ZB = zb_and(n2670, n2720);
    let n2723: ZB = zb_or(n2721, n2722);
    let n2724: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n2713);
    let n2725: ZN = zn_add(n2666, n2724);
    let n2726: ZB = zn_tile_flag_at(g.cache, g.cart, n2678, n2725, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2727: ZB = zb_not(n2726);
    let n2728: ZB = zb_and(n2723, n2727);
    let n2729: ZB = zb_and(n2723, n2726);
    let n2730: ZB = zb_or(n2728, n2729);
    let n2731: ZB = zb_and(n2727, n2730);
    let n2732: ZB = zb_and(n2726, n2730);
    let n2733: ZB = zb_or(n2731, n2732);
    let n2734: ZB = zb_and(n2727, n2733);
    let n2735: ZB = zb_and(n2726, n2733);
    let n2736: ZN = zn_add(n2666, n2713);
    let n2737: ZB = zn_le(zn_splat(P8::from_raw(196608i32)), n2668);
    let n2738: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n2668);
    let n2739: ZB = zb_and(n2734, n2737);
    let n2740: ZB = zb_and(n2734, n2738);
    let n2741: ZB = zb_and(n2669, n2739);
    let n2742: ZB = zb_and(n2670, n2739);
    let n2743: ZB = zb_or(n2741, n2742);
    let n2744: ZB = zb_and(n2669, n2743);
    let n2745: ZB = zb_and(n2670, n2743);
    let n2746: ZB = zb_or(n2744, n2745);
    let n2747: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n2736);
    let n2748: ZN = zn_add(n2666, n2747);
    let n2749: ZB = zn_tile_flag_at(g.cache, g.cart, n2678, n2748, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2750: ZB = zb_not(n2749);
    let n2751: ZB = zb_and(n2746, n2750);
    let n2752: ZB = zb_and(n2746, n2749);
    let n2753: ZB = zb_or(n2751, n2752);
    let n2754: ZB = zb_and(n2750, n2753);
    let n2755: ZB = zb_and(n2749, n2753);
    let n2756: ZB = zb_or(n2754, n2755);
    let n2757: ZB = zb_and(n2750, n2756);
    let n2758: ZB = zb_and(n2749, n2756);
    let n2759: ZN = zn_add(n2666, n2736);
    let n2760: ZB = zn_le(zn_splat(P8::from_raw(262144i32)), n2668);
    let n2761: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n2668);
    let n2762: ZB = zb_and(n2757, n2760);
    let n2763: ZB = zb_and(n2757, n2761);
    let n2764: ZB = zb_and(n2669, n2762);
    let n2765: ZB = zb_and(n2670, n2762);
    let n2766: ZB = zb_or(n2764, n2765);
    let n2767: ZB = zb_and(n2669, n2766);
    let n2768: ZB = zb_and(n2670, n2766);
    let n2769: ZB = zb_or(n2767, n2768);
    let n2770: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n2759);
    let n2771: ZN = zn_add(n2666, n2770);
    let n2772: ZB = zn_tile_flag_at(g.cache, g.cart, n2678, n2771, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2773: ZB = zb_not(n2772);
    let n2774: ZB = zb_and(n2769, n2773);
    let n2775: ZB = zb_and(n2769, n2772);
    let n2776: ZB = zb_or(n2774, n2775);
    let n2777: ZB = zb_and(n2773, n2776);
    let n2778: ZB = zb_and(n2772, n2776);
    let n2779: ZB = zb_or(n2777, n2778);
    let n2780: ZB = zb_and(n2773, n2779);
    let n2781: ZB = zb_and(n2772, n2779);
    let n2782: ZN = zn_add(n2666, n2759);
    let n2783: ZB = zn_le(zn_splat(P8::from_raw(327680i32)), n2668);
    let n2784: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n2668);
    let n2785: ZB = zb_and(n2780, n2783);
    let n2786: ZB = zb_and(n2780, n2784);
    let n2787: ZB = zb_and(n2669, n2785);
    let n2788: ZB = zb_and(n2670, n2785);
    let n2789: ZB = zb_or(n2787, n2788);
    let n2790: ZB = zb_and(n2669, n2789);
    let n2791: ZB = zb_and(n2670, n2789);
    let n2792: ZB = zb_or(n2790, n2791);
    let n2793: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n2782);
    let n2794: ZN = zn_add(n2666, n2793);
    let n2795: ZB = zn_tile_flag_at(g.cache, g.cart, n2678, n2794, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2796: ZB = zb_not(n2795);
    let n2797: ZB = zb_and(n2792, n2796);
    let n2798: ZB = zb_and(n2792, n2795);
    let n2799: ZB = zb_or(n2797, n2798);
    let n2800: ZB = zb_and(n2796, n2799);
    let n2801: ZB = zb_and(n2795, n2799);
    let n2802: ZB = zb_or(n2800, n2801);
    let n2803: ZB = zb_and(n2796, n2802);
    let n2804: ZB = zb_and(n2795, n2802);
    let n2805: ZN = zn_add(n2666, n2782);
    let n2806: ZB = zn_le(zn_splat(P8::from_raw(393216i32)), n2668);
    let n2807: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n2668);
    let n2808: ZB = zb_and(n2803, n2806);
    let n2809: ZB = zb_and(n2803, n2807);
    let n2810: ZB = zb_and(n2669, n2808);
    let n2811: ZB = zb_and(n2670, n2808);
    let n2812: ZB = zb_or(n2810, n2811);
    let n2813: ZB = zb_and(n2669, n2812);
    let n2814: ZB = zb_and(n2670, n2812);
    let n2815: ZB = zb_or(n2813, n2814);
    let n2816: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n2805);
    let n2817: ZN = zn_add(n2666, n2816);
    let n2818: ZB = zn_tile_flag_at(g.cache, g.cart, n2678, n2817, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2819: ZB = zb_not(n2818);
    let n2820: ZB = zb_and(n2815, n2819);
    let n2821: ZB = zb_and(n2815, n2818);
    let n2822: ZB = zb_or(n2820, n2821);
    let n2823: ZB = zb_and(n2819, n2822);
    let n2824: ZB = zb_and(n2818, n2822);
    let n2825: ZB = zb_or(n2823, n2824);
    let n2826: ZB = zb_and(n2819, n2825);
    let n2827: ZB = zb_and(n2818, n2825);
    let n2828: ZN = zn_add(n2666, n2805);
    let n2829: ZB = zn_le(zn_splat(P8::from_raw(458752i32)), n2668);
    let n2830: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n2668);
    let n2831: ZB = zb_and(n2826, n2829);
    let n2832: ZB = zb_and(n2826, n2830);
    let n2833: ZB = zb_and(n2669, n2831);
    let n2834: ZB = zb_and(n2670, n2831);
    let n2835: ZB = zb_or(n2833, n2834);
    let n2836: ZB = zb_and(n2669, n2835);
    let n2837: ZB = zb_and(n2670, n2835);
    let n2838: ZB = zb_or(n2836, n2837);
    let n2839: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n2828);
    let n2840: ZN = zn_add(n2666, n2839);
    let n2841: ZB = zn_tile_flag_at(g.cache, g.cart, n2678, n2840, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2842: ZB = zb_not(n2841);
    let n2843: ZB = zb_and(n2838, n2842);
    let n2844: ZB = zb_and(n2838, n2841);
    let n2845: ZB = zb_or(n2843, n2844);
    let n2846: ZB = zb_and(n2842, n2845);
    let n2847: ZB = zb_and(n2841, n2845);
    let n2848: ZB = zb_or(n2846, n2847);
    let n2849: ZB = zb_and(n2842, n2848);
    let n2850: ZB = zb_and(n2841, n2848);
    let n2851: ZN = zn_add(n2666, n2828);
    let n2852: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n2668);
    let n2853: ZB = zb_and(n2652, n2852);
    let n2854: ZN = zsel_n(n2841, n2828, n2851);
    let n2855: ZN = zsel_n(n2841, zn_splat(P8::from_raw(0i32)), n307);
    let n2856: ZB = zb_or(n2849, n2850);
    let n2857: ZB = zsel_b(n2841, n2652, n2853);
    let n2858: ZN = zsel_n(n2830, n2828, n2854);
    let n2859: ZN = zsel_n(n2830, n307, n2855);
    let n2860: ZB = zb_or(n2832, n2856);
    let n2861: ZB = zsel_b(n2830, n2652, n2857);
    let n2862: ZN = zsel_n(n2818, n2805, n2858);
    let n2863: ZN = zsel_n(n2818, zn_splat(P8::from_raw(0i32)), n2859);
    let n2864: ZB = zb_or(n2827, n2860);
    let n2865: ZB = zsel_b(n2818, n2652, n2861);
    let n2866: ZN = zsel_n(n2807, n2805, n2862);
    let n2867: ZN = zsel_n(n2807, n307, n2863);
    let n2868: ZB = zb_or(n2809, n2864);
    let n2869: ZB = zsel_b(n2807, n2652, n2865);
    let n2870: ZN = zsel_n(n2795, n2782, n2866);
    let n2871: ZN = zsel_n(n2795, zn_splat(P8::from_raw(0i32)), n2867);
    let n2872: ZB = zb_or(n2804, n2868);
    let n2873: ZB = zsel_b(n2795, n2652, n2869);
    let n2874: ZN = zsel_n(n2784, n2782, n2870);
    let n2875: ZN = zsel_n(n2784, n307, n2871);
    let n2876: ZB = zb_or(n2786, n2872);
    let n2877: ZB = zsel_b(n2784, n2652, n2873);
    let n2878: ZN = zsel_n(n2772, n2759, n2874);
    let n2879: ZN = zsel_n(n2772, zn_splat(P8::from_raw(0i32)), n2875);
    let n2880: ZB = zb_or(n2781, n2876);
    let n2881: ZB = zsel_b(n2772, n2652, n2877);
    let n2882: ZN = zsel_n(n2761, n2759, n2878);
    let n2883: ZN = zsel_n(n2761, n307, n2879);
    let n2884: ZB = zb_or(n2763, n2880);
    let n2885: ZB = zsel_b(n2761, n2652, n2881);
    let n2886: ZN = zsel_n(n2749, n2736, n2882);
    let n2887: ZN = zsel_n(n2749, zn_splat(P8::from_raw(0i32)), n2883);
    let n2888: ZB = zb_or(n2758, n2884);
    let n2889: ZB = zsel_b(n2749, n2652, n2885);
    let n2890: ZN = zsel_n(n2738, n2736, n2886);
    let n2891: ZN = zsel_n(n2738, n307, n2887);
    let n2892: ZB = zb_or(n2740, n2888);
    let n2893: ZB = zsel_b(n2738, n2652, n2889);
    let n2894: ZN = zsel_n(n2726, n2713, n2890);
    let n2895: ZN = zsel_n(n2726, zn_splat(P8::from_raw(0i32)), n2891);
    let n2896: ZB = zb_or(n2735, n2892);
    let n2897: ZB = zsel_b(n2726, n2652, n2893);
    let n2898: ZN = zsel_n(n2715, n2713, n2894);
    let n2899: ZN = zsel_n(n2715, n307, n2895);
    let n2900: ZB = zb_or(n2717, n2896);
    let n2901: ZB = zsel_b(n2715, n2652, n2897);
    let n2902: ZN = zsel_n(n2703, n2690, n2898);
    let n2903: ZN = zsel_n(n2703, zn_splat(P8::from_raw(0i32)), n2899);
    let n2904: ZB = zb_or(n2712, n2900);
    let n2905: ZB = zsel_b(n2703, n2652, n2901);
    let n2906: ZN = zsel_n(n2692, n2690, n2902);
    let n2907: ZN = zsel_n(n2692, n307, n2903);
    let n2908: ZB = zb_or(n2694, n2904);
    let n2909: ZB = zsel_b(n2692, n2652, n2905);
    let n2910: ZN = zsel_n(n2680, n305, n2906);
    let n2911: ZN = zsel_n(n2680, zn_splat(P8::from_raw(0i32)), n2907);
    let n2912: ZB = zb_or(n2689, n2908);
    let n2913: ZB = zsel_b(n2680, n2652, n2909);
    let n2914: ZN = zsel_n(n2440, n2649, r_c300);
    let n2915: ZN = zsel_n(n2440, n2910, n305);
    let n2916: ZN = zsel_n(n2440, n2650, n306);
    let n2917: ZN = zsel_n(n2440, n2911, n307);
    let n2918: ZB = zb_or(n2443, n2912);
    let n2919: ZB = zb_or(n2441, n2913);
    let n2920: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n2914);
    let n2921: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n2915);
    let n2922: ZN = zn_div(n2920, zn_splat(P8::from_raw(524288i32)));
    let n2923: ZN = zn_flr(n2922);
    let n2924: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n2923);
    let n2925: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n2920);
    let n2926: ZN = zn_sub(n2925, zn_splat(P8::from_raw(65536i32)));
    let n2927: ZN = zn_div(n2926, zn_splat(P8::from_raw(524288i32)));
    let n2928: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n2927);
    let n2929: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n2924);
    let n2930: ZB = zn_le(n2929, n2928);
    let n2931: ZB = zn_gt(n2929, n2928);
    let n2932: ZB = zb_and(n2918, n2930);
    let n2933: ZB = zb_and(n2918, n2931);
    let n2934: ZN = zn_div(n2921, zn_splat(P8::from_raw(524288i32)));
    let n2935: ZN = zn_flr(n2934);
    let n2936: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n2935);
    let n2937: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n2921);
    let n2938: ZN = zn_sub(n2937, zn_splat(P8::from_raw(65536i32)));
    let n2939: ZN = zn_div(n2938, zn_splat(P8::from_raw(524288i32)));
    let n2940: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n2939);
    let n2941: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n2936);
    let n2942: ZB = zn_le(n2941, n2940);
    let n2943: ZB = zn_gt(n2941, n2940);
    let n2944: ZB = zb_and(n2932, n2942);
    let n2945: ZB = zb_and(n2932, n2943);
    let n2946: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n2929);
    let n2947: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n2941);
    let n2948: ZN = zn_mget(g.cart, n2946, n2947);
    let n2949: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n2948);
    let n2950: ZB = zb_not(n2949);
    let n2951: ZB = zb_and(n2944, n2949);
    let n2952: ZB = zb_and(n2944, n2950);
    let n2953: ZN = zn_rem(n2938, zn_splat(P8::from_raw(524288i32)));
    let n2954: ZB = zn_ge(n2953, zn_splat(P8::from_raw(393216i32)));
    let n2955: ZB = zn_lt(n2953, zn_splat(P8::from_raw(393216i32)));
    let n2956: ZB = zb_and(n2951, n2955);
    let n2957: ZB = zb_and(n2951, n2954);
    let n2958: ZN = zn_mul(n2941, zn_splat(P8::from_raw(524288i32)));
    let n2959: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n2958);
    let n2960: ZB = zn_eq(n2937, n2959);
    let n2961: ZB = zb_or(n2956, n2957);
    let n2962: ZB = zb_or(n2954, n2960);
    let n2963: ZB = zb_or(n2952, n2961);
    let n2964: ZB = zb_and(n2949, n2962);
    let n2965: ZB = zb_not(n2964);
    let n2966: ZB = zb_and(n2963, n2964);
    let n2967: ZB = zb_and(n2963, n2965);
    let n2968: ZB = zn_ge(n2917, zn_splat(P8::from_raw(0i32)));
    let n2969: ZB = zb_or(n2966, n2967);
    let n2970: ZB = zb_and(n2964, n2968);
    let n2971: ZB = zb_not(n2970);
    let n2972: ZB = zb_and(n2969, n2970);
    let n2973: ZB = zb_and(n2969, n2971);
    let n2974: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n2948);
    let n2975: ZB = zb_not(n2974);
    let n2976: ZB = zb_and(n2973, n2974);
    let n2977: ZB = zb_and(n2973, n2975);
    let n2978: ZN = zn_rem(n2921, zn_splat(P8::from_raw(524288i32)));
    let n2979: ZB = zn_le(n2978, zn_splat(P8::from_raw(131072i32)));
    let n2980: ZB = zb_or(n2976, n2977);
    let n2981: ZB = zb_and(n2974, n2979);
    let n2982: ZB = zb_not(n2981);
    let n2983: ZB = zb_and(n2980, n2981);
    let n2984: ZB = zb_and(n2980, n2982);
    let n2985: ZB = zn_le(n2917, zn_splat(P8::from_raw(0i32)));
    let n2986: ZB = zb_or(n2983, n2984);
    let n2987: ZB = zb_and(n2981, n2985);
    let n2988: ZB = zb_not(n2987);
    let n2989: ZB = zb_and(n2986, n2987);
    let n2990: ZB = zb_and(n2986, n2988);
    let n2991: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n2948);
    let n2992: ZB = zb_not(n2991);
    let n2993: ZB = zb_and(n2990, n2991);
    let n2994: ZB = zb_and(n2990, n2992);
    let n2995: ZN = zn_rem(n2920, zn_splat(P8::from_raw(524288i32)));
    let n2996: ZB = zn_le(n2995, zn_splat(P8::from_raw(131072i32)));
    let n2997: ZB = zb_or(n2993, n2994);
    let n2998: ZB = zb_and(n2991, n2996);
    let n2999: ZB = zb_not(n2998);
    let n3000: ZB = zb_and(n2997, n2998);
    let n3001: ZB = zb_and(n2997, n2999);
    let n3002: ZB = zn_le(n2916, zn_splat(P8::from_raw(0i32)));
    let n3003: ZB = zb_or(n3000, n3001);
    let n3004: ZB = zb_and(n2998, n3002);
    let n3005: ZB = zb_not(n3004);
    let n3006: ZB = zb_and(n3003, n3004);
    let n3007: ZB = zb_and(n3003, n3005);
    let n3008: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n2948);
    let n3009: ZB = zb_not(n3008);
    let n3010: ZB = zb_and(n3007, n3008);
    let n3011: ZB = zb_and(n3007, n3009);
    let n3012: ZN = zn_rem(n2926, zn_splat(P8::from_raw(524288i32)));
    let n3013: ZB = zn_ge(n3012, zn_splat(P8::from_raw(393216i32)));
    let n3014: ZB = zn_lt(n3012, zn_splat(P8::from_raw(393216i32)));
    let n3015: ZB = zb_and(n3010, n3014);
    let n3016: ZB = zb_and(n3010, n3013);
    let n3017: ZN = zn_mul(n2929, zn_splat(P8::from_raw(524288i32)));
    let n3018: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n3017);
    let n3019: ZB = zn_eq(n2925, n3018);
    let n3020: ZB = zb_or(n3015, n3016);
    let n3021: ZB = zb_or(n3013, n3019);
    let n3022: ZB = zb_or(n3011, n3020);
    let n3023: ZB = zb_and(n3008, n3021);
    let n3024: ZB = zb_not(n3023);
    let n3025: ZB = zb_and(n3022, n3023);
    let n3026: ZB = zb_and(n3022, n3024);
    let n3027: ZB = zn_ge(n2916, zn_splat(P8::from_raw(0i32)));
    let n3028: ZB = zb_or(n3025, n3026);
    let n3029: ZB = zb_and(n3023, n3027);
    let n3030: ZB = zb_not(n3029);
    let n3031: ZB = zb_and(n3028, n3029);
    let n3032: ZB = zb_and(n3028, n3030);
    let n3033: ZB = zb_or(n3006, n3031);
    let n3034: ZB = zb_or(n2989, n3033);
    let n3035: ZB = zb_or(n2972, n3034);
    let n3036: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n2936);
    let n3037: ZB = zn_le(n3036, n2940);
    let n3038: ZB = zn_gt(n3036, n2940);
    let n3039: ZB = zb_and(n3032, n3037);
    let n3040: ZB = zb_and(n3032, n3038);
    let n3041: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n3036);
    let n3042: ZN = zn_mget(g.cart, n2946, n3041);
    let n3043: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n3042);
    let n3044: ZB = zb_not(n3043);
    let n3045: ZB = zb_and(n3039, n3043);
    let n3046: ZB = zb_and(n3039, n3044);
    let n3047: ZB = zb_and(n2955, n3045);
    let n3048: ZB = zb_and(n2954, n3045);
    let n3049: ZN = zn_mul(n3036, zn_splat(P8::from_raw(524288i32)));
    let n3050: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n3049);
    let n3051: ZB = zn_eq(n2937, n3050);
    let n3052: ZB = zb_or(n3047, n3048);
    let n3053: ZB = zb_or(n2954, n3051);
    let n3054: ZB = zb_or(n3046, n3052);
    let n3055: ZB = zb_and(n3043, n3053);
    let n3056: ZB = zb_not(n3055);
    let n3057: ZB = zb_and(n3054, n3055);
    let n3058: ZB = zb_and(n3054, n3056);
    let n3059: ZB = zb_or(n3057, n3058);
    let n3060: ZB = zb_and(n2968, n3055);
    let n3061: ZB = zb_not(n3060);
    let n3062: ZB = zb_and(n3059, n3060);
    let n3063: ZB = zb_and(n3059, n3061);
    let n3064: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n3042);
    let n3065: ZB = zb_not(n3064);
    let n3066: ZB = zb_and(n3063, n3064);
    let n3067: ZB = zb_and(n3063, n3065);
    let n3068: ZB = zb_or(n3066, n3067);
    let n3069: ZB = zb_and(n2979, n3064);
    let n3070: ZB = zb_not(n3069);
    let n3071: ZB = zb_and(n3068, n3069);
    let n3072: ZB = zb_and(n3068, n3070);
    let n3073: ZB = zb_or(n3071, n3072);
    let n3074: ZB = zb_and(n2985, n3069);
    let n3075: ZB = zb_not(n3074);
    let n3076: ZB = zb_and(n3073, n3074);
    let n3077: ZB = zb_and(n3073, n3075);
    let n3078: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n3042);
    let n3079: ZB = zb_not(n3078);
    let n3080: ZB = zb_and(n3077, n3078);
    let n3081: ZB = zb_and(n3077, n3079);
    let n3082: ZB = zb_or(n3080, n3081);
    let n3083: ZB = zb_and(n2996, n3078);
    let n3084: ZB = zb_not(n3083);
    let n3085: ZB = zb_and(n3082, n3083);
    let n3086: ZB = zb_and(n3082, n3084);
    let n3087: ZB = zb_or(n3085, n3086);
    let n3088: ZB = zb_and(n3002, n3083);
    let n3089: ZB = zb_not(n3088);
    let n3090: ZB = zb_and(n3087, n3088);
    let n3091: ZB = zb_and(n3087, n3089);
    let n3092: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n3042);
    let n3093: ZB = zb_not(n3092);
    let n3094: ZB = zb_and(n3091, n3092);
    let n3095: ZB = zb_and(n3091, n3093);
    let n3096: ZB = zb_and(n3014, n3094);
    let n3097: ZB = zb_and(n3013, n3094);
    let n3098: ZB = zb_or(n3096, n3097);
    let n3099: ZB = zb_or(n3095, n3098);
    let n3100: ZB = zb_and(n3021, n3092);
    let n3101: ZB = zb_not(n3100);
    let n3102: ZB = zb_and(n3099, n3100);
    let n3103: ZB = zb_and(n3099, n3101);
    let n3104: ZB = zb_or(n3102, n3103);
    let n3105: ZB = zb_and(n3027, n3100);
    let n3106: ZB = zb_not(n3105);
    let n3107: ZB = zb_and(n3104, n3105);
    let n3108: ZB = zb_and(n3104, n3106);
    let n3109: ZB = zb_or(n3090, n3107);
    let n3110: ZB = zb_or(n3076, n3109);
    let n3111: ZB = zb_or(n3062, n3110);
    let n3112: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n2936);
    let n3113: ZB = zn_le(n3112, n2940);
    let n3114: ZB = zn_gt(n3112, n2940);
    let n3115: ZB = zb_and(n3108, n3113);
    let n3116: ZB = zb_and(n3108, n3114);
    let n3117: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n3112);
    let n3118: ZN = zn_mget(g.cart, n2946, n3117);
    let n3119: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n3118);
    let n3120: ZB = zb_not(n3119);
    let n3121: ZB = zb_and(n3115, n3119);
    let n3122: ZB = zb_and(n3115, n3120);
    let n3123: ZB = zb_and(n2955, n3121);
    let n3124: ZB = zb_and(n2954, n3121);
    let n3125: ZN = zn_mul(n3112, zn_splat(P8::from_raw(524288i32)));
    let n3126: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n3125);
    let n3127: ZB = zn_eq(n2937, n3126);
    let n3128: ZB = zb_or(n3123, n3124);
    let n3129: ZB = zb_or(n2954, n3127);
    let n3130: ZB = zb_or(n3122, n3128);
    let n3131: ZB = zb_and(n3119, n3129);
    let n3132: ZB = zb_not(n3131);
    let n3133: ZB = zb_and(n3130, n3131);
    let n3134: ZB = zb_and(n3130, n3132);
    let n3135: ZB = zb_or(n3133, n3134);
    let n3136: ZB = zb_and(n2968, n3131);
    let n3137: ZB = zb_not(n3136);
    let n3138: ZB = zb_and(n3135, n3136);
    let n3139: ZB = zb_and(n3135, n3137);
    let n3140: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n3118);
    let n3141: ZB = zb_not(n3140);
    let n3142: ZB = zb_and(n3139, n3140);
    let n3143: ZB = zb_and(n3139, n3141);
    let n3144: ZB = zb_or(n3142, n3143);
    let n3145: ZB = zb_and(n2979, n3140);
    let n3146: ZB = zb_not(n3145);
    let n3147: ZB = zb_and(n3144, n3145);
    let n3148: ZB = zb_and(n3144, n3146);
    let n3149: ZB = zb_or(n3147, n3148);
    let n3150: ZB = zb_and(n2985, n3145);
    let n3151: ZB = zb_not(n3150);
    let n3152: ZB = zb_and(n3149, n3150);
    let n3153: ZB = zb_and(n3149, n3151);
    let n3154: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n3118);
    let n3155: ZB = zb_not(n3154);
    let n3156: ZB = zb_and(n3153, n3154);
    let n3157: ZB = zb_and(n3153, n3155);
    let n3158: ZB = zb_or(n3156, n3157);
    let n3159: ZB = zb_and(n2996, n3154);
    let n3160: ZB = zb_not(n3159);
    let n3161: ZB = zb_and(n3158, n3159);
    let n3162: ZB = zb_and(n3158, n3160);
    let n3163: ZB = zb_or(n3161, n3162);
    let n3164: ZB = zb_and(n3002, n3159);
    let n3165: ZB = zb_not(n3164);
    let n3166: ZB = zb_and(n3163, n3164);
    let n3167: ZB = zb_and(n3163, n3165);
    let n3168: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n3118);
    let n3169: ZB = zb_not(n3168);
    let n3170: ZB = zb_and(n3167, n3168);
    let n3171: ZB = zb_and(n3167, n3169);
    let n3172: ZB = zb_and(n3014, n3170);
    let n3173: ZB = zb_and(n3013, n3170);
    let n3174: ZB = zb_or(n3172, n3173);
    let n3175: ZB = zb_or(n3171, n3174);
    let n3176: ZB = zb_and(n3021, n3168);
    let n3177: ZB = zb_not(n3176);
    let n3178: ZB = zb_and(n3175, n3176);
    let n3179: ZB = zb_and(n3175, n3177);
    let n3180: ZB = zb_or(n3178, n3179);
    let n3181: ZB = zb_and(n3027, n3176);
    let n3182: ZB = zb_not(n3181);
    let n3183: ZB = zb_and(n3180, n3181);
    let n3184: ZB = zb_and(n3180, n3182);
    let n3185: ZB = zb_or(n3166, n3183);
    let n3186: ZB = zb_or(n3152, n3185);
    let n3187: ZB = zb_or(n3138, n3186);
    let n3188: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n2936);
    let n3189: ZB = zn_gt(n3188, n2940);
    let n3190: ZB = zb_and(n2919, n3189);
    let n3191: ZB = zb_or(n3116, n3184);
    let n3192: ZB = zsel_b(n3114, n2919, n3190);
    let n3193: ZB = zb_or(n3111, n3187);
    let n3194: ZB = zb_or(n3040, n3191);
    let n3195: ZB = zsel_b(n3038, n2919, n3192);
    let n3196: ZB = zb_or(n3035, n3193);
    let n3197: ZB = zb_or(n2945, n3194);
    let n3198: ZB = zsel_b(n2943, n2919, n3195);
    let n3199: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n2924);
    let n3200: ZB = zn_le(n3199, n2928);
    let n3201: ZB = zn_gt(n3199, n2928);
    let n3202: ZB = zb_and(n3197, n3200);
    let n3203: ZB = zb_and(n3197, n3201);
    let n3204: ZB = zb_and(n2942, n3202);
    let n3205: ZB = zb_and(n2943, n3202);
    let n3206: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n3199);
    let n3207: ZN = zn_mget(g.cart, n3206, n2947);
    let n3208: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n3207);
    let n3209: ZB = zb_not(n3208);
    let n3210: ZB = zb_and(n3204, n3208);
    let n3211: ZB = zb_and(n3204, n3209);
    let n3212: ZB = zb_and(n2955, n3210);
    let n3213: ZB = zb_and(n2954, n3210);
    let n3214: ZB = zb_or(n3212, n3213);
    let n3215: ZB = zb_or(n3211, n3214);
    let n3216: ZB = zb_and(n2962, n3208);
    let n3217: ZB = zb_not(n3216);
    let n3218: ZB = zb_and(n3215, n3216);
    let n3219: ZB = zb_and(n3215, n3217);
    let n3220: ZB = zb_or(n3218, n3219);
    let n3221: ZB = zb_and(n2968, n3216);
    let n3222: ZB = zb_not(n3221);
    let n3223: ZB = zb_and(n3220, n3221);
    let n3224: ZB = zb_and(n3220, n3222);
    let n3225: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n3207);
    let n3226: ZB = zb_not(n3225);
    let n3227: ZB = zb_and(n3224, n3225);
    let n3228: ZB = zb_and(n3224, n3226);
    let n3229: ZB = zb_or(n3227, n3228);
    let n3230: ZB = zb_and(n2979, n3225);
    let n3231: ZB = zb_not(n3230);
    let n3232: ZB = zb_and(n3229, n3230);
    let n3233: ZB = zb_and(n3229, n3231);
    let n3234: ZB = zb_or(n3232, n3233);
    let n3235: ZB = zb_and(n2985, n3230);
    let n3236: ZB = zb_not(n3235);
    let n3237: ZB = zb_and(n3234, n3235);
    let n3238: ZB = zb_and(n3234, n3236);
    let n3239: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n3207);
    let n3240: ZB = zb_not(n3239);
    let n3241: ZB = zb_and(n3238, n3239);
    let n3242: ZB = zb_and(n3238, n3240);
    let n3243: ZB = zb_or(n3241, n3242);
    let n3244: ZB = zb_and(n2996, n3239);
    let n3245: ZB = zb_not(n3244);
    let n3246: ZB = zb_and(n3243, n3244);
    let n3247: ZB = zb_and(n3243, n3245);
    let n3248: ZB = zb_or(n3246, n3247);
    let n3249: ZB = zb_and(n3002, n3244);
    let n3250: ZB = zb_not(n3249);
    let n3251: ZB = zb_and(n3248, n3249);
    let n3252: ZB = zb_and(n3248, n3250);
    let n3253: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n3207);
    let n3254: ZB = zb_not(n3253);
    let n3255: ZB = zb_and(n3252, n3253);
    let n3256: ZB = zb_and(n3252, n3254);
    let n3257: ZB = zb_and(n3014, n3255);
    let n3258: ZB = zb_and(n3013, n3255);
    let n3259: ZN = zn_mul(n3199, zn_splat(P8::from_raw(524288i32)));
    let n3260: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n3259);
    let n3261: ZB = zn_eq(n2925, n3260);
    let n3262: ZB = zb_or(n3257, n3258);
    let n3263: ZB = zb_or(n3013, n3261);
    let n3264: ZB = zb_or(n3256, n3262);
    let n3265: ZB = zb_and(n3253, n3263);
    let n3266: ZB = zb_not(n3265);
    let n3267: ZB = zb_and(n3264, n3265);
    let n3268: ZB = zb_and(n3264, n3266);
    let n3269: ZB = zb_or(n3267, n3268);
    let n3270: ZB = zb_and(n3027, n3265);
    let n3271: ZB = zb_not(n3270);
    let n3272: ZB = zb_and(n3269, n3270);
    let n3273: ZB = zb_and(n3269, n3271);
    let n3274: ZB = zb_or(n3251, n3272);
    let n3275: ZB = zb_or(n3237, n3274);
    let n3276: ZB = zb_or(n3223, n3275);
    let n3277: ZB = zb_and(n3037, n3273);
    let n3278: ZB = zb_and(n3038, n3273);
    let n3279: ZN = zn_mget(g.cart, n3206, n3041);
    let n3280: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n3279);
    let n3281: ZB = zb_not(n3280);
    let n3282: ZB = zb_and(n3277, n3280);
    let n3283: ZB = zb_and(n3277, n3281);
    let n3284: ZB = zb_and(n2955, n3282);
    let n3285: ZB = zb_and(n2954, n3282);
    let n3286: ZB = zb_or(n3284, n3285);
    let n3287: ZB = zb_or(n3283, n3286);
    let n3288: ZB = zb_and(n3053, n3280);
    let n3289: ZB = zb_not(n3288);
    let n3290: ZB = zb_and(n3287, n3288);
    let n3291: ZB = zb_and(n3287, n3289);
    let n3292: ZB = zb_or(n3290, n3291);
    let n3293: ZB = zb_and(n2968, n3288);
    let n3294: ZB = zb_not(n3293);
    let n3295: ZB = zb_and(n3292, n3293);
    let n3296: ZB = zb_and(n3292, n3294);
    let n3297: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n3279);
    let n3298: ZB = zb_not(n3297);
    let n3299: ZB = zb_and(n3296, n3297);
    let n3300: ZB = zb_and(n3296, n3298);
    let n3301: ZB = zb_or(n3299, n3300);
    let n3302: ZB = zb_and(n2979, n3297);
    let n3303: ZB = zb_not(n3302);
    let n3304: ZB = zb_and(n3301, n3302);
    let n3305: ZB = zb_and(n3301, n3303);
    let n3306: ZB = zb_or(n3304, n3305);
    let n3307: ZB = zb_and(n2985, n3302);
    let n3308: ZB = zb_not(n3307);
    let n3309: ZB = zb_and(n3306, n3307);
    let n3310: ZB = zb_and(n3306, n3308);
    let n3311: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n3279);
    let n3312: ZB = zb_not(n3311);
    let n3313: ZB = zb_and(n3310, n3311);
    let n3314: ZB = zb_and(n3310, n3312);
    let n3315: ZB = zb_or(n3313, n3314);
    let n3316: ZB = zb_and(n2996, n3311);
    let n3317: ZB = zb_not(n3316);
    let n3318: ZB = zb_and(n3315, n3316);
    let n3319: ZB = zb_and(n3315, n3317);
    let n3320: ZB = zb_or(n3318, n3319);
    let n3321: ZB = zb_and(n3002, n3316);
    let n3322: ZB = zb_not(n3321);
    let n3323: ZB = zb_and(n3320, n3321);
    let n3324: ZB = zb_and(n3320, n3322);
    let n3325: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n3279);
    let n3326: ZB = zb_not(n3325);
    let n3327: ZB = zb_and(n3324, n3325);
    let n3328: ZB = zb_and(n3324, n3326);
    let n3329: ZB = zb_and(n3014, n3327);
    let n3330: ZB = zb_and(n3013, n3327);
    let n3331: ZB = zb_or(n3329, n3330);
    let n3332: ZB = zb_or(n3328, n3331);
    let n3333: ZB = zb_and(n3263, n3325);
    let n3334: ZB = zb_not(n3333);
    let n3335: ZB = zb_and(n3332, n3333);
    let n3336: ZB = zb_and(n3332, n3334);
    let n3337: ZB = zb_or(n3335, n3336);
    let n3338: ZB = zb_and(n3027, n3333);
    let n3339: ZB = zb_not(n3338);
    let n3340: ZB = zb_and(n3337, n3338);
    let n3341: ZB = zb_and(n3337, n3339);
    let n3342: ZB = zb_or(n3323, n3340);
    let n3343: ZB = zb_or(n3309, n3342);
    let n3344: ZB = zb_or(n3295, n3343);
    let n3345: ZB = zb_and(n3113, n3341);
    let n3346: ZB = zb_and(n3114, n3341);
    let n3347: ZN = zn_mget(g.cart, n3206, n3117);
    let n3348: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n3347);
    let n3349: ZB = zb_not(n3348);
    let n3350: ZB = zb_and(n3345, n3348);
    let n3351: ZB = zb_and(n3345, n3349);
    let n3352: ZB = zb_and(n2955, n3350);
    let n3353: ZB = zb_and(n2954, n3350);
    let n3354: ZB = zb_or(n3352, n3353);
    let n3355: ZB = zb_or(n3351, n3354);
    let n3356: ZB = zb_and(n3129, n3348);
    let n3357: ZB = zb_not(n3356);
    let n3358: ZB = zb_and(n3355, n3356);
    let n3359: ZB = zb_and(n3355, n3357);
    let n3360: ZB = zb_or(n3358, n3359);
    let n3361: ZB = zb_and(n2968, n3356);
    let n3362: ZB = zb_not(n3361);
    let n3363: ZB = zb_and(n3360, n3361);
    let n3364: ZB = zb_and(n3360, n3362);
    let n3365: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n3347);
    let n3366: ZB = zb_not(n3365);
    let n3367: ZB = zb_and(n3364, n3365);
    let n3368: ZB = zb_and(n3364, n3366);
    let n3369: ZB = zb_or(n3367, n3368);
    let n3370: ZB = zb_and(n2979, n3365);
    let n3371: ZB = zb_not(n3370);
    let n3372: ZB = zb_and(n3369, n3370);
    let n3373: ZB = zb_and(n3369, n3371);
    let n3374: ZB = zb_or(n3372, n3373);
    let n3375: ZB = zb_and(n2985, n3370);
    let n3376: ZB = zb_not(n3375);
    let n3377: ZB = zb_and(n3374, n3375);
    let n3378: ZB = zb_and(n3374, n3376);
    let n3379: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n3347);
    let n3380: ZB = zb_not(n3379);
    let n3381: ZB = zb_and(n3378, n3379);
    let n3382: ZB = zb_and(n3378, n3380);
    let n3383: ZB = zb_or(n3381, n3382);
    let n3384: ZB = zb_and(n2996, n3379);
    let n3385: ZB = zb_not(n3384);
    let n3386: ZB = zb_and(n3383, n3384);
    let n3387: ZB = zb_and(n3383, n3385);
    let n3388: ZB = zb_or(n3386, n3387);
    let n3389: ZB = zb_and(n3002, n3384);
    let n3390: ZB = zb_not(n3389);
    let n3391: ZB = zb_and(n3388, n3389);
    let n3392: ZB = zb_and(n3388, n3390);
    let n3393: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n3347);
    let n3394: ZB = zb_not(n3393);
    let n3395: ZB = zb_and(n3392, n3393);
    let n3396: ZB = zb_and(n3392, n3394);
    let n3397: ZB = zb_and(n3014, n3395);
    let n3398: ZB = zb_and(n3013, n3395);
    let n3399: ZB = zb_or(n3397, n3398);
    let n3400: ZB = zb_or(n3396, n3399);
    let n3401: ZB = zb_and(n3263, n3393);
    let n3402: ZB = zb_not(n3401);
    let n3403: ZB = zb_and(n3400, n3401);
    let n3404: ZB = zb_and(n3400, n3402);
    let n3405: ZB = zb_or(n3403, n3404);
    let n3406: ZB = zb_and(n3027, n3401);
    let n3407: ZB = zb_not(n3406);
    let n3408: ZB = zb_and(n3405, n3406);
    let n3409: ZB = zb_and(n3405, n3407);
    let n3410: ZB = zb_or(n3391, n3408);
    let n3411: ZB = zb_or(n3377, n3410);
    let n3412: ZB = zb_or(n3363, n3411);
    let n3413: ZB = zb_and(n3189, n3198);
    let n3414: ZB = zb_or(n3346, n3409);
    let n3415: ZB = zsel_b(n3114, n3198, n3413);
    let n3416: ZB = zb_or(n3344, n3412);
    let n3417: ZB = zb_or(n3278, n3414);
    let n3418: ZB = zsel_b(n3038, n3198, n3415);
    let n3419: ZB = zb_or(n3276, n3416);
    let n3420: ZB = zb_or(n3205, n3417);
    let n3421: ZB = zsel_b(n2943, n3198, n3418);
    let n3422: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n2924);
    let n3423: ZB = zn_le(n3422, n2928);
    let n3424: ZB = zn_gt(n3422, n2928);
    let n3425: ZB = zb_and(n3420, n3423);
    let n3426: ZB = zb_and(n3420, n3424);
    let n3427: ZB = zb_and(n2942, n3425);
    let n3428: ZB = zb_and(n2943, n3425);
    let n3429: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n3422);
    let n3430: ZN = zn_mget(g.cart, n3429, n2947);
    let n3431: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n3430);
    let n3432: ZB = zb_not(n3431);
    let n3433: ZB = zb_and(n3427, n3431);
    let n3434: ZB = zb_and(n3427, n3432);
    let n3435: ZB = zb_and(n2955, n3433);
    let n3436: ZB = zb_and(n2954, n3433);
    let n3437: ZB = zb_or(n3435, n3436);
    let n3438: ZB = zb_or(n3434, n3437);
    let n3439: ZB = zb_and(n2962, n3431);
    let n3440: ZB = zb_not(n3439);
    let n3441: ZB = zb_and(n3438, n3439);
    let n3442: ZB = zb_and(n3438, n3440);
    let n3443: ZB = zb_or(n3441, n3442);
    let n3444: ZB = zb_and(n2968, n3439);
    let n3445: ZB = zb_not(n3444);
    let n3446: ZB = zb_and(n3443, n3444);
    let n3447: ZB = zb_and(n3443, n3445);
    let n3448: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n3430);
    let n3449: ZB = zb_not(n3448);
    let n3450: ZB = zb_and(n3447, n3448);
    let n3451: ZB = zb_and(n3447, n3449);
    let n3452: ZB = zb_or(n3450, n3451);
    let n3453: ZB = zb_and(n2979, n3448);
    let n3454: ZB = zb_not(n3453);
    let n3455: ZB = zb_and(n3452, n3453);
    let n3456: ZB = zb_and(n3452, n3454);
    let n3457: ZB = zb_or(n3455, n3456);
    let n3458: ZB = zb_and(n2985, n3453);
    let n3459: ZB = zb_not(n3458);
    let n3460: ZB = zb_and(n3457, n3458);
    let n3461: ZB = zb_and(n3457, n3459);
    let n3462: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n3430);
    let n3463: ZB = zb_not(n3462);
    let n3464: ZB = zb_and(n3461, n3462);
    let n3465: ZB = zb_and(n3461, n3463);
    let n3466: ZB = zb_or(n3464, n3465);
    let n3467: ZB = zb_and(n2996, n3462);
    let n3468: ZB = zb_not(n3467);
    let n3469: ZB = zb_and(n3466, n3467);
    let n3470: ZB = zb_and(n3466, n3468);
    let n3471: ZB = zb_or(n3469, n3470);
    let n3472: ZB = zb_and(n3002, n3467);
    let n3473: ZB = zb_not(n3472);
    let n3474: ZB = zb_and(n3471, n3472);
    let n3475: ZB = zb_and(n3471, n3473);
    let n3476: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n3430);
    let n3477: ZB = zb_not(n3476);
    let n3478: ZB = zb_and(n3475, n3476);
    let n3479: ZB = zb_and(n3475, n3477);
    let n3480: ZB = zb_and(n3014, n3478);
    let n3481: ZB = zb_and(n3013, n3478);
    let n3482: ZN = zn_mul(n3422, zn_splat(P8::from_raw(524288i32)));
    let n3483: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n3482);
    let n3484: ZB = zn_eq(n2925, n3483);
    let n3485: ZB = zb_or(n3480, n3481);
    let n3486: ZB = zb_or(n3013, n3484);
    let n3487: ZB = zb_or(n3479, n3485);
    let n3488: ZB = zb_and(n3476, n3486);
    let n3489: ZB = zb_not(n3488);
    let n3490: ZB = zb_and(n3487, n3488);
    let n3491: ZB = zb_and(n3487, n3489);
    let n3492: ZB = zb_or(n3490, n3491);
    let n3493: ZB = zb_and(n3027, n3488);
    let n3494: ZB = zb_not(n3493);
    let n3495: ZB = zb_and(n3492, n3493);
    let n3496: ZB = zb_and(n3492, n3494);
    let n3497: ZB = zb_or(n3474, n3495);
    let n3498: ZB = zb_or(n3460, n3497);
    let n3499: ZB = zb_or(n3446, n3498);
    let n3500: ZB = zb_and(n3037, n3496);
    let n3501: ZB = zb_and(n3038, n3496);
    let n3502: ZN = zn_mget(g.cart, n3429, n3041);
    let n3503: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n3502);
    let n3504: ZB = zb_not(n3503);
    let n3505: ZB = zb_and(n3500, n3503);
    let n3506: ZB = zb_and(n3500, n3504);
    let n3507: ZB = zb_and(n2955, n3505);
    let n3508: ZB = zb_and(n2954, n3505);
    let n3509: ZB = zb_or(n3507, n3508);
    let n3510: ZB = zb_or(n3506, n3509);
    let n3511: ZB = zb_and(n3053, n3503);
    let n3512: ZB = zb_not(n3511);
    let n3513: ZB = zb_and(n3510, n3511);
    let n3514: ZB = zb_and(n3510, n3512);
    let n3515: ZB = zb_or(n3513, n3514);
    let n3516: ZB = zb_and(n2968, n3511);
    let n3517: ZB = zb_not(n3516);
    let n3518: ZB = zb_and(n3515, n3516);
    let n3519: ZB = zb_and(n3515, n3517);
    let n3520: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n3502);
    let n3521: ZB = zb_not(n3520);
    let n3522: ZB = zb_and(n3519, n3520);
    let n3523: ZB = zb_and(n3519, n3521);
    let n3524: ZB = zb_or(n3522, n3523);
    let n3525: ZB = zb_and(n2979, n3520);
    let n3526: ZB = zb_not(n3525);
    let n3527: ZB = zb_and(n3524, n3525);
    let n3528: ZB = zb_and(n3524, n3526);
    let n3529: ZB = zb_or(n3527, n3528);
    let n3530: ZB = zb_and(n2985, n3525);
    let n3531: ZB = zb_not(n3530);
    let n3532: ZB = zb_and(n3529, n3530);
    let n3533: ZB = zb_and(n3529, n3531);
    let n3534: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n3502);
    let n3535: ZB = zb_not(n3534);
    let n3536: ZB = zb_and(n3533, n3534);
    let n3537: ZB = zb_and(n3533, n3535);
    let n3538: ZB = zb_or(n3536, n3537);
    let n3539: ZB = zb_and(n2996, n3534);
    let n3540: ZB = zb_not(n3539);
    let n3541: ZB = zb_and(n3538, n3539);
    let n3542: ZB = zb_and(n3538, n3540);
    let n3543: ZB = zb_or(n3541, n3542);
    let n3544: ZB = zb_and(n3002, n3539);
    let n3545: ZB = zb_not(n3544);
    let n3546: ZB = zb_and(n3543, n3544);
    let n3547: ZB = zb_and(n3543, n3545);
    let n3548: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n3502);
    let n3549: ZB = zb_not(n3548);
    let n3550: ZB = zb_and(n3547, n3548);
    let n3551: ZB = zb_and(n3547, n3549);
    let n3552: ZB = zb_and(n3014, n3550);
    let n3553: ZB = zb_and(n3013, n3550);
    let n3554: ZB = zb_or(n3552, n3553);
    let n3555: ZB = zb_or(n3551, n3554);
    let n3556: ZB = zb_and(n3486, n3548);
    let n3557: ZB = zb_not(n3556);
    let n3558: ZB = zb_and(n3555, n3556);
    let n3559: ZB = zb_and(n3555, n3557);
    let n3560: ZB = zb_or(n3558, n3559);
    let n3561: ZB = zb_and(n3027, n3556);
    let n3562: ZB = zb_not(n3561);
    let n3563: ZB = zb_and(n3560, n3561);
    let n3564: ZB = zb_and(n3560, n3562);
    let n3565: ZB = zb_or(n3546, n3563);
    let n3566: ZB = zb_or(n3532, n3565);
    let n3567: ZB = zb_or(n3518, n3566);
    let n3568: ZB = zb_and(n3113, n3564);
    let n3569: ZB = zb_and(n3114, n3564);
    let n3570: ZN = zn_mget(g.cart, n3429, n3117);
    let n3571: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n3570);
    let n3572: ZB = zb_not(n3571);
    let n3573: ZB = zb_and(n3568, n3571);
    let n3574: ZB = zb_and(n3568, n3572);
    let n3575: ZB = zb_and(n2955, n3573);
    let n3576: ZB = zb_and(n2954, n3573);
    let n3577: ZB = zb_or(n3575, n3576);
    let n3578: ZB = zb_or(n3574, n3577);
    let n3579: ZB = zb_and(n3129, n3571);
    let n3580: ZB = zb_not(n3579);
    let n3581: ZB = zb_and(n3578, n3579);
    let n3582: ZB = zb_and(n3578, n3580);
    let n3583: ZB = zb_or(n3581, n3582);
    let n3584: ZB = zb_and(n2968, n3579);
    let n3585: ZB = zb_not(n3584);
    let n3586: ZB = zb_and(n3583, n3584);
    let n3587: ZB = zb_and(n3583, n3585);
    let n3588: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n3570);
    let n3589: ZB = zb_not(n3588);
    let n3590: ZB = zb_and(n3587, n3588);
    let n3591: ZB = zb_and(n3587, n3589);
    let n3592: ZB = zb_or(n3590, n3591);
    let n3593: ZB = zb_and(n2979, n3588);
    let n3594: ZB = zb_not(n3593);
    let n3595: ZB = zb_and(n3592, n3593);
    let n3596: ZB = zb_and(n3592, n3594);
    let n3597: ZB = zb_or(n3595, n3596);
    let n3598: ZB = zb_and(n2985, n3593);
    let n3599: ZB = zb_not(n3598);
    let n3600: ZB = zb_and(n3597, n3598);
    let n3601: ZB = zb_and(n3597, n3599);
    let n3602: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n3570);
    let n3603: ZB = zb_not(n3602);
    let n3604: ZB = zb_and(n3601, n3602);
    let n3605: ZB = zb_and(n3601, n3603);
    let n3606: ZB = zb_or(n3604, n3605);
    let n3607: ZB = zb_and(n2996, n3602);
    let n3608: ZB = zb_not(n3607);
    let n3609: ZB = zb_and(n3606, n3607);
    let n3610: ZB = zb_and(n3606, n3608);
    let n3611: ZB = zb_or(n3609, n3610);
    let n3612: ZB = zb_and(n3002, n3607);
    let n3613: ZB = zb_not(n3612);
    let n3614: ZB = zb_and(n3611, n3612);
    let n3615: ZB = zb_and(n3611, n3613);
    let n3616: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n3570);
    let n3617: ZB = zb_not(n3616);
    let n3618: ZB = zb_and(n3615, n3616);
    let n3619: ZB = zb_and(n3615, n3617);
    let n3620: ZB = zb_and(n3014, n3618);
    let n3621: ZB = zb_and(n3013, n3618);
    let n3622: ZB = zb_or(n3620, n3621);
    let n3623: ZB = zb_or(n3619, n3622);
    let n3624: ZB = zb_and(n3486, n3616);
    let n3625: ZB = zb_not(n3624);
    let n3626: ZB = zb_and(n3623, n3624);
    let n3627: ZB = zb_and(n3623, n3625);
    let n3628: ZB = zb_or(n3626, n3627);
    let n3629: ZB = zb_and(n3027, n3624);
    let n3630: ZB = zb_not(n3629);
    let n3631: ZB = zb_and(n3628, n3629);
    let n3632: ZB = zb_and(n3628, n3630);
    let n3633: ZB = zb_or(n3614, n3631);
    let n3634: ZB = zb_or(n3600, n3633);
    let n3635: ZB = zb_or(n3586, n3634);
    let n3636: ZB = zb_and(n3189, n3421);
    let n3637: ZB = zb_or(n3569, n3632);
    let n3638: ZB = zsel_b(n3114, n3421, n3636);
    let n3639: ZB = zb_or(n3567, n3635);
    let n3640: ZB = zb_or(n3501, n3637);
    let n3641: ZB = zsel_b(n3038, n3421, n3638);
    let n3642: ZB = zb_or(n3499, n3639);
    let n3643: ZB = zb_or(n3428, n3640);
    let n3644: ZB = zsel_b(n2943, n3421, n3641);
    let n3645: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n2924);
    let n3646: ZB = zn_gt(n3645, n2928);
    let n3647: ZB = zb_and(n3644, n3646);
    let n3648: ZB = zb_or(n3419, n3642);
    let n3649: ZB = zsel_b(n3419, n3198, n3421);
    let n3650: ZB = zb_or(n3426, n3643);
    let n3651: ZB = zsel_b(n3424, n3421, n3647);
    let n3652: ZB = zb_or(n3196, n3648);
    let n3653: ZB = zsel_b(n3196, n2919, n3649);
    let n3654: ZB = zb_or(n3203, n3650);
    let n3655: ZB = zsel_b(n3201, n3198, n3651);
    let n3656: ZB = zb_or(n2933, n3654);
    let n3657: ZB = zsel_b(n2931, n2919, n3655);
    let n3658: ZB = zn_gt(n2915, zn_splat(P8::from_raw(8388608i32)));
    let n3659: ZB = zn_le(n2915, zn_splat(P8::from_raw(8388608i32)));
    let n3660: ZB = zb_and(n3652, n3658);
    let n3661: ZB = zb_and(n3652, n3659);
    let n3662: ZB = zb_or(n3660, n3661);
    let n3663: ZB = zb_and(n3656, n3658);
    let n3664: ZB = zb_or(n3662, n3663);
    let n3665: ZB = zsel_b(n3662, n3653, n3657);
    let n3666: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n2920);
    let n3667: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n2921);
    let n3668: ZB = zn_tile_flag_at(g.cache, g.cart, n3666, n3667, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n3669: ZB = zb_not(n3668);
    let n3670: ZB = zb_and(n3664, n3669);
    let n3671: ZB = zb_and(n3664, n3668);
    let n3672: ZB = zb_or(n3670, n3671);
    let n3673: ZB = zb_and(n3669, n3672);
    let n3674: ZB = zb_and(n3668, n3672);
    let n3675: ZB = zb_or(n3673, n3674);
    let n3676: ZB = zn_lt(n2427, zn_splat(P8::from_raw(65536i32)));
    let n3677: ZB = zn_ge(n2427, zn_splat(P8::from_raw(65536i32)));
    let n3678: ZN = zsel_n(n3676, zn_splat(P8::from_raw(65536i32)), n2427);
    let n3679: ZN = zsel_n(n3668, n3678, n2427);
    let n3680: ZN = zsel_n(n3668, zn_splat(P8::from_raw(393216i32)), n289);
    let n3681: ZB = zb_and(n3668, n3675);
    let n3682: ZB = zb_and(n3669, n3675);
    let n3683: ZB = zb_and(n3676, n3681);
    let n3684: ZB = zb_and(n3677, n3681);
    let n3685: ZB = zb_or(n3683, n3684);
    let n3686: ZB = zb_and(n286, n3682);
    let n3687: ZB = zb_and(n287, n3682);
    let n3688: ZB = zb_or(n3686, n3687);
    let n3689: ZB = zb_or(n3685, n3688);
    let n3690: ZB = zn_gt(n2916, r_c359);
    let n3691: ZB = zn_le(n2916, r_c359);
    let n3692: ZB = zn_gt(n2917, r_c360);
    let n3693: ZB = zn_le(n2917, r_c360);
    let n3694: ZN = zsel_n(n3669, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n3695: ZN = zn_abs(n2916);
    let n3696: ZB = zn_gt(n3695, zn_splat(P8::from_raw(65536i32)));
    let n3697: ZB = zn_le(n3695, zn_splat(P8::from_raw(65536i32)));
    let n3698: ZB = zn_gt(n2916, zn_splat(P8::from_raw(0i32)));
    let n3699: ZB = zn_lt(n2916, zn_splat(P8::from_raw(0i32)));
    let n3700: ZB = zn_gt(n2916, zn_splat(P8::from_raw(65536i32)));
    let n3701: ZB = zn_le(n2916, zn_splat(P8::from_raw(65536i32)));
    let n3702: ZN = zn_sub(n2916, zn_splat(P8::from_raw(9830i32)));
    let n3703: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n3702);
    let n3704: ZN = zn_add(zn_splat(P8::from_raw(9830i32)), n2916);
    let n3705: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n3704);
    let n3706: ZB = zn_gt(n2916, zn_splat(P8::from_raw(-65536i32)));
    let n3707: ZB = zn_le(n2916, zn_splat(P8::from_raw(-65536i32)));
    let n3708: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n3702);
    let n3709: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n3704);
    let n3710: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n3702);
    let n3711: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n3704);
    let n3712: ZN = zsel_n(n3706, n3708, n3709);
    let n3713: ZN = zsel_n(n3698, n3710, n3711);
    let n3714: ZN = zsel_n(n3700, n3703, n3705);
    let n3715: ZN = zsel_n(n3699, n3712, n3713);
    let n3716: ZN = zsel_n(n3698, n3714, n3715);
    let n3717: ZN = zn_sub(n2916, n3694);
    let n3718: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n3717);
    let n3719: ZN = zn_add(n2916, n3694);
    let n3720: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n3719);
    let n3721: ZN = zsel_n(n3698, n3718, n3720);
    let n3722: ZN = zsel_n(n3696, n3716, n3721);
    let n3723: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3722);
    let n3724: ZB = zb_not(n3723);
    let n3725: ZB = zn_lt(n3722, zn_splat(P8::from_raw(0i32)));
    let n3726: ZB = zsel_b(n3724, n3725, r_c361);
    let n3727: ZN = zn_abs(n2917);
    let n3728: ZB = zn_le(n3727, zn_splat(P8::from_raw(9830i32)));
    let n3729: ZB = zn_gt(n3727, zn_splat(P8::from_raw(9830i32)));
    let n3730: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n2921);
    let n3731: ZB = zn_gt(n2917, zn_splat(P8::from_raw(131072i32)));
    let n3732: ZB = zn_le(n2917, zn_splat(P8::from_raw(131072i32)));
    let n3733: ZB = zn_gt(n3680, zn_splat(P8::from_raw(0i32)));
    let n3734: ZB = zn_le(n3680, zn_splat(P8::from_raw(0i32)));
    let n3735: ZN = zn_add(zn_splat(P8::from_raw(-196608i32)), n2920);
    let n3736: ZB = zn_tile_flag_at(g.cache, g.cart, n3735, n3730, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n3737: ZB = zb_not(n3736);
    let n3738: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n2920);
    let n3739: ZB = zn_tile_flag_at(g.cache, g.cart, n3738, n3730, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n3740: ZB = zb_not(n3739);
    let n3741: ZN = zsel_n(n3739, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n3742: ZN = zsel_n(n3736, zn_splat(P8::from_raw(-65536i32)), n3741);
    let n3743: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3742);
    let n3744: ZB = zb_not(n3743);
    let n3745: ZB = zn_gt(n3679, zn_splat(P8::from_raw(0i32)));
    let n3746: ZB = zn_le(n3679, zn_splat(P8::from_raw(0i32)));
    let n3747: ZB = zb_not(n3726);
    let n3748: ZN = zsel_n(n3726, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n3749: ZB = zn_gt(n3748, zn_splat(P8::from_raw(0i32)));
    let n3750: ZB = zn_le(n3748, zn_splat(P8::from_raw(0i32)));
    let n3751: ZB = zn_lt(n3748, zn_splat(P8::from_raw(0i32)));
    let n3752: ZB = zn_ge(n3748, zn_splat(P8::from_raw(0i32)));
    let n3753: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3748);
    let n3754: ZB = zb_not(n3753);
    let n3755: ZB = zb_and(n291, n3689);
    let n3756: ZB = zb_and(n292, n3689);
    let n3757: ZB = zb_and(n3690, n3755);
    let n3758: ZB = zb_and(n3691, n3755);
    let n3759: ZB = zb_or(n3757, n3758);
    let n3760: ZB = zb_and(n3692, n3759);
    let n3761: ZB = zb_and(n3693, n3759);
    let n3762: ZB = zb_or(n3760, n3761);
    let n3763: ZB = zb_and(n3669, n3756);
    let n3764: ZB = zb_and(n3668, n3756);
    let n3765: ZB = zb_or(n3763, n3764);
    let n3766: ZB = zb_and(n3696, n3765);
    let n3767: ZB = zb_and(n3697, n3765);
    let n3768: ZB = zb_and(n3698, n3766);
    let n3769: ZB = zb_and(n3002, n3766);
    let n3770: ZB = zb_and(n3699, n3769);
    let n3771: ZB = zb_and(n3027, n3769);
    let n3772: ZB = zb_and(n3700, n3768);
    let n3773: ZB = zb_and(n3701, n3768);
    let n3774: ZB = zb_and(n3706, n3770);
    let n3775: ZB = zb_and(n3707, n3770);
    let n3776: ZB = zb_and(n3002, n3771);
    let n3777: ZB = zb_or(n3774, n3775);
    let n3778: ZB = zb_or(n3772, n3773);
    let n3779: ZB = zb_or(n3776, n3777);
    let n3780: ZB = zb_or(n3778, n3779);
    let n3781: ZB = zb_and(n3698, n3767);
    let n3782: ZB = zb_and(n3002, n3767);
    let n3783: ZB = zb_or(n3781, n3782);
    let n3784: ZB = zb_or(n3780, n3783);
    let n3785: ZB = zb_and(n3724, n3784);
    let n3786: ZB = zb_and(n3723, n3784);
    let n3787: ZB = zb_or(n3785, n3786);
    let n3788: ZB = zb_and(n3728, n3787);
    let n3789: ZB = zb_and(n3729, n3787);
    let n3790: ZB = zb_or(n3788, n3789);
    let n3791: ZB = zb_and(n3669, n3790);
    let n3792: ZB = zb_and(n3668, n3790);
    let n3793: ZB = zb_and(n3731, n3791);
    let n3794: ZB = zb_and(n3732, n3791);
    let n3795: ZB = zb_or(n3793, n3794);
    let n3796: ZB = zb_or(n3792, n3795);
    let n3797: ZB = zb_and(n3745, n3796);
    let n3798: ZB = zb_and(n3746, n3796);
    let n3799: ZB = zb_or(n3797, n3798);
    let n3800: ZB = zb_or(n3762, n3799);
    let n3801: ZB = zn_lt(n2915, zn_splat(P8::from_raw(-262144i32)));
    let n3802: ZB = zn_ge(n2915, zn_splat(P8::from_raw(-262144i32)));
    let n3803: ZB = zb_and(n3800, n3801);
    let n3804: ZB = zb_and(n3800, n3802);
    let n3805: ZB = zb_or(n3803, n3804);
    let n3808: ZN = zsel_n(n3658, n2078, n2077);
    let n3809: ZN = zsel_n(n3662, n3808, n2077);
    let n3811: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n3717);
    let n3812: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n3719);
    let n3813: ZN = zsel_n(n3706, n3811, n3812);
    let n3814: ZN = zsel_n(n3696, n3716, n3813);
    let n3815: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3814);
    let n3816: ZB = zb_not(n3815);
    let n3817: ZB = zn_lt(n3814, zn_splat(P8::from_raw(0i32)));
    let n3818: ZB = zsel_b(n3816, n3817, r_c361);
    let n3819: ZN = zn_add(zn_splat(P8::from_raw(-65536i32)), n2920);
    let n3820: ZB = zn_tile_flag_at(g.cache, g.cart, n3819, n3730, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n3821: ZB = zb_not(n3820);
    let n3822: ZN = zsel_n(n3820, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n3823: ZB = zn_gt(n2917, n3822);
    let n3824: ZB = zn_le(n2917, n3822);
    let n3825: ZB = zb_and(n3706, n3767);
    let n3826: ZB = zb_and(n3707, n3767);
    let n3827: ZB = zb_or(n3825, n3826);
    let n3828: ZB = zb_or(n3780, n3827);
    let n3829: ZB = zb_and(n3816, n3828);
    let n3830: ZB = zb_and(n3815, n3828);
    let n3831: ZB = zb_or(n3829, n3830);
    let n3832: ZB = zb_and(n3728, n3831);
    let n3833: ZB = zb_and(n3729, n3831);
    let n3834: ZB = zb_or(n3832, n3833);
    let n3835: ZB = zb_and(n3821, n3834);
    let n3836: ZB = zb_and(n3820, n3834);
    let n3837: ZB = zb_or(n3835, n3836);
    let n3838: ZB = zb_and(n3821, n3837);
    let n3839: ZB = zb_and(n3820, n3837);
    let n3840: ZB = zb_or(n3838, n3839);
    let n3841: ZB = zb_and(n3820, n3840);
    let n3842: ZB = zb_and(n3821, n3840);
    let n3843: ZB = zb_or(n3841, n3842);
    let n3844: ZB = zb_and(n3820, n3843);
    let n3845: ZB = zb_and(n3821, n3843);
    let n3846: ZB = zb_or(n3844, n3845);
    let n3847: ZB = zb_and(n3669, n3846);
    let n3848: ZB = zb_and(n3668, n3846);
    let n3849: ZB = zb_and(n3823, n3847);
    let n3850: ZB = zb_and(n3824, n3847);
    let n3851: ZB = zb_or(n3849, n3850);
    let n3852: ZB = zb_or(n3848, n3851);
    let n3853: ZB = zb_and(n3745, n3852);
    let n3854: ZB = zb_and(n3746, n3852);
    let n3855: ZB = zb_or(n3853, n3854);
    let n3856: ZB = zb_or(n3762, n3855);
    let n3857: ZB = zb_and(n3801, n3856);
    let n3858: ZB = zb_and(n3802, n3856);
    let n3859: ZB = zb_or(n3857, n3858);
    let n3862: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n3717);
    let n3863: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n3719);
    let n3864: ZN = zsel_n(n3700, n3862, n3863);
    let n3865: ZN = zsel_n(n3696, n3716, n3864);
    let n3866: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3865);
    let n3867: ZB = zb_not(n3866);
    let n3868: ZB = zn_lt(n3865, zn_splat(P8::from_raw(0i32)));
    let n3869: ZB = zsel_b(n3867, n3868, r_c361);
    let n3870: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n2920);
    let n3871: ZB = zn_tile_flag_at(g.cache, g.cart, n3870, n3730, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n3872: ZB = zb_not(n3871);
    let n3873: ZN = zsel_n(n3871, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n3874: ZB = zn_gt(n2917, n3873);
    let n3875: ZB = zn_le(n2917, n3873);
    let n3876: ZB = zb_and(n3700, n3767);
    let n3877: ZB = zb_and(n3701, n3767);
    let n3878: ZB = zb_or(n3876, n3877);
    let n3879: ZB = zb_or(n3780, n3878);
    let n3880: ZB = zb_and(n3867, n3879);
    let n3881: ZB = zb_and(n3866, n3879);
    let n3882: ZB = zb_or(n3880, n3881);
    let n3883: ZB = zb_and(n3728, n3882);
    let n3884: ZB = zb_and(n3729, n3882);
    let n3885: ZB = zb_or(n3883, n3884);
    let n3886: ZB = zb_and(n3872, n3885);
    let n3887: ZB = zb_and(n3871, n3885);
    let n3888: ZB = zb_or(n3886, n3887);
    let n3889: ZB = zb_and(n3872, n3888);
    let n3890: ZB = zb_and(n3871, n3888);
    let n3891: ZB = zb_or(n3889, n3890);
    let n3892: ZB = zb_and(n3871, n3891);
    let n3893: ZB = zb_and(n3872, n3891);
    let n3894: ZB = zb_or(n3892, n3893);
    let n3895: ZB = zb_and(n3871, n3894);
    let n3896: ZB = zb_and(n3872, n3894);
    let n3897: ZB = zb_or(n3895, n3896);
    let n3898: ZB = zb_and(n3669, n3897);
    let n3899: ZB = zb_and(n3668, n3897);
    let n3900: ZB = zb_and(n3874, n3898);
    let n3901: ZB = zb_and(n3875, n3898);
    let n3902: ZB = zb_or(n3900, n3901);
    let n3903: ZB = zb_or(n3899, n3902);
    let n3904: ZB = zb_and(n3745, n3903);
    let n3905: ZB = zb_and(n3746, n3903);
    let n3906: ZB = zb_or(n3904, n3905);
    let n3907: ZB = zb_or(n3762, n3906);
    let n3908: ZB = zb_and(n3801, n3907);
    let n3909: ZB = zb_and(n3802, n3907);
    let n3910: ZB = zb_or(n3908, n3909);
    let n3913: ZB = zb_and(n140, n3796);
    let n3914: ZB = zb_and(r_c294, n3796);
    let n3915: ZB = zb_and(n3733, n3913);
    let n3916: ZB = zb_and(n3734, n3913);
    let n3917: ZB = zb_and(n3737, n3916);
    let n3918: ZB = zb_and(n3736, n3916);
    let n3919: ZB = zb_or(n3917, n3918);
    let n3920: ZB = zb_and(n3737, n3919);
    let n3921: ZB = zb_and(n3736, n3919);
    let n3922: ZB = zb_or(n3920, n3921);
    let n3923: ZB = zb_and(n3736, n3922);
    let n3924: ZB = zb_and(n3737, n3922);
    let n3925: ZB = zb_and(n3740, n3924);
    let n3926: ZB = zb_and(n3739, n3924);
    let n3927: ZB = zb_or(n3925, n3926);
    let n3928: ZB = zb_and(n3740, n3927);
    let n3929: ZB = zb_and(n3739, n3927);
    let n3930: ZB = zb_or(n3928, n3929);
    let n3931: ZB = zb_and(n3739, n3930);
    let n3932: ZB = zb_and(n3740, n3930);
    let n3933: ZB = zb_or(n3931, n3932);
    let n3934: ZB = zb_or(n3923, n3933);
    let n3935: ZB = zb_and(n3744, n3934);
    let n3936: ZB = zb_and(n3743, n3934);
    let n3937: ZB = zb_or(n3935, n3936);
    let n3938: ZB = zb_or(n3915, n3937);
    let n3939: ZB = zb_or(n3914, n3938);
    let n3940: ZB = zb_and(n3745, n3939);
    let n3941: ZB = zb_and(n3746, n3939);
    let n3942: ZB = zb_or(n3940, n3941);
    let n3943: ZB = zb_or(n3762, n3942);
    let n3944: ZB = zb_and(n3801, n3943);
    let n3945: ZB = zb_and(n3802, n3943);
    let n3946: ZB = zb_or(n3944, n3945);
    let n3949: ZB = zb_and(n140, n3852);
    let n3950: ZB = zb_and(r_c294, n3852);
    let n3951: ZB = zb_and(n3733, n3949);
    let n3952: ZB = zb_and(n3734, n3949);
    let n3953: ZB = zb_and(n3737, n3952);
    let n3954: ZB = zb_and(n3736, n3952);
    let n3955: ZB = zb_or(n3953, n3954);
    let n3956: ZB = zb_and(n3737, n3955);
    let n3957: ZB = zb_and(n3736, n3955);
    let n3958: ZB = zb_or(n3956, n3957);
    let n3959: ZB = zb_and(n3736, n3958);
    let n3960: ZB = zb_and(n3737, n3958);
    let n3961: ZB = zb_and(n3740, n3960);
    let n3962: ZB = zb_and(n3739, n3960);
    let n3963: ZB = zb_or(n3961, n3962);
    let n3964: ZB = zb_and(n3740, n3963);
    let n3965: ZB = zb_and(n3739, n3963);
    let n3966: ZB = zb_or(n3964, n3965);
    let n3967: ZB = zb_and(n3739, n3966);
    let n3968: ZB = zb_and(n3740, n3966);
    let n3969: ZB = zb_or(n3967, n3968);
    let n3970: ZB = zb_or(n3959, n3969);
    let n3971: ZB = zb_and(n3744, n3970);
    let n3972: ZB = zb_and(n3743, n3970);
    let n3973: ZB = zb_or(n3971, n3972);
    let n3974: ZB = zb_or(n3951, n3973);
    let n3975: ZB = zb_or(n3950, n3974);
    let n3976: ZB = zb_and(n3745, n3975);
    let n3977: ZB = zb_and(n3746, n3975);
    let n3978: ZB = zb_or(n3976, n3977);
    let n3979: ZB = zb_or(n3762, n3978);
    let n3980: ZB = zb_and(n3801, n3979);
    let n3981: ZB = zb_and(n3802, n3979);
    let n3982: ZB = zb_or(n3980, n3981);
    let n3985: ZB = zb_and(n140, n3903);
    let n3986: ZB = zb_and(r_c294, n3903);
    let n3987: ZB = zb_and(n3733, n3985);
    let n3988: ZB = zb_and(n3734, n3985);
    let n3989: ZB = zb_and(n3737, n3988);
    let n3990: ZB = zb_and(n3736, n3988);
    let n3991: ZB = zb_or(n3989, n3990);
    let n3992: ZB = zb_and(n3737, n3991);
    let n3993: ZB = zb_and(n3736, n3991);
    let n3994: ZB = zb_or(n3992, n3993);
    let n3995: ZB = zb_and(n3736, n3994);
    let n3996: ZB = zb_and(n3737, n3994);
    let n3997: ZB = zb_and(n3740, n3996);
    let n3998: ZB = zb_and(n3739, n3996);
    let n3999: ZB = zb_or(n3997, n3998);
    let n4000: ZB = zb_and(n3740, n3999);
    let n4001: ZB = zb_and(n3739, n3999);
    let n4002: ZB = zb_or(n4000, n4001);
    let n4003: ZB = zb_and(n3739, n4002);
    let n4004: ZB = zb_and(n3740, n4002);
    let n4005: ZB = zb_or(n4003, n4004);
    let n4006: ZB = zb_or(n3995, n4005);
    let n4007: ZB = zb_and(n3744, n4006);
    let n4008: ZB = zb_and(n3743, n4006);
    let n4009: ZB = zb_or(n4007, n4008);
    let n4010: ZB = zb_or(n3987, n4009);
    let n4011: ZB = zb_or(n3986, n4010);
    let n4012: ZB = zb_and(n3745, n4011);
    let n4013: ZB = zb_and(n3746, n4011);
    let n4014: ZB = zb_or(n4012, n4013);
    let n4015: ZB = zb_or(n3762, n4014);
    let n4016: ZB = zb_and(n3801, n4015);
    let n4017: ZB = zb_and(n3802, n4015);
    let n4018: ZB = zb_or(n4016, n4017);
    let n4021: ZB = zb_and(n133, n3745);
    let n4022: ZB = zb_not(n4021);
    let n4023: ZN = zsel_n(n4021, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n4024: ZB = zb_or(r_c41, n4021);
    let n4025: ZN = zsel_n(n291, r_c20, n4023);
    let n4026: ZB = zsel_b(n291, r_c41, n4024);
    let n4027: ZB = zb_and(n3799, n4021);
    let n4028: ZB = zb_and(n3799, n4022);
    let n4029: ZB = zb_and(n3726, n4027);
    let n4030: ZB = zb_and(n3747, n4027);
    let n4031: ZB = zb_or(n4029, n4030);
    let n4032: ZB = zb_and(n3749, n4031);
    let n4033: ZB = zb_and(n3750, n4031);
    let n4034: ZB = zb_and(n3751, n4033);
    let n4035: ZB = zb_and(n3752, n4033);
    let n4036: ZB = zb_or(n4034, n4035);
    let n4037: ZB = zb_or(n4032, n4036);
    let n4038: ZB = zb_and(n3754, n4037);
    let n4039: ZB = zb_and(n3753, n4037);
    let n4040: ZB = zb_or(n4038, n4039);
    let n4041: ZB = zb_or(n4028, n4040);
    let n4042: ZB = zb_or(n3762, n4041);
    let n4043: ZB = zb_and(n3801, n4042);
    let n4044: ZB = zb_and(n3802, n4042);
    let n4045: ZB = zb_or(n4043, n4044);
    let n4046: ZB = zb_and(n3802, n4045);
    let n4047: ZB = zn_gt(n4025, zn_splat(P8::from_raw(0i32)));
    let n4048: ZB = zn_le(n4025, zn_splat(P8::from_raw(0i32)));
    let n4049: ZB = zb_and(n4046, n4047);
    let n4050: ZB = zb_and(n4046, n4048);
    let n4051: ZB = zb_or(n4049, n4050);
    let n4052: ZB = zb_and(n3855, n4021);
    let n4053: ZB = zb_and(n3855, n4022);
    let n4054: ZB = zb_or(n4052, n4053);
    let n4055: ZB = zb_or(n3762, n4054);
    let n4056: ZB = zb_and(n3801, n4055);
    let n4057: ZB = zb_and(n3802, n4055);
    let n4058: ZB = zb_or(n4056, n4057);
    let n4059: ZB = zb_and(n3802, n4058);
    let n4060: ZB = zb_and(n4047, n4059);
    let n4061: ZB = zb_and(n4048, n4059);
    let n4062: ZB = zb_or(n4060, n4061);
    let n4063: ZB = zb_and(n3906, n4021);
    let n4064: ZB = zb_and(n3906, n4022);
    let n4065: ZB = zb_or(n4063, n4064);
    let n4066: ZB = zb_or(n3762, n4065);
    let n4067: ZB = zb_and(n3801, n4066);
    let n4068: ZB = zb_and(n3802, n4066);
    let n4069: ZB = zb_or(n4067, n4068);
    let n4070: ZB = zb_and(n3802, n4069);
    let n4071: ZB = zb_and(n4047, n4070);
    let n4072: ZB = zb_and(n4048, n4070);
    let n4073: ZB = zb_or(n4071, n4072);
    let n4074: ZB = zb_or(n4027, n4028);
    let n4075: ZB = zb_or(n3762, n4074);
    let n4076: ZB = zb_and(n3801, n4075);
    let n4077: ZB = zb_and(n3802, n4075);
    let n4078: ZB = zb_or(n4076, n4077);
    let n4079: ZB = zb_and(n3802, n4078);
    let n4080: ZB = zb_and(n4047, n4079);
    let n4081: ZB = zb_and(n4048, n4079);
    let n4082: ZB = zb_or(n4080, n4081);
    let n4083: ZB = zb_and(n3942, n4021);
    let n4084: ZB = zb_and(n3942, n4022);
    let n4085: ZB = zb_and(n3726, n4083);
    let n4086: ZB = zb_and(n3747, n4083);
    let n4087: ZB = zb_or(n4085, n4086);
    let n4088: ZB = zb_and(n3749, n4087);
    let n4089: ZB = zb_and(n3750, n4087);
    let n4090: ZB = zb_and(n3751, n4089);
    let n4091: ZB = zb_and(n3752, n4089);
    let n4092: ZB = zb_or(n4090, n4091);
    let n4093: ZB = zb_or(n4088, n4092);
    let n4094: ZB = zb_and(n3754, n4093);
    let n4095: ZB = zb_and(n3753, n4093);
    let n4096: ZB = zb_or(n4094, n4095);
    let n4097: ZB = zb_or(n4084, n4096);
    let n4098: ZB = zb_or(n3762, n4097);
    let n4099: ZB = zb_and(n3801, n4098);
    let n4100: ZB = zb_and(n3802, n4098);
    let n4101: ZB = zb_or(n4099, n4100);
    let n4102: ZB = zb_and(n3802, n4101);
    let n4103: ZB = zb_and(n4047, n4102);
    let n4104: ZB = zb_and(n4048, n4102);
    let n4105: ZB = zb_or(n4103, n4104);
    let n4106: ZB = zb_and(n3978, n4021);
    let n4107: ZB = zb_and(n3978, n4022);
    let n4108: ZB = zb_or(n4106, n4107);
    let n4109: ZB = zb_or(n3762, n4108);
    let n4110: ZB = zb_and(n3801, n4109);
    let n4111: ZB = zb_and(n3802, n4109);
    let n4112: ZB = zb_or(n4110, n4111);
    let n4113: ZB = zb_and(n3802, n4112);
    let n4114: ZB = zb_and(n4047, n4113);
    let n4115: ZB = zb_and(n4048, n4113);
    let n4116: ZB = zb_or(n4114, n4115);
    let n4117: ZB = zb_and(n4014, n4021);
    let n4118: ZB = zb_and(n4014, n4022);
    let n4119: ZB = zb_or(n4117, n4118);
    let n4120: ZB = zb_or(n3762, n4119);
    let n4121: ZB = zb_and(n3801, n4120);
    let n4122: ZB = zb_and(n3802, n4120);
    let n4123: ZB = zb_or(n4121, n4122);
    let n4124: ZB = zb_and(n3802, n4123);
    let n4125: ZB = zb_and(n4047, n4124);
    let n4126: ZB = zb_and(n4048, n4124);
    let n4127: ZB = zb_or(n4125, n4126);
    let n4128: ZB = zb_or(n4083, n4084);
    let n4129: ZB = zb_or(n3762, n4128);
    let n4130: ZB = zb_and(n3801, n4129);
    let n4131: ZB = zb_and(n3802, n4129);
    let n4132: ZB = zb_or(n4130, n4131);
    let n4133: ZB = zb_and(n3802, n4132);
    let n4134: ZB = zb_and(n4047, n4133);
    let n4135: ZB = zb_and(n4048, n4133);
    let n4136: ZB = zb_or(n4134, n4135);
    let n4140: ZB = zb_and(n3656, n3659);
    let n4141: ZB = zb_and(n3669, n4140);
    let n4142: ZB = zb_and(n3668, n4140);
    let n4143: ZB = zb_or(n4141, n4142);
    let n4144: ZB = zb_and(n3669, n4143);
    let n4145: ZB = zb_and(n3668, n4143);
    let n4146: ZB = zb_or(n4144, n4145);
    let n4147: ZB = zb_and(n3668, n4146);
    let n4148: ZB = zb_and(n3669, n4146);
    let n4149: ZB = zb_and(n3676, n4147);
    let n4150: ZB = zb_and(n3677, n4147);
    let n4151: ZB = zb_or(n4149, n4150);
    let n4152: ZB = zb_and(n286, n4148);
    let n4153: ZB = zb_and(n287, n4148);
    let n4154: ZB = zb_or(n4152, n4153);
    let n4155: ZB = zb_or(n4151, n4154);
    let n4156: ZB = zb_and(n291, n4155);
    let n4157: ZB = zb_and(n292, n4155);
    let n4158: ZB = zb_and(n3690, n4156);
    let n4159: ZB = zb_and(n3691, n4156);
    let n4160: ZB = zb_or(n4158, n4159);
    let n4161: ZB = zb_and(n3692, n4160);
    let n4162: ZB = zb_and(n3693, n4160);
    let n4163: ZB = zb_or(n4161, n4162);
    let n4164: ZB = zb_and(n3669, n4157);
    let n4165: ZB = zb_and(n3668, n4157);
    let n4166: ZB = zb_or(n4164, n4165);
    let n4167: ZB = zb_and(n3696, n4166);
    let n4168: ZB = zb_and(n3697, n4166);
    let n4169: ZB = zb_and(n3698, n4167);
    let n4170: ZB = zb_and(n3002, n4167);
    let n4171: ZB = zb_and(n3699, n4170);
    let n4172: ZB = zb_and(n3027, n4170);
    let n4173: ZB = zb_and(n3700, n4169);
    let n4174: ZB = zb_and(n3701, n4169);
    let n4175: ZB = zb_and(n3706, n4171);
    let n4176: ZB = zb_and(n3707, n4171);
    let n4177: ZB = zb_and(n3002, n4172);
    let n4178: ZB = zb_or(n4175, n4176);
    let n4179: ZB = zb_or(n4173, n4174);
    let n4180: ZB = zb_or(n4177, n4178);
    let n4181: ZB = zb_or(n4179, n4180);
    let n4182: ZB = zb_and(n3698, n4168);
    let n4183: ZB = zb_and(n3002, n4168);
    let n4184: ZB = zb_or(n4182, n4183);
    let n4185: ZB = zb_or(n4181, n4184);
    let n4186: ZB = zb_and(n3724, n4185);
    let n4187: ZB = zb_and(n3723, n4185);
    let n4188: ZB = zb_or(n4186, n4187);
    let n4189: ZB = zb_and(n3728, n4188);
    let n4190: ZB = zb_and(n3729, n4188);
    let n4191: ZB = zb_or(n4189, n4190);
    let n4192: ZB = zb_and(n3669, n4191);
    let n4193: ZB = zb_and(n3668, n4191);
    let n4194: ZB = zb_and(n3731, n4192);
    let n4195: ZB = zb_and(n3732, n4192);
    let n4196: ZB = zb_or(n4194, n4195);
    let n4197: ZB = zb_or(n4193, n4196);
    let n4198: ZB = zb_and(n3745, n4197);
    let n4199: ZB = zb_and(n3746, n4197);
    let n4200: ZB = zb_or(n4198, n4199);
    let n4201: ZB = zb_or(n4163, n4200);
    let n4202: ZB = zb_and(n3801, n4201);
    let n4203: ZB = zb_and(n3802, n4201);
    let n4204: ZB = zb_or(n4202, n4203);
    let n4205: ZB = zb_and(n3801, n4204);
    let n4206: ZB = zb_and(n3801, n3805);
    let n4207: ZB = zb_not(n4205);
    let n4208: ZB = zb_or(n4205, n4206);
    let n4209: ZB = zsel_b(n4205, n3657, n3665);
    let n4211: ZN = zsel_n(n4205, r_c87, n3809);
    let n4212: ZN = zsel_n(n4205, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4214: ZB = zb_and(n3706, n4168);
    let n4215: ZB = zb_and(n3707, n4168);
    let n4216: ZB = zb_or(n4214, n4215);
    let n4217: ZB = zb_or(n4181, n4216);
    let n4218: ZB = zb_and(n3816, n4217);
    let n4219: ZB = zb_and(n3815, n4217);
    let n4220: ZB = zb_or(n4218, n4219);
    let n4221: ZB = zb_and(n3728, n4220);
    let n4222: ZB = zb_and(n3729, n4220);
    let n4223: ZB = zb_or(n4221, n4222);
    let n4224: ZB = zb_and(n3821, n4223);
    let n4225: ZB = zb_and(n3820, n4223);
    let n4226: ZB = zb_or(n4224, n4225);
    let n4227: ZB = zb_and(n3821, n4226);
    let n4228: ZB = zb_and(n3820, n4226);
    let n4229: ZB = zb_or(n4227, n4228);
    let n4230: ZB = zb_and(n3820, n4229);
    let n4231: ZB = zb_and(n3821, n4229);
    let n4232: ZB = zb_or(n4230, n4231);
    let n4233: ZB = zb_and(n3820, n4232);
    let n4234: ZB = zb_and(n3821, n4232);
    let n4235: ZB = zb_or(n4233, n4234);
    let n4236: ZB = zb_and(n3669, n4235);
    let n4237: ZB = zb_and(n3668, n4235);
    let n4238: ZB = zb_and(n3823, n4236);
    let n4239: ZB = zb_and(n3824, n4236);
    let n4240: ZB = zb_or(n4238, n4239);
    let n4241: ZB = zb_or(n4237, n4240);
    let n4242: ZB = zb_and(n3745, n4241);
    let n4243: ZB = zb_and(n3746, n4241);
    let n4244: ZB = zb_or(n4242, n4243);
    let n4245: ZB = zb_or(n4163, n4244);
    let n4246: ZB = zb_and(n3801, n4245);
    let n4247: ZB = zb_and(n3802, n4245);
    let n4248: ZB = zb_or(n4246, n4247);
    let n4249: ZB = zb_and(n3801, n4248);
    let n4250: ZB = zb_and(n3801, n3859);
    let n4251: ZB = zb_not(n4249);
    let n4252: ZB = zb_or(n4249, n4250);
    let n4253: ZB = zsel_b(n4249, n3657, n3665);
    let n4255: ZN = zsel_n(n4249, r_c87, n3809);
    let n4256: ZN = zsel_n(n4249, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4258: ZB = zb_and(n3700, n4168);
    let n4259: ZB = zb_and(n3701, n4168);
    let n4260: ZB = zb_or(n4258, n4259);
    let n4261: ZB = zb_or(n4181, n4260);
    let n4262: ZB = zb_and(n3867, n4261);
    let n4263: ZB = zb_and(n3866, n4261);
    let n4264: ZB = zb_or(n4262, n4263);
    let n4265: ZB = zb_and(n3728, n4264);
    let n4266: ZB = zb_and(n3729, n4264);
    let n4267: ZB = zb_or(n4265, n4266);
    let n4268: ZB = zb_and(n3872, n4267);
    let n4269: ZB = zb_and(n3871, n4267);
    let n4270: ZB = zb_or(n4268, n4269);
    let n4271: ZB = zb_and(n3872, n4270);
    let n4272: ZB = zb_and(n3871, n4270);
    let n4273: ZB = zb_or(n4271, n4272);
    let n4274: ZB = zb_and(n3871, n4273);
    let n4275: ZB = zb_and(n3872, n4273);
    let n4276: ZB = zb_or(n4274, n4275);
    let n4277: ZB = zb_and(n3871, n4276);
    let n4278: ZB = zb_and(n3872, n4276);
    let n4279: ZB = zb_or(n4277, n4278);
    let n4280: ZB = zb_and(n3669, n4279);
    let n4281: ZB = zb_and(n3668, n4279);
    let n4282: ZB = zb_and(n3874, n4280);
    let n4283: ZB = zb_and(n3875, n4280);
    let n4284: ZB = zb_or(n4282, n4283);
    let n4285: ZB = zb_or(n4281, n4284);
    let n4286: ZB = zb_and(n3745, n4285);
    let n4287: ZB = zb_and(n3746, n4285);
    let n4288: ZB = zb_or(n4286, n4287);
    let n4289: ZB = zb_or(n4163, n4288);
    let n4290: ZB = zb_and(n3801, n4289);
    let n4291: ZB = zb_and(n3802, n4289);
    let n4292: ZB = zb_or(n4290, n4291);
    let n4293: ZB = zb_and(n3801, n4292);
    let n4294: ZB = zb_and(n3801, n3910);
    let n4295: ZB = zb_not(n4293);
    let n4296: ZB = zb_or(n4293, n4294);
    let n4297: ZB = zsel_b(n4293, n3657, n3665);
    let n4299: ZN = zsel_n(n4293, r_c87, n3809);
    let n4300: ZN = zsel_n(n4293, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4302: ZB = zb_and(n140, n4197);
    let n4303: ZB = zb_and(r_c294, n4197);
    let n4304: ZB = zb_and(n3733, n4302);
    let n4305: ZB = zb_and(n3734, n4302);
    let n4306: ZB = zb_and(n3737, n4305);
    let n4307: ZB = zb_and(n3736, n4305);
    let n4308: ZB = zb_or(n4306, n4307);
    let n4309: ZB = zb_and(n3737, n4308);
    let n4310: ZB = zb_and(n3736, n4308);
    let n4311: ZB = zb_or(n4309, n4310);
    let n4312: ZB = zb_and(n3736, n4311);
    let n4313: ZB = zb_and(n3737, n4311);
    let n4314: ZB = zb_and(n3740, n4313);
    let n4315: ZB = zb_and(n3739, n4313);
    let n4316: ZB = zb_or(n4314, n4315);
    let n4317: ZB = zb_and(n3740, n4316);
    let n4318: ZB = zb_and(n3739, n4316);
    let n4319: ZB = zb_or(n4317, n4318);
    let n4320: ZB = zb_and(n3739, n4319);
    let n4321: ZB = zb_and(n3740, n4319);
    let n4322: ZB = zb_or(n4320, n4321);
    let n4323: ZB = zb_or(n4312, n4322);
    let n4324: ZB = zb_and(n3744, n4323);
    let n4325: ZB = zb_and(n3743, n4323);
    let n4326: ZB = zb_or(n4324, n4325);
    let n4327: ZB = zb_or(n4304, n4326);
    let n4328: ZB = zb_or(n4303, n4327);
    let n4329: ZB = zb_and(n3745, n4328);
    let n4330: ZB = zb_and(n3746, n4328);
    let n4331: ZB = zb_or(n4329, n4330);
    let n4332: ZB = zb_or(n4163, n4331);
    let n4333: ZB = zb_and(n3801, n4332);
    let n4334: ZB = zb_and(n3802, n4332);
    let n4335: ZB = zb_or(n4333, n4334);
    let n4336: ZB = zb_and(n3801, n4335);
    let n4337: ZB = zb_and(n3801, n3946);
    let n4338: ZB = zb_not(n4336);
    let n4339: ZB = zb_or(n4336, n4337);
    let n4340: ZB = zsel_b(n4336, n3657, n3665);
    let n4342: ZN = zsel_n(n4336, r_c87, n3809);
    let n4343: ZN = zsel_n(n4336, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4345: ZB = zb_and(n140, n4241);
    let n4346: ZB = zb_and(r_c294, n4241);
    let n4347: ZB = zb_and(n3733, n4345);
    let n4348: ZB = zb_and(n3734, n4345);
    let n4349: ZB = zb_and(n3737, n4348);
    let n4350: ZB = zb_and(n3736, n4348);
    let n4351: ZB = zb_or(n4349, n4350);
    let n4352: ZB = zb_and(n3737, n4351);
    let n4353: ZB = zb_and(n3736, n4351);
    let n4354: ZB = zb_or(n4352, n4353);
    let n4355: ZB = zb_and(n3736, n4354);
    let n4356: ZB = zb_and(n3737, n4354);
    let n4357: ZB = zb_and(n3740, n4356);
    let n4358: ZB = zb_and(n3739, n4356);
    let n4359: ZB = zb_or(n4357, n4358);
    let n4360: ZB = zb_and(n3740, n4359);
    let n4361: ZB = zb_and(n3739, n4359);
    let n4362: ZB = zb_or(n4360, n4361);
    let n4363: ZB = zb_and(n3739, n4362);
    let n4364: ZB = zb_and(n3740, n4362);
    let n4365: ZB = zb_or(n4363, n4364);
    let n4366: ZB = zb_or(n4355, n4365);
    let n4367: ZB = zb_and(n3744, n4366);
    let n4368: ZB = zb_and(n3743, n4366);
    let n4369: ZB = zb_or(n4367, n4368);
    let n4370: ZB = zb_or(n4347, n4369);
    let n4371: ZB = zb_or(n4346, n4370);
    let n4372: ZB = zb_and(n3745, n4371);
    let n4373: ZB = zb_and(n3746, n4371);
    let n4374: ZB = zb_or(n4372, n4373);
    let n4375: ZB = zb_or(n4163, n4374);
    let n4376: ZB = zb_and(n3801, n4375);
    let n4377: ZB = zb_and(n3802, n4375);
    let n4378: ZB = zb_or(n4376, n4377);
    let n4379: ZB = zb_and(n3801, n4378);
    let n4380: ZB = zb_and(n3801, n3982);
    let n4381: ZB = zb_not(n4379);
    let n4382: ZB = zb_or(n4379, n4380);
    let n4383: ZB = zsel_b(n4379, n3657, n3665);
    let n4385: ZN = zsel_n(n4379, r_c87, n3809);
    let n4386: ZN = zsel_n(n4379, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4388: ZB = zb_and(n140, n4285);
    let n4389: ZB = zb_and(r_c294, n4285);
    let n4390: ZB = zb_and(n3733, n4388);
    let n4391: ZB = zb_and(n3734, n4388);
    let n4392: ZB = zb_and(n3737, n4391);
    let n4393: ZB = zb_and(n3736, n4391);
    let n4394: ZB = zb_or(n4392, n4393);
    let n4395: ZB = zb_and(n3737, n4394);
    let n4396: ZB = zb_and(n3736, n4394);
    let n4397: ZB = zb_or(n4395, n4396);
    let n4398: ZB = zb_and(n3736, n4397);
    let n4399: ZB = zb_and(n3737, n4397);
    let n4400: ZB = zb_and(n3740, n4399);
    let n4401: ZB = zb_and(n3739, n4399);
    let n4402: ZB = zb_or(n4400, n4401);
    let n4403: ZB = zb_and(n3740, n4402);
    let n4404: ZB = zb_and(n3739, n4402);
    let n4405: ZB = zb_or(n4403, n4404);
    let n4406: ZB = zb_and(n3739, n4405);
    let n4407: ZB = zb_and(n3740, n4405);
    let n4408: ZB = zb_or(n4406, n4407);
    let n4409: ZB = zb_or(n4398, n4408);
    let n4410: ZB = zb_and(n3744, n4409);
    let n4411: ZB = zb_and(n3743, n4409);
    let n4412: ZB = zb_or(n4410, n4411);
    let n4413: ZB = zb_or(n4390, n4412);
    let n4414: ZB = zb_or(n4389, n4413);
    let n4415: ZB = zb_and(n3745, n4414);
    let n4416: ZB = zb_and(n3746, n4414);
    let n4417: ZB = zb_or(n4415, n4416);
    let n4418: ZB = zb_or(n4163, n4417);
    let n4419: ZB = zb_and(n3801, n4418);
    let n4420: ZB = zb_and(n3802, n4418);
    let n4421: ZB = zb_or(n4419, n4420);
    let n4422: ZB = zb_and(n3801, n4421);
    let n4423: ZB = zb_and(n3801, n4018);
    let n4424: ZB = zb_not(n4422);
    let n4425: ZB = zb_or(n4422, n4423);
    let n4426: ZB = zsel_b(n4422, n3657, n3665);
    let n4428: ZN = zsel_n(n4422, r_c87, n3809);
    let n4429: ZN = zsel_n(n4422, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4431: ZB = zb_and(n4021, n4200);
    let n4432: ZB = zb_and(n4022, n4200);
    let n4433: ZB = zb_and(n3726, n4431);
    let n4434: ZB = zb_and(n3747, n4431);
    let n4435: ZB = zb_or(n4433, n4434);
    let n4436: ZB = zb_and(n3749, n4435);
    let n4437: ZB = zb_and(n3750, n4435);
    let n4438: ZB = zb_and(n3751, n4437);
    let n4439: ZB = zb_and(n3752, n4437);
    let n4440: ZB = zb_or(n4438, n4439);
    let n4441: ZB = zb_or(n4436, n4440);
    let n4442: ZB = zb_and(n3754, n4441);
    let n4443: ZB = zb_and(n3753, n4441);
    let n4444: ZB = zb_or(n4442, n4443);
    let n4445: ZB = zb_or(n4432, n4444);
    let n4446: ZB = zb_or(n4163, n4445);
    let n4447: ZB = zb_and(n3801, n4446);
    let n4448: ZB = zb_and(n3802, n4446);
    let n4449: ZB = zb_or(n4447, n4448);
    let n4450: ZB = zb_and(n3801, n4449);
    let n4451: ZB = zb_and(n3801, n4045);
    let n4452: ZB = zb_not(n4450);
    let n4453: ZB = zb_or(n4450, n4451);
    let n4454: ZB = zsel_b(n4450, n3657, n3665);
    let n4455: ZB = zb_and(n4047, n4453);
    let n4456: ZB = zb_and(n4048, n4453);
    let n4457: ZB = zb_or(n4455, n4456);
    let n4458: ZN = zsel_n(n4450, r_c87, n3809);
    let n4459: ZN = zsel_n(n4450, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4461: ZB = zb_and(n4021, n4244);
    let n4462: ZB = zb_and(n4022, n4244);
    let n4463: ZB = zb_or(n4461, n4462);
    let n4464: ZB = zb_or(n4163, n4463);
    let n4465: ZB = zb_and(n3801, n4464);
    let n4466: ZB = zb_and(n3802, n4464);
    let n4467: ZB = zb_or(n4465, n4466);
    let n4468: ZB = zb_and(n3801, n4467);
    let n4469: ZB = zb_and(n3801, n4058);
    let n4470: ZB = zb_not(n4468);
    let n4471: ZB = zb_or(n4468, n4469);
    let n4472: ZB = zsel_b(n4468, n3657, n3665);
    let n4473: ZB = zb_and(n4047, n4471);
    let n4474: ZB = zb_and(n4048, n4471);
    let n4475: ZB = zb_or(n4473, n4474);
    let n4476: ZN = zsel_n(n4468, r_c87, n3809);
    let n4477: ZN = zsel_n(n4468, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4479: ZB = zb_and(n4021, n4288);
    let n4480: ZB = zb_and(n4022, n4288);
    let n4481: ZB = zb_or(n4479, n4480);
    let n4482: ZB = zb_or(n4163, n4481);
    let n4483: ZB = zb_and(n3801, n4482);
    let n4484: ZB = zb_and(n3802, n4482);
    let n4485: ZB = zb_or(n4483, n4484);
    let n4486: ZB = zb_and(n3801, n4485);
    let n4487: ZB = zb_and(n3801, n4069);
    let n4488: ZB = zb_not(n4486);
    let n4489: ZB = zb_or(n4486, n4487);
    let n4490: ZB = zsel_b(n4486, n3657, n3665);
    let n4491: ZB = zb_and(n4047, n4489);
    let n4492: ZB = zb_and(n4048, n4489);
    let n4493: ZB = zb_or(n4491, n4492);
    let n4494: ZN = zsel_n(n4486, r_c87, n3809);
    let n4495: ZN = zsel_n(n4486, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4497: ZB = zb_or(n4431, n4432);
    let n4498: ZB = zb_or(n4163, n4497);
    let n4499: ZB = zb_and(n3801, n4498);
    let n4500: ZB = zb_and(n3802, n4498);
    let n4501: ZB = zb_or(n4499, n4500);
    let n4502: ZB = zb_and(n3801, n4501);
    let n4503: ZB = zb_and(n3801, n4078);
    let n4504: ZB = zb_not(n4502);
    let n4505: ZB = zb_or(n4502, n4503);
    let n4506: ZB = zsel_b(n4502, n3657, n3665);
    let n4507: ZB = zb_and(n4047, n4505);
    let n4508: ZB = zb_and(n4048, n4505);
    let n4509: ZB = zb_or(n4507, n4508);
    let n4510: ZN = zsel_n(n4502, r_c87, n3809);
    let n4511: ZN = zsel_n(n4502, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4513: ZB = zb_and(n4021, n4331);
    let n4514: ZB = zb_and(n4022, n4331);
    let n4515: ZB = zb_and(n3726, n4513);
    let n4516: ZB = zb_and(n3747, n4513);
    let n4517: ZB = zb_or(n4515, n4516);
    let n4518: ZB = zb_and(n3749, n4517);
    let n4519: ZB = zb_and(n3750, n4517);
    let n4520: ZB = zb_and(n3751, n4519);
    let n4521: ZB = zb_and(n3752, n4519);
    let n4522: ZB = zb_or(n4520, n4521);
    let n4523: ZB = zb_or(n4518, n4522);
    let n4524: ZB = zb_and(n3754, n4523);
    let n4525: ZB = zb_and(n3753, n4523);
    let n4526: ZB = zb_or(n4524, n4525);
    let n4527: ZB = zb_or(n4514, n4526);
    let n4528: ZB = zb_or(n4163, n4527);
    let n4529: ZB = zb_and(n3801, n4528);
    let n4530: ZB = zb_and(n3802, n4528);
    let n4531: ZB = zb_or(n4529, n4530);
    let n4532: ZB = zb_and(n3801, n4531);
    let n4533: ZB = zb_and(n3801, n4101);
    let n4534: ZB = zb_not(n4532);
    let n4535: ZB = zb_or(n4532, n4533);
    let n4536: ZB = zsel_b(n4532, n3657, n3665);
    let n4537: ZB = zb_and(n4047, n4535);
    let n4538: ZB = zb_and(n4048, n4535);
    let n4539: ZB = zb_or(n4537, n4538);
    let n4540: ZN = zsel_n(n4532, r_c87, n3809);
    let n4541: ZN = zsel_n(n4532, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4543: ZB = zb_and(n4021, n4374);
    let n4544: ZB = zb_and(n4022, n4374);
    let n4545: ZB = zb_or(n4543, n4544);
    let n4546: ZB = zb_or(n4163, n4545);
    let n4547: ZB = zb_and(n3801, n4546);
    let n4548: ZB = zb_and(n3802, n4546);
    let n4549: ZB = zb_or(n4547, n4548);
    let n4550: ZB = zb_and(n3801, n4549);
    let n4551: ZB = zb_and(n3801, n4112);
    let n4552: ZB = zb_not(n4550);
    let n4553: ZB = zb_or(n4550, n4551);
    let n4554: ZB = zsel_b(n4550, n3657, n3665);
    let n4555: ZB = zb_and(n4047, n4553);
    let n4556: ZB = zb_and(n4048, n4553);
    let n4557: ZB = zb_or(n4555, n4556);
    let n4558: ZN = zsel_n(n4550, r_c87, n3809);
    let n4559: ZN = zsel_n(n4550, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4561: ZB = zb_and(n4021, n4417);
    let n4562: ZB = zb_and(n4022, n4417);
    let n4563: ZB = zb_or(n4561, n4562);
    let n4564: ZB = zb_or(n4163, n4563);
    let n4565: ZB = zb_and(n3801, n4564);
    let n4566: ZB = zb_and(n3802, n4564);
    let n4567: ZB = zb_or(n4565, n4566);
    let n4568: ZB = zb_and(n3801, n4567);
    let n4569: ZB = zb_and(n3801, n4123);
    let n4570: ZB = zb_not(n4568);
    let n4571: ZB = zb_or(n4568, n4569);
    let n4572: ZB = zsel_b(n4568, n3657, n3665);
    let n4573: ZB = zb_and(n4047, n4571);
    let n4574: ZB = zb_and(n4048, n4571);
    let n4575: ZB = zb_or(n4573, n4574);
    let n4576: ZN = zsel_n(n4568, r_c87, n3809);
    let n4577: ZN = zsel_n(n4568, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4579: ZB = zb_or(n4513, n4514);
    let n4580: ZB = zb_or(n4163, n4579);
    let n4581: ZB = zb_and(n3801, n4580);
    let n4582: ZB = zb_and(n3802, n4580);
    let n4583: ZB = zb_or(n4581, n4582);
    let n4584: ZB = zb_and(n3801, n4583);
    let n4585: ZB = zb_and(n3801, n4132);
    let n4586: ZB = zb_not(n4584);
    let n4587: ZB = zb_or(n4584, n4585);
    let n4588: ZB = zsel_b(n4584, n3657, n3665);
    let n4589: ZB = zb_and(n4047, n4587);
    let n4590: ZB = zb_and(n4048, n4587);
    let n4591: ZB = zb_or(n4589, n4590);
    let n4592: ZN = zsel_n(n4584, r_c87, n3809);
    let n4593: ZN = zsel_n(n4584, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4595: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n4596: ZB = zb_and(r_c293, n295);
    let n4597: ZB = zb_and(r_c294, n295);
    let n4598: ZN = zn_sub(n2445, zn_splat(P8::from_raw(32768i32)));
    let n4599: ZN = zn_sub(n4598, n2446);
    let n4600: ZN = zsel_n(n2581, zn_splat(P8::from_raw(0i32)), n4599);
    let n4601: ZN = zsel_n(n2576, n4599, n4600);
    let n4602: ZN = zsel_n(n2564, zn_splat(P8::from_raw(0i32)), n4601);
    let n4603: ZN = zsel_n(n2559, n4599, n4602);
    let n4604: ZN = zsel_n(n2547, zn_splat(P8::from_raw(0i32)), n4603);
    let n4605: ZN = zsel_n(n2542, n4599, n4604);
    let n4606: ZN = zsel_n(n2530, zn_splat(P8::from_raw(0i32)), n4605);
    let n4607: ZN = zsel_n(n2525, n4599, n4606);
    let n4608: ZN = zsel_n(n2513, zn_splat(P8::from_raw(0i32)), n4607);
    let n4609: ZN = zsel_n(n2508, n4599, n4608);
    let n4610: ZN = zsel_n(n2496, zn_splat(P8::from_raw(0i32)), n4609);
    let n4611: ZN = zsel_n(n2491, n4599, n4610);
    let n4612: ZN = zsel_n(n2479, zn_splat(P8::from_raw(0i32)), n4611);
    let n4613: ZN = zsel_n(n2474, n4599, n4612);
    let n4614: ZN = zsel_n(n2462, zn_splat(P8::from_raw(0i32)), n4613);
    let n4615: ZN = zn_sub(n2654, zn_splat(P8::from_raw(32768i32)));
    let n4616: ZN = zn_sub(n4615, n2655);
    let n4617: ZN = zsel_n(n2841, zn_splat(P8::from_raw(0i32)), n4616);
    let n4618: ZN = zsel_n(n2830, n4616, n4617);
    let n4619: ZN = zsel_n(n2818, zn_splat(P8::from_raw(0i32)), n4618);
    let n4620: ZN = zsel_n(n2807, n4616, n4619);
    let n4621: ZN = zsel_n(n2795, zn_splat(P8::from_raw(0i32)), n4620);
    let n4622: ZN = zsel_n(n2784, n4616, n4621);
    let n4623: ZN = zsel_n(n2772, zn_splat(P8::from_raw(0i32)), n4622);
    let n4624: ZN = zsel_n(n2761, n4616, n4623);
    let n4625: ZN = zsel_n(n2749, zn_splat(P8::from_raw(0i32)), n4624);
    let n4626: ZN = zsel_n(n2738, n4616, n4625);
    let n4627: ZN = zsel_n(n2726, zn_splat(P8::from_raw(0i32)), n4626);
    let n4628: ZN = zsel_n(n2715, n4616, n4627);
    let n4629: ZN = zsel_n(n2703, zn_splat(P8::from_raw(0i32)), n4628);
    let n4630: ZN = zsel_n(n2692, n4616, n4629);
    let n4631: ZN = zsel_n(n2680, zn_splat(P8::from_raw(0i32)), n4630);
    let n4632: ZN = zsel_n(n2440, n4614, r_c367);
    let n4633: ZN = zsel_n(n2440, n4631, r_c368);
    let n4634: ZN = zn_sub(n2916, r_c357);
    let n4635: ZN = zn_max(r_c359, n4634);
    let n4636: ZN = zn_add(r_c357, n2916);
    let n4637: ZN = zn_min(r_c359, n4636);
    let n4638: ZN = zsel_n(n3690, n4635, n4637);
    let n4639: ZN = zn_sub(n2917, r_c358);
    let n4640: ZN = zn_max(r_c360, n4639);
    let n4641: ZN = zn_add(r_c358, n2917);
    let n4642: ZN = zn_min(r_c360, n4641);
    let n4643: ZN = zsel_n(n3692, n4640, n4642);
    let n4644: ZN = zsel_n(n3728, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(13762i32)));
    let n4645: ZN = zn_sub(n2917, n4644);
    let n4646: ZN = zn_max(zn_splat(P8::from_raw(131072i32)), n4645);
    let n4647: ZN = zn_add(n2917, n4644);
    let n4648: ZN = zn_min(zn_splat(P8::from_raw(131072i32)), n4647);
    let n4649: ZN = zsel_n(n3731, n4646, n4648);
    let n4650: ZN = zsel_n(n3669, n4649, n2917);
    let n4651: ZN = zn_neg(n3742);
    let n4652: ZN = zn_mul(n4651, zn_splat(P8::from_raw(131072i32)));
    let n4653: ZN = zsel_n(n3744, n4652, n3722);
    let n4654: ZN = zsel_n(n3744, zn_splat(P8::from_raw(-131072i32)), n4650);
    let n4655: ZN = zsel_n(n3733, zn_splat(P8::from_raw(0i32)), n3680);
    let n4656: ZN = zsel_n(n3733, n3722, n4653);
    let n4657: ZN = zsel_n(n3733, zn_splat(P8::from_raw(-131072i32)), n4654);
    let n4658: ZN = zn_sub(n3679, zn_splat(P8::from_raw(65536i32)));
    let n4659: ZN = zsel_n(n3751, zn_splat(P8::from_raw(-131072i32)), zn_splat(P8::from_raw(0i32)));
    let n4660: ZN = zsel_n(n3749, zn_splat(P8::from_raw(131072i32)), n4659);
    let n4661: ZN = zsel_n(n3754, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n4662: ZB = zsel_b(n291, r_c361, n3726);
    let n4663: ZN = zsel_n(n291, n4638, n3722);
    let n4664: ZN = zsel_n(n291, n4643, n4650);
    let n4665: ZB = zb_and(n3802, n4204);
    let n4666: ZN = zsel_n(n295, n4595, r_c20);
    let n4667: ZN = zsel_n(n295, r_c241, n303);
    let n4668: ZN = zsel_n(n295, r_c254, n304);
    let n4669: ZN = zsel_n(n295, r_c281, n290);
    let n4670: ZN = zsel_n(n295, r_c283, n294);
    let n4671: ZN = zsel_n(n295, r_c284, n3679);
    let n4672: ZN = zsel_n(n295, r_c286, n3680);
    let n4673: ZN = zsel_n(n295, r_c300, n2914);
    let n4674: ZN = zsel_n(n295, r_c301, n2915);
    let n4675: ZB = zsel_b(n295, r_c361, n4662);
    let n4676: ZN = zsel_n(n295, r_c367, n4632);
    let n4677: ZN = zsel_n(n295, r_c368, n4633);
    let n4678: ZN = zsel_n(n295, r_c369, n4663);
    let n4679: ZN = zsel_n(n295, r_c370, n4664);
    let n4680: ZB = zb_or(n295, n4665);
    let n4681: ZB = zb_or(n295, n3657);
    let n4682: ZB = zn_gt(n4666, zn_splat(P8::from_raw(0i32)));
    let n4683: ZB = zn_le(n4666, zn_splat(P8::from_raw(0i32)));
    let n4684: ZB = zb_and(n4680, n4682);
    let n4685: ZB = zb_and(n4680, n4683);
    let n4686: ZB = zn_lt(n4673, zn_splat(P8::from_raw(-65536i32)));
    let n4687: ZB = zn_ge(n4673, zn_splat(P8::from_raw(-65536i32)));
    let n4688: ZB = zb_and(n4685, n4687);
    let n4689: ZB = zb_and(n4685, n4686);
    let n4690: ZB = zn_gt(n4673, zn_splat(P8::from_raw(7929856i32)));
    let n4691: ZB = zb_or(n4688, n4689);
    let n4692: ZB = zb_or(n4686, n4690);
    let n4693: ZB = zb_not(n4692);
    let n4694: ZB = zb_and(n4691, n4692);
    let n4695: ZB = zb_and(n4691, n4693);
    let n4696: ZN = zn_min(zn_splat(P8::from_raw(7929856i32)), n4673);
    let n4697: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n4696);
    let n4698: ZN = zsel_n(n4692, n4697, n4673);
    let n4699: ZN = zsel_n(n4692, zn_splat(P8::from_raw(0i32)), n4678);
    let n4700: ZB = zb_or(n4694, n4695);
    let n4701: ZN = zsel_n(n4682, n4673, n4698);
    let n4702: ZN = zsel_n(n4682, n4678, n4699);
    let n4703: ZB = zb_or(n4684, n4700);
    let n4705: ZN = zn_max(n3822, n4645);
    let n4706: ZN = zn_min(n3822, n4647);
    let n4707: ZN = zsel_n(n3823, n4705, n4706);
    let n4708: ZN = zsel_n(n3669, n4707, n2917);
    let n4709: ZN = zsel_n(n3744, n4652, n3814);
    let n4710: ZN = zsel_n(n3744, zn_splat(P8::from_raw(-131072i32)), n4708);
    let n4711: ZN = zsel_n(n3733, n3814, n4709);
    let n4712: ZN = zsel_n(n3733, zn_splat(P8::from_raw(-131072i32)), n4710);
    let n4713: ZB = zsel_b(n291, r_c361, n3818);
    let n4714: ZN = zsel_n(n291, n4638, n3814);
    let n4715: ZN = zsel_n(n291, n4643, n4708);
    let n4716: ZB = zb_and(n3802, n4248);
    let n4717: ZB = zsel_b(n295, r_c361, n4713);
    let n4718: ZN = zsel_n(n295, r_c369, n4714);
    let n4719: ZN = zsel_n(n295, r_c370, n4715);
    let n4720: ZB = zb_or(n295, n4716);
    let n4721: ZB = zb_and(n4682, n4720);
    let n4722: ZB = zb_and(n4683, n4720);
    let n4723: ZB = zb_and(n4687, n4722);
    let n4724: ZB = zb_and(n4686, n4722);
    let n4725: ZB = zb_or(n4723, n4724);
    let n4726: ZB = zb_and(n4692, n4725);
    let n4727: ZB = zb_and(n4693, n4725);
    let n4728: ZN = zsel_n(n4692, zn_splat(P8::from_raw(0i32)), n4718);
    let n4729: ZB = zb_or(n4726, n4727);
    let n4730: ZN = zsel_n(n4682, n4718, n4728);
    let n4731: ZB = zb_or(n4721, n4729);
    let n4732: ZN = zn_max(n3873, n4645);
    let n4733: ZN = zn_min(n3873, n4647);
    let n4734: ZN = zsel_n(n3874, n4732, n4733);
    let n4735: ZN = zsel_n(n3669, n4734, n2917);
    let n4736: ZN = zsel_n(n3744, n4652, n3865);
    let n4737: ZN = zsel_n(n3744, zn_splat(P8::from_raw(-131072i32)), n4735);
    let n4738: ZN = zsel_n(n3733, n3865, n4736);
    let n4739: ZN = zsel_n(n3733, zn_splat(P8::from_raw(-131072i32)), n4737);
    let n4740: ZB = zsel_b(n291, r_c361, n3869);
    let n4741: ZN = zsel_n(n291, n4638, n3865);
    let n4742: ZN = zsel_n(n291, n4643, n4735);
    let n4743: ZB = zb_and(n3802, n4292);
    let n4744: ZB = zsel_b(n295, r_c361, n4740);
    let n4745: ZN = zsel_n(n295, r_c369, n4741);
    let n4746: ZN = zsel_n(n295, r_c370, n4742);
    let n4747: ZB = zb_or(n295, n4743);
    let n4748: ZB = zb_and(n4682, n4747);
    let n4749: ZB = zb_and(n4683, n4747);
    let n4750: ZB = zb_and(n4687, n4749);
    let n4751: ZB = zb_and(n4686, n4749);
    let n4752: ZB = zb_or(n4750, n4751);
    let n4753: ZB = zb_and(n4692, n4752);
    let n4754: ZB = zb_and(n4693, n4752);
    let n4755: ZN = zsel_n(n4692, zn_splat(P8::from_raw(0i32)), n4745);
    let n4756: ZB = zb_or(n4753, n4754);
    let n4757: ZN = zsel_n(n4682, n4745, n4755);
    let n4758: ZB = zb_or(n4748, n4756);
    let n4759: ZB = zb_or(r_c294, n134);
    let n4760: ZN = zsel_n(n140, n4655, n3680);
    let n4761: ZN = zsel_n(n140, n4656, n3722);
    let n4762: ZN = zsel_n(n140, n4657, n4650);
    let n4763: ZN = zsel_n(n291, n3680, n4760);
    let n4764: ZN = zsel_n(n291, n4638, n4761);
    let n4765: ZN = zsel_n(n291, n4643, n4762);
    let n4766: ZB = zb_and(n3802, n4335);
    let n4767: ZN = zsel_n(n295, r_c286, n4763);
    let n4768: ZN = zsel_n(n295, r_c369, n4764);
    let n4769: ZN = zsel_n(n295, r_c370, n4765);
    let n4770: ZB = zb_or(n295, n4766);
    let n4771: ZB = zb_and(n4682, n4770);
    let n4772: ZB = zb_and(n4683, n4770);
    let n4773: ZB = zb_and(n4687, n4772);
    let n4774: ZB = zb_and(n4686, n4772);
    let n4775: ZB = zb_or(n4773, n4774);
    let n4776: ZB = zb_and(n4692, n4775);
    let n4777: ZB = zb_and(n4693, n4775);
    let n4778: ZN = zsel_n(n4692, zn_splat(P8::from_raw(0i32)), n4768);
    let n4779: ZB = zb_or(n4776, n4777);
    let n4780: ZN = zsel_n(n4682, n4768, n4778);
    let n4781: ZB = zb_or(n4771, n4779);
    let n4782: ZN = zsel_n(n140, n4711, n3814);
    let n4783: ZN = zsel_n(n140, n4712, n4708);
    let n4784: ZN = zsel_n(n291, n4638, n4782);
    let n4785: ZN = zsel_n(n291, n4643, n4783);
    let n4786: ZB = zb_and(n3802, n4378);
    let n4787: ZN = zsel_n(n295, r_c369, n4784);
    let n4788: ZN = zsel_n(n295, r_c370, n4785);
    let n4789: ZB = zb_or(n295, n4786);
    let n4790: ZB = zb_and(n4682, n4789);
    let n4791: ZB = zb_and(n4683, n4789);
    let n4792: ZB = zb_and(n4687, n4791);
    let n4793: ZB = zb_and(n4686, n4791);
    let n4794: ZB = zb_or(n4792, n4793);
    let n4795: ZB = zb_and(n4692, n4794);
    let n4796: ZB = zb_and(n4693, n4794);
    let n4797: ZN = zsel_n(n4692, zn_splat(P8::from_raw(0i32)), n4787);
    let n4798: ZB = zb_or(n4795, n4796);
    let n4799: ZN = zsel_n(n4682, n4787, n4797);
    let n4800: ZB = zb_or(n4790, n4798);
    let n4801: ZN = zsel_n(n140, n4738, n3865);
    let n4802: ZN = zsel_n(n140, n4739, n4735);
    let n4803: ZN = zsel_n(n291, n4638, n4801);
    let n4804: ZN = zsel_n(n291, n4643, n4802);
    let n4805: ZB = zb_and(n3802, n4421);
    let n4806: ZN = zsel_n(n295, r_c369, n4803);
    let n4807: ZN = zsel_n(n295, r_c370, n4804);
    let n4808: ZB = zb_or(n295, n4805);
    let n4809: ZB = zb_and(n4682, n4808);
    let n4810: ZB = zb_and(n4683, n4808);
    let n4811: ZB = zb_and(n4687, n4810);
    let n4812: ZB = zb_and(n4686, n4810);
    let n4813: ZB = zb_or(n4811, n4812);
    let n4814: ZB = zb_and(n4692, n4813);
    let n4815: ZB = zb_and(n4693, n4813);
    let n4816: ZN = zsel_n(n4692, zn_splat(P8::from_raw(0i32)), n4806);
    let n4817: ZB = zb_or(n4814, n4815);
    let n4818: ZN = zsel_n(n4682, n4806, n4816);
    let n4819: ZB = zb_or(n4809, n4817);
    let n4820: ZB = zb_or(r_c293, n134);
    let n4821: ZN = zsel_n(n4021, zn_splat(P8::from_raw(655360i32)), n290);
    let n4822: ZN = zsel_n(n4021, zn_splat(P8::from_raw(262144i32)), r_c283);
    let n4823: ZN = zsel_n(n4021, n4658, n3679);
    let n4824: ZN = zsel_n(n4021, zn_splat(P8::from_raw(98304i32)), r_c357);
    let n4825: ZN = zsel_n(n4021, n4661, r_c358);
    let n4826: ZN = zsel_n(n4021, n4660, r_c359);
    let n4827: ZN = zsel_n(n4021, zn_splat(P8::from_raw(0i32)), r_c360);
    let n4828: ZN = zsel_n(n4021, n3748, n3722);
    let n4829: ZN = zsel_n(n4021, zn_splat(P8::from_raw(0i32)), n4650);
    let n4830: ZN = zsel_n(n291, n290, n4821);
    let n4831: ZN = zsel_n(n291, n293, n4822);
    let n4832: ZN = zsel_n(n291, n3679, n4823);
    let n4833: ZN = zsel_n(n291, r_c357, n4824);
    let n4834: ZN = zsel_n(n291, r_c358, n4825);
    let n4835: ZN = zsel_n(n291, r_c359, n4826);
    let n4836: ZN = zsel_n(n291, r_c360, n4827);
    let n4837: ZN = zsel_n(n291, n4638, n4828);
    let n4838: ZN = zsel_n(n291, n4643, n4829);
    let n4839: ZB = zb_and(n3802, n4449);
    let n4840: ZN = zsel_n(n295, n4595, n4025);
    let n4841: ZB = zsel_b(n295, r_c41, n4026);
    let n4842: ZN = zsel_n(n295, r_c281, n4830);
    let n4843: ZN = zsel_n(n295, r_c283, n4831);
    let n4844: ZN = zsel_n(n295, r_c284, n4832);
    let n4845: ZN = zsel_n(n295, r_c357, n4833);
    let n4846: ZN = zsel_n(n295, r_c358, n4834);
    let n4847: ZN = zsel_n(n295, r_c359, n4835);
    let n4848: ZN = zsel_n(n295, r_c360, n4836);
    let n4849: ZN = zsel_n(n295, r_c369, n4837);
    let n4850: ZN = zsel_n(n295, r_c370, n4838);
    let n4851: ZB = zb_or(n295, n4839);
    let n4852: ZB = zn_gt(n4840, zn_splat(P8::from_raw(0i32)));
    let n4853: ZB = zn_le(n4840, zn_splat(P8::from_raw(0i32)));
    let n4854: ZB = zb_and(n4851, n4852);
    let n4855: ZB = zb_and(n4851, n4853);
    let n4856: ZB = zb_and(n4687, n4855);
    let n4857: ZB = zb_and(n4686, n4855);
    let n4858: ZB = zb_or(n4856, n4857);
    let n4859: ZB = zb_and(n4692, n4858);
    let n4860: ZB = zb_and(n4693, n4858);
    let n4861: ZN = zsel_n(n4692, zn_splat(P8::from_raw(0i32)), n4849);
    let n4862: ZB = zb_or(n4859, n4860);
    let n4863: ZN = zsel_n(n4852, n4673, n4698);
    let n4864: ZN = zsel_n(n4852, n4849, n4861);
    let n4865: ZB = zb_or(n4854, n4862);
    let n4866: ZN = zsel_n(n4021, zn_splat(P8::from_raw(69510i32)), r_c358);
    let n4867: ZN = zsel_n(n4021, zn_splat(P8::from_raw(-131072i32)), r_c359);
    let n4868: ZN = zsel_n(n4021, zn_splat(P8::from_raw(-327680i32)), n3814);
    let n4869: ZN = zsel_n(n4021, zn_splat(P8::from_raw(0i32)), n4708);
    let n4870: ZN = zsel_n(n291, r_c358, n4866);
    let n4871: ZN = zsel_n(n291, r_c359, n4867);
    let n4872: ZN = zsel_n(n291, n4638, n4868);
    let n4873: ZN = zsel_n(n291, n4643, n4869);
    let n4874: ZB = zb_and(n3802, n4467);
    let n4875: ZN = zsel_n(n295, r_c358, n4870);
    let n4876: ZN = zsel_n(n295, r_c359, n4871);
    let n4877: ZN = zsel_n(n295, r_c369, n4872);
    let n4878: ZN = zsel_n(n295, r_c370, n4873);
    let n4879: ZB = zb_or(n295, n4874);
    let n4880: ZB = zb_and(n4852, n4879);
    let n4881: ZB = zb_and(n4853, n4879);
    let n4882: ZB = zb_and(n4687, n4881);
    let n4883: ZB = zb_and(n4686, n4881);
    let n4884: ZB = zb_or(n4882, n4883);
    let n4885: ZB = zb_and(n4692, n4884);
    let n4886: ZB = zb_and(n4693, n4884);
    let n4887: ZN = zsel_n(n4692, zn_splat(P8::from_raw(0i32)), n4877);
    let n4888: ZB = zb_or(n4885, n4886);
    let n4889: ZN = zsel_n(n4852, n4877, n4887);
    let n4890: ZB = zb_or(n4880, n4888);
    let n4891: ZN = zsel_n(n4021, zn_splat(P8::from_raw(131072i32)), r_c359);
    let n4892: ZN = zsel_n(n4021, zn_splat(P8::from_raw(327680i32)), n3865);
    let n4893: ZN = zsel_n(n4021, zn_splat(P8::from_raw(0i32)), n4735);
    let n4894: ZN = zsel_n(n291, r_c359, n4891);
    let n4895: ZN = zsel_n(n291, n4638, n4892);
    let n4896: ZN = zsel_n(n291, n4643, n4893);
    let n4897: ZB = zb_and(n3802, n4485);
    let n4898: ZN = zsel_n(n295, r_c359, n4894);
    let n4899: ZN = zsel_n(n295, r_c369, n4895);
    let n4900: ZN = zsel_n(n295, r_c370, n4896);
    let n4901: ZB = zb_or(n295, n4897);
    let n4902: ZB = zb_and(n4852, n4901);
    let n4903: ZB = zb_and(n4853, n4901);
    let n4904: ZB = zb_and(n4687, n4903);
    let n4905: ZB = zb_and(n4686, n4903);
    let n4906: ZB = zb_or(n4904, n4905);
    let n4907: ZB = zb_and(n4692, n4906);
    let n4908: ZB = zb_and(n4693, n4906);
    let n4909: ZN = zsel_n(n4692, zn_splat(P8::from_raw(0i32)), n4899);
    let n4910: ZB = zb_or(n4907, n4908);
    let n4911: ZN = zsel_n(n4852, n4899, n4909);
    let n4912: ZB = zb_or(n4902, n4910);
    let n4913: ZN = zsel_n(n4021, zn_splat(P8::from_raw(69510i32)), r_c357);
    let n4914: ZN = zsel_n(n4021, zn_splat(P8::from_raw(98304i32)), r_c358);
    let n4915: ZN = zsel_n(n4021, zn_splat(P8::from_raw(0i32)), r_c359);
    let n4916: ZN = zsel_n(n4021, zn_splat(P8::from_raw(-98304i32)), r_c360);
    let n4917: ZN = zsel_n(n4021, zn_splat(P8::from_raw(0i32)), n3722);
    let n4918: ZN = zsel_n(n4021, zn_splat(P8::from_raw(-327680i32)), n4650);
    let n4919: ZN = zsel_n(n291, r_c357, n4913);
    let n4920: ZN = zsel_n(n291, r_c358, n4914);
    let n4921: ZN = zsel_n(n291, r_c359, n4915);
    let n4922: ZN = zsel_n(n291, r_c360, n4916);
    let n4923: ZN = zsel_n(n291, n4638, n4917);
    let n4924: ZN = zsel_n(n291, n4643, n4918);
    let n4925: ZB = zb_and(n3802, n4501);
    let n4926: ZN = zsel_n(n295, r_c357, n4919);
    let n4927: ZN = zsel_n(n295, r_c358, n4920);
    let n4928: ZN = zsel_n(n295, r_c359, n4921);
    let n4929: ZN = zsel_n(n295, r_c360, n4922);
    let n4930: ZN = zsel_n(n295, r_c369, n4923);
    let n4931: ZN = zsel_n(n295, r_c370, n4924);
    let n4932: ZB = zb_or(n295, n4925);
    let n4933: ZB = zb_and(n4852, n4932);
    let n4934: ZB = zb_and(n4853, n4932);
    let n4935: ZB = zb_and(n4687, n4934);
    let n4936: ZB = zb_and(n4686, n4934);
    let n4937: ZB = zb_or(n4935, n4936);
    let n4938: ZB = zb_and(n4692, n4937);
    let n4939: ZB = zb_and(n4693, n4937);
    let n4940: ZN = zsel_n(n4692, zn_splat(P8::from_raw(0i32)), n4930);
    let n4941: ZB = zb_or(n4938, n4939);
    let n4942: ZN = zsel_n(n4852, n4930, n4940);
    let n4943: ZB = zb_or(n4933, n4941);
    let n4944: ZN = zsel_n(n4021, zn_splat(P8::from_raw(-231700i32)), n3814);
    let n4945: ZN = zsel_n(n4021, zn_splat(P8::from_raw(-231700i32)), n4708);
    let n4946: ZN = zsel_n(n291, n4638, n4944);
    let n4947: ZN = zsel_n(n291, n4643, n4945);
    let n4948: ZN = zsel_n(n295, r_c369, n4946);
    let n4949: ZN = zsel_n(n295, r_c370, n4947);
    let n4950: ZN = zsel_n(n4692, zn_splat(P8::from_raw(0i32)), n4948);
    let n4951: ZN = zsel_n(n4852, n4948, n4950);
    let n4952: ZN = zsel_n(n4021, zn_splat(P8::from_raw(231700i32)), n3865);
    let n4953: ZN = zsel_n(n4021, zn_splat(P8::from_raw(-231700i32)), n4735);
    let n4954: ZN = zsel_n(n291, n4638, n4952);
    let n4955: ZN = zsel_n(n291, n4643, n4953);
    let n4956: ZN = zsel_n(n295, r_c369, n4954);
    let n4957: ZN = zsel_n(n295, r_c370, n4955);
    let n4958: ZN = zsel_n(n4692, zn_splat(P8::from_raw(0i32)), n4956);
    let n4959: ZN = zsel_n(n4852, n4956, n4958);
    let n4960: ZN = zsel_n(n4021, zn_splat(P8::from_raw(131072i32)), r_c360);
    let n4961: ZN = zsel_n(n4021, zn_splat(P8::from_raw(327680i32)), n4650);
    let n4962: ZN = zsel_n(n291, r_c360, n4960);
    let n4963: ZN = zsel_n(n291, n4643, n4961);
    let n4964: ZN = zsel_n(n295, r_c360, n4962);
    let n4965: ZN = zsel_n(n295, r_c370, n4963);
    let n4966: ZN = zsel_n(n4021, zn_splat(P8::from_raw(231700i32)), n4708);
    let n4967: ZN = zsel_n(n291, n4643, n4966);
    let n4968: ZN = zsel_n(n295, r_c370, n4967);
    let n4969: ZN = zsel_n(n4021, zn_splat(P8::from_raw(231700i32)), n4735);
    let n4970: ZN = zsel_n(n291, n4643, n4969);
    let n4971: ZN = zsel_n(n295, r_c370, n4970);
    let n4972: ZN = zsel_n(n4021, n3748, n4761);
    let n4973: ZN = zsel_n(n4021, zn_splat(P8::from_raw(0i32)), n4762);
    let n4974: ZN = zsel_n(n291, n4638, n4972);
    let n4975: ZN = zsel_n(n291, n4643, n4973);
    let n4976: ZB = zb_and(n3802, n4531);
    let n4977: ZN = zsel_n(n295, r_c369, n4974);
    let n4978: ZN = zsel_n(n295, r_c370, n4975);
    let n4979: ZB = zb_or(n295, n4976);
    let n4980: ZB = zb_and(n4852, n4979);
    let n4981: ZB = zb_and(n4853, n4979);
    let n4982: ZB = zb_and(n4687, n4981);
    let n4983: ZB = zb_and(n4686, n4981);
    let n4984: ZB = zb_or(n4982, n4983);
    let n4985: ZB = zb_and(n4692, n4984);
    let n4986: ZB = zb_and(n4693, n4984);
    let n4987: ZN = zsel_n(n4692, zn_splat(P8::from_raw(0i32)), n4977);
    let n4988: ZB = zb_or(n4985, n4986);
    let n4989: ZN = zsel_n(n4852, n4977, n4987);
    let n4990: ZB = zb_or(n4980, n4988);
    let n4991: ZN = zsel_n(n4021, zn_splat(P8::from_raw(-327680i32)), n4782);
    let n4992: ZN = zsel_n(n4021, zn_splat(P8::from_raw(0i32)), n4783);
    let n4993: ZN = zsel_n(n291, n4638, n4991);
    let n4994: ZN = zsel_n(n291, n4643, n4992);
    let n4995: ZB = zb_and(n3802, n4549);
    let n4996: ZN = zsel_n(n295, r_c369, n4993);
    let n4997: ZN = zsel_n(n295, r_c370, n4994);
    let n4998: ZB = zb_or(n295, n4995);
    let n4999: ZB = zb_and(n4852, n4998);
    let n5000: ZB = zb_and(n4853, n4998);
    let n5001: ZB = zb_and(n4687, n5000);
    let n5002: ZB = zb_and(n4686, n5000);
    let n5003: ZB = zb_or(n5001, n5002);
    let n5004: ZB = zb_and(n4692, n5003);
    let n5005: ZB = zb_and(n4693, n5003);
    let n5006: ZN = zsel_n(n4692, zn_splat(P8::from_raw(0i32)), n4996);
    let n5007: ZB = zb_or(n5004, n5005);
    let n5008: ZN = zsel_n(n4852, n4996, n5006);
    let n5009: ZB = zb_or(n4999, n5007);
    let n5010: ZN = zsel_n(n4021, zn_splat(P8::from_raw(327680i32)), n4801);
    let n5011: ZN = zsel_n(n4021, zn_splat(P8::from_raw(0i32)), n4802);
    let n5012: ZN = zsel_n(n291, n4638, n5010);
    let n5013: ZN = zsel_n(n291, n4643, n5011);
    let n5014: ZB = zb_and(n3802, n4567);
    let n5015: ZN = zsel_n(n295, r_c369, n5012);
    let n5016: ZN = zsel_n(n295, r_c370, n5013);
    let n5017: ZB = zb_or(n295, n5014);
    let n5018: ZB = zb_and(n4852, n5017);
    let n5019: ZB = zb_and(n4853, n5017);
    let n5020: ZB = zb_and(n4687, n5019);
    let n5021: ZB = zb_and(n4686, n5019);
    let n5022: ZB = zb_or(n5020, n5021);
    let n5023: ZB = zb_and(n4692, n5022);
    let n5024: ZB = zb_and(n4693, n5022);
    let n5025: ZN = zsel_n(n4692, zn_splat(P8::from_raw(0i32)), n5015);
    let n5026: ZB = zb_or(n5023, n5024);
    let n5027: ZN = zsel_n(n4852, n5015, n5025);
    let n5028: ZB = zb_or(n5018, n5026);
    let n5029: ZN = zsel_n(n4021, zn_splat(P8::from_raw(0i32)), n4761);
    let n5030: ZN = zsel_n(n4021, zn_splat(P8::from_raw(-327680i32)), n4762);
    let n5031: ZN = zsel_n(n291, n4638, n5029);
    let n5032: ZN = zsel_n(n291, n4643, n5030);
    let n5033: ZB = zb_and(n3802, n4583);
    let n5034: ZN = zsel_n(n295, r_c369, n5031);
    let n5035: ZN = zsel_n(n295, r_c370, n5032);
    let n5036: ZB = zb_or(n295, n5033);
    let n5037: ZB = zb_and(n4852, n5036);
    let n5038: ZB = zb_and(n4853, n5036);
    let n5039: ZB = zb_and(n4687, n5038);
    let n5040: ZB = zb_and(n4686, n5038);
    let n5041: ZB = zb_or(n5039, n5040);
    let n5042: ZB = zb_and(n4692, n5041);
    let n5043: ZB = zb_and(n4693, n5041);
    let n5044: ZN = zsel_n(n4692, zn_splat(P8::from_raw(0i32)), n5034);
    let n5045: ZB = zb_or(n5042, n5043);
    let n5046: ZN = zsel_n(n4852, n5034, n5044);
    let n5047: ZB = zb_or(n5037, n5045);
    let n5048: ZN = zsel_n(n4021, zn_splat(P8::from_raw(-231700i32)), n4782);
    let n5049: ZN = zsel_n(n4021, zn_splat(P8::from_raw(-231700i32)), n4783);
    let n5050: ZN = zsel_n(n291, n4638, n5048);
    let n5051: ZN = zsel_n(n291, n4643, n5049);
    let n5052: ZN = zsel_n(n295, r_c369, n5050);
    let n5053: ZN = zsel_n(n295, r_c370, n5051);
    let n5054: ZN = zsel_n(n4692, zn_splat(P8::from_raw(0i32)), n5052);
    let n5055: ZN = zsel_n(n4852, n5052, n5054);
    let n5056: ZN = zsel_n(n4021, zn_splat(P8::from_raw(231700i32)), n4801);
    let n5057: ZN = zsel_n(n4021, zn_splat(P8::from_raw(-231700i32)), n4802);
    let n5058: ZN = zsel_n(n291, n4638, n5056);
    let n5059: ZN = zsel_n(n291, n4643, n5057);
    let n5060: ZN = zsel_n(n295, r_c369, n5058);
    let n5061: ZN = zsel_n(n295, r_c370, n5059);
    let n5062: ZN = zsel_n(n4692, zn_splat(P8::from_raw(0i32)), n5060);
    let n5063: ZN = zsel_n(n4852, n5060, n5062);
    let n5064: ZN = zsel_n(n4021, zn_splat(P8::from_raw(327680i32)), n4762);
    let n5065: ZN = zsel_n(n291, n4643, n5064);
    let n5066: ZN = zsel_n(n295, r_c370, n5065);
    let n5067: ZN = zsel_n(n4021, zn_splat(P8::from_raw(231700i32)), n4783);
    let n5068: ZN = zsel_n(n291, n4643, n5067);
    let n5069: ZN = zsel_n(n295, r_c370, n5068);
    let n5070: ZN = zsel_n(n4021, zn_splat(P8::from_raw(231700i32)), n4802);
    let n5071: ZN = zsel_n(n291, n4643, n5070);
    let n5072: ZN = zsel_n(n295, r_c370, n5071);
    let n5075: ZW = zw_bits_n(r_c39);
    let n5076: ZW = zw_mix1(zw_splat(11400714819323198485u64), n5075, 39u64);
    let n5077: ZW = zw_mix2(zw_splat(11562461410679940143u64), n5075, 39u64);
    let n5078: ZW = zw_bits_n(n115);
    let n5079: ZW = zw_mix1(n5076, n5078, 84u64);
    let n5080: ZW = zw_mix2(n5077, n5078, 84u64);
    let n5081: ZW = zw_bits_n(n215);
    let n5082: ZW = zw_mix1(n5079, n5081, 85u64);
    let n5083: ZW = zw_mix2(n5080, n5081, 85u64);
    let n5084: ZW = zw_bits_n(n214);
    let n5085: ZW = zw_mix1(n5082, n5084, 86u64);
    let n5086: ZW = zw_mix2(n5083, n5084, 86u64);
    let n5087: ZW = zw_bits_n(r_c87);
    let n5088: ZW = zw_mix1(n5085, n5087, 87u64);
    let n5089: ZW = zw_mix2(n5086, n5087, 87u64);
    let n5090: ZW = zw_bits_n(n303);
    let n5091: ZW = zw_mix1(n5088, n5090, 241u64);
    let n5092: ZW = zw_mix2(n5089, n5090, 241u64);
    let n5093: ZW = zw_bits_n(n304);
    let n5094: ZW = zw_mix1(n5091, n5093, 254u64);
    let n5095: ZW = zw_mix2(n5092, n5093, 254u64);
    let n5096: ZW = zw_bits_n(n770);
    let n5097: ZW = zw_mix1(n5094, n5096, 302u64);
    let n5098: ZW = zw_mix2(n5095, n5096, 302u64);
    let n5099: ZW = zw_bits_n(n545);
    let n5100: ZW = zw_mix1(n5097, n5099, 368u64);
    let n5101: ZW = zw_mix2(n5098, n5099, 368u64);
    let n5102: ZW = zw_bits_n(n771);
    let n5103: ZW = zw_mix1(n5100, n5102, 369u64);
    let n5104: ZW = zw_mix2(n5101, n5102, 369u64);
    let n5105: ZW = zw_bits_n(r_c20);
    let n5106: ZW = zw_mix1(n5103, n5105, 20u64);
    let n5107: ZW = zw_mix2(n5104, n5105, 20u64);
    let n5108: ZW = zw_bits_b(r_c41);
    let n5109: ZW = zw_mix1(n5106, n5108, 41u64);
    let n5110: ZW = zw_mix2(n5107, n5108, 41u64);
    let n5111: ZW = zw_bits_n(n290);
    let n5112: ZW = zw_mix1(n5109, n5111, 282u64);
    let n5113: ZW = zw_mix2(n5110, n5111, 282u64);
    let n5114: ZW = zw_bits_n(n294);
    let n5115: ZW = zw_mix1(n5112, n5114, 284u64);
    let n5116: ZW = zw_mix2(n5113, n5114, 284u64);
    let n5117: u64 = P8::from_raw(65536i32).as_raw_u32() as u64;
    let n5118: ZW = zw_mix1(n5115, zw_splat(n5117), 285u64);
    let n5119: ZW = zw_mix2(n5116, zw_splat(n5117), 285u64);
    let n5120: ZW = zw_bits_n(n1434);
    let n5121: ZW = zw_mix1(n5118, n5120, 287u64);
    let n5122: ZW = zw_mix2(n5119, n5120, 287u64);
    let n5123: u64 = false as u64;
    let n5124: ZW = zw_mix1(n5121, zw_splat(n5123), 294u64);
    let n5125: ZW = zw_mix2(n5122, zw_splat(n5123), 294u64);
    let n5126: ZW = zw_mix1(n5124, zw_splat(n5123), 295u64);
    let n5127: ZW = zw_mix2(n5125, zw_splat(n5123), 295u64);
    let n5128: ZW = zw_bits_n(n1580);
    let n5129: ZW = zw_mix1(n5126, n5128, 301u64);
    let n5130: ZW = zw_mix2(n5127, n5128, 301u64);
    let n5131: ZW = zw_bits_n(r_c357);
    let n5132: ZW = zw_mix1(n5129, n5131, 358u64);
    let n5133: ZW = zw_mix2(n5130, n5131, 358u64);
    let n5134: ZW = zw_bits_n(r_c358);
    let n5135: ZW = zw_mix1(n5132, n5134, 359u64);
    let n5136: ZW = zw_mix2(n5133, n5134, 359u64);
    let n5137: ZW = zw_bits_n(r_c359);
    let n5138: ZW = zw_mix1(n5135, n5137, 360u64);
    let n5139: ZW = zw_mix2(n5136, n5137, 360u64);
    let n5140: ZW = zw_bits_n(r_c360);
    let n5141: ZW = zw_mix1(n5138, n5140, 361u64);
    let n5142: ZW = zw_mix2(n5139, n5140, 361u64);
    let n5143: ZW = zw_bits_b(n1560);
    let n5144: ZW = zw_mix1(n5141, n5143, 362u64);
    let n5145: ZW = zw_mix2(n5142, n5143, 362u64);
    let n5146: ZW = zw_bits_n(n1581);
    let n5147: ZW = zw_mix1(n5144, n5146, 370u64);
    let n5148: ZW = zw_mix2(n5145, n5146, 370u64);
    let n5149: ZW = zw_bits_n(n1562);
    let n5150: ZW = zw_mix1(n5147, n5149, 371u64);
    let n5151: ZW = zw_mix2(n5148, n5149, 371u64);
    let n5152: ZW = zw_bits_b(n1637);
    let n5153: ZW = zw_mix1(n5141, n5152, 362u64);
    let n5154: ZW = zw_mix2(n5142, n5152, 362u64);
    let n5155: ZW = zw_bits_n(n1649);
    let n5156: ZW = zw_mix1(n5153, n5155, 370u64);
    let n5157: ZW = zw_mix2(n5154, n5155, 370u64);
    let n5158: ZW = zw_bits_n(n1639);
    let n5159: ZW = zw_mix1(n5156, n5158, 371u64);
    let n5160: ZW = zw_mix2(n5157, n5158, 371u64);
    let n5161: ZW = zw_bits_b(n1693);
    let n5162: ZW = zw_mix1(n5141, n5161, 362u64);
    let n5163: ZW = zw_mix2(n5142, n5161, 362u64);
    let n5164: ZW = zw_bits_n(n1705);
    let n5165: ZW = zw_mix1(n5162, n5164, 370u64);
    let n5166: ZW = zw_mix2(n5163, n5164, 370u64);
    let n5167: ZW = zw_bits_n(n1695);
    let n5168: ZW = zw_mix1(n5165, n5167, 371u64);
    let n5169: ZW = zw_mix2(n5166, n5167, 371u64);
    let n5170: ZW = zw_bits_n(n1736);
    let n5171: ZW = zw_mix1(n5118, n5170, 287u64);
    let n5172: ZW = zw_mix2(n5119, n5170, 287u64);
    let n5173: ZW = zw_mix1(n5171, zw_splat(n5123), 294u64);
    let n5174: ZW = zw_mix2(n5172, zw_splat(n5123), 294u64);
    let n5175: u64 = true as u64;
    let n5176: ZW = zw_mix1(n5173, zw_splat(n5175), 295u64);
    let n5177: ZW = zw_mix2(n5174, zw_splat(n5175), 295u64);
    let n5178: ZW = zw_mix1(n5176, n5128, 301u64);
    let n5179: ZW = zw_mix2(n5177, n5128, 301u64);
    let n5180: ZW = zw_mix1(n5178, n5131, 358u64);
    let n5181: ZW = zw_mix2(n5179, n5131, 358u64);
    let n5182: ZW = zw_mix1(n5180, n5134, 359u64);
    let n5183: ZW = zw_mix2(n5181, n5134, 359u64);
    let n5184: ZW = zw_mix1(n5182, n5137, 360u64);
    let n5185: ZW = zw_mix2(n5183, n5137, 360u64);
    let n5186: ZW = zw_mix1(n5184, n5140, 361u64);
    let n5187: ZW = zw_mix2(n5185, n5140, 361u64);
    let n5188: ZW = zw_mix1(n5186, n5143, 362u64);
    let n5189: ZW = zw_mix2(n5187, n5143, 362u64);
    let n5190: ZW = zw_bits_n(n1748);
    let n5191: ZW = zw_mix1(n5188, n5190, 370u64);
    let n5192: ZW = zw_mix2(n5189, n5190, 370u64);
    let n5193: ZW = zw_bits_n(n1738);
    let n5194: ZW = zw_mix1(n5191, n5193, 371u64);
    let n5195: ZW = zw_mix2(n5192, n5193, 371u64);
    let n5196: ZW = zw_mix1(n5186, n5152, 362u64);
    let n5197: ZW = zw_mix2(n5187, n5152, 362u64);
    let n5198: ZW = zw_bits_n(n1789);
    let n5199: ZW = zw_mix1(n5196, n5198, 370u64);
    let n5200: ZW = zw_mix2(n5197, n5198, 370u64);
    let n5201: ZW = zw_bits_n(n1779);
    let n5202: ZW = zw_mix1(n5199, n5201, 371u64);
    let n5203: ZW = zw_mix2(n5200, n5201, 371u64);
    let n5204: ZW = zw_mix1(n5186, n5161, 362u64);
    let n5205: ZW = zw_mix2(n5187, n5161, 362u64);
    let n5206: ZW = zw_bits_n(n1830);
    let n5207: ZW = zw_mix1(n5204, n5206, 370u64);
    let n5208: ZW = zw_mix2(n5205, n5206, 370u64);
    let n5209: ZW = zw_bits_n(n1820);
    let n5210: ZW = zw_mix1(n5207, n5209, 371u64);
    let n5211: ZW = zw_mix2(n5208, n5209, 371u64);
    let n5212: ZW = zw_bits_n(n1838);
    let n5213: ZW = zw_mix1(n5103, n5212, 20u64);
    let n5214: ZW = zw_mix2(n5104, n5212, 20u64);
    let n5215: ZW = zw_bits_b(n1839);
    let n5216: ZW = zw_mix1(n5213, n5215, 41u64);
    let n5217: ZW = zw_mix2(n5214, n5215, 41u64);
    let n5218: ZW = zw_bits_n(n1840);
    let n5219: ZW = zw_mix1(n5216, n5218, 282u64);
    let n5220: ZW = zw_mix2(n5217, n5218, 282u64);
    let n5221: ZW = zw_bits_n(n1841);
    let n5222: ZW = zw_mix1(n5219, n5221, 284u64);
    let n5223: ZW = zw_mix2(n5220, n5221, 284u64);
    let n5224: ZW = zw_bits_n(n1842);
    let n5225: ZW = zw_mix1(n5222, n5224, 285u64);
    let n5226: ZW = zw_mix2(n5223, n5224, 285u64);
    let n5227: ZW = zw_mix1(n5225, n5120, 287u64);
    let n5228: ZW = zw_mix2(n5226, n5120, 287u64);
    let n5229: ZW = zw_mix1(n5227, zw_splat(n5175), 294u64);
    let n5230: ZW = zw_mix2(n5228, zw_splat(n5175), 294u64);
    let n5231: ZW = zw_mix1(n5229, zw_splat(n5123), 295u64);
    let n5232: ZW = zw_mix2(n5230, zw_splat(n5123), 295u64);
    let n5233: ZW = zw_bits_n(n1880);
    let n5234: ZW = zw_mix1(n5231, n5233, 301u64);
    let n5235: ZW = zw_mix2(n5232, n5233, 301u64);
    let n5236: ZW = zw_bits_n(n1843);
    let n5237: ZW = zw_mix1(n5234, n5236, 358u64);
    let n5238: ZW = zw_mix2(n5235, n5236, 358u64);
    let n5239: ZW = zw_bits_n(n1866);
    let n5240: ZW = zw_mix1(n5237, n5239, 359u64);
    let n5241: ZW = zw_mix2(n5238, n5239, 359u64);
    let n5242: ZW = zw_bits_n(n1867);
    let n5243: ZW = zw_mix1(n5240, n5242, 360u64);
    let n5244: ZW = zw_mix2(n5241, n5242, 360u64);
    let n5245: ZW = zw_bits_n(n1844);
    let n5246: ZW = zw_mix1(n5243, n5245, 361u64);
    let n5247: ZW = zw_mix2(n5244, n5245, 361u64);
    let n5248: ZW = zw_mix1(n5246, n5143, 362u64);
    let n5249: ZW = zw_mix2(n5247, n5143, 362u64);
    let n5250: ZW = zw_bits_n(n1881);
    let n5251: ZW = zw_mix1(n5248, n5250, 370u64);
    let n5252: ZW = zw_mix2(n5249, n5250, 370u64);
    let n5253: ZW = zw_bits_n(n1869);
    let n5254: ZW = zw_mix1(n5251, n5253, 371u64);
    let n5255: ZW = zw_mix2(n5252, n5253, 371u64);
    let n5256: ZW = zw_bits_n(n1890);
    let n5257: ZW = zw_mix1(n5237, n5256, 359u64);
    let n5258: ZW = zw_mix2(n5238, n5256, 359u64);
    let n5259: ZW = zw_bits_n(n1891);
    let n5260: ZW = zw_mix1(n5257, n5259, 360u64);
    let n5261: ZW = zw_mix2(n5258, n5259, 360u64);
    let n5262: ZW = zw_mix1(n5260, n5245, 361u64);
    let n5263: ZW = zw_mix2(n5261, n5245, 361u64);
    let n5264: ZW = zw_mix1(n5262, n5152, 362u64);
    let n5265: ZW = zw_mix2(n5263, n5152, 362u64);
    let n5266: ZW = zw_bits_n(n1904);
    let n5267: ZW = zw_mix1(n5264, n5266, 370u64);
    let n5268: ZW = zw_mix2(n5265, n5266, 370u64);
    let n5269: ZW = zw_bits_n(n1893);
    let n5270: ZW = zw_mix1(n5267, n5269, 371u64);
    let n5271: ZW = zw_mix2(n5268, n5269, 371u64);
    let n5272: ZW = zw_bits_n(n1912);
    let n5273: ZW = zw_mix1(n5257, n5272, 360u64);
    let n5274: ZW = zw_mix2(n5258, n5272, 360u64);
    let n5275: ZW = zw_mix1(n5273, n5245, 361u64);
    let n5276: ZW = zw_mix2(n5274, n5245, 361u64);
    let n5277: ZW = zw_mix1(n5275, n5161, 362u64);
    let n5278: ZW = zw_mix2(n5276, n5161, 362u64);
    let n5279: ZW = zw_bits_n(n1925);
    let n5280: ZW = zw_mix1(n5277, n5279, 370u64);
    let n5281: ZW = zw_mix2(n5278, n5279, 370u64);
    let n5282: ZW = zw_bits_n(n1914);
    let n5283: ZW = zw_mix1(n5280, n5282, 371u64);
    let n5284: ZW = zw_mix2(n5281, n5282, 371u64);
    let n5285: ZW = zw_bits_n(n1930);
    let n5286: ZW = zw_mix1(n5234, n5285, 358u64);
    let n5287: ZW = zw_mix2(n5235, n5285, 358u64);
    let n5288: ZW = zw_bits_n(n1937);
    let n5289: ZW = zw_mix1(n5286, n5288, 359u64);
    let n5290: ZW = zw_mix2(n5287, n5288, 359u64);
    let n5291: ZW = zw_bits_n(n1938);
    let n5292: ZW = zw_mix1(n5289, n5291, 360u64);
    let n5293: ZW = zw_mix2(n5290, n5291, 360u64);
    let n5294: ZW = zw_bits_n(n1931);
    let n5295: ZW = zw_mix1(n5292, n5294, 361u64);
    let n5296: ZW = zw_mix2(n5293, n5294, 361u64);
    let n5297: ZW = zw_mix1(n5295, n5143, 362u64);
    let n5298: ZW = zw_mix2(n5296, n5143, 362u64);
    let n5299: ZW = zw_bits_n(n1951);
    let n5300: ZW = zw_mix1(n5297, n5299, 370u64);
    let n5301: ZW = zw_mix2(n5298, n5299, 370u64);
    let n5302: ZW = zw_bits_n(n1940);
    let n5303: ZW = zw_mix1(n5300, n5302, 371u64);
    let n5304: ZW = zw_mix2(n5301, n5302, 371u64);
    let n5305: ZW = zw_mix1(n5286, n5256, 359u64);
    let n5306: ZW = zw_mix2(n5287, n5256, 359u64);
    let n5307: ZW = zw_mix1(n5305, n5259, 360u64);
    let n5308: ZW = zw_mix2(n5306, n5259, 360u64);
    let n5309: ZW = zw_mix1(n5307, n5294, 361u64);
    let n5310: ZW = zw_mix2(n5308, n5294, 361u64);
    let n5311: ZW = zw_mix1(n5309, n5152, 362u64);
    let n5312: ZW = zw_mix2(n5310, n5152, 362u64);
    let n5313: ZW = zw_bits_n(n1958);
    let n5314: ZW = zw_mix1(n5311, n5313, 370u64);
    let n5315: ZW = zw_mix2(n5312, n5313, 370u64);
    let n5316: ZW = zw_bits_n(n1956);
    let n5317: ZW = zw_mix1(n5314, n5316, 371u64);
    let n5318: ZW = zw_mix2(n5315, n5316, 371u64);
    let n5319: ZW = zw_mix1(n5305, n5272, 360u64);
    let n5320: ZW = zw_mix2(n5306, n5272, 360u64);
    let n5321: ZW = zw_mix1(n5319, n5294, 361u64);
    let n5322: ZW = zw_mix2(n5320, n5294, 361u64);
    let n5323: ZW = zw_mix1(n5321, n5161, 362u64);
    let n5324: ZW = zw_mix2(n5322, n5161, 362u64);
    let n5325: ZW = zw_bits_n(n1964);
    let n5326: ZW = zw_mix1(n5323, n5325, 370u64);
    let n5327: ZW = zw_mix2(n5324, n5325, 370u64);
    let n5328: ZW = zw_bits_n(n1962);
    let n5329: ZW = zw_mix1(n5326, n5328, 371u64);
    let n5330: ZW = zw_mix2(n5327, n5328, 371u64);
    let n5331: ZW = zw_bits_n(n1966);
    let n5332: ZW = zw_mix1(n5292, n5331, 361u64);
    let n5333: ZW = zw_mix2(n5293, n5331, 361u64);
    let n5334: ZW = zw_mix1(n5332, n5143, 362u64);
    let n5335: ZW = zw_mix2(n5333, n5143, 362u64);
    let n5336: ZW = zw_mix1(n5334, n5299, 370u64);
    let n5337: ZW = zw_mix2(n5335, n5299, 370u64);
    let n5338: ZW = zw_bits_n(n1968);
    let n5339: ZW = zw_mix1(n5336, n5338, 371u64);
    let n5340: ZW = zw_mix2(n5337, n5338, 371u64);
    let n5341: ZW = zw_mix1(n5307, n5331, 361u64);
    let n5342: ZW = zw_mix2(n5308, n5331, 361u64);
    let n5343: ZW = zw_mix1(n5341, n5152, 362u64);
    let n5344: ZW = zw_mix2(n5342, n5152, 362u64);
    let n5345: ZW = zw_mix1(n5343, n5313, 370u64);
    let n5346: ZW = zw_mix2(n5344, n5313, 370u64);
    let n5347: ZW = zw_bits_n(n1970);
    let n5348: ZW = zw_mix1(n5345, n5347, 371u64);
    let n5349: ZW = zw_mix2(n5346, n5347, 371u64);
    let n5350: ZW = zw_mix1(n5319, n5331, 361u64);
    let n5351: ZW = zw_mix2(n5320, n5331, 361u64);
    let n5352: ZW = zw_mix1(n5350, n5161, 362u64);
    let n5353: ZW = zw_mix2(n5351, n5161, 362u64);
    let n5354: ZW = zw_mix1(n5352, n5325, 370u64);
    let n5355: ZW = zw_mix2(n5353, n5325, 370u64);
    let n5356: ZW = zw_bits_n(n1972);
    let n5357: ZW = zw_mix1(n5354, n5356, 371u64);
    let n5358: ZW = zw_mix2(n5355, n5356, 371u64);
    let n5359: ZW = zw_mix1(n5225, n5170, 287u64);
    let n5360: ZW = zw_mix2(n5226, n5170, 287u64);
    let n5361: ZW = zw_mix1(n5359, zw_splat(n5175), 294u64);
    let n5362: ZW = zw_mix2(n5360, zw_splat(n5175), 294u64);
    let n5363: ZW = zw_mix1(n5361, zw_splat(n5175), 295u64);
    let n5364: ZW = zw_mix2(n5362, zw_splat(n5175), 295u64);
    let n5365: ZW = zw_mix1(n5363, n5233, 301u64);
    let n5366: ZW = zw_mix2(n5364, n5233, 301u64);
    let n5367: ZW = zw_mix1(n5365, n5236, 358u64);
    let n5368: ZW = zw_mix2(n5366, n5236, 358u64);
    let n5369: ZW = zw_mix1(n5367, n5239, 359u64);
    let n5370: ZW = zw_mix2(n5368, n5239, 359u64);
    let n5371: ZW = zw_mix1(n5369, n5242, 360u64);
    let n5372: ZW = zw_mix2(n5370, n5242, 360u64);
    let n5373: ZW = zw_mix1(n5371, n5245, 361u64);
    let n5374: ZW = zw_mix2(n5372, n5245, 361u64);
    let n5375: ZW = zw_mix1(n5373, n5143, 362u64);
    let n5376: ZW = zw_mix2(n5374, n5143, 362u64);
    let n5377: ZW = zw_bits_n(n2002);
    let n5378: ZW = zw_mix1(n5375, n5377, 370u64);
    let n5379: ZW = zw_mix2(n5376, n5377, 370u64);
    let n5380: ZW = zw_bits_n(n1991);
    let n5381: ZW = zw_mix1(n5378, n5380, 371u64);
    let n5382: ZW = zw_mix2(n5379, n5380, 371u64);
    let n5383: ZW = zw_mix1(n5367, n5256, 359u64);
    let n5384: ZW = zw_mix2(n5368, n5256, 359u64);
    let n5385: ZW = zw_mix1(n5383, n5259, 360u64);
    let n5386: ZW = zw_mix2(n5384, n5259, 360u64);
    let n5387: ZW = zw_mix1(n5385, n5245, 361u64);
    let n5388: ZW = zw_mix2(n5386, n5245, 361u64);
    let n5389: ZW = zw_mix1(n5387, n5152, 362u64);
    let n5390: ZW = zw_mix2(n5388, n5152, 362u64);
    let n5391: ZW = zw_bits_n(n2021);
    let n5392: ZW = zw_mix1(n5389, n5391, 370u64);
    let n5393: ZW = zw_mix2(n5390, n5391, 370u64);
    let n5394: ZW = zw_bits_n(n2010);
    let n5395: ZW = zw_mix1(n5392, n5394, 371u64);
    let n5396: ZW = zw_mix2(n5393, n5394, 371u64);
    let n5397: ZW = zw_mix1(n5383, n5272, 360u64);
    let n5398: ZW = zw_mix2(n5384, n5272, 360u64);
    let n5399: ZW = zw_mix1(n5397, n5245, 361u64);
    let n5400: ZW = zw_mix2(n5398, n5245, 361u64);
    let n5401: ZW = zw_mix1(n5399, n5161, 362u64);
    let n5402: ZW = zw_mix2(n5400, n5161, 362u64);
    let n5403: ZW = zw_bits_n(n2040);
    let n5404: ZW = zw_mix1(n5401, n5403, 370u64);
    let n5405: ZW = zw_mix2(n5402, n5403, 370u64);
    let n5406: ZW = zw_bits_n(n2029);
    let n5407: ZW = zw_mix1(n5404, n5406, 371u64);
    let n5408: ZW = zw_mix2(n5405, n5406, 371u64);
    let n5409: ZW = zw_mix1(n5365, n5285, 358u64);
    let n5410: ZW = zw_mix2(n5366, n5285, 358u64);
    let n5411: ZW = zw_mix1(n5409, n5288, 359u64);
    let n5412: ZW = zw_mix2(n5410, n5288, 359u64);
    let n5413: ZW = zw_mix1(n5411, n5291, 360u64);
    let n5414: ZW = zw_mix2(n5412, n5291, 360u64);
    let n5415: ZW = zw_mix1(n5413, n5294, 361u64);
    let n5416: ZW = zw_mix2(n5414, n5294, 361u64);
    let n5417: ZW = zw_mix1(n5415, n5143, 362u64);
    let n5418: ZW = zw_mix2(n5416, n5143, 362u64);
    let n5419: ZW = zw_bits_n(n2057);
    let n5420: ZW = zw_mix1(n5417, n5419, 370u64);
    let n5421: ZW = zw_mix2(n5418, n5419, 370u64);
    let n5422: ZW = zw_bits_n(n2046);
    let n5423: ZW = zw_mix1(n5420, n5422, 371u64);
    let n5424: ZW = zw_mix2(n5421, n5422, 371u64);
    let n5425: ZW = zw_mix1(n5409, n5256, 359u64);
    let n5426: ZW = zw_mix2(n5410, n5256, 359u64);
    let n5427: ZW = zw_mix1(n5425, n5259, 360u64);
    let n5428: ZW = zw_mix2(n5426, n5259, 360u64);
    let n5429: ZW = zw_mix1(n5427, n5294, 361u64);
    let n5430: ZW = zw_mix2(n5428, n5294, 361u64);
    let n5431: ZW = zw_mix1(n5429, n5152, 362u64);
    let n5432: ZW = zw_mix2(n5430, n5152, 362u64);
    let n5433: ZW = zw_bits_n(n2064);
    let n5434: ZW = zw_mix1(n5431, n5433, 370u64);
    let n5435: ZW = zw_mix2(n5432, n5433, 370u64);
    let n5436: ZW = zw_bits_n(n2062);
    let n5437: ZW = zw_mix1(n5434, n5436, 371u64);
    let n5438: ZW = zw_mix2(n5435, n5436, 371u64);
    let n5439: ZW = zw_mix1(n5425, n5272, 360u64);
    let n5440: ZW = zw_mix2(n5426, n5272, 360u64);
    let n5441: ZW = zw_mix1(n5439, n5294, 361u64);
    let n5442: ZW = zw_mix2(n5440, n5294, 361u64);
    let n5443: ZW = zw_mix1(n5441, n5161, 362u64);
    let n5444: ZW = zw_mix2(n5442, n5161, 362u64);
    let n5445: ZW = zw_bits_n(n2070);
    let n5446: ZW = zw_mix1(n5443, n5445, 370u64);
    let n5447: ZW = zw_mix2(n5444, n5445, 370u64);
    let n5448: ZW = zw_bits_n(n2068);
    let n5449: ZW = zw_mix1(n5446, n5448, 371u64);
    let n5450: ZW = zw_mix2(n5447, n5448, 371u64);
    let n5451: ZW = zw_mix1(n5413, n5331, 361u64);
    let n5452: ZW = zw_mix2(n5414, n5331, 361u64);
    let n5453: ZW = zw_mix1(n5451, n5143, 362u64);
    let n5454: ZW = zw_mix2(n5452, n5143, 362u64);
    let n5455: ZW = zw_mix1(n5453, n5419, 370u64);
    let n5456: ZW = zw_mix2(n5454, n5419, 370u64);
    let n5457: ZW = zw_bits_n(n2072);
    let n5458: ZW = zw_mix1(n5455, n5457, 371u64);
    let n5459: ZW = zw_mix2(n5456, n5457, 371u64);
    let n5460: ZW = zw_mix1(n5427, n5331, 361u64);
    let n5461: ZW = zw_mix2(n5428, n5331, 361u64);
    let n5462: ZW = zw_mix1(n5460, n5152, 362u64);
    let n5463: ZW = zw_mix2(n5461, n5152, 362u64);
    let n5464: ZW = zw_mix1(n5462, n5433, 370u64);
    let n5465: ZW = zw_mix2(n5463, n5433, 370u64);
    let n5466: ZW = zw_bits_n(n2074);
    let n5467: ZW = zw_mix1(n5464, n5466, 371u64);
    let n5468: ZW = zw_mix2(n5465, n5466, 371u64);
    let n5469: ZW = zw_mix1(n5439, n5331, 361u64);
    let n5470: ZW = zw_mix2(n5440, n5331, 361u64);
    let n5471: ZW = zw_mix1(n5469, n5161, 362u64);
    let n5472: ZW = zw_mix2(n5470, n5161, 362u64);
    let n5473: ZW = zw_mix1(n5471, n5445, 370u64);
    let n5474: ZW = zw_mix2(n5472, n5445, 370u64);
    let n5475: ZW = zw_bits_n(n2076);
    let n5476: ZW = zw_mix1(n5473, n5475, 371u64);
    let n5477: ZW = zw_mix2(n5474, n5475, 371u64);
    let n5478: ZW = zw_mix1(zw_splat(11400714819323198485u64), n5078, 84u64);
    let n5479: ZW = zw_mix2(zw_splat(11562461410679940143u64), n5078, 84u64);
    let n5480: ZW = zw_mix1(n5478, n5081, 85u64);
    let n5481: ZW = zw_mix2(n5479, n5081, 85u64);
    let n5482: ZW = zw_mix1(n5480, n5084, 86u64);
    let n5483: ZW = zw_mix2(n5481, n5084, 86u64);
    let n5484: ZW = zw_bits_n(n2077);
    let n5485: ZW = zw_mix1(n5482, n5484, 87u64);
    let n5486: ZW = zw_mix2(n5483, n5484, 87u64);
    let n5487: ZW = zw_mix1(n5485, n5090, 240u64);
    let n5488: ZW = zw_mix2(n5486, n5090, 240u64);
    let n5489: ZW = zw_mix1(n5487, n5093, 253u64);
    let n5490: ZW = zw_mix2(n5488, n5093, 253u64);
    let n5491: ZW = zw_mix1(n5489, n5105, 20u64);
    let n5492: ZW = zw_mix2(n5490, n5105, 20u64);
    let n5493: ZW = zw_mix1(n5491, n5108, 41u64);
    let n5494: ZW = zw_mix2(n5492, n5108, 41u64);
    let n5495: ZW = zw_mix1(n5489, n5212, 20u64);
    let n5496: ZW = zw_mix2(n5490, n5212, 20u64);
    let n5497: ZW = zw_mix1(n5495, n5215, 41u64);
    let n5498: ZW = zw_mix2(n5496, n5215, 41u64);
    let n5499: ZW = zw_bits_n(n3809);
    let n5500: ZW = zw_mix1(n5482, n5499, 87u64);
    let n5501: ZW = zw_mix2(n5483, n5499, 87u64);
    let n5502: ZW = zw_mix1(n5500, n5090, 240u64);
    let n5503: ZW = zw_mix2(n5501, n5090, 240u64);
    let n5504: ZW = zw_mix1(n5502, n5093, 253u64);
    let n5505: ZW = zw_mix2(n5503, n5093, 253u64);
    let n5506: ZW = zw_mix1(n5504, n5105, 20u64);
    let n5507: ZW = zw_mix2(n5505, n5105, 20u64);
    let n5508: ZW = zw_mix1(n5506, n5108, 41u64);
    let n5509: ZW = zw_mix2(n5507, n5108, 41u64);
    let n5510: ZW = zw_bits_n(n4025);
    let n5511: ZW = zw_mix1(n5504, n5510, 20u64);
    let n5512: ZW = zw_mix2(n5505, n5510, 20u64);
    let n5513: ZW = zw_bits_b(n4026);
    let n5514: ZW = zw_mix1(n5511, n5513, 41u64);
    let n5515: ZW = zw_mix2(n5512, n5513, 41u64);
    let n5516: ZW = zw_mix1(n5482, n5105, 20u64);
    let n5517: ZW = zw_mix2(n5483, n5105, 20u64);
    let n5518: ZW = zw_bits_b(n4207);
    let n5519: ZW = zw_mix1(n5516, n5518, 38u64);
    let n5520: ZW = zw_mix2(n5517, n5518, 38u64);
    let n5521: ZW = zw_bits_n(n4212);
    let n5522: ZW = zw_mix1(n5519, n5521, 39u64);
    let n5523: ZW = zw_mix2(n5520, n5521, 39u64);
    let n5524: ZW = zw_bits_n(n4211);
    let n5525: ZW = zw_mix1(n5522, n5524, 87u64);
    let n5526: ZW = zw_mix2(n5523, n5524, 87u64);
    let n5527: ZW = zw_bits_b(n4251);
    let n5528: ZW = zw_mix1(n5516, n5527, 38u64);
    let n5529: ZW = zw_mix2(n5517, n5527, 38u64);
    let n5530: ZW = zw_bits_n(n4256);
    let n5531: ZW = zw_mix1(n5528, n5530, 39u64);
    let n5532: ZW = zw_mix2(n5529, n5530, 39u64);
    let n5533: ZW = zw_bits_n(n4255);
    let n5534: ZW = zw_mix1(n5531, n5533, 87u64);
    let n5535: ZW = zw_mix2(n5532, n5533, 87u64);
    let n5536: ZW = zw_bits_b(n4295);
    let n5537: ZW = zw_mix1(n5516, n5536, 38u64);
    let n5538: ZW = zw_mix2(n5517, n5536, 38u64);
    let n5539: ZW = zw_bits_n(n4300);
    let n5540: ZW = zw_mix1(n5537, n5539, 39u64);
    let n5541: ZW = zw_mix2(n5538, n5539, 39u64);
    let n5542: ZW = zw_bits_n(n4299);
    let n5543: ZW = zw_mix1(n5540, n5542, 87u64);
    let n5544: ZW = zw_mix2(n5541, n5542, 87u64);
    let n5545: ZW = zw_bits_b(n4338);
    let n5546: ZW = zw_mix1(n5516, n5545, 38u64);
    let n5547: ZW = zw_mix2(n5517, n5545, 38u64);
    let n5548: ZW = zw_bits_n(n4343);
    let n5549: ZW = zw_mix1(n5546, n5548, 39u64);
    let n5550: ZW = zw_mix2(n5547, n5548, 39u64);
    let n5551: ZW = zw_bits_n(n4342);
    let n5552: ZW = zw_mix1(n5549, n5551, 87u64);
    let n5553: ZW = zw_mix2(n5550, n5551, 87u64);
    let n5554: ZW = zw_bits_b(n4381);
    let n5555: ZW = zw_mix1(n5516, n5554, 38u64);
    let n5556: ZW = zw_mix2(n5517, n5554, 38u64);
    let n5557: ZW = zw_bits_n(n4386);
    let n5558: ZW = zw_mix1(n5555, n5557, 39u64);
    let n5559: ZW = zw_mix2(n5556, n5557, 39u64);
    let n5560: ZW = zw_bits_n(n4385);
    let n5561: ZW = zw_mix1(n5558, n5560, 87u64);
    let n5562: ZW = zw_mix2(n5559, n5560, 87u64);
    let n5563: ZW = zw_bits_b(n4424);
    let n5564: ZW = zw_mix1(n5516, n5563, 38u64);
    let n5565: ZW = zw_mix2(n5517, n5563, 38u64);
    let n5566: ZW = zw_bits_n(n4429);
    let n5567: ZW = zw_mix1(n5564, n5566, 39u64);
    let n5568: ZW = zw_mix2(n5565, n5566, 39u64);
    let n5569: ZW = zw_bits_n(n4428);
    let n5570: ZW = zw_mix1(n5567, n5569, 87u64);
    let n5571: ZW = zw_mix2(n5568, n5569, 87u64);
    let n5572: ZW = zw_mix1(n5482, n5510, 20u64);
    let n5573: ZW = zw_mix2(n5483, n5510, 20u64);
    let n5574: ZW = zw_bits_b(n4452);
    let n5575: ZW = zw_mix1(n5572, n5574, 38u64);
    let n5576: ZW = zw_mix2(n5573, n5574, 38u64);
    let n5577: ZW = zw_bits_n(n4459);
    let n5578: ZW = zw_mix1(n5575, n5577, 39u64);
    let n5579: ZW = zw_mix2(n5576, n5577, 39u64);
    let n5580: ZW = zw_bits_n(n4458);
    let n5581: ZW = zw_mix1(n5578, n5580, 87u64);
    let n5582: ZW = zw_mix2(n5579, n5580, 87u64);
    let n5583: ZW = zw_bits_b(n4470);
    let n5584: ZW = zw_mix1(n5572, n5583, 38u64);
    let n5585: ZW = zw_mix2(n5573, n5583, 38u64);
    let n5586: ZW = zw_bits_n(n4477);
    let n5587: ZW = zw_mix1(n5584, n5586, 39u64);
    let n5588: ZW = zw_mix2(n5585, n5586, 39u64);
    let n5589: ZW = zw_bits_n(n4476);
    let n5590: ZW = zw_mix1(n5587, n5589, 87u64);
    let n5591: ZW = zw_mix2(n5588, n5589, 87u64);
    let n5592: ZW = zw_bits_b(n4488);
    let n5593: ZW = zw_mix1(n5572, n5592, 38u64);
    let n5594: ZW = zw_mix2(n5573, n5592, 38u64);
    let n5595: ZW = zw_bits_n(n4495);
    let n5596: ZW = zw_mix1(n5593, n5595, 39u64);
    let n5597: ZW = zw_mix2(n5594, n5595, 39u64);
    let n5598: ZW = zw_bits_n(n4494);
    let n5599: ZW = zw_mix1(n5596, n5598, 87u64);
    let n5600: ZW = zw_mix2(n5597, n5598, 87u64);
    let n5601: ZW = zw_bits_b(n4504);
    let n5602: ZW = zw_mix1(n5572, n5601, 38u64);
    let n5603: ZW = zw_mix2(n5573, n5601, 38u64);
    let n5604: ZW = zw_bits_n(n4511);
    let n5605: ZW = zw_mix1(n5602, n5604, 39u64);
    let n5606: ZW = zw_mix2(n5603, n5604, 39u64);
    let n5607: ZW = zw_bits_n(n4510);
    let n5608: ZW = zw_mix1(n5605, n5607, 87u64);
    let n5609: ZW = zw_mix2(n5606, n5607, 87u64);
    let n5610: ZW = zw_bits_b(n4534);
    let n5611: ZW = zw_mix1(n5572, n5610, 38u64);
    let n5612: ZW = zw_mix2(n5573, n5610, 38u64);
    let n5613: ZW = zw_bits_n(n4541);
    let n5614: ZW = zw_mix1(n5611, n5613, 39u64);
    let n5615: ZW = zw_mix2(n5612, n5613, 39u64);
    let n5616: ZW = zw_bits_n(n4540);
    let n5617: ZW = zw_mix1(n5614, n5616, 87u64);
    let n5618: ZW = zw_mix2(n5615, n5616, 87u64);
    let n5619: ZW = zw_bits_b(n4552);
    let n5620: ZW = zw_mix1(n5572, n5619, 38u64);
    let n5621: ZW = zw_mix2(n5573, n5619, 38u64);
    let n5622: ZW = zw_bits_n(n4559);
    let n5623: ZW = zw_mix1(n5620, n5622, 39u64);
    let n5624: ZW = zw_mix2(n5621, n5622, 39u64);
    let n5625: ZW = zw_bits_n(n4558);
    let n5626: ZW = zw_mix1(n5623, n5625, 87u64);
    let n5627: ZW = zw_mix2(n5624, n5625, 87u64);
    let n5628: ZW = zw_bits_b(n4570);
    let n5629: ZW = zw_mix1(n5572, n5628, 38u64);
    let n5630: ZW = zw_mix2(n5573, n5628, 38u64);
    let n5631: ZW = zw_bits_n(n4577);
    let n5632: ZW = zw_mix1(n5629, n5631, 39u64);
    let n5633: ZW = zw_mix2(n5630, n5631, 39u64);
    let n5634: ZW = zw_bits_n(n4576);
    let n5635: ZW = zw_mix1(n5632, n5634, 87u64);
    let n5636: ZW = zw_mix2(n5633, n5634, 87u64);
    let n5637: ZW = zw_bits_b(n4586);
    let n5638: ZW = zw_mix1(n5572, n5637, 38u64);
    let n5639: ZW = zw_mix2(n5573, n5637, 38u64);
    let n5640: ZW = zw_bits_n(n4593);
    let n5641: ZW = zw_mix1(n5638, n5640, 39u64);
    let n5642: ZW = zw_mix2(n5639, n5640, 39u64);
    let n5643: ZW = zw_bits_n(n4592);
    let n5644: ZW = zw_mix1(n5641, n5643, 87u64);
    let n5645: ZW = zw_mix2(n5642, n5643, 87u64);
    let n5646: ZW = zw_bits_n(n4667);
    let n5647: ZW = zw_mix1(n5088, n5646, 241u64);
    let n5648: ZW = zw_mix2(n5089, n5646, 241u64);
    let n5649: ZW = zw_bits_n(n4668);
    let n5650: ZW = zw_mix1(n5647, n5649, 254u64);
    let n5651: ZW = zw_mix2(n5648, n5649, 254u64);
    let n5652: ZW = zw_bits_n(n4674);
    let n5653: ZW = zw_mix1(n5650, n5652, 301u64);
    let n5654: ZW = zw_mix2(n5651, n5652, 301u64);
    let n5655: ZW = zw_bits_n(n4676);
    let n5656: ZW = zw_mix1(n5653, n5655, 367u64);
    let n5657: ZW = zw_mix2(n5654, n5655, 367u64);
    let n5658: ZW = zw_bits_n(n4677);
    let n5659: ZW = zw_mix1(n5656, n5658, 368u64);
    let n5660: ZW = zw_mix2(n5657, n5658, 368u64);
    let n5661: ZW = zw_bits_n(n4666);
    let n5662: ZW = zw_mix1(n5659, n5661, 20u64);
    let n5663: ZW = zw_mix2(n5660, n5661, 20u64);
    let n5664: ZW = zw_mix1(n5662, n5108, 41u64);
    let n5665: ZW = zw_mix2(n5663, n5108, 41u64);
    let n5666: ZW = zw_bits_n(n4669);
    let n5667: ZW = zw_mix1(n5664, n5666, 281u64);
    let n5668: ZW = zw_mix2(n5665, n5666, 281u64);
    let n5669: ZW = zw_bits_n(n4670);
    let n5670: ZW = zw_mix1(n5667, n5669, 283u64);
    let n5671: ZW = zw_mix2(n5668, n5669, 283u64);
    let n5672: ZW = zw_bits_n(n4671);
    let n5673: ZW = zw_mix1(n5670, n5672, 284u64);
    let n5674: ZW = zw_mix2(n5671, n5672, 284u64);
    let n5675: ZW = zw_bits_n(n4672);
    let n5676: ZW = zw_mix1(n5673, n5675, 286u64);
    let n5677: ZW = zw_mix2(n5674, n5675, 286u64);
    let n5678: ZW = zw_bits_b(n4596);
    let n5679: ZW = zw_mix1(n5676, n5678, 293u64);
    let n5680: ZW = zw_mix2(n5677, n5678, 293u64);
    let n5681: ZW = zw_bits_b(n4597);
    let n5682: ZW = zw_mix1(n5679, n5681, 294u64);
    let n5683: ZW = zw_mix2(n5680, n5681, 294u64);
    let n5684: ZW = zw_bits_n(n4701);
    let n5685: ZW = zw_mix1(n5682, n5684, 300u64);
    let n5686: ZW = zw_mix2(n5683, n5684, 300u64);
    let n5687: ZW = zw_mix1(n5685, n5131, 357u64);
    let n5688: ZW = zw_mix2(n5686, n5131, 357u64);
    let n5689: ZW = zw_mix1(n5687, n5134, 358u64);
    let n5690: ZW = zw_mix2(n5688, n5134, 358u64);
    let n5691: ZW = zw_mix1(n5689, n5137, 359u64);
    let n5692: ZW = zw_mix2(n5690, n5137, 359u64);
    let n5693: ZW = zw_mix1(n5691, n5140, 360u64);
    let n5694: ZW = zw_mix2(n5692, n5140, 360u64);
    let n5695: ZW = zw_bits_b(n4675);
    let n5696: ZW = zw_mix1(n5693, n5695, 361u64);
    let n5697: ZW = zw_mix2(n5694, n5695, 361u64);
    let n5698: ZW = zw_bits_n(n4702);
    let n5699: ZW = zw_mix1(n5696, n5698, 369u64);
    let n5700: ZW = zw_mix2(n5697, n5698, 369u64);
    let n5701: ZW = zw_bits_n(n4679);
    let n5702: ZW = zw_mix1(n5699, n5701, 370u64);
    let n5703: ZW = zw_mix2(n5700, n5701, 370u64);
    let n5704: ZW = zw_bits_b(n4717);
    let n5705: ZW = zw_mix1(n5693, n5704, 361u64);
    let n5706: ZW = zw_mix2(n5694, n5704, 361u64);
    let n5707: ZW = zw_bits_n(n4730);
    let n5708: ZW = zw_mix1(n5705, n5707, 369u64);
    let n5709: ZW = zw_mix2(n5706, n5707, 369u64);
    let n5710: ZW = zw_bits_n(n4719);
    let n5711: ZW = zw_mix1(n5708, n5710, 370u64);
    let n5712: ZW = zw_mix2(n5709, n5710, 370u64);
    let n5713: ZW = zw_bits_b(n4744);
    let n5714: ZW = zw_mix1(n5693, n5713, 361u64);
    let n5715: ZW = zw_mix2(n5694, n5713, 361u64);
    let n5716: ZW = zw_bits_n(n4757);
    let n5717: ZW = zw_mix1(n5714, n5716, 369u64);
    let n5718: ZW = zw_mix2(n5715, n5716, 369u64);
    let n5719: ZW = zw_bits_n(n4746);
    let n5720: ZW = zw_mix1(n5717, n5719, 370u64);
    let n5721: ZW = zw_mix2(n5718, n5719, 370u64);
    let n5722: ZW = zw_bits_n(n4767);
    let n5723: ZW = zw_mix1(n5673, n5722, 286u64);
    let n5724: ZW = zw_mix2(n5674, n5722, 286u64);
    let n5725: ZW = zw_mix1(n5723, n5678, 293u64);
    let n5726: ZW = zw_mix2(n5724, n5678, 293u64);
    let n5727: ZW = zw_bits_b(n4759);
    let n5728: ZW = zw_mix1(n5725, n5727, 294u64);
    let n5729: ZW = zw_mix2(n5726, n5727, 294u64);
    let n5730: ZW = zw_mix1(n5728, n5684, 300u64);
    let n5731: ZW = zw_mix2(n5729, n5684, 300u64);
    let n5732: ZW = zw_mix1(n5730, n5131, 357u64);
    let n5733: ZW = zw_mix2(n5731, n5131, 357u64);
    let n5734: ZW = zw_mix1(n5732, n5134, 358u64);
    let n5735: ZW = zw_mix2(n5733, n5134, 358u64);
    let n5736: ZW = zw_mix1(n5734, n5137, 359u64);
    let n5737: ZW = zw_mix2(n5735, n5137, 359u64);
    let n5738: ZW = zw_mix1(n5736, n5140, 360u64);
    let n5739: ZW = zw_mix2(n5737, n5140, 360u64);
    let n5740: ZW = zw_mix1(n5738, n5695, 361u64);
    let n5741: ZW = zw_mix2(n5739, n5695, 361u64);
    let n5742: ZW = zw_bits_n(n4780);
    let n5743: ZW = zw_mix1(n5740, n5742, 369u64);
    let n5744: ZW = zw_mix2(n5741, n5742, 369u64);
    let n5745: ZW = zw_bits_n(n4769);
    let n5746: ZW = zw_mix1(n5743, n5745, 370u64);
    let n5747: ZW = zw_mix2(n5744, n5745, 370u64);
    let n5748: ZW = zw_mix1(n5738, n5704, 361u64);
    let n5749: ZW = zw_mix2(n5739, n5704, 361u64);
    let n5750: ZW = zw_bits_n(n4799);
    let n5751: ZW = zw_mix1(n5748, n5750, 369u64);
    let n5752: ZW = zw_mix2(n5749, n5750, 369u64);
    let n5753: ZW = zw_bits_n(n4788);
    let n5754: ZW = zw_mix1(n5751, n5753, 370u64);
    let n5755: ZW = zw_mix2(n5752, n5753, 370u64);
    let n5756: ZW = zw_mix1(n5738, n5713, 361u64);
    let n5757: ZW = zw_mix2(n5739, n5713, 361u64);
    let n5758: ZW = zw_bits_n(n4818);
    let n5759: ZW = zw_mix1(n5756, n5758, 369u64);
    let n5760: ZW = zw_mix2(n5757, n5758, 369u64);
    let n5761: ZW = zw_bits_n(n4807);
    let n5762: ZW = zw_mix1(n5759, n5761, 370u64);
    let n5763: ZW = zw_mix2(n5760, n5761, 370u64);
    let n5764: ZW = zw_bits_n(n4840);
    let n5765: ZW = zw_mix1(n5659, n5764, 20u64);
    let n5766: ZW = zw_mix2(n5660, n5764, 20u64);
    let n5767: ZW = zw_bits_b(n4841);
    let n5768: ZW = zw_mix1(n5765, n5767, 41u64);
    let n5769: ZW = zw_mix2(n5766, n5767, 41u64);
    let n5770: ZW = zw_bits_n(n4842);
    let n5771: ZW = zw_mix1(n5768, n5770, 281u64);
    let n5772: ZW = zw_mix2(n5769, n5770, 281u64);
    let n5773: ZW = zw_bits_n(n4843);
    let n5774: ZW = zw_mix1(n5771, n5773, 283u64);
    let n5775: ZW = zw_mix2(n5772, n5773, 283u64);
    let n5776: ZW = zw_bits_n(n4844);
    let n5777: ZW = zw_mix1(n5774, n5776, 284u64);
    let n5778: ZW = zw_mix2(n5775, n5776, 284u64);
    let n5779: ZW = zw_mix1(n5777, n5675, 286u64);
    let n5780: ZW = zw_mix2(n5778, n5675, 286u64);
    let n5781: ZW = zw_bits_b(n4820);
    let n5782: ZW = zw_mix1(n5779, n5781, 293u64);
    let n5783: ZW = zw_mix2(n5780, n5781, 293u64);
    let n5784: ZW = zw_mix1(n5782, n5681, 294u64);
    let n5785: ZW = zw_mix2(n5783, n5681, 294u64);
    let n5786: ZW = zw_bits_n(n4863);
    let n5787: ZW = zw_mix1(n5784, n5786, 300u64);
    let n5788: ZW = zw_mix2(n5785, n5786, 300u64);
    let n5789: ZW = zw_bits_n(n4845);
    let n5790: ZW = zw_mix1(n5787, n5789, 357u64);
    let n5791: ZW = zw_mix2(n5788, n5789, 357u64);
    let n5792: ZW = zw_bits_n(n4846);
    let n5793: ZW = zw_mix1(n5790, n5792, 358u64);
    let n5794: ZW = zw_mix2(n5791, n5792, 358u64);
    let n5795: ZW = zw_bits_n(n4847);
    let n5796: ZW = zw_mix1(n5793, n5795, 359u64);
    let n5797: ZW = zw_mix2(n5794, n5795, 359u64);
    let n5798: ZW = zw_bits_n(n4848);
    let n5799: ZW = zw_mix1(n5796, n5798, 360u64);
    let n5800: ZW = zw_mix2(n5797, n5798, 360u64);
    let n5801: ZW = zw_mix1(n5799, n5695, 361u64);
    let n5802: ZW = zw_mix2(n5800, n5695, 361u64);
    let n5803: ZW = zw_bits_n(n4864);
    let n5804: ZW = zw_mix1(n5801, n5803, 369u64);
    let n5805: ZW = zw_mix2(n5802, n5803, 369u64);
    let n5806: ZW = zw_bits_n(n4850);
    let n5807: ZW = zw_mix1(n5804, n5806, 370u64);
    let n5808: ZW = zw_mix2(n5805, n5806, 370u64);
    let n5809: ZW = zw_bits_n(n4875);
    let n5810: ZW = zw_mix1(n5790, n5809, 358u64);
    let n5811: ZW = zw_mix2(n5791, n5809, 358u64);
    let n5812: ZW = zw_bits_n(n4876);
    let n5813: ZW = zw_mix1(n5810, n5812, 359u64);
    let n5814: ZW = zw_mix2(n5811, n5812, 359u64);
    let n5815: ZW = zw_mix1(n5813, n5798, 360u64);
    let n5816: ZW = zw_mix2(n5814, n5798, 360u64);
    let n5817: ZW = zw_mix1(n5815, n5704, 361u64);
    let n5818: ZW = zw_mix2(n5816, n5704, 361u64);
    let n5819: ZW = zw_bits_n(n4889);
    let n5820: ZW = zw_mix1(n5817, n5819, 369u64);
    let n5821: ZW = zw_mix2(n5818, n5819, 369u64);
    let n5822: ZW = zw_bits_n(n4878);
    let n5823: ZW = zw_mix1(n5820, n5822, 370u64);
    let n5824: ZW = zw_mix2(n5821, n5822, 370u64);
    let n5825: ZW = zw_bits_n(n4898);
    let n5826: ZW = zw_mix1(n5810, n5825, 359u64);
    let n5827: ZW = zw_mix2(n5811, n5825, 359u64);
    let n5828: ZW = zw_mix1(n5826, n5798, 360u64);
    let n5829: ZW = zw_mix2(n5827, n5798, 360u64);
    let n5830: ZW = zw_mix1(n5828, n5713, 361u64);
    let n5831: ZW = zw_mix2(n5829, n5713, 361u64);
    let n5832: ZW = zw_bits_n(n4911);
    let n5833: ZW = zw_mix1(n5830, n5832, 369u64);
    let n5834: ZW = zw_mix2(n5831, n5832, 369u64);
    let n5835: ZW = zw_bits_n(n4900);
    let n5836: ZW = zw_mix1(n5833, n5835, 370u64);
    let n5837: ZW = zw_mix2(n5834, n5835, 370u64);
    let n5838: ZW = zw_bits_n(n4926);
    let n5839: ZW = zw_mix1(n5787, n5838, 357u64);
    let n5840: ZW = zw_mix2(n5788, n5838, 357u64);
    let n5841: ZW = zw_bits_n(n4927);
    let n5842: ZW = zw_mix1(n5839, n5841, 358u64);
    let n5843: ZW = zw_mix2(n5840, n5841, 358u64);
    let n5844: ZW = zw_bits_n(n4928);
    let n5845: ZW = zw_mix1(n5842, n5844, 359u64);
    let n5846: ZW = zw_mix2(n5843, n5844, 359u64);
    let n5847: ZW = zw_bits_n(n4929);
    let n5848: ZW = zw_mix1(n5845, n5847, 360u64);
    let n5849: ZW = zw_mix2(n5846, n5847, 360u64);
    let n5850: ZW = zw_mix1(n5848, n5695, 361u64);
    let n5851: ZW = zw_mix2(n5849, n5695, 361u64);
    let n5852: ZW = zw_bits_n(n4942);
    let n5853: ZW = zw_mix1(n5850, n5852, 369u64);
    let n5854: ZW = zw_mix2(n5851, n5852, 369u64);
    let n5855: ZW = zw_bits_n(n4931);
    let n5856: ZW = zw_mix1(n5853, n5855, 370u64);
    let n5857: ZW = zw_mix2(n5854, n5855, 370u64);
    let n5858: ZW = zw_mix1(n5839, n5809, 358u64);
    let n5859: ZW = zw_mix2(n5840, n5809, 358u64);
    let n5860: ZW = zw_mix1(n5858, n5812, 359u64);
    let n5861: ZW = zw_mix2(n5859, n5812, 359u64);
    let n5862: ZW = zw_mix1(n5860, n5847, 360u64);
    let n5863: ZW = zw_mix2(n5861, n5847, 360u64);
    let n5864: ZW = zw_mix1(n5862, n5704, 361u64);
    let n5865: ZW = zw_mix2(n5863, n5704, 361u64);
    let n5866: ZW = zw_bits_n(n4951);
    let n5867: ZW = zw_mix1(n5864, n5866, 369u64);
    let n5868: ZW = zw_mix2(n5865, n5866, 369u64);
    let n5869: ZW = zw_bits_n(n4949);
    let n5870: ZW = zw_mix1(n5867, n5869, 370u64);
    let n5871: ZW = zw_mix2(n5868, n5869, 370u64);
    let n5872: ZW = zw_mix1(n5858, n5825, 359u64);
    let n5873: ZW = zw_mix2(n5859, n5825, 359u64);
    let n5874: ZW = zw_mix1(n5872, n5847, 360u64);
    let n5875: ZW = zw_mix2(n5873, n5847, 360u64);
    let n5876: ZW = zw_mix1(n5874, n5713, 361u64);
    let n5877: ZW = zw_mix2(n5875, n5713, 361u64);
    let n5878: ZW = zw_bits_n(n4959);
    let n5879: ZW = zw_mix1(n5876, n5878, 369u64);
    let n5880: ZW = zw_mix2(n5877, n5878, 369u64);
    let n5881: ZW = zw_bits_n(n4957);
    let n5882: ZW = zw_mix1(n5879, n5881, 370u64);
    let n5883: ZW = zw_mix2(n5880, n5881, 370u64);
    let n5884: ZW = zw_bits_n(n4964);
    let n5885: ZW = zw_mix1(n5845, n5884, 360u64);
    let n5886: ZW = zw_mix2(n5846, n5884, 360u64);
    let n5887: ZW = zw_mix1(n5885, n5695, 361u64);
    let n5888: ZW = zw_mix2(n5886, n5695, 361u64);
    let n5889: ZW = zw_mix1(n5887, n5852, 369u64);
    let n5890: ZW = zw_mix2(n5888, n5852, 369u64);
    let n5891: ZW = zw_bits_n(n4965);
    let n5892: ZW = zw_mix1(n5889, n5891, 370u64);
    let n5893: ZW = zw_mix2(n5890, n5891, 370u64);
    let n5894: ZW = zw_mix1(n5860, n5884, 360u64);
    let n5895: ZW = zw_mix2(n5861, n5884, 360u64);
    let n5896: ZW = zw_mix1(n5894, n5704, 361u64);
    let n5897: ZW = zw_mix2(n5895, n5704, 361u64);
    let n5898: ZW = zw_mix1(n5896, n5866, 369u64);
    let n5899: ZW = zw_mix2(n5897, n5866, 369u64);
    let n5900: ZW = zw_bits_n(n4968);
    let n5901: ZW = zw_mix1(n5898, n5900, 370u64);
    let n5902: ZW = zw_mix2(n5899, n5900, 370u64);
    let n5903: ZW = zw_mix1(n5872, n5884, 360u64);
    let n5904: ZW = zw_mix2(n5873, n5884, 360u64);
    let n5905: ZW = zw_mix1(n5903, n5713, 361u64);
    let n5906: ZW = zw_mix2(n5904, n5713, 361u64);
    let n5907: ZW = zw_mix1(n5905, n5878, 369u64);
    let n5908: ZW = zw_mix2(n5906, n5878, 369u64);
    let n5909: ZW = zw_bits_n(n4971);
    let n5910: ZW = zw_mix1(n5907, n5909, 370u64);
    let n5911: ZW = zw_mix2(n5908, n5909, 370u64);
    let n5912: ZW = zw_mix1(n5777, n5722, 286u64);
    let n5913: ZW = zw_mix2(n5778, n5722, 286u64);
    let n5914: ZW = zw_mix1(n5912, n5781, 293u64);
    let n5915: ZW = zw_mix2(n5913, n5781, 293u64);
    let n5916: ZW = zw_mix1(n5914, n5727, 294u64);
    let n5917: ZW = zw_mix2(n5915, n5727, 294u64);
    let n5918: ZW = zw_mix1(n5916, n5786, 300u64);
    let n5919: ZW = zw_mix2(n5917, n5786, 300u64);
    let n5920: ZW = zw_mix1(n5918, n5789, 357u64);
    let n5921: ZW = zw_mix2(n5919, n5789, 357u64);
    let n5922: ZW = zw_mix1(n5920, n5792, 358u64);
    let n5923: ZW = zw_mix2(n5921, n5792, 358u64);
    let n5924: ZW = zw_mix1(n5922, n5795, 359u64);
    let n5925: ZW = zw_mix2(n5923, n5795, 359u64);
    let n5926: ZW = zw_mix1(n5924, n5798, 360u64);
    let n5927: ZW = zw_mix2(n5925, n5798, 360u64);
    let n5928: ZW = zw_mix1(n5926, n5695, 361u64);
    let n5929: ZW = zw_mix2(n5927, n5695, 361u64);
    let n5930: ZW = zw_bits_n(n4989);
    let n5931: ZW = zw_mix1(n5928, n5930, 369u64);
    let n5932: ZW = zw_mix2(n5929, n5930, 369u64);
    let n5933: ZW = zw_bits_n(n4978);
    let n5934: ZW = zw_mix1(n5931, n5933, 370u64);
    let n5935: ZW = zw_mix2(n5932, n5933, 370u64);
    let n5936: ZW = zw_mix1(n5920, n5809, 358u64);
    let n5937: ZW = zw_mix2(n5921, n5809, 358u64);
    let n5938: ZW = zw_mix1(n5936, n5812, 359u64);
    let n5939: ZW = zw_mix2(n5937, n5812, 359u64);
    let n5940: ZW = zw_mix1(n5938, n5798, 360u64);
    let n5941: ZW = zw_mix2(n5939, n5798, 360u64);
    let n5942: ZW = zw_mix1(n5940, n5704, 361u64);
    let n5943: ZW = zw_mix2(n5941, n5704, 361u64);
    let n5944: ZW = zw_bits_n(n5008);
    let n5945: ZW = zw_mix1(n5942, n5944, 369u64);
    let n5946: ZW = zw_mix2(n5943, n5944, 369u64);
    let n5947: ZW = zw_bits_n(n4997);
    let n5948: ZW = zw_mix1(n5945, n5947, 370u64);
    let n5949: ZW = zw_mix2(n5946, n5947, 370u64);
    let n5950: ZW = zw_mix1(n5936, n5825, 359u64);
    let n5951: ZW = zw_mix2(n5937, n5825, 359u64);
    let n5952: ZW = zw_mix1(n5950, n5798, 360u64);
    let n5953: ZW = zw_mix2(n5951, n5798, 360u64);
    let n5954: ZW = zw_mix1(n5952, n5713, 361u64);
    let n5955: ZW = zw_mix2(n5953, n5713, 361u64);
    let n5956: ZW = zw_bits_n(n5027);
    let n5957: ZW = zw_mix1(n5954, n5956, 369u64);
    let n5958: ZW = zw_mix2(n5955, n5956, 369u64);
    let n5959: ZW = zw_bits_n(n5016);
    let n5960: ZW = zw_mix1(n5957, n5959, 370u64);
    let n5961: ZW = zw_mix2(n5958, n5959, 370u64);
    let n5962: ZW = zw_mix1(n5918, n5838, 357u64);
    let n5963: ZW = zw_mix2(n5919, n5838, 357u64);
    let n5964: ZW = zw_mix1(n5962, n5841, 358u64);
    let n5965: ZW = zw_mix2(n5963, n5841, 358u64);
    let n5966: ZW = zw_mix1(n5964, n5844, 359u64);
    let n5967: ZW = zw_mix2(n5965, n5844, 359u64);
    let n5968: ZW = zw_mix1(n5966, n5847, 360u64);
    let n5969: ZW = zw_mix2(n5967, n5847, 360u64);
    let n5970: ZW = zw_mix1(n5968, n5695, 361u64);
    let n5971: ZW = zw_mix2(n5969, n5695, 361u64);
    let n5972: ZW = zw_bits_n(n5046);
    let n5973: ZW = zw_mix1(n5970, n5972, 369u64);
    let n5974: ZW = zw_mix2(n5971, n5972, 369u64);
    let n5975: ZW = zw_bits_n(n5035);
    let n5976: ZW = zw_mix1(n5973, n5975, 370u64);
    let n5977: ZW = zw_mix2(n5974, n5975, 370u64);
    let n5978: ZW = zw_mix1(n5962, n5809, 358u64);
    let n5979: ZW = zw_mix2(n5963, n5809, 358u64);
    let n5980: ZW = zw_mix1(n5978, n5812, 359u64);
    let n5981: ZW = zw_mix2(n5979, n5812, 359u64);
    let n5982: ZW = zw_mix1(n5980, n5847, 360u64);
    let n5983: ZW = zw_mix2(n5981, n5847, 360u64);
    let n5984: ZW = zw_mix1(n5982, n5704, 361u64);
    let n5985: ZW = zw_mix2(n5983, n5704, 361u64);
    let n5986: ZW = zw_bits_n(n5055);
    let n5987: ZW = zw_mix1(n5984, n5986, 369u64);
    let n5988: ZW = zw_mix2(n5985, n5986, 369u64);
    let n5989: ZW = zw_bits_n(n5053);
    let n5990: ZW = zw_mix1(n5987, n5989, 370u64);
    let n5991: ZW = zw_mix2(n5988, n5989, 370u64);
    let n5992: ZW = zw_mix1(n5978, n5825, 359u64);
    let n5993: ZW = zw_mix2(n5979, n5825, 359u64);
    let n5994: ZW = zw_mix1(n5992, n5847, 360u64);
    let n5995: ZW = zw_mix2(n5993, n5847, 360u64);
    let n5996: ZW = zw_mix1(n5994, n5713, 361u64);
    let n5997: ZW = zw_mix2(n5995, n5713, 361u64);
    let n5998: ZW = zw_bits_n(n5063);
    let n5999: ZW = zw_mix1(n5996, n5998, 369u64);
    let n6000: ZW = zw_mix2(n5997, n5998, 369u64);
    let n6001: ZW = zw_bits_n(n5061);
    let n6002: ZW = zw_mix1(n5999, n6001, 370u64);
    let n6003: ZW = zw_mix2(n6000, n6001, 370u64);
    let n6004: ZW = zw_mix1(n5966, n5884, 360u64);
    let n6005: ZW = zw_mix2(n5967, n5884, 360u64);
    let n6006: ZW = zw_mix1(n6004, n5695, 361u64);
    let n6007: ZW = zw_mix2(n6005, n5695, 361u64);
    let n6008: ZW = zw_mix1(n6006, n5972, 369u64);
    let n6009: ZW = zw_mix2(n6007, n5972, 369u64);
    let n6010: ZW = zw_bits_n(n5066);
    let n6011: ZW = zw_mix1(n6008, n6010, 370u64);
    let n6012: ZW = zw_mix2(n6009, n6010, 370u64);
    let n6013: ZW = zw_mix1(n5980, n5884, 360u64);
    let n6014: ZW = zw_mix2(n5981, n5884, 360u64);
    let n6015: ZW = zw_mix1(n6013, n5704, 361u64);
    let n6016: ZW = zw_mix2(n6014, n5704, 361u64);
    let n6017: ZW = zw_mix1(n6015, n5986, 369u64);
    let n6018: ZW = zw_mix2(n6016, n5986, 369u64);
    let n6019: ZW = zw_bits_n(n5069);
    let n6020: ZW = zw_mix1(n6017, n6019, 370u64);
    let n6021: ZW = zw_mix2(n6018, n6019, 370u64);
    let n6022: ZW = zw_mix1(n5992, n5884, 360u64);
    let n6023: ZW = zw_mix2(n5993, n5884, 360u64);
    let n6024: ZW = zw_mix1(n6022, n5713, 361u64);
    let n6025: ZW = zw_mix2(n6023, n5713, 361u64);
    let n6026: ZW = zw_mix1(n6024, n5998, 369u64);
    let n6027: ZW = zw_mix2(n6025, n5998, 369u64);
    let n6028: ZW = zw_bits_n(n5072);
    let n6029: ZW = zw_mix1(n6026, n6028, 370u64);
    let n6030: ZW = zw_mix2(n6027, n6028, 370u64);
    let ok_v0_b0: u16 = ALL & zb_holds(n1419) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v0_b0: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v0_b0: u16 = ALL & zb_holds(n1579);
    let ok_v1_b1: u16 = ALL & zb_holds(n1419) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v1_b1: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v1_b1: u16 = ALL & zb_holds(n1648);
    let ok_v2_b2: u16 = ALL & zb_holds(n1419) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v2_b2: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v2_b2: u16 = ALL & zb_holds(n1704);
    let ok_v16_b3: u16 = ALL & zb_holds(n1419) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v16_b3: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v16_b3: u16 = ALL & zb_holds(n1747);
    let ok_v17_b4: u16 = ALL & zb_holds(n1419) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v17_b4: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v17_b4: u16 = ALL & zb_holds(n1788);
    let ok_v18_b5: u16 = ALL & zb_holds(n1419) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v18_b5: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v18_b5: u16 = ALL & zb_holds(n1829);
    let ok_v32_b6: u16 = ALL & zb_holds(n1419) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v32_b6: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v32_b6: u16 = ALL & zb_holds(n1882);
    let ok_v33_b7: u16 = ALL & zb_holds(n1419) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v33_b7: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v33_b7: u16 = ALL & zb_holds(n1905);
    let ok_v34_b8: u16 = ALL & zb_holds(n1419) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v34_b8: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v34_b8: u16 = ALL & zb_holds(n1926);
    let ok_v36_b9: u16 = ALL & zb_holds(n1419) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v36_b9: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v36_b9: u16 = ALL & zb_holds(n1952);
    let ok_v37_b10: u16 = ALL & zb_holds(n1419) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v37_b10: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v37_b10: u16 = ALL & zb_holds(n1905);
    let ok_v38_b11: u16 = ALL & zb_holds(n1419) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v38_b11: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v38_b11: u16 = ALL & zb_holds(n1926);
    let ok_v40_b12: u16 = ALL & zb_holds(n1419) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v40_b12: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v40_b12: u16 = ALL & zb_holds(n1952);
    let ok_v41_b13: u16 = ALL & zb_holds(n1419) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v41_b13: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v41_b13: u16 = ALL & zb_holds(n1905);
    let ok_v42_b14: u16 = ALL & zb_holds(n1419) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v42_b14: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v42_b14: u16 = ALL & zb_holds(n1926);
    let ok_v48_b15: u16 = ALL & zb_holds(n1419) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v48_b15: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v48_b15: u16 = ALL & zb_holds(n2003);
    let ok_v49_b16: u16 = ALL & zb_holds(n1419) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v49_b16: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v49_b16: u16 = ALL & zb_holds(n2022);
    let ok_v50_b17: u16 = ALL & zb_holds(n1419) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v50_b17: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v50_b17: u16 = ALL & zb_holds(n2041);
    let ok_v52_b18: u16 = ALL & zb_holds(n1419) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v52_b18: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v52_b18: u16 = ALL & zb_holds(n2058);
    let ok_v53_b19: u16 = ALL & zb_holds(n1419) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v53_b19: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v53_b19: u16 = ALL & zb_holds(n2022);
    let ok_v54_b20: u16 = ALL & zb_holds(n1419) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v54_b20: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v54_b20: u16 = ALL & zb_holds(n2041);
    let ok_v56_b21: u16 = ALL & zb_holds(n1419) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v56_b21: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v56_b21: u16 = ALL & zb_holds(n2058);
    let ok_v57_b22: u16 = ALL & zb_holds(n1419) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v57_b22: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v57_b22: u16 = ALL & zb_holds(n2022);
    let ok_v58_b23: u16 = ALL & zb_holds(n1419) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v58_b23: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v58_b23: u16 = ALL & zb_holds(n2041);
    let ok_v0_b24: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n2152);
    let bd_v0_b24: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v0_b24: u16 = ALL & zb_holds(n134) & zb_holds(n2204);
    let ok_v1_b25: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n2152);
    let bd_v1_b25: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v1_b25: u16 = ALL & zb_holds(n134) & zb_holds(n2232);
    let ok_v2_b26: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n2152);
    let bd_v2_b26: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v2_b26: u16 = ALL & zb_holds(n134) & zb_holds(n2259);
    let ok_v16_b27: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n2152);
    let bd_v16_b27: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v16_b27: u16 = ALL & zb_holds(n134) & zb_holds(n2288);
    let ok_v17_b28: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n2152);
    let bd_v17_b28: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v17_b28: u16 = ALL & zb_holds(n134) & zb_holds(n2317);
    let ok_v18_b29: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n2152);
    let bd_v18_b29: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v18_b29: u16 = ALL & zb_holds(n134) & zb_holds(n2346);
    let ok_v32_b30: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n2152);
    let bd_v32_b30: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v32_b30: u16 = ALL & zb_holds(n2366);
    let ok_v33_b31: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n2152);
    let bd_v33_b31: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v33_b31: u16 = ALL & zb_holds(n2373);
    let ok_v34_b32: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n2152);
    let bd_v34_b32: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v34_b32: u16 = ALL & zb_holds(n2380);
    let ok_v36_b33: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n2152);
    let bd_v36_b33: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v36_b33: u16 = ALL & zb_holds(n2385);
    let ok_v48_b34: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n2152);
    let bd_v48_b34: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v48_b34: u16 = ALL & zb_holds(n2404);
    let ok_v49_b35: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n2152);
    let bd_v49_b35: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v49_b35: u16 = ALL & zb_holds(n2411);
    let ok_v50_b36: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n2152);
    let bd_v50_b36: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v50_b36: u16 = ALL & zb_holds(n2418);
    let ok_v52_b37: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n2152);
    let bd_v52_b37: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v52_b37: u16 = ALL & zb_holds(n2423);
    let ok_v0_b38: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n3665);
    let bd_v0_b38: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v0_b38: u16 = ALL & zb_holds(n134) & zb_holds(n3802) & zb_holds(n3805);
    let ok_v1_b39: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n3665);
    let bd_v1_b39: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v1_b39: u16 = ALL & zb_holds(n134) & zb_holds(n3802) & zb_holds(n3859);
    let ok_v2_b40: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n3665);
    let bd_v2_b40: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v2_b40: u16 = ALL & zb_holds(n134) & zb_holds(n3802) & zb_holds(n3910);
    let ok_v16_b41: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n3665);
    let bd_v16_b41: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v16_b41: u16 = ALL & zb_holds(n134) & zb_holds(n3802) & zb_holds(n3946);
    let ok_v17_b42: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n3665);
    let bd_v17_b42: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v17_b42: u16 = ALL & zb_holds(n134) & zb_holds(n3802) & zb_holds(n3982);
    let ok_v18_b43: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n3665);
    let bd_v18_b43: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v18_b43: u16 = ALL & zb_holds(n134) & zb_holds(n3802) & zb_holds(n4018);
    let ok_v32_b44: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n3665);
    let bd_v32_b44: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v32_b44: u16 = ALL & zb_holds(n4051);
    let ok_v33_b45: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n3665);
    let bd_v33_b45: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v33_b45: u16 = ALL & zb_holds(n4062);
    let ok_v34_b46: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n3665);
    let bd_v34_b46: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v34_b46: u16 = ALL & zb_holds(n4073);
    let ok_v36_b47: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n3665);
    let bd_v36_b47: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v36_b47: u16 = ALL & zb_holds(n4082);
    let ok_v48_b48: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n3665);
    let bd_v48_b48: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v48_b48: u16 = ALL & zb_holds(n4105);
    let ok_v49_b49: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n3665);
    let bd_v49_b49: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v49_b49: u16 = ALL & zb_holds(n4116);
    let ok_v50_b50: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n3665);
    let bd_v50_b50: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v50_b50: u16 = ALL & zb_holds(n4127);
    let ok_v52_b51: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n3665);
    let bd_v52_b51: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v52_b51: u16 = ALL & zb_holds(n4136);
    let ok_v0_b52: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4209);
    let bd_v0_b52: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v0_b52: u16 = ALL & zb_holds(n134) & zb_holds(n4208);
    let ok_v1_b53: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4253);
    let bd_v1_b53: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v1_b53: u16 = ALL & zb_holds(n134) & zb_holds(n4252);
    let ok_v2_b54: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4297);
    let bd_v2_b54: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v2_b54: u16 = ALL & zb_holds(n134) & zb_holds(n4296);
    let ok_v16_b55: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4340);
    let bd_v16_b55: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v16_b55: u16 = ALL & zb_holds(n134) & zb_holds(n4339);
    let ok_v17_b56: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4383);
    let bd_v17_b56: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v17_b56: u16 = ALL & zb_holds(n134) & zb_holds(n4382);
    let ok_v18_b57: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4426);
    let bd_v18_b57: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v18_b57: u16 = ALL & zb_holds(n134) & zb_holds(n4425);
    let ok_v32_b58: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4454);
    let bd_v32_b58: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v32_b58: u16 = ALL & zb_holds(n4457);
    let ok_v33_b59: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4472);
    let bd_v33_b59: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v33_b59: u16 = ALL & zb_holds(n4475);
    let ok_v34_b60: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4490);
    let bd_v34_b60: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v34_b60: u16 = ALL & zb_holds(n4493);
    let ok_v36_b61: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4506);
    let bd_v36_b61: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v36_b61: u16 = ALL & zb_holds(n4509);
    let ok_v48_b62: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4536);
    let bd_v48_b62: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v48_b62: u16 = ALL & zb_holds(n4539);
    let ok_v49_b63: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4554);
    let bd_v49_b63: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v49_b63: u16 = ALL & zb_holds(n4557);
    let ok_v50_b64: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4572);
    let bd_v50_b64: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v50_b64: u16 = ALL & zb_holds(n4575);
    let ok_v52_b65: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4588);
    let bd_v52_b65: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v52_b65: u16 = ALL & zb_holds(n4591);
    let ok_v0_b66: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4681);
    let bd_v0_b66: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v0_b66: u16 = ALL & zb_holds(n4703);
    let ok_v1_b67: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4681);
    let bd_v1_b67: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v1_b67: u16 = ALL & zb_holds(n4731);
    let ok_v2_b68: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4681);
    let bd_v2_b68: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v2_b68: u16 = ALL & zb_holds(n4758);
    let ok_v16_b69: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4681);
    let bd_v16_b69: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v16_b69: u16 = ALL & zb_holds(n4781);
    let ok_v17_b70: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4681);
    let bd_v17_b70: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v17_b70: u16 = ALL & zb_holds(n4800);
    let ok_v18_b71: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4681);
    let bd_v18_b71: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v18_b71: u16 = ALL & zb_holds(n4819);
    let ok_v32_b72: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4681);
    let bd_v32_b72: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v32_b72: u16 = ALL & zb_holds(n4865);
    let ok_v33_b73: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4681);
    let bd_v33_b73: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v33_b73: u16 = ALL & zb_holds(n4890);
    let ok_v34_b74: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4681);
    let bd_v34_b74: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v34_b74: u16 = ALL & zb_holds(n4912);
    let ok_v36_b75: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4681);
    let bd_v36_b75: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v36_b75: u16 = ALL & zb_holds(n4943);
    let ok_v37_b76: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4681);
    let bd_v37_b76: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v37_b76: u16 = ALL & zb_holds(n4890);
    let ok_v38_b77: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4681);
    let bd_v38_b77: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v38_b77: u16 = ALL & zb_holds(n4912);
    let ok_v40_b78: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4681);
    let bd_v40_b78: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v40_b78: u16 = ALL & zb_holds(n4943);
    let ok_v41_b79: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4681);
    let bd_v41_b79: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v41_b79: u16 = ALL & zb_holds(n4890);
    let ok_v42_b80: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4681);
    let bd_v42_b80: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v42_b80: u16 = ALL & zb_holds(n4912);
    let ok_v48_b81: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4681);
    let bd_v48_b81: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v48_b81: u16 = ALL & zb_holds(n4990);
    let ok_v49_b82: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4681);
    let bd_v49_b82: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v49_b82: u16 = ALL & zb_holds(n5009);
    let ok_v50_b83: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4681);
    let bd_v50_b83: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v50_b83: u16 = ALL & zb_holds(n5028);
    let ok_v52_b84: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4681);
    let bd_v52_b84: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v52_b84: u16 = ALL & zb_holds(n5047);
    let ok_v53_b85: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4681);
    let bd_v53_b85: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v53_b85: u16 = ALL & zb_holds(n5009);
    let ok_v54_b86: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4681);
    let bd_v54_b86: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v54_b86: u16 = ALL & zb_holds(n5028);
    let ok_v56_b87: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4681);
    let bd_v56_b87: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v56_b87: u16 = ALL & zb_holds(n5047);
    let ok_v57_b88: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4681);
    let bd_v57_b88: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v57_b88: u16 = ALL & zb_holds(n5009);
    let ok_v58_b89: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n4681);
    let bd_v58_b89: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v58_b89: u16 = ALL & zb_holds(n5028);
    let sh0 = KShared0 {
        c87: r_c87,
        c39: r_c39,
        c84: n115,
        c86: n214,
        c241: n303,
        c254: n304,
        c368: n545,
        c369: n771,
        c302: n770,
        c85: n215,
    };
    let sh1 = KShared1 {
        c87: n2077,
        c84: n115,
        c86: n214,
        c240: n303,
        c253: n304,
        c85: n215,
    };
    let sh2 = KShared2 {
        c87: n3809,
        c84: n115,
        c86: n214,
        c240: n303,
        c253: n304,
        c85: n215,
    };
    let sh3 = KShared3 {
        c84: n115,
        c86: n214,
        c85: n215,
    };
    let sh4 = KShared4 {
        c87: r_c87,
        c39: r_c39,
        c84: n115,
        c86: n214,
        c241: n4667,
        c254: n4668,
        c367: n4676,
        c368: n4677,
        c301: n4674,
        c85: n215,
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
        c282: n290,
        c360: r_c359,
        c361: r_c360,
        c284: n294,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n1560,
        c287: n1434,
        c294: zb_splat(false),
        c295: zb_splat(false),
        c370: n1581,
        c371: n1562,
        c301: n1580,
        h1: n5150, h2: n5151,
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
        c282: n290,
        c360: r_c359,
        c361: r_c360,
        c284: n294,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n1637,
        c287: n1434,
        c294: zb_splat(false),
        c295: zb_splat(false),
        c370: n1649,
        c371: n1639,
        c301: n1580,
        h1: n5159, h2: n5160,
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
        c282: n290,
        c360: r_c359,
        c361: r_c360,
        c284: n294,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n1693,
        c287: n1434,
        c294: zb_splat(false),
        c295: zb_splat(false),
        c370: n1705,
        c371: n1695,
        c301: n1580,
        h1: n5168, h2: n5169,
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
        c282: n290,
        c360: r_c359,
        c361: r_c360,
        c284: n294,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n1560,
        c287: n1736,
        c294: zb_splat(false),
        c295: zb_splat(true),
        c370: n1748,
        c371: n1738,
        c301: n1580,
        h1: n5194, h2: n5195,
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
        c282: n290,
        c360: r_c359,
        c361: r_c360,
        c284: n294,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n1637,
        c287: n1736,
        c294: zb_splat(false),
        c295: zb_splat(true),
        c370: n1789,
        c371: n1779,
        c301: n1580,
        h1: n5202, h2: n5203,
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
        c282: n290,
        c360: r_c359,
        c361: r_c360,
        c284: n294,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n1693,
        c287: n1736,
        c294: zb_splat(false),
        c295: zb_splat(true),
        c370: n1830,
        c371: n1820,
        c301: n1580,
        h1: n5210, h2: n5211,
    };
    // body 5: buttons 0x12, forks 0x0
    sink.o0(18, take_0_5, &sh0, &o0);
    declined |= live_v32_b6 & (if bd_v32_b6 { ALL } else { !ok_v32_b6 });
    take_0_6 |= live_v32_b6 & ok_v32_b6 & (if bd_v32_b6 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1838,
        c41: n1839,
        c358: n1843,
        c359: n1866,
        c282: n1840,
        c360: n1867,
        c361: n1844,
        c284: n1841,
        c285: n1842,
        c362: n1560,
        c287: n1434,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1881,
        c371: n1869,
        c301: n1880,
        h1: n5254, h2: n5255,
    };
    // body 6: buttons 0x20, forks 0x0
    sink.o0(32, take_0_6, &sh0, &o0);
    declined |= live_v33_b7 & (if bd_v33_b7 { ALL } else { !ok_v33_b7 });
    take_0_7 |= live_v33_b7 & ok_v33_b7 & (if bd_v33_b7 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1838,
        c41: n1839,
        c358: n1843,
        c359: n1890,
        c282: n1840,
        c360: n1891,
        c361: n1844,
        c284: n1841,
        c285: n1842,
        c362: n1637,
        c287: n1434,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1904,
        c371: n1893,
        c301: n1880,
        h1: n5270, h2: n5271,
    };
    // body 7: buttons 0x21, forks 0x0
    sink.o0(33, take_0_7, &sh0, &o0);
    declined |= live_v34_b8 & (if bd_v34_b8 { ALL } else { !ok_v34_b8 });
    take_0_8 |= live_v34_b8 & ok_v34_b8 & (if bd_v34_b8 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1838,
        c41: n1839,
        c358: n1843,
        c359: n1890,
        c282: n1840,
        c360: n1912,
        c361: n1844,
        c284: n1841,
        c285: n1842,
        c362: n1693,
        c287: n1434,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1925,
        c371: n1914,
        c301: n1880,
        h1: n5283, h2: n5284,
    };
    // body 8: buttons 0x22, forks 0x0
    sink.o0(34, take_0_8, &sh0, &o0);
    declined |= live_v36_b9 & (if bd_v36_b9 { ALL } else { !ok_v36_b9 });
    take_0_9 |= live_v36_b9 & ok_v36_b9 & (if bd_v36_b9 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1838,
        c41: n1839,
        c358: n1930,
        c359: n1937,
        c282: n1840,
        c360: n1938,
        c361: n1931,
        c284: n1841,
        c285: n1842,
        c362: n1560,
        c287: n1434,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1951,
        c371: n1940,
        c301: n1880,
        h1: n5303, h2: n5304,
    };
    // body 9: buttons 0x24, forks 0x0
    sink.o0(36, take_0_9, &sh0, &o0);
    declined |= live_v37_b10 & (if bd_v37_b10 { ALL } else { !ok_v37_b10 });
    take_0_10 |= live_v37_b10 & ok_v37_b10 & (if bd_v37_b10 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1838,
        c41: n1839,
        c358: n1930,
        c359: n1890,
        c282: n1840,
        c360: n1891,
        c361: n1931,
        c284: n1841,
        c285: n1842,
        c362: n1637,
        c287: n1434,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1958,
        c371: n1956,
        c301: n1880,
        h1: n5317, h2: n5318,
    };
    // body 10: buttons 0x25, forks 0x0
    sink.o0(37, take_0_10, &sh0, &o0);
    declined |= live_v38_b11 & (if bd_v38_b11 { ALL } else { !ok_v38_b11 });
    take_0_11 |= live_v38_b11 & ok_v38_b11 & (if bd_v38_b11 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1838,
        c41: n1839,
        c358: n1930,
        c359: n1890,
        c282: n1840,
        c360: n1912,
        c361: n1931,
        c284: n1841,
        c285: n1842,
        c362: n1693,
        c287: n1434,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1964,
        c371: n1962,
        c301: n1880,
        h1: n5329, h2: n5330,
    };
    // body 11: buttons 0x26, forks 0x0
    sink.o0(38, take_0_11, &sh0, &o0);
    declined |= live_v40_b12 & (if bd_v40_b12 { ALL } else { !ok_v40_b12 });
    take_0_12 |= live_v40_b12 & ok_v40_b12 & (if bd_v40_b12 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1838,
        c41: n1839,
        c358: n1930,
        c359: n1937,
        c282: n1840,
        c360: n1938,
        c361: n1966,
        c284: n1841,
        c285: n1842,
        c362: n1560,
        c287: n1434,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1951,
        c371: n1968,
        c301: n1880,
        h1: n5339, h2: n5340,
    };
    // body 12: buttons 0x28, forks 0x0
    sink.o0(40, take_0_12, &sh0, &o0);
    declined |= live_v41_b13 & (if bd_v41_b13 { ALL } else { !ok_v41_b13 });
    take_0_13 |= live_v41_b13 & ok_v41_b13 & (if bd_v41_b13 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1838,
        c41: n1839,
        c358: n1930,
        c359: n1890,
        c282: n1840,
        c360: n1891,
        c361: n1966,
        c284: n1841,
        c285: n1842,
        c362: n1637,
        c287: n1434,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1958,
        c371: n1970,
        c301: n1880,
        h1: n5348, h2: n5349,
    };
    // body 13: buttons 0x29, forks 0x0
    sink.o0(41, take_0_13, &sh0, &o0);
    declined |= live_v42_b14 & (if bd_v42_b14 { ALL } else { !ok_v42_b14 });
    take_0_14 |= live_v42_b14 & ok_v42_b14 & (if bd_v42_b14 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1838,
        c41: n1839,
        c358: n1930,
        c359: n1890,
        c282: n1840,
        c360: n1912,
        c361: n1966,
        c284: n1841,
        c285: n1842,
        c362: n1693,
        c287: n1434,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1964,
        c371: n1972,
        c301: n1880,
        h1: n5357, h2: n5358,
    };
    // body 14: buttons 0x2a, forks 0x0
    sink.o0(42, take_0_14, &sh0, &o0);
    declined |= live_v48_b15 & (if bd_v48_b15 { ALL } else { !ok_v48_b15 });
    take_0_15 |= live_v48_b15 & ok_v48_b15 & (if bd_v48_b15 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1838,
        c41: n1839,
        c358: n1843,
        c359: n1866,
        c282: n1840,
        c360: n1867,
        c361: n1844,
        c284: n1841,
        c285: n1842,
        c362: n1560,
        c287: n1736,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n2002,
        c371: n1991,
        c301: n1880,
        h1: n5381, h2: n5382,
    };
    // body 15: buttons 0x30, forks 0x0
    sink.o0(48, take_0_15, &sh0, &o0);
    declined |= live_v49_b16 & (if bd_v49_b16 { ALL } else { !ok_v49_b16 });
    take_0_16 |= live_v49_b16 & ok_v49_b16 & (if bd_v49_b16 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1838,
        c41: n1839,
        c358: n1843,
        c359: n1890,
        c282: n1840,
        c360: n1891,
        c361: n1844,
        c284: n1841,
        c285: n1842,
        c362: n1637,
        c287: n1736,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n2021,
        c371: n2010,
        c301: n1880,
        h1: n5395, h2: n5396,
    };
    // body 16: buttons 0x31, forks 0x0
    sink.o0(49, take_0_16, &sh0, &o0);
    declined |= live_v50_b17 & (if bd_v50_b17 { ALL } else { !ok_v50_b17 });
    take_0_17 |= live_v50_b17 & ok_v50_b17 & (if bd_v50_b17 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1838,
        c41: n1839,
        c358: n1843,
        c359: n1890,
        c282: n1840,
        c360: n1912,
        c361: n1844,
        c284: n1841,
        c285: n1842,
        c362: n1693,
        c287: n1736,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n2040,
        c371: n2029,
        c301: n1880,
        h1: n5407, h2: n5408,
    };
    // body 17: buttons 0x32, forks 0x0
    sink.o0(50, take_0_17, &sh0, &o0);
    declined |= live_v52_b18 & (if bd_v52_b18 { ALL } else { !ok_v52_b18 });
    take_0_18 |= live_v52_b18 & ok_v52_b18 & (if bd_v52_b18 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1838,
        c41: n1839,
        c358: n1930,
        c359: n1937,
        c282: n1840,
        c360: n1938,
        c361: n1931,
        c284: n1841,
        c285: n1842,
        c362: n1560,
        c287: n1736,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n2057,
        c371: n2046,
        c301: n1880,
        h1: n5423, h2: n5424,
    };
    // body 18: buttons 0x34, forks 0x0
    sink.o0(52, take_0_18, &sh0, &o0);
    declined |= live_v53_b19 & (if bd_v53_b19 { ALL } else { !ok_v53_b19 });
    take_0_19 |= live_v53_b19 & ok_v53_b19 & (if bd_v53_b19 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1838,
        c41: n1839,
        c358: n1930,
        c359: n1890,
        c282: n1840,
        c360: n1891,
        c361: n1931,
        c284: n1841,
        c285: n1842,
        c362: n1637,
        c287: n1736,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n2064,
        c371: n2062,
        c301: n1880,
        h1: n5437, h2: n5438,
    };
    // body 19: buttons 0x35, forks 0x0
    sink.o0(53, take_0_19, &sh0, &o0);
    declined |= live_v54_b20 & (if bd_v54_b20 { ALL } else { !ok_v54_b20 });
    take_0_20 |= live_v54_b20 & ok_v54_b20 & (if bd_v54_b20 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1838,
        c41: n1839,
        c358: n1930,
        c359: n1890,
        c282: n1840,
        c360: n1912,
        c361: n1931,
        c284: n1841,
        c285: n1842,
        c362: n1693,
        c287: n1736,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n2070,
        c371: n2068,
        c301: n1880,
        h1: n5449, h2: n5450,
    };
    // body 20: buttons 0x36, forks 0x0
    sink.o0(54, take_0_20, &sh0, &o0);
    declined |= live_v56_b21 & (if bd_v56_b21 { ALL } else { !ok_v56_b21 });
    take_0_21 |= live_v56_b21 & ok_v56_b21 & (if bd_v56_b21 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1838,
        c41: n1839,
        c358: n1930,
        c359: n1937,
        c282: n1840,
        c360: n1938,
        c361: n1966,
        c284: n1841,
        c285: n1842,
        c362: n1560,
        c287: n1736,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n2057,
        c371: n2072,
        c301: n1880,
        h1: n5458, h2: n5459,
    };
    // body 21: buttons 0x38, forks 0x0
    sink.o0(56, take_0_21, &sh0, &o0);
    declined |= live_v57_b22 & (if bd_v57_b22 { ALL } else { !ok_v57_b22 });
    take_0_22 |= live_v57_b22 & ok_v57_b22 & (if bd_v57_b22 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1838,
        c41: n1839,
        c358: n1930,
        c359: n1890,
        c282: n1840,
        c360: n1891,
        c361: n1966,
        c284: n1841,
        c285: n1842,
        c362: n1637,
        c287: n1736,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n2064,
        c371: n2074,
        c301: n1880,
        h1: n5467, h2: n5468,
    };
    // body 22: buttons 0x39, forks 0x0
    sink.o0(57, take_0_22, &sh0, &o0);
    declined |= live_v58_b23 & (if bd_v58_b23 { ALL } else { !ok_v58_b23 });
    take_0_23 |= live_v58_b23 & ok_v58_b23 & (if bd_v58_b23 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1838,
        c41: n1839,
        c358: n1930,
        c359: n1890,
        c282: n1840,
        c360: n1912,
        c361: n1966,
        c284: n1841,
        c285: n1842,
        c362: n1693,
        c287: n1736,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n2070,
        c371: n2076,
        c301: n1880,
        h1: n5476, h2: n5477,
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
        h1: n5493, h2: n5494,
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
        c20: n1838,
        c41: n1839,
        h1: n5497, h2: n5498,
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
        h1: n5508, h2: n5509,
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
        c20: n4025,
        c41: n4026,
        h1: n5514, h2: n5515,
    };
    // body 51: buttons 0x34, forks 0x0
    sink.o2(52, take_2_1, &sh2, &o2);
    declined |= live_v0_b52 & (if bd_v0_b52 { ALL } else { !ok_v0_b52 });
    take_3_0 |= live_v0_b52 & ok_v0_b52 & (if bd_v0_b52 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4211,
        c39: n4212,
        c20: r_c20,
        c38: n4207,
        h1: n5525, h2: n5526,
    };
    // body 52: buttons 0x00, forks 0x0
    sink.o3(0, take_3_0, &sh3, &o3);
    declined |= live_v1_b53 & (if bd_v1_b53 { ALL } else { !ok_v1_b53 });
    take_3_1 |= live_v1_b53 & ok_v1_b53 & (if bd_v1_b53 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4255,
        c39: n4256,
        c20: r_c20,
        c38: n4251,
        h1: n5534, h2: n5535,
    };
    // body 53: buttons 0x01, forks 0x0
    sink.o3(1, take_3_1, &sh3, &o3);
    declined |= live_v2_b54 & (if bd_v2_b54 { ALL } else { !ok_v2_b54 });
    take_3_2 |= live_v2_b54 & ok_v2_b54 & (if bd_v2_b54 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4299,
        c39: n4300,
        c20: r_c20,
        c38: n4295,
        h1: n5543, h2: n5544,
    };
    // body 54: buttons 0x02, forks 0x0
    sink.o3(2, take_3_2, &sh3, &o3);
    declined |= live_v16_b55 & (if bd_v16_b55 { ALL } else { !ok_v16_b55 });
    take_3_3 |= live_v16_b55 & ok_v16_b55 & (if bd_v16_b55 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4342,
        c39: n4343,
        c20: r_c20,
        c38: n4338,
        h1: n5552, h2: n5553,
    };
    // body 55: buttons 0x10, forks 0x0
    sink.o3(16, take_3_3, &sh3, &o3);
    declined |= live_v17_b56 & (if bd_v17_b56 { ALL } else { !ok_v17_b56 });
    take_3_4 |= live_v17_b56 & ok_v17_b56 & (if bd_v17_b56 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4385,
        c39: n4386,
        c20: r_c20,
        c38: n4381,
        h1: n5561, h2: n5562,
    };
    // body 56: buttons 0x11, forks 0x0
    sink.o3(17, take_3_4, &sh3, &o3);
    declined |= live_v18_b57 & (if bd_v18_b57 { ALL } else { !ok_v18_b57 });
    take_3_5 |= live_v18_b57 & ok_v18_b57 & (if bd_v18_b57 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4428,
        c39: n4429,
        c20: r_c20,
        c38: n4424,
        h1: n5570, h2: n5571,
    };
    // body 57: buttons 0x12, forks 0x0
    sink.o3(18, take_3_5, &sh3, &o3);
    declined |= live_v32_b58 & (if bd_v32_b58 { ALL } else { !ok_v32_b58 });
    take_3_6 |= live_v32_b58 & ok_v32_b58 & (if bd_v32_b58 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4458,
        c39: n4459,
        c20: n4025,
        c38: n4452,
        h1: n5581, h2: n5582,
    };
    // body 58: buttons 0x20, forks 0x0
    sink.o3(32, take_3_6, &sh3, &o3);
    declined |= live_v33_b59 & (if bd_v33_b59 { ALL } else { !ok_v33_b59 });
    take_3_7 |= live_v33_b59 & ok_v33_b59 & (if bd_v33_b59 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4476,
        c39: n4477,
        c20: n4025,
        c38: n4470,
        h1: n5590, h2: n5591,
    };
    // body 59: buttons 0x21, forks 0x0
    sink.o3(33, take_3_7, &sh3, &o3);
    declined |= live_v34_b60 & (if bd_v34_b60 { ALL } else { !ok_v34_b60 });
    take_3_8 |= live_v34_b60 & ok_v34_b60 & (if bd_v34_b60 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4494,
        c39: n4495,
        c20: n4025,
        c38: n4488,
        h1: n5599, h2: n5600,
    };
    // body 60: buttons 0x22, forks 0x0
    sink.o3(34, take_3_8, &sh3, &o3);
    declined |= live_v36_b61 & (if bd_v36_b61 { ALL } else { !ok_v36_b61 });
    take_3_9 |= live_v36_b61 & ok_v36_b61 & (if bd_v36_b61 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4510,
        c39: n4511,
        c20: n4025,
        c38: n4504,
        h1: n5608, h2: n5609,
    };
    // body 61: buttons 0x24, forks 0x0
    sink.o3(36, take_3_9, &sh3, &o3);
    declined |= live_v48_b62 & (if bd_v48_b62 { ALL } else { !ok_v48_b62 });
    take_3_10 |= live_v48_b62 & ok_v48_b62 & (if bd_v48_b62 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4540,
        c39: n4541,
        c20: n4025,
        c38: n4534,
        h1: n5617, h2: n5618,
    };
    // body 62: buttons 0x30, forks 0x0
    sink.o3(48, take_3_10, &sh3, &o3);
    declined |= live_v49_b63 & (if bd_v49_b63 { ALL } else { !ok_v49_b63 });
    take_3_11 |= live_v49_b63 & ok_v49_b63 & (if bd_v49_b63 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4558,
        c39: n4559,
        c20: n4025,
        c38: n4552,
        h1: n5626, h2: n5627,
    };
    // body 63: buttons 0x31, forks 0x0
    sink.o3(49, take_3_11, &sh3, &o3);
    declined |= live_v50_b64 & (if bd_v50_b64 { ALL } else { !ok_v50_b64 });
    take_3_12 |= live_v50_b64 & ok_v50_b64 & (if bd_v50_b64 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4576,
        c39: n4577,
        c20: n4025,
        c38: n4570,
        h1: n5635, h2: n5636,
    };
    // body 64: buttons 0x32, forks 0x0
    sink.o3(50, take_3_12, &sh3, &o3);
    declined |= live_v52_b65 & (if bd_v52_b65 { ALL } else { !ok_v52_b65 });
    take_3_13 |= live_v52_b65 & ok_v52_b65 & (if bd_v52_b65 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4592,
        c39: n4593,
        c20: n4025,
        c38: n4586,
        h1: n5644, h2: n5645,
    };
    // body 65: buttons 0x34, forks 0x0
    sink.o3(52, take_3_13, &sh3, &o3);
    declined |= live_v0_b66 & (if bd_v0_b66 { ALL } else { !ok_v0_b66 });
    take_4_0 |= live_v0_b66 & ok_v0_b66 & (if bd_v0_b66 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4666,
        c41: r_c41,
        c357: r_c357,
        c358: r_c358,
        c281: n4669,
        c359: r_c359,
        c360: r_c360,
        c283: n4670,
        c284: n4671,
        c361: n4675,
        c286: n4672,
        c293: n4596,
        c294: n4597,
        c369: n4702,
        c370: n4679,
        c300: n4701,
        h1: n5702, h2: n5703,
    };
    // body 66: buttons 0x00, forks 0x0
    sink.o4(0, take_4_0, &sh4, &o4);
    declined |= live_v1_b67 & (if bd_v1_b67 { ALL } else { !ok_v1_b67 });
    take_4_1 |= live_v1_b67 & ok_v1_b67 & (if bd_v1_b67 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4666,
        c41: r_c41,
        c357: r_c357,
        c358: r_c358,
        c281: n4669,
        c359: r_c359,
        c360: r_c360,
        c283: n4670,
        c284: n4671,
        c361: n4717,
        c286: n4672,
        c293: n4596,
        c294: n4597,
        c369: n4730,
        c370: n4719,
        c300: n4701,
        h1: n5711, h2: n5712,
    };
    // body 67: buttons 0x01, forks 0x0
    sink.o4(1, take_4_1, &sh4, &o4);
    declined |= live_v2_b68 & (if bd_v2_b68 { ALL } else { !ok_v2_b68 });
    take_4_2 |= live_v2_b68 & ok_v2_b68 & (if bd_v2_b68 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4666,
        c41: r_c41,
        c357: r_c357,
        c358: r_c358,
        c281: n4669,
        c359: r_c359,
        c360: r_c360,
        c283: n4670,
        c284: n4671,
        c361: n4744,
        c286: n4672,
        c293: n4596,
        c294: n4597,
        c369: n4757,
        c370: n4746,
        c300: n4701,
        h1: n5720, h2: n5721,
    };
    // body 68: buttons 0x02, forks 0x0
    sink.o4(2, take_4_2, &sh4, &o4);
    declined |= live_v16_b69 & (if bd_v16_b69 { ALL } else { !ok_v16_b69 });
    take_4_3 |= live_v16_b69 & ok_v16_b69 & (if bd_v16_b69 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4666,
        c41: r_c41,
        c357: r_c357,
        c358: r_c358,
        c281: n4669,
        c359: r_c359,
        c360: r_c360,
        c283: n4670,
        c284: n4671,
        c361: n4675,
        c286: n4767,
        c293: n4596,
        c294: n4759,
        c369: n4780,
        c370: n4769,
        c300: n4701,
        h1: n5746, h2: n5747,
    };
    // body 69: buttons 0x10, forks 0x0
    sink.o4(16, take_4_3, &sh4, &o4);
    declined |= live_v17_b70 & (if bd_v17_b70 { ALL } else { !ok_v17_b70 });
    take_4_4 |= live_v17_b70 & ok_v17_b70 & (if bd_v17_b70 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4666,
        c41: r_c41,
        c357: r_c357,
        c358: r_c358,
        c281: n4669,
        c359: r_c359,
        c360: r_c360,
        c283: n4670,
        c284: n4671,
        c361: n4717,
        c286: n4767,
        c293: n4596,
        c294: n4759,
        c369: n4799,
        c370: n4788,
        c300: n4701,
        h1: n5754, h2: n5755,
    };
    // body 70: buttons 0x11, forks 0x0
    sink.o4(17, take_4_4, &sh4, &o4);
    declined |= live_v18_b71 & (if bd_v18_b71 { ALL } else { !ok_v18_b71 });
    take_4_5 |= live_v18_b71 & ok_v18_b71 & (if bd_v18_b71 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4666,
        c41: r_c41,
        c357: r_c357,
        c358: r_c358,
        c281: n4669,
        c359: r_c359,
        c360: r_c360,
        c283: n4670,
        c284: n4671,
        c361: n4744,
        c286: n4767,
        c293: n4596,
        c294: n4759,
        c369: n4818,
        c370: n4807,
        c300: n4701,
        h1: n5762, h2: n5763,
    };
    // body 71: buttons 0x12, forks 0x0
    sink.o4(18, take_4_5, &sh4, &o4);
    declined |= live_v32_b72 & (if bd_v32_b72 { ALL } else { !ok_v32_b72 });
    take_4_6 |= live_v32_b72 & ok_v32_b72 & (if bd_v32_b72 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4840,
        c41: n4841,
        c357: n4845,
        c358: n4846,
        c281: n4842,
        c359: n4847,
        c360: n4848,
        c283: n4843,
        c284: n4844,
        c361: n4675,
        c286: n4672,
        c293: n4820,
        c294: n4597,
        c369: n4864,
        c370: n4850,
        c300: n4863,
        h1: n5807, h2: n5808,
    };
    // body 72: buttons 0x20, forks 0x0
    sink.o4(32, take_4_6, &sh4, &o4);
    declined |= live_v33_b73 & (if bd_v33_b73 { ALL } else { !ok_v33_b73 });
    take_4_7 |= live_v33_b73 & ok_v33_b73 & (if bd_v33_b73 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4840,
        c41: n4841,
        c357: n4845,
        c358: n4875,
        c281: n4842,
        c359: n4876,
        c360: n4848,
        c283: n4843,
        c284: n4844,
        c361: n4717,
        c286: n4672,
        c293: n4820,
        c294: n4597,
        c369: n4889,
        c370: n4878,
        c300: n4863,
        h1: n5823, h2: n5824,
    };
    // body 73: buttons 0x21, forks 0x0
    sink.o4(33, take_4_7, &sh4, &o4);
    declined |= live_v34_b74 & (if bd_v34_b74 { ALL } else { !ok_v34_b74 });
    take_4_8 |= live_v34_b74 & ok_v34_b74 & (if bd_v34_b74 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4840,
        c41: n4841,
        c357: n4845,
        c358: n4875,
        c281: n4842,
        c359: n4898,
        c360: n4848,
        c283: n4843,
        c284: n4844,
        c361: n4744,
        c286: n4672,
        c293: n4820,
        c294: n4597,
        c369: n4911,
        c370: n4900,
        c300: n4863,
        h1: n5836, h2: n5837,
    };
    // body 74: buttons 0x22, forks 0x0
    sink.o4(34, take_4_8, &sh4, &o4);
    declined |= live_v36_b75 & (if bd_v36_b75 { ALL } else { !ok_v36_b75 });
    take_4_9 |= live_v36_b75 & ok_v36_b75 & (if bd_v36_b75 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4840,
        c41: n4841,
        c357: n4926,
        c358: n4927,
        c281: n4842,
        c359: n4928,
        c360: n4929,
        c283: n4843,
        c284: n4844,
        c361: n4675,
        c286: n4672,
        c293: n4820,
        c294: n4597,
        c369: n4942,
        c370: n4931,
        c300: n4863,
        h1: n5856, h2: n5857,
    };
    // body 75: buttons 0x24, forks 0x0
    sink.o4(36, take_4_9, &sh4, &o4);
    declined |= live_v37_b76 & (if bd_v37_b76 { ALL } else { !ok_v37_b76 });
    take_4_10 |= live_v37_b76 & ok_v37_b76 & (if bd_v37_b76 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4840,
        c41: n4841,
        c357: n4926,
        c358: n4875,
        c281: n4842,
        c359: n4876,
        c360: n4929,
        c283: n4843,
        c284: n4844,
        c361: n4717,
        c286: n4672,
        c293: n4820,
        c294: n4597,
        c369: n4951,
        c370: n4949,
        c300: n4863,
        h1: n5870, h2: n5871,
    };
    // body 76: buttons 0x25, forks 0x0
    sink.o4(37, take_4_10, &sh4, &o4);
    declined |= live_v38_b77 & (if bd_v38_b77 { ALL } else { !ok_v38_b77 });
    take_4_11 |= live_v38_b77 & ok_v38_b77 & (if bd_v38_b77 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4840,
        c41: n4841,
        c357: n4926,
        c358: n4875,
        c281: n4842,
        c359: n4898,
        c360: n4929,
        c283: n4843,
        c284: n4844,
        c361: n4744,
        c286: n4672,
        c293: n4820,
        c294: n4597,
        c369: n4959,
        c370: n4957,
        c300: n4863,
        h1: n5882, h2: n5883,
    };
    // body 77: buttons 0x26, forks 0x0
    sink.o4(38, take_4_11, &sh4, &o4);
    declined |= live_v40_b78 & (if bd_v40_b78 { ALL } else { !ok_v40_b78 });
    take_4_12 |= live_v40_b78 & ok_v40_b78 & (if bd_v40_b78 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4840,
        c41: n4841,
        c357: n4926,
        c358: n4927,
        c281: n4842,
        c359: n4928,
        c360: n4964,
        c283: n4843,
        c284: n4844,
        c361: n4675,
        c286: n4672,
        c293: n4820,
        c294: n4597,
        c369: n4942,
        c370: n4965,
        c300: n4863,
        h1: n5892, h2: n5893,
    };
    // body 78: buttons 0x28, forks 0x0
    sink.o4(40, take_4_12, &sh4, &o4);
    declined |= live_v41_b79 & (if bd_v41_b79 { ALL } else { !ok_v41_b79 });
    take_4_13 |= live_v41_b79 & ok_v41_b79 & (if bd_v41_b79 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4840,
        c41: n4841,
        c357: n4926,
        c358: n4875,
        c281: n4842,
        c359: n4876,
        c360: n4964,
        c283: n4843,
        c284: n4844,
        c361: n4717,
        c286: n4672,
        c293: n4820,
        c294: n4597,
        c369: n4951,
        c370: n4968,
        c300: n4863,
        h1: n5901, h2: n5902,
    };
    // body 79: buttons 0x29, forks 0x0
    sink.o4(41, take_4_13, &sh4, &o4);
    declined |= live_v42_b80 & (if bd_v42_b80 { ALL } else { !ok_v42_b80 });
    take_4_14 |= live_v42_b80 & ok_v42_b80 & (if bd_v42_b80 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4840,
        c41: n4841,
        c357: n4926,
        c358: n4875,
        c281: n4842,
        c359: n4898,
        c360: n4964,
        c283: n4843,
        c284: n4844,
        c361: n4744,
        c286: n4672,
        c293: n4820,
        c294: n4597,
        c369: n4959,
        c370: n4971,
        c300: n4863,
        h1: n5910, h2: n5911,
    };
    // body 80: buttons 0x2a, forks 0x0
    sink.o4(42, take_4_14, &sh4, &o4);
    declined |= live_v48_b81 & (if bd_v48_b81 { ALL } else { !ok_v48_b81 });
    take_4_15 |= live_v48_b81 & ok_v48_b81 & (if bd_v48_b81 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4840,
        c41: n4841,
        c357: n4845,
        c358: n4846,
        c281: n4842,
        c359: n4847,
        c360: n4848,
        c283: n4843,
        c284: n4844,
        c361: n4675,
        c286: n4767,
        c293: n4820,
        c294: n4759,
        c369: n4989,
        c370: n4978,
        c300: n4863,
        h1: n5934, h2: n5935,
    };
    // body 81: buttons 0x30, forks 0x0
    sink.o4(48, take_4_15, &sh4, &o4);
    declined |= live_v49_b82 & (if bd_v49_b82 { ALL } else { !ok_v49_b82 });
    take_4_16 |= live_v49_b82 & ok_v49_b82 & (if bd_v49_b82 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4840,
        c41: n4841,
        c357: n4845,
        c358: n4875,
        c281: n4842,
        c359: n4876,
        c360: n4848,
        c283: n4843,
        c284: n4844,
        c361: n4717,
        c286: n4767,
        c293: n4820,
        c294: n4759,
        c369: n5008,
        c370: n4997,
        c300: n4863,
        h1: n5948, h2: n5949,
    };
    // body 82: buttons 0x31, forks 0x0
    sink.o4(49, take_4_16, &sh4, &o4);
    declined |= live_v50_b83 & (if bd_v50_b83 { ALL } else { !ok_v50_b83 });
    take_4_17 |= live_v50_b83 & ok_v50_b83 & (if bd_v50_b83 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4840,
        c41: n4841,
        c357: n4845,
        c358: n4875,
        c281: n4842,
        c359: n4898,
        c360: n4848,
        c283: n4843,
        c284: n4844,
        c361: n4744,
        c286: n4767,
        c293: n4820,
        c294: n4759,
        c369: n5027,
        c370: n5016,
        c300: n4863,
        h1: n5960, h2: n5961,
    };
    // body 83: buttons 0x32, forks 0x0
    sink.o4(50, take_4_17, &sh4, &o4);
    declined |= live_v52_b84 & (if bd_v52_b84 { ALL } else { !ok_v52_b84 });
    take_4_18 |= live_v52_b84 & ok_v52_b84 & (if bd_v52_b84 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4840,
        c41: n4841,
        c357: n4926,
        c358: n4927,
        c281: n4842,
        c359: n4928,
        c360: n4929,
        c283: n4843,
        c284: n4844,
        c361: n4675,
        c286: n4767,
        c293: n4820,
        c294: n4759,
        c369: n5046,
        c370: n5035,
        c300: n4863,
        h1: n5976, h2: n5977,
    };
    // body 84: buttons 0x34, forks 0x0
    sink.o4(52, take_4_18, &sh4, &o4);
    declined |= live_v53_b85 & (if bd_v53_b85 { ALL } else { !ok_v53_b85 });
    take_4_19 |= live_v53_b85 & ok_v53_b85 & (if bd_v53_b85 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4840,
        c41: n4841,
        c357: n4926,
        c358: n4875,
        c281: n4842,
        c359: n4876,
        c360: n4929,
        c283: n4843,
        c284: n4844,
        c361: n4717,
        c286: n4767,
        c293: n4820,
        c294: n4759,
        c369: n5055,
        c370: n5053,
        c300: n4863,
        h1: n5990, h2: n5991,
    };
    // body 85: buttons 0x35, forks 0x0
    sink.o4(53, take_4_19, &sh4, &o4);
    declined |= live_v54_b86 & (if bd_v54_b86 { ALL } else { !ok_v54_b86 });
    take_4_20 |= live_v54_b86 & ok_v54_b86 & (if bd_v54_b86 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4840,
        c41: n4841,
        c357: n4926,
        c358: n4875,
        c281: n4842,
        c359: n4898,
        c360: n4929,
        c283: n4843,
        c284: n4844,
        c361: n4744,
        c286: n4767,
        c293: n4820,
        c294: n4759,
        c369: n5063,
        c370: n5061,
        c300: n4863,
        h1: n6002, h2: n6003,
    };
    // body 86: buttons 0x36, forks 0x0
    sink.o4(54, take_4_20, &sh4, &o4);
    declined |= live_v56_b87 & (if bd_v56_b87 { ALL } else { !ok_v56_b87 });
    take_4_21 |= live_v56_b87 & ok_v56_b87 & (if bd_v56_b87 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4840,
        c41: n4841,
        c357: n4926,
        c358: n4927,
        c281: n4842,
        c359: n4928,
        c360: n4964,
        c283: n4843,
        c284: n4844,
        c361: n4675,
        c286: n4767,
        c293: n4820,
        c294: n4759,
        c369: n5046,
        c370: n5066,
        c300: n4863,
        h1: n6011, h2: n6012,
    };
    // body 87: buttons 0x38, forks 0x0
    sink.o4(56, take_4_21, &sh4, &o4);
    declined |= live_v57_b88 & (if bd_v57_b88 { ALL } else { !ok_v57_b88 });
    take_4_22 |= live_v57_b88 & ok_v57_b88 & (if bd_v57_b88 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4840,
        c41: n4841,
        c357: n4926,
        c358: n4875,
        c281: n4842,
        c359: n4876,
        c360: n4964,
        c283: n4843,
        c284: n4844,
        c361: n4717,
        c286: n4767,
        c293: n4820,
        c294: n4759,
        c369: n5055,
        c370: n5069,
        c300: n4863,
        h1: n6020, h2: n6021,
    };
    // body 88: buttons 0x39, forks 0x0
    sink.o4(57, take_4_22, &sh4, &o4);
    declined |= live_v58_b89 & (if bd_v58_b89 { ALL } else { !ok_v58_b89 });
    take_4_23 |= live_v58_b89 & ok_v58_b89 & (if bd_v58_b89 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n4840,
        c41: n4841,
        c357: n4926,
        c358: n4875,
        c281: n4842,
        c359: n4898,
        c360: n4964,
        c283: n4843,
        c284: n4844,
        c361: n4744,
        c286: n4767,
        c293: n4820,
        c294: n4759,
        c369: n5063,
        c370: n5072,
        c300: n4863,
        h1: n6029, h2: n6030,
    };
    // body 89: buttons 0x3a, forks 0x0
    sink.o4(58, take_4_23, &sh4, &o4);
    declined
}
