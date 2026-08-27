// GENERATED from a TRACED frame (shape 5). Do not edit.
//
// One input shape, 5 output shapes, 54 distinct button
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

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_0: u64 = 9886934607507694323;
pub const KPART2_0: u64 = 14605166486962040740;

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

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_1: u64 = 4306842259935538591;
pub const KPART2_1: u64 = 9317244076476002296;

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

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_2: u64 = 11249491028751283876;
pub const KPART2_2: u64 = 11361579102548976869;

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
        if let Col::N(v) = &mut acc.cols[240] { v.push(sh.c240.lane(i)); }
        if let Col::N(v) = &mut acc.cols[253] { v.push(sh.c253.lane(i)); }
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

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_3: u64 = 2180868251631278096;
pub const KPART2_3: u64 = 15589330933981481702;

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

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_4: u64 = 11562517508697409072;
pub const KPART2_4: u64 = 13858899965377538456;

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
        if celeste_engine::runtime2::key_check() { acc.row_keys.push(key); }
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
    let n240: ZB = zn_lt(n236, zn_splat(P8::from_raw(0i32)));
    let n241: ZN = zsel_n(n240, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n242: ZN = zsel_n(n239, zn_splat(P8::from_raw(65536i32)), n241);
    let n243: ZN = zn_abs(n236);
    let n244: ZN = zn_add(zn_splat(P8::from_raw(7274496i32)), n242);
    let n245: ZN = zn_add(zn_splat(P8::from_raw(7077888i32)), n242);
    let n246: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n243);
    let n247: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n245);
    let n248: ZN = zn_add(n242, n247);
    let n249: ZN = zn_add(n242, n245);
    let n250: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n243);
    let n251: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n249);
    let n252: ZN = zn_add(n242, n251);
    let n253: ZN = zn_add(n242, n249);
    let n254: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n243);
    let n255: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n253);
    let n256: ZN = zn_add(n242, n255);
    let n257: ZN = zn_add(n242, n253);
    let n258: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n243);
    let n259: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n257);
    let n260: ZN = zn_add(n242, n259);
    let n261: ZN = zn_add(n242, n257);
    let n262: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n243);
    let n263: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n261);
    let n264: ZN = zn_add(n242, n263);
    let n265: ZN = zn_add(n242, n261);
    let n266: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n243);
    let n267: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n265);
    let n268: ZN = zn_add(n242, n267);
    let n269: ZN = zn_add(n242, n265);
    let n270: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n243);
    let n271: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n269);
    let n272: ZN = zn_add(n242, n271);
    let n273: ZN = zn_add(n242, n269);
    let n274: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n243);
    let n275: ZB = zn_gt(r_c286, zn_splat(P8::from_raw(0i32)));
    let n276: ZN = zn_sub(r_c286, zn_splat(P8::from_raw(65536i32)));
    let n277: ZN = zsel_n(n275, n276, r_c286);
    let n278: ZN = zn_sub(r_c281, zn_splat(P8::from_raw(65536i32)));
    let n279: ZB = zn_gt(r_c283, zn_splat(P8::from_raw(0i32)));
    let n280: ZN = zn_sub(r_c283, zn_splat(P8::from_raw(65536i32)));
    let n281: ZN = zsel_n(n279, n280, r_c283);
    let n282: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n283: ZN = zsel_n(n227, zn_splat(P8::from_raw(655360i32)), r_c241);
    let n284: ZN = zsel_n(n227, zn_splat(P8::from_raw(1245184i32)), r_c254);
    let n285: ZN = zsel_n(n226, n283, r_c241);
    let n286: ZN = zsel_n(n226, n284, r_c254);
    let n287: ZN = zsel_n(n226, n231, r_c301);
    let n288: ZN = zsel_n(n226, n232, r_c369);
    let n289: ZN = zsel_n(n226, n233, r_c370);
    let n290: ZN = zsel_n(n201, n180, n285);
    let n291: ZN = zsel_n(n201, n203, n286);
    let n292: ZN = zsel_n(n201, r_c301, n287);
    let n293: ZN = zsel_n(n201, r_c369, n288);
    let n294: ZN = zsel_n(n201, r_c370, n289);
    let n295: ZB = zb_and(n134, n229);
    let n296: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n292);
    let n297: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n296);
    let n298: ZB = zn_gt(n297, zn_splat(P8::from_raw(7340032i32)));
    let n299: ZB = zb_and(n229, n298);
    let n300: ZB = zb_and(n230, n299);
    let n301: ZB = zn_lt(n296, zn_splat(P8::from_raw(7864320i32)));
    let n302: ZB = zb_and(n300, n301);
    let n303: ZB = zb_and(n134, n302);
    let n304: ZB = zn_ge(n294, zn_splat(P8::from_raw(0i32)));
    let n305: ZN = zn_mul(n293, zn_splat(P8::from_raw(13107i32)));
    let n306: ZB = zb_and(n230, n295);
    let n307: ZB = zb_and(n298, n306);
    let n308: ZB = zb_and(n301, n307);
    let n309: ZB = zb_and(n304, n308);
    let n310: ZN = zn_add(r_c367, n305);
    let n311: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n310);
    let n312: ZN = zn_flr(n311);
    let n313: ZN = zn_sub(n311, zn_splat(P8::from_raw(32768i32)));
    let n314: ZN = zn_sub(n313, n312);
    let n315: ZB = zn_gt(n312, zn_splat(P8::from_raw(0i32)));
    let n316: ZB = zn_lt(n312, zn_splat(P8::from_raw(0i32)));
    let n317: ZN = zsel_n(n316, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n318: ZN = zsel_n(n315, zn_splat(P8::from_raw(65536i32)), n317);
    let n319: ZN = zn_abs(n312);
    let n320: ZN = zn_add(n216, n318);
    let n321: ZB = zn_tile_flag_at(g.cache, g.cart, n320, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n322: ZN = zn_add(r_c300, n318);
    let n323: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n319);
    let n324: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n322);
    let n325: ZN = zn_add(n318, n324);
    let n326: ZB = zn_tile_flag_at(g.cache, g.cart, n325, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n327: ZN = zn_add(n318, n322);
    let n328: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n319);
    let n329: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n327);
    let n330: ZN = zn_add(n318, n329);
    let n331: ZB = zn_tile_flag_at(g.cache, g.cart, n330, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n332: ZN = zn_add(n318, n327);
    let n333: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n319);
    let n334: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n332);
    let n335: ZN = zn_add(n318, n334);
    let n336: ZB = zn_tile_flag_at(g.cache, g.cart, n335, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n337: ZN = zn_add(n318, n332);
    let n338: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n319);
    let n339: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n337);
    let n340: ZN = zn_add(n318, n339);
    let n341: ZB = zn_tile_flag_at(g.cache, g.cart, n340, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n342: ZN = zn_add(n318, n337);
    let n343: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n319);
    let n344: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n342);
    let n345: ZN = zn_add(n318, n344);
    let n346: ZB = zn_tile_flag_at(g.cache, g.cart, n345, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n347: ZN = zn_add(n318, n342);
    let n348: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n319);
    let n349: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n347);
    let n350: ZN = zn_add(n318, n349);
    let n351: ZB = zn_tile_flag_at(g.cache, g.cart, n350, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n352: ZN = zn_add(n318, n347);
    let n353: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n319);
    let n354: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n352);
    let n355: ZN = zn_add(n318, n354);
    let n356: ZB = zn_tile_flag_at(g.cache, g.cart, n355, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n357: ZN = zn_add(n318, n352);
    let n358: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n319);
    let n359: ZN = zsel_n(n356, n352, n357);
    let n360: ZN = zsel_n(n356, zn_splat(P8::from_raw(0i32)), n314);
    let n361: ZN = zsel_n(n356, zn_splat(P8::from_raw(0i32)), n305);
    let n362: ZB = zb_or(n356, n358);
    let n363: ZN = zsel_n(n353, n352, n359);
    let n364: ZN = zsel_n(n353, n314, n360);
    let n365: ZN = zsel_n(n353, n305, n361);
    let n366: ZB = zb_or(n353, n362);
    let n367: ZN = zsel_n(n351, n347, n363);
    let n368: ZN = zsel_n(n351, zn_splat(P8::from_raw(0i32)), n364);
    let n369: ZN = zsel_n(n351, zn_splat(P8::from_raw(0i32)), n365);
    let n370: ZB = zb_or(n351, n366);
    let n371: ZN = zsel_n(n348, n347, n367);
    let n372: ZN = zsel_n(n348, n314, n368);
    let n373: ZN = zsel_n(n348, n305, n369);
    let n374: ZB = zb_or(n348, n370);
    let n375: ZN = zsel_n(n346, n342, n371);
    let n376: ZN = zsel_n(n346, zn_splat(P8::from_raw(0i32)), n372);
    let n377: ZN = zsel_n(n346, zn_splat(P8::from_raw(0i32)), n373);
    let n378: ZB = zb_or(n346, n374);
    let n379: ZN = zsel_n(n343, n342, n375);
    let n380: ZN = zsel_n(n343, n314, n376);
    let n381: ZN = zsel_n(n343, n305, n377);
    let n382: ZB = zb_or(n343, n378);
    let n383: ZN = zsel_n(n341, n337, n379);
    let n384: ZN = zsel_n(n341, zn_splat(P8::from_raw(0i32)), n380);
    let n385: ZN = zsel_n(n341, zn_splat(P8::from_raw(0i32)), n381);
    let n386: ZB = zb_or(n341, n382);
    let n387: ZN = zsel_n(n338, n337, n383);
    let n388: ZN = zsel_n(n338, n314, n384);
    let n389: ZN = zsel_n(n338, n305, n385);
    let n390: ZB = zb_or(n338, n386);
    let n391: ZN = zsel_n(n336, n332, n387);
    let n392: ZN = zsel_n(n336, zn_splat(P8::from_raw(0i32)), n388);
    let n393: ZN = zsel_n(n336, zn_splat(P8::from_raw(0i32)), n389);
    let n394: ZB = zb_or(n336, n390);
    let n395: ZN = zsel_n(n333, n332, n391);
    let n396: ZN = zsel_n(n333, n314, n392);
    let n397: ZN = zsel_n(n333, n305, n393);
    let n398: ZB = zb_or(n333, n394);
    let n399: ZN = zsel_n(n331, n327, n395);
    let n400: ZN = zsel_n(n331, zn_splat(P8::from_raw(0i32)), n396);
    let n401: ZN = zsel_n(n331, zn_splat(P8::from_raw(0i32)), n397);
    let n402: ZB = zb_or(n331, n398);
    let n403: ZN = zsel_n(n328, n327, n399);
    let n404: ZN = zsel_n(n328, n314, n400);
    let n405: ZN = zsel_n(n328, n305, n401);
    let n406: ZB = zb_or(n328, n402);
    let n407: ZN = zsel_n(n326, n322, n403);
    let n408: ZN = zsel_n(n326, zn_splat(P8::from_raw(0i32)), n404);
    let n409: ZN = zsel_n(n326, zn_splat(P8::from_raw(0i32)), n405);
    let n410: ZB = zb_or(n326, n406);
    let n411: ZN = zsel_n(n323, n322, n407);
    let n412: ZN = zsel_n(n323, n314, n408);
    let n413: ZN = zsel_n(n323, n305, n409);
    let n414: ZB = zb_or(n323, n410);
    let n415: ZN = zsel_n(n321, r_c300, n411);
    let n416: ZN = zsel_n(n321, zn_splat(P8::from_raw(0i32)), n412);
    let n417: ZN = zsel_n(n321, zn_splat(P8::from_raw(0i32)), n413);
    let n418: ZB = zb_or(n321, n414);
    let n419: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n415);
    let n420: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n419);
    let n421: ZB = zn_tile_flag_at(g.cache, g.cart, n420, n244, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n422: ZB = zn_tile_flag_at(g.cache, g.cart, n420, n248, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n423: ZB = zn_tile_flag_at(g.cache, g.cart, n420, n252, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n424: ZB = zn_tile_flag_at(g.cache, g.cart, n420, n256, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n425: ZB = zn_tile_flag_at(g.cache, g.cart, n420, n260, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n426: ZB = zn_tile_flag_at(g.cache, g.cart, n420, n264, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n427: ZB = zn_tile_flag_at(g.cache, g.cart, n420, n268, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n428: ZB = zn_tile_flag_at(g.cache, g.cart, n420, n272, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n429: ZB = zb_and(n274, n418);
    let n430: ZN = zsel_n(n428, n269, n273);
    let n431: ZN = zsel_n(n428, zn_splat(P8::from_raw(0i32)), n238);
    let n432: ZN = zsel_n(n428, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(-196608i32)));
    let n433: ZB = zsel_b(n428, n418, n429);
    let n434: ZN = zsel_n(n270, n269, n430);
    let n435: ZN = zsel_n(n270, n238, n431);
    let n436: ZN = zsel_n(n270, zn_splat(P8::from_raw(-196608i32)), n432);
    let n437: ZB = zsel_b(n270, n418, n433);
    let n438: ZN = zsel_n(n427, n265, n434);
    let n439: ZN = zsel_n(n427, zn_splat(P8::from_raw(0i32)), n435);
    let n440: ZN = zsel_n(n427, zn_splat(P8::from_raw(0i32)), n436);
    let n441: ZB = zsel_b(n427, n418, n437);
    let n442: ZN = zsel_n(n266, n265, n438);
    let n443: ZN = zsel_n(n266, n238, n439);
    let n444: ZN = zsel_n(n266, zn_splat(P8::from_raw(-196608i32)), n440);
    let n445: ZB = zsel_b(n266, n418, n441);
    let n446: ZN = zsel_n(n426, n261, n442);
    let n447: ZN = zsel_n(n426, zn_splat(P8::from_raw(0i32)), n443);
    let n448: ZN = zsel_n(n426, zn_splat(P8::from_raw(0i32)), n444);
    let n449: ZB = zsel_b(n426, n418, n445);
    let n450: ZN = zsel_n(n262, n261, n446);
    let n451: ZN = zsel_n(n262, n238, n447);
    let n452: ZN = zsel_n(n262, zn_splat(P8::from_raw(-196608i32)), n448);
    let n453: ZB = zsel_b(n262, n418, n449);
    let n454: ZN = zsel_n(n425, n257, n450);
    let n455: ZN = zsel_n(n425, zn_splat(P8::from_raw(0i32)), n451);
    let n456: ZN = zsel_n(n425, zn_splat(P8::from_raw(0i32)), n452);
    let n457: ZB = zsel_b(n425, n418, n453);
    let n458: ZN = zsel_n(n258, n257, n454);
    let n459: ZN = zsel_n(n258, n238, n455);
    let n460: ZN = zsel_n(n258, zn_splat(P8::from_raw(-196608i32)), n456);
    let n461: ZB = zsel_b(n258, n418, n457);
    let n462: ZN = zsel_n(n424, n253, n458);
    let n463: ZN = zsel_n(n424, zn_splat(P8::from_raw(0i32)), n459);
    let n464: ZN = zsel_n(n424, zn_splat(P8::from_raw(0i32)), n460);
    let n465: ZB = zsel_b(n424, n418, n461);
    let n466: ZN = zsel_n(n254, n253, n462);
    let n467: ZN = zsel_n(n254, n238, n463);
    let n468: ZN = zsel_n(n254, zn_splat(P8::from_raw(-196608i32)), n464);
    let n469: ZB = zsel_b(n254, n418, n465);
    let n470: ZN = zsel_n(n423, n249, n466);
    let n471: ZN = zsel_n(n423, zn_splat(P8::from_raw(0i32)), n467);
    let n472: ZN = zsel_n(n423, zn_splat(P8::from_raw(0i32)), n468);
    let n473: ZB = zsel_b(n423, n418, n469);
    let n474: ZN = zsel_n(n250, n249, n470);
    let n475: ZN = zsel_n(n250, n238, n471);
    let n476: ZN = zsel_n(n250, zn_splat(P8::from_raw(-196608i32)), n472);
    let n477: ZB = zsel_b(n250, n418, n473);
    let n478: ZN = zsel_n(n422, n245, n474);
    let n479: ZN = zsel_n(n422, zn_splat(P8::from_raw(0i32)), n475);
    let n480: ZN = zsel_n(n422, zn_splat(P8::from_raw(0i32)), n476);
    let n481: ZB = zsel_b(n422, n418, n477);
    let n482: ZN = zsel_n(n246, n245, n478);
    let n483: ZN = zsel_n(n246, n238, n479);
    let n484: ZN = zsel_n(n246, zn_splat(P8::from_raw(-196608i32)), n480);
    let n485: ZB = zsel_b(n246, n418, n481);
    let n486: ZN = zsel_n(n421, zn_splat(P8::from_raw(7077888i32)), n482);
    let n487: ZN = zsel_n(n421, zn_splat(P8::from_raw(0i32)), n483);
    let n488: ZN = zsel_n(n421, zn_splat(P8::from_raw(0i32)), n484);
    let n489: ZB = zsel_b(n421, n418, n485);
    let n490: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n486);
    let n491: ZN = zn_div(n419, zn_splat(P8::from_raw(524288i32)));
    let n492: ZN = zn_flr(n491);
    let n493: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n492);
    let n494: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n419);
    let n495: ZN = zn_sub(n494, zn_splat(P8::from_raw(65536i32)));
    let n496: ZN = zn_div(n495, zn_splat(P8::from_raw(524288i32)));
    let n497: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n496);
    let n498: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n493);
    let n499: ZB = zn_le(n498, n497);
    let n500: ZB = zn_gt(n498, n497);
    let n501: ZB = zb_and(n309, n499);
    let n502: ZB = zb_and(n309, n500);
    let n503: ZN = zn_div(n490, zn_splat(P8::from_raw(524288i32)));
    let n504: ZN = zn_flr(n503);
    let n505: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n504);
    let n506: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n490);
    let n507: ZN = zn_sub(n506, zn_splat(P8::from_raw(65536i32)));
    let n508: ZN = zn_div(n507, zn_splat(P8::from_raw(524288i32)));
    let n509: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n508);
    let n510: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n505);
    let n511: ZB = zn_le(n510, n509);
    let n512: ZB = zn_gt(n510, n509);
    let n513: ZB = zb_and(n501, n511);
    let n514: ZB = zb_and(n501, n512);
    let n515: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n498);
    let n516: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n510);
    let n517: ZN = zn_mget(g.cart, n515, n516);
    let n518: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n517);
    let n519: ZN = zn_rem(n507, zn_splat(P8::from_raw(524288i32)));
    let n520: ZB = zn_ge(n519, zn_splat(P8::from_raw(393216i32)));
    let n521: ZN = zn_mul(n510, zn_splat(P8::from_raw(524288i32)));
    let n522: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n521);
    let n523: ZB = zn_eq(n506, n522);
    let n524: ZB = zb_or(n520, n523);
    let n525: ZB = zb_and(n518, n524);
    let n526: ZB = zn_ge(n488, zn_splat(P8::from_raw(0i32)));
    let n527: ZB = zb_and(n525, n526);
    let n528: ZB = zb_not(n527);
    let n529: ZB = zb_and(n513, n528);
    let n530: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n517);
    let n531: ZN = zn_rem(n490, zn_splat(P8::from_raw(524288i32)));
    let n532: ZB = zn_le(n531, zn_splat(P8::from_raw(131072i32)));
    let n533: ZB = zb_and(n530, n532);
    let n534: ZB = zb_not(n533);
    let n535: ZB = zb_and(n529, n533);
    let n536: ZB = zb_and(n529, n534);
    let n537: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n517);
    let n538: ZN = zn_rem(n419, zn_splat(P8::from_raw(524288i32)));
    let n539: ZB = zn_le(n538, zn_splat(P8::from_raw(131072i32)));
    let n540: ZB = zb_and(n537, n539);
    let n541: ZB = zn_le(n417, zn_splat(P8::from_raw(0i32)));
    let n542: ZB = zb_and(n540, n541);
    let n543: ZB = zb_not(n542);
    let n544: ZB = zb_and(n536, n543);
    let n545: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n517);
    let n546: ZN = zn_rem(n495, zn_splat(P8::from_raw(524288i32)));
    let n547: ZB = zn_ge(n546, zn_splat(P8::from_raw(393216i32)));
    let n548: ZN = zn_mul(n498, zn_splat(P8::from_raw(524288i32)));
    let n549: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n548);
    let n550: ZB = zn_eq(n494, n549);
    let n551: ZB = zb_or(n547, n550);
    let n552: ZB = zb_and(n545, n551);
    let n553: ZB = zn_ge(n417, zn_splat(P8::from_raw(0i32)));
    let n554: ZB = zb_and(n552, n553);
    let n555: ZB = zb_not(n554);
    let n556: ZB = zb_and(n544, n555);
    let n557: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n505);
    let n558: ZB = zn_le(n557, n509);
    let n559: ZB = zn_gt(n557, n509);
    let n560: ZB = zb_and(n556, n558);
    let n561: ZB = zb_and(n556, n559);
    let n562: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n557);
    let n563: ZN = zn_mget(g.cart, n515, n562);
    let n564: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n563);
    let n565: ZN = zn_mul(n557, zn_splat(P8::from_raw(524288i32)));
    let n566: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n565);
    let n567: ZB = zn_eq(n506, n566);
    let n568: ZB = zb_or(n520, n567);
    let n569: ZB = zb_and(n564, n568);
    let n570: ZB = zb_and(n526, n569);
    let n571: ZB = zb_not(n570);
    let n572: ZB = zb_and(n560, n571);
    let n573: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n563);
    let n574: ZB = zb_and(n532, n573);
    let n575: ZB = zb_not(n574);
    let n576: ZB = zb_and(n572, n574);
    let n577: ZB = zb_and(n572, n575);
    let n578: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n563);
    let n579: ZB = zb_and(n539, n578);
    let n580: ZB = zb_and(n541, n579);
    let n581: ZB = zb_not(n580);
    let n582: ZB = zb_and(n577, n581);
    let n583: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n563);
    let n584: ZB = zb_and(n551, n583);
    let n585: ZB = zb_and(n553, n584);
    let n586: ZB = zb_not(n585);
    let n587: ZB = zb_and(n582, n586);
    let n588: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n505);
    let n589: ZB = zn_le(n588, n509);
    let n590: ZB = zn_gt(n588, n509);
    let n591: ZB = zb_and(n587, n589);
    let n592: ZB = zb_and(n587, n590);
    let n593: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n588);
    let n594: ZN = zn_mget(g.cart, n515, n593);
    let n595: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n594);
    let n596: ZN = zn_mul(n588, zn_splat(P8::from_raw(524288i32)));
    let n597: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n596);
    let n598: ZB = zn_eq(n506, n597);
    let n599: ZB = zb_or(n520, n598);
    let n600: ZB = zb_and(n595, n599);
    let n601: ZB = zb_and(n526, n600);
    let n602: ZB = zb_not(n601);
    let n603: ZB = zb_and(n591, n602);
    let n604: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n594);
    let n605: ZB = zb_and(n532, n604);
    let n606: ZB = zb_not(n605);
    let n607: ZB = zb_and(n603, n605);
    let n608: ZB = zb_and(n603, n606);
    let n609: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n594);
    let n610: ZB = zb_and(n539, n609);
    let n611: ZB = zb_and(n541, n610);
    let n612: ZB = zb_not(n611);
    let n613: ZB = zb_and(n608, n612);
    let n614: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n594);
    let n615: ZB = zb_and(n551, n614);
    let n616: ZB = zb_and(n553, n615);
    let n617: ZB = zb_not(n616);
    let n618: ZB = zb_and(n613, n617);
    let n619: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n505);
    let n620: ZB = zn_gt(n619, n509);
    let n621: ZB = zb_and(n489, n620);
    let n622: ZB = zb_or(n592, n618);
    let n623: ZB = zsel_b(n590, n489, n621);
    let n624: ZB = zb_or(n561, n622);
    let n625: ZB = zsel_b(n559, n489, n623);
    let n626: ZB = zb_or(n514, n624);
    let n627: ZB = zsel_b(n512, n489, n625);
    let n628: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n493);
    let n629: ZB = zn_le(n628, n497);
    let n630: ZB = zn_gt(n628, n497);
    let n631: ZB = zb_and(n626, n629);
    let n632: ZB = zb_and(n626, n630);
    let n633: ZB = zb_and(n512, n631);
    let n634: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n628);
    let n635: ZN = zn_mget(g.cart, n634, n516);
    let n636: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n635);
    let n637: ZB = zb_and(n511, n626);
    let n638: ZB = zb_and(n629, n637);
    let n639: ZB = zb_and(n524, n636);
    let n640: ZB = zb_and(n526, n639);
    let n641: ZB = zb_not(n640);
    let n642: ZB = zb_and(n638, n641);
    let n643: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n635);
    let n644: ZB = zb_and(n532, n643);
    let n645: ZB = zb_not(n644);
    let n646: ZB = zb_and(n642, n644);
    let n647: ZB = zb_and(n642, n645);
    let n648: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n635);
    let n649: ZB = zb_and(n539, n648);
    let n650: ZB = zb_and(n541, n649);
    let n651: ZB = zb_not(n650);
    let n652: ZB = zb_and(n647, n651);
    let n653: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n635);
    let n654: ZN = zn_mul(n628, zn_splat(P8::from_raw(524288i32)));
    let n655: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n654);
    let n656: ZB = zn_eq(n494, n655);
    let n657: ZB = zb_or(n547, n656);
    let n658: ZB = zb_and(n653, n657);
    let n659: ZB = zb_and(n553, n658);
    let n660: ZB = zb_not(n659);
    let n661: ZB = zb_and(n652, n660);
    let n662: ZB = zb_and(n559, n661);
    let n663: ZN = zn_mget(g.cart, n634, n562);
    let n664: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n663);
    let n665: ZB = zb_and(n558, n652);
    let n666: ZB = zb_and(n660, n665);
    let n667: ZB = zb_and(n568, n664);
    let n668: ZB = zb_and(n526, n667);
    let n669: ZB = zb_not(n668);
    let n670: ZB = zb_and(n666, n669);
    let n671: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n663);
    let n672: ZB = zb_and(n532, n671);
    let n673: ZB = zb_not(n672);
    let n674: ZB = zb_and(n670, n672);
    let n675: ZB = zb_and(n670, n673);
    let n676: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n663);
    let n677: ZB = zb_and(n539, n676);
    let n678: ZB = zb_and(n541, n677);
    let n679: ZB = zb_not(n678);
    let n680: ZB = zb_and(n675, n679);
    let n681: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n663);
    let n682: ZB = zb_and(n657, n681);
    let n683: ZB = zb_and(n553, n682);
    let n684: ZB = zb_not(n683);
    let n685: ZB = zb_and(n680, n684);
    let n686: ZB = zb_and(n590, n685);
    let n687: ZN = zn_mget(g.cart, n634, n593);
    let n688: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n687);
    let n689: ZB = zb_and(n589, n680);
    let n690: ZB = zb_and(n684, n689);
    let n691: ZB = zb_and(n599, n688);
    let n692: ZB = zb_and(n526, n691);
    let n693: ZB = zb_not(n692);
    let n694: ZB = zb_and(n690, n693);
    let n695: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n687);
    let n696: ZB = zb_and(n532, n695);
    let n697: ZB = zb_not(n696);
    let n698: ZB = zb_and(n694, n696);
    let n699: ZB = zb_and(n694, n697);
    let n700: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n687);
    let n701: ZB = zb_and(n539, n700);
    let n702: ZB = zb_and(n541, n701);
    let n703: ZB = zb_not(n702);
    let n704: ZB = zb_and(n699, n703);
    let n705: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n687);
    let n706: ZB = zb_and(n657, n705);
    let n707: ZB = zb_and(n553, n706);
    let n708: ZB = zb_not(n707);
    let n709: ZB = zb_and(n704, n708);
    let n710: ZB = zb_and(n620, n627);
    let n711: ZB = zb_or(n686, n709);
    let n712: ZB = zsel_b(n590, n627, n710);
    let n713: ZB = zb_or(n662, n711);
    let n714: ZB = zsel_b(n559, n627, n712);
    let n715: ZB = zb_or(n633, n713);
    let n716: ZB = zsel_b(n512, n627, n714);
    let n717: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n493);
    let n718: ZB = zn_le(n717, n497);
    let n719: ZB = zn_gt(n717, n497);
    let n720: ZB = zb_and(n715, n718);
    let n721: ZB = zb_and(n715, n719);
    let n722: ZB = zb_and(n512, n720);
    let n723: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n717);
    let n724: ZN = zn_mget(g.cart, n723, n516);
    let n725: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n724);
    let n726: ZB = zb_and(n511, n715);
    let n727: ZB = zb_and(n718, n726);
    let n728: ZB = zb_and(n524, n725);
    let n729: ZB = zb_and(n526, n728);
    let n730: ZB = zb_not(n729);
    let n731: ZB = zb_and(n727, n730);
    let n732: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n724);
    let n733: ZB = zb_and(n532, n732);
    let n734: ZB = zb_not(n733);
    let n735: ZB = zb_and(n731, n733);
    let n736: ZB = zb_and(n731, n734);
    let n737: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n724);
    let n738: ZB = zb_and(n539, n737);
    let n739: ZB = zb_and(n541, n738);
    let n740: ZB = zb_not(n739);
    let n741: ZB = zb_and(n736, n740);
    let n742: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n724);
    let n743: ZN = zn_mul(n717, zn_splat(P8::from_raw(524288i32)));
    let n744: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n743);
    let n745: ZB = zn_eq(n494, n744);
    let n746: ZB = zb_or(n547, n745);
    let n747: ZB = zb_and(n742, n746);
    let n748: ZB = zb_and(n553, n747);
    let n749: ZB = zb_not(n748);
    let n750: ZB = zb_and(n741, n749);
    let n751: ZB = zb_and(n559, n750);
    let n752: ZN = zn_mget(g.cart, n723, n562);
    let n753: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n752);
    let n754: ZB = zb_and(n558, n741);
    let n755: ZB = zb_and(n749, n754);
    let n756: ZB = zb_and(n568, n753);
    let n757: ZB = zb_and(n526, n756);
    let n758: ZB = zb_not(n757);
    let n759: ZB = zb_and(n755, n758);
    let n760: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n752);
    let n761: ZB = zb_and(n532, n760);
    let n762: ZB = zb_not(n761);
    let n763: ZB = zb_and(n759, n761);
    let n764: ZB = zb_and(n759, n762);
    let n765: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n752);
    let n766: ZB = zb_and(n539, n765);
    let n767: ZB = zb_and(n541, n766);
    let n768: ZB = zb_not(n767);
    let n769: ZB = zb_and(n764, n768);
    let n770: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n752);
    let n771: ZB = zb_and(n746, n770);
    let n772: ZB = zb_and(n553, n771);
    let n773: ZB = zb_not(n772);
    let n774: ZB = zb_and(n769, n773);
    let n775: ZB = zb_and(n590, n774);
    let n776: ZN = zn_mget(g.cart, n723, n593);
    let n777: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n776);
    let n778: ZB = zb_and(n589, n769);
    let n779: ZB = zb_and(n773, n778);
    let n780: ZB = zb_and(n599, n777);
    let n781: ZB = zb_and(n526, n780);
    let n782: ZB = zb_not(n781);
    let n783: ZB = zb_and(n779, n782);
    let n784: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n776);
    let n785: ZB = zb_and(n532, n784);
    let n786: ZB = zb_not(n785);
    let n787: ZB = zb_and(n783, n785);
    let n788: ZB = zb_and(n783, n786);
    let n789: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n776);
    let n790: ZB = zb_and(n539, n789);
    let n791: ZB = zb_and(n541, n790);
    let n792: ZB = zb_not(n791);
    let n793: ZB = zb_and(n788, n792);
    let n794: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n776);
    let n795: ZB = zb_and(n746, n794);
    let n796: ZB = zb_and(n553, n795);
    let n797: ZB = zb_not(n796);
    let n798: ZB = zb_and(n793, n797);
    let n799: ZB = zb_and(n620, n716);
    let n800: ZB = zb_or(n775, n798);
    let n801: ZB = zsel_b(n590, n716, n799);
    let n802: ZB = zb_or(n751, n800);
    let n803: ZB = zsel_b(n559, n716, n801);
    let n804: ZB = zb_or(n722, n802);
    let n805: ZB = zsel_b(n512, n716, n803);
    let n806: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n493);
    let n807: ZB = zn_gt(n806, n497);
    let n808: ZB = zb_and(n805, n807);
    let n809: ZB = zb_or(n721, n804);
    let n810: ZB = zsel_b(n719, n716, n808);
    let n811: ZB = zb_or(n632, n809);
    let n812: ZB = zsel_b(n630, n627, n810);
    let n813: ZB = zb_or(n502, n811);
    let n814: ZB = zsel_b(n500, n489, n812);
    let n815: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n490);
    let n816: ZB = zn_tile_flag_at(g.cache, g.cart, n420, n815, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n817: ZB = zb_not(n816);
    let n818: ZN = zsel_n(n816, zn_splat(P8::from_raw(393216i32)), n277);
    let n819: ZB = zn_gt(n417, r_c359);
    let n820: ZN = zn_sub(n417, r_c357);
    let n821: ZN = zn_max(r_c359, n820);
    let n822: ZN = zn_add(r_c357, n417);
    let n823: ZN = zn_min(r_c359, n822);
    let n824: ZN = zsel_n(n819, n821, n823);
    let n825: ZB = zn_gt(n488, r_c360);
    let n826: ZN = zn_sub(n488, r_c358);
    let n827: ZN = zn_max(r_c360, n826);
    let n828: ZN = zn_add(r_c358, n488);
    let n829: ZN = zn_min(r_c360, n828);
    let n830: ZN = zsel_n(n825, n827, n829);
    let n831: ZN = zsel_n(n817, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n832: ZN = zn_abs(n417);
    let n833: ZB = zn_gt(n832, zn_splat(P8::from_raw(65536i32)));
    let n834: ZB = zn_gt(n417, zn_splat(P8::from_raw(0i32)));
    let n835: ZB = zn_lt(n417, zn_splat(P8::from_raw(0i32)));
    let n836: ZB = zn_gt(n417, zn_splat(P8::from_raw(65536i32)));
    let n837: ZN = zn_sub(n417, zn_splat(P8::from_raw(9830i32)));
    let n838: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n837);
    let n839: ZN = zn_add(zn_splat(P8::from_raw(9830i32)), n417);
    let n840: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n839);
    let n841: ZB = zn_gt(n417, zn_splat(P8::from_raw(-65536i32)));
    let n842: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n837);
    let n843: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n839);
    let n844: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n837);
    let n845: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n839);
    let n846: ZN = zsel_n(n841, n842, n843);
    let n847: ZN = zsel_n(n834, n844, n845);
    let n848: ZN = zsel_n(n836, n838, n840);
    let n849: ZN = zsel_n(n835, n846, n847);
    let n850: ZN = zsel_n(n834, n848, n849);
    let n851: ZN = zn_sub(n417, n831);
    let n852: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n851);
    let n853: ZN = zn_add(n417, n831);
    let n854: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n853);
    let n855: ZN = zsel_n(n834, n852, n854);
    let n856: ZN = zsel_n(n833, n850, n855);
    let n857: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n856);
    let n858: ZB = zb_not(n857);
    let n859: ZB = zn_lt(n856, zn_splat(P8::from_raw(0i32)));
    let n860: ZB = zsel_b(n858, n859, r_c361);
    let n861: ZN = zn_abs(n488);
    let n862: ZB = zn_le(n861, zn_splat(P8::from_raw(9830i32)));
    let n863: ZN = zsel_n(n862, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(13762i32)));
    let n864: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n490);
    let n865: ZN = zn_add(n488, n863);
    let n866: ZN = zn_min(zn_splat(P8::from_raw(131072i32)), n865);
    let n867: ZN = zsel_n(n817, n866, n488);
    let n868: ZB = zn_gt(n818, zn_splat(P8::from_raw(0i32)));
    let n869: ZN = zn_add(zn_splat(P8::from_raw(-196608i32)), n419);
    let n870: ZB = zn_tile_flag_at(g.cache, g.cart, n869, n864, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n871: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n419);
    let n872: ZB = zn_tile_flag_at(g.cache, g.cart, n871, n864, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n873: ZN = zsel_n(n872, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n874: ZN = zsel_n(n870, zn_splat(P8::from_raw(-65536i32)), n873);
    let n875: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n874);
    let n876: ZB = zb_not(n875);
    let n877: ZN = zn_neg(n874);
    let n878: ZN = zn_mul(n877, zn_splat(P8::from_raw(131072i32)));
    let n879: ZN = zsel_n(n876, n878, n856);
    let n880: ZN = zsel_n(n876, zn_splat(P8::from_raw(-131072i32)), n867);
    let n881: ZN = zsel_n(n868, zn_splat(P8::from_raw(0i32)), n818);
    let n882: ZN = zsel_n(n868, n856, n879);
    let n883: ZN = zsel_n(n868, zn_splat(P8::from_raw(-131072i32)), n880);
    let n884: ZN = zsel_n(n860, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n885: ZB = zn_gt(n884, zn_splat(P8::from_raw(0i32)));
    let n886: ZB = zn_lt(n884, zn_splat(P8::from_raw(0i32)));
    let n887: ZN = zsel_n(n886, zn_splat(P8::from_raw(-131072i32)), zn_splat(P8::from_raw(0i32)));
    let n888: ZN = zsel_n(n885, zn_splat(P8::from_raw(131072i32)), n887);
    let n889: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n884);
    let n890: ZB = zb_not(n889);
    let n891: ZN = zsel_n(n890, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n892: ZB = zsel_b(n279, r_c361, n860);
    let n893: ZN = zsel_n(n279, n824, n856);
    let n894: ZN = zsel_n(n279, n830, n867);
    let n896: ZB = zn_lt(n415, zn_splat(P8::from_raw(-65536i32)));
    let n897: ZB = zn_gt(n415, zn_splat(P8::from_raw(7929856i32)));
    let n898: ZB = zb_or(n896, n897);
    let n899: ZN = zn_min(zn_splat(P8::from_raw(7929856i32)), n415);
    let n900: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n899);
    let n901: ZN = zsel_n(n898, n900, n415);
    let n902: ZN = zsel_n(n898, zn_splat(P8::from_raw(0i32)), n893);
    let n903: ZN = zsel_n(n282, n415, n901);
    let n904: ZN = zsel_n(n282, n893, n902);
    let n917: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n851);
    let n918: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n853);
    let n919: ZN = zsel_n(n841, n917, n918);
    let n920: ZN = zsel_n(n833, n850, n919);
    let n921: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n920);
    let n922: ZB = zb_not(n921);
    let n923: ZB = zn_lt(n920, zn_splat(P8::from_raw(0i32)));
    let n924: ZB = zsel_b(n922, n923, r_c361);
    let n925: ZN = zn_add(zn_splat(P8::from_raw(-65536i32)), n419);
    let n926: ZB = zn_tile_flag_at(g.cache, g.cart, n925, n864, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n927: ZN = zsel_n(n926, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n928: ZN = zn_min(n865, n927);
    let n929: ZN = zsel_n(n817, n928, n488);
    let n930: ZN = zsel_n(n876, n878, n920);
    let n931: ZN = zsel_n(n876, zn_splat(P8::from_raw(-131072i32)), n929);
    let n932: ZN = zsel_n(n868, n920, n930);
    let n933: ZN = zsel_n(n868, zn_splat(P8::from_raw(-131072i32)), n931);
    let n934: ZB = zsel_b(n279, r_c361, n924);
    let n935: ZN = zsel_n(n279, n824, n920);
    let n936: ZN = zsel_n(n279, n830, n929);
    let n937: ZN = zsel_n(n898, zn_splat(P8::from_raw(0i32)), n935);
    let n938: ZN = zsel_n(n282, n935, n937);
    let n939: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n851);
    let n940: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n853);
    let n941: ZN = zsel_n(n836, n939, n940);
    let n942: ZN = zsel_n(n833, n850, n941);
    let n943: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n942);
    let n944: ZB = zb_not(n943);
    let n945: ZB = zn_lt(n942, zn_splat(P8::from_raw(0i32)));
    let n946: ZB = zsel_b(n944, n945, r_c361);
    let n947: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n419);
    let n948: ZB = zn_tile_flag_at(g.cache, g.cart, n947, n864, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n949: ZN = zsel_n(n948, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n950: ZN = zn_min(n865, n949);
    let n951: ZN = zsel_n(n817, n950, n488);
    let n952: ZN = zsel_n(n876, n878, n942);
    let n953: ZN = zsel_n(n876, zn_splat(P8::from_raw(-131072i32)), n951);
    let n954: ZN = zsel_n(n868, n942, n952);
    let n955: ZN = zsel_n(n868, zn_splat(P8::from_raw(-131072i32)), n953);
    let n956: ZB = zsel_b(n279, r_c361, n946);
    let n957: ZN = zsel_n(n279, n824, n942);
    let n958: ZN = zsel_n(n279, n830, n951);
    let n959: ZN = zsel_n(n898, zn_splat(P8::from_raw(0i32)), n957);
    let n960: ZN = zsel_n(n282, n957, n959);
    let n961: ZN = zsel_n(n140, n881, n818);
    let n962: ZN = zsel_n(n140, n882, n856);
    let n963: ZN = zsel_n(n140, n883, n867);
    let n964: ZN = zsel_n(n279, n818, n961);
    let n965: ZN = zsel_n(n279, n824, n962);
    let n966: ZN = zsel_n(n279, n830, n963);
    let n967: ZN = zsel_n(n898, zn_splat(P8::from_raw(0i32)), n965);
    let n968: ZN = zsel_n(n282, n965, n967);
    let n969: ZN = zsel_n(n140, n932, n920);
    let n970: ZN = zsel_n(n140, n933, n929);
    let n971: ZN = zsel_n(n279, n824, n969);
    let n972: ZN = zsel_n(n279, n830, n970);
    let n973: ZN = zsel_n(n898, zn_splat(P8::from_raw(0i32)), n971);
    let n974: ZN = zsel_n(n282, n971, n973);
    let n975: ZN = zsel_n(n140, n954, n942);
    let n976: ZN = zsel_n(n140, n955, n951);
    let n977: ZN = zsel_n(n279, n824, n975);
    let n978: ZN = zsel_n(n279, n830, n976);
    let n979: ZN = zsel_n(n898, zn_splat(P8::from_raw(0i32)), n977);
    let n980: ZN = zsel_n(n282, n977, n979);
    let n981: ZN = zsel_n(n133, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n982: ZB = zb_or(r_c41, n133);
    let n983: ZN = zsel_n(n133, zn_splat(P8::from_raw(655360i32)), n278);
    let n984: ZN = zsel_n(n133, zn_splat(P8::from_raw(262144i32)), r_c283);
    let n985: ZN = zsel_n(n133, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n986: ZN = zsel_n(n133, zn_splat(P8::from_raw(98304i32)), r_c357);
    let n987: ZN = zsel_n(n133, zn_splat(P8::from_raw(0i32)), r_c360);
    let n988: ZN = zsel_n(n279, r_c20, n981);
    let n989: ZB = zsel_b(n279, r_c41, n982);
    let n990: ZN = zsel_n(n279, n278, n983);
    let n991: ZN = zsel_n(n279, n280, n984);
    let n992: ZN = zsel_n(n279, zn_splat(P8::from_raw(65536i32)), n985);
    let n993: ZN = zsel_n(n279, r_c357, n986);
    let n994: ZN = zsel_n(n279, r_c360, n987);
    let n995: ZB = zn_gt(n988, zn_splat(P8::from_raw(0i32)));
    let n996: ZN = zsel_n(n133, n891, r_c358);
    let n997: ZN = zsel_n(n133, n888, r_c359);
    let n998: ZN = zsel_n(n133, n884, n856);
    let n999: ZN = zsel_n(n133, zn_splat(P8::from_raw(0i32)), n867);
    let n1000: ZN = zsel_n(n279, r_c358, n996);
    let n1001: ZN = zsel_n(n279, r_c359, n997);
    let n1002: ZN = zsel_n(n279, n824, n998);
    let n1003: ZN = zsel_n(n279, n830, n999);
    let n1004: ZN = zsel_n(n898, zn_splat(P8::from_raw(0i32)), n1002);
    let n1005: ZN = zsel_n(n995, n415, n901);
    let n1006: ZN = zsel_n(n995, n1002, n1004);
    let n1007: ZN = zsel_n(n133, zn_splat(P8::from_raw(69510i32)), r_c358);
    let n1008: ZN = zsel_n(n133, zn_splat(P8::from_raw(-131072i32)), r_c359);
    let n1009: ZN = zsel_n(n133, zn_splat(P8::from_raw(-327680i32)), n920);
    let n1010: ZN = zsel_n(n133, zn_splat(P8::from_raw(0i32)), n929);
    let n1011: ZN = zsel_n(n279, r_c358, n1007);
    let n1012: ZN = zsel_n(n279, r_c359, n1008);
    let n1013: ZN = zsel_n(n279, n824, n1009);
    let n1014: ZN = zsel_n(n279, n830, n1010);
    let n1015: ZN = zsel_n(n898, zn_splat(P8::from_raw(0i32)), n1013);
    let n1016: ZN = zsel_n(n995, n1013, n1015);
    let n1017: ZN = zsel_n(n133, zn_splat(P8::from_raw(131072i32)), r_c359);
    let n1018: ZN = zsel_n(n133, zn_splat(P8::from_raw(327680i32)), n942);
    let n1019: ZN = zsel_n(n133, zn_splat(P8::from_raw(0i32)), n951);
    let n1020: ZN = zsel_n(n279, r_c359, n1017);
    let n1021: ZN = zsel_n(n279, n824, n1018);
    let n1022: ZN = zsel_n(n279, n830, n1019);
    let n1023: ZN = zsel_n(n898, zn_splat(P8::from_raw(0i32)), n1021);
    let n1024: ZN = zsel_n(n995, n1021, n1023);
    let n1026: ZN = zsel_n(n133, zn_splat(P8::from_raw(69510i32)), r_c357);
    let n1027: ZN = zsel_n(n133, zn_splat(P8::from_raw(-98304i32)), r_c360);
    let n1028: ZN = zsel_n(n279, r_c357, n1026);
    let n1029: ZN = zsel_n(n279, r_c360, n1027);
    let n1030: ZN = zsel_n(n133, zn_splat(P8::from_raw(98304i32)), r_c358);
    let n1031: ZN = zsel_n(n133, zn_splat(P8::from_raw(0i32)), r_c359);
    let n1032: ZN = zsel_n(n133, zn_splat(P8::from_raw(0i32)), n856);
    let n1033: ZN = zsel_n(n133, zn_splat(P8::from_raw(-327680i32)), n867);
    let n1034: ZN = zsel_n(n279, r_c358, n1030);
    let n1035: ZN = zsel_n(n279, r_c359, n1031);
    let n1036: ZN = zsel_n(n279, n824, n1032);
    let n1037: ZN = zsel_n(n279, n830, n1033);
    let n1038: ZN = zsel_n(n898, zn_splat(P8::from_raw(0i32)), n1036);
    let n1039: ZN = zsel_n(n995, n1036, n1038);
    let n1040: ZN = zsel_n(n133, zn_splat(P8::from_raw(-231700i32)), n920);
    let n1041: ZN = zsel_n(n133, zn_splat(P8::from_raw(-231700i32)), n929);
    let n1042: ZN = zsel_n(n279, n824, n1040);
    let n1043: ZN = zsel_n(n279, n830, n1041);
    let n1044: ZN = zsel_n(n898, zn_splat(P8::from_raw(0i32)), n1042);
    let n1045: ZN = zsel_n(n995, n1042, n1044);
    let n1046: ZN = zsel_n(n133, zn_splat(P8::from_raw(231700i32)), n942);
    let n1047: ZN = zsel_n(n133, zn_splat(P8::from_raw(-231700i32)), n951);
    let n1048: ZN = zsel_n(n279, n824, n1046);
    let n1049: ZN = zsel_n(n279, n830, n1047);
    let n1050: ZN = zsel_n(n898, zn_splat(P8::from_raw(0i32)), n1048);
    let n1051: ZN = zsel_n(n995, n1048, n1050);
    let n1052: ZN = zsel_n(n133, zn_splat(P8::from_raw(131072i32)), r_c360);
    let n1053: ZN = zsel_n(n279, r_c360, n1052);
    let n1054: ZN = zsel_n(n133, zn_splat(P8::from_raw(327680i32)), n867);
    let n1055: ZN = zsel_n(n279, n830, n1054);
    let n1056: ZN = zsel_n(n133, zn_splat(P8::from_raw(231700i32)), n929);
    let n1057: ZN = zsel_n(n279, n830, n1056);
    let n1058: ZN = zsel_n(n133, zn_splat(P8::from_raw(231700i32)), n951);
    let n1059: ZN = zsel_n(n279, n830, n1058);
    let n1060: ZN = zsel_n(n133, n884, n962);
    let n1061: ZN = zsel_n(n133, zn_splat(P8::from_raw(0i32)), n963);
    let n1062: ZN = zsel_n(n279, n824, n1060);
    let n1063: ZN = zsel_n(n279, n830, n1061);
    let n1064: ZN = zsel_n(n898, zn_splat(P8::from_raw(0i32)), n1062);
    let n1065: ZN = zsel_n(n995, n1062, n1064);
    let n1066: ZN = zsel_n(n133, zn_splat(P8::from_raw(-327680i32)), n969);
    let n1067: ZN = zsel_n(n133, zn_splat(P8::from_raw(0i32)), n970);
    let n1068: ZN = zsel_n(n279, n824, n1066);
    let n1069: ZN = zsel_n(n279, n830, n1067);
    let n1070: ZN = zsel_n(n898, zn_splat(P8::from_raw(0i32)), n1068);
    let n1071: ZN = zsel_n(n995, n1068, n1070);
    let n1072: ZN = zsel_n(n133, zn_splat(P8::from_raw(327680i32)), n975);
    let n1073: ZN = zsel_n(n133, zn_splat(P8::from_raw(0i32)), n976);
    let n1074: ZN = zsel_n(n279, n824, n1072);
    let n1075: ZN = zsel_n(n279, n830, n1073);
    let n1076: ZN = zsel_n(n898, zn_splat(P8::from_raw(0i32)), n1074);
    let n1077: ZN = zsel_n(n995, n1074, n1076);
    let n1078: ZN = zsel_n(n133, zn_splat(P8::from_raw(0i32)), n962);
    let n1079: ZN = zsel_n(n133, zn_splat(P8::from_raw(-327680i32)), n963);
    let n1080: ZN = zsel_n(n279, n824, n1078);
    let n1081: ZN = zsel_n(n279, n830, n1079);
    let n1082: ZN = zsel_n(n898, zn_splat(P8::from_raw(0i32)), n1080);
    let n1083: ZN = zsel_n(n995, n1080, n1082);
    let n1084: ZN = zsel_n(n133, zn_splat(P8::from_raw(-231700i32)), n969);
    let n1085: ZN = zsel_n(n133, zn_splat(P8::from_raw(-231700i32)), n970);
    let n1086: ZN = zsel_n(n279, n824, n1084);
    let n1087: ZN = zsel_n(n279, n830, n1085);
    let n1088: ZN = zsel_n(n898, zn_splat(P8::from_raw(0i32)), n1086);
    let n1089: ZN = zsel_n(n995, n1086, n1088);
    let n1090: ZN = zsel_n(n133, zn_splat(P8::from_raw(231700i32)), n975);
    let n1091: ZN = zsel_n(n133, zn_splat(P8::from_raw(-231700i32)), n976);
    let n1092: ZN = zsel_n(n279, n824, n1090);
    let n1093: ZN = zsel_n(n279, n830, n1091);
    let n1094: ZN = zsel_n(n898, zn_splat(P8::from_raw(0i32)), n1092);
    let n1095: ZN = zsel_n(n995, n1092, n1094);
    let n1096: ZN = zsel_n(n133, zn_splat(P8::from_raw(327680i32)), n963);
    let n1097: ZN = zsel_n(n279, n830, n1096);
    let n1098: ZN = zsel_n(n133, zn_splat(P8::from_raw(231700i32)), n970);
    let n1099: ZN = zsel_n(n279, n830, n1098);
    let n1100: ZN = zsel_n(n133, zn_splat(P8::from_raw(231700i32)), n976);
    let n1101: ZN = zsel_n(n279, n830, n1100);
    let n1102: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c87);
    let n1103: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1102);
    let n1104: ZB = zb_and(n513, n527);
    let n1105: ZB = zb_and(n536, n542);
    let n1106: ZB = zb_and(n544, n554);
    let n1107: ZB = zb_or(n1105, n1106);
    let n1108: ZB = zb_or(n535, n1107);
    let n1109: ZB = zb_or(n1104, n1108);
    let n1110: ZB = zb_and(n560, n570);
    let n1111: ZB = zb_and(n577, n580);
    let n1112: ZB = zb_and(n582, n585);
    let n1113: ZB = zb_or(n1111, n1112);
    let n1114: ZB = zb_or(n576, n1113);
    let n1115: ZB = zb_or(n1110, n1114);
    let n1116: ZB = zb_and(n591, n601);
    let n1117: ZB = zb_and(n608, n611);
    let n1118: ZB = zb_and(n613, n616);
    let n1119: ZB = zb_or(n1117, n1118);
    let n1120: ZB = zb_or(n607, n1119);
    let n1121: ZB = zb_or(n1116, n1120);
    let n1122: ZB = zb_or(n1115, n1121);
    let n1123: ZB = zb_or(n1109, n1122);
    let n1124: ZB = zb_and(n638, n640);
    let n1125: ZB = zb_and(n647, n650);
    let n1126: ZB = zb_and(n652, n659);
    let n1127: ZB = zb_or(n1125, n1126);
    let n1128: ZB = zb_or(n646, n1127);
    let n1129: ZB = zb_or(n1124, n1128);
    let n1130: ZB = zb_and(n666, n668);
    let n1131: ZB = zb_and(n675, n678);
    let n1132: ZB = zb_and(n680, n683);
    let n1133: ZB = zb_or(n1131, n1132);
    let n1134: ZB = zb_or(n674, n1133);
    let n1135: ZB = zb_or(n1130, n1134);
    let n1136: ZB = zb_and(n690, n692);
    let n1137: ZB = zb_and(n699, n702);
    let n1138: ZB = zb_and(n704, n707);
    let n1139: ZB = zb_or(n1137, n1138);
    let n1140: ZB = zb_or(n698, n1139);
    let n1141: ZB = zb_or(n1136, n1140);
    let n1142: ZB = zb_or(n1135, n1141);
    let n1143: ZB = zb_or(n1129, n1142);
    let n1144: ZB = zb_and(n727, n729);
    let n1145: ZB = zb_and(n736, n739);
    let n1146: ZB = zb_and(n741, n748);
    let n1147: ZB = zb_or(n1145, n1146);
    let n1148: ZB = zb_or(n735, n1147);
    let n1149: ZB = zb_or(n1144, n1148);
    let n1150: ZB = zb_and(n755, n757);
    let n1151: ZB = zb_and(n764, n767);
    let n1152: ZB = zb_and(n769, n772);
    let n1153: ZB = zb_or(n1151, n1152);
    let n1154: ZB = zb_or(n763, n1153);
    let n1155: ZB = zb_or(n1150, n1154);
    let n1156: ZB = zb_and(n779, n781);
    let n1157: ZB = zb_and(n788, n791);
    let n1158: ZB = zb_and(n793, n796);
    let n1159: ZB = zb_or(n1157, n1158);
    let n1160: ZB = zb_or(n787, n1159);
    let n1161: ZB = zb_or(n1156, n1160);
    let n1162: ZB = zb_or(n1155, n1161);
    let n1163: ZB = zb_or(n1149, n1162);
    let n1164: ZB = zb_or(n1143, n1163);
    let n1165: ZB = zsel_b(n1143, n627, n716);
    let n1166: ZB = zb_or(n1123, n1164);
    let n1167: ZB = zsel_b(n1123, n489, n1165);
    let n1168: ZB = zsel_b(n1166, n1167, n814);
    let n1171: ZN = zsel_n(n227, zn_splat(P8::from_raw(65536i32)), r_c284);
    let n1172: ZN = zsel_n(n226, n1171, r_c284);
    let n1173: ZN = zsel_n(n201, r_c284, n1172);
    let n1174: ZB = zb_not(n302);
    let n1175: ZB = zb_and(n134, n1174);
    let n1176: ZB = zn_lt(n294, zn_splat(P8::from_raw(0i32)));
    let n1177: ZB = zb_and(n303, n1176);
    let n1178: ZB = zb_or(n1175, n1177);
    let n1179: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n293);
    let n1180: ZB = zb_not(n1179);
    let n1181: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n294);
    let n1182: ZB = zb_not(n1181);
    let n1183: ZB = zb_or(n1180, n1182);
    let n1184: ZB = zb_not(n1183);
    let n1185: ZN = zn_add(r_c367, n293);
    let n1186: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n1185);
    let n1187: ZN = zn_flr(n1186);
    let n1188: ZB = zn_gt(n1187, zn_splat(P8::from_raw(0i32)));
    let n1189: ZB = zn_lt(n1187, zn_splat(P8::from_raw(0i32)));
    let n1190: ZN = zsel_n(n1189, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n1191: ZN = zsel_n(n1188, zn_splat(P8::from_raw(65536i32)), n1190);
    let n1192: ZN = zn_abs(n1187);
    let n1193: ZN = zn_add(n216, n1191);
    let n1194: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n296);
    let n1195: ZB = zn_tile_flag_at(g.cache, g.cart, n1193, n1194, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1196: ZN = zn_add(r_c300, n1191);
    let n1197: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n1192);
    let n1198: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1196);
    let n1199: ZN = zn_add(n1191, n1198);
    let n1200: ZB = zn_tile_flag_at(g.cache, g.cart, n1199, n1194, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1201: ZN = zn_add(n1191, n1196);
    let n1202: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n1192);
    let n1203: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1201);
    let n1204: ZN = zn_add(n1191, n1203);
    let n1205: ZB = zn_tile_flag_at(g.cache, g.cart, n1204, n1194, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1206: ZN = zn_add(n1191, n1201);
    let n1207: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n1192);
    let n1208: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1206);
    let n1209: ZN = zn_add(n1191, n1208);
    let n1210: ZB = zn_tile_flag_at(g.cache, g.cart, n1209, n1194, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1211: ZN = zn_add(n1191, n1206);
    let n1212: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n1192);
    let n1213: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1211);
    let n1214: ZN = zn_add(n1191, n1213);
    let n1215: ZB = zn_tile_flag_at(g.cache, g.cart, n1214, n1194, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1216: ZN = zn_add(n1191, n1211);
    let n1217: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n1192);
    let n1218: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1216);
    let n1219: ZN = zn_add(n1191, n1218);
    let n1220: ZB = zn_tile_flag_at(g.cache, g.cart, n1219, n1194, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1221: ZN = zn_add(n1191, n1216);
    let n1222: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n1192);
    let n1223: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1221);
    let n1224: ZN = zn_add(n1191, n1223);
    let n1225: ZB = zn_tile_flag_at(g.cache, g.cart, n1224, n1194, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1226: ZN = zn_add(n1191, n1221);
    let n1227: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n1192);
    let n1228: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1226);
    let n1229: ZN = zn_add(n1191, n1228);
    let n1230: ZB = zn_tile_flag_at(g.cache, g.cart, n1229, n1194, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1231: ZN = zn_add(n1191, n1226);
    let n1232: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n1192);
    let n1233: ZN = zsel_n(n1230, n1226, n1231);
    let n1234: ZN = zsel_n(n1230, zn_splat(P8::from_raw(0i32)), n293);
    let n1235: ZB = zb_or(n1230, n1232);
    let n1236: ZN = zsel_n(n1227, n1226, n1233);
    let n1237: ZN = zsel_n(n1227, n293, n1234);
    let n1238: ZB = zb_or(n1227, n1235);
    let n1239: ZN = zsel_n(n1225, n1221, n1236);
    let n1240: ZN = zsel_n(n1225, zn_splat(P8::from_raw(0i32)), n1237);
    let n1241: ZB = zb_or(n1225, n1238);
    let n1242: ZN = zsel_n(n1222, n1221, n1239);
    let n1243: ZN = zsel_n(n1222, n293, n1240);
    let n1244: ZB = zb_or(n1222, n1241);
    let n1245: ZN = zsel_n(n1220, n1216, n1242);
    let n1246: ZN = zsel_n(n1220, zn_splat(P8::from_raw(0i32)), n1243);
    let n1247: ZB = zb_or(n1220, n1244);
    let n1248: ZN = zsel_n(n1217, n1216, n1245);
    let n1249: ZN = zsel_n(n1217, n293, n1246);
    let n1250: ZB = zb_or(n1217, n1247);
    let n1251: ZN = zsel_n(n1215, n1211, n1248);
    let n1252: ZN = zsel_n(n1215, zn_splat(P8::from_raw(0i32)), n1249);
    let n1253: ZB = zb_or(n1215, n1250);
    let n1254: ZN = zsel_n(n1212, n1211, n1251);
    let n1255: ZN = zsel_n(n1212, n293, n1252);
    let n1256: ZB = zb_or(n1212, n1253);
    let n1257: ZN = zsel_n(n1210, n1206, n1254);
    let n1258: ZN = zsel_n(n1210, zn_splat(P8::from_raw(0i32)), n1255);
    let n1259: ZB = zb_or(n1210, n1256);
    let n1260: ZN = zsel_n(n1207, n1206, n1257);
    let n1261: ZN = zsel_n(n1207, n293, n1258);
    let n1262: ZB = zb_or(n1207, n1259);
    let n1263: ZN = zsel_n(n1205, n1201, n1260);
    let n1264: ZN = zsel_n(n1205, zn_splat(P8::from_raw(0i32)), n1261);
    let n1265: ZB = zb_or(n1205, n1262);
    let n1266: ZN = zsel_n(n1202, n1201, n1263);
    let n1267: ZN = zsel_n(n1202, n293, n1264);
    let n1268: ZB = zb_or(n1202, n1265);
    let n1269: ZN = zsel_n(n1200, n1196, n1266);
    let n1270: ZN = zsel_n(n1200, zn_splat(P8::from_raw(0i32)), n1267);
    let n1271: ZB = zb_or(n1200, n1268);
    let n1272: ZN = zsel_n(n1197, n1196, n1269);
    let n1273: ZN = zsel_n(n1197, n293, n1270);
    let n1274: ZB = zb_or(n1197, n1271);
    let n1275: ZN = zsel_n(n1195, r_c300, n1272);
    let n1276: ZN = zsel_n(n1195, zn_splat(P8::from_raw(0i32)), n1273);
    let n1277: ZB = zb_or(n1195, n1274);
    let n1278: ZN = zn_add(r_c368, n294);
    let n1279: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n1278);
    let n1280: ZN = zn_flr(n1279);
    let n1281: ZB = zn_gt(n1280, zn_splat(P8::from_raw(0i32)));
    let n1282: ZB = zn_lt(n1280, zn_splat(P8::from_raw(0i32)));
    let n1283: ZN = zsel_n(n1282, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n1284: ZN = zsel_n(n1281, zn_splat(P8::from_raw(65536i32)), n1283);
    let n1285: ZN = zn_abs(n1280);
    let n1286: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1275);
    let n1287: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1286);
    let n1288: ZN = zn_add(n296, n1284);
    let n1289: ZB = zn_tile_flag_at(g.cache, g.cart, n1287, n1288, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1290: ZN = zn_add(n292, n1284);
    let n1291: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n1285);
    let n1292: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1290);
    let n1293: ZN = zn_add(n1284, n1292);
    let n1294: ZB = zn_tile_flag_at(g.cache, g.cart, n1287, n1293, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1295: ZN = zn_add(n1284, n1290);
    let n1296: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n1285);
    let n1297: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1295);
    let n1298: ZN = zn_add(n1284, n1297);
    let n1299: ZB = zn_tile_flag_at(g.cache, g.cart, n1287, n1298, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1300: ZN = zn_add(n1284, n1295);
    let n1301: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n1285);
    let n1302: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1300);
    let n1303: ZN = zn_add(n1284, n1302);
    let n1304: ZB = zn_tile_flag_at(g.cache, g.cart, n1287, n1303, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1305: ZN = zn_add(n1284, n1300);
    let n1306: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n1285);
    let n1307: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1305);
    let n1308: ZN = zn_add(n1284, n1307);
    let n1309: ZB = zn_tile_flag_at(g.cache, g.cart, n1287, n1308, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1310: ZN = zn_add(n1284, n1305);
    let n1311: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n1285);
    let n1312: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1310);
    let n1313: ZN = zn_add(n1284, n1312);
    let n1314: ZB = zn_tile_flag_at(g.cache, g.cart, n1287, n1313, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1315: ZN = zn_add(n1284, n1310);
    let n1316: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n1285);
    let n1317: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1315);
    let n1318: ZN = zn_add(n1284, n1317);
    let n1319: ZB = zn_tile_flag_at(g.cache, g.cart, n1287, n1318, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1320: ZN = zn_add(n1284, n1315);
    let n1321: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n1285);
    let n1322: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1320);
    let n1323: ZN = zn_add(n1284, n1322);
    let n1324: ZB = zn_tile_flag_at(g.cache, g.cart, n1287, n1323, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1325: ZN = zn_add(n1284, n1320);
    let n1326: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n1285);
    let n1327: ZB = zb_and(n1277, n1326);
    let n1328: ZN = zsel_n(n1324, n1320, n1325);
    let n1329: ZN = zsel_n(n1324, zn_splat(P8::from_raw(0i32)), n294);
    let n1330: ZB = zsel_b(n1324, n1277, n1327);
    let n1331: ZN = zsel_n(n1321, n1320, n1328);
    let n1332: ZN = zsel_n(n1321, n294, n1329);
    let n1333: ZB = zsel_b(n1321, n1277, n1330);
    let n1334: ZN = zsel_n(n1319, n1315, n1331);
    let n1335: ZN = zsel_n(n1319, zn_splat(P8::from_raw(0i32)), n1332);
    let n1336: ZB = zsel_b(n1319, n1277, n1333);
    let n1337: ZN = zsel_n(n1316, n1315, n1334);
    let n1338: ZN = zsel_n(n1316, n294, n1335);
    let n1339: ZB = zsel_b(n1316, n1277, n1336);
    let n1340: ZN = zsel_n(n1314, n1310, n1337);
    let n1341: ZN = zsel_n(n1314, zn_splat(P8::from_raw(0i32)), n1338);
    let n1342: ZB = zsel_b(n1314, n1277, n1339);
    let n1343: ZN = zsel_n(n1311, n1310, n1340);
    let n1344: ZN = zsel_n(n1311, n294, n1341);
    let n1345: ZB = zsel_b(n1311, n1277, n1342);
    let n1346: ZN = zsel_n(n1309, n1305, n1343);
    let n1347: ZN = zsel_n(n1309, zn_splat(P8::from_raw(0i32)), n1344);
    let n1348: ZB = zsel_b(n1309, n1277, n1345);
    let n1349: ZN = zsel_n(n1306, n1305, n1346);
    let n1350: ZN = zsel_n(n1306, n294, n1347);
    let n1351: ZB = zsel_b(n1306, n1277, n1348);
    let n1352: ZN = zsel_n(n1304, n1300, n1349);
    let n1353: ZN = zsel_n(n1304, zn_splat(P8::from_raw(0i32)), n1350);
    let n1354: ZB = zsel_b(n1304, n1277, n1351);
    let n1355: ZN = zsel_n(n1301, n1300, n1352);
    let n1356: ZN = zsel_n(n1301, n294, n1353);
    let n1357: ZB = zsel_b(n1301, n1277, n1354);
    let n1358: ZN = zsel_n(n1299, n1295, n1355);
    let n1359: ZN = zsel_n(n1299, zn_splat(P8::from_raw(0i32)), n1356);
    let n1360: ZB = zsel_b(n1299, n1277, n1357);
    let n1361: ZN = zsel_n(n1296, n1295, n1358);
    let n1362: ZN = zsel_n(n1296, n294, n1359);
    let n1363: ZB = zsel_b(n1296, n1277, n1360);
    let n1364: ZN = zsel_n(n1294, n1290, n1361);
    let n1365: ZN = zsel_n(n1294, zn_splat(P8::from_raw(0i32)), n1362);
    let n1366: ZB = zsel_b(n1294, n1277, n1363);
    let n1367: ZN = zsel_n(n1291, n1290, n1364);
    let n1368: ZN = zsel_n(n1291, n294, n1365);
    let n1369: ZB = zsel_b(n1291, n1277, n1366);
    let n1370: ZN = zsel_n(n1289, n292, n1367);
    let n1371: ZN = zsel_n(n1289, zn_splat(P8::from_raw(0i32)), n1368);
    let n1372: ZB = zsel_b(n1289, n1277, n1369);
    let n1373: ZN = zsel_n(n1183, n1275, r_c300);
    let n1374: ZN = zsel_n(n1183, n1370, n292);
    let n1375: ZN = zsel_n(n1183, n1276, n293);
    let n1376: ZN = zsel_n(n1183, n1371, n294);
    let n1377: ZB = zb_or(n1184, n1372);
    let n1378: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1373);
    let n1379: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1374);
    let n1380: ZN = zn_div(n1378, zn_splat(P8::from_raw(524288i32)));
    let n1381: ZN = zn_flr(n1380);
    let n1382: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1381);
    let n1383: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n1378);
    let n1384: ZN = zn_sub(n1383, zn_splat(P8::from_raw(65536i32)));
    let n1385: ZN = zn_div(n1384, zn_splat(P8::from_raw(524288i32)));
    let n1386: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n1385);
    let n1387: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1382);
    let n1388: ZB = zn_le(n1387, n1386);
    let n1389: ZB = zn_gt(n1387, n1386);
    let n1390: ZB = zb_and(n1178, n1388);
    let n1391: ZB = zb_and(n1178, n1389);
    let n1392: ZN = zn_div(n1379, zn_splat(P8::from_raw(524288i32)));
    let n1393: ZN = zn_flr(n1392);
    let n1394: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1393);
    let n1395: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n1379);
    let n1396: ZN = zn_sub(n1395, zn_splat(P8::from_raw(65536i32)));
    let n1397: ZN = zn_div(n1396, zn_splat(P8::from_raw(524288i32)));
    let n1398: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n1397);
    let n1399: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1394);
    let n1400: ZB = zn_le(n1399, n1398);
    let n1401: ZB = zn_gt(n1399, n1398);
    let n1402: ZB = zb_and(n1390, n1400);
    let n1403: ZB = zb_and(n1390, n1401);
    let n1404: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n1387);
    let n1405: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1399);
    let n1406: ZN = zn_mget(g.cart, n1404, n1405);
    let n1407: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1406);
    let n1408: ZN = zn_rem(n1396, zn_splat(P8::from_raw(524288i32)));
    let n1409: ZB = zn_ge(n1408, zn_splat(P8::from_raw(393216i32)));
    let n1410: ZN = zn_mul(n1399, zn_splat(P8::from_raw(524288i32)));
    let n1411: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1410);
    let n1412: ZB = zn_eq(n1395, n1411);
    let n1413: ZB = zb_or(n1409, n1412);
    let n1414: ZB = zb_and(n1407, n1413);
    let n1415: ZB = zn_ge(n1376, zn_splat(P8::from_raw(0i32)));
    let n1416: ZB = zb_and(n1414, n1415);
    let n1417: ZB = zb_not(n1416);
    let n1418: ZB = zb_and(n1402, n1416);
    let n1419: ZB = zb_and(n1402, n1417);
    let n1420: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1406);
    let n1421: ZN = zn_rem(n1379, zn_splat(P8::from_raw(524288i32)));
    let n1422: ZB = zn_le(n1421, zn_splat(P8::from_raw(131072i32)));
    let n1423: ZB = zb_and(n1420, n1422);
    let n1424: ZB = zn_le(n1376, zn_splat(P8::from_raw(0i32)));
    let n1425: ZB = zb_and(n1423, n1424);
    let n1426: ZB = zb_not(n1425);
    let n1427: ZB = zb_and(n1419, n1425);
    let n1428: ZB = zb_and(n1419, n1426);
    let n1429: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1406);
    let n1430: ZN = zn_rem(n1378, zn_splat(P8::from_raw(524288i32)));
    let n1431: ZB = zn_le(n1430, zn_splat(P8::from_raw(131072i32)));
    let n1432: ZB = zb_and(n1429, n1431);
    let n1433: ZB = zn_le(n1375, zn_splat(P8::from_raw(0i32)));
    let n1434: ZB = zb_and(n1432, n1433);
    let n1435: ZB = zb_not(n1434);
    let n1436: ZB = zb_and(n1428, n1434);
    let n1437: ZB = zb_and(n1428, n1435);
    let n1438: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1406);
    let n1439: ZN = zn_rem(n1384, zn_splat(P8::from_raw(524288i32)));
    let n1440: ZB = zn_ge(n1439, zn_splat(P8::from_raw(393216i32)));
    let n1441: ZN = zn_mul(n1387, zn_splat(P8::from_raw(524288i32)));
    let n1442: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1441);
    let n1443: ZB = zn_eq(n1383, n1442);
    let n1444: ZB = zb_or(n1440, n1443);
    let n1445: ZB = zb_and(n1438, n1444);
    let n1446: ZB = zn_ge(n1375, zn_splat(P8::from_raw(0i32)));
    let n1447: ZB = zb_and(n1445, n1446);
    let n1448: ZB = zb_not(n1447);
    let n1449: ZB = zb_and(n1437, n1447);
    let n1450: ZB = zb_and(n1437, n1448);
    let n1451: ZB = zb_or(n1436, n1449);
    let n1452: ZB = zb_or(n1427, n1451);
    let n1453: ZB = zb_or(n1418, n1452);
    let n1454: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1394);
    let n1455: ZB = zn_le(n1454, n1398);
    let n1456: ZB = zn_gt(n1454, n1398);
    let n1457: ZB = zb_and(n1450, n1455);
    let n1458: ZB = zb_and(n1450, n1456);
    let n1459: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1454);
    let n1460: ZN = zn_mget(g.cart, n1404, n1459);
    let n1461: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1460);
    let n1462: ZN = zn_mul(n1454, zn_splat(P8::from_raw(524288i32)));
    let n1463: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1462);
    let n1464: ZB = zn_eq(n1395, n1463);
    let n1465: ZB = zb_or(n1409, n1464);
    let n1466: ZB = zb_and(n1461, n1465);
    let n1467: ZB = zb_and(n1415, n1466);
    let n1468: ZB = zb_not(n1467);
    let n1469: ZB = zb_and(n1457, n1467);
    let n1470: ZB = zb_and(n1457, n1468);
    let n1471: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1460);
    let n1472: ZB = zb_and(n1422, n1471);
    let n1473: ZB = zb_and(n1424, n1472);
    let n1474: ZB = zb_not(n1473);
    let n1475: ZB = zb_and(n1470, n1473);
    let n1476: ZB = zb_and(n1470, n1474);
    let n1477: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1460);
    let n1478: ZB = zb_and(n1431, n1477);
    let n1479: ZB = zb_and(n1433, n1478);
    let n1480: ZB = zb_not(n1479);
    let n1481: ZB = zb_and(n1476, n1479);
    let n1482: ZB = zb_and(n1476, n1480);
    let n1483: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1460);
    let n1484: ZB = zb_and(n1444, n1483);
    let n1485: ZB = zb_and(n1446, n1484);
    let n1486: ZB = zb_not(n1485);
    let n1487: ZB = zb_and(n1482, n1485);
    let n1488: ZB = zb_and(n1482, n1486);
    let n1489: ZB = zb_or(n1481, n1487);
    let n1490: ZB = zb_or(n1475, n1489);
    let n1491: ZB = zb_or(n1469, n1490);
    let n1492: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n1394);
    let n1493: ZB = zn_le(n1492, n1398);
    let n1494: ZB = zn_gt(n1492, n1398);
    let n1495: ZB = zb_and(n1488, n1493);
    let n1496: ZB = zb_and(n1488, n1494);
    let n1497: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1492);
    let n1498: ZN = zn_mget(g.cart, n1404, n1497);
    let n1499: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1498);
    let n1500: ZN = zn_mul(n1492, zn_splat(P8::from_raw(524288i32)));
    let n1501: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1500);
    let n1502: ZB = zn_eq(n1395, n1501);
    let n1503: ZB = zb_or(n1409, n1502);
    let n1504: ZB = zb_and(n1499, n1503);
    let n1505: ZB = zb_and(n1415, n1504);
    let n1506: ZB = zb_not(n1505);
    let n1507: ZB = zb_and(n1495, n1505);
    let n1508: ZB = zb_and(n1495, n1506);
    let n1509: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1498);
    let n1510: ZB = zb_and(n1422, n1509);
    let n1511: ZB = zb_and(n1424, n1510);
    let n1512: ZB = zb_not(n1511);
    let n1513: ZB = zb_and(n1508, n1511);
    let n1514: ZB = zb_and(n1508, n1512);
    let n1515: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1498);
    let n1516: ZB = zb_and(n1431, n1515);
    let n1517: ZB = zb_and(n1433, n1516);
    let n1518: ZB = zb_not(n1517);
    let n1519: ZB = zb_and(n1514, n1517);
    let n1520: ZB = zb_and(n1514, n1518);
    let n1521: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1498);
    let n1522: ZB = zb_and(n1444, n1521);
    let n1523: ZB = zb_and(n1446, n1522);
    let n1524: ZB = zb_not(n1523);
    let n1525: ZB = zb_and(n1520, n1523);
    let n1526: ZB = zb_and(n1520, n1524);
    let n1527: ZB = zb_or(n1519, n1525);
    let n1528: ZB = zb_or(n1513, n1527);
    let n1529: ZB = zb_or(n1507, n1528);
    let n1530: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1394);
    let n1531: ZB = zn_gt(n1530, n1398);
    let n1532: ZB = zb_and(n1377, n1531);
    let n1533: ZB = zb_or(n1496, n1526);
    let n1534: ZB = zsel_b(n1494, n1377, n1532);
    let n1535: ZB = zb_or(n1491, n1529);
    let n1536: ZB = zb_or(n1458, n1533);
    let n1537: ZB = zsel_b(n1456, n1377, n1534);
    let n1538: ZB = zb_or(n1453, n1535);
    let n1539: ZB = zb_or(n1403, n1536);
    let n1540: ZB = zsel_b(n1401, n1377, n1537);
    let n1541: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1382);
    let n1542: ZB = zn_le(n1541, n1386);
    let n1543: ZB = zn_gt(n1541, n1386);
    let n1544: ZB = zb_and(n1539, n1542);
    let n1545: ZB = zb_and(n1539, n1543);
    let n1546: ZB = zb_and(n1401, n1544);
    let n1547: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n1541);
    let n1548: ZN = zn_mget(g.cart, n1547, n1405);
    let n1549: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1548);
    let n1550: ZB = zb_and(n1400, n1539);
    let n1551: ZB = zb_and(n1542, n1550);
    let n1552: ZB = zb_and(n1413, n1549);
    let n1553: ZB = zb_and(n1415, n1552);
    let n1554: ZB = zb_not(n1553);
    let n1555: ZB = zb_and(n1551, n1553);
    let n1556: ZB = zb_and(n1551, n1554);
    let n1557: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1548);
    let n1558: ZB = zb_and(n1422, n1557);
    let n1559: ZB = zb_and(n1424, n1558);
    let n1560: ZB = zb_not(n1559);
    let n1561: ZB = zb_and(n1556, n1559);
    let n1562: ZB = zb_and(n1556, n1560);
    let n1563: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1548);
    let n1564: ZB = zb_and(n1431, n1563);
    let n1565: ZB = zb_and(n1433, n1564);
    let n1566: ZB = zb_not(n1565);
    let n1567: ZB = zb_and(n1562, n1565);
    let n1568: ZB = zb_and(n1562, n1566);
    let n1569: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1548);
    let n1570: ZN = zn_mul(n1541, zn_splat(P8::from_raw(524288i32)));
    let n1571: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1570);
    let n1572: ZB = zn_eq(n1383, n1571);
    let n1573: ZB = zb_or(n1440, n1572);
    let n1574: ZB = zb_and(n1569, n1573);
    let n1575: ZB = zb_and(n1446, n1574);
    let n1576: ZB = zb_not(n1575);
    let n1577: ZB = zb_and(n1568, n1575);
    let n1578: ZB = zb_and(n1568, n1576);
    let n1579: ZB = zb_or(n1567, n1577);
    let n1580: ZB = zb_or(n1561, n1579);
    let n1581: ZB = zb_or(n1555, n1580);
    let n1582: ZB = zb_and(n1456, n1578);
    let n1583: ZN = zn_mget(g.cart, n1547, n1459);
    let n1584: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1583);
    let n1585: ZB = zb_and(n1455, n1568);
    let n1586: ZB = zb_and(n1576, n1585);
    let n1587: ZB = zb_and(n1465, n1584);
    let n1588: ZB = zb_and(n1415, n1587);
    let n1589: ZB = zb_not(n1588);
    let n1590: ZB = zb_and(n1586, n1588);
    let n1591: ZB = zb_and(n1586, n1589);
    let n1592: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1583);
    let n1593: ZB = zb_and(n1422, n1592);
    let n1594: ZB = zb_and(n1424, n1593);
    let n1595: ZB = zb_not(n1594);
    let n1596: ZB = zb_and(n1591, n1594);
    let n1597: ZB = zb_and(n1591, n1595);
    let n1598: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1583);
    let n1599: ZB = zb_and(n1431, n1598);
    let n1600: ZB = zb_and(n1433, n1599);
    let n1601: ZB = zb_not(n1600);
    let n1602: ZB = zb_and(n1597, n1600);
    let n1603: ZB = zb_and(n1597, n1601);
    let n1604: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1583);
    let n1605: ZB = zb_and(n1573, n1604);
    let n1606: ZB = zb_and(n1446, n1605);
    let n1607: ZB = zb_not(n1606);
    let n1608: ZB = zb_and(n1603, n1606);
    let n1609: ZB = zb_and(n1603, n1607);
    let n1610: ZB = zb_or(n1602, n1608);
    let n1611: ZB = zb_or(n1596, n1610);
    let n1612: ZB = zb_or(n1590, n1611);
    let n1613: ZB = zb_and(n1494, n1609);
    let n1614: ZN = zn_mget(g.cart, n1547, n1497);
    let n1615: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1614);
    let n1616: ZB = zb_and(n1493, n1603);
    let n1617: ZB = zb_and(n1607, n1616);
    let n1618: ZB = zb_and(n1503, n1615);
    let n1619: ZB = zb_and(n1415, n1618);
    let n1620: ZB = zb_not(n1619);
    let n1621: ZB = zb_and(n1617, n1619);
    let n1622: ZB = zb_and(n1617, n1620);
    let n1623: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1614);
    let n1624: ZB = zb_and(n1422, n1623);
    let n1625: ZB = zb_and(n1424, n1624);
    let n1626: ZB = zb_not(n1625);
    let n1627: ZB = zb_and(n1622, n1625);
    let n1628: ZB = zb_and(n1622, n1626);
    let n1629: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1614);
    let n1630: ZB = zb_and(n1431, n1629);
    let n1631: ZB = zb_and(n1433, n1630);
    let n1632: ZB = zb_not(n1631);
    let n1633: ZB = zb_and(n1628, n1631);
    let n1634: ZB = zb_and(n1628, n1632);
    let n1635: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1614);
    let n1636: ZB = zb_and(n1573, n1635);
    let n1637: ZB = zb_and(n1446, n1636);
    let n1638: ZB = zb_not(n1637);
    let n1639: ZB = zb_and(n1634, n1637);
    let n1640: ZB = zb_and(n1634, n1638);
    let n1641: ZB = zb_or(n1633, n1639);
    let n1642: ZB = zb_or(n1627, n1641);
    let n1643: ZB = zb_or(n1621, n1642);
    let n1644: ZB = zb_and(n1531, n1540);
    let n1645: ZB = zb_or(n1613, n1640);
    let n1646: ZB = zsel_b(n1494, n1540, n1644);
    let n1647: ZB = zb_or(n1612, n1643);
    let n1648: ZB = zb_or(n1582, n1645);
    let n1649: ZB = zsel_b(n1456, n1540, n1646);
    let n1650: ZB = zb_or(n1581, n1647);
    let n1651: ZB = zb_or(n1546, n1648);
    let n1652: ZB = zsel_b(n1401, n1540, n1649);
    let n1653: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n1382);
    let n1654: ZB = zn_le(n1653, n1386);
    let n1655: ZB = zn_gt(n1653, n1386);
    let n1656: ZB = zb_and(n1651, n1654);
    let n1657: ZB = zb_and(n1651, n1655);
    let n1658: ZB = zb_and(n1401, n1656);
    let n1659: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n1653);
    let n1660: ZN = zn_mget(g.cart, n1659, n1405);
    let n1661: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1660);
    let n1662: ZB = zb_and(n1400, n1651);
    let n1663: ZB = zb_and(n1654, n1662);
    let n1664: ZB = zb_and(n1413, n1661);
    let n1665: ZB = zb_and(n1415, n1664);
    let n1666: ZB = zb_not(n1665);
    let n1667: ZB = zb_and(n1663, n1665);
    let n1668: ZB = zb_and(n1663, n1666);
    let n1669: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1660);
    let n1670: ZB = zb_and(n1422, n1669);
    let n1671: ZB = zb_and(n1424, n1670);
    let n1672: ZB = zb_not(n1671);
    let n1673: ZB = zb_and(n1668, n1671);
    let n1674: ZB = zb_and(n1668, n1672);
    let n1675: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1660);
    let n1676: ZB = zb_and(n1431, n1675);
    let n1677: ZB = zb_and(n1433, n1676);
    let n1678: ZB = zb_not(n1677);
    let n1679: ZB = zb_and(n1674, n1677);
    let n1680: ZB = zb_and(n1674, n1678);
    let n1681: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1660);
    let n1682: ZN = zn_mul(n1653, zn_splat(P8::from_raw(524288i32)));
    let n1683: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1682);
    let n1684: ZB = zn_eq(n1383, n1683);
    let n1685: ZB = zb_or(n1440, n1684);
    let n1686: ZB = zb_and(n1681, n1685);
    let n1687: ZB = zb_and(n1446, n1686);
    let n1688: ZB = zb_not(n1687);
    let n1689: ZB = zb_and(n1680, n1687);
    let n1690: ZB = zb_and(n1680, n1688);
    let n1691: ZB = zb_or(n1679, n1689);
    let n1692: ZB = zb_or(n1673, n1691);
    let n1693: ZB = zb_or(n1667, n1692);
    let n1694: ZB = zb_and(n1456, n1690);
    let n1695: ZN = zn_mget(g.cart, n1659, n1459);
    let n1696: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1695);
    let n1697: ZB = zb_and(n1455, n1680);
    let n1698: ZB = zb_and(n1688, n1697);
    let n1699: ZB = zb_and(n1465, n1696);
    let n1700: ZB = zb_and(n1415, n1699);
    let n1701: ZB = zb_not(n1700);
    let n1702: ZB = zb_and(n1698, n1700);
    let n1703: ZB = zb_and(n1698, n1701);
    let n1704: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1695);
    let n1705: ZB = zb_and(n1422, n1704);
    let n1706: ZB = zb_and(n1424, n1705);
    let n1707: ZB = zb_not(n1706);
    let n1708: ZB = zb_and(n1703, n1706);
    let n1709: ZB = zb_and(n1703, n1707);
    let n1710: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1695);
    let n1711: ZB = zb_and(n1431, n1710);
    let n1712: ZB = zb_and(n1433, n1711);
    let n1713: ZB = zb_not(n1712);
    let n1714: ZB = zb_and(n1709, n1712);
    let n1715: ZB = zb_and(n1709, n1713);
    let n1716: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1695);
    let n1717: ZB = zb_and(n1685, n1716);
    let n1718: ZB = zb_and(n1446, n1717);
    let n1719: ZB = zb_not(n1718);
    let n1720: ZB = zb_and(n1715, n1718);
    let n1721: ZB = zb_and(n1715, n1719);
    let n1722: ZB = zb_or(n1714, n1720);
    let n1723: ZB = zb_or(n1708, n1722);
    let n1724: ZB = zb_or(n1702, n1723);
    let n1725: ZB = zb_and(n1494, n1721);
    let n1726: ZN = zn_mget(g.cart, n1659, n1497);
    let n1727: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1726);
    let n1728: ZB = zb_and(n1493, n1715);
    let n1729: ZB = zb_and(n1719, n1728);
    let n1730: ZB = zb_and(n1503, n1727);
    let n1731: ZB = zb_and(n1415, n1730);
    let n1732: ZB = zb_not(n1731);
    let n1733: ZB = zb_and(n1729, n1731);
    let n1734: ZB = zb_and(n1729, n1732);
    let n1735: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1726);
    let n1736: ZB = zb_and(n1422, n1735);
    let n1737: ZB = zb_and(n1424, n1736);
    let n1738: ZB = zb_not(n1737);
    let n1739: ZB = zb_and(n1734, n1737);
    let n1740: ZB = zb_and(n1734, n1738);
    let n1741: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1726);
    let n1742: ZB = zb_and(n1431, n1741);
    let n1743: ZB = zb_and(n1433, n1742);
    let n1744: ZB = zb_not(n1743);
    let n1745: ZB = zb_and(n1740, n1743);
    let n1746: ZB = zb_and(n1740, n1744);
    let n1747: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1726);
    let n1748: ZB = zb_and(n1685, n1747);
    let n1749: ZB = zb_and(n1446, n1748);
    let n1750: ZB = zb_not(n1749);
    let n1751: ZB = zb_and(n1746, n1749);
    let n1752: ZB = zb_and(n1746, n1750);
    let n1753: ZB = zb_or(n1745, n1751);
    let n1754: ZB = zb_or(n1739, n1753);
    let n1755: ZB = zb_or(n1733, n1754);
    let n1756: ZB = zb_and(n1531, n1652);
    let n1757: ZB = zb_or(n1725, n1752);
    let n1758: ZB = zsel_b(n1494, n1652, n1756);
    let n1759: ZB = zb_or(n1724, n1755);
    let n1760: ZB = zb_or(n1694, n1757);
    let n1761: ZB = zsel_b(n1456, n1652, n1758);
    let n1762: ZB = zb_or(n1693, n1759);
    let n1763: ZB = zb_or(n1658, n1760);
    let n1764: ZB = zsel_b(n1401, n1652, n1761);
    let n1765: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1382);
    let n1766: ZB = zn_gt(n1765, n1386);
    let n1767: ZB = zb_and(n1764, n1766);
    let n1768: ZB = zb_or(n1650, n1762);
    let n1769: ZB = zsel_b(n1650, n1540, n1652);
    let n1770: ZB = zb_or(n1657, n1763);
    let n1771: ZB = zsel_b(n1655, n1652, n1767);
    let n1772: ZB = zb_or(n1538, n1768);
    let n1773: ZB = zsel_b(n1538, n1377, n1769);
    let n1774: ZB = zb_or(n1545, n1770);
    let n1775: ZB = zsel_b(n1543, n1540, n1771);
    let n1776: ZB = zb_or(n1391, n1774);
    let n1777: ZB = zsel_b(n1389, n1377, n1775);
    let n1778: ZB = zn_gt(n1374, zn_splat(P8::from_raw(8388608i32)));
    let n1779: ZB = zn_le(n1374, zn_splat(P8::from_raw(8388608i32)));
    let n1780: ZB = zb_and(n1776, n1778);
    let n1781: ZB = zb_or(n1772, n1780);
    let n1782: ZB = zsel_b(n1772, n1773, n1777);
    let n1783: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1378);
    let n1784: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1379);
    let n1785: ZB = zn_tile_flag_at(g.cache, g.cart, n1783, n1784, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1786: ZB = zb_not(n1785);
    let n1787: ZB = zn_lt(n1173, zn_splat(P8::from_raw(65536i32)));
    let n1788: ZN = zsel_n(n1787, zn_splat(P8::from_raw(65536i32)), n1173);
    let n1789: ZN = zsel_n(n1785, n1788, n1173);
    let n1790: ZN = zsel_n(n1785, zn_splat(P8::from_raw(393216i32)), n277);
    let n1791: ZB = zn_gt(n1375, r_c359);
    let n1792: ZB = zn_gt(n1376, r_c360);
    let n1793: ZN = zsel_n(n1786, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n1794: ZN = zn_abs(n1375);
    let n1795: ZB = zn_gt(n1794, zn_splat(P8::from_raw(65536i32)));
    let n1796: ZB = zn_gt(n1375, zn_splat(P8::from_raw(0i32)));
    let n1797: ZB = zn_lt(n1375, zn_splat(P8::from_raw(0i32)));
    let n1798: ZB = zn_gt(n1375, zn_splat(P8::from_raw(65536i32)));
    let n1799: ZN = zn_sub(n1375, zn_splat(P8::from_raw(9830i32)));
    let n1800: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n1799);
    let n1801: ZN = zn_add(zn_splat(P8::from_raw(9830i32)), n1375);
    let n1802: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n1801);
    let n1803: ZB = zn_gt(n1375, zn_splat(P8::from_raw(-65536i32)));
    let n1804: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1799);
    let n1805: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n1801);
    let n1806: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1799);
    let n1807: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n1801);
    let n1808: ZN = zsel_n(n1803, n1804, n1805);
    let n1809: ZN = zsel_n(n1796, n1806, n1807);
    let n1810: ZN = zsel_n(n1798, n1800, n1802);
    let n1811: ZN = zsel_n(n1797, n1808, n1809);
    let n1812: ZN = zsel_n(n1796, n1810, n1811);
    let n1813: ZN = zn_sub(n1375, n1793);
    let n1814: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1813);
    let n1815: ZN = zn_add(n1375, n1793);
    let n1816: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n1815);
    let n1817: ZN = zsel_n(n1796, n1814, n1816);
    let n1818: ZN = zsel_n(n1795, n1812, n1817);
    let n1819: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1818);
    let n1820: ZB = zb_not(n1819);
    let n1821: ZB = zn_lt(n1818, zn_splat(P8::from_raw(0i32)));
    let n1822: ZB = zsel_b(n1820, n1821, r_c361);
    let n1823: ZN = zn_abs(n1376);
    let n1824: ZB = zn_le(n1823, zn_splat(P8::from_raw(9830i32)));
    let n1825: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1379);
    let n1826: ZB = zn_gt(n1376, zn_splat(P8::from_raw(131072i32)));
    let n1827: ZB = zn_gt(n1790, zn_splat(P8::from_raw(0i32)));
    let n1828: ZN = zn_add(zn_splat(P8::from_raw(-196608i32)), n1378);
    let n1829: ZB = zn_tile_flag_at(g.cache, g.cart, n1828, n1825, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1830: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1378);
    let n1831: ZB = zn_tile_flag_at(g.cache, g.cart, n1830, n1825, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1832: ZN = zsel_n(n1831, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n1833: ZN = zsel_n(n1829, zn_splat(P8::from_raw(-65536i32)), n1832);
    let n1834: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1833);
    let n1835: ZB = zb_not(n1834);
    let n1836: ZB = zn_gt(n1789, zn_splat(P8::from_raw(0i32)));
    let n1837: ZN = zsel_n(n1822, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n1838: ZB = zn_gt(n1837, zn_splat(P8::from_raw(0i32)));
    let n1839: ZB = zn_lt(n1837, zn_splat(P8::from_raw(0i32)));
    let n1840: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1837);
    let n1841: ZB = zb_not(n1840);
    let n1842: ZB = zn_lt(n1374, zn_splat(P8::from_raw(-262144i32)));
    let n1843: ZB = zn_ge(n1374, zn_splat(P8::from_raw(-262144i32)));
    let n1844: ZB = zb_and(n1781, n1842);
    let n1846: ZN = zsel_n(n1778, n1103, n1102);
    let n1847: ZN = zsel_n(n1772, n1846, n1102);
    let n1849: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1813);
    let n1850: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n1815);
    let n1851: ZN = zsel_n(n1803, n1849, n1850);
    let n1852: ZN = zsel_n(n1795, n1812, n1851);
    let n1853: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1852);
    let n1854: ZB = zb_not(n1853);
    let n1855: ZB = zn_lt(n1852, zn_splat(P8::from_raw(0i32)));
    let n1856: ZB = zsel_b(n1854, n1855, r_c361);
    let n1857: ZN = zn_add(zn_splat(P8::from_raw(-65536i32)), n1378);
    let n1858: ZB = zn_tile_flag_at(g.cache, g.cart, n1857, n1825, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1859: ZN = zsel_n(n1858, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n1860: ZB = zn_gt(n1376, n1859);
    let n1861: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n1813);
    let n1862: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n1815);
    let n1863: ZN = zsel_n(n1798, n1861, n1862);
    let n1864: ZN = zsel_n(n1795, n1812, n1863);
    let n1865: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1864);
    let n1866: ZB = zb_not(n1865);
    let n1867: ZB = zn_lt(n1864, zn_splat(P8::from_raw(0i32)));
    let n1868: ZB = zsel_b(n1866, n1867, r_c361);
    let n1869: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1378);
    let n1870: ZB = zn_tile_flag_at(g.cache, g.cart, n1869, n1825, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1871: ZN = zsel_n(n1870, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n1872: ZB = zn_gt(n1376, n1871);
    let n1873: ZB = zb_and(n133, n1836);
    let n1874: ZN = zsel_n(n1873, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n1875: ZB = zb_or(r_c41, n1873);
    let n1876: ZN = zsel_n(n279, r_c20, n1874);
    let n1877: ZB = zsel_b(n279, r_c41, n1875);
    let n1881: ZB = zb_and(n1776, n1779);
    let n1882: ZB = zb_and(n1842, n1881);
    let n1883: ZB = zb_and(n1843, n1881);
    let n1884: ZB = zb_not(n1882);
    let n1885: ZB = zb_or(n1844, n1882);
    let n1886: ZB = zsel_b(n1882, n1777, n1782);
    let n1887: ZN = zsel_n(n1882, r_c87, n1847);
    let n1888: ZN = zsel_n(n1882, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1890: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n1891: ZB = zb_and(r_c293, n282);
    let n1892: ZB = zb_and(r_c294, n282);
    let n1893: ZN = zn_sub(n1186, zn_splat(P8::from_raw(32768i32)));
    let n1894: ZN = zn_sub(n1893, n1187);
    let n1895: ZN = zsel_n(n1230, zn_splat(P8::from_raw(0i32)), n1894);
    let n1896: ZN = zsel_n(n1227, n1894, n1895);
    let n1897: ZN = zsel_n(n1225, zn_splat(P8::from_raw(0i32)), n1896);
    let n1898: ZN = zsel_n(n1222, n1894, n1897);
    let n1899: ZN = zsel_n(n1220, zn_splat(P8::from_raw(0i32)), n1898);
    let n1900: ZN = zsel_n(n1217, n1894, n1899);
    let n1901: ZN = zsel_n(n1215, zn_splat(P8::from_raw(0i32)), n1900);
    let n1902: ZN = zsel_n(n1212, n1894, n1901);
    let n1903: ZN = zsel_n(n1210, zn_splat(P8::from_raw(0i32)), n1902);
    let n1904: ZN = zsel_n(n1207, n1894, n1903);
    let n1905: ZN = zsel_n(n1205, zn_splat(P8::from_raw(0i32)), n1904);
    let n1906: ZN = zsel_n(n1202, n1894, n1905);
    let n1907: ZN = zsel_n(n1200, zn_splat(P8::from_raw(0i32)), n1906);
    let n1908: ZN = zsel_n(n1197, n1894, n1907);
    let n1909: ZN = zsel_n(n1195, zn_splat(P8::from_raw(0i32)), n1908);
    let n1910: ZN = zn_sub(n1279, zn_splat(P8::from_raw(32768i32)));
    let n1911: ZN = zn_sub(n1910, n1280);
    let n1912: ZN = zsel_n(n1324, zn_splat(P8::from_raw(0i32)), n1911);
    let n1913: ZN = zsel_n(n1321, n1911, n1912);
    let n1914: ZN = zsel_n(n1319, zn_splat(P8::from_raw(0i32)), n1913);
    let n1915: ZN = zsel_n(n1316, n1911, n1914);
    let n1916: ZN = zsel_n(n1314, zn_splat(P8::from_raw(0i32)), n1915);
    let n1917: ZN = zsel_n(n1311, n1911, n1916);
    let n1918: ZN = zsel_n(n1309, zn_splat(P8::from_raw(0i32)), n1917);
    let n1919: ZN = zsel_n(n1306, n1911, n1918);
    let n1920: ZN = zsel_n(n1304, zn_splat(P8::from_raw(0i32)), n1919);
    let n1921: ZN = zsel_n(n1301, n1911, n1920);
    let n1922: ZN = zsel_n(n1299, zn_splat(P8::from_raw(0i32)), n1921);
    let n1923: ZN = zsel_n(n1296, n1911, n1922);
    let n1924: ZN = zsel_n(n1294, zn_splat(P8::from_raw(0i32)), n1923);
    let n1925: ZN = zsel_n(n1291, n1911, n1924);
    let n1926: ZN = zsel_n(n1289, zn_splat(P8::from_raw(0i32)), n1925);
    let n1927: ZN = zsel_n(n1183, n1909, r_c367);
    let n1928: ZN = zsel_n(n1183, n1926, r_c368);
    let n1929: ZN = zn_sub(n1375, r_c357);
    let n1930: ZN = zn_max(r_c359, n1929);
    let n1931: ZN = zn_add(r_c357, n1375);
    let n1932: ZN = zn_min(r_c359, n1931);
    let n1933: ZN = zsel_n(n1791, n1930, n1932);
    let n1934: ZN = zn_sub(n1376, r_c358);
    let n1935: ZN = zn_max(r_c360, n1934);
    let n1936: ZN = zn_add(r_c358, n1376);
    let n1937: ZN = zn_min(r_c360, n1936);
    let n1938: ZN = zsel_n(n1792, n1935, n1937);
    let n1939: ZN = zsel_n(n1824, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(13762i32)));
    let n1940: ZN = zn_sub(n1376, n1939);
    let n1941: ZN = zn_max(zn_splat(P8::from_raw(131072i32)), n1940);
    let n1942: ZN = zn_add(n1376, n1939);
    let n1943: ZN = zn_min(zn_splat(P8::from_raw(131072i32)), n1942);
    let n1944: ZN = zsel_n(n1826, n1941, n1943);
    let n1945: ZN = zsel_n(n1786, n1944, n1376);
    let n1946: ZN = zn_neg(n1833);
    let n1947: ZN = zn_mul(n1946, zn_splat(P8::from_raw(131072i32)));
    let n1948: ZN = zsel_n(n1835, n1947, n1818);
    let n1949: ZN = zsel_n(n1835, zn_splat(P8::from_raw(-131072i32)), n1945);
    let n1950: ZN = zsel_n(n1827, zn_splat(P8::from_raw(0i32)), n1790);
    let n1951: ZN = zsel_n(n1827, n1818, n1948);
    let n1952: ZN = zsel_n(n1827, zn_splat(P8::from_raw(-131072i32)), n1949);
    let n1953: ZN = zn_sub(n1789, zn_splat(P8::from_raw(65536i32)));
    let n1954: ZN = zsel_n(n1839, zn_splat(P8::from_raw(-131072i32)), zn_splat(P8::from_raw(0i32)));
    let n1955: ZN = zsel_n(n1838, zn_splat(P8::from_raw(131072i32)), n1954);
    let n1956: ZN = zsel_n(n1841, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n1957: ZB = zsel_b(n279, r_c361, n1822);
    let n1958: ZN = zsel_n(n279, n1933, n1818);
    let n1959: ZN = zsel_n(n279, n1938, n1945);
    let n1960: ZN = zsel_n(n282, n1890, r_c20);
    let n1961: ZN = zsel_n(n282, r_c241, n290);
    let n1962: ZN = zsel_n(n282, r_c254, n291);
    let n1963: ZN = zsel_n(n282, r_c281, n278);
    let n1964: ZN = zsel_n(n282, r_c283, n281);
    let n1965: ZN = zsel_n(n282, r_c284, n1789);
    let n1966: ZN = zsel_n(n282, r_c286, n1790);
    let n1967: ZN = zsel_n(n282, r_c300, n1373);
    let n1968: ZN = zsel_n(n282, r_c301, n1374);
    let n1969: ZB = zsel_b(n282, r_c361, n1957);
    let n1970: ZN = zsel_n(n282, r_c367, n1927);
    let n1971: ZN = zsel_n(n282, r_c368, n1928);
    let n1972: ZN = zsel_n(n282, r_c369, n1958);
    let n1973: ZN = zsel_n(n282, r_c370, n1959);
    let n1974: ZB = zb_or(n282, n1883);
    let n1975: ZB = zb_or(n282, n1777);
    let n1976: ZB = zn_gt(n1960, zn_splat(P8::from_raw(0i32)));
    let n1977: ZB = zn_lt(n1967, zn_splat(P8::from_raw(-65536i32)));
    let n1978: ZB = zn_gt(n1967, zn_splat(P8::from_raw(7929856i32)));
    let n1979: ZB = zb_or(n1977, n1978);
    let n1980: ZN = zn_min(zn_splat(P8::from_raw(7929856i32)), n1967);
    let n1981: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1980);
    let n1982: ZN = zsel_n(n1979, n1981, n1967);
    let n1983: ZN = zsel_n(n1979, zn_splat(P8::from_raw(0i32)), n1972);
    let n1984: ZN = zsel_n(n1976, n1967, n1982);
    let n1985: ZN = zsel_n(n1976, n1972, n1983);
    let n1987: ZN = zn_max(n1859, n1940);
    let n1988: ZN = zn_min(n1859, n1942);
    let n1989: ZN = zsel_n(n1860, n1987, n1988);
    let n1990: ZN = zsel_n(n1786, n1989, n1376);
    let n1991: ZN = zsel_n(n1835, n1947, n1852);
    let n1992: ZN = zsel_n(n1835, zn_splat(P8::from_raw(-131072i32)), n1990);
    let n1993: ZN = zsel_n(n1827, n1852, n1991);
    let n1994: ZN = zsel_n(n1827, zn_splat(P8::from_raw(-131072i32)), n1992);
    let n1995: ZB = zsel_b(n279, r_c361, n1856);
    let n1996: ZN = zsel_n(n279, n1933, n1852);
    let n1997: ZN = zsel_n(n279, n1938, n1990);
    let n1998: ZB = zsel_b(n282, r_c361, n1995);
    let n1999: ZN = zsel_n(n282, r_c369, n1996);
    let n2000: ZN = zsel_n(n282, r_c370, n1997);
    let n2001: ZN = zsel_n(n1979, zn_splat(P8::from_raw(0i32)), n1999);
    let n2002: ZN = zsel_n(n1976, n1999, n2001);
    let n2003: ZN = zn_max(n1871, n1940);
    let n2004: ZN = zn_min(n1871, n1942);
    let n2005: ZN = zsel_n(n1872, n2003, n2004);
    let n2006: ZN = zsel_n(n1786, n2005, n1376);
    let n2007: ZN = zsel_n(n1835, n1947, n1864);
    let n2008: ZN = zsel_n(n1835, zn_splat(P8::from_raw(-131072i32)), n2006);
    let n2009: ZN = zsel_n(n1827, n1864, n2007);
    let n2010: ZN = zsel_n(n1827, zn_splat(P8::from_raw(-131072i32)), n2008);
    let n2011: ZB = zsel_b(n279, r_c361, n1868);
    let n2012: ZN = zsel_n(n279, n1933, n1864);
    let n2013: ZN = zsel_n(n279, n1938, n2006);
    let n2014: ZB = zsel_b(n282, r_c361, n2011);
    let n2015: ZN = zsel_n(n282, r_c369, n2012);
    let n2016: ZN = zsel_n(n282, r_c370, n2013);
    let n2017: ZN = zsel_n(n1979, zn_splat(P8::from_raw(0i32)), n2015);
    let n2018: ZN = zsel_n(n1976, n2015, n2017);
    let n2019: ZB = zb_or(r_c294, n134);
    let n2020: ZN = zsel_n(n140, n1950, n1790);
    let n2021: ZN = zsel_n(n140, n1951, n1818);
    let n2022: ZN = zsel_n(n140, n1952, n1945);
    let n2023: ZN = zsel_n(n279, n1790, n2020);
    let n2024: ZN = zsel_n(n279, n1933, n2021);
    let n2025: ZN = zsel_n(n279, n1938, n2022);
    let n2026: ZN = zsel_n(n282, r_c286, n2023);
    let n2027: ZN = zsel_n(n282, r_c369, n2024);
    let n2028: ZN = zsel_n(n282, r_c370, n2025);
    let n2029: ZN = zsel_n(n1979, zn_splat(P8::from_raw(0i32)), n2027);
    let n2030: ZN = zsel_n(n1976, n2027, n2029);
    let n2031: ZN = zsel_n(n140, n1993, n1852);
    let n2032: ZN = zsel_n(n140, n1994, n1990);
    let n2033: ZN = zsel_n(n279, n1933, n2031);
    let n2034: ZN = zsel_n(n279, n1938, n2032);
    let n2035: ZN = zsel_n(n282, r_c369, n2033);
    let n2036: ZN = zsel_n(n282, r_c370, n2034);
    let n2037: ZN = zsel_n(n1979, zn_splat(P8::from_raw(0i32)), n2035);
    let n2038: ZN = zsel_n(n1976, n2035, n2037);
    let n2039: ZN = zsel_n(n140, n2009, n1864);
    let n2040: ZN = zsel_n(n140, n2010, n2006);
    let n2041: ZN = zsel_n(n279, n1933, n2039);
    let n2042: ZN = zsel_n(n279, n1938, n2040);
    let n2043: ZN = zsel_n(n282, r_c369, n2041);
    let n2044: ZN = zsel_n(n282, r_c370, n2042);
    let n2045: ZN = zsel_n(n1979, zn_splat(P8::from_raw(0i32)), n2043);
    let n2046: ZN = zsel_n(n1976, n2043, n2045);
    let n2047: ZB = zb_or(r_c293, n134);
    let n2048: ZN = zsel_n(n1873, zn_splat(P8::from_raw(655360i32)), n278);
    let n2049: ZN = zsel_n(n1873, zn_splat(P8::from_raw(262144i32)), r_c283);
    let n2050: ZN = zsel_n(n1873, n1953, n1789);
    let n2051: ZN = zsel_n(n1873, zn_splat(P8::from_raw(98304i32)), r_c357);
    let n2052: ZN = zsel_n(n1873, n1956, r_c358);
    let n2053: ZN = zsel_n(n1873, n1955, r_c359);
    let n2054: ZN = zsel_n(n1873, zn_splat(P8::from_raw(0i32)), r_c360);
    let n2055: ZN = zsel_n(n1873, n1837, n1818);
    let n2056: ZN = zsel_n(n1873, zn_splat(P8::from_raw(0i32)), n1945);
    let n2057: ZN = zsel_n(n279, n278, n2048);
    let n2058: ZN = zsel_n(n279, n280, n2049);
    let n2059: ZN = zsel_n(n279, n1789, n2050);
    let n2060: ZN = zsel_n(n279, r_c357, n2051);
    let n2061: ZN = zsel_n(n279, r_c358, n2052);
    let n2062: ZN = zsel_n(n279, r_c359, n2053);
    let n2063: ZN = zsel_n(n279, r_c360, n2054);
    let n2064: ZN = zsel_n(n279, n1933, n2055);
    let n2065: ZN = zsel_n(n279, n1938, n2056);
    let n2066: ZN = zsel_n(n282, n1890, n1876);
    let n2067: ZB = zsel_b(n282, r_c41, n1877);
    let n2068: ZN = zsel_n(n282, r_c281, n2057);
    let n2069: ZN = zsel_n(n282, r_c283, n2058);
    let n2070: ZN = zsel_n(n282, r_c284, n2059);
    let n2071: ZN = zsel_n(n282, r_c357, n2060);
    let n2072: ZN = zsel_n(n282, r_c358, n2061);
    let n2073: ZN = zsel_n(n282, r_c359, n2062);
    let n2074: ZN = zsel_n(n282, r_c360, n2063);
    let n2075: ZN = zsel_n(n282, r_c369, n2064);
    let n2076: ZN = zsel_n(n282, r_c370, n2065);
    let n2077: ZB = zn_gt(n2066, zn_splat(P8::from_raw(0i32)));
    let n2078: ZN = zsel_n(n1979, zn_splat(P8::from_raw(0i32)), n2075);
    let n2079: ZN = zsel_n(n2077, n1967, n1982);
    let n2080: ZN = zsel_n(n2077, n2075, n2078);
    let n2081: ZN = zsel_n(n1873, zn_splat(P8::from_raw(69510i32)), r_c358);
    let n2082: ZN = zsel_n(n1873, zn_splat(P8::from_raw(-131072i32)), r_c359);
    let n2083: ZN = zsel_n(n1873, zn_splat(P8::from_raw(-327680i32)), n1852);
    let n2084: ZN = zsel_n(n1873, zn_splat(P8::from_raw(0i32)), n1990);
    let n2085: ZN = zsel_n(n279, r_c358, n2081);
    let n2086: ZN = zsel_n(n279, r_c359, n2082);
    let n2087: ZN = zsel_n(n279, n1933, n2083);
    let n2088: ZN = zsel_n(n279, n1938, n2084);
    let n2089: ZN = zsel_n(n282, r_c358, n2085);
    let n2090: ZN = zsel_n(n282, r_c359, n2086);
    let n2091: ZN = zsel_n(n282, r_c369, n2087);
    let n2092: ZN = zsel_n(n282, r_c370, n2088);
    let n2093: ZN = zsel_n(n1979, zn_splat(P8::from_raw(0i32)), n2091);
    let n2094: ZN = zsel_n(n2077, n2091, n2093);
    let n2095: ZN = zsel_n(n1873, zn_splat(P8::from_raw(131072i32)), r_c359);
    let n2096: ZN = zsel_n(n1873, zn_splat(P8::from_raw(327680i32)), n1864);
    let n2097: ZN = zsel_n(n1873, zn_splat(P8::from_raw(0i32)), n2006);
    let n2098: ZN = zsel_n(n279, r_c359, n2095);
    let n2099: ZN = zsel_n(n279, n1933, n2096);
    let n2100: ZN = zsel_n(n279, n1938, n2097);
    let n2101: ZN = zsel_n(n282, r_c359, n2098);
    let n2102: ZN = zsel_n(n282, r_c369, n2099);
    let n2103: ZN = zsel_n(n282, r_c370, n2100);
    let n2104: ZN = zsel_n(n1979, zn_splat(P8::from_raw(0i32)), n2102);
    let n2105: ZN = zsel_n(n2077, n2102, n2104);
    let n2106: ZN = zsel_n(n1873, zn_splat(P8::from_raw(69510i32)), r_c357);
    let n2107: ZN = zsel_n(n1873, zn_splat(P8::from_raw(98304i32)), r_c358);
    let n2108: ZN = zsel_n(n1873, zn_splat(P8::from_raw(0i32)), r_c359);
    let n2109: ZN = zsel_n(n1873, zn_splat(P8::from_raw(-98304i32)), r_c360);
    let n2110: ZN = zsel_n(n1873, zn_splat(P8::from_raw(0i32)), n1818);
    let n2111: ZN = zsel_n(n1873, zn_splat(P8::from_raw(-327680i32)), n1945);
    let n2112: ZN = zsel_n(n279, r_c357, n2106);
    let n2113: ZN = zsel_n(n279, r_c358, n2107);
    let n2114: ZN = zsel_n(n279, r_c359, n2108);
    let n2115: ZN = zsel_n(n279, r_c360, n2109);
    let n2116: ZN = zsel_n(n279, n1933, n2110);
    let n2117: ZN = zsel_n(n279, n1938, n2111);
    let n2118: ZN = zsel_n(n282, r_c357, n2112);
    let n2119: ZN = zsel_n(n282, r_c358, n2113);
    let n2120: ZN = zsel_n(n282, r_c359, n2114);
    let n2121: ZN = zsel_n(n282, r_c360, n2115);
    let n2122: ZN = zsel_n(n282, r_c369, n2116);
    let n2123: ZN = zsel_n(n282, r_c370, n2117);
    let n2124: ZN = zsel_n(n1979, zn_splat(P8::from_raw(0i32)), n2122);
    let n2125: ZN = zsel_n(n2077, n2122, n2124);
    let n2126: ZN = zsel_n(n1873, zn_splat(P8::from_raw(-231700i32)), n1852);
    let n2127: ZN = zsel_n(n1873, zn_splat(P8::from_raw(-231700i32)), n1990);
    let n2128: ZN = zsel_n(n279, n1933, n2126);
    let n2129: ZN = zsel_n(n279, n1938, n2127);
    let n2130: ZN = zsel_n(n282, r_c369, n2128);
    let n2131: ZN = zsel_n(n282, r_c370, n2129);
    let n2132: ZN = zsel_n(n1979, zn_splat(P8::from_raw(0i32)), n2130);
    let n2133: ZN = zsel_n(n2077, n2130, n2132);
    let n2134: ZN = zsel_n(n1873, zn_splat(P8::from_raw(231700i32)), n1864);
    let n2135: ZN = zsel_n(n1873, zn_splat(P8::from_raw(-231700i32)), n2006);
    let n2136: ZN = zsel_n(n279, n1933, n2134);
    let n2137: ZN = zsel_n(n279, n1938, n2135);
    let n2138: ZN = zsel_n(n282, r_c369, n2136);
    let n2139: ZN = zsel_n(n282, r_c370, n2137);
    let n2140: ZN = zsel_n(n1979, zn_splat(P8::from_raw(0i32)), n2138);
    let n2141: ZN = zsel_n(n2077, n2138, n2140);
    let n2142: ZN = zsel_n(n1873, zn_splat(P8::from_raw(131072i32)), r_c360);
    let n2143: ZN = zsel_n(n1873, zn_splat(P8::from_raw(327680i32)), n1945);
    let n2144: ZN = zsel_n(n279, r_c360, n2142);
    let n2145: ZN = zsel_n(n279, n1938, n2143);
    let n2146: ZN = zsel_n(n282, r_c360, n2144);
    let n2147: ZN = zsel_n(n282, r_c370, n2145);
    let n2148: ZN = zsel_n(n1873, zn_splat(P8::from_raw(231700i32)), n1990);
    let n2149: ZN = zsel_n(n279, n1938, n2148);
    let n2150: ZN = zsel_n(n282, r_c370, n2149);
    let n2151: ZN = zsel_n(n1873, zn_splat(P8::from_raw(231700i32)), n2006);
    let n2152: ZN = zsel_n(n279, n1938, n2151);
    let n2153: ZN = zsel_n(n282, r_c370, n2152);
    let n2154: ZN = zsel_n(n1873, n1837, n2021);
    let n2155: ZN = zsel_n(n1873, zn_splat(P8::from_raw(0i32)), n2022);
    let n2156: ZN = zsel_n(n279, n1933, n2154);
    let n2157: ZN = zsel_n(n279, n1938, n2155);
    let n2158: ZN = zsel_n(n282, r_c369, n2156);
    let n2159: ZN = zsel_n(n282, r_c370, n2157);
    let n2160: ZN = zsel_n(n1979, zn_splat(P8::from_raw(0i32)), n2158);
    let n2161: ZN = zsel_n(n2077, n2158, n2160);
    let n2162: ZN = zsel_n(n1873, zn_splat(P8::from_raw(-327680i32)), n2031);
    let n2163: ZN = zsel_n(n1873, zn_splat(P8::from_raw(0i32)), n2032);
    let n2164: ZN = zsel_n(n279, n1933, n2162);
    let n2165: ZN = zsel_n(n279, n1938, n2163);
    let n2166: ZN = zsel_n(n282, r_c369, n2164);
    let n2167: ZN = zsel_n(n282, r_c370, n2165);
    let n2168: ZN = zsel_n(n1979, zn_splat(P8::from_raw(0i32)), n2166);
    let n2169: ZN = zsel_n(n2077, n2166, n2168);
    let n2170: ZN = zsel_n(n1873, zn_splat(P8::from_raw(327680i32)), n2039);
    let n2171: ZN = zsel_n(n1873, zn_splat(P8::from_raw(0i32)), n2040);
    let n2172: ZN = zsel_n(n279, n1933, n2170);
    let n2173: ZN = zsel_n(n279, n1938, n2171);
    let n2174: ZN = zsel_n(n282, r_c369, n2172);
    let n2175: ZN = zsel_n(n282, r_c370, n2173);
    let n2176: ZN = zsel_n(n1979, zn_splat(P8::from_raw(0i32)), n2174);
    let n2177: ZN = zsel_n(n2077, n2174, n2176);
    let n2178: ZN = zsel_n(n1873, zn_splat(P8::from_raw(0i32)), n2021);
    let n2179: ZN = zsel_n(n1873, zn_splat(P8::from_raw(-327680i32)), n2022);
    let n2180: ZN = zsel_n(n279, n1933, n2178);
    let n2181: ZN = zsel_n(n279, n1938, n2179);
    let n2182: ZN = zsel_n(n282, r_c369, n2180);
    let n2183: ZN = zsel_n(n282, r_c370, n2181);
    let n2184: ZN = zsel_n(n1979, zn_splat(P8::from_raw(0i32)), n2182);
    let n2185: ZN = zsel_n(n2077, n2182, n2184);
    let n2186: ZN = zsel_n(n1873, zn_splat(P8::from_raw(-231700i32)), n2031);
    let n2187: ZN = zsel_n(n1873, zn_splat(P8::from_raw(-231700i32)), n2032);
    let n2188: ZN = zsel_n(n279, n1933, n2186);
    let n2189: ZN = zsel_n(n279, n1938, n2187);
    let n2190: ZN = zsel_n(n282, r_c369, n2188);
    let n2191: ZN = zsel_n(n282, r_c370, n2189);
    let n2192: ZN = zsel_n(n1979, zn_splat(P8::from_raw(0i32)), n2190);
    let n2193: ZN = zsel_n(n2077, n2190, n2192);
    let n2194: ZN = zsel_n(n1873, zn_splat(P8::from_raw(231700i32)), n2039);
    let n2195: ZN = zsel_n(n1873, zn_splat(P8::from_raw(-231700i32)), n2040);
    let n2196: ZN = zsel_n(n279, n1933, n2194);
    let n2197: ZN = zsel_n(n279, n1938, n2195);
    let n2198: ZN = zsel_n(n282, r_c369, n2196);
    let n2199: ZN = zsel_n(n282, r_c370, n2197);
    let n2200: ZN = zsel_n(n1979, zn_splat(P8::from_raw(0i32)), n2198);
    let n2201: ZN = zsel_n(n2077, n2198, n2200);
    let n2202: ZN = zsel_n(n1873, zn_splat(P8::from_raw(327680i32)), n2022);
    let n2203: ZN = zsel_n(n279, n1938, n2202);
    let n2204: ZN = zsel_n(n282, r_c370, n2203);
    let n2205: ZN = zsel_n(n1873, zn_splat(P8::from_raw(231700i32)), n2032);
    let n2206: ZN = zsel_n(n279, n1938, n2205);
    let n2207: ZN = zsel_n(n282, r_c370, n2206);
    let n2208: ZN = zsel_n(n1873, zn_splat(P8::from_raw(231700i32)), n2040);
    let n2209: ZN = zsel_n(n279, n1938, n2208);
    let n2210: ZN = zsel_n(n282, r_c370, n2209);
    let n2212: ZW = zw_cellmix_n(39u64, r_c39, 1542469173u64);
    let n2213: ZW = zw_cellmix_n(39u64, r_c39, 668265263u64);
    let n2214: ZW = zw_add(zw_splat(0u64), n2212);
    let n2215: ZW = zw_add(zw_splat(0u64), n2213);
    let n2216: ZW = zw_cellmix_n(84u64, n115, 1542469173u64);
    let n2217: ZW = zw_cellmix_n(84u64, n115, 668265263u64);
    let n2218: ZW = zw_add(n2214, n2216);
    let n2219: ZW = zw_add(n2215, n2217);
    let n2220: ZW = zw_cellmix_n(85u64, n215, 1542469173u64);
    let n2221: ZW = zw_cellmix_n(85u64, n215, 668265263u64);
    let n2222: ZW = zw_add(n2218, n2220);
    let n2223: ZW = zw_add(n2219, n2221);
    let n2224: ZW = zw_cellmix_n(86u64, n214, 1542469173u64);
    let n2225: ZW = zw_cellmix_n(86u64, n214, 668265263u64);
    let n2226: ZW = zw_add(n2222, n2224);
    let n2227: ZW = zw_add(n2223, n2225);
    let n2228: ZW = zw_cellmix_n(87u64, r_c87, 1542469173u64);
    let n2229: ZW = zw_cellmix_n(87u64, r_c87, 668265263u64);
    let n2230: ZW = zw_add(n2226, n2228);
    let n2231: ZW = zw_add(n2227, n2229);
    let n2232: ZW = zw_cellmix_n(241u64, n290, 1542469173u64);
    let n2233: ZW = zw_cellmix_n(241u64, n290, 668265263u64);
    let n2234: ZW = zw_add(n2230, n2232);
    let n2235: ZW = zw_add(n2231, n2233);
    let n2236: ZW = zw_cellmix_n(254u64, n291, 1542469173u64);
    let n2237: ZW = zw_cellmix_n(254u64, n291, 668265263u64);
    let n2238: ZW = zw_add(n2234, n2236);
    let n2239: ZW = zw_add(n2235, n2237);
    let n2240: ZW = zw_cellmix_n(302u64, n486, 1542469173u64);
    let n2241: ZW = zw_cellmix_n(302u64, n486, 668265263u64);
    let n2242: ZW = zw_add(n2238, n2240);
    let n2243: ZW = zw_add(n2239, n2241);
    let n2244: ZW = zw_cellmix_n(368u64, n416, 1542469173u64);
    let n2245: ZW = zw_cellmix_n(368u64, n416, 668265263u64);
    let n2246: ZW = zw_add(n2242, n2244);
    let n2247: ZW = zw_add(n2243, n2245);
    let n2248: ZW = zw_cellmix_n(369u64, n487, 1542469173u64);
    let n2249: ZW = zw_cellmix_n(369u64, n487, 668265263u64);
    let n2250: ZW = zw_add(n2246, n2248);
    let n2251: ZW = zw_add(n2247, n2249);
    let n2252: ZW = zw_cellmix_n(20u64, r_c20, 1542469173u64);
    let n2253: ZW = zw_cellmix_n(20u64, r_c20, 668265263u64);
    let n2254: ZW = zw_add(n2250, n2252);
    let n2255: ZW = zw_add(n2251, n2253);
    let n2256: ZW = zw_cellmix_b(41u64, r_c41, 1542469173u64);
    let n2257: ZW = zw_cellmix_b(41u64, r_c41, 668265263u64);
    let n2258: ZW = zw_add(n2254, n2256);
    let n2259: ZW = zw_add(n2255, n2257);
    let n2260: ZW = zw_cellmix_n(282u64, n278, 1542469173u64);
    let n2261: ZW = zw_cellmix_n(282u64, n278, 668265263u64);
    let n2262: ZW = zw_add(n2258, n2260);
    let n2263: ZW = zw_add(n2259, n2261);
    let n2264: ZW = zw_cellmix_n(284u64, n281, 1542469173u64);
    let n2265: ZW = zw_cellmix_n(284u64, n281, 668265263u64);
    let n2266: ZW = zw_add(n2262, n2264);
    let n2267: ZW = zw_add(n2263, n2265);
    let n2268: ZW = zw_cellmix_n(285u64, zn_splat(P8::from_raw(65536i32)), 1542469173u64);
    let n2269: ZW = zw_cellmix_n(285u64, zn_splat(P8::from_raw(65536i32)), 668265263u64);
    let n2270: ZW = zw_add(n2266, n2268);
    let n2271: ZW = zw_add(n2267, n2269);
    let n2272: ZW = zw_cellmix_n(287u64, n818, 1542469173u64);
    let n2273: ZW = zw_cellmix_n(287u64, n818, 668265263u64);
    let n2274: ZW = zw_add(n2270, n2272);
    let n2275: ZW = zw_add(n2271, n2273);
    let n2276: ZW = zw_cellmix_b(294u64, zb_splat(false), 1542469173u64);
    let n2277: ZW = zw_cellmix_b(294u64, zb_splat(false), 668265263u64);
    let n2278: ZW = zw_add(n2274, n2276);
    let n2279: ZW = zw_add(n2275, n2277);
    let n2280: ZW = zw_cellmix_b(295u64, zb_splat(false), 1542469173u64);
    let n2281: ZW = zw_cellmix_b(295u64, zb_splat(false), 668265263u64);
    let n2282: ZW = zw_add(n2278, n2280);
    let n2283: ZW = zw_add(n2279, n2281);
    let n2284: ZW = zw_cellmix_n(301u64, n903, 1542469173u64);
    let n2285: ZW = zw_cellmix_n(301u64, n903, 668265263u64);
    let n2286: ZW = zw_add(n2282, n2284);
    let n2287: ZW = zw_add(n2283, n2285);
    let n2288: ZW = zw_cellmix_n(358u64, r_c357, 1542469173u64);
    let n2289: ZW = zw_cellmix_n(358u64, r_c357, 668265263u64);
    let n2290: ZW = zw_add(n2286, n2288);
    let n2291: ZW = zw_add(n2287, n2289);
    let n2292: ZW = zw_cellmix_n(359u64, r_c358, 1542469173u64);
    let n2293: ZW = zw_cellmix_n(359u64, r_c358, 668265263u64);
    let n2294: ZW = zw_add(n2290, n2292);
    let n2295: ZW = zw_add(n2291, n2293);
    let n2296: ZW = zw_cellmix_n(360u64, r_c359, 1542469173u64);
    let n2297: ZW = zw_cellmix_n(360u64, r_c359, 668265263u64);
    let n2298: ZW = zw_add(n2294, n2296);
    let n2299: ZW = zw_add(n2295, n2297);
    let n2300: ZW = zw_cellmix_n(361u64, r_c360, 1542469173u64);
    let n2301: ZW = zw_cellmix_n(361u64, r_c360, 668265263u64);
    let n2302: ZW = zw_add(n2298, n2300);
    let n2303: ZW = zw_add(n2299, n2301);
    let n2304: ZW = zw_cellmix_b(362u64, n892, 1542469173u64);
    let n2305: ZW = zw_cellmix_b(362u64, n892, 668265263u64);
    let n2306: ZW = zw_add(n2302, n2304);
    let n2307: ZW = zw_add(n2303, n2305);
    let n2308: ZW = zw_cellmix_n(370u64, n904, 1542469173u64);
    let n2309: ZW = zw_cellmix_n(370u64, n904, 668265263u64);
    let n2310: ZW = zw_add(n2306, n2308);
    let n2311: ZW = zw_add(n2307, n2309);
    let n2312: ZW = zw_cellmix_n(371u64, n894, 1542469173u64);
    let n2313: ZW = zw_cellmix_n(371u64, n894, 668265263u64);
    let n2314: ZW = zw_add(n2310, n2312);
    let n2315: ZW = zw_add(n2311, n2313);
    let n2316: ZW = zw_cellmix_b(362u64, n934, 1542469173u64);
    let n2317: ZW = zw_cellmix_b(362u64, n934, 668265263u64);
    let n2318: ZW = zw_add(n2302, n2316);
    let n2319: ZW = zw_add(n2303, n2317);
    let n2320: ZW = zw_cellmix_n(370u64, n938, 1542469173u64);
    let n2321: ZW = zw_cellmix_n(370u64, n938, 668265263u64);
    let n2322: ZW = zw_add(n2318, n2320);
    let n2323: ZW = zw_add(n2319, n2321);
    let n2324: ZW = zw_cellmix_n(371u64, n936, 1542469173u64);
    let n2325: ZW = zw_cellmix_n(371u64, n936, 668265263u64);
    let n2326: ZW = zw_add(n2322, n2324);
    let n2327: ZW = zw_add(n2323, n2325);
    let n2328: ZW = zw_cellmix_b(362u64, n956, 1542469173u64);
    let n2329: ZW = zw_cellmix_b(362u64, n956, 668265263u64);
    let n2330: ZW = zw_add(n2302, n2328);
    let n2331: ZW = zw_add(n2303, n2329);
    let n2332: ZW = zw_cellmix_n(370u64, n960, 1542469173u64);
    let n2333: ZW = zw_cellmix_n(370u64, n960, 668265263u64);
    let n2334: ZW = zw_add(n2330, n2332);
    let n2335: ZW = zw_add(n2331, n2333);
    let n2336: ZW = zw_cellmix_n(371u64, n958, 1542469173u64);
    let n2337: ZW = zw_cellmix_n(371u64, n958, 668265263u64);
    let n2338: ZW = zw_add(n2334, n2336);
    let n2339: ZW = zw_add(n2335, n2337);
    let n2340: ZW = zw_cellmix_n(287u64, n964, 1542469173u64);
    let n2341: ZW = zw_cellmix_n(287u64, n964, 668265263u64);
    let n2342: ZW = zw_add(n2270, n2340);
    let n2343: ZW = zw_add(n2271, n2341);
    let n2344: ZW = zw_add(n2342, n2276);
    let n2345: ZW = zw_add(n2343, n2277);
    let n2346: ZW = zw_cellmix_b(295u64, zb_splat(true), 1542469173u64);
    let n2347: ZW = zw_cellmix_b(295u64, zb_splat(true), 668265263u64);
    let n2348: ZW = zw_add(n2344, n2346);
    let n2349: ZW = zw_add(n2345, n2347);
    let n2350: ZW = zw_add(n2348, n2284);
    let n2351: ZW = zw_add(n2349, n2285);
    let n2352: ZW = zw_add(n2350, n2288);
    let n2353: ZW = zw_add(n2351, n2289);
    let n2354: ZW = zw_add(n2352, n2292);
    let n2355: ZW = zw_add(n2353, n2293);
    let n2356: ZW = zw_add(n2354, n2296);
    let n2357: ZW = zw_add(n2355, n2297);
    let n2358: ZW = zw_add(n2356, n2300);
    let n2359: ZW = zw_add(n2357, n2301);
    let n2360: ZW = zw_add(n2358, n2304);
    let n2361: ZW = zw_add(n2359, n2305);
    let n2362: ZW = zw_cellmix_n(370u64, n968, 1542469173u64);
    let n2363: ZW = zw_cellmix_n(370u64, n968, 668265263u64);
    let n2364: ZW = zw_add(n2360, n2362);
    let n2365: ZW = zw_add(n2361, n2363);
    let n2366: ZW = zw_cellmix_n(371u64, n966, 1542469173u64);
    let n2367: ZW = zw_cellmix_n(371u64, n966, 668265263u64);
    let n2368: ZW = zw_add(n2364, n2366);
    let n2369: ZW = zw_add(n2365, n2367);
    let n2370: ZW = zw_add(n2358, n2316);
    let n2371: ZW = zw_add(n2359, n2317);
    let n2372: ZW = zw_cellmix_n(370u64, n974, 1542469173u64);
    let n2373: ZW = zw_cellmix_n(370u64, n974, 668265263u64);
    let n2374: ZW = zw_add(n2370, n2372);
    let n2375: ZW = zw_add(n2371, n2373);
    let n2376: ZW = zw_cellmix_n(371u64, n972, 1542469173u64);
    let n2377: ZW = zw_cellmix_n(371u64, n972, 668265263u64);
    let n2378: ZW = zw_add(n2374, n2376);
    let n2379: ZW = zw_add(n2375, n2377);
    let n2380: ZW = zw_add(n2358, n2328);
    let n2381: ZW = zw_add(n2359, n2329);
    let n2382: ZW = zw_cellmix_n(370u64, n980, 1542469173u64);
    let n2383: ZW = zw_cellmix_n(370u64, n980, 668265263u64);
    let n2384: ZW = zw_add(n2380, n2382);
    let n2385: ZW = zw_add(n2381, n2383);
    let n2386: ZW = zw_cellmix_n(371u64, n978, 1542469173u64);
    let n2387: ZW = zw_cellmix_n(371u64, n978, 668265263u64);
    let n2388: ZW = zw_add(n2384, n2386);
    let n2389: ZW = zw_add(n2385, n2387);
    let n2390: ZW = zw_cellmix_n(20u64, n988, 1542469173u64);
    let n2391: ZW = zw_cellmix_n(20u64, n988, 668265263u64);
    let n2392: ZW = zw_add(n2250, n2390);
    let n2393: ZW = zw_add(n2251, n2391);
    let n2394: ZW = zw_cellmix_b(41u64, n989, 1542469173u64);
    let n2395: ZW = zw_cellmix_b(41u64, n989, 668265263u64);
    let n2396: ZW = zw_add(n2392, n2394);
    let n2397: ZW = zw_add(n2393, n2395);
    let n2398: ZW = zw_cellmix_n(282u64, n990, 1542469173u64);
    let n2399: ZW = zw_cellmix_n(282u64, n990, 668265263u64);
    let n2400: ZW = zw_add(n2396, n2398);
    let n2401: ZW = zw_add(n2397, n2399);
    let n2402: ZW = zw_cellmix_n(284u64, n991, 1542469173u64);
    let n2403: ZW = zw_cellmix_n(284u64, n991, 668265263u64);
    let n2404: ZW = zw_add(n2400, n2402);
    let n2405: ZW = zw_add(n2401, n2403);
    let n2406: ZW = zw_cellmix_n(285u64, n992, 1542469173u64);
    let n2407: ZW = zw_cellmix_n(285u64, n992, 668265263u64);
    let n2408: ZW = zw_add(n2404, n2406);
    let n2409: ZW = zw_add(n2405, n2407);
    let n2410: ZW = zw_add(n2408, n2272);
    let n2411: ZW = zw_add(n2409, n2273);
    let n2412: ZW = zw_cellmix_b(294u64, zb_splat(true), 1542469173u64);
    let n2413: ZW = zw_cellmix_b(294u64, zb_splat(true), 668265263u64);
    let n2414: ZW = zw_add(n2410, n2412);
    let n2415: ZW = zw_add(n2411, n2413);
    let n2416: ZW = zw_add(n2414, n2280);
    let n2417: ZW = zw_add(n2415, n2281);
    let n2418: ZW = zw_cellmix_n(301u64, n1005, 1542469173u64);
    let n2419: ZW = zw_cellmix_n(301u64, n1005, 668265263u64);
    let n2420: ZW = zw_add(n2416, n2418);
    let n2421: ZW = zw_add(n2417, n2419);
    let n2422: ZW = zw_cellmix_n(358u64, n993, 1542469173u64);
    let n2423: ZW = zw_cellmix_n(358u64, n993, 668265263u64);
    let n2424: ZW = zw_add(n2420, n2422);
    let n2425: ZW = zw_add(n2421, n2423);
    let n2426: ZW = zw_cellmix_n(359u64, n1000, 1542469173u64);
    let n2427: ZW = zw_cellmix_n(359u64, n1000, 668265263u64);
    let n2428: ZW = zw_add(n2424, n2426);
    let n2429: ZW = zw_add(n2425, n2427);
    let n2430: ZW = zw_cellmix_n(360u64, n1001, 1542469173u64);
    let n2431: ZW = zw_cellmix_n(360u64, n1001, 668265263u64);
    let n2432: ZW = zw_add(n2428, n2430);
    let n2433: ZW = zw_add(n2429, n2431);
    let n2434: ZW = zw_cellmix_n(361u64, n994, 1542469173u64);
    let n2435: ZW = zw_cellmix_n(361u64, n994, 668265263u64);
    let n2436: ZW = zw_add(n2432, n2434);
    let n2437: ZW = zw_add(n2433, n2435);
    let n2438: ZW = zw_add(n2436, n2304);
    let n2439: ZW = zw_add(n2437, n2305);
    let n2440: ZW = zw_cellmix_n(370u64, n1006, 1542469173u64);
    let n2441: ZW = zw_cellmix_n(370u64, n1006, 668265263u64);
    let n2442: ZW = zw_add(n2438, n2440);
    let n2443: ZW = zw_add(n2439, n2441);
    let n2444: ZW = zw_cellmix_n(371u64, n1003, 1542469173u64);
    let n2445: ZW = zw_cellmix_n(371u64, n1003, 668265263u64);
    let n2446: ZW = zw_add(n2442, n2444);
    let n2447: ZW = zw_add(n2443, n2445);
    let n2448: ZW = zw_cellmix_n(359u64, n1011, 1542469173u64);
    let n2449: ZW = zw_cellmix_n(359u64, n1011, 668265263u64);
    let n2450: ZW = zw_add(n2424, n2448);
    let n2451: ZW = zw_add(n2425, n2449);
    let n2452: ZW = zw_cellmix_n(360u64, n1012, 1542469173u64);
    let n2453: ZW = zw_cellmix_n(360u64, n1012, 668265263u64);
    let n2454: ZW = zw_add(n2450, n2452);
    let n2455: ZW = zw_add(n2451, n2453);
    let n2456: ZW = zw_add(n2454, n2434);
    let n2457: ZW = zw_add(n2455, n2435);
    let n2458: ZW = zw_add(n2456, n2316);
    let n2459: ZW = zw_add(n2457, n2317);
    let n2460: ZW = zw_cellmix_n(370u64, n1016, 1542469173u64);
    let n2461: ZW = zw_cellmix_n(370u64, n1016, 668265263u64);
    let n2462: ZW = zw_add(n2458, n2460);
    let n2463: ZW = zw_add(n2459, n2461);
    let n2464: ZW = zw_cellmix_n(371u64, n1014, 1542469173u64);
    let n2465: ZW = zw_cellmix_n(371u64, n1014, 668265263u64);
    let n2466: ZW = zw_add(n2462, n2464);
    let n2467: ZW = zw_add(n2463, n2465);
    let n2468: ZW = zw_cellmix_n(360u64, n1020, 1542469173u64);
    let n2469: ZW = zw_cellmix_n(360u64, n1020, 668265263u64);
    let n2470: ZW = zw_add(n2450, n2468);
    let n2471: ZW = zw_add(n2451, n2469);
    let n2472: ZW = zw_add(n2470, n2434);
    let n2473: ZW = zw_add(n2471, n2435);
    let n2474: ZW = zw_add(n2472, n2328);
    let n2475: ZW = zw_add(n2473, n2329);
    let n2476: ZW = zw_cellmix_n(370u64, n1024, 1542469173u64);
    let n2477: ZW = zw_cellmix_n(370u64, n1024, 668265263u64);
    let n2478: ZW = zw_add(n2474, n2476);
    let n2479: ZW = zw_add(n2475, n2477);
    let n2480: ZW = zw_cellmix_n(371u64, n1022, 1542469173u64);
    let n2481: ZW = zw_cellmix_n(371u64, n1022, 668265263u64);
    let n2482: ZW = zw_add(n2478, n2480);
    let n2483: ZW = zw_add(n2479, n2481);
    let n2484: ZW = zw_cellmix_n(358u64, n1028, 1542469173u64);
    let n2485: ZW = zw_cellmix_n(358u64, n1028, 668265263u64);
    let n2486: ZW = zw_add(n2420, n2484);
    let n2487: ZW = zw_add(n2421, n2485);
    let n2488: ZW = zw_cellmix_n(359u64, n1034, 1542469173u64);
    let n2489: ZW = zw_cellmix_n(359u64, n1034, 668265263u64);
    let n2490: ZW = zw_add(n2486, n2488);
    let n2491: ZW = zw_add(n2487, n2489);
    let n2492: ZW = zw_cellmix_n(360u64, n1035, 1542469173u64);
    let n2493: ZW = zw_cellmix_n(360u64, n1035, 668265263u64);
    let n2494: ZW = zw_add(n2490, n2492);
    let n2495: ZW = zw_add(n2491, n2493);
    let n2496: ZW = zw_cellmix_n(361u64, n1029, 1542469173u64);
    let n2497: ZW = zw_cellmix_n(361u64, n1029, 668265263u64);
    let n2498: ZW = zw_add(n2494, n2496);
    let n2499: ZW = zw_add(n2495, n2497);
    let n2500: ZW = zw_add(n2498, n2304);
    let n2501: ZW = zw_add(n2499, n2305);
    let n2502: ZW = zw_cellmix_n(370u64, n1039, 1542469173u64);
    let n2503: ZW = zw_cellmix_n(370u64, n1039, 668265263u64);
    let n2504: ZW = zw_add(n2500, n2502);
    let n2505: ZW = zw_add(n2501, n2503);
    let n2506: ZW = zw_cellmix_n(371u64, n1037, 1542469173u64);
    let n2507: ZW = zw_cellmix_n(371u64, n1037, 668265263u64);
    let n2508: ZW = zw_add(n2504, n2506);
    let n2509: ZW = zw_add(n2505, n2507);
    let n2510: ZW = zw_add(n2486, n2448);
    let n2511: ZW = zw_add(n2487, n2449);
    let n2512: ZW = zw_add(n2510, n2452);
    let n2513: ZW = zw_add(n2511, n2453);
    let n2514: ZW = zw_add(n2512, n2496);
    let n2515: ZW = zw_add(n2513, n2497);
    let n2516: ZW = zw_add(n2514, n2316);
    let n2517: ZW = zw_add(n2515, n2317);
    let n2518: ZW = zw_cellmix_n(370u64, n1045, 1542469173u64);
    let n2519: ZW = zw_cellmix_n(370u64, n1045, 668265263u64);
    let n2520: ZW = zw_add(n2516, n2518);
    let n2521: ZW = zw_add(n2517, n2519);
    let n2522: ZW = zw_cellmix_n(371u64, n1043, 1542469173u64);
    let n2523: ZW = zw_cellmix_n(371u64, n1043, 668265263u64);
    let n2524: ZW = zw_add(n2520, n2522);
    let n2525: ZW = zw_add(n2521, n2523);
    let n2526: ZW = zw_add(n2510, n2468);
    let n2527: ZW = zw_add(n2511, n2469);
    let n2528: ZW = zw_add(n2526, n2496);
    let n2529: ZW = zw_add(n2527, n2497);
    let n2530: ZW = zw_add(n2528, n2328);
    let n2531: ZW = zw_add(n2529, n2329);
    let n2532: ZW = zw_cellmix_n(370u64, n1051, 1542469173u64);
    let n2533: ZW = zw_cellmix_n(370u64, n1051, 668265263u64);
    let n2534: ZW = zw_add(n2530, n2532);
    let n2535: ZW = zw_add(n2531, n2533);
    let n2536: ZW = zw_cellmix_n(371u64, n1049, 1542469173u64);
    let n2537: ZW = zw_cellmix_n(371u64, n1049, 668265263u64);
    let n2538: ZW = zw_add(n2534, n2536);
    let n2539: ZW = zw_add(n2535, n2537);
    let n2540: ZW = zw_cellmix_n(361u64, n1053, 1542469173u64);
    let n2541: ZW = zw_cellmix_n(361u64, n1053, 668265263u64);
    let n2542: ZW = zw_add(n2494, n2540);
    let n2543: ZW = zw_add(n2495, n2541);
    let n2544: ZW = zw_add(n2542, n2304);
    let n2545: ZW = zw_add(n2543, n2305);
    let n2546: ZW = zw_add(n2544, n2502);
    let n2547: ZW = zw_add(n2545, n2503);
    let n2548: ZW = zw_cellmix_n(371u64, n1055, 1542469173u64);
    let n2549: ZW = zw_cellmix_n(371u64, n1055, 668265263u64);
    let n2550: ZW = zw_add(n2546, n2548);
    let n2551: ZW = zw_add(n2547, n2549);
    let n2552: ZW = zw_add(n2512, n2540);
    let n2553: ZW = zw_add(n2513, n2541);
    let n2554: ZW = zw_add(n2552, n2316);
    let n2555: ZW = zw_add(n2553, n2317);
    let n2556: ZW = zw_add(n2554, n2518);
    let n2557: ZW = zw_add(n2555, n2519);
    let n2558: ZW = zw_cellmix_n(371u64, n1057, 1542469173u64);
    let n2559: ZW = zw_cellmix_n(371u64, n1057, 668265263u64);
    let n2560: ZW = zw_add(n2556, n2558);
    let n2561: ZW = zw_add(n2557, n2559);
    let n2562: ZW = zw_add(n2526, n2540);
    let n2563: ZW = zw_add(n2527, n2541);
    let n2564: ZW = zw_add(n2562, n2328);
    let n2565: ZW = zw_add(n2563, n2329);
    let n2566: ZW = zw_add(n2564, n2532);
    let n2567: ZW = zw_add(n2565, n2533);
    let n2568: ZW = zw_cellmix_n(371u64, n1059, 1542469173u64);
    let n2569: ZW = zw_cellmix_n(371u64, n1059, 668265263u64);
    let n2570: ZW = zw_add(n2566, n2568);
    let n2571: ZW = zw_add(n2567, n2569);
    let n2572: ZW = zw_add(n2408, n2340);
    let n2573: ZW = zw_add(n2409, n2341);
    let n2574: ZW = zw_add(n2572, n2412);
    let n2575: ZW = zw_add(n2573, n2413);
    let n2576: ZW = zw_add(n2574, n2346);
    let n2577: ZW = zw_add(n2575, n2347);
    let n2578: ZW = zw_add(n2576, n2418);
    let n2579: ZW = zw_add(n2577, n2419);
    let n2580: ZW = zw_add(n2578, n2422);
    let n2581: ZW = zw_add(n2579, n2423);
    let n2582: ZW = zw_add(n2580, n2426);
    let n2583: ZW = zw_add(n2581, n2427);
    let n2584: ZW = zw_add(n2582, n2430);
    let n2585: ZW = zw_add(n2583, n2431);
    let n2586: ZW = zw_add(n2584, n2434);
    let n2587: ZW = zw_add(n2585, n2435);
    let n2588: ZW = zw_add(n2586, n2304);
    let n2589: ZW = zw_add(n2587, n2305);
    let n2590: ZW = zw_cellmix_n(370u64, n1065, 1542469173u64);
    let n2591: ZW = zw_cellmix_n(370u64, n1065, 668265263u64);
    let n2592: ZW = zw_add(n2588, n2590);
    let n2593: ZW = zw_add(n2589, n2591);
    let n2594: ZW = zw_cellmix_n(371u64, n1063, 1542469173u64);
    let n2595: ZW = zw_cellmix_n(371u64, n1063, 668265263u64);
    let n2596: ZW = zw_add(n2592, n2594);
    let n2597: ZW = zw_add(n2593, n2595);
    let n2598: ZW = zw_add(n2580, n2448);
    let n2599: ZW = zw_add(n2581, n2449);
    let n2600: ZW = zw_add(n2598, n2452);
    let n2601: ZW = zw_add(n2599, n2453);
    let n2602: ZW = zw_add(n2600, n2434);
    let n2603: ZW = zw_add(n2601, n2435);
    let n2604: ZW = zw_add(n2602, n2316);
    let n2605: ZW = zw_add(n2603, n2317);
    let n2606: ZW = zw_cellmix_n(370u64, n1071, 1542469173u64);
    let n2607: ZW = zw_cellmix_n(370u64, n1071, 668265263u64);
    let n2608: ZW = zw_add(n2604, n2606);
    let n2609: ZW = zw_add(n2605, n2607);
    let n2610: ZW = zw_cellmix_n(371u64, n1069, 1542469173u64);
    let n2611: ZW = zw_cellmix_n(371u64, n1069, 668265263u64);
    let n2612: ZW = zw_add(n2608, n2610);
    let n2613: ZW = zw_add(n2609, n2611);
    let n2614: ZW = zw_add(n2598, n2468);
    let n2615: ZW = zw_add(n2599, n2469);
    let n2616: ZW = zw_add(n2614, n2434);
    let n2617: ZW = zw_add(n2615, n2435);
    let n2618: ZW = zw_add(n2616, n2328);
    let n2619: ZW = zw_add(n2617, n2329);
    let n2620: ZW = zw_cellmix_n(370u64, n1077, 1542469173u64);
    let n2621: ZW = zw_cellmix_n(370u64, n1077, 668265263u64);
    let n2622: ZW = zw_add(n2618, n2620);
    let n2623: ZW = zw_add(n2619, n2621);
    let n2624: ZW = zw_cellmix_n(371u64, n1075, 1542469173u64);
    let n2625: ZW = zw_cellmix_n(371u64, n1075, 668265263u64);
    let n2626: ZW = zw_add(n2622, n2624);
    let n2627: ZW = zw_add(n2623, n2625);
    let n2628: ZW = zw_add(n2578, n2484);
    let n2629: ZW = zw_add(n2579, n2485);
    let n2630: ZW = zw_add(n2628, n2488);
    let n2631: ZW = zw_add(n2629, n2489);
    let n2632: ZW = zw_add(n2630, n2492);
    let n2633: ZW = zw_add(n2631, n2493);
    let n2634: ZW = zw_add(n2632, n2496);
    let n2635: ZW = zw_add(n2633, n2497);
    let n2636: ZW = zw_add(n2634, n2304);
    let n2637: ZW = zw_add(n2635, n2305);
    let n2638: ZW = zw_cellmix_n(370u64, n1083, 1542469173u64);
    let n2639: ZW = zw_cellmix_n(370u64, n1083, 668265263u64);
    let n2640: ZW = zw_add(n2636, n2638);
    let n2641: ZW = zw_add(n2637, n2639);
    let n2642: ZW = zw_cellmix_n(371u64, n1081, 1542469173u64);
    let n2643: ZW = zw_cellmix_n(371u64, n1081, 668265263u64);
    let n2644: ZW = zw_add(n2640, n2642);
    let n2645: ZW = zw_add(n2641, n2643);
    let n2646: ZW = zw_add(n2628, n2448);
    let n2647: ZW = zw_add(n2629, n2449);
    let n2648: ZW = zw_add(n2646, n2452);
    let n2649: ZW = zw_add(n2647, n2453);
    let n2650: ZW = zw_add(n2648, n2496);
    let n2651: ZW = zw_add(n2649, n2497);
    let n2652: ZW = zw_add(n2650, n2316);
    let n2653: ZW = zw_add(n2651, n2317);
    let n2654: ZW = zw_cellmix_n(370u64, n1089, 1542469173u64);
    let n2655: ZW = zw_cellmix_n(370u64, n1089, 668265263u64);
    let n2656: ZW = zw_add(n2652, n2654);
    let n2657: ZW = zw_add(n2653, n2655);
    let n2658: ZW = zw_cellmix_n(371u64, n1087, 1542469173u64);
    let n2659: ZW = zw_cellmix_n(371u64, n1087, 668265263u64);
    let n2660: ZW = zw_add(n2656, n2658);
    let n2661: ZW = zw_add(n2657, n2659);
    let n2662: ZW = zw_add(n2646, n2468);
    let n2663: ZW = zw_add(n2647, n2469);
    let n2664: ZW = zw_add(n2662, n2496);
    let n2665: ZW = zw_add(n2663, n2497);
    let n2666: ZW = zw_add(n2664, n2328);
    let n2667: ZW = zw_add(n2665, n2329);
    let n2668: ZW = zw_cellmix_n(370u64, n1095, 1542469173u64);
    let n2669: ZW = zw_cellmix_n(370u64, n1095, 668265263u64);
    let n2670: ZW = zw_add(n2666, n2668);
    let n2671: ZW = zw_add(n2667, n2669);
    let n2672: ZW = zw_cellmix_n(371u64, n1093, 1542469173u64);
    let n2673: ZW = zw_cellmix_n(371u64, n1093, 668265263u64);
    let n2674: ZW = zw_add(n2670, n2672);
    let n2675: ZW = zw_add(n2671, n2673);
    let n2676: ZW = zw_add(n2632, n2540);
    let n2677: ZW = zw_add(n2633, n2541);
    let n2678: ZW = zw_add(n2676, n2304);
    let n2679: ZW = zw_add(n2677, n2305);
    let n2680: ZW = zw_add(n2678, n2638);
    let n2681: ZW = zw_add(n2679, n2639);
    let n2682: ZW = zw_cellmix_n(371u64, n1097, 1542469173u64);
    let n2683: ZW = zw_cellmix_n(371u64, n1097, 668265263u64);
    let n2684: ZW = zw_add(n2680, n2682);
    let n2685: ZW = zw_add(n2681, n2683);
    let n2686: ZW = zw_add(n2648, n2540);
    let n2687: ZW = zw_add(n2649, n2541);
    let n2688: ZW = zw_add(n2686, n2316);
    let n2689: ZW = zw_add(n2687, n2317);
    let n2690: ZW = zw_add(n2688, n2654);
    let n2691: ZW = zw_add(n2689, n2655);
    let n2692: ZW = zw_cellmix_n(371u64, n1099, 1542469173u64);
    let n2693: ZW = zw_cellmix_n(371u64, n1099, 668265263u64);
    let n2694: ZW = zw_add(n2690, n2692);
    let n2695: ZW = zw_add(n2691, n2693);
    let n2696: ZW = zw_add(n2662, n2540);
    let n2697: ZW = zw_add(n2663, n2541);
    let n2698: ZW = zw_add(n2696, n2328);
    let n2699: ZW = zw_add(n2697, n2329);
    let n2700: ZW = zw_add(n2698, n2668);
    let n2701: ZW = zw_add(n2699, n2669);
    let n2702: ZW = zw_cellmix_n(371u64, n1101, 1542469173u64);
    let n2703: ZW = zw_cellmix_n(371u64, n1101, 668265263u64);
    let n2704: ZW = zw_add(n2700, n2702);
    let n2705: ZW = zw_add(n2701, n2703);
    let n2706: ZW = zw_add(zw_splat(0u64), n2216);
    let n2707: ZW = zw_add(zw_splat(0u64), n2217);
    let n2708: ZW = zw_add(n2706, n2220);
    let n2709: ZW = zw_add(n2707, n2221);
    let n2710: ZW = zw_add(n2708, n2224);
    let n2711: ZW = zw_add(n2709, n2225);
    let n2712: ZW = zw_cellmix_n(87u64, n1102, 1542469173u64);
    let n2713: ZW = zw_cellmix_n(87u64, n1102, 668265263u64);
    let n2714: ZW = zw_add(n2710, n2712);
    let n2715: ZW = zw_add(n2711, n2713);
    let n2716: ZW = zw_cellmix_n(240u64, n290, 1542469173u64);
    let n2717: ZW = zw_cellmix_n(240u64, n290, 668265263u64);
    let n2718: ZW = zw_add(n2714, n2716);
    let n2719: ZW = zw_add(n2715, n2717);
    let n2720: ZW = zw_cellmix_n(253u64, n291, 1542469173u64);
    let n2721: ZW = zw_cellmix_n(253u64, n291, 668265263u64);
    let n2722: ZW = zw_add(n2718, n2720);
    let n2723: ZW = zw_add(n2719, n2721);
    let n2724: ZW = zw_add(n2722, n2252);
    let n2725: ZW = zw_add(n2723, n2253);
    let n2726: ZW = zw_add(n2724, n2256);
    let n2727: ZW = zw_add(n2725, n2257);
    let n2728: ZW = zw_add(n2722, n2390);
    let n2729: ZW = zw_add(n2723, n2391);
    let n2730: ZW = zw_add(n2728, n2394);
    let n2731: ZW = zw_add(n2729, n2395);
    let n2732: ZW = zw_cellmix_n(87u64, n1847, 1542469173u64);
    let n2733: ZW = zw_cellmix_n(87u64, n1847, 668265263u64);
    let n2734: ZW = zw_add(n2710, n2732);
    let n2735: ZW = zw_add(n2711, n2733);
    let n2736: ZW = zw_add(n2734, n2716);
    let n2737: ZW = zw_add(n2735, n2717);
    let n2738: ZW = zw_add(n2736, n2720);
    let n2739: ZW = zw_add(n2737, n2721);
    let n2740: ZW = zw_add(n2738, n2252);
    let n2741: ZW = zw_add(n2739, n2253);
    let n2742: ZW = zw_add(n2740, n2256);
    let n2743: ZW = zw_add(n2741, n2257);
    let n2744: ZW = zw_cellmix_n(20u64, n1876, 1542469173u64);
    let n2745: ZW = zw_cellmix_n(20u64, n1876, 668265263u64);
    let n2746: ZW = zw_add(n2738, n2744);
    let n2747: ZW = zw_add(n2739, n2745);
    let n2748: ZW = zw_cellmix_b(41u64, n1877, 1542469173u64);
    let n2749: ZW = zw_cellmix_b(41u64, n1877, 668265263u64);
    let n2750: ZW = zw_add(n2746, n2748);
    let n2751: ZW = zw_add(n2747, n2749);
    let n2752: ZW = zw_cellmix_b(38u64, n1884, 1542469173u64);
    let n2753: ZW = zw_cellmix_b(38u64, n1884, 668265263u64);
    let n2754: ZW = zw_add(zw_splat(0u64), n2752);
    let n2755: ZW = zw_add(zw_splat(0u64), n2753);
    let n2756: ZW = zw_cellmix_n(39u64, n1888, 1542469173u64);
    let n2757: ZW = zw_cellmix_n(39u64, n1888, 668265263u64);
    let n2758: ZW = zw_add(n2754, n2756);
    let n2759: ZW = zw_add(n2755, n2757);
    let n2760: ZW = zw_add(n2758, n2216);
    let n2761: ZW = zw_add(n2759, n2217);
    let n2762: ZW = zw_add(n2760, n2220);
    let n2763: ZW = zw_add(n2761, n2221);
    let n2764: ZW = zw_add(n2762, n2224);
    let n2765: ZW = zw_add(n2763, n2225);
    let n2766: ZW = zw_cellmix_n(87u64, n1887, 1542469173u64);
    let n2767: ZW = zw_cellmix_n(87u64, n1887, 668265263u64);
    let n2768: ZW = zw_add(n2764, n2766);
    let n2769: ZW = zw_add(n2765, n2767);
    let n2770: ZW = zw_add(n2768, n2252);
    let n2771: ZW = zw_add(n2769, n2253);
    let n2772: ZW = zw_add(n2768, n2744);
    let n2773: ZW = zw_add(n2769, n2745);
    let n2774: ZW = zw_cellmix_n(241u64, n1961, 1542469173u64);
    let n2775: ZW = zw_cellmix_n(241u64, n1961, 668265263u64);
    let n2776: ZW = zw_add(n2230, n2774);
    let n2777: ZW = zw_add(n2231, n2775);
    let n2778: ZW = zw_cellmix_n(254u64, n1962, 1542469173u64);
    let n2779: ZW = zw_cellmix_n(254u64, n1962, 668265263u64);
    let n2780: ZW = zw_add(n2776, n2778);
    let n2781: ZW = zw_add(n2777, n2779);
    let n2782: ZW = zw_cellmix_n(301u64, n1968, 1542469173u64);
    let n2783: ZW = zw_cellmix_n(301u64, n1968, 668265263u64);
    let n2784: ZW = zw_add(n2780, n2782);
    let n2785: ZW = zw_add(n2781, n2783);
    let n2786: ZW = zw_cellmix_n(367u64, n1970, 1542469173u64);
    let n2787: ZW = zw_cellmix_n(367u64, n1970, 668265263u64);
    let n2788: ZW = zw_add(n2784, n2786);
    let n2789: ZW = zw_add(n2785, n2787);
    let n2790: ZW = zw_cellmix_n(368u64, n1971, 1542469173u64);
    let n2791: ZW = zw_cellmix_n(368u64, n1971, 668265263u64);
    let n2792: ZW = zw_add(n2788, n2790);
    let n2793: ZW = zw_add(n2789, n2791);
    let n2794: ZW = zw_cellmix_n(20u64, n1960, 1542469173u64);
    let n2795: ZW = zw_cellmix_n(20u64, n1960, 668265263u64);
    let n2796: ZW = zw_add(n2792, n2794);
    let n2797: ZW = zw_add(n2793, n2795);
    let n2798: ZW = zw_add(n2796, n2256);
    let n2799: ZW = zw_add(n2797, n2257);
    let n2800: ZW = zw_cellmix_n(281u64, n1963, 1542469173u64);
    let n2801: ZW = zw_cellmix_n(281u64, n1963, 668265263u64);
    let n2802: ZW = zw_add(n2798, n2800);
    let n2803: ZW = zw_add(n2799, n2801);
    let n2804: ZW = zw_cellmix_n(283u64, n1964, 1542469173u64);
    let n2805: ZW = zw_cellmix_n(283u64, n1964, 668265263u64);
    let n2806: ZW = zw_add(n2802, n2804);
    let n2807: ZW = zw_add(n2803, n2805);
    let n2808: ZW = zw_cellmix_n(284u64, n1965, 1542469173u64);
    let n2809: ZW = zw_cellmix_n(284u64, n1965, 668265263u64);
    let n2810: ZW = zw_add(n2806, n2808);
    let n2811: ZW = zw_add(n2807, n2809);
    let n2812: ZW = zw_cellmix_n(286u64, n1966, 1542469173u64);
    let n2813: ZW = zw_cellmix_n(286u64, n1966, 668265263u64);
    let n2814: ZW = zw_add(n2810, n2812);
    let n2815: ZW = zw_add(n2811, n2813);
    let n2816: ZW = zw_cellmix_b(293u64, n1891, 1542469173u64);
    let n2817: ZW = zw_cellmix_b(293u64, n1891, 668265263u64);
    let n2818: ZW = zw_add(n2814, n2816);
    let n2819: ZW = zw_add(n2815, n2817);
    let n2820: ZW = zw_cellmix_b(294u64, n1892, 1542469173u64);
    let n2821: ZW = zw_cellmix_b(294u64, n1892, 668265263u64);
    let n2822: ZW = zw_add(n2818, n2820);
    let n2823: ZW = zw_add(n2819, n2821);
    let n2824: ZW = zw_cellmix_n(300u64, n1984, 1542469173u64);
    let n2825: ZW = zw_cellmix_n(300u64, n1984, 668265263u64);
    let n2826: ZW = zw_add(n2822, n2824);
    let n2827: ZW = zw_add(n2823, n2825);
    let n2828: ZW = zw_cellmix_n(357u64, r_c357, 1542469173u64);
    let n2829: ZW = zw_cellmix_n(357u64, r_c357, 668265263u64);
    let n2830: ZW = zw_add(n2826, n2828);
    let n2831: ZW = zw_add(n2827, n2829);
    let n2832: ZW = zw_cellmix_n(358u64, r_c358, 1542469173u64);
    let n2833: ZW = zw_cellmix_n(358u64, r_c358, 668265263u64);
    let n2834: ZW = zw_add(n2830, n2832);
    let n2835: ZW = zw_add(n2831, n2833);
    let n2836: ZW = zw_cellmix_n(359u64, r_c359, 1542469173u64);
    let n2837: ZW = zw_cellmix_n(359u64, r_c359, 668265263u64);
    let n2838: ZW = zw_add(n2834, n2836);
    let n2839: ZW = zw_add(n2835, n2837);
    let n2840: ZW = zw_cellmix_n(360u64, r_c360, 1542469173u64);
    let n2841: ZW = zw_cellmix_n(360u64, r_c360, 668265263u64);
    let n2842: ZW = zw_add(n2838, n2840);
    let n2843: ZW = zw_add(n2839, n2841);
    let n2844: ZW = zw_cellmix_b(361u64, n1969, 1542469173u64);
    let n2845: ZW = zw_cellmix_b(361u64, n1969, 668265263u64);
    let n2846: ZW = zw_add(n2842, n2844);
    let n2847: ZW = zw_add(n2843, n2845);
    let n2848: ZW = zw_cellmix_n(369u64, n1985, 1542469173u64);
    let n2849: ZW = zw_cellmix_n(369u64, n1985, 668265263u64);
    let n2850: ZW = zw_add(n2846, n2848);
    let n2851: ZW = zw_add(n2847, n2849);
    let n2852: ZW = zw_cellmix_n(370u64, n1973, 1542469173u64);
    let n2853: ZW = zw_cellmix_n(370u64, n1973, 668265263u64);
    let n2854: ZW = zw_add(n2850, n2852);
    let n2855: ZW = zw_add(n2851, n2853);
    let n2856: ZW = zw_cellmix_b(361u64, n1998, 1542469173u64);
    let n2857: ZW = zw_cellmix_b(361u64, n1998, 668265263u64);
    let n2858: ZW = zw_add(n2842, n2856);
    let n2859: ZW = zw_add(n2843, n2857);
    let n2860: ZW = zw_cellmix_n(369u64, n2002, 1542469173u64);
    let n2861: ZW = zw_cellmix_n(369u64, n2002, 668265263u64);
    let n2862: ZW = zw_add(n2858, n2860);
    let n2863: ZW = zw_add(n2859, n2861);
    let n2864: ZW = zw_cellmix_n(370u64, n2000, 1542469173u64);
    let n2865: ZW = zw_cellmix_n(370u64, n2000, 668265263u64);
    let n2866: ZW = zw_add(n2862, n2864);
    let n2867: ZW = zw_add(n2863, n2865);
    let n2868: ZW = zw_cellmix_b(361u64, n2014, 1542469173u64);
    let n2869: ZW = zw_cellmix_b(361u64, n2014, 668265263u64);
    let n2870: ZW = zw_add(n2842, n2868);
    let n2871: ZW = zw_add(n2843, n2869);
    let n2872: ZW = zw_cellmix_n(369u64, n2018, 1542469173u64);
    let n2873: ZW = zw_cellmix_n(369u64, n2018, 668265263u64);
    let n2874: ZW = zw_add(n2870, n2872);
    let n2875: ZW = zw_add(n2871, n2873);
    let n2876: ZW = zw_cellmix_n(370u64, n2016, 1542469173u64);
    let n2877: ZW = zw_cellmix_n(370u64, n2016, 668265263u64);
    let n2878: ZW = zw_add(n2874, n2876);
    let n2879: ZW = zw_add(n2875, n2877);
    let n2880: ZW = zw_cellmix_n(286u64, n2026, 1542469173u64);
    let n2881: ZW = zw_cellmix_n(286u64, n2026, 668265263u64);
    let n2882: ZW = zw_add(n2810, n2880);
    let n2883: ZW = zw_add(n2811, n2881);
    let n2884: ZW = zw_add(n2882, n2816);
    let n2885: ZW = zw_add(n2883, n2817);
    let n2886: ZW = zw_cellmix_b(294u64, n2019, 1542469173u64);
    let n2887: ZW = zw_cellmix_b(294u64, n2019, 668265263u64);
    let n2888: ZW = zw_add(n2884, n2886);
    let n2889: ZW = zw_add(n2885, n2887);
    let n2890: ZW = zw_add(n2888, n2824);
    let n2891: ZW = zw_add(n2889, n2825);
    let n2892: ZW = zw_add(n2890, n2828);
    let n2893: ZW = zw_add(n2891, n2829);
    let n2894: ZW = zw_add(n2892, n2832);
    let n2895: ZW = zw_add(n2893, n2833);
    let n2896: ZW = zw_add(n2894, n2836);
    let n2897: ZW = zw_add(n2895, n2837);
    let n2898: ZW = zw_add(n2896, n2840);
    let n2899: ZW = zw_add(n2897, n2841);
    let n2900: ZW = zw_add(n2898, n2844);
    let n2901: ZW = zw_add(n2899, n2845);
    let n2902: ZW = zw_cellmix_n(369u64, n2030, 1542469173u64);
    let n2903: ZW = zw_cellmix_n(369u64, n2030, 668265263u64);
    let n2904: ZW = zw_add(n2900, n2902);
    let n2905: ZW = zw_add(n2901, n2903);
    let n2906: ZW = zw_cellmix_n(370u64, n2028, 1542469173u64);
    let n2907: ZW = zw_cellmix_n(370u64, n2028, 668265263u64);
    let n2908: ZW = zw_add(n2904, n2906);
    let n2909: ZW = zw_add(n2905, n2907);
    let n2910: ZW = zw_add(n2898, n2856);
    let n2911: ZW = zw_add(n2899, n2857);
    let n2912: ZW = zw_cellmix_n(369u64, n2038, 1542469173u64);
    let n2913: ZW = zw_cellmix_n(369u64, n2038, 668265263u64);
    let n2914: ZW = zw_add(n2910, n2912);
    let n2915: ZW = zw_add(n2911, n2913);
    let n2916: ZW = zw_cellmix_n(370u64, n2036, 1542469173u64);
    let n2917: ZW = zw_cellmix_n(370u64, n2036, 668265263u64);
    let n2918: ZW = zw_add(n2914, n2916);
    let n2919: ZW = zw_add(n2915, n2917);
    let n2920: ZW = zw_add(n2898, n2868);
    let n2921: ZW = zw_add(n2899, n2869);
    let n2922: ZW = zw_cellmix_n(369u64, n2046, 1542469173u64);
    let n2923: ZW = zw_cellmix_n(369u64, n2046, 668265263u64);
    let n2924: ZW = zw_add(n2920, n2922);
    let n2925: ZW = zw_add(n2921, n2923);
    let n2926: ZW = zw_cellmix_n(370u64, n2044, 1542469173u64);
    let n2927: ZW = zw_cellmix_n(370u64, n2044, 668265263u64);
    let n2928: ZW = zw_add(n2924, n2926);
    let n2929: ZW = zw_add(n2925, n2927);
    let n2930: ZW = zw_cellmix_n(20u64, n2066, 1542469173u64);
    let n2931: ZW = zw_cellmix_n(20u64, n2066, 668265263u64);
    let n2932: ZW = zw_add(n2792, n2930);
    let n2933: ZW = zw_add(n2793, n2931);
    let n2934: ZW = zw_cellmix_b(41u64, n2067, 1542469173u64);
    let n2935: ZW = zw_cellmix_b(41u64, n2067, 668265263u64);
    let n2936: ZW = zw_add(n2932, n2934);
    let n2937: ZW = zw_add(n2933, n2935);
    let n2938: ZW = zw_cellmix_n(281u64, n2068, 1542469173u64);
    let n2939: ZW = zw_cellmix_n(281u64, n2068, 668265263u64);
    let n2940: ZW = zw_add(n2936, n2938);
    let n2941: ZW = zw_add(n2937, n2939);
    let n2942: ZW = zw_cellmix_n(283u64, n2069, 1542469173u64);
    let n2943: ZW = zw_cellmix_n(283u64, n2069, 668265263u64);
    let n2944: ZW = zw_add(n2940, n2942);
    let n2945: ZW = zw_add(n2941, n2943);
    let n2946: ZW = zw_cellmix_n(284u64, n2070, 1542469173u64);
    let n2947: ZW = zw_cellmix_n(284u64, n2070, 668265263u64);
    let n2948: ZW = zw_add(n2944, n2946);
    let n2949: ZW = zw_add(n2945, n2947);
    let n2950: ZW = zw_add(n2948, n2812);
    let n2951: ZW = zw_add(n2949, n2813);
    let n2952: ZW = zw_cellmix_b(293u64, n2047, 1542469173u64);
    let n2953: ZW = zw_cellmix_b(293u64, n2047, 668265263u64);
    let n2954: ZW = zw_add(n2950, n2952);
    let n2955: ZW = zw_add(n2951, n2953);
    let n2956: ZW = zw_add(n2954, n2820);
    let n2957: ZW = zw_add(n2955, n2821);
    let n2958: ZW = zw_cellmix_n(300u64, n2079, 1542469173u64);
    let n2959: ZW = zw_cellmix_n(300u64, n2079, 668265263u64);
    let n2960: ZW = zw_add(n2956, n2958);
    let n2961: ZW = zw_add(n2957, n2959);
    let n2962: ZW = zw_cellmix_n(357u64, n2071, 1542469173u64);
    let n2963: ZW = zw_cellmix_n(357u64, n2071, 668265263u64);
    let n2964: ZW = zw_add(n2960, n2962);
    let n2965: ZW = zw_add(n2961, n2963);
    let n2966: ZW = zw_cellmix_n(358u64, n2072, 1542469173u64);
    let n2967: ZW = zw_cellmix_n(358u64, n2072, 668265263u64);
    let n2968: ZW = zw_add(n2964, n2966);
    let n2969: ZW = zw_add(n2965, n2967);
    let n2970: ZW = zw_cellmix_n(359u64, n2073, 1542469173u64);
    let n2971: ZW = zw_cellmix_n(359u64, n2073, 668265263u64);
    let n2972: ZW = zw_add(n2968, n2970);
    let n2973: ZW = zw_add(n2969, n2971);
    let n2974: ZW = zw_cellmix_n(360u64, n2074, 1542469173u64);
    let n2975: ZW = zw_cellmix_n(360u64, n2074, 668265263u64);
    let n2976: ZW = zw_add(n2972, n2974);
    let n2977: ZW = zw_add(n2973, n2975);
    let n2978: ZW = zw_add(n2976, n2844);
    let n2979: ZW = zw_add(n2977, n2845);
    let n2980: ZW = zw_cellmix_n(369u64, n2080, 1542469173u64);
    let n2981: ZW = zw_cellmix_n(369u64, n2080, 668265263u64);
    let n2982: ZW = zw_add(n2978, n2980);
    let n2983: ZW = zw_add(n2979, n2981);
    let n2984: ZW = zw_cellmix_n(370u64, n2076, 1542469173u64);
    let n2985: ZW = zw_cellmix_n(370u64, n2076, 668265263u64);
    let n2986: ZW = zw_add(n2982, n2984);
    let n2987: ZW = zw_add(n2983, n2985);
    let n2988: ZW = zw_cellmix_n(358u64, n2089, 1542469173u64);
    let n2989: ZW = zw_cellmix_n(358u64, n2089, 668265263u64);
    let n2990: ZW = zw_add(n2964, n2988);
    let n2991: ZW = zw_add(n2965, n2989);
    let n2992: ZW = zw_cellmix_n(359u64, n2090, 1542469173u64);
    let n2993: ZW = zw_cellmix_n(359u64, n2090, 668265263u64);
    let n2994: ZW = zw_add(n2990, n2992);
    let n2995: ZW = zw_add(n2991, n2993);
    let n2996: ZW = zw_add(n2994, n2974);
    let n2997: ZW = zw_add(n2995, n2975);
    let n2998: ZW = zw_add(n2996, n2856);
    let n2999: ZW = zw_add(n2997, n2857);
    let n3000: ZW = zw_cellmix_n(369u64, n2094, 1542469173u64);
    let n3001: ZW = zw_cellmix_n(369u64, n2094, 668265263u64);
    let n3002: ZW = zw_add(n2998, n3000);
    let n3003: ZW = zw_add(n2999, n3001);
    let n3004: ZW = zw_cellmix_n(370u64, n2092, 1542469173u64);
    let n3005: ZW = zw_cellmix_n(370u64, n2092, 668265263u64);
    let n3006: ZW = zw_add(n3002, n3004);
    let n3007: ZW = zw_add(n3003, n3005);
    let n3008: ZW = zw_cellmix_n(359u64, n2101, 1542469173u64);
    let n3009: ZW = zw_cellmix_n(359u64, n2101, 668265263u64);
    let n3010: ZW = zw_add(n2990, n3008);
    let n3011: ZW = zw_add(n2991, n3009);
    let n3012: ZW = zw_add(n3010, n2974);
    let n3013: ZW = zw_add(n3011, n2975);
    let n3014: ZW = zw_add(n3012, n2868);
    let n3015: ZW = zw_add(n3013, n2869);
    let n3016: ZW = zw_cellmix_n(369u64, n2105, 1542469173u64);
    let n3017: ZW = zw_cellmix_n(369u64, n2105, 668265263u64);
    let n3018: ZW = zw_add(n3014, n3016);
    let n3019: ZW = zw_add(n3015, n3017);
    let n3020: ZW = zw_cellmix_n(370u64, n2103, 1542469173u64);
    let n3021: ZW = zw_cellmix_n(370u64, n2103, 668265263u64);
    let n3022: ZW = zw_add(n3018, n3020);
    let n3023: ZW = zw_add(n3019, n3021);
    let n3024: ZW = zw_cellmix_n(357u64, n2118, 1542469173u64);
    let n3025: ZW = zw_cellmix_n(357u64, n2118, 668265263u64);
    let n3026: ZW = zw_add(n2960, n3024);
    let n3027: ZW = zw_add(n2961, n3025);
    let n3028: ZW = zw_cellmix_n(358u64, n2119, 1542469173u64);
    let n3029: ZW = zw_cellmix_n(358u64, n2119, 668265263u64);
    let n3030: ZW = zw_add(n3026, n3028);
    let n3031: ZW = zw_add(n3027, n3029);
    let n3032: ZW = zw_cellmix_n(359u64, n2120, 1542469173u64);
    let n3033: ZW = zw_cellmix_n(359u64, n2120, 668265263u64);
    let n3034: ZW = zw_add(n3030, n3032);
    let n3035: ZW = zw_add(n3031, n3033);
    let n3036: ZW = zw_cellmix_n(360u64, n2121, 1542469173u64);
    let n3037: ZW = zw_cellmix_n(360u64, n2121, 668265263u64);
    let n3038: ZW = zw_add(n3034, n3036);
    let n3039: ZW = zw_add(n3035, n3037);
    let n3040: ZW = zw_add(n3038, n2844);
    let n3041: ZW = zw_add(n3039, n2845);
    let n3042: ZW = zw_cellmix_n(369u64, n2125, 1542469173u64);
    let n3043: ZW = zw_cellmix_n(369u64, n2125, 668265263u64);
    let n3044: ZW = zw_add(n3040, n3042);
    let n3045: ZW = zw_add(n3041, n3043);
    let n3046: ZW = zw_cellmix_n(370u64, n2123, 1542469173u64);
    let n3047: ZW = zw_cellmix_n(370u64, n2123, 668265263u64);
    let n3048: ZW = zw_add(n3044, n3046);
    let n3049: ZW = zw_add(n3045, n3047);
    let n3050: ZW = zw_add(n3026, n2988);
    let n3051: ZW = zw_add(n3027, n2989);
    let n3052: ZW = zw_add(n3050, n2992);
    let n3053: ZW = zw_add(n3051, n2993);
    let n3054: ZW = zw_add(n3052, n3036);
    let n3055: ZW = zw_add(n3053, n3037);
    let n3056: ZW = zw_add(n3054, n2856);
    let n3057: ZW = zw_add(n3055, n2857);
    let n3058: ZW = zw_cellmix_n(369u64, n2133, 1542469173u64);
    let n3059: ZW = zw_cellmix_n(369u64, n2133, 668265263u64);
    let n3060: ZW = zw_add(n3056, n3058);
    let n3061: ZW = zw_add(n3057, n3059);
    let n3062: ZW = zw_cellmix_n(370u64, n2131, 1542469173u64);
    let n3063: ZW = zw_cellmix_n(370u64, n2131, 668265263u64);
    let n3064: ZW = zw_add(n3060, n3062);
    let n3065: ZW = zw_add(n3061, n3063);
    let n3066: ZW = zw_add(n3050, n3008);
    let n3067: ZW = zw_add(n3051, n3009);
    let n3068: ZW = zw_add(n3066, n3036);
    let n3069: ZW = zw_add(n3067, n3037);
    let n3070: ZW = zw_add(n3068, n2868);
    let n3071: ZW = zw_add(n3069, n2869);
    let n3072: ZW = zw_cellmix_n(369u64, n2141, 1542469173u64);
    let n3073: ZW = zw_cellmix_n(369u64, n2141, 668265263u64);
    let n3074: ZW = zw_add(n3070, n3072);
    let n3075: ZW = zw_add(n3071, n3073);
    let n3076: ZW = zw_cellmix_n(370u64, n2139, 1542469173u64);
    let n3077: ZW = zw_cellmix_n(370u64, n2139, 668265263u64);
    let n3078: ZW = zw_add(n3074, n3076);
    let n3079: ZW = zw_add(n3075, n3077);
    let n3080: ZW = zw_cellmix_n(360u64, n2146, 1542469173u64);
    let n3081: ZW = zw_cellmix_n(360u64, n2146, 668265263u64);
    let n3082: ZW = zw_add(n3034, n3080);
    let n3083: ZW = zw_add(n3035, n3081);
    let n3084: ZW = zw_add(n3082, n2844);
    let n3085: ZW = zw_add(n3083, n2845);
    let n3086: ZW = zw_add(n3084, n3042);
    let n3087: ZW = zw_add(n3085, n3043);
    let n3088: ZW = zw_cellmix_n(370u64, n2147, 1542469173u64);
    let n3089: ZW = zw_cellmix_n(370u64, n2147, 668265263u64);
    let n3090: ZW = zw_add(n3086, n3088);
    let n3091: ZW = zw_add(n3087, n3089);
    let n3092: ZW = zw_add(n3052, n3080);
    let n3093: ZW = zw_add(n3053, n3081);
    let n3094: ZW = zw_add(n3092, n2856);
    let n3095: ZW = zw_add(n3093, n2857);
    let n3096: ZW = zw_add(n3094, n3058);
    let n3097: ZW = zw_add(n3095, n3059);
    let n3098: ZW = zw_cellmix_n(370u64, n2150, 1542469173u64);
    let n3099: ZW = zw_cellmix_n(370u64, n2150, 668265263u64);
    let n3100: ZW = zw_add(n3096, n3098);
    let n3101: ZW = zw_add(n3097, n3099);
    let n3102: ZW = zw_add(n3066, n3080);
    let n3103: ZW = zw_add(n3067, n3081);
    let n3104: ZW = zw_add(n3102, n2868);
    let n3105: ZW = zw_add(n3103, n2869);
    let n3106: ZW = zw_add(n3104, n3072);
    let n3107: ZW = zw_add(n3105, n3073);
    let n3108: ZW = zw_cellmix_n(370u64, n2153, 1542469173u64);
    let n3109: ZW = zw_cellmix_n(370u64, n2153, 668265263u64);
    let n3110: ZW = zw_add(n3106, n3108);
    let n3111: ZW = zw_add(n3107, n3109);
    let n3112: ZW = zw_add(n2948, n2880);
    let n3113: ZW = zw_add(n2949, n2881);
    let n3114: ZW = zw_add(n3112, n2952);
    let n3115: ZW = zw_add(n3113, n2953);
    let n3116: ZW = zw_add(n3114, n2886);
    let n3117: ZW = zw_add(n3115, n2887);
    let n3118: ZW = zw_add(n3116, n2958);
    let n3119: ZW = zw_add(n3117, n2959);
    let n3120: ZW = zw_add(n3118, n2962);
    let n3121: ZW = zw_add(n3119, n2963);
    let n3122: ZW = zw_add(n3120, n2966);
    let n3123: ZW = zw_add(n3121, n2967);
    let n3124: ZW = zw_add(n3122, n2970);
    let n3125: ZW = zw_add(n3123, n2971);
    let n3126: ZW = zw_add(n3124, n2974);
    let n3127: ZW = zw_add(n3125, n2975);
    let n3128: ZW = zw_add(n3126, n2844);
    let n3129: ZW = zw_add(n3127, n2845);
    let n3130: ZW = zw_cellmix_n(369u64, n2161, 1542469173u64);
    let n3131: ZW = zw_cellmix_n(369u64, n2161, 668265263u64);
    let n3132: ZW = zw_add(n3128, n3130);
    let n3133: ZW = zw_add(n3129, n3131);
    let n3134: ZW = zw_cellmix_n(370u64, n2159, 1542469173u64);
    let n3135: ZW = zw_cellmix_n(370u64, n2159, 668265263u64);
    let n3136: ZW = zw_add(n3132, n3134);
    let n3137: ZW = zw_add(n3133, n3135);
    let n3138: ZW = zw_add(n3120, n2988);
    let n3139: ZW = zw_add(n3121, n2989);
    let n3140: ZW = zw_add(n3138, n2992);
    let n3141: ZW = zw_add(n3139, n2993);
    let n3142: ZW = zw_add(n3140, n2974);
    let n3143: ZW = zw_add(n3141, n2975);
    let n3144: ZW = zw_add(n3142, n2856);
    let n3145: ZW = zw_add(n3143, n2857);
    let n3146: ZW = zw_cellmix_n(369u64, n2169, 1542469173u64);
    let n3147: ZW = zw_cellmix_n(369u64, n2169, 668265263u64);
    let n3148: ZW = zw_add(n3144, n3146);
    let n3149: ZW = zw_add(n3145, n3147);
    let n3150: ZW = zw_cellmix_n(370u64, n2167, 1542469173u64);
    let n3151: ZW = zw_cellmix_n(370u64, n2167, 668265263u64);
    let n3152: ZW = zw_add(n3148, n3150);
    let n3153: ZW = zw_add(n3149, n3151);
    let n3154: ZW = zw_add(n3138, n3008);
    let n3155: ZW = zw_add(n3139, n3009);
    let n3156: ZW = zw_add(n3154, n2974);
    let n3157: ZW = zw_add(n3155, n2975);
    let n3158: ZW = zw_add(n3156, n2868);
    let n3159: ZW = zw_add(n3157, n2869);
    let n3160: ZW = zw_cellmix_n(369u64, n2177, 1542469173u64);
    let n3161: ZW = zw_cellmix_n(369u64, n2177, 668265263u64);
    let n3162: ZW = zw_add(n3158, n3160);
    let n3163: ZW = zw_add(n3159, n3161);
    let n3164: ZW = zw_cellmix_n(370u64, n2175, 1542469173u64);
    let n3165: ZW = zw_cellmix_n(370u64, n2175, 668265263u64);
    let n3166: ZW = zw_add(n3162, n3164);
    let n3167: ZW = zw_add(n3163, n3165);
    let n3168: ZW = zw_add(n3118, n3024);
    let n3169: ZW = zw_add(n3119, n3025);
    let n3170: ZW = zw_add(n3168, n3028);
    let n3171: ZW = zw_add(n3169, n3029);
    let n3172: ZW = zw_add(n3170, n3032);
    let n3173: ZW = zw_add(n3171, n3033);
    let n3174: ZW = zw_add(n3172, n3036);
    let n3175: ZW = zw_add(n3173, n3037);
    let n3176: ZW = zw_add(n3174, n2844);
    let n3177: ZW = zw_add(n3175, n2845);
    let n3178: ZW = zw_cellmix_n(369u64, n2185, 1542469173u64);
    let n3179: ZW = zw_cellmix_n(369u64, n2185, 668265263u64);
    let n3180: ZW = zw_add(n3176, n3178);
    let n3181: ZW = zw_add(n3177, n3179);
    let n3182: ZW = zw_cellmix_n(370u64, n2183, 1542469173u64);
    let n3183: ZW = zw_cellmix_n(370u64, n2183, 668265263u64);
    let n3184: ZW = zw_add(n3180, n3182);
    let n3185: ZW = zw_add(n3181, n3183);
    let n3186: ZW = zw_add(n3168, n2988);
    let n3187: ZW = zw_add(n3169, n2989);
    let n3188: ZW = zw_add(n3186, n2992);
    let n3189: ZW = zw_add(n3187, n2993);
    let n3190: ZW = zw_add(n3188, n3036);
    let n3191: ZW = zw_add(n3189, n3037);
    let n3192: ZW = zw_add(n3190, n2856);
    let n3193: ZW = zw_add(n3191, n2857);
    let n3194: ZW = zw_cellmix_n(369u64, n2193, 1542469173u64);
    let n3195: ZW = zw_cellmix_n(369u64, n2193, 668265263u64);
    let n3196: ZW = zw_add(n3192, n3194);
    let n3197: ZW = zw_add(n3193, n3195);
    let n3198: ZW = zw_cellmix_n(370u64, n2191, 1542469173u64);
    let n3199: ZW = zw_cellmix_n(370u64, n2191, 668265263u64);
    let n3200: ZW = zw_add(n3196, n3198);
    let n3201: ZW = zw_add(n3197, n3199);
    let n3202: ZW = zw_add(n3186, n3008);
    let n3203: ZW = zw_add(n3187, n3009);
    let n3204: ZW = zw_add(n3202, n3036);
    let n3205: ZW = zw_add(n3203, n3037);
    let n3206: ZW = zw_add(n3204, n2868);
    let n3207: ZW = zw_add(n3205, n2869);
    let n3208: ZW = zw_cellmix_n(369u64, n2201, 1542469173u64);
    let n3209: ZW = zw_cellmix_n(369u64, n2201, 668265263u64);
    let n3210: ZW = zw_add(n3206, n3208);
    let n3211: ZW = zw_add(n3207, n3209);
    let n3212: ZW = zw_cellmix_n(370u64, n2199, 1542469173u64);
    let n3213: ZW = zw_cellmix_n(370u64, n2199, 668265263u64);
    let n3214: ZW = zw_add(n3210, n3212);
    let n3215: ZW = zw_add(n3211, n3213);
    let n3216: ZW = zw_add(n3172, n3080);
    let n3217: ZW = zw_add(n3173, n3081);
    let n3218: ZW = zw_add(n3216, n2844);
    let n3219: ZW = zw_add(n3217, n2845);
    let n3220: ZW = zw_add(n3218, n3178);
    let n3221: ZW = zw_add(n3219, n3179);
    let n3222: ZW = zw_cellmix_n(370u64, n2204, 1542469173u64);
    let n3223: ZW = zw_cellmix_n(370u64, n2204, 668265263u64);
    let n3224: ZW = zw_add(n3220, n3222);
    let n3225: ZW = zw_add(n3221, n3223);
    let n3226: ZW = zw_add(n3188, n3080);
    let n3227: ZW = zw_add(n3189, n3081);
    let n3228: ZW = zw_add(n3226, n2856);
    let n3229: ZW = zw_add(n3227, n2857);
    let n3230: ZW = zw_add(n3228, n3194);
    let n3231: ZW = zw_add(n3229, n3195);
    let n3232: ZW = zw_cellmix_n(370u64, n2207, 1542469173u64);
    let n3233: ZW = zw_cellmix_n(370u64, n2207, 668265263u64);
    let n3234: ZW = zw_add(n3230, n3232);
    let n3235: ZW = zw_add(n3231, n3233);
    let n3236: ZW = zw_add(n3202, n3080);
    let n3237: ZW = zw_add(n3203, n3081);
    let n3238: ZW = zw_add(n3236, n2868);
    let n3239: ZW = zw_add(n3237, n2869);
    let n3240: ZW = zw_add(n3238, n3208);
    let n3241: ZW = zw_add(n3239, n3209);
    let n3242: ZW = zw_cellmix_n(370u64, n2210, 1542469173u64);
    let n3243: ZW = zw_cellmix_n(370u64, n2210, 668265263u64);
    let n3244: ZW = zw_add(n3240, n3242);
    let n3245: ZW = zw_add(n3241, n3243);
    let ok_v0_b0: u16 = ALL & zb_holds(n814) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v0_b0: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v0_b0: u16 = ALL & zb_holds(n134) & zb_holds(n813);
    let ok_v1_b1: u16 = ALL & zb_holds(n814) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v1_b1: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v1_b1: u16 = ALL & zb_holds(n134) & zb_holds(n813);
    let ok_v2_b2: u16 = ALL & zb_holds(n814) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v2_b2: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v2_b2: u16 = ALL & zb_holds(n134) & zb_holds(n813);
    let ok_v16_b3: u16 = ALL & zb_holds(n814) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v16_b3: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v16_b3: u16 = ALL & zb_holds(n134) & zb_holds(n813);
    let ok_v17_b4: u16 = ALL & zb_holds(n814) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v17_b4: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v17_b4: u16 = ALL & zb_holds(n134) & zb_holds(n813);
    let ok_v18_b5: u16 = ALL & zb_holds(n814) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v18_b5: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v18_b5: u16 = ALL & zb_holds(n134) & zb_holds(n813);
    let ok_v32_b6: u16 = ALL & zb_holds(n814) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v32_b6: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v32_b6: u16 = ALL & zb_holds(n813);
    let ok_v33_b7: u16 = ALL & zb_holds(n814) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v33_b7: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v33_b7: u16 = ALL & zb_holds(n813);
    let ok_v34_b8: u16 = ALL & zb_holds(n814) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v34_b8: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v34_b8: u16 = ALL & zb_holds(n813);
    let ok_v36_b9: u16 = ALL & zb_holds(n814) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v36_b9: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v36_b9: u16 = ALL & zb_holds(n813);
    let ok_v37_b10: u16 = ALL & zb_holds(n814) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v37_b10: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v37_b10: u16 = ALL & zb_holds(n813);
    let ok_v38_b11: u16 = ALL & zb_holds(n814) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v38_b11: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v38_b11: u16 = ALL & zb_holds(n813);
    let ok_v40_b12: u16 = ALL & zb_holds(n814) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v40_b12: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v40_b12: u16 = ALL & zb_holds(n813);
    let ok_v41_b13: u16 = ALL & zb_holds(n814) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v41_b13: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v41_b13: u16 = ALL & zb_holds(n813);
    let ok_v42_b14: u16 = ALL & zb_holds(n814) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v42_b14: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v42_b14: u16 = ALL & zb_holds(n813);
    let ok_v48_b15: u16 = ALL & zb_holds(n814) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v48_b15: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v48_b15: u16 = ALL & zb_holds(n813);
    let ok_v49_b16: u16 = ALL & zb_holds(n814) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v49_b16: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v49_b16: u16 = ALL & zb_holds(n813);
    let ok_v50_b17: u16 = ALL & zb_holds(n814) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v50_b17: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v50_b17: u16 = ALL & zb_holds(n813);
    let ok_v52_b18: u16 = ALL & zb_holds(n814) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v52_b18: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v52_b18: u16 = ALL & zb_holds(n813);
    let ok_v53_b19: u16 = ALL & zb_holds(n814) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v53_b19: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v53_b19: u16 = ALL & zb_holds(n813);
    let ok_v54_b20: u16 = ALL & zb_holds(n814) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v54_b20: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v54_b20: u16 = ALL & zb_holds(n813);
    let ok_v56_b21: u16 = ALL & zb_holds(n814) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v56_b21: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v56_b21: u16 = ALL & zb_holds(n813);
    let ok_v57_b22: u16 = ALL & zb_holds(n814) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v57_b22: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v57_b22: u16 = ALL & zb_holds(n813);
    let ok_v58_b23: u16 = ALL & zb_holds(n814) & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143);
    let bd_v58_b23: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v58_b23: u16 = ALL & zb_holds(n813);
    let ok_v0_b24: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n1168);
    let bd_v0_b24: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v0_b24: u16 = ALL & zb_holds(n1166);
    let ok_v32_b25: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n1168);
    let bd_v32_b25: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v32_b25: u16 = ALL & zb_holds(n1166);
    let ok_v0_b26: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n1782);
    let bd_v0_b26: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v0_b26: u16 = ALL & zb_holds(n1781) & zb_holds(n1843);
    let ok_v32_b27: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n1782);
    let bd_v32_b27: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v32_b27: u16 = ALL & zb_holds(n1781) & zb_holds(n1843);
    let ok_v0_b28: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n1886);
    let bd_v0_b28: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v0_b28: u16 = ALL & zb_holds(n1885);
    let ok_v32_b29: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n1886);
    let bd_v32_b29: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v32_b29: u16 = ALL & zb_holds(n1885);
    let ok_v0_b30: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n1975);
    let bd_v0_b30: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v0_b30: u16 = ALL & zb_holds(n1974);
    let ok_v1_b31: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n1975);
    let bd_v1_b31: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v1_b31: u16 = ALL & zb_holds(n1974);
    let ok_v2_b32: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n1975);
    let bd_v2_b32: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v2_b32: u16 = ALL & zb_holds(n1974);
    let ok_v16_b33: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n1975);
    let bd_v16_b33: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v16_b33: u16 = ALL & zb_holds(n1974);
    let ok_v17_b34: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n1975);
    let bd_v17_b34: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v17_b34: u16 = ALL & zb_holds(n1974);
    let ok_v18_b35: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n1975);
    let bd_v18_b35: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v18_b35: u16 = ALL & zb_holds(n1974);
    let ok_v32_b36: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n1975);
    let bd_v32_b36: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v32_b36: u16 = ALL & zb_holds(n1974);
    let ok_v33_b37: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n1975);
    let bd_v33_b37: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v33_b37: u16 = ALL & zb_holds(n1974);
    let ok_v34_b38: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n1975);
    let bd_v34_b38: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v34_b38: u16 = ALL & zb_holds(n1974);
    let ok_v36_b39: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n1975);
    let bd_v36_b39: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v36_b39: u16 = ALL & zb_holds(n1974);
    let ok_v37_b40: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n1975);
    let bd_v37_b40: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v37_b40: u16 = ALL & zb_holds(n1974);
    let ok_v38_b41: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n1975);
    let bd_v38_b41: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v38_b41: u16 = ALL & zb_holds(n1974);
    let ok_v40_b42: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n1975);
    let bd_v40_b42: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v40_b42: u16 = ALL & zb_holds(n1974);
    let ok_v41_b43: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n1975);
    let bd_v41_b43: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v41_b43: u16 = ALL & zb_holds(n1974);
    let ok_v42_b44: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n1975);
    let bd_v42_b44: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v42_b44: u16 = ALL & zb_holds(n1974);
    let ok_v48_b45: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n1975);
    let bd_v48_b45: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v48_b45: u16 = ALL & zb_holds(n1974);
    let ok_v49_b46: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n1975);
    let bd_v49_b46: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v49_b46: u16 = ALL & zb_holds(n1974);
    let ok_v50_b47: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n1975);
    let bd_v50_b47: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v50_b47: u16 = ALL & zb_holds(n1974);
    let ok_v52_b48: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n1975);
    let bd_v52_b48: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v52_b48: u16 = ALL & zb_holds(n1974);
    let ok_v53_b49: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n1975);
    let bd_v53_b49: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v53_b49: u16 = ALL & zb_holds(n1974);
    let ok_v54_b50: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n1975);
    let bd_v54_b50: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v54_b50: u16 = ALL & zb_holds(n1974);
    let ok_v56_b51: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n1975);
    let bd_v56_b51: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v56_b51: u16 = ALL & zb_holds(n1974);
    let ok_v57_b52: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n1975);
    let bd_v57_b52: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v57_b52: u16 = ALL & zb_holds(n1974);
    let ok_v58_b53: u16 = ALL & zb_holds(n204) & zb_holds(n135) & zb_holds(r_c296) & zb_holds(n205) & zb_holds(r_c279) & zb_holds(n153) & zb_holds(n152) & zb_holds(n158) & zb_holds(n151) & zb_holds(n110) & zb_holds(r_c271) & zb_holds(n132) & zb_holds(n109) & zb_holds(n107) & zb_holds(n141) & zb_holds(n149) & zb_holds(n106) & zb_holds(r_c260) & zb_holds(n165) & zb_holds(n164) & zb_holds(n160) & zb_holds(n148) & zb_holds(r_c252) & zb_holds(n105) & zb_holds(n104) & zb_holds(n102) & zb_holds(n101) & zb_holds(n159) & zb_holds(n145) & zb_holds(r_c240) & zb_holds(n144) & zb_holds(r_c175) & zb_holds(n143) & zb_holds(n1975);
    let bd_v58_b53: bool = !n209 || !n208 || !n207 || !n206 || !n131 || !n108 || !n150 || !n142 || !n147 || !n103 || !n163 || !n146;
    let live_v58_b53: u16 = ALL & zb_holds(n1974);
    let sh0 = KShared0 {
        c87: r_c87,
        c39: r_c39,
        c84: n115,
        c86: n214,
        c241: n290,
        c254: n291,
        c368: n416,
        c369: n487,
        c302: n486,
        c85: n215,
    };
    let sh1 = KShared1 {
        c87: n1102,
        c84: n115,
        c86: n214,
        c240: n290,
        c253: n291,
        c85: n215,
    };
    let sh2 = KShared2 {
        c87: n1847,
        c84: n115,
        c86: n214,
        c240: n290,
        c253: n291,
        c85: n215,
    };
    let sh3 = KShared3 {
        c87: n1887,
        c39: n1888,
        c84: n115,
        c86: n214,
        c85: n215,
        c38: n1884,
    };
    let sh4 = KShared4 {
        c87: r_c87,
        c39: r_c39,
        c84: n115,
        c86: n214,
        c241: n1961,
        c254: n1962,
        c367: n1970,
        c368: n1971,
        c301: n1968,
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
    // 54 distinct button assignments; per outcome they fall
    // into [24, 2, 2, 2, 24] groups that write identical values.
    declined |= live_v0_b0 & (if bd_v0_b0 { ALL } else { !ok_v0_b0 });
    take_0_0 |= live_v0_b0 & ok_v0_b0 & (if bd_v0_b0 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c358: r_c357,
        c359: r_c358,
        c282: n278,
        c360: r_c359,
        c361: r_c360,
        c284: n281,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n892,
        c287: n818,
        c294: zb_splat(false),
        c295: zb_splat(false),
        c370: n904,
        c371: n894,
        c301: n903,
        h1: n2314, h2: n2315,
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
        c282: n278,
        c360: r_c359,
        c361: r_c360,
        c284: n281,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n934,
        c287: n818,
        c294: zb_splat(false),
        c295: zb_splat(false),
        c370: n938,
        c371: n936,
        c301: n903,
        h1: n2326, h2: n2327,
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
        c282: n278,
        c360: r_c359,
        c361: r_c360,
        c284: n281,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n956,
        c287: n818,
        c294: zb_splat(false),
        c295: zb_splat(false),
        c370: n960,
        c371: n958,
        c301: n903,
        h1: n2338, h2: n2339,
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
        c282: n278,
        c360: r_c359,
        c361: r_c360,
        c284: n281,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n892,
        c287: n964,
        c294: zb_splat(false),
        c295: zb_splat(true),
        c370: n968,
        c371: n966,
        c301: n903,
        h1: n2368, h2: n2369,
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
        c282: n278,
        c360: r_c359,
        c361: r_c360,
        c284: n281,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n934,
        c287: n964,
        c294: zb_splat(false),
        c295: zb_splat(true),
        c370: n974,
        c371: n972,
        c301: n903,
        h1: n2378, h2: n2379,
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
        c282: n278,
        c360: r_c359,
        c361: r_c360,
        c284: n281,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n956,
        c287: n964,
        c294: zb_splat(false),
        c295: zb_splat(true),
        c370: n980,
        c371: n978,
        c301: n903,
        h1: n2388, h2: n2389,
    };
    // body 5: buttons 0x12, forks 0x0
    sink.o0(18, take_0_5, &sh0, &o0);
    declined |= live_v32_b6 & (if bd_v32_b6 { ALL } else { !ok_v32_b6 });
    take_0_6 |= live_v32_b6 & ok_v32_b6 & (if bd_v32_b6 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n988,
        c41: n989,
        c358: n993,
        c359: n1000,
        c282: n990,
        c360: n1001,
        c361: n994,
        c284: n991,
        c285: n992,
        c362: n892,
        c287: n818,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1006,
        c371: n1003,
        c301: n1005,
        h1: n2446, h2: n2447,
    };
    // body 6: buttons 0x20, forks 0x0
    sink.o0(32, take_0_6, &sh0, &o0);
    declined |= live_v33_b7 & (if bd_v33_b7 { ALL } else { !ok_v33_b7 });
    take_0_7 |= live_v33_b7 & ok_v33_b7 & (if bd_v33_b7 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n988,
        c41: n989,
        c358: n993,
        c359: n1011,
        c282: n990,
        c360: n1012,
        c361: n994,
        c284: n991,
        c285: n992,
        c362: n934,
        c287: n818,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1016,
        c371: n1014,
        c301: n1005,
        h1: n2466, h2: n2467,
    };
    // body 7: buttons 0x21, forks 0x0
    sink.o0(33, take_0_7, &sh0, &o0);
    declined |= live_v34_b8 & (if bd_v34_b8 { ALL } else { !ok_v34_b8 });
    take_0_8 |= live_v34_b8 & ok_v34_b8 & (if bd_v34_b8 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n988,
        c41: n989,
        c358: n993,
        c359: n1011,
        c282: n990,
        c360: n1020,
        c361: n994,
        c284: n991,
        c285: n992,
        c362: n956,
        c287: n818,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1024,
        c371: n1022,
        c301: n1005,
        h1: n2482, h2: n2483,
    };
    // body 8: buttons 0x22, forks 0x0
    sink.o0(34, take_0_8, &sh0, &o0);
    declined |= live_v36_b9 & (if bd_v36_b9 { ALL } else { !ok_v36_b9 });
    take_0_9 |= live_v36_b9 & ok_v36_b9 & (if bd_v36_b9 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n988,
        c41: n989,
        c358: n1028,
        c359: n1034,
        c282: n990,
        c360: n1035,
        c361: n1029,
        c284: n991,
        c285: n992,
        c362: n892,
        c287: n818,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1039,
        c371: n1037,
        c301: n1005,
        h1: n2508, h2: n2509,
    };
    // body 9: buttons 0x24, forks 0x0
    sink.o0(36, take_0_9, &sh0, &o0);
    declined |= live_v37_b10 & (if bd_v37_b10 { ALL } else { !ok_v37_b10 });
    take_0_10 |= live_v37_b10 & ok_v37_b10 & (if bd_v37_b10 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n988,
        c41: n989,
        c358: n1028,
        c359: n1011,
        c282: n990,
        c360: n1012,
        c361: n1029,
        c284: n991,
        c285: n992,
        c362: n934,
        c287: n818,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1045,
        c371: n1043,
        c301: n1005,
        h1: n2524, h2: n2525,
    };
    // body 10: buttons 0x25, forks 0x0
    sink.o0(37, take_0_10, &sh0, &o0);
    declined |= live_v38_b11 & (if bd_v38_b11 { ALL } else { !ok_v38_b11 });
    take_0_11 |= live_v38_b11 & ok_v38_b11 & (if bd_v38_b11 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n988,
        c41: n989,
        c358: n1028,
        c359: n1011,
        c282: n990,
        c360: n1020,
        c361: n1029,
        c284: n991,
        c285: n992,
        c362: n956,
        c287: n818,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1051,
        c371: n1049,
        c301: n1005,
        h1: n2538, h2: n2539,
    };
    // body 11: buttons 0x26, forks 0x0
    sink.o0(38, take_0_11, &sh0, &o0);
    declined |= live_v40_b12 & (if bd_v40_b12 { ALL } else { !ok_v40_b12 });
    take_0_12 |= live_v40_b12 & ok_v40_b12 & (if bd_v40_b12 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n988,
        c41: n989,
        c358: n1028,
        c359: n1034,
        c282: n990,
        c360: n1035,
        c361: n1053,
        c284: n991,
        c285: n992,
        c362: n892,
        c287: n818,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1039,
        c371: n1055,
        c301: n1005,
        h1: n2550, h2: n2551,
    };
    // body 12: buttons 0x28, forks 0x0
    sink.o0(40, take_0_12, &sh0, &o0);
    declined |= live_v41_b13 & (if bd_v41_b13 { ALL } else { !ok_v41_b13 });
    take_0_13 |= live_v41_b13 & ok_v41_b13 & (if bd_v41_b13 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n988,
        c41: n989,
        c358: n1028,
        c359: n1011,
        c282: n990,
        c360: n1012,
        c361: n1053,
        c284: n991,
        c285: n992,
        c362: n934,
        c287: n818,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1045,
        c371: n1057,
        c301: n1005,
        h1: n2560, h2: n2561,
    };
    // body 13: buttons 0x29, forks 0x0
    sink.o0(41, take_0_13, &sh0, &o0);
    declined |= live_v42_b14 & (if bd_v42_b14 { ALL } else { !ok_v42_b14 });
    take_0_14 |= live_v42_b14 & ok_v42_b14 & (if bd_v42_b14 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n988,
        c41: n989,
        c358: n1028,
        c359: n1011,
        c282: n990,
        c360: n1020,
        c361: n1053,
        c284: n991,
        c285: n992,
        c362: n956,
        c287: n818,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1051,
        c371: n1059,
        c301: n1005,
        h1: n2570, h2: n2571,
    };
    // body 14: buttons 0x2a, forks 0x0
    sink.o0(42, take_0_14, &sh0, &o0);
    declined |= live_v48_b15 & (if bd_v48_b15 { ALL } else { !ok_v48_b15 });
    take_0_15 |= live_v48_b15 & ok_v48_b15 & (if bd_v48_b15 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n988,
        c41: n989,
        c358: n993,
        c359: n1000,
        c282: n990,
        c360: n1001,
        c361: n994,
        c284: n991,
        c285: n992,
        c362: n892,
        c287: n964,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n1065,
        c371: n1063,
        c301: n1005,
        h1: n2596, h2: n2597,
    };
    // body 15: buttons 0x30, forks 0x0
    sink.o0(48, take_0_15, &sh0, &o0);
    declined |= live_v49_b16 & (if bd_v49_b16 { ALL } else { !ok_v49_b16 });
    take_0_16 |= live_v49_b16 & ok_v49_b16 & (if bd_v49_b16 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n988,
        c41: n989,
        c358: n993,
        c359: n1011,
        c282: n990,
        c360: n1012,
        c361: n994,
        c284: n991,
        c285: n992,
        c362: n934,
        c287: n964,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n1071,
        c371: n1069,
        c301: n1005,
        h1: n2612, h2: n2613,
    };
    // body 16: buttons 0x31, forks 0x0
    sink.o0(49, take_0_16, &sh0, &o0);
    declined |= live_v50_b17 & (if bd_v50_b17 { ALL } else { !ok_v50_b17 });
    take_0_17 |= live_v50_b17 & ok_v50_b17 & (if bd_v50_b17 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n988,
        c41: n989,
        c358: n993,
        c359: n1011,
        c282: n990,
        c360: n1020,
        c361: n994,
        c284: n991,
        c285: n992,
        c362: n956,
        c287: n964,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n1077,
        c371: n1075,
        c301: n1005,
        h1: n2626, h2: n2627,
    };
    // body 17: buttons 0x32, forks 0x0
    sink.o0(50, take_0_17, &sh0, &o0);
    declined |= live_v52_b18 & (if bd_v52_b18 { ALL } else { !ok_v52_b18 });
    take_0_18 |= live_v52_b18 & ok_v52_b18 & (if bd_v52_b18 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n988,
        c41: n989,
        c358: n1028,
        c359: n1034,
        c282: n990,
        c360: n1035,
        c361: n1029,
        c284: n991,
        c285: n992,
        c362: n892,
        c287: n964,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n1083,
        c371: n1081,
        c301: n1005,
        h1: n2644, h2: n2645,
    };
    // body 18: buttons 0x34, forks 0x0
    sink.o0(52, take_0_18, &sh0, &o0);
    declined |= live_v53_b19 & (if bd_v53_b19 { ALL } else { !ok_v53_b19 });
    take_0_19 |= live_v53_b19 & ok_v53_b19 & (if bd_v53_b19 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n988,
        c41: n989,
        c358: n1028,
        c359: n1011,
        c282: n990,
        c360: n1012,
        c361: n1029,
        c284: n991,
        c285: n992,
        c362: n934,
        c287: n964,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n1089,
        c371: n1087,
        c301: n1005,
        h1: n2660, h2: n2661,
    };
    // body 19: buttons 0x35, forks 0x0
    sink.o0(53, take_0_19, &sh0, &o0);
    declined |= live_v54_b20 & (if bd_v54_b20 { ALL } else { !ok_v54_b20 });
    take_0_20 |= live_v54_b20 & ok_v54_b20 & (if bd_v54_b20 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n988,
        c41: n989,
        c358: n1028,
        c359: n1011,
        c282: n990,
        c360: n1020,
        c361: n1029,
        c284: n991,
        c285: n992,
        c362: n956,
        c287: n964,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n1095,
        c371: n1093,
        c301: n1005,
        h1: n2674, h2: n2675,
    };
    // body 20: buttons 0x36, forks 0x0
    sink.o0(54, take_0_20, &sh0, &o0);
    declined |= live_v56_b21 & (if bd_v56_b21 { ALL } else { !ok_v56_b21 });
    take_0_21 |= live_v56_b21 & ok_v56_b21 & (if bd_v56_b21 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n988,
        c41: n989,
        c358: n1028,
        c359: n1034,
        c282: n990,
        c360: n1035,
        c361: n1053,
        c284: n991,
        c285: n992,
        c362: n892,
        c287: n964,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n1083,
        c371: n1097,
        c301: n1005,
        h1: n2684, h2: n2685,
    };
    // body 21: buttons 0x38, forks 0x0
    sink.o0(56, take_0_21, &sh0, &o0);
    declined |= live_v57_b22 & (if bd_v57_b22 { ALL } else { !ok_v57_b22 });
    take_0_22 |= live_v57_b22 & ok_v57_b22 & (if bd_v57_b22 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n988,
        c41: n989,
        c358: n1028,
        c359: n1011,
        c282: n990,
        c360: n1012,
        c361: n1053,
        c284: n991,
        c285: n992,
        c362: n934,
        c287: n964,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n1089,
        c371: n1099,
        c301: n1005,
        h1: n2694, h2: n2695,
    };
    // body 22: buttons 0x39, forks 0x0
    sink.o0(57, take_0_22, &sh0, &o0);
    declined |= live_v58_b23 & (if bd_v58_b23 { ALL } else { !ok_v58_b23 });
    take_0_23 |= live_v58_b23 & ok_v58_b23 & (if bd_v58_b23 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n988,
        c41: n989,
        c358: n1028,
        c359: n1011,
        c282: n990,
        c360: n1020,
        c361: n1053,
        c284: n991,
        c285: n992,
        c362: n956,
        c287: n964,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n1095,
        c371: n1101,
        c301: n1005,
        h1: n2704, h2: n2705,
    };
    // body 23: buttons 0x3a, forks 0x0
    sink.o0(58, take_0_23, &sh0, &o0);
    declined |= live_v0_b24 & (if bd_v0_b24 { ALL } else { !ok_v0_b24 });
    take_1_0 |= live_v0_b24 & ok_v0_b24 & (if bd_v0_b24 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: r_c41,
        h1: n2726, h2: n2727,
    };
    // body 24: buttons 0x00, forks 0x0
    sink.o1(0, take_1_0, &sh1, &o1);
    declined |= live_v32_b25 & (if bd_v32_b25 { ALL } else { !ok_v32_b25 });
    take_1_1 |= live_v32_b25 & ok_v32_b25 & (if bd_v32_b25 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n988,
        c41: n989,
        h1: n2730, h2: n2731,
    };
    // body 25: buttons 0x20, forks 0x0
    sink.o1(32, take_1_1, &sh1, &o1);
    declined |= live_v0_b26 & (if bd_v0_b26 { ALL } else { !ok_v0_b26 });
    take_2_0 |= live_v0_b26 & ok_v0_b26 & (if bd_v0_b26 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: r_c20,
        c41: r_c41,
        h1: n2742, h2: n2743,
    };
    // body 26: buttons 0x00, forks 0x0
    sink.o2(0, take_2_0, &sh2, &o2);
    declined |= live_v32_b27 & (if bd_v32_b27 { ALL } else { !ok_v32_b27 });
    take_2_1 |= live_v32_b27 & ok_v32_b27 & (if bd_v32_b27 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1876,
        c41: n1877,
        h1: n2750, h2: n2751,
    };
    // body 27: buttons 0x20, forks 0x0
    sink.o2(32, take_2_1, &sh2, &o2);
    declined |= live_v0_b28 & (if bd_v0_b28 { ALL } else { !ok_v0_b28 });
    take_3_0 |= live_v0_b28 & ok_v0_b28 & (if bd_v0_b28 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: r_c20,
        h1: n2770, h2: n2771,
    };
    // body 28: buttons 0x00, forks 0x0
    sink.o3(0, take_3_0, &sh3, &o3);
    declined |= live_v32_b29 & (if bd_v32_b29 { ALL } else { !ok_v32_b29 });
    take_3_1 |= live_v32_b29 & ok_v32_b29 & (if bd_v32_b29 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1876,
        h1: n2772, h2: n2773,
    };
    // body 29: buttons 0x20, forks 0x0
    sink.o3(32, take_3_1, &sh3, &o3);
    declined |= live_v0_b30 & (if bd_v0_b30 { ALL } else { !ok_v0_b30 });
    take_4_0 |= live_v0_b30 & ok_v0_b30 & (if bd_v0_b30 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n1960,
        c41: r_c41,
        c357: r_c357,
        c358: r_c358,
        c281: n1963,
        c359: r_c359,
        c360: r_c360,
        c283: n1964,
        c284: n1965,
        c361: n1969,
        c286: n1966,
        c293: n1891,
        c294: n1892,
        c369: n1985,
        c370: n1973,
        c300: n1984,
        h1: n2854, h2: n2855,
    };
    // body 30: buttons 0x00, forks 0x0
    sink.o4(0, take_4_0, &sh4, &o4);
    declined |= live_v1_b31 & (if bd_v1_b31 { ALL } else { !ok_v1_b31 });
    take_4_1 |= live_v1_b31 & ok_v1_b31 & (if bd_v1_b31 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n1960,
        c41: r_c41,
        c357: r_c357,
        c358: r_c358,
        c281: n1963,
        c359: r_c359,
        c360: r_c360,
        c283: n1964,
        c284: n1965,
        c361: n1998,
        c286: n1966,
        c293: n1891,
        c294: n1892,
        c369: n2002,
        c370: n2000,
        c300: n1984,
        h1: n2866, h2: n2867,
    };
    // body 31: buttons 0x01, forks 0x0
    sink.o4(1, take_4_1, &sh4, &o4);
    declined |= live_v2_b32 & (if bd_v2_b32 { ALL } else { !ok_v2_b32 });
    take_4_2 |= live_v2_b32 & ok_v2_b32 & (if bd_v2_b32 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n1960,
        c41: r_c41,
        c357: r_c357,
        c358: r_c358,
        c281: n1963,
        c359: r_c359,
        c360: r_c360,
        c283: n1964,
        c284: n1965,
        c361: n2014,
        c286: n1966,
        c293: n1891,
        c294: n1892,
        c369: n2018,
        c370: n2016,
        c300: n1984,
        h1: n2878, h2: n2879,
    };
    // body 32: buttons 0x02, forks 0x0
    sink.o4(2, take_4_2, &sh4, &o4);
    declined |= live_v16_b33 & (if bd_v16_b33 { ALL } else { !ok_v16_b33 });
    take_4_3 |= live_v16_b33 & ok_v16_b33 & (if bd_v16_b33 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n1960,
        c41: r_c41,
        c357: r_c357,
        c358: r_c358,
        c281: n1963,
        c359: r_c359,
        c360: r_c360,
        c283: n1964,
        c284: n1965,
        c361: n1969,
        c286: n2026,
        c293: n1891,
        c294: n2019,
        c369: n2030,
        c370: n2028,
        c300: n1984,
        h1: n2908, h2: n2909,
    };
    // body 33: buttons 0x10, forks 0x0
    sink.o4(16, take_4_3, &sh4, &o4);
    declined |= live_v17_b34 & (if bd_v17_b34 { ALL } else { !ok_v17_b34 });
    take_4_4 |= live_v17_b34 & ok_v17_b34 & (if bd_v17_b34 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n1960,
        c41: r_c41,
        c357: r_c357,
        c358: r_c358,
        c281: n1963,
        c359: r_c359,
        c360: r_c360,
        c283: n1964,
        c284: n1965,
        c361: n1998,
        c286: n2026,
        c293: n1891,
        c294: n2019,
        c369: n2038,
        c370: n2036,
        c300: n1984,
        h1: n2918, h2: n2919,
    };
    // body 34: buttons 0x11, forks 0x0
    sink.o4(17, take_4_4, &sh4, &o4);
    declined |= live_v18_b35 & (if bd_v18_b35 { ALL } else { !ok_v18_b35 });
    take_4_5 |= live_v18_b35 & ok_v18_b35 & (if bd_v18_b35 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n1960,
        c41: r_c41,
        c357: r_c357,
        c358: r_c358,
        c281: n1963,
        c359: r_c359,
        c360: r_c360,
        c283: n1964,
        c284: n1965,
        c361: n2014,
        c286: n2026,
        c293: n1891,
        c294: n2019,
        c369: n2046,
        c370: n2044,
        c300: n1984,
        h1: n2928, h2: n2929,
    };
    // body 35: buttons 0x12, forks 0x0
    sink.o4(18, take_4_5, &sh4, &o4);
    declined |= live_v32_b36 & (if bd_v32_b36 { ALL } else { !ok_v32_b36 });
    take_4_6 |= live_v32_b36 & ok_v32_b36 & (if bd_v32_b36 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n2066,
        c41: n2067,
        c357: n2071,
        c358: n2072,
        c281: n2068,
        c359: n2073,
        c360: n2074,
        c283: n2069,
        c284: n2070,
        c361: n1969,
        c286: n1966,
        c293: n2047,
        c294: n1892,
        c369: n2080,
        c370: n2076,
        c300: n2079,
        h1: n2986, h2: n2987,
    };
    // body 36: buttons 0x20, forks 0x0
    sink.o4(32, take_4_6, &sh4, &o4);
    declined |= live_v33_b37 & (if bd_v33_b37 { ALL } else { !ok_v33_b37 });
    take_4_7 |= live_v33_b37 & ok_v33_b37 & (if bd_v33_b37 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n2066,
        c41: n2067,
        c357: n2071,
        c358: n2089,
        c281: n2068,
        c359: n2090,
        c360: n2074,
        c283: n2069,
        c284: n2070,
        c361: n1998,
        c286: n1966,
        c293: n2047,
        c294: n1892,
        c369: n2094,
        c370: n2092,
        c300: n2079,
        h1: n3006, h2: n3007,
    };
    // body 37: buttons 0x21, forks 0x0
    sink.o4(33, take_4_7, &sh4, &o4);
    declined |= live_v34_b38 & (if bd_v34_b38 { ALL } else { !ok_v34_b38 });
    take_4_8 |= live_v34_b38 & ok_v34_b38 & (if bd_v34_b38 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n2066,
        c41: n2067,
        c357: n2071,
        c358: n2089,
        c281: n2068,
        c359: n2101,
        c360: n2074,
        c283: n2069,
        c284: n2070,
        c361: n2014,
        c286: n1966,
        c293: n2047,
        c294: n1892,
        c369: n2105,
        c370: n2103,
        c300: n2079,
        h1: n3022, h2: n3023,
    };
    // body 38: buttons 0x22, forks 0x0
    sink.o4(34, take_4_8, &sh4, &o4);
    declined |= live_v36_b39 & (if bd_v36_b39 { ALL } else { !ok_v36_b39 });
    take_4_9 |= live_v36_b39 & ok_v36_b39 & (if bd_v36_b39 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n2066,
        c41: n2067,
        c357: n2118,
        c358: n2119,
        c281: n2068,
        c359: n2120,
        c360: n2121,
        c283: n2069,
        c284: n2070,
        c361: n1969,
        c286: n1966,
        c293: n2047,
        c294: n1892,
        c369: n2125,
        c370: n2123,
        c300: n2079,
        h1: n3048, h2: n3049,
    };
    // body 39: buttons 0x24, forks 0x0
    sink.o4(36, take_4_9, &sh4, &o4);
    declined |= live_v37_b40 & (if bd_v37_b40 { ALL } else { !ok_v37_b40 });
    take_4_10 |= live_v37_b40 & ok_v37_b40 & (if bd_v37_b40 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n2066,
        c41: n2067,
        c357: n2118,
        c358: n2089,
        c281: n2068,
        c359: n2090,
        c360: n2121,
        c283: n2069,
        c284: n2070,
        c361: n1998,
        c286: n1966,
        c293: n2047,
        c294: n1892,
        c369: n2133,
        c370: n2131,
        c300: n2079,
        h1: n3064, h2: n3065,
    };
    // body 40: buttons 0x25, forks 0x0
    sink.o4(37, take_4_10, &sh4, &o4);
    declined |= live_v38_b41 & (if bd_v38_b41 { ALL } else { !ok_v38_b41 });
    take_4_11 |= live_v38_b41 & ok_v38_b41 & (if bd_v38_b41 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n2066,
        c41: n2067,
        c357: n2118,
        c358: n2089,
        c281: n2068,
        c359: n2101,
        c360: n2121,
        c283: n2069,
        c284: n2070,
        c361: n2014,
        c286: n1966,
        c293: n2047,
        c294: n1892,
        c369: n2141,
        c370: n2139,
        c300: n2079,
        h1: n3078, h2: n3079,
    };
    // body 41: buttons 0x26, forks 0x0
    sink.o4(38, take_4_11, &sh4, &o4);
    declined |= live_v40_b42 & (if bd_v40_b42 { ALL } else { !ok_v40_b42 });
    take_4_12 |= live_v40_b42 & ok_v40_b42 & (if bd_v40_b42 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n2066,
        c41: n2067,
        c357: n2118,
        c358: n2119,
        c281: n2068,
        c359: n2120,
        c360: n2146,
        c283: n2069,
        c284: n2070,
        c361: n1969,
        c286: n1966,
        c293: n2047,
        c294: n1892,
        c369: n2125,
        c370: n2147,
        c300: n2079,
        h1: n3090, h2: n3091,
    };
    // body 42: buttons 0x28, forks 0x0
    sink.o4(40, take_4_12, &sh4, &o4);
    declined |= live_v41_b43 & (if bd_v41_b43 { ALL } else { !ok_v41_b43 });
    take_4_13 |= live_v41_b43 & ok_v41_b43 & (if bd_v41_b43 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n2066,
        c41: n2067,
        c357: n2118,
        c358: n2089,
        c281: n2068,
        c359: n2090,
        c360: n2146,
        c283: n2069,
        c284: n2070,
        c361: n1998,
        c286: n1966,
        c293: n2047,
        c294: n1892,
        c369: n2133,
        c370: n2150,
        c300: n2079,
        h1: n3100, h2: n3101,
    };
    // body 43: buttons 0x29, forks 0x0
    sink.o4(41, take_4_13, &sh4, &o4);
    declined |= live_v42_b44 & (if bd_v42_b44 { ALL } else { !ok_v42_b44 });
    take_4_14 |= live_v42_b44 & ok_v42_b44 & (if bd_v42_b44 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n2066,
        c41: n2067,
        c357: n2118,
        c358: n2089,
        c281: n2068,
        c359: n2101,
        c360: n2146,
        c283: n2069,
        c284: n2070,
        c361: n2014,
        c286: n1966,
        c293: n2047,
        c294: n1892,
        c369: n2141,
        c370: n2153,
        c300: n2079,
        h1: n3110, h2: n3111,
    };
    // body 44: buttons 0x2a, forks 0x0
    sink.o4(42, take_4_14, &sh4, &o4);
    declined |= live_v48_b45 & (if bd_v48_b45 { ALL } else { !ok_v48_b45 });
    take_4_15 |= live_v48_b45 & ok_v48_b45 & (if bd_v48_b45 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n2066,
        c41: n2067,
        c357: n2071,
        c358: n2072,
        c281: n2068,
        c359: n2073,
        c360: n2074,
        c283: n2069,
        c284: n2070,
        c361: n1969,
        c286: n2026,
        c293: n2047,
        c294: n2019,
        c369: n2161,
        c370: n2159,
        c300: n2079,
        h1: n3136, h2: n3137,
    };
    // body 45: buttons 0x30, forks 0x0
    sink.o4(48, take_4_15, &sh4, &o4);
    declined |= live_v49_b46 & (if bd_v49_b46 { ALL } else { !ok_v49_b46 });
    take_4_16 |= live_v49_b46 & ok_v49_b46 & (if bd_v49_b46 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n2066,
        c41: n2067,
        c357: n2071,
        c358: n2089,
        c281: n2068,
        c359: n2090,
        c360: n2074,
        c283: n2069,
        c284: n2070,
        c361: n1998,
        c286: n2026,
        c293: n2047,
        c294: n2019,
        c369: n2169,
        c370: n2167,
        c300: n2079,
        h1: n3152, h2: n3153,
    };
    // body 46: buttons 0x31, forks 0x0
    sink.o4(49, take_4_16, &sh4, &o4);
    declined |= live_v50_b47 & (if bd_v50_b47 { ALL } else { !ok_v50_b47 });
    take_4_17 |= live_v50_b47 & ok_v50_b47 & (if bd_v50_b47 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n2066,
        c41: n2067,
        c357: n2071,
        c358: n2089,
        c281: n2068,
        c359: n2101,
        c360: n2074,
        c283: n2069,
        c284: n2070,
        c361: n2014,
        c286: n2026,
        c293: n2047,
        c294: n2019,
        c369: n2177,
        c370: n2175,
        c300: n2079,
        h1: n3166, h2: n3167,
    };
    // body 47: buttons 0x32, forks 0x0
    sink.o4(50, take_4_17, &sh4, &o4);
    declined |= live_v52_b48 & (if bd_v52_b48 { ALL } else { !ok_v52_b48 });
    take_4_18 |= live_v52_b48 & ok_v52_b48 & (if bd_v52_b48 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n2066,
        c41: n2067,
        c357: n2118,
        c358: n2119,
        c281: n2068,
        c359: n2120,
        c360: n2121,
        c283: n2069,
        c284: n2070,
        c361: n1969,
        c286: n2026,
        c293: n2047,
        c294: n2019,
        c369: n2185,
        c370: n2183,
        c300: n2079,
        h1: n3184, h2: n3185,
    };
    // body 48: buttons 0x34, forks 0x0
    sink.o4(52, take_4_18, &sh4, &o4);
    declined |= live_v53_b49 & (if bd_v53_b49 { ALL } else { !ok_v53_b49 });
    take_4_19 |= live_v53_b49 & ok_v53_b49 & (if bd_v53_b49 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n2066,
        c41: n2067,
        c357: n2118,
        c358: n2089,
        c281: n2068,
        c359: n2090,
        c360: n2121,
        c283: n2069,
        c284: n2070,
        c361: n1998,
        c286: n2026,
        c293: n2047,
        c294: n2019,
        c369: n2193,
        c370: n2191,
        c300: n2079,
        h1: n3200, h2: n3201,
    };
    // body 49: buttons 0x35, forks 0x0
    sink.o4(53, take_4_19, &sh4, &o4);
    declined |= live_v54_b50 & (if bd_v54_b50 { ALL } else { !ok_v54_b50 });
    take_4_20 |= live_v54_b50 & ok_v54_b50 & (if bd_v54_b50 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n2066,
        c41: n2067,
        c357: n2118,
        c358: n2089,
        c281: n2068,
        c359: n2101,
        c360: n2121,
        c283: n2069,
        c284: n2070,
        c361: n2014,
        c286: n2026,
        c293: n2047,
        c294: n2019,
        c369: n2201,
        c370: n2199,
        c300: n2079,
        h1: n3214, h2: n3215,
    };
    // body 50: buttons 0x36, forks 0x0
    sink.o4(54, take_4_20, &sh4, &o4);
    declined |= live_v56_b51 & (if bd_v56_b51 { ALL } else { !ok_v56_b51 });
    take_4_21 |= live_v56_b51 & ok_v56_b51 & (if bd_v56_b51 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n2066,
        c41: n2067,
        c357: n2118,
        c358: n2119,
        c281: n2068,
        c359: n2120,
        c360: n2146,
        c283: n2069,
        c284: n2070,
        c361: n1969,
        c286: n2026,
        c293: n2047,
        c294: n2019,
        c369: n2185,
        c370: n2204,
        c300: n2079,
        h1: n3224, h2: n3225,
    };
    // body 51: buttons 0x38, forks 0x0
    sink.o4(56, take_4_21, &sh4, &o4);
    declined |= live_v57_b52 & (if bd_v57_b52 { ALL } else { !ok_v57_b52 });
    take_4_22 |= live_v57_b52 & ok_v57_b52 & (if bd_v57_b52 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n2066,
        c41: n2067,
        c357: n2118,
        c358: n2089,
        c281: n2068,
        c359: n2090,
        c360: n2146,
        c283: n2069,
        c284: n2070,
        c361: n1998,
        c286: n2026,
        c293: n2047,
        c294: n2019,
        c369: n2193,
        c370: n2207,
        c300: n2079,
        h1: n3234, h2: n3235,
    };
    // body 52: buttons 0x39, forks 0x0
    sink.o4(57, take_4_22, &sh4, &o4);
    declined |= live_v58_b53 & (if bd_v58_b53 { ALL } else { !ok_v58_b53 });
    take_4_23 |= live_v58_b53 & ok_v58_b53 & (if bd_v58_b53 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n2066,
        c41: n2067,
        c357: n2118,
        c358: n2089,
        c281: n2068,
        c359: n2101,
        c360: n2146,
        c283: n2069,
        c284: n2070,
        c361: n2014,
        c286: n2026,
        c293: n2047,
        c294: n2019,
        c369: n2201,
        c370: n2210,
        c300: n2079,
        h1: n3244, h2: n3245,
    };
    // body 53: buttons 0x3a, forks 0x0
    sink.o4(58, take_4_23, &sh4, &o4);
    declined
}
