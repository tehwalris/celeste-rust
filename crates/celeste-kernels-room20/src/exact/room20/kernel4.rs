// GENERATED from a TRACED frame (shape 4). Do not edit.
//
// One input shape, 3 output shapes, 28 distinct button
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
pub const SHAPE: u64 = 7975170682095821243;

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
    pub c340: P8,
    pub c341: P8,
    pub c342: P8,
    pub c343: P8,
    pub c350: P8,
    pub c351: P8,
    pub c352: P8,
    pub c353: P8,
    pub c364: P8,
    pub c365: P8,
    pub c366: P8,
    pub c367: P8,
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
    pub c241: ZN,
    pub c338: u16,
    pub c339: u16,
    pub c243: ZN,
    pub c244: ZN,
    pub c344: ZN,
    pub c345: ZN,
    pub c252: u16,
    pub c346: ZN,
    pub c347: ZN,
    pub c254: ZN,
    pub c256: ZN,
    pub c257: ZN,
    pub c260: u16,
    pub c261: ZN,
    pub c348: u16,
    pub c349: u16,
    pub c263: ZN,
    pub c264: ZN,
    pub c354: ZN,
    pub c355: ZN,
    pub c272: u16,
    pub c356: ZN,
    pub c357: ZN,
    pub c274: ZN,
    pub c276: ZN,
    pub c277: ZN,
    pub c280: u16,
    pub c358: ZN,
    pub c359: ZN,
    pub c282: ZN,
    pub c360: ZN,
    pub c361: ZN,
    pub c284: ZN,
    pub c285: ZN,
    pub c362: u16,
    pub c363: u16,
    pub c287: ZN,
    pub c294: u16,
    pub c295: u16,
    pub c368: ZN,
    pub c369: ZN,
    pub c297: u16,
    pub c370: ZN,
    pub c371: ZN,
    pub c301: ZN,
    pub c302: ZN,
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
    pub c338: u32,
    pub c339: u32,
    pub c243: u32,
    pub c244: u32,
    pub c344: u32,
    pub c345: u32,
    pub c252: u32,
    pub c346: u32,
    pub c347: u32,
    pub c254: u32,
    pub c256: u32,
    pub c257: u32,
    pub c260: u32,
    pub c261: u32,
    pub c348: u32,
    pub c349: u32,
    pub c263: u32,
    pub c264: u32,
    pub c354: u32,
    pub c355: u32,
    pub c272: u32,
    pub c356: u32,
    pub c357: u32,
    pub c274: u32,
    pub c276: u32,
    pub c277: u32,
    pub c280: u32,
    pub c358: u32,
    pub c359: u32,
    pub c282: u32,
    pub c360: u32,
    pub c361: u32,
    pub c284: u32,
    pub c285: u32,
    pub c362: u32,
    pub c363: u32,
    pub c287: u32,
    pub c294: u32,
    pub c295: u32,
    pub c368: u32,
    pub c369: u32,
    pub c297: u32,
    pub c370: u32,
    pub c371: u32,
    pub c301: u32,
    pub c302: u32,
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
        c340: match &b.cols[cell("objects[0].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c341: match &b.cols[cell("objects[0].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c342: match &b.cols[cell("objects[0].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c343: match &b.cols[cell("objects[0].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c350: match &b.cols[cell("objects[1].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c351: match &b.cols[cell("objects[1].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c352: match &b.cols[cell("objects[1].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c353: match &b.cols[cell("objects[1].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c364: match &b.cols[cell("objects[2].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c365: match &b.cols[cell("objects[2].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c366: match &b.cols[cell("objects[2].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c367: match &b.cols[cell("objects[2].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
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
        c338: cell("objects[0].flip.x")?,
        c339: cell("objects[0].flip.y")?,
        c243: cell("objects[0].hide_for")?,
        c244: cell("objects[0].hide_in")?,
        c344: cell("objects[0].rem.x")?,
        c345: cell("objects[0].rem.y")?,
        c252: cell("objects[0].solids")?,
        c346: cell("objects[0].spd.x")?,
        c347: cell("objects[0].spd.y")?,
        c254: cell("objects[0].spr")?,
        c256: cell("objects[0].x")?,
        c257: cell("objects[0].y")?,
        c260: cell("objects[1].collideable")?,
        c261: cell("objects[1].delay")?,
        c348: cell("objects[1].flip.x")?,
        c349: cell("objects[1].flip.y")?,
        c263: cell("objects[1].hide_for")?,
        c264: cell("objects[1].hide_in")?,
        c354: cell("objects[1].rem.x")?,
        c355: cell("objects[1].rem.y")?,
        c272: cell("objects[1].solids")?,
        c356: cell("objects[1].spd.x")?,
        c357: cell("objects[1].spd.y")?,
        c274: cell("objects[1].spr")?,
        c276: cell("objects[1].x")?,
        c277: cell("objects[1].y")?,
        c280: cell("objects[2].collideable")?,
        c358: cell("objects[2].dash_accel.x")?,
        c359: cell("objects[2].dash_accel.y")?,
        c282: cell("objects[2].dash_effect_time")?,
        c360: cell("objects[2].dash_target.x")?,
        c361: cell("objects[2].dash_target.y")?,
        c284: cell("objects[2].dash_time")?,
        c285: cell("objects[2].djump")?,
        c362: cell("objects[2].flip.x")?,
        c363: cell("objects[2].flip.y")?,
        c287: cell("objects[2].grace")?,
        c294: cell("objects[2].p_dash")?,
        c295: cell("objects[2].p_jump")?,
        c368: cell("objects[2].rem.x")?,
        c369: cell("objects[2].rem.y")?,
        c297: cell("objects[2].solids")?,
        c370: cell("objects[2].spd.x")?,
        c371: cell("objects[2].spd.y")?,
        c301: cell("objects[2].x")?,
        c302: cell("objects[2].y")?,
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
        c339: match &b.cols[s.c339 as usize] {
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
        c346: match &b.cols[s.c346 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c347: match &b.cols[s.c347 as usize] {
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
        c261: match &b.cols[s.c261 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
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
        c349: match &b.cols[s.c349 as usize] {
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
        c263: match &b.cols[s.c263 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c264: match &b.cols[s.c264 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
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
        c274: match &b.cols[s.c274 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c276: match &b.cols[s.c276 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c277: match &b.cols[s.c277 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c280: match &b.cols[s.c280 as usize] {
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
        c360: match &b.cols[s.c360 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c361: match &b.cols[s.c361 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c284: match &b.cols[s.c284 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c285: match &b.cols[s.c285 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
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
        c363: match &b.cols[s.c363 as usize] {
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
        c287: match &b.cols[s.c287 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
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

/// Outcome 0's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared0 {
    pub c87: ZN,
    pub c84: ZN,
    pub c86: ZN,
    pub c240: ZN,
    pub c253: ZN,
    pub c260: ZN,
    pub c273: ZN,
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
    pub c85: ZN,
    pub c38: ZB,
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
    pub c241: ZN,
    pub c254: ZN,
    pub c261: ZN,
    pub c274: ZN,
    pub c368: ZN,
    pub c369: ZN,
    pub c302: ZN,
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

/// An EMPTY accumulator with outcome 1's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append1`.
pub fn acc1(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_1, OUT_GLOBALS_1, OUT_PTRS_1, 0, cart, cache);
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

pub const OUTCOMES: usize = 3;

pub fn acc(i: usize, cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    match i {
        0 => acc0(cart, cache),
        1 => acc1(cart, cache),
        2 => acc2(cart, cache),
        _ => panic!("outcome {} of 3", i),
    }
}

pub fn out_slots(i: usize) -> &'static [(u32, &'static str)] {
    match i {
        0 => OUT_SLOTS_0,
        1 => OUT_SLOTS_1,
        2 => OUT_SLOTS_2,
        _ => panic!("outcome {} of 3", i),
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
    let r_c261: ZN = rin.c261;
    let r_c263: ZN = rin.c263;
    let r_c264: ZN = rin.c264;
    let r_c272: ZB = ZB { val: rin.c272, known: ALL };
    let r_c274: ZN = rin.c274;
    let r_c276: ZN = rin.c276;
    let r_c277: ZN = rin.c277;
    let r_c280: ZB = ZB { val: rin.c280, known: ALL };
    let r_c282: ZN = rin.c282;
    let r_c284: ZN = rin.c284;
    let r_c285: ZN = rin.c285;
    let r_c287: ZN = rin.c287;
    let r_c294: ZB = ZB { val: rin.c294, known: ALL };
    let r_c295: ZB = ZB { val: rin.c295, known: ALL };
    let r_c297: ZB = ZB { val: rin.c297, known: ALL };
    let r_c301: ZN = rin.c301;
    let r_c302: ZN = rin.c302;
    let r_c338: ZB = ZB { val: rin.c338, known: ALL };
    let r_c339: ZB = ZB { val: rin.c339, known: ALL };
    let r_c344: ZN = rin.c344;
    let r_c345: ZN = rin.c345;
    let r_c346: ZN = rin.c346;
    let r_c347: ZN = rin.c347;
    let r_c348: ZB = ZB { val: rin.c348, known: ALL };
    let r_c349: ZB = ZB { val: rin.c349, known: ALL };
    let r_c354: ZN = rin.c354;
    let r_c355: ZN = rin.c355;
    let r_c356: ZN = rin.c356;
    let r_c357: ZN = rin.c357;
    let r_c358: ZN = rin.c358;
    let r_c359: ZN = rin.c359;
    let r_c360: ZN = rin.c360;
    let r_c361: ZN = rin.c361;
    let r_c362: ZB = ZB { val: rin.c362, known: ALL };
    let r_c363: ZB = ZB { val: rin.c363, known: ALL };
    let r_c368: ZN = rin.c368;
    let r_c369: ZN = rin.c369;
    let r_c370: ZN = rin.c370;
    let r_c371: ZN = rin.c371;
    let n99: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c243);
    let n100: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c244);
    let n101: bool = P8::from_raw(0i32) == u.c342;
    let n102: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c344);
    let n103: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c345);
    let n104: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c263);
    let n105: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c264);
    let n106: bool = P8::from_raw(0i32) == u.c353;
    let n110: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c84);
    let n111: ZN = zn_rem(n110, zn_splat(P8::from_raw(1966080i32)));
    let n112: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n111);
    let n121: bool = P8::from_raw(0i32) == u.c352;
    let n122: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c354);
    let n123: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c355);
    let n124: ZB = zn_le(r_c20, zn_splat(P8::from_raw(0i32)));
    let n128: ZB = zb_not(r_c294);
    let n129: ZB = zb_not(r_c42);
    let n130: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), r_c88);
    let n131: ZB = zb_not(r_c338);
    let n132: bool = P8::from_raw(524288i32) == u.c340;
    let n133: bool = P8::from_raw(0i32) == u.c343;
    let n134: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c346);
    let n135: ZB = zb_not(r_c348);
    let n136: bool = P8::from_raw(524288i32) == u.c350;
    let n137: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c356);
    let n138: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c86);
    let n142: ZB = zb_not(r_c339);
    let n143: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c347);
    let n144: ZB = zb_not(r_c349);
    let n145: bool = P8::from_raw(524288i32) == u.c351;
    let n146: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c357);
    let n147: ZB = zn_eq(zn_splat(P8::from_raw(1179648i32)), r_c274);
    let n148: ZB = zn_eq(zn_splat(P8::from_raw(7340032i32)), r_c277);
    let n149: ZN = zn_sub(r_c241, zn_splat(P8::from_raw(65536i32)));
    let n150: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c87);
    let n151: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n150);
    let n152: ZB = zn_gt(r_c241, zn_splat(P8::from_raw(0i32)));
    let n153: bool = P8::from_raw(524288i32) == u.c341;
    let n154: ZB = zn_eq(zn_splat(P8::from_raw(2621440i32)), r_c256);
    let n155: ZB = zn_eq(zn_splat(P8::from_raw(7340032i32)), r_c257);
    let n169: ZB = zn_le(n149, zn_splat(P8::from_raw(0i32)));
    let n170: ZN = zsel_n(n152, n149, r_c241);
    let n174: ZB = zn_eq(zn_splat(P8::from_raw(1179648i32)), r_c254);
    let n175: ZB = zb_not(n174);
    let n176: ZN = zsel_n(n169, zn_splat(P8::from_raw(1179648i32)), r_c254);
    let n177: ZN = zsel_n(n152, n176, r_c254);
    let n178: ZB = zb_not(r_c43);
    let n179: ZB = zb_not(r_c295);
    let n180: ZB = zn_eq(zn_splat(P8::from_raw(6815744i32)), r_c276);
    let n181: ZB = zb_not(n147);
    let n182: ZB = zn_gt(r_c261, zn_splat(P8::from_raw(0i32)));
    let n183: ZN = zn_sub(r_c261, zn_splat(P8::from_raw(65536i32)));
    let n184: ZB = zn_le(n183, zn_splat(P8::from_raw(0i32)));
    let n185: ZN = zsel_n(n184, zn_splat(P8::from_raw(1179648i32)), r_c274);
    let n186: ZN = zsel_n(n182, n183, r_c261);
    let n187: ZN = zsel_n(n182, n185, r_c274);
    let n203: ZB = zb_not(r_c363);
    let n204: bool = P8::from_raw(327680i32) == u.c364;
    let n205: bool = P8::from_raw(393216i32) == u.c365;
    let n206: bool = P8::from_raw(65536i32) == u.c366;
    let n207: bool = P8::from_raw(196608i32) == u.c367;
    let n208: ZB = zb_not(r_c38);
    let n209: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c85);
    let n210: ZN = zn_rem(n209, zn_splat(P8::from_raw(3932160i32)));
    let n211: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n210);
    let n212: ZN = zsel_n(n211, n138, r_c86);
    let n213: ZN = zsel_n(n112, n212, r_c86);
    let n214: ZN = zsel_n(n112, n210, r_c85);
    let n215: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c301);
    let n216: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n215);
    let n217: ZB = zn_gt(n216, zn_splat(P8::from_raw(2621440i32)));
    let n218: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), r_c302);
    let n219: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n218);
    let n220: ZB = zn_gt(n219, zn_splat(P8::from_raw(7340032i32)));
    let n221: ZB = zb_and(n217, n220);
    let n222: ZB = zn_lt(n215, zn_splat(P8::from_raw(3145728i32)));
    let n223: ZB = zb_and(n221, n222);
    let n224: ZB = zn_lt(n218, zn_splat(P8::from_raw(7864320i32)));
    let n225: ZB = zb_and(n223, n224);
    let n226: ZB = zn_ge(r_c371, zn_splat(P8::from_raw(0i32)));
    let n227: ZN = zn_mul(r_c370, zn_splat(P8::from_raw(13107i32)));
    let n228: ZN = zsel_n(n226, zn_splat(P8::from_raw(65536i32)), r_c285);
    let n229: ZN = zsel_n(n226, zn_splat(P8::from_raw(7077888i32)), r_c302);
    let n230: ZN = zsel_n(n226, zn_splat(P8::from_raw(655360i32)), r_c241);
    let n231: ZN = zsel_n(n226, zn_splat(P8::from_raw(1245184i32)), r_c254);
    let n232: ZN = zsel_n(n226, n227, r_c370);
    let n233: ZN = zsel_n(n226, zn_splat(P8::from_raw(-196608i32)), r_c371);
    let n234: ZN = zsel_n(n225, n230, r_c241);
    let n235: ZN = zsel_n(n225, n231, r_c254);
    let n236: ZN = zsel_n(n225, n228, r_c285);
    let n237: ZN = zsel_n(n225, n229, r_c302);
    let n238: ZN = zsel_n(n225, n232, r_c370);
    let n239: ZN = zsel_n(n225, n233, r_c371);
    let n240: ZN = zsel_n(n175, n170, n234);
    let n241: ZN = zsel_n(n175, n177, n235);
    let n242: ZN = zsel_n(n175, r_c285, n236);
    let n243: ZN = zsel_n(n175, r_c302, n237);
    let n244: ZN = zsel_n(n175, r_c370, n238);
    let n245: ZN = zsel_n(n175, r_c371, n239);
    let n246: ZB = zn_gt(n216, zn_splat(P8::from_raw(6815744i32)));
    let n247: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n243);
    let n248: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n247);
    let n249: ZB = zn_gt(n248, zn_splat(P8::from_raw(7340032i32)));
    let n250: ZB = zb_and(n246, n249);
    let n251: ZB = zn_lt(n215, zn_splat(P8::from_raw(7340032i32)));
    let n252: ZB = zb_and(n250, n251);
    let n253: ZB = zn_lt(n247, zn_splat(P8::from_raw(7864320i32)));
    let n254: ZB = zb_and(n252, n253);
    let n255: ZB = zn_ge(n245, zn_splat(P8::from_raw(0i32)));
    let n256: ZN = zn_mul(n244, zn_splat(P8::from_raw(13107i32)));
    let n257: ZN = zsel_n(n255, zn_splat(P8::from_raw(65536i32)), n242);
    let n258: ZN = zsel_n(n255, zn_splat(P8::from_raw(7077888i32)), n243);
    let n259: ZN = zsel_n(n255, zn_splat(P8::from_raw(655360i32)), r_c261);
    let n260: ZN = zsel_n(n255, zn_splat(P8::from_raw(1245184i32)), r_c274);
    let n261: ZN = zsel_n(n255, n256, n244);
    let n262: ZN = zsel_n(n255, zn_splat(P8::from_raw(-196608i32)), n245);
    let n263: ZN = zsel_n(n254, n259, r_c261);
    let n264: ZN = zsel_n(n254, n260, r_c274);
    let n265: ZN = zsel_n(n254, n257, n242);
    let n266: ZN = zsel_n(n254, n258, n243);
    let n267: ZN = zsel_n(n254, n261, n244);
    let n268: ZN = zsel_n(n254, n262, n245);
    let n269: ZN = zsel_n(n181, n186, n263);
    let n270: ZN = zsel_n(n181, n187, n264);
    let n271: ZN = zsel_n(n181, n242, n265);
    let n272: ZN = zsel_n(n181, n243, n266);
    let n273: ZN = zsel_n(n181, n244, n267);
    let n274: ZN = zsel_n(n181, n245, n268);
    let n275: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n273);
    let n276: ZB = zb_not(n275);
    let n277: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n274);
    let n278: ZB = zb_not(n277);
    let n279: ZB = zb_or(n276, n278);
    let n280: ZB = zb_not(n279);
    let n281: ZN = zn_add(r_c368, n273);
    let n282: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n281);
    let n283: ZN = zn_flr(n282);
    let n284: ZB = zn_gt(n283, zn_splat(P8::from_raw(0i32)));
    let n285: ZB = zn_lt(n283, zn_splat(P8::from_raw(0i32)));
    let n286: ZN = zsel_n(n285, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n287: ZN = zsel_n(n284, zn_splat(P8::from_raw(65536i32)), n286);
    let n288: ZN = zn_abs(n283);
    let n289: ZN = zn_add(n215, n287);
    let n290: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n272);
    let n291: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n290);
    let n292: ZB = zn_tile_flag_at(g.cache, g.cart, n289, n291, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n293: ZN = zn_add(r_c301, n287);
    let n294: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n288);
    let n295: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n293);
    let n296: ZN = zn_add(n287, n295);
    let n297: ZB = zn_tile_flag_at(g.cache, g.cart, n296, n291, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n298: ZN = zn_add(n287, n293);
    let n299: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n288);
    let n300: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n298);
    let n301: ZN = zn_add(n287, n300);
    let n302: ZB = zn_tile_flag_at(g.cache, g.cart, n301, n291, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n303: ZN = zn_add(n287, n298);
    let n304: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n288);
    let n305: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n303);
    let n306: ZN = zn_add(n287, n305);
    let n307: ZB = zn_tile_flag_at(g.cache, g.cart, n306, n291, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n308: ZN = zn_add(n287, n303);
    let n309: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n288);
    let n310: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n308);
    let n311: ZN = zn_add(n287, n310);
    let n312: ZB = zn_tile_flag_at(g.cache, g.cart, n311, n291, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n313: ZN = zn_add(n287, n308);
    let n314: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n288);
    let n315: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n313);
    let n316: ZN = zn_add(n287, n315);
    let n317: ZB = zn_tile_flag_at(g.cache, g.cart, n316, n291, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n318: ZN = zn_add(n287, n313);
    let n319: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n288);
    let n320: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n318);
    let n321: ZN = zn_add(n287, n320);
    let n322: ZB = zn_tile_flag_at(g.cache, g.cart, n321, n291, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n323: ZN = zn_add(n287, n318);
    let n324: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n288);
    let n325: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n323);
    let n326: ZN = zn_add(n287, n325);
    let n327: ZB = zn_tile_flag_at(g.cache, g.cart, n326, n291, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n328: ZN = zn_add(n287, n323);
    let n329: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n288);
    let n330: ZN = zsel_n(n327, n323, n328);
    let n331: ZN = zsel_n(n327, zn_splat(P8::from_raw(0i32)), n273);
    let n332: ZB = zb_or(n327, n329);
    let n333: ZN = zsel_n(n324, n323, n330);
    let n334: ZN = zsel_n(n324, n273, n331);
    let n335: ZB = zb_or(n324, n332);
    let n336: ZN = zsel_n(n322, n318, n333);
    let n337: ZN = zsel_n(n322, zn_splat(P8::from_raw(0i32)), n334);
    let n338: ZB = zb_or(n322, n335);
    let n339: ZN = zsel_n(n319, n318, n336);
    let n340: ZN = zsel_n(n319, n273, n337);
    let n341: ZB = zb_or(n319, n338);
    let n342: ZN = zsel_n(n317, n313, n339);
    let n343: ZN = zsel_n(n317, zn_splat(P8::from_raw(0i32)), n340);
    let n344: ZB = zb_or(n317, n341);
    let n345: ZN = zsel_n(n314, n313, n342);
    let n346: ZN = zsel_n(n314, n273, n343);
    let n347: ZB = zb_or(n314, n344);
    let n348: ZN = zsel_n(n312, n308, n345);
    let n349: ZN = zsel_n(n312, zn_splat(P8::from_raw(0i32)), n346);
    let n350: ZB = zb_or(n312, n347);
    let n351: ZN = zsel_n(n309, n308, n348);
    let n352: ZN = zsel_n(n309, n273, n349);
    let n353: ZB = zb_or(n309, n350);
    let n354: ZN = zsel_n(n307, n303, n351);
    let n355: ZN = zsel_n(n307, zn_splat(P8::from_raw(0i32)), n352);
    let n356: ZB = zb_or(n307, n353);
    let n357: ZN = zsel_n(n304, n303, n354);
    let n358: ZN = zsel_n(n304, n273, n355);
    let n359: ZB = zb_or(n304, n356);
    let n360: ZN = zsel_n(n302, n298, n357);
    let n361: ZN = zsel_n(n302, zn_splat(P8::from_raw(0i32)), n358);
    let n362: ZB = zb_or(n302, n359);
    let n363: ZN = zsel_n(n299, n298, n360);
    let n364: ZN = zsel_n(n299, n273, n361);
    let n365: ZB = zb_or(n299, n362);
    let n366: ZN = zsel_n(n297, n293, n363);
    let n367: ZN = zsel_n(n297, zn_splat(P8::from_raw(0i32)), n364);
    let n368: ZB = zb_or(n297, n365);
    let n369: ZN = zsel_n(n294, n293, n366);
    let n370: ZN = zsel_n(n294, n273, n367);
    let n371: ZB = zb_or(n294, n368);
    let n372: ZN = zsel_n(n292, r_c301, n369);
    let n373: ZN = zsel_n(n292, zn_splat(P8::from_raw(0i32)), n370);
    let n374: ZB = zb_or(n292, n371);
    let n375: ZN = zn_add(r_c369, n274);
    let n376: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n375);
    let n377: ZN = zn_flr(n376);
    let n378: ZB = zn_gt(n377, zn_splat(P8::from_raw(0i32)));
    let n379: ZB = zn_lt(n377, zn_splat(P8::from_raw(0i32)));
    let n380: ZN = zsel_n(n379, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n381: ZN = zsel_n(n378, zn_splat(P8::from_raw(65536i32)), n380);
    let n382: ZN = zn_abs(n377);
    let n383: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n372);
    let n384: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n383);
    let n385: ZN = zn_add(n290, n381);
    let n386: ZB = zn_tile_flag_at(g.cache, g.cart, n384, n385, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n387: ZN = zn_add(n272, n381);
    let n388: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n382);
    let n389: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n387);
    let n390: ZN = zn_add(n381, n389);
    let n391: ZB = zn_tile_flag_at(g.cache, g.cart, n384, n390, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n392: ZN = zn_add(n381, n387);
    let n393: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n382);
    let n394: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n392);
    let n395: ZN = zn_add(n381, n394);
    let n396: ZB = zn_tile_flag_at(g.cache, g.cart, n384, n395, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n397: ZN = zn_add(n381, n392);
    let n398: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n382);
    let n399: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n397);
    let n400: ZN = zn_add(n381, n399);
    let n401: ZB = zn_tile_flag_at(g.cache, g.cart, n384, n400, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n402: ZN = zn_add(n381, n397);
    let n403: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n382);
    let n404: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n402);
    let n405: ZN = zn_add(n381, n404);
    let n406: ZB = zn_tile_flag_at(g.cache, g.cart, n384, n405, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n407: ZN = zn_add(n381, n402);
    let n408: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n382);
    let n409: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n407);
    let n410: ZN = zn_add(n381, n409);
    let n411: ZB = zn_tile_flag_at(g.cache, g.cart, n384, n410, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n412: ZN = zn_add(n381, n407);
    let n413: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n382);
    let n414: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n412);
    let n415: ZN = zn_add(n381, n414);
    let n416: ZB = zn_tile_flag_at(g.cache, g.cart, n384, n415, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n417: ZN = zn_add(n381, n412);
    let n418: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n382);
    let n419: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n417);
    let n420: ZN = zn_add(n381, n419);
    let n421: ZB = zn_tile_flag_at(g.cache, g.cart, n384, n420, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n422: ZN = zn_add(n381, n417);
    let n423: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n382);
    let n424: ZB = zb_and(n374, n423);
    let n425: ZN = zsel_n(n421, n417, n422);
    let n426: ZN = zsel_n(n421, zn_splat(P8::from_raw(0i32)), n274);
    let n427: ZB = zsel_b(n421, n374, n424);
    let n428: ZN = zsel_n(n418, n417, n425);
    let n429: ZN = zsel_n(n418, n274, n426);
    let n430: ZB = zsel_b(n418, n374, n427);
    let n431: ZN = zsel_n(n416, n412, n428);
    let n432: ZN = zsel_n(n416, zn_splat(P8::from_raw(0i32)), n429);
    let n433: ZB = zsel_b(n416, n374, n430);
    let n434: ZN = zsel_n(n413, n412, n431);
    let n435: ZN = zsel_n(n413, n274, n432);
    let n436: ZB = zsel_b(n413, n374, n433);
    let n437: ZN = zsel_n(n411, n407, n434);
    let n438: ZN = zsel_n(n411, zn_splat(P8::from_raw(0i32)), n435);
    let n439: ZB = zsel_b(n411, n374, n436);
    let n440: ZN = zsel_n(n408, n407, n437);
    let n441: ZN = zsel_n(n408, n274, n438);
    let n442: ZB = zsel_b(n408, n374, n439);
    let n443: ZN = zsel_n(n406, n402, n440);
    let n444: ZN = zsel_n(n406, zn_splat(P8::from_raw(0i32)), n441);
    let n445: ZB = zsel_b(n406, n374, n442);
    let n446: ZN = zsel_n(n403, n402, n443);
    let n447: ZN = zsel_n(n403, n274, n444);
    let n448: ZB = zsel_b(n403, n374, n445);
    let n449: ZN = zsel_n(n401, n397, n446);
    let n450: ZN = zsel_n(n401, zn_splat(P8::from_raw(0i32)), n447);
    let n451: ZB = zsel_b(n401, n374, n448);
    let n452: ZN = zsel_n(n398, n397, n449);
    let n453: ZN = zsel_n(n398, n274, n450);
    let n454: ZB = zsel_b(n398, n374, n451);
    let n455: ZN = zsel_n(n396, n392, n452);
    let n456: ZN = zsel_n(n396, zn_splat(P8::from_raw(0i32)), n453);
    let n457: ZB = zsel_b(n396, n374, n454);
    let n458: ZN = zsel_n(n393, n392, n455);
    let n459: ZN = zsel_n(n393, n274, n456);
    let n460: ZB = zsel_b(n393, n374, n457);
    let n461: ZN = zsel_n(n391, n387, n458);
    let n462: ZN = zsel_n(n391, zn_splat(P8::from_raw(0i32)), n459);
    let n463: ZB = zsel_b(n391, n374, n460);
    let n464: ZN = zsel_n(n388, n387, n461);
    let n465: ZN = zsel_n(n388, n274, n462);
    let n466: ZB = zsel_b(n388, n374, n463);
    let n467: ZN = zsel_n(n386, n272, n464);
    let n468: ZN = zsel_n(n386, zn_splat(P8::from_raw(0i32)), n465);
    let n469: ZB = zsel_b(n386, n374, n466);
    let n470: ZN = zsel_n(n279, n372, r_c301);
    let n471: ZN = zsel_n(n279, n467, n272);
    let n472: ZN = zsel_n(n279, n373, n273);
    let n473: ZN = zsel_n(n279, n468, n274);
    let n474: ZB = zb_or(n280, n469);
    let n475: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n470);
    let n476: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n471);
    let n477: ZN = zn_div(n475, zn_splat(P8::from_raw(524288i32)));
    let n478: ZN = zn_flr(n477);
    let n479: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n478);
    let n480: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n475);
    let n481: ZN = zn_sub(n480, zn_splat(P8::from_raw(65536i32)));
    let n482: ZN = zn_div(n481, zn_splat(P8::from_raw(524288i32)));
    let n483: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n482);
    let n484: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n479);
    let n485: ZB = zn_le(n484, n483);
    let n486: ZB = zn_gt(n484, n483);
    let n487: ZB = zb_and(n124, n485);
    let n488: ZB = zb_and(n124, n486);
    let n489: ZN = zn_div(n476, zn_splat(P8::from_raw(524288i32)));
    let n490: ZN = zn_flr(n489);
    let n491: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n490);
    let n492: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n476);
    let n493: ZN = zn_sub(n492, zn_splat(P8::from_raw(65536i32)));
    let n494: ZN = zn_div(n493, zn_splat(P8::from_raw(524288i32)));
    let n495: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n494);
    let n496: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n491);
    let n497: ZB = zn_le(n496, n495);
    let n498: ZB = zn_gt(n496, n495);
    let n499: ZB = zb_and(n487, n497);
    let n500: ZB = zb_and(n487, n498);
    let n501: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n484);
    let n502: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n496);
    let n503: ZN = zn_mget(g.cart, n501, n502);
    let n504: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n503);
    let n505: ZN = zn_rem(n493, zn_splat(P8::from_raw(524288i32)));
    let n506: ZB = zn_ge(n505, zn_splat(P8::from_raw(393216i32)));
    let n507: ZN = zn_mul(n496, zn_splat(P8::from_raw(524288i32)));
    let n508: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n507);
    let n509: ZB = zn_eq(n492, n508);
    let n510: ZB = zb_or(n506, n509);
    let n511: ZB = zb_and(n504, n510);
    let n512: ZB = zn_ge(n473, zn_splat(P8::from_raw(0i32)));
    let n513: ZB = zb_and(n511, n512);
    let n514: ZB = zb_not(n513);
    let n515: ZB = zb_and(n499, n513);
    let n516: ZB = zb_and(n499, n514);
    let n517: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n503);
    let n518: ZN = zn_rem(n476, zn_splat(P8::from_raw(524288i32)));
    let n519: ZB = zn_le(n518, zn_splat(P8::from_raw(131072i32)));
    let n520: ZB = zb_and(n517, n519);
    let n521: ZB = zn_le(n473, zn_splat(P8::from_raw(0i32)));
    let n522: ZB = zb_and(n520, n521);
    let n523: ZB = zb_not(n522);
    let n524: ZB = zb_and(n516, n522);
    let n525: ZB = zb_and(n516, n523);
    let n526: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n503);
    let n527: ZN = zn_rem(n475, zn_splat(P8::from_raw(524288i32)));
    let n528: ZB = zn_le(n527, zn_splat(P8::from_raw(131072i32)));
    let n529: ZB = zb_and(n526, n528);
    let n530: ZB = zn_le(n472, zn_splat(P8::from_raw(0i32)));
    let n531: ZB = zb_and(n529, n530);
    let n532: ZB = zb_not(n531);
    let n533: ZB = zb_and(n525, n531);
    let n534: ZB = zb_and(n525, n532);
    let n535: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n503);
    let n536: ZN = zn_rem(n481, zn_splat(P8::from_raw(524288i32)));
    let n537: ZB = zn_ge(n536, zn_splat(P8::from_raw(393216i32)));
    let n538: ZN = zn_mul(n484, zn_splat(P8::from_raw(524288i32)));
    let n539: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n538);
    let n540: ZB = zn_eq(n480, n539);
    let n541: ZB = zb_or(n537, n540);
    let n542: ZB = zb_and(n535, n541);
    let n543: ZB = zn_ge(n472, zn_splat(P8::from_raw(0i32)));
    let n544: ZB = zb_and(n542, n543);
    let n545: ZB = zb_not(n544);
    let n546: ZB = zb_and(n534, n544);
    let n547: ZB = zb_and(n534, n545);
    let n548: ZB = zb_or(n533, n546);
    let n549: ZB = zb_or(n524, n548);
    let n550: ZB = zb_or(n515, n549);
    let n551: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n491);
    let n552: ZB = zn_le(n551, n495);
    let n553: ZB = zn_gt(n551, n495);
    let n554: ZB = zb_and(n547, n552);
    let n555: ZB = zb_and(n547, n553);
    let n556: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n551);
    let n557: ZN = zn_mget(g.cart, n501, n556);
    let n558: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n557);
    let n559: ZN = zn_mul(n551, zn_splat(P8::from_raw(524288i32)));
    let n560: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n559);
    let n561: ZB = zn_eq(n492, n560);
    let n562: ZB = zb_or(n506, n561);
    let n563: ZB = zb_and(n558, n562);
    let n564: ZB = zb_and(n512, n563);
    let n565: ZB = zb_not(n564);
    let n566: ZB = zb_and(n554, n564);
    let n567: ZB = zb_and(n554, n565);
    let n568: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n557);
    let n569: ZB = zb_and(n519, n568);
    let n570: ZB = zb_and(n521, n569);
    let n571: ZB = zb_not(n570);
    let n572: ZB = zb_and(n567, n570);
    let n573: ZB = zb_and(n567, n571);
    let n574: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n557);
    let n575: ZB = zb_and(n528, n574);
    let n576: ZB = zb_and(n530, n575);
    let n577: ZB = zb_not(n576);
    let n578: ZB = zb_and(n573, n576);
    let n579: ZB = zb_and(n573, n577);
    let n580: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n557);
    let n581: ZB = zb_and(n541, n580);
    let n582: ZB = zb_and(n543, n581);
    let n583: ZB = zb_not(n582);
    let n584: ZB = zb_and(n579, n582);
    let n585: ZB = zb_and(n579, n583);
    let n586: ZB = zb_or(n578, n584);
    let n587: ZB = zb_or(n572, n586);
    let n588: ZB = zb_or(n566, n587);
    let n589: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n491);
    let n590: ZB = zn_le(n589, n495);
    let n591: ZB = zn_gt(n589, n495);
    let n592: ZB = zb_and(n585, n590);
    let n593: ZB = zb_and(n585, n591);
    let n594: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n589);
    let n595: ZN = zn_mget(g.cart, n501, n594);
    let n596: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n595);
    let n597: ZN = zn_mul(n589, zn_splat(P8::from_raw(524288i32)));
    let n598: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n597);
    let n599: ZB = zn_eq(n492, n598);
    let n600: ZB = zb_or(n506, n599);
    let n601: ZB = zb_and(n596, n600);
    let n602: ZB = zb_and(n512, n601);
    let n603: ZB = zb_not(n602);
    let n604: ZB = zb_and(n592, n602);
    let n605: ZB = zb_and(n592, n603);
    let n606: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n595);
    let n607: ZB = zb_and(n519, n606);
    let n608: ZB = zb_and(n521, n607);
    let n609: ZB = zb_not(n608);
    let n610: ZB = zb_and(n605, n608);
    let n611: ZB = zb_and(n605, n609);
    let n612: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n595);
    let n613: ZB = zb_and(n528, n612);
    let n614: ZB = zb_and(n530, n613);
    let n615: ZB = zb_not(n614);
    let n616: ZB = zb_and(n611, n614);
    let n617: ZB = zb_and(n611, n615);
    let n618: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n595);
    let n619: ZB = zb_and(n541, n618);
    let n620: ZB = zb_and(n543, n619);
    let n621: ZB = zb_not(n620);
    let n622: ZB = zb_and(n617, n620);
    let n623: ZB = zb_and(n617, n621);
    let n624: ZB = zb_or(n616, n622);
    let n625: ZB = zb_or(n610, n624);
    let n626: ZB = zb_or(n604, n625);
    let n627: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n491);
    let n628: ZB = zn_gt(n627, n495);
    let n629: ZB = zb_and(n474, n628);
    let n630: ZB = zb_or(n593, n623);
    let n631: ZB = zsel_b(n591, n474, n629);
    let n632: ZB = zb_or(n588, n626);
    let n633: ZB = zb_or(n555, n630);
    let n634: ZB = zsel_b(n553, n474, n631);
    let n635: ZB = zb_or(n550, n632);
    let n636: ZB = zb_or(n500, n633);
    let n637: ZB = zsel_b(n498, n474, n634);
    let n638: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n479);
    let n639: ZB = zn_le(n638, n483);
    let n640: ZB = zn_gt(n638, n483);
    let n641: ZB = zb_and(n636, n639);
    let n642: ZB = zb_and(n636, n640);
    let n643: ZB = zb_and(n498, n641);
    let n644: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n638);
    let n645: ZN = zn_mget(g.cart, n644, n502);
    let n646: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n645);
    let n647: ZB = zb_and(n497, n636);
    let n648: ZB = zb_and(n639, n647);
    let n649: ZB = zb_and(n510, n646);
    let n650: ZB = zb_and(n512, n649);
    let n651: ZB = zb_not(n650);
    let n652: ZB = zb_and(n648, n650);
    let n653: ZB = zb_and(n648, n651);
    let n654: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n645);
    let n655: ZB = zb_and(n519, n654);
    let n656: ZB = zb_and(n521, n655);
    let n657: ZB = zb_not(n656);
    let n658: ZB = zb_and(n653, n656);
    let n659: ZB = zb_and(n653, n657);
    let n660: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n645);
    let n661: ZB = zb_and(n528, n660);
    let n662: ZB = zb_and(n530, n661);
    let n663: ZB = zb_not(n662);
    let n664: ZB = zb_and(n659, n662);
    let n665: ZB = zb_and(n659, n663);
    let n666: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n645);
    let n667: ZN = zn_mul(n638, zn_splat(P8::from_raw(524288i32)));
    let n668: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n667);
    let n669: ZB = zn_eq(n480, n668);
    let n670: ZB = zb_or(n537, n669);
    let n671: ZB = zb_and(n666, n670);
    let n672: ZB = zb_and(n543, n671);
    let n673: ZB = zb_not(n672);
    let n674: ZB = zb_and(n665, n672);
    let n675: ZB = zb_and(n665, n673);
    let n676: ZB = zb_or(n664, n674);
    let n677: ZB = zb_or(n658, n676);
    let n678: ZB = zb_or(n652, n677);
    let n679: ZB = zb_and(n553, n675);
    let n680: ZN = zn_mget(g.cart, n644, n556);
    let n681: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n680);
    let n682: ZB = zb_and(n552, n665);
    let n683: ZB = zb_and(n673, n682);
    let n684: ZB = zb_and(n562, n681);
    let n685: ZB = zb_and(n512, n684);
    let n686: ZB = zb_not(n685);
    let n687: ZB = zb_and(n683, n685);
    let n688: ZB = zb_and(n683, n686);
    let n689: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n680);
    let n690: ZB = zb_and(n519, n689);
    let n691: ZB = zb_and(n521, n690);
    let n692: ZB = zb_not(n691);
    let n693: ZB = zb_and(n688, n691);
    let n694: ZB = zb_and(n688, n692);
    let n695: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n680);
    let n696: ZB = zb_and(n528, n695);
    let n697: ZB = zb_and(n530, n696);
    let n698: ZB = zb_not(n697);
    let n699: ZB = zb_and(n694, n697);
    let n700: ZB = zb_and(n694, n698);
    let n701: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n680);
    let n702: ZB = zb_and(n670, n701);
    let n703: ZB = zb_and(n543, n702);
    let n704: ZB = zb_not(n703);
    let n705: ZB = zb_and(n700, n703);
    let n706: ZB = zb_and(n700, n704);
    let n707: ZB = zb_or(n699, n705);
    let n708: ZB = zb_or(n693, n707);
    let n709: ZB = zb_or(n687, n708);
    let n710: ZB = zb_and(n591, n706);
    let n711: ZN = zn_mget(g.cart, n644, n594);
    let n712: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n711);
    let n713: ZB = zb_and(n590, n700);
    let n714: ZB = zb_and(n704, n713);
    let n715: ZB = zb_and(n600, n712);
    let n716: ZB = zb_and(n512, n715);
    let n717: ZB = zb_not(n716);
    let n718: ZB = zb_and(n714, n716);
    let n719: ZB = zb_and(n714, n717);
    let n720: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n711);
    let n721: ZB = zb_and(n519, n720);
    let n722: ZB = zb_and(n521, n721);
    let n723: ZB = zb_not(n722);
    let n724: ZB = zb_and(n719, n722);
    let n725: ZB = zb_and(n719, n723);
    let n726: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n711);
    let n727: ZB = zb_and(n528, n726);
    let n728: ZB = zb_and(n530, n727);
    let n729: ZB = zb_not(n728);
    let n730: ZB = zb_and(n725, n728);
    let n731: ZB = zb_and(n725, n729);
    let n732: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n711);
    let n733: ZB = zb_and(n670, n732);
    let n734: ZB = zb_and(n543, n733);
    let n735: ZB = zb_not(n734);
    let n736: ZB = zb_and(n731, n734);
    let n737: ZB = zb_and(n731, n735);
    let n738: ZB = zb_or(n730, n736);
    let n739: ZB = zb_or(n724, n738);
    let n740: ZB = zb_or(n718, n739);
    let n741: ZB = zb_and(n628, n637);
    let n742: ZB = zb_or(n710, n737);
    let n743: ZB = zsel_b(n591, n637, n741);
    let n744: ZB = zb_or(n709, n740);
    let n745: ZB = zb_or(n679, n742);
    let n746: ZB = zsel_b(n553, n637, n743);
    let n747: ZB = zb_or(n678, n744);
    let n748: ZB = zb_or(n643, n745);
    let n749: ZB = zsel_b(n498, n637, n746);
    let n750: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n479);
    let n751: ZB = zn_le(n750, n483);
    let n752: ZB = zn_gt(n750, n483);
    let n753: ZB = zb_and(n748, n751);
    let n754: ZB = zb_and(n748, n752);
    let n755: ZB = zb_and(n498, n753);
    let n756: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n750);
    let n757: ZN = zn_mget(g.cart, n756, n502);
    let n758: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n757);
    let n759: ZB = zb_and(n497, n748);
    let n760: ZB = zb_and(n751, n759);
    let n761: ZB = zb_and(n510, n758);
    let n762: ZB = zb_and(n512, n761);
    let n763: ZB = zb_not(n762);
    let n764: ZB = zb_and(n760, n762);
    let n765: ZB = zb_and(n760, n763);
    let n766: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n757);
    let n767: ZB = zb_and(n519, n766);
    let n768: ZB = zb_and(n521, n767);
    let n769: ZB = zb_not(n768);
    let n770: ZB = zb_and(n765, n768);
    let n771: ZB = zb_and(n765, n769);
    let n772: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n757);
    let n773: ZB = zb_and(n528, n772);
    let n774: ZB = zb_and(n530, n773);
    let n775: ZB = zb_not(n774);
    let n776: ZB = zb_and(n771, n774);
    let n777: ZB = zb_and(n771, n775);
    let n778: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n757);
    let n779: ZN = zn_mul(n750, zn_splat(P8::from_raw(524288i32)));
    let n780: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n779);
    let n781: ZB = zn_eq(n480, n780);
    let n782: ZB = zb_or(n537, n781);
    let n783: ZB = zb_and(n778, n782);
    let n784: ZB = zb_and(n543, n783);
    let n785: ZB = zb_not(n784);
    let n786: ZB = zb_and(n777, n784);
    let n787: ZB = zb_and(n777, n785);
    let n788: ZB = zb_or(n776, n786);
    let n789: ZB = zb_or(n770, n788);
    let n790: ZB = zb_or(n764, n789);
    let n791: ZB = zb_and(n553, n787);
    let n792: ZN = zn_mget(g.cart, n756, n556);
    let n793: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n792);
    let n794: ZB = zb_and(n552, n777);
    let n795: ZB = zb_and(n785, n794);
    let n796: ZB = zb_and(n562, n793);
    let n797: ZB = zb_and(n512, n796);
    let n798: ZB = zb_not(n797);
    let n799: ZB = zb_and(n795, n797);
    let n800: ZB = zb_and(n795, n798);
    let n801: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n792);
    let n802: ZB = zb_and(n519, n801);
    let n803: ZB = zb_and(n521, n802);
    let n804: ZB = zb_not(n803);
    let n805: ZB = zb_and(n800, n803);
    let n806: ZB = zb_and(n800, n804);
    let n807: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n792);
    let n808: ZB = zb_and(n528, n807);
    let n809: ZB = zb_and(n530, n808);
    let n810: ZB = zb_not(n809);
    let n811: ZB = zb_and(n806, n809);
    let n812: ZB = zb_and(n806, n810);
    let n813: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n792);
    let n814: ZB = zb_and(n782, n813);
    let n815: ZB = zb_and(n543, n814);
    let n816: ZB = zb_not(n815);
    let n817: ZB = zb_and(n812, n815);
    let n818: ZB = zb_and(n812, n816);
    let n819: ZB = zb_or(n811, n817);
    let n820: ZB = zb_or(n805, n819);
    let n821: ZB = zb_or(n799, n820);
    let n822: ZB = zb_and(n591, n818);
    let n823: ZN = zn_mget(g.cart, n756, n594);
    let n824: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n823);
    let n825: ZB = zb_and(n590, n812);
    let n826: ZB = zb_and(n816, n825);
    let n827: ZB = zb_and(n600, n824);
    let n828: ZB = zb_and(n512, n827);
    let n829: ZB = zb_not(n828);
    let n830: ZB = zb_and(n826, n828);
    let n831: ZB = zb_and(n826, n829);
    let n832: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n823);
    let n833: ZB = zb_and(n519, n832);
    let n834: ZB = zb_and(n521, n833);
    let n835: ZB = zb_not(n834);
    let n836: ZB = zb_and(n831, n834);
    let n837: ZB = zb_and(n831, n835);
    let n838: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n823);
    let n839: ZB = zb_and(n528, n838);
    let n840: ZB = zb_and(n530, n839);
    let n841: ZB = zb_not(n840);
    let n842: ZB = zb_and(n837, n840);
    let n843: ZB = zb_and(n837, n841);
    let n844: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n823);
    let n845: ZB = zb_and(n782, n844);
    let n846: ZB = zb_and(n543, n845);
    let n847: ZB = zb_not(n846);
    let n848: ZB = zb_and(n843, n846);
    let n849: ZB = zb_and(n843, n847);
    let n850: ZB = zb_or(n842, n848);
    let n851: ZB = zb_or(n836, n850);
    let n852: ZB = zb_or(n830, n851);
    let n853: ZB = zb_and(n628, n749);
    let n854: ZB = zb_or(n822, n849);
    let n855: ZB = zsel_b(n591, n749, n853);
    let n856: ZB = zb_or(n821, n852);
    let n857: ZB = zb_or(n791, n854);
    let n858: ZB = zsel_b(n553, n749, n855);
    let n859: ZB = zb_or(n790, n856);
    let n860: ZB = zb_or(n755, n857);
    let n861: ZB = zsel_b(n498, n749, n858);
    let n862: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n479);
    let n863: ZB = zn_gt(n862, n483);
    let n864: ZB = zb_and(n861, n863);
    let n865: ZB = zb_or(n747, n859);
    let n866: ZB = zsel_b(n747, n637, n749);
    let n867: ZB = zb_or(n754, n860);
    let n868: ZB = zsel_b(n752, n749, n864);
    let n869: ZB = zb_or(n635, n865);
    let n870: ZB = zsel_b(n635, n474, n866);
    let n871: ZB = zb_or(n642, n867);
    let n872: ZB = zsel_b(n640, n637, n868);
    let n873: ZB = zb_or(n488, n871);
    let n874: ZB = zsel_b(n486, n474, n872);
    let n875: ZB = zn_gt(n471, zn_splat(P8::from_raw(8388608i32)));
    let n876: ZB = zn_le(n471, zn_splat(P8::from_raw(8388608i32)));
    let n877: ZB = zb_and(n873, n875);
    let n878: ZB = zb_or(n869, n877);
    let n879: ZB = zsel_b(n869, n870, n874);
    let n880: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n475);
    let n881: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n476);
    let n882: ZB = zn_tile_flag_at(g.cache, g.cart, n880, n881, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n883: ZB = zb_not(n882);
    let n884: ZB = zn_lt(n271, zn_splat(P8::from_raw(65536i32)));
    let n885: ZN = zsel_n(n884, zn_splat(P8::from_raw(65536i32)), n271);
    let n886: ZB = zn_gt(r_c287, zn_splat(P8::from_raw(0i32)));
    let n887: ZN = zn_sub(r_c287, zn_splat(P8::from_raw(65536i32)));
    let n888: ZN = zsel_n(n886, n887, r_c287);
    let n889: ZN = zsel_n(n882, n885, n271);
    let n890: ZN = zsel_n(n882, zn_splat(P8::from_raw(393216i32)), n888);
    let n891: ZB = zn_gt(r_c284, zn_splat(P8::from_raw(0i32)));
    let n892: ZB = zn_gt(n472, r_c360);
    let n893: ZB = zn_gt(n473, r_c361);
    let n894: ZN = zsel_n(n883, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n895: ZN = zn_abs(n472);
    let n896: ZB = zn_gt(n895, zn_splat(P8::from_raw(65536i32)));
    let n897: ZB = zn_gt(n472, zn_splat(P8::from_raw(0i32)));
    let n898: ZB = zn_lt(n472, zn_splat(P8::from_raw(0i32)));
    let n899: ZB = zn_gt(n472, zn_splat(P8::from_raw(65536i32)));
    let n900: ZN = zn_sub(n472, zn_splat(P8::from_raw(9830i32)));
    let n901: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n900);
    let n902: ZN = zn_add(zn_splat(P8::from_raw(9830i32)), n472);
    let n903: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n902);
    let n904: ZB = zn_gt(n472, zn_splat(P8::from_raw(-65536i32)));
    let n905: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n900);
    let n906: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n902);
    let n907: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n900);
    let n908: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n902);
    let n909: ZN = zsel_n(n904, n905, n906);
    let n910: ZN = zsel_n(n897, n907, n908);
    let n911: ZN = zsel_n(n899, n901, n903);
    let n912: ZN = zsel_n(n898, n909, n910);
    let n913: ZN = zsel_n(n897, n911, n912);
    let n914: ZN = zn_sub(n472, n894);
    let n915: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n914);
    let n916: ZN = zn_add(n472, n894);
    let n917: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n916);
    let n918: ZN = zsel_n(n897, n915, n917);
    let n919: ZN = zsel_n(n896, n913, n918);
    let n920: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n919);
    let n921: ZB = zb_not(n920);
    let n922: ZB = zn_lt(n919, zn_splat(P8::from_raw(0i32)));
    let n923: ZB = zsel_b(n921, n922, r_c362);
    let n924: ZN = zn_abs(n473);
    let n925: ZB = zn_le(n924, zn_splat(P8::from_raw(9830i32)));
    let n926: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n476);
    let n927: ZB = zn_gt(n473, zn_splat(P8::from_raw(131072i32)));
    let n928: ZB = zn_gt(n890, zn_splat(P8::from_raw(0i32)));
    let n929: ZN = zn_add(zn_splat(P8::from_raw(-196608i32)), n475);
    let n930: ZB = zn_tile_flag_at(g.cache, g.cart, n929, n926, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n931: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n475);
    let n932: ZB = zn_tile_flag_at(g.cache, g.cart, n931, n926, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n933: ZN = zsel_n(n932, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n934: ZN = zsel_n(n930, zn_splat(P8::from_raw(-65536i32)), n933);
    let n935: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n934);
    let n936: ZB = zb_not(n935);
    let n937: ZB = zn_gt(n889, zn_splat(P8::from_raw(0i32)));
    let n938: ZN = zsel_n(n923, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n939: ZB = zn_gt(n938, zn_splat(P8::from_raw(0i32)));
    let n940: ZB = zn_lt(n938, zn_splat(P8::from_raw(0i32)));
    let n941: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n938);
    let n942: ZB = zb_not(n941);
    let n943: ZB = zn_lt(n471, zn_splat(P8::from_raw(-262144i32)));
    let n944: ZB = zn_ge(n471, zn_splat(P8::from_raw(-262144i32)));
    let n945: ZB = zb_and(n878, n943);
    let n947: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n957: ZN = zsel_n(n875, n151, n150);
    let n958: ZN = zsel_n(n869, n957, n150);
    let n962: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n914);
    let n963: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n916);
    let n964: ZN = zsel_n(n904, n962, n963);
    let n965: ZN = zsel_n(n896, n913, n964);
    let n966: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n965);
    let n967: ZB = zb_not(n966);
    let n968: ZB = zn_lt(n965, zn_splat(P8::from_raw(0i32)));
    let n969: ZB = zsel_b(n967, n968, r_c362);
    let n970: ZN = zn_add(zn_splat(P8::from_raw(-65536i32)), n475);
    let n971: ZB = zn_tile_flag_at(g.cache, g.cart, n970, n926, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n972: ZN = zsel_n(n971, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n973: ZB = zn_gt(n473, n972);
    let n974: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n914);
    let n975: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n916);
    let n976: ZN = zsel_n(n899, n974, n975);
    let n977: ZN = zsel_n(n896, n913, n976);
    let n978: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n977);
    let n979: ZB = zb_not(n978);
    let n980: ZB = zn_lt(n977, zn_splat(P8::from_raw(0i32)));
    let n981: ZB = zsel_b(n979, n980, r_c362);
    let n982: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n475);
    let n983: ZB = zn_tile_flag_at(g.cache, g.cart, n982, n926, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n984: ZN = zsel_n(n983, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n985: ZB = zn_gt(n473, n984);
    let n986: ZB = zb_and(n128, n937);
    let n987: ZN = zsel_n(n986, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n988: ZB = zb_or(r_c41, n986);
    let n989: ZN = zsel_n(n891, r_c20, n987);
    let n990: ZB = zsel_b(n891, r_c41, n988);
    let n995: ZB = zb_and(n873, n876);
    let n996: ZB = zb_and(n943, n995);
    let n997: ZB = zb_and(n944, n995);
    let n998: ZB = zb_not(n996);
    let n999: ZB = zb_or(n945, n996);
    let n1000: ZB = zsel_b(n996, n874, n879);
    let n1001: ZN = zsel_n(n996, r_c87, n958);
    let n1002: ZN = zsel_n(n996, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1013: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n1014: ZN = zn_sub(n282, zn_splat(P8::from_raw(32768i32)));
    let n1015: ZN = zn_sub(n1014, n283);
    let n1016: ZN = zsel_n(n327, zn_splat(P8::from_raw(0i32)), n1015);
    let n1017: ZN = zsel_n(n324, n1015, n1016);
    let n1018: ZN = zsel_n(n322, zn_splat(P8::from_raw(0i32)), n1017);
    let n1019: ZN = zsel_n(n319, n1015, n1018);
    let n1020: ZN = zsel_n(n317, zn_splat(P8::from_raw(0i32)), n1019);
    let n1021: ZN = zsel_n(n314, n1015, n1020);
    let n1022: ZN = zsel_n(n312, zn_splat(P8::from_raw(0i32)), n1021);
    let n1023: ZN = zsel_n(n309, n1015, n1022);
    let n1024: ZN = zsel_n(n307, zn_splat(P8::from_raw(0i32)), n1023);
    let n1025: ZN = zsel_n(n304, n1015, n1024);
    let n1026: ZN = zsel_n(n302, zn_splat(P8::from_raw(0i32)), n1025);
    let n1027: ZN = zsel_n(n299, n1015, n1026);
    let n1028: ZN = zsel_n(n297, zn_splat(P8::from_raw(0i32)), n1027);
    let n1029: ZN = zsel_n(n294, n1015, n1028);
    let n1030: ZN = zsel_n(n292, zn_splat(P8::from_raw(0i32)), n1029);
    let n1031: ZN = zn_sub(n376, zn_splat(P8::from_raw(32768i32)));
    let n1032: ZN = zn_sub(n1031, n377);
    let n1033: ZN = zsel_n(n421, zn_splat(P8::from_raw(0i32)), n1032);
    let n1034: ZN = zsel_n(n418, n1032, n1033);
    let n1035: ZN = zsel_n(n416, zn_splat(P8::from_raw(0i32)), n1034);
    let n1036: ZN = zsel_n(n413, n1032, n1035);
    let n1037: ZN = zsel_n(n411, zn_splat(P8::from_raw(0i32)), n1036);
    let n1038: ZN = zsel_n(n408, n1032, n1037);
    let n1039: ZN = zsel_n(n406, zn_splat(P8::from_raw(0i32)), n1038);
    let n1040: ZN = zsel_n(n403, n1032, n1039);
    let n1041: ZN = zsel_n(n401, zn_splat(P8::from_raw(0i32)), n1040);
    let n1042: ZN = zsel_n(n398, n1032, n1041);
    let n1043: ZN = zsel_n(n396, zn_splat(P8::from_raw(0i32)), n1042);
    let n1044: ZN = zsel_n(n393, n1032, n1043);
    let n1045: ZN = zsel_n(n391, zn_splat(P8::from_raw(0i32)), n1044);
    let n1046: ZN = zsel_n(n388, n1032, n1045);
    let n1047: ZN = zsel_n(n386, zn_splat(P8::from_raw(0i32)), n1046);
    let n1048: ZN = zsel_n(n279, n1030, r_c368);
    let n1049: ZN = zsel_n(n279, n1047, r_c369);
    let n1050: ZN = zn_sub(r_c282, zn_splat(P8::from_raw(65536i32)));
    let n1051: ZN = zn_sub(r_c284, zn_splat(P8::from_raw(65536i32)));
    let n1052: ZN = zn_sub(n472, r_c358);
    let n1053: ZN = zn_max(r_c360, n1052);
    let n1054: ZN = zn_add(n472, r_c358);
    let n1055: ZN = zn_min(r_c360, n1054);
    let n1056: ZN = zsel_n(n892, n1053, n1055);
    let n1057: ZN = zn_sub(n473, r_c359);
    let n1058: ZN = zn_max(r_c361, n1057);
    let n1059: ZN = zn_add(n473, r_c359);
    let n1060: ZN = zn_min(r_c361, n1059);
    let n1061: ZN = zsel_n(n893, n1058, n1060);
    let n1062: ZN = zsel_n(n925, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(13762i32)));
    let n1063: ZN = zn_sub(n473, n1062);
    let n1064: ZN = zn_max(zn_splat(P8::from_raw(131072i32)), n1063);
    let n1065: ZN = zn_add(n473, n1062);
    let n1066: ZN = zn_min(zn_splat(P8::from_raw(131072i32)), n1065);
    let n1067: ZN = zsel_n(n927, n1064, n1066);
    let n1068: ZN = zsel_n(n883, n1067, n473);
    let n1069: ZN = zn_neg(n934);
    let n1070: ZN = zn_mul(n1069, zn_splat(P8::from_raw(131072i32)));
    let n1071: ZN = zsel_n(n936, n1070, n919);
    let n1072: ZN = zsel_n(n936, zn_splat(P8::from_raw(-131072i32)), n1068);
    let n1073: ZN = zsel_n(n928, zn_splat(P8::from_raw(0i32)), n890);
    let n1074: ZN = zsel_n(n928, n919, n1071);
    let n1075: ZN = zsel_n(n928, zn_splat(P8::from_raw(-131072i32)), n1072);
    let n1076: ZN = zn_sub(n889, zn_splat(P8::from_raw(65536i32)));
    let n1077: ZN = zsel_n(n940, zn_splat(P8::from_raw(-131072i32)), zn_splat(P8::from_raw(0i32)));
    let n1078: ZN = zsel_n(n939, zn_splat(P8::from_raw(131072i32)), n1077);
    let n1079: ZN = zsel_n(n942, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n1080: ZN = zsel_n(n891, n1051, r_c284);
    let n1081: ZB = zsel_b(n891, r_c362, n923);
    let n1082: ZN = zsel_n(n891, n1056, n919);
    let n1083: ZN = zsel_n(n891, n1061, n1068);
    let n1084: ZN = zsel_n(n947, n1013, r_c20);
    let n1085: ZN = zsel_n(n947, r_c241, n240);
    let n1086: ZN = zsel_n(n947, r_c254, n241);
    let n1087: ZN = zsel_n(n947, r_c261, n269);
    let n1088: ZN = zsel_n(n947, r_c274, n270);
    let n1089: ZN = zsel_n(n947, r_c282, n1050);
    let n1090: ZN = zsel_n(n947, r_c284, n1080);
    let n1091: ZN = zsel_n(n947, r_c285, n889);
    let n1092: ZN = zsel_n(n947, r_c287, n890);
    let n1093: ZB = zb_and(r_c294, n947);
    let n1094: ZB = zb_and(r_c295, n947);
    let n1095: ZN = zsel_n(n947, r_c301, n470);
    let n1096: ZN = zsel_n(n947, r_c302, n471);
    let n1097: ZB = zsel_b(n947, r_c362, n1081);
    let n1098: ZN = zsel_n(n947, r_c368, n1048);
    let n1099: ZN = zsel_n(n947, r_c369, n1049);
    let n1100: ZN = zsel_n(n947, r_c370, n1082);
    let n1101: ZN = zsel_n(n947, r_c371, n1083);
    let n1102: ZB = zb_or(n947, n997);
    let n1103: ZB = zb_or(n874, n947);
    let n1104: ZB = zn_gt(n1084, zn_splat(P8::from_raw(0i32)));
    let n1105: ZB = zn_lt(n1095, zn_splat(P8::from_raw(-65536i32)));
    let n1106: ZB = zn_gt(n1095, zn_splat(P8::from_raw(7929856i32)));
    let n1107: ZB = zb_or(n1105, n1106);
    let n1108: ZN = zn_min(zn_splat(P8::from_raw(7929856i32)), n1095);
    let n1109: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1108);
    let n1110: ZN = zsel_n(n1107, n1109, n1095);
    let n1111: ZN = zsel_n(n1107, zn_splat(P8::from_raw(0i32)), n1100);
    let n1112: ZN = zsel_n(n1104, n1095, n1110);
    let n1113: ZN = zsel_n(n1104, n1100, n1111);
    let n1115: ZN = zn_max(n972, n1063);
    let n1116: ZN = zn_min(n972, n1065);
    let n1117: ZN = zsel_n(n973, n1115, n1116);
    let n1118: ZN = zsel_n(n883, n1117, n473);
    let n1119: ZN = zsel_n(n936, n1070, n965);
    let n1120: ZN = zsel_n(n936, zn_splat(P8::from_raw(-131072i32)), n1118);
    let n1121: ZN = zsel_n(n928, n965, n1119);
    let n1122: ZN = zsel_n(n928, zn_splat(P8::from_raw(-131072i32)), n1120);
    let n1123: ZB = zsel_b(n891, r_c362, n969);
    let n1124: ZN = zsel_n(n891, n1056, n965);
    let n1125: ZN = zsel_n(n891, n1061, n1118);
    let n1126: ZB = zsel_b(n947, r_c362, n1123);
    let n1127: ZN = zsel_n(n947, r_c370, n1124);
    let n1128: ZN = zsel_n(n947, r_c371, n1125);
    let n1129: ZN = zsel_n(n1107, zn_splat(P8::from_raw(0i32)), n1127);
    let n1130: ZN = zsel_n(n1104, n1127, n1129);
    let n1131: ZN = zn_max(n984, n1063);
    let n1132: ZN = zn_min(n984, n1065);
    let n1133: ZN = zsel_n(n985, n1131, n1132);
    let n1134: ZN = zsel_n(n883, n1133, n473);
    let n1135: ZN = zsel_n(n936, n1070, n977);
    let n1136: ZN = zsel_n(n936, zn_splat(P8::from_raw(-131072i32)), n1134);
    let n1137: ZN = zsel_n(n928, n977, n1135);
    let n1138: ZN = zsel_n(n928, zn_splat(P8::from_raw(-131072i32)), n1136);
    let n1139: ZB = zsel_b(n891, r_c362, n981);
    let n1140: ZN = zsel_n(n891, n1056, n977);
    let n1141: ZN = zsel_n(n891, n1061, n1134);
    let n1142: ZB = zsel_b(n947, r_c362, n1139);
    let n1143: ZN = zsel_n(n947, r_c370, n1140);
    let n1144: ZN = zsel_n(n947, r_c371, n1141);
    let n1145: ZN = zsel_n(n1107, zn_splat(P8::from_raw(0i32)), n1143);
    let n1146: ZN = zsel_n(n1104, n1143, n1145);
    let n1147: ZN = zsel_n(n179, n1073, n890);
    let n1148: ZN = zsel_n(n179, n1074, n919);
    let n1149: ZN = zsel_n(n179, n1075, n1068);
    let n1150: ZN = zsel_n(n891, n890, n1147);
    let n1151: ZN = zsel_n(n891, n1056, n1148);
    let n1152: ZN = zsel_n(n891, n1061, n1149);
    let n1153: ZN = zsel_n(n947, r_c287, n1150);
    let n1154: ZB = zb_or(r_c295, n124);
    let n1155: ZN = zsel_n(n947, r_c370, n1151);
    let n1156: ZN = zsel_n(n947, r_c371, n1152);
    let n1157: ZN = zsel_n(n1107, zn_splat(P8::from_raw(0i32)), n1155);
    let n1158: ZN = zsel_n(n1104, n1155, n1157);
    let n1159: ZN = zsel_n(n179, n1121, n965);
    let n1160: ZN = zsel_n(n179, n1122, n1118);
    let n1161: ZN = zsel_n(n891, n1056, n1159);
    let n1162: ZN = zsel_n(n891, n1061, n1160);
    let n1163: ZN = zsel_n(n947, r_c370, n1161);
    let n1164: ZN = zsel_n(n947, r_c371, n1162);
    let n1165: ZN = zsel_n(n1107, zn_splat(P8::from_raw(0i32)), n1163);
    let n1166: ZN = zsel_n(n1104, n1163, n1165);
    let n1167: ZN = zsel_n(n179, n1137, n977);
    let n1168: ZN = zsel_n(n179, n1138, n1134);
    let n1169: ZN = zsel_n(n891, n1056, n1167);
    let n1170: ZN = zsel_n(n891, n1061, n1168);
    let n1171: ZN = zsel_n(n947, r_c370, n1169);
    let n1172: ZN = zsel_n(n947, r_c371, n1170);
    let n1173: ZN = zsel_n(n1107, zn_splat(P8::from_raw(0i32)), n1171);
    let n1174: ZN = zsel_n(n1104, n1171, n1173);
    let n1175: ZN = zsel_n(n986, zn_splat(P8::from_raw(655360i32)), n1050);
    let n1176: ZN = zsel_n(n986, zn_splat(P8::from_raw(262144i32)), r_c284);
    let n1177: ZN = zsel_n(n986, n1076, n889);
    let n1178: ZN = zsel_n(n986, zn_splat(P8::from_raw(98304i32)), r_c358);
    let n1179: ZN = zsel_n(n986, n1079, r_c359);
    let n1180: ZN = zsel_n(n986, n1078, r_c360);
    let n1181: ZN = zsel_n(n986, zn_splat(P8::from_raw(0i32)), r_c361);
    let n1182: ZN = zsel_n(n986, n938, n919);
    let n1183: ZN = zsel_n(n986, zn_splat(P8::from_raw(0i32)), n1068);
    let n1184: ZN = zsel_n(n891, n1050, n1175);
    let n1185: ZN = zsel_n(n891, n1051, n1176);
    let n1186: ZN = zsel_n(n891, n889, n1177);
    let n1187: ZN = zsel_n(n891, r_c358, n1178);
    let n1188: ZN = zsel_n(n891, r_c359, n1179);
    let n1189: ZN = zsel_n(n891, r_c360, n1180);
    let n1190: ZN = zsel_n(n891, r_c361, n1181);
    let n1191: ZN = zsel_n(n891, n1056, n1182);
    let n1192: ZN = zsel_n(n891, n1061, n1183);
    let n1193: ZN = zsel_n(n947, n1013, n989);
    let n1194: ZB = zsel_b(n947, r_c41, n990);
    let n1195: ZN = zsel_n(n947, r_c282, n1184);
    let n1196: ZN = zsel_n(n947, r_c284, n1185);
    let n1197: ZN = zsel_n(n947, r_c285, n1186);
    let n1198: ZB = zb_or(r_c294, n124);
    let n1199: ZN = zsel_n(n947, r_c358, n1187);
    let n1200: ZN = zsel_n(n947, r_c359, n1188);
    let n1201: ZN = zsel_n(n947, r_c360, n1189);
    let n1202: ZN = zsel_n(n947, r_c361, n1190);
    let n1203: ZN = zsel_n(n947, r_c370, n1191);
    let n1204: ZN = zsel_n(n947, r_c371, n1192);
    let n1205: ZB = zn_gt(n1193, zn_splat(P8::from_raw(0i32)));
    let n1206: ZN = zsel_n(n1107, zn_splat(P8::from_raw(0i32)), n1203);
    let n1207: ZN = zsel_n(n1205, n1095, n1110);
    let n1208: ZN = zsel_n(n1205, n1203, n1206);
    let n1209: ZN = zsel_n(n986, zn_splat(P8::from_raw(69510i32)), r_c359);
    let n1210: ZN = zsel_n(n986, zn_splat(P8::from_raw(-131072i32)), r_c360);
    let n1211: ZN = zsel_n(n986, zn_splat(P8::from_raw(-327680i32)), n965);
    let n1212: ZN = zsel_n(n986, zn_splat(P8::from_raw(0i32)), n1118);
    let n1213: ZN = zsel_n(n891, r_c359, n1209);
    let n1214: ZN = zsel_n(n891, r_c360, n1210);
    let n1215: ZN = zsel_n(n891, n1056, n1211);
    let n1216: ZN = zsel_n(n891, n1061, n1212);
    let n1217: ZN = zsel_n(n947, r_c359, n1213);
    let n1218: ZN = zsel_n(n947, r_c360, n1214);
    let n1219: ZN = zsel_n(n947, r_c370, n1215);
    let n1220: ZN = zsel_n(n947, r_c371, n1216);
    let n1221: ZN = zsel_n(n1107, zn_splat(P8::from_raw(0i32)), n1219);
    let n1222: ZN = zsel_n(n1205, n1219, n1221);
    let n1223: ZN = zsel_n(n986, zn_splat(P8::from_raw(131072i32)), r_c360);
    let n1224: ZN = zsel_n(n986, zn_splat(P8::from_raw(327680i32)), n977);
    let n1225: ZN = zsel_n(n986, zn_splat(P8::from_raw(0i32)), n1134);
    let n1226: ZN = zsel_n(n891, r_c360, n1223);
    let n1227: ZN = zsel_n(n891, n1056, n1224);
    let n1228: ZN = zsel_n(n891, n1061, n1225);
    let n1229: ZN = zsel_n(n947, r_c360, n1226);
    let n1230: ZN = zsel_n(n947, r_c370, n1227);
    let n1231: ZN = zsel_n(n947, r_c371, n1228);
    let n1232: ZN = zsel_n(n1107, zn_splat(P8::from_raw(0i32)), n1230);
    let n1233: ZN = zsel_n(n1205, n1230, n1232);
    let n1235: ZN = zsel_n(n986, zn_splat(P8::from_raw(69510i32)), r_c358);
    let n1236: ZN = zsel_n(n986, zn_splat(P8::from_raw(98304i32)), r_c359);
    let n1237: ZN = zsel_n(n986, zn_splat(P8::from_raw(0i32)), r_c360);
    let n1238: ZN = zsel_n(n986, zn_splat(P8::from_raw(-98304i32)), r_c361);
    let n1239: ZN = zsel_n(n986, zn_splat(P8::from_raw(0i32)), n919);
    let n1240: ZN = zsel_n(n986, zn_splat(P8::from_raw(-327680i32)), n1068);
    let n1241: ZN = zsel_n(n891, r_c358, n1235);
    let n1242: ZN = zsel_n(n891, r_c359, n1236);
    let n1243: ZN = zsel_n(n891, r_c360, n1237);
    let n1244: ZN = zsel_n(n891, r_c361, n1238);
    let n1245: ZN = zsel_n(n891, n1056, n1239);
    let n1246: ZN = zsel_n(n891, n1061, n1240);
    let n1247: ZN = zsel_n(n947, r_c358, n1241);
    let n1248: ZN = zsel_n(n947, r_c359, n1242);
    let n1249: ZN = zsel_n(n947, r_c360, n1243);
    let n1250: ZN = zsel_n(n947, r_c361, n1244);
    let n1251: ZN = zsel_n(n947, r_c370, n1245);
    let n1252: ZN = zsel_n(n947, r_c371, n1246);
    let n1253: ZN = zsel_n(n1107, zn_splat(P8::from_raw(0i32)), n1251);
    let n1254: ZN = zsel_n(n1205, n1251, n1253);
    let n1255: ZN = zsel_n(n986, zn_splat(P8::from_raw(-231700i32)), n965);
    let n1256: ZN = zsel_n(n986, zn_splat(P8::from_raw(-231700i32)), n1118);
    let n1257: ZN = zsel_n(n891, n1056, n1255);
    let n1258: ZN = zsel_n(n891, n1061, n1256);
    let n1259: ZN = zsel_n(n947, r_c370, n1257);
    let n1260: ZN = zsel_n(n947, r_c371, n1258);
    let n1261: ZN = zsel_n(n1107, zn_splat(P8::from_raw(0i32)), n1259);
    let n1262: ZN = zsel_n(n1205, n1259, n1261);
    let n1263: ZN = zsel_n(n986, zn_splat(P8::from_raw(231700i32)), n977);
    let n1264: ZN = zsel_n(n986, zn_splat(P8::from_raw(-231700i32)), n1134);
    let n1265: ZN = zsel_n(n891, n1056, n1263);
    let n1266: ZN = zsel_n(n891, n1061, n1264);
    let n1267: ZN = zsel_n(n947, r_c370, n1265);
    let n1268: ZN = zsel_n(n947, r_c371, n1266);
    let n1269: ZN = zsel_n(n1107, zn_splat(P8::from_raw(0i32)), n1267);
    let n1270: ZN = zsel_n(n1205, n1267, n1269);
    let n1271: ZN = zsel_n(n986, zn_splat(P8::from_raw(131072i32)), r_c361);
    let n1272: ZN = zsel_n(n986, zn_splat(P8::from_raw(327680i32)), n1068);
    let n1273: ZN = zsel_n(n891, r_c361, n1271);
    let n1274: ZN = zsel_n(n891, n1061, n1272);
    let n1275: ZN = zsel_n(n947, r_c361, n1273);
    let n1276: ZN = zsel_n(n947, r_c371, n1274);
    let n1277: ZN = zsel_n(n986, zn_splat(P8::from_raw(231700i32)), n1118);
    let n1278: ZN = zsel_n(n891, n1061, n1277);
    let n1279: ZN = zsel_n(n947, r_c371, n1278);
    let n1280: ZN = zsel_n(n986, zn_splat(P8::from_raw(231700i32)), n1134);
    let n1281: ZN = zsel_n(n891, n1061, n1280);
    let n1282: ZN = zsel_n(n947, r_c371, n1281);
    let n1283: ZN = zsel_n(n986, n938, n1148);
    let n1284: ZN = zsel_n(n986, zn_splat(P8::from_raw(0i32)), n1149);
    let n1285: ZN = zsel_n(n891, n1056, n1283);
    let n1286: ZN = zsel_n(n891, n1061, n1284);
    let n1287: ZN = zsel_n(n947, r_c370, n1285);
    let n1288: ZN = zsel_n(n947, r_c371, n1286);
    let n1289: ZN = zsel_n(n1107, zn_splat(P8::from_raw(0i32)), n1287);
    let n1290: ZN = zsel_n(n1205, n1287, n1289);
    let n1291: ZN = zsel_n(n986, zn_splat(P8::from_raw(-327680i32)), n1159);
    let n1292: ZN = zsel_n(n986, zn_splat(P8::from_raw(0i32)), n1160);
    let n1293: ZN = zsel_n(n891, n1056, n1291);
    let n1294: ZN = zsel_n(n891, n1061, n1292);
    let n1295: ZN = zsel_n(n947, r_c370, n1293);
    let n1296: ZN = zsel_n(n947, r_c371, n1294);
    let n1297: ZN = zsel_n(n1107, zn_splat(P8::from_raw(0i32)), n1295);
    let n1298: ZN = zsel_n(n1205, n1295, n1297);
    let n1299: ZN = zsel_n(n986, zn_splat(P8::from_raw(327680i32)), n1167);
    let n1300: ZN = zsel_n(n986, zn_splat(P8::from_raw(0i32)), n1168);
    let n1301: ZN = zsel_n(n891, n1056, n1299);
    let n1302: ZN = zsel_n(n891, n1061, n1300);
    let n1303: ZN = zsel_n(n947, r_c370, n1301);
    let n1304: ZN = zsel_n(n947, r_c371, n1302);
    let n1305: ZN = zsel_n(n1107, zn_splat(P8::from_raw(0i32)), n1303);
    let n1306: ZN = zsel_n(n1205, n1303, n1305);
    let n1307: ZN = zsel_n(n986, zn_splat(P8::from_raw(0i32)), n1148);
    let n1308: ZN = zsel_n(n986, zn_splat(P8::from_raw(-327680i32)), n1149);
    let n1309: ZN = zsel_n(n891, n1056, n1307);
    let n1310: ZN = zsel_n(n891, n1061, n1308);
    let n1311: ZN = zsel_n(n947, r_c370, n1309);
    let n1312: ZN = zsel_n(n947, r_c371, n1310);
    let n1313: ZN = zsel_n(n1107, zn_splat(P8::from_raw(0i32)), n1311);
    let n1314: ZN = zsel_n(n1205, n1311, n1313);
    let n1315: ZN = zsel_n(n986, zn_splat(P8::from_raw(-231700i32)), n1159);
    let n1316: ZN = zsel_n(n986, zn_splat(P8::from_raw(-231700i32)), n1160);
    let n1317: ZN = zsel_n(n891, n1056, n1315);
    let n1318: ZN = zsel_n(n891, n1061, n1316);
    let n1319: ZN = zsel_n(n947, r_c370, n1317);
    let n1320: ZN = zsel_n(n947, r_c371, n1318);
    let n1321: ZN = zsel_n(n1107, zn_splat(P8::from_raw(0i32)), n1319);
    let n1322: ZN = zsel_n(n1205, n1319, n1321);
    let n1323: ZN = zsel_n(n986, zn_splat(P8::from_raw(231700i32)), n1167);
    let n1324: ZN = zsel_n(n986, zn_splat(P8::from_raw(-231700i32)), n1168);
    let n1325: ZN = zsel_n(n891, n1056, n1323);
    let n1326: ZN = zsel_n(n891, n1061, n1324);
    let n1327: ZN = zsel_n(n947, r_c370, n1325);
    let n1328: ZN = zsel_n(n947, r_c371, n1326);
    let n1329: ZN = zsel_n(n1107, zn_splat(P8::from_raw(0i32)), n1327);
    let n1330: ZN = zsel_n(n1205, n1327, n1329);
    let n1331: ZN = zsel_n(n986, zn_splat(P8::from_raw(327680i32)), n1149);
    let n1332: ZN = zsel_n(n891, n1061, n1331);
    let n1333: ZN = zsel_n(n947, r_c371, n1332);
    let n1334: ZN = zsel_n(n986, zn_splat(P8::from_raw(231700i32)), n1160);
    let n1335: ZN = zsel_n(n891, n1061, n1334);
    let n1336: ZN = zsel_n(n947, r_c371, n1335);
    let n1337: ZN = zsel_n(n986, zn_splat(P8::from_raw(231700i32)), n1168);
    let n1338: ZN = zsel_n(n891, n1061, n1337);
    let n1339: ZN = zsel_n(n947, r_c371, n1338);
    let n1342: ZW = zw_bits_n(n111);
    let n1343: ZW = zw_mix1(zw_splat(11400714819323198485u64), n1342, 84u64);
    let n1344: ZW = zw_mix2(zw_splat(11562461410679940143u64), n1342, 84u64);
    let n1345: ZW = zw_bits_n(n214);
    let n1346: ZW = zw_mix1(n1343, n1345, 85u64);
    let n1347: ZW = zw_mix2(n1344, n1345, 85u64);
    let n1348: ZW = zw_bits_n(n213);
    let n1349: ZW = zw_mix1(n1346, n1348, 86u64);
    let n1350: ZW = zw_mix2(n1347, n1348, 86u64);
    let n1351: ZW = zw_bits_n(n958);
    let n1352: ZW = zw_mix1(n1349, n1351, 87u64);
    let n1353: ZW = zw_mix2(n1350, n1351, 87u64);
    let n1354: ZW = zw_bits_n(n240);
    let n1355: ZW = zw_mix1(n1352, n1354, 240u64);
    let n1356: ZW = zw_mix2(n1353, n1354, 240u64);
    let n1357: ZW = zw_bits_n(n241);
    let n1358: ZW = zw_mix1(n1355, n1357, 253u64);
    let n1359: ZW = zw_mix2(n1356, n1357, 253u64);
    let n1360: ZW = zw_bits_n(n269);
    let n1361: ZW = zw_mix1(n1358, n1360, 260u64);
    let n1362: ZW = zw_mix2(n1359, n1360, 260u64);
    let n1363: ZW = zw_bits_n(n270);
    let n1364: ZW = zw_mix1(n1361, n1363, 273u64);
    let n1365: ZW = zw_mix2(n1362, n1363, 273u64);
    let n1366: ZW = zw_bits_n(r_c20);
    let n1367: ZW = zw_mix1(n1364, n1366, 20u64);
    let n1368: ZW = zw_mix2(n1365, n1366, 20u64);
    let n1369: ZW = zw_bits_b(r_c41);
    let n1370: ZW = zw_mix1(n1367, n1369, 41u64);
    let n1371: ZW = zw_mix2(n1368, n1369, 41u64);
    let n1372: ZW = zw_bits_n(n989);
    let n1373: ZW = zw_mix1(n1364, n1372, 20u64);
    let n1374: ZW = zw_mix2(n1365, n1372, 20u64);
    let n1375: ZW = zw_bits_b(n990);
    let n1376: ZW = zw_mix1(n1373, n1375, 41u64);
    let n1377: ZW = zw_mix2(n1374, n1375, 41u64);
    let n1378: ZW = zw_bits_b(n998);
    let n1379: ZW = zw_mix1(zw_splat(11400714819323198485u64), n1378, 38u64);
    let n1380: ZW = zw_mix2(zw_splat(11562461410679940143u64), n1378, 38u64);
    let n1381: ZW = zw_bits_n(n1002);
    let n1382: ZW = zw_mix1(n1379, n1381, 39u64);
    let n1383: ZW = zw_mix2(n1380, n1381, 39u64);
    let n1384: ZW = zw_mix1(n1382, n1342, 84u64);
    let n1385: ZW = zw_mix2(n1383, n1342, 84u64);
    let n1386: ZW = zw_mix1(n1384, n1345, 85u64);
    let n1387: ZW = zw_mix2(n1385, n1345, 85u64);
    let n1388: ZW = zw_mix1(n1386, n1348, 86u64);
    let n1389: ZW = zw_mix2(n1387, n1348, 86u64);
    let n1390: ZW = zw_bits_n(n1001);
    let n1391: ZW = zw_mix1(n1388, n1390, 87u64);
    let n1392: ZW = zw_mix2(n1389, n1390, 87u64);
    let n1393: ZW = zw_mix1(n1391, n1366, 20u64);
    let n1394: ZW = zw_mix2(n1392, n1366, 20u64);
    let n1395: ZW = zw_mix1(n1391, n1372, 20u64);
    let n1396: ZW = zw_mix2(n1392, n1372, 20u64);
    let n1397: ZW = zw_bits_n(r_c39);
    let n1398: ZW = zw_mix1(zw_splat(11400714819323198485u64), n1397, 39u64);
    let n1399: ZW = zw_mix2(zw_splat(11562461410679940143u64), n1397, 39u64);
    let n1400: ZW = zw_mix1(n1398, n1342, 84u64);
    let n1401: ZW = zw_mix2(n1399, n1342, 84u64);
    let n1402: ZW = zw_mix1(n1400, n1345, 85u64);
    let n1403: ZW = zw_mix2(n1401, n1345, 85u64);
    let n1404: ZW = zw_mix1(n1402, n1348, 86u64);
    let n1405: ZW = zw_mix2(n1403, n1348, 86u64);
    let n1406: ZW = zw_bits_n(r_c87);
    let n1407: ZW = zw_mix1(n1404, n1406, 87u64);
    let n1408: ZW = zw_mix2(n1405, n1406, 87u64);
    let n1409: ZW = zw_bits_n(n1085);
    let n1410: ZW = zw_mix1(n1407, n1409, 241u64);
    let n1411: ZW = zw_mix2(n1408, n1409, 241u64);
    let n1412: ZW = zw_bits_n(n1086);
    let n1413: ZW = zw_mix1(n1410, n1412, 254u64);
    let n1414: ZW = zw_mix2(n1411, n1412, 254u64);
    let n1415: ZW = zw_bits_n(n1087);
    let n1416: ZW = zw_mix1(n1413, n1415, 261u64);
    let n1417: ZW = zw_mix2(n1414, n1415, 261u64);
    let n1418: ZW = zw_bits_n(n1088);
    let n1419: ZW = zw_mix1(n1416, n1418, 274u64);
    let n1420: ZW = zw_mix2(n1417, n1418, 274u64);
    let n1421: ZW = zw_bits_n(n1096);
    let n1422: ZW = zw_mix1(n1419, n1421, 302u64);
    let n1423: ZW = zw_mix2(n1420, n1421, 302u64);
    let n1424: ZW = zw_bits_n(n1098);
    let n1425: ZW = zw_mix1(n1422, n1424, 368u64);
    let n1426: ZW = zw_mix2(n1423, n1424, 368u64);
    let n1427: ZW = zw_bits_n(n1099);
    let n1428: ZW = zw_mix1(n1425, n1427, 369u64);
    let n1429: ZW = zw_mix2(n1426, n1427, 369u64);
    let n1430: ZW = zw_bits_n(n1084);
    let n1431: ZW = zw_mix1(n1428, n1430, 20u64);
    let n1432: ZW = zw_mix2(n1429, n1430, 20u64);
    let n1433: ZW = zw_mix1(n1431, n1369, 41u64);
    let n1434: ZW = zw_mix2(n1432, n1369, 41u64);
    let n1435: ZW = zw_bits_n(n1089);
    let n1436: ZW = zw_mix1(n1433, n1435, 282u64);
    let n1437: ZW = zw_mix2(n1434, n1435, 282u64);
    let n1438: ZW = zw_bits_n(n1090);
    let n1439: ZW = zw_mix1(n1436, n1438, 284u64);
    let n1440: ZW = zw_mix2(n1437, n1438, 284u64);
    let n1441: ZW = zw_bits_n(n1091);
    let n1442: ZW = zw_mix1(n1439, n1441, 285u64);
    let n1443: ZW = zw_mix2(n1440, n1441, 285u64);
    let n1444: ZW = zw_bits_n(n1092);
    let n1445: ZW = zw_mix1(n1442, n1444, 287u64);
    let n1446: ZW = zw_mix2(n1443, n1444, 287u64);
    let n1447: ZW = zw_bits_b(n1093);
    let n1448: ZW = zw_mix1(n1445, n1447, 294u64);
    let n1449: ZW = zw_mix2(n1446, n1447, 294u64);
    let n1450: ZW = zw_bits_b(n1094);
    let n1451: ZW = zw_mix1(n1448, n1450, 295u64);
    let n1452: ZW = zw_mix2(n1449, n1450, 295u64);
    let n1453: ZW = zw_bits_n(n1112);
    let n1454: ZW = zw_mix1(n1451, n1453, 301u64);
    let n1455: ZW = zw_mix2(n1452, n1453, 301u64);
    let n1456: ZW = zw_bits_n(r_c358);
    let n1457: ZW = zw_mix1(n1454, n1456, 358u64);
    let n1458: ZW = zw_mix2(n1455, n1456, 358u64);
    let n1459: ZW = zw_bits_n(r_c359);
    let n1460: ZW = zw_mix1(n1457, n1459, 359u64);
    let n1461: ZW = zw_mix2(n1458, n1459, 359u64);
    let n1462: ZW = zw_bits_n(r_c360);
    let n1463: ZW = zw_mix1(n1460, n1462, 360u64);
    let n1464: ZW = zw_mix2(n1461, n1462, 360u64);
    let n1465: ZW = zw_bits_n(r_c361);
    let n1466: ZW = zw_mix1(n1463, n1465, 361u64);
    let n1467: ZW = zw_mix2(n1464, n1465, 361u64);
    let n1468: ZW = zw_bits_b(n1097);
    let n1469: ZW = zw_mix1(n1466, n1468, 362u64);
    let n1470: ZW = zw_mix2(n1467, n1468, 362u64);
    let n1471: ZW = zw_bits_n(n1113);
    let n1472: ZW = zw_mix1(n1469, n1471, 370u64);
    let n1473: ZW = zw_mix2(n1470, n1471, 370u64);
    let n1474: ZW = zw_bits_n(n1101);
    let n1475: ZW = zw_mix1(n1472, n1474, 371u64);
    let n1476: ZW = zw_mix2(n1473, n1474, 371u64);
    let n1477: ZW = zw_bits_b(n1126);
    let n1478: ZW = zw_mix1(n1466, n1477, 362u64);
    let n1479: ZW = zw_mix2(n1467, n1477, 362u64);
    let n1480: ZW = zw_bits_n(n1130);
    let n1481: ZW = zw_mix1(n1478, n1480, 370u64);
    let n1482: ZW = zw_mix2(n1479, n1480, 370u64);
    let n1483: ZW = zw_bits_n(n1128);
    let n1484: ZW = zw_mix1(n1481, n1483, 371u64);
    let n1485: ZW = zw_mix2(n1482, n1483, 371u64);
    let n1486: ZW = zw_bits_b(n1142);
    let n1487: ZW = zw_mix1(n1466, n1486, 362u64);
    let n1488: ZW = zw_mix2(n1467, n1486, 362u64);
    let n1489: ZW = zw_bits_n(n1146);
    let n1490: ZW = zw_mix1(n1487, n1489, 370u64);
    let n1491: ZW = zw_mix2(n1488, n1489, 370u64);
    let n1492: ZW = zw_bits_n(n1144);
    let n1493: ZW = zw_mix1(n1490, n1492, 371u64);
    let n1494: ZW = zw_mix2(n1491, n1492, 371u64);
    let n1495: ZW = zw_bits_n(n1153);
    let n1496: ZW = zw_mix1(n1442, n1495, 287u64);
    let n1497: ZW = zw_mix2(n1443, n1495, 287u64);
    let n1498: ZW = zw_mix1(n1496, n1447, 294u64);
    let n1499: ZW = zw_mix2(n1497, n1447, 294u64);
    let n1500: ZW = zw_bits_b(n1154);
    let n1501: ZW = zw_mix1(n1498, n1500, 295u64);
    let n1502: ZW = zw_mix2(n1499, n1500, 295u64);
    let n1503: ZW = zw_mix1(n1501, n1453, 301u64);
    let n1504: ZW = zw_mix2(n1502, n1453, 301u64);
    let n1505: ZW = zw_mix1(n1503, n1456, 358u64);
    let n1506: ZW = zw_mix2(n1504, n1456, 358u64);
    let n1507: ZW = zw_mix1(n1505, n1459, 359u64);
    let n1508: ZW = zw_mix2(n1506, n1459, 359u64);
    let n1509: ZW = zw_mix1(n1507, n1462, 360u64);
    let n1510: ZW = zw_mix2(n1508, n1462, 360u64);
    let n1511: ZW = zw_mix1(n1509, n1465, 361u64);
    let n1512: ZW = zw_mix2(n1510, n1465, 361u64);
    let n1513: ZW = zw_mix1(n1511, n1468, 362u64);
    let n1514: ZW = zw_mix2(n1512, n1468, 362u64);
    let n1515: ZW = zw_bits_n(n1158);
    let n1516: ZW = zw_mix1(n1513, n1515, 370u64);
    let n1517: ZW = zw_mix2(n1514, n1515, 370u64);
    let n1518: ZW = zw_bits_n(n1156);
    let n1519: ZW = zw_mix1(n1516, n1518, 371u64);
    let n1520: ZW = zw_mix2(n1517, n1518, 371u64);
    let n1521: ZW = zw_mix1(n1511, n1477, 362u64);
    let n1522: ZW = zw_mix2(n1512, n1477, 362u64);
    let n1523: ZW = zw_bits_n(n1166);
    let n1524: ZW = zw_mix1(n1521, n1523, 370u64);
    let n1525: ZW = zw_mix2(n1522, n1523, 370u64);
    let n1526: ZW = zw_bits_n(n1164);
    let n1527: ZW = zw_mix1(n1524, n1526, 371u64);
    let n1528: ZW = zw_mix2(n1525, n1526, 371u64);
    let n1529: ZW = zw_mix1(n1511, n1486, 362u64);
    let n1530: ZW = zw_mix2(n1512, n1486, 362u64);
    let n1531: ZW = zw_bits_n(n1174);
    let n1532: ZW = zw_mix1(n1529, n1531, 370u64);
    let n1533: ZW = zw_mix2(n1530, n1531, 370u64);
    let n1534: ZW = zw_bits_n(n1172);
    let n1535: ZW = zw_mix1(n1532, n1534, 371u64);
    let n1536: ZW = zw_mix2(n1533, n1534, 371u64);
    let n1537: ZW = zw_bits_n(n1193);
    let n1538: ZW = zw_mix1(n1428, n1537, 20u64);
    let n1539: ZW = zw_mix2(n1429, n1537, 20u64);
    let n1540: ZW = zw_bits_b(n1194);
    let n1541: ZW = zw_mix1(n1538, n1540, 41u64);
    let n1542: ZW = zw_mix2(n1539, n1540, 41u64);
    let n1543: ZW = zw_bits_n(n1195);
    let n1544: ZW = zw_mix1(n1541, n1543, 282u64);
    let n1545: ZW = zw_mix2(n1542, n1543, 282u64);
    let n1546: ZW = zw_bits_n(n1196);
    let n1547: ZW = zw_mix1(n1544, n1546, 284u64);
    let n1548: ZW = zw_mix2(n1545, n1546, 284u64);
    let n1549: ZW = zw_bits_n(n1197);
    let n1550: ZW = zw_mix1(n1547, n1549, 285u64);
    let n1551: ZW = zw_mix2(n1548, n1549, 285u64);
    let n1552: ZW = zw_mix1(n1550, n1444, 287u64);
    let n1553: ZW = zw_mix2(n1551, n1444, 287u64);
    let n1554: ZW = zw_bits_b(n1198);
    let n1555: ZW = zw_mix1(n1552, n1554, 294u64);
    let n1556: ZW = zw_mix2(n1553, n1554, 294u64);
    let n1557: ZW = zw_mix1(n1555, n1450, 295u64);
    let n1558: ZW = zw_mix2(n1556, n1450, 295u64);
    let n1559: ZW = zw_bits_n(n1207);
    let n1560: ZW = zw_mix1(n1557, n1559, 301u64);
    let n1561: ZW = zw_mix2(n1558, n1559, 301u64);
    let n1562: ZW = zw_bits_n(n1199);
    let n1563: ZW = zw_mix1(n1560, n1562, 358u64);
    let n1564: ZW = zw_mix2(n1561, n1562, 358u64);
    let n1565: ZW = zw_bits_n(n1200);
    let n1566: ZW = zw_mix1(n1563, n1565, 359u64);
    let n1567: ZW = zw_mix2(n1564, n1565, 359u64);
    let n1568: ZW = zw_bits_n(n1201);
    let n1569: ZW = zw_mix1(n1566, n1568, 360u64);
    let n1570: ZW = zw_mix2(n1567, n1568, 360u64);
    let n1571: ZW = zw_bits_n(n1202);
    let n1572: ZW = zw_mix1(n1569, n1571, 361u64);
    let n1573: ZW = zw_mix2(n1570, n1571, 361u64);
    let n1574: ZW = zw_mix1(n1572, n1468, 362u64);
    let n1575: ZW = zw_mix2(n1573, n1468, 362u64);
    let n1576: ZW = zw_bits_n(n1208);
    let n1577: ZW = zw_mix1(n1574, n1576, 370u64);
    let n1578: ZW = zw_mix2(n1575, n1576, 370u64);
    let n1579: ZW = zw_bits_n(n1204);
    let n1580: ZW = zw_mix1(n1577, n1579, 371u64);
    let n1581: ZW = zw_mix2(n1578, n1579, 371u64);
    let n1582: ZW = zw_bits_n(n1217);
    let n1583: ZW = zw_mix1(n1563, n1582, 359u64);
    let n1584: ZW = zw_mix2(n1564, n1582, 359u64);
    let n1585: ZW = zw_bits_n(n1218);
    let n1586: ZW = zw_mix1(n1583, n1585, 360u64);
    let n1587: ZW = zw_mix2(n1584, n1585, 360u64);
    let n1588: ZW = zw_mix1(n1586, n1571, 361u64);
    let n1589: ZW = zw_mix2(n1587, n1571, 361u64);
    let n1590: ZW = zw_mix1(n1588, n1477, 362u64);
    let n1591: ZW = zw_mix2(n1589, n1477, 362u64);
    let n1592: ZW = zw_bits_n(n1222);
    let n1593: ZW = zw_mix1(n1590, n1592, 370u64);
    let n1594: ZW = zw_mix2(n1591, n1592, 370u64);
    let n1595: ZW = zw_bits_n(n1220);
    let n1596: ZW = zw_mix1(n1593, n1595, 371u64);
    let n1597: ZW = zw_mix2(n1594, n1595, 371u64);
    let n1598: ZW = zw_bits_n(n1229);
    let n1599: ZW = zw_mix1(n1583, n1598, 360u64);
    let n1600: ZW = zw_mix2(n1584, n1598, 360u64);
    let n1601: ZW = zw_mix1(n1599, n1571, 361u64);
    let n1602: ZW = zw_mix2(n1600, n1571, 361u64);
    let n1603: ZW = zw_mix1(n1601, n1486, 362u64);
    let n1604: ZW = zw_mix2(n1602, n1486, 362u64);
    let n1605: ZW = zw_bits_n(n1233);
    let n1606: ZW = zw_mix1(n1603, n1605, 370u64);
    let n1607: ZW = zw_mix2(n1604, n1605, 370u64);
    let n1608: ZW = zw_bits_n(n1231);
    let n1609: ZW = zw_mix1(n1606, n1608, 371u64);
    let n1610: ZW = zw_mix2(n1607, n1608, 371u64);
    let n1611: ZW = zw_bits_n(n1247);
    let n1612: ZW = zw_mix1(n1560, n1611, 358u64);
    let n1613: ZW = zw_mix2(n1561, n1611, 358u64);
    let n1614: ZW = zw_bits_n(n1248);
    let n1615: ZW = zw_mix1(n1612, n1614, 359u64);
    let n1616: ZW = zw_mix2(n1613, n1614, 359u64);
    let n1617: ZW = zw_bits_n(n1249);
    let n1618: ZW = zw_mix1(n1615, n1617, 360u64);
    let n1619: ZW = zw_mix2(n1616, n1617, 360u64);
    let n1620: ZW = zw_bits_n(n1250);
    let n1621: ZW = zw_mix1(n1618, n1620, 361u64);
    let n1622: ZW = zw_mix2(n1619, n1620, 361u64);
    let n1623: ZW = zw_mix1(n1621, n1468, 362u64);
    let n1624: ZW = zw_mix2(n1622, n1468, 362u64);
    let n1625: ZW = zw_bits_n(n1254);
    let n1626: ZW = zw_mix1(n1623, n1625, 370u64);
    let n1627: ZW = zw_mix2(n1624, n1625, 370u64);
    let n1628: ZW = zw_bits_n(n1252);
    let n1629: ZW = zw_mix1(n1626, n1628, 371u64);
    let n1630: ZW = zw_mix2(n1627, n1628, 371u64);
    let n1631: ZW = zw_mix1(n1612, n1582, 359u64);
    let n1632: ZW = zw_mix2(n1613, n1582, 359u64);
    let n1633: ZW = zw_mix1(n1631, n1585, 360u64);
    let n1634: ZW = zw_mix2(n1632, n1585, 360u64);
    let n1635: ZW = zw_mix1(n1633, n1620, 361u64);
    let n1636: ZW = zw_mix2(n1634, n1620, 361u64);
    let n1637: ZW = zw_mix1(n1635, n1477, 362u64);
    let n1638: ZW = zw_mix2(n1636, n1477, 362u64);
    let n1639: ZW = zw_bits_n(n1262);
    let n1640: ZW = zw_mix1(n1637, n1639, 370u64);
    let n1641: ZW = zw_mix2(n1638, n1639, 370u64);
    let n1642: ZW = zw_bits_n(n1260);
    let n1643: ZW = zw_mix1(n1640, n1642, 371u64);
    let n1644: ZW = zw_mix2(n1641, n1642, 371u64);
    let n1645: ZW = zw_mix1(n1631, n1598, 360u64);
    let n1646: ZW = zw_mix2(n1632, n1598, 360u64);
    let n1647: ZW = zw_mix1(n1645, n1620, 361u64);
    let n1648: ZW = zw_mix2(n1646, n1620, 361u64);
    let n1649: ZW = zw_mix1(n1647, n1486, 362u64);
    let n1650: ZW = zw_mix2(n1648, n1486, 362u64);
    let n1651: ZW = zw_bits_n(n1270);
    let n1652: ZW = zw_mix1(n1649, n1651, 370u64);
    let n1653: ZW = zw_mix2(n1650, n1651, 370u64);
    let n1654: ZW = zw_bits_n(n1268);
    let n1655: ZW = zw_mix1(n1652, n1654, 371u64);
    let n1656: ZW = zw_mix2(n1653, n1654, 371u64);
    let n1657: ZW = zw_bits_n(n1275);
    let n1658: ZW = zw_mix1(n1618, n1657, 361u64);
    let n1659: ZW = zw_mix2(n1619, n1657, 361u64);
    let n1660: ZW = zw_mix1(n1658, n1468, 362u64);
    let n1661: ZW = zw_mix2(n1659, n1468, 362u64);
    let n1662: ZW = zw_mix1(n1660, n1625, 370u64);
    let n1663: ZW = zw_mix2(n1661, n1625, 370u64);
    let n1664: ZW = zw_bits_n(n1276);
    let n1665: ZW = zw_mix1(n1662, n1664, 371u64);
    let n1666: ZW = zw_mix2(n1663, n1664, 371u64);
    let n1667: ZW = zw_mix1(n1633, n1657, 361u64);
    let n1668: ZW = zw_mix2(n1634, n1657, 361u64);
    let n1669: ZW = zw_mix1(n1667, n1477, 362u64);
    let n1670: ZW = zw_mix2(n1668, n1477, 362u64);
    let n1671: ZW = zw_mix1(n1669, n1639, 370u64);
    let n1672: ZW = zw_mix2(n1670, n1639, 370u64);
    let n1673: ZW = zw_bits_n(n1279);
    let n1674: ZW = zw_mix1(n1671, n1673, 371u64);
    let n1675: ZW = zw_mix2(n1672, n1673, 371u64);
    let n1676: ZW = zw_mix1(n1645, n1657, 361u64);
    let n1677: ZW = zw_mix2(n1646, n1657, 361u64);
    let n1678: ZW = zw_mix1(n1676, n1486, 362u64);
    let n1679: ZW = zw_mix2(n1677, n1486, 362u64);
    let n1680: ZW = zw_mix1(n1678, n1651, 370u64);
    let n1681: ZW = zw_mix2(n1679, n1651, 370u64);
    let n1682: ZW = zw_bits_n(n1282);
    let n1683: ZW = zw_mix1(n1680, n1682, 371u64);
    let n1684: ZW = zw_mix2(n1681, n1682, 371u64);
    let n1685: ZW = zw_mix1(n1550, n1495, 287u64);
    let n1686: ZW = zw_mix2(n1551, n1495, 287u64);
    let n1687: ZW = zw_mix1(n1685, n1554, 294u64);
    let n1688: ZW = zw_mix2(n1686, n1554, 294u64);
    let n1689: ZW = zw_mix1(n1687, n1500, 295u64);
    let n1690: ZW = zw_mix2(n1688, n1500, 295u64);
    let n1691: ZW = zw_mix1(n1689, n1559, 301u64);
    let n1692: ZW = zw_mix2(n1690, n1559, 301u64);
    let n1693: ZW = zw_mix1(n1691, n1562, 358u64);
    let n1694: ZW = zw_mix2(n1692, n1562, 358u64);
    let n1695: ZW = zw_mix1(n1693, n1565, 359u64);
    let n1696: ZW = zw_mix2(n1694, n1565, 359u64);
    let n1697: ZW = zw_mix1(n1695, n1568, 360u64);
    let n1698: ZW = zw_mix2(n1696, n1568, 360u64);
    let n1699: ZW = zw_mix1(n1697, n1571, 361u64);
    let n1700: ZW = zw_mix2(n1698, n1571, 361u64);
    let n1701: ZW = zw_mix1(n1699, n1468, 362u64);
    let n1702: ZW = zw_mix2(n1700, n1468, 362u64);
    let n1703: ZW = zw_bits_n(n1290);
    let n1704: ZW = zw_mix1(n1701, n1703, 370u64);
    let n1705: ZW = zw_mix2(n1702, n1703, 370u64);
    let n1706: ZW = zw_bits_n(n1288);
    let n1707: ZW = zw_mix1(n1704, n1706, 371u64);
    let n1708: ZW = zw_mix2(n1705, n1706, 371u64);
    let n1709: ZW = zw_mix1(n1693, n1582, 359u64);
    let n1710: ZW = zw_mix2(n1694, n1582, 359u64);
    let n1711: ZW = zw_mix1(n1709, n1585, 360u64);
    let n1712: ZW = zw_mix2(n1710, n1585, 360u64);
    let n1713: ZW = zw_mix1(n1711, n1571, 361u64);
    let n1714: ZW = zw_mix2(n1712, n1571, 361u64);
    let n1715: ZW = zw_mix1(n1713, n1477, 362u64);
    let n1716: ZW = zw_mix2(n1714, n1477, 362u64);
    let n1717: ZW = zw_bits_n(n1298);
    let n1718: ZW = zw_mix1(n1715, n1717, 370u64);
    let n1719: ZW = zw_mix2(n1716, n1717, 370u64);
    let n1720: ZW = zw_bits_n(n1296);
    let n1721: ZW = zw_mix1(n1718, n1720, 371u64);
    let n1722: ZW = zw_mix2(n1719, n1720, 371u64);
    let n1723: ZW = zw_mix1(n1709, n1598, 360u64);
    let n1724: ZW = zw_mix2(n1710, n1598, 360u64);
    let n1725: ZW = zw_mix1(n1723, n1571, 361u64);
    let n1726: ZW = zw_mix2(n1724, n1571, 361u64);
    let n1727: ZW = zw_mix1(n1725, n1486, 362u64);
    let n1728: ZW = zw_mix2(n1726, n1486, 362u64);
    let n1729: ZW = zw_bits_n(n1306);
    let n1730: ZW = zw_mix1(n1727, n1729, 370u64);
    let n1731: ZW = zw_mix2(n1728, n1729, 370u64);
    let n1732: ZW = zw_bits_n(n1304);
    let n1733: ZW = zw_mix1(n1730, n1732, 371u64);
    let n1734: ZW = zw_mix2(n1731, n1732, 371u64);
    let n1735: ZW = zw_mix1(n1691, n1611, 358u64);
    let n1736: ZW = zw_mix2(n1692, n1611, 358u64);
    let n1737: ZW = zw_mix1(n1735, n1614, 359u64);
    let n1738: ZW = zw_mix2(n1736, n1614, 359u64);
    let n1739: ZW = zw_mix1(n1737, n1617, 360u64);
    let n1740: ZW = zw_mix2(n1738, n1617, 360u64);
    let n1741: ZW = zw_mix1(n1739, n1620, 361u64);
    let n1742: ZW = zw_mix2(n1740, n1620, 361u64);
    let n1743: ZW = zw_mix1(n1741, n1468, 362u64);
    let n1744: ZW = zw_mix2(n1742, n1468, 362u64);
    let n1745: ZW = zw_bits_n(n1314);
    let n1746: ZW = zw_mix1(n1743, n1745, 370u64);
    let n1747: ZW = zw_mix2(n1744, n1745, 370u64);
    let n1748: ZW = zw_bits_n(n1312);
    let n1749: ZW = zw_mix1(n1746, n1748, 371u64);
    let n1750: ZW = zw_mix2(n1747, n1748, 371u64);
    let n1751: ZW = zw_mix1(n1735, n1582, 359u64);
    let n1752: ZW = zw_mix2(n1736, n1582, 359u64);
    let n1753: ZW = zw_mix1(n1751, n1585, 360u64);
    let n1754: ZW = zw_mix2(n1752, n1585, 360u64);
    let n1755: ZW = zw_mix1(n1753, n1620, 361u64);
    let n1756: ZW = zw_mix2(n1754, n1620, 361u64);
    let n1757: ZW = zw_mix1(n1755, n1477, 362u64);
    let n1758: ZW = zw_mix2(n1756, n1477, 362u64);
    let n1759: ZW = zw_bits_n(n1322);
    let n1760: ZW = zw_mix1(n1757, n1759, 370u64);
    let n1761: ZW = zw_mix2(n1758, n1759, 370u64);
    let n1762: ZW = zw_bits_n(n1320);
    let n1763: ZW = zw_mix1(n1760, n1762, 371u64);
    let n1764: ZW = zw_mix2(n1761, n1762, 371u64);
    let n1765: ZW = zw_mix1(n1751, n1598, 360u64);
    let n1766: ZW = zw_mix2(n1752, n1598, 360u64);
    let n1767: ZW = zw_mix1(n1765, n1620, 361u64);
    let n1768: ZW = zw_mix2(n1766, n1620, 361u64);
    let n1769: ZW = zw_mix1(n1767, n1486, 362u64);
    let n1770: ZW = zw_mix2(n1768, n1486, 362u64);
    let n1771: ZW = zw_bits_n(n1330);
    let n1772: ZW = zw_mix1(n1769, n1771, 370u64);
    let n1773: ZW = zw_mix2(n1770, n1771, 370u64);
    let n1774: ZW = zw_bits_n(n1328);
    let n1775: ZW = zw_mix1(n1772, n1774, 371u64);
    let n1776: ZW = zw_mix2(n1773, n1774, 371u64);
    let n1777: ZW = zw_mix1(n1739, n1657, 361u64);
    let n1778: ZW = zw_mix2(n1740, n1657, 361u64);
    let n1779: ZW = zw_mix1(n1777, n1468, 362u64);
    let n1780: ZW = zw_mix2(n1778, n1468, 362u64);
    let n1781: ZW = zw_mix1(n1779, n1745, 370u64);
    let n1782: ZW = zw_mix2(n1780, n1745, 370u64);
    let n1783: ZW = zw_bits_n(n1333);
    let n1784: ZW = zw_mix1(n1781, n1783, 371u64);
    let n1785: ZW = zw_mix2(n1782, n1783, 371u64);
    let n1786: ZW = zw_mix1(n1753, n1657, 361u64);
    let n1787: ZW = zw_mix2(n1754, n1657, 361u64);
    let n1788: ZW = zw_mix1(n1786, n1477, 362u64);
    let n1789: ZW = zw_mix2(n1787, n1477, 362u64);
    let n1790: ZW = zw_mix1(n1788, n1759, 370u64);
    let n1791: ZW = zw_mix2(n1789, n1759, 370u64);
    let n1792: ZW = zw_bits_n(n1336);
    let n1793: ZW = zw_mix1(n1790, n1792, 371u64);
    let n1794: ZW = zw_mix2(n1791, n1792, 371u64);
    let n1795: ZW = zw_mix1(n1765, n1657, 361u64);
    let n1796: ZW = zw_mix2(n1766, n1657, 361u64);
    let n1797: ZW = zw_mix1(n1795, n1486, 362u64);
    let n1798: ZW = zw_mix2(n1796, n1486, 362u64);
    let n1799: ZW = zw_mix1(n1797, n1771, 370u64);
    let n1800: ZW = zw_mix2(n1798, n1771, 370u64);
    let n1801: ZW = zw_bits_n(n1339);
    let n1802: ZW = zw_mix1(n1799, n1801, 371u64);
    let n1803: ZW = zw_mix2(n1800, n1801, 371u64);
    let ok_v0_b0: u16 = ALL & zb_holds(n879) & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129);
    let bd_v0_b0: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v0_b0: u16 = ALL & zb_holds(n878) & zb_holds(n944);
    let ok_v32_b1: u16 = ALL & zb_holds(n879) & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129);
    let bd_v32_b1: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v32_b1: u16 = ALL & zb_holds(n878) & zb_holds(n944);
    let ok_v0_b2: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n1000);
    let bd_v0_b2: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v0_b2: u16 = ALL & zb_holds(n999);
    let ok_v32_b3: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n1000);
    let bd_v32_b3: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v32_b3: u16 = ALL & zb_holds(n999);
    let ok_v0_b4: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n1103);
    let bd_v0_b4: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v0_b4: u16 = ALL & zb_holds(n1102);
    let ok_v1_b5: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n1103);
    let bd_v1_b5: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v1_b5: u16 = ALL & zb_holds(n1102);
    let ok_v2_b6: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n1103);
    let bd_v2_b6: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v2_b6: u16 = ALL & zb_holds(n1102);
    let ok_v16_b7: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n1103);
    let bd_v16_b7: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v16_b7: u16 = ALL & zb_holds(n1102);
    let ok_v17_b8: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n1103);
    let bd_v17_b8: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v17_b8: u16 = ALL & zb_holds(n1102);
    let ok_v18_b9: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n1103);
    let bd_v18_b9: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v18_b9: u16 = ALL & zb_holds(n1102);
    let ok_v32_b10: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n1103);
    let bd_v32_b10: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v32_b10: u16 = ALL & zb_holds(n1102);
    let ok_v33_b11: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n1103);
    let bd_v33_b11: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v33_b11: u16 = ALL & zb_holds(n1102);
    let ok_v34_b12: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n1103);
    let bd_v34_b12: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v34_b12: u16 = ALL & zb_holds(n1102);
    let ok_v36_b13: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n1103);
    let bd_v36_b13: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v36_b13: u16 = ALL & zb_holds(n1102);
    let ok_v37_b14: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n1103);
    let bd_v37_b14: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v37_b14: u16 = ALL & zb_holds(n1102);
    let ok_v38_b15: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n1103);
    let bd_v38_b15: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v38_b15: u16 = ALL & zb_holds(n1102);
    let ok_v40_b16: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n1103);
    let bd_v40_b16: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v40_b16: u16 = ALL & zb_holds(n1102);
    let ok_v41_b17: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n1103);
    let bd_v41_b17: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v41_b17: u16 = ALL & zb_holds(n1102);
    let ok_v42_b18: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n1103);
    let bd_v42_b18: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v42_b18: u16 = ALL & zb_holds(n1102);
    let ok_v48_b19: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n1103);
    let bd_v48_b19: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v48_b19: u16 = ALL & zb_holds(n1102);
    let ok_v49_b20: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n1103);
    let bd_v49_b20: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v49_b20: u16 = ALL & zb_holds(n1102);
    let ok_v50_b21: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n1103);
    let bd_v50_b21: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v50_b21: u16 = ALL & zb_holds(n1102);
    let ok_v52_b22: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n1103);
    let bd_v52_b22: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v52_b22: u16 = ALL & zb_holds(n1102);
    let ok_v53_b23: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n1103);
    let bd_v53_b23: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v53_b23: u16 = ALL & zb_holds(n1102);
    let ok_v54_b24: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n1103);
    let bd_v54_b24: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v54_b24: u16 = ALL & zb_holds(n1102);
    let ok_v56_b25: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n1103);
    let bd_v56_b25: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v56_b25: u16 = ALL & zb_holds(n1102);
    let ok_v57_b26: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n1103);
    let bd_v57_b26: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v57_b26: u16 = ALL & zb_holds(n1102);
    let ok_v58_b27: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n1103);
    let bd_v58_b27: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v58_b27: u16 = ALL & zb_holds(n1102);
    let sh0 = KShared0 {
        c87: n958,
        c84: n111,
        c86: n213,
        c240: n240,
        c253: n241,
        c260: n269,
        c273: n270,
        c85: n214,
    };
    let sh1 = KShared1 {
        c87: n1001,
        c39: n1002,
        c84: n111,
        c86: n213,
        c85: n214,
        c38: n998,
    };
    let sh2 = KShared2 {
        c87: r_c87,
        c39: r_c39,
        c84: n111,
        c86: n213,
        c241: n1085,
        c254: n1086,
        c261: n1087,
        c274: n1088,
        c368: n1098,
        c369: n1099,
        c302: n1096,
        c85: n214,
    };
    let mut take_0_0: u16 = 0;
    let mut take_0_1: u16 = 0;
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
    // 28 distinct button assignments; per outcome they fall
    // into [2, 2, 24] groups that write identical values.
    declined |= live_v0_b0 & (if bd_v0_b0 { ALL } else { !ok_v0_b0 });
    take_0_0 |= live_v0_b0 & ok_v0_b0 & (if bd_v0_b0 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        h1: n1370, h2: n1371,
    };
    // body 0: buttons 0x00, forks 0x0
    sink.o0(0, take_0_0, &sh0, &o0);
    declined |= live_v32_b1 & (if bd_v32_b1 { ALL } else { !ok_v32_b1 });
    take_0_1 |= live_v32_b1 & ok_v32_b1 & (if bd_v32_b1 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n989,
        c41: n990,
        h1: n1376, h2: n1377,
    };
    // body 1: buttons 0x20, forks 0x0
    sink.o0(32, take_0_1, &sh0, &o0);
    declined |= live_v0_b2 & (if bd_v0_b2 { ALL } else { !ok_v0_b2 });
    take_1_0 |= live_v0_b2 & ok_v0_b2 & (if bd_v0_b2 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        h1: n1393, h2: n1394,
    };
    // body 2: buttons 0x00, forks 0x0
    sink.o1(0, take_1_0, &sh1, &o1);
    declined |= live_v32_b3 & (if bd_v32_b3 { ALL } else { !ok_v32_b3 });
    take_1_1 |= live_v32_b3 & ok_v32_b3 & (if bd_v32_b3 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n989,
        h1: n1395, h2: n1396,
    };
    // body 3: buttons 0x20, forks 0x0
    sink.o1(32, take_1_1, &sh1, &o1);
    declined |= live_v0_b4 & (if bd_v0_b4 { ALL } else { !ok_v0_b4 });
    take_2_0 |= live_v0_b4 & ok_v0_b4 & (if bd_v0_b4 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1084,
        c41: r_c41,
        c358: r_c358,
        c359: r_c359,
        c282: n1089,
        c360: r_c360,
        c361: r_c361,
        c284: n1090,
        c285: n1091,
        c362: n1097,
        c287: n1092,
        c294: n1093,
        c295: n1094,
        c370: n1113,
        c371: n1101,
        c301: n1112,
        h1: n1475, h2: n1476,
    };
    // body 4: buttons 0x00, forks 0x0
    sink.o2(0, take_2_0, &sh2, &o2);
    declined |= live_v1_b5 & (if bd_v1_b5 { ALL } else { !ok_v1_b5 });
    take_2_1 |= live_v1_b5 & ok_v1_b5 & (if bd_v1_b5 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1084,
        c41: r_c41,
        c358: r_c358,
        c359: r_c359,
        c282: n1089,
        c360: r_c360,
        c361: r_c361,
        c284: n1090,
        c285: n1091,
        c362: n1126,
        c287: n1092,
        c294: n1093,
        c295: n1094,
        c370: n1130,
        c371: n1128,
        c301: n1112,
        h1: n1484, h2: n1485,
    };
    // body 5: buttons 0x01, forks 0x0
    sink.o2(1, take_2_1, &sh2, &o2);
    declined |= live_v2_b6 & (if bd_v2_b6 { ALL } else { !ok_v2_b6 });
    take_2_2 |= live_v2_b6 & ok_v2_b6 & (if bd_v2_b6 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1084,
        c41: r_c41,
        c358: r_c358,
        c359: r_c359,
        c282: n1089,
        c360: r_c360,
        c361: r_c361,
        c284: n1090,
        c285: n1091,
        c362: n1142,
        c287: n1092,
        c294: n1093,
        c295: n1094,
        c370: n1146,
        c371: n1144,
        c301: n1112,
        h1: n1493, h2: n1494,
    };
    // body 6: buttons 0x02, forks 0x0
    sink.o2(2, take_2_2, &sh2, &o2);
    declined |= live_v16_b7 & (if bd_v16_b7 { ALL } else { !ok_v16_b7 });
    take_2_3 |= live_v16_b7 & ok_v16_b7 & (if bd_v16_b7 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1084,
        c41: r_c41,
        c358: r_c358,
        c359: r_c359,
        c282: n1089,
        c360: r_c360,
        c361: r_c361,
        c284: n1090,
        c285: n1091,
        c362: n1097,
        c287: n1153,
        c294: n1093,
        c295: n1154,
        c370: n1158,
        c371: n1156,
        c301: n1112,
        h1: n1519, h2: n1520,
    };
    // body 7: buttons 0x10, forks 0x0
    sink.o2(16, take_2_3, &sh2, &o2);
    declined |= live_v17_b8 & (if bd_v17_b8 { ALL } else { !ok_v17_b8 });
    take_2_4 |= live_v17_b8 & ok_v17_b8 & (if bd_v17_b8 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1084,
        c41: r_c41,
        c358: r_c358,
        c359: r_c359,
        c282: n1089,
        c360: r_c360,
        c361: r_c361,
        c284: n1090,
        c285: n1091,
        c362: n1126,
        c287: n1153,
        c294: n1093,
        c295: n1154,
        c370: n1166,
        c371: n1164,
        c301: n1112,
        h1: n1527, h2: n1528,
    };
    // body 8: buttons 0x11, forks 0x0
    sink.o2(17, take_2_4, &sh2, &o2);
    declined |= live_v18_b9 & (if bd_v18_b9 { ALL } else { !ok_v18_b9 });
    take_2_5 |= live_v18_b9 & ok_v18_b9 & (if bd_v18_b9 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1084,
        c41: r_c41,
        c358: r_c358,
        c359: r_c359,
        c282: n1089,
        c360: r_c360,
        c361: r_c361,
        c284: n1090,
        c285: n1091,
        c362: n1142,
        c287: n1153,
        c294: n1093,
        c295: n1154,
        c370: n1174,
        c371: n1172,
        c301: n1112,
        h1: n1535, h2: n1536,
    };
    // body 9: buttons 0x12, forks 0x0
    sink.o2(18, take_2_5, &sh2, &o2);
    declined |= live_v32_b10 & (if bd_v32_b10 { ALL } else { !ok_v32_b10 });
    take_2_6 |= live_v32_b10 & ok_v32_b10 & (if bd_v32_b10 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1193,
        c41: n1194,
        c358: n1199,
        c359: n1200,
        c282: n1195,
        c360: n1201,
        c361: n1202,
        c284: n1196,
        c285: n1197,
        c362: n1097,
        c287: n1092,
        c294: n1198,
        c295: n1094,
        c370: n1208,
        c371: n1204,
        c301: n1207,
        h1: n1580, h2: n1581,
    };
    // body 10: buttons 0x20, forks 0x0
    sink.o2(32, take_2_6, &sh2, &o2);
    declined |= live_v33_b11 & (if bd_v33_b11 { ALL } else { !ok_v33_b11 });
    take_2_7 |= live_v33_b11 & ok_v33_b11 & (if bd_v33_b11 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1193,
        c41: n1194,
        c358: n1199,
        c359: n1217,
        c282: n1195,
        c360: n1218,
        c361: n1202,
        c284: n1196,
        c285: n1197,
        c362: n1126,
        c287: n1092,
        c294: n1198,
        c295: n1094,
        c370: n1222,
        c371: n1220,
        c301: n1207,
        h1: n1596, h2: n1597,
    };
    // body 11: buttons 0x21, forks 0x0
    sink.o2(33, take_2_7, &sh2, &o2);
    declined |= live_v34_b12 & (if bd_v34_b12 { ALL } else { !ok_v34_b12 });
    take_2_8 |= live_v34_b12 & ok_v34_b12 & (if bd_v34_b12 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1193,
        c41: n1194,
        c358: n1199,
        c359: n1217,
        c282: n1195,
        c360: n1229,
        c361: n1202,
        c284: n1196,
        c285: n1197,
        c362: n1142,
        c287: n1092,
        c294: n1198,
        c295: n1094,
        c370: n1233,
        c371: n1231,
        c301: n1207,
        h1: n1609, h2: n1610,
    };
    // body 12: buttons 0x22, forks 0x0
    sink.o2(34, take_2_8, &sh2, &o2);
    declined |= live_v36_b13 & (if bd_v36_b13 { ALL } else { !ok_v36_b13 });
    take_2_9 |= live_v36_b13 & ok_v36_b13 & (if bd_v36_b13 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1193,
        c41: n1194,
        c358: n1247,
        c359: n1248,
        c282: n1195,
        c360: n1249,
        c361: n1250,
        c284: n1196,
        c285: n1197,
        c362: n1097,
        c287: n1092,
        c294: n1198,
        c295: n1094,
        c370: n1254,
        c371: n1252,
        c301: n1207,
        h1: n1629, h2: n1630,
    };
    // body 13: buttons 0x24, forks 0x0
    sink.o2(36, take_2_9, &sh2, &o2);
    declined |= live_v37_b14 & (if bd_v37_b14 { ALL } else { !ok_v37_b14 });
    take_2_10 |= live_v37_b14 & ok_v37_b14 & (if bd_v37_b14 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1193,
        c41: n1194,
        c358: n1247,
        c359: n1217,
        c282: n1195,
        c360: n1218,
        c361: n1250,
        c284: n1196,
        c285: n1197,
        c362: n1126,
        c287: n1092,
        c294: n1198,
        c295: n1094,
        c370: n1262,
        c371: n1260,
        c301: n1207,
        h1: n1643, h2: n1644,
    };
    // body 14: buttons 0x25, forks 0x0
    sink.o2(37, take_2_10, &sh2, &o2);
    declined |= live_v38_b15 & (if bd_v38_b15 { ALL } else { !ok_v38_b15 });
    take_2_11 |= live_v38_b15 & ok_v38_b15 & (if bd_v38_b15 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1193,
        c41: n1194,
        c358: n1247,
        c359: n1217,
        c282: n1195,
        c360: n1229,
        c361: n1250,
        c284: n1196,
        c285: n1197,
        c362: n1142,
        c287: n1092,
        c294: n1198,
        c295: n1094,
        c370: n1270,
        c371: n1268,
        c301: n1207,
        h1: n1655, h2: n1656,
    };
    // body 15: buttons 0x26, forks 0x0
    sink.o2(38, take_2_11, &sh2, &o2);
    declined |= live_v40_b16 & (if bd_v40_b16 { ALL } else { !ok_v40_b16 });
    take_2_12 |= live_v40_b16 & ok_v40_b16 & (if bd_v40_b16 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1193,
        c41: n1194,
        c358: n1247,
        c359: n1248,
        c282: n1195,
        c360: n1249,
        c361: n1275,
        c284: n1196,
        c285: n1197,
        c362: n1097,
        c287: n1092,
        c294: n1198,
        c295: n1094,
        c370: n1254,
        c371: n1276,
        c301: n1207,
        h1: n1665, h2: n1666,
    };
    // body 16: buttons 0x28, forks 0x0
    sink.o2(40, take_2_12, &sh2, &o2);
    declined |= live_v41_b17 & (if bd_v41_b17 { ALL } else { !ok_v41_b17 });
    take_2_13 |= live_v41_b17 & ok_v41_b17 & (if bd_v41_b17 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1193,
        c41: n1194,
        c358: n1247,
        c359: n1217,
        c282: n1195,
        c360: n1218,
        c361: n1275,
        c284: n1196,
        c285: n1197,
        c362: n1126,
        c287: n1092,
        c294: n1198,
        c295: n1094,
        c370: n1262,
        c371: n1279,
        c301: n1207,
        h1: n1674, h2: n1675,
    };
    // body 17: buttons 0x29, forks 0x0
    sink.o2(41, take_2_13, &sh2, &o2);
    declined |= live_v42_b18 & (if bd_v42_b18 { ALL } else { !ok_v42_b18 });
    take_2_14 |= live_v42_b18 & ok_v42_b18 & (if bd_v42_b18 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1193,
        c41: n1194,
        c358: n1247,
        c359: n1217,
        c282: n1195,
        c360: n1229,
        c361: n1275,
        c284: n1196,
        c285: n1197,
        c362: n1142,
        c287: n1092,
        c294: n1198,
        c295: n1094,
        c370: n1270,
        c371: n1282,
        c301: n1207,
        h1: n1683, h2: n1684,
    };
    // body 18: buttons 0x2a, forks 0x0
    sink.o2(42, take_2_14, &sh2, &o2);
    declined |= live_v48_b19 & (if bd_v48_b19 { ALL } else { !ok_v48_b19 });
    take_2_15 |= live_v48_b19 & ok_v48_b19 & (if bd_v48_b19 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1193,
        c41: n1194,
        c358: n1199,
        c359: n1200,
        c282: n1195,
        c360: n1201,
        c361: n1202,
        c284: n1196,
        c285: n1197,
        c362: n1097,
        c287: n1153,
        c294: n1198,
        c295: n1154,
        c370: n1290,
        c371: n1288,
        c301: n1207,
        h1: n1707, h2: n1708,
    };
    // body 19: buttons 0x30, forks 0x0
    sink.o2(48, take_2_15, &sh2, &o2);
    declined |= live_v49_b20 & (if bd_v49_b20 { ALL } else { !ok_v49_b20 });
    take_2_16 |= live_v49_b20 & ok_v49_b20 & (if bd_v49_b20 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1193,
        c41: n1194,
        c358: n1199,
        c359: n1217,
        c282: n1195,
        c360: n1218,
        c361: n1202,
        c284: n1196,
        c285: n1197,
        c362: n1126,
        c287: n1153,
        c294: n1198,
        c295: n1154,
        c370: n1298,
        c371: n1296,
        c301: n1207,
        h1: n1721, h2: n1722,
    };
    // body 20: buttons 0x31, forks 0x0
    sink.o2(49, take_2_16, &sh2, &o2);
    declined |= live_v50_b21 & (if bd_v50_b21 { ALL } else { !ok_v50_b21 });
    take_2_17 |= live_v50_b21 & ok_v50_b21 & (if bd_v50_b21 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1193,
        c41: n1194,
        c358: n1199,
        c359: n1217,
        c282: n1195,
        c360: n1229,
        c361: n1202,
        c284: n1196,
        c285: n1197,
        c362: n1142,
        c287: n1153,
        c294: n1198,
        c295: n1154,
        c370: n1306,
        c371: n1304,
        c301: n1207,
        h1: n1733, h2: n1734,
    };
    // body 21: buttons 0x32, forks 0x0
    sink.o2(50, take_2_17, &sh2, &o2);
    declined |= live_v52_b22 & (if bd_v52_b22 { ALL } else { !ok_v52_b22 });
    take_2_18 |= live_v52_b22 & ok_v52_b22 & (if bd_v52_b22 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1193,
        c41: n1194,
        c358: n1247,
        c359: n1248,
        c282: n1195,
        c360: n1249,
        c361: n1250,
        c284: n1196,
        c285: n1197,
        c362: n1097,
        c287: n1153,
        c294: n1198,
        c295: n1154,
        c370: n1314,
        c371: n1312,
        c301: n1207,
        h1: n1749, h2: n1750,
    };
    // body 22: buttons 0x34, forks 0x0
    sink.o2(52, take_2_18, &sh2, &o2);
    declined |= live_v53_b23 & (if bd_v53_b23 { ALL } else { !ok_v53_b23 });
    take_2_19 |= live_v53_b23 & ok_v53_b23 & (if bd_v53_b23 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1193,
        c41: n1194,
        c358: n1247,
        c359: n1217,
        c282: n1195,
        c360: n1218,
        c361: n1250,
        c284: n1196,
        c285: n1197,
        c362: n1126,
        c287: n1153,
        c294: n1198,
        c295: n1154,
        c370: n1322,
        c371: n1320,
        c301: n1207,
        h1: n1763, h2: n1764,
    };
    // body 23: buttons 0x35, forks 0x0
    sink.o2(53, take_2_19, &sh2, &o2);
    declined |= live_v54_b24 & (if bd_v54_b24 { ALL } else { !ok_v54_b24 });
    take_2_20 |= live_v54_b24 & ok_v54_b24 & (if bd_v54_b24 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1193,
        c41: n1194,
        c358: n1247,
        c359: n1217,
        c282: n1195,
        c360: n1229,
        c361: n1250,
        c284: n1196,
        c285: n1197,
        c362: n1142,
        c287: n1153,
        c294: n1198,
        c295: n1154,
        c370: n1330,
        c371: n1328,
        c301: n1207,
        h1: n1775, h2: n1776,
    };
    // body 24: buttons 0x36, forks 0x0
    sink.o2(54, take_2_20, &sh2, &o2);
    declined |= live_v56_b25 & (if bd_v56_b25 { ALL } else { !ok_v56_b25 });
    take_2_21 |= live_v56_b25 & ok_v56_b25 & (if bd_v56_b25 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1193,
        c41: n1194,
        c358: n1247,
        c359: n1248,
        c282: n1195,
        c360: n1249,
        c361: n1275,
        c284: n1196,
        c285: n1197,
        c362: n1097,
        c287: n1153,
        c294: n1198,
        c295: n1154,
        c370: n1314,
        c371: n1333,
        c301: n1207,
        h1: n1784, h2: n1785,
    };
    // body 25: buttons 0x38, forks 0x0
    sink.o2(56, take_2_21, &sh2, &o2);
    declined |= live_v57_b26 & (if bd_v57_b26 { ALL } else { !ok_v57_b26 });
    take_2_22 |= live_v57_b26 & ok_v57_b26 & (if bd_v57_b26 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1193,
        c41: n1194,
        c358: n1247,
        c359: n1217,
        c282: n1195,
        c360: n1218,
        c361: n1275,
        c284: n1196,
        c285: n1197,
        c362: n1126,
        c287: n1153,
        c294: n1198,
        c295: n1154,
        c370: n1322,
        c371: n1336,
        c301: n1207,
        h1: n1793, h2: n1794,
    };
    // body 26: buttons 0x39, forks 0x0
    sink.o2(57, take_2_22, &sh2, &o2);
    declined |= live_v58_b27 & (if bd_v58_b27 { ALL } else { !ok_v58_b27 });
    take_2_23 |= live_v58_b27 & ok_v58_b27 & (if bd_v58_b27 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1193,
        c41: n1194,
        c358: n1247,
        c359: n1217,
        c282: n1195,
        c360: n1229,
        c361: n1275,
        c284: n1196,
        c285: n1197,
        c362: n1142,
        c287: n1153,
        c294: n1198,
        c295: n1154,
        c370: n1330,
        c371: n1339,
        c301: n1207,
        h1: n1802, h2: n1803,
    };
    // body 27: buttons 0x3a, forks 0x0
    sink.o2(58, take_2_23, &sh2, &o2);
    declined
}
