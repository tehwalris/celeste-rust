// GENERATED from a TRACED frame (shape 4). Do not edit.
//
// One input shape, 3 output shapes, 52 distinct button
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
    pub c84: ZN,
    pub c86: ZN,
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
    let n505: ZB = zb_not(n504);
    let n506: ZB = zb_and(n499, n504);
    let n507: ZB = zb_and(n499, n505);
    let n508: ZN = zn_rem(n493, zn_splat(P8::from_raw(524288i32)));
    let n509: ZB = zn_ge(n508, zn_splat(P8::from_raw(393216i32)));
    let n510: ZB = zn_lt(n508, zn_splat(P8::from_raw(393216i32)));
    let n511: ZB = zb_and(n506, n510);
    let n512: ZB = zb_and(n506, n509);
    let n513: ZN = zn_mul(n496, zn_splat(P8::from_raw(524288i32)));
    let n514: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n513);
    let n515: ZB = zn_eq(n492, n514);
    let n516: ZB = zb_or(n511, n512);
    let n517: ZB = zb_or(n509, n515);
    let n518: ZB = zb_or(n507, n516);
    let n519: ZB = zb_and(n504, n517);
    let n520: ZB = zb_not(n519);
    let n521: ZB = zb_and(n518, n519);
    let n522: ZB = zb_and(n518, n520);
    let n523: ZB = zn_ge(n473, zn_splat(P8::from_raw(0i32)));
    let n524: ZB = zb_or(n521, n522);
    let n525: ZB = zb_and(n519, n523);
    let n526: ZB = zb_not(n525);
    let n527: ZB = zb_and(n524, n525);
    let n528: ZB = zb_and(n524, n526);
    let n529: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n503);
    let n530: ZB = zb_not(n529);
    let n531: ZB = zb_and(n528, n529);
    let n532: ZB = zb_and(n528, n530);
    let n533: ZN = zn_rem(n476, zn_splat(P8::from_raw(524288i32)));
    let n534: ZB = zn_le(n533, zn_splat(P8::from_raw(131072i32)));
    let n535: ZB = zb_or(n531, n532);
    let n536: ZB = zb_and(n529, n534);
    let n537: ZB = zb_not(n536);
    let n538: ZB = zb_and(n535, n536);
    let n539: ZB = zb_and(n535, n537);
    let n540: ZB = zn_le(n473, zn_splat(P8::from_raw(0i32)));
    let n541: ZB = zb_or(n538, n539);
    let n542: ZB = zb_and(n536, n540);
    let n543: ZB = zb_not(n542);
    let n544: ZB = zb_and(n541, n542);
    let n545: ZB = zb_and(n541, n543);
    let n546: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n503);
    let n547: ZB = zb_not(n546);
    let n548: ZB = zb_and(n545, n546);
    let n549: ZB = zb_and(n545, n547);
    let n550: ZN = zn_rem(n475, zn_splat(P8::from_raw(524288i32)));
    let n551: ZB = zn_le(n550, zn_splat(P8::from_raw(131072i32)));
    let n552: ZB = zb_or(n548, n549);
    let n553: ZB = zb_and(n546, n551);
    let n554: ZB = zb_not(n553);
    let n555: ZB = zb_and(n552, n553);
    let n556: ZB = zb_and(n552, n554);
    let n557: ZB = zn_le(n472, zn_splat(P8::from_raw(0i32)));
    let n558: ZB = zb_or(n555, n556);
    let n559: ZB = zb_and(n553, n557);
    let n560: ZB = zb_not(n559);
    let n561: ZB = zb_and(n558, n559);
    let n562: ZB = zb_and(n558, n560);
    let n563: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n503);
    let n564: ZB = zb_not(n563);
    let n565: ZB = zb_and(n562, n563);
    let n566: ZB = zb_and(n562, n564);
    let n567: ZN = zn_rem(n481, zn_splat(P8::from_raw(524288i32)));
    let n568: ZB = zn_ge(n567, zn_splat(P8::from_raw(393216i32)));
    let n569: ZB = zn_lt(n567, zn_splat(P8::from_raw(393216i32)));
    let n570: ZB = zb_and(n565, n569);
    let n571: ZB = zb_and(n565, n568);
    let n572: ZN = zn_mul(n484, zn_splat(P8::from_raw(524288i32)));
    let n573: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n572);
    let n574: ZB = zn_eq(n480, n573);
    let n575: ZB = zb_or(n570, n571);
    let n576: ZB = zb_or(n568, n574);
    let n577: ZB = zb_or(n566, n575);
    let n578: ZB = zb_and(n563, n576);
    let n579: ZB = zb_not(n578);
    let n580: ZB = zb_and(n577, n578);
    let n581: ZB = zb_and(n577, n579);
    let n582: ZB = zn_ge(n472, zn_splat(P8::from_raw(0i32)));
    let n583: ZB = zb_or(n580, n581);
    let n584: ZB = zb_and(n578, n582);
    let n585: ZB = zb_not(n584);
    let n586: ZB = zb_and(n583, n584);
    let n587: ZB = zb_and(n583, n585);
    let n588: ZB = zb_or(n561, n586);
    let n589: ZB = zb_or(n544, n588);
    let n590: ZB = zb_or(n527, n589);
    let n591: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n491);
    let n592: ZB = zn_le(n591, n495);
    let n593: ZB = zn_gt(n591, n495);
    let n594: ZB = zb_and(n587, n592);
    let n595: ZB = zb_and(n587, n593);
    let n596: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n591);
    let n597: ZN = zn_mget(g.cart, n501, n596);
    let n598: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n597);
    let n599: ZB = zb_not(n598);
    let n600: ZB = zb_and(n594, n598);
    let n601: ZB = zb_and(n594, n599);
    let n602: ZB = zb_and(n510, n600);
    let n603: ZB = zb_and(n509, n600);
    let n604: ZN = zn_mul(n591, zn_splat(P8::from_raw(524288i32)));
    let n605: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n604);
    let n606: ZB = zn_eq(n492, n605);
    let n607: ZB = zb_or(n602, n603);
    let n608: ZB = zb_or(n509, n606);
    let n609: ZB = zb_or(n601, n607);
    let n610: ZB = zb_and(n598, n608);
    let n611: ZB = zb_not(n610);
    let n612: ZB = zb_and(n609, n610);
    let n613: ZB = zb_and(n609, n611);
    let n614: ZB = zb_or(n612, n613);
    let n615: ZB = zb_and(n523, n610);
    let n616: ZB = zb_not(n615);
    let n617: ZB = zb_and(n614, n615);
    let n618: ZB = zb_and(n614, n616);
    let n619: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n597);
    let n620: ZB = zb_not(n619);
    let n621: ZB = zb_and(n618, n619);
    let n622: ZB = zb_and(n618, n620);
    let n623: ZB = zb_or(n621, n622);
    let n624: ZB = zb_and(n534, n619);
    let n625: ZB = zb_not(n624);
    let n626: ZB = zb_and(n623, n624);
    let n627: ZB = zb_and(n623, n625);
    let n628: ZB = zb_or(n626, n627);
    let n629: ZB = zb_and(n540, n624);
    let n630: ZB = zb_not(n629);
    let n631: ZB = zb_and(n628, n629);
    let n632: ZB = zb_and(n628, n630);
    let n633: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n597);
    let n634: ZB = zb_not(n633);
    let n635: ZB = zb_and(n632, n633);
    let n636: ZB = zb_and(n632, n634);
    let n637: ZB = zb_or(n635, n636);
    let n638: ZB = zb_and(n551, n633);
    let n639: ZB = zb_not(n638);
    let n640: ZB = zb_and(n637, n638);
    let n641: ZB = zb_and(n637, n639);
    let n642: ZB = zb_or(n640, n641);
    let n643: ZB = zb_and(n557, n638);
    let n644: ZB = zb_not(n643);
    let n645: ZB = zb_and(n642, n643);
    let n646: ZB = zb_and(n642, n644);
    let n647: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n597);
    let n648: ZB = zb_not(n647);
    let n649: ZB = zb_and(n646, n647);
    let n650: ZB = zb_and(n646, n648);
    let n651: ZB = zb_and(n569, n649);
    let n652: ZB = zb_and(n568, n649);
    let n653: ZB = zb_or(n651, n652);
    let n654: ZB = zb_or(n650, n653);
    let n655: ZB = zb_and(n576, n647);
    let n656: ZB = zb_not(n655);
    let n657: ZB = zb_and(n654, n655);
    let n658: ZB = zb_and(n654, n656);
    let n659: ZB = zb_or(n657, n658);
    let n660: ZB = zb_and(n582, n655);
    let n661: ZB = zb_not(n660);
    let n662: ZB = zb_and(n659, n660);
    let n663: ZB = zb_and(n659, n661);
    let n664: ZB = zb_or(n645, n662);
    let n665: ZB = zb_or(n631, n664);
    let n666: ZB = zb_or(n617, n665);
    let n667: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n491);
    let n668: ZB = zn_le(n667, n495);
    let n669: ZB = zn_gt(n667, n495);
    let n670: ZB = zb_and(n663, n668);
    let n671: ZB = zb_and(n663, n669);
    let n672: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n667);
    let n673: ZN = zn_mget(g.cart, n501, n672);
    let n674: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n673);
    let n675: ZB = zb_not(n674);
    let n676: ZB = zb_and(n670, n674);
    let n677: ZB = zb_and(n670, n675);
    let n678: ZB = zb_and(n510, n676);
    let n679: ZB = zb_and(n509, n676);
    let n680: ZN = zn_mul(n667, zn_splat(P8::from_raw(524288i32)));
    let n681: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n680);
    let n682: ZB = zn_eq(n492, n681);
    let n683: ZB = zb_or(n678, n679);
    let n684: ZB = zb_or(n509, n682);
    let n685: ZB = zb_or(n677, n683);
    let n686: ZB = zb_and(n674, n684);
    let n687: ZB = zb_not(n686);
    let n688: ZB = zb_and(n685, n686);
    let n689: ZB = zb_and(n685, n687);
    let n690: ZB = zb_or(n688, n689);
    let n691: ZB = zb_and(n523, n686);
    let n692: ZB = zb_not(n691);
    let n693: ZB = zb_and(n690, n691);
    let n694: ZB = zb_and(n690, n692);
    let n695: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n673);
    let n696: ZB = zb_not(n695);
    let n697: ZB = zb_and(n694, n695);
    let n698: ZB = zb_and(n694, n696);
    let n699: ZB = zb_or(n697, n698);
    let n700: ZB = zb_and(n534, n695);
    let n701: ZB = zb_not(n700);
    let n702: ZB = zb_and(n699, n700);
    let n703: ZB = zb_and(n699, n701);
    let n704: ZB = zb_or(n702, n703);
    let n705: ZB = zb_and(n540, n700);
    let n706: ZB = zb_not(n705);
    let n707: ZB = zb_and(n704, n705);
    let n708: ZB = zb_and(n704, n706);
    let n709: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n673);
    let n710: ZB = zb_not(n709);
    let n711: ZB = zb_and(n708, n709);
    let n712: ZB = zb_and(n708, n710);
    let n713: ZB = zb_or(n711, n712);
    let n714: ZB = zb_and(n551, n709);
    let n715: ZB = zb_not(n714);
    let n716: ZB = zb_and(n713, n714);
    let n717: ZB = zb_and(n713, n715);
    let n718: ZB = zb_or(n716, n717);
    let n719: ZB = zb_and(n557, n714);
    let n720: ZB = zb_not(n719);
    let n721: ZB = zb_and(n718, n719);
    let n722: ZB = zb_and(n718, n720);
    let n723: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n673);
    let n724: ZB = zb_not(n723);
    let n725: ZB = zb_and(n722, n723);
    let n726: ZB = zb_and(n722, n724);
    let n727: ZB = zb_and(n569, n725);
    let n728: ZB = zb_and(n568, n725);
    let n729: ZB = zb_or(n727, n728);
    let n730: ZB = zb_or(n726, n729);
    let n731: ZB = zb_and(n576, n723);
    let n732: ZB = zb_not(n731);
    let n733: ZB = zb_and(n730, n731);
    let n734: ZB = zb_and(n730, n732);
    let n735: ZB = zb_or(n733, n734);
    let n736: ZB = zb_and(n582, n731);
    let n737: ZB = zb_not(n736);
    let n738: ZB = zb_and(n735, n736);
    let n739: ZB = zb_and(n735, n737);
    let n740: ZB = zb_or(n721, n738);
    let n741: ZB = zb_or(n707, n740);
    let n742: ZB = zb_or(n693, n741);
    let n743: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n491);
    let n744: ZB = zn_gt(n743, n495);
    let n745: ZB = zb_and(n474, n744);
    let n746: ZB = zb_or(n671, n739);
    let n747: ZB = zsel_b(n669, n474, n745);
    let n748: ZB = zb_or(n666, n742);
    let n749: ZB = zb_or(n595, n746);
    let n750: ZB = zsel_b(n593, n474, n747);
    let n751: ZB = zb_or(n590, n748);
    let n752: ZB = zb_or(n500, n749);
    let n753: ZB = zsel_b(n498, n474, n750);
    let n754: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n479);
    let n755: ZB = zn_le(n754, n483);
    let n756: ZB = zn_gt(n754, n483);
    let n757: ZB = zb_and(n752, n755);
    let n758: ZB = zb_and(n752, n756);
    let n759: ZB = zb_and(n497, n757);
    let n760: ZB = zb_and(n498, n757);
    let n761: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n754);
    let n762: ZN = zn_mget(g.cart, n761, n502);
    let n763: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n762);
    let n764: ZB = zb_not(n763);
    let n765: ZB = zb_and(n759, n763);
    let n766: ZB = zb_and(n759, n764);
    let n767: ZB = zb_and(n510, n765);
    let n768: ZB = zb_and(n509, n765);
    let n769: ZB = zb_or(n767, n768);
    let n770: ZB = zb_or(n766, n769);
    let n771: ZB = zb_and(n517, n763);
    let n772: ZB = zb_not(n771);
    let n773: ZB = zb_and(n770, n771);
    let n774: ZB = zb_and(n770, n772);
    let n775: ZB = zb_or(n773, n774);
    let n776: ZB = zb_and(n523, n771);
    let n777: ZB = zb_not(n776);
    let n778: ZB = zb_and(n775, n776);
    let n779: ZB = zb_and(n775, n777);
    let n780: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n762);
    let n781: ZB = zb_not(n780);
    let n782: ZB = zb_and(n779, n780);
    let n783: ZB = zb_and(n779, n781);
    let n784: ZB = zb_or(n782, n783);
    let n785: ZB = zb_and(n534, n780);
    let n786: ZB = zb_not(n785);
    let n787: ZB = zb_and(n784, n785);
    let n788: ZB = zb_and(n784, n786);
    let n789: ZB = zb_or(n787, n788);
    let n790: ZB = zb_and(n540, n785);
    let n791: ZB = zb_not(n790);
    let n792: ZB = zb_and(n789, n790);
    let n793: ZB = zb_and(n789, n791);
    let n794: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n762);
    let n795: ZB = zb_not(n794);
    let n796: ZB = zb_and(n793, n794);
    let n797: ZB = zb_and(n793, n795);
    let n798: ZB = zb_or(n796, n797);
    let n799: ZB = zb_and(n551, n794);
    let n800: ZB = zb_not(n799);
    let n801: ZB = zb_and(n798, n799);
    let n802: ZB = zb_and(n798, n800);
    let n803: ZB = zb_or(n801, n802);
    let n804: ZB = zb_and(n557, n799);
    let n805: ZB = zb_not(n804);
    let n806: ZB = zb_and(n803, n804);
    let n807: ZB = zb_and(n803, n805);
    let n808: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n762);
    let n809: ZB = zb_not(n808);
    let n810: ZB = zb_and(n807, n808);
    let n811: ZB = zb_and(n807, n809);
    let n812: ZB = zb_and(n569, n810);
    let n813: ZB = zb_and(n568, n810);
    let n814: ZN = zn_mul(n754, zn_splat(P8::from_raw(524288i32)));
    let n815: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n814);
    let n816: ZB = zn_eq(n480, n815);
    let n817: ZB = zb_or(n812, n813);
    let n818: ZB = zb_or(n568, n816);
    let n819: ZB = zb_or(n811, n817);
    let n820: ZB = zb_and(n808, n818);
    let n821: ZB = zb_not(n820);
    let n822: ZB = zb_and(n819, n820);
    let n823: ZB = zb_and(n819, n821);
    let n824: ZB = zb_or(n822, n823);
    let n825: ZB = zb_and(n582, n820);
    let n826: ZB = zb_not(n825);
    let n827: ZB = zb_and(n824, n825);
    let n828: ZB = zb_and(n824, n826);
    let n829: ZB = zb_or(n806, n827);
    let n830: ZB = zb_or(n792, n829);
    let n831: ZB = zb_or(n778, n830);
    let n832: ZB = zb_and(n592, n828);
    let n833: ZB = zb_and(n593, n828);
    let n834: ZN = zn_mget(g.cart, n761, n596);
    let n835: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n834);
    let n836: ZB = zb_not(n835);
    let n837: ZB = zb_and(n832, n835);
    let n838: ZB = zb_and(n832, n836);
    let n839: ZB = zb_and(n510, n837);
    let n840: ZB = zb_and(n509, n837);
    let n841: ZB = zb_or(n839, n840);
    let n842: ZB = zb_or(n838, n841);
    let n843: ZB = zb_and(n608, n835);
    let n844: ZB = zb_not(n843);
    let n845: ZB = zb_and(n842, n843);
    let n846: ZB = zb_and(n842, n844);
    let n847: ZB = zb_or(n845, n846);
    let n848: ZB = zb_and(n523, n843);
    let n849: ZB = zb_not(n848);
    let n850: ZB = zb_and(n847, n848);
    let n851: ZB = zb_and(n847, n849);
    let n852: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n834);
    let n853: ZB = zb_not(n852);
    let n854: ZB = zb_and(n851, n852);
    let n855: ZB = zb_and(n851, n853);
    let n856: ZB = zb_or(n854, n855);
    let n857: ZB = zb_and(n534, n852);
    let n858: ZB = zb_not(n857);
    let n859: ZB = zb_and(n856, n857);
    let n860: ZB = zb_and(n856, n858);
    let n861: ZB = zb_or(n859, n860);
    let n862: ZB = zb_and(n540, n857);
    let n863: ZB = zb_not(n862);
    let n864: ZB = zb_and(n861, n862);
    let n865: ZB = zb_and(n861, n863);
    let n866: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n834);
    let n867: ZB = zb_not(n866);
    let n868: ZB = zb_and(n865, n866);
    let n869: ZB = zb_and(n865, n867);
    let n870: ZB = zb_or(n868, n869);
    let n871: ZB = zb_and(n551, n866);
    let n872: ZB = zb_not(n871);
    let n873: ZB = zb_and(n870, n871);
    let n874: ZB = zb_and(n870, n872);
    let n875: ZB = zb_or(n873, n874);
    let n876: ZB = zb_and(n557, n871);
    let n877: ZB = zb_not(n876);
    let n878: ZB = zb_and(n875, n876);
    let n879: ZB = zb_and(n875, n877);
    let n880: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n834);
    let n881: ZB = zb_not(n880);
    let n882: ZB = zb_and(n879, n880);
    let n883: ZB = zb_and(n879, n881);
    let n884: ZB = zb_and(n569, n882);
    let n885: ZB = zb_and(n568, n882);
    let n886: ZB = zb_or(n884, n885);
    let n887: ZB = zb_or(n883, n886);
    let n888: ZB = zb_and(n818, n880);
    let n889: ZB = zb_not(n888);
    let n890: ZB = zb_and(n887, n888);
    let n891: ZB = zb_and(n887, n889);
    let n892: ZB = zb_or(n890, n891);
    let n893: ZB = zb_and(n582, n888);
    let n894: ZB = zb_not(n893);
    let n895: ZB = zb_and(n892, n893);
    let n896: ZB = zb_and(n892, n894);
    let n897: ZB = zb_or(n878, n895);
    let n898: ZB = zb_or(n864, n897);
    let n899: ZB = zb_or(n850, n898);
    let n900: ZB = zb_and(n668, n896);
    let n901: ZB = zb_and(n669, n896);
    let n902: ZN = zn_mget(g.cart, n761, n672);
    let n903: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n902);
    let n904: ZB = zb_not(n903);
    let n905: ZB = zb_and(n900, n903);
    let n906: ZB = zb_and(n900, n904);
    let n907: ZB = zb_and(n510, n905);
    let n908: ZB = zb_and(n509, n905);
    let n909: ZB = zb_or(n907, n908);
    let n910: ZB = zb_or(n906, n909);
    let n911: ZB = zb_and(n684, n903);
    let n912: ZB = zb_not(n911);
    let n913: ZB = zb_and(n910, n911);
    let n914: ZB = zb_and(n910, n912);
    let n915: ZB = zb_or(n913, n914);
    let n916: ZB = zb_and(n523, n911);
    let n917: ZB = zb_not(n916);
    let n918: ZB = zb_and(n915, n916);
    let n919: ZB = zb_and(n915, n917);
    let n920: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n902);
    let n921: ZB = zb_not(n920);
    let n922: ZB = zb_and(n919, n920);
    let n923: ZB = zb_and(n919, n921);
    let n924: ZB = zb_or(n922, n923);
    let n925: ZB = zb_and(n534, n920);
    let n926: ZB = zb_not(n925);
    let n927: ZB = zb_and(n924, n925);
    let n928: ZB = zb_and(n924, n926);
    let n929: ZB = zb_or(n927, n928);
    let n930: ZB = zb_and(n540, n925);
    let n931: ZB = zb_not(n930);
    let n932: ZB = zb_and(n929, n930);
    let n933: ZB = zb_and(n929, n931);
    let n934: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n902);
    let n935: ZB = zb_not(n934);
    let n936: ZB = zb_and(n933, n934);
    let n937: ZB = zb_and(n933, n935);
    let n938: ZB = zb_or(n936, n937);
    let n939: ZB = zb_and(n551, n934);
    let n940: ZB = zb_not(n939);
    let n941: ZB = zb_and(n938, n939);
    let n942: ZB = zb_and(n938, n940);
    let n943: ZB = zb_or(n941, n942);
    let n944: ZB = zb_and(n557, n939);
    let n945: ZB = zb_not(n944);
    let n946: ZB = zb_and(n943, n944);
    let n947: ZB = zb_and(n943, n945);
    let n948: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n902);
    let n949: ZB = zb_not(n948);
    let n950: ZB = zb_and(n947, n948);
    let n951: ZB = zb_and(n947, n949);
    let n952: ZB = zb_and(n569, n950);
    let n953: ZB = zb_and(n568, n950);
    let n954: ZB = zb_or(n952, n953);
    let n955: ZB = zb_or(n951, n954);
    let n956: ZB = zb_and(n818, n948);
    let n957: ZB = zb_not(n956);
    let n958: ZB = zb_and(n955, n956);
    let n959: ZB = zb_and(n955, n957);
    let n960: ZB = zb_or(n958, n959);
    let n961: ZB = zb_and(n582, n956);
    let n962: ZB = zb_not(n961);
    let n963: ZB = zb_and(n960, n961);
    let n964: ZB = zb_and(n960, n962);
    let n965: ZB = zb_or(n946, n963);
    let n966: ZB = zb_or(n932, n965);
    let n967: ZB = zb_or(n918, n966);
    let n968: ZB = zb_and(n744, n753);
    let n969: ZB = zb_or(n901, n964);
    let n970: ZB = zsel_b(n669, n753, n968);
    let n971: ZB = zb_or(n899, n967);
    let n972: ZB = zb_or(n833, n969);
    let n973: ZB = zsel_b(n593, n753, n970);
    let n974: ZB = zb_or(n831, n971);
    let n975: ZB = zb_or(n760, n972);
    let n976: ZB = zsel_b(n498, n753, n973);
    let n977: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n479);
    let n978: ZB = zn_le(n977, n483);
    let n979: ZB = zn_gt(n977, n483);
    let n980: ZB = zb_and(n975, n978);
    let n981: ZB = zb_and(n975, n979);
    let n982: ZB = zb_and(n497, n980);
    let n983: ZB = zb_and(n498, n980);
    let n984: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n977);
    let n985: ZN = zn_mget(g.cart, n984, n502);
    let n986: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n985);
    let n987: ZB = zb_not(n986);
    let n988: ZB = zb_and(n982, n986);
    let n989: ZB = zb_and(n982, n987);
    let n990: ZB = zb_and(n510, n988);
    let n991: ZB = zb_and(n509, n988);
    let n992: ZB = zb_or(n990, n991);
    let n993: ZB = zb_or(n989, n992);
    let n994: ZB = zb_and(n517, n986);
    let n995: ZB = zb_not(n994);
    let n996: ZB = zb_and(n993, n994);
    let n997: ZB = zb_and(n993, n995);
    let n998: ZB = zb_or(n996, n997);
    let n999: ZB = zb_and(n523, n994);
    let n1000: ZB = zb_not(n999);
    let n1001: ZB = zb_and(n998, n999);
    let n1002: ZB = zb_and(n998, n1000);
    let n1003: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n985);
    let n1004: ZB = zb_not(n1003);
    let n1005: ZB = zb_and(n1002, n1003);
    let n1006: ZB = zb_and(n1002, n1004);
    let n1007: ZB = zb_or(n1005, n1006);
    let n1008: ZB = zb_and(n534, n1003);
    let n1009: ZB = zb_not(n1008);
    let n1010: ZB = zb_and(n1007, n1008);
    let n1011: ZB = zb_and(n1007, n1009);
    let n1012: ZB = zb_or(n1010, n1011);
    let n1013: ZB = zb_and(n540, n1008);
    let n1014: ZB = zb_not(n1013);
    let n1015: ZB = zb_and(n1012, n1013);
    let n1016: ZB = zb_and(n1012, n1014);
    let n1017: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n985);
    let n1018: ZB = zb_not(n1017);
    let n1019: ZB = zb_and(n1016, n1017);
    let n1020: ZB = zb_and(n1016, n1018);
    let n1021: ZB = zb_or(n1019, n1020);
    let n1022: ZB = zb_and(n551, n1017);
    let n1023: ZB = zb_not(n1022);
    let n1024: ZB = zb_and(n1021, n1022);
    let n1025: ZB = zb_and(n1021, n1023);
    let n1026: ZB = zb_or(n1024, n1025);
    let n1027: ZB = zb_and(n557, n1022);
    let n1028: ZB = zb_not(n1027);
    let n1029: ZB = zb_and(n1026, n1027);
    let n1030: ZB = zb_and(n1026, n1028);
    let n1031: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n985);
    let n1032: ZB = zb_not(n1031);
    let n1033: ZB = zb_and(n1030, n1031);
    let n1034: ZB = zb_and(n1030, n1032);
    let n1035: ZB = zb_and(n569, n1033);
    let n1036: ZB = zb_and(n568, n1033);
    let n1037: ZN = zn_mul(n977, zn_splat(P8::from_raw(524288i32)));
    let n1038: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1037);
    let n1039: ZB = zn_eq(n480, n1038);
    let n1040: ZB = zb_or(n1035, n1036);
    let n1041: ZB = zb_or(n568, n1039);
    let n1042: ZB = zb_or(n1034, n1040);
    let n1043: ZB = zb_and(n1031, n1041);
    let n1044: ZB = zb_not(n1043);
    let n1045: ZB = zb_and(n1042, n1043);
    let n1046: ZB = zb_and(n1042, n1044);
    let n1047: ZB = zb_or(n1045, n1046);
    let n1048: ZB = zb_and(n582, n1043);
    let n1049: ZB = zb_not(n1048);
    let n1050: ZB = zb_and(n1047, n1048);
    let n1051: ZB = zb_and(n1047, n1049);
    let n1052: ZB = zb_or(n1029, n1050);
    let n1053: ZB = zb_or(n1015, n1052);
    let n1054: ZB = zb_or(n1001, n1053);
    let n1055: ZB = zb_and(n592, n1051);
    let n1056: ZB = zb_and(n593, n1051);
    let n1057: ZN = zn_mget(g.cart, n984, n596);
    let n1058: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1057);
    let n1059: ZB = zb_not(n1058);
    let n1060: ZB = zb_and(n1055, n1058);
    let n1061: ZB = zb_and(n1055, n1059);
    let n1062: ZB = zb_and(n510, n1060);
    let n1063: ZB = zb_and(n509, n1060);
    let n1064: ZB = zb_or(n1062, n1063);
    let n1065: ZB = zb_or(n1061, n1064);
    let n1066: ZB = zb_and(n608, n1058);
    let n1067: ZB = zb_not(n1066);
    let n1068: ZB = zb_and(n1065, n1066);
    let n1069: ZB = zb_and(n1065, n1067);
    let n1070: ZB = zb_or(n1068, n1069);
    let n1071: ZB = zb_and(n523, n1066);
    let n1072: ZB = zb_not(n1071);
    let n1073: ZB = zb_and(n1070, n1071);
    let n1074: ZB = zb_and(n1070, n1072);
    let n1075: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1057);
    let n1076: ZB = zb_not(n1075);
    let n1077: ZB = zb_and(n1074, n1075);
    let n1078: ZB = zb_and(n1074, n1076);
    let n1079: ZB = zb_or(n1077, n1078);
    let n1080: ZB = zb_and(n534, n1075);
    let n1081: ZB = zb_not(n1080);
    let n1082: ZB = zb_and(n1079, n1080);
    let n1083: ZB = zb_and(n1079, n1081);
    let n1084: ZB = zb_or(n1082, n1083);
    let n1085: ZB = zb_and(n540, n1080);
    let n1086: ZB = zb_not(n1085);
    let n1087: ZB = zb_and(n1084, n1085);
    let n1088: ZB = zb_and(n1084, n1086);
    let n1089: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1057);
    let n1090: ZB = zb_not(n1089);
    let n1091: ZB = zb_and(n1088, n1089);
    let n1092: ZB = zb_and(n1088, n1090);
    let n1093: ZB = zb_or(n1091, n1092);
    let n1094: ZB = zb_and(n551, n1089);
    let n1095: ZB = zb_not(n1094);
    let n1096: ZB = zb_and(n1093, n1094);
    let n1097: ZB = zb_and(n1093, n1095);
    let n1098: ZB = zb_or(n1096, n1097);
    let n1099: ZB = zb_and(n557, n1094);
    let n1100: ZB = zb_not(n1099);
    let n1101: ZB = zb_and(n1098, n1099);
    let n1102: ZB = zb_and(n1098, n1100);
    let n1103: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1057);
    let n1104: ZB = zb_not(n1103);
    let n1105: ZB = zb_and(n1102, n1103);
    let n1106: ZB = zb_and(n1102, n1104);
    let n1107: ZB = zb_and(n569, n1105);
    let n1108: ZB = zb_and(n568, n1105);
    let n1109: ZB = zb_or(n1107, n1108);
    let n1110: ZB = zb_or(n1106, n1109);
    let n1111: ZB = zb_and(n1041, n1103);
    let n1112: ZB = zb_not(n1111);
    let n1113: ZB = zb_and(n1110, n1111);
    let n1114: ZB = zb_and(n1110, n1112);
    let n1115: ZB = zb_or(n1113, n1114);
    let n1116: ZB = zb_and(n582, n1111);
    let n1117: ZB = zb_not(n1116);
    let n1118: ZB = zb_and(n1115, n1116);
    let n1119: ZB = zb_and(n1115, n1117);
    let n1120: ZB = zb_or(n1101, n1118);
    let n1121: ZB = zb_or(n1087, n1120);
    let n1122: ZB = zb_or(n1073, n1121);
    let n1123: ZB = zb_and(n668, n1119);
    let n1124: ZB = zb_and(n669, n1119);
    let n1125: ZN = zn_mget(g.cart, n984, n672);
    let n1126: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1125);
    let n1127: ZB = zb_not(n1126);
    let n1128: ZB = zb_and(n1123, n1126);
    let n1129: ZB = zb_and(n1123, n1127);
    let n1130: ZB = zb_and(n510, n1128);
    let n1131: ZB = zb_and(n509, n1128);
    let n1132: ZB = zb_or(n1130, n1131);
    let n1133: ZB = zb_or(n1129, n1132);
    let n1134: ZB = zb_and(n684, n1126);
    let n1135: ZB = zb_not(n1134);
    let n1136: ZB = zb_and(n1133, n1134);
    let n1137: ZB = zb_and(n1133, n1135);
    let n1138: ZB = zb_or(n1136, n1137);
    let n1139: ZB = zb_and(n523, n1134);
    let n1140: ZB = zb_not(n1139);
    let n1141: ZB = zb_and(n1138, n1139);
    let n1142: ZB = zb_and(n1138, n1140);
    let n1143: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1125);
    let n1144: ZB = zb_not(n1143);
    let n1145: ZB = zb_and(n1142, n1143);
    let n1146: ZB = zb_and(n1142, n1144);
    let n1147: ZB = zb_or(n1145, n1146);
    let n1148: ZB = zb_and(n534, n1143);
    let n1149: ZB = zb_not(n1148);
    let n1150: ZB = zb_and(n1147, n1148);
    let n1151: ZB = zb_and(n1147, n1149);
    let n1152: ZB = zb_or(n1150, n1151);
    let n1153: ZB = zb_and(n540, n1148);
    let n1154: ZB = zb_not(n1153);
    let n1155: ZB = zb_and(n1152, n1153);
    let n1156: ZB = zb_and(n1152, n1154);
    let n1157: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1125);
    let n1158: ZB = zb_not(n1157);
    let n1159: ZB = zb_and(n1156, n1157);
    let n1160: ZB = zb_and(n1156, n1158);
    let n1161: ZB = zb_or(n1159, n1160);
    let n1162: ZB = zb_and(n551, n1157);
    let n1163: ZB = zb_not(n1162);
    let n1164: ZB = zb_and(n1161, n1162);
    let n1165: ZB = zb_and(n1161, n1163);
    let n1166: ZB = zb_or(n1164, n1165);
    let n1167: ZB = zb_and(n557, n1162);
    let n1168: ZB = zb_not(n1167);
    let n1169: ZB = zb_and(n1166, n1167);
    let n1170: ZB = zb_and(n1166, n1168);
    let n1171: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1125);
    let n1172: ZB = zb_not(n1171);
    let n1173: ZB = zb_and(n1170, n1171);
    let n1174: ZB = zb_and(n1170, n1172);
    let n1175: ZB = zb_and(n569, n1173);
    let n1176: ZB = zb_and(n568, n1173);
    let n1177: ZB = zb_or(n1175, n1176);
    let n1178: ZB = zb_or(n1174, n1177);
    let n1179: ZB = zb_and(n1041, n1171);
    let n1180: ZB = zb_not(n1179);
    let n1181: ZB = zb_and(n1178, n1179);
    let n1182: ZB = zb_and(n1178, n1180);
    let n1183: ZB = zb_or(n1181, n1182);
    let n1184: ZB = zb_and(n582, n1179);
    let n1185: ZB = zb_not(n1184);
    let n1186: ZB = zb_and(n1183, n1184);
    let n1187: ZB = zb_and(n1183, n1185);
    let n1188: ZB = zb_or(n1169, n1186);
    let n1189: ZB = zb_or(n1155, n1188);
    let n1190: ZB = zb_or(n1141, n1189);
    let n1191: ZB = zb_and(n744, n976);
    let n1192: ZB = zb_or(n1124, n1187);
    let n1193: ZB = zsel_b(n669, n976, n1191);
    let n1194: ZB = zb_or(n1122, n1190);
    let n1195: ZB = zb_or(n1056, n1192);
    let n1196: ZB = zsel_b(n593, n976, n1193);
    let n1197: ZB = zb_or(n1054, n1194);
    let n1198: ZB = zb_or(n983, n1195);
    let n1199: ZB = zsel_b(n498, n976, n1196);
    let n1200: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n479);
    let n1201: ZB = zn_gt(n1200, n483);
    let n1202: ZB = zb_and(n1199, n1201);
    let n1203: ZB = zb_or(n974, n1197);
    let n1204: ZB = zsel_b(n974, n753, n976);
    let n1205: ZB = zb_or(n981, n1198);
    let n1206: ZB = zsel_b(n979, n976, n1202);
    let n1207: ZB = zb_or(n751, n1203);
    let n1208: ZB = zsel_b(n751, n474, n1204);
    let n1209: ZB = zb_or(n758, n1205);
    let n1210: ZB = zsel_b(n756, n753, n1206);
    let n1211: ZB = zb_or(n488, n1209);
    let n1212: ZB = zsel_b(n486, n474, n1210);
    let n1213: ZB = zn_gt(n471, zn_splat(P8::from_raw(8388608i32)));
    let n1214: ZB = zn_le(n471, zn_splat(P8::from_raw(8388608i32)));
    let n1215: ZB = zb_and(n1207, n1213);
    let n1216: ZB = zb_and(n1207, n1214);
    let n1217: ZB = zb_or(n1215, n1216);
    let n1218: ZB = zb_and(n1211, n1213);
    let n1219: ZB = zb_or(n1217, n1218);
    let n1220: ZB = zsel_b(n1217, n1208, n1212);
    let n1221: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n475);
    let n1222: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n476);
    let n1223: ZB = zn_tile_flag_at(g.cache, g.cart, n1221, n1222, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1224: ZB = zb_not(n1223);
    let n1225: ZB = zb_and(n1219, n1224);
    let n1226: ZB = zb_and(n1219, n1223);
    let n1227: ZB = zb_or(n1225, n1226);
    let n1228: ZB = zb_and(n1224, n1227);
    let n1229: ZB = zb_and(n1223, n1227);
    let n1230: ZB = zb_or(n1228, n1229);
    let n1231: ZB = zn_lt(n271, zn_splat(P8::from_raw(65536i32)));
    let n1232: ZB = zn_ge(n271, zn_splat(P8::from_raw(65536i32)));
    let n1233: ZN = zsel_n(n1231, zn_splat(P8::from_raw(65536i32)), n271);
    let n1234: ZB = zn_gt(r_c287, zn_splat(P8::from_raw(0i32)));
    let n1235: ZB = zn_le(r_c287, zn_splat(P8::from_raw(0i32)));
    let n1236: ZN = zn_sub(r_c287, zn_splat(P8::from_raw(65536i32)));
    let n1237: ZN = zsel_n(n1234, n1236, r_c287);
    let n1238: ZN = zsel_n(n1223, n1233, n271);
    let n1239: ZN = zsel_n(n1223, zn_splat(P8::from_raw(393216i32)), n1237);
    let n1240: ZB = zb_and(n1223, n1230);
    let n1241: ZB = zb_and(n1224, n1230);
    let n1242: ZB = zb_and(n1231, n1240);
    let n1243: ZB = zb_and(n1232, n1240);
    let n1244: ZB = zb_or(n1242, n1243);
    let n1245: ZB = zb_and(n1234, n1241);
    let n1246: ZB = zb_and(n1235, n1241);
    let n1247: ZB = zb_or(n1245, n1246);
    let n1248: ZB = zb_or(n1244, n1247);
    let n1249: ZB = zn_gt(r_c284, zn_splat(P8::from_raw(0i32)));
    let n1250: ZB = zn_le(r_c284, zn_splat(P8::from_raw(0i32)));
    let n1251: ZB = zn_gt(n472, r_c360);
    let n1252: ZB = zn_le(n472, r_c360);
    let n1253: ZB = zn_gt(n473, r_c361);
    let n1254: ZB = zn_le(n473, r_c361);
    let n1255: ZN = zsel_n(n1224, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n1256: ZN = zn_abs(n472);
    let n1257: ZB = zn_gt(n1256, zn_splat(P8::from_raw(65536i32)));
    let n1258: ZB = zn_le(n1256, zn_splat(P8::from_raw(65536i32)));
    let n1259: ZB = zn_gt(n472, zn_splat(P8::from_raw(0i32)));
    let n1260: ZB = zn_lt(n472, zn_splat(P8::from_raw(0i32)));
    let n1261: ZB = zn_gt(n472, zn_splat(P8::from_raw(65536i32)));
    let n1262: ZB = zn_le(n472, zn_splat(P8::from_raw(65536i32)));
    let n1263: ZN = zn_sub(n472, zn_splat(P8::from_raw(9830i32)));
    let n1264: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n1263);
    let n1265: ZN = zn_add(zn_splat(P8::from_raw(9830i32)), n472);
    let n1266: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n1265);
    let n1267: ZB = zn_gt(n472, zn_splat(P8::from_raw(-65536i32)));
    let n1268: ZB = zn_le(n472, zn_splat(P8::from_raw(-65536i32)));
    let n1269: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1263);
    let n1270: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n1265);
    let n1271: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1263);
    let n1272: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n1265);
    let n1273: ZN = zsel_n(n1267, n1269, n1270);
    let n1274: ZN = zsel_n(n1259, n1271, n1272);
    let n1275: ZN = zsel_n(n1261, n1264, n1266);
    let n1276: ZN = zsel_n(n1260, n1273, n1274);
    let n1277: ZN = zsel_n(n1259, n1275, n1276);
    let n1278: ZN = zn_sub(n472, n1255);
    let n1279: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1278);
    let n1280: ZN = zn_add(n472, n1255);
    let n1281: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n1280);
    let n1282: ZN = zsel_n(n1259, n1279, n1281);
    let n1283: ZN = zsel_n(n1257, n1277, n1282);
    let n1284: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1283);
    let n1285: ZB = zb_not(n1284);
    let n1286: ZB = zn_lt(n1283, zn_splat(P8::from_raw(0i32)));
    let n1287: ZB = zsel_b(n1285, n1286, r_c362);
    let n1288: ZN = zn_abs(n473);
    let n1289: ZB = zn_le(n1288, zn_splat(P8::from_raw(9830i32)));
    let n1290: ZB = zn_gt(n1288, zn_splat(P8::from_raw(9830i32)));
    let n1291: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n476);
    let n1292: ZB = zn_gt(n473, zn_splat(P8::from_raw(131072i32)));
    let n1293: ZB = zn_le(n473, zn_splat(P8::from_raw(131072i32)));
    let n1294: ZB = zn_gt(n1239, zn_splat(P8::from_raw(0i32)));
    let n1295: ZB = zn_le(n1239, zn_splat(P8::from_raw(0i32)));
    let n1296: ZN = zn_add(zn_splat(P8::from_raw(-196608i32)), n475);
    let n1297: ZB = zn_tile_flag_at(g.cache, g.cart, n1296, n1291, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1298: ZB = zb_not(n1297);
    let n1299: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n475);
    let n1300: ZB = zn_tile_flag_at(g.cache, g.cart, n1299, n1291, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1301: ZB = zb_not(n1300);
    let n1302: ZN = zsel_n(n1300, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n1303: ZN = zsel_n(n1297, zn_splat(P8::from_raw(-65536i32)), n1302);
    let n1304: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1303);
    let n1305: ZB = zb_not(n1304);
    let n1306: ZB = zn_gt(n1238, zn_splat(P8::from_raw(0i32)));
    let n1307: ZB = zn_le(n1238, zn_splat(P8::from_raw(0i32)));
    let n1308: ZB = zb_not(n1287);
    let n1309: ZN = zsel_n(n1287, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n1310: ZB = zn_gt(n1309, zn_splat(P8::from_raw(0i32)));
    let n1311: ZB = zn_le(n1309, zn_splat(P8::from_raw(0i32)));
    let n1312: ZB = zn_lt(n1309, zn_splat(P8::from_raw(0i32)));
    let n1313: ZB = zn_ge(n1309, zn_splat(P8::from_raw(0i32)));
    let n1314: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1309);
    let n1315: ZB = zb_not(n1314);
    let n1316: ZB = zb_and(n1248, n1249);
    let n1317: ZB = zb_and(n1248, n1250);
    let n1318: ZB = zb_and(n1251, n1316);
    let n1319: ZB = zb_and(n1252, n1316);
    let n1320: ZB = zb_or(n1318, n1319);
    let n1321: ZB = zb_and(n1253, n1320);
    let n1322: ZB = zb_and(n1254, n1320);
    let n1323: ZB = zb_or(n1321, n1322);
    let n1324: ZB = zb_and(n1224, n1317);
    let n1325: ZB = zb_and(n1223, n1317);
    let n1326: ZB = zb_or(n1324, n1325);
    let n1327: ZB = zb_and(n1257, n1326);
    let n1328: ZB = zb_and(n1258, n1326);
    let n1329: ZB = zb_and(n1259, n1327);
    let n1330: ZB = zb_and(n557, n1327);
    let n1331: ZB = zb_and(n1260, n1330);
    let n1332: ZB = zb_and(n582, n1330);
    let n1333: ZB = zb_and(n1261, n1329);
    let n1334: ZB = zb_and(n1262, n1329);
    let n1335: ZB = zb_and(n1267, n1331);
    let n1336: ZB = zb_and(n1268, n1331);
    let n1337: ZB = zb_and(n557, n1332);
    let n1338: ZB = zb_or(n1335, n1336);
    let n1339: ZB = zb_or(n1333, n1334);
    let n1340: ZB = zb_or(n1337, n1338);
    let n1341: ZB = zb_or(n1339, n1340);
    let n1342: ZB = zb_and(n1259, n1328);
    let n1343: ZB = zb_and(n557, n1328);
    let n1344: ZB = zb_or(n1342, n1343);
    let n1345: ZB = zb_or(n1341, n1344);
    let n1346: ZB = zb_and(n1285, n1345);
    let n1347: ZB = zb_and(n1284, n1345);
    let n1348: ZB = zb_or(n1346, n1347);
    let n1349: ZB = zb_and(n1289, n1348);
    let n1350: ZB = zb_and(n1290, n1348);
    let n1351: ZB = zb_or(n1349, n1350);
    let n1352: ZB = zb_and(n1224, n1351);
    let n1353: ZB = zb_and(n1223, n1351);
    let n1354: ZB = zb_and(n1292, n1352);
    let n1355: ZB = zb_and(n1293, n1352);
    let n1356: ZB = zb_or(n1354, n1355);
    let n1357: ZB = zb_or(n1353, n1356);
    let n1358: ZB = zb_and(n1306, n1357);
    let n1359: ZB = zb_and(n1307, n1357);
    let n1360: ZB = zb_or(n1358, n1359);
    let n1361: ZB = zb_or(n1323, n1360);
    let n1362: ZB = zn_lt(n471, zn_splat(P8::from_raw(-262144i32)));
    let n1363: ZB = zn_ge(n471, zn_splat(P8::from_raw(-262144i32)));
    let n1364: ZB = zb_and(n1361, n1362);
    let n1365: ZB = zb_and(n1361, n1363);
    let n1366: ZB = zb_or(n1364, n1365);
    let n1368: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n1379: ZN = zsel_n(n1213, n151, n150);
    let n1380: ZN = zsel_n(n1217, n1379, n150);
    let n1384: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1278);
    let n1385: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n1280);
    let n1386: ZN = zsel_n(n1267, n1384, n1385);
    let n1387: ZN = zsel_n(n1257, n1277, n1386);
    let n1388: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1387);
    let n1389: ZB = zb_not(n1388);
    let n1390: ZB = zn_lt(n1387, zn_splat(P8::from_raw(0i32)));
    let n1391: ZB = zsel_b(n1389, n1390, r_c362);
    let n1392: ZN = zn_add(zn_splat(P8::from_raw(-65536i32)), n475);
    let n1393: ZB = zn_tile_flag_at(g.cache, g.cart, n1392, n1291, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1394: ZB = zb_not(n1393);
    let n1395: ZN = zsel_n(n1393, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n1396: ZB = zn_gt(n473, n1395);
    let n1397: ZB = zn_le(n473, n1395);
    let n1398: ZB = zb_and(n1267, n1328);
    let n1399: ZB = zb_and(n1268, n1328);
    let n1400: ZB = zb_or(n1398, n1399);
    let n1401: ZB = zb_or(n1341, n1400);
    let n1402: ZB = zb_and(n1389, n1401);
    let n1403: ZB = zb_and(n1388, n1401);
    let n1404: ZB = zb_or(n1402, n1403);
    let n1405: ZB = zb_and(n1289, n1404);
    let n1406: ZB = zb_and(n1290, n1404);
    let n1407: ZB = zb_or(n1405, n1406);
    let n1408: ZB = zb_and(n1394, n1407);
    let n1409: ZB = zb_and(n1393, n1407);
    let n1410: ZB = zb_or(n1408, n1409);
    let n1411: ZB = zb_and(n1394, n1410);
    let n1412: ZB = zb_and(n1393, n1410);
    let n1413: ZB = zb_or(n1411, n1412);
    let n1414: ZB = zb_and(n1393, n1413);
    let n1415: ZB = zb_and(n1394, n1413);
    let n1416: ZB = zb_or(n1414, n1415);
    let n1417: ZB = zb_and(n1393, n1416);
    let n1418: ZB = zb_and(n1394, n1416);
    let n1419: ZB = zb_or(n1417, n1418);
    let n1420: ZB = zb_and(n1224, n1419);
    let n1421: ZB = zb_and(n1223, n1419);
    let n1422: ZB = zb_and(n1396, n1420);
    let n1423: ZB = zb_and(n1397, n1420);
    let n1424: ZB = zb_or(n1422, n1423);
    let n1425: ZB = zb_or(n1421, n1424);
    let n1426: ZB = zb_and(n1306, n1425);
    let n1427: ZB = zb_and(n1307, n1425);
    let n1428: ZB = zb_or(n1426, n1427);
    let n1429: ZB = zb_or(n1323, n1428);
    let n1430: ZB = zb_and(n1362, n1429);
    let n1431: ZB = zb_and(n1363, n1429);
    let n1432: ZB = zb_or(n1430, n1431);
    let n1435: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n1278);
    let n1436: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n1280);
    let n1437: ZN = zsel_n(n1261, n1435, n1436);
    let n1438: ZN = zsel_n(n1257, n1277, n1437);
    let n1439: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1438);
    let n1440: ZB = zb_not(n1439);
    let n1441: ZB = zn_lt(n1438, zn_splat(P8::from_raw(0i32)));
    let n1442: ZB = zsel_b(n1440, n1441, r_c362);
    let n1443: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n475);
    let n1444: ZB = zn_tile_flag_at(g.cache, g.cart, n1443, n1291, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1445: ZB = zb_not(n1444);
    let n1446: ZN = zsel_n(n1444, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n1447: ZB = zn_gt(n473, n1446);
    let n1448: ZB = zn_le(n473, n1446);
    let n1449: ZB = zb_and(n1261, n1328);
    let n1450: ZB = zb_and(n1262, n1328);
    let n1451: ZB = zb_or(n1449, n1450);
    let n1452: ZB = zb_or(n1341, n1451);
    let n1453: ZB = zb_and(n1440, n1452);
    let n1454: ZB = zb_and(n1439, n1452);
    let n1455: ZB = zb_or(n1453, n1454);
    let n1456: ZB = zb_and(n1289, n1455);
    let n1457: ZB = zb_and(n1290, n1455);
    let n1458: ZB = zb_or(n1456, n1457);
    let n1459: ZB = zb_and(n1445, n1458);
    let n1460: ZB = zb_and(n1444, n1458);
    let n1461: ZB = zb_or(n1459, n1460);
    let n1462: ZB = zb_and(n1445, n1461);
    let n1463: ZB = zb_and(n1444, n1461);
    let n1464: ZB = zb_or(n1462, n1463);
    let n1465: ZB = zb_and(n1444, n1464);
    let n1466: ZB = zb_and(n1445, n1464);
    let n1467: ZB = zb_or(n1465, n1466);
    let n1468: ZB = zb_and(n1444, n1467);
    let n1469: ZB = zb_and(n1445, n1467);
    let n1470: ZB = zb_or(n1468, n1469);
    let n1471: ZB = zb_and(n1224, n1470);
    let n1472: ZB = zb_and(n1223, n1470);
    let n1473: ZB = zb_and(n1447, n1471);
    let n1474: ZB = zb_and(n1448, n1471);
    let n1475: ZB = zb_or(n1473, n1474);
    let n1476: ZB = zb_or(n1472, n1475);
    let n1477: ZB = zb_and(n1306, n1476);
    let n1478: ZB = zb_and(n1307, n1476);
    let n1479: ZB = zb_or(n1477, n1478);
    let n1480: ZB = zb_or(n1323, n1479);
    let n1481: ZB = zb_and(n1362, n1480);
    let n1482: ZB = zb_and(n1363, n1480);
    let n1483: ZB = zb_or(n1481, n1482);
    let n1486: ZB = zb_and(n179, n1357);
    let n1487: ZB = zb_and(r_c295, n1357);
    let n1488: ZB = zb_and(n1294, n1486);
    let n1489: ZB = zb_and(n1295, n1486);
    let n1490: ZB = zb_and(n1298, n1489);
    let n1491: ZB = zb_and(n1297, n1489);
    let n1492: ZB = zb_or(n1490, n1491);
    let n1493: ZB = zb_and(n1298, n1492);
    let n1494: ZB = zb_and(n1297, n1492);
    let n1495: ZB = zb_or(n1493, n1494);
    let n1496: ZB = zb_and(n1297, n1495);
    let n1497: ZB = zb_and(n1298, n1495);
    let n1498: ZB = zb_and(n1301, n1497);
    let n1499: ZB = zb_and(n1300, n1497);
    let n1500: ZB = zb_or(n1498, n1499);
    let n1501: ZB = zb_and(n1301, n1500);
    let n1502: ZB = zb_and(n1300, n1500);
    let n1503: ZB = zb_or(n1501, n1502);
    let n1504: ZB = zb_and(n1300, n1503);
    let n1505: ZB = zb_and(n1301, n1503);
    let n1506: ZB = zb_or(n1504, n1505);
    let n1507: ZB = zb_or(n1496, n1506);
    let n1508: ZB = zb_and(n1305, n1507);
    let n1509: ZB = zb_and(n1304, n1507);
    let n1510: ZB = zb_or(n1508, n1509);
    let n1511: ZB = zb_or(n1488, n1510);
    let n1512: ZB = zb_or(n1487, n1511);
    let n1513: ZB = zb_and(n1306, n1512);
    let n1514: ZB = zb_and(n1307, n1512);
    let n1515: ZB = zb_or(n1513, n1514);
    let n1516: ZB = zb_or(n1323, n1515);
    let n1517: ZB = zb_and(n1362, n1516);
    let n1518: ZB = zb_and(n1363, n1516);
    let n1519: ZB = zb_or(n1517, n1518);
    let n1522: ZB = zb_and(n179, n1425);
    let n1523: ZB = zb_and(r_c295, n1425);
    let n1524: ZB = zb_and(n1294, n1522);
    let n1525: ZB = zb_and(n1295, n1522);
    let n1526: ZB = zb_and(n1298, n1525);
    let n1527: ZB = zb_and(n1297, n1525);
    let n1528: ZB = zb_or(n1526, n1527);
    let n1529: ZB = zb_and(n1298, n1528);
    let n1530: ZB = zb_and(n1297, n1528);
    let n1531: ZB = zb_or(n1529, n1530);
    let n1532: ZB = zb_and(n1297, n1531);
    let n1533: ZB = zb_and(n1298, n1531);
    let n1534: ZB = zb_and(n1301, n1533);
    let n1535: ZB = zb_and(n1300, n1533);
    let n1536: ZB = zb_or(n1534, n1535);
    let n1537: ZB = zb_and(n1301, n1536);
    let n1538: ZB = zb_and(n1300, n1536);
    let n1539: ZB = zb_or(n1537, n1538);
    let n1540: ZB = zb_and(n1300, n1539);
    let n1541: ZB = zb_and(n1301, n1539);
    let n1542: ZB = zb_or(n1540, n1541);
    let n1543: ZB = zb_or(n1532, n1542);
    let n1544: ZB = zb_and(n1305, n1543);
    let n1545: ZB = zb_and(n1304, n1543);
    let n1546: ZB = zb_or(n1544, n1545);
    let n1547: ZB = zb_or(n1524, n1546);
    let n1548: ZB = zb_or(n1523, n1547);
    let n1549: ZB = zb_and(n1306, n1548);
    let n1550: ZB = zb_and(n1307, n1548);
    let n1551: ZB = zb_or(n1549, n1550);
    let n1552: ZB = zb_or(n1323, n1551);
    let n1553: ZB = zb_and(n1362, n1552);
    let n1554: ZB = zb_and(n1363, n1552);
    let n1555: ZB = zb_or(n1553, n1554);
    let n1558: ZB = zb_and(n179, n1476);
    let n1559: ZB = zb_and(r_c295, n1476);
    let n1560: ZB = zb_and(n1294, n1558);
    let n1561: ZB = zb_and(n1295, n1558);
    let n1562: ZB = zb_and(n1298, n1561);
    let n1563: ZB = zb_and(n1297, n1561);
    let n1564: ZB = zb_or(n1562, n1563);
    let n1565: ZB = zb_and(n1298, n1564);
    let n1566: ZB = zb_and(n1297, n1564);
    let n1567: ZB = zb_or(n1565, n1566);
    let n1568: ZB = zb_and(n1297, n1567);
    let n1569: ZB = zb_and(n1298, n1567);
    let n1570: ZB = zb_and(n1301, n1569);
    let n1571: ZB = zb_and(n1300, n1569);
    let n1572: ZB = zb_or(n1570, n1571);
    let n1573: ZB = zb_and(n1301, n1572);
    let n1574: ZB = zb_and(n1300, n1572);
    let n1575: ZB = zb_or(n1573, n1574);
    let n1576: ZB = zb_and(n1300, n1575);
    let n1577: ZB = zb_and(n1301, n1575);
    let n1578: ZB = zb_or(n1576, n1577);
    let n1579: ZB = zb_or(n1568, n1578);
    let n1580: ZB = zb_and(n1305, n1579);
    let n1581: ZB = zb_and(n1304, n1579);
    let n1582: ZB = zb_or(n1580, n1581);
    let n1583: ZB = zb_or(n1560, n1582);
    let n1584: ZB = zb_or(n1559, n1583);
    let n1585: ZB = zb_and(n1306, n1584);
    let n1586: ZB = zb_and(n1307, n1584);
    let n1587: ZB = zb_or(n1585, n1586);
    let n1588: ZB = zb_or(n1323, n1587);
    let n1589: ZB = zb_and(n1362, n1588);
    let n1590: ZB = zb_and(n1363, n1588);
    let n1591: ZB = zb_or(n1589, n1590);
    let n1594: ZB = zb_and(n128, n1306);
    let n1595: ZB = zb_not(n1594);
    let n1596: ZN = zsel_n(n1594, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n1597: ZB = zb_or(r_c41, n1594);
    let n1598: ZN = zsel_n(n1249, r_c20, n1596);
    let n1599: ZB = zsel_b(n1249, r_c41, n1597);
    let n1600: ZB = zb_and(n1360, n1594);
    let n1601: ZB = zb_and(n1360, n1595);
    let n1602: ZB = zb_and(n1287, n1600);
    let n1603: ZB = zb_and(n1308, n1600);
    let n1604: ZB = zb_or(n1602, n1603);
    let n1605: ZB = zb_and(n1310, n1604);
    let n1606: ZB = zb_and(n1311, n1604);
    let n1607: ZB = zb_and(n1312, n1606);
    let n1608: ZB = zb_and(n1313, n1606);
    let n1609: ZB = zb_or(n1607, n1608);
    let n1610: ZB = zb_or(n1605, n1609);
    let n1611: ZB = zb_and(n1315, n1610);
    let n1612: ZB = zb_and(n1314, n1610);
    let n1613: ZB = zb_or(n1611, n1612);
    let n1614: ZB = zb_or(n1601, n1613);
    let n1615: ZB = zb_or(n1323, n1614);
    let n1616: ZB = zb_and(n1362, n1615);
    let n1617: ZB = zb_and(n1363, n1615);
    let n1618: ZB = zb_or(n1616, n1617);
    let n1619: ZB = zb_and(n1363, n1618);
    let n1620: ZB = zn_gt(n1598, zn_splat(P8::from_raw(0i32)));
    let n1621: ZB = zn_le(n1598, zn_splat(P8::from_raw(0i32)));
    let n1622: ZB = zb_and(n1619, n1620);
    let n1623: ZB = zb_and(n1619, n1621);
    let n1624: ZB = zb_or(n1622, n1623);
    let n1625: ZB = zb_and(n1428, n1594);
    let n1626: ZB = zb_and(n1428, n1595);
    let n1627: ZB = zb_or(n1625, n1626);
    let n1628: ZB = zb_or(n1323, n1627);
    let n1629: ZB = zb_and(n1362, n1628);
    let n1630: ZB = zb_and(n1363, n1628);
    let n1631: ZB = zb_or(n1629, n1630);
    let n1632: ZB = zb_and(n1363, n1631);
    let n1633: ZB = zb_and(n1620, n1632);
    let n1634: ZB = zb_and(n1621, n1632);
    let n1635: ZB = zb_or(n1633, n1634);
    let n1636: ZB = zb_and(n1479, n1594);
    let n1637: ZB = zb_and(n1479, n1595);
    let n1638: ZB = zb_or(n1636, n1637);
    let n1639: ZB = zb_or(n1323, n1638);
    let n1640: ZB = zb_and(n1362, n1639);
    let n1641: ZB = zb_and(n1363, n1639);
    let n1642: ZB = zb_or(n1640, n1641);
    let n1643: ZB = zb_and(n1363, n1642);
    let n1644: ZB = zb_and(n1620, n1643);
    let n1645: ZB = zb_and(n1621, n1643);
    let n1646: ZB = zb_or(n1644, n1645);
    let n1647: ZB = zb_or(n1600, n1601);
    let n1648: ZB = zb_or(n1323, n1647);
    let n1649: ZB = zb_and(n1362, n1648);
    let n1650: ZB = zb_and(n1363, n1648);
    let n1651: ZB = zb_or(n1649, n1650);
    let n1652: ZB = zb_and(n1363, n1651);
    let n1653: ZB = zb_and(n1620, n1652);
    let n1654: ZB = zb_and(n1621, n1652);
    let n1655: ZB = zb_or(n1653, n1654);
    let n1656: ZB = zb_and(n1515, n1594);
    let n1657: ZB = zb_and(n1515, n1595);
    let n1658: ZB = zb_and(n1287, n1656);
    let n1659: ZB = zb_and(n1308, n1656);
    let n1660: ZB = zb_or(n1658, n1659);
    let n1661: ZB = zb_and(n1310, n1660);
    let n1662: ZB = zb_and(n1311, n1660);
    let n1663: ZB = zb_and(n1312, n1662);
    let n1664: ZB = zb_and(n1313, n1662);
    let n1665: ZB = zb_or(n1663, n1664);
    let n1666: ZB = zb_or(n1661, n1665);
    let n1667: ZB = zb_and(n1315, n1666);
    let n1668: ZB = zb_and(n1314, n1666);
    let n1669: ZB = zb_or(n1667, n1668);
    let n1670: ZB = zb_or(n1657, n1669);
    let n1671: ZB = zb_or(n1323, n1670);
    let n1672: ZB = zb_and(n1362, n1671);
    let n1673: ZB = zb_and(n1363, n1671);
    let n1674: ZB = zb_or(n1672, n1673);
    let n1675: ZB = zb_and(n1363, n1674);
    let n1676: ZB = zb_and(n1620, n1675);
    let n1677: ZB = zb_and(n1621, n1675);
    let n1678: ZB = zb_or(n1676, n1677);
    let n1679: ZB = zb_and(n1551, n1594);
    let n1680: ZB = zb_and(n1551, n1595);
    let n1681: ZB = zb_or(n1679, n1680);
    let n1682: ZB = zb_or(n1323, n1681);
    let n1683: ZB = zb_and(n1362, n1682);
    let n1684: ZB = zb_and(n1363, n1682);
    let n1685: ZB = zb_or(n1683, n1684);
    let n1686: ZB = zb_and(n1363, n1685);
    let n1687: ZB = zb_and(n1620, n1686);
    let n1688: ZB = zb_and(n1621, n1686);
    let n1689: ZB = zb_or(n1687, n1688);
    let n1690: ZB = zb_and(n1587, n1594);
    let n1691: ZB = zb_and(n1587, n1595);
    let n1692: ZB = zb_or(n1690, n1691);
    let n1693: ZB = zb_or(n1323, n1692);
    let n1694: ZB = zb_and(n1362, n1693);
    let n1695: ZB = zb_and(n1363, n1693);
    let n1696: ZB = zb_or(n1694, n1695);
    let n1697: ZB = zb_and(n1363, n1696);
    let n1698: ZB = zb_and(n1620, n1697);
    let n1699: ZB = zb_and(n1621, n1697);
    let n1700: ZB = zb_or(n1698, n1699);
    let n1701: ZB = zb_or(n1656, n1657);
    let n1702: ZB = zb_or(n1323, n1701);
    let n1703: ZB = zb_and(n1362, n1702);
    let n1704: ZB = zb_and(n1363, n1702);
    let n1705: ZB = zb_or(n1703, n1704);
    let n1706: ZB = zb_and(n1363, n1705);
    let n1707: ZB = zb_and(n1620, n1706);
    let n1708: ZB = zb_and(n1621, n1706);
    let n1709: ZB = zb_or(n1707, n1708);
    let n1714: ZB = zb_and(n1211, n1214);
    let n1715: ZB = zb_and(n1224, n1714);
    let n1716: ZB = zb_and(n1223, n1714);
    let n1717: ZB = zb_or(n1715, n1716);
    let n1718: ZB = zb_and(n1224, n1717);
    let n1719: ZB = zb_and(n1223, n1717);
    let n1720: ZB = zb_or(n1718, n1719);
    let n1721: ZB = zb_and(n1223, n1720);
    let n1722: ZB = zb_and(n1224, n1720);
    let n1723: ZB = zb_and(n1231, n1721);
    let n1724: ZB = zb_and(n1232, n1721);
    let n1725: ZB = zb_or(n1723, n1724);
    let n1726: ZB = zb_and(n1234, n1722);
    let n1727: ZB = zb_and(n1235, n1722);
    let n1728: ZB = zb_or(n1726, n1727);
    let n1729: ZB = zb_or(n1725, n1728);
    let n1730: ZB = zb_and(n1249, n1729);
    let n1731: ZB = zb_and(n1250, n1729);
    let n1732: ZB = zb_and(n1251, n1730);
    let n1733: ZB = zb_and(n1252, n1730);
    let n1734: ZB = zb_or(n1732, n1733);
    let n1735: ZB = zb_and(n1253, n1734);
    let n1736: ZB = zb_and(n1254, n1734);
    let n1737: ZB = zb_or(n1735, n1736);
    let n1738: ZB = zb_and(n1224, n1731);
    let n1739: ZB = zb_and(n1223, n1731);
    let n1740: ZB = zb_or(n1738, n1739);
    let n1741: ZB = zb_and(n1257, n1740);
    let n1742: ZB = zb_and(n1258, n1740);
    let n1743: ZB = zb_and(n1259, n1741);
    let n1744: ZB = zb_and(n557, n1741);
    let n1745: ZB = zb_and(n1260, n1744);
    let n1746: ZB = zb_and(n582, n1744);
    let n1747: ZB = zb_and(n1261, n1743);
    let n1748: ZB = zb_and(n1262, n1743);
    let n1749: ZB = zb_and(n1267, n1745);
    let n1750: ZB = zb_and(n1268, n1745);
    let n1751: ZB = zb_and(n557, n1746);
    let n1752: ZB = zb_or(n1749, n1750);
    let n1753: ZB = zb_or(n1747, n1748);
    let n1754: ZB = zb_or(n1751, n1752);
    let n1755: ZB = zb_or(n1753, n1754);
    let n1756: ZB = zb_and(n1259, n1742);
    let n1757: ZB = zb_and(n557, n1742);
    let n1758: ZB = zb_or(n1756, n1757);
    let n1759: ZB = zb_or(n1755, n1758);
    let n1760: ZB = zb_and(n1285, n1759);
    let n1761: ZB = zb_and(n1284, n1759);
    let n1762: ZB = zb_or(n1760, n1761);
    let n1763: ZB = zb_and(n1289, n1762);
    let n1764: ZB = zb_and(n1290, n1762);
    let n1765: ZB = zb_or(n1763, n1764);
    let n1766: ZB = zb_and(n1224, n1765);
    let n1767: ZB = zb_and(n1223, n1765);
    let n1768: ZB = zb_and(n1292, n1766);
    let n1769: ZB = zb_and(n1293, n1766);
    let n1770: ZB = zb_or(n1768, n1769);
    let n1771: ZB = zb_or(n1767, n1770);
    let n1772: ZB = zb_and(n1306, n1771);
    let n1773: ZB = zb_and(n1307, n1771);
    let n1774: ZB = zb_or(n1772, n1773);
    let n1775: ZB = zb_or(n1737, n1774);
    let n1776: ZB = zb_and(n1362, n1775);
    let n1777: ZB = zb_and(n1363, n1775);
    let n1778: ZB = zb_or(n1776, n1777);
    let n1779: ZB = zb_and(n1362, n1778);
    let n1780: ZB = zb_and(n1362, n1366);
    let n1781: ZB = zb_not(n1779);
    let n1782: ZB = zb_or(n1779, n1780);
    let n1783: ZB = zsel_b(n1779, n1212, n1220);
    let n1785: ZN = zsel_n(n1779, r_c87, n1380);
    let n1786: ZN = zsel_n(n1779, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1788: ZB = zb_and(n1267, n1742);
    let n1789: ZB = zb_and(n1268, n1742);
    let n1790: ZB = zb_or(n1788, n1789);
    let n1791: ZB = zb_or(n1755, n1790);
    let n1792: ZB = zb_and(n1389, n1791);
    let n1793: ZB = zb_and(n1388, n1791);
    let n1794: ZB = zb_or(n1792, n1793);
    let n1795: ZB = zb_and(n1289, n1794);
    let n1796: ZB = zb_and(n1290, n1794);
    let n1797: ZB = zb_or(n1795, n1796);
    let n1798: ZB = zb_and(n1394, n1797);
    let n1799: ZB = zb_and(n1393, n1797);
    let n1800: ZB = zb_or(n1798, n1799);
    let n1801: ZB = zb_and(n1394, n1800);
    let n1802: ZB = zb_and(n1393, n1800);
    let n1803: ZB = zb_or(n1801, n1802);
    let n1804: ZB = zb_and(n1393, n1803);
    let n1805: ZB = zb_and(n1394, n1803);
    let n1806: ZB = zb_or(n1804, n1805);
    let n1807: ZB = zb_and(n1393, n1806);
    let n1808: ZB = zb_and(n1394, n1806);
    let n1809: ZB = zb_or(n1807, n1808);
    let n1810: ZB = zb_and(n1224, n1809);
    let n1811: ZB = zb_and(n1223, n1809);
    let n1812: ZB = zb_and(n1396, n1810);
    let n1813: ZB = zb_and(n1397, n1810);
    let n1814: ZB = zb_or(n1812, n1813);
    let n1815: ZB = zb_or(n1811, n1814);
    let n1816: ZB = zb_and(n1306, n1815);
    let n1817: ZB = zb_and(n1307, n1815);
    let n1818: ZB = zb_or(n1816, n1817);
    let n1819: ZB = zb_or(n1737, n1818);
    let n1820: ZB = zb_and(n1362, n1819);
    let n1821: ZB = zb_and(n1363, n1819);
    let n1822: ZB = zb_or(n1820, n1821);
    let n1823: ZB = zb_and(n1362, n1822);
    let n1824: ZB = zb_and(n1362, n1432);
    let n1825: ZB = zb_not(n1823);
    let n1826: ZB = zb_or(n1823, n1824);
    let n1827: ZB = zsel_b(n1823, n1212, n1220);
    let n1829: ZN = zsel_n(n1823, r_c87, n1380);
    let n1830: ZN = zsel_n(n1823, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1832: ZB = zb_and(n1261, n1742);
    let n1833: ZB = zb_and(n1262, n1742);
    let n1834: ZB = zb_or(n1832, n1833);
    let n1835: ZB = zb_or(n1755, n1834);
    let n1836: ZB = zb_and(n1440, n1835);
    let n1837: ZB = zb_and(n1439, n1835);
    let n1838: ZB = zb_or(n1836, n1837);
    let n1839: ZB = zb_and(n1289, n1838);
    let n1840: ZB = zb_and(n1290, n1838);
    let n1841: ZB = zb_or(n1839, n1840);
    let n1842: ZB = zb_and(n1445, n1841);
    let n1843: ZB = zb_and(n1444, n1841);
    let n1844: ZB = zb_or(n1842, n1843);
    let n1845: ZB = zb_and(n1445, n1844);
    let n1846: ZB = zb_and(n1444, n1844);
    let n1847: ZB = zb_or(n1845, n1846);
    let n1848: ZB = zb_and(n1444, n1847);
    let n1849: ZB = zb_and(n1445, n1847);
    let n1850: ZB = zb_or(n1848, n1849);
    let n1851: ZB = zb_and(n1444, n1850);
    let n1852: ZB = zb_and(n1445, n1850);
    let n1853: ZB = zb_or(n1851, n1852);
    let n1854: ZB = zb_and(n1224, n1853);
    let n1855: ZB = zb_and(n1223, n1853);
    let n1856: ZB = zb_and(n1447, n1854);
    let n1857: ZB = zb_and(n1448, n1854);
    let n1858: ZB = zb_or(n1856, n1857);
    let n1859: ZB = zb_or(n1855, n1858);
    let n1860: ZB = zb_and(n1306, n1859);
    let n1861: ZB = zb_and(n1307, n1859);
    let n1862: ZB = zb_or(n1860, n1861);
    let n1863: ZB = zb_or(n1737, n1862);
    let n1864: ZB = zb_and(n1362, n1863);
    let n1865: ZB = zb_and(n1363, n1863);
    let n1866: ZB = zb_or(n1864, n1865);
    let n1867: ZB = zb_and(n1362, n1866);
    let n1868: ZB = zb_and(n1362, n1483);
    let n1869: ZB = zb_not(n1867);
    let n1870: ZB = zb_or(n1867, n1868);
    let n1871: ZB = zsel_b(n1867, n1212, n1220);
    let n1873: ZN = zsel_n(n1867, r_c87, n1380);
    let n1874: ZN = zsel_n(n1867, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1876: ZB = zb_and(n179, n1771);
    let n1877: ZB = zb_and(r_c295, n1771);
    let n1878: ZB = zb_and(n1294, n1876);
    let n1879: ZB = zb_and(n1295, n1876);
    let n1880: ZB = zb_and(n1298, n1879);
    let n1881: ZB = zb_and(n1297, n1879);
    let n1882: ZB = zb_or(n1880, n1881);
    let n1883: ZB = zb_and(n1298, n1882);
    let n1884: ZB = zb_and(n1297, n1882);
    let n1885: ZB = zb_or(n1883, n1884);
    let n1886: ZB = zb_and(n1297, n1885);
    let n1887: ZB = zb_and(n1298, n1885);
    let n1888: ZB = zb_and(n1301, n1887);
    let n1889: ZB = zb_and(n1300, n1887);
    let n1890: ZB = zb_or(n1888, n1889);
    let n1891: ZB = zb_and(n1301, n1890);
    let n1892: ZB = zb_and(n1300, n1890);
    let n1893: ZB = zb_or(n1891, n1892);
    let n1894: ZB = zb_and(n1300, n1893);
    let n1895: ZB = zb_and(n1301, n1893);
    let n1896: ZB = zb_or(n1894, n1895);
    let n1897: ZB = zb_or(n1886, n1896);
    let n1898: ZB = zb_and(n1305, n1897);
    let n1899: ZB = zb_and(n1304, n1897);
    let n1900: ZB = zb_or(n1898, n1899);
    let n1901: ZB = zb_or(n1878, n1900);
    let n1902: ZB = zb_or(n1877, n1901);
    let n1903: ZB = zb_and(n1306, n1902);
    let n1904: ZB = zb_and(n1307, n1902);
    let n1905: ZB = zb_or(n1903, n1904);
    let n1906: ZB = zb_or(n1737, n1905);
    let n1907: ZB = zb_and(n1362, n1906);
    let n1908: ZB = zb_and(n1363, n1906);
    let n1909: ZB = zb_or(n1907, n1908);
    let n1910: ZB = zb_and(n1362, n1909);
    let n1911: ZB = zb_and(n1362, n1519);
    let n1912: ZB = zb_not(n1910);
    let n1913: ZB = zb_or(n1910, n1911);
    let n1914: ZB = zsel_b(n1910, n1212, n1220);
    let n1916: ZN = zsel_n(n1910, r_c87, n1380);
    let n1917: ZN = zsel_n(n1910, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1919: ZB = zb_and(n179, n1815);
    let n1920: ZB = zb_and(r_c295, n1815);
    let n1921: ZB = zb_and(n1294, n1919);
    let n1922: ZB = zb_and(n1295, n1919);
    let n1923: ZB = zb_and(n1298, n1922);
    let n1924: ZB = zb_and(n1297, n1922);
    let n1925: ZB = zb_or(n1923, n1924);
    let n1926: ZB = zb_and(n1298, n1925);
    let n1927: ZB = zb_and(n1297, n1925);
    let n1928: ZB = zb_or(n1926, n1927);
    let n1929: ZB = zb_and(n1297, n1928);
    let n1930: ZB = zb_and(n1298, n1928);
    let n1931: ZB = zb_and(n1301, n1930);
    let n1932: ZB = zb_and(n1300, n1930);
    let n1933: ZB = zb_or(n1931, n1932);
    let n1934: ZB = zb_and(n1301, n1933);
    let n1935: ZB = zb_and(n1300, n1933);
    let n1936: ZB = zb_or(n1934, n1935);
    let n1937: ZB = zb_and(n1300, n1936);
    let n1938: ZB = zb_and(n1301, n1936);
    let n1939: ZB = zb_or(n1937, n1938);
    let n1940: ZB = zb_or(n1929, n1939);
    let n1941: ZB = zb_and(n1305, n1940);
    let n1942: ZB = zb_and(n1304, n1940);
    let n1943: ZB = zb_or(n1941, n1942);
    let n1944: ZB = zb_or(n1921, n1943);
    let n1945: ZB = zb_or(n1920, n1944);
    let n1946: ZB = zb_and(n1306, n1945);
    let n1947: ZB = zb_and(n1307, n1945);
    let n1948: ZB = zb_or(n1946, n1947);
    let n1949: ZB = zb_or(n1737, n1948);
    let n1950: ZB = zb_and(n1362, n1949);
    let n1951: ZB = zb_and(n1363, n1949);
    let n1952: ZB = zb_or(n1950, n1951);
    let n1953: ZB = zb_and(n1362, n1952);
    let n1954: ZB = zb_and(n1362, n1555);
    let n1955: ZB = zb_not(n1953);
    let n1956: ZB = zb_or(n1953, n1954);
    let n1957: ZB = zsel_b(n1953, n1212, n1220);
    let n1959: ZN = zsel_n(n1953, r_c87, n1380);
    let n1960: ZN = zsel_n(n1953, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1962: ZB = zb_and(n179, n1859);
    let n1963: ZB = zb_and(r_c295, n1859);
    let n1964: ZB = zb_and(n1294, n1962);
    let n1965: ZB = zb_and(n1295, n1962);
    let n1966: ZB = zb_and(n1298, n1965);
    let n1967: ZB = zb_and(n1297, n1965);
    let n1968: ZB = zb_or(n1966, n1967);
    let n1969: ZB = zb_and(n1298, n1968);
    let n1970: ZB = zb_and(n1297, n1968);
    let n1971: ZB = zb_or(n1969, n1970);
    let n1972: ZB = zb_and(n1297, n1971);
    let n1973: ZB = zb_and(n1298, n1971);
    let n1974: ZB = zb_and(n1301, n1973);
    let n1975: ZB = zb_and(n1300, n1973);
    let n1976: ZB = zb_or(n1974, n1975);
    let n1977: ZB = zb_and(n1301, n1976);
    let n1978: ZB = zb_and(n1300, n1976);
    let n1979: ZB = zb_or(n1977, n1978);
    let n1980: ZB = zb_and(n1300, n1979);
    let n1981: ZB = zb_and(n1301, n1979);
    let n1982: ZB = zb_or(n1980, n1981);
    let n1983: ZB = zb_or(n1972, n1982);
    let n1984: ZB = zb_and(n1305, n1983);
    let n1985: ZB = zb_and(n1304, n1983);
    let n1986: ZB = zb_or(n1984, n1985);
    let n1987: ZB = zb_or(n1964, n1986);
    let n1988: ZB = zb_or(n1963, n1987);
    let n1989: ZB = zb_and(n1306, n1988);
    let n1990: ZB = zb_and(n1307, n1988);
    let n1991: ZB = zb_or(n1989, n1990);
    let n1992: ZB = zb_or(n1737, n1991);
    let n1993: ZB = zb_and(n1362, n1992);
    let n1994: ZB = zb_and(n1363, n1992);
    let n1995: ZB = zb_or(n1993, n1994);
    let n1996: ZB = zb_and(n1362, n1995);
    let n1997: ZB = zb_and(n1362, n1591);
    let n1998: ZB = zb_not(n1996);
    let n1999: ZB = zb_or(n1996, n1997);
    let n2000: ZB = zsel_b(n1996, n1212, n1220);
    let n2002: ZN = zsel_n(n1996, r_c87, n1380);
    let n2003: ZN = zsel_n(n1996, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n2005: ZB = zb_and(n1594, n1774);
    let n2006: ZB = zb_and(n1595, n1774);
    let n2007: ZB = zb_and(n1287, n2005);
    let n2008: ZB = zb_and(n1308, n2005);
    let n2009: ZB = zb_or(n2007, n2008);
    let n2010: ZB = zb_and(n1310, n2009);
    let n2011: ZB = zb_and(n1311, n2009);
    let n2012: ZB = zb_and(n1312, n2011);
    let n2013: ZB = zb_and(n1313, n2011);
    let n2014: ZB = zb_or(n2012, n2013);
    let n2015: ZB = zb_or(n2010, n2014);
    let n2016: ZB = zb_and(n1315, n2015);
    let n2017: ZB = zb_and(n1314, n2015);
    let n2018: ZB = zb_or(n2016, n2017);
    let n2019: ZB = zb_or(n2006, n2018);
    let n2020: ZB = zb_or(n1737, n2019);
    let n2021: ZB = zb_and(n1362, n2020);
    let n2022: ZB = zb_and(n1363, n2020);
    let n2023: ZB = zb_or(n2021, n2022);
    let n2024: ZB = zb_and(n1362, n2023);
    let n2025: ZB = zb_and(n1362, n1618);
    let n2026: ZB = zb_not(n2024);
    let n2027: ZB = zb_or(n2024, n2025);
    let n2028: ZB = zsel_b(n2024, n1212, n1220);
    let n2029: ZB = zb_and(n1620, n2027);
    let n2030: ZB = zb_and(n1621, n2027);
    let n2031: ZB = zb_or(n2029, n2030);
    let n2032: ZN = zsel_n(n2024, r_c87, n1380);
    let n2033: ZN = zsel_n(n2024, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n2035: ZB = zb_and(n1594, n1818);
    let n2036: ZB = zb_and(n1595, n1818);
    let n2037: ZB = zb_or(n2035, n2036);
    let n2038: ZB = zb_or(n1737, n2037);
    let n2039: ZB = zb_and(n1362, n2038);
    let n2040: ZB = zb_and(n1363, n2038);
    let n2041: ZB = zb_or(n2039, n2040);
    let n2042: ZB = zb_and(n1362, n2041);
    let n2043: ZB = zb_and(n1362, n1631);
    let n2044: ZB = zb_not(n2042);
    let n2045: ZB = zb_or(n2042, n2043);
    let n2046: ZB = zsel_b(n2042, n1212, n1220);
    let n2047: ZB = zb_and(n1620, n2045);
    let n2048: ZB = zb_and(n1621, n2045);
    let n2049: ZB = zb_or(n2047, n2048);
    let n2050: ZN = zsel_n(n2042, r_c87, n1380);
    let n2051: ZN = zsel_n(n2042, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n2053: ZB = zb_and(n1594, n1862);
    let n2054: ZB = zb_and(n1595, n1862);
    let n2055: ZB = zb_or(n2053, n2054);
    let n2056: ZB = zb_or(n1737, n2055);
    let n2057: ZB = zb_and(n1362, n2056);
    let n2058: ZB = zb_and(n1363, n2056);
    let n2059: ZB = zb_or(n2057, n2058);
    let n2060: ZB = zb_and(n1362, n2059);
    let n2061: ZB = zb_and(n1362, n1642);
    let n2062: ZB = zb_not(n2060);
    let n2063: ZB = zb_or(n2060, n2061);
    let n2064: ZB = zsel_b(n2060, n1212, n1220);
    let n2065: ZB = zb_and(n1620, n2063);
    let n2066: ZB = zb_and(n1621, n2063);
    let n2067: ZB = zb_or(n2065, n2066);
    let n2068: ZN = zsel_n(n2060, r_c87, n1380);
    let n2069: ZN = zsel_n(n2060, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n2071: ZB = zb_or(n2005, n2006);
    let n2072: ZB = zb_or(n1737, n2071);
    let n2073: ZB = zb_and(n1362, n2072);
    let n2074: ZB = zb_and(n1363, n2072);
    let n2075: ZB = zb_or(n2073, n2074);
    let n2076: ZB = zb_and(n1362, n2075);
    let n2077: ZB = zb_and(n1362, n1651);
    let n2078: ZB = zb_not(n2076);
    let n2079: ZB = zb_or(n2076, n2077);
    let n2080: ZB = zsel_b(n2076, n1212, n1220);
    let n2081: ZB = zb_and(n1620, n2079);
    let n2082: ZB = zb_and(n1621, n2079);
    let n2083: ZB = zb_or(n2081, n2082);
    let n2084: ZN = zsel_n(n2076, r_c87, n1380);
    let n2085: ZN = zsel_n(n2076, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n2087: ZB = zb_and(n1594, n1905);
    let n2088: ZB = zb_and(n1595, n1905);
    let n2089: ZB = zb_and(n1287, n2087);
    let n2090: ZB = zb_and(n1308, n2087);
    let n2091: ZB = zb_or(n2089, n2090);
    let n2092: ZB = zb_and(n1310, n2091);
    let n2093: ZB = zb_and(n1311, n2091);
    let n2094: ZB = zb_and(n1312, n2093);
    let n2095: ZB = zb_and(n1313, n2093);
    let n2096: ZB = zb_or(n2094, n2095);
    let n2097: ZB = zb_or(n2092, n2096);
    let n2098: ZB = zb_and(n1315, n2097);
    let n2099: ZB = zb_and(n1314, n2097);
    let n2100: ZB = zb_or(n2098, n2099);
    let n2101: ZB = zb_or(n2088, n2100);
    let n2102: ZB = zb_or(n1737, n2101);
    let n2103: ZB = zb_and(n1362, n2102);
    let n2104: ZB = zb_and(n1363, n2102);
    let n2105: ZB = zb_or(n2103, n2104);
    let n2106: ZB = zb_and(n1362, n2105);
    let n2107: ZB = zb_and(n1362, n1674);
    let n2108: ZB = zb_not(n2106);
    let n2109: ZB = zb_or(n2106, n2107);
    let n2110: ZB = zsel_b(n2106, n1212, n1220);
    let n2111: ZB = zb_and(n1620, n2109);
    let n2112: ZB = zb_and(n1621, n2109);
    let n2113: ZB = zb_or(n2111, n2112);
    let n2114: ZN = zsel_n(n2106, r_c87, n1380);
    let n2115: ZN = zsel_n(n2106, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n2117: ZB = zb_and(n1594, n1948);
    let n2118: ZB = zb_and(n1595, n1948);
    let n2119: ZB = zb_or(n2117, n2118);
    let n2120: ZB = zb_or(n1737, n2119);
    let n2121: ZB = zb_and(n1362, n2120);
    let n2122: ZB = zb_and(n1363, n2120);
    let n2123: ZB = zb_or(n2121, n2122);
    let n2124: ZB = zb_and(n1362, n2123);
    let n2125: ZB = zb_and(n1362, n1685);
    let n2126: ZB = zb_not(n2124);
    let n2127: ZB = zb_or(n2124, n2125);
    let n2128: ZB = zsel_b(n2124, n1212, n1220);
    let n2129: ZB = zb_and(n1620, n2127);
    let n2130: ZB = zb_and(n1621, n2127);
    let n2131: ZB = zb_or(n2129, n2130);
    let n2132: ZN = zsel_n(n2124, r_c87, n1380);
    let n2133: ZN = zsel_n(n2124, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n2135: ZB = zb_and(n1594, n1991);
    let n2136: ZB = zb_and(n1595, n1991);
    let n2137: ZB = zb_or(n2135, n2136);
    let n2138: ZB = zb_or(n1737, n2137);
    let n2139: ZB = zb_and(n1362, n2138);
    let n2140: ZB = zb_and(n1363, n2138);
    let n2141: ZB = zb_or(n2139, n2140);
    let n2142: ZB = zb_and(n1362, n2141);
    let n2143: ZB = zb_and(n1362, n1696);
    let n2144: ZB = zb_not(n2142);
    let n2145: ZB = zb_or(n2142, n2143);
    let n2146: ZB = zsel_b(n2142, n1212, n1220);
    let n2147: ZB = zb_and(n1620, n2145);
    let n2148: ZB = zb_and(n1621, n2145);
    let n2149: ZB = zb_or(n2147, n2148);
    let n2150: ZN = zsel_n(n2142, r_c87, n1380);
    let n2151: ZN = zsel_n(n2142, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n2153: ZB = zb_or(n2087, n2088);
    let n2154: ZB = zb_or(n1737, n2153);
    let n2155: ZB = zb_and(n1362, n2154);
    let n2156: ZB = zb_and(n1363, n2154);
    let n2157: ZB = zb_or(n2155, n2156);
    let n2158: ZB = zb_and(n1362, n2157);
    let n2159: ZB = zb_and(n1362, n1705);
    let n2160: ZB = zb_not(n2158);
    let n2161: ZB = zb_or(n2158, n2159);
    let n2162: ZB = zsel_b(n2158, n1212, n1220);
    let n2163: ZB = zb_and(n1620, n2161);
    let n2164: ZB = zb_and(n1621, n2161);
    let n2165: ZB = zb_or(n2163, n2164);
    let n2166: ZN = zsel_n(n2158, r_c87, n1380);
    let n2167: ZN = zsel_n(n2158, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n2178: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n2179: ZN = zn_sub(n282, zn_splat(P8::from_raw(32768i32)));
    let n2180: ZN = zn_sub(n2179, n283);
    let n2181: ZN = zsel_n(n327, zn_splat(P8::from_raw(0i32)), n2180);
    let n2182: ZN = zsel_n(n324, n2180, n2181);
    let n2183: ZN = zsel_n(n322, zn_splat(P8::from_raw(0i32)), n2182);
    let n2184: ZN = zsel_n(n319, n2180, n2183);
    let n2185: ZN = zsel_n(n317, zn_splat(P8::from_raw(0i32)), n2184);
    let n2186: ZN = zsel_n(n314, n2180, n2185);
    let n2187: ZN = zsel_n(n312, zn_splat(P8::from_raw(0i32)), n2186);
    let n2188: ZN = zsel_n(n309, n2180, n2187);
    let n2189: ZN = zsel_n(n307, zn_splat(P8::from_raw(0i32)), n2188);
    let n2190: ZN = zsel_n(n304, n2180, n2189);
    let n2191: ZN = zsel_n(n302, zn_splat(P8::from_raw(0i32)), n2190);
    let n2192: ZN = zsel_n(n299, n2180, n2191);
    let n2193: ZN = zsel_n(n297, zn_splat(P8::from_raw(0i32)), n2192);
    let n2194: ZN = zsel_n(n294, n2180, n2193);
    let n2195: ZN = zsel_n(n292, zn_splat(P8::from_raw(0i32)), n2194);
    let n2196: ZN = zn_sub(n376, zn_splat(P8::from_raw(32768i32)));
    let n2197: ZN = zn_sub(n2196, n377);
    let n2198: ZN = zsel_n(n421, zn_splat(P8::from_raw(0i32)), n2197);
    let n2199: ZN = zsel_n(n418, n2197, n2198);
    let n2200: ZN = zsel_n(n416, zn_splat(P8::from_raw(0i32)), n2199);
    let n2201: ZN = zsel_n(n413, n2197, n2200);
    let n2202: ZN = zsel_n(n411, zn_splat(P8::from_raw(0i32)), n2201);
    let n2203: ZN = zsel_n(n408, n2197, n2202);
    let n2204: ZN = zsel_n(n406, zn_splat(P8::from_raw(0i32)), n2203);
    let n2205: ZN = zsel_n(n403, n2197, n2204);
    let n2206: ZN = zsel_n(n401, zn_splat(P8::from_raw(0i32)), n2205);
    let n2207: ZN = zsel_n(n398, n2197, n2206);
    let n2208: ZN = zsel_n(n396, zn_splat(P8::from_raw(0i32)), n2207);
    let n2209: ZN = zsel_n(n393, n2197, n2208);
    let n2210: ZN = zsel_n(n391, zn_splat(P8::from_raw(0i32)), n2209);
    let n2211: ZN = zsel_n(n388, n2197, n2210);
    let n2212: ZN = zsel_n(n386, zn_splat(P8::from_raw(0i32)), n2211);
    let n2213: ZN = zsel_n(n279, n2195, r_c368);
    let n2214: ZN = zsel_n(n279, n2212, r_c369);
    let n2215: ZN = zn_sub(r_c282, zn_splat(P8::from_raw(65536i32)));
    let n2216: ZN = zn_sub(r_c284, zn_splat(P8::from_raw(65536i32)));
    let n2217: ZN = zn_sub(n472, r_c358);
    let n2218: ZN = zn_max(r_c360, n2217);
    let n2219: ZN = zn_add(n472, r_c358);
    let n2220: ZN = zn_min(r_c360, n2219);
    let n2221: ZN = zsel_n(n1251, n2218, n2220);
    let n2222: ZN = zn_sub(n473, r_c359);
    let n2223: ZN = zn_max(r_c361, n2222);
    let n2224: ZN = zn_add(n473, r_c359);
    let n2225: ZN = zn_min(r_c361, n2224);
    let n2226: ZN = zsel_n(n1253, n2223, n2225);
    let n2227: ZN = zsel_n(n1289, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(13762i32)));
    let n2228: ZN = zn_sub(n473, n2227);
    let n2229: ZN = zn_max(zn_splat(P8::from_raw(131072i32)), n2228);
    let n2230: ZN = zn_add(n473, n2227);
    let n2231: ZN = zn_min(zn_splat(P8::from_raw(131072i32)), n2230);
    let n2232: ZN = zsel_n(n1292, n2229, n2231);
    let n2233: ZN = zsel_n(n1224, n2232, n473);
    let n2234: ZN = zn_neg(n1303);
    let n2235: ZN = zn_mul(n2234, zn_splat(P8::from_raw(131072i32)));
    let n2236: ZN = zsel_n(n1305, n2235, n1283);
    let n2237: ZN = zsel_n(n1305, zn_splat(P8::from_raw(-131072i32)), n2233);
    let n2238: ZN = zsel_n(n1294, zn_splat(P8::from_raw(0i32)), n1239);
    let n2239: ZN = zsel_n(n1294, n1283, n2236);
    let n2240: ZN = zsel_n(n1294, zn_splat(P8::from_raw(-131072i32)), n2237);
    let n2241: ZN = zn_sub(n1238, zn_splat(P8::from_raw(65536i32)));
    let n2242: ZN = zsel_n(n1312, zn_splat(P8::from_raw(-131072i32)), zn_splat(P8::from_raw(0i32)));
    let n2243: ZN = zsel_n(n1310, zn_splat(P8::from_raw(131072i32)), n2242);
    let n2244: ZN = zsel_n(n1315, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n2245: ZN = zsel_n(n1249, n2216, r_c284);
    let n2246: ZB = zsel_b(n1249, r_c362, n1287);
    let n2247: ZN = zsel_n(n1249, n2221, n1283);
    let n2248: ZN = zsel_n(n1249, n2226, n2233);
    let n2249: ZB = zb_and(n1363, n1778);
    let n2250: ZN = zsel_n(n1368, n2178, r_c20);
    let n2251: ZN = zsel_n(n1368, r_c241, n240);
    let n2252: ZN = zsel_n(n1368, r_c254, n241);
    let n2253: ZN = zsel_n(n1368, r_c261, n269);
    let n2254: ZN = zsel_n(n1368, r_c274, n270);
    let n2255: ZN = zsel_n(n1368, r_c282, n2215);
    let n2256: ZN = zsel_n(n1368, r_c284, n2245);
    let n2257: ZN = zsel_n(n1368, r_c285, n1238);
    let n2258: ZN = zsel_n(n1368, r_c287, n1239);
    let n2259: ZB = zb_and(r_c294, n1368);
    let n2260: ZB = zb_and(r_c295, n1368);
    let n2261: ZN = zsel_n(n1368, r_c301, n470);
    let n2262: ZN = zsel_n(n1368, r_c302, n471);
    let n2263: ZB = zsel_b(n1368, r_c362, n2246);
    let n2264: ZN = zsel_n(n1368, r_c368, n2213);
    let n2265: ZN = zsel_n(n1368, r_c369, n2214);
    let n2266: ZN = zsel_n(n1368, r_c370, n2247);
    let n2267: ZN = zsel_n(n1368, r_c371, n2248);
    let n2268: ZB = zb_or(n1368, n2249);
    let n2269: ZB = zb_or(n1212, n1368);
    let n2270: ZB = zn_gt(n2250, zn_splat(P8::from_raw(0i32)));
    let n2271: ZB = zn_le(n2250, zn_splat(P8::from_raw(0i32)));
    let n2272: ZB = zb_and(n2268, n2270);
    let n2273: ZB = zb_and(n2268, n2271);
    let n2274: ZB = zn_lt(n2261, zn_splat(P8::from_raw(-65536i32)));
    let n2275: ZB = zn_ge(n2261, zn_splat(P8::from_raw(-65536i32)));
    let n2276: ZB = zb_and(n2273, n2275);
    let n2277: ZB = zb_and(n2273, n2274);
    let n2278: ZB = zn_gt(n2261, zn_splat(P8::from_raw(7929856i32)));
    let n2279: ZB = zb_or(n2276, n2277);
    let n2280: ZB = zb_or(n2274, n2278);
    let n2281: ZB = zb_not(n2280);
    let n2282: ZB = zb_and(n2279, n2280);
    let n2283: ZB = zb_and(n2279, n2281);
    let n2284: ZN = zn_min(zn_splat(P8::from_raw(7929856i32)), n2261);
    let n2285: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n2284);
    let n2286: ZN = zsel_n(n2280, n2285, n2261);
    let n2287: ZN = zsel_n(n2280, zn_splat(P8::from_raw(0i32)), n2266);
    let n2288: ZB = zb_or(n2282, n2283);
    let n2289: ZN = zsel_n(n2270, n2261, n2286);
    let n2290: ZN = zsel_n(n2270, n2266, n2287);
    let n2291: ZB = zb_or(n2272, n2288);
    let n2293: ZN = zn_max(n1395, n2228);
    let n2294: ZN = zn_min(n1395, n2230);
    let n2295: ZN = zsel_n(n1396, n2293, n2294);
    let n2296: ZN = zsel_n(n1224, n2295, n473);
    let n2297: ZN = zsel_n(n1305, n2235, n1387);
    let n2298: ZN = zsel_n(n1305, zn_splat(P8::from_raw(-131072i32)), n2296);
    let n2299: ZN = zsel_n(n1294, n1387, n2297);
    let n2300: ZN = zsel_n(n1294, zn_splat(P8::from_raw(-131072i32)), n2298);
    let n2301: ZB = zsel_b(n1249, r_c362, n1391);
    let n2302: ZN = zsel_n(n1249, n2221, n1387);
    let n2303: ZN = zsel_n(n1249, n2226, n2296);
    let n2304: ZB = zb_and(n1363, n1822);
    let n2305: ZB = zsel_b(n1368, r_c362, n2301);
    let n2306: ZN = zsel_n(n1368, r_c370, n2302);
    let n2307: ZN = zsel_n(n1368, r_c371, n2303);
    let n2308: ZB = zb_or(n1368, n2304);
    let n2309: ZB = zb_and(n2270, n2308);
    let n2310: ZB = zb_and(n2271, n2308);
    let n2311: ZB = zb_and(n2275, n2310);
    let n2312: ZB = zb_and(n2274, n2310);
    let n2313: ZB = zb_or(n2311, n2312);
    let n2314: ZB = zb_and(n2280, n2313);
    let n2315: ZB = zb_and(n2281, n2313);
    let n2316: ZN = zsel_n(n2280, zn_splat(P8::from_raw(0i32)), n2306);
    let n2317: ZB = zb_or(n2314, n2315);
    let n2318: ZN = zsel_n(n2270, n2306, n2316);
    let n2319: ZB = zb_or(n2309, n2317);
    let n2320: ZN = zn_max(n1446, n2228);
    let n2321: ZN = zn_min(n1446, n2230);
    let n2322: ZN = zsel_n(n1447, n2320, n2321);
    let n2323: ZN = zsel_n(n1224, n2322, n473);
    let n2324: ZN = zsel_n(n1305, n2235, n1438);
    let n2325: ZN = zsel_n(n1305, zn_splat(P8::from_raw(-131072i32)), n2323);
    let n2326: ZN = zsel_n(n1294, n1438, n2324);
    let n2327: ZN = zsel_n(n1294, zn_splat(P8::from_raw(-131072i32)), n2325);
    let n2328: ZB = zsel_b(n1249, r_c362, n1442);
    let n2329: ZN = zsel_n(n1249, n2221, n1438);
    let n2330: ZN = zsel_n(n1249, n2226, n2323);
    let n2331: ZB = zb_and(n1363, n1866);
    let n2332: ZB = zsel_b(n1368, r_c362, n2328);
    let n2333: ZN = zsel_n(n1368, r_c370, n2329);
    let n2334: ZN = zsel_n(n1368, r_c371, n2330);
    let n2335: ZB = zb_or(n1368, n2331);
    let n2336: ZB = zb_and(n2270, n2335);
    let n2337: ZB = zb_and(n2271, n2335);
    let n2338: ZB = zb_and(n2275, n2337);
    let n2339: ZB = zb_and(n2274, n2337);
    let n2340: ZB = zb_or(n2338, n2339);
    let n2341: ZB = zb_and(n2280, n2340);
    let n2342: ZB = zb_and(n2281, n2340);
    let n2343: ZN = zsel_n(n2280, zn_splat(P8::from_raw(0i32)), n2333);
    let n2344: ZB = zb_or(n2341, n2342);
    let n2345: ZN = zsel_n(n2270, n2333, n2343);
    let n2346: ZB = zb_or(n2336, n2344);
    let n2347: ZN = zsel_n(n179, n2238, n1239);
    let n2348: ZN = zsel_n(n179, n2239, n1283);
    let n2349: ZN = zsel_n(n179, n2240, n2233);
    let n2350: ZN = zsel_n(n1249, n1239, n2347);
    let n2351: ZN = zsel_n(n1249, n2221, n2348);
    let n2352: ZN = zsel_n(n1249, n2226, n2349);
    let n2353: ZB = zb_and(n1363, n1909);
    let n2354: ZN = zsel_n(n1368, r_c287, n2350);
    let n2355: ZB = zb_or(r_c295, n124);
    let n2356: ZN = zsel_n(n1368, r_c370, n2351);
    let n2357: ZN = zsel_n(n1368, r_c371, n2352);
    let n2358: ZB = zb_or(n1368, n2353);
    let n2359: ZB = zb_and(n2270, n2358);
    let n2360: ZB = zb_and(n2271, n2358);
    let n2361: ZB = zb_and(n2275, n2360);
    let n2362: ZB = zb_and(n2274, n2360);
    let n2363: ZB = zb_or(n2361, n2362);
    let n2364: ZB = zb_and(n2280, n2363);
    let n2365: ZB = zb_and(n2281, n2363);
    let n2366: ZN = zsel_n(n2280, zn_splat(P8::from_raw(0i32)), n2356);
    let n2367: ZB = zb_or(n2364, n2365);
    let n2368: ZN = zsel_n(n2270, n2356, n2366);
    let n2369: ZB = zb_or(n2359, n2367);
    let n2370: ZN = zsel_n(n179, n2299, n1387);
    let n2371: ZN = zsel_n(n179, n2300, n2296);
    let n2372: ZN = zsel_n(n1249, n2221, n2370);
    let n2373: ZN = zsel_n(n1249, n2226, n2371);
    let n2374: ZB = zb_and(n1363, n1952);
    let n2375: ZN = zsel_n(n1368, r_c370, n2372);
    let n2376: ZN = zsel_n(n1368, r_c371, n2373);
    let n2377: ZB = zb_or(n1368, n2374);
    let n2378: ZB = zb_and(n2270, n2377);
    let n2379: ZB = zb_and(n2271, n2377);
    let n2380: ZB = zb_and(n2275, n2379);
    let n2381: ZB = zb_and(n2274, n2379);
    let n2382: ZB = zb_or(n2380, n2381);
    let n2383: ZB = zb_and(n2280, n2382);
    let n2384: ZB = zb_and(n2281, n2382);
    let n2385: ZN = zsel_n(n2280, zn_splat(P8::from_raw(0i32)), n2375);
    let n2386: ZB = zb_or(n2383, n2384);
    let n2387: ZN = zsel_n(n2270, n2375, n2385);
    let n2388: ZB = zb_or(n2378, n2386);
    let n2389: ZN = zsel_n(n179, n2326, n1438);
    let n2390: ZN = zsel_n(n179, n2327, n2323);
    let n2391: ZN = zsel_n(n1249, n2221, n2389);
    let n2392: ZN = zsel_n(n1249, n2226, n2390);
    let n2393: ZB = zb_and(n1363, n1995);
    let n2394: ZN = zsel_n(n1368, r_c370, n2391);
    let n2395: ZN = zsel_n(n1368, r_c371, n2392);
    let n2396: ZB = zb_or(n1368, n2393);
    let n2397: ZB = zb_and(n2270, n2396);
    let n2398: ZB = zb_and(n2271, n2396);
    let n2399: ZB = zb_and(n2275, n2398);
    let n2400: ZB = zb_and(n2274, n2398);
    let n2401: ZB = zb_or(n2399, n2400);
    let n2402: ZB = zb_and(n2280, n2401);
    let n2403: ZB = zb_and(n2281, n2401);
    let n2404: ZN = zsel_n(n2280, zn_splat(P8::from_raw(0i32)), n2394);
    let n2405: ZB = zb_or(n2402, n2403);
    let n2406: ZN = zsel_n(n2270, n2394, n2404);
    let n2407: ZB = zb_or(n2397, n2405);
    let n2408: ZN = zsel_n(n1594, zn_splat(P8::from_raw(655360i32)), n2215);
    let n2409: ZN = zsel_n(n1594, zn_splat(P8::from_raw(262144i32)), r_c284);
    let n2410: ZN = zsel_n(n1594, n2241, n1238);
    let n2411: ZN = zsel_n(n1594, zn_splat(P8::from_raw(98304i32)), r_c358);
    let n2412: ZN = zsel_n(n1594, n2244, r_c359);
    let n2413: ZN = zsel_n(n1594, n2243, r_c360);
    let n2414: ZN = zsel_n(n1594, zn_splat(P8::from_raw(0i32)), r_c361);
    let n2415: ZN = zsel_n(n1594, n1309, n1283);
    let n2416: ZN = zsel_n(n1594, zn_splat(P8::from_raw(0i32)), n2233);
    let n2417: ZN = zsel_n(n1249, n2215, n2408);
    let n2418: ZN = zsel_n(n1249, n2216, n2409);
    let n2419: ZN = zsel_n(n1249, n1238, n2410);
    let n2420: ZN = zsel_n(n1249, r_c358, n2411);
    let n2421: ZN = zsel_n(n1249, r_c359, n2412);
    let n2422: ZN = zsel_n(n1249, r_c360, n2413);
    let n2423: ZN = zsel_n(n1249, r_c361, n2414);
    let n2424: ZN = zsel_n(n1249, n2221, n2415);
    let n2425: ZN = zsel_n(n1249, n2226, n2416);
    let n2426: ZB = zb_and(n1363, n2023);
    let n2427: ZN = zsel_n(n1368, n2178, n1598);
    let n2428: ZB = zsel_b(n1368, r_c41, n1599);
    let n2429: ZN = zsel_n(n1368, r_c282, n2417);
    let n2430: ZN = zsel_n(n1368, r_c284, n2418);
    let n2431: ZN = zsel_n(n1368, r_c285, n2419);
    let n2432: ZB = zb_or(r_c294, n124);
    let n2433: ZN = zsel_n(n1368, r_c358, n2420);
    let n2434: ZN = zsel_n(n1368, r_c359, n2421);
    let n2435: ZN = zsel_n(n1368, r_c360, n2422);
    let n2436: ZN = zsel_n(n1368, r_c361, n2423);
    let n2437: ZN = zsel_n(n1368, r_c370, n2424);
    let n2438: ZN = zsel_n(n1368, r_c371, n2425);
    let n2439: ZB = zb_or(n1368, n2426);
    let n2440: ZB = zn_gt(n2427, zn_splat(P8::from_raw(0i32)));
    let n2441: ZB = zn_le(n2427, zn_splat(P8::from_raw(0i32)));
    let n2442: ZB = zb_and(n2439, n2440);
    let n2443: ZB = zb_and(n2439, n2441);
    let n2444: ZB = zb_and(n2275, n2443);
    let n2445: ZB = zb_and(n2274, n2443);
    let n2446: ZB = zb_or(n2444, n2445);
    let n2447: ZB = zb_and(n2280, n2446);
    let n2448: ZB = zb_and(n2281, n2446);
    let n2449: ZN = zsel_n(n2280, zn_splat(P8::from_raw(0i32)), n2437);
    let n2450: ZB = zb_or(n2447, n2448);
    let n2451: ZN = zsel_n(n2440, n2261, n2286);
    let n2452: ZN = zsel_n(n2440, n2437, n2449);
    let n2453: ZB = zb_or(n2442, n2450);
    let n2454: ZN = zsel_n(n1594, zn_splat(P8::from_raw(69510i32)), r_c359);
    let n2455: ZN = zsel_n(n1594, zn_splat(P8::from_raw(-131072i32)), r_c360);
    let n2456: ZN = zsel_n(n1594, zn_splat(P8::from_raw(-327680i32)), n1387);
    let n2457: ZN = zsel_n(n1594, zn_splat(P8::from_raw(0i32)), n2296);
    let n2458: ZN = zsel_n(n1249, r_c359, n2454);
    let n2459: ZN = zsel_n(n1249, r_c360, n2455);
    let n2460: ZN = zsel_n(n1249, n2221, n2456);
    let n2461: ZN = zsel_n(n1249, n2226, n2457);
    let n2462: ZB = zb_and(n1363, n2041);
    let n2463: ZN = zsel_n(n1368, r_c359, n2458);
    let n2464: ZN = zsel_n(n1368, r_c360, n2459);
    let n2465: ZN = zsel_n(n1368, r_c370, n2460);
    let n2466: ZN = zsel_n(n1368, r_c371, n2461);
    let n2467: ZB = zb_or(n1368, n2462);
    let n2468: ZB = zb_and(n2440, n2467);
    let n2469: ZB = zb_and(n2441, n2467);
    let n2470: ZB = zb_and(n2275, n2469);
    let n2471: ZB = zb_and(n2274, n2469);
    let n2472: ZB = zb_or(n2470, n2471);
    let n2473: ZB = zb_and(n2280, n2472);
    let n2474: ZB = zb_and(n2281, n2472);
    let n2475: ZN = zsel_n(n2280, zn_splat(P8::from_raw(0i32)), n2465);
    let n2476: ZB = zb_or(n2473, n2474);
    let n2477: ZN = zsel_n(n2440, n2465, n2475);
    let n2478: ZB = zb_or(n2468, n2476);
    let n2479: ZN = zsel_n(n1594, zn_splat(P8::from_raw(131072i32)), r_c360);
    let n2480: ZN = zsel_n(n1594, zn_splat(P8::from_raw(327680i32)), n1438);
    let n2481: ZN = zsel_n(n1594, zn_splat(P8::from_raw(0i32)), n2323);
    let n2482: ZN = zsel_n(n1249, r_c360, n2479);
    let n2483: ZN = zsel_n(n1249, n2221, n2480);
    let n2484: ZN = zsel_n(n1249, n2226, n2481);
    let n2485: ZB = zb_and(n1363, n2059);
    let n2486: ZN = zsel_n(n1368, r_c360, n2482);
    let n2487: ZN = zsel_n(n1368, r_c370, n2483);
    let n2488: ZN = zsel_n(n1368, r_c371, n2484);
    let n2489: ZB = zb_or(n1368, n2485);
    let n2490: ZB = zb_and(n2440, n2489);
    let n2491: ZB = zb_and(n2441, n2489);
    let n2492: ZB = zb_and(n2275, n2491);
    let n2493: ZB = zb_and(n2274, n2491);
    let n2494: ZB = zb_or(n2492, n2493);
    let n2495: ZB = zb_and(n2280, n2494);
    let n2496: ZB = zb_and(n2281, n2494);
    let n2497: ZN = zsel_n(n2280, zn_splat(P8::from_raw(0i32)), n2487);
    let n2498: ZB = zb_or(n2495, n2496);
    let n2499: ZN = zsel_n(n2440, n2487, n2497);
    let n2500: ZB = zb_or(n2490, n2498);
    let n2502: ZN = zsel_n(n1594, zn_splat(P8::from_raw(69510i32)), r_c358);
    let n2503: ZN = zsel_n(n1594, zn_splat(P8::from_raw(98304i32)), r_c359);
    let n2504: ZN = zsel_n(n1594, zn_splat(P8::from_raw(0i32)), r_c360);
    let n2505: ZN = zsel_n(n1594, zn_splat(P8::from_raw(-98304i32)), r_c361);
    let n2506: ZN = zsel_n(n1594, zn_splat(P8::from_raw(0i32)), n1283);
    let n2507: ZN = zsel_n(n1594, zn_splat(P8::from_raw(-327680i32)), n2233);
    let n2508: ZN = zsel_n(n1249, r_c358, n2502);
    let n2509: ZN = zsel_n(n1249, r_c359, n2503);
    let n2510: ZN = zsel_n(n1249, r_c360, n2504);
    let n2511: ZN = zsel_n(n1249, r_c361, n2505);
    let n2512: ZN = zsel_n(n1249, n2221, n2506);
    let n2513: ZN = zsel_n(n1249, n2226, n2507);
    let n2514: ZB = zb_and(n1363, n2075);
    let n2515: ZN = zsel_n(n1368, r_c358, n2508);
    let n2516: ZN = zsel_n(n1368, r_c359, n2509);
    let n2517: ZN = zsel_n(n1368, r_c360, n2510);
    let n2518: ZN = zsel_n(n1368, r_c361, n2511);
    let n2519: ZN = zsel_n(n1368, r_c370, n2512);
    let n2520: ZN = zsel_n(n1368, r_c371, n2513);
    let n2521: ZB = zb_or(n1368, n2514);
    let n2522: ZB = zb_and(n2440, n2521);
    let n2523: ZB = zb_and(n2441, n2521);
    let n2524: ZB = zb_and(n2275, n2523);
    let n2525: ZB = zb_and(n2274, n2523);
    let n2526: ZB = zb_or(n2524, n2525);
    let n2527: ZB = zb_and(n2280, n2526);
    let n2528: ZB = zb_and(n2281, n2526);
    let n2529: ZN = zsel_n(n2280, zn_splat(P8::from_raw(0i32)), n2519);
    let n2530: ZB = zb_or(n2527, n2528);
    let n2531: ZN = zsel_n(n2440, n2519, n2529);
    let n2532: ZB = zb_or(n2522, n2530);
    let n2533: ZN = zsel_n(n1594, zn_splat(P8::from_raw(-231700i32)), n1387);
    let n2534: ZN = zsel_n(n1594, zn_splat(P8::from_raw(-231700i32)), n2296);
    let n2535: ZN = zsel_n(n1249, n2221, n2533);
    let n2536: ZN = zsel_n(n1249, n2226, n2534);
    let n2537: ZN = zsel_n(n1368, r_c370, n2535);
    let n2538: ZN = zsel_n(n1368, r_c371, n2536);
    let n2539: ZN = zsel_n(n2280, zn_splat(P8::from_raw(0i32)), n2537);
    let n2540: ZN = zsel_n(n2440, n2537, n2539);
    let n2541: ZN = zsel_n(n1594, zn_splat(P8::from_raw(231700i32)), n1438);
    let n2542: ZN = zsel_n(n1594, zn_splat(P8::from_raw(-231700i32)), n2323);
    let n2543: ZN = zsel_n(n1249, n2221, n2541);
    let n2544: ZN = zsel_n(n1249, n2226, n2542);
    let n2545: ZN = zsel_n(n1368, r_c370, n2543);
    let n2546: ZN = zsel_n(n1368, r_c371, n2544);
    let n2547: ZN = zsel_n(n2280, zn_splat(P8::from_raw(0i32)), n2545);
    let n2548: ZN = zsel_n(n2440, n2545, n2547);
    let n2549: ZN = zsel_n(n1594, zn_splat(P8::from_raw(131072i32)), r_c361);
    let n2550: ZN = zsel_n(n1594, zn_splat(P8::from_raw(327680i32)), n2233);
    let n2551: ZN = zsel_n(n1249, r_c361, n2549);
    let n2552: ZN = zsel_n(n1249, n2226, n2550);
    let n2553: ZN = zsel_n(n1368, r_c361, n2551);
    let n2554: ZN = zsel_n(n1368, r_c371, n2552);
    let n2555: ZN = zsel_n(n1594, zn_splat(P8::from_raw(231700i32)), n2296);
    let n2556: ZN = zsel_n(n1249, n2226, n2555);
    let n2557: ZN = zsel_n(n1368, r_c371, n2556);
    let n2558: ZN = zsel_n(n1594, zn_splat(P8::from_raw(231700i32)), n2323);
    let n2559: ZN = zsel_n(n1249, n2226, n2558);
    let n2560: ZN = zsel_n(n1368, r_c371, n2559);
    let n2561: ZN = zsel_n(n1594, n1309, n2348);
    let n2562: ZN = zsel_n(n1594, zn_splat(P8::from_raw(0i32)), n2349);
    let n2563: ZN = zsel_n(n1249, n2221, n2561);
    let n2564: ZN = zsel_n(n1249, n2226, n2562);
    let n2565: ZB = zb_and(n1363, n2105);
    let n2566: ZN = zsel_n(n1368, r_c370, n2563);
    let n2567: ZN = zsel_n(n1368, r_c371, n2564);
    let n2568: ZB = zb_or(n1368, n2565);
    let n2569: ZB = zb_and(n2440, n2568);
    let n2570: ZB = zb_and(n2441, n2568);
    let n2571: ZB = zb_and(n2275, n2570);
    let n2572: ZB = zb_and(n2274, n2570);
    let n2573: ZB = zb_or(n2571, n2572);
    let n2574: ZB = zb_and(n2280, n2573);
    let n2575: ZB = zb_and(n2281, n2573);
    let n2576: ZN = zsel_n(n2280, zn_splat(P8::from_raw(0i32)), n2566);
    let n2577: ZB = zb_or(n2574, n2575);
    let n2578: ZN = zsel_n(n2440, n2566, n2576);
    let n2579: ZB = zb_or(n2569, n2577);
    let n2580: ZN = zsel_n(n1594, zn_splat(P8::from_raw(-327680i32)), n2370);
    let n2581: ZN = zsel_n(n1594, zn_splat(P8::from_raw(0i32)), n2371);
    let n2582: ZN = zsel_n(n1249, n2221, n2580);
    let n2583: ZN = zsel_n(n1249, n2226, n2581);
    let n2584: ZB = zb_and(n1363, n2123);
    let n2585: ZN = zsel_n(n1368, r_c370, n2582);
    let n2586: ZN = zsel_n(n1368, r_c371, n2583);
    let n2587: ZB = zb_or(n1368, n2584);
    let n2588: ZB = zb_and(n2440, n2587);
    let n2589: ZB = zb_and(n2441, n2587);
    let n2590: ZB = zb_and(n2275, n2589);
    let n2591: ZB = zb_and(n2274, n2589);
    let n2592: ZB = zb_or(n2590, n2591);
    let n2593: ZB = zb_and(n2280, n2592);
    let n2594: ZB = zb_and(n2281, n2592);
    let n2595: ZN = zsel_n(n2280, zn_splat(P8::from_raw(0i32)), n2585);
    let n2596: ZB = zb_or(n2593, n2594);
    let n2597: ZN = zsel_n(n2440, n2585, n2595);
    let n2598: ZB = zb_or(n2588, n2596);
    let n2599: ZN = zsel_n(n1594, zn_splat(P8::from_raw(327680i32)), n2389);
    let n2600: ZN = zsel_n(n1594, zn_splat(P8::from_raw(0i32)), n2390);
    let n2601: ZN = zsel_n(n1249, n2221, n2599);
    let n2602: ZN = zsel_n(n1249, n2226, n2600);
    let n2603: ZB = zb_and(n1363, n2141);
    let n2604: ZN = zsel_n(n1368, r_c370, n2601);
    let n2605: ZN = zsel_n(n1368, r_c371, n2602);
    let n2606: ZB = zb_or(n1368, n2603);
    let n2607: ZB = zb_and(n2440, n2606);
    let n2608: ZB = zb_and(n2441, n2606);
    let n2609: ZB = zb_and(n2275, n2608);
    let n2610: ZB = zb_and(n2274, n2608);
    let n2611: ZB = zb_or(n2609, n2610);
    let n2612: ZB = zb_and(n2280, n2611);
    let n2613: ZB = zb_and(n2281, n2611);
    let n2614: ZN = zsel_n(n2280, zn_splat(P8::from_raw(0i32)), n2604);
    let n2615: ZB = zb_or(n2612, n2613);
    let n2616: ZN = zsel_n(n2440, n2604, n2614);
    let n2617: ZB = zb_or(n2607, n2615);
    let n2618: ZN = zsel_n(n1594, zn_splat(P8::from_raw(0i32)), n2348);
    let n2619: ZN = zsel_n(n1594, zn_splat(P8::from_raw(-327680i32)), n2349);
    let n2620: ZN = zsel_n(n1249, n2221, n2618);
    let n2621: ZN = zsel_n(n1249, n2226, n2619);
    let n2622: ZB = zb_and(n1363, n2157);
    let n2623: ZN = zsel_n(n1368, r_c370, n2620);
    let n2624: ZN = zsel_n(n1368, r_c371, n2621);
    let n2625: ZB = zb_or(n1368, n2622);
    let n2626: ZB = zb_and(n2440, n2625);
    let n2627: ZB = zb_and(n2441, n2625);
    let n2628: ZB = zb_and(n2275, n2627);
    let n2629: ZB = zb_and(n2274, n2627);
    let n2630: ZB = zb_or(n2628, n2629);
    let n2631: ZB = zb_and(n2280, n2630);
    let n2632: ZB = zb_and(n2281, n2630);
    let n2633: ZN = zsel_n(n2280, zn_splat(P8::from_raw(0i32)), n2623);
    let n2634: ZB = zb_or(n2631, n2632);
    let n2635: ZN = zsel_n(n2440, n2623, n2633);
    let n2636: ZB = zb_or(n2626, n2634);
    let n2637: ZN = zsel_n(n1594, zn_splat(P8::from_raw(-231700i32)), n2370);
    let n2638: ZN = zsel_n(n1594, zn_splat(P8::from_raw(-231700i32)), n2371);
    let n2639: ZN = zsel_n(n1249, n2221, n2637);
    let n2640: ZN = zsel_n(n1249, n2226, n2638);
    let n2641: ZN = zsel_n(n1368, r_c370, n2639);
    let n2642: ZN = zsel_n(n1368, r_c371, n2640);
    let n2643: ZN = zsel_n(n2280, zn_splat(P8::from_raw(0i32)), n2641);
    let n2644: ZN = zsel_n(n2440, n2641, n2643);
    let n2645: ZN = zsel_n(n1594, zn_splat(P8::from_raw(231700i32)), n2389);
    let n2646: ZN = zsel_n(n1594, zn_splat(P8::from_raw(-231700i32)), n2390);
    let n2647: ZN = zsel_n(n1249, n2221, n2645);
    let n2648: ZN = zsel_n(n1249, n2226, n2646);
    let n2649: ZN = zsel_n(n1368, r_c370, n2647);
    let n2650: ZN = zsel_n(n1368, r_c371, n2648);
    let n2651: ZN = zsel_n(n2280, zn_splat(P8::from_raw(0i32)), n2649);
    let n2652: ZN = zsel_n(n2440, n2649, n2651);
    let n2653: ZN = zsel_n(n1594, zn_splat(P8::from_raw(327680i32)), n2349);
    let n2654: ZN = zsel_n(n1249, n2226, n2653);
    let n2655: ZN = zsel_n(n1368, r_c371, n2654);
    let n2656: ZN = zsel_n(n1594, zn_splat(P8::from_raw(231700i32)), n2371);
    let n2657: ZN = zsel_n(n1249, n2226, n2656);
    let n2658: ZN = zsel_n(n1368, r_c371, n2657);
    let n2659: ZN = zsel_n(n1594, zn_splat(P8::from_raw(231700i32)), n2390);
    let n2660: ZN = zsel_n(n1249, n2226, n2659);
    let n2661: ZN = zsel_n(n1368, r_c371, n2660);
    let n2664: ZW = zw_bits_n(n111);
    let n2665: ZW = zw_mix1(zw_splat(11400714819323198485u64), n2664, 84u64);
    let n2666: ZW = zw_mix2(zw_splat(11562461410679940143u64), n2664, 84u64);
    let n2667: ZW = zw_bits_n(n214);
    let n2668: ZW = zw_mix1(n2665, n2667, 85u64);
    let n2669: ZW = zw_mix2(n2666, n2667, 85u64);
    let n2670: ZW = zw_bits_n(n213);
    let n2671: ZW = zw_mix1(n2668, n2670, 86u64);
    let n2672: ZW = zw_mix2(n2669, n2670, 86u64);
    let n2673: ZW = zw_bits_n(n1380);
    let n2674: ZW = zw_mix1(n2671, n2673, 87u64);
    let n2675: ZW = zw_mix2(n2672, n2673, 87u64);
    let n2676: ZW = zw_bits_n(n240);
    let n2677: ZW = zw_mix1(n2674, n2676, 240u64);
    let n2678: ZW = zw_mix2(n2675, n2676, 240u64);
    let n2679: ZW = zw_bits_n(n241);
    let n2680: ZW = zw_mix1(n2677, n2679, 253u64);
    let n2681: ZW = zw_mix2(n2678, n2679, 253u64);
    let n2682: ZW = zw_bits_n(n269);
    let n2683: ZW = zw_mix1(n2680, n2682, 260u64);
    let n2684: ZW = zw_mix2(n2681, n2682, 260u64);
    let n2685: ZW = zw_bits_n(n270);
    let n2686: ZW = zw_mix1(n2683, n2685, 273u64);
    let n2687: ZW = zw_mix2(n2684, n2685, 273u64);
    let n2688: ZW = zw_bits_n(r_c20);
    let n2689: ZW = zw_mix1(n2686, n2688, 20u64);
    let n2690: ZW = zw_mix2(n2687, n2688, 20u64);
    let n2691: ZW = zw_bits_b(r_c41);
    let n2692: ZW = zw_mix1(n2689, n2691, 41u64);
    let n2693: ZW = zw_mix2(n2690, n2691, 41u64);
    let n2694: ZW = zw_bits_n(n1598);
    let n2695: ZW = zw_mix1(n2686, n2694, 20u64);
    let n2696: ZW = zw_mix2(n2687, n2694, 20u64);
    let n2697: ZW = zw_bits_b(n1599);
    let n2698: ZW = zw_mix1(n2695, n2697, 41u64);
    let n2699: ZW = zw_mix2(n2696, n2697, 41u64);
    let n2700: ZW = zw_mix1(n2671, n2688, 20u64);
    let n2701: ZW = zw_mix2(n2672, n2688, 20u64);
    let n2702: ZW = zw_bits_b(n1781);
    let n2703: ZW = zw_mix1(n2700, n2702, 38u64);
    let n2704: ZW = zw_mix2(n2701, n2702, 38u64);
    let n2705: ZW = zw_bits_n(n1786);
    let n2706: ZW = zw_mix1(n2703, n2705, 39u64);
    let n2707: ZW = zw_mix2(n2704, n2705, 39u64);
    let n2708: ZW = zw_bits_n(n1785);
    let n2709: ZW = zw_mix1(n2706, n2708, 87u64);
    let n2710: ZW = zw_mix2(n2707, n2708, 87u64);
    let n2711: ZW = zw_bits_b(n1825);
    let n2712: ZW = zw_mix1(n2700, n2711, 38u64);
    let n2713: ZW = zw_mix2(n2701, n2711, 38u64);
    let n2714: ZW = zw_bits_n(n1830);
    let n2715: ZW = zw_mix1(n2712, n2714, 39u64);
    let n2716: ZW = zw_mix2(n2713, n2714, 39u64);
    let n2717: ZW = zw_bits_n(n1829);
    let n2718: ZW = zw_mix1(n2715, n2717, 87u64);
    let n2719: ZW = zw_mix2(n2716, n2717, 87u64);
    let n2720: ZW = zw_bits_b(n1869);
    let n2721: ZW = zw_mix1(n2700, n2720, 38u64);
    let n2722: ZW = zw_mix2(n2701, n2720, 38u64);
    let n2723: ZW = zw_bits_n(n1874);
    let n2724: ZW = zw_mix1(n2721, n2723, 39u64);
    let n2725: ZW = zw_mix2(n2722, n2723, 39u64);
    let n2726: ZW = zw_bits_n(n1873);
    let n2727: ZW = zw_mix1(n2724, n2726, 87u64);
    let n2728: ZW = zw_mix2(n2725, n2726, 87u64);
    let n2729: ZW = zw_bits_b(n1912);
    let n2730: ZW = zw_mix1(n2700, n2729, 38u64);
    let n2731: ZW = zw_mix2(n2701, n2729, 38u64);
    let n2732: ZW = zw_bits_n(n1917);
    let n2733: ZW = zw_mix1(n2730, n2732, 39u64);
    let n2734: ZW = zw_mix2(n2731, n2732, 39u64);
    let n2735: ZW = zw_bits_n(n1916);
    let n2736: ZW = zw_mix1(n2733, n2735, 87u64);
    let n2737: ZW = zw_mix2(n2734, n2735, 87u64);
    let n2738: ZW = zw_bits_b(n1955);
    let n2739: ZW = zw_mix1(n2700, n2738, 38u64);
    let n2740: ZW = zw_mix2(n2701, n2738, 38u64);
    let n2741: ZW = zw_bits_n(n1960);
    let n2742: ZW = zw_mix1(n2739, n2741, 39u64);
    let n2743: ZW = zw_mix2(n2740, n2741, 39u64);
    let n2744: ZW = zw_bits_n(n1959);
    let n2745: ZW = zw_mix1(n2742, n2744, 87u64);
    let n2746: ZW = zw_mix2(n2743, n2744, 87u64);
    let n2747: ZW = zw_bits_b(n1998);
    let n2748: ZW = zw_mix1(n2700, n2747, 38u64);
    let n2749: ZW = zw_mix2(n2701, n2747, 38u64);
    let n2750: ZW = zw_bits_n(n2003);
    let n2751: ZW = zw_mix1(n2748, n2750, 39u64);
    let n2752: ZW = zw_mix2(n2749, n2750, 39u64);
    let n2753: ZW = zw_bits_n(n2002);
    let n2754: ZW = zw_mix1(n2751, n2753, 87u64);
    let n2755: ZW = zw_mix2(n2752, n2753, 87u64);
    let n2756: ZW = zw_mix1(n2671, n2694, 20u64);
    let n2757: ZW = zw_mix2(n2672, n2694, 20u64);
    let n2758: ZW = zw_bits_b(n2026);
    let n2759: ZW = zw_mix1(n2756, n2758, 38u64);
    let n2760: ZW = zw_mix2(n2757, n2758, 38u64);
    let n2761: ZW = zw_bits_n(n2033);
    let n2762: ZW = zw_mix1(n2759, n2761, 39u64);
    let n2763: ZW = zw_mix2(n2760, n2761, 39u64);
    let n2764: ZW = zw_bits_n(n2032);
    let n2765: ZW = zw_mix1(n2762, n2764, 87u64);
    let n2766: ZW = zw_mix2(n2763, n2764, 87u64);
    let n2767: ZW = zw_bits_b(n2044);
    let n2768: ZW = zw_mix1(n2756, n2767, 38u64);
    let n2769: ZW = zw_mix2(n2757, n2767, 38u64);
    let n2770: ZW = zw_bits_n(n2051);
    let n2771: ZW = zw_mix1(n2768, n2770, 39u64);
    let n2772: ZW = zw_mix2(n2769, n2770, 39u64);
    let n2773: ZW = zw_bits_n(n2050);
    let n2774: ZW = zw_mix1(n2771, n2773, 87u64);
    let n2775: ZW = zw_mix2(n2772, n2773, 87u64);
    let n2776: ZW = zw_bits_b(n2062);
    let n2777: ZW = zw_mix1(n2756, n2776, 38u64);
    let n2778: ZW = zw_mix2(n2757, n2776, 38u64);
    let n2779: ZW = zw_bits_n(n2069);
    let n2780: ZW = zw_mix1(n2777, n2779, 39u64);
    let n2781: ZW = zw_mix2(n2778, n2779, 39u64);
    let n2782: ZW = zw_bits_n(n2068);
    let n2783: ZW = zw_mix1(n2780, n2782, 87u64);
    let n2784: ZW = zw_mix2(n2781, n2782, 87u64);
    let n2785: ZW = zw_bits_b(n2078);
    let n2786: ZW = zw_mix1(n2756, n2785, 38u64);
    let n2787: ZW = zw_mix2(n2757, n2785, 38u64);
    let n2788: ZW = zw_bits_n(n2085);
    let n2789: ZW = zw_mix1(n2786, n2788, 39u64);
    let n2790: ZW = zw_mix2(n2787, n2788, 39u64);
    let n2791: ZW = zw_bits_n(n2084);
    let n2792: ZW = zw_mix1(n2789, n2791, 87u64);
    let n2793: ZW = zw_mix2(n2790, n2791, 87u64);
    let n2794: ZW = zw_bits_b(n2108);
    let n2795: ZW = zw_mix1(n2756, n2794, 38u64);
    let n2796: ZW = zw_mix2(n2757, n2794, 38u64);
    let n2797: ZW = zw_bits_n(n2115);
    let n2798: ZW = zw_mix1(n2795, n2797, 39u64);
    let n2799: ZW = zw_mix2(n2796, n2797, 39u64);
    let n2800: ZW = zw_bits_n(n2114);
    let n2801: ZW = zw_mix1(n2798, n2800, 87u64);
    let n2802: ZW = zw_mix2(n2799, n2800, 87u64);
    let n2803: ZW = zw_bits_b(n2126);
    let n2804: ZW = zw_mix1(n2756, n2803, 38u64);
    let n2805: ZW = zw_mix2(n2757, n2803, 38u64);
    let n2806: ZW = zw_bits_n(n2133);
    let n2807: ZW = zw_mix1(n2804, n2806, 39u64);
    let n2808: ZW = zw_mix2(n2805, n2806, 39u64);
    let n2809: ZW = zw_bits_n(n2132);
    let n2810: ZW = zw_mix1(n2807, n2809, 87u64);
    let n2811: ZW = zw_mix2(n2808, n2809, 87u64);
    let n2812: ZW = zw_bits_b(n2144);
    let n2813: ZW = zw_mix1(n2756, n2812, 38u64);
    let n2814: ZW = zw_mix2(n2757, n2812, 38u64);
    let n2815: ZW = zw_bits_n(n2151);
    let n2816: ZW = zw_mix1(n2813, n2815, 39u64);
    let n2817: ZW = zw_mix2(n2814, n2815, 39u64);
    let n2818: ZW = zw_bits_n(n2150);
    let n2819: ZW = zw_mix1(n2816, n2818, 87u64);
    let n2820: ZW = zw_mix2(n2817, n2818, 87u64);
    let n2821: ZW = zw_bits_b(n2160);
    let n2822: ZW = zw_mix1(n2756, n2821, 38u64);
    let n2823: ZW = zw_mix2(n2757, n2821, 38u64);
    let n2824: ZW = zw_bits_n(n2167);
    let n2825: ZW = zw_mix1(n2822, n2824, 39u64);
    let n2826: ZW = zw_mix2(n2823, n2824, 39u64);
    let n2827: ZW = zw_bits_n(n2166);
    let n2828: ZW = zw_mix1(n2825, n2827, 87u64);
    let n2829: ZW = zw_mix2(n2826, n2827, 87u64);
    let n2830: ZW = zw_bits_n(r_c39);
    let n2831: ZW = zw_mix1(zw_splat(11400714819323198485u64), n2830, 39u64);
    let n2832: ZW = zw_mix2(zw_splat(11562461410679940143u64), n2830, 39u64);
    let n2833: ZW = zw_mix1(n2831, n2664, 84u64);
    let n2834: ZW = zw_mix2(n2832, n2664, 84u64);
    let n2835: ZW = zw_mix1(n2833, n2667, 85u64);
    let n2836: ZW = zw_mix2(n2834, n2667, 85u64);
    let n2837: ZW = zw_mix1(n2835, n2670, 86u64);
    let n2838: ZW = zw_mix2(n2836, n2670, 86u64);
    let n2839: ZW = zw_bits_n(r_c87);
    let n2840: ZW = zw_mix1(n2837, n2839, 87u64);
    let n2841: ZW = zw_mix2(n2838, n2839, 87u64);
    let n2842: ZW = zw_bits_n(n2251);
    let n2843: ZW = zw_mix1(n2840, n2842, 241u64);
    let n2844: ZW = zw_mix2(n2841, n2842, 241u64);
    let n2845: ZW = zw_bits_n(n2252);
    let n2846: ZW = zw_mix1(n2843, n2845, 254u64);
    let n2847: ZW = zw_mix2(n2844, n2845, 254u64);
    let n2848: ZW = zw_bits_n(n2253);
    let n2849: ZW = zw_mix1(n2846, n2848, 261u64);
    let n2850: ZW = zw_mix2(n2847, n2848, 261u64);
    let n2851: ZW = zw_bits_n(n2254);
    let n2852: ZW = zw_mix1(n2849, n2851, 274u64);
    let n2853: ZW = zw_mix2(n2850, n2851, 274u64);
    let n2854: ZW = zw_bits_n(n2262);
    let n2855: ZW = zw_mix1(n2852, n2854, 302u64);
    let n2856: ZW = zw_mix2(n2853, n2854, 302u64);
    let n2857: ZW = zw_bits_n(n2264);
    let n2858: ZW = zw_mix1(n2855, n2857, 368u64);
    let n2859: ZW = zw_mix2(n2856, n2857, 368u64);
    let n2860: ZW = zw_bits_n(n2265);
    let n2861: ZW = zw_mix1(n2858, n2860, 369u64);
    let n2862: ZW = zw_mix2(n2859, n2860, 369u64);
    let n2863: ZW = zw_bits_n(n2250);
    let n2864: ZW = zw_mix1(n2861, n2863, 20u64);
    let n2865: ZW = zw_mix2(n2862, n2863, 20u64);
    let n2866: ZW = zw_mix1(n2864, n2691, 41u64);
    let n2867: ZW = zw_mix2(n2865, n2691, 41u64);
    let n2868: ZW = zw_bits_n(n2255);
    let n2869: ZW = zw_mix1(n2866, n2868, 282u64);
    let n2870: ZW = zw_mix2(n2867, n2868, 282u64);
    let n2871: ZW = zw_bits_n(n2256);
    let n2872: ZW = zw_mix1(n2869, n2871, 284u64);
    let n2873: ZW = zw_mix2(n2870, n2871, 284u64);
    let n2874: ZW = zw_bits_n(n2257);
    let n2875: ZW = zw_mix1(n2872, n2874, 285u64);
    let n2876: ZW = zw_mix2(n2873, n2874, 285u64);
    let n2877: ZW = zw_bits_n(n2258);
    let n2878: ZW = zw_mix1(n2875, n2877, 287u64);
    let n2879: ZW = zw_mix2(n2876, n2877, 287u64);
    let n2880: ZW = zw_bits_b(n2259);
    let n2881: ZW = zw_mix1(n2878, n2880, 294u64);
    let n2882: ZW = zw_mix2(n2879, n2880, 294u64);
    let n2883: ZW = zw_bits_b(n2260);
    let n2884: ZW = zw_mix1(n2881, n2883, 295u64);
    let n2885: ZW = zw_mix2(n2882, n2883, 295u64);
    let n2886: ZW = zw_bits_n(n2289);
    let n2887: ZW = zw_mix1(n2884, n2886, 301u64);
    let n2888: ZW = zw_mix2(n2885, n2886, 301u64);
    let n2889: ZW = zw_bits_n(r_c358);
    let n2890: ZW = zw_mix1(n2887, n2889, 358u64);
    let n2891: ZW = zw_mix2(n2888, n2889, 358u64);
    let n2892: ZW = zw_bits_n(r_c359);
    let n2893: ZW = zw_mix1(n2890, n2892, 359u64);
    let n2894: ZW = zw_mix2(n2891, n2892, 359u64);
    let n2895: ZW = zw_bits_n(r_c360);
    let n2896: ZW = zw_mix1(n2893, n2895, 360u64);
    let n2897: ZW = zw_mix2(n2894, n2895, 360u64);
    let n2898: ZW = zw_bits_n(r_c361);
    let n2899: ZW = zw_mix1(n2896, n2898, 361u64);
    let n2900: ZW = zw_mix2(n2897, n2898, 361u64);
    let n2901: ZW = zw_bits_b(n2263);
    let n2902: ZW = zw_mix1(n2899, n2901, 362u64);
    let n2903: ZW = zw_mix2(n2900, n2901, 362u64);
    let n2904: ZW = zw_bits_n(n2290);
    let n2905: ZW = zw_mix1(n2902, n2904, 370u64);
    let n2906: ZW = zw_mix2(n2903, n2904, 370u64);
    let n2907: ZW = zw_bits_n(n2267);
    let n2908: ZW = zw_mix1(n2905, n2907, 371u64);
    let n2909: ZW = zw_mix2(n2906, n2907, 371u64);
    let n2910: ZW = zw_bits_b(n2305);
    let n2911: ZW = zw_mix1(n2899, n2910, 362u64);
    let n2912: ZW = zw_mix2(n2900, n2910, 362u64);
    let n2913: ZW = zw_bits_n(n2318);
    let n2914: ZW = zw_mix1(n2911, n2913, 370u64);
    let n2915: ZW = zw_mix2(n2912, n2913, 370u64);
    let n2916: ZW = zw_bits_n(n2307);
    let n2917: ZW = zw_mix1(n2914, n2916, 371u64);
    let n2918: ZW = zw_mix2(n2915, n2916, 371u64);
    let n2919: ZW = zw_bits_b(n2332);
    let n2920: ZW = zw_mix1(n2899, n2919, 362u64);
    let n2921: ZW = zw_mix2(n2900, n2919, 362u64);
    let n2922: ZW = zw_bits_n(n2345);
    let n2923: ZW = zw_mix1(n2920, n2922, 370u64);
    let n2924: ZW = zw_mix2(n2921, n2922, 370u64);
    let n2925: ZW = zw_bits_n(n2334);
    let n2926: ZW = zw_mix1(n2923, n2925, 371u64);
    let n2927: ZW = zw_mix2(n2924, n2925, 371u64);
    let n2928: ZW = zw_bits_n(n2354);
    let n2929: ZW = zw_mix1(n2875, n2928, 287u64);
    let n2930: ZW = zw_mix2(n2876, n2928, 287u64);
    let n2931: ZW = zw_mix1(n2929, n2880, 294u64);
    let n2932: ZW = zw_mix2(n2930, n2880, 294u64);
    let n2933: ZW = zw_bits_b(n2355);
    let n2934: ZW = zw_mix1(n2931, n2933, 295u64);
    let n2935: ZW = zw_mix2(n2932, n2933, 295u64);
    let n2936: ZW = zw_mix1(n2934, n2886, 301u64);
    let n2937: ZW = zw_mix2(n2935, n2886, 301u64);
    let n2938: ZW = zw_mix1(n2936, n2889, 358u64);
    let n2939: ZW = zw_mix2(n2937, n2889, 358u64);
    let n2940: ZW = zw_mix1(n2938, n2892, 359u64);
    let n2941: ZW = zw_mix2(n2939, n2892, 359u64);
    let n2942: ZW = zw_mix1(n2940, n2895, 360u64);
    let n2943: ZW = zw_mix2(n2941, n2895, 360u64);
    let n2944: ZW = zw_mix1(n2942, n2898, 361u64);
    let n2945: ZW = zw_mix2(n2943, n2898, 361u64);
    let n2946: ZW = zw_mix1(n2944, n2901, 362u64);
    let n2947: ZW = zw_mix2(n2945, n2901, 362u64);
    let n2948: ZW = zw_bits_n(n2368);
    let n2949: ZW = zw_mix1(n2946, n2948, 370u64);
    let n2950: ZW = zw_mix2(n2947, n2948, 370u64);
    let n2951: ZW = zw_bits_n(n2357);
    let n2952: ZW = zw_mix1(n2949, n2951, 371u64);
    let n2953: ZW = zw_mix2(n2950, n2951, 371u64);
    let n2954: ZW = zw_mix1(n2944, n2910, 362u64);
    let n2955: ZW = zw_mix2(n2945, n2910, 362u64);
    let n2956: ZW = zw_bits_n(n2387);
    let n2957: ZW = zw_mix1(n2954, n2956, 370u64);
    let n2958: ZW = zw_mix2(n2955, n2956, 370u64);
    let n2959: ZW = zw_bits_n(n2376);
    let n2960: ZW = zw_mix1(n2957, n2959, 371u64);
    let n2961: ZW = zw_mix2(n2958, n2959, 371u64);
    let n2962: ZW = zw_mix1(n2944, n2919, 362u64);
    let n2963: ZW = zw_mix2(n2945, n2919, 362u64);
    let n2964: ZW = zw_bits_n(n2406);
    let n2965: ZW = zw_mix1(n2962, n2964, 370u64);
    let n2966: ZW = zw_mix2(n2963, n2964, 370u64);
    let n2967: ZW = zw_bits_n(n2395);
    let n2968: ZW = zw_mix1(n2965, n2967, 371u64);
    let n2969: ZW = zw_mix2(n2966, n2967, 371u64);
    let n2970: ZW = zw_bits_n(n2427);
    let n2971: ZW = zw_mix1(n2861, n2970, 20u64);
    let n2972: ZW = zw_mix2(n2862, n2970, 20u64);
    let n2973: ZW = zw_bits_b(n2428);
    let n2974: ZW = zw_mix1(n2971, n2973, 41u64);
    let n2975: ZW = zw_mix2(n2972, n2973, 41u64);
    let n2976: ZW = zw_bits_n(n2429);
    let n2977: ZW = zw_mix1(n2974, n2976, 282u64);
    let n2978: ZW = zw_mix2(n2975, n2976, 282u64);
    let n2979: ZW = zw_bits_n(n2430);
    let n2980: ZW = zw_mix1(n2977, n2979, 284u64);
    let n2981: ZW = zw_mix2(n2978, n2979, 284u64);
    let n2982: ZW = zw_bits_n(n2431);
    let n2983: ZW = zw_mix1(n2980, n2982, 285u64);
    let n2984: ZW = zw_mix2(n2981, n2982, 285u64);
    let n2985: ZW = zw_mix1(n2983, n2877, 287u64);
    let n2986: ZW = zw_mix2(n2984, n2877, 287u64);
    let n2987: ZW = zw_bits_b(n2432);
    let n2988: ZW = zw_mix1(n2985, n2987, 294u64);
    let n2989: ZW = zw_mix2(n2986, n2987, 294u64);
    let n2990: ZW = zw_mix1(n2988, n2883, 295u64);
    let n2991: ZW = zw_mix2(n2989, n2883, 295u64);
    let n2992: ZW = zw_bits_n(n2451);
    let n2993: ZW = zw_mix1(n2990, n2992, 301u64);
    let n2994: ZW = zw_mix2(n2991, n2992, 301u64);
    let n2995: ZW = zw_bits_n(n2433);
    let n2996: ZW = zw_mix1(n2993, n2995, 358u64);
    let n2997: ZW = zw_mix2(n2994, n2995, 358u64);
    let n2998: ZW = zw_bits_n(n2434);
    let n2999: ZW = zw_mix1(n2996, n2998, 359u64);
    let n3000: ZW = zw_mix2(n2997, n2998, 359u64);
    let n3001: ZW = zw_bits_n(n2435);
    let n3002: ZW = zw_mix1(n2999, n3001, 360u64);
    let n3003: ZW = zw_mix2(n3000, n3001, 360u64);
    let n3004: ZW = zw_bits_n(n2436);
    let n3005: ZW = zw_mix1(n3002, n3004, 361u64);
    let n3006: ZW = zw_mix2(n3003, n3004, 361u64);
    let n3007: ZW = zw_mix1(n3005, n2901, 362u64);
    let n3008: ZW = zw_mix2(n3006, n2901, 362u64);
    let n3009: ZW = zw_bits_n(n2452);
    let n3010: ZW = zw_mix1(n3007, n3009, 370u64);
    let n3011: ZW = zw_mix2(n3008, n3009, 370u64);
    let n3012: ZW = zw_bits_n(n2438);
    let n3013: ZW = zw_mix1(n3010, n3012, 371u64);
    let n3014: ZW = zw_mix2(n3011, n3012, 371u64);
    let n3015: ZW = zw_bits_n(n2463);
    let n3016: ZW = zw_mix1(n2996, n3015, 359u64);
    let n3017: ZW = zw_mix2(n2997, n3015, 359u64);
    let n3018: ZW = zw_bits_n(n2464);
    let n3019: ZW = zw_mix1(n3016, n3018, 360u64);
    let n3020: ZW = zw_mix2(n3017, n3018, 360u64);
    let n3021: ZW = zw_mix1(n3019, n3004, 361u64);
    let n3022: ZW = zw_mix2(n3020, n3004, 361u64);
    let n3023: ZW = zw_mix1(n3021, n2910, 362u64);
    let n3024: ZW = zw_mix2(n3022, n2910, 362u64);
    let n3025: ZW = zw_bits_n(n2477);
    let n3026: ZW = zw_mix1(n3023, n3025, 370u64);
    let n3027: ZW = zw_mix2(n3024, n3025, 370u64);
    let n3028: ZW = zw_bits_n(n2466);
    let n3029: ZW = zw_mix1(n3026, n3028, 371u64);
    let n3030: ZW = zw_mix2(n3027, n3028, 371u64);
    let n3031: ZW = zw_bits_n(n2486);
    let n3032: ZW = zw_mix1(n3016, n3031, 360u64);
    let n3033: ZW = zw_mix2(n3017, n3031, 360u64);
    let n3034: ZW = zw_mix1(n3032, n3004, 361u64);
    let n3035: ZW = zw_mix2(n3033, n3004, 361u64);
    let n3036: ZW = zw_mix1(n3034, n2919, 362u64);
    let n3037: ZW = zw_mix2(n3035, n2919, 362u64);
    let n3038: ZW = zw_bits_n(n2499);
    let n3039: ZW = zw_mix1(n3036, n3038, 370u64);
    let n3040: ZW = zw_mix2(n3037, n3038, 370u64);
    let n3041: ZW = zw_bits_n(n2488);
    let n3042: ZW = zw_mix1(n3039, n3041, 371u64);
    let n3043: ZW = zw_mix2(n3040, n3041, 371u64);
    let n3044: ZW = zw_bits_n(n2515);
    let n3045: ZW = zw_mix1(n2993, n3044, 358u64);
    let n3046: ZW = zw_mix2(n2994, n3044, 358u64);
    let n3047: ZW = zw_bits_n(n2516);
    let n3048: ZW = zw_mix1(n3045, n3047, 359u64);
    let n3049: ZW = zw_mix2(n3046, n3047, 359u64);
    let n3050: ZW = zw_bits_n(n2517);
    let n3051: ZW = zw_mix1(n3048, n3050, 360u64);
    let n3052: ZW = zw_mix2(n3049, n3050, 360u64);
    let n3053: ZW = zw_bits_n(n2518);
    let n3054: ZW = zw_mix1(n3051, n3053, 361u64);
    let n3055: ZW = zw_mix2(n3052, n3053, 361u64);
    let n3056: ZW = zw_mix1(n3054, n2901, 362u64);
    let n3057: ZW = zw_mix2(n3055, n2901, 362u64);
    let n3058: ZW = zw_bits_n(n2531);
    let n3059: ZW = zw_mix1(n3056, n3058, 370u64);
    let n3060: ZW = zw_mix2(n3057, n3058, 370u64);
    let n3061: ZW = zw_bits_n(n2520);
    let n3062: ZW = zw_mix1(n3059, n3061, 371u64);
    let n3063: ZW = zw_mix2(n3060, n3061, 371u64);
    let n3064: ZW = zw_mix1(n3045, n3015, 359u64);
    let n3065: ZW = zw_mix2(n3046, n3015, 359u64);
    let n3066: ZW = zw_mix1(n3064, n3018, 360u64);
    let n3067: ZW = zw_mix2(n3065, n3018, 360u64);
    let n3068: ZW = zw_mix1(n3066, n3053, 361u64);
    let n3069: ZW = zw_mix2(n3067, n3053, 361u64);
    let n3070: ZW = zw_mix1(n3068, n2910, 362u64);
    let n3071: ZW = zw_mix2(n3069, n2910, 362u64);
    let n3072: ZW = zw_bits_n(n2540);
    let n3073: ZW = zw_mix1(n3070, n3072, 370u64);
    let n3074: ZW = zw_mix2(n3071, n3072, 370u64);
    let n3075: ZW = zw_bits_n(n2538);
    let n3076: ZW = zw_mix1(n3073, n3075, 371u64);
    let n3077: ZW = zw_mix2(n3074, n3075, 371u64);
    let n3078: ZW = zw_mix1(n3064, n3031, 360u64);
    let n3079: ZW = zw_mix2(n3065, n3031, 360u64);
    let n3080: ZW = zw_mix1(n3078, n3053, 361u64);
    let n3081: ZW = zw_mix2(n3079, n3053, 361u64);
    let n3082: ZW = zw_mix1(n3080, n2919, 362u64);
    let n3083: ZW = zw_mix2(n3081, n2919, 362u64);
    let n3084: ZW = zw_bits_n(n2548);
    let n3085: ZW = zw_mix1(n3082, n3084, 370u64);
    let n3086: ZW = zw_mix2(n3083, n3084, 370u64);
    let n3087: ZW = zw_bits_n(n2546);
    let n3088: ZW = zw_mix1(n3085, n3087, 371u64);
    let n3089: ZW = zw_mix2(n3086, n3087, 371u64);
    let n3090: ZW = zw_bits_n(n2553);
    let n3091: ZW = zw_mix1(n3051, n3090, 361u64);
    let n3092: ZW = zw_mix2(n3052, n3090, 361u64);
    let n3093: ZW = zw_mix1(n3091, n2901, 362u64);
    let n3094: ZW = zw_mix2(n3092, n2901, 362u64);
    let n3095: ZW = zw_mix1(n3093, n3058, 370u64);
    let n3096: ZW = zw_mix2(n3094, n3058, 370u64);
    let n3097: ZW = zw_bits_n(n2554);
    let n3098: ZW = zw_mix1(n3095, n3097, 371u64);
    let n3099: ZW = zw_mix2(n3096, n3097, 371u64);
    let n3100: ZW = zw_mix1(n3066, n3090, 361u64);
    let n3101: ZW = zw_mix2(n3067, n3090, 361u64);
    let n3102: ZW = zw_mix1(n3100, n2910, 362u64);
    let n3103: ZW = zw_mix2(n3101, n2910, 362u64);
    let n3104: ZW = zw_mix1(n3102, n3072, 370u64);
    let n3105: ZW = zw_mix2(n3103, n3072, 370u64);
    let n3106: ZW = zw_bits_n(n2557);
    let n3107: ZW = zw_mix1(n3104, n3106, 371u64);
    let n3108: ZW = zw_mix2(n3105, n3106, 371u64);
    let n3109: ZW = zw_mix1(n3078, n3090, 361u64);
    let n3110: ZW = zw_mix2(n3079, n3090, 361u64);
    let n3111: ZW = zw_mix1(n3109, n2919, 362u64);
    let n3112: ZW = zw_mix2(n3110, n2919, 362u64);
    let n3113: ZW = zw_mix1(n3111, n3084, 370u64);
    let n3114: ZW = zw_mix2(n3112, n3084, 370u64);
    let n3115: ZW = zw_bits_n(n2560);
    let n3116: ZW = zw_mix1(n3113, n3115, 371u64);
    let n3117: ZW = zw_mix2(n3114, n3115, 371u64);
    let n3118: ZW = zw_mix1(n2983, n2928, 287u64);
    let n3119: ZW = zw_mix2(n2984, n2928, 287u64);
    let n3120: ZW = zw_mix1(n3118, n2987, 294u64);
    let n3121: ZW = zw_mix2(n3119, n2987, 294u64);
    let n3122: ZW = zw_mix1(n3120, n2933, 295u64);
    let n3123: ZW = zw_mix2(n3121, n2933, 295u64);
    let n3124: ZW = zw_mix1(n3122, n2992, 301u64);
    let n3125: ZW = zw_mix2(n3123, n2992, 301u64);
    let n3126: ZW = zw_mix1(n3124, n2995, 358u64);
    let n3127: ZW = zw_mix2(n3125, n2995, 358u64);
    let n3128: ZW = zw_mix1(n3126, n2998, 359u64);
    let n3129: ZW = zw_mix2(n3127, n2998, 359u64);
    let n3130: ZW = zw_mix1(n3128, n3001, 360u64);
    let n3131: ZW = zw_mix2(n3129, n3001, 360u64);
    let n3132: ZW = zw_mix1(n3130, n3004, 361u64);
    let n3133: ZW = zw_mix2(n3131, n3004, 361u64);
    let n3134: ZW = zw_mix1(n3132, n2901, 362u64);
    let n3135: ZW = zw_mix2(n3133, n2901, 362u64);
    let n3136: ZW = zw_bits_n(n2578);
    let n3137: ZW = zw_mix1(n3134, n3136, 370u64);
    let n3138: ZW = zw_mix2(n3135, n3136, 370u64);
    let n3139: ZW = zw_bits_n(n2567);
    let n3140: ZW = zw_mix1(n3137, n3139, 371u64);
    let n3141: ZW = zw_mix2(n3138, n3139, 371u64);
    let n3142: ZW = zw_mix1(n3126, n3015, 359u64);
    let n3143: ZW = zw_mix2(n3127, n3015, 359u64);
    let n3144: ZW = zw_mix1(n3142, n3018, 360u64);
    let n3145: ZW = zw_mix2(n3143, n3018, 360u64);
    let n3146: ZW = zw_mix1(n3144, n3004, 361u64);
    let n3147: ZW = zw_mix2(n3145, n3004, 361u64);
    let n3148: ZW = zw_mix1(n3146, n2910, 362u64);
    let n3149: ZW = zw_mix2(n3147, n2910, 362u64);
    let n3150: ZW = zw_bits_n(n2597);
    let n3151: ZW = zw_mix1(n3148, n3150, 370u64);
    let n3152: ZW = zw_mix2(n3149, n3150, 370u64);
    let n3153: ZW = zw_bits_n(n2586);
    let n3154: ZW = zw_mix1(n3151, n3153, 371u64);
    let n3155: ZW = zw_mix2(n3152, n3153, 371u64);
    let n3156: ZW = zw_mix1(n3142, n3031, 360u64);
    let n3157: ZW = zw_mix2(n3143, n3031, 360u64);
    let n3158: ZW = zw_mix1(n3156, n3004, 361u64);
    let n3159: ZW = zw_mix2(n3157, n3004, 361u64);
    let n3160: ZW = zw_mix1(n3158, n2919, 362u64);
    let n3161: ZW = zw_mix2(n3159, n2919, 362u64);
    let n3162: ZW = zw_bits_n(n2616);
    let n3163: ZW = zw_mix1(n3160, n3162, 370u64);
    let n3164: ZW = zw_mix2(n3161, n3162, 370u64);
    let n3165: ZW = zw_bits_n(n2605);
    let n3166: ZW = zw_mix1(n3163, n3165, 371u64);
    let n3167: ZW = zw_mix2(n3164, n3165, 371u64);
    let n3168: ZW = zw_mix1(n3124, n3044, 358u64);
    let n3169: ZW = zw_mix2(n3125, n3044, 358u64);
    let n3170: ZW = zw_mix1(n3168, n3047, 359u64);
    let n3171: ZW = zw_mix2(n3169, n3047, 359u64);
    let n3172: ZW = zw_mix1(n3170, n3050, 360u64);
    let n3173: ZW = zw_mix2(n3171, n3050, 360u64);
    let n3174: ZW = zw_mix1(n3172, n3053, 361u64);
    let n3175: ZW = zw_mix2(n3173, n3053, 361u64);
    let n3176: ZW = zw_mix1(n3174, n2901, 362u64);
    let n3177: ZW = zw_mix2(n3175, n2901, 362u64);
    let n3178: ZW = zw_bits_n(n2635);
    let n3179: ZW = zw_mix1(n3176, n3178, 370u64);
    let n3180: ZW = zw_mix2(n3177, n3178, 370u64);
    let n3181: ZW = zw_bits_n(n2624);
    let n3182: ZW = zw_mix1(n3179, n3181, 371u64);
    let n3183: ZW = zw_mix2(n3180, n3181, 371u64);
    let n3184: ZW = zw_mix1(n3168, n3015, 359u64);
    let n3185: ZW = zw_mix2(n3169, n3015, 359u64);
    let n3186: ZW = zw_mix1(n3184, n3018, 360u64);
    let n3187: ZW = zw_mix2(n3185, n3018, 360u64);
    let n3188: ZW = zw_mix1(n3186, n3053, 361u64);
    let n3189: ZW = zw_mix2(n3187, n3053, 361u64);
    let n3190: ZW = zw_mix1(n3188, n2910, 362u64);
    let n3191: ZW = zw_mix2(n3189, n2910, 362u64);
    let n3192: ZW = zw_bits_n(n2644);
    let n3193: ZW = zw_mix1(n3190, n3192, 370u64);
    let n3194: ZW = zw_mix2(n3191, n3192, 370u64);
    let n3195: ZW = zw_bits_n(n2642);
    let n3196: ZW = zw_mix1(n3193, n3195, 371u64);
    let n3197: ZW = zw_mix2(n3194, n3195, 371u64);
    let n3198: ZW = zw_mix1(n3184, n3031, 360u64);
    let n3199: ZW = zw_mix2(n3185, n3031, 360u64);
    let n3200: ZW = zw_mix1(n3198, n3053, 361u64);
    let n3201: ZW = zw_mix2(n3199, n3053, 361u64);
    let n3202: ZW = zw_mix1(n3200, n2919, 362u64);
    let n3203: ZW = zw_mix2(n3201, n2919, 362u64);
    let n3204: ZW = zw_bits_n(n2652);
    let n3205: ZW = zw_mix1(n3202, n3204, 370u64);
    let n3206: ZW = zw_mix2(n3203, n3204, 370u64);
    let n3207: ZW = zw_bits_n(n2650);
    let n3208: ZW = zw_mix1(n3205, n3207, 371u64);
    let n3209: ZW = zw_mix2(n3206, n3207, 371u64);
    let n3210: ZW = zw_mix1(n3172, n3090, 361u64);
    let n3211: ZW = zw_mix2(n3173, n3090, 361u64);
    let n3212: ZW = zw_mix1(n3210, n2901, 362u64);
    let n3213: ZW = zw_mix2(n3211, n2901, 362u64);
    let n3214: ZW = zw_mix1(n3212, n3178, 370u64);
    let n3215: ZW = zw_mix2(n3213, n3178, 370u64);
    let n3216: ZW = zw_bits_n(n2655);
    let n3217: ZW = zw_mix1(n3214, n3216, 371u64);
    let n3218: ZW = zw_mix2(n3215, n3216, 371u64);
    let n3219: ZW = zw_mix1(n3186, n3090, 361u64);
    let n3220: ZW = zw_mix2(n3187, n3090, 361u64);
    let n3221: ZW = zw_mix1(n3219, n2910, 362u64);
    let n3222: ZW = zw_mix2(n3220, n2910, 362u64);
    let n3223: ZW = zw_mix1(n3221, n3192, 370u64);
    let n3224: ZW = zw_mix2(n3222, n3192, 370u64);
    let n3225: ZW = zw_bits_n(n2658);
    let n3226: ZW = zw_mix1(n3223, n3225, 371u64);
    let n3227: ZW = zw_mix2(n3224, n3225, 371u64);
    let n3228: ZW = zw_mix1(n3198, n3090, 361u64);
    let n3229: ZW = zw_mix2(n3199, n3090, 361u64);
    let n3230: ZW = zw_mix1(n3228, n2919, 362u64);
    let n3231: ZW = zw_mix2(n3229, n2919, 362u64);
    let n3232: ZW = zw_mix1(n3230, n3204, 370u64);
    let n3233: ZW = zw_mix2(n3231, n3204, 370u64);
    let n3234: ZW = zw_bits_n(n2661);
    let n3235: ZW = zw_mix1(n3232, n3234, 371u64);
    let n3236: ZW = zw_mix2(n3233, n3234, 371u64);
    let ok_v0_b0: u16 = ALL & zb_holds(n1220) & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129);
    let bd_v0_b0: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v0_b0: u16 = ALL & zb_holds(n124) & zb_holds(n1363) & zb_holds(n1366);
    let ok_v1_b1: u16 = ALL & zb_holds(n1220) & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129);
    let bd_v1_b1: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v1_b1: u16 = ALL & zb_holds(n124) & zb_holds(n1363) & zb_holds(n1432);
    let ok_v2_b2: u16 = ALL & zb_holds(n1220) & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129);
    let bd_v2_b2: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v2_b2: u16 = ALL & zb_holds(n124) & zb_holds(n1363) & zb_holds(n1483);
    let ok_v16_b3: u16 = ALL & zb_holds(n1220) & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129);
    let bd_v16_b3: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v16_b3: u16 = ALL & zb_holds(n124) & zb_holds(n1363) & zb_holds(n1519);
    let ok_v17_b4: u16 = ALL & zb_holds(n1220) & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129);
    let bd_v17_b4: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v17_b4: u16 = ALL & zb_holds(n124) & zb_holds(n1363) & zb_holds(n1555);
    let ok_v18_b5: u16 = ALL & zb_holds(n1220) & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129);
    let bd_v18_b5: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v18_b5: u16 = ALL & zb_holds(n124) & zb_holds(n1363) & zb_holds(n1591);
    let ok_v32_b6: u16 = ALL & zb_holds(n1220) & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129);
    let bd_v32_b6: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v32_b6: u16 = ALL & zb_holds(n1624);
    let ok_v33_b7: u16 = ALL & zb_holds(n1220) & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129);
    let bd_v33_b7: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v33_b7: u16 = ALL & zb_holds(n1635);
    let ok_v34_b8: u16 = ALL & zb_holds(n1220) & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129);
    let bd_v34_b8: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v34_b8: u16 = ALL & zb_holds(n1646);
    let ok_v36_b9: u16 = ALL & zb_holds(n1220) & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129);
    let bd_v36_b9: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v36_b9: u16 = ALL & zb_holds(n1655);
    let ok_v48_b10: u16 = ALL & zb_holds(n1220) & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129);
    let bd_v48_b10: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v48_b10: u16 = ALL & zb_holds(n1678);
    let ok_v49_b11: u16 = ALL & zb_holds(n1220) & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129);
    let bd_v49_b11: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v49_b11: u16 = ALL & zb_holds(n1689);
    let ok_v50_b12: u16 = ALL & zb_holds(n1220) & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129);
    let bd_v50_b12: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v50_b12: u16 = ALL & zb_holds(n1700);
    let ok_v52_b13: u16 = ALL & zb_holds(n1220) & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129);
    let bd_v52_b13: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v52_b13: u16 = ALL & zb_holds(n1709);
    let ok_v0_b14: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n1783);
    let bd_v0_b14: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v0_b14: u16 = ALL & zb_holds(n124) & zb_holds(n1782);
    let ok_v1_b15: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n1827);
    let bd_v1_b15: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v1_b15: u16 = ALL & zb_holds(n124) & zb_holds(n1826);
    let ok_v2_b16: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n1871);
    let bd_v2_b16: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v2_b16: u16 = ALL & zb_holds(n124) & zb_holds(n1870);
    let ok_v16_b17: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n1914);
    let bd_v16_b17: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v16_b17: u16 = ALL & zb_holds(n124) & zb_holds(n1913);
    let ok_v17_b18: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n1957);
    let bd_v17_b18: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v17_b18: u16 = ALL & zb_holds(n124) & zb_holds(n1956);
    let ok_v18_b19: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n2000);
    let bd_v18_b19: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v18_b19: u16 = ALL & zb_holds(n124) & zb_holds(n1999);
    let ok_v32_b20: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n2028);
    let bd_v32_b20: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v32_b20: u16 = ALL & zb_holds(n2031);
    let ok_v33_b21: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n2046);
    let bd_v33_b21: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v33_b21: u16 = ALL & zb_holds(n2049);
    let ok_v34_b22: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n2064);
    let bd_v34_b22: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v34_b22: u16 = ALL & zb_holds(n2067);
    let ok_v36_b23: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n2080);
    let bd_v36_b23: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v36_b23: u16 = ALL & zb_holds(n2083);
    let ok_v48_b24: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n2110);
    let bd_v48_b24: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v48_b24: u16 = ALL & zb_holds(n2113);
    let ok_v49_b25: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n2128);
    let bd_v49_b25: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v49_b25: u16 = ALL & zb_holds(n2131);
    let ok_v50_b26: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n2146);
    let bd_v50_b26: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v50_b26: u16 = ALL & zb_holds(n2149);
    let ok_v52_b27: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n2162);
    let bd_v52_b27: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v52_b27: u16 = ALL & zb_holds(n2165);
    let ok_v0_b28: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n2269);
    let bd_v0_b28: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v0_b28: u16 = ALL & zb_holds(n2291);
    let ok_v1_b29: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n2269);
    let bd_v1_b29: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v1_b29: u16 = ALL & zb_holds(n2319);
    let ok_v2_b30: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n2269);
    let bd_v2_b30: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v2_b30: u16 = ALL & zb_holds(n2346);
    let ok_v16_b31: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n2269);
    let bd_v16_b31: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v16_b31: u16 = ALL & zb_holds(n2369);
    let ok_v17_b32: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n2269);
    let bd_v17_b32: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v17_b32: u16 = ALL & zb_holds(n2388);
    let ok_v18_b33: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n2269);
    let bd_v18_b33: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v18_b33: u16 = ALL & zb_holds(n2407);
    let ok_v32_b34: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n2269);
    let bd_v32_b34: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v32_b34: u16 = ALL & zb_holds(n2453);
    let ok_v33_b35: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n2269);
    let bd_v33_b35: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v33_b35: u16 = ALL & zb_holds(n2478);
    let ok_v34_b36: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n2269);
    let bd_v34_b36: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v34_b36: u16 = ALL & zb_holds(n2500);
    let ok_v36_b37: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n2269);
    let bd_v36_b37: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v36_b37: u16 = ALL & zb_holds(n2532);
    let ok_v37_b38: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n2269);
    let bd_v37_b38: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v37_b38: u16 = ALL & zb_holds(n2478);
    let ok_v38_b39: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n2269);
    let bd_v38_b39: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v38_b39: u16 = ALL & zb_holds(n2500);
    let ok_v40_b40: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n2269);
    let bd_v40_b40: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v40_b40: u16 = ALL & zb_holds(n2532);
    let ok_v41_b41: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n2269);
    let bd_v41_b41: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v41_b41: u16 = ALL & zb_holds(n2478);
    let ok_v42_b42: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n2269);
    let bd_v42_b42: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v42_b42: u16 = ALL & zb_holds(n2500);
    let ok_v48_b43: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n2269);
    let bd_v48_b43: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v48_b43: u16 = ALL & zb_holds(n2579);
    let ok_v49_b44: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n2269);
    let bd_v49_b44: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v49_b44: u16 = ALL & zb_holds(n2598);
    let ok_v50_b45: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n2269);
    let bd_v50_b45: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v50_b45: u16 = ALL & zb_holds(n2617);
    let ok_v52_b46: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n2269);
    let bd_v52_b46: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v52_b46: u16 = ALL & zb_holds(n2636);
    let ok_v53_b47: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n2269);
    let bd_v53_b47: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v53_b47: u16 = ALL & zb_holds(n2598);
    let ok_v54_b48: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n2269);
    let bd_v54_b48: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v54_b48: u16 = ALL & zb_holds(n2617);
    let ok_v56_b49: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n2269);
    let bd_v56_b49: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v56_b49: u16 = ALL & zb_holds(n2636);
    let ok_v57_b50: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n2269);
    let bd_v57_b50: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v57_b50: u16 = ALL & zb_holds(n2598);
    let ok_v58_b51: u16 = ALL & zb_holds(n208) & zb_holds(n178) & zb_holds(r_c297) & zb_holds(n203) & zb_holds(r_c280) & zb_holds(n148) & zb_holds(n180) & zb_holds(n146) & zb_holds(n137) & zb_holds(r_c272) & zb_holds(n123) & zb_holds(n122) & zb_holds(n105) & zb_holds(n104) & zb_holds(n144) & zb_holds(n135) & zb_holds(r_c260) & zb_holds(n155) & zb_holds(n154) & zb_holds(n143) & zb_holds(n134) & zb_holds(r_c252) & zb_holds(n103) & zb_holds(n102) & zb_holds(n100) & zb_holds(n99) & zb_holds(n142) & zb_holds(n131) & zb_holds(r_c240) & zb_holds(n130) & zb_holds(r_c175) & zb_holds(n129) & zb_holds(n2269);
    let bd_v58_b51: bool = !n207 || !n206 || !n205 || !n204 || !n106 || !n121 || !n145 || !n136 || !n133 || !n101 || !n153 || !n132;
    let live_v58_b51: u16 = ALL & zb_holds(n2617);
    let sh0 = KShared0 {
        c87: n1380,
        c84: n111,
        c86: n213,
        c240: n240,
        c253: n241,
        c260: n269,
        c273: n270,
        c85: n214,
    };
    let sh1 = KShared1 {
        c84: n111,
        c86: n213,
        c85: n214,
    };
    let sh2 = KShared2 {
        c87: r_c87,
        c39: r_c39,
        c84: n111,
        c86: n213,
        c241: n2251,
        c254: n2252,
        c261: n2253,
        c274: n2254,
        c368: n2264,
        c369: n2265,
        c302: n2262,
        c85: n214,
    };
    let mut take_0_0: u16 = 0;
    let mut take_0_1: u16 = 0;
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
    // 52 distinct button assignments; per outcome they fall
    // into [2, 14, 24] groups that write identical values.
    declined |= live_v0_b0 & (if bd_v0_b0 { ALL } else { !ok_v0_b0 });
    take_0_0 |= live_v0_b0 & ok_v0_b0 & (if bd_v0_b0 { 0 } else { ALL });
    declined |= live_v1_b1 & (if bd_v1_b1 { ALL } else { !ok_v1_b1 });
    take_0_0 |= live_v1_b1 & ok_v1_b1 & (if bd_v1_b1 { 0 } else { ALL });
    declined |= live_v2_b2 & (if bd_v2_b2 { ALL } else { !ok_v2_b2 });
    take_0_0 |= live_v2_b2 & ok_v2_b2 & (if bd_v2_b2 { 0 } else { ALL });
    declined |= live_v16_b3 & (if bd_v16_b3 { ALL } else { !ok_v16_b3 });
    take_0_0 |= live_v16_b3 & ok_v16_b3 & (if bd_v16_b3 { 0 } else { ALL });
    declined |= live_v17_b4 & (if bd_v17_b4 { ALL } else { !ok_v17_b4 });
    take_0_0 |= live_v17_b4 & ok_v17_b4 & (if bd_v17_b4 { 0 } else { ALL });
    declined |= live_v18_b5 & (if bd_v18_b5 { ALL } else { !ok_v18_b5 });
    take_0_0 |= live_v18_b5 & ok_v18_b5 & (if bd_v18_b5 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        h1: n2692, h2: n2693,
    };
    // body 5: buttons 0x12, forks 0x0
    sink.o0(18, take_0_0, &sh0, &o0);
    declined |= live_v32_b6 & (if bd_v32_b6 { ALL } else { !ok_v32_b6 });
    take_0_1 |= live_v32_b6 & ok_v32_b6 & (if bd_v32_b6 { 0 } else { ALL });
    declined |= live_v33_b7 & (if bd_v33_b7 { ALL } else { !ok_v33_b7 });
    take_0_1 |= live_v33_b7 & ok_v33_b7 & (if bd_v33_b7 { 0 } else { ALL });
    declined |= live_v34_b8 & (if bd_v34_b8 { ALL } else { !ok_v34_b8 });
    take_0_1 |= live_v34_b8 & ok_v34_b8 & (if bd_v34_b8 { 0 } else { ALL });
    declined |= live_v36_b9 & (if bd_v36_b9 { ALL } else { !ok_v36_b9 });
    take_0_1 |= live_v36_b9 & ok_v36_b9 & (if bd_v36_b9 { 0 } else { ALL });
    declined |= live_v48_b10 & (if bd_v48_b10 { ALL } else { !ok_v48_b10 });
    take_0_1 |= live_v48_b10 & ok_v48_b10 & (if bd_v48_b10 { 0 } else { ALL });
    declined |= live_v49_b11 & (if bd_v49_b11 { ALL } else { !ok_v49_b11 });
    take_0_1 |= live_v49_b11 & ok_v49_b11 & (if bd_v49_b11 { 0 } else { ALL });
    declined |= live_v50_b12 & (if bd_v50_b12 { ALL } else { !ok_v50_b12 });
    take_0_1 |= live_v50_b12 & ok_v50_b12 & (if bd_v50_b12 { 0 } else { ALL });
    declined |= live_v52_b13 & (if bd_v52_b13 { ALL } else { !ok_v52_b13 });
    take_0_1 |= live_v52_b13 & ok_v52_b13 & (if bd_v52_b13 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1598,
        c41: n1599,
        h1: n2698, h2: n2699,
    };
    // body 13: buttons 0x34, forks 0x0
    sink.o0(52, take_0_1, &sh0, &o0);
    declined |= live_v0_b14 & (if bd_v0_b14 { ALL } else { !ok_v0_b14 });
    take_1_0 |= live_v0_b14 & ok_v0_b14 & (if bd_v0_b14 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n1785,
        c39: n1786,
        c20: r_c20,
        c38: n1781,
        h1: n2709, h2: n2710,
    };
    // body 14: buttons 0x00, forks 0x0
    sink.o1(0, take_1_0, &sh1, &o1);
    declined |= live_v1_b15 & (if bd_v1_b15 { ALL } else { !ok_v1_b15 });
    take_1_1 |= live_v1_b15 & ok_v1_b15 & (if bd_v1_b15 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n1829,
        c39: n1830,
        c20: r_c20,
        c38: n1825,
        h1: n2718, h2: n2719,
    };
    // body 15: buttons 0x01, forks 0x0
    sink.o1(1, take_1_1, &sh1, &o1);
    declined |= live_v2_b16 & (if bd_v2_b16 { ALL } else { !ok_v2_b16 });
    take_1_2 |= live_v2_b16 & ok_v2_b16 & (if bd_v2_b16 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n1873,
        c39: n1874,
        c20: r_c20,
        c38: n1869,
        h1: n2727, h2: n2728,
    };
    // body 16: buttons 0x02, forks 0x0
    sink.o1(2, take_1_2, &sh1, &o1);
    declined |= live_v16_b17 & (if bd_v16_b17 { ALL } else { !ok_v16_b17 });
    take_1_3 |= live_v16_b17 & ok_v16_b17 & (if bd_v16_b17 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n1916,
        c39: n1917,
        c20: r_c20,
        c38: n1912,
        h1: n2736, h2: n2737,
    };
    // body 17: buttons 0x10, forks 0x0
    sink.o1(16, take_1_3, &sh1, &o1);
    declined |= live_v17_b18 & (if bd_v17_b18 { ALL } else { !ok_v17_b18 });
    take_1_4 |= live_v17_b18 & ok_v17_b18 & (if bd_v17_b18 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n1959,
        c39: n1960,
        c20: r_c20,
        c38: n1955,
        h1: n2745, h2: n2746,
    };
    // body 18: buttons 0x11, forks 0x0
    sink.o1(17, take_1_4, &sh1, &o1);
    declined |= live_v18_b19 & (if bd_v18_b19 { ALL } else { !ok_v18_b19 });
    take_1_5 |= live_v18_b19 & ok_v18_b19 & (if bd_v18_b19 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n2002,
        c39: n2003,
        c20: r_c20,
        c38: n1998,
        h1: n2754, h2: n2755,
    };
    // body 19: buttons 0x12, forks 0x0
    sink.o1(18, take_1_5, &sh1, &o1);
    declined |= live_v32_b20 & (if bd_v32_b20 { ALL } else { !ok_v32_b20 });
    take_1_6 |= live_v32_b20 & ok_v32_b20 & (if bd_v32_b20 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n2032,
        c39: n2033,
        c20: n1598,
        c38: n2026,
        h1: n2765, h2: n2766,
    };
    // body 20: buttons 0x20, forks 0x0
    sink.o1(32, take_1_6, &sh1, &o1);
    declined |= live_v33_b21 & (if bd_v33_b21 { ALL } else { !ok_v33_b21 });
    take_1_7 |= live_v33_b21 & ok_v33_b21 & (if bd_v33_b21 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n2050,
        c39: n2051,
        c20: n1598,
        c38: n2044,
        h1: n2774, h2: n2775,
    };
    // body 21: buttons 0x21, forks 0x0
    sink.o1(33, take_1_7, &sh1, &o1);
    declined |= live_v34_b22 & (if bd_v34_b22 { ALL } else { !ok_v34_b22 });
    take_1_8 |= live_v34_b22 & ok_v34_b22 & (if bd_v34_b22 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n2068,
        c39: n2069,
        c20: n1598,
        c38: n2062,
        h1: n2783, h2: n2784,
    };
    // body 22: buttons 0x22, forks 0x0
    sink.o1(34, take_1_8, &sh1, &o1);
    declined |= live_v36_b23 & (if bd_v36_b23 { ALL } else { !ok_v36_b23 });
    take_1_9 |= live_v36_b23 & ok_v36_b23 & (if bd_v36_b23 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n2084,
        c39: n2085,
        c20: n1598,
        c38: n2078,
        h1: n2792, h2: n2793,
    };
    // body 23: buttons 0x24, forks 0x0
    sink.o1(36, take_1_9, &sh1, &o1);
    declined |= live_v48_b24 & (if bd_v48_b24 { ALL } else { !ok_v48_b24 });
    take_1_10 |= live_v48_b24 & ok_v48_b24 & (if bd_v48_b24 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n2114,
        c39: n2115,
        c20: n1598,
        c38: n2108,
        h1: n2801, h2: n2802,
    };
    // body 24: buttons 0x30, forks 0x0
    sink.o1(48, take_1_10, &sh1, &o1);
    declined |= live_v49_b25 & (if bd_v49_b25 { ALL } else { !ok_v49_b25 });
    take_1_11 |= live_v49_b25 & ok_v49_b25 & (if bd_v49_b25 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n2132,
        c39: n2133,
        c20: n1598,
        c38: n2126,
        h1: n2810, h2: n2811,
    };
    // body 25: buttons 0x31, forks 0x0
    sink.o1(49, take_1_11, &sh1, &o1);
    declined |= live_v50_b26 & (if bd_v50_b26 { ALL } else { !ok_v50_b26 });
    take_1_12 |= live_v50_b26 & ok_v50_b26 & (if bd_v50_b26 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n2150,
        c39: n2151,
        c20: n1598,
        c38: n2144,
        h1: n2819, h2: n2820,
    };
    // body 26: buttons 0x32, forks 0x0
    sink.o1(50, take_1_12, &sh1, &o1);
    declined |= live_v52_b27 & (if bd_v52_b27 { ALL } else { !ok_v52_b27 });
    take_1_13 |= live_v52_b27 & ok_v52_b27 & (if bd_v52_b27 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n2166,
        c39: n2167,
        c20: n1598,
        c38: n2160,
        h1: n2828, h2: n2829,
    };
    // body 27: buttons 0x34, forks 0x0
    sink.o1(52, take_1_13, &sh1, &o1);
    declined |= live_v0_b28 & (if bd_v0_b28 { ALL } else { !ok_v0_b28 });
    take_2_0 |= live_v0_b28 & ok_v0_b28 & (if bd_v0_b28 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2250,
        c41: r_c41,
        c358: r_c358,
        c359: r_c359,
        c282: n2255,
        c360: r_c360,
        c361: r_c361,
        c284: n2256,
        c285: n2257,
        c362: n2263,
        c287: n2258,
        c294: n2259,
        c295: n2260,
        c370: n2290,
        c371: n2267,
        c301: n2289,
        h1: n2908, h2: n2909,
    };
    // body 28: buttons 0x00, forks 0x0
    sink.o2(0, take_2_0, &sh2, &o2);
    declined |= live_v1_b29 & (if bd_v1_b29 { ALL } else { !ok_v1_b29 });
    take_2_1 |= live_v1_b29 & ok_v1_b29 & (if bd_v1_b29 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2250,
        c41: r_c41,
        c358: r_c358,
        c359: r_c359,
        c282: n2255,
        c360: r_c360,
        c361: r_c361,
        c284: n2256,
        c285: n2257,
        c362: n2305,
        c287: n2258,
        c294: n2259,
        c295: n2260,
        c370: n2318,
        c371: n2307,
        c301: n2289,
        h1: n2917, h2: n2918,
    };
    // body 29: buttons 0x01, forks 0x0
    sink.o2(1, take_2_1, &sh2, &o2);
    declined |= live_v2_b30 & (if bd_v2_b30 { ALL } else { !ok_v2_b30 });
    take_2_2 |= live_v2_b30 & ok_v2_b30 & (if bd_v2_b30 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2250,
        c41: r_c41,
        c358: r_c358,
        c359: r_c359,
        c282: n2255,
        c360: r_c360,
        c361: r_c361,
        c284: n2256,
        c285: n2257,
        c362: n2332,
        c287: n2258,
        c294: n2259,
        c295: n2260,
        c370: n2345,
        c371: n2334,
        c301: n2289,
        h1: n2926, h2: n2927,
    };
    // body 30: buttons 0x02, forks 0x0
    sink.o2(2, take_2_2, &sh2, &o2);
    declined |= live_v16_b31 & (if bd_v16_b31 { ALL } else { !ok_v16_b31 });
    take_2_3 |= live_v16_b31 & ok_v16_b31 & (if bd_v16_b31 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2250,
        c41: r_c41,
        c358: r_c358,
        c359: r_c359,
        c282: n2255,
        c360: r_c360,
        c361: r_c361,
        c284: n2256,
        c285: n2257,
        c362: n2263,
        c287: n2354,
        c294: n2259,
        c295: n2355,
        c370: n2368,
        c371: n2357,
        c301: n2289,
        h1: n2952, h2: n2953,
    };
    // body 31: buttons 0x10, forks 0x0
    sink.o2(16, take_2_3, &sh2, &o2);
    declined |= live_v17_b32 & (if bd_v17_b32 { ALL } else { !ok_v17_b32 });
    take_2_4 |= live_v17_b32 & ok_v17_b32 & (if bd_v17_b32 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2250,
        c41: r_c41,
        c358: r_c358,
        c359: r_c359,
        c282: n2255,
        c360: r_c360,
        c361: r_c361,
        c284: n2256,
        c285: n2257,
        c362: n2305,
        c287: n2354,
        c294: n2259,
        c295: n2355,
        c370: n2387,
        c371: n2376,
        c301: n2289,
        h1: n2960, h2: n2961,
    };
    // body 32: buttons 0x11, forks 0x0
    sink.o2(17, take_2_4, &sh2, &o2);
    declined |= live_v18_b33 & (if bd_v18_b33 { ALL } else { !ok_v18_b33 });
    take_2_5 |= live_v18_b33 & ok_v18_b33 & (if bd_v18_b33 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2250,
        c41: r_c41,
        c358: r_c358,
        c359: r_c359,
        c282: n2255,
        c360: r_c360,
        c361: r_c361,
        c284: n2256,
        c285: n2257,
        c362: n2332,
        c287: n2354,
        c294: n2259,
        c295: n2355,
        c370: n2406,
        c371: n2395,
        c301: n2289,
        h1: n2968, h2: n2969,
    };
    // body 33: buttons 0x12, forks 0x0
    sink.o2(18, take_2_5, &sh2, &o2);
    declined |= live_v32_b34 & (if bd_v32_b34 { ALL } else { !ok_v32_b34 });
    take_2_6 |= live_v32_b34 & ok_v32_b34 & (if bd_v32_b34 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2427,
        c41: n2428,
        c358: n2433,
        c359: n2434,
        c282: n2429,
        c360: n2435,
        c361: n2436,
        c284: n2430,
        c285: n2431,
        c362: n2263,
        c287: n2258,
        c294: n2432,
        c295: n2260,
        c370: n2452,
        c371: n2438,
        c301: n2451,
        h1: n3013, h2: n3014,
    };
    // body 34: buttons 0x20, forks 0x0
    sink.o2(32, take_2_6, &sh2, &o2);
    declined |= live_v33_b35 & (if bd_v33_b35 { ALL } else { !ok_v33_b35 });
    take_2_7 |= live_v33_b35 & ok_v33_b35 & (if bd_v33_b35 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2427,
        c41: n2428,
        c358: n2433,
        c359: n2463,
        c282: n2429,
        c360: n2464,
        c361: n2436,
        c284: n2430,
        c285: n2431,
        c362: n2305,
        c287: n2258,
        c294: n2432,
        c295: n2260,
        c370: n2477,
        c371: n2466,
        c301: n2451,
        h1: n3029, h2: n3030,
    };
    // body 35: buttons 0x21, forks 0x0
    sink.o2(33, take_2_7, &sh2, &o2);
    declined |= live_v34_b36 & (if bd_v34_b36 { ALL } else { !ok_v34_b36 });
    take_2_8 |= live_v34_b36 & ok_v34_b36 & (if bd_v34_b36 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2427,
        c41: n2428,
        c358: n2433,
        c359: n2463,
        c282: n2429,
        c360: n2486,
        c361: n2436,
        c284: n2430,
        c285: n2431,
        c362: n2332,
        c287: n2258,
        c294: n2432,
        c295: n2260,
        c370: n2499,
        c371: n2488,
        c301: n2451,
        h1: n3042, h2: n3043,
    };
    // body 36: buttons 0x22, forks 0x0
    sink.o2(34, take_2_8, &sh2, &o2);
    declined |= live_v36_b37 & (if bd_v36_b37 { ALL } else { !ok_v36_b37 });
    take_2_9 |= live_v36_b37 & ok_v36_b37 & (if bd_v36_b37 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2427,
        c41: n2428,
        c358: n2515,
        c359: n2516,
        c282: n2429,
        c360: n2517,
        c361: n2518,
        c284: n2430,
        c285: n2431,
        c362: n2263,
        c287: n2258,
        c294: n2432,
        c295: n2260,
        c370: n2531,
        c371: n2520,
        c301: n2451,
        h1: n3062, h2: n3063,
    };
    // body 37: buttons 0x24, forks 0x0
    sink.o2(36, take_2_9, &sh2, &o2);
    declined |= live_v37_b38 & (if bd_v37_b38 { ALL } else { !ok_v37_b38 });
    take_2_10 |= live_v37_b38 & ok_v37_b38 & (if bd_v37_b38 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2427,
        c41: n2428,
        c358: n2515,
        c359: n2463,
        c282: n2429,
        c360: n2464,
        c361: n2518,
        c284: n2430,
        c285: n2431,
        c362: n2305,
        c287: n2258,
        c294: n2432,
        c295: n2260,
        c370: n2540,
        c371: n2538,
        c301: n2451,
        h1: n3076, h2: n3077,
    };
    // body 38: buttons 0x25, forks 0x0
    sink.o2(37, take_2_10, &sh2, &o2);
    declined |= live_v38_b39 & (if bd_v38_b39 { ALL } else { !ok_v38_b39 });
    take_2_11 |= live_v38_b39 & ok_v38_b39 & (if bd_v38_b39 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2427,
        c41: n2428,
        c358: n2515,
        c359: n2463,
        c282: n2429,
        c360: n2486,
        c361: n2518,
        c284: n2430,
        c285: n2431,
        c362: n2332,
        c287: n2258,
        c294: n2432,
        c295: n2260,
        c370: n2548,
        c371: n2546,
        c301: n2451,
        h1: n3088, h2: n3089,
    };
    // body 39: buttons 0x26, forks 0x0
    sink.o2(38, take_2_11, &sh2, &o2);
    declined |= live_v40_b40 & (if bd_v40_b40 { ALL } else { !ok_v40_b40 });
    take_2_12 |= live_v40_b40 & ok_v40_b40 & (if bd_v40_b40 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2427,
        c41: n2428,
        c358: n2515,
        c359: n2516,
        c282: n2429,
        c360: n2517,
        c361: n2553,
        c284: n2430,
        c285: n2431,
        c362: n2263,
        c287: n2258,
        c294: n2432,
        c295: n2260,
        c370: n2531,
        c371: n2554,
        c301: n2451,
        h1: n3098, h2: n3099,
    };
    // body 40: buttons 0x28, forks 0x0
    sink.o2(40, take_2_12, &sh2, &o2);
    declined |= live_v41_b41 & (if bd_v41_b41 { ALL } else { !ok_v41_b41 });
    take_2_13 |= live_v41_b41 & ok_v41_b41 & (if bd_v41_b41 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2427,
        c41: n2428,
        c358: n2515,
        c359: n2463,
        c282: n2429,
        c360: n2464,
        c361: n2553,
        c284: n2430,
        c285: n2431,
        c362: n2305,
        c287: n2258,
        c294: n2432,
        c295: n2260,
        c370: n2540,
        c371: n2557,
        c301: n2451,
        h1: n3107, h2: n3108,
    };
    // body 41: buttons 0x29, forks 0x0
    sink.o2(41, take_2_13, &sh2, &o2);
    declined |= live_v42_b42 & (if bd_v42_b42 { ALL } else { !ok_v42_b42 });
    take_2_14 |= live_v42_b42 & ok_v42_b42 & (if bd_v42_b42 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2427,
        c41: n2428,
        c358: n2515,
        c359: n2463,
        c282: n2429,
        c360: n2486,
        c361: n2553,
        c284: n2430,
        c285: n2431,
        c362: n2332,
        c287: n2258,
        c294: n2432,
        c295: n2260,
        c370: n2548,
        c371: n2560,
        c301: n2451,
        h1: n3116, h2: n3117,
    };
    // body 42: buttons 0x2a, forks 0x0
    sink.o2(42, take_2_14, &sh2, &o2);
    declined |= live_v48_b43 & (if bd_v48_b43 { ALL } else { !ok_v48_b43 });
    take_2_15 |= live_v48_b43 & ok_v48_b43 & (if bd_v48_b43 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2427,
        c41: n2428,
        c358: n2433,
        c359: n2434,
        c282: n2429,
        c360: n2435,
        c361: n2436,
        c284: n2430,
        c285: n2431,
        c362: n2263,
        c287: n2354,
        c294: n2432,
        c295: n2355,
        c370: n2578,
        c371: n2567,
        c301: n2451,
        h1: n3140, h2: n3141,
    };
    // body 43: buttons 0x30, forks 0x0
    sink.o2(48, take_2_15, &sh2, &o2);
    declined |= live_v49_b44 & (if bd_v49_b44 { ALL } else { !ok_v49_b44 });
    take_2_16 |= live_v49_b44 & ok_v49_b44 & (if bd_v49_b44 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2427,
        c41: n2428,
        c358: n2433,
        c359: n2463,
        c282: n2429,
        c360: n2464,
        c361: n2436,
        c284: n2430,
        c285: n2431,
        c362: n2305,
        c287: n2354,
        c294: n2432,
        c295: n2355,
        c370: n2597,
        c371: n2586,
        c301: n2451,
        h1: n3154, h2: n3155,
    };
    // body 44: buttons 0x31, forks 0x0
    sink.o2(49, take_2_16, &sh2, &o2);
    declined |= live_v50_b45 & (if bd_v50_b45 { ALL } else { !ok_v50_b45 });
    take_2_17 |= live_v50_b45 & ok_v50_b45 & (if bd_v50_b45 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2427,
        c41: n2428,
        c358: n2433,
        c359: n2463,
        c282: n2429,
        c360: n2486,
        c361: n2436,
        c284: n2430,
        c285: n2431,
        c362: n2332,
        c287: n2354,
        c294: n2432,
        c295: n2355,
        c370: n2616,
        c371: n2605,
        c301: n2451,
        h1: n3166, h2: n3167,
    };
    // body 45: buttons 0x32, forks 0x0
    sink.o2(50, take_2_17, &sh2, &o2);
    declined |= live_v52_b46 & (if bd_v52_b46 { ALL } else { !ok_v52_b46 });
    take_2_18 |= live_v52_b46 & ok_v52_b46 & (if bd_v52_b46 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2427,
        c41: n2428,
        c358: n2515,
        c359: n2516,
        c282: n2429,
        c360: n2517,
        c361: n2518,
        c284: n2430,
        c285: n2431,
        c362: n2263,
        c287: n2354,
        c294: n2432,
        c295: n2355,
        c370: n2635,
        c371: n2624,
        c301: n2451,
        h1: n3182, h2: n3183,
    };
    // body 46: buttons 0x34, forks 0x0
    sink.o2(52, take_2_18, &sh2, &o2);
    declined |= live_v53_b47 & (if bd_v53_b47 { ALL } else { !ok_v53_b47 });
    take_2_19 |= live_v53_b47 & ok_v53_b47 & (if bd_v53_b47 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2427,
        c41: n2428,
        c358: n2515,
        c359: n2463,
        c282: n2429,
        c360: n2464,
        c361: n2518,
        c284: n2430,
        c285: n2431,
        c362: n2305,
        c287: n2354,
        c294: n2432,
        c295: n2355,
        c370: n2644,
        c371: n2642,
        c301: n2451,
        h1: n3196, h2: n3197,
    };
    // body 47: buttons 0x35, forks 0x0
    sink.o2(53, take_2_19, &sh2, &o2);
    declined |= live_v54_b48 & (if bd_v54_b48 { ALL } else { !ok_v54_b48 });
    take_2_20 |= live_v54_b48 & ok_v54_b48 & (if bd_v54_b48 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2427,
        c41: n2428,
        c358: n2515,
        c359: n2463,
        c282: n2429,
        c360: n2486,
        c361: n2518,
        c284: n2430,
        c285: n2431,
        c362: n2332,
        c287: n2354,
        c294: n2432,
        c295: n2355,
        c370: n2652,
        c371: n2650,
        c301: n2451,
        h1: n3208, h2: n3209,
    };
    // body 48: buttons 0x36, forks 0x0
    sink.o2(54, take_2_20, &sh2, &o2);
    declined |= live_v56_b49 & (if bd_v56_b49 { ALL } else { !ok_v56_b49 });
    take_2_21 |= live_v56_b49 & ok_v56_b49 & (if bd_v56_b49 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2427,
        c41: n2428,
        c358: n2515,
        c359: n2516,
        c282: n2429,
        c360: n2517,
        c361: n2553,
        c284: n2430,
        c285: n2431,
        c362: n2263,
        c287: n2354,
        c294: n2432,
        c295: n2355,
        c370: n2635,
        c371: n2655,
        c301: n2451,
        h1: n3217, h2: n3218,
    };
    // body 49: buttons 0x38, forks 0x0
    sink.o2(56, take_2_21, &sh2, &o2);
    declined |= live_v57_b50 & (if bd_v57_b50 { ALL } else { !ok_v57_b50 });
    take_2_22 |= live_v57_b50 & ok_v57_b50 & (if bd_v57_b50 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2427,
        c41: n2428,
        c358: n2515,
        c359: n2463,
        c282: n2429,
        c360: n2464,
        c361: n2553,
        c284: n2430,
        c285: n2431,
        c362: n2305,
        c287: n2354,
        c294: n2432,
        c295: n2355,
        c370: n2644,
        c371: n2658,
        c301: n2451,
        h1: n3226, h2: n3227,
    };
    // body 50: buttons 0x39, forks 0x0
    sink.o2(57, take_2_22, &sh2, &o2);
    declined |= live_v58_b51 & (if bd_v58_b51 { ALL } else { !ok_v58_b51 });
    take_2_23 |= live_v58_b51 & ok_v58_b51 & (if bd_v58_b51 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2427,
        c41: n2428,
        c358: n2515,
        c359: n2463,
        c282: n2429,
        c360: n2486,
        c361: n2553,
        c284: n2430,
        c285: n2431,
        c362: n2332,
        c287: n2354,
        c294: n2432,
        c295: n2355,
        c370: n2652,
        c371: n2661,
        c301: n2451,
        h1: n3235, h2: n3236,
    };
    // body 51: buttons 0x3a, forks 0x0
    sink.o2(58, take_2_23, &sh2, &o2);
    declined
}
