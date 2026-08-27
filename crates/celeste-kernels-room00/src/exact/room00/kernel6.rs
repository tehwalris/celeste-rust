// GENERATED from a TRACED frame (shape 6). Do not edit.
//
// One input shape, 6 output shapes, 78 distinct button
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
pub const SHAPE: u64 = 5354716000392713856;

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
];

/// Block-uniform inputs, in `UNI_SLOTS` order.
pub struct Uni {
    pub c300: P8,
    pub c301: P8,
    pub c302: P8,
    pub c303: P8,
    pub c314: P8,
    pub c315: P8,
    pub c316: P8,
    pub c317: P8,
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
    ("objects[0].rem.x", "num"),
    ("objects[0].rem.y", "num"),
    ("objects[0].solids", "bool"),
    ("objects[0].spd.x", "num"),
    ("objects[0].spd.y", "num"),
    ("objects[0].spr", "num"),
    ("objects[0].x", "num"),
    ("objects[0].y", "num"),
    ("objects[1].collideable", "bool"),
    ("objects[1].dash_accel.x", "num"),
    ("objects[1].dash_accel.y", "num"),
    ("objects[1].dash_effect_time", "num"),
    ("objects[1].dash_target.x", "num"),
    ("objects[1].dash_target.y", "num"),
    ("objects[1].dash_time", "num"),
    ("objects[1].djump", "num"),
    ("objects[1].flip.x", "bool"),
    ("objects[1].flip.y", "bool"),
    ("objects[1].grace", "num"),
    ("objects[1].p_dash", "bool"),
    ("objects[1].p_jump", "bool"),
    ("objects[1].rem.x", "num"),
    ("objects[1].rem.y", "num"),
    ("objects[1].solids", "bool"),
    ("objects[1].spd.x", "num"),
    ("objects[1].spd.y", "num"),
    ("objects[1].x", "num"),
    ("objects[1].y", "num"),
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
    pub c234: u16,
    pub c298: u16,
    pub c299: u16,
    pub c304: ZN,
    pub c305: ZN,
    pub c243: u16,
    pub c306: ZN,
    pub c307: ZN,
    pub c245: ZN,
    pub c247: ZN,
    pub c248: ZN,
    pub c251: u16,
    pub c308: ZN,
    pub c309: ZN,
    pub c253: ZN,
    pub c310: ZN,
    pub c311: ZN,
    pub c255: ZN,
    pub c256: ZN,
    pub c312: u16,
    pub c313: u16,
    pub c258: ZN,
    pub c265: u16,
    pub c266: u16,
    pub c318: ZN,
    pub c319: ZN,
    pub c268: u16,
    pub c320: ZN,
    pub c321: ZN,
    pub c272: ZN,
    pub c273: ZN,
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
    pub c234: u32,
    pub c298: u32,
    pub c299: u32,
    pub c304: u32,
    pub c305: u32,
    pub c243: u32,
    pub c306: u32,
    pub c307: u32,
    pub c245: u32,
    pub c247: u32,
    pub c248: u32,
    pub c251: u32,
    pub c308: u32,
    pub c309: u32,
    pub c253: u32,
    pub c310: u32,
    pub c311: u32,
    pub c255: u32,
    pub c256: u32,
    pub c312: u32,
    pub c313: u32,
    pub c258: u32,
    pub c265: u32,
    pub c266: u32,
    pub c318: u32,
    pub c319: u32,
    pub c268: u32,
    pub c320: u32,
    pub c321: u32,
    pub c272: u32,
    pub c273: u32,
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
        c300: match &b.cols[cell("objects[0].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c301: match &b.cols[cell("objects[0].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c302: match &b.cols[cell("objects[0].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c303: match &b.cols[cell("objects[0].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c314: match &b.cols[cell("objects[1].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c315: match &b.cols[cell("objects[1].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c316: match &b.cols[cell("objects[1].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c317: match &b.cols[cell("objects[1].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
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
        c234: cell("objects[0].collideable")?,
        c298: cell("objects[0].flip.x")?,
        c299: cell("objects[0].flip.y")?,
        c304: cell("objects[0].rem.x")?,
        c305: cell("objects[0].rem.y")?,
        c243: cell("objects[0].solids")?,
        c306: cell("objects[0].spd.x")?,
        c307: cell("objects[0].spd.y")?,
        c245: cell("objects[0].spr")?,
        c247: cell("objects[0].x")?,
        c248: cell("objects[0].y")?,
        c251: cell("objects[1].collideable")?,
        c308: cell("objects[1].dash_accel.x")?,
        c309: cell("objects[1].dash_accel.y")?,
        c253: cell("objects[1].dash_effect_time")?,
        c310: cell("objects[1].dash_target.x")?,
        c311: cell("objects[1].dash_target.y")?,
        c255: cell("objects[1].dash_time")?,
        c256: cell("objects[1].djump")?,
        c312: cell("objects[1].flip.x")?,
        c313: cell("objects[1].flip.y")?,
        c258: cell("objects[1].grace")?,
        c265: cell("objects[1].p_dash")?,
        c266: cell("objects[1].p_jump")?,
        c318: cell("objects[1].rem.x")?,
        c319: cell("objects[1].rem.y")?,
        c268: cell("objects[1].solids")?,
        c320: cell("objects[1].spd.x")?,
        c321: cell("objects[1].spd.y")?,
        c272: cell("objects[1].x")?,
        c273: cell("objects[1].y")?,
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
        c234: match &b.cols[s.c234 as usize] {
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
        c298: match &b.cols[s.c298 as usize] {
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
        c299: match &b.cols[s.c299 as usize] {
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
        c305: match &b.cols[s.c305 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c243: match &b.cols[s.c243 as usize] {
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
        c306: match &b.cols[s.c306 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c307: match &b.cols[s.c307 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c245: match &b.cols[s.c245 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c247: match &b.cols[s.c247 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c248: match &b.cols[s.c248 as usize] {
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
        c308: match &b.cols[s.c308 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c309: match &b.cols[s.c309 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c253: match &b.cols[s.c253 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c310: match &b.cols[s.c310 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c311: match &b.cols[s.c311 as usize] {
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
        c258: match &b.cols[s.c258 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c265: match &b.cols[s.c265 as usize] {
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
        c266: match &b.cols[s.c266 as usize] {
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
        c320: match &b.cols[s.c320 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c321: match &b.cols[s.c321 as usize] {
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
    (176, "balloon.tile"),
    (203, "big_chest.tile"),
    (195, "chest.if_not_fruit"),
    (197, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (189, "fake_wall.if_not_fruit"),
    (190, "fake_wall.tile"),
    (179, "fall_floor.tile"),
    (185, "fly_fruit.if_not_fruit"),
    (187, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (181, "fruit.if_not_fruit"),
    (183, "fruit.tile"),
    (171, "got_fruit[0]"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (192, "key.if_not_fruit"),
    (193, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (234, "objects[0].collideable"),
    (270, "objects[0].dash_accel.x"),
    (271, "objects[0].dash_accel.y"),
    (236, "objects[0].dash_effect_time"),
    (272, "objects[0].dash_target.x"),
    (273, "objects[0].dash_target.y"),
    (238, "objects[0].dash_time"),
    (239, "objects[0].djump"),
    (274, "objects[0].flip.x"),
    (275, "objects[0].flip.y"),
    (241, "objects[0].grace"),
    (276, "objects[0].hitbox.h"),
    (277, "objects[0].hitbox.w"),
    (278, "objects[0].hitbox.x"),
    (279, "objects[0].hitbox.y"),
    (248, "objects[0].p_dash"),
    (249, "objects[0].p_jump"),
    (280, "objects[0].rem.x"),
    (281, "objects[0].rem.y"),
    (251, "objects[0].solids"),
    (282, "objects[0].spd.x"),
    (283, "objects[0].spd.y"),
    (255, "objects[0].x"),
    (256, "objects[0].y"),
    (43, "pause_player"),
    (157, "player_spawn.tile"),
    (159, "room.x"),
    (160, "room.y"),
    (85, "seconds"),
    (173, "spring.tile"),
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
    SCell::Arr(&[151, 152]),
    SCell::Obj(&[(7, 153), (5, 154), (6, 155)]),
    SCell::Obj(&[(5, 156), (8, 157), (6, 158)]),
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
    SCell::Obj(&[(1, 159), (2, 160)]),
    SCell::Arr(&[161, 162, 163, 164, 165, 166, 167, 168, 169, 170]),
    SCell::Arr(&[171]),
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 172), (8, 173), (6, 174)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 175), (8, 176), (6, 177)]),
    SCell::Obj(&[(5, 178), (8, 179), (6, 180)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 181), (5, 182), (8, 183), (6, 184)]),
    SCell::Obj(&[(9, 185), (5, 186), (8, 187), (6, 188)]),
    SCell::Obj(&[(9, 189), (8, 190), (6, 191)]),
    SCell::Obj(&[(9, 192), (8, 193), (6, 194)]),
    SCell::Obj(&[(9, 195), (5, 196), (8, 197), (6, 198)]),
    SCell::Obj(&[(5, 199), (6, 200)]),
    SCell::Obj(&[(7, 201), (5, 202), (8, 203)]),
    SCell::Obj(&[(7, 204), (5, 205)]),
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
    SCell::Obj(&[(21, 232), (20, 233), (11, 234), (28, 235), (33, 236), (27, 237), (26, 238), (30, 239), (14, 240), (29, 241), (15, 242), (19, 243), (18, 244), (22, 245), (23, 246), (24, 247), (32, 248), (31, 249), (4, 250), (12, 251), (3, 252), (13, 253), (0, 254), (1, 255), (2, 256)]),
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
    SCell::Clo(26, &[206]),
    SCell::Clo(25, &[206]),
    SCell::Obj(&[(1, 270), (2, 271)]),
    SCell::Obj(&[(1, 272), (2, 273)]),
    SCell::Obj(&[(1, 274), (2, 275)]),
    SCell::Obj(&[(17, 276), (16, 277), (1, 278), (2, 279)]),
    SCell::Clo(24, &[206]),
    SCell::Clo(23, &[206]),
    SCell::Clo(27, &[206]),
    SCell::Clo(28, &[206]),
    SCell::Clo(29, &[206]),
    SCell::Obj(&[(1, 280), (2, 281)]),
    SCell::Obj(&[(1, 282), (2, 283)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
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
    (151, 206),
    (153, 207),
    (154, 208),
    (155, 209),
    (156, 210),
    (158, 211),
    (161, 94),
    (162, 116),
    (163, 118),
    (164, 119),
    (165, 121),
    (166, 122),
    (167, 123),
    (168, 124),
    (169, 125),
    (170, 127),
    (172, 212),
    (174, 213),
    (175, 214),
    (177, 215),
    (178, 216),
    (180, 217),
    (182, 218),
    (184, 219),
    (186, 220),
    (188, 221),
    (191, 222),
    (194, 223),
    (196, 224),
    (198, 225),
    (199, 226),
    (200, 227),
    (201, 228),
    (202, 229),
    (204, 230),
    (205, 231),
    (232, 257),
    (233, 258),
    (235, 259),
    (237, 260),
    (240, 261),
    (242, 262),
    (243, 263),
    (244, 264),
    (245, 265),
    (246, 266),
    (247, 267),
    (250, 268),
    (252, 269),
    (254, 93),
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
    pub c280: ZN,
    pub c281: ZN,
    pub c256: ZN,
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
    pub c270: ZN,
    pub c271: ZN,
    pub c236: ZN,
    pub c272: ZN,
    pub c273: ZN,
    pub c238: ZN,
    pub c274: ZB,
    pub c241: ZN,
    pub c248: ZB,
    pub c249: ZB,
    pub c282: ZN,
    pub c283: ZN,
    pub c255: ZN,
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
    (175, "balloon.tile"),
    (202, "big_chest.tile"),
    (194, "chest.if_not_fruit"),
    (196, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (188, "fake_wall.if_not_fruit"),
    (189, "fake_wall.tile"),
    (178, "fall_floor.tile"),
    (184, "fly_fruit.if_not_fruit"),
    (186, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (180, "fruit.if_not_fruit"),
    (182, "fruit.tile"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (191, "key.if_not_fruit"),
    (192, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (234, "objects[0].collideable"),
    (300, "objects[0].dash_accel.x"),
    (301, "objects[0].dash_accel.y"),
    (236, "objects[0].dash_effect_time"),
    (302, "objects[0].dash_target.x"),
    (303, "objects[0].dash_target.y"),
    (238, "objects[0].dash_time"),
    (239, "objects[0].djump"),
    (304, "objects[0].flip.x"),
    (305, "objects[0].flip.y"),
    (241, "objects[0].grace"),
    (306, "objects[0].hitbox.h"),
    (307, "objects[0].hitbox.w"),
    (308, "objects[0].hitbox.x"),
    (309, "objects[0].hitbox.y"),
    (248, "objects[0].p_dash"),
    (249, "objects[0].p_jump"),
    (310, "objects[0].rem.x"),
    (311, "objects[0].rem.y"),
    (251, "objects[0].solids"),
    (312, "objects[0].spd.x"),
    (313, "objects[0].spd.y"),
    (255, "objects[0].x"),
    (256, "objects[0].y"),
    (259, "objects[1].collideable"),
    (314, "objects[1].flip.x"),
    (315, "objects[1].flip.y"),
    (316, "objects[1].hitbox.h"),
    (317, "objects[1].hitbox.w"),
    (318, "objects[1].hitbox.x"),
    (319, "objects[1].hitbox.y"),
    (267, "objects[1].off"),
    (320, "objects[1].rem.x"),
    (321, "objects[1].rem.y"),
    (269, "objects[1].solids"),
    (322, "objects[1].spd.x"),
    (323, "objects[1].spd.y"),
    (271, "objects[1].spr"),
    (272, "objects[1].start"),
    (274, "objects[1].x"),
    (275, "objects[1].y"),
    (43, "pause_player"),
    (157, "player_spawn.tile"),
    (159, "room.x"),
    (160, "room.y"),
    (85, "seconds"),
    (172, "spring.tile"),
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
    SCell::Arr(&[151, 152]),
    SCell::Obj(&[(7, 153), (5, 154), (6, 155)]),
    SCell::Obj(&[(5, 156), (8, 157), (6, 158)]),
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
    SCell::Obj(&[(1, 159), (2, 160)]),
    SCell::Arr(&[161, 162, 163, 164, 165, 166, 167, 168, 169, 170]),
    SCell::Unk,
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 171), (8, 172), (6, 173)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 174), (8, 175), (6, 176)]),
    SCell::Obj(&[(5, 177), (8, 178), (6, 179)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 180), (5, 181), (8, 182), (6, 183)]),
    SCell::Obj(&[(9, 184), (5, 185), (8, 186), (6, 187)]),
    SCell::Obj(&[(9, 188), (8, 189), (6, 190)]),
    SCell::Obj(&[(9, 191), (8, 192), (6, 193)]),
    SCell::Obj(&[(9, 194), (5, 195), (8, 196), (6, 197)]),
    SCell::Obj(&[(5, 198), (6, 199)]),
    SCell::Obj(&[(7, 200), (5, 201), (8, 202)]),
    SCell::Obj(&[(7, 203), (5, 204)]),
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
    SCell::Obj(&[(21, 232), (20, 233), (11, 234), (28, 235), (33, 236), (27, 237), (26, 238), (30, 239), (14, 240), (29, 241), (15, 242), (19, 243), (18, 244), (22, 245), (23, 246), (24, 247), (32, 248), (31, 249), (4, 250), (12, 251), (3, 252), (13, 253), (0, 254), (1, 255), (2, 256)]),
    SCell::Obj(&[(21, 257), (20, 258), (11, 259), (14, 260), (15, 261), (19, 262), (18, 263), (22, 264), (23, 265), (24, 266), (41, 267), (4, 268), (12, 269), (3, 270), (13, 271), (38, 272), (0, 273), (1, 274), (2, 275)]),
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
    SCell::Clo(26, &[205]),
    SCell::Clo(25, &[205]),
    SCell::Obj(&[(1, 300), (2, 301)]),
    SCell::Obj(&[(1, 302), (2, 303)]),
    SCell::Obj(&[(1, 304), (2, 305)]),
    SCell::Obj(&[(17, 306), (16, 307), (1, 308), (2, 309)]),
    SCell::Clo(24, &[205]),
    SCell::Clo(23, &[205]),
    SCell::Clo(27, &[205]),
    SCell::Clo(28, &[205]),
    SCell::Clo(29, &[205]),
    SCell::Obj(&[(1, 310), (2, 311)]),
    SCell::Obj(&[(1, 312), (2, 313)]),
    SCell::Clo(26, &[206]),
    SCell::Clo(25, &[206]),
    SCell::Obj(&[(1, 314), (2, 315)]),
    SCell::Obj(&[(17, 316), (16, 317), (1, 318), (2, 319)]),
    SCell::Clo(24, &[206]),
    SCell::Clo(23, &[206]),
    SCell::Clo(27, &[206]),
    SCell::Clo(28, &[206]),
    SCell::Clo(29, &[206]),
    SCell::Obj(&[(1, 320), (2, 321)]),
    SCell::Obj(&[(1, 322), (2, 323)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
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
    (151, 205),
    (152, 206),
    (153, 207),
    (154, 208),
    (155, 209),
    (156, 210),
    (158, 211),
    (161, 94),
    (162, 116),
    (163, 118),
    (164, 119),
    (165, 121),
    (166, 122),
    (167, 123),
    (168, 124),
    (169, 125),
    (170, 127),
    (171, 212),
    (173, 213),
    (174, 214),
    (176, 215),
    (177, 216),
    (179, 217),
    (181, 218),
    (183, 219),
    (185, 220),
    (187, 221),
    (190, 222),
    (193, 223),
    (195, 224),
    (197, 225),
    (198, 226),
    (199, 227),
    (200, 228),
    (201, 229),
    (203, 230),
    (204, 231),
    (232, 276),
    (233, 277),
    (235, 278),
    (237, 279),
    (240, 280),
    (242, 281),
    (243, 282),
    (244, 283),
    (245, 284),
    (246, 285),
    (247, 286),
    (250, 287),
    (252, 288),
    (254, 93),
    (257, 289),
    (258, 290),
    (260, 291),
    (261, 292),
    (262, 293),
    (263, 294),
    (264, 295),
    (265, 296),
    (266, 297),
    (268, 298),
    (270, 299),
    (273, 121),
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
    pub c310: ZN,
    pub c311: ZN,
    pub c256: ZN,
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
    pub c300: ZN,
    pub c301: ZN,
    pub c236: ZN,
    pub c302: ZN,
    pub c303: ZN,
    pub c238: ZN,
    pub c239: ZN,
    pub c304: ZB,
    pub c241: ZN,
    pub c248: ZB,
    pub c249: ZB,
    pub c312: ZN,
    pub c313: ZN,
    pub c255: ZN,
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
    (175, "balloon.tile"),
    (202, "big_chest.tile"),
    (194, "chest.if_not_fruit"),
    (196, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (188, "fake_wall.if_not_fruit"),
    (189, "fake_wall.tile"),
    (178, "fall_floor.tile"),
    (184, "fly_fruit.if_not_fruit"),
    (186, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (180, "fruit.if_not_fruit"),
    (182, "fruit.tile"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (191, "key.if_not_fruit"),
    (192, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (233, "objects[0].collideable"),
    (261, "objects[0].flip.x"),
    (262, "objects[0].flip.y"),
    (263, "objects[0].hitbox.h"),
    (264, "objects[0].hitbox.w"),
    (265, "objects[0].hitbox.x"),
    (266, "objects[0].hitbox.y"),
    (241, "objects[0].off"),
    (267, "objects[0].rem.x"),
    (268, "objects[0].rem.y"),
    (243, "objects[0].solids"),
    (269, "objects[0].spd.x"),
    (270, "objects[0].spd.y"),
    (245, "objects[0].spr"),
    (246, "objects[0].start"),
    (248, "objects[0].x"),
    (249, "objects[0].y"),
    (43, "pause_player"),
    (157, "player_spawn.tile"),
    (159, "room.x"),
    (160, "room.y"),
    (85, "seconds"),
    (172, "spring.tile"),
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
    SCell::Arr(&[151, 152]),
    SCell::Obj(&[(7, 153), (5, 154), (6, 155)]),
    SCell::Obj(&[(5, 156), (8, 157), (6, 158)]),
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
    SCell::Obj(&[(1, 159), (2, 160)]),
    SCell::Arr(&[161, 162, 163, 164, 165, 166, 167, 168, 169, 170]),
    SCell::Unk,
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 171), (8, 172), (6, 173)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 174), (8, 175), (6, 176)]),
    SCell::Obj(&[(5, 177), (8, 178), (6, 179)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 180), (5, 181), (8, 182), (6, 183)]),
    SCell::Obj(&[(9, 184), (5, 185), (8, 186), (6, 187)]),
    SCell::Obj(&[(9, 188), (8, 189), (6, 190)]),
    SCell::Obj(&[(9, 191), (8, 192), (6, 193)]),
    SCell::Obj(&[(9, 194), (5, 195), (8, 196), (6, 197)]),
    SCell::Obj(&[(5, 198), (6, 199)]),
    SCell::Obj(&[(7, 200), (5, 201), (8, 202)]),
    SCell::Obj(&[(7, 203), (5, 204)]),
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
    SCell::Obj(&[(21, 231), (20, 232), (11, 233), (14, 234), (15, 235), (19, 236), (18, 237), (22, 238), (23, 239), (24, 240), (41, 241), (4, 242), (12, 243), (3, 244), (13, 245), (38, 246), (0, 247), (1, 248), (2, 249)]),
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
    SCell::Clo(26, &[205]),
    SCell::Clo(25, &[205]),
    SCell::Obj(&[(1, 261), (2, 262)]),
    SCell::Obj(&[(17, 263), (16, 264), (1, 265), (2, 266)]),
    SCell::Clo(24, &[205]),
    SCell::Clo(23, &[205]),
    SCell::Clo(27, &[205]),
    SCell::Clo(28, &[205]),
    SCell::Clo(29, &[205]),
    SCell::Obj(&[(1, 267), (2, 268)]),
    SCell::Obj(&[(1, 269), (2, 270)]),
    SCell::Val,
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
    (151, 205),
    (153, 206),
    (154, 207),
    (155, 208),
    (156, 209),
    (158, 210),
    (161, 94),
    (162, 116),
    (163, 118),
    (164, 119),
    (165, 121),
    (166, 122),
    (167, 123),
    (168, 124),
    (169, 125),
    (170, 127),
    (171, 211),
    (173, 212),
    (174, 213),
    (176, 214),
    (177, 215),
    (179, 216),
    (181, 217),
    (183, 218),
    (185, 219),
    (187, 220),
    (190, 221),
    (193, 222),
    (195, 223),
    (197, 224),
    (198, 225),
    (199, 226),
    (200, 227),
    (201, 228),
    (203, 229),
    (204, 230),
    (231, 250),
    (232, 251),
    (234, 252),
    (235, 253),
    (236, 254),
    (237, 255),
    (238, 256),
    (239, 257),
    (240, 258),
    (242, 259),
    (244, 260),
    (247, 121),
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
    (175, "balloon.tile"),
    (202, "big_chest.tile"),
    (194, "chest.if_not_fruit"),
    (196, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (188, "fake_wall.if_not_fruit"),
    (189, "fake_wall.tile"),
    (178, "fall_floor.tile"),
    (184, "fly_fruit.if_not_fruit"),
    (186, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (180, "fruit.if_not_fruit"),
    (182, "fruit.tile"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (191, "key.if_not_fruit"),
    (192, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (233, "objects[0].collideable"),
    (259, "objects[0].flip.x"),
    (260, "objects[0].flip.y"),
    (261, "objects[0].hitbox.h"),
    (262, "objects[0].hitbox.w"),
    (263, "objects[0].hitbox.x"),
    (264, "objects[0].hitbox.y"),
    (265, "objects[0].rem.x"),
    (266, "objects[0].rem.y"),
    (242, "objects[0].solids"),
    (267, "objects[0].spd.x"),
    (268, "objects[0].spd.y"),
    (244, "objects[0].spr"),
    (246, "objects[0].x"),
    (247, "objects[0].y"),
    (43, "pause_player"),
    (157, "player_spawn.tile"),
    (159, "room.x"),
    (160, "room.y"),
    (85, "seconds"),
    (172, "spring.tile"),
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
    SCell::Arr(&[151, 152]),
    SCell::Obj(&[(7, 153), (5, 154), (6, 155)]),
    SCell::Obj(&[(5, 156), (8, 157), (6, 158)]),
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
    SCell::Obj(&[(1, 159), (2, 160)]),
    SCell::Arr(&[161, 162, 163, 164, 165, 166, 167, 168, 169, 170]),
    SCell::Unk,
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 171), (8, 172), (6, 173)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 174), (8, 175), (6, 176)]),
    SCell::Obj(&[(5, 177), (8, 178), (6, 179)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 180), (5, 181), (8, 182), (6, 183)]),
    SCell::Obj(&[(9, 184), (5, 185), (8, 186), (6, 187)]),
    SCell::Obj(&[(9, 188), (8, 189), (6, 190)]),
    SCell::Obj(&[(9, 191), (8, 192), (6, 193)]),
    SCell::Obj(&[(9, 194), (5, 195), (8, 196), (6, 197)]),
    SCell::Obj(&[(5, 198), (6, 199)]),
    SCell::Obj(&[(7, 200), (5, 201), (8, 202)]),
    SCell::Obj(&[(7, 203), (5, 204)]),
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
    SCell::Obj(&[(21, 231), (20, 232), (11, 233), (14, 234), (15, 235), (19, 236), (18, 237), (22, 238), (23, 239), (24, 240), (4, 241), (12, 242), (3, 243), (13, 244), (0, 245), (1, 246), (2, 247)]),
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
    SCell::Clo(26, &[205]),
    SCell::Clo(25, &[205]),
    SCell::Obj(&[(1, 259), (2, 260)]),
    SCell::Obj(&[(17, 261), (16, 262), (1, 263), (2, 264)]),
    SCell::Clo(24, &[205]),
    SCell::Clo(23, &[205]),
    SCell::Clo(27, &[205]),
    SCell::Clo(28, &[205]),
    SCell::Clo(29, &[205]),
    SCell::Obj(&[(1, 265), (2, 266)]),
    SCell::Obj(&[(1, 267), (2, 268)]),
    SCell::Val,
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
    (151, 205),
    (153, 206),
    (154, 207),
    (155, 208),
    (156, 209),
    (158, 210),
    (161, 94),
    (162, 116),
    (163, 118),
    (164, 119),
    (165, 121),
    (166, 122),
    (167, 123),
    (168, 124),
    (169, 125),
    (170, 127),
    (171, 211),
    (173, 212),
    (174, 213),
    (176, 214),
    (177, 215),
    (179, 216),
    (181, 217),
    (183, 218),
    (185, 219),
    (187, 220),
    (190, 221),
    (193, 222),
    (195, 223),
    (197, 224),
    (198, 225),
    (199, 226),
    (200, 227),
    (201, 228),
    (203, 229),
    (204, 230),
    (231, 248),
    (232, 249),
    (234, 250),
    (235, 251),
    (236, 252),
    (237, 253),
    (238, 254),
    (239, 255),
    (240, 256),
    (241, 257),
    (243, 258),
    (245, 123),
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
    (175, "balloon.tile"),
    (202, "big_chest.tile"),
    (194, "chest.if_not_fruit"),
    (196, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (188, "fake_wall.if_not_fruit"),
    (189, "fake_wall.tile"),
    (178, "fall_floor.tile"),
    (184, "fly_fruit.if_not_fruit"),
    (186, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (180, "fruit.if_not_fruit"),
    (182, "fruit.tile"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (191, "key.if_not_fruit"),
    (192, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (233, "objects[0].collideable"),
    (234, "objects[0].delay"),
    (263, "objects[0].flip.x"),
    (264, "objects[0].flip.y"),
    (265, "objects[0].hitbox.h"),
    (266, "objects[0].hitbox.w"),
    (267, "objects[0].hitbox.x"),
    (268, "objects[0].hitbox.y"),
    (269, "objects[0].rem.x"),
    (270, "objects[0].rem.y"),
    (243, "objects[0].solids"),
    (271, "objects[0].spd.x"),
    (272, "objects[0].spd.y"),
    (245, "objects[0].spr"),
    (246, "objects[0].state"),
    (273, "objects[0].target.x"),
    (274, "objects[0].target.y"),
    (157, "objects[0].type.tile"),
    (249, "objects[0].x"),
    (250, "objects[0].y"),
    (43, "pause_player"),
    (159, "room.x"),
    (160, "room.y"),
    (85, "seconds"),
    (172, "spring.tile"),
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
    SCell::Arr(&[151, 152]),
    SCell::Obj(&[(7, 153), (5, 154), (6, 155)]),
    SCell::Obj(&[(5, 156), (8, 157), (6, 158)]),
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
    SCell::Obj(&[(1, 159), (2, 160)]),
    SCell::Arr(&[161, 162, 163, 164, 165, 166, 167, 168, 169, 170]),
    SCell::Unk,
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 171), (8, 172), (6, 173)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 174), (8, 175), (6, 176)]),
    SCell::Obj(&[(5, 177), (8, 178), (6, 179)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 180), (5, 181), (8, 182), (6, 183)]),
    SCell::Obj(&[(9, 184), (5, 185), (8, 186), (6, 187)]),
    SCell::Obj(&[(9, 188), (8, 189), (6, 190)]),
    SCell::Obj(&[(9, 191), (8, 192), (6, 193)]),
    SCell::Obj(&[(9, 194), (5, 195), (8, 196), (6, 197)]),
    SCell::Obj(&[(5, 198), (6, 199)]),
    SCell::Obj(&[(7, 200), (5, 201), (8, 202)]),
    SCell::Obj(&[(7, 203), (5, 204)]),
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
    SCell::Obj(&[(21, 231), (20, 232), (11, 233), (35, 234), (14, 235), (15, 236), (19, 237), (18, 238), (22, 239), (23, 240), (24, 241), (4, 242), (12, 243), (3, 244), (13, 245), (25, 246), (34, 247), (0, 248), (1, 249), (2, 250)]),
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
    SCell::Clo(26, &[205]),
    SCell::Clo(25, &[205]),
    SCell::Obj(&[(1, 263), (2, 264)]),
    SCell::Obj(&[(17, 265), (16, 266), (1, 267), (2, 268)]),
    SCell::Clo(24, &[205]),
    SCell::Clo(23, &[205]),
    SCell::Clo(27, &[205]),
    SCell::Clo(28, &[205]),
    SCell::Clo(29, &[205]),
    SCell::Obj(&[(1, 269), (2, 270)]),
    SCell::Obj(&[(1, 271), (2, 272)]),
    SCell::Obj(&[(1, 273), (2, 274)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
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
    (151, 205),
    (153, 206),
    (154, 207),
    (155, 208),
    (156, 209),
    (158, 210),
    (161, 94),
    (162, 116),
    (163, 118),
    (164, 119),
    (165, 121),
    (166, 122),
    (167, 123),
    (168, 124),
    (169, 125),
    (170, 127),
    (171, 211),
    (173, 212),
    (174, 213),
    (176, 214),
    (177, 215),
    (179, 216),
    (181, 217),
    (183, 218),
    (185, 219),
    (187, 220),
    (190, 221),
    (193, 222),
    (195, 223),
    (197, 224),
    (198, 225),
    (199, 226),
    (200, 227),
    (201, 228),
    (203, 229),
    (204, 230),
    (231, 251),
    (232, 252),
    (235, 253),
    (236, 254),
    (237, 255),
    (238, 256),
    (239, 257),
    (240, 258),
    (241, 259),
    (242, 260),
    (244, 261),
    (247, 262),
    (248, 94),
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
    pub c234: ZN,
    pub c246: ZN,
    pub c250: ZN,
    pub c85: ZN,
    pub c38: ZB,
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
    (175, "balloon.tile"),
    (202, "big_chest.tile"),
    (194, "chest.if_not_fruit"),
    (196, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (188, "fake_wall.if_not_fruit"),
    (189, "fake_wall.tile"),
    (178, "fall_floor.tile"),
    (184, "fly_fruit.if_not_fruit"),
    (186, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (180, "fruit.if_not_fruit"),
    (182, "fruit.tile"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (191, "key.if_not_fruit"),
    (192, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (234, "objects[0].collideable"),
    (298, "objects[0].flip.x"),
    (299, "objects[0].flip.y"),
    (300, "objects[0].hitbox.h"),
    (301, "objects[0].hitbox.w"),
    (302, "objects[0].hitbox.x"),
    (303, "objects[0].hitbox.y"),
    (304, "objects[0].rem.x"),
    (305, "objects[0].rem.y"),
    (243, "objects[0].solids"),
    (306, "objects[0].spd.x"),
    (307, "objects[0].spd.y"),
    (245, "objects[0].spr"),
    (247, "objects[0].x"),
    (248, "objects[0].y"),
    (251, "objects[1].collideable"),
    (308, "objects[1].dash_accel.x"),
    (309, "objects[1].dash_accel.y"),
    (253, "objects[1].dash_effect_time"),
    (310, "objects[1].dash_target.x"),
    (311, "objects[1].dash_target.y"),
    (255, "objects[1].dash_time"),
    (256, "objects[1].djump"),
    (312, "objects[1].flip.x"),
    (313, "objects[1].flip.y"),
    (258, "objects[1].grace"),
    (314, "objects[1].hitbox.h"),
    (315, "objects[1].hitbox.w"),
    (316, "objects[1].hitbox.x"),
    (317, "objects[1].hitbox.y"),
    (265, "objects[1].p_dash"),
    (266, "objects[1].p_jump"),
    (318, "objects[1].rem.x"),
    (319, "objects[1].rem.y"),
    (268, "objects[1].solids"),
    (320, "objects[1].spd.x"),
    (321, "objects[1].spd.y"),
    (272, "objects[1].x"),
    (273, "objects[1].y"),
    (43, "pause_player"),
    (157, "player_spawn.tile"),
    (159, "room.x"),
    (160, "room.y"),
    (85, "seconds"),
    (172, "spring.tile"),
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
    SCell::Arr(&[151, 152]),
    SCell::Obj(&[(7, 153), (5, 154), (6, 155)]),
    SCell::Obj(&[(5, 156), (8, 157), (6, 158)]),
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
    SCell::Obj(&[(1, 159), (2, 160)]),
    SCell::Arr(&[161, 162, 163, 164, 165, 166, 167, 168, 169, 170]),
    SCell::Unk,
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 171), (8, 172), (6, 173)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 174), (8, 175), (6, 176)]),
    SCell::Obj(&[(5, 177), (8, 178), (6, 179)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 180), (5, 181), (8, 182), (6, 183)]),
    SCell::Obj(&[(9, 184), (5, 185), (8, 186), (6, 187)]),
    SCell::Obj(&[(9, 188), (8, 189), (6, 190)]),
    SCell::Obj(&[(9, 191), (8, 192), (6, 193)]),
    SCell::Obj(&[(9, 194), (5, 195), (8, 196), (6, 197)]),
    SCell::Obj(&[(5, 198), (6, 199)]),
    SCell::Obj(&[(7, 200), (5, 201), (8, 202)]),
    SCell::Obj(&[(7, 203), (5, 204)]),
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
    SCell::Obj(&[(21, 232), (20, 233), (11, 234), (14, 235), (15, 236), (19, 237), (18, 238), (22, 239), (23, 240), (24, 241), (4, 242), (12, 243), (3, 244), (13, 245), (0, 246), (1, 247), (2, 248)]),
    SCell::Obj(&[(21, 249), (20, 250), (11, 251), (28, 252), (33, 253), (27, 254), (26, 255), (30, 256), (14, 257), (29, 258), (15, 259), (19, 260), (18, 261), (22, 262), (23, 263), (24, 264), (32, 265), (31, 266), (4, 267), (12, 268), (3, 269), (13, 270), (0, 271), (1, 272), (2, 273)]),
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
    SCell::Clo(26, &[205]),
    SCell::Clo(25, &[205]),
    SCell::Obj(&[(1, 298), (2, 299)]),
    SCell::Obj(&[(17, 300), (16, 301), (1, 302), (2, 303)]),
    SCell::Clo(24, &[205]),
    SCell::Clo(23, &[205]),
    SCell::Clo(27, &[205]),
    SCell::Clo(28, &[205]),
    SCell::Clo(29, &[205]),
    SCell::Obj(&[(1, 304), (2, 305)]),
    SCell::Obj(&[(1, 306), (2, 307)]),
    SCell::Clo(26, &[206]),
    SCell::Clo(25, &[206]),
    SCell::Obj(&[(1, 308), (2, 309)]),
    SCell::Obj(&[(1, 310), (2, 311)]),
    SCell::Obj(&[(1, 312), (2, 313)]),
    SCell::Obj(&[(17, 314), (16, 315), (1, 316), (2, 317)]),
    SCell::Clo(24, &[206]),
    SCell::Clo(23, &[206]),
    SCell::Clo(27, &[206]),
    SCell::Clo(28, &[206]),
    SCell::Clo(29, &[206]),
    SCell::Obj(&[(1, 318), (2, 319)]),
    SCell::Obj(&[(1, 320), (2, 321)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
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
    (151, 205),
    (152, 206),
    (153, 207),
    (154, 208),
    (155, 209),
    (156, 210),
    (158, 211),
    (161, 94),
    (162, 116),
    (163, 118),
    (164, 119),
    (165, 121),
    (166, 122),
    (167, 123),
    (168, 124),
    (169, 125),
    (170, 127),
    (171, 212),
    (173, 213),
    (174, 214),
    (176, 215),
    (177, 216),
    (179, 217),
    (181, 218),
    (183, 219),
    (185, 220),
    (187, 221),
    (190, 222),
    (193, 223),
    (195, 224),
    (197, 225),
    (198, 226),
    (199, 227),
    (200, 228),
    (201, 229),
    (203, 230),
    (204, 231),
    (232, 274),
    (233, 275),
    (235, 276),
    (236, 277),
    (237, 278),
    (238, 279),
    (239, 280),
    (240, 281),
    (241, 282),
    (242, 283),
    (244, 284),
    (246, 123),
    (249, 285),
    (250, 286),
    (252, 287),
    (254, 288),
    (257, 289),
    (259, 290),
    (260, 291),
    (261, 292),
    (262, 293),
    (263, 294),
    (264, 295),
    (267, 296),
    (269, 297),
    (271, 93),
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
    pub c318: ZN,
    pub c319: ZN,
    pub c273: ZN,
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
    pub c308: ZN,
    pub c309: ZN,
    pub c253: ZN,
    pub c310: ZN,
    pub c311: ZN,
    pub c255: ZN,
    pub c256: ZN,
    pub c312: ZB,
    pub c258: ZN,
    pub c265: ZB,
    pub c266: ZB,
    pub c320: ZN,
    pub c321: ZN,
    pub c272: ZN,
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
    b.cols[176] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[203] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[195] = Col::U(AV::Bool(true));
    b.cols[197] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::N(Vec::new());
    b.cols[189] = Col::U(AV::Bool(true));
    b.cols[190] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[179] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[185] = Col::U(AV::Bool(true));
    b.cols[187] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
    b.cols[20] = Col::N(Vec::new());
    b.cols[181] = Col::U(AV::Bool(true));
    b.cols[183] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[171] = Col::U(AV::Bool(true));
    b.cols[41] = Col::V(Vec::new());
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[192] = Col::U(AV::Bool(true));
    b.cols[193] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[86] = Col::N(Vec::new());
    b.cols[234] = Col::U(AV::Bool(true));
    b.cols[270] = Col::N(Vec::new());
    b.cols[271] = Col::N(Vec::new());
    b.cols[236] = Col::N(Vec::new());
    b.cols[272] = Col::N(Vec::new());
    b.cols[273] = Col::N(Vec::new());
    b.cols[238] = Col::N(Vec::new());
    b.cols[239] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[274] = Col::V(Vec::new());
    b.cols[275] = Col::U(AV::Bool(false));
    b.cols[241] = Col::N(Vec::new());
    b.cols[276] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[277] = Col::U(AV::Num(P8::from_raw(393216i32)));
    b.cols[278] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[279] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[248] = Col::V(Vec::new());
    b.cols[249] = Col::V(Vec::new());
    b.cols[280] = Col::N(Vec::new());
    b.cols[281] = Col::N(Vec::new());
    b.cols[251] = Col::U(AV::Bool(true));
    b.cols[282] = Col::N(Vec::new());
    b.cols[283] = Col::N(Vec::new());
    b.cols[255] = Col::N(Vec::new());
    b.cols[256] = Col::N(Vec::new());
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[157] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[160] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[173] = Col::U(AV::Num(P8::from_raw(1179648i32)));
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
pub const KPART1_0: u64 = 10368174679772814864;
pub const KPART2_0: u64 = 16397051069404573333;

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
        if let Col::N(v) = &mut acc.cols[270] { v.push(kv.c270.lane(i)); }
        if let Col::N(v) = &mut acc.cols[271] { v.push(kv.c271.lane(i)); }
        if let Col::N(v) = &mut acc.cols[236] { v.push(kv.c236.lane(i)); }
        if let Col::N(v) = &mut acc.cols[272] { v.push(kv.c272.lane(i)); }
        if let Col::N(v) = &mut acc.cols[273] { v.push(kv.c273.lane(i)); }
        if let Col::N(v) = &mut acc.cols[238] { v.push(kv.c238.lane(i)); }
        if let Col::V(v) = &mut acc.cols[274] {
            v.push(if kv.c274.known & (1 << i) != 0 {
                AV::Bool(kv.c274.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[241] { v.push(kv.c241.lane(i)); }
        if let Col::V(v) = &mut acc.cols[248] {
            v.push(if kv.c248.known & (1 << i) != 0 {
                AV::Bool(kv.c248.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::V(v) = &mut acc.cols[249] {
            v.push(if kv.c249.known & (1 << i) != 0 {
                AV::Bool(kv.c249.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[280] { v.push(sh.c280.lane(i)); }
        if let Col::N(v) = &mut acc.cols[281] { v.push(sh.c281.lane(i)); }
        if let Col::N(v) = &mut acc.cols[282] { v.push(kv.c282.lane(i)); }
        if let Col::N(v) = &mut acc.cols[283] { v.push(kv.c283.lane(i)); }
        if let Col::N(v) = &mut acc.cols[255] { v.push(kv.c255.lane(i)); }
        if let Col::N(v) = &mut acc.cols[256] { v.push(sh.c256.lane(i)); }
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
    b.cols[175] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[202] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[194] = Col::U(AV::Bool(true));
    b.cols[196] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::N(Vec::new());
    b.cols[188] = Col::U(AV::Bool(true));
    b.cols[189] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[178] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[184] = Col::U(AV::Bool(true));
    b.cols[186] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
    b.cols[20] = Col::N(Vec::new());
    b.cols[180] = Col::U(AV::Bool(true));
    b.cols[182] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[41] = Col::V(Vec::new());
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[191] = Col::U(AV::Bool(true));
    b.cols[192] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[86] = Col::N(Vec::new());
    b.cols[234] = Col::U(AV::Bool(true));
    b.cols[300] = Col::N(Vec::new());
    b.cols[301] = Col::N(Vec::new());
    b.cols[236] = Col::N(Vec::new());
    b.cols[302] = Col::N(Vec::new());
    b.cols[303] = Col::N(Vec::new());
    b.cols[238] = Col::N(Vec::new());
    b.cols[239] = Col::N(Vec::new());
    b.cols[304] = Col::V(Vec::new());
    b.cols[305] = Col::U(AV::Bool(false));
    b.cols[241] = Col::N(Vec::new());
    b.cols[306] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[307] = Col::U(AV::Num(P8::from_raw(393216i32)));
    b.cols[308] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[309] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[248] = Col::V(Vec::new());
    b.cols[249] = Col::V(Vec::new());
    b.cols[310] = Col::N(Vec::new());
    b.cols[311] = Col::N(Vec::new());
    b.cols[251] = Col::U(AV::Bool(true));
    b.cols[312] = Col::N(Vec::new());
    b.cols[313] = Col::N(Vec::new());
    b.cols[255] = Col::N(Vec::new());
    b.cols[256] = Col::N(Vec::new());
    b.cols[259] = Col::U(AV::Bool(true));
    b.cols[314] = Col::U(AV::Bool(false));
    b.cols[315] = Col::U(AV::Bool(false));
    b.cols[316] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[317] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[318] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[319] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[267] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[320] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[321] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[269] = Col::U(AV::Bool(true));
    b.cols[322] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[323] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[271] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[272] = Col::U(AV::Num(P8::from_raw(2359296i32)));
    b.cols[274] = Col::U(AV::Num(P8::from_raw(786432i32)));
    b.cols[275] = Col::U(AV::Num(P8::from_raw(2333641i32)));
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[157] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[160] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[172] = Col::U(AV::Num(P8::from_raw(1179648i32)));
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
pub const KPART1_1: u64 = 9890089525395762139;
pub const KPART2_1: u64 = 13543709414086565786;

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
        if let Col::N(v) = &mut acc.cols[300] { v.push(kv.c300.lane(i)); }
        if let Col::N(v) = &mut acc.cols[301] { v.push(kv.c301.lane(i)); }
        if let Col::N(v) = &mut acc.cols[236] { v.push(kv.c236.lane(i)); }
        if let Col::N(v) = &mut acc.cols[302] { v.push(kv.c302.lane(i)); }
        if let Col::N(v) = &mut acc.cols[303] { v.push(kv.c303.lane(i)); }
        if let Col::N(v) = &mut acc.cols[238] { v.push(kv.c238.lane(i)); }
        if let Col::N(v) = &mut acc.cols[239] { v.push(kv.c239.lane(i)); }
        if let Col::V(v) = &mut acc.cols[304] {
            v.push(if kv.c304.known & (1 << i) != 0 {
                AV::Bool(kv.c304.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[241] { v.push(kv.c241.lane(i)); }
        if let Col::V(v) = &mut acc.cols[248] {
            v.push(if kv.c248.known & (1 << i) != 0 {
                AV::Bool(kv.c248.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::V(v) = &mut acc.cols[249] {
            v.push(if kv.c249.known & (1 << i) != 0 {
                AV::Bool(kv.c249.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[310] { v.push(sh.c310.lane(i)); }
        if let Col::N(v) = &mut acc.cols[311] { v.push(sh.c311.lane(i)); }
        if let Col::N(v) = &mut acc.cols[312] { v.push(kv.c312.lane(i)); }
        if let Col::N(v) = &mut acc.cols[313] { v.push(kv.c313.lane(i)); }
        if let Col::N(v) = &mut acc.cols[255] { v.push(kv.c255.lane(i)); }
        if let Col::N(v) = &mut acc.cols[256] { v.push(sh.c256.lane(i)); }
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
    b.cols[175] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[202] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[194] = Col::U(AV::Bool(true));
    b.cols[196] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::U(AV::Num(P8::from_raw(983040i32)));
    b.cols[188] = Col::U(AV::Bool(true));
    b.cols[189] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[178] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[184] = Col::U(AV::Bool(true));
    b.cols[186] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
    b.cols[20] = Col::N(Vec::new());
    b.cols[180] = Col::U(AV::Bool(true));
    b.cols[182] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[41] = Col::V(Vec::new());
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[191] = Col::U(AV::Bool(true));
    b.cols[192] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[86] = Col::N(Vec::new());
    b.cols[233] = Col::U(AV::Bool(true));
    b.cols[261] = Col::U(AV::Bool(false));
    b.cols[262] = Col::U(AV::Bool(false));
    b.cols[263] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[264] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[265] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[266] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[241] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[267] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[268] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[243] = Col::U(AV::Bool(true));
    b.cols[269] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[270] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[245] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[246] = Col::U(AV::Num(P8::from_raw(2359296i32)));
    b.cols[248] = Col::U(AV::Num(P8::from_raw(786432i32)));
    b.cols[249] = Col::U(AV::Num(P8::from_raw(2333641i32)));
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[157] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[160] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[172] = Col::U(AV::Num(P8::from_raw(1179648i32)));
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
pub const KPART1_2: u64 = 10376036704650479192;
pub const KPART2_2: u64 = 3869514956790187917;

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
    b.cols[175] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[202] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[194] = Col::U(AV::Bool(true));
    b.cols[196] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::U(AV::Num(P8::from_raw(983040i32)));
    b.cols[188] = Col::U(AV::Bool(true));
    b.cols[189] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[178] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[184] = Col::U(AV::Bool(true));
    b.cols[186] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
    b.cols[20] = Col::N(Vec::new());
    b.cols[180] = Col::U(AV::Bool(true));
    b.cols[182] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[41] = Col::V(Vec::new());
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[191] = Col::U(AV::Bool(true));
    b.cols[192] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[86] = Col::N(Vec::new());
    b.cols[233] = Col::U(AV::Bool(true));
    b.cols[259] = Col::U(AV::Bool(false));
    b.cols[260] = Col::U(AV::Bool(false));
    b.cols[261] = Col::U(AV::Num(P8::from_raw(1048576i32)));
    b.cols[262] = Col::U(AV::Num(P8::from_raw(1048576i32)));
    b.cols[263] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[264] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[265] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[266] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[242] = Col::U(AV::Bool(true));
    b.cols[267] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[268] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[244] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[246] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[247] = Col::U(AV::Num(P8::from_raw(2097152i32)));
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[157] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[160] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[172] = Col::U(AV::Num(P8::from_raw(1179648i32)));
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
pub const KPART1_3: u64 = 15647309184744847883;
pub const KPART2_3: u64 = 12094890971602204182;

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
    b.cols[175] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[202] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[194] = Col::U(AV::Bool(true));
    b.cols[196] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::N(Vec::new());
    b.cols[188] = Col::U(AV::Bool(true));
    b.cols[189] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[178] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[184] = Col::U(AV::Bool(true));
    b.cols[186] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
    b.cols[20] = Col::N(Vec::new());
    b.cols[180] = Col::U(AV::Bool(true));
    b.cols[182] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[41] = Col::U(AV::Bool(false));
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[191] = Col::U(AV::Bool(true));
    b.cols[192] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[86] = Col::N(Vec::new());
    b.cols[233] = Col::U(AV::Bool(true));
    b.cols[234] = Col::N(Vec::new());
    b.cols[263] = Col::U(AV::Bool(false));
    b.cols[264] = Col::U(AV::Bool(false));
    b.cols[265] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[266] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[267] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[268] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[269] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[270] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[243] = Col::U(AV::Bool(false));
    b.cols[271] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[272] = Col::U(AV::Num(P8::from_raw(-262144i32)));
    b.cols[245] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[246] = Col::N(Vec::new());
    b.cols[273] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[274] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[157] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[249] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[250] = Col::N(Vec::new());
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[160] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[172] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[38] = Col::V(Vec::new());
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
pub const KPART1_4: u64 = 3417410494478606713;
pub const KPART2_4: u64 = 14607016929930880940;

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
        if let Col::N(v) = &mut acc.cols[86] { v.push(sh.c86.lane(i)); }
        if let Col::N(v) = &mut acc.cols[234] { v.push(sh.c234.lane(i)); }
        if let Col::N(v) = &mut acc.cols[246] { v.push(sh.c246.lane(i)); }
        if let Col::N(v) = &mut acc.cols[250] { v.push(sh.c250.lane(i)); }
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

/// An EMPTY accumulator with outcome 5's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append5`.
pub fn acc5(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_5, OUT_GLOBALS_5, OUT_PTRS_5, 0, cart, cache);
    b.cols[175] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[202] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[194] = Col::U(AV::Bool(true));
    b.cols[196] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::N(Vec::new());
    b.cols[188] = Col::U(AV::Bool(true));
    b.cols[189] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[178] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[184] = Col::U(AV::Bool(true));
    b.cols[186] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
    b.cols[20] = Col::N(Vec::new());
    b.cols[180] = Col::U(AV::Bool(true));
    b.cols[182] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[41] = Col::V(Vec::new());
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[191] = Col::U(AV::Bool(true));
    b.cols[192] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[86] = Col::N(Vec::new());
    b.cols[234] = Col::U(AV::Bool(true));
    b.cols[298] = Col::U(AV::Bool(false));
    b.cols[299] = Col::U(AV::Bool(false));
    b.cols[300] = Col::U(AV::Num(P8::from_raw(1048576i32)));
    b.cols[301] = Col::U(AV::Num(P8::from_raw(1048576i32)));
    b.cols[302] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[303] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[304] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[305] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[243] = Col::U(AV::Bool(true));
    b.cols[306] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[307] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[245] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[247] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[248] = Col::U(AV::Num(P8::from_raw(2097152i32)));
    b.cols[251] = Col::U(AV::Bool(true));
    b.cols[308] = Col::N(Vec::new());
    b.cols[309] = Col::N(Vec::new());
    b.cols[253] = Col::N(Vec::new());
    b.cols[310] = Col::N(Vec::new());
    b.cols[311] = Col::N(Vec::new());
    b.cols[255] = Col::N(Vec::new());
    b.cols[256] = Col::N(Vec::new());
    b.cols[312] = Col::V(Vec::new());
    b.cols[313] = Col::U(AV::Bool(false));
    b.cols[258] = Col::N(Vec::new());
    b.cols[314] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[315] = Col::U(AV::Num(P8::from_raw(393216i32)));
    b.cols[316] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[317] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[265] = Col::V(Vec::new());
    b.cols[266] = Col::V(Vec::new());
    b.cols[318] = Col::N(Vec::new());
    b.cols[319] = Col::N(Vec::new());
    b.cols[268] = Col::U(AV::Bool(true));
    b.cols[320] = Col::N(Vec::new());
    b.cols[321] = Col::N(Vec::new());
    b.cols[272] = Col::N(Vec::new());
    b.cols[273] = Col::N(Vec::new());
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[157] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[160] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[172] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[38] = Col::U(AV::Bool(false));
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
pub const KPART1_5: u64 = 15539437657828496309;
pub const KPART2_5: u64 = 9983820771003891464;

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
        if let Col::N(v) = &mut acc.cols[39] { v.push(sh.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[84] { v.push(sh.c84.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::V(v) = &mut acc.cols[41] {
            v.push(if kv.c41.known & (1 << i) != 0 {
                AV::Bool(kv.c41.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[86] { v.push(sh.c86.lane(i)); }
        if let Col::N(v) = &mut acc.cols[308] { v.push(kv.c308.lane(i)); }
        if let Col::N(v) = &mut acc.cols[309] { v.push(kv.c309.lane(i)); }
        if let Col::N(v) = &mut acc.cols[253] { v.push(kv.c253.lane(i)); }
        if let Col::N(v) = &mut acc.cols[310] { v.push(kv.c310.lane(i)); }
        if let Col::N(v) = &mut acc.cols[311] { v.push(kv.c311.lane(i)); }
        if let Col::N(v) = &mut acc.cols[255] { v.push(kv.c255.lane(i)); }
        if let Col::N(v) = &mut acc.cols[256] { v.push(kv.c256.lane(i)); }
        if let Col::V(v) = &mut acc.cols[312] {
            v.push(if kv.c312.known & (1 << i) != 0 {
                AV::Bool(kv.c312.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[258] { v.push(kv.c258.lane(i)); }
        if let Col::V(v) = &mut acc.cols[265] {
            v.push(if kv.c265.known & (1 << i) != 0 {
                AV::Bool(kv.c265.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::V(v) = &mut acc.cols[266] {
            v.push(if kv.c266.known & (1 << i) != 0 {
                AV::Bool(kv.c266.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[318] { v.push(sh.c318.lane(i)); }
        if let Col::N(v) = &mut acc.cols[319] { v.push(sh.c319.lane(i)); }
        if let Col::N(v) = &mut acc.cols[320] { v.push(kv.c320.lane(i)); }
        if let Col::N(v) = &mut acc.cols[321] { v.push(kv.c321.lane(i)); }
        if let Col::N(v) = &mut acc.cols[272] { v.push(kv.c272.lane(i)); }
        if let Col::N(v) = &mut acc.cols[273] { v.push(sh.c273.lane(i)); }
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
    let r_c234: ZB = ZB { val: rin.c234, known: ALL };
    let r_c243: ZB = ZB { val: rin.c243, known: ALL };
    let r_c245: ZN = rin.c245;
    let r_c247: ZN = rin.c247;
    let r_c248: ZN = rin.c248;
    let r_c251: ZB = ZB { val: rin.c251, known: ALL };
    let r_c253: ZN = rin.c253;
    let r_c255: ZN = rin.c255;
    let r_c256: ZN = rin.c256;
    let r_c258: ZN = rin.c258;
    let r_c265: ZB = ZB { val: rin.c265, known: ALL };
    let r_c266: ZB = ZB { val: rin.c266, known: ALL };
    let r_c268: ZB = ZB { val: rin.c268, known: ALL };
    let r_c272: ZN = rin.c272;
    let r_c273: ZN = rin.c273;
    let r_c298: ZB = ZB { val: rin.c298, known: ALL };
    let r_c299: ZB = ZB { val: rin.c299, known: ALL };
    let r_c304: ZN = rin.c304;
    let r_c305: ZN = rin.c305;
    let r_c306: ZN = rin.c306;
    let r_c307: ZN = rin.c307;
    let r_c308: ZN = rin.c308;
    let r_c309: ZN = rin.c309;
    let r_c310: ZN = rin.c310;
    let r_c311: ZN = rin.c311;
    let r_c312: ZB = ZB { val: rin.c312, known: ALL };
    let r_c313: ZB = ZB { val: rin.c313, known: ALL };
    let r_c318: ZN = rin.c318;
    let r_c319: ZN = rin.c319;
    let r_c320: ZN = rin.c320;
    let r_c321: ZN = rin.c321;
    let n72: ZB = zb_not(r_c42);
    let n73: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), r_c88);
    let n74: ZB = zb_not(r_c298);
    let n75: ZB = zb_not(r_c299);
    let n76: bool = P8::from_raw(0i32) == u.c302;
    let n77: bool = P8::from_raw(0i32) == u.c303;
    let n78: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c304);
    let n79: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c305);
    let n80: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c306);
    let n81: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c307);
    let n82: ZB = zn_eq(zn_splat(P8::from_raw(4194304i32)), r_c245);
    let n83: ZB = zn_eq(zn_splat(P8::from_raw(524288i32)), r_c247);
    let n84: ZB = zn_eq(zn_splat(P8::from_raw(2097152i32)), r_c248);
    let n89: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c84);
    let n90: ZN = zn_rem(n89, zn_splat(P8::from_raw(1966080i32)));
    let n91: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n90);
    let n108: bool = P8::from_raw(1048576i32) == u.c300;
    let n109: bool = P8::from_raw(1048576i32) == u.c301;
    let n110: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c86);
    let n111: ZB = zn_le(r_c20, zn_splat(P8::from_raw(0i32)));
    let n133: ZB = zb_not(r_c313);
    let n134: bool = P8::from_raw(327680i32) == u.c314;
    let n135: bool = P8::from_raw(393216i32) == u.c315;
    let n136: bool = P8::from_raw(65536i32) == u.c316;
    let n137: bool = P8::from_raw(196608i32) == u.c317;
    let n138: ZB = zb_not(r_c43);
    let n139: ZB = zb_not(r_c38);
    let n140: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c85);
    let n141: ZN = zn_rem(n140, zn_splat(P8::from_raw(3932160i32)));
    let n142: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n141);
    let n143: ZN = zsel_n(n142, n110, r_c86);
    let n144: ZN = zsel_n(n91, n143, r_c86);
    let n145: ZN = zsel_n(n91, n141, r_c85);
    let n146: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), r_c273);
    let n147: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n146);
    let n148: ZB = zn_gt(n147, zn_splat(P8::from_raw(2031616i32)));
    let n149: ZB = zn_lt(n146, zn_splat(P8::from_raw(3211264i32)));
    let n150: ZB = zn_gt(r_c253, zn_splat(P8::from_raw(0i32)));
    let n151: ZB = zn_gt(r_c320, zn_splat(P8::from_raw(0i32)));
    let n152: ZB = zn_lt(r_c320, zn_splat(P8::from_raw(0i32)));
    let n154: ZN = zsel_n(n152, zn_splat(P8::from_raw(98304i32)), zn_splat(P8::from_raw(0i32)));
    let n155: ZN = zsel_n(n151, zn_splat(P8::from_raw(-98304i32)), n154);
    let n156: ZN = zn_add(r_c318, n155);
    let n157: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n156);
    let n158: ZN = zn_flr(n157);
    let n159: ZN = zn_sub(n157, zn_splat(P8::from_raw(32768i32)));
    let n160: ZN = zn_sub(n159, n158);
    let n161: ZB = zn_gt(n158, zn_splat(P8::from_raw(0i32)));
    let n162: ZB = zn_lt(n158, zn_splat(P8::from_raw(0i32)));
    let n163: ZN = zsel_n(n162, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n164: ZN = zsel_n(n161, zn_splat(P8::from_raw(65536i32)), n163);
    let n165: ZN = zn_abs(n158);
    let n166: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n146);
    let n167: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n165);
    let n168: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n165);
    let n169: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n165);
    let n170: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n165);
    let n171: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n165);
    let n172: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n165);
    let n173: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n165);
    let n174: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n165);
    let n175: ZN = zn_add(r_c319, zn_splat(P8::from_raw(-98304i32)));
    let n176: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n175);
    let n177: ZN = zn_flr(n176);
    let n178: ZN = zn_sub(n176, zn_splat(P8::from_raw(32768i32)));
    let n179: ZN = zn_sub(n178, n177);
    let n180: ZB = zn_gt(n177, zn_splat(P8::from_raw(0i32)));
    let n181: ZB = zn_lt(n177, zn_splat(P8::from_raw(0i32)));
    let n182: ZN = zsel_n(n181, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n183: ZN = zsel_n(n180, zn_splat(P8::from_raw(65536i32)), n182);
    let n184: ZN = zn_abs(n177);
    let n185: ZN = zn_add(n146, n183);
    let n186: ZN = zn_add(r_c273, n183);
    let n187: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n184);
    let n188: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n186);
    let n189: ZN = zn_add(n183, n188);
    let n190: ZN = zn_add(n183, n186);
    let n191: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n184);
    let n192: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n190);
    let n193: ZN = zn_add(n183, n192);
    let n194: ZN = zn_add(n183, n190);
    let n195: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n184);
    let n196: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n194);
    let n197: ZN = zn_add(n183, n196);
    let n198: ZN = zn_add(n183, n194);
    let n199: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n184);
    let n200: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n198);
    let n201: ZN = zn_add(n183, n200);
    let n202: ZN = zn_add(n183, n198);
    let n203: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n184);
    let n204: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n202);
    let n205: ZN = zn_add(n183, n204);
    let n206: ZN = zn_add(n183, n202);
    let n207: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n184);
    let n208: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n206);
    let n209: ZN = zn_add(n183, n208);
    let n210: ZN = zn_add(n183, n206);
    let n211: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n184);
    let n212: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n210);
    let n213: ZN = zn_add(n183, n212);
    let n214: ZN = zn_add(n183, n210);
    let n215: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n184);
    let n216: ZB = zb_not(r_c266);
    let n217: ZB = zb_not(r_c265);
    let n218: ZB = zn_lt(r_c256, zn_splat(P8::from_raw(65536i32)));
    let n219: ZN = zsel_n(n218, zn_splat(P8::from_raw(65536i32)), r_c256);
    let n220: ZB = zn_gt(r_c258, zn_splat(P8::from_raw(0i32)));
    let n221: ZN = zn_sub(r_c258, zn_splat(P8::from_raw(65536i32)));
    let n222: ZN = zsel_n(n220, n221, r_c258);
    let n223: ZN = zn_sub(r_c253, zn_splat(P8::from_raw(65536i32)));
    let n225: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c272);
    let n226: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n225);
    let n227: ZB = zn_gt(n226, zn_splat(P8::from_raw(458752i32)));
    let n228: ZB = zb_and(n148, n227);
    let n229: ZB = zn_lt(n225, zn_splat(P8::from_raw(1638400i32)));
    let n230: ZB = zb_and(n228, n229);
    let n231: ZB = zb_and(n149, n230);
    let n232: ZB = zb_and(n111, n231);
    let n233: ZB = zb_and(n111, n148);
    let n234: ZB = zb_and(n149, n233);
    let n235: ZB = zb_and(n150, n234);
    let n236: ZB = zb_and(n227, n235);
    let n237: ZB = zb_and(n229, n236);
    let n238: ZN = zn_add(n164, n225);
    let n239: ZB = zn_tile_flag_at(g.cache, g.cart, n238, n166, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n240: ZN = zn_add(r_c272, n164);
    let n241: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n240);
    let n242: ZN = zn_add(n164, n241);
    let n243: ZB = zn_tile_flag_at(g.cache, g.cart, n242, n166, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n244: ZN = zn_add(n164, n240);
    let n245: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n244);
    let n246: ZN = zn_add(n164, n245);
    let n247: ZB = zn_tile_flag_at(g.cache, g.cart, n246, n166, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n248: ZN = zn_add(n164, n244);
    let n249: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n248);
    let n250: ZN = zn_add(n164, n249);
    let n251: ZB = zn_tile_flag_at(g.cache, g.cart, n250, n166, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n252: ZN = zn_add(n164, n248);
    let n253: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n252);
    let n254: ZN = zn_add(n164, n253);
    let n255: ZB = zn_tile_flag_at(g.cache, g.cart, n254, n166, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n256: ZN = zn_add(n164, n252);
    let n257: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n256);
    let n258: ZN = zn_add(n164, n257);
    let n259: ZB = zn_tile_flag_at(g.cache, g.cart, n258, n166, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n260: ZN = zn_add(n164, n256);
    let n261: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n260);
    let n262: ZN = zn_add(n164, n261);
    let n263: ZB = zn_tile_flag_at(g.cache, g.cart, n262, n166, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n264: ZN = zn_add(n164, n260);
    let n265: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n264);
    let n266: ZN = zn_add(n164, n265);
    let n267: ZB = zn_tile_flag_at(g.cache, g.cart, n266, n166, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n268: ZN = zn_add(n164, n264);
    let n269: ZN = zsel_n(n267, n264, n268);
    let n270: ZN = zsel_n(n267, zn_splat(P8::from_raw(0i32)), n160);
    let n271: ZN = zsel_n(n267, zn_splat(P8::from_raw(0i32)), n155);
    let n272: ZB = zb_or(n174, n267);
    let n273: ZN = zsel_n(n173, n264, n269);
    let n274: ZN = zsel_n(n173, n160, n270);
    let n275: ZN = zsel_n(n173, n155, n271);
    let n276: ZB = zb_or(n173, n272);
    let n277: ZN = zsel_n(n263, n260, n273);
    let n278: ZN = zsel_n(n263, zn_splat(P8::from_raw(0i32)), n274);
    let n279: ZN = zsel_n(n263, zn_splat(P8::from_raw(0i32)), n275);
    let n280: ZB = zb_or(n263, n276);
    let n281: ZN = zsel_n(n172, n260, n277);
    let n282: ZN = zsel_n(n172, n160, n278);
    let n283: ZN = zsel_n(n172, n155, n279);
    let n284: ZB = zb_or(n172, n280);
    let n285: ZN = zsel_n(n259, n256, n281);
    let n286: ZN = zsel_n(n259, zn_splat(P8::from_raw(0i32)), n282);
    let n287: ZN = zsel_n(n259, zn_splat(P8::from_raw(0i32)), n283);
    let n288: ZB = zb_or(n259, n284);
    let n289: ZN = zsel_n(n171, n256, n285);
    let n290: ZN = zsel_n(n171, n160, n286);
    let n291: ZN = zsel_n(n171, n155, n287);
    let n292: ZB = zb_or(n171, n288);
    let n293: ZN = zsel_n(n255, n252, n289);
    let n294: ZN = zsel_n(n255, zn_splat(P8::from_raw(0i32)), n290);
    let n295: ZN = zsel_n(n255, zn_splat(P8::from_raw(0i32)), n291);
    let n296: ZB = zb_or(n255, n292);
    let n297: ZN = zsel_n(n170, n252, n293);
    let n298: ZN = zsel_n(n170, n160, n294);
    let n299: ZN = zsel_n(n170, n155, n295);
    let n300: ZB = zb_or(n170, n296);
    let n301: ZN = zsel_n(n251, n248, n297);
    let n302: ZN = zsel_n(n251, zn_splat(P8::from_raw(0i32)), n298);
    let n303: ZN = zsel_n(n251, zn_splat(P8::from_raw(0i32)), n299);
    let n304: ZB = zb_or(n251, n300);
    let n305: ZN = zsel_n(n169, n248, n301);
    let n306: ZN = zsel_n(n169, n160, n302);
    let n307: ZN = zsel_n(n169, n155, n303);
    let n308: ZB = zb_or(n169, n304);
    let n309: ZN = zsel_n(n247, n244, n305);
    let n310: ZN = zsel_n(n247, zn_splat(P8::from_raw(0i32)), n306);
    let n311: ZN = zsel_n(n247, zn_splat(P8::from_raw(0i32)), n307);
    let n312: ZB = zb_or(n247, n308);
    let n313: ZN = zsel_n(n168, n244, n309);
    let n314: ZN = zsel_n(n168, n160, n310);
    let n315: ZN = zsel_n(n168, n155, n311);
    let n316: ZB = zb_or(n168, n312);
    let n317: ZN = zsel_n(n243, n240, n313);
    let n318: ZN = zsel_n(n243, zn_splat(P8::from_raw(0i32)), n314);
    let n319: ZN = zsel_n(n243, zn_splat(P8::from_raw(0i32)), n315);
    let n320: ZB = zb_or(n243, n316);
    let n321: ZN = zsel_n(n167, n240, n317);
    let n322: ZN = zsel_n(n167, n160, n318);
    let n323: ZN = zsel_n(n167, n155, n319);
    let n324: ZB = zb_or(n167, n320);
    let n325: ZN = zsel_n(n239, r_c272, n321);
    let n326: ZN = zsel_n(n239, zn_splat(P8::from_raw(0i32)), n322);
    let n327: ZN = zsel_n(n239, zn_splat(P8::from_raw(0i32)), n323);
    let n328: ZB = zb_or(n239, n324);
    let n329: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n325);
    let n330: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n329);
    let n331: ZB = zn_tile_flag_at(g.cache, g.cart, n330, n185, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n332: ZB = zn_tile_flag_at(g.cache, g.cart, n330, n189, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n333: ZB = zn_tile_flag_at(g.cache, g.cart, n330, n193, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n334: ZB = zn_tile_flag_at(g.cache, g.cart, n330, n197, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n335: ZB = zn_tile_flag_at(g.cache, g.cart, n330, n201, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n336: ZB = zn_tile_flag_at(g.cache, g.cart, n330, n205, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n337: ZB = zn_tile_flag_at(g.cache, g.cart, n330, n209, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n338: ZB = zn_tile_flag_at(g.cache, g.cart, n330, n213, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n339: ZB = zb_and(n215, n328);
    let n340: ZN = zsel_n(n338, n210, n214);
    let n341: ZN = zsel_n(n338, zn_splat(P8::from_raw(0i32)), n179);
    let n342: ZN = zsel_n(n338, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(-98304i32)));
    let n343: ZB = zsel_b(n338, n328, n339);
    let n344: ZN = zsel_n(n211, n210, n340);
    let n345: ZN = zsel_n(n211, n179, n341);
    let n346: ZN = zsel_n(n211, zn_splat(P8::from_raw(-98304i32)), n342);
    let n347: ZB = zsel_b(n211, n328, n343);
    let n348: ZN = zsel_n(n337, n206, n344);
    let n349: ZN = zsel_n(n337, zn_splat(P8::from_raw(0i32)), n345);
    let n350: ZN = zsel_n(n337, zn_splat(P8::from_raw(0i32)), n346);
    let n351: ZB = zsel_b(n337, n328, n347);
    let n352: ZN = zsel_n(n207, n206, n348);
    let n353: ZN = zsel_n(n207, n179, n349);
    let n354: ZN = zsel_n(n207, zn_splat(P8::from_raw(-98304i32)), n350);
    let n355: ZB = zsel_b(n207, n328, n351);
    let n356: ZN = zsel_n(n336, n202, n352);
    let n357: ZN = zsel_n(n336, zn_splat(P8::from_raw(0i32)), n353);
    let n358: ZN = zsel_n(n336, zn_splat(P8::from_raw(0i32)), n354);
    let n359: ZB = zsel_b(n336, n328, n355);
    let n360: ZN = zsel_n(n203, n202, n356);
    let n361: ZN = zsel_n(n203, n179, n357);
    let n362: ZN = zsel_n(n203, zn_splat(P8::from_raw(-98304i32)), n358);
    let n363: ZB = zsel_b(n203, n328, n359);
    let n364: ZN = zsel_n(n335, n198, n360);
    let n365: ZN = zsel_n(n335, zn_splat(P8::from_raw(0i32)), n361);
    let n366: ZN = zsel_n(n335, zn_splat(P8::from_raw(0i32)), n362);
    let n367: ZB = zsel_b(n335, n328, n363);
    let n368: ZN = zsel_n(n199, n198, n364);
    let n369: ZN = zsel_n(n199, n179, n365);
    let n370: ZN = zsel_n(n199, zn_splat(P8::from_raw(-98304i32)), n366);
    let n371: ZB = zsel_b(n199, n328, n367);
    let n372: ZN = zsel_n(n334, n194, n368);
    let n373: ZN = zsel_n(n334, zn_splat(P8::from_raw(0i32)), n369);
    let n374: ZN = zsel_n(n334, zn_splat(P8::from_raw(0i32)), n370);
    let n375: ZB = zsel_b(n334, n328, n371);
    let n376: ZN = zsel_n(n195, n194, n372);
    let n377: ZN = zsel_n(n195, n179, n373);
    let n378: ZN = zsel_n(n195, zn_splat(P8::from_raw(-98304i32)), n374);
    let n379: ZB = zsel_b(n195, n328, n375);
    let n380: ZN = zsel_n(n333, n190, n376);
    let n381: ZN = zsel_n(n333, zn_splat(P8::from_raw(0i32)), n377);
    let n382: ZN = zsel_n(n333, zn_splat(P8::from_raw(0i32)), n378);
    let n383: ZB = zsel_b(n333, n328, n379);
    let n384: ZN = zsel_n(n191, n190, n380);
    let n385: ZN = zsel_n(n191, n179, n381);
    let n386: ZN = zsel_n(n191, zn_splat(P8::from_raw(-98304i32)), n382);
    let n387: ZB = zsel_b(n191, n328, n383);
    let n388: ZN = zsel_n(n332, n186, n384);
    let n389: ZN = zsel_n(n332, zn_splat(P8::from_raw(0i32)), n385);
    let n390: ZN = zsel_n(n332, zn_splat(P8::from_raw(0i32)), n386);
    let n391: ZB = zsel_b(n332, n328, n387);
    let n392: ZN = zsel_n(n187, n186, n388);
    let n393: ZN = zsel_n(n187, n179, n389);
    let n394: ZN = zsel_n(n187, zn_splat(P8::from_raw(-98304i32)), n390);
    let n395: ZB = zsel_b(n187, n328, n391);
    let n396: ZN = zsel_n(n331, r_c273, n392);
    let n397: ZN = zsel_n(n331, zn_splat(P8::from_raw(0i32)), n393);
    let n398: ZN = zsel_n(n331, zn_splat(P8::from_raw(0i32)), n394);
    let n399: ZB = zsel_b(n331, n328, n395);
    let n400: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n396);
    let n401: ZN = zn_div(n329, zn_splat(P8::from_raw(524288i32)));
    let n402: ZN = zn_flr(n401);
    let n403: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n402);
    let n404: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n329);
    let n405: ZN = zn_sub(n404, zn_splat(P8::from_raw(65536i32)));
    let n406: ZN = zn_div(n405, zn_splat(P8::from_raw(524288i32)));
    let n407: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n406);
    let n408: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n403);
    let n409: ZB = zn_le(n408, n407);
    let n410: ZB = zn_gt(n408, n407);
    let n411: ZB = zb_and(n237, n409);
    let n412: ZB = zb_and(n237, n410);
    let n413: ZN = zn_div(n400, zn_splat(P8::from_raw(524288i32)));
    let n414: ZN = zn_flr(n413);
    let n415: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n414);
    let n416: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n400);
    let n417: ZN = zn_sub(n416, zn_splat(P8::from_raw(65536i32)));
    let n418: ZN = zn_div(n417, zn_splat(P8::from_raw(524288i32)));
    let n419: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n418);
    let n420: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n415);
    let n421: ZB = zn_le(n420, n419);
    let n422: ZB = zn_gt(n420, n419);
    let n423: ZB = zb_and(n411, n421);
    let n424: ZB = zb_and(n411, n422);
    let n425: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n408);
    let n426: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n420);
    let n427: ZN = zn_mget(g.cart, n425, n426);
    let n428: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n427);
    let n429: ZN = zn_rem(n417, zn_splat(P8::from_raw(524288i32)));
    let n430: ZB = zn_ge(n429, zn_splat(P8::from_raw(393216i32)));
    let n431: ZN = zn_mul(n420, zn_splat(P8::from_raw(524288i32)));
    let n432: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n431);
    let n433: ZB = zn_eq(n416, n432);
    let n434: ZB = zb_or(n430, n433);
    let n435: ZB = zb_and(n428, n434);
    let n436: ZB = zn_ge(n398, zn_splat(P8::from_raw(0i32)));
    let n437: ZB = zb_and(n435, n436);
    let n438: ZB = zb_not(n437);
    let n439: ZB = zb_and(n423, n438);
    let n440: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n427);
    let n441: ZN = zn_rem(n400, zn_splat(P8::from_raw(524288i32)));
    let n442: ZB = zn_le(n441, zn_splat(P8::from_raw(131072i32)));
    let n443: ZB = zb_and(n440, n442);
    let n444: ZB = zb_not(n443);
    let n445: ZB = zb_and(n439, n443);
    let n446: ZB = zb_and(n439, n444);
    let n447: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n427);
    let n448: ZN = zn_rem(n329, zn_splat(P8::from_raw(524288i32)));
    let n449: ZB = zn_le(n448, zn_splat(P8::from_raw(131072i32)));
    let n450: ZB = zb_and(n447, n449);
    let n451: ZB = zn_le(n327, zn_splat(P8::from_raw(0i32)));
    let n452: ZB = zb_and(n450, n451);
    let n453: ZB = zb_not(n452);
    let n454: ZB = zb_and(n446, n453);
    let n455: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n427);
    let n456: ZN = zn_rem(n405, zn_splat(P8::from_raw(524288i32)));
    let n457: ZB = zn_ge(n456, zn_splat(P8::from_raw(393216i32)));
    let n458: ZN = zn_mul(n408, zn_splat(P8::from_raw(524288i32)));
    let n459: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n458);
    let n460: ZB = zn_eq(n404, n459);
    let n461: ZB = zb_or(n457, n460);
    let n462: ZB = zb_and(n455, n461);
    let n463: ZB = zn_ge(n327, zn_splat(P8::from_raw(0i32)));
    let n464: ZB = zb_and(n462, n463);
    let n465: ZB = zb_not(n464);
    let n466: ZB = zb_and(n454, n465);
    let n467: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n415);
    let n468: ZB = zn_le(n467, n419);
    let n469: ZB = zn_gt(n467, n419);
    let n470: ZB = zb_and(n466, n468);
    let n471: ZB = zb_and(n466, n469);
    let n472: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n467);
    let n473: ZN = zn_mget(g.cart, n425, n472);
    let n474: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n473);
    let n475: ZN = zn_mul(n467, zn_splat(P8::from_raw(524288i32)));
    let n476: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n475);
    let n477: ZB = zn_eq(n416, n476);
    let n478: ZB = zb_or(n430, n477);
    let n479: ZB = zb_and(n474, n478);
    let n480: ZB = zb_and(n436, n479);
    let n481: ZB = zb_not(n480);
    let n482: ZB = zb_and(n470, n481);
    let n483: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n473);
    let n484: ZB = zb_and(n442, n483);
    let n485: ZB = zb_not(n484);
    let n486: ZB = zb_and(n482, n484);
    let n487: ZB = zb_and(n482, n485);
    let n488: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n473);
    let n489: ZB = zb_and(n449, n488);
    let n490: ZB = zb_and(n451, n489);
    let n491: ZB = zb_not(n490);
    let n492: ZB = zb_and(n487, n491);
    let n493: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n473);
    let n494: ZB = zb_and(n461, n493);
    let n495: ZB = zb_and(n463, n494);
    let n496: ZB = zb_not(n495);
    let n497: ZB = zb_and(n492, n496);
    let n498: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n415);
    let n499: ZB = zn_le(n498, n419);
    let n500: ZB = zn_gt(n498, n419);
    let n501: ZB = zb_and(n497, n499);
    let n502: ZB = zb_and(n497, n500);
    let n503: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n498);
    let n504: ZN = zn_mget(g.cart, n425, n503);
    let n505: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n504);
    let n506: ZN = zn_mul(n498, zn_splat(P8::from_raw(524288i32)));
    let n507: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n506);
    let n508: ZB = zn_eq(n416, n507);
    let n509: ZB = zb_or(n430, n508);
    let n510: ZB = zb_and(n505, n509);
    let n511: ZB = zb_and(n436, n510);
    let n512: ZB = zb_not(n511);
    let n513: ZB = zb_and(n501, n512);
    let n514: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n504);
    let n515: ZB = zb_and(n442, n514);
    let n516: ZB = zb_not(n515);
    let n517: ZB = zb_and(n513, n515);
    let n518: ZB = zb_and(n513, n516);
    let n519: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n504);
    let n520: ZB = zb_and(n449, n519);
    let n521: ZB = zb_and(n451, n520);
    let n522: ZB = zb_not(n521);
    let n523: ZB = zb_and(n518, n522);
    let n524: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n504);
    let n525: ZB = zb_and(n461, n524);
    let n526: ZB = zb_and(n463, n525);
    let n527: ZB = zb_not(n526);
    let n528: ZB = zb_and(n523, n527);
    let n529: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n415);
    let n530: ZB = zn_gt(n529, n419);
    let n531: ZB = zb_and(n399, n530);
    let n532: ZB = zb_or(n502, n528);
    let n533: ZB = zsel_b(n500, n399, n531);
    let n534: ZB = zb_or(n471, n532);
    let n535: ZB = zsel_b(n469, n399, n533);
    let n536: ZB = zb_or(n424, n534);
    let n537: ZB = zsel_b(n422, n399, n535);
    let n538: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n403);
    let n539: ZB = zn_le(n538, n407);
    let n540: ZB = zn_gt(n538, n407);
    let n541: ZB = zb_and(n536, n539);
    let n542: ZB = zb_and(n536, n540);
    let n543: ZB = zb_and(n422, n541);
    let n544: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n538);
    let n545: ZN = zn_mget(g.cart, n544, n426);
    let n546: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n545);
    let n547: ZB = zb_and(n421, n536);
    let n548: ZB = zb_and(n539, n547);
    let n549: ZB = zb_and(n434, n546);
    let n550: ZB = zb_and(n436, n549);
    let n551: ZB = zb_not(n550);
    let n552: ZB = zb_and(n548, n551);
    let n553: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n545);
    let n554: ZB = zb_and(n442, n553);
    let n555: ZB = zb_not(n554);
    let n556: ZB = zb_and(n552, n554);
    let n557: ZB = zb_and(n552, n555);
    let n558: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n545);
    let n559: ZB = zb_and(n449, n558);
    let n560: ZB = zb_and(n451, n559);
    let n561: ZB = zb_not(n560);
    let n562: ZB = zb_and(n557, n561);
    let n563: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n545);
    let n564: ZN = zn_mul(n538, zn_splat(P8::from_raw(524288i32)));
    let n565: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n564);
    let n566: ZB = zn_eq(n404, n565);
    let n567: ZB = zb_or(n457, n566);
    let n568: ZB = zb_and(n563, n567);
    let n569: ZB = zb_and(n463, n568);
    let n570: ZB = zb_not(n569);
    let n571: ZB = zb_and(n562, n570);
    let n572: ZB = zb_and(n469, n571);
    let n573: ZN = zn_mget(g.cart, n544, n472);
    let n574: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n573);
    let n575: ZB = zb_and(n468, n562);
    let n576: ZB = zb_and(n570, n575);
    let n577: ZB = zb_and(n478, n574);
    let n578: ZB = zb_and(n436, n577);
    let n579: ZB = zb_not(n578);
    let n580: ZB = zb_and(n576, n579);
    let n581: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n573);
    let n582: ZB = zb_and(n442, n581);
    let n583: ZB = zb_not(n582);
    let n584: ZB = zb_and(n580, n582);
    let n585: ZB = zb_and(n580, n583);
    let n586: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n573);
    let n587: ZB = zb_and(n449, n586);
    let n588: ZB = zb_and(n451, n587);
    let n589: ZB = zb_not(n588);
    let n590: ZB = zb_and(n585, n589);
    let n591: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n573);
    let n592: ZB = zb_and(n567, n591);
    let n593: ZB = zb_and(n463, n592);
    let n594: ZB = zb_not(n593);
    let n595: ZB = zb_and(n590, n594);
    let n596: ZB = zb_and(n500, n595);
    let n597: ZN = zn_mget(g.cart, n544, n503);
    let n598: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n597);
    let n599: ZB = zb_and(n499, n590);
    let n600: ZB = zb_and(n594, n599);
    let n601: ZB = zb_and(n509, n598);
    let n602: ZB = zb_and(n436, n601);
    let n603: ZB = zb_not(n602);
    let n604: ZB = zb_and(n600, n603);
    let n605: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n597);
    let n606: ZB = zb_and(n442, n605);
    let n607: ZB = zb_not(n606);
    let n608: ZB = zb_and(n604, n606);
    let n609: ZB = zb_and(n604, n607);
    let n610: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n597);
    let n611: ZB = zb_and(n449, n610);
    let n612: ZB = zb_and(n451, n611);
    let n613: ZB = zb_not(n612);
    let n614: ZB = zb_and(n609, n613);
    let n615: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n597);
    let n616: ZB = zb_and(n567, n615);
    let n617: ZB = zb_and(n463, n616);
    let n618: ZB = zb_not(n617);
    let n619: ZB = zb_and(n614, n618);
    let n620: ZB = zb_and(n530, n537);
    let n621: ZB = zb_or(n596, n619);
    let n622: ZB = zsel_b(n500, n537, n620);
    let n623: ZB = zb_or(n572, n621);
    let n624: ZB = zsel_b(n469, n537, n622);
    let n625: ZB = zb_or(n543, n623);
    let n626: ZB = zsel_b(n422, n537, n624);
    let n627: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n403);
    let n628: ZB = zn_le(n627, n407);
    let n629: ZB = zn_gt(n627, n407);
    let n630: ZB = zb_and(n625, n628);
    let n631: ZB = zb_and(n625, n629);
    let n632: ZB = zb_and(n422, n630);
    let n633: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n627);
    let n634: ZN = zn_mget(g.cart, n633, n426);
    let n635: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n634);
    let n636: ZB = zb_and(n421, n625);
    let n637: ZB = zb_and(n628, n636);
    let n638: ZB = zb_and(n434, n635);
    let n639: ZB = zb_and(n436, n638);
    let n640: ZB = zb_not(n639);
    let n641: ZB = zb_and(n637, n640);
    let n642: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n634);
    let n643: ZB = zb_and(n442, n642);
    let n644: ZB = zb_not(n643);
    let n645: ZB = zb_and(n641, n643);
    let n646: ZB = zb_and(n641, n644);
    let n647: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n634);
    let n648: ZB = zb_and(n449, n647);
    let n649: ZB = zb_and(n451, n648);
    let n650: ZB = zb_not(n649);
    let n651: ZB = zb_and(n646, n650);
    let n652: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n634);
    let n653: ZN = zn_mul(n627, zn_splat(P8::from_raw(524288i32)));
    let n654: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n653);
    let n655: ZB = zn_eq(n404, n654);
    let n656: ZB = zb_or(n457, n655);
    let n657: ZB = zb_and(n652, n656);
    let n658: ZB = zb_and(n463, n657);
    let n659: ZB = zb_not(n658);
    let n660: ZB = zb_and(n651, n659);
    let n661: ZB = zb_and(n469, n660);
    let n662: ZN = zn_mget(g.cart, n633, n472);
    let n663: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n662);
    let n664: ZB = zb_and(n468, n651);
    let n665: ZB = zb_and(n659, n664);
    let n666: ZB = zb_and(n478, n663);
    let n667: ZB = zb_and(n436, n666);
    let n668: ZB = zb_not(n667);
    let n669: ZB = zb_and(n665, n668);
    let n670: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n662);
    let n671: ZB = zb_and(n442, n670);
    let n672: ZB = zb_not(n671);
    let n673: ZB = zb_and(n669, n671);
    let n674: ZB = zb_and(n669, n672);
    let n675: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n662);
    let n676: ZB = zb_and(n449, n675);
    let n677: ZB = zb_and(n451, n676);
    let n678: ZB = zb_not(n677);
    let n679: ZB = zb_and(n674, n678);
    let n680: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n662);
    let n681: ZB = zb_and(n656, n680);
    let n682: ZB = zb_and(n463, n681);
    let n683: ZB = zb_not(n682);
    let n684: ZB = zb_and(n679, n683);
    let n685: ZB = zb_and(n500, n684);
    let n686: ZN = zn_mget(g.cart, n633, n503);
    let n687: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n686);
    let n688: ZB = zb_and(n499, n679);
    let n689: ZB = zb_and(n683, n688);
    let n690: ZB = zb_and(n509, n687);
    let n691: ZB = zb_and(n436, n690);
    let n692: ZB = zb_not(n691);
    let n693: ZB = zb_and(n689, n692);
    let n694: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n686);
    let n695: ZB = zb_and(n442, n694);
    let n696: ZB = zb_not(n695);
    let n697: ZB = zb_and(n693, n695);
    let n698: ZB = zb_and(n693, n696);
    let n699: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n686);
    let n700: ZB = zb_and(n449, n699);
    let n701: ZB = zb_and(n451, n700);
    let n702: ZB = zb_not(n701);
    let n703: ZB = zb_and(n698, n702);
    let n704: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n686);
    let n705: ZB = zb_and(n656, n704);
    let n706: ZB = zb_and(n463, n705);
    let n707: ZB = zb_not(n706);
    let n708: ZB = zb_and(n703, n707);
    let n709: ZB = zb_and(n530, n626);
    let n710: ZB = zb_or(n685, n708);
    let n711: ZB = zsel_b(n500, n626, n709);
    let n712: ZB = zb_or(n661, n710);
    let n713: ZB = zsel_b(n469, n626, n711);
    let n714: ZB = zb_or(n632, n712);
    let n715: ZB = zsel_b(n422, n626, n713);
    let n716: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n403);
    let n717: ZB = zn_gt(n716, n407);
    let n718: ZB = zb_and(n715, n717);
    let n719: ZB = zb_or(n631, n714);
    let n720: ZB = zsel_b(n629, n626, n718);
    let n721: ZB = zb_or(n542, n719);
    let n722: ZB = zsel_b(n540, n537, n720);
    let n723: ZB = zb_or(n412, n721);
    let n724: ZB = zsel_b(n410, n399, n722);
    let n725: ZB = zn_le(n396, zn_splat(P8::from_raw(8388608i32)));
    let n726: ZB = zb_and(n723, n725);
    let n727: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n400);
    let n728: ZB = zn_tile_flag_at(g.cache, g.cart, n330, n727, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n729: ZB = zb_not(n728);
    let n730: ZN = zsel_n(n728, n219, r_c256);
    let n731: ZN = zsel_n(n728, zn_splat(P8::from_raw(393216i32)), n222);
    let n732: ZN = zsel_n(n729, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n733: ZN = zn_abs(n327);
    let n734: ZB = zn_gt(n733, zn_splat(P8::from_raw(65536i32)));
    let n735: ZB = zn_gt(n327, zn_splat(P8::from_raw(0i32)));
    let n736: ZB = zn_lt(n327, zn_splat(P8::from_raw(0i32)));
    let n737: ZB = zn_gt(n327, zn_splat(P8::from_raw(65536i32)));
    let n738: ZN = zn_sub(n327, zn_splat(P8::from_raw(9830i32)));
    let n739: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n738);
    let n740: ZN = zn_add(zn_splat(P8::from_raw(9830i32)), n327);
    let n741: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n740);
    let n742: ZB = zn_gt(n327, zn_splat(P8::from_raw(-65536i32)));
    let n743: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n738);
    let n744: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n740);
    let n745: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n738);
    let n746: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n740);
    let n747: ZN = zsel_n(n742, n743, n744);
    let n748: ZN = zsel_n(n735, n745, n746);
    let n749: ZN = zsel_n(n737, n739, n741);
    let n750: ZN = zsel_n(n736, n747, n748);
    let n751: ZN = zsel_n(n735, n749, n750);
    let n752: ZN = zn_sub(n327, n732);
    let n753: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n752);
    let n754: ZN = zn_add(n327, n732);
    let n755: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n754);
    let n756: ZN = zsel_n(n735, n753, n755);
    let n757: ZN = zsel_n(n734, n751, n756);
    let n758: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n757);
    let n759: ZB = zb_not(n758);
    let n760: ZB = zn_lt(n757, zn_splat(P8::from_raw(0i32)));
    let n761: ZB = zsel_b(n759, n760, r_c312);
    let n762: ZN = zn_abs(n398);
    let n763: ZB = zn_le(n762, zn_splat(P8::from_raw(9830i32)));
    let n764: ZN = zsel_n(n763, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(13762i32)));
    let n765: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n400);
    let n766: ZN = zn_add(n398, n764);
    let n767: ZN = zn_min(zn_splat(P8::from_raw(131072i32)), n766);
    let n768: ZN = zsel_n(n729, n767, n398);
    let n769: ZB = zn_gt(n731, zn_splat(P8::from_raw(0i32)));
    let n770: ZN = zn_add(zn_splat(P8::from_raw(-196608i32)), n329);
    let n771: ZB = zn_tile_flag_at(g.cache, g.cart, n770, n765, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n772: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n329);
    let n773: ZB = zn_tile_flag_at(g.cache, g.cart, n772, n765, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n774: ZN = zsel_n(n773, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n775: ZN = zsel_n(n771, zn_splat(P8::from_raw(-65536i32)), n774);
    let n776: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n775);
    let n777: ZB = zb_not(n776);
    let n778: ZN = zn_neg(n775);
    let n779: ZN = zn_mul(n778, zn_splat(P8::from_raw(131072i32)));
    let n780: ZN = zsel_n(n777, n779, n757);
    let n781: ZN = zsel_n(n777, zn_splat(P8::from_raw(-131072i32)), n768);
    let n782: ZN = zsel_n(n769, zn_splat(P8::from_raw(0i32)), n731);
    let n783: ZN = zsel_n(n769, n757, n780);
    let n784: ZN = zsel_n(n769, zn_splat(P8::from_raw(-131072i32)), n781);
    let n785: ZB = zn_gt(n730, zn_splat(P8::from_raw(0i32)));
    let n786: ZN = zsel_n(n761, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n787: ZB = zn_gt(n786, zn_splat(P8::from_raw(0i32)));
    let n788: ZB = zn_lt(n786, zn_splat(P8::from_raw(0i32)));
    let n789: ZN = zsel_n(n788, zn_splat(P8::from_raw(-131072i32)), zn_splat(P8::from_raw(0i32)));
    let n790: ZN = zsel_n(n787, zn_splat(P8::from_raw(131072i32)), n789);
    let n791: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n786);
    let n792: ZB = zb_not(n791);
    let n793: ZN = zsel_n(n792, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n794: ZB = zn_lt(n396, zn_splat(P8::from_raw(-262144i32)));
    let n795: ZB = zn_ge(n396, zn_splat(P8::from_raw(-262144i32)));
    let n796: ZB = zb_and(n726, n794);
    let n798: ZB = zn_gt(n404, zn_splat(P8::from_raw(786432i32)));
    let n800: ZB = zn_gt(n416, zn_splat(P8::from_raw(2359296i32)));
    let n801: ZB = zb_and(n798, n800);
    let n802: ZB = zn_lt(n329, zn_splat(P8::from_raw(1310720i32)));
    let n803: ZB = zb_and(n801, n802);
    let n804: ZB = zn_lt(n400, zn_splat(P8::from_raw(2883584i32)));
    let n805: ZB = zb_and(n803, n804);
    let n806: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n807: ZB = zn_lt(n325, zn_splat(P8::from_raw(-65536i32)));
    let n808: ZB = zn_gt(n325, zn_splat(P8::from_raw(7929856i32)));
    let n814: ZB = zb_or(n807, n808);
    let n815: ZN = zn_min(zn_splat(P8::from_raw(7929856i32)), n325);
    let n816: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n815);
    let n817: ZN = zsel_n(n814, n816, n325);
    let n818: ZN = zsel_n(n814, zn_splat(P8::from_raw(0i32)), n757);
    let n819: ZN = zsel_n(n806, n325, n817);
    let n820: ZN = zsel_n(n806, n757, n818);
    let n833: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n752);
    let n834: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n754);
    let n835: ZN = zsel_n(n742, n833, n834);
    let n836: ZN = zsel_n(n734, n751, n835);
    let n837: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n836);
    let n838: ZB = zb_not(n837);
    let n839: ZB = zn_lt(n836, zn_splat(P8::from_raw(0i32)));
    let n840: ZB = zsel_b(n838, n839, r_c312);
    let n841: ZN = zn_add(zn_splat(P8::from_raw(-65536i32)), n329);
    let n842: ZB = zn_tile_flag_at(g.cache, g.cart, n841, n765, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n843: ZN = zsel_n(n842, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n844: ZN = zn_min(n766, n843);
    let n845: ZN = zsel_n(n729, n844, n398);
    let n846: ZN = zsel_n(n777, n779, n836);
    let n847: ZN = zsel_n(n777, zn_splat(P8::from_raw(-131072i32)), n845);
    let n848: ZN = zsel_n(n769, n836, n846);
    let n849: ZN = zsel_n(n769, zn_splat(P8::from_raw(-131072i32)), n847);
    let n850: ZN = zsel_n(n814, zn_splat(P8::from_raw(0i32)), n836);
    let n851: ZN = zsel_n(n806, n836, n850);
    let n852: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n752);
    let n853: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n754);
    let n854: ZN = zsel_n(n737, n852, n853);
    let n855: ZN = zsel_n(n734, n751, n854);
    let n856: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n855);
    let n857: ZB = zb_not(n856);
    let n858: ZB = zn_lt(n855, zn_splat(P8::from_raw(0i32)));
    let n859: ZB = zsel_b(n857, n858, r_c312);
    let n860: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n329);
    let n861: ZB = zn_tile_flag_at(g.cache, g.cart, n860, n765, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n862: ZN = zsel_n(n861, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n863: ZN = zn_min(n766, n862);
    let n864: ZN = zsel_n(n729, n863, n398);
    let n865: ZN = zsel_n(n777, n779, n855);
    let n866: ZN = zsel_n(n777, zn_splat(P8::from_raw(-131072i32)), n864);
    let n867: ZN = zsel_n(n769, n855, n865);
    let n868: ZN = zsel_n(n769, zn_splat(P8::from_raw(-131072i32)), n866);
    let n869: ZN = zsel_n(n814, zn_splat(P8::from_raw(0i32)), n855);
    let n870: ZN = zsel_n(n806, n855, n869);
    let n871: ZN = zsel_n(n216, n782, n731);
    let n872: ZN = zsel_n(n216, n783, n757);
    let n873: ZN = zsel_n(n216, n784, n768);
    let n879: ZN = zsel_n(n814, zn_splat(P8::from_raw(0i32)), n872);
    let n880: ZN = zsel_n(n806, n872, n879);
    let n881: ZN = zsel_n(n216, n848, n836);
    let n882: ZN = zsel_n(n216, n849, n845);
    let n883: ZN = zsel_n(n814, zn_splat(P8::from_raw(0i32)), n881);
    let n884: ZN = zsel_n(n806, n881, n883);
    let n885: ZN = zsel_n(n216, n867, n855);
    let n886: ZN = zsel_n(n216, n868, n864);
    let n887: ZN = zsel_n(n814, zn_splat(P8::from_raw(0i32)), n885);
    let n888: ZN = zsel_n(n806, n885, n887);
    let n889: ZB = zb_and(n217, n785);
    let n890: ZN = zsel_n(n889, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n891: ZB = zb_or(r_c41, n889);
    let n892: ZN = zsel_n(n889, zn_splat(P8::from_raw(655360i32)), n223);
    let n893: ZN = zsel_n(n889, zn_splat(P8::from_raw(262144i32)), zn_splat(P8::from_raw(-65536i32)));
    let n894: ZN = zsel_n(n889, zn_splat(P8::from_raw(98304i32)), r_c308);
    let n895: ZN = zsel_n(n889, n793, r_c309);
    let n896: ZN = zsel_n(n889, n790, r_c310);
    let n897: ZN = zsel_n(n889, zn_splat(P8::from_raw(0i32)), r_c311);
    let n898: ZN = zsel_n(n889, n786, n757);
    let n899: ZN = zsel_n(n889, zn_splat(P8::from_raw(0i32)), n768);
    let n900: ZB = zn_gt(n890, zn_splat(P8::from_raw(0i32)));
    let n901: ZN = zsel_n(n814, zn_splat(P8::from_raw(0i32)), n898);
    let n902: ZN = zsel_n(n900, n325, n817);
    let n903: ZN = zsel_n(n900, n898, n901);
    let n907: ZN = zsel_n(n889, zn_splat(P8::from_raw(69510i32)), r_c309);
    let n908: ZN = zsel_n(n889, zn_splat(P8::from_raw(-131072i32)), r_c310);
    let n909: ZN = zsel_n(n889, zn_splat(P8::from_raw(-327680i32)), n836);
    let n910: ZN = zsel_n(n889, zn_splat(P8::from_raw(0i32)), n845);
    let n911: ZN = zsel_n(n814, zn_splat(P8::from_raw(0i32)), n909);
    let n912: ZN = zsel_n(n900, n909, n911);
    let n913: ZN = zsel_n(n889, zn_splat(P8::from_raw(131072i32)), r_c310);
    let n914: ZN = zsel_n(n889, zn_splat(P8::from_raw(327680i32)), n855);
    let n915: ZN = zsel_n(n889, zn_splat(P8::from_raw(0i32)), n864);
    let n916: ZN = zsel_n(n814, zn_splat(P8::from_raw(0i32)), n914);
    let n917: ZN = zsel_n(n900, n914, n916);
    let n918: ZN = zsel_n(n889, zn_splat(P8::from_raw(69510i32)), r_c308);
    let n919: ZN = zsel_n(n889, zn_splat(P8::from_raw(98304i32)), r_c309);
    let n920: ZN = zsel_n(n889, zn_splat(P8::from_raw(0i32)), r_c310);
    let n921: ZN = zsel_n(n889, zn_splat(P8::from_raw(-98304i32)), r_c311);
    let n922: ZN = zsel_n(n889, zn_splat(P8::from_raw(0i32)), n757);
    let n923: ZN = zsel_n(n889, zn_splat(P8::from_raw(-327680i32)), n768);
    let n924: ZN = zsel_n(n814, zn_splat(P8::from_raw(0i32)), n922);
    let n925: ZN = zsel_n(n900, n922, n924);
    let n926: ZN = zsel_n(n889, zn_splat(P8::from_raw(-231700i32)), n836);
    let n927: ZN = zsel_n(n889, zn_splat(P8::from_raw(-231700i32)), n845);
    let n928: ZN = zsel_n(n814, zn_splat(P8::from_raw(0i32)), n926);
    let n929: ZN = zsel_n(n900, n926, n928);
    let n930: ZN = zsel_n(n889, zn_splat(P8::from_raw(231700i32)), n855);
    let n931: ZN = zsel_n(n889, zn_splat(P8::from_raw(-231700i32)), n864);
    let n932: ZN = zsel_n(n814, zn_splat(P8::from_raw(0i32)), n930);
    let n933: ZN = zsel_n(n900, n930, n932);
    let n934: ZN = zsel_n(n889, zn_splat(P8::from_raw(131072i32)), r_c311);
    let n935: ZN = zsel_n(n889, zn_splat(P8::from_raw(327680i32)), n768);
    let n936: ZN = zsel_n(n889, zn_splat(P8::from_raw(231700i32)), n845);
    let n937: ZN = zsel_n(n889, zn_splat(P8::from_raw(231700i32)), n864);
    let n938: ZN = zsel_n(n889, n786, n872);
    let n939: ZN = zsel_n(n889, zn_splat(P8::from_raw(0i32)), n873);
    let n940: ZN = zsel_n(n814, zn_splat(P8::from_raw(0i32)), n938);
    let n941: ZN = zsel_n(n900, n938, n940);
    let n942: ZN = zsel_n(n889, zn_splat(P8::from_raw(-327680i32)), n881);
    let n943: ZN = zsel_n(n889, zn_splat(P8::from_raw(0i32)), n882);
    let n944: ZN = zsel_n(n814, zn_splat(P8::from_raw(0i32)), n942);
    let n945: ZN = zsel_n(n900, n942, n944);
    let n946: ZN = zsel_n(n889, zn_splat(P8::from_raw(327680i32)), n885);
    let n947: ZN = zsel_n(n889, zn_splat(P8::from_raw(0i32)), n886);
    let n948: ZN = zsel_n(n814, zn_splat(P8::from_raw(0i32)), n946);
    let n949: ZN = zsel_n(n900, n946, n948);
    let n950: ZN = zsel_n(n889, zn_splat(P8::from_raw(0i32)), n872);
    let n951: ZN = zsel_n(n889, zn_splat(P8::from_raw(-327680i32)), n873);
    let n952: ZN = zsel_n(n814, zn_splat(P8::from_raw(0i32)), n950);
    let n953: ZN = zsel_n(n900, n950, n952);
    let n954: ZN = zsel_n(n889, zn_splat(P8::from_raw(-231700i32)), n881);
    let n955: ZN = zsel_n(n889, zn_splat(P8::from_raw(-231700i32)), n882);
    let n956: ZN = zsel_n(n814, zn_splat(P8::from_raw(0i32)), n954);
    let n957: ZN = zsel_n(n900, n954, n956);
    let n958: ZN = zsel_n(n889, zn_splat(P8::from_raw(231700i32)), n885);
    let n959: ZN = zsel_n(n889, zn_splat(P8::from_raw(-231700i32)), n886);
    let n960: ZN = zsel_n(n814, zn_splat(P8::from_raw(0i32)), n958);
    let n961: ZN = zsel_n(n900, n958, n960);
    let n962: ZN = zsel_n(n889, zn_splat(P8::from_raw(327680i32)), n873);
    let n963: ZN = zsel_n(n889, zn_splat(P8::from_raw(231700i32)), n882);
    let n964: ZN = zsel_n(n889, zn_splat(P8::from_raw(231700i32)), n886);
    let n966: ZN = zn_sub(n730, zn_splat(P8::from_raw(65536i32)));
    let n967: ZB = zb_not(n805);
    let n970: ZN = zsel_n(n889, n966, n730);
    let n971: ZB = zb_and(n423, n437);
    let n972: ZB = zb_and(n446, n452);
    let n973: ZB = zb_and(n454, n464);
    let n974: ZB = zb_or(n972, n973);
    let n975: ZB = zb_or(n445, n974);
    let n976: ZB = zb_or(n971, n975);
    let n977: ZB = zb_and(n470, n480);
    let n978: ZB = zb_and(n487, n490);
    let n979: ZB = zb_and(n492, n495);
    let n980: ZB = zb_or(n978, n979);
    let n981: ZB = zb_or(n486, n980);
    let n982: ZB = zb_or(n977, n981);
    let n983: ZB = zb_and(n501, n511);
    let n984: ZB = zb_and(n518, n521);
    let n985: ZB = zb_and(n523, n526);
    let n986: ZB = zb_or(n984, n985);
    let n987: ZB = zb_or(n517, n986);
    let n988: ZB = zb_or(n983, n987);
    let n989: ZB = zb_or(n982, n988);
    let n990: ZB = zb_or(n976, n989);
    let n991: ZB = zb_and(n548, n550);
    let n992: ZB = zb_and(n557, n560);
    let n993: ZB = zb_and(n562, n569);
    let n994: ZB = zb_or(n992, n993);
    let n995: ZB = zb_or(n556, n994);
    let n996: ZB = zb_or(n991, n995);
    let n997: ZB = zb_and(n576, n578);
    let n998: ZB = zb_and(n585, n588);
    let n999: ZB = zb_and(n590, n593);
    let n1000: ZB = zb_or(n998, n999);
    let n1001: ZB = zb_or(n584, n1000);
    let n1002: ZB = zb_or(n997, n1001);
    let n1003: ZB = zb_and(n600, n602);
    let n1004: ZB = zb_and(n609, n612);
    let n1005: ZB = zb_and(n614, n617);
    let n1006: ZB = zb_or(n1004, n1005);
    let n1007: ZB = zb_or(n608, n1006);
    let n1008: ZB = zb_or(n1003, n1007);
    let n1009: ZB = zb_or(n1002, n1008);
    let n1010: ZB = zb_or(n996, n1009);
    let n1011: ZB = zb_and(n637, n639);
    let n1012: ZB = zb_and(n646, n649);
    let n1013: ZB = zb_and(n651, n658);
    let n1014: ZB = zb_or(n1012, n1013);
    let n1015: ZB = zb_or(n645, n1014);
    let n1016: ZB = zb_or(n1011, n1015);
    let n1017: ZB = zb_and(n665, n667);
    let n1018: ZB = zb_and(n674, n677);
    let n1019: ZB = zb_and(n679, n682);
    let n1020: ZB = zb_or(n1018, n1019);
    let n1021: ZB = zb_or(n673, n1020);
    let n1022: ZB = zb_or(n1017, n1021);
    let n1023: ZB = zb_and(n689, n691);
    let n1024: ZB = zb_and(n698, n701);
    let n1025: ZB = zb_and(n703, n706);
    let n1026: ZB = zb_or(n1024, n1025);
    let n1027: ZB = zb_or(n697, n1026);
    let n1028: ZB = zb_or(n1023, n1027);
    let n1029: ZB = zb_or(n1022, n1028);
    let n1030: ZB = zb_or(n1016, n1029);
    let n1031: ZB = zb_or(n1010, n1030);
    let n1032: ZB = zsel_b(n1010, n537, n626);
    let n1033: ZB = zb_or(n990, n1031);
    let n1034: ZB = zsel_b(n990, n399, n1032);
    let n1035: ZB = zn_gt(n396, zn_splat(P8::from_raw(8388608i32)));
    let n1036: ZB = zb_and(n723, n1035);
    let n1037: ZB = zb_or(n1033, n1036);
    let n1038: ZB = zsel_b(n1033, n1034, n724);
    let n1039: ZB = zb_and(n794, n1037);
    let n1041: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c87);
    let n1042: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1041);
    let n1043: ZN = zsel_n(n1035, n1042, n1041);
    let n1044: ZN = zsel_n(n1033, n1043, n1041);
    let n1048: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c320);
    let n1051: ZB = zn_le(r_c253, zn_splat(P8::from_raw(0i32)));
    let n1052: ZB = zb_not(n1048);
    let n1053: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c321);
    let n1054: ZB = zb_not(n1053);
    let n1055: ZB = zb_or(n1052, n1054);
    let n1056: ZB = zb_not(n1055);
    let n1057: ZN = zn_add(r_c318, r_c320);
    let n1058: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n1057);
    let n1059: ZN = zn_flr(n1058);
    let n1060: ZB = zn_gt(n1059, zn_splat(P8::from_raw(0i32)));
    let n1061: ZB = zn_lt(n1059, zn_splat(P8::from_raw(0i32)));
    let n1062: ZN = zsel_n(n1061, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n1063: ZN = zsel_n(n1060, zn_splat(P8::from_raw(65536i32)), n1062);
    let n1064: ZN = zn_abs(n1059);
    let n1065: ZB = zn_gt(zn_splat(P8::from_raw(3145728i32)), n166);
    let n1066: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n147);
    let n1067: ZB = zn_lt(zn_splat(P8::from_raw(2097152i32)), n1066);
    let n1068: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n1064);
    let n1069: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n1064);
    let n1070: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n1064);
    let n1071: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n1064);
    let n1072: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n1064);
    let n1073: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n1064);
    let n1074: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n1064);
    let n1075: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n1064);
    let n1076: ZN = zn_add(r_c319, r_c321);
    let n1077: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n1076);
    let n1078: ZN = zn_flr(n1077);
    let n1079: ZB = zn_gt(n1078, zn_splat(P8::from_raw(0i32)));
    let n1080: ZB = zn_lt(n1078, zn_splat(P8::from_raw(0i32)));
    let n1081: ZN = zsel_n(n1080, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n1082: ZN = zsel_n(n1079, zn_splat(P8::from_raw(65536i32)), n1081);
    let n1083: ZN = zn_abs(n1078);
    let n1084: ZN = zn_add(n146, n1082);
    let n1085: ZB = zn_gt(zn_splat(P8::from_raw(3145728i32)), n1084);
    let n1086: ZN = zn_add(n147, n1082);
    let n1087: ZB = zn_lt(zn_splat(P8::from_raw(2097152i32)), n1086);
    let n1088: ZN = zn_add(r_c273, n1082);
    let n1089: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n1083);
    let n1090: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1088);
    let n1091: ZN = zn_add(n1082, n1090);
    let n1092: ZB = zn_gt(zn_splat(P8::from_raw(3145728i32)), n1091);
    let n1093: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n1090);
    let n1094: ZN = zn_add(n1082, n1093);
    let n1095: ZB = zn_lt(zn_splat(P8::from_raw(2097152i32)), n1094);
    let n1096: ZN = zn_add(n1082, n1088);
    let n1097: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n1083);
    let n1098: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1096);
    let n1099: ZN = zn_add(n1082, n1098);
    let n1100: ZB = zn_gt(zn_splat(P8::from_raw(3145728i32)), n1099);
    let n1101: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n1098);
    let n1102: ZN = zn_add(n1082, n1101);
    let n1103: ZB = zn_lt(zn_splat(P8::from_raw(2097152i32)), n1102);
    let n1104: ZN = zn_add(n1082, n1096);
    let n1105: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n1083);
    let n1106: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1104);
    let n1107: ZN = zn_add(n1082, n1106);
    let n1108: ZB = zn_gt(zn_splat(P8::from_raw(3145728i32)), n1107);
    let n1109: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n1106);
    let n1110: ZN = zn_add(n1082, n1109);
    let n1111: ZB = zn_lt(zn_splat(P8::from_raw(2097152i32)), n1110);
    let n1112: ZN = zn_add(n1082, n1104);
    let n1113: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n1083);
    let n1114: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1112);
    let n1115: ZN = zn_add(n1082, n1114);
    let n1116: ZB = zn_gt(zn_splat(P8::from_raw(3145728i32)), n1115);
    let n1117: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n1114);
    let n1118: ZN = zn_add(n1082, n1117);
    let n1119: ZB = zn_lt(zn_splat(P8::from_raw(2097152i32)), n1118);
    let n1120: ZN = zn_add(n1082, n1112);
    let n1121: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n1083);
    let n1122: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1120);
    let n1123: ZN = zn_add(n1082, n1122);
    let n1124: ZB = zn_gt(zn_splat(P8::from_raw(3145728i32)), n1123);
    let n1125: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n1122);
    let n1126: ZN = zn_add(n1082, n1125);
    let n1127: ZB = zn_lt(zn_splat(P8::from_raw(2097152i32)), n1126);
    let n1128: ZN = zn_add(n1082, n1120);
    let n1129: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n1083);
    let n1130: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1128);
    let n1131: ZN = zn_add(n1082, n1130);
    let n1132: ZB = zn_gt(zn_splat(P8::from_raw(3145728i32)), n1131);
    let n1133: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n1130);
    let n1134: ZN = zn_add(n1082, n1133);
    let n1135: ZB = zn_lt(zn_splat(P8::from_raw(2097152i32)), n1134);
    let n1136: ZN = zn_add(n1082, n1128);
    let n1137: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n1083);
    let n1138: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1136);
    let n1139: ZN = zn_add(n1082, n1138);
    let n1140: ZB = zn_gt(zn_splat(P8::from_raw(3145728i32)), n1139);
    let n1141: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n1138);
    let n1142: ZN = zn_add(n1082, n1141);
    let n1143: ZB = zn_lt(zn_splat(P8::from_raw(2097152i32)), n1142);
    let n1144: ZN = zn_add(n1082, n1136);
    let n1145: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n1083);
    let n1146: ZB = zn_gt(r_c255, zn_splat(P8::from_raw(0i32)));
    let n1147: ZB = zb_not(n231);
    let n1148: ZB = zb_and(n111, n1147);
    let n1149: ZB = zb_and(n232, n1051);
    let n1150: ZB = zb_or(n1148, n1149);
    let n1151: ZN = zn_add(n225, n1063);
    let n1152: ZB = zn_tile_flag_at(g.cache, g.cart, n1151, n166, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1153: ZB = zn_gt(zn_splat(P8::from_raw(1572864i32)), n1151);
    let n1154: ZB = zb_and(n1065, n1153);
    let n1155: ZN = zn_add(n226, n1063);
    let n1156: ZB = zn_lt(zn_splat(P8::from_raw(524288i32)), n1155);
    let n1157: ZB = zb_and(n1154, n1156);
    let n1158: ZB = zb_and(n1067, n1157);
    let n1159: ZB = zb_or(n1152, n1158);
    let n1160: ZN = zn_add(r_c272, n1063);
    let n1161: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1160);
    let n1162: ZN = zn_add(n1063, n1161);
    let n1163: ZB = zn_tile_flag_at(g.cache, g.cart, n1162, n166, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1164: ZB = zn_gt(zn_splat(P8::from_raw(1572864i32)), n1162);
    let n1165: ZB = zb_and(n1065, n1164);
    let n1166: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n1161);
    let n1167: ZN = zn_add(n1063, n1166);
    let n1168: ZB = zn_lt(zn_splat(P8::from_raw(524288i32)), n1167);
    let n1169: ZB = zb_and(n1165, n1168);
    let n1170: ZB = zb_and(n1067, n1169);
    let n1171: ZB = zb_or(n1163, n1170);
    let n1172: ZN = zn_add(n1063, n1160);
    let n1173: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1172);
    let n1174: ZN = zn_add(n1063, n1173);
    let n1175: ZB = zn_tile_flag_at(g.cache, g.cart, n1174, n166, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1176: ZB = zn_gt(zn_splat(P8::from_raw(1572864i32)), n1174);
    let n1177: ZB = zb_and(n1065, n1176);
    let n1178: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n1173);
    let n1179: ZN = zn_add(n1063, n1178);
    let n1180: ZB = zn_lt(zn_splat(P8::from_raw(524288i32)), n1179);
    let n1181: ZB = zb_and(n1177, n1180);
    let n1182: ZB = zb_and(n1067, n1181);
    let n1183: ZB = zb_or(n1175, n1182);
    let n1184: ZN = zn_add(n1063, n1172);
    let n1185: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1184);
    let n1186: ZN = zn_add(n1063, n1185);
    let n1187: ZB = zn_tile_flag_at(g.cache, g.cart, n1186, n166, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1188: ZB = zn_gt(zn_splat(P8::from_raw(1572864i32)), n1186);
    let n1189: ZB = zb_and(n1065, n1188);
    let n1190: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n1185);
    let n1191: ZN = zn_add(n1063, n1190);
    let n1192: ZB = zn_lt(zn_splat(P8::from_raw(524288i32)), n1191);
    let n1193: ZB = zb_and(n1189, n1192);
    let n1194: ZB = zb_and(n1067, n1193);
    let n1195: ZB = zb_or(n1187, n1194);
    let n1196: ZN = zn_add(n1063, n1184);
    let n1197: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1196);
    let n1198: ZN = zn_add(n1063, n1197);
    let n1199: ZB = zn_tile_flag_at(g.cache, g.cart, n1198, n166, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1200: ZB = zn_gt(zn_splat(P8::from_raw(1572864i32)), n1198);
    let n1201: ZB = zb_and(n1065, n1200);
    let n1202: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n1197);
    let n1203: ZN = zn_add(n1063, n1202);
    let n1204: ZB = zn_lt(zn_splat(P8::from_raw(524288i32)), n1203);
    let n1205: ZB = zb_and(n1201, n1204);
    let n1206: ZB = zb_and(n1067, n1205);
    let n1207: ZB = zb_or(n1199, n1206);
    let n1208: ZN = zn_add(n1063, n1196);
    let n1209: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1208);
    let n1210: ZN = zn_add(n1063, n1209);
    let n1211: ZB = zn_tile_flag_at(g.cache, g.cart, n1210, n166, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1212: ZB = zn_gt(zn_splat(P8::from_raw(1572864i32)), n1210);
    let n1213: ZB = zb_and(n1065, n1212);
    let n1214: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n1209);
    let n1215: ZN = zn_add(n1063, n1214);
    let n1216: ZB = zn_lt(zn_splat(P8::from_raw(524288i32)), n1215);
    let n1217: ZB = zb_and(n1213, n1216);
    let n1218: ZB = zb_and(n1067, n1217);
    let n1219: ZB = zb_or(n1211, n1218);
    let n1220: ZN = zn_add(n1063, n1208);
    let n1221: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1220);
    let n1222: ZN = zn_add(n1063, n1221);
    let n1223: ZB = zn_tile_flag_at(g.cache, g.cart, n1222, n166, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1224: ZB = zn_gt(zn_splat(P8::from_raw(1572864i32)), n1222);
    let n1225: ZB = zb_and(n1065, n1224);
    let n1226: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n1221);
    let n1227: ZN = zn_add(n1063, n1226);
    let n1228: ZB = zn_lt(zn_splat(P8::from_raw(524288i32)), n1227);
    let n1229: ZB = zb_and(n1225, n1228);
    let n1230: ZB = zb_and(n1067, n1229);
    let n1231: ZB = zb_or(n1223, n1230);
    let n1232: ZN = zn_add(n1063, n1220);
    let n1233: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1232);
    let n1234: ZN = zn_add(n1063, n1233);
    let n1235: ZB = zn_tile_flag_at(g.cache, g.cart, n1234, n166, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1236: ZB = zn_gt(zn_splat(P8::from_raw(1572864i32)), n1234);
    let n1237: ZB = zb_and(n1065, n1236);
    let n1238: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n1233);
    let n1239: ZN = zn_add(n1063, n1238);
    let n1240: ZB = zn_lt(zn_splat(P8::from_raw(524288i32)), n1239);
    let n1241: ZB = zb_and(n1237, n1240);
    let n1242: ZB = zb_and(n1067, n1241);
    let n1243: ZB = zb_or(n1235, n1242);
    let n1244: ZN = zn_add(n1063, n1232);
    let n1245: ZN = zsel_n(n1243, n1232, n1244);
    let n1246: ZN = zsel_n(n1243, zn_splat(P8::from_raw(0i32)), r_c320);
    let n1247: ZB = zb_or(n1075, n1243);
    let n1248: ZN = zsel_n(n1074, n1232, n1245);
    let n1249: ZN = zsel_n(n1074, r_c320, n1246);
    let n1250: ZB = zb_or(n1074, n1247);
    let n1251: ZN = zsel_n(n1231, n1220, n1248);
    let n1252: ZN = zsel_n(n1231, zn_splat(P8::from_raw(0i32)), n1249);
    let n1253: ZB = zb_or(n1231, n1250);
    let n1254: ZN = zsel_n(n1073, n1220, n1251);
    let n1255: ZN = zsel_n(n1073, r_c320, n1252);
    let n1256: ZB = zb_or(n1073, n1253);
    let n1257: ZN = zsel_n(n1219, n1208, n1254);
    let n1258: ZN = zsel_n(n1219, zn_splat(P8::from_raw(0i32)), n1255);
    let n1259: ZB = zb_or(n1219, n1256);
    let n1260: ZN = zsel_n(n1072, n1208, n1257);
    let n1261: ZN = zsel_n(n1072, r_c320, n1258);
    let n1262: ZB = zb_or(n1072, n1259);
    let n1263: ZN = zsel_n(n1207, n1196, n1260);
    let n1264: ZN = zsel_n(n1207, zn_splat(P8::from_raw(0i32)), n1261);
    let n1265: ZB = zb_or(n1207, n1262);
    let n1266: ZN = zsel_n(n1071, n1196, n1263);
    let n1267: ZN = zsel_n(n1071, r_c320, n1264);
    let n1268: ZB = zb_or(n1071, n1265);
    let n1269: ZN = zsel_n(n1195, n1184, n1266);
    let n1270: ZN = zsel_n(n1195, zn_splat(P8::from_raw(0i32)), n1267);
    let n1271: ZB = zb_or(n1195, n1268);
    let n1272: ZN = zsel_n(n1070, n1184, n1269);
    let n1273: ZN = zsel_n(n1070, r_c320, n1270);
    let n1274: ZB = zb_or(n1070, n1271);
    let n1275: ZN = zsel_n(n1183, n1172, n1272);
    let n1276: ZN = zsel_n(n1183, zn_splat(P8::from_raw(0i32)), n1273);
    let n1277: ZB = zb_or(n1183, n1274);
    let n1278: ZN = zsel_n(n1069, n1172, n1275);
    let n1279: ZN = zsel_n(n1069, r_c320, n1276);
    let n1280: ZB = zb_or(n1069, n1277);
    let n1281: ZN = zsel_n(n1171, n1160, n1278);
    let n1282: ZN = zsel_n(n1171, zn_splat(P8::from_raw(0i32)), n1279);
    let n1283: ZB = zb_or(n1171, n1280);
    let n1284: ZN = zsel_n(n1068, n1160, n1281);
    let n1285: ZN = zsel_n(n1068, r_c320, n1282);
    let n1286: ZB = zb_or(n1068, n1283);
    let n1287: ZN = zsel_n(n1159, r_c272, n1284);
    let n1288: ZN = zsel_n(n1159, zn_splat(P8::from_raw(0i32)), n1285);
    let n1289: ZB = zb_or(n1159, n1286);
    let n1290: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1287);
    let n1291: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1290);
    let n1292: ZB = zn_tile_flag_at(g.cache, g.cart, n1291, n1084, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1293: ZB = zn_gt(zn_splat(P8::from_raw(1572864i32)), n1291);
    let n1294: ZB = zb_and(n1085, n1293);
    let n1295: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n1290);
    let n1296: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1295);
    let n1297: ZB = zn_lt(zn_splat(P8::from_raw(524288i32)), n1296);
    let n1298: ZB = zb_and(n1294, n1297);
    let n1299: ZB = zb_and(n1087, n1298);
    let n1300: ZB = zb_or(n1292, n1299);
    let n1301: ZB = zn_tile_flag_at(g.cache, g.cart, n1291, n1091, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1302: ZB = zb_and(n1092, n1293);
    let n1303: ZB = zb_and(n1297, n1302);
    let n1304: ZB = zb_and(n1095, n1303);
    let n1305: ZB = zb_or(n1301, n1304);
    let n1306: ZB = zn_tile_flag_at(g.cache, g.cart, n1291, n1099, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1307: ZB = zb_and(n1100, n1293);
    let n1308: ZB = zb_and(n1297, n1307);
    let n1309: ZB = zb_and(n1103, n1308);
    let n1310: ZB = zb_or(n1306, n1309);
    let n1311: ZB = zn_tile_flag_at(g.cache, g.cart, n1291, n1107, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1312: ZB = zb_and(n1108, n1293);
    let n1313: ZB = zb_and(n1297, n1312);
    let n1314: ZB = zb_and(n1111, n1313);
    let n1315: ZB = zb_or(n1311, n1314);
    let n1316: ZB = zn_tile_flag_at(g.cache, g.cart, n1291, n1115, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1317: ZB = zb_and(n1116, n1293);
    let n1318: ZB = zb_and(n1297, n1317);
    let n1319: ZB = zb_and(n1119, n1318);
    let n1320: ZB = zb_or(n1316, n1319);
    let n1321: ZB = zn_tile_flag_at(g.cache, g.cart, n1291, n1123, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1322: ZB = zb_and(n1124, n1293);
    let n1323: ZB = zb_and(n1297, n1322);
    let n1324: ZB = zb_and(n1127, n1323);
    let n1325: ZB = zb_or(n1321, n1324);
    let n1326: ZB = zn_tile_flag_at(g.cache, g.cart, n1291, n1131, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1327: ZB = zb_and(n1132, n1293);
    let n1328: ZB = zb_and(n1297, n1327);
    let n1329: ZB = zb_and(n1135, n1328);
    let n1330: ZB = zb_or(n1326, n1329);
    let n1331: ZB = zn_tile_flag_at(g.cache, g.cart, n1291, n1139, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1332: ZB = zb_and(n1140, n1293);
    let n1333: ZB = zb_and(n1297, n1332);
    let n1334: ZB = zb_and(n1143, n1333);
    let n1335: ZB = zb_or(n1331, n1334);
    let n1336: ZB = zb_and(n1145, n1289);
    let n1337: ZN = zsel_n(n1335, n1136, n1144);
    let n1338: ZN = zsel_n(n1335, zn_splat(P8::from_raw(0i32)), r_c321);
    let n1339: ZB = zsel_b(n1335, n1289, n1336);
    let n1340: ZN = zsel_n(n1137, n1136, n1337);
    let n1341: ZN = zsel_n(n1137, r_c321, n1338);
    let n1342: ZB = zsel_b(n1137, n1289, n1339);
    let n1343: ZN = zsel_n(n1330, n1128, n1340);
    let n1344: ZN = zsel_n(n1330, zn_splat(P8::from_raw(0i32)), n1341);
    let n1345: ZB = zsel_b(n1330, n1289, n1342);
    let n1346: ZN = zsel_n(n1129, n1128, n1343);
    let n1347: ZN = zsel_n(n1129, r_c321, n1344);
    let n1348: ZB = zsel_b(n1129, n1289, n1345);
    let n1349: ZN = zsel_n(n1325, n1120, n1346);
    let n1350: ZN = zsel_n(n1325, zn_splat(P8::from_raw(0i32)), n1347);
    let n1351: ZB = zsel_b(n1325, n1289, n1348);
    let n1352: ZN = zsel_n(n1121, n1120, n1349);
    let n1353: ZN = zsel_n(n1121, r_c321, n1350);
    let n1354: ZB = zsel_b(n1121, n1289, n1351);
    let n1355: ZN = zsel_n(n1320, n1112, n1352);
    let n1356: ZN = zsel_n(n1320, zn_splat(P8::from_raw(0i32)), n1353);
    let n1357: ZB = zsel_b(n1320, n1289, n1354);
    let n1358: ZN = zsel_n(n1113, n1112, n1355);
    let n1359: ZN = zsel_n(n1113, r_c321, n1356);
    let n1360: ZB = zsel_b(n1113, n1289, n1357);
    let n1361: ZN = zsel_n(n1315, n1104, n1358);
    let n1362: ZN = zsel_n(n1315, zn_splat(P8::from_raw(0i32)), n1359);
    let n1363: ZB = zsel_b(n1315, n1289, n1360);
    let n1364: ZN = zsel_n(n1105, n1104, n1361);
    let n1365: ZN = zsel_n(n1105, r_c321, n1362);
    let n1366: ZB = zsel_b(n1105, n1289, n1363);
    let n1367: ZN = zsel_n(n1310, n1096, n1364);
    let n1368: ZN = zsel_n(n1310, zn_splat(P8::from_raw(0i32)), n1365);
    let n1369: ZB = zsel_b(n1310, n1289, n1366);
    let n1370: ZN = zsel_n(n1097, n1096, n1367);
    let n1371: ZN = zsel_n(n1097, r_c321, n1368);
    let n1372: ZB = zsel_b(n1097, n1289, n1369);
    let n1373: ZN = zsel_n(n1305, n1088, n1370);
    let n1374: ZN = zsel_n(n1305, zn_splat(P8::from_raw(0i32)), n1371);
    let n1375: ZB = zsel_b(n1305, n1289, n1372);
    let n1376: ZN = zsel_n(n1089, n1088, n1373);
    let n1377: ZN = zsel_n(n1089, r_c321, n1374);
    let n1378: ZB = zsel_b(n1089, n1289, n1375);
    let n1379: ZN = zsel_n(n1300, r_c273, n1376);
    let n1380: ZN = zsel_n(n1300, zn_splat(P8::from_raw(0i32)), n1377);
    let n1381: ZB = zsel_b(n1300, n1289, n1378);
    let n1382: ZN = zsel_n(n1055, n1287, r_c272);
    let n1383: ZN = zsel_n(n1055, n1379, r_c273);
    let n1384: ZN = zsel_n(n1055, n1288, r_c320);
    let n1385: ZN = zsel_n(n1055, n1380, r_c321);
    let n1386: ZB = zb_or(n1056, n1381);
    let n1387: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1382);
    let n1388: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1383);
    let n1389: ZN = zn_div(n1387, zn_splat(P8::from_raw(524288i32)));
    let n1390: ZN = zn_flr(n1389);
    let n1391: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1390);
    let n1392: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n1387);
    let n1393: ZN = zn_sub(n1392, zn_splat(P8::from_raw(65536i32)));
    let n1394: ZN = zn_div(n1393, zn_splat(P8::from_raw(524288i32)));
    let n1395: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n1394);
    let n1396: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1391);
    let n1397: ZB = zn_le(n1396, n1395);
    let n1398: ZB = zn_gt(n1396, n1395);
    let n1399: ZB = zb_and(n1150, n1397);
    let n1400: ZB = zb_and(n1150, n1398);
    let n1401: ZN = zn_div(n1388, zn_splat(P8::from_raw(524288i32)));
    let n1402: ZN = zn_flr(n1401);
    let n1403: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1402);
    let n1404: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n1388);
    let n1405: ZN = zn_sub(n1404, zn_splat(P8::from_raw(65536i32)));
    let n1406: ZN = zn_div(n1405, zn_splat(P8::from_raw(524288i32)));
    let n1407: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n1406);
    let n1408: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1403);
    let n1409: ZB = zn_le(n1408, n1407);
    let n1410: ZB = zn_gt(n1408, n1407);
    let n1411: ZB = zb_and(n1399, n1409);
    let n1412: ZB = zb_and(n1399, n1410);
    let n1413: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1396);
    let n1414: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1408);
    let n1415: ZN = zn_mget(g.cart, n1413, n1414);
    let n1416: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1415);
    let n1417: ZN = zn_rem(n1405, zn_splat(P8::from_raw(524288i32)));
    let n1418: ZB = zn_ge(n1417, zn_splat(P8::from_raw(393216i32)));
    let n1419: ZN = zn_mul(n1408, zn_splat(P8::from_raw(524288i32)));
    let n1420: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1419);
    let n1421: ZB = zn_eq(n1404, n1420);
    let n1422: ZB = zb_or(n1418, n1421);
    let n1423: ZB = zb_and(n1416, n1422);
    let n1424: ZB = zn_ge(n1385, zn_splat(P8::from_raw(0i32)));
    let n1425: ZB = zb_and(n1423, n1424);
    let n1426: ZB = zb_not(n1425);
    let n1427: ZB = zb_and(n1411, n1425);
    let n1428: ZB = zb_and(n1411, n1426);
    let n1429: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1415);
    let n1430: ZN = zn_rem(n1388, zn_splat(P8::from_raw(524288i32)));
    let n1431: ZB = zn_le(n1430, zn_splat(P8::from_raw(131072i32)));
    let n1432: ZB = zb_and(n1429, n1431);
    let n1433: ZB = zn_le(n1385, zn_splat(P8::from_raw(0i32)));
    let n1434: ZB = zb_and(n1432, n1433);
    let n1435: ZB = zb_not(n1434);
    let n1436: ZB = zb_and(n1428, n1434);
    let n1437: ZB = zb_and(n1428, n1435);
    let n1438: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1415);
    let n1439: ZN = zn_rem(n1387, zn_splat(P8::from_raw(524288i32)));
    let n1440: ZB = zn_le(n1439, zn_splat(P8::from_raw(131072i32)));
    let n1441: ZB = zb_and(n1438, n1440);
    let n1442: ZB = zn_le(n1384, zn_splat(P8::from_raw(0i32)));
    let n1443: ZB = zb_and(n1441, n1442);
    let n1444: ZB = zb_not(n1443);
    let n1445: ZB = zb_and(n1437, n1443);
    let n1446: ZB = zb_and(n1437, n1444);
    let n1447: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1415);
    let n1448: ZN = zn_rem(n1393, zn_splat(P8::from_raw(524288i32)));
    let n1449: ZB = zn_ge(n1448, zn_splat(P8::from_raw(393216i32)));
    let n1450: ZN = zn_mul(n1396, zn_splat(P8::from_raw(524288i32)));
    let n1451: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1450);
    let n1452: ZB = zn_eq(n1392, n1451);
    let n1453: ZB = zb_or(n1449, n1452);
    let n1454: ZB = zb_and(n1447, n1453);
    let n1455: ZB = zn_ge(n1384, zn_splat(P8::from_raw(0i32)));
    let n1456: ZB = zb_and(n1454, n1455);
    let n1457: ZB = zb_not(n1456);
    let n1458: ZB = zb_and(n1446, n1456);
    let n1459: ZB = zb_and(n1446, n1457);
    let n1460: ZB = zb_or(n1445, n1458);
    let n1461: ZB = zb_or(n1436, n1460);
    let n1462: ZB = zb_or(n1427, n1461);
    let n1463: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1403);
    let n1464: ZB = zn_le(n1463, n1407);
    let n1465: ZB = zn_gt(n1463, n1407);
    let n1466: ZB = zb_and(n1459, n1464);
    let n1467: ZB = zb_and(n1459, n1465);
    let n1468: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1463);
    let n1469: ZN = zn_mget(g.cart, n1413, n1468);
    let n1470: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1469);
    let n1471: ZN = zn_mul(n1463, zn_splat(P8::from_raw(524288i32)));
    let n1472: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1471);
    let n1473: ZB = zn_eq(n1404, n1472);
    let n1474: ZB = zb_or(n1418, n1473);
    let n1475: ZB = zb_and(n1470, n1474);
    let n1476: ZB = zb_and(n1424, n1475);
    let n1477: ZB = zb_not(n1476);
    let n1478: ZB = zb_and(n1466, n1476);
    let n1479: ZB = zb_and(n1466, n1477);
    let n1480: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1469);
    let n1481: ZB = zb_and(n1431, n1480);
    let n1482: ZB = zb_and(n1433, n1481);
    let n1483: ZB = zb_not(n1482);
    let n1484: ZB = zb_and(n1479, n1482);
    let n1485: ZB = zb_and(n1479, n1483);
    let n1486: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1469);
    let n1487: ZB = zb_and(n1440, n1486);
    let n1488: ZB = zb_and(n1442, n1487);
    let n1489: ZB = zb_not(n1488);
    let n1490: ZB = zb_and(n1485, n1488);
    let n1491: ZB = zb_and(n1485, n1489);
    let n1492: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1469);
    let n1493: ZB = zb_and(n1453, n1492);
    let n1494: ZB = zb_and(n1455, n1493);
    let n1495: ZB = zb_not(n1494);
    let n1496: ZB = zb_and(n1491, n1494);
    let n1497: ZB = zb_and(n1491, n1495);
    let n1498: ZB = zb_or(n1490, n1496);
    let n1499: ZB = zb_or(n1484, n1498);
    let n1500: ZB = zb_or(n1478, n1499);
    let n1501: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n1403);
    let n1502: ZB = zn_le(n1501, n1407);
    let n1503: ZB = zn_gt(n1501, n1407);
    let n1504: ZB = zb_and(n1497, n1502);
    let n1505: ZB = zb_and(n1497, n1503);
    let n1506: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1501);
    let n1507: ZN = zn_mget(g.cart, n1413, n1506);
    let n1508: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1507);
    let n1509: ZN = zn_mul(n1501, zn_splat(P8::from_raw(524288i32)));
    let n1510: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1509);
    let n1511: ZB = zn_eq(n1404, n1510);
    let n1512: ZB = zb_or(n1418, n1511);
    let n1513: ZB = zb_and(n1508, n1512);
    let n1514: ZB = zb_and(n1424, n1513);
    let n1515: ZB = zb_not(n1514);
    let n1516: ZB = zb_and(n1504, n1514);
    let n1517: ZB = zb_and(n1504, n1515);
    let n1518: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1507);
    let n1519: ZB = zb_and(n1431, n1518);
    let n1520: ZB = zb_and(n1433, n1519);
    let n1521: ZB = zb_not(n1520);
    let n1522: ZB = zb_and(n1517, n1520);
    let n1523: ZB = zb_and(n1517, n1521);
    let n1524: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1507);
    let n1525: ZB = zb_and(n1440, n1524);
    let n1526: ZB = zb_and(n1442, n1525);
    let n1527: ZB = zb_not(n1526);
    let n1528: ZB = zb_and(n1523, n1526);
    let n1529: ZB = zb_and(n1523, n1527);
    let n1530: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1507);
    let n1531: ZB = zb_and(n1453, n1530);
    let n1532: ZB = zb_and(n1455, n1531);
    let n1533: ZB = zb_not(n1532);
    let n1534: ZB = zb_and(n1529, n1532);
    let n1535: ZB = zb_and(n1529, n1533);
    let n1536: ZB = zb_or(n1528, n1534);
    let n1537: ZB = zb_or(n1522, n1536);
    let n1538: ZB = zb_or(n1516, n1537);
    let n1539: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1403);
    let n1540: ZB = zn_gt(n1539, n1407);
    let n1541: ZB = zb_and(n1386, n1540);
    let n1542: ZB = zb_or(n1505, n1535);
    let n1543: ZB = zsel_b(n1503, n1386, n1541);
    let n1544: ZB = zb_or(n1500, n1538);
    let n1545: ZB = zb_or(n1467, n1542);
    let n1546: ZB = zsel_b(n1465, n1386, n1543);
    let n1547: ZB = zb_or(n1462, n1544);
    let n1548: ZB = zb_or(n1412, n1545);
    let n1549: ZB = zsel_b(n1410, n1386, n1546);
    let n1550: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1391);
    let n1551: ZB = zn_le(n1550, n1395);
    let n1552: ZB = zn_gt(n1550, n1395);
    let n1553: ZB = zb_and(n1548, n1551);
    let n1554: ZB = zb_and(n1548, n1552);
    let n1555: ZB = zb_and(n1410, n1553);
    let n1556: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1550);
    let n1557: ZN = zn_mget(g.cart, n1556, n1414);
    let n1558: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1557);
    let n1559: ZB = zb_and(n1409, n1548);
    let n1560: ZB = zb_and(n1551, n1559);
    let n1561: ZB = zb_and(n1422, n1558);
    let n1562: ZB = zb_and(n1424, n1561);
    let n1563: ZB = zb_not(n1562);
    let n1564: ZB = zb_and(n1560, n1562);
    let n1565: ZB = zb_and(n1560, n1563);
    let n1566: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1557);
    let n1567: ZB = zb_and(n1431, n1566);
    let n1568: ZB = zb_and(n1433, n1567);
    let n1569: ZB = zb_not(n1568);
    let n1570: ZB = zb_and(n1565, n1568);
    let n1571: ZB = zb_and(n1565, n1569);
    let n1572: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1557);
    let n1573: ZB = zb_and(n1440, n1572);
    let n1574: ZB = zb_and(n1442, n1573);
    let n1575: ZB = zb_not(n1574);
    let n1576: ZB = zb_and(n1571, n1574);
    let n1577: ZB = zb_and(n1571, n1575);
    let n1578: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1557);
    let n1579: ZN = zn_mul(n1550, zn_splat(P8::from_raw(524288i32)));
    let n1580: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1579);
    let n1581: ZB = zn_eq(n1392, n1580);
    let n1582: ZB = zb_or(n1449, n1581);
    let n1583: ZB = zb_and(n1578, n1582);
    let n1584: ZB = zb_and(n1455, n1583);
    let n1585: ZB = zb_not(n1584);
    let n1586: ZB = zb_and(n1577, n1584);
    let n1587: ZB = zb_and(n1577, n1585);
    let n1588: ZB = zb_or(n1576, n1586);
    let n1589: ZB = zb_or(n1570, n1588);
    let n1590: ZB = zb_or(n1564, n1589);
    let n1591: ZB = zb_and(n1465, n1587);
    let n1592: ZN = zn_mget(g.cart, n1556, n1468);
    let n1593: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1592);
    let n1594: ZB = zb_and(n1464, n1577);
    let n1595: ZB = zb_and(n1585, n1594);
    let n1596: ZB = zb_and(n1474, n1593);
    let n1597: ZB = zb_and(n1424, n1596);
    let n1598: ZB = zb_not(n1597);
    let n1599: ZB = zb_and(n1595, n1597);
    let n1600: ZB = zb_and(n1595, n1598);
    let n1601: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1592);
    let n1602: ZB = zb_and(n1431, n1601);
    let n1603: ZB = zb_and(n1433, n1602);
    let n1604: ZB = zb_not(n1603);
    let n1605: ZB = zb_and(n1600, n1603);
    let n1606: ZB = zb_and(n1600, n1604);
    let n1607: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1592);
    let n1608: ZB = zb_and(n1440, n1607);
    let n1609: ZB = zb_and(n1442, n1608);
    let n1610: ZB = zb_not(n1609);
    let n1611: ZB = zb_and(n1606, n1609);
    let n1612: ZB = zb_and(n1606, n1610);
    let n1613: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1592);
    let n1614: ZB = zb_and(n1582, n1613);
    let n1615: ZB = zb_and(n1455, n1614);
    let n1616: ZB = zb_not(n1615);
    let n1617: ZB = zb_and(n1612, n1615);
    let n1618: ZB = zb_and(n1612, n1616);
    let n1619: ZB = zb_or(n1611, n1617);
    let n1620: ZB = zb_or(n1605, n1619);
    let n1621: ZB = zb_or(n1599, n1620);
    let n1622: ZB = zb_and(n1503, n1618);
    let n1623: ZN = zn_mget(g.cart, n1556, n1506);
    let n1624: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1623);
    let n1625: ZB = zb_and(n1502, n1612);
    let n1626: ZB = zb_and(n1616, n1625);
    let n1627: ZB = zb_and(n1512, n1624);
    let n1628: ZB = zb_and(n1424, n1627);
    let n1629: ZB = zb_not(n1628);
    let n1630: ZB = zb_and(n1626, n1628);
    let n1631: ZB = zb_and(n1626, n1629);
    let n1632: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1623);
    let n1633: ZB = zb_and(n1431, n1632);
    let n1634: ZB = zb_and(n1433, n1633);
    let n1635: ZB = zb_not(n1634);
    let n1636: ZB = zb_and(n1631, n1634);
    let n1637: ZB = zb_and(n1631, n1635);
    let n1638: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1623);
    let n1639: ZB = zb_and(n1440, n1638);
    let n1640: ZB = zb_and(n1442, n1639);
    let n1641: ZB = zb_not(n1640);
    let n1642: ZB = zb_and(n1637, n1640);
    let n1643: ZB = zb_and(n1637, n1641);
    let n1644: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1623);
    let n1645: ZB = zb_and(n1582, n1644);
    let n1646: ZB = zb_and(n1455, n1645);
    let n1647: ZB = zb_not(n1646);
    let n1648: ZB = zb_and(n1643, n1646);
    let n1649: ZB = zb_and(n1643, n1647);
    let n1650: ZB = zb_or(n1642, n1648);
    let n1651: ZB = zb_or(n1636, n1650);
    let n1652: ZB = zb_or(n1630, n1651);
    let n1653: ZB = zb_and(n1540, n1549);
    let n1654: ZB = zb_or(n1622, n1649);
    let n1655: ZB = zsel_b(n1503, n1549, n1653);
    let n1656: ZB = zb_or(n1621, n1652);
    let n1657: ZB = zb_or(n1591, n1654);
    let n1658: ZB = zsel_b(n1465, n1549, n1655);
    let n1659: ZB = zb_or(n1590, n1656);
    let n1660: ZB = zb_or(n1555, n1657);
    let n1661: ZB = zsel_b(n1410, n1549, n1658);
    let n1662: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n1391);
    let n1663: ZB = zn_le(n1662, n1395);
    let n1664: ZB = zn_gt(n1662, n1395);
    let n1665: ZB = zb_and(n1660, n1663);
    let n1666: ZB = zb_and(n1660, n1664);
    let n1667: ZB = zb_and(n1410, n1665);
    let n1668: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1662);
    let n1669: ZN = zn_mget(g.cart, n1668, n1414);
    let n1670: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1669);
    let n1671: ZB = zb_and(n1409, n1660);
    let n1672: ZB = zb_and(n1663, n1671);
    let n1673: ZB = zb_and(n1422, n1670);
    let n1674: ZB = zb_and(n1424, n1673);
    let n1675: ZB = zb_not(n1674);
    let n1676: ZB = zb_and(n1672, n1674);
    let n1677: ZB = zb_and(n1672, n1675);
    let n1678: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1669);
    let n1679: ZB = zb_and(n1431, n1678);
    let n1680: ZB = zb_and(n1433, n1679);
    let n1681: ZB = zb_not(n1680);
    let n1682: ZB = zb_and(n1677, n1680);
    let n1683: ZB = zb_and(n1677, n1681);
    let n1684: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1669);
    let n1685: ZB = zb_and(n1440, n1684);
    let n1686: ZB = zb_and(n1442, n1685);
    let n1687: ZB = zb_not(n1686);
    let n1688: ZB = zb_and(n1683, n1686);
    let n1689: ZB = zb_and(n1683, n1687);
    let n1690: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1669);
    let n1691: ZN = zn_mul(n1662, zn_splat(P8::from_raw(524288i32)));
    let n1692: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1691);
    let n1693: ZB = zn_eq(n1392, n1692);
    let n1694: ZB = zb_or(n1449, n1693);
    let n1695: ZB = zb_and(n1690, n1694);
    let n1696: ZB = zb_and(n1455, n1695);
    let n1697: ZB = zb_not(n1696);
    let n1698: ZB = zb_and(n1689, n1696);
    let n1699: ZB = zb_and(n1689, n1697);
    let n1700: ZB = zb_or(n1688, n1698);
    let n1701: ZB = zb_or(n1682, n1700);
    let n1702: ZB = zb_or(n1676, n1701);
    let n1703: ZB = zb_and(n1465, n1699);
    let n1704: ZN = zn_mget(g.cart, n1668, n1468);
    let n1705: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1704);
    let n1706: ZB = zb_and(n1464, n1689);
    let n1707: ZB = zb_and(n1697, n1706);
    let n1708: ZB = zb_and(n1474, n1705);
    let n1709: ZB = zb_and(n1424, n1708);
    let n1710: ZB = zb_not(n1709);
    let n1711: ZB = zb_and(n1707, n1709);
    let n1712: ZB = zb_and(n1707, n1710);
    let n1713: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1704);
    let n1714: ZB = zb_and(n1431, n1713);
    let n1715: ZB = zb_and(n1433, n1714);
    let n1716: ZB = zb_not(n1715);
    let n1717: ZB = zb_and(n1712, n1715);
    let n1718: ZB = zb_and(n1712, n1716);
    let n1719: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1704);
    let n1720: ZB = zb_and(n1440, n1719);
    let n1721: ZB = zb_and(n1442, n1720);
    let n1722: ZB = zb_not(n1721);
    let n1723: ZB = zb_and(n1718, n1721);
    let n1724: ZB = zb_and(n1718, n1722);
    let n1725: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1704);
    let n1726: ZB = zb_and(n1694, n1725);
    let n1727: ZB = zb_and(n1455, n1726);
    let n1728: ZB = zb_not(n1727);
    let n1729: ZB = zb_and(n1724, n1727);
    let n1730: ZB = zb_and(n1724, n1728);
    let n1731: ZB = zb_or(n1723, n1729);
    let n1732: ZB = zb_or(n1717, n1731);
    let n1733: ZB = zb_or(n1711, n1732);
    let n1734: ZB = zb_and(n1503, n1730);
    let n1735: ZN = zn_mget(g.cart, n1668, n1506);
    let n1736: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1735);
    let n1737: ZB = zb_and(n1502, n1724);
    let n1738: ZB = zb_and(n1728, n1737);
    let n1739: ZB = zb_and(n1512, n1736);
    let n1740: ZB = zb_and(n1424, n1739);
    let n1741: ZB = zb_not(n1740);
    let n1742: ZB = zb_and(n1738, n1740);
    let n1743: ZB = zb_and(n1738, n1741);
    let n1744: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1735);
    let n1745: ZB = zb_and(n1431, n1744);
    let n1746: ZB = zb_and(n1433, n1745);
    let n1747: ZB = zb_not(n1746);
    let n1748: ZB = zb_and(n1743, n1746);
    let n1749: ZB = zb_and(n1743, n1747);
    let n1750: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1735);
    let n1751: ZB = zb_and(n1440, n1750);
    let n1752: ZB = zb_and(n1442, n1751);
    let n1753: ZB = zb_not(n1752);
    let n1754: ZB = zb_and(n1749, n1752);
    let n1755: ZB = zb_and(n1749, n1753);
    let n1756: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1735);
    let n1757: ZB = zb_and(n1694, n1756);
    let n1758: ZB = zb_and(n1455, n1757);
    let n1759: ZB = zb_not(n1758);
    let n1760: ZB = zb_and(n1755, n1758);
    let n1761: ZB = zb_and(n1755, n1759);
    let n1762: ZB = zb_or(n1754, n1760);
    let n1763: ZB = zb_or(n1748, n1762);
    let n1764: ZB = zb_or(n1742, n1763);
    let n1765: ZB = zb_and(n1540, n1661);
    let n1766: ZB = zb_or(n1734, n1761);
    let n1767: ZB = zsel_b(n1503, n1661, n1765);
    let n1768: ZB = zb_or(n1733, n1764);
    let n1769: ZB = zb_or(n1703, n1766);
    let n1770: ZB = zsel_b(n1465, n1661, n1767);
    let n1771: ZB = zb_or(n1702, n1768);
    let n1772: ZB = zb_or(n1667, n1769);
    let n1773: ZB = zsel_b(n1410, n1661, n1770);
    let n1774: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1391);
    let n1775: ZB = zn_gt(n1774, n1395);
    let n1776: ZB = zb_and(n1773, n1775);
    let n1777: ZB = zb_or(n1659, n1771);
    let n1778: ZB = zsel_b(n1659, n1549, n1661);
    let n1779: ZB = zb_or(n1666, n1772);
    let n1780: ZB = zsel_b(n1664, n1661, n1776);
    let n1781: ZB = zb_or(n1547, n1777);
    let n1782: ZB = zsel_b(n1547, n1386, n1778);
    let n1783: ZB = zb_or(n1554, n1779);
    let n1784: ZB = zsel_b(n1552, n1549, n1780);
    let n1785: ZB = zb_or(n1400, n1783);
    let n1786: ZB = zsel_b(n1398, n1386, n1784);
    let n1787: ZB = zn_gt(n1383, zn_splat(P8::from_raw(8388608i32)));
    let n1788: ZB = zn_le(n1383, zn_splat(P8::from_raw(8388608i32)));
    let n1789: ZB = zb_and(n1785, n1787);
    let n1790: ZB = zb_or(n1781, n1789);
    let n1791: ZB = zsel_b(n1781, n1782, n1786);
    let n1792: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1387);
    let n1793: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1388);
    let n1794: ZB = zn_tile_flag_at(g.cache, g.cart, n1792, n1793, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1795: ZB = zn_gt(zn_splat(P8::from_raw(1572864i32)), n1792);
    let n1796: ZB = zn_gt(zn_splat(P8::from_raw(3145728i32)), n1793);
    let n1797: ZB = zb_and(n1795, n1796);
    let n1798: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1392);
    let n1799: ZB = zn_lt(zn_splat(P8::from_raw(524288i32)), n1798);
    let n1800: ZB = zb_and(n1797, n1799);
    let n1801: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1404);
    let n1802: ZB = zn_lt(zn_splat(P8::from_raw(2097152i32)), n1801);
    let n1803: ZB = zb_and(n1800, n1802);
    let n1804: ZB = zb_or(n1794, n1803);
    let n1805: ZB = zb_not(n1804);
    let n1806: ZN = zsel_n(n1804, n219, r_c256);
    let n1807: ZN = zsel_n(n1804, zn_splat(P8::from_raw(393216i32)), n222);
    let n1808: ZB = zn_gt(n1384, r_c310);
    let n1809: ZB = zn_gt(n1385, r_c311);
    let n1810: ZN = zsel_n(n1805, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n1811: ZN = zn_abs(n1384);
    let n1812: ZB = zn_gt(n1811, zn_splat(P8::from_raw(65536i32)));
    let n1813: ZB = zn_gt(n1384, zn_splat(P8::from_raw(0i32)));
    let n1814: ZB = zn_lt(n1384, zn_splat(P8::from_raw(0i32)));
    let n1815: ZB = zn_gt(n1384, zn_splat(P8::from_raw(65536i32)));
    let n1816: ZN = zn_sub(n1384, zn_splat(P8::from_raw(9830i32)));
    let n1817: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n1816);
    let n1818: ZN = zn_add(zn_splat(P8::from_raw(9830i32)), n1384);
    let n1819: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n1818);
    let n1820: ZB = zn_gt(n1384, zn_splat(P8::from_raw(-65536i32)));
    let n1821: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1816);
    let n1822: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n1818);
    let n1823: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1816);
    let n1824: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n1818);
    let n1825: ZN = zsel_n(n1820, n1821, n1822);
    let n1826: ZN = zsel_n(n1813, n1823, n1824);
    let n1827: ZN = zsel_n(n1815, n1817, n1819);
    let n1828: ZN = zsel_n(n1814, n1825, n1826);
    let n1829: ZN = zsel_n(n1813, n1827, n1828);
    let n1830: ZN = zn_sub(n1384, n1810);
    let n1831: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1830);
    let n1832: ZN = zn_add(n1384, n1810);
    let n1833: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n1832);
    let n1834: ZN = zsel_n(n1813, n1831, n1833);
    let n1835: ZN = zsel_n(n1812, n1829, n1834);
    let n1836: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1835);
    let n1837: ZB = zb_not(n1836);
    let n1838: ZB = zn_lt(n1835, zn_splat(P8::from_raw(0i32)));
    let n1839: ZB = zsel_b(n1837, n1838, r_c312);
    let n1840: ZN = zn_abs(n1385);
    let n1841: ZB = zn_le(n1840, zn_splat(P8::from_raw(9830i32)));
    let n1842: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1388);
    let n1843: ZB = zn_gt(zn_splat(P8::from_raw(3145728i32)), n1842);
    let n1844: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1404);
    let n1845: ZB = zn_lt(zn_splat(P8::from_raw(2097152i32)), n1844);
    let n1846: ZB = zn_gt(n1385, zn_splat(P8::from_raw(131072i32)));
    let n1847: ZB = zn_gt(n1807, zn_splat(P8::from_raw(0i32)));
    let n1848: ZN = zn_add(zn_splat(P8::from_raw(-196608i32)), n1387);
    let n1849: ZB = zn_tile_flag_at(g.cache, g.cart, n1848, n1842, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1850: ZB = zn_gt(zn_splat(P8::from_raw(1572864i32)), n1848);
    let n1851: ZB = zb_and(n1843, n1850);
    let n1852: ZN = zn_add(zn_splat(P8::from_raw(-196608i32)), n1392);
    let n1853: ZB = zn_lt(zn_splat(P8::from_raw(524288i32)), n1852);
    let n1854: ZB = zb_and(n1851, n1853);
    let n1855: ZB = zb_and(n1845, n1854);
    let n1856: ZB = zb_or(n1849, n1855);
    let n1857: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1387);
    let n1858: ZB = zn_tile_flag_at(g.cache, g.cart, n1857, n1842, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1859: ZB = zn_gt(zn_splat(P8::from_raw(1572864i32)), n1857);
    let n1860: ZB = zb_and(n1843, n1859);
    let n1861: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1392);
    let n1862: ZB = zn_lt(zn_splat(P8::from_raw(524288i32)), n1861);
    let n1863: ZB = zb_and(n1860, n1862);
    let n1864: ZB = zb_and(n1845, n1863);
    let n1865: ZB = zb_or(n1858, n1864);
    let n1866: ZN = zsel_n(n1865, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n1867: ZN = zsel_n(n1856, zn_splat(P8::from_raw(-65536i32)), n1866);
    let n1868: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1867);
    let n1869: ZB = zb_not(n1868);
    let n1870: ZB = zn_gt(n1806, zn_splat(P8::from_raw(0i32)));
    let n1871: ZN = zsel_n(n1839, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n1872: ZB = zn_gt(n1871, zn_splat(P8::from_raw(0i32)));
    let n1873: ZB = zn_lt(n1871, zn_splat(P8::from_raw(0i32)));
    let n1874: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1871);
    let n1875: ZB = zb_not(n1874);
    let n1876: ZB = zn_lt(n1383, zn_splat(P8::from_raw(-262144i32)));
    let n1877: ZB = zn_ge(n1383, zn_splat(P8::from_raw(-262144i32)));
    let n1878: ZB = zb_and(n1790, n1876);
    let n1880: ZN = zsel_n(n1787, n1042, n1041);
    let n1881: ZN = zsel_n(n1781, n1880, n1041);
    let n1883: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1830);
    let n1884: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n1832);
    let n1885: ZN = zsel_n(n1820, n1883, n1884);
    let n1886: ZN = zsel_n(n1812, n1829, n1885);
    let n1887: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1886);
    let n1888: ZB = zb_not(n1887);
    let n1889: ZB = zn_lt(n1886, zn_splat(P8::from_raw(0i32)));
    let n1890: ZB = zsel_b(n1888, n1889, r_c312);
    let n1891: ZN = zn_add(zn_splat(P8::from_raw(-65536i32)), n1387);
    let n1892: ZB = zn_tile_flag_at(g.cache, g.cart, n1891, n1842, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1893: ZB = zn_gt(zn_splat(P8::from_raw(1572864i32)), n1891);
    let n1894: ZB = zb_and(n1843, n1893);
    let n1895: ZN = zn_add(zn_splat(P8::from_raw(-65536i32)), n1392);
    let n1896: ZB = zn_lt(zn_splat(P8::from_raw(524288i32)), n1895);
    let n1897: ZB = zb_and(n1894, n1896);
    let n1898: ZB = zb_and(n1845, n1897);
    let n1899: ZB = zb_or(n1892, n1898);
    let n1900: ZN = zsel_n(n1899, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n1901: ZB = zn_gt(n1385, n1900);
    let n1902: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n1830);
    let n1903: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n1832);
    let n1904: ZN = zsel_n(n1815, n1902, n1903);
    let n1905: ZN = zsel_n(n1812, n1829, n1904);
    let n1906: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1905);
    let n1907: ZB = zb_not(n1906);
    let n1908: ZB = zn_lt(n1905, zn_splat(P8::from_raw(0i32)));
    let n1909: ZB = zsel_b(n1907, n1908, r_c312);
    let n1910: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1387);
    let n1911: ZB = zn_tile_flag_at(g.cache, g.cart, n1910, n1842, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1912: ZB = zn_gt(zn_splat(P8::from_raw(1572864i32)), n1910);
    let n1913: ZB = zb_and(n1843, n1912);
    let n1914: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1392);
    let n1915: ZB = zn_lt(zn_splat(P8::from_raw(524288i32)), n1914);
    let n1916: ZB = zb_and(n1913, n1915);
    let n1917: ZB = zb_and(n1845, n1916);
    let n1918: ZB = zb_or(n1911, n1917);
    let n1919: ZN = zsel_n(n1918, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n1920: ZB = zn_gt(n1385, n1919);
    let n1921: ZB = zb_and(n217, n1870);
    let n1922: ZN = zsel_n(n1921, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n1923: ZB = zb_or(r_c41, n1921);
    let n1924: ZN = zsel_n(n1146, r_c20, n1922);
    let n1925: ZB = zsel_b(n1146, r_c41, n1923);
    let n1928: ZB = zb_not(n796);
    let n1929: ZB = zb_or(n796, n1039);
    let n1930: ZB = zsel_b(n796, n724, n1038);
    let n1931: ZB = zb_and(n1785, n1788);
    let n1932: ZB = zb_and(n1876, n1931);
    let n1933: ZB = zb_and(n1877, n1931);
    let n1934: ZB = zb_not(n1932);
    let n1935: ZB = zb_or(n1878, n1932);
    let n1936: ZB = zsel_b(n1932, n1786, n1791);
    let n1937: ZB = zsel_b(n1935, n1934, n1928);
    let n1938: ZN = zsel_n(n1935, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(196608i32)));
    let n1939: ZN = zsel_n(n1935, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n1940: ZN = zsel_n(n1935, zn_splat(P8::from_raw(8388608i32)), zn_splat(P8::from_raw(8126464i32)));
    let n1941: ZB = zb_or(n1929, n1935);
    let n1942: ZB = zsel_b(n1935, n1936, n1930);
    let n1943: ZN = zsel_n(n796, r_c87, n1044);
    let n1944: ZN = zsel_n(n796, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1945: ZN = zsel_n(n1932, r_c87, n1881);
    let n1946: ZN = zsel_n(n1932, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1947: ZN = zsel_n(n1935, n1945, n1943);
    let n1948: ZN = zsel_n(n1935, n1946, n1944);
    let n1950: ZN = zsel_n(n1935, n1924, n890);
    let n1951: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n1952: ZN = zn_sub(n1058, zn_splat(P8::from_raw(32768i32)));
    let n1953: ZN = zn_sub(n1952, n1059);
    let n1954: ZN = zn_sub(n1077, zn_splat(P8::from_raw(32768i32)));
    let n1955: ZN = zn_sub(n1954, n1078);
    let n1956: ZN = zn_sub(r_c255, zn_splat(P8::from_raw(65536i32)));
    let n1957: ZB = zb_and(r_c265, n806);
    let n1958: ZB = zb_and(r_c266, n806);
    let n1959: ZN = zsel_n(n1243, zn_splat(P8::from_raw(0i32)), n1953);
    let n1960: ZN = zsel_n(n1074, n1953, n1959);
    let n1961: ZN = zsel_n(n1231, zn_splat(P8::from_raw(0i32)), n1960);
    let n1962: ZN = zsel_n(n1073, n1953, n1961);
    let n1963: ZN = zsel_n(n1219, zn_splat(P8::from_raw(0i32)), n1962);
    let n1964: ZN = zsel_n(n1072, n1953, n1963);
    let n1965: ZN = zsel_n(n1207, zn_splat(P8::from_raw(0i32)), n1964);
    let n1966: ZN = zsel_n(n1071, n1953, n1965);
    let n1967: ZN = zsel_n(n1195, zn_splat(P8::from_raw(0i32)), n1966);
    let n1968: ZN = zsel_n(n1070, n1953, n1967);
    let n1969: ZN = zsel_n(n1183, zn_splat(P8::from_raw(0i32)), n1968);
    let n1970: ZN = zsel_n(n1069, n1953, n1969);
    let n1971: ZN = zsel_n(n1171, zn_splat(P8::from_raw(0i32)), n1970);
    let n1972: ZN = zsel_n(n1068, n1953, n1971);
    let n1973: ZN = zsel_n(n1159, zn_splat(P8::from_raw(0i32)), n1972);
    let n1974: ZN = zsel_n(n1335, zn_splat(P8::from_raw(0i32)), n1955);
    let n1975: ZN = zsel_n(n1137, n1955, n1974);
    let n1976: ZN = zsel_n(n1330, zn_splat(P8::from_raw(0i32)), n1975);
    let n1977: ZN = zsel_n(n1129, n1955, n1976);
    let n1978: ZN = zsel_n(n1325, zn_splat(P8::from_raw(0i32)), n1977);
    let n1979: ZN = zsel_n(n1121, n1955, n1978);
    let n1980: ZN = zsel_n(n1320, zn_splat(P8::from_raw(0i32)), n1979);
    let n1981: ZN = zsel_n(n1113, n1955, n1980);
    let n1982: ZN = zsel_n(n1315, zn_splat(P8::from_raw(0i32)), n1981);
    let n1983: ZN = zsel_n(n1105, n1955, n1982);
    let n1984: ZN = zsel_n(n1310, zn_splat(P8::from_raw(0i32)), n1983);
    let n1985: ZN = zsel_n(n1097, n1955, n1984);
    let n1986: ZN = zsel_n(n1305, zn_splat(P8::from_raw(0i32)), n1985);
    let n1987: ZN = zsel_n(n1089, n1955, n1986);
    let n1988: ZN = zsel_n(n1300, zn_splat(P8::from_raw(0i32)), n1987);
    let n1989: ZN = zsel_n(n1055, n1973, r_c318);
    let n1990: ZN = zsel_n(n1055, n1988, r_c319);
    let n1991: ZN = zn_sub(n1384, r_c308);
    let n1992: ZN = zn_max(r_c310, n1991);
    let n1993: ZN = zn_add(r_c308, n1384);
    let n1994: ZN = zn_min(r_c310, n1993);
    let n1995: ZN = zsel_n(n1808, n1992, n1994);
    let n1996: ZN = zn_sub(n1385, r_c309);
    let n1997: ZN = zn_max(r_c311, n1996);
    let n1998: ZN = zn_add(r_c309, n1385);
    let n1999: ZN = zn_min(r_c311, n1998);
    let n2000: ZN = zsel_n(n1809, n1997, n1999);
    let n2001: ZN = zsel_n(n1841, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(13762i32)));
    let n2002: ZN = zn_sub(n1385, n2001);
    let n2003: ZN = zn_max(zn_splat(P8::from_raw(131072i32)), n2002);
    let n2004: ZN = zn_add(n1385, n2001);
    let n2005: ZN = zn_min(zn_splat(P8::from_raw(131072i32)), n2004);
    let n2006: ZN = zsel_n(n1846, n2003, n2005);
    let n2007: ZN = zsel_n(n1805, n2006, n1385);
    let n2008: ZN = zn_neg(n1867);
    let n2009: ZN = zn_mul(n2008, zn_splat(P8::from_raw(131072i32)));
    let n2010: ZN = zsel_n(n1869, n2009, n1835);
    let n2011: ZN = zsel_n(n1869, zn_splat(P8::from_raw(-131072i32)), n2007);
    let n2012: ZN = zsel_n(n1847, zn_splat(P8::from_raw(0i32)), n1807);
    let n2013: ZN = zsel_n(n1847, n1835, n2010);
    let n2014: ZN = zsel_n(n1847, zn_splat(P8::from_raw(-131072i32)), n2011);
    let n2015: ZN = zn_sub(n1806, zn_splat(P8::from_raw(65536i32)));
    let n2016: ZN = zsel_n(n1873, zn_splat(P8::from_raw(-131072i32)), zn_splat(P8::from_raw(0i32)));
    let n2017: ZN = zsel_n(n1872, zn_splat(P8::from_raw(131072i32)), n2016);
    let n2018: ZN = zsel_n(n1875, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n2019: ZN = zsel_n(n1146, n1956, r_c255);
    let n2020: ZB = zsel_b(n1146, r_c312, n1839);
    let n2021: ZN = zsel_n(n1146, n1995, n1835);
    let n2022: ZN = zsel_n(n1146, n2000, n2007);
    let n2023: ZN = zsel_n(n806, n1951, r_c20);
    let n2024: ZN = zsel_n(n806, r_c253, n223);
    let n2025: ZN = zsel_n(n806, r_c255, n2019);
    let n2026: ZN = zsel_n(n806, r_c256, n1806);
    let n2027: ZN = zsel_n(n806, r_c258, n1807);
    let n2028: ZN = zsel_n(n806, r_c272, n1382);
    let n2029: ZN = zsel_n(n806, r_c273, n1383);
    let n2030: ZB = zsel_b(n806, r_c312, n2020);
    let n2031: ZN = zsel_n(n806, r_c318, n1989);
    let n2032: ZN = zsel_n(n806, r_c319, n1990);
    let n2033: ZN = zsel_n(n806, r_c320, n2021);
    let n2034: ZN = zsel_n(n806, r_c321, n2022);
    let n2035: ZB = zb_or(n806, n1933);
    let n2036: ZB = zb_or(n806, n1786);
    let n2037: ZB = zn_gt(n2023, zn_splat(P8::from_raw(0i32)));
    let n2038: ZB = zn_lt(n2028, zn_splat(P8::from_raw(-65536i32)));
    let n2039: ZB = zn_gt(n2028, zn_splat(P8::from_raw(7929856i32)));
    let n2040: ZB = zb_or(n2038, n2039);
    let n2041: ZN = zn_min(zn_splat(P8::from_raw(7929856i32)), n2028);
    let n2042: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n2041);
    let n2043: ZN = zsel_n(n2040, n2042, n2028);
    let n2044: ZN = zsel_n(n2040, zn_splat(P8::from_raw(0i32)), n2033);
    let n2045: ZN = zsel_n(n2037, n2028, n2043);
    let n2046: ZN = zsel_n(n2037, n2033, n2044);
    let n2048: ZN = zn_max(n1900, n2002);
    let n2049: ZN = zn_min(n1900, n2004);
    let n2050: ZN = zsel_n(n1901, n2048, n2049);
    let n2051: ZN = zsel_n(n1805, n2050, n1385);
    let n2052: ZN = zsel_n(n1869, n2009, n1886);
    let n2053: ZN = zsel_n(n1869, zn_splat(P8::from_raw(-131072i32)), n2051);
    let n2054: ZN = zsel_n(n1847, n1886, n2052);
    let n2055: ZN = zsel_n(n1847, zn_splat(P8::from_raw(-131072i32)), n2053);
    let n2056: ZB = zsel_b(n1146, r_c312, n1890);
    let n2057: ZN = zsel_n(n1146, n1995, n1886);
    let n2058: ZN = zsel_n(n1146, n2000, n2051);
    let n2059: ZB = zsel_b(n806, r_c312, n2056);
    let n2060: ZN = zsel_n(n806, r_c320, n2057);
    let n2061: ZN = zsel_n(n806, r_c321, n2058);
    let n2062: ZN = zsel_n(n2040, zn_splat(P8::from_raw(0i32)), n2060);
    let n2063: ZN = zsel_n(n2037, n2060, n2062);
    let n2064: ZN = zn_max(n1919, n2002);
    let n2065: ZN = zn_min(n1919, n2004);
    let n2066: ZN = zsel_n(n1920, n2064, n2065);
    let n2067: ZN = zsel_n(n1805, n2066, n1385);
    let n2068: ZN = zsel_n(n1869, n2009, n1905);
    let n2069: ZN = zsel_n(n1869, zn_splat(P8::from_raw(-131072i32)), n2067);
    let n2070: ZN = zsel_n(n1847, n1905, n2068);
    let n2071: ZN = zsel_n(n1847, zn_splat(P8::from_raw(-131072i32)), n2069);
    let n2072: ZB = zsel_b(n1146, r_c312, n1909);
    let n2073: ZN = zsel_n(n1146, n1995, n1905);
    let n2074: ZN = zsel_n(n1146, n2000, n2067);
    let n2075: ZB = zsel_b(n806, r_c312, n2072);
    let n2076: ZN = zsel_n(n806, r_c320, n2073);
    let n2077: ZN = zsel_n(n806, r_c321, n2074);
    let n2078: ZN = zsel_n(n2040, zn_splat(P8::from_raw(0i32)), n2076);
    let n2079: ZN = zsel_n(n2037, n2076, n2078);
    let n2080: ZB = zb_or(r_c266, n111);
    let n2081: ZN = zsel_n(n216, n2012, n1807);
    let n2082: ZN = zsel_n(n216, n2013, n1835);
    let n2083: ZN = zsel_n(n216, n2014, n2007);
    let n2084: ZN = zsel_n(n1146, n1807, n2081);
    let n2085: ZN = zsel_n(n1146, n1995, n2082);
    let n2086: ZN = zsel_n(n1146, n2000, n2083);
    let n2087: ZN = zsel_n(n806, r_c258, n2084);
    let n2088: ZN = zsel_n(n806, r_c320, n2085);
    let n2089: ZN = zsel_n(n806, r_c321, n2086);
    let n2090: ZN = zsel_n(n2040, zn_splat(P8::from_raw(0i32)), n2088);
    let n2091: ZN = zsel_n(n2037, n2088, n2090);
    let n2092: ZN = zsel_n(n216, n2054, n1886);
    let n2093: ZN = zsel_n(n216, n2055, n2051);
    let n2094: ZN = zsel_n(n1146, n1995, n2092);
    let n2095: ZN = zsel_n(n1146, n2000, n2093);
    let n2096: ZN = zsel_n(n806, r_c320, n2094);
    let n2097: ZN = zsel_n(n806, r_c321, n2095);
    let n2098: ZN = zsel_n(n2040, zn_splat(P8::from_raw(0i32)), n2096);
    let n2099: ZN = zsel_n(n2037, n2096, n2098);
    let n2100: ZN = zsel_n(n216, n2070, n1905);
    let n2101: ZN = zsel_n(n216, n2071, n2067);
    let n2102: ZN = zsel_n(n1146, n1995, n2100);
    let n2103: ZN = zsel_n(n1146, n2000, n2101);
    let n2104: ZN = zsel_n(n806, r_c320, n2102);
    let n2105: ZN = zsel_n(n806, r_c321, n2103);
    let n2106: ZN = zsel_n(n2040, zn_splat(P8::from_raw(0i32)), n2104);
    let n2107: ZN = zsel_n(n2037, n2104, n2106);
    let n2108: ZB = zb_or(r_c265, n111);
    let n2109: ZN = zsel_n(n1921, zn_splat(P8::from_raw(655360i32)), n223);
    let n2110: ZN = zsel_n(n1921, zn_splat(P8::from_raw(262144i32)), r_c255);
    let n2111: ZN = zsel_n(n1921, n2015, n1806);
    let n2112: ZN = zsel_n(n1921, zn_splat(P8::from_raw(98304i32)), r_c308);
    let n2113: ZN = zsel_n(n1921, n2018, r_c309);
    let n2114: ZN = zsel_n(n1921, n2017, r_c310);
    let n2115: ZN = zsel_n(n1921, zn_splat(P8::from_raw(0i32)), r_c311);
    let n2116: ZN = zsel_n(n1921, n1871, n1835);
    let n2117: ZN = zsel_n(n1921, zn_splat(P8::from_raw(0i32)), n2007);
    let n2118: ZN = zsel_n(n1146, n223, n2109);
    let n2119: ZN = zsel_n(n1146, n1956, n2110);
    let n2120: ZN = zsel_n(n1146, n1806, n2111);
    let n2121: ZN = zsel_n(n1146, r_c308, n2112);
    let n2122: ZN = zsel_n(n1146, r_c309, n2113);
    let n2123: ZN = zsel_n(n1146, r_c310, n2114);
    let n2124: ZN = zsel_n(n1146, r_c311, n2115);
    let n2125: ZN = zsel_n(n1146, n1995, n2116);
    let n2126: ZN = zsel_n(n1146, n2000, n2117);
    let n2127: ZN = zsel_n(n806, n1951, n1924);
    let n2128: ZB = zsel_b(n806, r_c41, n1925);
    let n2129: ZN = zsel_n(n806, r_c253, n2118);
    let n2130: ZN = zsel_n(n806, r_c255, n2119);
    let n2131: ZN = zsel_n(n806, r_c256, n2120);
    let n2132: ZN = zsel_n(n806, r_c308, n2121);
    let n2133: ZN = zsel_n(n806, r_c309, n2122);
    let n2134: ZN = zsel_n(n806, r_c310, n2123);
    let n2135: ZN = zsel_n(n806, r_c311, n2124);
    let n2136: ZN = zsel_n(n806, r_c320, n2125);
    let n2137: ZN = zsel_n(n806, r_c321, n2126);
    let n2138: ZB = zn_gt(n2127, zn_splat(P8::from_raw(0i32)));
    let n2139: ZN = zsel_n(n2040, zn_splat(P8::from_raw(0i32)), n2136);
    let n2140: ZN = zsel_n(n2138, n2028, n2043);
    let n2141: ZN = zsel_n(n2138, n2136, n2139);
    let n2142: ZN = zsel_n(n1921, zn_splat(P8::from_raw(69510i32)), r_c309);
    let n2143: ZN = zsel_n(n1921, zn_splat(P8::from_raw(-131072i32)), r_c310);
    let n2144: ZN = zsel_n(n1921, zn_splat(P8::from_raw(-327680i32)), n1886);
    let n2145: ZN = zsel_n(n1921, zn_splat(P8::from_raw(0i32)), n2051);
    let n2146: ZN = zsel_n(n1146, r_c309, n2142);
    let n2147: ZN = zsel_n(n1146, r_c310, n2143);
    let n2148: ZN = zsel_n(n1146, n1995, n2144);
    let n2149: ZN = zsel_n(n1146, n2000, n2145);
    let n2150: ZN = zsel_n(n806, r_c309, n2146);
    let n2151: ZN = zsel_n(n806, r_c310, n2147);
    let n2152: ZN = zsel_n(n806, r_c320, n2148);
    let n2153: ZN = zsel_n(n806, r_c321, n2149);
    let n2154: ZN = zsel_n(n2040, zn_splat(P8::from_raw(0i32)), n2152);
    let n2155: ZN = zsel_n(n2138, n2152, n2154);
    let n2156: ZN = zsel_n(n1921, zn_splat(P8::from_raw(131072i32)), r_c310);
    let n2157: ZN = zsel_n(n1921, zn_splat(P8::from_raw(327680i32)), n1905);
    let n2158: ZN = zsel_n(n1921, zn_splat(P8::from_raw(0i32)), n2067);
    let n2159: ZN = zsel_n(n1146, r_c310, n2156);
    let n2160: ZN = zsel_n(n1146, n1995, n2157);
    let n2161: ZN = zsel_n(n1146, n2000, n2158);
    let n2162: ZN = zsel_n(n806, r_c310, n2159);
    let n2163: ZN = zsel_n(n806, r_c320, n2160);
    let n2164: ZN = zsel_n(n806, r_c321, n2161);
    let n2165: ZN = zsel_n(n2040, zn_splat(P8::from_raw(0i32)), n2163);
    let n2166: ZN = zsel_n(n2138, n2163, n2165);
    let n2167: ZN = zsel_n(n1921, zn_splat(P8::from_raw(69510i32)), r_c308);
    let n2168: ZN = zsel_n(n1921, zn_splat(P8::from_raw(98304i32)), r_c309);
    let n2169: ZN = zsel_n(n1921, zn_splat(P8::from_raw(0i32)), r_c310);
    let n2170: ZN = zsel_n(n1921, zn_splat(P8::from_raw(-98304i32)), r_c311);
    let n2171: ZN = zsel_n(n1921, zn_splat(P8::from_raw(0i32)), n1835);
    let n2172: ZN = zsel_n(n1921, zn_splat(P8::from_raw(-327680i32)), n2007);
    let n2173: ZN = zsel_n(n1146, r_c308, n2167);
    let n2174: ZN = zsel_n(n1146, r_c309, n2168);
    let n2175: ZN = zsel_n(n1146, r_c310, n2169);
    let n2176: ZN = zsel_n(n1146, r_c311, n2170);
    let n2177: ZN = zsel_n(n1146, n1995, n2171);
    let n2178: ZN = zsel_n(n1146, n2000, n2172);
    let n2179: ZN = zsel_n(n806, r_c308, n2173);
    let n2180: ZN = zsel_n(n806, r_c309, n2174);
    let n2181: ZN = zsel_n(n806, r_c310, n2175);
    let n2182: ZN = zsel_n(n806, r_c311, n2176);
    let n2183: ZN = zsel_n(n806, r_c320, n2177);
    let n2184: ZN = zsel_n(n806, r_c321, n2178);
    let n2185: ZN = zsel_n(n2040, zn_splat(P8::from_raw(0i32)), n2183);
    let n2186: ZN = zsel_n(n2138, n2183, n2185);
    let n2187: ZN = zsel_n(n1921, zn_splat(P8::from_raw(-231700i32)), n1886);
    let n2188: ZN = zsel_n(n1921, zn_splat(P8::from_raw(-231700i32)), n2051);
    let n2189: ZN = zsel_n(n1146, n1995, n2187);
    let n2190: ZN = zsel_n(n1146, n2000, n2188);
    let n2191: ZN = zsel_n(n806, r_c320, n2189);
    let n2192: ZN = zsel_n(n806, r_c321, n2190);
    let n2193: ZN = zsel_n(n2040, zn_splat(P8::from_raw(0i32)), n2191);
    let n2194: ZN = zsel_n(n2138, n2191, n2193);
    let n2195: ZN = zsel_n(n1921, zn_splat(P8::from_raw(231700i32)), n1905);
    let n2196: ZN = zsel_n(n1921, zn_splat(P8::from_raw(-231700i32)), n2067);
    let n2197: ZN = zsel_n(n1146, n1995, n2195);
    let n2198: ZN = zsel_n(n1146, n2000, n2196);
    let n2199: ZN = zsel_n(n806, r_c320, n2197);
    let n2200: ZN = zsel_n(n806, r_c321, n2198);
    let n2201: ZN = zsel_n(n2040, zn_splat(P8::from_raw(0i32)), n2199);
    let n2202: ZN = zsel_n(n2138, n2199, n2201);
    let n2203: ZN = zsel_n(n1921, zn_splat(P8::from_raw(131072i32)), r_c311);
    let n2204: ZN = zsel_n(n1921, zn_splat(P8::from_raw(327680i32)), n2007);
    let n2205: ZN = zsel_n(n1146, r_c311, n2203);
    let n2206: ZN = zsel_n(n1146, n2000, n2204);
    let n2207: ZN = zsel_n(n806, r_c311, n2205);
    let n2208: ZN = zsel_n(n806, r_c321, n2206);
    let n2209: ZN = zsel_n(n1921, zn_splat(P8::from_raw(231700i32)), n2051);
    let n2210: ZN = zsel_n(n1146, n2000, n2209);
    let n2211: ZN = zsel_n(n806, r_c321, n2210);
    let n2212: ZN = zsel_n(n1921, zn_splat(P8::from_raw(231700i32)), n2067);
    let n2213: ZN = zsel_n(n1146, n2000, n2212);
    let n2214: ZN = zsel_n(n806, r_c321, n2213);
    let n2215: ZN = zsel_n(n1921, n1871, n2082);
    let n2216: ZN = zsel_n(n1921, zn_splat(P8::from_raw(0i32)), n2083);
    let n2217: ZN = zsel_n(n1146, n1995, n2215);
    let n2218: ZN = zsel_n(n1146, n2000, n2216);
    let n2219: ZN = zsel_n(n806, r_c320, n2217);
    let n2220: ZN = zsel_n(n806, r_c321, n2218);
    let n2221: ZN = zsel_n(n2040, zn_splat(P8::from_raw(0i32)), n2219);
    let n2222: ZN = zsel_n(n2138, n2219, n2221);
    let n2223: ZN = zsel_n(n1921, zn_splat(P8::from_raw(-327680i32)), n2092);
    let n2224: ZN = zsel_n(n1921, zn_splat(P8::from_raw(0i32)), n2093);
    let n2225: ZN = zsel_n(n1146, n1995, n2223);
    let n2226: ZN = zsel_n(n1146, n2000, n2224);
    let n2227: ZN = zsel_n(n806, r_c320, n2225);
    let n2228: ZN = zsel_n(n806, r_c321, n2226);
    let n2229: ZN = zsel_n(n2040, zn_splat(P8::from_raw(0i32)), n2227);
    let n2230: ZN = zsel_n(n2138, n2227, n2229);
    let n2231: ZN = zsel_n(n1921, zn_splat(P8::from_raw(327680i32)), n2100);
    let n2232: ZN = zsel_n(n1921, zn_splat(P8::from_raw(0i32)), n2101);
    let n2233: ZN = zsel_n(n1146, n1995, n2231);
    let n2234: ZN = zsel_n(n1146, n2000, n2232);
    let n2235: ZN = zsel_n(n806, r_c320, n2233);
    let n2236: ZN = zsel_n(n806, r_c321, n2234);
    let n2237: ZN = zsel_n(n2040, zn_splat(P8::from_raw(0i32)), n2235);
    let n2238: ZN = zsel_n(n2138, n2235, n2237);
    let n2239: ZN = zsel_n(n1921, zn_splat(P8::from_raw(0i32)), n2082);
    let n2240: ZN = zsel_n(n1921, zn_splat(P8::from_raw(-327680i32)), n2083);
    let n2241: ZN = zsel_n(n1146, n1995, n2239);
    let n2242: ZN = zsel_n(n1146, n2000, n2240);
    let n2243: ZN = zsel_n(n806, r_c320, n2241);
    let n2244: ZN = zsel_n(n806, r_c321, n2242);
    let n2245: ZN = zsel_n(n2040, zn_splat(P8::from_raw(0i32)), n2243);
    let n2246: ZN = zsel_n(n2138, n2243, n2245);
    let n2247: ZN = zsel_n(n1921, zn_splat(P8::from_raw(-231700i32)), n2092);
    let n2248: ZN = zsel_n(n1921, zn_splat(P8::from_raw(-231700i32)), n2093);
    let n2249: ZN = zsel_n(n1146, n1995, n2247);
    let n2250: ZN = zsel_n(n1146, n2000, n2248);
    let n2251: ZN = zsel_n(n806, r_c320, n2249);
    let n2252: ZN = zsel_n(n806, r_c321, n2250);
    let n2253: ZN = zsel_n(n2040, zn_splat(P8::from_raw(0i32)), n2251);
    let n2254: ZN = zsel_n(n2138, n2251, n2253);
    let n2255: ZN = zsel_n(n1921, zn_splat(P8::from_raw(231700i32)), n2100);
    let n2256: ZN = zsel_n(n1921, zn_splat(P8::from_raw(-231700i32)), n2101);
    let n2257: ZN = zsel_n(n1146, n1995, n2255);
    let n2258: ZN = zsel_n(n1146, n2000, n2256);
    let n2259: ZN = zsel_n(n806, r_c320, n2257);
    let n2260: ZN = zsel_n(n806, r_c321, n2258);
    let n2261: ZN = zsel_n(n2040, zn_splat(P8::from_raw(0i32)), n2259);
    let n2262: ZN = zsel_n(n2138, n2259, n2261);
    let n2263: ZN = zsel_n(n1921, zn_splat(P8::from_raw(327680i32)), n2083);
    let n2264: ZN = zsel_n(n1146, n2000, n2263);
    let n2265: ZN = zsel_n(n806, r_c321, n2264);
    let n2266: ZN = zsel_n(n1921, zn_splat(P8::from_raw(231700i32)), n2093);
    let n2267: ZN = zsel_n(n1146, n2000, n2266);
    let n2268: ZN = zsel_n(n806, r_c321, n2267);
    let n2269: ZN = zsel_n(n1921, zn_splat(P8::from_raw(231700i32)), n2101);
    let n2270: ZN = zsel_n(n1146, n2000, n2269);
    let n2271: ZN = zsel_n(n806, r_c321, n2270);
    let n2273: ZW = zw_cellmix_n(39u64, r_c39, 1542469173u64);
    let n2274: ZW = zw_cellmix_n(39u64, r_c39, 668265263u64);
    let n2275: ZW = zw_add(zw_splat(0u64), n2273);
    let n2276: ZW = zw_add(zw_splat(0u64), n2274);
    let n2277: ZW = zw_cellmix_n(84u64, n90, 1542469173u64);
    let n2278: ZW = zw_cellmix_n(84u64, n90, 668265263u64);
    let n2279: ZW = zw_add(n2275, n2277);
    let n2280: ZW = zw_add(n2276, n2278);
    let n2281: ZW = zw_cellmix_n(85u64, n145, 1542469173u64);
    let n2282: ZW = zw_cellmix_n(85u64, n145, 668265263u64);
    let n2283: ZW = zw_add(n2279, n2281);
    let n2284: ZW = zw_add(n2280, n2282);
    let n2285: ZW = zw_cellmix_n(86u64, n144, 1542469173u64);
    let n2286: ZW = zw_cellmix_n(86u64, n144, 668265263u64);
    let n2287: ZW = zw_add(n2283, n2285);
    let n2288: ZW = zw_add(n2284, n2286);
    let n2289: ZW = zw_cellmix_n(87u64, r_c87, 1542469173u64);
    let n2290: ZW = zw_cellmix_n(87u64, r_c87, 668265263u64);
    let n2291: ZW = zw_add(n2287, n2289);
    let n2292: ZW = zw_add(n2288, n2290);
    let n2293: ZW = zw_cellmix_n(256u64, n396, 1542469173u64);
    let n2294: ZW = zw_cellmix_n(256u64, n396, 668265263u64);
    let n2295: ZW = zw_add(n2291, n2293);
    let n2296: ZW = zw_add(n2292, n2294);
    let n2297: ZW = zw_cellmix_n(280u64, n326, 1542469173u64);
    let n2298: ZW = zw_cellmix_n(280u64, n326, 668265263u64);
    let n2299: ZW = zw_add(n2295, n2297);
    let n2300: ZW = zw_add(n2296, n2298);
    let n2301: ZW = zw_cellmix_n(281u64, n397, 1542469173u64);
    let n2302: ZW = zw_cellmix_n(281u64, n397, 668265263u64);
    let n2303: ZW = zw_add(n2299, n2301);
    let n2304: ZW = zw_add(n2300, n2302);
    let n2305: ZW = zw_cellmix_n(20u64, r_c20, 1542469173u64);
    let n2306: ZW = zw_cellmix_n(20u64, r_c20, 668265263u64);
    let n2307: ZW = zw_add(n2303, n2305);
    let n2308: ZW = zw_add(n2304, n2306);
    let n2309: ZW = zw_cellmix_b(41u64, r_c41, 1542469173u64);
    let n2310: ZW = zw_cellmix_b(41u64, r_c41, 668265263u64);
    let n2311: ZW = zw_add(n2307, n2309);
    let n2312: ZW = zw_add(n2308, n2310);
    let n2313: ZW = zw_cellmix_n(236u64, n223, 1542469173u64);
    let n2314: ZW = zw_cellmix_n(236u64, n223, 668265263u64);
    let n2315: ZW = zw_add(n2311, n2313);
    let n2316: ZW = zw_add(n2312, n2314);
    let n2317: ZW = zw_cellmix_n(238u64, zn_splat(P8::from_raw(-65536i32)), 1542469173u64);
    let n2318: ZW = zw_cellmix_n(238u64, zn_splat(P8::from_raw(-65536i32)), 668265263u64);
    let n2319: ZW = zw_add(n2315, n2317);
    let n2320: ZW = zw_add(n2316, n2318);
    let n2321: ZW = zw_cellmix_n(241u64, n731, 1542469173u64);
    let n2322: ZW = zw_cellmix_n(241u64, n731, 668265263u64);
    let n2323: ZW = zw_add(n2319, n2321);
    let n2324: ZW = zw_add(n2320, n2322);
    let n2325: ZW = zw_cellmix_b(248u64, zb_splat(false), 1542469173u64);
    let n2326: ZW = zw_cellmix_b(248u64, zb_splat(false), 668265263u64);
    let n2327: ZW = zw_add(n2323, n2325);
    let n2328: ZW = zw_add(n2324, n2326);
    let n2329: ZW = zw_cellmix_b(249u64, zb_splat(false), 1542469173u64);
    let n2330: ZW = zw_cellmix_b(249u64, zb_splat(false), 668265263u64);
    let n2331: ZW = zw_add(n2327, n2329);
    let n2332: ZW = zw_add(n2328, n2330);
    let n2333: ZW = zw_cellmix_n(255u64, n819, 1542469173u64);
    let n2334: ZW = zw_cellmix_n(255u64, n819, 668265263u64);
    let n2335: ZW = zw_add(n2331, n2333);
    let n2336: ZW = zw_add(n2332, n2334);
    let n2337: ZW = zw_cellmix_n(270u64, r_c308, 1542469173u64);
    let n2338: ZW = zw_cellmix_n(270u64, r_c308, 668265263u64);
    let n2339: ZW = zw_add(n2335, n2337);
    let n2340: ZW = zw_add(n2336, n2338);
    let n2341: ZW = zw_cellmix_n(271u64, r_c309, 1542469173u64);
    let n2342: ZW = zw_cellmix_n(271u64, r_c309, 668265263u64);
    let n2343: ZW = zw_add(n2339, n2341);
    let n2344: ZW = zw_add(n2340, n2342);
    let n2345: ZW = zw_cellmix_n(272u64, r_c310, 1542469173u64);
    let n2346: ZW = zw_cellmix_n(272u64, r_c310, 668265263u64);
    let n2347: ZW = zw_add(n2343, n2345);
    let n2348: ZW = zw_add(n2344, n2346);
    let n2349: ZW = zw_cellmix_n(273u64, r_c311, 1542469173u64);
    let n2350: ZW = zw_cellmix_n(273u64, r_c311, 668265263u64);
    let n2351: ZW = zw_add(n2347, n2349);
    let n2352: ZW = zw_add(n2348, n2350);
    let n2353: ZW = zw_cellmix_b(274u64, n761, 1542469173u64);
    let n2354: ZW = zw_cellmix_b(274u64, n761, 668265263u64);
    let n2355: ZW = zw_add(n2351, n2353);
    let n2356: ZW = zw_add(n2352, n2354);
    let n2357: ZW = zw_cellmix_n(282u64, n820, 1542469173u64);
    let n2358: ZW = zw_cellmix_n(282u64, n820, 668265263u64);
    let n2359: ZW = zw_add(n2355, n2357);
    let n2360: ZW = zw_add(n2356, n2358);
    let n2361: ZW = zw_cellmix_n(283u64, n768, 1542469173u64);
    let n2362: ZW = zw_cellmix_n(283u64, n768, 668265263u64);
    let n2363: ZW = zw_add(n2359, n2361);
    let n2364: ZW = zw_add(n2360, n2362);
    let n2365: ZW = zw_cellmix_b(274u64, n840, 1542469173u64);
    let n2366: ZW = zw_cellmix_b(274u64, n840, 668265263u64);
    let n2367: ZW = zw_add(n2351, n2365);
    let n2368: ZW = zw_add(n2352, n2366);
    let n2369: ZW = zw_cellmix_n(282u64, n851, 1542469173u64);
    let n2370: ZW = zw_cellmix_n(282u64, n851, 668265263u64);
    let n2371: ZW = zw_add(n2367, n2369);
    let n2372: ZW = zw_add(n2368, n2370);
    let n2373: ZW = zw_cellmix_n(283u64, n845, 1542469173u64);
    let n2374: ZW = zw_cellmix_n(283u64, n845, 668265263u64);
    let n2375: ZW = zw_add(n2371, n2373);
    let n2376: ZW = zw_add(n2372, n2374);
    let n2377: ZW = zw_cellmix_b(274u64, n859, 1542469173u64);
    let n2378: ZW = zw_cellmix_b(274u64, n859, 668265263u64);
    let n2379: ZW = zw_add(n2351, n2377);
    let n2380: ZW = zw_add(n2352, n2378);
    let n2381: ZW = zw_cellmix_n(282u64, n870, 1542469173u64);
    let n2382: ZW = zw_cellmix_n(282u64, n870, 668265263u64);
    let n2383: ZW = zw_add(n2379, n2381);
    let n2384: ZW = zw_add(n2380, n2382);
    let n2385: ZW = zw_cellmix_n(283u64, n864, 1542469173u64);
    let n2386: ZW = zw_cellmix_n(283u64, n864, 668265263u64);
    let n2387: ZW = zw_add(n2383, n2385);
    let n2388: ZW = zw_add(n2384, n2386);
    let n2389: ZW = zw_cellmix_n(241u64, n871, 1542469173u64);
    let n2390: ZW = zw_cellmix_n(241u64, n871, 668265263u64);
    let n2391: ZW = zw_add(n2319, n2389);
    let n2392: ZW = zw_add(n2320, n2390);
    let n2393: ZW = zw_add(n2391, n2325);
    let n2394: ZW = zw_add(n2392, n2326);
    let n2395: ZW = zw_cellmix_b(249u64, zb_splat(true), 1542469173u64);
    let n2396: ZW = zw_cellmix_b(249u64, zb_splat(true), 668265263u64);
    let n2397: ZW = zw_add(n2393, n2395);
    let n2398: ZW = zw_add(n2394, n2396);
    let n2399: ZW = zw_add(n2397, n2333);
    let n2400: ZW = zw_add(n2398, n2334);
    let n2401: ZW = zw_add(n2399, n2337);
    let n2402: ZW = zw_add(n2400, n2338);
    let n2403: ZW = zw_add(n2401, n2341);
    let n2404: ZW = zw_add(n2402, n2342);
    let n2405: ZW = zw_add(n2403, n2345);
    let n2406: ZW = zw_add(n2404, n2346);
    let n2407: ZW = zw_add(n2405, n2349);
    let n2408: ZW = zw_add(n2406, n2350);
    let n2409: ZW = zw_add(n2407, n2353);
    let n2410: ZW = zw_add(n2408, n2354);
    let n2411: ZW = zw_cellmix_n(282u64, n880, 1542469173u64);
    let n2412: ZW = zw_cellmix_n(282u64, n880, 668265263u64);
    let n2413: ZW = zw_add(n2409, n2411);
    let n2414: ZW = zw_add(n2410, n2412);
    let n2415: ZW = zw_cellmix_n(283u64, n873, 1542469173u64);
    let n2416: ZW = zw_cellmix_n(283u64, n873, 668265263u64);
    let n2417: ZW = zw_add(n2413, n2415);
    let n2418: ZW = zw_add(n2414, n2416);
    let n2419: ZW = zw_add(n2407, n2365);
    let n2420: ZW = zw_add(n2408, n2366);
    let n2421: ZW = zw_cellmix_n(282u64, n884, 1542469173u64);
    let n2422: ZW = zw_cellmix_n(282u64, n884, 668265263u64);
    let n2423: ZW = zw_add(n2419, n2421);
    let n2424: ZW = zw_add(n2420, n2422);
    let n2425: ZW = zw_cellmix_n(283u64, n882, 1542469173u64);
    let n2426: ZW = zw_cellmix_n(283u64, n882, 668265263u64);
    let n2427: ZW = zw_add(n2423, n2425);
    let n2428: ZW = zw_add(n2424, n2426);
    let n2429: ZW = zw_add(n2407, n2377);
    let n2430: ZW = zw_add(n2408, n2378);
    let n2431: ZW = zw_cellmix_n(282u64, n888, 1542469173u64);
    let n2432: ZW = zw_cellmix_n(282u64, n888, 668265263u64);
    let n2433: ZW = zw_add(n2429, n2431);
    let n2434: ZW = zw_add(n2430, n2432);
    let n2435: ZW = zw_cellmix_n(283u64, n886, 1542469173u64);
    let n2436: ZW = zw_cellmix_n(283u64, n886, 668265263u64);
    let n2437: ZW = zw_add(n2433, n2435);
    let n2438: ZW = zw_add(n2434, n2436);
    let n2439: ZW = zw_cellmix_n(20u64, n890, 1542469173u64);
    let n2440: ZW = zw_cellmix_n(20u64, n890, 668265263u64);
    let n2441: ZW = zw_add(n2303, n2439);
    let n2442: ZW = zw_add(n2304, n2440);
    let n2443: ZW = zw_cellmix_b(41u64, n891, 1542469173u64);
    let n2444: ZW = zw_cellmix_b(41u64, n891, 668265263u64);
    let n2445: ZW = zw_add(n2441, n2443);
    let n2446: ZW = zw_add(n2442, n2444);
    let n2447: ZW = zw_cellmix_n(236u64, n892, 1542469173u64);
    let n2448: ZW = zw_cellmix_n(236u64, n892, 668265263u64);
    let n2449: ZW = zw_add(n2445, n2447);
    let n2450: ZW = zw_add(n2446, n2448);
    let n2451: ZW = zw_cellmix_n(238u64, n893, 1542469173u64);
    let n2452: ZW = zw_cellmix_n(238u64, n893, 668265263u64);
    let n2453: ZW = zw_add(n2449, n2451);
    let n2454: ZW = zw_add(n2450, n2452);
    let n2455: ZW = zw_add(n2453, n2321);
    let n2456: ZW = zw_add(n2454, n2322);
    let n2457: ZW = zw_cellmix_b(248u64, zb_splat(true), 1542469173u64);
    let n2458: ZW = zw_cellmix_b(248u64, zb_splat(true), 668265263u64);
    let n2459: ZW = zw_add(n2455, n2457);
    let n2460: ZW = zw_add(n2456, n2458);
    let n2461: ZW = zw_add(n2459, n2329);
    let n2462: ZW = zw_add(n2460, n2330);
    let n2463: ZW = zw_cellmix_n(255u64, n902, 1542469173u64);
    let n2464: ZW = zw_cellmix_n(255u64, n902, 668265263u64);
    let n2465: ZW = zw_add(n2461, n2463);
    let n2466: ZW = zw_add(n2462, n2464);
    let n2467: ZW = zw_cellmix_n(270u64, n894, 1542469173u64);
    let n2468: ZW = zw_cellmix_n(270u64, n894, 668265263u64);
    let n2469: ZW = zw_add(n2465, n2467);
    let n2470: ZW = zw_add(n2466, n2468);
    let n2471: ZW = zw_cellmix_n(271u64, n895, 1542469173u64);
    let n2472: ZW = zw_cellmix_n(271u64, n895, 668265263u64);
    let n2473: ZW = zw_add(n2469, n2471);
    let n2474: ZW = zw_add(n2470, n2472);
    let n2475: ZW = zw_cellmix_n(272u64, n896, 1542469173u64);
    let n2476: ZW = zw_cellmix_n(272u64, n896, 668265263u64);
    let n2477: ZW = zw_add(n2473, n2475);
    let n2478: ZW = zw_add(n2474, n2476);
    let n2479: ZW = zw_cellmix_n(273u64, n897, 1542469173u64);
    let n2480: ZW = zw_cellmix_n(273u64, n897, 668265263u64);
    let n2481: ZW = zw_add(n2477, n2479);
    let n2482: ZW = zw_add(n2478, n2480);
    let n2483: ZW = zw_add(n2481, n2353);
    let n2484: ZW = zw_add(n2482, n2354);
    let n2485: ZW = zw_cellmix_n(282u64, n903, 1542469173u64);
    let n2486: ZW = zw_cellmix_n(282u64, n903, 668265263u64);
    let n2487: ZW = zw_add(n2483, n2485);
    let n2488: ZW = zw_add(n2484, n2486);
    let n2489: ZW = zw_cellmix_n(283u64, n899, 1542469173u64);
    let n2490: ZW = zw_cellmix_n(283u64, n899, 668265263u64);
    let n2491: ZW = zw_add(n2487, n2489);
    let n2492: ZW = zw_add(n2488, n2490);
    let n2493: ZW = zw_cellmix_n(271u64, n907, 1542469173u64);
    let n2494: ZW = zw_cellmix_n(271u64, n907, 668265263u64);
    let n2495: ZW = zw_add(n2469, n2493);
    let n2496: ZW = zw_add(n2470, n2494);
    let n2497: ZW = zw_cellmix_n(272u64, n908, 1542469173u64);
    let n2498: ZW = zw_cellmix_n(272u64, n908, 668265263u64);
    let n2499: ZW = zw_add(n2495, n2497);
    let n2500: ZW = zw_add(n2496, n2498);
    let n2501: ZW = zw_add(n2499, n2479);
    let n2502: ZW = zw_add(n2500, n2480);
    let n2503: ZW = zw_add(n2501, n2365);
    let n2504: ZW = zw_add(n2502, n2366);
    let n2505: ZW = zw_cellmix_n(282u64, n912, 1542469173u64);
    let n2506: ZW = zw_cellmix_n(282u64, n912, 668265263u64);
    let n2507: ZW = zw_add(n2503, n2505);
    let n2508: ZW = zw_add(n2504, n2506);
    let n2509: ZW = zw_cellmix_n(283u64, n910, 1542469173u64);
    let n2510: ZW = zw_cellmix_n(283u64, n910, 668265263u64);
    let n2511: ZW = zw_add(n2507, n2509);
    let n2512: ZW = zw_add(n2508, n2510);
    let n2513: ZW = zw_cellmix_n(272u64, n913, 1542469173u64);
    let n2514: ZW = zw_cellmix_n(272u64, n913, 668265263u64);
    let n2515: ZW = zw_add(n2495, n2513);
    let n2516: ZW = zw_add(n2496, n2514);
    let n2517: ZW = zw_add(n2515, n2479);
    let n2518: ZW = zw_add(n2516, n2480);
    let n2519: ZW = zw_add(n2517, n2377);
    let n2520: ZW = zw_add(n2518, n2378);
    let n2521: ZW = zw_cellmix_n(282u64, n917, 1542469173u64);
    let n2522: ZW = zw_cellmix_n(282u64, n917, 668265263u64);
    let n2523: ZW = zw_add(n2519, n2521);
    let n2524: ZW = zw_add(n2520, n2522);
    let n2525: ZW = zw_cellmix_n(283u64, n915, 1542469173u64);
    let n2526: ZW = zw_cellmix_n(283u64, n915, 668265263u64);
    let n2527: ZW = zw_add(n2523, n2525);
    let n2528: ZW = zw_add(n2524, n2526);
    let n2529: ZW = zw_cellmix_n(270u64, n918, 1542469173u64);
    let n2530: ZW = zw_cellmix_n(270u64, n918, 668265263u64);
    let n2531: ZW = zw_add(n2465, n2529);
    let n2532: ZW = zw_add(n2466, n2530);
    let n2533: ZW = zw_cellmix_n(271u64, n919, 1542469173u64);
    let n2534: ZW = zw_cellmix_n(271u64, n919, 668265263u64);
    let n2535: ZW = zw_add(n2531, n2533);
    let n2536: ZW = zw_add(n2532, n2534);
    let n2537: ZW = zw_cellmix_n(272u64, n920, 1542469173u64);
    let n2538: ZW = zw_cellmix_n(272u64, n920, 668265263u64);
    let n2539: ZW = zw_add(n2535, n2537);
    let n2540: ZW = zw_add(n2536, n2538);
    let n2541: ZW = zw_cellmix_n(273u64, n921, 1542469173u64);
    let n2542: ZW = zw_cellmix_n(273u64, n921, 668265263u64);
    let n2543: ZW = zw_add(n2539, n2541);
    let n2544: ZW = zw_add(n2540, n2542);
    let n2545: ZW = zw_add(n2543, n2353);
    let n2546: ZW = zw_add(n2544, n2354);
    let n2547: ZW = zw_cellmix_n(282u64, n925, 1542469173u64);
    let n2548: ZW = zw_cellmix_n(282u64, n925, 668265263u64);
    let n2549: ZW = zw_add(n2545, n2547);
    let n2550: ZW = zw_add(n2546, n2548);
    let n2551: ZW = zw_cellmix_n(283u64, n923, 1542469173u64);
    let n2552: ZW = zw_cellmix_n(283u64, n923, 668265263u64);
    let n2553: ZW = zw_add(n2549, n2551);
    let n2554: ZW = zw_add(n2550, n2552);
    let n2555: ZW = zw_add(n2531, n2493);
    let n2556: ZW = zw_add(n2532, n2494);
    let n2557: ZW = zw_add(n2555, n2497);
    let n2558: ZW = zw_add(n2556, n2498);
    let n2559: ZW = zw_add(n2557, n2541);
    let n2560: ZW = zw_add(n2558, n2542);
    let n2561: ZW = zw_add(n2559, n2365);
    let n2562: ZW = zw_add(n2560, n2366);
    let n2563: ZW = zw_cellmix_n(282u64, n929, 1542469173u64);
    let n2564: ZW = zw_cellmix_n(282u64, n929, 668265263u64);
    let n2565: ZW = zw_add(n2561, n2563);
    let n2566: ZW = zw_add(n2562, n2564);
    let n2567: ZW = zw_cellmix_n(283u64, n927, 1542469173u64);
    let n2568: ZW = zw_cellmix_n(283u64, n927, 668265263u64);
    let n2569: ZW = zw_add(n2565, n2567);
    let n2570: ZW = zw_add(n2566, n2568);
    let n2571: ZW = zw_add(n2555, n2513);
    let n2572: ZW = zw_add(n2556, n2514);
    let n2573: ZW = zw_add(n2571, n2541);
    let n2574: ZW = zw_add(n2572, n2542);
    let n2575: ZW = zw_add(n2573, n2377);
    let n2576: ZW = zw_add(n2574, n2378);
    let n2577: ZW = zw_cellmix_n(282u64, n933, 1542469173u64);
    let n2578: ZW = zw_cellmix_n(282u64, n933, 668265263u64);
    let n2579: ZW = zw_add(n2575, n2577);
    let n2580: ZW = zw_add(n2576, n2578);
    let n2581: ZW = zw_cellmix_n(283u64, n931, 1542469173u64);
    let n2582: ZW = zw_cellmix_n(283u64, n931, 668265263u64);
    let n2583: ZW = zw_add(n2579, n2581);
    let n2584: ZW = zw_add(n2580, n2582);
    let n2585: ZW = zw_cellmix_n(273u64, n934, 1542469173u64);
    let n2586: ZW = zw_cellmix_n(273u64, n934, 668265263u64);
    let n2587: ZW = zw_add(n2539, n2585);
    let n2588: ZW = zw_add(n2540, n2586);
    let n2589: ZW = zw_add(n2587, n2353);
    let n2590: ZW = zw_add(n2588, n2354);
    let n2591: ZW = zw_add(n2589, n2547);
    let n2592: ZW = zw_add(n2590, n2548);
    let n2593: ZW = zw_cellmix_n(283u64, n935, 1542469173u64);
    let n2594: ZW = zw_cellmix_n(283u64, n935, 668265263u64);
    let n2595: ZW = zw_add(n2591, n2593);
    let n2596: ZW = zw_add(n2592, n2594);
    let n2597: ZW = zw_add(n2557, n2585);
    let n2598: ZW = zw_add(n2558, n2586);
    let n2599: ZW = zw_add(n2597, n2365);
    let n2600: ZW = zw_add(n2598, n2366);
    let n2601: ZW = zw_add(n2599, n2563);
    let n2602: ZW = zw_add(n2600, n2564);
    let n2603: ZW = zw_cellmix_n(283u64, n936, 1542469173u64);
    let n2604: ZW = zw_cellmix_n(283u64, n936, 668265263u64);
    let n2605: ZW = zw_add(n2601, n2603);
    let n2606: ZW = zw_add(n2602, n2604);
    let n2607: ZW = zw_add(n2571, n2585);
    let n2608: ZW = zw_add(n2572, n2586);
    let n2609: ZW = zw_add(n2607, n2377);
    let n2610: ZW = zw_add(n2608, n2378);
    let n2611: ZW = zw_add(n2609, n2577);
    let n2612: ZW = zw_add(n2610, n2578);
    let n2613: ZW = zw_cellmix_n(283u64, n937, 1542469173u64);
    let n2614: ZW = zw_cellmix_n(283u64, n937, 668265263u64);
    let n2615: ZW = zw_add(n2611, n2613);
    let n2616: ZW = zw_add(n2612, n2614);
    let n2617: ZW = zw_add(n2453, n2389);
    let n2618: ZW = zw_add(n2454, n2390);
    let n2619: ZW = zw_add(n2617, n2457);
    let n2620: ZW = zw_add(n2618, n2458);
    let n2621: ZW = zw_add(n2619, n2395);
    let n2622: ZW = zw_add(n2620, n2396);
    let n2623: ZW = zw_add(n2621, n2463);
    let n2624: ZW = zw_add(n2622, n2464);
    let n2625: ZW = zw_add(n2623, n2467);
    let n2626: ZW = zw_add(n2624, n2468);
    let n2627: ZW = zw_add(n2625, n2471);
    let n2628: ZW = zw_add(n2626, n2472);
    let n2629: ZW = zw_add(n2627, n2475);
    let n2630: ZW = zw_add(n2628, n2476);
    let n2631: ZW = zw_add(n2629, n2479);
    let n2632: ZW = zw_add(n2630, n2480);
    let n2633: ZW = zw_add(n2631, n2353);
    let n2634: ZW = zw_add(n2632, n2354);
    let n2635: ZW = zw_cellmix_n(282u64, n941, 1542469173u64);
    let n2636: ZW = zw_cellmix_n(282u64, n941, 668265263u64);
    let n2637: ZW = zw_add(n2633, n2635);
    let n2638: ZW = zw_add(n2634, n2636);
    let n2639: ZW = zw_cellmix_n(283u64, n939, 1542469173u64);
    let n2640: ZW = zw_cellmix_n(283u64, n939, 668265263u64);
    let n2641: ZW = zw_add(n2637, n2639);
    let n2642: ZW = zw_add(n2638, n2640);
    let n2643: ZW = zw_add(n2625, n2493);
    let n2644: ZW = zw_add(n2626, n2494);
    let n2645: ZW = zw_add(n2643, n2497);
    let n2646: ZW = zw_add(n2644, n2498);
    let n2647: ZW = zw_add(n2645, n2479);
    let n2648: ZW = zw_add(n2646, n2480);
    let n2649: ZW = zw_add(n2647, n2365);
    let n2650: ZW = zw_add(n2648, n2366);
    let n2651: ZW = zw_cellmix_n(282u64, n945, 1542469173u64);
    let n2652: ZW = zw_cellmix_n(282u64, n945, 668265263u64);
    let n2653: ZW = zw_add(n2649, n2651);
    let n2654: ZW = zw_add(n2650, n2652);
    let n2655: ZW = zw_cellmix_n(283u64, n943, 1542469173u64);
    let n2656: ZW = zw_cellmix_n(283u64, n943, 668265263u64);
    let n2657: ZW = zw_add(n2653, n2655);
    let n2658: ZW = zw_add(n2654, n2656);
    let n2659: ZW = zw_add(n2643, n2513);
    let n2660: ZW = zw_add(n2644, n2514);
    let n2661: ZW = zw_add(n2659, n2479);
    let n2662: ZW = zw_add(n2660, n2480);
    let n2663: ZW = zw_add(n2661, n2377);
    let n2664: ZW = zw_add(n2662, n2378);
    let n2665: ZW = zw_cellmix_n(282u64, n949, 1542469173u64);
    let n2666: ZW = zw_cellmix_n(282u64, n949, 668265263u64);
    let n2667: ZW = zw_add(n2663, n2665);
    let n2668: ZW = zw_add(n2664, n2666);
    let n2669: ZW = zw_cellmix_n(283u64, n947, 1542469173u64);
    let n2670: ZW = zw_cellmix_n(283u64, n947, 668265263u64);
    let n2671: ZW = zw_add(n2667, n2669);
    let n2672: ZW = zw_add(n2668, n2670);
    let n2673: ZW = zw_add(n2623, n2529);
    let n2674: ZW = zw_add(n2624, n2530);
    let n2675: ZW = zw_add(n2673, n2533);
    let n2676: ZW = zw_add(n2674, n2534);
    let n2677: ZW = zw_add(n2675, n2537);
    let n2678: ZW = zw_add(n2676, n2538);
    let n2679: ZW = zw_add(n2677, n2541);
    let n2680: ZW = zw_add(n2678, n2542);
    let n2681: ZW = zw_add(n2679, n2353);
    let n2682: ZW = zw_add(n2680, n2354);
    let n2683: ZW = zw_cellmix_n(282u64, n953, 1542469173u64);
    let n2684: ZW = zw_cellmix_n(282u64, n953, 668265263u64);
    let n2685: ZW = zw_add(n2681, n2683);
    let n2686: ZW = zw_add(n2682, n2684);
    let n2687: ZW = zw_cellmix_n(283u64, n951, 1542469173u64);
    let n2688: ZW = zw_cellmix_n(283u64, n951, 668265263u64);
    let n2689: ZW = zw_add(n2685, n2687);
    let n2690: ZW = zw_add(n2686, n2688);
    let n2691: ZW = zw_add(n2673, n2493);
    let n2692: ZW = zw_add(n2674, n2494);
    let n2693: ZW = zw_add(n2691, n2497);
    let n2694: ZW = zw_add(n2692, n2498);
    let n2695: ZW = zw_add(n2693, n2541);
    let n2696: ZW = zw_add(n2694, n2542);
    let n2697: ZW = zw_add(n2695, n2365);
    let n2698: ZW = zw_add(n2696, n2366);
    let n2699: ZW = zw_cellmix_n(282u64, n957, 1542469173u64);
    let n2700: ZW = zw_cellmix_n(282u64, n957, 668265263u64);
    let n2701: ZW = zw_add(n2697, n2699);
    let n2702: ZW = zw_add(n2698, n2700);
    let n2703: ZW = zw_cellmix_n(283u64, n955, 1542469173u64);
    let n2704: ZW = zw_cellmix_n(283u64, n955, 668265263u64);
    let n2705: ZW = zw_add(n2701, n2703);
    let n2706: ZW = zw_add(n2702, n2704);
    let n2707: ZW = zw_add(n2691, n2513);
    let n2708: ZW = zw_add(n2692, n2514);
    let n2709: ZW = zw_add(n2707, n2541);
    let n2710: ZW = zw_add(n2708, n2542);
    let n2711: ZW = zw_add(n2709, n2377);
    let n2712: ZW = zw_add(n2710, n2378);
    let n2713: ZW = zw_cellmix_n(282u64, n961, 1542469173u64);
    let n2714: ZW = zw_cellmix_n(282u64, n961, 668265263u64);
    let n2715: ZW = zw_add(n2711, n2713);
    let n2716: ZW = zw_add(n2712, n2714);
    let n2717: ZW = zw_cellmix_n(283u64, n959, 1542469173u64);
    let n2718: ZW = zw_cellmix_n(283u64, n959, 668265263u64);
    let n2719: ZW = zw_add(n2715, n2717);
    let n2720: ZW = zw_add(n2716, n2718);
    let n2721: ZW = zw_add(n2677, n2585);
    let n2722: ZW = zw_add(n2678, n2586);
    let n2723: ZW = zw_add(n2721, n2353);
    let n2724: ZW = zw_add(n2722, n2354);
    let n2725: ZW = zw_add(n2723, n2683);
    let n2726: ZW = zw_add(n2724, n2684);
    let n2727: ZW = zw_cellmix_n(283u64, n962, 1542469173u64);
    let n2728: ZW = zw_cellmix_n(283u64, n962, 668265263u64);
    let n2729: ZW = zw_add(n2725, n2727);
    let n2730: ZW = zw_add(n2726, n2728);
    let n2731: ZW = zw_add(n2693, n2585);
    let n2732: ZW = zw_add(n2694, n2586);
    let n2733: ZW = zw_add(n2731, n2365);
    let n2734: ZW = zw_add(n2732, n2366);
    let n2735: ZW = zw_add(n2733, n2699);
    let n2736: ZW = zw_add(n2734, n2700);
    let n2737: ZW = zw_cellmix_n(283u64, n963, 1542469173u64);
    let n2738: ZW = zw_cellmix_n(283u64, n963, 668265263u64);
    let n2739: ZW = zw_add(n2735, n2737);
    let n2740: ZW = zw_add(n2736, n2738);
    let n2741: ZW = zw_add(n2707, n2585);
    let n2742: ZW = zw_add(n2708, n2586);
    let n2743: ZW = zw_add(n2741, n2377);
    let n2744: ZW = zw_add(n2742, n2378);
    let n2745: ZW = zw_add(n2743, n2713);
    let n2746: ZW = zw_add(n2744, n2714);
    let n2747: ZW = zw_cellmix_n(283u64, n964, 1542469173u64);
    let n2748: ZW = zw_cellmix_n(283u64, n964, 668265263u64);
    let n2749: ZW = zw_add(n2745, n2747);
    let n2750: ZW = zw_add(n2746, n2748);
    let n2751: ZW = zw_cellmix_n(310u64, n326, 1542469173u64);
    let n2752: ZW = zw_cellmix_n(310u64, n326, 668265263u64);
    let n2753: ZW = zw_add(n2295, n2751);
    let n2754: ZW = zw_add(n2296, n2752);
    let n2755: ZW = zw_cellmix_n(311u64, n397, 1542469173u64);
    let n2756: ZW = zw_cellmix_n(311u64, n397, 668265263u64);
    let n2757: ZW = zw_add(n2753, n2755);
    let n2758: ZW = zw_add(n2754, n2756);
    let n2759: ZW = zw_add(n2757, n2305);
    let n2760: ZW = zw_add(n2758, n2306);
    let n2761: ZW = zw_add(n2759, n2309);
    let n2762: ZW = zw_add(n2760, n2310);
    let n2763: ZW = zw_add(n2761, n2313);
    let n2764: ZW = zw_add(n2762, n2314);
    let n2765: ZW = zw_add(n2763, n2317);
    let n2766: ZW = zw_add(n2764, n2318);
    let n2767: ZW = zw_cellmix_n(239u64, n730, 1542469173u64);
    let n2768: ZW = zw_cellmix_n(239u64, n730, 668265263u64);
    let n2769: ZW = zw_add(n2765, n2767);
    let n2770: ZW = zw_add(n2766, n2768);
    let n2771: ZW = zw_add(n2769, n2321);
    let n2772: ZW = zw_add(n2770, n2322);
    let n2773: ZW = zw_add(n2771, n2325);
    let n2774: ZW = zw_add(n2772, n2326);
    let n2775: ZW = zw_add(n2773, n2329);
    let n2776: ZW = zw_add(n2774, n2330);
    let n2777: ZW = zw_add(n2775, n2333);
    let n2778: ZW = zw_add(n2776, n2334);
    let n2779: ZW = zw_cellmix_n(300u64, r_c308, 1542469173u64);
    let n2780: ZW = zw_cellmix_n(300u64, r_c308, 668265263u64);
    let n2781: ZW = zw_add(n2777, n2779);
    let n2782: ZW = zw_add(n2778, n2780);
    let n2783: ZW = zw_cellmix_n(301u64, r_c309, 1542469173u64);
    let n2784: ZW = zw_cellmix_n(301u64, r_c309, 668265263u64);
    let n2785: ZW = zw_add(n2781, n2783);
    let n2786: ZW = zw_add(n2782, n2784);
    let n2787: ZW = zw_cellmix_n(302u64, r_c310, 1542469173u64);
    let n2788: ZW = zw_cellmix_n(302u64, r_c310, 668265263u64);
    let n2789: ZW = zw_add(n2785, n2787);
    let n2790: ZW = zw_add(n2786, n2788);
    let n2791: ZW = zw_cellmix_n(303u64, r_c311, 1542469173u64);
    let n2792: ZW = zw_cellmix_n(303u64, r_c311, 668265263u64);
    let n2793: ZW = zw_add(n2789, n2791);
    let n2794: ZW = zw_add(n2790, n2792);
    let n2795: ZW = zw_cellmix_b(304u64, n761, 1542469173u64);
    let n2796: ZW = zw_cellmix_b(304u64, n761, 668265263u64);
    let n2797: ZW = zw_add(n2793, n2795);
    let n2798: ZW = zw_add(n2794, n2796);
    let n2799: ZW = zw_cellmix_n(312u64, n820, 1542469173u64);
    let n2800: ZW = zw_cellmix_n(312u64, n820, 668265263u64);
    let n2801: ZW = zw_add(n2797, n2799);
    let n2802: ZW = zw_add(n2798, n2800);
    let n2803: ZW = zw_cellmix_n(313u64, n768, 1542469173u64);
    let n2804: ZW = zw_cellmix_n(313u64, n768, 668265263u64);
    let n2805: ZW = zw_add(n2801, n2803);
    let n2806: ZW = zw_add(n2802, n2804);
    let n2807: ZW = zw_cellmix_b(304u64, n840, 1542469173u64);
    let n2808: ZW = zw_cellmix_b(304u64, n840, 668265263u64);
    let n2809: ZW = zw_add(n2793, n2807);
    let n2810: ZW = zw_add(n2794, n2808);
    let n2811: ZW = zw_cellmix_n(312u64, n851, 1542469173u64);
    let n2812: ZW = zw_cellmix_n(312u64, n851, 668265263u64);
    let n2813: ZW = zw_add(n2809, n2811);
    let n2814: ZW = zw_add(n2810, n2812);
    let n2815: ZW = zw_cellmix_n(313u64, n845, 1542469173u64);
    let n2816: ZW = zw_cellmix_n(313u64, n845, 668265263u64);
    let n2817: ZW = zw_add(n2813, n2815);
    let n2818: ZW = zw_add(n2814, n2816);
    let n2819: ZW = zw_cellmix_b(304u64, n859, 1542469173u64);
    let n2820: ZW = zw_cellmix_b(304u64, n859, 668265263u64);
    let n2821: ZW = zw_add(n2793, n2819);
    let n2822: ZW = zw_add(n2794, n2820);
    let n2823: ZW = zw_cellmix_n(312u64, n870, 1542469173u64);
    let n2824: ZW = zw_cellmix_n(312u64, n870, 668265263u64);
    let n2825: ZW = zw_add(n2821, n2823);
    let n2826: ZW = zw_add(n2822, n2824);
    let n2827: ZW = zw_cellmix_n(313u64, n864, 1542469173u64);
    let n2828: ZW = zw_cellmix_n(313u64, n864, 668265263u64);
    let n2829: ZW = zw_add(n2825, n2827);
    let n2830: ZW = zw_add(n2826, n2828);
    let n2831: ZW = zw_add(n2769, n2389);
    let n2832: ZW = zw_add(n2770, n2390);
    let n2833: ZW = zw_add(n2831, n2325);
    let n2834: ZW = zw_add(n2832, n2326);
    let n2835: ZW = zw_add(n2833, n2395);
    let n2836: ZW = zw_add(n2834, n2396);
    let n2837: ZW = zw_add(n2835, n2333);
    let n2838: ZW = zw_add(n2836, n2334);
    let n2839: ZW = zw_add(n2837, n2779);
    let n2840: ZW = zw_add(n2838, n2780);
    let n2841: ZW = zw_add(n2839, n2783);
    let n2842: ZW = zw_add(n2840, n2784);
    let n2843: ZW = zw_add(n2841, n2787);
    let n2844: ZW = zw_add(n2842, n2788);
    let n2845: ZW = zw_add(n2843, n2791);
    let n2846: ZW = zw_add(n2844, n2792);
    let n2847: ZW = zw_add(n2845, n2795);
    let n2848: ZW = zw_add(n2846, n2796);
    let n2849: ZW = zw_cellmix_n(312u64, n880, 1542469173u64);
    let n2850: ZW = zw_cellmix_n(312u64, n880, 668265263u64);
    let n2851: ZW = zw_add(n2847, n2849);
    let n2852: ZW = zw_add(n2848, n2850);
    let n2853: ZW = zw_cellmix_n(313u64, n873, 1542469173u64);
    let n2854: ZW = zw_cellmix_n(313u64, n873, 668265263u64);
    let n2855: ZW = zw_add(n2851, n2853);
    let n2856: ZW = zw_add(n2852, n2854);
    let n2857: ZW = zw_add(n2845, n2807);
    let n2858: ZW = zw_add(n2846, n2808);
    let n2859: ZW = zw_cellmix_n(312u64, n884, 1542469173u64);
    let n2860: ZW = zw_cellmix_n(312u64, n884, 668265263u64);
    let n2861: ZW = zw_add(n2857, n2859);
    let n2862: ZW = zw_add(n2858, n2860);
    let n2863: ZW = zw_cellmix_n(313u64, n882, 1542469173u64);
    let n2864: ZW = zw_cellmix_n(313u64, n882, 668265263u64);
    let n2865: ZW = zw_add(n2861, n2863);
    let n2866: ZW = zw_add(n2862, n2864);
    let n2867: ZW = zw_add(n2845, n2819);
    let n2868: ZW = zw_add(n2846, n2820);
    let n2869: ZW = zw_cellmix_n(312u64, n888, 1542469173u64);
    let n2870: ZW = zw_cellmix_n(312u64, n888, 668265263u64);
    let n2871: ZW = zw_add(n2867, n2869);
    let n2872: ZW = zw_add(n2868, n2870);
    let n2873: ZW = zw_cellmix_n(313u64, n886, 1542469173u64);
    let n2874: ZW = zw_cellmix_n(313u64, n886, 668265263u64);
    let n2875: ZW = zw_add(n2871, n2873);
    let n2876: ZW = zw_add(n2872, n2874);
    let n2877: ZW = zw_add(n2757, n2439);
    let n2878: ZW = zw_add(n2758, n2440);
    let n2879: ZW = zw_add(n2877, n2443);
    let n2880: ZW = zw_add(n2878, n2444);
    let n2881: ZW = zw_add(n2879, n2447);
    let n2882: ZW = zw_add(n2880, n2448);
    let n2883: ZW = zw_add(n2881, n2451);
    let n2884: ZW = zw_add(n2882, n2452);
    let n2885: ZW = zw_cellmix_n(239u64, n970, 1542469173u64);
    let n2886: ZW = zw_cellmix_n(239u64, n970, 668265263u64);
    let n2887: ZW = zw_add(n2883, n2885);
    let n2888: ZW = zw_add(n2884, n2886);
    let n2889: ZW = zw_add(n2887, n2321);
    let n2890: ZW = zw_add(n2888, n2322);
    let n2891: ZW = zw_add(n2889, n2457);
    let n2892: ZW = zw_add(n2890, n2458);
    let n2893: ZW = zw_add(n2891, n2329);
    let n2894: ZW = zw_add(n2892, n2330);
    let n2895: ZW = zw_add(n2893, n2463);
    let n2896: ZW = zw_add(n2894, n2464);
    let n2897: ZW = zw_cellmix_n(300u64, n894, 1542469173u64);
    let n2898: ZW = zw_cellmix_n(300u64, n894, 668265263u64);
    let n2899: ZW = zw_add(n2895, n2897);
    let n2900: ZW = zw_add(n2896, n2898);
    let n2901: ZW = zw_cellmix_n(301u64, n895, 1542469173u64);
    let n2902: ZW = zw_cellmix_n(301u64, n895, 668265263u64);
    let n2903: ZW = zw_add(n2899, n2901);
    let n2904: ZW = zw_add(n2900, n2902);
    let n2905: ZW = zw_cellmix_n(302u64, n896, 1542469173u64);
    let n2906: ZW = zw_cellmix_n(302u64, n896, 668265263u64);
    let n2907: ZW = zw_add(n2903, n2905);
    let n2908: ZW = zw_add(n2904, n2906);
    let n2909: ZW = zw_cellmix_n(303u64, n897, 1542469173u64);
    let n2910: ZW = zw_cellmix_n(303u64, n897, 668265263u64);
    let n2911: ZW = zw_add(n2907, n2909);
    let n2912: ZW = zw_add(n2908, n2910);
    let n2913: ZW = zw_add(n2911, n2795);
    let n2914: ZW = zw_add(n2912, n2796);
    let n2915: ZW = zw_cellmix_n(312u64, n903, 1542469173u64);
    let n2916: ZW = zw_cellmix_n(312u64, n903, 668265263u64);
    let n2917: ZW = zw_add(n2913, n2915);
    let n2918: ZW = zw_add(n2914, n2916);
    let n2919: ZW = zw_cellmix_n(313u64, n899, 1542469173u64);
    let n2920: ZW = zw_cellmix_n(313u64, n899, 668265263u64);
    let n2921: ZW = zw_add(n2917, n2919);
    let n2922: ZW = zw_add(n2918, n2920);
    let n2923: ZW = zw_cellmix_n(301u64, n907, 1542469173u64);
    let n2924: ZW = zw_cellmix_n(301u64, n907, 668265263u64);
    let n2925: ZW = zw_add(n2899, n2923);
    let n2926: ZW = zw_add(n2900, n2924);
    let n2927: ZW = zw_cellmix_n(302u64, n908, 1542469173u64);
    let n2928: ZW = zw_cellmix_n(302u64, n908, 668265263u64);
    let n2929: ZW = zw_add(n2925, n2927);
    let n2930: ZW = zw_add(n2926, n2928);
    let n2931: ZW = zw_add(n2929, n2909);
    let n2932: ZW = zw_add(n2930, n2910);
    let n2933: ZW = zw_add(n2931, n2807);
    let n2934: ZW = zw_add(n2932, n2808);
    let n2935: ZW = zw_cellmix_n(312u64, n912, 1542469173u64);
    let n2936: ZW = zw_cellmix_n(312u64, n912, 668265263u64);
    let n2937: ZW = zw_add(n2933, n2935);
    let n2938: ZW = zw_add(n2934, n2936);
    let n2939: ZW = zw_cellmix_n(313u64, n910, 1542469173u64);
    let n2940: ZW = zw_cellmix_n(313u64, n910, 668265263u64);
    let n2941: ZW = zw_add(n2937, n2939);
    let n2942: ZW = zw_add(n2938, n2940);
    let n2943: ZW = zw_cellmix_n(302u64, n913, 1542469173u64);
    let n2944: ZW = zw_cellmix_n(302u64, n913, 668265263u64);
    let n2945: ZW = zw_add(n2925, n2943);
    let n2946: ZW = zw_add(n2926, n2944);
    let n2947: ZW = zw_add(n2945, n2909);
    let n2948: ZW = zw_add(n2946, n2910);
    let n2949: ZW = zw_add(n2947, n2819);
    let n2950: ZW = zw_add(n2948, n2820);
    let n2951: ZW = zw_cellmix_n(312u64, n917, 1542469173u64);
    let n2952: ZW = zw_cellmix_n(312u64, n917, 668265263u64);
    let n2953: ZW = zw_add(n2949, n2951);
    let n2954: ZW = zw_add(n2950, n2952);
    let n2955: ZW = zw_cellmix_n(313u64, n915, 1542469173u64);
    let n2956: ZW = zw_cellmix_n(313u64, n915, 668265263u64);
    let n2957: ZW = zw_add(n2953, n2955);
    let n2958: ZW = zw_add(n2954, n2956);
    let n2959: ZW = zw_cellmix_n(300u64, n918, 1542469173u64);
    let n2960: ZW = zw_cellmix_n(300u64, n918, 668265263u64);
    let n2961: ZW = zw_add(n2895, n2959);
    let n2962: ZW = zw_add(n2896, n2960);
    let n2963: ZW = zw_cellmix_n(301u64, n919, 1542469173u64);
    let n2964: ZW = zw_cellmix_n(301u64, n919, 668265263u64);
    let n2965: ZW = zw_add(n2961, n2963);
    let n2966: ZW = zw_add(n2962, n2964);
    let n2967: ZW = zw_cellmix_n(302u64, n920, 1542469173u64);
    let n2968: ZW = zw_cellmix_n(302u64, n920, 668265263u64);
    let n2969: ZW = zw_add(n2965, n2967);
    let n2970: ZW = zw_add(n2966, n2968);
    let n2971: ZW = zw_cellmix_n(303u64, n921, 1542469173u64);
    let n2972: ZW = zw_cellmix_n(303u64, n921, 668265263u64);
    let n2973: ZW = zw_add(n2969, n2971);
    let n2974: ZW = zw_add(n2970, n2972);
    let n2975: ZW = zw_add(n2973, n2795);
    let n2976: ZW = zw_add(n2974, n2796);
    let n2977: ZW = zw_cellmix_n(312u64, n925, 1542469173u64);
    let n2978: ZW = zw_cellmix_n(312u64, n925, 668265263u64);
    let n2979: ZW = zw_add(n2975, n2977);
    let n2980: ZW = zw_add(n2976, n2978);
    let n2981: ZW = zw_cellmix_n(313u64, n923, 1542469173u64);
    let n2982: ZW = zw_cellmix_n(313u64, n923, 668265263u64);
    let n2983: ZW = zw_add(n2979, n2981);
    let n2984: ZW = zw_add(n2980, n2982);
    let n2985: ZW = zw_add(n2961, n2923);
    let n2986: ZW = zw_add(n2962, n2924);
    let n2987: ZW = zw_add(n2985, n2927);
    let n2988: ZW = zw_add(n2986, n2928);
    let n2989: ZW = zw_add(n2987, n2971);
    let n2990: ZW = zw_add(n2988, n2972);
    let n2991: ZW = zw_add(n2989, n2807);
    let n2992: ZW = zw_add(n2990, n2808);
    let n2993: ZW = zw_cellmix_n(312u64, n929, 1542469173u64);
    let n2994: ZW = zw_cellmix_n(312u64, n929, 668265263u64);
    let n2995: ZW = zw_add(n2991, n2993);
    let n2996: ZW = zw_add(n2992, n2994);
    let n2997: ZW = zw_cellmix_n(313u64, n927, 1542469173u64);
    let n2998: ZW = zw_cellmix_n(313u64, n927, 668265263u64);
    let n2999: ZW = zw_add(n2995, n2997);
    let n3000: ZW = zw_add(n2996, n2998);
    let n3001: ZW = zw_add(n2985, n2943);
    let n3002: ZW = zw_add(n2986, n2944);
    let n3003: ZW = zw_add(n3001, n2971);
    let n3004: ZW = zw_add(n3002, n2972);
    let n3005: ZW = zw_add(n3003, n2819);
    let n3006: ZW = zw_add(n3004, n2820);
    let n3007: ZW = zw_cellmix_n(312u64, n933, 1542469173u64);
    let n3008: ZW = zw_cellmix_n(312u64, n933, 668265263u64);
    let n3009: ZW = zw_add(n3005, n3007);
    let n3010: ZW = zw_add(n3006, n3008);
    let n3011: ZW = zw_cellmix_n(313u64, n931, 1542469173u64);
    let n3012: ZW = zw_cellmix_n(313u64, n931, 668265263u64);
    let n3013: ZW = zw_add(n3009, n3011);
    let n3014: ZW = zw_add(n3010, n3012);
    let n3015: ZW = zw_cellmix_n(303u64, n934, 1542469173u64);
    let n3016: ZW = zw_cellmix_n(303u64, n934, 668265263u64);
    let n3017: ZW = zw_add(n2969, n3015);
    let n3018: ZW = zw_add(n2970, n3016);
    let n3019: ZW = zw_add(n3017, n2795);
    let n3020: ZW = zw_add(n3018, n2796);
    let n3021: ZW = zw_add(n3019, n2977);
    let n3022: ZW = zw_add(n3020, n2978);
    let n3023: ZW = zw_cellmix_n(313u64, n935, 1542469173u64);
    let n3024: ZW = zw_cellmix_n(313u64, n935, 668265263u64);
    let n3025: ZW = zw_add(n3021, n3023);
    let n3026: ZW = zw_add(n3022, n3024);
    let n3027: ZW = zw_add(n2987, n3015);
    let n3028: ZW = zw_add(n2988, n3016);
    let n3029: ZW = zw_add(n3027, n2807);
    let n3030: ZW = zw_add(n3028, n2808);
    let n3031: ZW = zw_add(n3029, n2993);
    let n3032: ZW = zw_add(n3030, n2994);
    let n3033: ZW = zw_cellmix_n(313u64, n936, 1542469173u64);
    let n3034: ZW = zw_cellmix_n(313u64, n936, 668265263u64);
    let n3035: ZW = zw_add(n3031, n3033);
    let n3036: ZW = zw_add(n3032, n3034);
    let n3037: ZW = zw_add(n3001, n3015);
    let n3038: ZW = zw_add(n3002, n3016);
    let n3039: ZW = zw_add(n3037, n2819);
    let n3040: ZW = zw_add(n3038, n2820);
    let n3041: ZW = zw_add(n3039, n3007);
    let n3042: ZW = zw_add(n3040, n3008);
    let n3043: ZW = zw_cellmix_n(313u64, n937, 1542469173u64);
    let n3044: ZW = zw_cellmix_n(313u64, n937, 668265263u64);
    let n3045: ZW = zw_add(n3041, n3043);
    let n3046: ZW = zw_add(n3042, n3044);
    let n3047: ZW = zw_add(n2887, n2389);
    let n3048: ZW = zw_add(n2888, n2390);
    let n3049: ZW = zw_add(n3047, n2457);
    let n3050: ZW = zw_add(n3048, n2458);
    let n3051: ZW = zw_add(n3049, n2395);
    let n3052: ZW = zw_add(n3050, n2396);
    let n3053: ZW = zw_add(n3051, n2463);
    let n3054: ZW = zw_add(n3052, n2464);
    let n3055: ZW = zw_add(n3053, n2897);
    let n3056: ZW = zw_add(n3054, n2898);
    let n3057: ZW = zw_add(n3055, n2901);
    let n3058: ZW = zw_add(n3056, n2902);
    let n3059: ZW = zw_add(n3057, n2905);
    let n3060: ZW = zw_add(n3058, n2906);
    let n3061: ZW = zw_add(n3059, n2909);
    let n3062: ZW = zw_add(n3060, n2910);
    let n3063: ZW = zw_add(n3061, n2795);
    let n3064: ZW = zw_add(n3062, n2796);
    let n3065: ZW = zw_cellmix_n(312u64, n941, 1542469173u64);
    let n3066: ZW = zw_cellmix_n(312u64, n941, 668265263u64);
    let n3067: ZW = zw_add(n3063, n3065);
    let n3068: ZW = zw_add(n3064, n3066);
    let n3069: ZW = zw_cellmix_n(313u64, n939, 1542469173u64);
    let n3070: ZW = zw_cellmix_n(313u64, n939, 668265263u64);
    let n3071: ZW = zw_add(n3067, n3069);
    let n3072: ZW = zw_add(n3068, n3070);
    let n3073: ZW = zw_add(n3055, n2923);
    let n3074: ZW = zw_add(n3056, n2924);
    let n3075: ZW = zw_add(n3073, n2927);
    let n3076: ZW = zw_add(n3074, n2928);
    let n3077: ZW = zw_add(n3075, n2909);
    let n3078: ZW = zw_add(n3076, n2910);
    let n3079: ZW = zw_add(n3077, n2807);
    let n3080: ZW = zw_add(n3078, n2808);
    let n3081: ZW = zw_cellmix_n(312u64, n945, 1542469173u64);
    let n3082: ZW = zw_cellmix_n(312u64, n945, 668265263u64);
    let n3083: ZW = zw_add(n3079, n3081);
    let n3084: ZW = zw_add(n3080, n3082);
    let n3085: ZW = zw_cellmix_n(313u64, n943, 1542469173u64);
    let n3086: ZW = zw_cellmix_n(313u64, n943, 668265263u64);
    let n3087: ZW = zw_add(n3083, n3085);
    let n3088: ZW = zw_add(n3084, n3086);
    let n3089: ZW = zw_add(n3073, n2943);
    let n3090: ZW = zw_add(n3074, n2944);
    let n3091: ZW = zw_add(n3089, n2909);
    let n3092: ZW = zw_add(n3090, n2910);
    let n3093: ZW = zw_add(n3091, n2819);
    let n3094: ZW = zw_add(n3092, n2820);
    let n3095: ZW = zw_cellmix_n(312u64, n949, 1542469173u64);
    let n3096: ZW = zw_cellmix_n(312u64, n949, 668265263u64);
    let n3097: ZW = zw_add(n3093, n3095);
    let n3098: ZW = zw_add(n3094, n3096);
    let n3099: ZW = zw_cellmix_n(313u64, n947, 1542469173u64);
    let n3100: ZW = zw_cellmix_n(313u64, n947, 668265263u64);
    let n3101: ZW = zw_add(n3097, n3099);
    let n3102: ZW = zw_add(n3098, n3100);
    let n3103: ZW = zw_add(n3053, n2959);
    let n3104: ZW = zw_add(n3054, n2960);
    let n3105: ZW = zw_add(n3103, n2963);
    let n3106: ZW = zw_add(n3104, n2964);
    let n3107: ZW = zw_add(n3105, n2967);
    let n3108: ZW = zw_add(n3106, n2968);
    let n3109: ZW = zw_add(n3107, n2971);
    let n3110: ZW = zw_add(n3108, n2972);
    let n3111: ZW = zw_add(n3109, n2795);
    let n3112: ZW = zw_add(n3110, n2796);
    let n3113: ZW = zw_cellmix_n(312u64, n953, 1542469173u64);
    let n3114: ZW = zw_cellmix_n(312u64, n953, 668265263u64);
    let n3115: ZW = zw_add(n3111, n3113);
    let n3116: ZW = zw_add(n3112, n3114);
    let n3117: ZW = zw_cellmix_n(313u64, n951, 1542469173u64);
    let n3118: ZW = zw_cellmix_n(313u64, n951, 668265263u64);
    let n3119: ZW = zw_add(n3115, n3117);
    let n3120: ZW = zw_add(n3116, n3118);
    let n3121: ZW = zw_add(n3103, n2923);
    let n3122: ZW = zw_add(n3104, n2924);
    let n3123: ZW = zw_add(n3121, n2927);
    let n3124: ZW = zw_add(n3122, n2928);
    let n3125: ZW = zw_add(n3123, n2971);
    let n3126: ZW = zw_add(n3124, n2972);
    let n3127: ZW = zw_add(n3125, n2807);
    let n3128: ZW = zw_add(n3126, n2808);
    let n3129: ZW = zw_cellmix_n(312u64, n957, 1542469173u64);
    let n3130: ZW = zw_cellmix_n(312u64, n957, 668265263u64);
    let n3131: ZW = zw_add(n3127, n3129);
    let n3132: ZW = zw_add(n3128, n3130);
    let n3133: ZW = zw_cellmix_n(313u64, n955, 1542469173u64);
    let n3134: ZW = zw_cellmix_n(313u64, n955, 668265263u64);
    let n3135: ZW = zw_add(n3131, n3133);
    let n3136: ZW = zw_add(n3132, n3134);
    let n3137: ZW = zw_add(n3121, n2943);
    let n3138: ZW = zw_add(n3122, n2944);
    let n3139: ZW = zw_add(n3137, n2971);
    let n3140: ZW = zw_add(n3138, n2972);
    let n3141: ZW = zw_add(n3139, n2819);
    let n3142: ZW = zw_add(n3140, n2820);
    let n3143: ZW = zw_cellmix_n(312u64, n961, 1542469173u64);
    let n3144: ZW = zw_cellmix_n(312u64, n961, 668265263u64);
    let n3145: ZW = zw_add(n3141, n3143);
    let n3146: ZW = zw_add(n3142, n3144);
    let n3147: ZW = zw_cellmix_n(313u64, n959, 1542469173u64);
    let n3148: ZW = zw_cellmix_n(313u64, n959, 668265263u64);
    let n3149: ZW = zw_add(n3145, n3147);
    let n3150: ZW = zw_add(n3146, n3148);
    let n3151: ZW = zw_add(n3107, n3015);
    let n3152: ZW = zw_add(n3108, n3016);
    let n3153: ZW = zw_add(n3151, n2795);
    let n3154: ZW = zw_add(n3152, n2796);
    let n3155: ZW = zw_add(n3153, n3113);
    let n3156: ZW = zw_add(n3154, n3114);
    let n3157: ZW = zw_cellmix_n(313u64, n962, 1542469173u64);
    let n3158: ZW = zw_cellmix_n(313u64, n962, 668265263u64);
    let n3159: ZW = zw_add(n3155, n3157);
    let n3160: ZW = zw_add(n3156, n3158);
    let n3161: ZW = zw_add(n3123, n3015);
    let n3162: ZW = zw_add(n3124, n3016);
    let n3163: ZW = zw_add(n3161, n2807);
    let n3164: ZW = zw_add(n3162, n2808);
    let n3165: ZW = zw_add(n3163, n3129);
    let n3166: ZW = zw_add(n3164, n3130);
    let n3167: ZW = zw_cellmix_n(313u64, n963, 1542469173u64);
    let n3168: ZW = zw_cellmix_n(313u64, n963, 668265263u64);
    let n3169: ZW = zw_add(n3165, n3167);
    let n3170: ZW = zw_add(n3166, n3168);
    let n3171: ZW = zw_add(n3137, n3015);
    let n3172: ZW = zw_add(n3138, n3016);
    let n3173: ZW = zw_add(n3171, n2819);
    let n3174: ZW = zw_add(n3172, n2820);
    let n3175: ZW = zw_add(n3173, n3143);
    let n3176: ZW = zw_add(n3174, n3144);
    let n3177: ZW = zw_cellmix_n(313u64, n964, 1542469173u64);
    let n3178: ZW = zw_cellmix_n(313u64, n964, 668265263u64);
    let n3179: ZW = zw_add(n3175, n3177);
    let n3180: ZW = zw_add(n3176, n3178);
    let n3181: ZW = zw_add(zw_splat(0u64), n2277);
    let n3182: ZW = zw_add(zw_splat(0u64), n2278);
    let n3183: ZW = zw_add(n3181, n2281);
    let n3184: ZW = zw_add(n3182, n2282);
    let n3185: ZW = zw_add(n3183, n2285);
    let n3186: ZW = zw_add(n3184, n2286);
    let n3187: ZW = zw_cellmix_n(87u64, n1044, 1542469173u64);
    let n3188: ZW = zw_cellmix_n(87u64, n1044, 668265263u64);
    let n3189: ZW = zw_add(n3185, n3187);
    let n3190: ZW = zw_add(n3186, n3188);
    let n3191: ZW = zw_add(n3189, n2305);
    let n3192: ZW = zw_add(n3190, n2306);
    let n3193: ZW = zw_add(n3191, n2309);
    let n3194: ZW = zw_add(n3192, n2310);
    let n3195: ZW = zw_add(n3189, n2439);
    let n3196: ZW = zw_add(n3190, n2440);
    let n3197: ZW = zw_add(n3195, n2443);
    let n3198: ZW = zw_add(n3196, n2444);
    let n3199: ZW = zw_cellmix_n(87u64, n1881, 1542469173u64);
    let n3200: ZW = zw_cellmix_n(87u64, n1881, 668265263u64);
    let n3201: ZW = zw_add(n3185, n3199);
    let n3202: ZW = zw_add(n3186, n3200);
    let n3203: ZW = zw_add(n3201, n2305);
    let n3204: ZW = zw_add(n3202, n2306);
    let n3205: ZW = zw_add(n3203, n2309);
    let n3206: ZW = zw_add(n3204, n2310);
    let n3207: ZW = zw_cellmix_n(20u64, n1924, 1542469173u64);
    let n3208: ZW = zw_cellmix_n(20u64, n1924, 668265263u64);
    let n3209: ZW = zw_add(n3201, n3207);
    let n3210: ZW = zw_add(n3202, n3208);
    let n3211: ZW = zw_cellmix_b(41u64, n1925, 1542469173u64);
    let n3212: ZW = zw_cellmix_b(41u64, n1925, 668265263u64);
    let n3213: ZW = zw_add(n3209, n3211);
    let n3214: ZW = zw_add(n3210, n3212);
    let n3215: ZW = zw_cellmix_b(38u64, n1937, 1542469173u64);
    let n3216: ZW = zw_cellmix_b(38u64, n1937, 668265263u64);
    let n3217: ZW = zw_add(zw_splat(0u64), n3215);
    let n3218: ZW = zw_add(zw_splat(0u64), n3216);
    let n3219: ZW = zw_cellmix_n(39u64, n1948, 1542469173u64);
    let n3220: ZW = zw_cellmix_n(39u64, n1948, 668265263u64);
    let n3221: ZW = zw_add(n3217, n3219);
    let n3222: ZW = zw_add(n3218, n3220);
    let n3223: ZW = zw_add(n3221, n2277);
    let n3224: ZW = zw_add(n3222, n2278);
    let n3225: ZW = zw_add(n3223, n2281);
    let n3226: ZW = zw_add(n3224, n2282);
    let n3227: ZW = zw_add(n3225, n2285);
    let n3228: ZW = zw_add(n3226, n2286);
    let n3229: ZW = zw_cellmix_n(87u64, n1947, 1542469173u64);
    let n3230: ZW = zw_cellmix_n(87u64, n1947, 668265263u64);
    let n3231: ZW = zw_add(n3227, n3229);
    let n3232: ZW = zw_add(n3228, n3230);
    let n3233: ZW = zw_cellmix_n(234u64, n1938, 1542469173u64);
    let n3234: ZW = zw_cellmix_n(234u64, n1938, 668265263u64);
    let n3235: ZW = zw_add(n3231, n3233);
    let n3236: ZW = zw_add(n3232, n3234);
    let n3237: ZW = zw_cellmix_n(246u64, n1939, 1542469173u64);
    let n3238: ZW = zw_cellmix_n(246u64, n1939, 668265263u64);
    let n3239: ZW = zw_add(n3235, n3237);
    let n3240: ZW = zw_add(n3236, n3238);
    let n3241: ZW = zw_cellmix_n(250u64, n1940, 1542469173u64);
    let n3242: ZW = zw_cellmix_n(250u64, n1940, 668265263u64);
    let n3243: ZW = zw_add(n3239, n3241);
    let n3244: ZW = zw_add(n3240, n3242);
    let n3245: ZW = zw_add(n3243, n2305);
    let n3246: ZW = zw_add(n3244, n2306);
    let n3247: ZW = zw_cellmix_n(20u64, n1950, 1542469173u64);
    let n3248: ZW = zw_cellmix_n(20u64, n1950, 668265263u64);
    let n3249: ZW = zw_add(n3243, n3247);
    let n3250: ZW = zw_add(n3244, n3248);
    let n3251: ZW = zw_cellmix_n(273u64, n2029, 1542469173u64);
    let n3252: ZW = zw_cellmix_n(273u64, n2029, 668265263u64);
    let n3253: ZW = zw_add(n2291, n3251);
    let n3254: ZW = zw_add(n2292, n3252);
    let n3255: ZW = zw_cellmix_n(318u64, n2031, 1542469173u64);
    let n3256: ZW = zw_cellmix_n(318u64, n2031, 668265263u64);
    let n3257: ZW = zw_add(n3253, n3255);
    let n3258: ZW = zw_add(n3254, n3256);
    let n3259: ZW = zw_cellmix_n(319u64, n2032, 1542469173u64);
    let n3260: ZW = zw_cellmix_n(319u64, n2032, 668265263u64);
    let n3261: ZW = zw_add(n3257, n3259);
    let n3262: ZW = zw_add(n3258, n3260);
    let n3263: ZW = zw_cellmix_n(20u64, n2023, 1542469173u64);
    let n3264: ZW = zw_cellmix_n(20u64, n2023, 668265263u64);
    let n3265: ZW = zw_add(n3261, n3263);
    let n3266: ZW = zw_add(n3262, n3264);
    let n3267: ZW = zw_add(n3265, n2309);
    let n3268: ZW = zw_add(n3266, n2310);
    let n3269: ZW = zw_cellmix_n(253u64, n2024, 1542469173u64);
    let n3270: ZW = zw_cellmix_n(253u64, n2024, 668265263u64);
    let n3271: ZW = zw_add(n3267, n3269);
    let n3272: ZW = zw_add(n3268, n3270);
    let n3273: ZW = zw_cellmix_n(255u64, n2025, 1542469173u64);
    let n3274: ZW = zw_cellmix_n(255u64, n2025, 668265263u64);
    let n3275: ZW = zw_add(n3271, n3273);
    let n3276: ZW = zw_add(n3272, n3274);
    let n3277: ZW = zw_cellmix_n(256u64, n2026, 1542469173u64);
    let n3278: ZW = zw_cellmix_n(256u64, n2026, 668265263u64);
    let n3279: ZW = zw_add(n3275, n3277);
    let n3280: ZW = zw_add(n3276, n3278);
    let n3281: ZW = zw_cellmix_n(258u64, n2027, 1542469173u64);
    let n3282: ZW = zw_cellmix_n(258u64, n2027, 668265263u64);
    let n3283: ZW = zw_add(n3279, n3281);
    let n3284: ZW = zw_add(n3280, n3282);
    let n3285: ZW = zw_cellmix_b(265u64, n1957, 1542469173u64);
    let n3286: ZW = zw_cellmix_b(265u64, n1957, 668265263u64);
    let n3287: ZW = zw_add(n3283, n3285);
    let n3288: ZW = zw_add(n3284, n3286);
    let n3289: ZW = zw_cellmix_b(266u64, n1958, 1542469173u64);
    let n3290: ZW = zw_cellmix_b(266u64, n1958, 668265263u64);
    let n3291: ZW = zw_add(n3287, n3289);
    let n3292: ZW = zw_add(n3288, n3290);
    let n3293: ZW = zw_cellmix_n(272u64, n2045, 1542469173u64);
    let n3294: ZW = zw_cellmix_n(272u64, n2045, 668265263u64);
    let n3295: ZW = zw_add(n3291, n3293);
    let n3296: ZW = zw_add(n3292, n3294);
    let n3297: ZW = zw_cellmix_n(308u64, r_c308, 1542469173u64);
    let n3298: ZW = zw_cellmix_n(308u64, r_c308, 668265263u64);
    let n3299: ZW = zw_add(n3295, n3297);
    let n3300: ZW = zw_add(n3296, n3298);
    let n3301: ZW = zw_cellmix_n(309u64, r_c309, 1542469173u64);
    let n3302: ZW = zw_cellmix_n(309u64, r_c309, 668265263u64);
    let n3303: ZW = zw_add(n3299, n3301);
    let n3304: ZW = zw_add(n3300, n3302);
    let n3305: ZW = zw_cellmix_n(310u64, r_c310, 1542469173u64);
    let n3306: ZW = zw_cellmix_n(310u64, r_c310, 668265263u64);
    let n3307: ZW = zw_add(n3303, n3305);
    let n3308: ZW = zw_add(n3304, n3306);
    let n3309: ZW = zw_cellmix_n(311u64, r_c311, 1542469173u64);
    let n3310: ZW = zw_cellmix_n(311u64, r_c311, 668265263u64);
    let n3311: ZW = zw_add(n3307, n3309);
    let n3312: ZW = zw_add(n3308, n3310);
    let n3313: ZW = zw_cellmix_b(312u64, n2030, 1542469173u64);
    let n3314: ZW = zw_cellmix_b(312u64, n2030, 668265263u64);
    let n3315: ZW = zw_add(n3311, n3313);
    let n3316: ZW = zw_add(n3312, n3314);
    let n3317: ZW = zw_cellmix_n(320u64, n2046, 1542469173u64);
    let n3318: ZW = zw_cellmix_n(320u64, n2046, 668265263u64);
    let n3319: ZW = zw_add(n3315, n3317);
    let n3320: ZW = zw_add(n3316, n3318);
    let n3321: ZW = zw_cellmix_n(321u64, n2034, 1542469173u64);
    let n3322: ZW = zw_cellmix_n(321u64, n2034, 668265263u64);
    let n3323: ZW = zw_add(n3319, n3321);
    let n3324: ZW = zw_add(n3320, n3322);
    let n3325: ZW = zw_cellmix_b(312u64, n2059, 1542469173u64);
    let n3326: ZW = zw_cellmix_b(312u64, n2059, 668265263u64);
    let n3327: ZW = zw_add(n3311, n3325);
    let n3328: ZW = zw_add(n3312, n3326);
    let n3329: ZW = zw_cellmix_n(320u64, n2063, 1542469173u64);
    let n3330: ZW = zw_cellmix_n(320u64, n2063, 668265263u64);
    let n3331: ZW = zw_add(n3327, n3329);
    let n3332: ZW = zw_add(n3328, n3330);
    let n3333: ZW = zw_cellmix_n(321u64, n2061, 1542469173u64);
    let n3334: ZW = zw_cellmix_n(321u64, n2061, 668265263u64);
    let n3335: ZW = zw_add(n3331, n3333);
    let n3336: ZW = zw_add(n3332, n3334);
    let n3337: ZW = zw_cellmix_b(312u64, n2075, 1542469173u64);
    let n3338: ZW = zw_cellmix_b(312u64, n2075, 668265263u64);
    let n3339: ZW = zw_add(n3311, n3337);
    let n3340: ZW = zw_add(n3312, n3338);
    let n3341: ZW = zw_cellmix_n(320u64, n2079, 1542469173u64);
    let n3342: ZW = zw_cellmix_n(320u64, n2079, 668265263u64);
    let n3343: ZW = zw_add(n3339, n3341);
    let n3344: ZW = zw_add(n3340, n3342);
    let n3345: ZW = zw_cellmix_n(321u64, n2077, 1542469173u64);
    let n3346: ZW = zw_cellmix_n(321u64, n2077, 668265263u64);
    let n3347: ZW = zw_add(n3343, n3345);
    let n3348: ZW = zw_add(n3344, n3346);
    let n3349: ZW = zw_cellmix_n(258u64, n2087, 1542469173u64);
    let n3350: ZW = zw_cellmix_n(258u64, n2087, 668265263u64);
    let n3351: ZW = zw_add(n3279, n3349);
    let n3352: ZW = zw_add(n3280, n3350);
    let n3353: ZW = zw_add(n3351, n3285);
    let n3354: ZW = zw_add(n3352, n3286);
    let n3355: ZW = zw_cellmix_b(266u64, n2080, 1542469173u64);
    let n3356: ZW = zw_cellmix_b(266u64, n2080, 668265263u64);
    let n3357: ZW = zw_add(n3353, n3355);
    let n3358: ZW = zw_add(n3354, n3356);
    let n3359: ZW = zw_add(n3357, n3293);
    let n3360: ZW = zw_add(n3358, n3294);
    let n3361: ZW = zw_add(n3359, n3297);
    let n3362: ZW = zw_add(n3360, n3298);
    let n3363: ZW = zw_add(n3361, n3301);
    let n3364: ZW = zw_add(n3362, n3302);
    let n3365: ZW = zw_add(n3363, n3305);
    let n3366: ZW = zw_add(n3364, n3306);
    let n3367: ZW = zw_add(n3365, n3309);
    let n3368: ZW = zw_add(n3366, n3310);
    let n3369: ZW = zw_add(n3367, n3313);
    let n3370: ZW = zw_add(n3368, n3314);
    let n3371: ZW = zw_cellmix_n(320u64, n2091, 1542469173u64);
    let n3372: ZW = zw_cellmix_n(320u64, n2091, 668265263u64);
    let n3373: ZW = zw_add(n3369, n3371);
    let n3374: ZW = zw_add(n3370, n3372);
    let n3375: ZW = zw_cellmix_n(321u64, n2089, 1542469173u64);
    let n3376: ZW = zw_cellmix_n(321u64, n2089, 668265263u64);
    let n3377: ZW = zw_add(n3373, n3375);
    let n3378: ZW = zw_add(n3374, n3376);
    let n3379: ZW = zw_add(n3367, n3325);
    let n3380: ZW = zw_add(n3368, n3326);
    let n3381: ZW = zw_cellmix_n(320u64, n2099, 1542469173u64);
    let n3382: ZW = zw_cellmix_n(320u64, n2099, 668265263u64);
    let n3383: ZW = zw_add(n3379, n3381);
    let n3384: ZW = zw_add(n3380, n3382);
    let n3385: ZW = zw_cellmix_n(321u64, n2097, 1542469173u64);
    let n3386: ZW = zw_cellmix_n(321u64, n2097, 668265263u64);
    let n3387: ZW = zw_add(n3383, n3385);
    let n3388: ZW = zw_add(n3384, n3386);
    let n3389: ZW = zw_add(n3367, n3337);
    let n3390: ZW = zw_add(n3368, n3338);
    let n3391: ZW = zw_cellmix_n(320u64, n2107, 1542469173u64);
    let n3392: ZW = zw_cellmix_n(320u64, n2107, 668265263u64);
    let n3393: ZW = zw_add(n3389, n3391);
    let n3394: ZW = zw_add(n3390, n3392);
    let n3395: ZW = zw_cellmix_n(321u64, n2105, 1542469173u64);
    let n3396: ZW = zw_cellmix_n(321u64, n2105, 668265263u64);
    let n3397: ZW = zw_add(n3393, n3395);
    let n3398: ZW = zw_add(n3394, n3396);
    let n3399: ZW = zw_cellmix_n(20u64, n2127, 1542469173u64);
    let n3400: ZW = zw_cellmix_n(20u64, n2127, 668265263u64);
    let n3401: ZW = zw_add(n3261, n3399);
    let n3402: ZW = zw_add(n3262, n3400);
    let n3403: ZW = zw_cellmix_b(41u64, n2128, 1542469173u64);
    let n3404: ZW = zw_cellmix_b(41u64, n2128, 668265263u64);
    let n3405: ZW = zw_add(n3401, n3403);
    let n3406: ZW = zw_add(n3402, n3404);
    let n3407: ZW = zw_cellmix_n(253u64, n2129, 1542469173u64);
    let n3408: ZW = zw_cellmix_n(253u64, n2129, 668265263u64);
    let n3409: ZW = zw_add(n3405, n3407);
    let n3410: ZW = zw_add(n3406, n3408);
    let n3411: ZW = zw_cellmix_n(255u64, n2130, 1542469173u64);
    let n3412: ZW = zw_cellmix_n(255u64, n2130, 668265263u64);
    let n3413: ZW = zw_add(n3409, n3411);
    let n3414: ZW = zw_add(n3410, n3412);
    let n3415: ZW = zw_cellmix_n(256u64, n2131, 1542469173u64);
    let n3416: ZW = zw_cellmix_n(256u64, n2131, 668265263u64);
    let n3417: ZW = zw_add(n3413, n3415);
    let n3418: ZW = zw_add(n3414, n3416);
    let n3419: ZW = zw_add(n3417, n3281);
    let n3420: ZW = zw_add(n3418, n3282);
    let n3421: ZW = zw_cellmix_b(265u64, n2108, 1542469173u64);
    let n3422: ZW = zw_cellmix_b(265u64, n2108, 668265263u64);
    let n3423: ZW = zw_add(n3419, n3421);
    let n3424: ZW = zw_add(n3420, n3422);
    let n3425: ZW = zw_add(n3423, n3289);
    let n3426: ZW = zw_add(n3424, n3290);
    let n3427: ZW = zw_cellmix_n(272u64, n2140, 1542469173u64);
    let n3428: ZW = zw_cellmix_n(272u64, n2140, 668265263u64);
    let n3429: ZW = zw_add(n3425, n3427);
    let n3430: ZW = zw_add(n3426, n3428);
    let n3431: ZW = zw_cellmix_n(308u64, n2132, 1542469173u64);
    let n3432: ZW = zw_cellmix_n(308u64, n2132, 668265263u64);
    let n3433: ZW = zw_add(n3429, n3431);
    let n3434: ZW = zw_add(n3430, n3432);
    let n3435: ZW = zw_cellmix_n(309u64, n2133, 1542469173u64);
    let n3436: ZW = zw_cellmix_n(309u64, n2133, 668265263u64);
    let n3437: ZW = zw_add(n3433, n3435);
    let n3438: ZW = zw_add(n3434, n3436);
    let n3439: ZW = zw_cellmix_n(310u64, n2134, 1542469173u64);
    let n3440: ZW = zw_cellmix_n(310u64, n2134, 668265263u64);
    let n3441: ZW = zw_add(n3437, n3439);
    let n3442: ZW = zw_add(n3438, n3440);
    let n3443: ZW = zw_cellmix_n(311u64, n2135, 1542469173u64);
    let n3444: ZW = zw_cellmix_n(311u64, n2135, 668265263u64);
    let n3445: ZW = zw_add(n3441, n3443);
    let n3446: ZW = zw_add(n3442, n3444);
    let n3447: ZW = zw_add(n3445, n3313);
    let n3448: ZW = zw_add(n3446, n3314);
    let n3449: ZW = zw_cellmix_n(320u64, n2141, 1542469173u64);
    let n3450: ZW = zw_cellmix_n(320u64, n2141, 668265263u64);
    let n3451: ZW = zw_add(n3447, n3449);
    let n3452: ZW = zw_add(n3448, n3450);
    let n3453: ZW = zw_cellmix_n(321u64, n2137, 1542469173u64);
    let n3454: ZW = zw_cellmix_n(321u64, n2137, 668265263u64);
    let n3455: ZW = zw_add(n3451, n3453);
    let n3456: ZW = zw_add(n3452, n3454);
    let n3457: ZW = zw_cellmix_n(309u64, n2150, 1542469173u64);
    let n3458: ZW = zw_cellmix_n(309u64, n2150, 668265263u64);
    let n3459: ZW = zw_add(n3433, n3457);
    let n3460: ZW = zw_add(n3434, n3458);
    let n3461: ZW = zw_cellmix_n(310u64, n2151, 1542469173u64);
    let n3462: ZW = zw_cellmix_n(310u64, n2151, 668265263u64);
    let n3463: ZW = zw_add(n3459, n3461);
    let n3464: ZW = zw_add(n3460, n3462);
    let n3465: ZW = zw_add(n3463, n3443);
    let n3466: ZW = zw_add(n3464, n3444);
    let n3467: ZW = zw_add(n3465, n3325);
    let n3468: ZW = zw_add(n3466, n3326);
    let n3469: ZW = zw_cellmix_n(320u64, n2155, 1542469173u64);
    let n3470: ZW = zw_cellmix_n(320u64, n2155, 668265263u64);
    let n3471: ZW = zw_add(n3467, n3469);
    let n3472: ZW = zw_add(n3468, n3470);
    let n3473: ZW = zw_cellmix_n(321u64, n2153, 1542469173u64);
    let n3474: ZW = zw_cellmix_n(321u64, n2153, 668265263u64);
    let n3475: ZW = zw_add(n3471, n3473);
    let n3476: ZW = zw_add(n3472, n3474);
    let n3477: ZW = zw_cellmix_n(310u64, n2162, 1542469173u64);
    let n3478: ZW = zw_cellmix_n(310u64, n2162, 668265263u64);
    let n3479: ZW = zw_add(n3459, n3477);
    let n3480: ZW = zw_add(n3460, n3478);
    let n3481: ZW = zw_add(n3479, n3443);
    let n3482: ZW = zw_add(n3480, n3444);
    let n3483: ZW = zw_add(n3481, n3337);
    let n3484: ZW = zw_add(n3482, n3338);
    let n3485: ZW = zw_cellmix_n(320u64, n2166, 1542469173u64);
    let n3486: ZW = zw_cellmix_n(320u64, n2166, 668265263u64);
    let n3487: ZW = zw_add(n3483, n3485);
    let n3488: ZW = zw_add(n3484, n3486);
    let n3489: ZW = zw_cellmix_n(321u64, n2164, 1542469173u64);
    let n3490: ZW = zw_cellmix_n(321u64, n2164, 668265263u64);
    let n3491: ZW = zw_add(n3487, n3489);
    let n3492: ZW = zw_add(n3488, n3490);
    let n3493: ZW = zw_cellmix_n(308u64, n2179, 1542469173u64);
    let n3494: ZW = zw_cellmix_n(308u64, n2179, 668265263u64);
    let n3495: ZW = zw_add(n3429, n3493);
    let n3496: ZW = zw_add(n3430, n3494);
    let n3497: ZW = zw_cellmix_n(309u64, n2180, 1542469173u64);
    let n3498: ZW = zw_cellmix_n(309u64, n2180, 668265263u64);
    let n3499: ZW = zw_add(n3495, n3497);
    let n3500: ZW = zw_add(n3496, n3498);
    let n3501: ZW = zw_cellmix_n(310u64, n2181, 1542469173u64);
    let n3502: ZW = zw_cellmix_n(310u64, n2181, 668265263u64);
    let n3503: ZW = zw_add(n3499, n3501);
    let n3504: ZW = zw_add(n3500, n3502);
    let n3505: ZW = zw_cellmix_n(311u64, n2182, 1542469173u64);
    let n3506: ZW = zw_cellmix_n(311u64, n2182, 668265263u64);
    let n3507: ZW = zw_add(n3503, n3505);
    let n3508: ZW = zw_add(n3504, n3506);
    let n3509: ZW = zw_add(n3507, n3313);
    let n3510: ZW = zw_add(n3508, n3314);
    let n3511: ZW = zw_cellmix_n(320u64, n2186, 1542469173u64);
    let n3512: ZW = zw_cellmix_n(320u64, n2186, 668265263u64);
    let n3513: ZW = zw_add(n3509, n3511);
    let n3514: ZW = zw_add(n3510, n3512);
    let n3515: ZW = zw_cellmix_n(321u64, n2184, 1542469173u64);
    let n3516: ZW = zw_cellmix_n(321u64, n2184, 668265263u64);
    let n3517: ZW = zw_add(n3513, n3515);
    let n3518: ZW = zw_add(n3514, n3516);
    let n3519: ZW = zw_add(n3495, n3457);
    let n3520: ZW = zw_add(n3496, n3458);
    let n3521: ZW = zw_add(n3519, n3461);
    let n3522: ZW = zw_add(n3520, n3462);
    let n3523: ZW = zw_add(n3521, n3505);
    let n3524: ZW = zw_add(n3522, n3506);
    let n3525: ZW = zw_add(n3523, n3325);
    let n3526: ZW = zw_add(n3524, n3326);
    let n3527: ZW = zw_cellmix_n(320u64, n2194, 1542469173u64);
    let n3528: ZW = zw_cellmix_n(320u64, n2194, 668265263u64);
    let n3529: ZW = zw_add(n3525, n3527);
    let n3530: ZW = zw_add(n3526, n3528);
    let n3531: ZW = zw_cellmix_n(321u64, n2192, 1542469173u64);
    let n3532: ZW = zw_cellmix_n(321u64, n2192, 668265263u64);
    let n3533: ZW = zw_add(n3529, n3531);
    let n3534: ZW = zw_add(n3530, n3532);
    let n3535: ZW = zw_add(n3519, n3477);
    let n3536: ZW = zw_add(n3520, n3478);
    let n3537: ZW = zw_add(n3535, n3505);
    let n3538: ZW = zw_add(n3536, n3506);
    let n3539: ZW = zw_add(n3537, n3337);
    let n3540: ZW = zw_add(n3538, n3338);
    let n3541: ZW = zw_cellmix_n(320u64, n2202, 1542469173u64);
    let n3542: ZW = zw_cellmix_n(320u64, n2202, 668265263u64);
    let n3543: ZW = zw_add(n3539, n3541);
    let n3544: ZW = zw_add(n3540, n3542);
    let n3545: ZW = zw_cellmix_n(321u64, n2200, 1542469173u64);
    let n3546: ZW = zw_cellmix_n(321u64, n2200, 668265263u64);
    let n3547: ZW = zw_add(n3543, n3545);
    let n3548: ZW = zw_add(n3544, n3546);
    let n3549: ZW = zw_cellmix_n(311u64, n2207, 1542469173u64);
    let n3550: ZW = zw_cellmix_n(311u64, n2207, 668265263u64);
    let n3551: ZW = zw_add(n3503, n3549);
    let n3552: ZW = zw_add(n3504, n3550);
    let n3553: ZW = zw_add(n3551, n3313);
    let n3554: ZW = zw_add(n3552, n3314);
    let n3555: ZW = zw_add(n3553, n3511);
    let n3556: ZW = zw_add(n3554, n3512);
    let n3557: ZW = zw_cellmix_n(321u64, n2208, 1542469173u64);
    let n3558: ZW = zw_cellmix_n(321u64, n2208, 668265263u64);
    let n3559: ZW = zw_add(n3555, n3557);
    let n3560: ZW = zw_add(n3556, n3558);
    let n3561: ZW = zw_add(n3521, n3549);
    let n3562: ZW = zw_add(n3522, n3550);
    let n3563: ZW = zw_add(n3561, n3325);
    let n3564: ZW = zw_add(n3562, n3326);
    let n3565: ZW = zw_add(n3563, n3527);
    let n3566: ZW = zw_add(n3564, n3528);
    let n3567: ZW = zw_cellmix_n(321u64, n2211, 1542469173u64);
    let n3568: ZW = zw_cellmix_n(321u64, n2211, 668265263u64);
    let n3569: ZW = zw_add(n3565, n3567);
    let n3570: ZW = zw_add(n3566, n3568);
    let n3571: ZW = zw_add(n3535, n3549);
    let n3572: ZW = zw_add(n3536, n3550);
    let n3573: ZW = zw_add(n3571, n3337);
    let n3574: ZW = zw_add(n3572, n3338);
    let n3575: ZW = zw_add(n3573, n3541);
    let n3576: ZW = zw_add(n3574, n3542);
    let n3577: ZW = zw_cellmix_n(321u64, n2214, 1542469173u64);
    let n3578: ZW = zw_cellmix_n(321u64, n2214, 668265263u64);
    let n3579: ZW = zw_add(n3575, n3577);
    let n3580: ZW = zw_add(n3576, n3578);
    let n3581: ZW = zw_add(n3417, n3349);
    let n3582: ZW = zw_add(n3418, n3350);
    let n3583: ZW = zw_add(n3581, n3421);
    let n3584: ZW = zw_add(n3582, n3422);
    let n3585: ZW = zw_add(n3583, n3355);
    let n3586: ZW = zw_add(n3584, n3356);
    let n3587: ZW = zw_add(n3585, n3427);
    let n3588: ZW = zw_add(n3586, n3428);
    let n3589: ZW = zw_add(n3587, n3431);
    let n3590: ZW = zw_add(n3588, n3432);
    let n3591: ZW = zw_add(n3589, n3435);
    let n3592: ZW = zw_add(n3590, n3436);
    let n3593: ZW = zw_add(n3591, n3439);
    let n3594: ZW = zw_add(n3592, n3440);
    let n3595: ZW = zw_add(n3593, n3443);
    let n3596: ZW = zw_add(n3594, n3444);
    let n3597: ZW = zw_add(n3595, n3313);
    let n3598: ZW = zw_add(n3596, n3314);
    let n3599: ZW = zw_cellmix_n(320u64, n2222, 1542469173u64);
    let n3600: ZW = zw_cellmix_n(320u64, n2222, 668265263u64);
    let n3601: ZW = zw_add(n3597, n3599);
    let n3602: ZW = zw_add(n3598, n3600);
    let n3603: ZW = zw_cellmix_n(321u64, n2220, 1542469173u64);
    let n3604: ZW = zw_cellmix_n(321u64, n2220, 668265263u64);
    let n3605: ZW = zw_add(n3601, n3603);
    let n3606: ZW = zw_add(n3602, n3604);
    let n3607: ZW = zw_add(n3589, n3457);
    let n3608: ZW = zw_add(n3590, n3458);
    let n3609: ZW = zw_add(n3607, n3461);
    let n3610: ZW = zw_add(n3608, n3462);
    let n3611: ZW = zw_add(n3609, n3443);
    let n3612: ZW = zw_add(n3610, n3444);
    let n3613: ZW = zw_add(n3611, n3325);
    let n3614: ZW = zw_add(n3612, n3326);
    let n3615: ZW = zw_cellmix_n(320u64, n2230, 1542469173u64);
    let n3616: ZW = zw_cellmix_n(320u64, n2230, 668265263u64);
    let n3617: ZW = zw_add(n3613, n3615);
    let n3618: ZW = zw_add(n3614, n3616);
    let n3619: ZW = zw_cellmix_n(321u64, n2228, 1542469173u64);
    let n3620: ZW = zw_cellmix_n(321u64, n2228, 668265263u64);
    let n3621: ZW = zw_add(n3617, n3619);
    let n3622: ZW = zw_add(n3618, n3620);
    let n3623: ZW = zw_add(n3607, n3477);
    let n3624: ZW = zw_add(n3608, n3478);
    let n3625: ZW = zw_add(n3623, n3443);
    let n3626: ZW = zw_add(n3624, n3444);
    let n3627: ZW = zw_add(n3625, n3337);
    let n3628: ZW = zw_add(n3626, n3338);
    let n3629: ZW = zw_cellmix_n(320u64, n2238, 1542469173u64);
    let n3630: ZW = zw_cellmix_n(320u64, n2238, 668265263u64);
    let n3631: ZW = zw_add(n3627, n3629);
    let n3632: ZW = zw_add(n3628, n3630);
    let n3633: ZW = zw_cellmix_n(321u64, n2236, 1542469173u64);
    let n3634: ZW = zw_cellmix_n(321u64, n2236, 668265263u64);
    let n3635: ZW = zw_add(n3631, n3633);
    let n3636: ZW = zw_add(n3632, n3634);
    let n3637: ZW = zw_add(n3587, n3493);
    let n3638: ZW = zw_add(n3588, n3494);
    let n3639: ZW = zw_add(n3637, n3497);
    let n3640: ZW = zw_add(n3638, n3498);
    let n3641: ZW = zw_add(n3639, n3501);
    let n3642: ZW = zw_add(n3640, n3502);
    let n3643: ZW = zw_add(n3641, n3505);
    let n3644: ZW = zw_add(n3642, n3506);
    let n3645: ZW = zw_add(n3643, n3313);
    let n3646: ZW = zw_add(n3644, n3314);
    let n3647: ZW = zw_cellmix_n(320u64, n2246, 1542469173u64);
    let n3648: ZW = zw_cellmix_n(320u64, n2246, 668265263u64);
    let n3649: ZW = zw_add(n3645, n3647);
    let n3650: ZW = zw_add(n3646, n3648);
    let n3651: ZW = zw_cellmix_n(321u64, n2244, 1542469173u64);
    let n3652: ZW = zw_cellmix_n(321u64, n2244, 668265263u64);
    let n3653: ZW = zw_add(n3649, n3651);
    let n3654: ZW = zw_add(n3650, n3652);
    let n3655: ZW = zw_add(n3637, n3457);
    let n3656: ZW = zw_add(n3638, n3458);
    let n3657: ZW = zw_add(n3655, n3461);
    let n3658: ZW = zw_add(n3656, n3462);
    let n3659: ZW = zw_add(n3657, n3505);
    let n3660: ZW = zw_add(n3658, n3506);
    let n3661: ZW = zw_add(n3659, n3325);
    let n3662: ZW = zw_add(n3660, n3326);
    let n3663: ZW = zw_cellmix_n(320u64, n2254, 1542469173u64);
    let n3664: ZW = zw_cellmix_n(320u64, n2254, 668265263u64);
    let n3665: ZW = zw_add(n3661, n3663);
    let n3666: ZW = zw_add(n3662, n3664);
    let n3667: ZW = zw_cellmix_n(321u64, n2252, 1542469173u64);
    let n3668: ZW = zw_cellmix_n(321u64, n2252, 668265263u64);
    let n3669: ZW = zw_add(n3665, n3667);
    let n3670: ZW = zw_add(n3666, n3668);
    let n3671: ZW = zw_add(n3655, n3477);
    let n3672: ZW = zw_add(n3656, n3478);
    let n3673: ZW = zw_add(n3671, n3505);
    let n3674: ZW = zw_add(n3672, n3506);
    let n3675: ZW = zw_add(n3673, n3337);
    let n3676: ZW = zw_add(n3674, n3338);
    let n3677: ZW = zw_cellmix_n(320u64, n2262, 1542469173u64);
    let n3678: ZW = zw_cellmix_n(320u64, n2262, 668265263u64);
    let n3679: ZW = zw_add(n3675, n3677);
    let n3680: ZW = zw_add(n3676, n3678);
    let n3681: ZW = zw_cellmix_n(321u64, n2260, 1542469173u64);
    let n3682: ZW = zw_cellmix_n(321u64, n2260, 668265263u64);
    let n3683: ZW = zw_add(n3679, n3681);
    let n3684: ZW = zw_add(n3680, n3682);
    let n3685: ZW = zw_add(n3641, n3549);
    let n3686: ZW = zw_add(n3642, n3550);
    let n3687: ZW = zw_add(n3685, n3313);
    let n3688: ZW = zw_add(n3686, n3314);
    let n3689: ZW = zw_add(n3687, n3647);
    let n3690: ZW = zw_add(n3688, n3648);
    let n3691: ZW = zw_cellmix_n(321u64, n2265, 1542469173u64);
    let n3692: ZW = zw_cellmix_n(321u64, n2265, 668265263u64);
    let n3693: ZW = zw_add(n3689, n3691);
    let n3694: ZW = zw_add(n3690, n3692);
    let n3695: ZW = zw_add(n3657, n3549);
    let n3696: ZW = zw_add(n3658, n3550);
    let n3697: ZW = zw_add(n3695, n3325);
    let n3698: ZW = zw_add(n3696, n3326);
    let n3699: ZW = zw_add(n3697, n3663);
    let n3700: ZW = zw_add(n3698, n3664);
    let n3701: ZW = zw_cellmix_n(321u64, n2268, 1542469173u64);
    let n3702: ZW = zw_cellmix_n(321u64, n2268, 668265263u64);
    let n3703: ZW = zw_add(n3699, n3701);
    let n3704: ZW = zw_add(n3700, n3702);
    let n3705: ZW = zw_add(n3671, n3549);
    let n3706: ZW = zw_add(n3672, n3550);
    let n3707: ZW = zw_add(n3705, n3337);
    let n3708: ZW = zw_add(n3706, n3338);
    let n3709: ZW = zw_add(n3707, n3677);
    let n3710: ZW = zw_add(n3708, n3678);
    let n3711: ZW = zw_cellmix_n(321u64, n2271, 1542469173u64);
    let n3712: ZW = zw_cellmix_n(321u64, n2271, 668265263u64);
    let n3713: ZW = zw_add(n3709, n3711);
    let n3714: ZW = zw_add(n3710, n3712);
    let ok_v0_b0: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v0_b0: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v0_b0: u16 = ALL & zb_holds(n804) & zb_holds(n802) & zb_holds(n800) & zb_holds(n798) & zb_holds(n111) & zb_holds(n723) & zb_holds(n725) & zb_holds(n795);
    let ok_v1_b1: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v1_b1: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v1_b1: u16 = ALL & zb_holds(n804) & zb_holds(n802) & zb_holds(n800) & zb_holds(n798) & zb_holds(n111) & zb_holds(n723) & zb_holds(n725) & zb_holds(n795);
    let ok_v2_b2: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v2_b2: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v2_b2: u16 = ALL & zb_holds(n804) & zb_holds(n802) & zb_holds(n800) & zb_holds(n798) & zb_holds(n111) & zb_holds(n723) & zb_holds(n725) & zb_holds(n795);
    let ok_v16_b3: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v16_b3: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v16_b3: u16 = ALL & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n804) & zb_holds(n802) & zb_holds(n800) & zb_holds(n111) & zb_holds(n798);
    let ok_v17_b4: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v17_b4: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v17_b4: u16 = ALL & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n804) & zb_holds(n802) & zb_holds(n800) & zb_holds(n111) & zb_holds(n798);
    let ok_v18_b5: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v18_b5: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v18_b5: u16 = ALL & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n804) & zb_holds(n802) & zb_holds(n800) & zb_holds(n111) & zb_holds(n798);
    let ok_v32_b6: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v32_b6: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v32_b6: u16 = ALL & zb_holds(n804) & zb_holds(n802) & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n798) & zb_holds(n800);
    let ok_v33_b7: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v33_b7: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v33_b7: u16 = ALL & zb_holds(n804) & zb_holds(n802) & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n798) & zb_holds(n800);
    let ok_v34_b8: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v34_b8: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v34_b8: u16 = ALL & zb_holds(n804) & zb_holds(n802) & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n798) & zb_holds(n800);
    let ok_v36_b9: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v36_b9: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v36_b9: u16 = ALL & zb_holds(n804) & zb_holds(n802) & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n798) & zb_holds(n800);
    let ok_v37_b10: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v37_b10: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v37_b10: u16 = ALL & zb_holds(n804) & zb_holds(n802) & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n798) & zb_holds(n800);
    let ok_v38_b11: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v38_b11: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v38_b11: u16 = ALL & zb_holds(n804) & zb_holds(n802) & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n798) & zb_holds(n800);
    let ok_v40_b12: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v40_b12: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v40_b12: u16 = ALL & zb_holds(n804) & zb_holds(n802) & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n798) & zb_holds(n800);
    let ok_v41_b13: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v41_b13: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v41_b13: u16 = ALL & zb_holds(n804) & zb_holds(n802) & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n798) & zb_holds(n800);
    let ok_v42_b14: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v42_b14: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v42_b14: u16 = ALL & zb_holds(n804) & zb_holds(n802) & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n798) & zb_holds(n800);
    let ok_v48_b15: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v48_b15: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v48_b15: u16 = ALL & zb_holds(n804) & zb_holds(n802) & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n798) & zb_holds(n800);
    let ok_v49_b16: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v49_b16: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v49_b16: u16 = ALL & zb_holds(n804) & zb_holds(n802) & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n798) & zb_holds(n800);
    let ok_v50_b17: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v50_b17: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v50_b17: u16 = ALL & zb_holds(n804) & zb_holds(n802) & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n798) & zb_holds(n800);
    let ok_v52_b18: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v52_b18: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v52_b18: u16 = ALL & zb_holds(n804) & zb_holds(n802) & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n798) & zb_holds(n800);
    let ok_v53_b19: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v53_b19: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v53_b19: u16 = ALL & zb_holds(n804) & zb_holds(n802) & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n798) & zb_holds(n800);
    let ok_v54_b20: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v54_b20: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v54_b20: u16 = ALL & zb_holds(n804) & zb_holds(n802) & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n798) & zb_holds(n800);
    let ok_v56_b21: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v56_b21: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v56_b21: u16 = ALL & zb_holds(n804) & zb_holds(n802) & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n798) & zb_holds(n800);
    let ok_v57_b22: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v57_b22: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v57_b22: u16 = ALL & zb_holds(n804) & zb_holds(n802) & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n798) & zb_holds(n800);
    let ok_v58_b23: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v58_b23: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v58_b23: u16 = ALL & zb_holds(n804) & zb_holds(n802) & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n798) & zb_holds(n800);
    let ok_v0_b24: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v0_b24: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v0_b24: u16 = ALL & zb_holds(n111) & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n967);
    let ok_v1_b25: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v1_b25: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v1_b25: u16 = ALL & zb_holds(n111) & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n967);
    let ok_v2_b26: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v2_b26: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v2_b26: u16 = ALL & zb_holds(n111) & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n967);
    let ok_v16_b27: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v16_b27: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v16_b27: u16 = ALL & zb_holds(n111) & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n967);
    let ok_v17_b28: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v17_b28: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v17_b28: u16 = ALL & zb_holds(n111) & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n967);
    let ok_v18_b29: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v18_b29: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v18_b29: u16 = ALL & zb_holds(n111) & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n967);
    let ok_v32_b30: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v32_b30: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v32_b30: u16 = ALL & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n967);
    let ok_v33_b31: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v33_b31: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v33_b31: u16 = ALL & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n967);
    let ok_v34_b32: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v34_b32: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v34_b32: u16 = ALL & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n967);
    let ok_v36_b33: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v36_b33: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v36_b33: u16 = ALL & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n967);
    let ok_v37_b34: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v37_b34: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v37_b34: u16 = ALL & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n967);
    let ok_v38_b35: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v38_b35: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v38_b35: u16 = ALL & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n967);
    let ok_v40_b36: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v40_b36: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v40_b36: u16 = ALL & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n967);
    let ok_v41_b37: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v41_b37: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v41_b37: u16 = ALL & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n967);
    let ok_v42_b38: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v42_b38: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v42_b38: u16 = ALL & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n967);
    let ok_v48_b39: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v48_b39: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v48_b39: u16 = ALL & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n967);
    let ok_v49_b40: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v49_b40: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v49_b40: u16 = ALL & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n967);
    let ok_v50_b41: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v50_b41: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v50_b41: u16 = ALL & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n967);
    let ok_v52_b42: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v52_b42: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v52_b42: u16 = ALL & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n967);
    let ok_v53_b43: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v53_b43: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v53_b43: u16 = ALL & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n967);
    let ok_v54_b44: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v54_b44: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v54_b44: u16 = ALL & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n967);
    let ok_v56_b45: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v56_b45: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v56_b45: u16 = ALL & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n967);
    let ok_v57_b46: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v57_b46: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v57_b46: u16 = ALL & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n967);
    let ok_v58_b47: u16 = ALL & zb_holds(n724) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v58_b47: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v58_b47: u16 = ALL & zb_holds(n723) & zb_holds(n725) & zb_holds(n795) & zb_holds(n967);
    let ok_v0_b48: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n1038);
    let bd_v0_b48: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v0_b48: u16 = ALL & zb_holds(n795) & zb_holds(n1037);
    let ok_v32_b49: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n1038);
    let bd_v32_b49: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v32_b49: u16 = ALL & zb_holds(n795) & zb_holds(n1037);
    let ok_v0_b50: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n1791);
    let bd_v0_b50: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v0_b50: u16 = ALL & zb_holds(n1790) & zb_holds(n1877);
    let ok_v32_b51: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n1791);
    let bd_v32_b51: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v32_b51: u16 = ALL & zb_holds(n1790) & zb_holds(n1877);
    let ok_v0_b52: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n1942);
    let bd_v0_b52: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v0_b52: u16 = ALL & zb_holds(n1941);
    let ok_v32_b53: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n1942);
    let bd_v32_b53: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v32_b53: u16 = ALL & zb_holds(n1941);
    let ok_v0_b54: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2036);
    let bd_v0_b54: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v0_b54: u16 = ALL & zb_holds(n2035);
    let ok_v1_b55: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2036);
    let bd_v1_b55: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v1_b55: u16 = ALL & zb_holds(n2035);
    let ok_v2_b56: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2036);
    let bd_v2_b56: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v2_b56: u16 = ALL & zb_holds(n2035);
    let ok_v16_b57: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2036);
    let bd_v16_b57: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v16_b57: u16 = ALL & zb_holds(n2035);
    let ok_v17_b58: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2036);
    let bd_v17_b58: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v17_b58: u16 = ALL & zb_holds(n2035);
    let ok_v18_b59: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2036);
    let bd_v18_b59: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v18_b59: u16 = ALL & zb_holds(n2035);
    let ok_v32_b60: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2036);
    let bd_v32_b60: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v32_b60: u16 = ALL & zb_holds(n2035);
    let ok_v33_b61: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2036);
    let bd_v33_b61: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v33_b61: u16 = ALL & zb_holds(n2035);
    let ok_v34_b62: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2036);
    let bd_v34_b62: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v34_b62: u16 = ALL & zb_holds(n2035);
    let ok_v36_b63: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2036);
    let bd_v36_b63: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v36_b63: u16 = ALL & zb_holds(n2035);
    let ok_v37_b64: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2036);
    let bd_v37_b64: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v37_b64: u16 = ALL & zb_holds(n2035);
    let ok_v38_b65: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2036);
    let bd_v38_b65: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v38_b65: u16 = ALL & zb_holds(n2035);
    let ok_v40_b66: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2036);
    let bd_v40_b66: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v40_b66: u16 = ALL & zb_holds(n2035);
    let ok_v41_b67: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2036);
    let bd_v41_b67: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v41_b67: u16 = ALL & zb_holds(n2035);
    let ok_v42_b68: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2036);
    let bd_v42_b68: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v42_b68: u16 = ALL & zb_holds(n2035);
    let ok_v48_b69: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2036);
    let bd_v48_b69: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v48_b69: u16 = ALL & zb_holds(n2035);
    let ok_v49_b70: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2036);
    let bd_v49_b70: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v49_b70: u16 = ALL & zb_holds(n2035);
    let ok_v50_b71: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2036);
    let bd_v50_b71: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v50_b71: u16 = ALL & zb_holds(n2035);
    let ok_v52_b72: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2036);
    let bd_v52_b72: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v52_b72: u16 = ALL & zb_holds(n2035);
    let ok_v53_b73: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2036);
    let bd_v53_b73: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v53_b73: u16 = ALL & zb_holds(n2035);
    let ok_v54_b74: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2036);
    let bd_v54_b74: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v54_b74: u16 = ALL & zb_holds(n2035);
    let ok_v56_b75: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2036);
    let bd_v56_b75: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v56_b75: u16 = ALL & zb_holds(n2035);
    let ok_v57_b76: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2036);
    let bd_v57_b76: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v57_b76: u16 = ALL & zb_holds(n2035);
    let ok_v58_b77: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2036);
    let bd_v58_b77: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v58_b77: u16 = ALL & zb_holds(n2035);
    let sh0 = KShared0 {
        c87: r_c87,
        c39: r_c39,
        c84: n90,
        c86: n144,
        c280: n326,
        c281: n397,
        c256: n396,
        c85: n145,
    };
    let sh1 = KShared1 {
        c87: r_c87,
        c39: r_c39,
        c84: n90,
        c86: n144,
        c310: n326,
        c311: n397,
        c256: n396,
        c85: n145,
    };
    let sh2 = KShared2 {
        c87: n1044,
        c84: n90,
        c86: n144,
        c85: n145,
    };
    let sh3 = KShared3 {
        c87: n1881,
        c84: n90,
        c86: n144,
        c85: n145,
    };
    let sh4 = KShared4 {
        c87: n1947,
        c39: n1948,
        c84: n90,
        c86: n144,
        c234: n1938,
        c246: n1939,
        c250: n1940,
        c85: n145,
        c38: n1937,
    };
    let sh5 = KShared5 {
        c87: r_c87,
        c39: r_c39,
        c84: n90,
        c86: n144,
        c318: n2031,
        c319: n2032,
        c273: n2029,
        c85: n145,
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
    // 78 distinct button assignments; per outcome they fall
    // into [24, 24, 2, 2, 2, 24] groups that write identical values.
    declined |= live_v0_b0 & (if bd_v0_b0 { ALL } else { !ok_v0_b0 });
    take_0_0 |= live_v0_b0 & ok_v0_b0 & (if bd_v0_b0 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c270: r_c308,
        c271: r_c309,
        c236: n223,
        c272: r_c310,
        c273: r_c311,
        c238: zn_splat(P8::from_raw(-65536i32)),
        c274: n761,
        c241: n731,
        c248: zb_splat(false),
        c249: zb_splat(false),
        c282: n820,
        c283: n768,
        c255: n819,
        h1: n2363, h2: n2364,
    };
    // body 0: buttons 0x00, forks 0x0
    sink.o0(0, take_0_0, &sh0, &o0);
    declined |= live_v1_b1 & (if bd_v1_b1 { ALL } else { !ok_v1_b1 });
    take_0_1 |= live_v1_b1 & ok_v1_b1 & (if bd_v1_b1 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c270: r_c308,
        c271: r_c309,
        c236: n223,
        c272: r_c310,
        c273: r_c311,
        c238: zn_splat(P8::from_raw(-65536i32)),
        c274: n840,
        c241: n731,
        c248: zb_splat(false),
        c249: zb_splat(false),
        c282: n851,
        c283: n845,
        c255: n819,
        h1: n2375, h2: n2376,
    };
    // body 1: buttons 0x01, forks 0x0
    sink.o0(1, take_0_1, &sh0, &o0);
    declined |= live_v2_b2 & (if bd_v2_b2 { ALL } else { !ok_v2_b2 });
    take_0_2 |= live_v2_b2 & ok_v2_b2 & (if bd_v2_b2 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c270: r_c308,
        c271: r_c309,
        c236: n223,
        c272: r_c310,
        c273: r_c311,
        c238: zn_splat(P8::from_raw(-65536i32)),
        c274: n859,
        c241: n731,
        c248: zb_splat(false),
        c249: zb_splat(false),
        c282: n870,
        c283: n864,
        c255: n819,
        h1: n2387, h2: n2388,
    };
    // body 2: buttons 0x02, forks 0x0
    sink.o0(2, take_0_2, &sh0, &o0);
    declined |= live_v16_b3 & (if bd_v16_b3 { ALL } else { !ok_v16_b3 });
    take_0_3 |= live_v16_b3 & ok_v16_b3 & (if bd_v16_b3 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c270: r_c308,
        c271: r_c309,
        c236: n223,
        c272: r_c310,
        c273: r_c311,
        c238: zn_splat(P8::from_raw(-65536i32)),
        c274: n761,
        c241: n871,
        c248: zb_splat(false),
        c249: zb_splat(true),
        c282: n880,
        c283: n873,
        c255: n819,
        h1: n2417, h2: n2418,
    };
    // body 3: buttons 0x10, forks 0x0
    sink.o0(16, take_0_3, &sh0, &o0);
    declined |= live_v17_b4 & (if bd_v17_b4 { ALL } else { !ok_v17_b4 });
    take_0_4 |= live_v17_b4 & ok_v17_b4 & (if bd_v17_b4 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c270: r_c308,
        c271: r_c309,
        c236: n223,
        c272: r_c310,
        c273: r_c311,
        c238: zn_splat(P8::from_raw(-65536i32)),
        c274: n840,
        c241: n871,
        c248: zb_splat(false),
        c249: zb_splat(true),
        c282: n884,
        c283: n882,
        c255: n819,
        h1: n2427, h2: n2428,
    };
    // body 4: buttons 0x11, forks 0x0
    sink.o0(17, take_0_4, &sh0, &o0);
    declined |= live_v18_b5 & (if bd_v18_b5 { ALL } else { !ok_v18_b5 });
    take_0_5 |= live_v18_b5 & ok_v18_b5 & (if bd_v18_b5 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c270: r_c308,
        c271: r_c309,
        c236: n223,
        c272: r_c310,
        c273: r_c311,
        c238: zn_splat(P8::from_raw(-65536i32)),
        c274: n859,
        c241: n871,
        c248: zb_splat(false),
        c249: zb_splat(true),
        c282: n888,
        c283: n886,
        c255: n819,
        h1: n2437, h2: n2438,
    };
    // body 5: buttons 0x12, forks 0x0
    sink.o0(18, take_0_5, &sh0, &o0);
    declined |= live_v32_b6 & (if bd_v32_b6 { ALL } else { !ok_v32_b6 });
    take_0_6 |= live_v32_b6 & ok_v32_b6 & (if bd_v32_b6 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n890,
        c41: n891,
        c270: n894,
        c271: n895,
        c236: n892,
        c272: n896,
        c273: n897,
        c238: n893,
        c274: n761,
        c241: n731,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n903,
        c283: n899,
        c255: n902,
        h1: n2491, h2: n2492,
    };
    // body 6: buttons 0x20, forks 0x0
    sink.o0(32, take_0_6, &sh0, &o0);
    declined |= live_v33_b7 & (if bd_v33_b7 { ALL } else { !ok_v33_b7 });
    take_0_7 |= live_v33_b7 & ok_v33_b7 & (if bd_v33_b7 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n890,
        c41: n891,
        c270: n894,
        c271: n907,
        c236: n892,
        c272: n908,
        c273: n897,
        c238: n893,
        c274: n840,
        c241: n731,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n912,
        c283: n910,
        c255: n902,
        h1: n2511, h2: n2512,
    };
    // body 7: buttons 0x21, forks 0x0
    sink.o0(33, take_0_7, &sh0, &o0);
    declined |= live_v34_b8 & (if bd_v34_b8 { ALL } else { !ok_v34_b8 });
    take_0_8 |= live_v34_b8 & ok_v34_b8 & (if bd_v34_b8 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n890,
        c41: n891,
        c270: n894,
        c271: n907,
        c236: n892,
        c272: n913,
        c273: n897,
        c238: n893,
        c274: n859,
        c241: n731,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n917,
        c283: n915,
        c255: n902,
        h1: n2527, h2: n2528,
    };
    // body 8: buttons 0x22, forks 0x0
    sink.o0(34, take_0_8, &sh0, &o0);
    declined |= live_v36_b9 & (if bd_v36_b9 { ALL } else { !ok_v36_b9 });
    take_0_9 |= live_v36_b9 & ok_v36_b9 & (if bd_v36_b9 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n890,
        c41: n891,
        c270: n918,
        c271: n919,
        c236: n892,
        c272: n920,
        c273: n921,
        c238: n893,
        c274: n761,
        c241: n731,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n925,
        c283: n923,
        c255: n902,
        h1: n2553, h2: n2554,
    };
    // body 9: buttons 0x24, forks 0x0
    sink.o0(36, take_0_9, &sh0, &o0);
    declined |= live_v37_b10 & (if bd_v37_b10 { ALL } else { !ok_v37_b10 });
    take_0_10 |= live_v37_b10 & ok_v37_b10 & (if bd_v37_b10 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n890,
        c41: n891,
        c270: n918,
        c271: n907,
        c236: n892,
        c272: n908,
        c273: n921,
        c238: n893,
        c274: n840,
        c241: n731,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n929,
        c283: n927,
        c255: n902,
        h1: n2569, h2: n2570,
    };
    // body 10: buttons 0x25, forks 0x0
    sink.o0(37, take_0_10, &sh0, &o0);
    declined |= live_v38_b11 & (if bd_v38_b11 { ALL } else { !ok_v38_b11 });
    take_0_11 |= live_v38_b11 & ok_v38_b11 & (if bd_v38_b11 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n890,
        c41: n891,
        c270: n918,
        c271: n907,
        c236: n892,
        c272: n913,
        c273: n921,
        c238: n893,
        c274: n859,
        c241: n731,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n933,
        c283: n931,
        c255: n902,
        h1: n2583, h2: n2584,
    };
    // body 11: buttons 0x26, forks 0x0
    sink.o0(38, take_0_11, &sh0, &o0);
    declined |= live_v40_b12 & (if bd_v40_b12 { ALL } else { !ok_v40_b12 });
    take_0_12 |= live_v40_b12 & ok_v40_b12 & (if bd_v40_b12 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n890,
        c41: n891,
        c270: n918,
        c271: n919,
        c236: n892,
        c272: n920,
        c273: n934,
        c238: n893,
        c274: n761,
        c241: n731,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n925,
        c283: n935,
        c255: n902,
        h1: n2595, h2: n2596,
    };
    // body 12: buttons 0x28, forks 0x0
    sink.o0(40, take_0_12, &sh0, &o0);
    declined |= live_v41_b13 & (if bd_v41_b13 { ALL } else { !ok_v41_b13 });
    take_0_13 |= live_v41_b13 & ok_v41_b13 & (if bd_v41_b13 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n890,
        c41: n891,
        c270: n918,
        c271: n907,
        c236: n892,
        c272: n908,
        c273: n934,
        c238: n893,
        c274: n840,
        c241: n731,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n929,
        c283: n936,
        c255: n902,
        h1: n2605, h2: n2606,
    };
    // body 13: buttons 0x29, forks 0x0
    sink.o0(41, take_0_13, &sh0, &o0);
    declined |= live_v42_b14 & (if bd_v42_b14 { ALL } else { !ok_v42_b14 });
    take_0_14 |= live_v42_b14 & ok_v42_b14 & (if bd_v42_b14 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n890,
        c41: n891,
        c270: n918,
        c271: n907,
        c236: n892,
        c272: n913,
        c273: n934,
        c238: n893,
        c274: n859,
        c241: n731,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n933,
        c283: n937,
        c255: n902,
        h1: n2615, h2: n2616,
    };
    // body 14: buttons 0x2a, forks 0x0
    sink.o0(42, take_0_14, &sh0, &o0);
    declined |= live_v48_b15 & (if bd_v48_b15 { ALL } else { !ok_v48_b15 });
    take_0_15 |= live_v48_b15 & ok_v48_b15 & (if bd_v48_b15 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n890,
        c41: n891,
        c270: n894,
        c271: n895,
        c236: n892,
        c272: n896,
        c273: n897,
        c238: n893,
        c274: n761,
        c241: n871,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n941,
        c283: n939,
        c255: n902,
        h1: n2641, h2: n2642,
    };
    // body 15: buttons 0x30, forks 0x0
    sink.o0(48, take_0_15, &sh0, &o0);
    declined |= live_v49_b16 & (if bd_v49_b16 { ALL } else { !ok_v49_b16 });
    take_0_16 |= live_v49_b16 & ok_v49_b16 & (if bd_v49_b16 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n890,
        c41: n891,
        c270: n894,
        c271: n907,
        c236: n892,
        c272: n908,
        c273: n897,
        c238: n893,
        c274: n840,
        c241: n871,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n945,
        c283: n943,
        c255: n902,
        h1: n2657, h2: n2658,
    };
    // body 16: buttons 0x31, forks 0x0
    sink.o0(49, take_0_16, &sh0, &o0);
    declined |= live_v50_b17 & (if bd_v50_b17 { ALL } else { !ok_v50_b17 });
    take_0_17 |= live_v50_b17 & ok_v50_b17 & (if bd_v50_b17 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n890,
        c41: n891,
        c270: n894,
        c271: n907,
        c236: n892,
        c272: n913,
        c273: n897,
        c238: n893,
        c274: n859,
        c241: n871,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n949,
        c283: n947,
        c255: n902,
        h1: n2671, h2: n2672,
    };
    // body 17: buttons 0x32, forks 0x0
    sink.o0(50, take_0_17, &sh0, &o0);
    declined |= live_v52_b18 & (if bd_v52_b18 { ALL } else { !ok_v52_b18 });
    take_0_18 |= live_v52_b18 & ok_v52_b18 & (if bd_v52_b18 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n890,
        c41: n891,
        c270: n918,
        c271: n919,
        c236: n892,
        c272: n920,
        c273: n921,
        c238: n893,
        c274: n761,
        c241: n871,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n953,
        c283: n951,
        c255: n902,
        h1: n2689, h2: n2690,
    };
    // body 18: buttons 0x34, forks 0x0
    sink.o0(52, take_0_18, &sh0, &o0);
    declined |= live_v53_b19 & (if bd_v53_b19 { ALL } else { !ok_v53_b19 });
    take_0_19 |= live_v53_b19 & ok_v53_b19 & (if bd_v53_b19 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n890,
        c41: n891,
        c270: n918,
        c271: n907,
        c236: n892,
        c272: n908,
        c273: n921,
        c238: n893,
        c274: n840,
        c241: n871,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n957,
        c283: n955,
        c255: n902,
        h1: n2705, h2: n2706,
    };
    // body 19: buttons 0x35, forks 0x0
    sink.o0(53, take_0_19, &sh0, &o0);
    declined |= live_v54_b20 & (if bd_v54_b20 { ALL } else { !ok_v54_b20 });
    take_0_20 |= live_v54_b20 & ok_v54_b20 & (if bd_v54_b20 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n890,
        c41: n891,
        c270: n918,
        c271: n907,
        c236: n892,
        c272: n913,
        c273: n921,
        c238: n893,
        c274: n859,
        c241: n871,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n961,
        c283: n959,
        c255: n902,
        h1: n2719, h2: n2720,
    };
    // body 20: buttons 0x36, forks 0x0
    sink.o0(54, take_0_20, &sh0, &o0);
    declined |= live_v56_b21 & (if bd_v56_b21 { ALL } else { !ok_v56_b21 });
    take_0_21 |= live_v56_b21 & ok_v56_b21 & (if bd_v56_b21 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n890,
        c41: n891,
        c270: n918,
        c271: n919,
        c236: n892,
        c272: n920,
        c273: n934,
        c238: n893,
        c274: n761,
        c241: n871,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n953,
        c283: n962,
        c255: n902,
        h1: n2729, h2: n2730,
    };
    // body 21: buttons 0x38, forks 0x0
    sink.o0(56, take_0_21, &sh0, &o0);
    declined |= live_v57_b22 & (if bd_v57_b22 { ALL } else { !ok_v57_b22 });
    take_0_22 |= live_v57_b22 & ok_v57_b22 & (if bd_v57_b22 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n890,
        c41: n891,
        c270: n918,
        c271: n907,
        c236: n892,
        c272: n908,
        c273: n934,
        c238: n893,
        c274: n840,
        c241: n871,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n957,
        c283: n963,
        c255: n902,
        h1: n2739, h2: n2740,
    };
    // body 22: buttons 0x39, forks 0x0
    sink.o0(57, take_0_22, &sh0, &o0);
    declined |= live_v58_b23 & (if bd_v58_b23 { ALL } else { !ok_v58_b23 });
    take_0_23 |= live_v58_b23 & ok_v58_b23 & (if bd_v58_b23 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n890,
        c41: n891,
        c270: n918,
        c271: n907,
        c236: n892,
        c272: n913,
        c273: n934,
        c238: n893,
        c274: n859,
        c241: n871,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n961,
        c283: n964,
        c255: n902,
        h1: n2749, h2: n2750,
    };
    // body 23: buttons 0x3a, forks 0x0
    sink.o0(58, take_0_23, &sh0, &o0);
    declined |= live_v0_b24 & (if bd_v0_b24 { ALL } else { !ok_v0_b24 });
    take_1_0 |= live_v0_b24 & ok_v0_b24 & (if bd_v0_b24 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: r_c41,
        c300: r_c308,
        c301: r_c309,
        c236: n223,
        c302: r_c310,
        c303: r_c311,
        c238: zn_splat(P8::from_raw(-65536i32)),
        c239: n730,
        c304: n761,
        c241: n731,
        c248: zb_splat(false),
        c249: zb_splat(false),
        c312: n820,
        c313: n768,
        c255: n819,
        h1: n2805, h2: n2806,
    };
    // body 24: buttons 0x00, forks 0x0
    sink.o1(0, take_1_0, &sh1, &o1);
    declined |= live_v1_b25 & (if bd_v1_b25 { ALL } else { !ok_v1_b25 });
    take_1_1 |= live_v1_b25 & ok_v1_b25 & (if bd_v1_b25 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: r_c41,
        c300: r_c308,
        c301: r_c309,
        c236: n223,
        c302: r_c310,
        c303: r_c311,
        c238: zn_splat(P8::from_raw(-65536i32)),
        c239: n730,
        c304: n840,
        c241: n731,
        c248: zb_splat(false),
        c249: zb_splat(false),
        c312: n851,
        c313: n845,
        c255: n819,
        h1: n2817, h2: n2818,
    };
    // body 25: buttons 0x01, forks 0x0
    sink.o1(1, take_1_1, &sh1, &o1);
    declined |= live_v2_b26 & (if bd_v2_b26 { ALL } else { !ok_v2_b26 });
    take_1_2 |= live_v2_b26 & ok_v2_b26 & (if bd_v2_b26 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: r_c41,
        c300: r_c308,
        c301: r_c309,
        c236: n223,
        c302: r_c310,
        c303: r_c311,
        c238: zn_splat(P8::from_raw(-65536i32)),
        c239: n730,
        c304: n859,
        c241: n731,
        c248: zb_splat(false),
        c249: zb_splat(false),
        c312: n870,
        c313: n864,
        c255: n819,
        h1: n2829, h2: n2830,
    };
    // body 26: buttons 0x02, forks 0x0
    sink.o1(2, take_1_2, &sh1, &o1);
    declined |= live_v16_b27 & (if bd_v16_b27 { ALL } else { !ok_v16_b27 });
    take_1_3 |= live_v16_b27 & ok_v16_b27 & (if bd_v16_b27 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: r_c41,
        c300: r_c308,
        c301: r_c309,
        c236: n223,
        c302: r_c310,
        c303: r_c311,
        c238: zn_splat(P8::from_raw(-65536i32)),
        c239: n730,
        c304: n761,
        c241: n871,
        c248: zb_splat(false),
        c249: zb_splat(true),
        c312: n880,
        c313: n873,
        c255: n819,
        h1: n2855, h2: n2856,
    };
    // body 27: buttons 0x10, forks 0x0
    sink.o1(16, take_1_3, &sh1, &o1);
    declined |= live_v17_b28 & (if bd_v17_b28 { ALL } else { !ok_v17_b28 });
    take_1_4 |= live_v17_b28 & ok_v17_b28 & (if bd_v17_b28 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: r_c41,
        c300: r_c308,
        c301: r_c309,
        c236: n223,
        c302: r_c310,
        c303: r_c311,
        c238: zn_splat(P8::from_raw(-65536i32)),
        c239: n730,
        c304: n840,
        c241: n871,
        c248: zb_splat(false),
        c249: zb_splat(true),
        c312: n884,
        c313: n882,
        c255: n819,
        h1: n2865, h2: n2866,
    };
    // body 28: buttons 0x11, forks 0x0
    sink.o1(17, take_1_4, &sh1, &o1);
    declined |= live_v18_b29 & (if bd_v18_b29 { ALL } else { !ok_v18_b29 });
    take_1_5 |= live_v18_b29 & ok_v18_b29 & (if bd_v18_b29 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: r_c41,
        c300: r_c308,
        c301: r_c309,
        c236: n223,
        c302: r_c310,
        c303: r_c311,
        c238: zn_splat(P8::from_raw(-65536i32)),
        c239: n730,
        c304: n859,
        c241: n871,
        c248: zb_splat(false),
        c249: zb_splat(true),
        c312: n888,
        c313: n886,
        c255: n819,
        h1: n2875, h2: n2876,
    };
    // body 29: buttons 0x12, forks 0x0
    sink.o1(18, take_1_5, &sh1, &o1);
    declined |= live_v32_b30 & (if bd_v32_b30 { ALL } else { !ok_v32_b30 });
    take_1_6 |= live_v32_b30 & ok_v32_b30 & (if bd_v32_b30 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n890,
        c41: n891,
        c300: n894,
        c301: n895,
        c236: n892,
        c302: n896,
        c303: n897,
        c238: n893,
        c239: n970,
        c304: n761,
        c241: n731,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c312: n903,
        c313: n899,
        c255: n902,
        h1: n2921, h2: n2922,
    };
    // body 30: buttons 0x20, forks 0x0
    sink.o1(32, take_1_6, &sh1, &o1);
    declined |= live_v33_b31 & (if bd_v33_b31 { ALL } else { !ok_v33_b31 });
    take_1_7 |= live_v33_b31 & ok_v33_b31 & (if bd_v33_b31 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n890,
        c41: n891,
        c300: n894,
        c301: n907,
        c236: n892,
        c302: n908,
        c303: n897,
        c238: n893,
        c239: n970,
        c304: n840,
        c241: n731,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c312: n912,
        c313: n910,
        c255: n902,
        h1: n2941, h2: n2942,
    };
    // body 31: buttons 0x21, forks 0x0
    sink.o1(33, take_1_7, &sh1, &o1);
    declined |= live_v34_b32 & (if bd_v34_b32 { ALL } else { !ok_v34_b32 });
    take_1_8 |= live_v34_b32 & ok_v34_b32 & (if bd_v34_b32 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n890,
        c41: n891,
        c300: n894,
        c301: n907,
        c236: n892,
        c302: n913,
        c303: n897,
        c238: n893,
        c239: n970,
        c304: n859,
        c241: n731,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c312: n917,
        c313: n915,
        c255: n902,
        h1: n2957, h2: n2958,
    };
    // body 32: buttons 0x22, forks 0x0
    sink.o1(34, take_1_8, &sh1, &o1);
    declined |= live_v36_b33 & (if bd_v36_b33 { ALL } else { !ok_v36_b33 });
    take_1_9 |= live_v36_b33 & ok_v36_b33 & (if bd_v36_b33 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n890,
        c41: n891,
        c300: n918,
        c301: n919,
        c236: n892,
        c302: n920,
        c303: n921,
        c238: n893,
        c239: n970,
        c304: n761,
        c241: n731,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c312: n925,
        c313: n923,
        c255: n902,
        h1: n2983, h2: n2984,
    };
    // body 33: buttons 0x24, forks 0x0
    sink.o1(36, take_1_9, &sh1, &o1);
    declined |= live_v37_b34 & (if bd_v37_b34 { ALL } else { !ok_v37_b34 });
    take_1_10 |= live_v37_b34 & ok_v37_b34 & (if bd_v37_b34 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n890,
        c41: n891,
        c300: n918,
        c301: n907,
        c236: n892,
        c302: n908,
        c303: n921,
        c238: n893,
        c239: n970,
        c304: n840,
        c241: n731,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c312: n929,
        c313: n927,
        c255: n902,
        h1: n2999, h2: n3000,
    };
    // body 34: buttons 0x25, forks 0x0
    sink.o1(37, take_1_10, &sh1, &o1);
    declined |= live_v38_b35 & (if bd_v38_b35 { ALL } else { !ok_v38_b35 });
    take_1_11 |= live_v38_b35 & ok_v38_b35 & (if bd_v38_b35 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n890,
        c41: n891,
        c300: n918,
        c301: n907,
        c236: n892,
        c302: n913,
        c303: n921,
        c238: n893,
        c239: n970,
        c304: n859,
        c241: n731,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c312: n933,
        c313: n931,
        c255: n902,
        h1: n3013, h2: n3014,
    };
    // body 35: buttons 0x26, forks 0x0
    sink.o1(38, take_1_11, &sh1, &o1);
    declined |= live_v40_b36 & (if bd_v40_b36 { ALL } else { !ok_v40_b36 });
    take_1_12 |= live_v40_b36 & ok_v40_b36 & (if bd_v40_b36 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n890,
        c41: n891,
        c300: n918,
        c301: n919,
        c236: n892,
        c302: n920,
        c303: n934,
        c238: n893,
        c239: n970,
        c304: n761,
        c241: n731,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c312: n925,
        c313: n935,
        c255: n902,
        h1: n3025, h2: n3026,
    };
    // body 36: buttons 0x28, forks 0x0
    sink.o1(40, take_1_12, &sh1, &o1);
    declined |= live_v41_b37 & (if bd_v41_b37 { ALL } else { !ok_v41_b37 });
    take_1_13 |= live_v41_b37 & ok_v41_b37 & (if bd_v41_b37 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n890,
        c41: n891,
        c300: n918,
        c301: n907,
        c236: n892,
        c302: n908,
        c303: n934,
        c238: n893,
        c239: n970,
        c304: n840,
        c241: n731,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c312: n929,
        c313: n936,
        c255: n902,
        h1: n3035, h2: n3036,
    };
    // body 37: buttons 0x29, forks 0x0
    sink.o1(41, take_1_13, &sh1, &o1);
    declined |= live_v42_b38 & (if bd_v42_b38 { ALL } else { !ok_v42_b38 });
    take_1_14 |= live_v42_b38 & ok_v42_b38 & (if bd_v42_b38 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n890,
        c41: n891,
        c300: n918,
        c301: n907,
        c236: n892,
        c302: n913,
        c303: n934,
        c238: n893,
        c239: n970,
        c304: n859,
        c241: n731,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c312: n933,
        c313: n937,
        c255: n902,
        h1: n3045, h2: n3046,
    };
    // body 38: buttons 0x2a, forks 0x0
    sink.o1(42, take_1_14, &sh1, &o1);
    declined |= live_v48_b39 & (if bd_v48_b39 { ALL } else { !ok_v48_b39 });
    take_1_15 |= live_v48_b39 & ok_v48_b39 & (if bd_v48_b39 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n890,
        c41: n891,
        c300: n894,
        c301: n895,
        c236: n892,
        c302: n896,
        c303: n897,
        c238: n893,
        c239: n970,
        c304: n761,
        c241: n871,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c312: n941,
        c313: n939,
        c255: n902,
        h1: n3071, h2: n3072,
    };
    // body 39: buttons 0x30, forks 0x0
    sink.o1(48, take_1_15, &sh1, &o1);
    declined |= live_v49_b40 & (if bd_v49_b40 { ALL } else { !ok_v49_b40 });
    take_1_16 |= live_v49_b40 & ok_v49_b40 & (if bd_v49_b40 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n890,
        c41: n891,
        c300: n894,
        c301: n907,
        c236: n892,
        c302: n908,
        c303: n897,
        c238: n893,
        c239: n970,
        c304: n840,
        c241: n871,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c312: n945,
        c313: n943,
        c255: n902,
        h1: n3087, h2: n3088,
    };
    // body 40: buttons 0x31, forks 0x0
    sink.o1(49, take_1_16, &sh1, &o1);
    declined |= live_v50_b41 & (if bd_v50_b41 { ALL } else { !ok_v50_b41 });
    take_1_17 |= live_v50_b41 & ok_v50_b41 & (if bd_v50_b41 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n890,
        c41: n891,
        c300: n894,
        c301: n907,
        c236: n892,
        c302: n913,
        c303: n897,
        c238: n893,
        c239: n970,
        c304: n859,
        c241: n871,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c312: n949,
        c313: n947,
        c255: n902,
        h1: n3101, h2: n3102,
    };
    // body 41: buttons 0x32, forks 0x0
    sink.o1(50, take_1_17, &sh1, &o1);
    declined |= live_v52_b42 & (if bd_v52_b42 { ALL } else { !ok_v52_b42 });
    take_1_18 |= live_v52_b42 & ok_v52_b42 & (if bd_v52_b42 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n890,
        c41: n891,
        c300: n918,
        c301: n919,
        c236: n892,
        c302: n920,
        c303: n921,
        c238: n893,
        c239: n970,
        c304: n761,
        c241: n871,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c312: n953,
        c313: n951,
        c255: n902,
        h1: n3119, h2: n3120,
    };
    // body 42: buttons 0x34, forks 0x0
    sink.o1(52, take_1_18, &sh1, &o1);
    declined |= live_v53_b43 & (if bd_v53_b43 { ALL } else { !ok_v53_b43 });
    take_1_19 |= live_v53_b43 & ok_v53_b43 & (if bd_v53_b43 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n890,
        c41: n891,
        c300: n918,
        c301: n907,
        c236: n892,
        c302: n908,
        c303: n921,
        c238: n893,
        c239: n970,
        c304: n840,
        c241: n871,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c312: n957,
        c313: n955,
        c255: n902,
        h1: n3135, h2: n3136,
    };
    // body 43: buttons 0x35, forks 0x0
    sink.o1(53, take_1_19, &sh1, &o1);
    declined |= live_v54_b44 & (if bd_v54_b44 { ALL } else { !ok_v54_b44 });
    take_1_20 |= live_v54_b44 & ok_v54_b44 & (if bd_v54_b44 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n890,
        c41: n891,
        c300: n918,
        c301: n907,
        c236: n892,
        c302: n913,
        c303: n921,
        c238: n893,
        c239: n970,
        c304: n859,
        c241: n871,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c312: n961,
        c313: n959,
        c255: n902,
        h1: n3149, h2: n3150,
    };
    // body 44: buttons 0x36, forks 0x0
    sink.o1(54, take_1_20, &sh1, &o1);
    declined |= live_v56_b45 & (if bd_v56_b45 { ALL } else { !ok_v56_b45 });
    take_1_21 |= live_v56_b45 & ok_v56_b45 & (if bd_v56_b45 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n890,
        c41: n891,
        c300: n918,
        c301: n919,
        c236: n892,
        c302: n920,
        c303: n934,
        c238: n893,
        c239: n970,
        c304: n761,
        c241: n871,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c312: n953,
        c313: n962,
        c255: n902,
        h1: n3159, h2: n3160,
    };
    // body 45: buttons 0x38, forks 0x0
    sink.o1(56, take_1_21, &sh1, &o1);
    declined |= live_v57_b46 & (if bd_v57_b46 { ALL } else { !ok_v57_b46 });
    take_1_22 |= live_v57_b46 & ok_v57_b46 & (if bd_v57_b46 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n890,
        c41: n891,
        c300: n918,
        c301: n907,
        c236: n892,
        c302: n908,
        c303: n934,
        c238: n893,
        c239: n970,
        c304: n840,
        c241: n871,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c312: n957,
        c313: n963,
        c255: n902,
        h1: n3169, h2: n3170,
    };
    // body 46: buttons 0x39, forks 0x0
    sink.o1(57, take_1_22, &sh1, &o1);
    declined |= live_v58_b47 & (if bd_v58_b47 { ALL } else { !ok_v58_b47 });
    take_1_23 |= live_v58_b47 & ok_v58_b47 & (if bd_v58_b47 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n890,
        c41: n891,
        c300: n918,
        c301: n907,
        c236: n892,
        c302: n913,
        c303: n934,
        c238: n893,
        c239: n970,
        c304: n859,
        c241: n871,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c312: n961,
        c313: n964,
        c255: n902,
        h1: n3179, h2: n3180,
    };
    // body 47: buttons 0x3a, forks 0x0
    sink.o1(58, take_1_23, &sh1, &o1);
    declined |= live_v0_b48 & (if bd_v0_b48 { ALL } else { !ok_v0_b48 });
    take_2_0 |= live_v0_b48 & ok_v0_b48 & (if bd_v0_b48 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: r_c20,
        c41: r_c41,
        h1: n3193, h2: n3194,
    };
    // body 48: buttons 0x00, forks 0x0
    sink.o2(0, take_2_0, &sh2, &o2);
    declined |= live_v32_b49 & (if bd_v32_b49 { ALL } else { !ok_v32_b49 });
    take_2_1 |= live_v32_b49 & ok_v32_b49 & (if bd_v32_b49 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n890,
        c41: n891,
        h1: n3197, h2: n3198,
    };
    // body 49: buttons 0x20, forks 0x0
    sink.o2(32, take_2_1, &sh2, &o2);
    declined |= live_v0_b50 & (if bd_v0_b50 { ALL } else { !ok_v0_b50 });
    take_3_0 |= live_v0_b50 & ok_v0_b50 & (if bd_v0_b50 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: r_c20,
        c41: r_c41,
        h1: n3205, h2: n3206,
    };
    // body 50: buttons 0x00, forks 0x0
    sink.o3(0, take_3_0, &sh3, &o3);
    declined |= live_v32_b51 & (if bd_v32_b51 { ALL } else { !ok_v32_b51 });
    take_3_1 |= live_v32_b51 & ok_v32_b51 & (if bd_v32_b51 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1924,
        c41: n1925,
        h1: n3213, h2: n3214,
    };
    // body 51: buttons 0x20, forks 0x0
    sink.o3(32, take_3_1, &sh3, &o3);
    declined |= live_v0_b52 & (if bd_v0_b52 { ALL } else { !ok_v0_b52 });
    take_4_0 |= live_v0_b52 & ok_v0_b52 & (if bd_v0_b52 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: r_c20,
        h1: n3245, h2: n3246,
    };
    // body 52: buttons 0x00, forks 0x0
    sink.o4(0, take_4_0, &sh4, &o4);
    declined |= live_v32_b53 & (if bd_v32_b53 { ALL } else { !ok_v32_b53 });
    take_4_1 |= live_v32_b53 & ok_v32_b53 & (if bd_v32_b53 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n1950,
        h1: n3249, h2: n3250,
    };
    // body 53: buttons 0x20, forks 0x0
    sink.o4(32, take_4_1, &sh4, &o4);
    declined |= live_v0_b54 & (if bd_v0_b54 { ALL } else { !ok_v0_b54 });
    take_5_0 |= live_v0_b54 & ok_v0_b54 & (if bd_v0_b54 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n2023,
        c41: r_c41,
        c308: r_c308,
        c309: r_c309,
        c253: n2024,
        c310: r_c310,
        c311: r_c311,
        c255: n2025,
        c256: n2026,
        c312: n2030,
        c258: n2027,
        c265: n1957,
        c266: n1958,
        c320: n2046,
        c321: n2034,
        c272: n2045,
        h1: n3323, h2: n3324,
    };
    // body 54: buttons 0x00, forks 0x0
    sink.o5(0, take_5_0, &sh5, &o5);
    declined |= live_v1_b55 & (if bd_v1_b55 { ALL } else { !ok_v1_b55 });
    take_5_1 |= live_v1_b55 & ok_v1_b55 & (if bd_v1_b55 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n2023,
        c41: r_c41,
        c308: r_c308,
        c309: r_c309,
        c253: n2024,
        c310: r_c310,
        c311: r_c311,
        c255: n2025,
        c256: n2026,
        c312: n2059,
        c258: n2027,
        c265: n1957,
        c266: n1958,
        c320: n2063,
        c321: n2061,
        c272: n2045,
        h1: n3335, h2: n3336,
    };
    // body 55: buttons 0x01, forks 0x0
    sink.o5(1, take_5_1, &sh5, &o5);
    declined |= live_v2_b56 & (if bd_v2_b56 { ALL } else { !ok_v2_b56 });
    take_5_2 |= live_v2_b56 & ok_v2_b56 & (if bd_v2_b56 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n2023,
        c41: r_c41,
        c308: r_c308,
        c309: r_c309,
        c253: n2024,
        c310: r_c310,
        c311: r_c311,
        c255: n2025,
        c256: n2026,
        c312: n2075,
        c258: n2027,
        c265: n1957,
        c266: n1958,
        c320: n2079,
        c321: n2077,
        c272: n2045,
        h1: n3347, h2: n3348,
    };
    // body 56: buttons 0x02, forks 0x0
    sink.o5(2, take_5_2, &sh5, &o5);
    declined |= live_v16_b57 & (if bd_v16_b57 { ALL } else { !ok_v16_b57 });
    take_5_3 |= live_v16_b57 & ok_v16_b57 & (if bd_v16_b57 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n2023,
        c41: r_c41,
        c308: r_c308,
        c309: r_c309,
        c253: n2024,
        c310: r_c310,
        c311: r_c311,
        c255: n2025,
        c256: n2026,
        c312: n2030,
        c258: n2087,
        c265: n1957,
        c266: n2080,
        c320: n2091,
        c321: n2089,
        c272: n2045,
        h1: n3377, h2: n3378,
    };
    // body 57: buttons 0x10, forks 0x0
    sink.o5(16, take_5_3, &sh5, &o5);
    declined |= live_v17_b58 & (if bd_v17_b58 { ALL } else { !ok_v17_b58 });
    take_5_4 |= live_v17_b58 & ok_v17_b58 & (if bd_v17_b58 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n2023,
        c41: r_c41,
        c308: r_c308,
        c309: r_c309,
        c253: n2024,
        c310: r_c310,
        c311: r_c311,
        c255: n2025,
        c256: n2026,
        c312: n2059,
        c258: n2087,
        c265: n1957,
        c266: n2080,
        c320: n2099,
        c321: n2097,
        c272: n2045,
        h1: n3387, h2: n3388,
    };
    // body 58: buttons 0x11, forks 0x0
    sink.o5(17, take_5_4, &sh5, &o5);
    declined |= live_v18_b59 & (if bd_v18_b59 { ALL } else { !ok_v18_b59 });
    take_5_5 |= live_v18_b59 & ok_v18_b59 & (if bd_v18_b59 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n2023,
        c41: r_c41,
        c308: r_c308,
        c309: r_c309,
        c253: n2024,
        c310: r_c310,
        c311: r_c311,
        c255: n2025,
        c256: n2026,
        c312: n2075,
        c258: n2087,
        c265: n1957,
        c266: n2080,
        c320: n2107,
        c321: n2105,
        c272: n2045,
        h1: n3397, h2: n3398,
    };
    // body 59: buttons 0x12, forks 0x0
    sink.o5(18, take_5_5, &sh5, &o5);
    declined |= live_v32_b60 & (if bd_v32_b60 { ALL } else { !ok_v32_b60 });
    take_5_6 |= live_v32_b60 & ok_v32_b60 & (if bd_v32_b60 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n2127,
        c41: n2128,
        c308: n2132,
        c309: n2133,
        c253: n2129,
        c310: n2134,
        c311: n2135,
        c255: n2130,
        c256: n2131,
        c312: n2030,
        c258: n2027,
        c265: n2108,
        c266: n1958,
        c320: n2141,
        c321: n2137,
        c272: n2140,
        h1: n3455, h2: n3456,
    };
    // body 60: buttons 0x20, forks 0x0
    sink.o5(32, take_5_6, &sh5, &o5);
    declined |= live_v33_b61 & (if bd_v33_b61 { ALL } else { !ok_v33_b61 });
    take_5_7 |= live_v33_b61 & ok_v33_b61 & (if bd_v33_b61 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n2127,
        c41: n2128,
        c308: n2132,
        c309: n2150,
        c253: n2129,
        c310: n2151,
        c311: n2135,
        c255: n2130,
        c256: n2131,
        c312: n2059,
        c258: n2027,
        c265: n2108,
        c266: n1958,
        c320: n2155,
        c321: n2153,
        c272: n2140,
        h1: n3475, h2: n3476,
    };
    // body 61: buttons 0x21, forks 0x0
    sink.o5(33, take_5_7, &sh5, &o5);
    declined |= live_v34_b62 & (if bd_v34_b62 { ALL } else { !ok_v34_b62 });
    take_5_8 |= live_v34_b62 & ok_v34_b62 & (if bd_v34_b62 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n2127,
        c41: n2128,
        c308: n2132,
        c309: n2150,
        c253: n2129,
        c310: n2162,
        c311: n2135,
        c255: n2130,
        c256: n2131,
        c312: n2075,
        c258: n2027,
        c265: n2108,
        c266: n1958,
        c320: n2166,
        c321: n2164,
        c272: n2140,
        h1: n3491, h2: n3492,
    };
    // body 62: buttons 0x22, forks 0x0
    sink.o5(34, take_5_8, &sh5, &o5);
    declined |= live_v36_b63 & (if bd_v36_b63 { ALL } else { !ok_v36_b63 });
    take_5_9 |= live_v36_b63 & ok_v36_b63 & (if bd_v36_b63 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n2127,
        c41: n2128,
        c308: n2179,
        c309: n2180,
        c253: n2129,
        c310: n2181,
        c311: n2182,
        c255: n2130,
        c256: n2131,
        c312: n2030,
        c258: n2027,
        c265: n2108,
        c266: n1958,
        c320: n2186,
        c321: n2184,
        c272: n2140,
        h1: n3517, h2: n3518,
    };
    // body 63: buttons 0x24, forks 0x0
    sink.o5(36, take_5_9, &sh5, &o5);
    declined |= live_v37_b64 & (if bd_v37_b64 { ALL } else { !ok_v37_b64 });
    take_5_10 |= live_v37_b64 & ok_v37_b64 & (if bd_v37_b64 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n2127,
        c41: n2128,
        c308: n2179,
        c309: n2150,
        c253: n2129,
        c310: n2151,
        c311: n2182,
        c255: n2130,
        c256: n2131,
        c312: n2059,
        c258: n2027,
        c265: n2108,
        c266: n1958,
        c320: n2194,
        c321: n2192,
        c272: n2140,
        h1: n3533, h2: n3534,
    };
    // body 64: buttons 0x25, forks 0x0
    sink.o5(37, take_5_10, &sh5, &o5);
    declined |= live_v38_b65 & (if bd_v38_b65 { ALL } else { !ok_v38_b65 });
    take_5_11 |= live_v38_b65 & ok_v38_b65 & (if bd_v38_b65 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n2127,
        c41: n2128,
        c308: n2179,
        c309: n2150,
        c253: n2129,
        c310: n2162,
        c311: n2182,
        c255: n2130,
        c256: n2131,
        c312: n2075,
        c258: n2027,
        c265: n2108,
        c266: n1958,
        c320: n2202,
        c321: n2200,
        c272: n2140,
        h1: n3547, h2: n3548,
    };
    // body 65: buttons 0x26, forks 0x0
    sink.o5(38, take_5_11, &sh5, &o5);
    declined |= live_v40_b66 & (if bd_v40_b66 { ALL } else { !ok_v40_b66 });
    take_5_12 |= live_v40_b66 & ok_v40_b66 & (if bd_v40_b66 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n2127,
        c41: n2128,
        c308: n2179,
        c309: n2180,
        c253: n2129,
        c310: n2181,
        c311: n2207,
        c255: n2130,
        c256: n2131,
        c312: n2030,
        c258: n2027,
        c265: n2108,
        c266: n1958,
        c320: n2186,
        c321: n2208,
        c272: n2140,
        h1: n3559, h2: n3560,
    };
    // body 66: buttons 0x28, forks 0x0
    sink.o5(40, take_5_12, &sh5, &o5);
    declined |= live_v41_b67 & (if bd_v41_b67 { ALL } else { !ok_v41_b67 });
    take_5_13 |= live_v41_b67 & ok_v41_b67 & (if bd_v41_b67 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n2127,
        c41: n2128,
        c308: n2179,
        c309: n2150,
        c253: n2129,
        c310: n2151,
        c311: n2207,
        c255: n2130,
        c256: n2131,
        c312: n2059,
        c258: n2027,
        c265: n2108,
        c266: n1958,
        c320: n2194,
        c321: n2211,
        c272: n2140,
        h1: n3569, h2: n3570,
    };
    // body 67: buttons 0x29, forks 0x0
    sink.o5(41, take_5_13, &sh5, &o5);
    declined |= live_v42_b68 & (if bd_v42_b68 { ALL } else { !ok_v42_b68 });
    take_5_14 |= live_v42_b68 & ok_v42_b68 & (if bd_v42_b68 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n2127,
        c41: n2128,
        c308: n2179,
        c309: n2150,
        c253: n2129,
        c310: n2162,
        c311: n2207,
        c255: n2130,
        c256: n2131,
        c312: n2075,
        c258: n2027,
        c265: n2108,
        c266: n1958,
        c320: n2202,
        c321: n2214,
        c272: n2140,
        h1: n3579, h2: n3580,
    };
    // body 68: buttons 0x2a, forks 0x0
    sink.o5(42, take_5_14, &sh5, &o5);
    declined |= live_v48_b69 & (if bd_v48_b69 { ALL } else { !ok_v48_b69 });
    take_5_15 |= live_v48_b69 & ok_v48_b69 & (if bd_v48_b69 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n2127,
        c41: n2128,
        c308: n2132,
        c309: n2133,
        c253: n2129,
        c310: n2134,
        c311: n2135,
        c255: n2130,
        c256: n2131,
        c312: n2030,
        c258: n2087,
        c265: n2108,
        c266: n2080,
        c320: n2222,
        c321: n2220,
        c272: n2140,
        h1: n3605, h2: n3606,
    };
    // body 69: buttons 0x30, forks 0x0
    sink.o5(48, take_5_15, &sh5, &o5);
    declined |= live_v49_b70 & (if bd_v49_b70 { ALL } else { !ok_v49_b70 });
    take_5_16 |= live_v49_b70 & ok_v49_b70 & (if bd_v49_b70 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n2127,
        c41: n2128,
        c308: n2132,
        c309: n2150,
        c253: n2129,
        c310: n2151,
        c311: n2135,
        c255: n2130,
        c256: n2131,
        c312: n2059,
        c258: n2087,
        c265: n2108,
        c266: n2080,
        c320: n2230,
        c321: n2228,
        c272: n2140,
        h1: n3621, h2: n3622,
    };
    // body 70: buttons 0x31, forks 0x0
    sink.o5(49, take_5_16, &sh5, &o5);
    declined |= live_v50_b71 & (if bd_v50_b71 { ALL } else { !ok_v50_b71 });
    take_5_17 |= live_v50_b71 & ok_v50_b71 & (if bd_v50_b71 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n2127,
        c41: n2128,
        c308: n2132,
        c309: n2150,
        c253: n2129,
        c310: n2162,
        c311: n2135,
        c255: n2130,
        c256: n2131,
        c312: n2075,
        c258: n2087,
        c265: n2108,
        c266: n2080,
        c320: n2238,
        c321: n2236,
        c272: n2140,
        h1: n3635, h2: n3636,
    };
    // body 71: buttons 0x32, forks 0x0
    sink.o5(50, take_5_17, &sh5, &o5);
    declined |= live_v52_b72 & (if bd_v52_b72 { ALL } else { !ok_v52_b72 });
    take_5_18 |= live_v52_b72 & ok_v52_b72 & (if bd_v52_b72 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n2127,
        c41: n2128,
        c308: n2179,
        c309: n2180,
        c253: n2129,
        c310: n2181,
        c311: n2182,
        c255: n2130,
        c256: n2131,
        c312: n2030,
        c258: n2087,
        c265: n2108,
        c266: n2080,
        c320: n2246,
        c321: n2244,
        c272: n2140,
        h1: n3653, h2: n3654,
    };
    // body 72: buttons 0x34, forks 0x0
    sink.o5(52, take_5_18, &sh5, &o5);
    declined |= live_v53_b73 & (if bd_v53_b73 { ALL } else { !ok_v53_b73 });
    take_5_19 |= live_v53_b73 & ok_v53_b73 & (if bd_v53_b73 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n2127,
        c41: n2128,
        c308: n2179,
        c309: n2150,
        c253: n2129,
        c310: n2151,
        c311: n2182,
        c255: n2130,
        c256: n2131,
        c312: n2059,
        c258: n2087,
        c265: n2108,
        c266: n2080,
        c320: n2254,
        c321: n2252,
        c272: n2140,
        h1: n3669, h2: n3670,
    };
    // body 73: buttons 0x35, forks 0x0
    sink.o5(53, take_5_19, &sh5, &o5);
    declined |= live_v54_b74 & (if bd_v54_b74 { ALL } else { !ok_v54_b74 });
    take_5_20 |= live_v54_b74 & ok_v54_b74 & (if bd_v54_b74 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n2127,
        c41: n2128,
        c308: n2179,
        c309: n2150,
        c253: n2129,
        c310: n2162,
        c311: n2182,
        c255: n2130,
        c256: n2131,
        c312: n2075,
        c258: n2087,
        c265: n2108,
        c266: n2080,
        c320: n2262,
        c321: n2260,
        c272: n2140,
        h1: n3683, h2: n3684,
    };
    // body 74: buttons 0x36, forks 0x0
    sink.o5(54, take_5_20, &sh5, &o5);
    declined |= live_v56_b75 & (if bd_v56_b75 { ALL } else { !ok_v56_b75 });
    take_5_21 |= live_v56_b75 & ok_v56_b75 & (if bd_v56_b75 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n2127,
        c41: n2128,
        c308: n2179,
        c309: n2180,
        c253: n2129,
        c310: n2181,
        c311: n2207,
        c255: n2130,
        c256: n2131,
        c312: n2030,
        c258: n2087,
        c265: n2108,
        c266: n2080,
        c320: n2246,
        c321: n2265,
        c272: n2140,
        h1: n3693, h2: n3694,
    };
    // body 75: buttons 0x38, forks 0x0
    sink.o5(56, take_5_21, &sh5, &o5);
    declined |= live_v57_b76 & (if bd_v57_b76 { ALL } else { !ok_v57_b76 });
    take_5_22 |= live_v57_b76 & ok_v57_b76 & (if bd_v57_b76 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n2127,
        c41: n2128,
        c308: n2179,
        c309: n2150,
        c253: n2129,
        c310: n2151,
        c311: n2207,
        c255: n2130,
        c256: n2131,
        c312: n2059,
        c258: n2087,
        c265: n2108,
        c266: n2080,
        c320: n2254,
        c321: n2268,
        c272: n2140,
        h1: n3703, h2: n3704,
    };
    // body 76: buttons 0x39, forks 0x0
    sink.o5(57, take_5_22, &sh5, &o5);
    declined |= live_v58_b77 & (if bd_v58_b77 { ALL } else { !ok_v58_b77 });
    take_5_23 |= live_v58_b77 & ok_v58_b77 & (if bd_v58_b77 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n2127,
        c41: n2128,
        c308: n2179,
        c309: n2150,
        c253: n2129,
        c310: n2162,
        c311: n2207,
        c255: n2130,
        c256: n2131,
        c312: n2075,
        c258: n2087,
        c265: n2108,
        c266: n2080,
        c320: n2262,
        c321: n2271,
        c272: n2140,
        h1: n3713, h2: n3714,
    };
    // body 77: buttons 0x3a, forks 0x0
    sink.o5(58, take_5_23, &sh5, &o5);
    declined
}
