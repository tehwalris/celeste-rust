// GENERATED from a TRACED frame (shape 5). Do not edit.
//
// One input shape, 4 output shapes, 52 distinct button
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
pub const SHAPE: u64 = 37911473208088391;

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
    pub c306: P8,
    pub c307: P8,
    pub c308: P8,
    pub c309: P8,
    pub c316: P8,
    pub c317: P8,
    pub c318: P8,
    pub c319: P8,
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
    ("objects[0].dash_accel.x", "num"),
    ("objects[0].dash_accel.y", "num"),
    ("objects[0].dash_effect_time", "num"),
    ("objects[0].dash_target.x", "num"),
    ("objects[0].dash_target.y", "num"),
    ("objects[0].dash_time", "num"),
    ("objects[0].djump", "num"),
    ("objects[0].flip.x", "bool"),
    ("objects[0].flip.y", "bool"),
    ("objects[0].grace", "num"),
    ("objects[0].p_dash", "bool"),
    ("objects[0].p_jump", "bool"),
    ("objects[0].rem.x", "num"),
    ("objects[0].rem.y", "num"),
    ("objects[0].solids", "bool"),
    ("objects[0].spd.x", "num"),
    ("objects[0].spd.y", "num"),
    ("objects[0].x", "num"),
    ("objects[0].y", "num"),
    ("objects[1].collideable", "bool"),
    ("objects[1].flip.x", "bool"),
    ("objects[1].flip.y", "bool"),
    ("objects[1].off", "num"),
    ("objects[1].rem.x", "num"),
    ("objects[1].rem.y", "num"),
    ("objects[1].solids", "bool"),
    ("objects[1].spd.x", "num"),
    ("objects[1].spd.y", "num"),
    ("objects[1].spr", "num"),
    ("objects[1].start", "num"),
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
    pub c300: ZN,
    pub c301: ZN,
    pub c236: ZN,
    pub c302: ZN,
    pub c303: ZN,
    pub c238: ZN,
    pub c239: ZN,
    pub c304: u16,
    pub c305: u16,
    pub c241: ZN,
    pub c248: u16,
    pub c249: u16,
    pub c310: ZN,
    pub c311: ZN,
    pub c251: u16,
    pub c312: ZN,
    pub c313: ZN,
    pub c255: ZN,
    pub c256: ZN,
    pub c259: u16,
    pub c314: u16,
    pub c315: u16,
    pub c267: ZN,
    pub c320: ZN,
    pub c321: ZN,
    pub c269: u16,
    pub c322: ZN,
    pub c323: ZN,
    pub c271: ZN,
    pub c272: ZN,
    pub c274: ZN,
    pub c275: ZN,
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
    pub c300: u32,
    pub c301: u32,
    pub c236: u32,
    pub c302: u32,
    pub c303: u32,
    pub c238: u32,
    pub c239: u32,
    pub c304: u32,
    pub c305: u32,
    pub c241: u32,
    pub c248: u32,
    pub c249: u32,
    pub c310: u32,
    pub c311: u32,
    pub c251: u32,
    pub c312: u32,
    pub c313: u32,
    pub c255: u32,
    pub c256: u32,
    pub c259: u32,
    pub c314: u32,
    pub c315: u32,
    pub c267: u32,
    pub c320: u32,
    pub c321: u32,
    pub c269: u32,
    pub c322: u32,
    pub c323: u32,
    pub c271: u32,
    pub c272: u32,
    pub c274: u32,
    pub c275: u32,
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
        c306: match &b.cols[cell("objects[0].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c307: match &b.cols[cell("objects[0].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c308: match &b.cols[cell("objects[0].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c309: match &b.cols[cell("objects[0].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c316: match &b.cols[cell("objects[1].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c317: match &b.cols[cell("objects[1].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c318: match &b.cols[cell("objects[1].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c319: match &b.cols[cell("objects[1].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
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
        c300: cell("objects[0].dash_accel.x")?,
        c301: cell("objects[0].dash_accel.y")?,
        c236: cell("objects[0].dash_effect_time")?,
        c302: cell("objects[0].dash_target.x")?,
        c303: cell("objects[0].dash_target.y")?,
        c238: cell("objects[0].dash_time")?,
        c239: cell("objects[0].djump")?,
        c304: cell("objects[0].flip.x")?,
        c305: cell("objects[0].flip.y")?,
        c241: cell("objects[0].grace")?,
        c248: cell("objects[0].p_dash")?,
        c249: cell("objects[0].p_jump")?,
        c310: cell("objects[0].rem.x")?,
        c311: cell("objects[0].rem.y")?,
        c251: cell("objects[0].solids")?,
        c312: cell("objects[0].spd.x")?,
        c313: cell("objects[0].spd.y")?,
        c255: cell("objects[0].x")?,
        c256: cell("objects[0].y")?,
        c259: cell("objects[1].collideable")?,
        c314: cell("objects[1].flip.x")?,
        c315: cell("objects[1].flip.y")?,
        c267: cell("objects[1].off")?,
        c320: cell("objects[1].rem.x")?,
        c321: cell("objects[1].rem.y")?,
        c269: cell("objects[1].solids")?,
        c322: cell("objects[1].spd.x")?,
        c323: cell("objects[1].spd.y")?,
        c271: cell("objects[1].spr")?,
        c272: cell("objects[1].start")?,
        c274: cell("objects[1].x")?,
        c275: cell("objects[1].y")?,
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
        c236: match &b.cols[s.c236 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c302: match &b.cols[s.c302 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c303: match &b.cols[s.c303 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c238: match &b.cols[s.c238 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c239: match &b.cols[s.c239 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c304: match &b.cols[s.c304 as usize] {
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
        c305: match &b.cols[s.c305 as usize] {
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
        c249: match &b.cols[s.c249 as usize] {
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
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c311: match &b.cols[s.c311 as usize] {
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
        c312: match &b.cols[s.c312 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c313: match &b.cols[s.c313 as usize] {
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
        c314: match &b.cols[s.c314 as usize] {
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
        c315: match &b.cols[s.c315 as usize] {
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
        c267: match &b.cols[s.c267 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
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
        c269: match &b.cols[s.c269 as usize] {
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
        c322: match &b.cols[s.c322 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c323: match &b.cols[s.c323 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c271: match &b.cols[s.c271 as usize] {
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

/// Outcome 1's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared1 {
    pub c87: ZN,
    pub c84: ZN,
    pub c86: ZN,
    pub c241: ZN,
    pub c249: ZN,
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
    pub c85: ZN,
    pub c38: ZB,
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
    pub c310: ZN,
    pub c311: ZN,
    pub c256: ZN,
    pub c267: ZN,
    pub c275: ZN,
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
    b.cols[241] = Col::N(Vec::new());
    b.cols[267] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[268] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[243] = Col::U(AV::Bool(true));
    b.cols[269] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[270] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[245] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[246] = Col::U(AV::Num(P8::from_raw(2359296i32)));
    b.cols[248] = Col::U(AV::Num(P8::from_raw(786432i32)));
    b.cols[249] = Col::N(Vec::new());
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[157] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[160] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[172] = Col::U(AV::Num(P8::from_raw(1179648i32)));
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
pub const KPART1_1: u64 = 3549381895084469594;
pub const KPART2_1: u64 = 788818541505896789;

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
        if let Col::N(v) = &mut acc.cols[241] { v.push(sh.c241.lane(i)); }
        if let Col::N(v) = &mut acc.cols[249] { v.push(sh.c249.lane(i)); }
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
    b.cols[234] = Col::U(AV::Num(P8::from_raw(196608i32)));
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
    b.cols[246] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[273] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[274] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[157] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[249] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[250] = Col::U(AV::Num(P8::from_raw(8126464i32)));
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[160] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[172] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[38] = Col::V(Vec::new());
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
pub const KPART1_2: u64 = 8929835150018199120;
pub const KPART2_2: u64 = 6320912223011660372;

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
    b.cols[267] = Col::N(Vec::new());
    b.cols[320] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[321] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[269] = Col::U(AV::Bool(true));
    b.cols[322] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[323] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[271] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[272] = Col::U(AV::Num(P8::from_raw(2359296i32)));
    b.cols[274] = Col::U(AV::Num(P8::from_raw(786432i32)));
    b.cols[275] = Col::N(Vec::new());
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[157] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[160] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[172] = Col::U(AV::Num(P8::from_raw(1179648i32)));
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
pub const KPART1_3: u64 = 4506360063398802172;
pub const KPART2_3: u64 = 15048838345758023163;

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
        if let Col::N(v) = &mut acc.cols[267] { v.push(sh.c267.lane(i)); }
        if let Col::N(v) = &mut acc.cols[275] { v.push(sh.c275.lane(i)); }
        if let Col::N(v) = &mut acc.cols[85] { v.push(sh.c85.lane(i)); }
        if !org.is_empty() { acc.origin.push(org[i]); }
        wrote |= 1 << i;
        acc.width += 1;
    }
    wrote
}

pub const OUTCOMES: usize = 4;

pub fn acc(i: usize, cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    match i {
        0 => acc0(cart, cache),
        1 => acc1(cart, cache),
        2 => acc2(cart, cache),
        3 => acc3(cart, cache),
        _ => panic!("outcome {} of 4", i),
    }
}

pub fn out_slots(i: usize) -> &'static [(u32, &'static str)] {
    match i {
        0 => OUT_SLOTS_0,
        1 => OUT_SLOTS_1,
        2 => OUT_SLOTS_2,
        3 => OUT_SLOTS_3,
        _ => panic!("outcome {} of 4", i),
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
    let r_c236: ZN = rin.c236;
    let r_c238: ZN = rin.c238;
    let r_c239: ZN = rin.c239;
    let r_c241: ZN = rin.c241;
    let r_c248: ZB = ZB { val: rin.c248, known: ALL };
    let r_c249: ZB = ZB { val: rin.c249, known: ALL };
    let r_c251: ZB = ZB { val: rin.c251, known: ALL };
    let r_c255: ZN = rin.c255;
    let r_c256: ZN = rin.c256;
    let r_c259: ZB = ZB { val: rin.c259, known: ALL };
    let r_c267: ZN = rin.c267;
    let r_c269: ZB = ZB { val: rin.c269, known: ALL };
    let r_c271: ZN = rin.c271;
    let r_c272: ZN = rin.c272;
    let r_c274: ZN = rin.c274;
    let r_c275: ZN = rin.c275;
    let r_c300: ZN = rin.c300;
    let r_c301: ZN = rin.c301;
    let r_c302: ZN = rin.c302;
    let r_c303: ZN = rin.c303;
    let r_c304: ZB = ZB { val: rin.c304, known: ALL };
    let r_c305: ZB = ZB { val: rin.c305, known: ALL };
    let r_c310: ZN = rin.c310;
    let r_c311: ZN = rin.c311;
    let r_c312: ZN = rin.c312;
    let r_c313: ZN = rin.c313;
    let r_c314: ZB = ZB { val: rin.c314, known: ALL };
    let r_c315: ZB = ZB { val: rin.c315, known: ALL };
    let r_c320: ZN = rin.c320;
    let r_c321: ZN = rin.c321;
    let r_c322: ZN = rin.c322;
    let r_c323: ZN = rin.c323;
    let n70: ZB = zb_not(r_c42);
    let n71: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), r_c88);
    let n72: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c313);
    let n73: ZB = zb_not(r_c314);
    let n74: bool = P8::from_raw(0i32) == u.c318;
    let n75: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c322);
    let n79: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c84);
    let n80: ZN = zn_rem(n79, zn_splat(P8::from_raw(1966080i32)));
    let n81: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n80);
    let n97: ZB = zb_not(r_c248);
    let n98: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c86);
    let n99: ZB = zn_le(r_c20, zn_splat(P8::from_raw(0i32)));
    let n107: ZB = zb_not(r_c43);
    let n108: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c323);
    let n111: ZB = zb_not(r_c305);
    let n112: bool = P8::from_raw(327680i32) == u.c306;
    let n113: bool = P8::from_raw(393216i32) == u.c307;
    let n114: bool = P8::from_raw(65536i32) == u.c308;
    let n115: bool = P8::from_raw(196608i32) == u.c309;
    let n116: ZB = zb_not(r_c315);
    let n117: bool = P8::from_raw(524288i32) == u.c316;
    let n118: bool = P8::from_raw(524288i32) == u.c317;
    let n119: bool = P8::from_raw(0i32) == u.c319;
    let n120: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c320);
    let n121: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c321);
    let n122: ZB = zn_eq(zn_splat(P8::from_raw(1703936i32)), r_c271);
    let n123: ZB = zn_eq(zn_splat(P8::from_raw(2359296i32)), r_c272);
    let n124: ZB = zn_eq(zn_splat(P8::from_raw(786432i32)), r_c274);
    let n125: ZB = zb_not(r_c38);
    let n126: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c85);
    let n127: ZN = zn_rem(n126, zn_splat(P8::from_raw(3932160i32)));
    let n128: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n127);
    let n129: ZN = zsel_n(n128, n98, r_c86);
    let n130: ZN = zsel_n(n81, n129, r_c86);
    let n131: ZN = zsel_n(n81, n127, r_c85);
    let n132: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c312);
    let n133: ZB = zb_not(n132);
    let n134: ZB = zb_not(n72);
    let n135: ZB = zb_or(n133, n134);
    let n136: ZB = zb_not(n135);
    let n137: ZN = zn_add(r_c310, r_c312);
    let n138: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n137);
    let n139: ZN = zn_flr(n138);
    let n140: ZN = zn_sub(n138, zn_splat(P8::from_raw(32768i32)));
    let n141: ZN = zn_sub(n140, n139);
    let n142: ZB = zn_gt(n139, zn_splat(P8::from_raw(0i32)));
    let n143: ZB = zn_lt(n139, zn_splat(P8::from_raw(0i32)));
    let n144: ZN = zsel_n(n143, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n145: ZN = zsel_n(n142, zn_splat(P8::from_raw(65536i32)), n144);
    let n146: ZN = zn_abs(n139);
    let n147: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c255);
    let n148: ZN = zn_add(n145, n147);
    let n149: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), r_c256);
    let n150: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n149);
    let n151: ZB = zn_tile_flag_at(g.cache, g.cart, n148, n150, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n152: ZN = zn_add(r_c255, n145);
    let n153: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n146);
    let n154: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n152);
    let n155: ZN = zn_add(n145, n154);
    let n156: ZB = zn_tile_flag_at(g.cache, g.cart, n155, n150, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n157: ZN = zn_add(n145, n152);
    let n158: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n146);
    let n159: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n157);
    let n160: ZN = zn_add(n145, n159);
    let n161: ZB = zn_tile_flag_at(g.cache, g.cart, n160, n150, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n162: ZN = zn_add(n145, n157);
    let n163: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n146);
    let n164: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n162);
    let n165: ZN = zn_add(n145, n164);
    let n166: ZB = zn_tile_flag_at(g.cache, g.cart, n165, n150, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n167: ZN = zn_add(n145, n162);
    let n168: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n146);
    let n169: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n167);
    let n170: ZN = zn_add(n145, n169);
    let n171: ZB = zn_tile_flag_at(g.cache, g.cart, n170, n150, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n172: ZN = zn_add(n145, n167);
    let n173: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n146);
    let n174: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n172);
    let n175: ZN = zn_add(n145, n174);
    let n176: ZB = zn_tile_flag_at(g.cache, g.cart, n175, n150, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n177: ZN = zn_add(n145, n172);
    let n178: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n146);
    let n179: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n177);
    let n180: ZN = zn_add(n145, n179);
    let n181: ZB = zn_tile_flag_at(g.cache, g.cart, n180, n150, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n182: ZN = zn_add(n145, n177);
    let n183: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n146);
    let n184: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n182);
    let n185: ZN = zn_add(n145, n184);
    let n186: ZB = zn_tile_flag_at(g.cache, g.cart, n185, n150, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n187: ZN = zn_add(n145, n182);
    let n188: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n146);
    let n189: ZN = zsel_n(n186, n182, n187);
    let n190: ZN = zsel_n(n186, zn_splat(P8::from_raw(0i32)), n141);
    let n191: ZN = zsel_n(n186, zn_splat(P8::from_raw(0i32)), r_c312);
    let n192: ZB = zb_or(n186, n188);
    let n193: ZN = zsel_n(n183, n182, n189);
    let n194: ZN = zsel_n(n183, n141, n190);
    let n195: ZN = zsel_n(n183, r_c312, n191);
    let n196: ZB = zb_or(n183, n192);
    let n197: ZN = zsel_n(n181, n177, n193);
    let n198: ZN = zsel_n(n181, zn_splat(P8::from_raw(0i32)), n194);
    let n199: ZN = zsel_n(n181, zn_splat(P8::from_raw(0i32)), n195);
    let n200: ZB = zb_or(n181, n196);
    let n201: ZN = zsel_n(n178, n177, n197);
    let n202: ZN = zsel_n(n178, n141, n198);
    let n203: ZN = zsel_n(n178, r_c312, n199);
    let n204: ZB = zb_or(n178, n200);
    let n205: ZN = zsel_n(n176, n172, n201);
    let n206: ZN = zsel_n(n176, zn_splat(P8::from_raw(0i32)), n202);
    let n207: ZN = zsel_n(n176, zn_splat(P8::from_raw(0i32)), n203);
    let n208: ZB = zb_or(n176, n204);
    let n209: ZN = zsel_n(n173, n172, n205);
    let n210: ZN = zsel_n(n173, n141, n206);
    let n211: ZN = zsel_n(n173, r_c312, n207);
    let n212: ZB = zb_or(n173, n208);
    let n213: ZN = zsel_n(n171, n167, n209);
    let n214: ZN = zsel_n(n171, zn_splat(P8::from_raw(0i32)), n210);
    let n215: ZN = zsel_n(n171, zn_splat(P8::from_raw(0i32)), n211);
    let n216: ZB = zb_or(n171, n212);
    let n217: ZN = zsel_n(n168, n167, n213);
    let n218: ZN = zsel_n(n168, n141, n214);
    let n219: ZN = zsel_n(n168, r_c312, n215);
    let n220: ZB = zb_or(n168, n216);
    let n221: ZN = zsel_n(n166, n162, n217);
    let n222: ZN = zsel_n(n166, zn_splat(P8::from_raw(0i32)), n218);
    let n223: ZN = zsel_n(n166, zn_splat(P8::from_raw(0i32)), n219);
    let n224: ZB = zb_or(n166, n220);
    let n225: ZN = zsel_n(n163, n162, n221);
    let n226: ZN = zsel_n(n163, n141, n222);
    let n227: ZN = zsel_n(n163, r_c312, n223);
    let n228: ZB = zb_or(n163, n224);
    let n229: ZN = zsel_n(n161, n157, n225);
    let n230: ZN = zsel_n(n161, zn_splat(P8::from_raw(0i32)), n226);
    let n231: ZN = zsel_n(n161, zn_splat(P8::from_raw(0i32)), n227);
    let n232: ZB = zb_or(n161, n228);
    let n233: ZN = zsel_n(n158, n157, n229);
    let n234: ZN = zsel_n(n158, n141, n230);
    let n235: ZN = zsel_n(n158, r_c312, n231);
    let n236: ZB = zb_or(n158, n232);
    let n237: ZN = zsel_n(n156, n152, n233);
    let n238: ZN = zsel_n(n156, zn_splat(P8::from_raw(0i32)), n234);
    let n239: ZN = zsel_n(n156, zn_splat(P8::from_raw(0i32)), n235);
    let n240: ZB = zb_or(n156, n236);
    let n241: ZN = zsel_n(n153, n152, n237);
    let n242: ZN = zsel_n(n153, n141, n238);
    let n243: ZN = zsel_n(n153, r_c312, n239);
    let n244: ZB = zb_or(n153, n240);
    let n245: ZN = zsel_n(n151, r_c255, n241);
    let n246: ZN = zsel_n(n151, zn_splat(P8::from_raw(0i32)), n242);
    let n247: ZN = zsel_n(n151, zn_splat(P8::from_raw(0i32)), n243);
    let n248: ZB = zb_or(n151, n244);
    let n249: ZN = zn_add(r_c311, r_c313);
    let n250: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n249);
    let n251: ZN = zn_flr(n250);
    let n252: ZN = zn_sub(n250, zn_splat(P8::from_raw(32768i32)));
    let n253: ZN = zn_sub(n252, n251);
    let n254: ZB = zn_gt(n251, zn_splat(P8::from_raw(0i32)));
    let n255: ZB = zn_lt(n251, zn_splat(P8::from_raw(0i32)));
    let n256: ZN = zsel_n(n255, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n257: ZN = zsel_n(n254, zn_splat(P8::from_raw(65536i32)), n256);
    let n258: ZN = zn_abs(n251);
    let n259: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n245);
    let n260: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n259);
    let n261: ZN = zn_add(n149, n257);
    let n262: ZB = zn_tile_flag_at(g.cache, g.cart, n260, n261, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n263: ZN = zn_add(r_c256, n257);
    let n264: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n258);
    let n265: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n263);
    let n266: ZN = zn_add(n257, n265);
    let n267: ZB = zn_tile_flag_at(g.cache, g.cart, n260, n266, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n268: ZN = zn_add(n257, n263);
    let n269: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n258);
    let n270: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n268);
    let n271: ZN = zn_add(n257, n270);
    let n272: ZB = zn_tile_flag_at(g.cache, g.cart, n260, n271, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n273: ZN = zn_add(n257, n268);
    let n274: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n258);
    let n275: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n273);
    let n276: ZN = zn_add(n257, n275);
    let n277: ZB = zn_tile_flag_at(g.cache, g.cart, n260, n276, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n278: ZN = zn_add(n257, n273);
    let n279: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n258);
    let n280: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n278);
    let n281: ZN = zn_add(n257, n280);
    let n282: ZB = zn_tile_flag_at(g.cache, g.cart, n260, n281, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n283: ZN = zn_add(n257, n278);
    let n284: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n258);
    let n285: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n283);
    let n286: ZN = zn_add(n257, n285);
    let n287: ZB = zn_tile_flag_at(g.cache, g.cart, n260, n286, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n288: ZN = zn_add(n257, n283);
    let n289: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n258);
    let n290: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n288);
    let n291: ZN = zn_add(n257, n290);
    let n292: ZB = zn_tile_flag_at(g.cache, g.cart, n260, n291, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n293: ZN = zn_add(n257, n288);
    let n294: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n258);
    let n295: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n293);
    let n296: ZN = zn_add(n257, n295);
    let n297: ZB = zn_tile_flag_at(g.cache, g.cart, n260, n296, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n298: ZN = zn_add(n257, n293);
    let n299: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n258);
    let n300: ZB = zb_and(n248, n299);
    let n301: ZN = zsel_n(n297, n293, n298);
    let n302: ZN = zsel_n(n297, zn_splat(P8::from_raw(0i32)), n253);
    let n303: ZN = zsel_n(n297, zn_splat(P8::from_raw(0i32)), r_c313);
    let n304: ZB = zsel_b(n297, n248, n300);
    let n305: ZN = zsel_n(n294, n293, n301);
    let n306: ZN = zsel_n(n294, n253, n302);
    let n307: ZN = zsel_n(n294, r_c313, n303);
    let n308: ZB = zsel_b(n294, n248, n304);
    let n309: ZN = zsel_n(n292, n288, n305);
    let n310: ZN = zsel_n(n292, zn_splat(P8::from_raw(0i32)), n306);
    let n311: ZN = zsel_n(n292, zn_splat(P8::from_raw(0i32)), n307);
    let n312: ZB = zsel_b(n292, n248, n308);
    let n313: ZN = zsel_n(n289, n288, n309);
    let n314: ZN = zsel_n(n289, n253, n310);
    let n315: ZN = zsel_n(n289, r_c313, n311);
    let n316: ZB = zsel_b(n289, n248, n312);
    let n317: ZN = zsel_n(n287, n283, n313);
    let n318: ZN = zsel_n(n287, zn_splat(P8::from_raw(0i32)), n314);
    let n319: ZN = zsel_n(n287, zn_splat(P8::from_raw(0i32)), n315);
    let n320: ZB = zsel_b(n287, n248, n316);
    let n321: ZN = zsel_n(n284, n283, n317);
    let n322: ZN = zsel_n(n284, n253, n318);
    let n323: ZN = zsel_n(n284, r_c313, n319);
    let n324: ZB = zsel_b(n284, n248, n320);
    let n325: ZN = zsel_n(n282, n278, n321);
    let n326: ZN = zsel_n(n282, zn_splat(P8::from_raw(0i32)), n322);
    let n327: ZN = zsel_n(n282, zn_splat(P8::from_raw(0i32)), n323);
    let n328: ZB = zsel_b(n282, n248, n324);
    let n329: ZN = zsel_n(n279, n278, n325);
    let n330: ZN = zsel_n(n279, n253, n326);
    let n331: ZN = zsel_n(n279, r_c313, n327);
    let n332: ZB = zsel_b(n279, n248, n328);
    let n333: ZN = zsel_n(n277, n273, n329);
    let n334: ZN = zsel_n(n277, zn_splat(P8::from_raw(0i32)), n330);
    let n335: ZN = zsel_n(n277, zn_splat(P8::from_raw(0i32)), n331);
    let n336: ZB = zsel_b(n277, n248, n332);
    let n337: ZN = zsel_n(n274, n273, n333);
    let n338: ZN = zsel_n(n274, n253, n334);
    let n339: ZN = zsel_n(n274, r_c313, n335);
    let n340: ZB = zsel_b(n274, n248, n336);
    let n341: ZN = zsel_n(n272, n268, n337);
    let n342: ZN = zsel_n(n272, zn_splat(P8::from_raw(0i32)), n338);
    let n343: ZN = zsel_n(n272, zn_splat(P8::from_raw(0i32)), n339);
    let n344: ZB = zsel_b(n272, n248, n340);
    let n345: ZN = zsel_n(n269, n268, n341);
    let n346: ZN = zsel_n(n269, n253, n342);
    let n347: ZN = zsel_n(n269, r_c313, n343);
    let n348: ZB = zsel_b(n269, n248, n344);
    let n349: ZN = zsel_n(n267, n263, n345);
    let n350: ZN = zsel_n(n267, zn_splat(P8::from_raw(0i32)), n346);
    let n351: ZN = zsel_n(n267, zn_splat(P8::from_raw(0i32)), n347);
    let n352: ZB = zsel_b(n267, n248, n348);
    let n353: ZN = zsel_n(n264, n263, n349);
    let n354: ZN = zsel_n(n264, n253, n350);
    let n355: ZN = zsel_n(n264, r_c313, n351);
    let n356: ZB = zsel_b(n264, n248, n352);
    let n357: ZN = zsel_n(n262, r_c256, n353);
    let n358: ZN = zsel_n(n262, zn_splat(P8::from_raw(0i32)), n354);
    let n359: ZN = zsel_n(n262, zn_splat(P8::from_raw(0i32)), n355);
    let n360: ZB = zsel_b(n262, n248, n356);
    let n361: ZN = zsel_n(n135, n245, r_c255);
    let n362: ZN = zsel_n(n135, n357, r_c256);
    let n363: ZN = zsel_n(n135, n246, r_c310);
    let n364: ZN = zsel_n(n135, n358, r_c311);
    let n365: ZN = zsel_n(n135, n247, r_c312);
    let n366: ZN = zsel_n(n135, n359, r_c313);
    let n367: ZB = zb_or(n136, n360);
    let n368: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n361);
    let n369: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n362);
    let n370: ZN = zn_div(n368, zn_splat(P8::from_raw(524288i32)));
    let n371: ZN = zn_flr(n370);
    let n372: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n371);
    let n373: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n368);
    let n374: ZN = zn_sub(n373, zn_splat(P8::from_raw(65536i32)));
    let n375: ZN = zn_div(n374, zn_splat(P8::from_raw(524288i32)));
    let n376: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n375);
    let n377: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n372);
    let n378: ZB = zn_le(n377, n376);
    let n379: ZB = zn_gt(n377, n376);
    let n380: ZB = zb_and(n99, n378);
    let n381: ZB = zb_and(n99, n379);
    let n382: ZN = zn_div(n369, zn_splat(P8::from_raw(524288i32)));
    let n383: ZN = zn_flr(n382);
    let n384: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n383);
    let n385: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n369);
    let n386: ZN = zn_sub(n385, zn_splat(P8::from_raw(65536i32)));
    let n387: ZN = zn_div(n386, zn_splat(P8::from_raw(524288i32)));
    let n388: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n387);
    let n389: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n384);
    let n390: ZB = zn_le(n389, n388);
    let n391: ZB = zn_gt(n389, n388);
    let n392: ZB = zb_and(n380, n390);
    let n393: ZB = zb_and(n380, n391);
    let n394: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n377);
    let n395: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n389);
    let n396: ZN = zn_mget(g.cart, n394, n395);
    let n397: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n396);
    let n398: ZN = zn_rem(n386, zn_splat(P8::from_raw(524288i32)));
    let n399: ZB = zn_ge(n398, zn_splat(P8::from_raw(393216i32)));
    let n400: ZN = zn_mul(n389, zn_splat(P8::from_raw(524288i32)));
    let n401: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n400);
    let n402: ZB = zn_eq(n385, n401);
    let n403: ZB = zb_or(n399, n402);
    let n404: ZB = zb_and(n397, n403);
    let n405: ZB = zn_ge(n366, zn_splat(P8::from_raw(0i32)));
    let n406: ZB = zb_and(n404, n405);
    let n407: ZB = zb_not(n406);
    let n408: ZB = zb_and(n392, n407);
    let n409: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n396);
    let n410: ZN = zn_rem(n369, zn_splat(P8::from_raw(524288i32)));
    let n411: ZB = zn_le(n410, zn_splat(P8::from_raw(131072i32)));
    let n412: ZB = zb_and(n409, n411);
    let n413: ZB = zn_le(n366, zn_splat(P8::from_raw(0i32)));
    let n414: ZB = zb_and(n412, n413);
    let n415: ZB = zb_not(n414);
    let n416: ZB = zb_and(n408, n415);
    let n417: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n396);
    let n418: ZN = zn_rem(n368, zn_splat(P8::from_raw(524288i32)));
    let n419: ZB = zn_le(n418, zn_splat(P8::from_raw(131072i32)));
    let n420: ZB = zb_and(n417, n419);
    let n421: ZB = zn_le(n365, zn_splat(P8::from_raw(0i32)));
    let n422: ZB = zb_and(n420, n421);
    let n423: ZB = zb_not(n422);
    let n424: ZB = zb_and(n416, n423);
    let n425: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n396);
    let n426: ZN = zn_rem(n374, zn_splat(P8::from_raw(524288i32)));
    let n427: ZB = zn_ge(n426, zn_splat(P8::from_raw(393216i32)));
    let n428: ZN = zn_mul(n377, zn_splat(P8::from_raw(524288i32)));
    let n429: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n428);
    let n430: ZB = zn_eq(n373, n429);
    let n431: ZB = zb_or(n427, n430);
    let n432: ZB = zb_and(n425, n431);
    let n433: ZB = zn_ge(n365, zn_splat(P8::from_raw(0i32)));
    let n434: ZB = zb_and(n432, n433);
    let n435: ZB = zb_not(n434);
    let n436: ZB = zb_and(n424, n435);
    let n437: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n384);
    let n438: ZB = zn_le(n437, n388);
    let n439: ZB = zn_gt(n437, n388);
    let n440: ZB = zb_and(n436, n438);
    let n441: ZB = zb_and(n436, n439);
    let n442: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n437);
    let n443: ZN = zn_mget(g.cart, n394, n442);
    let n444: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n443);
    let n445: ZN = zn_mul(n437, zn_splat(P8::from_raw(524288i32)));
    let n446: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n445);
    let n447: ZB = zn_eq(n385, n446);
    let n448: ZB = zb_or(n399, n447);
    let n449: ZB = zb_and(n444, n448);
    let n450: ZB = zb_and(n405, n449);
    let n451: ZB = zb_not(n450);
    let n452: ZB = zb_and(n440, n451);
    let n453: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n443);
    let n454: ZB = zb_and(n411, n453);
    let n455: ZB = zb_and(n413, n454);
    let n456: ZB = zb_not(n455);
    let n457: ZB = zb_and(n452, n456);
    let n458: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n443);
    let n459: ZB = zb_and(n419, n458);
    let n460: ZB = zb_and(n421, n459);
    let n461: ZB = zb_not(n460);
    let n462: ZB = zb_and(n457, n461);
    let n463: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n443);
    let n464: ZB = zb_and(n431, n463);
    let n465: ZB = zb_and(n433, n464);
    let n466: ZB = zb_not(n465);
    let n467: ZB = zb_and(n462, n466);
    let n468: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n384);
    let n469: ZB = zn_le(n468, n388);
    let n470: ZB = zn_gt(n468, n388);
    let n471: ZB = zb_and(n467, n469);
    let n472: ZB = zb_and(n467, n470);
    let n473: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n468);
    let n474: ZN = zn_mget(g.cart, n394, n473);
    let n475: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n474);
    let n476: ZN = zn_mul(n468, zn_splat(P8::from_raw(524288i32)));
    let n477: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n476);
    let n478: ZB = zn_eq(n385, n477);
    let n479: ZB = zb_or(n399, n478);
    let n480: ZB = zb_and(n475, n479);
    let n481: ZB = zb_and(n405, n480);
    let n482: ZB = zb_not(n481);
    let n483: ZB = zb_and(n471, n482);
    let n484: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n474);
    let n485: ZB = zb_and(n411, n484);
    let n486: ZB = zb_and(n413, n485);
    let n487: ZB = zb_not(n486);
    let n488: ZB = zb_and(n483, n487);
    let n489: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n474);
    let n490: ZB = zb_and(n419, n489);
    let n491: ZB = zb_and(n421, n490);
    let n492: ZB = zb_not(n491);
    let n493: ZB = zb_and(n488, n492);
    let n494: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n474);
    let n495: ZB = zb_and(n431, n494);
    let n496: ZB = zb_and(n433, n495);
    let n497: ZB = zb_not(n496);
    let n498: ZB = zb_and(n493, n497);
    let n499: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n384);
    let n500: ZB = zn_gt(n499, n388);
    let n501: ZB = zb_and(n367, n500);
    let n502: ZB = zb_or(n472, n498);
    let n503: ZB = zsel_b(n470, n367, n501);
    let n504: ZB = zb_or(n441, n502);
    let n505: ZB = zsel_b(n439, n367, n503);
    let n506: ZB = zb_or(n393, n504);
    let n507: ZB = zsel_b(n391, n367, n505);
    let n508: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n372);
    let n509: ZB = zn_le(n508, n376);
    let n510: ZB = zn_gt(n508, n376);
    let n511: ZB = zb_and(n506, n509);
    let n512: ZB = zb_and(n506, n510);
    let n513: ZB = zb_and(n391, n511);
    let n514: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n508);
    let n515: ZN = zn_mget(g.cart, n514, n395);
    let n516: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n515);
    let n517: ZB = zb_and(n390, n506);
    let n518: ZB = zb_and(n509, n517);
    let n519: ZB = zb_and(n403, n516);
    let n520: ZB = zb_and(n405, n519);
    let n521: ZB = zb_not(n520);
    let n522: ZB = zb_and(n518, n521);
    let n523: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n515);
    let n524: ZB = zb_and(n411, n523);
    let n525: ZB = zb_and(n413, n524);
    let n526: ZB = zb_not(n525);
    let n527: ZB = zb_and(n522, n526);
    let n528: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n515);
    let n529: ZB = zb_and(n419, n528);
    let n530: ZB = zb_and(n421, n529);
    let n531: ZB = zb_not(n530);
    let n532: ZB = zb_and(n527, n531);
    let n533: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n515);
    let n534: ZN = zn_mul(n508, zn_splat(P8::from_raw(524288i32)));
    let n535: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n534);
    let n536: ZB = zn_eq(n373, n535);
    let n537: ZB = zb_or(n427, n536);
    let n538: ZB = zb_and(n533, n537);
    let n539: ZB = zb_and(n433, n538);
    let n540: ZB = zb_not(n539);
    let n541: ZB = zb_and(n532, n540);
    let n542: ZB = zb_and(n439, n541);
    let n543: ZN = zn_mget(g.cart, n514, n442);
    let n544: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n543);
    let n545: ZB = zb_and(n438, n532);
    let n546: ZB = zb_and(n540, n545);
    let n547: ZB = zb_and(n448, n544);
    let n548: ZB = zb_and(n405, n547);
    let n549: ZB = zb_not(n548);
    let n550: ZB = zb_and(n546, n549);
    let n551: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n543);
    let n552: ZB = zb_and(n411, n551);
    let n553: ZB = zb_and(n413, n552);
    let n554: ZB = zb_not(n553);
    let n555: ZB = zb_and(n550, n554);
    let n556: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n543);
    let n557: ZB = zb_and(n419, n556);
    let n558: ZB = zb_and(n421, n557);
    let n559: ZB = zb_not(n558);
    let n560: ZB = zb_and(n555, n559);
    let n561: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n543);
    let n562: ZB = zb_and(n537, n561);
    let n563: ZB = zb_and(n433, n562);
    let n564: ZB = zb_not(n563);
    let n565: ZB = zb_and(n560, n564);
    let n566: ZB = zb_and(n470, n565);
    let n567: ZN = zn_mget(g.cart, n514, n473);
    let n568: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n567);
    let n569: ZB = zb_and(n469, n560);
    let n570: ZB = zb_and(n564, n569);
    let n571: ZB = zb_and(n479, n568);
    let n572: ZB = zb_and(n405, n571);
    let n573: ZB = zb_not(n572);
    let n574: ZB = zb_and(n570, n573);
    let n575: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n567);
    let n576: ZB = zb_and(n411, n575);
    let n577: ZB = zb_and(n413, n576);
    let n578: ZB = zb_not(n577);
    let n579: ZB = zb_and(n574, n578);
    let n580: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n567);
    let n581: ZB = zb_and(n419, n580);
    let n582: ZB = zb_and(n421, n581);
    let n583: ZB = zb_not(n582);
    let n584: ZB = zb_and(n579, n583);
    let n585: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n567);
    let n586: ZB = zb_and(n537, n585);
    let n587: ZB = zb_and(n433, n586);
    let n588: ZB = zb_not(n587);
    let n589: ZB = zb_and(n584, n588);
    let n590: ZB = zb_and(n500, n507);
    let n591: ZB = zb_or(n566, n589);
    let n592: ZB = zsel_b(n470, n507, n590);
    let n593: ZB = zb_or(n542, n591);
    let n594: ZB = zsel_b(n439, n507, n592);
    let n595: ZB = zb_or(n513, n593);
    let n596: ZB = zsel_b(n391, n507, n594);
    let n597: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n372);
    let n598: ZB = zn_le(n597, n376);
    let n599: ZB = zn_gt(n597, n376);
    let n600: ZB = zb_and(n595, n598);
    let n601: ZB = zb_and(n595, n599);
    let n602: ZB = zb_and(n391, n600);
    let n603: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n597);
    let n604: ZN = zn_mget(g.cart, n603, n395);
    let n605: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n604);
    let n606: ZB = zb_and(n390, n595);
    let n607: ZB = zb_and(n598, n606);
    let n608: ZB = zb_and(n403, n605);
    let n609: ZB = zb_and(n405, n608);
    let n610: ZB = zb_not(n609);
    let n611: ZB = zb_and(n607, n610);
    let n612: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n604);
    let n613: ZB = zb_and(n411, n612);
    let n614: ZB = zb_and(n413, n613);
    let n615: ZB = zb_not(n614);
    let n616: ZB = zb_and(n611, n615);
    let n617: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n604);
    let n618: ZB = zb_and(n419, n617);
    let n619: ZB = zb_and(n421, n618);
    let n620: ZB = zb_not(n619);
    let n621: ZB = zb_and(n616, n620);
    let n622: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n604);
    let n623: ZN = zn_mul(n597, zn_splat(P8::from_raw(524288i32)));
    let n624: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n623);
    let n625: ZB = zn_eq(n373, n624);
    let n626: ZB = zb_or(n427, n625);
    let n627: ZB = zb_and(n622, n626);
    let n628: ZB = zb_and(n433, n627);
    let n629: ZB = zb_not(n628);
    let n630: ZB = zb_and(n621, n629);
    let n631: ZB = zb_and(n439, n630);
    let n632: ZN = zn_mget(g.cart, n603, n442);
    let n633: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n632);
    let n634: ZB = zb_and(n438, n621);
    let n635: ZB = zb_and(n629, n634);
    let n636: ZB = zb_and(n448, n633);
    let n637: ZB = zb_and(n405, n636);
    let n638: ZB = zb_not(n637);
    let n639: ZB = zb_and(n635, n638);
    let n640: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n632);
    let n641: ZB = zb_and(n411, n640);
    let n642: ZB = zb_and(n413, n641);
    let n643: ZB = zb_not(n642);
    let n644: ZB = zb_and(n639, n643);
    let n645: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n632);
    let n646: ZB = zb_and(n419, n645);
    let n647: ZB = zb_and(n421, n646);
    let n648: ZB = zb_not(n647);
    let n649: ZB = zb_and(n644, n648);
    let n650: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n632);
    let n651: ZB = zb_and(n626, n650);
    let n652: ZB = zb_and(n433, n651);
    let n653: ZB = zb_not(n652);
    let n654: ZB = zb_and(n649, n653);
    let n655: ZB = zb_and(n470, n654);
    let n656: ZN = zn_mget(g.cart, n603, n473);
    let n657: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n656);
    let n658: ZB = zb_and(n469, n649);
    let n659: ZB = zb_and(n653, n658);
    let n660: ZB = zb_and(n479, n657);
    let n661: ZB = zb_and(n405, n660);
    let n662: ZB = zb_not(n661);
    let n663: ZB = zb_and(n659, n662);
    let n664: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n656);
    let n665: ZB = zb_and(n411, n664);
    let n666: ZB = zb_and(n413, n665);
    let n667: ZB = zb_not(n666);
    let n668: ZB = zb_and(n663, n667);
    let n669: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n656);
    let n670: ZB = zb_and(n419, n669);
    let n671: ZB = zb_and(n421, n670);
    let n672: ZB = zb_not(n671);
    let n673: ZB = zb_and(n668, n672);
    let n674: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n656);
    let n675: ZB = zb_and(n626, n674);
    let n676: ZB = zb_and(n433, n675);
    let n677: ZB = zb_not(n676);
    let n678: ZB = zb_and(n673, n677);
    let n679: ZB = zb_and(n500, n596);
    let n680: ZB = zb_or(n655, n678);
    let n681: ZB = zsel_b(n470, n596, n679);
    let n682: ZB = zb_or(n631, n680);
    let n683: ZB = zsel_b(n439, n596, n681);
    let n684: ZB = zb_or(n602, n682);
    let n685: ZB = zsel_b(n391, n596, n683);
    let n686: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n372);
    let n687: ZB = zn_gt(n686, n376);
    let n688: ZB = zb_and(n685, n687);
    let n689: ZB = zb_or(n601, n684);
    let n690: ZB = zsel_b(n599, n596, n688);
    let n691: ZB = zb_or(n512, n689);
    let n692: ZB = zsel_b(n510, n507, n690);
    let n693: ZB = zb_or(n381, n691);
    let n694: ZB = zsel_b(n379, n367, n692);
    let n695: ZB = zn_le(n362, zn_splat(P8::from_raw(8388608i32)));
    let n696: ZB = zb_and(n693, n695);
    let n697: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n368);
    let n698: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n369);
    let n699: ZB = zn_tile_flag_at(g.cache, g.cart, n697, n698, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n700: ZB = zb_not(n699);
    let n701: ZB = zb_not(r_c249);
    let n702: ZB = zn_lt(r_c239, zn_splat(P8::from_raw(65536i32)));
    let n703: ZN = zsel_n(n702, zn_splat(P8::from_raw(65536i32)), r_c239);
    let n704: ZB = zn_gt(r_c241, zn_splat(P8::from_raw(0i32)));
    let n705: ZN = zn_sub(r_c241, zn_splat(P8::from_raw(65536i32)));
    let n706: ZN = zsel_n(n704, n705, r_c241);
    let n707: ZN = zsel_n(n699, n703, r_c239);
    let n708: ZN = zsel_n(n699, zn_splat(P8::from_raw(393216i32)), n706);
    let n709: ZN = zn_sub(r_c236, zn_splat(P8::from_raw(65536i32)));
    let n710: ZB = zn_gt(r_c238, zn_splat(P8::from_raw(0i32)));
    let n711: ZN = zn_sub(r_c238, zn_splat(P8::from_raw(65536i32)));
    let n712: ZB = zn_gt(n365, r_c302);
    let n713: ZN = zn_sub(n365, r_c300);
    let n714: ZN = zn_max(r_c302, n713);
    let n715: ZN = zn_add(r_c300, n365);
    let n716: ZN = zn_min(r_c302, n715);
    let n717: ZN = zsel_n(n712, n714, n716);
    let n718: ZB = zn_gt(n366, r_c303);
    let n719: ZN = zn_sub(n366, r_c301);
    let n720: ZN = zn_max(r_c303, n719);
    let n721: ZN = zn_add(r_c301, n366);
    let n722: ZN = zn_min(r_c303, n721);
    let n723: ZN = zsel_n(n718, n720, n722);
    let n724: ZN = zsel_n(n700, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n725: ZN = zn_abs(n365);
    let n726: ZB = zn_gt(n725, zn_splat(P8::from_raw(65536i32)));
    let n727: ZB = zn_gt(n365, zn_splat(P8::from_raw(0i32)));
    let n728: ZB = zn_lt(n365, zn_splat(P8::from_raw(0i32)));
    let n729: ZB = zn_gt(n365, zn_splat(P8::from_raw(65536i32)));
    let n730: ZN = zn_sub(n365, zn_splat(P8::from_raw(9830i32)));
    let n731: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n730);
    let n732: ZN = zn_add(zn_splat(P8::from_raw(9830i32)), n365);
    let n733: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n732);
    let n734: ZB = zn_gt(n365, zn_splat(P8::from_raw(-65536i32)));
    let n735: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n730);
    let n736: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n732);
    let n737: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n730);
    let n738: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n732);
    let n739: ZN = zsel_n(n734, n735, n736);
    let n740: ZN = zsel_n(n727, n737, n738);
    let n741: ZN = zsel_n(n729, n731, n733);
    let n742: ZN = zsel_n(n728, n739, n740);
    let n743: ZN = zsel_n(n727, n741, n742);
    let n744: ZN = zn_sub(n365, n724);
    let n745: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n744);
    let n746: ZN = zn_add(n365, n724);
    let n747: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n746);
    let n748: ZN = zsel_n(n727, n745, n747);
    let n749: ZN = zsel_n(n726, n743, n748);
    let n750: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n749);
    let n751: ZB = zb_not(n750);
    let n752: ZB = zn_lt(n749, zn_splat(P8::from_raw(0i32)));
    let n753: ZB = zsel_b(n751, n752, r_c304);
    let n754: ZN = zn_abs(n366);
    let n755: ZB = zn_le(n754, zn_splat(P8::from_raw(9830i32)));
    let n756: ZN = zsel_n(n755, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(13762i32)));
    let n757: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n369);
    let n758: ZB = zn_gt(n366, zn_splat(P8::from_raw(131072i32)));
    let n759: ZN = zn_sub(n366, n756);
    let n760: ZN = zn_max(zn_splat(P8::from_raw(131072i32)), n759);
    let n761: ZN = zn_add(n366, n756);
    let n762: ZN = zn_min(zn_splat(P8::from_raw(131072i32)), n761);
    let n763: ZN = zsel_n(n758, n760, n762);
    let n764: ZN = zsel_n(n700, n763, n366);
    let n765: ZB = zn_gt(n708, zn_splat(P8::from_raw(0i32)));
    let n766: ZN = zn_add(zn_splat(P8::from_raw(-196608i32)), n368);
    let n767: ZB = zn_tile_flag_at(g.cache, g.cart, n766, n757, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n768: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n368);
    let n769: ZB = zn_tile_flag_at(g.cache, g.cart, n768, n757, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n770: ZN = zsel_n(n769, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n771: ZN = zsel_n(n767, zn_splat(P8::from_raw(-65536i32)), n770);
    let n772: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n771);
    let n773: ZB = zb_not(n772);
    let n774: ZN = zn_neg(n771);
    let n775: ZN = zn_mul(n774, zn_splat(P8::from_raw(131072i32)));
    let n776: ZN = zsel_n(n773, n775, n749);
    let n777: ZN = zsel_n(n773, zn_splat(P8::from_raw(-131072i32)), n764);
    let n778: ZN = zsel_n(n765, zn_splat(P8::from_raw(0i32)), n708);
    let n779: ZN = zsel_n(n765, n749, n776);
    let n780: ZN = zsel_n(n765, zn_splat(P8::from_raw(-131072i32)), n777);
    let n781: ZB = zn_gt(n707, zn_splat(P8::from_raw(0i32)));
    let n782: ZN = zsel_n(n753, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n783: ZB = zn_gt(n782, zn_splat(P8::from_raw(0i32)));
    let n784: ZB = zn_lt(n782, zn_splat(P8::from_raw(0i32)));
    let n785: ZN = zsel_n(n784, zn_splat(P8::from_raw(-131072i32)), zn_splat(P8::from_raw(0i32)));
    let n786: ZN = zsel_n(n783, zn_splat(P8::from_raw(131072i32)), n785);
    let n787: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n782);
    let n788: ZB = zb_not(n787);
    let n789: ZN = zsel_n(n788, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n790: ZN = zsel_n(n710, n711, r_c238);
    let n791: ZB = zsel_b(n710, r_c304, n753);
    let n792: ZN = zsel_n(n710, n717, n749);
    let n793: ZN = zsel_n(n710, n723, n764);
    let n794: ZB = zn_lt(n362, zn_splat(P8::from_raw(-262144i32)));
    let n795: ZB = zn_ge(n362, zn_splat(P8::from_raw(-262144i32)));
    let n796: ZB = zb_and(n696, n794);
    let n797: ZB = zb_and(n696, n795);
    let n798: ZB = zn_gt(n373, zn_splat(P8::from_raw(786432i32)));
    let n800: ZN = zn_add(zn_splat(P8::from_raw(0i32)), r_c275);
    let n801: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n800);
    let n802: ZB = zn_gt(n385, n801);
    let n803: ZB = zb_and(n798, n802);
    let n804: ZB = zn_lt(n368, zn_splat(P8::from_raw(1310720i32)));
    let n805: ZB = zb_and(n803, n804);
    let n806: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n800);
    let n807: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n806);
    let n808: ZB = zn_lt(n369, n807);
    let n809: ZB = zb_and(n805, n808);
    let n810: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n811: ZB = zn_lt(n361, zn_splat(P8::from_raw(-65536i32)));
    let n812: ZB = zn_gt(n361, zn_splat(P8::from_raw(7929856i32)));
    let n818: ZB = zb_or(n811, n812);
    let n819: ZN = zn_min(zn_splat(P8::from_raw(7929856i32)), n361);
    let n820: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n819);
    let n821: ZN = zsel_n(n818, n820, n361);
    let n822: ZN = zsel_n(n818, zn_splat(P8::from_raw(0i32)), n792);
    let n823: ZN = zsel_n(n810, n361, n821);
    let n824: ZN = zsel_n(n810, n792, n822);
    let n851: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n744);
    let n852: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n746);
    let n853: ZN = zsel_n(n734, n851, n852);
    let n854: ZN = zsel_n(n726, n743, n853);
    let n855: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n854);
    let n856: ZB = zb_not(n855);
    let n857: ZB = zn_lt(n854, zn_splat(P8::from_raw(0i32)));
    let n858: ZB = zsel_b(n856, n857, r_c304);
    let n859: ZN = zn_add(zn_splat(P8::from_raw(-65536i32)), n368);
    let n860: ZB = zn_tile_flag_at(g.cache, g.cart, n859, n757, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n861: ZN = zsel_n(n860, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n862: ZB = zn_gt(n366, n861);
    let n863: ZN = zn_max(n759, n861);
    let n864: ZN = zn_min(n761, n861);
    let n865: ZN = zsel_n(n862, n863, n864);
    let n866: ZN = zsel_n(n700, n865, n366);
    let n867: ZN = zsel_n(n773, n775, n854);
    let n868: ZN = zsel_n(n773, zn_splat(P8::from_raw(-131072i32)), n866);
    let n869: ZN = zsel_n(n765, n854, n867);
    let n870: ZN = zsel_n(n765, zn_splat(P8::from_raw(-131072i32)), n868);
    let n871: ZB = zsel_b(n710, r_c304, n858);
    let n872: ZN = zsel_n(n710, n717, n854);
    let n873: ZN = zsel_n(n710, n723, n866);
    let n874: ZN = zsel_n(n818, zn_splat(P8::from_raw(0i32)), n872);
    let n875: ZN = zsel_n(n810, n872, n874);
    let n876: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n744);
    let n877: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n746);
    let n878: ZN = zsel_n(n729, n876, n877);
    let n879: ZN = zsel_n(n726, n743, n878);
    let n880: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n879);
    let n881: ZB = zb_not(n880);
    let n882: ZB = zn_lt(n879, zn_splat(P8::from_raw(0i32)));
    let n883: ZB = zsel_b(n881, n882, r_c304);
    let n884: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n368);
    let n885: ZB = zn_tile_flag_at(g.cache, g.cart, n884, n757, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n886: ZN = zsel_n(n885, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n887: ZB = zn_gt(n366, n886);
    let n888: ZN = zn_max(n759, n886);
    let n889: ZN = zn_min(n761, n886);
    let n890: ZN = zsel_n(n887, n888, n889);
    let n891: ZN = zsel_n(n700, n890, n366);
    let n892: ZN = zsel_n(n773, n775, n879);
    let n893: ZN = zsel_n(n773, zn_splat(P8::from_raw(-131072i32)), n891);
    let n894: ZN = zsel_n(n765, n879, n892);
    let n895: ZN = zsel_n(n765, zn_splat(P8::from_raw(-131072i32)), n893);
    let n896: ZB = zsel_b(n710, r_c304, n883);
    let n897: ZN = zsel_n(n710, n717, n879);
    let n898: ZN = zsel_n(n710, n723, n891);
    let n899: ZN = zsel_n(n818, zn_splat(P8::from_raw(0i32)), n897);
    let n900: ZN = zsel_n(n810, n897, n899);
    let n901: ZN = zsel_n(n701, n778, n708);
    let n902: ZN = zsel_n(n701, n779, n749);
    let n903: ZN = zsel_n(n701, n780, n764);
    let n904: ZN = zsel_n(n710, n708, n901);
    let n905: ZN = zsel_n(n710, n717, n902);
    let n906: ZN = zsel_n(n710, n723, n903);
    let n912: ZN = zsel_n(n818, zn_splat(P8::from_raw(0i32)), n905);
    let n913: ZN = zsel_n(n810, n905, n912);
    let n914: ZN = zsel_n(n701, n869, n854);
    let n915: ZN = zsel_n(n701, n870, n866);
    let n916: ZN = zsel_n(n710, n717, n914);
    let n917: ZN = zsel_n(n710, n723, n915);
    let n918: ZN = zsel_n(n818, zn_splat(P8::from_raw(0i32)), n916);
    let n919: ZN = zsel_n(n810, n916, n918);
    let n920: ZN = zsel_n(n701, n894, n879);
    let n921: ZN = zsel_n(n701, n895, n891);
    let n922: ZN = zsel_n(n710, n717, n920);
    let n923: ZN = zsel_n(n710, n723, n921);
    let n924: ZN = zsel_n(n818, zn_splat(P8::from_raw(0i32)), n922);
    let n925: ZN = zsel_n(n810, n922, n924);
    let n926: ZB = zb_and(n97, n781);
    let n927: ZN = zsel_n(n926, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n928: ZB = zb_or(r_c41, n926);
    let n929: ZN = zsel_n(n926, zn_splat(P8::from_raw(655360i32)), n709);
    let n930: ZN = zsel_n(n926, zn_splat(P8::from_raw(262144i32)), r_c238);
    let n931: ZN = zsel_n(n926, zn_splat(P8::from_raw(98304i32)), r_c300);
    let n932: ZN = zsel_n(n926, n789, r_c301);
    let n933: ZN = zsel_n(n926, n786, r_c302);
    let n934: ZN = zsel_n(n926, zn_splat(P8::from_raw(0i32)), r_c303);
    let n935: ZN = zsel_n(n926, n782, n749);
    let n936: ZN = zsel_n(n926, zn_splat(P8::from_raw(0i32)), n764);
    let n937: ZN = zsel_n(n710, r_c20, n927);
    let n938: ZB = zsel_b(n710, r_c41, n928);
    let n939: ZN = zsel_n(n710, n709, n929);
    let n940: ZN = zsel_n(n710, n711, n930);
    let n941: ZN = zsel_n(n710, r_c300, n931);
    let n942: ZN = zsel_n(n710, r_c301, n932);
    let n943: ZN = zsel_n(n710, r_c302, n933);
    let n944: ZN = zsel_n(n710, r_c303, n934);
    let n945: ZN = zsel_n(n710, n717, n935);
    let n946: ZN = zsel_n(n710, n723, n936);
    let n947: ZB = zn_gt(n937, zn_splat(P8::from_raw(0i32)));
    let n948: ZN = zsel_n(n818, zn_splat(P8::from_raw(0i32)), n945);
    let n949: ZN = zsel_n(n947, n361, n821);
    let n950: ZN = zsel_n(n947, n945, n948);
    let n954: ZN = zsel_n(n926, zn_splat(P8::from_raw(69510i32)), r_c301);
    let n955: ZN = zsel_n(n926, zn_splat(P8::from_raw(-131072i32)), r_c302);
    let n956: ZN = zsel_n(n926, zn_splat(P8::from_raw(-327680i32)), n854);
    let n957: ZN = zsel_n(n926, zn_splat(P8::from_raw(0i32)), n866);
    let n958: ZN = zsel_n(n710, r_c301, n954);
    let n959: ZN = zsel_n(n710, r_c302, n955);
    let n960: ZN = zsel_n(n710, n717, n956);
    let n961: ZN = zsel_n(n710, n723, n957);
    let n962: ZN = zsel_n(n818, zn_splat(P8::from_raw(0i32)), n960);
    let n963: ZN = zsel_n(n947, n960, n962);
    let n964: ZN = zsel_n(n926, zn_splat(P8::from_raw(131072i32)), r_c302);
    let n965: ZN = zsel_n(n926, zn_splat(P8::from_raw(327680i32)), n879);
    let n966: ZN = zsel_n(n926, zn_splat(P8::from_raw(0i32)), n891);
    let n967: ZN = zsel_n(n710, r_c302, n964);
    let n968: ZN = zsel_n(n710, n717, n965);
    let n969: ZN = zsel_n(n710, n723, n966);
    let n970: ZN = zsel_n(n818, zn_splat(P8::from_raw(0i32)), n968);
    let n971: ZN = zsel_n(n947, n968, n970);
    let n973: ZN = zsel_n(n926, zn_splat(P8::from_raw(69510i32)), r_c300);
    let n974: ZN = zsel_n(n926, zn_splat(P8::from_raw(98304i32)), r_c301);
    let n975: ZN = zsel_n(n926, zn_splat(P8::from_raw(0i32)), r_c302);
    let n976: ZN = zsel_n(n926, zn_splat(P8::from_raw(-98304i32)), r_c303);
    let n977: ZN = zsel_n(n926, zn_splat(P8::from_raw(0i32)), n749);
    let n978: ZN = zsel_n(n926, zn_splat(P8::from_raw(-327680i32)), n764);
    let n979: ZN = zsel_n(n710, r_c300, n973);
    let n980: ZN = zsel_n(n710, r_c301, n974);
    let n981: ZN = zsel_n(n710, r_c302, n975);
    let n982: ZN = zsel_n(n710, r_c303, n976);
    let n983: ZN = zsel_n(n710, n717, n977);
    let n984: ZN = zsel_n(n710, n723, n978);
    let n985: ZN = zsel_n(n818, zn_splat(P8::from_raw(0i32)), n983);
    let n986: ZN = zsel_n(n947, n983, n985);
    let n987: ZN = zsel_n(n926, zn_splat(P8::from_raw(-231700i32)), n854);
    let n988: ZN = zsel_n(n926, zn_splat(P8::from_raw(-231700i32)), n866);
    let n989: ZN = zsel_n(n710, n717, n987);
    let n990: ZN = zsel_n(n710, n723, n988);
    let n991: ZN = zsel_n(n818, zn_splat(P8::from_raw(0i32)), n989);
    let n992: ZN = zsel_n(n947, n989, n991);
    let n993: ZN = zsel_n(n926, zn_splat(P8::from_raw(231700i32)), n879);
    let n994: ZN = zsel_n(n926, zn_splat(P8::from_raw(-231700i32)), n891);
    let n995: ZN = zsel_n(n710, n717, n993);
    let n996: ZN = zsel_n(n710, n723, n994);
    let n997: ZN = zsel_n(n818, zn_splat(P8::from_raw(0i32)), n995);
    let n998: ZN = zsel_n(n947, n995, n997);
    let n999: ZN = zsel_n(n926, zn_splat(P8::from_raw(131072i32)), r_c303);
    let n1000: ZN = zsel_n(n926, zn_splat(P8::from_raw(327680i32)), n764);
    let n1001: ZN = zsel_n(n710, r_c303, n999);
    let n1002: ZN = zsel_n(n710, n723, n1000);
    let n1003: ZN = zsel_n(n926, zn_splat(P8::from_raw(231700i32)), n866);
    let n1004: ZN = zsel_n(n710, n723, n1003);
    let n1005: ZN = zsel_n(n926, zn_splat(P8::from_raw(231700i32)), n891);
    let n1006: ZN = zsel_n(n710, n723, n1005);
    let n1007: ZN = zsel_n(n926, n782, n902);
    let n1008: ZN = zsel_n(n926, zn_splat(P8::from_raw(0i32)), n903);
    let n1009: ZN = zsel_n(n710, n717, n1007);
    let n1010: ZN = zsel_n(n710, n723, n1008);
    let n1011: ZN = zsel_n(n818, zn_splat(P8::from_raw(0i32)), n1009);
    let n1012: ZN = zsel_n(n947, n1009, n1011);
    let n1018: ZN = zsel_n(n926, zn_splat(P8::from_raw(-327680i32)), n914);
    let n1019: ZN = zsel_n(n926, zn_splat(P8::from_raw(0i32)), n915);
    let n1020: ZN = zsel_n(n710, n717, n1018);
    let n1021: ZN = zsel_n(n710, n723, n1019);
    let n1022: ZN = zsel_n(n818, zn_splat(P8::from_raw(0i32)), n1020);
    let n1023: ZN = zsel_n(n947, n1020, n1022);
    let n1024: ZN = zsel_n(n926, zn_splat(P8::from_raw(327680i32)), n920);
    let n1025: ZN = zsel_n(n926, zn_splat(P8::from_raw(0i32)), n921);
    let n1026: ZN = zsel_n(n710, n717, n1024);
    let n1027: ZN = zsel_n(n710, n723, n1025);
    let n1028: ZN = zsel_n(n818, zn_splat(P8::from_raw(0i32)), n1026);
    let n1029: ZN = zsel_n(n947, n1026, n1028);
    let n1030: ZN = zsel_n(n926, zn_splat(P8::from_raw(0i32)), n902);
    let n1031: ZN = zsel_n(n926, zn_splat(P8::from_raw(-327680i32)), n903);
    let n1032: ZN = zsel_n(n710, n717, n1030);
    let n1033: ZN = zsel_n(n710, n723, n1031);
    let n1034: ZN = zsel_n(n818, zn_splat(P8::from_raw(0i32)), n1032);
    let n1035: ZN = zsel_n(n947, n1032, n1034);
    let n1036: ZN = zsel_n(n926, zn_splat(P8::from_raw(-231700i32)), n914);
    let n1037: ZN = zsel_n(n926, zn_splat(P8::from_raw(-231700i32)), n915);
    let n1038: ZN = zsel_n(n710, n717, n1036);
    let n1039: ZN = zsel_n(n710, n723, n1037);
    let n1040: ZN = zsel_n(n818, zn_splat(P8::from_raw(0i32)), n1038);
    let n1041: ZN = zsel_n(n947, n1038, n1040);
    let n1042: ZN = zsel_n(n926, zn_splat(P8::from_raw(231700i32)), n920);
    let n1043: ZN = zsel_n(n926, zn_splat(P8::from_raw(-231700i32)), n921);
    let n1044: ZN = zsel_n(n710, n717, n1042);
    let n1045: ZN = zsel_n(n710, n723, n1043);
    let n1046: ZN = zsel_n(n818, zn_splat(P8::from_raw(0i32)), n1044);
    let n1047: ZN = zsel_n(n947, n1044, n1046);
    let n1048: ZN = zsel_n(n926, zn_splat(P8::from_raw(327680i32)), n903);
    let n1049: ZN = zsel_n(n710, n723, n1048);
    let n1050: ZN = zsel_n(n926, zn_splat(P8::from_raw(231700i32)), n915);
    let n1051: ZN = zsel_n(n710, n723, n1050);
    let n1052: ZN = zsel_n(n926, zn_splat(P8::from_raw(231700i32)), n921);
    let n1053: ZN = zsel_n(n710, n723, n1052);
    let n1057: ZB = zb_and(n392, n406);
    let n1058: ZB = zb_and(n408, n414);
    let n1059: ZB = zb_and(n416, n422);
    let n1060: ZB = zb_and(n424, n434);
    let n1061: ZB = zb_or(n1059, n1060);
    let n1062: ZB = zb_or(n1058, n1061);
    let n1063: ZB = zb_or(n1057, n1062);
    let n1064: ZB = zb_and(n440, n450);
    let n1065: ZB = zb_and(n452, n455);
    let n1066: ZB = zb_and(n457, n460);
    let n1067: ZB = zb_and(n462, n465);
    let n1068: ZB = zb_or(n1066, n1067);
    let n1069: ZB = zb_or(n1065, n1068);
    let n1070: ZB = zb_or(n1064, n1069);
    let n1071: ZB = zb_and(n471, n481);
    let n1072: ZB = zb_and(n483, n486);
    let n1073: ZB = zb_and(n488, n491);
    let n1074: ZB = zb_and(n493, n496);
    let n1075: ZB = zb_or(n1073, n1074);
    let n1076: ZB = zb_or(n1072, n1075);
    let n1077: ZB = zb_or(n1071, n1076);
    let n1078: ZB = zb_or(n1070, n1077);
    let n1079: ZB = zb_or(n1063, n1078);
    let n1080: ZB = zb_and(n518, n520);
    let n1081: ZB = zb_and(n522, n525);
    let n1082: ZB = zb_and(n527, n530);
    let n1083: ZB = zb_and(n532, n539);
    let n1084: ZB = zb_or(n1082, n1083);
    let n1085: ZB = zb_or(n1081, n1084);
    let n1086: ZB = zb_or(n1080, n1085);
    let n1087: ZB = zb_and(n546, n548);
    let n1088: ZB = zb_and(n550, n553);
    let n1089: ZB = zb_and(n555, n558);
    let n1090: ZB = zb_and(n560, n563);
    let n1091: ZB = zb_or(n1089, n1090);
    let n1092: ZB = zb_or(n1088, n1091);
    let n1093: ZB = zb_or(n1087, n1092);
    let n1094: ZB = zb_and(n570, n572);
    let n1095: ZB = zb_and(n574, n577);
    let n1096: ZB = zb_and(n579, n582);
    let n1097: ZB = zb_and(n584, n587);
    let n1098: ZB = zb_or(n1096, n1097);
    let n1099: ZB = zb_or(n1095, n1098);
    let n1100: ZB = zb_or(n1094, n1099);
    let n1101: ZB = zb_or(n1093, n1100);
    let n1102: ZB = zb_or(n1086, n1101);
    let n1103: ZB = zb_and(n607, n609);
    let n1104: ZB = zb_and(n611, n614);
    let n1105: ZB = zb_and(n616, n619);
    let n1106: ZB = zb_and(n621, n628);
    let n1107: ZB = zb_or(n1105, n1106);
    let n1108: ZB = zb_or(n1104, n1107);
    let n1109: ZB = zb_or(n1103, n1108);
    let n1110: ZB = zb_and(n635, n637);
    let n1111: ZB = zb_and(n639, n642);
    let n1112: ZB = zb_and(n644, n647);
    let n1113: ZB = zb_and(n649, n652);
    let n1114: ZB = zb_or(n1112, n1113);
    let n1115: ZB = zb_or(n1111, n1114);
    let n1116: ZB = zb_or(n1110, n1115);
    let n1117: ZB = zb_and(n659, n661);
    let n1118: ZB = zb_and(n663, n666);
    let n1119: ZB = zb_and(n668, n671);
    let n1120: ZB = zb_and(n673, n676);
    let n1121: ZB = zb_or(n1119, n1120);
    let n1122: ZB = zb_or(n1118, n1121);
    let n1123: ZB = zb_or(n1117, n1122);
    let n1124: ZB = zb_or(n1116, n1123);
    let n1125: ZB = zb_or(n1109, n1124);
    let n1126: ZB = zb_or(n1102, n1125);
    let n1127: ZB = zsel_b(n1102, n507, n596);
    let n1128: ZB = zb_or(n1079, n1126);
    let n1129: ZB = zsel_b(n1079, n367, n1127);
    let n1130: ZB = zn_gt(n362, zn_splat(P8::from_raw(8388608i32)));
    let n1131: ZB = zb_and(n693, n1130);
    let n1132: ZB = zb_or(n1128, n1131);
    let n1133: ZB = zsel_b(n1128, n1129, n694);
    let n1134: ZB = zb_and(n794, n1132);
    let n1136: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c267);
    let n1137: ZN = zn_div(n1136, zn_splat(P8::from_raw(2621440i32)));
    let n1138: ZN = zn_sin(n1137);
    let n1139: ZN = zn_mul(n1138, zn_splat(P8::from_raw(163840i32)));
    let n1140: ZN = zn_add(zn_splat(P8::from_raw(2359296i32)), n1139);
    let n1141: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c87);
    let n1142: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1141);
    let n1143: ZN = zsel_n(n1130, n1142, n1141);
    let n1144: ZN = zsel_n(n1128, n1143, n1141);
    let n1148: ZB = zb_not(n796);
    let n1149: ZB = zb_or(n796, n1134);
    let n1150: ZB = zsel_b(n796, n694, n1133);
    let n1151: ZN = zsel_n(n796, r_c87, n1144);
    let n1152: ZN = zsel_n(n796, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1154: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n1155: ZN = zn_sub(n707, zn_splat(P8::from_raw(65536i32)));
    let n1156: ZB = zb_not(n809);
    let n1157: ZB = zb_and(n797, n1156);
    let n1158: ZN = zsel_n(n810, n1154, r_c20);
    let n1159: ZN = zsel_n(n810, r_c236, n709);
    let n1160: ZN = zsel_n(n810, r_c238, n790);
    let n1161: ZN = zsel_n(n810, r_c239, n707);
    let n1162: ZN = zsel_n(n810, r_c241, n708);
    let n1163: ZB = zb_and(r_c248, n810);
    let n1164: ZB = zb_and(r_c249, n810);
    let n1165: ZN = zsel_n(n810, r_c255, n361);
    let n1166: ZN = zsel_n(n810, r_c256, n362);
    let n1167: ZN = zsel_n(n810, r_c267, n1136);
    let n1168: ZN = zsel_n(n810, r_c275, n1140);
    let n1169: ZB = zsel_b(n810, r_c304, n791);
    let n1170: ZN = zsel_n(n810, r_c310, n363);
    let n1171: ZN = zsel_n(n810, r_c311, n364);
    let n1172: ZN = zsel_n(n810, r_c312, n792);
    let n1173: ZN = zsel_n(n810, r_c313, n793);
    let n1174: ZB = zb_or(n810, n1157);
    let n1175: ZB = zb_or(n694, n810);
    let n1176: ZB = zn_gt(n1158, zn_splat(P8::from_raw(0i32)));
    let n1177: ZB = zn_lt(n1165, zn_splat(P8::from_raw(-65536i32)));
    let n1178: ZB = zn_gt(n1165, zn_splat(P8::from_raw(7929856i32)));
    let n1179: ZB = zb_or(n1177, n1178);
    let n1180: ZN = zn_min(zn_splat(P8::from_raw(7929856i32)), n1165);
    let n1181: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1180);
    let n1182: ZN = zsel_n(n1179, n1181, n1165);
    let n1183: ZN = zsel_n(n1179, zn_splat(P8::from_raw(0i32)), n1172);
    let n1184: ZN = zsel_n(n1176, n1165, n1182);
    let n1185: ZN = zsel_n(n1176, n1172, n1183);
    let n1187: ZB = zsel_b(n810, r_c304, n871);
    let n1188: ZN = zsel_n(n810, r_c312, n872);
    let n1189: ZN = zsel_n(n810, r_c313, n873);
    let n1190: ZN = zsel_n(n1179, zn_splat(P8::from_raw(0i32)), n1188);
    let n1191: ZN = zsel_n(n1176, n1188, n1190);
    let n1192: ZB = zsel_b(n810, r_c304, n896);
    let n1193: ZN = zsel_n(n810, r_c312, n897);
    let n1194: ZN = zsel_n(n810, r_c313, n898);
    let n1195: ZN = zsel_n(n1179, zn_splat(P8::from_raw(0i32)), n1193);
    let n1196: ZN = zsel_n(n1176, n1193, n1195);
    let n1197: ZN = zsel_n(n810, r_c241, n904);
    let n1198: ZB = zb_or(r_c249, n99);
    let n1199: ZN = zsel_n(n810, r_c312, n905);
    let n1200: ZN = zsel_n(n810, r_c313, n906);
    let n1201: ZN = zsel_n(n1179, zn_splat(P8::from_raw(0i32)), n1199);
    let n1202: ZN = zsel_n(n1176, n1199, n1201);
    let n1203: ZN = zsel_n(n810, r_c312, n916);
    let n1204: ZN = zsel_n(n810, r_c313, n917);
    let n1205: ZN = zsel_n(n1179, zn_splat(P8::from_raw(0i32)), n1203);
    let n1206: ZN = zsel_n(n1176, n1203, n1205);
    let n1207: ZN = zsel_n(n810, r_c312, n922);
    let n1208: ZN = zsel_n(n810, r_c313, n923);
    let n1209: ZN = zsel_n(n1179, zn_splat(P8::from_raw(0i32)), n1207);
    let n1210: ZN = zsel_n(n1176, n1207, n1209);
    let n1211: ZN = zsel_n(n926, n1155, n707);
    let n1212: ZN = zsel_n(n710, n707, n1211);
    let n1213: ZN = zsel_n(n810, n1154, n937);
    let n1214: ZB = zsel_b(n810, r_c41, n938);
    let n1215: ZN = zsel_n(n810, r_c236, n939);
    let n1216: ZN = zsel_n(n810, r_c238, n940);
    let n1217: ZN = zsel_n(n810, r_c239, n1212);
    let n1218: ZB = zb_or(r_c248, n99);
    let n1219: ZN = zsel_n(n810, r_c300, n941);
    let n1220: ZN = zsel_n(n810, r_c301, n942);
    let n1221: ZN = zsel_n(n810, r_c302, n943);
    let n1222: ZN = zsel_n(n810, r_c303, n944);
    let n1223: ZN = zsel_n(n810, r_c312, n945);
    let n1224: ZN = zsel_n(n810, r_c313, n946);
    let n1225: ZB = zn_gt(n1213, zn_splat(P8::from_raw(0i32)));
    let n1226: ZN = zsel_n(n1179, zn_splat(P8::from_raw(0i32)), n1223);
    let n1227: ZN = zsel_n(n1225, n1165, n1182);
    let n1228: ZN = zsel_n(n1225, n1223, n1226);
    let n1229: ZN = zsel_n(n810, r_c301, n958);
    let n1230: ZN = zsel_n(n810, r_c302, n959);
    let n1231: ZN = zsel_n(n810, r_c312, n960);
    let n1232: ZN = zsel_n(n810, r_c313, n961);
    let n1233: ZN = zsel_n(n1179, zn_splat(P8::from_raw(0i32)), n1231);
    let n1234: ZN = zsel_n(n1225, n1231, n1233);
    let n1235: ZN = zsel_n(n810, r_c302, n967);
    let n1236: ZN = zsel_n(n810, r_c312, n968);
    let n1237: ZN = zsel_n(n810, r_c313, n969);
    let n1238: ZN = zsel_n(n1179, zn_splat(P8::from_raw(0i32)), n1236);
    let n1239: ZN = zsel_n(n1225, n1236, n1238);
    let n1240: ZN = zsel_n(n810, r_c300, n979);
    let n1241: ZN = zsel_n(n810, r_c301, n980);
    let n1242: ZN = zsel_n(n810, r_c302, n981);
    let n1243: ZN = zsel_n(n810, r_c303, n982);
    let n1244: ZN = zsel_n(n810, r_c312, n983);
    let n1245: ZN = zsel_n(n810, r_c313, n984);
    let n1246: ZN = zsel_n(n1179, zn_splat(P8::from_raw(0i32)), n1244);
    let n1247: ZN = zsel_n(n1225, n1244, n1246);
    let n1248: ZN = zsel_n(n810, r_c312, n989);
    let n1249: ZN = zsel_n(n810, r_c313, n990);
    let n1250: ZN = zsel_n(n1179, zn_splat(P8::from_raw(0i32)), n1248);
    let n1251: ZN = zsel_n(n1225, n1248, n1250);
    let n1252: ZN = zsel_n(n810, r_c312, n995);
    let n1253: ZN = zsel_n(n810, r_c313, n996);
    let n1254: ZN = zsel_n(n1179, zn_splat(P8::from_raw(0i32)), n1252);
    let n1255: ZN = zsel_n(n1225, n1252, n1254);
    let n1256: ZN = zsel_n(n810, r_c303, n1001);
    let n1257: ZN = zsel_n(n810, r_c313, n1002);
    let n1258: ZN = zsel_n(n810, r_c313, n1004);
    let n1259: ZN = zsel_n(n810, r_c313, n1006);
    let n1260: ZN = zsel_n(n810, r_c312, n1009);
    let n1261: ZN = zsel_n(n810, r_c313, n1010);
    let n1262: ZN = zsel_n(n1179, zn_splat(P8::from_raw(0i32)), n1260);
    let n1263: ZN = zsel_n(n1225, n1260, n1262);
    let n1264: ZN = zsel_n(n810, r_c312, n1020);
    let n1265: ZN = zsel_n(n810, r_c313, n1021);
    let n1266: ZN = zsel_n(n1179, zn_splat(P8::from_raw(0i32)), n1264);
    let n1267: ZN = zsel_n(n1225, n1264, n1266);
    let n1268: ZN = zsel_n(n810, r_c312, n1026);
    let n1269: ZN = zsel_n(n810, r_c313, n1027);
    let n1270: ZN = zsel_n(n1179, zn_splat(P8::from_raw(0i32)), n1268);
    let n1271: ZN = zsel_n(n1225, n1268, n1270);
    let n1272: ZN = zsel_n(n810, r_c312, n1032);
    let n1273: ZN = zsel_n(n810, r_c313, n1033);
    let n1274: ZN = zsel_n(n1179, zn_splat(P8::from_raw(0i32)), n1272);
    let n1275: ZN = zsel_n(n1225, n1272, n1274);
    let n1276: ZN = zsel_n(n810, r_c312, n1038);
    let n1277: ZN = zsel_n(n810, r_c313, n1039);
    let n1278: ZN = zsel_n(n1179, zn_splat(P8::from_raw(0i32)), n1276);
    let n1279: ZN = zsel_n(n1225, n1276, n1278);
    let n1280: ZN = zsel_n(n810, r_c312, n1044);
    let n1281: ZN = zsel_n(n810, r_c313, n1045);
    let n1282: ZN = zsel_n(n1179, zn_splat(P8::from_raw(0i32)), n1280);
    let n1283: ZN = zsel_n(n1225, n1280, n1282);
    let n1284: ZN = zsel_n(n810, r_c313, n1049);
    let n1285: ZN = zsel_n(n810, r_c313, n1051);
    let n1286: ZN = zsel_n(n810, r_c313, n1053);
    let n1288: ZW = zw_cellmix_n(39u64, r_c39, 1542469173u64);
    let n1289: ZW = zw_cellmix_n(39u64, r_c39, 668265263u64);
    let n1290: ZW = zw_add(zw_splat(0u64), n1288);
    let n1291: ZW = zw_add(zw_splat(0u64), n1289);
    let n1292: ZW = zw_cellmix_n(84u64, n80, 1542469173u64);
    let n1293: ZW = zw_cellmix_n(84u64, n80, 668265263u64);
    let n1294: ZW = zw_add(n1290, n1292);
    let n1295: ZW = zw_add(n1291, n1293);
    let n1296: ZW = zw_cellmix_n(85u64, n131, 1542469173u64);
    let n1297: ZW = zw_cellmix_n(85u64, n131, 668265263u64);
    let n1298: ZW = zw_add(n1294, n1296);
    let n1299: ZW = zw_add(n1295, n1297);
    let n1300: ZW = zw_cellmix_n(86u64, n130, 1542469173u64);
    let n1301: ZW = zw_cellmix_n(86u64, n130, 668265263u64);
    let n1302: ZW = zw_add(n1298, n1300);
    let n1303: ZW = zw_add(n1299, n1301);
    let n1304: ZW = zw_cellmix_n(87u64, r_c87, 1542469173u64);
    let n1305: ZW = zw_cellmix_n(87u64, r_c87, 668265263u64);
    let n1306: ZW = zw_add(n1302, n1304);
    let n1307: ZW = zw_add(n1303, n1305);
    let n1308: ZW = zw_cellmix_n(256u64, n362, 1542469173u64);
    let n1309: ZW = zw_cellmix_n(256u64, n362, 668265263u64);
    let n1310: ZW = zw_add(n1306, n1308);
    let n1311: ZW = zw_add(n1307, n1309);
    let n1312: ZW = zw_cellmix_n(280u64, n363, 1542469173u64);
    let n1313: ZW = zw_cellmix_n(280u64, n363, 668265263u64);
    let n1314: ZW = zw_add(n1310, n1312);
    let n1315: ZW = zw_add(n1311, n1313);
    let n1316: ZW = zw_cellmix_n(281u64, n364, 1542469173u64);
    let n1317: ZW = zw_cellmix_n(281u64, n364, 668265263u64);
    let n1318: ZW = zw_add(n1314, n1316);
    let n1319: ZW = zw_add(n1315, n1317);
    let n1320: ZW = zw_cellmix_n(20u64, r_c20, 1542469173u64);
    let n1321: ZW = zw_cellmix_n(20u64, r_c20, 668265263u64);
    let n1322: ZW = zw_add(n1318, n1320);
    let n1323: ZW = zw_add(n1319, n1321);
    let n1324: ZW = zw_cellmix_b(41u64, r_c41, 1542469173u64);
    let n1325: ZW = zw_cellmix_b(41u64, r_c41, 668265263u64);
    let n1326: ZW = zw_add(n1322, n1324);
    let n1327: ZW = zw_add(n1323, n1325);
    let n1328: ZW = zw_cellmix_n(236u64, n709, 1542469173u64);
    let n1329: ZW = zw_cellmix_n(236u64, n709, 668265263u64);
    let n1330: ZW = zw_add(n1326, n1328);
    let n1331: ZW = zw_add(n1327, n1329);
    let n1332: ZW = zw_cellmix_n(238u64, n790, 1542469173u64);
    let n1333: ZW = zw_cellmix_n(238u64, n790, 668265263u64);
    let n1334: ZW = zw_add(n1330, n1332);
    let n1335: ZW = zw_add(n1331, n1333);
    let n1336: ZW = zw_cellmix_n(241u64, n708, 1542469173u64);
    let n1337: ZW = zw_cellmix_n(241u64, n708, 668265263u64);
    let n1338: ZW = zw_add(n1334, n1336);
    let n1339: ZW = zw_add(n1335, n1337);
    let n1340: ZW = zw_cellmix_b(248u64, zb_splat(false), 1542469173u64);
    let n1341: ZW = zw_cellmix_b(248u64, zb_splat(false), 668265263u64);
    let n1342: ZW = zw_add(n1338, n1340);
    let n1343: ZW = zw_add(n1339, n1341);
    let n1344: ZW = zw_cellmix_b(249u64, zb_splat(false), 1542469173u64);
    let n1345: ZW = zw_cellmix_b(249u64, zb_splat(false), 668265263u64);
    let n1346: ZW = zw_add(n1342, n1344);
    let n1347: ZW = zw_add(n1343, n1345);
    let n1348: ZW = zw_cellmix_n(255u64, n823, 1542469173u64);
    let n1349: ZW = zw_cellmix_n(255u64, n823, 668265263u64);
    let n1350: ZW = zw_add(n1346, n1348);
    let n1351: ZW = zw_add(n1347, n1349);
    let n1352: ZW = zw_cellmix_n(270u64, r_c300, 1542469173u64);
    let n1353: ZW = zw_cellmix_n(270u64, r_c300, 668265263u64);
    let n1354: ZW = zw_add(n1350, n1352);
    let n1355: ZW = zw_add(n1351, n1353);
    let n1356: ZW = zw_cellmix_n(271u64, r_c301, 1542469173u64);
    let n1357: ZW = zw_cellmix_n(271u64, r_c301, 668265263u64);
    let n1358: ZW = zw_add(n1354, n1356);
    let n1359: ZW = zw_add(n1355, n1357);
    let n1360: ZW = zw_cellmix_n(272u64, r_c302, 1542469173u64);
    let n1361: ZW = zw_cellmix_n(272u64, r_c302, 668265263u64);
    let n1362: ZW = zw_add(n1358, n1360);
    let n1363: ZW = zw_add(n1359, n1361);
    let n1364: ZW = zw_cellmix_n(273u64, r_c303, 1542469173u64);
    let n1365: ZW = zw_cellmix_n(273u64, r_c303, 668265263u64);
    let n1366: ZW = zw_add(n1362, n1364);
    let n1367: ZW = zw_add(n1363, n1365);
    let n1368: ZW = zw_cellmix_b(274u64, n791, 1542469173u64);
    let n1369: ZW = zw_cellmix_b(274u64, n791, 668265263u64);
    let n1370: ZW = zw_add(n1366, n1368);
    let n1371: ZW = zw_add(n1367, n1369);
    let n1372: ZW = zw_cellmix_n(282u64, n824, 1542469173u64);
    let n1373: ZW = zw_cellmix_n(282u64, n824, 668265263u64);
    let n1374: ZW = zw_add(n1370, n1372);
    let n1375: ZW = zw_add(n1371, n1373);
    let n1376: ZW = zw_cellmix_n(283u64, n793, 1542469173u64);
    let n1377: ZW = zw_cellmix_n(283u64, n793, 668265263u64);
    let n1378: ZW = zw_add(n1374, n1376);
    let n1379: ZW = zw_add(n1375, n1377);
    let n1380: ZW = zw_cellmix_b(274u64, n871, 1542469173u64);
    let n1381: ZW = zw_cellmix_b(274u64, n871, 668265263u64);
    let n1382: ZW = zw_add(n1366, n1380);
    let n1383: ZW = zw_add(n1367, n1381);
    let n1384: ZW = zw_cellmix_n(282u64, n875, 1542469173u64);
    let n1385: ZW = zw_cellmix_n(282u64, n875, 668265263u64);
    let n1386: ZW = zw_add(n1382, n1384);
    let n1387: ZW = zw_add(n1383, n1385);
    let n1388: ZW = zw_cellmix_n(283u64, n873, 1542469173u64);
    let n1389: ZW = zw_cellmix_n(283u64, n873, 668265263u64);
    let n1390: ZW = zw_add(n1386, n1388);
    let n1391: ZW = zw_add(n1387, n1389);
    let n1392: ZW = zw_cellmix_b(274u64, n896, 1542469173u64);
    let n1393: ZW = zw_cellmix_b(274u64, n896, 668265263u64);
    let n1394: ZW = zw_add(n1366, n1392);
    let n1395: ZW = zw_add(n1367, n1393);
    let n1396: ZW = zw_cellmix_n(282u64, n900, 1542469173u64);
    let n1397: ZW = zw_cellmix_n(282u64, n900, 668265263u64);
    let n1398: ZW = zw_add(n1394, n1396);
    let n1399: ZW = zw_add(n1395, n1397);
    let n1400: ZW = zw_cellmix_n(283u64, n898, 1542469173u64);
    let n1401: ZW = zw_cellmix_n(283u64, n898, 668265263u64);
    let n1402: ZW = zw_add(n1398, n1400);
    let n1403: ZW = zw_add(n1399, n1401);
    let n1404: ZW = zw_cellmix_n(241u64, n904, 1542469173u64);
    let n1405: ZW = zw_cellmix_n(241u64, n904, 668265263u64);
    let n1406: ZW = zw_add(n1334, n1404);
    let n1407: ZW = zw_add(n1335, n1405);
    let n1408: ZW = zw_add(n1406, n1340);
    let n1409: ZW = zw_add(n1407, n1341);
    let n1410: ZW = zw_cellmix_b(249u64, zb_splat(true), 1542469173u64);
    let n1411: ZW = zw_cellmix_b(249u64, zb_splat(true), 668265263u64);
    let n1412: ZW = zw_add(n1408, n1410);
    let n1413: ZW = zw_add(n1409, n1411);
    let n1414: ZW = zw_add(n1412, n1348);
    let n1415: ZW = zw_add(n1413, n1349);
    let n1416: ZW = zw_add(n1414, n1352);
    let n1417: ZW = zw_add(n1415, n1353);
    let n1418: ZW = zw_add(n1416, n1356);
    let n1419: ZW = zw_add(n1417, n1357);
    let n1420: ZW = zw_add(n1418, n1360);
    let n1421: ZW = zw_add(n1419, n1361);
    let n1422: ZW = zw_add(n1420, n1364);
    let n1423: ZW = zw_add(n1421, n1365);
    let n1424: ZW = zw_add(n1422, n1368);
    let n1425: ZW = zw_add(n1423, n1369);
    let n1426: ZW = zw_cellmix_n(282u64, n913, 1542469173u64);
    let n1427: ZW = zw_cellmix_n(282u64, n913, 668265263u64);
    let n1428: ZW = zw_add(n1424, n1426);
    let n1429: ZW = zw_add(n1425, n1427);
    let n1430: ZW = zw_cellmix_n(283u64, n906, 1542469173u64);
    let n1431: ZW = zw_cellmix_n(283u64, n906, 668265263u64);
    let n1432: ZW = zw_add(n1428, n1430);
    let n1433: ZW = zw_add(n1429, n1431);
    let n1434: ZW = zw_add(n1422, n1380);
    let n1435: ZW = zw_add(n1423, n1381);
    let n1436: ZW = zw_cellmix_n(282u64, n919, 1542469173u64);
    let n1437: ZW = zw_cellmix_n(282u64, n919, 668265263u64);
    let n1438: ZW = zw_add(n1434, n1436);
    let n1439: ZW = zw_add(n1435, n1437);
    let n1440: ZW = zw_cellmix_n(283u64, n917, 1542469173u64);
    let n1441: ZW = zw_cellmix_n(283u64, n917, 668265263u64);
    let n1442: ZW = zw_add(n1438, n1440);
    let n1443: ZW = zw_add(n1439, n1441);
    let n1444: ZW = zw_add(n1422, n1392);
    let n1445: ZW = zw_add(n1423, n1393);
    let n1446: ZW = zw_cellmix_n(282u64, n925, 1542469173u64);
    let n1447: ZW = zw_cellmix_n(282u64, n925, 668265263u64);
    let n1448: ZW = zw_add(n1444, n1446);
    let n1449: ZW = zw_add(n1445, n1447);
    let n1450: ZW = zw_cellmix_n(283u64, n923, 1542469173u64);
    let n1451: ZW = zw_cellmix_n(283u64, n923, 668265263u64);
    let n1452: ZW = zw_add(n1448, n1450);
    let n1453: ZW = zw_add(n1449, n1451);
    let n1454: ZW = zw_cellmix_n(20u64, n937, 1542469173u64);
    let n1455: ZW = zw_cellmix_n(20u64, n937, 668265263u64);
    let n1456: ZW = zw_add(n1318, n1454);
    let n1457: ZW = zw_add(n1319, n1455);
    let n1458: ZW = zw_cellmix_b(41u64, n938, 1542469173u64);
    let n1459: ZW = zw_cellmix_b(41u64, n938, 668265263u64);
    let n1460: ZW = zw_add(n1456, n1458);
    let n1461: ZW = zw_add(n1457, n1459);
    let n1462: ZW = zw_cellmix_n(236u64, n939, 1542469173u64);
    let n1463: ZW = zw_cellmix_n(236u64, n939, 668265263u64);
    let n1464: ZW = zw_add(n1460, n1462);
    let n1465: ZW = zw_add(n1461, n1463);
    let n1466: ZW = zw_cellmix_n(238u64, n940, 1542469173u64);
    let n1467: ZW = zw_cellmix_n(238u64, n940, 668265263u64);
    let n1468: ZW = zw_add(n1464, n1466);
    let n1469: ZW = zw_add(n1465, n1467);
    let n1470: ZW = zw_add(n1468, n1336);
    let n1471: ZW = zw_add(n1469, n1337);
    let n1472: ZW = zw_cellmix_b(248u64, zb_splat(true), 1542469173u64);
    let n1473: ZW = zw_cellmix_b(248u64, zb_splat(true), 668265263u64);
    let n1474: ZW = zw_add(n1470, n1472);
    let n1475: ZW = zw_add(n1471, n1473);
    let n1476: ZW = zw_add(n1474, n1344);
    let n1477: ZW = zw_add(n1475, n1345);
    let n1478: ZW = zw_cellmix_n(255u64, n949, 1542469173u64);
    let n1479: ZW = zw_cellmix_n(255u64, n949, 668265263u64);
    let n1480: ZW = zw_add(n1476, n1478);
    let n1481: ZW = zw_add(n1477, n1479);
    let n1482: ZW = zw_cellmix_n(270u64, n941, 1542469173u64);
    let n1483: ZW = zw_cellmix_n(270u64, n941, 668265263u64);
    let n1484: ZW = zw_add(n1480, n1482);
    let n1485: ZW = zw_add(n1481, n1483);
    let n1486: ZW = zw_cellmix_n(271u64, n942, 1542469173u64);
    let n1487: ZW = zw_cellmix_n(271u64, n942, 668265263u64);
    let n1488: ZW = zw_add(n1484, n1486);
    let n1489: ZW = zw_add(n1485, n1487);
    let n1490: ZW = zw_cellmix_n(272u64, n943, 1542469173u64);
    let n1491: ZW = zw_cellmix_n(272u64, n943, 668265263u64);
    let n1492: ZW = zw_add(n1488, n1490);
    let n1493: ZW = zw_add(n1489, n1491);
    let n1494: ZW = zw_cellmix_n(273u64, n944, 1542469173u64);
    let n1495: ZW = zw_cellmix_n(273u64, n944, 668265263u64);
    let n1496: ZW = zw_add(n1492, n1494);
    let n1497: ZW = zw_add(n1493, n1495);
    let n1498: ZW = zw_add(n1496, n1368);
    let n1499: ZW = zw_add(n1497, n1369);
    let n1500: ZW = zw_cellmix_n(282u64, n950, 1542469173u64);
    let n1501: ZW = zw_cellmix_n(282u64, n950, 668265263u64);
    let n1502: ZW = zw_add(n1498, n1500);
    let n1503: ZW = zw_add(n1499, n1501);
    let n1504: ZW = zw_cellmix_n(283u64, n946, 1542469173u64);
    let n1505: ZW = zw_cellmix_n(283u64, n946, 668265263u64);
    let n1506: ZW = zw_add(n1502, n1504);
    let n1507: ZW = zw_add(n1503, n1505);
    let n1508: ZW = zw_cellmix_n(271u64, n958, 1542469173u64);
    let n1509: ZW = zw_cellmix_n(271u64, n958, 668265263u64);
    let n1510: ZW = zw_add(n1484, n1508);
    let n1511: ZW = zw_add(n1485, n1509);
    let n1512: ZW = zw_cellmix_n(272u64, n959, 1542469173u64);
    let n1513: ZW = zw_cellmix_n(272u64, n959, 668265263u64);
    let n1514: ZW = zw_add(n1510, n1512);
    let n1515: ZW = zw_add(n1511, n1513);
    let n1516: ZW = zw_add(n1514, n1494);
    let n1517: ZW = zw_add(n1515, n1495);
    let n1518: ZW = zw_add(n1516, n1380);
    let n1519: ZW = zw_add(n1517, n1381);
    let n1520: ZW = zw_cellmix_n(282u64, n963, 1542469173u64);
    let n1521: ZW = zw_cellmix_n(282u64, n963, 668265263u64);
    let n1522: ZW = zw_add(n1518, n1520);
    let n1523: ZW = zw_add(n1519, n1521);
    let n1524: ZW = zw_cellmix_n(283u64, n961, 1542469173u64);
    let n1525: ZW = zw_cellmix_n(283u64, n961, 668265263u64);
    let n1526: ZW = zw_add(n1522, n1524);
    let n1527: ZW = zw_add(n1523, n1525);
    let n1528: ZW = zw_cellmix_n(272u64, n967, 1542469173u64);
    let n1529: ZW = zw_cellmix_n(272u64, n967, 668265263u64);
    let n1530: ZW = zw_add(n1510, n1528);
    let n1531: ZW = zw_add(n1511, n1529);
    let n1532: ZW = zw_add(n1530, n1494);
    let n1533: ZW = zw_add(n1531, n1495);
    let n1534: ZW = zw_add(n1532, n1392);
    let n1535: ZW = zw_add(n1533, n1393);
    let n1536: ZW = zw_cellmix_n(282u64, n971, 1542469173u64);
    let n1537: ZW = zw_cellmix_n(282u64, n971, 668265263u64);
    let n1538: ZW = zw_add(n1534, n1536);
    let n1539: ZW = zw_add(n1535, n1537);
    let n1540: ZW = zw_cellmix_n(283u64, n969, 1542469173u64);
    let n1541: ZW = zw_cellmix_n(283u64, n969, 668265263u64);
    let n1542: ZW = zw_add(n1538, n1540);
    let n1543: ZW = zw_add(n1539, n1541);
    let n1544: ZW = zw_cellmix_n(270u64, n979, 1542469173u64);
    let n1545: ZW = zw_cellmix_n(270u64, n979, 668265263u64);
    let n1546: ZW = zw_add(n1480, n1544);
    let n1547: ZW = zw_add(n1481, n1545);
    let n1548: ZW = zw_cellmix_n(271u64, n980, 1542469173u64);
    let n1549: ZW = zw_cellmix_n(271u64, n980, 668265263u64);
    let n1550: ZW = zw_add(n1546, n1548);
    let n1551: ZW = zw_add(n1547, n1549);
    let n1552: ZW = zw_cellmix_n(272u64, n981, 1542469173u64);
    let n1553: ZW = zw_cellmix_n(272u64, n981, 668265263u64);
    let n1554: ZW = zw_add(n1550, n1552);
    let n1555: ZW = zw_add(n1551, n1553);
    let n1556: ZW = zw_cellmix_n(273u64, n982, 1542469173u64);
    let n1557: ZW = zw_cellmix_n(273u64, n982, 668265263u64);
    let n1558: ZW = zw_add(n1554, n1556);
    let n1559: ZW = zw_add(n1555, n1557);
    let n1560: ZW = zw_add(n1558, n1368);
    let n1561: ZW = zw_add(n1559, n1369);
    let n1562: ZW = zw_cellmix_n(282u64, n986, 1542469173u64);
    let n1563: ZW = zw_cellmix_n(282u64, n986, 668265263u64);
    let n1564: ZW = zw_add(n1560, n1562);
    let n1565: ZW = zw_add(n1561, n1563);
    let n1566: ZW = zw_cellmix_n(283u64, n984, 1542469173u64);
    let n1567: ZW = zw_cellmix_n(283u64, n984, 668265263u64);
    let n1568: ZW = zw_add(n1564, n1566);
    let n1569: ZW = zw_add(n1565, n1567);
    let n1570: ZW = zw_add(n1546, n1508);
    let n1571: ZW = zw_add(n1547, n1509);
    let n1572: ZW = zw_add(n1570, n1512);
    let n1573: ZW = zw_add(n1571, n1513);
    let n1574: ZW = zw_add(n1572, n1556);
    let n1575: ZW = zw_add(n1573, n1557);
    let n1576: ZW = zw_add(n1574, n1380);
    let n1577: ZW = zw_add(n1575, n1381);
    let n1578: ZW = zw_cellmix_n(282u64, n992, 1542469173u64);
    let n1579: ZW = zw_cellmix_n(282u64, n992, 668265263u64);
    let n1580: ZW = zw_add(n1576, n1578);
    let n1581: ZW = zw_add(n1577, n1579);
    let n1582: ZW = zw_cellmix_n(283u64, n990, 1542469173u64);
    let n1583: ZW = zw_cellmix_n(283u64, n990, 668265263u64);
    let n1584: ZW = zw_add(n1580, n1582);
    let n1585: ZW = zw_add(n1581, n1583);
    let n1586: ZW = zw_add(n1570, n1528);
    let n1587: ZW = zw_add(n1571, n1529);
    let n1588: ZW = zw_add(n1586, n1556);
    let n1589: ZW = zw_add(n1587, n1557);
    let n1590: ZW = zw_add(n1588, n1392);
    let n1591: ZW = zw_add(n1589, n1393);
    let n1592: ZW = zw_cellmix_n(282u64, n998, 1542469173u64);
    let n1593: ZW = zw_cellmix_n(282u64, n998, 668265263u64);
    let n1594: ZW = zw_add(n1590, n1592);
    let n1595: ZW = zw_add(n1591, n1593);
    let n1596: ZW = zw_cellmix_n(283u64, n996, 1542469173u64);
    let n1597: ZW = zw_cellmix_n(283u64, n996, 668265263u64);
    let n1598: ZW = zw_add(n1594, n1596);
    let n1599: ZW = zw_add(n1595, n1597);
    let n1600: ZW = zw_cellmix_n(273u64, n1001, 1542469173u64);
    let n1601: ZW = zw_cellmix_n(273u64, n1001, 668265263u64);
    let n1602: ZW = zw_add(n1554, n1600);
    let n1603: ZW = zw_add(n1555, n1601);
    let n1604: ZW = zw_add(n1602, n1368);
    let n1605: ZW = zw_add(n1603, n1369);
    let n1606: ZW = zw_add(n1604, n1562);
    let n1607: ZW = zw_add(n1605, n1563);
    let n1608: ZW = zw_cellmix_n(283u64, n1002, 1542469173u64);
    let n1609: ZW = zw_cellmix_n(283u64, n1002, 668265263u64);
    let n1610: ZW = zw_add(n1606, n1608);
    let n1611: ZW = zw_add(n1607, n1609);
    let n1612: ZW = zw_add(n1572, n1600);
    let n1613: ZW = zw_add(n1573, n1601);
    let n1614: ZW = zw_add(n1612, n1380);
    let n1615: ZW = zw_add(n1613, n1381);
    let n1616: ZW = zw_add(n1614, n1578);
    let n1617: ZW = zw_add(n1615, n1579);
    let n1618: ZW = zw_cellmix_n(283u64, n1004, 1542469173u64);
    let n1619: ZW = zw_cellmix_n(283u64, n1004, 668265263u64);
    let n1620: ZW = zw_add(n1616, n1618);
    let n1621: ZW = zw_add(n1617, n1619);
    let n1622: ZW = zw_add(n1586, n1600);
    let n1623: ZW = zw_add(n1587, n1601);
    let n1624: ZW = zw_add(n1622, n1392);
    let n1625: ZW = zw_add(n1623, n1393);
    let n1626: ZW = zw_add(n1624, n1592);
    let n1627: ZW = zw_add(n1625, n1593);
    let n1628: ZW = zw_cellmix_n(283u64, n1006, 1542469173u64);
    let n1629: ZW = zw_cellmix_n(283u64, n1006, 668265263u64);
    let n1630: ZW = zw_add(n1626, n1628);
    let n1631: ZW = zw_add(n1627, n1629);
    let n1632: ZW = zw_add(n1468, n1404);
    let n1633: ZW = zw_add(n1469, n1405);
    let n1634: ZW = zw_add(n1632, n1472);
    let n1635: ZW = zw_add(n1633, n1473);
    let n1636: ZW = zw_add(n1634, n1410);
    let n1637: ZW = zw_add(n1635, n1411);
    let n1638: ZW = zw_add(n1636, n1478);
    let n1639: ZW = zw_add(n1637, n1479);
    let n1640: ZW = zw_add(n1638, n1482);
    let n1641: ZW = zw_add(n1639, n1483);
    let n1642: ZW = zw_add(n1640, n1486);
    let n1643: ZW = zw_add(n1641, n1487);
    let n1644: ZW = zw_add(n1642, n1490);
    let n1645: ZW = zw_add(n1643, n1491);
    let n1646: ZW = zw_add(n1644, n1494);
    let n1647: ZW = zw_add(n1645, n1495);
    let n1648: ZW = zw_add(n1646, n1368);
    let n1649: ZW = zw_add(n1647, n1369);
    let n1650: ZW = zw_cellmix_n(282u64, n1012, 1542469173u64);
    let n1651: ZW = zw_cellmix_n(282u64, n1012, 668265263u64);
    let n1652: ZW = zw_add(n1648, n1650);
    let n1653: ZW = zw_add(n1649, n1651);
    let n1654: ZW = zw_cellmix_n(283u64, n1010, 1542469173u64);
    let n1655: ZW = zw_cellmix_n(283u64, n1010, 668265263u64);
    let n1656: ZW = zw_add(n1652, n1654);
    let n1657: ZW = zw_add(n1653, n1655);
    let n1658: ZW = zw_add(n1640, n1508);
    let n1659: ZW = zw_add(n1641, n1509);
    let n1660: ZW = zw_add(n1658, n1512);
    let n1661: ZW = zw_add(n1659, n1513);
    let n1662: ZW = zw_add(n1660, n1494);
    let n1663: ZW = zw_add(n1661, n1495);
    let n1664: ZW = zw_add(n1662, n1380);
    let n1665: ZW = zw_add(n1663, n1381);
    let n1666: ZW = zw_cellmix_n(282u64, n1023, 1542469173u64);
    let n1667: ZW = zw_cellmix_n(282u64, n1023, 668265263u64);
    let n1668: ZW = zw_add(n1664, n1666);
    let n1669: ZW = zw_add(n1665, n1667);
    let n1670: ZW = zw_cellmix_n(283u64, n1021, 1542469173u64);
    let n1671: ZW = zw_cellmix_n(283u64, n1021, 668265263u64);
    let n1672: ZW = zw_add(n1668, n1670);
    let n1673: ZW = zw_add(n1669, n1671);
    let n1674: ZW = zw_add(n1658, n1528);
    let n1675: ZW = zw_add(n1659, n1529);
    let n1676: ZW = zw_add(n1674, n1494);
    let n1677: ZW = zw_add(n1675, n1495);
    let n1678: ZW = zw_add(n1676, n1392);
    let n1679: ZW = zw_add(n1677, n1393);
    let n1680: ZW = zw_cellmix_n(282u64, n1029, 1542469173u64);
    let n1681: ZW = zw_cellmix_n(282u64, n1029, 668265263u64);
    let n1682: ZW = zw_add(n1678, n1680);
    let n1683: ZW = zw_add(n1679, n1681);
    let n1684: ZW = zw_cellmix_n(283u64, n1027, 1542469173u64);
    let n1685: ZW = zw_cellmix_n(283u64, n1027, 668265263u64);
    let n1686: ZW = zw_add(n1682, n1684);
    let n1687: ZW = zw_add(n1683, n1685);
    let n1688: ZW = zw_add(n1638, n1544);
    let n1689: ZW = zw_add(n1639, n1545);
    let n1690: ZW = zw_add(n1688, n1548);
    let n1691: ZW = zw_add(n1689, n1549);
    let n1692: ZW = zw_add(n1690, n1552);
    let n1693: ZW = zw_add(n1691, n1553);
    let n1694: ZW = zw_add(n1692, n1556);
    let n1695: ZW = zw_add(n1693, n1557);
    let n1696: ZW = zw_add(n1694, n1368);
    let n1697: ZW = zw_add(n1695, n1369);
    let n1698: ZW = zw_cellmix_n(282u64, n1035, 1542469173u64);
    let n1699: ZW = zw_cellmix_n(282u64, n1035, 668265263u64);
    let n1700: ZW = zw_add(n1696, n1698);
    let n1701: ZW = zw_add(n1697, n1699);
    let n1702: ZW = zw_cellmix_n(283u64, n1033, 1542469173u64);
    let n1703: ZW = zw_cellmix_n(283u64, n1033, 668265263u64);
    let n1704: ZW = zw_add(n1700, n1702);
    let n1705: ZW = zw_add(n1701, n1703);
    let n1706: ZW = zw_add(n1688, n1508);
    let n1707: ZW = zw_add(n1689, n1509);
    let n1708: ZW = zw_add(n1706, n1512);
    let n1709: ZW = zw_add(n1707, n1513);
    let n1710: ZW = zw_add(n1708, n1556);
    let n1711: ZW = zw_add(n1709, n1557);
    let n1712: ZW = zw_add(n1710, n1380);
    let n1713: ZW = zw_add(n1711, n1381);
    let n1714: ZW = zw_cellmix_n(282u64, n1041, 1542469173u64);
    let n1715: ZW = zw_cellmix_n(282u64, n1041, 668265263u64);
    let n1716: ZW = zw_add(n1712, n1714);
    let n1717: ZW = zw_add(n1713, n1715);
    let n1718: ZW = zw_cellmix_n(283u64, n1039, 1542469173u64);
    let n1719: ZW = zw_cellmix_n(283u64, n1039, 668265263u64);
    let n1720: ZW = zw_add(n1716, n1718);
    let n1721: ZW = zw_add(n1717, n1719);
    let n1722: ZW = zw_add(n1706, n1528);
    let n1723: ZW = zw_add(n1707, n1529);
    let n1724: ZW = zw_add(n1722, n1556);
    let n1725: ZW = zw_add(n1723, n1557);
    let n1726: ZW = zw_add(n1724, n1392);
    let n1727: ZW = zw_add(n1725, n1393);
    let n1728: ZW = zw_cellmix_n(282u64, n1047, 1542469173u64);
    let n1729: ZW = zw_cellmix_n(282u64, n1047, 668265263u64);
    let n1730: ZW = zw_add(n1726, n1728);
    let n1731: ZW = zw_add(n1727, n1729);
    let n1732: ZW = zw_cellmix_n(283u64, n1045, 1542469173u64);
    let n1733: ZW = zw_cellmix_n(283u64, n1045, 668265263u64);
    let n1734: ZW = zw_add(n1730, n1732);
    let n1735: ZW = zw_add(n1731, n1733);
    let n1736: ZW = zw_add(n1692, n1600);
    let n1737: ZW = zw_add(n1693, n1601);
    let n1738: ZW = zw_add(n1736, n1368);
    let n1739: ZW = zw_add(n1737, n1369);
    let n1740: ZW = zw_add(n1738, n1698);
    let n1741: ZW = zw_add(n1739, n1699);
    let n1742: ZW = zw_cellmix_n(283u64, n1049, 1542469173u64);
    let n1743: ZW = zw_cellmix_n(283u64, n1049, 668265263u64);
    let n1744: ZW = zw_add(n1740, n1742);
    let n1745: ZW = zw_add(n1741, n1743);
    let n1746: ZW = zw_add(n1708, n1600);
    let n1747: ZW = zw_add(n1709, n1601);
    let n1748: ZW = zw_add(n1746, n1380);
    let n1749: ZW = zw_add(n1747, n1381);
    let n1750: ZW = zw_add(n1748, n1714);
    let n1751: ZW = zw_add(n1749, n1715);
    let n1752: ZW = zw_cellmix_n(283u64, n1051, 1542469173u64);
    let n1753: ZW = zw_cellmix_n(283u64, n1051, 668265263u64);
    let n1754: ZW = zw_add(n1750, n1752);
    let n1755: ZW = zw_add(n1751, n1753);
    let n1756: ZW = zw_add(n1722, n1600);
    let n1757: ZW = zw_add(n1723, n1601);
    let n1758: ZW = zw_add(n1756, n1392);
    let n1759: ZW = zw_add(n1757, n1393);
    let n1760: ZW = zw_add(n1758, n1728);
    let n1761: ZW = zw_add(n1759, n1729);
    let n1762: ZW = zw_cellmix_n(283u64, n1053, 1542469173u64);
    let n1763: ZW = zw_cellmix_n(283u64, n1053, 668265263u64);
    let n1764: ZW = zw_add(n1760, n1762);
    let n1765: ZW = zw_add(n1761, n1763);
    let n1766: ZW = zw_add(zw_splat(0u64), n1292);
    let n1767: ZW = zw_add(zw_splat(0u64), n1293);
    let n1768: ZW = zw_add(n1766, n1296);
    let n1769: ZW = zw_add(n1767, n1297);
    let n1770: ZW = zw_add(n1768, n1300);
    let n1771: ZW = zw_add(n1769, n1301);
    let n1772: ZW = zw_cellmix_n(87u64, n1144, 1542469173u64);
    let n1773: ZW = zw_cellmix_n(87u64, n1144, 668265263u64);
    let n1774: ZW = zw_add(n1770, n1772);
    let n1775: ZW = zw_add(n1771, n1773);
    let n1776: ZW = zw_cellmix_n(241u64, n1136, 1542469173u64);
    let n1777: ZW = zw_cellmix_n(241u64, n1136, 668265263u64);
    let n1778: ZW = zw_add(n1774, n1776);
    let n1779: ZW = zw_add(n1775, n1777);
    let n1780: ZW = zw_cellmix_n(249u64, n1140, 1542469173u64);
    let n1781: ZW = zw_cellmix_n(249u64, n1140, 668265263u64);
    let n1782: ZW = zw_add(n1778, n1780);
    let n1783: ZW = zw_add(n1779, n1781);
    let n1784: ZW = zw_add(n1782, n1320);
    let n1785: ZW = zw_add(n1783, n1321);
    let n1786: ZW = zw_add(n1784, n1324);
    let n1787: ZW = zw_add(n1785, n1325);
    let n1788: ZW = zw_add(n1782, n1454);
    let n1789: ZW = zw_add(n1783, n1455);
    let n1790: ZW = zw_add(n1788, n1458);
    let n1791: ZW = zw_add(n1789, n1459);
    let n1792: ZW = zw_cellmix_b(38u64, n1148, 1542469173u64);
    let n1793: ZW = zw_cellmix_b(38u64, n1148, 668265263u64);
    let n1794: ZW = zw_add(zw_splat(0u64), n1792);
    let n1795: ZW = zw_add(zw_splat(0u64), n1793);
    let n1796: ZW = zw_cellmix_n(39u64, n1152, 1542469173u64);
    let n1797: ZW = zw_cellmix_n(39u64, n1152, 668265263u64);
    let n1798: ZW = zw_add(n1794, n1796);
    let n1799: ZW = zw_add(n1795, n1797);
    let n1800: ZW = zw_add(n1798, n1292);
    let n1801: ZW = zw_add(n1799, n1293);
    let n1802: ZW = zw_add(n1800, n1296);
    let n1803: ZW = zw_add(n1801, n1297);
    let n1804: ZW = zw_add(n1802, n1300);
    let n1805: ZW = zw_add(n1803, n1301);
    let n1806: ZW = zw_cellmix_n(87u64, n1151, 1542469173u64);
    let n1807: ZW = zw_cellmix_n(87u64, n1151, 668265263u64);
    let n1808: ZW = zw_add(n1804, n1806);
    let n1809: ZW = zw_add(n1805, n1807);
    let n1810: ZW = zw_add(n1808, n1320);
    let n1811: ZW = zw_add(n1809, n1321);
    let n1812: ZW = zw_add(n1808, n1454);
    let n1813: ZW = zw_add(n1809, n1455);
    let n1814: ZW = zw_cellmix_n(256u64, n1166, 1542469173u64);
    let n1815: ZW = zw_cellmix_n(256u64, n1166, 668265263u64);
    let n1816: ZW = zw_add(n1306, n1814);
    let n1817: ZW = zw_add(n1307, n1815);
    let n1818: ZW = zw_cellmix_n(267u64, n1167, 1542469173u64);
    let n1819: ZW = zw_cellmix_n(267u64, n1167, 668265263u64);
    let n1820: ZW = zw_add(n1816, n1818);
    let n1821: ZW = zw_add(n1817, n1819);
    let n1822: ZW = zw_cellmix_n(275u64, n1168, 1542469173u64);
    let n1823: ZW = zw_cellmix_n(275u64, n1168, 668265263u64);
    let n1824: ZW = zw_add(n1820, n1822);
    let n1825: ZW = zw_add(n1821, n1823);
    let n1826: ZW = zw_cellmix_n(310u64, n1170, 1542469173u64);
    let n1827: ZW = zw_cellmix_n(310u64, n1170, 668265263u64);
    let n1828: ZW = zw_add(n1824, n1826);
    let n1829: ZW = zw_add(n1825, n1827);
    let n1830: ZW = zw_cellmix_n(311u64, n1171, 1542469173u64);
    let n1831: ZW = zw_cellmix_n(311u64, n1171, 668265263u64);
    let n1832: ZW = zw_add(n1828, n1830);
    let n1833: ZW = zw_add(n1829, n1831);
    let n1834: ZW = zw_cellmix_n(20u64, n1158, 1542469173u64);
    let n1835: ZW = zw_cellmix_n(20u64, n1158, 668265263u64);
    let n1836: ZW = zw_add(n1832, n1834);
    let n1837: ZW = zw_add(n1833, n1835);
    let n1838: ZW = zw_add(n1836, n1324);
    let n1839: ZW = zw_add(n1837, n1325);
    let n1840: ZW = zw_cellmix_n(236u64, n1159, 1542469173u64);
    let n1841: ZW = zw_cellmix_n(236u64, n1159, 668265263u64);
    let n1842: ZW = zw_add(n1838, n1840);
    let n1843: ZW = zw_add(n1839, n1841);
    let n1844: ZW = zw_cellmix_n(238u64, n1160, 1542469173u64);
    let n1845: ZW = zw_cellmix_n(238u64, n1160, 668265263u64);
    let n1846: ZW = zw_add(n1842, n1844);
    let n1847: ZW = zw_add(n1843, n1845);
    let n1848: ZW = zw_cellmix_n(239u64, n1161, 1542469173u64);
    let n1849: ZW = zw_cellmix_n(239u64, n1161, 668265263u64);
    let n1850: ZW = zw_add(n1846, n1848);
    let n1851: ZW = zw_add(n1847, n1849);
    let n1852: ZW = zw_cellmix_n(241u64, n1162, 1542469173u64);
    let n1853: ZW = zw_cellmix_n(241u64, n1162, 668265263u64);
    let n1854: ZW = zw_add(n1850, n1852);
    let n1855: ZW = zw_add(n1851, n1853);
    let n1856: ZW = zw_cellmix_b(248u64, n1163, 1542469173u64);
    let n1857: ZW = zw_cellmix_b(248u64, n1163, 668265263u64);
    let n1858: ZW = zw_add(n1854, n1856);
    let n1859: ZW = zw_add(n1855, n1857);
    let n1860: ZW = zw_cellmix_b(249u64, n1164, 1542469173u64);
    let n1861: ZW = zw_cellmix_b(249u64, n1164, 668265263u64);
    let n1862: ZW = zw_add(n1858, n1860);
    let n1863: ZW = zw_add(n1859, n1861);
    let n1864: ZW = zw_cellmix_n(255u64, n1184, 1542469173u64);
    let n1865: ZW = zw_cellmix_n(255u64, n1184, 668265263u64);
    let n1866: ZW = zw_add(n1862, n1864);
    let n1867: ZW = zw_add(n1863, n1865);
    let n1868: ZW = zw_cellmix_n(300u64, r_c300, 1542469173u64);
    let n1869: ZW = zw_cellmix_n(300u64, r_c300, 668265263u64);
    let n1870: ZW = zw_add(n1866, n1868);
    let n1871: ZW = zw_add(n1867, n1869);
    let n1872: ZW = zw_cellmix_n(301u64, r_c301, 1542469173u64);
    let n1873: ZW = zw_cellmix_n(301u64, r_c301, 668265263u64);
    let n1874: ZW = zw_add(n1870, n1872);
    let n1875: ZW = zw_add(n1871, n1873);
    let n1876: ZW = zw_cellmix_n(302u64, r_c302, 1542469173u64);
    let n1877: ZW = zw_cellmix_n(302u64, r_c302, 668265263u64);
    let n1878: ZW = zw_add(n1874, n1876);
    let n1879: ZW = zw_add(n1875, n1877);
    let n1880: ZW = zw_cellmix_n(303u64, r_c303, 1542469173u64);
    let n1881: ZW = zw_cellmix_n(303u64, r_c303, 668265263u64);
    let n1882: ZW = zw_add(n1878, n1880);
    let n1883: ZW = zw_add(n1879, n1881);
    let n1884: ZW = zw_cellmix_b(304u64, n1169, 1542469173u64);
    let n1885: ZW = zw_cellmix_b(304u64, n1169, 668265263u64);
    let n1886: ZW = zw_add(n1882, n1884);
    let n1887: ZW = zw_add(n1883, n1885);
    let n1888: ZW = zw_cellmix_n(312u64, n1185, 1542469173u64);
    let n1889: ZW = zw_cellmix_n(312u64, n1185, 668265263u64);
    let n1890: ZW = zw_add(n1886, n1888);
    let n1891: ZW = zw_add(n1887, n1889);
    let n1892: ZW = zw_cellmix_n(313u64, n1173, 1542469173u64);
    let n1893: ZW = zw_cellmix_n(313u64, n1173, 668265263u64);
    let n1894: ZW = zw_add(n1890, n1892);
    let n1895: ZW = zw_add(n1891, n1893);
    let n1896: ZW = zw_cellmix_b(304u64, n1187, 1542469173u64);
    let n1897: ZW = zw_cellmix_b(304u64, n1187, 668265263u64);
    let n1898: ZW = zw_add(n1882, n1896);
    let n1899: ZW = zw_add(n1883, n1897);
    let n1900: ZW = zw_cellmix_n(312u64, n1191, 1542469173u64);
    let n1901: ZW = zw_cellmix_n(312u64, n1191, 668265263u64);
    let n1902: ZW = zw_add(n1898, n1900);
    let n1903: ZW = zw_add(n1899, n1901);
    let n1904: ZW = zw_cellmix_n(313u64, n1189, 1542469173u64);
    let n1905: ZW = zw_cellmix_n(313u64, n1189, 668265263u64);
    let n1906: ZW = zw_add(n1902, n1904);
    let n1907: ZW = zw_add(n1903, n1905);
    let n1908: ZW = zw_cellmix_b(304u64, n1192, 1542469173u64);
    let n1909: ZW = zw_cellmix_b(304u64, n1192, 668265263u64);
    let n1910: ZW = zw_add(n1882, n1908);
    let n1911: ZW = zw_add(n1883, n1909);
    let n1912: ZW = zw_cellmix_n(312u64, n1196, 1542469173u64);
    let n1913: ZW = zw_cellmix_n(312u64, n1196, 668265263u64);
    let n1914: ZW = zw_add(n1910, n1912);
    let n1915: ZW = zw_add(n1911, n1913);
    let n1916: ZW = zw_cellmix_n(313u64, n1194, 1542469173u64);
    let n1917: ZW = zw_cellmix_n(313u64, n1194, 668265263u64);
    let n1918: ZW = zw_add(n1914, n1916);
    let n1919: ZW = zw_add(n1915, n1917);
    let n1920: ZW = zw_cellmix_n(241u64, n1197, 1542469173u64);
    let n1921: ZW = zw_cellmix_n(241u64, n1197, 668265263u64);
    let n1922: ZW = zw_add(n1850, n1920);
    let n1923: ZW = zw_add(n1851, n1921);
    let n1924: ZW = zw_add(n1922, n1856);
    let n1925: ZW = zw_add(n1923, n1857);
    let n1926: ZW = zw_cellmix_b(249u64, n1198, 1542469173u64);
    let n1927: ZW = zw_cellmix_b(249u64, n1198, 668265263u64);
    let n1928: ZW = zw_add(n1924, n1926);
    let n1929: ZW = zw_add(n1925, n1927);
    let n1930: ZW = zw_add(n1928, n1864);
    let n1931: ZW = zw_add(n1929, n1865);
    let n1932: ZW = zw_add(n1930, n1868);
    let n1933: ZW = zw_add(n1931, n1869);
    let n1934: ZW = zw_add(n1932, n1872);
    let n1935: ZW = zw_add(n1933, n1873);
    let n1936: ZW = zw_add(n1934, n1876);
    let n1937: ZW = zw_add(n1935, n1877);
    let n1938: ZW = zw_add(n1936, n1880);
    let n1939: ZW = zw_add(n1937, n1881);
    let n1940: ZW = zw_add(n1938, n1884);
    let n1941: ZW = zw_add(n1939, n1885);
    let n1942: ZW = zw_cellmix_n(312u64, n1202, 1542469173u64);
    let n1943: ZW = zw_cellmix_n(312u64, n1202, 668265263u64);
    let n1944: ZW = zw_add(n1940, n1942);
    let n1945: ZW = zw_add(n1941, n1943);
    let n1946: ZW = zw_cellmix_n(313u64, n1200, 1542469173u64);
    let n1947: ZW = zw_cellmix_n(313u64, n1200, 668265263u64);
    let n1948: ZW = zw_add(n1944, n1946);
    let n1949: ZW = zw_add(n1945, n1947);
    let n1950: ZW = zw_add(n1938, n1896);
    let n1951: ZW = zw_add(n1939, n1897);
    let n1952: ZW = zw_cellmix_n(312u64, n1206, 1542469173u64);
    let n1953: ZW = zw_cellmix_n(312u64, n1206, 668265263u64);
    let n1954: ZW = zw_add(n1950, n1952);
    let n1955: ZW = zw_add(n1951, n1953);
    let n1956: ZW = zw_cellmix_n(313u64, n1204, 1542469173u64);
    let n1957: ZW = zw_cellmix_n(313u64, n1204, 668265263u64);
    let n1958: ZW = zw_add(n1954, n1956);
    let n1959: ZW = zw_add(n1955, n1957);
    let n1960: ZW = zw_add(n1938, n1908);
    let n1961: ZW = zw_add(n1939, n1909);
    let n1962: ZW = zw_cellmix_n(312u64, n1210, 1542469173u64);
    let n1963: ZW = zw_cellmix_n(312u64, n1210, 668265263u64);
    let n1964: ZW = zw_add(n1960, n1962);
    let n1965: ZW = zw_add(n1961, n1963);
    let n1966: ZW = zw_cellmix_n(313u64, n1208, 1542469173u64);
    let n1967: ZW = zw_cellmix_n(313u64, n1208, 668265263u64);
    let n1968: ZW = zw_add(n1964, n1966);
    let n1969: ZW = zw_add(n1965, n1967);
    let n1970: ZW = zw_cellmix_n(20u64, n1213, 1542469173u64);
    let n1971: ZW = zw_cellmix_n(20u64, n1213, 668265263u64);
    let n1972: ZW = zw_add(n1832, n1970);
    let n1973: ZW = zw_add(n1833, n1971);
    let n1974: ZW = zw_cellmix_b(41u64, n1214, 1542469173u64);
    let n1975: ZW = zw_cellmix_b(41u64, n1214, 668265263u64);
    let n1976: ZW = zw_add(n1972, n1974);
    let n1977: ZW = zw_add(n1973, n1975);
    let n1978: ZW = zw_cellmix_n(236u64, n1215, 1542469173u64);
    let n1979: ZW = zw_cellmix_n(236u64, n1215, 668265263u64);
    let n1980: ZW = zw_add(n1976, n1978);
    let n1981: ZW = zw_add(n1977, n1979);
    let n1982: ZW = zw_cellmix_n(238u64, n1216, 1542469173u64);
    let n1983: ZW = zw_cellmix_n(238u64, n1216, 668265263u64);
    let n1984: ZW = zw_add(n1980, n1982);
    let n1985: ZW = zw_add(n1981, n1983);
    let n1986: ZW = zw_cellmix_n(239u64, n1217, 1542469173u64);
    let n1987: ZW = zw_cellmix_n(239u64, n1217, 668265263u64);
    let n1988: ZW = zw_add(n1984, n1986);
    let n1989: ZW = zw_add(n1985, n1987);
    let n1990: ZW = zw_add(n1988, n1852);
    let n1991: ZW = zw_add(n1989, n1853);
    let n1992: ZW = zw_cellmix_b(248u64, n1218, 1542469173u64);
    let n1993: ZW = zw_cellmix_b(248u64, n1218, 668265263u64);
    let n1994: ZW = zw_add(n1990, n1992);
    let n1995: ZW = zw_add(n1991, n1993);
    let n1996: ZW = zw_add(n1994, n1860);
    let n1997: ZW = zw_add(n1995, n1861);
    let n1998: ZW = zw_cellmix_n(255u64, n1227, 1542469173u64);
    let n1999: ZW = zw_cellmix_n(255u64, n1227, 668265263u64);
    let n2000: ZW = zw_add(n1996, n1998);
    let n2001: ZW = zw_add(n1997, n1999);
    let n2002: ZW = zw_cellmix_n(300u64, n1219, 1542469173u64);
    let n2003: ZW = zw_cellmix_n(300u64, n1219, 668265263u64);
    let n2004: ZW = zw_add(n2000, n2002);
    let n2005: ZW = zw_add(n2001, n2003);
    let n2006: ZW = zw_cellmix_n(301u64, n1220, 1542469173u64);
    let n2007: ZW = zw_cellmix_n(301u64, n1220, 668265263u64);
    let n2008: ZW = zw_add(n2004, n2006);
    let n2009: ZW = zw_add(n2005, n2007);
    let n2010: ZW = zw_cellmix_n(302u64, n1221, 1542469173u64);
    let n2011: ZW = zw_cellmix_n(302u64, n1221, 668265263u64);
    let n2012: ZW = zw_add(n2008, n2010);
    let n2013: ZW = zw_add(n2009, n2011);
    let n2014: ZW = zw_cellmix_n(303u64, n1222, 1542469173u64);
    let n2015: ZW = zw_cellmix_n(303u64, n1222, 668265263u64);
    let n2016: ZW = zw_add(n2012, n2014);
    let n2017: ZW = zw_add(n2013, n2015);
    let n2018: ZW = zw_add(n2016, n1884);
    let n2019: ZW = zw_add(n2017, n1885);
    let n2020: ZW = zw_cellmix_n(312u64, n1228, 1542469173u64);
    let n2021: ZW = zw_cellmix_n(312u64, n1228, 668265263u64);
    let n2022: ZW = zw_add(n2018, n2020);
    let n2023: ZW = zw_add(n2019, n2021);
    let n2024: ZW = zw_cellmix_n(313u64, n1224, 1542469173u64);
    let n2025: ZW = zw_cellmix_n(313u64, n1224, 668265263u64);
    let n2026: ZW = zw_add(n2022, n2024);
    let n2027: ZW = zw_add(n2023, n2025);
    let n2028: ZW = zw_cellmix_n(301u64, n1229, 1542469173u64);
    let n2029: ZW = zw_cellmix_n(301u64, n1229, 668265263u64);
    let n2030: ZW = zw_add(n2004, n2028);
    let n2031: ZW = zw_add(n2005, n2029);
    let n2032: ZW = zw_cellmix_n(302u64, n1230, 1542469173u64);
    let n2033: ZW = zw_cellmix_n(302u64, n1230, 668265263u64);
    let n2034: ZW = zw_add(n2030, n2032);
    let n2035: ZW = zw_add(n2031, n2033);
    let n2036: ZW = zw_add(n2034, n2014);
    let n2037: ZW = zw_add(n2035, n2015);
    let n2038: ZW = zw_add(n2036, n1896);
    let n2039: ZW = zw_add(n2037, n1897);
    let n2040: ZW = zw_cellmix_n(312u64, n1234, 1542469173u64);
    let n2041: ZW = zw_cellmix_n(312u64, n1234, 668265263u64);
    let n2042: ZW = zw_add(n2038, n2040);
    let n2043: ZW = zw_add(n2039, n2041);
    let n2044: ZW = zw_cellmix_n(313u64, n1232, 1542469173u64);
    let n2045: ZW = zw_cellmix_n(313u64, n1232, 668265263u64);
    let n2046: ZW = zw_add(n2042, n2044);
    let n2047: ZW = zw_add(n2043, n2045);
    let n2048: ZW = zw_cellmix_n(302u64, n1235, 1542469173u64);
    let n2049: ZW = zw_cellmix_n(302u64, n1235, 668265263u64);
    let n2050: ZW = zw_add(n2030, n2048);
    let n2051: ZW = zw_add(n2031, n2049);
    let n2052: ZW = zw_add(n2050, n2014);
    let n2053: ZW = zw_add(n2051, n2015);
    let n2054: ZW = zw_add(n2052, n1908);
    let n2055: ZW = zw_add(n2053, n1909);
    let n2056: ZW = zw_cellmix_n(312u64, n1239, 1542469173u64);
    let n2057: ZW = zw_cellmix_n(312u64, n1239, 668265263u64);
    let n2058: ZW = zw_add(n2054, n2056);
    let n2059: ZW = zw_add(n2055, n2057);
    let n2060: ZW = zw_cellmix_n(313u64, n1237, 1542469173u64);
    let n2061: ZW = zw_cellmix_n(313u64, n1237, 668265263u64);
    let n2062: ZW = zw_add(n2058, n2060);
    let n2063: ZW = zw_add(n2059, n2061);
    let n2064: ZW = zw_cellmix_n(300u64, n1240, 1542469173u64);
    let n2065: ZW = zw_cellmix_n(300u64, n1240, 668265263u64);
    let n2066: ZW = zw_add(n2000, n2064);
    let n2067: ZW = zw_add(n2001, n2065);
    let n2068: ZW = zw_cellmix_n(301u64, n1241, 1542469173u64);
    let n2069: ZW = zw_cellmix_n(301u64, n1241, 668265263u64);
    let n2070: ZW = zw_add(n2066, n2068);
    let n2071: ZW = zw_add(n2067, n2069);
    let n2072: ZW = zw_cellmix_n(302u64, n1242, 1542469173u64);
    let n2073: ZW = zw_cellmix_n(302u64, n1242, 668265263u64);
    let n2074: ZW = zw_add(n2070, n2072);
    let n2075: ZW = zw_add(n2071, n2073);
    let n2076: ZW = zw_cellmix_n(303u64, n1243, 1542469173u64);
    let n2077: ZW = zw_cellmix_n(303u64, n1243, 668265263u64);
    let n2078: ZW = zw_add(n2074, n2076);
    let n2079: ZW = zw_add(n2075, n2077);
    let n2080: ZW = zw_add(n2078, n1884);
    let n2081: ZW = zw_add(n2079, n1885);
    let n2082: ZW = zw_cellmix_n(312u64, n1247, 1542469173u64);
    let n2083: ZW = zw_cellmix_n(312u64, n1247, 668265263u64);
    let n2084: ZW = zw_add(n2080, n2082);
    let n2085: ZW = zw_add(n2081, n2083);
    let n2086: ZW = zw_cellmix_n(313u64, n1245, 1542469173u64);
    let n2087: ZW = zw_cellmix_n(313u64, n1245, 668265263u64);
    let n2088: ZW = zw_add(n2084, n2086);
    let n2089: ZW = zw_add(n2085, n2087);
    let n2090: ZW = zw_add(n2066, n2028);
    let n2091: ZW = zw_add(n2067, n2029);
    let n2092: ZW = zw_add(n2090, n2032);
    let n2093: ZW = zw_add(n2091, n2033);
    let n2094: ZW = zw_add(n2092, n2076);
    let n2095: ZW = zw_add(n2093, n2077);
    let n2096: ZW = zw_add(n2094, n1896);
    let n2097: ZW = zw_add(n2095, n1897);
    let n2098: ZW = zw_cellmix_n(312u64, n1251, 1542469173u64);
    let n2099: ZW = zw_cellmix_n(312u64, n1251, 668265263u64);
    let n2100: ZW = zw_add(n2096, n2098);
    let n2101: ZW = zw_add(n2097, n2099);
    let n2102: ZW = zw_cellmix_n(313u64, n1249, 1542469173u64);
    let n2103: ZW = zw_cellmix_n(313u64, n1249, 668265263u64);
    let n2104: ZW = zw_add(n2100, n2102);
    let n2105: ZW = zw_add(n2101, n2103);
    let n2106: ZW = zw_add(n2090, n2048);
    let n2107: ZW = zw_add(n2091, n2049);
    let n2108: ZW = zw_add(n2106, n2076);
    let n2109: ZW = zw_add(n2107, n2077);
    let n2110: ZW = zw_add(n2108, n1908);
    let n2111: ZW = zw_add(n2109, n1909);
    let n2112: ZW = zw_cellmix_n(312u64, n1255, 1542469173u64);
    let n2113: ZW = zw_cellmix_n(312u64, n1255, 668265263u64);
    let n2114: ZW = zw_add(n2110, n2112);
    let n2115: ZW = zw_add(n2111, n2113);
    let n2116: ZW = zw_cellmix_n(313u64, n1253, 1542469173u64);
    let n2117: ZW = zw_cellmix_n(313u64, n1253, 668265263u64);
    let n2118: ZW = zw_add(n2114, n2116);
    let n2119: ZW = zw_add(n2115, n2117);
    let n2120: ZW = zw_cellmix_n(303u64, n1256, 1542469173u64);
    let n2121: ZW = zw_cellmix_n(303u64, n1256, 668265263u64);
    let n2122: ZW = zw_add(n2074, n2120);
    let n2123: ZW = zw_add(n2075, n2121);
    let n2124: ZW = zw_add(n2122, n1884);
    let n2125: ZW = zw_add(n2123, n1885);
    let n2126: ZW = zw_add(n2124, n2082);
    let n2127: ZW = zw_add(n2125, n2083);
    let n2128: ZW = zw_cellmix_n(313u64, n1257, 1542469173u64);
    let n2129: ZW = zw_cellmix_n(313u64, n1257, 668265263u64);
    let n2130: ZW = zw_add(n2126, n2128);
    let n2131: ZW = zw_add(n2127, n2129);
    let n2132: ZW = zw_add(n2092, n2120);
    let n2133: ZW = zw_add(n2093, n2121);
    let n2134: ZW = zw_add(n2132, n1896);
    let n2135: ZW = zw_add(n2133, n1897);
    let n2136: ZW = zw_add(n2134, n2098);
    let n2137: ZW = zw_add(n2135, n2099);
    let n2138: ZW = zw_cellmix_n(313u64, n1258, 1542469173u64);
    let n2139: ZW = zw_cellmix_n(313u64, n1258, 668265263u64);
    let n2140: ZW = zw_add(n2136, n2138);
    let n2141: ZW = zw_add(n2137, n2139);
    let n2142: ZW = zw_add(n2106, n2120);
    let n2143: ZW = zw_add(n2107, n2121);
    let n2144: ZW = zw_add(n2142, n1908);
    let n2145: ZW = zw_add(n2143, n1909);
    let n2146: ZW = zw_add(n2144, n2112);
    let n2147: ZW = zw_add(n2145, n2113);
    let n2148: ZW = zw_cellmix_n(313u64, n1259, 1542469173u64);
    let n2149: ZW = zw_cellmix_n(313u64, n1259, 668265263u64);
    let n2150: ZW = zw_add(n2146, n2148);
    let n2151: ZW = zw_add(n2147, n2149);
    let n2152: ZW = zw_add(n1988, n1920);
    let n2153: ZW = zw_add(n1989, n1921);
    let n2154: ZW = zw_add(n2152, n1992);
    let n2155: ZW = zw_add(n2153, n1993);
    let n2156: ZW = zw_add(n2154, n1926);
    let n2157: ZW = zw_add(n2155, n1927);
    let n2158: ZW = zw_add(n2156, n1998);
    let n2159: ZW = zw_add(n2157, n1999);
    let n2160: ZW = zw_add(n2158, n2002);
    let n2161: ZW = zw_add(n2159, n2003);
    let n2162: ZW = zw_add(n2160, n2006);
    let n2163: ZW = zw_add(n2161, n2007);
    let n2164: ZW = zw_add(n2162, n2010);
    let n2165: ZW = zw_add(n2163, n2011);
    let n2166: ZW = zw_add(n2164, n2014);
    let n2167: ZW = zw_add(n2165, n2015);
    let n2168: ZW = zw_add(n2166, n1884);
    let n2169: ZW = zw_add(n2167, n1885);
    let n2170: ZW = zw_cellmix_n(312u64, n1263, 1542469173u64);
    let n2171: ZW = zw_cellmix_n(312u64, n1263, 668265263u64);
    let n2172: ZW = zw_add(n2168, n2170);
    let n2173: ZW = zw_add(n2169, n2171);
    let n2174: ZW = zw_cellmix_n(313u64, n1261, 1542469173u64);
    let n2175: ZW = zw_cellmix_n(313u64, n1261, 668265263u64);
    let n2176: ZW = zw_add(n2172, n2174);
    let n2177: ZW = zw_add(n2173, n2175);
    let n2178: ZW = zw_add(n2160, n2028);
    let n2179: ZW = zw_add(n2161, n2029);
    let n2180: ZW = zw_add(n2178, n2032);
    let n2181: ZW = zw_add(n2179, n2033);
    let n2182: ZW = zw_add(n2180, n2014);
    let n2183: ZW = zw_add(n2181, n2015);
    let n2184: ZW = zw_add(n2182, n1896);
    let n2185: ZW = zw_add(n2183, n1897);
    let n2186: ZW = zw_cellmix_n(312u64, n1267, 1542469173u64);
    let n2187: ZW = zw_cellmix_n(312u64, n1267, 668265263u64);
    let n2188: ZW = zw_add(n2184, n2186);
    let n2189: ZW = zw_add(n2185, n2187);
    let n2190: ZW = zw_cellmix_n(313u64, n1265, 1542469173u64);
    let n2191: ZW = zw_cellmix_n(313u64, n1265, 668265263u64);
    let n2192: ZW = zw_add(n2188, n2190);
    let n2193: ZW = zw_add(n2189, n2191);
    let n2194: ZW = zw_add(n2178, n2048);
    let n2195: ZW = zw_add(n2179, n2049);
    let n2196: ZW = zw_add(n2194, n2014);
    let n2197: ZW = zw_add(n2195, n2015);
    let n2198: ZW = zw_add(n2196, n1908);
    let n2199: ZW = zw_add(n2197, n1909);
    let n2200: ZW = zw_cellmix_n(312u64, n1271, 1542469173u64);
    let n2201: ZW = zw_cellmix_n(312u64, n1271, 668265263u64);
    let n2202: ZW = zw_add(n2198, n2200);
    let n2203: ZW = zw_add(n2199, n2201);
    let n2204: ZW = zw_cellmix_n(313u64, n1269, 1542469173u64);
    let n2205: ZW = zw_cellmix_n(313u64, n1269, 668265263u64);
    let n2206: ZW = zw_add(n2202, n2204);
    let n2207: ZW = zw_add(n2203, n2205);
    let n2208: ZW = zw_add(n2158, n2064);
    let n2209: ZW = zw_add(n2159, n2065);
    let n2210: ZW = zw_add(n2208, n2068);
    let n2211: ZW = zw_add(n2209, n2069);
    let n2212: ZW = zw_add(n2210, n2072);
    let n2213: ZW = zw_add(n2211, n2073);
    let n2214: ZW = zw_add(n2212, n2076);
    let n2215: ZW = zw_add(n2213, n2077);
    let n2216: ZW = zw_add(n2214, n1884);
    let n2217: ZW = zw_add(n2215, n1885);
    let n2218: ZW = zw_cellmix_n(312u64, n1275, 1542469173u64);
    let n2219: ZW = zw_cellmix_n(312u64, n1275, 668265263u64);
    let n2220: ZW = zw_add(n2216, n2218);
    let n2221: ZW = zw_add(n2217, n2219);
    let n2222: ZW = zw_cellmix_n(313u64, n1273, 1542469173u64);
    let n2223: ZW = zw_cellmix_n(313u64, n1273, 668265263u64);
    let n2224: ZW = zw_add(n2220, n2222);
    let n2225: ZW = zw_add(n2221, n2223);
    let n2226: ZW = zw_add(n2208, n2028);
    let n2227: ZW = zw_add(n2209, n2029);
    let n2228: ZW = zw_add(n2226, n2032);
    let n2229: ZW = zw_add(n2227, n2033);
    let n2230: ZW = zw_add(n2228, n2076);
    let n2231: ZW = zw_add(n2229, n2077);
    let n2232: ZW = zw_add(n2230, n1896);
    let n2233: ZW = zw_add(n2231, n1897);
    let n2234: ZW = zw_cellmix_n(312u64, n1279, 1542469173u64);
    let n2235: ZW = zw_cellmix_n(312u64, n1279, 668265263u64);
    let n2236: ZW = zw_add(n2232, n2234);
    let n2237: ZW = zw_add(n2233, n2235);
    let n2238: ZW = zw_cellmix_n(313u64, n1277, 1542469173u64);
    let n2239: ZW = zw_cellmix_n(313u64, n1277, 668265263u64);
    let n2240: ZW = zw_add(n2236, n2238);
    let n2241: ZW = zw_add(n2237, n2239);
    let n2242: ZW = zw_add(n2226, n2048);
    let n2243: ZW = zw_add(n2227, n2049);
    let n2244: ZW = zw_add(n2242, n2076);
    let n2245: ZW = zw_add(n2243, n2077);
    let n2246: ZW = zw_add(n2244, n1908);
    let n2247: ZW = zw_add(n2245, n1909);
    let n2248: ZW = zw_cellmix_n(312u64, n1283, 1542469173u64);
    let n2249: ZW = zw_cellmix_n(312u64, n1283, 668265263u64);
    let n2250: ZW = zw_add(n2246, n2248);
    let n2251: ZW = zw_add(n2247, n2249);
    let n2252: ZW = zw_cellmix_n(313u64, n1281, 1542469173u64);
    let n2253: ZW = zw_cellmix_n(313u64, n1281, 668265263u64);
    let n2254: ZW = zw_add(n2250, n2252);
    let n2255: ZW = zw_add(n2251, n2253);
    let n2256: ZW = zw_add(n2212, n2120);
    let n2257: ZW = zw_add(n2213, n2121);
    let n2258: ZW = zw_add(n2256, n1884);
    let n2259: ZW = zw_add(n2257, n1885);
    let n2260: ZW = zw_add(n2258, n2218);
    let n2261: ZW = zw_add(n2259, n2219);
    let n2262: ZW = zw_cellmix_n(313u64, n1284, 1542469173u64);
    let n2263: ZW = zw_cellmix_n(313u64, n1284, 668265263u64);
    let n2264: ZW = zw_add(n2260, n2262);
    let n2265: ZW = zw_add(n2261, n2263);
    let n2266: ZW = zw_add(n2228, n2120);
    let n2267: ZW = zw_add(n2229, n2121);
    let n2268: ZW = zw_add(n2266, n1896);
    let n2269: ZW = zw_add(n2267, n1897);
    let n2270: ZW = zw_add(n2268, n2234);
    let n2271: ZW = zw_add(n2269, n2235);
    let n2272: ZW = zw_cellmix_n(313u64, n1285, 1542469173u64);
    let n2273: ZW = zw_cellmix_n(313u64, n1285, 668265263u64);
    let n2274: ZW = zw_add(n2270, n2272);
    let n2275: ZW = zw_add(n2271, n2273);
    let n2276: ZW = zw_add(n2242, n2120);
    let n2277: ZW = zw_add(n2243, n2121);
    let n2278: ZW = zw_add(n2276, n1908);
    let n2279: ZW = zw_add(n2277, n1909);
    let n2280: ZW = zw_add(n2278, n2248);
    let n2281: ZW = zw_add(n2279, n2249);
    let n2282: ZW = zw_cellmix_n(313u64, n1286, 1542469173u64);
    let n2283: ZW = zw_cellmix_n(313u64, n1286, 668265263u64);
    let n2284: ZW = zw_add(n2280, n2282);
    let n2285: ZW = zw_add(n2281, n2283);
    let ok_v0_b0: u16 = ALL & zb_holds(n694) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v0_b0: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v0_b0: u16 = ALL & zb_holds(n808) & zb_holds(n804) & zb_holds(n802) & zb_holds(n798) & zb_holds(n99) & zb_holds(n693) & zb_holds(n695) & zb_holds(n795);
    let ok_v1_b1: u16 = ALL & zb_holds(n694) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v1_b1: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v1_b1: u16 = ALL & zb_holds(n808) & zb_holds(n804) & zb_holds(n802) & zb_holds(n798) & zb_holds(n99) & zb_holds(n693) & zb_holds(n695) & zb_holds(n795);
    let ok_v2_b2: u16 = ALL & zb_holds(n694) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v2_b2: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v2_b2: u16 = ALL & zb_holds(n808) & zb_holds(n804) & zb_holds(n802) & zb_holds(n798) & zb_holds(n99) & zb_holds(n693) & zb_holds(n695) & zb_holds(n795);
    let ok_v16_b3: u16 = ALL & zb_holds(n694) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v16_b3: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v16_b3: u16 = ALL & zb_holds(n693) & zb_holds(n695) & zb_holds(n795) & zb_holds(n808) & zb_holds(n804) & zb_holds(n802) & zb_holds(n99) & zb_holds(n798);
    let ok_v17_b4: u16 = ALL & zb_holds(n694) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v17_b4: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v17_b4: u16 = ALL & zb_holds(n693) & zb_holds(n695) & zb_holds(n795) & zb_holds(n808) & zb_holds(n804) & zb_holds(n802) & zb_holds(n99) & zb_holds(n798);
    let ok_v18_b5: u16 = ALL & zb_holds(n694) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v18_b5: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v18_b5: u16 = ALL & zb_holds(n693) & zb_holds(n695) & zb_holds(n795) & zb_holds(n808) & zb_holds(n804) & zb_holds(n802) & zb_holds(n99) & zb_holds(n798);
    let ok_v32_b6: u16 = ALL & zb_holds(n694) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v32_b6: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v32_b6: u16 = ALL & zb_holds(n808) & zb_holds(n804) & zb_holds(n693) & zb_holds(n695) & zb_holds(n795) & zb_holds(n798) & zb_holds(n802);
    let ok_v33_b7: u16 = ALL & zb_holds(n694) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v33_b7: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v33_b7: u16 = ALL & zb_holds(n808) & zb_holds(n804) & zb_holds(n693) & zb_holds(n695) & zb_holds(n795) & zb_holds(n798) & zb_holds(n802);
    let ok_v34_b8: u16 = ALL & zb_holds(n694) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v34_b8: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v34_b8: u16 = ALL & zb_holds(n808) & zb_holds(n804) & zb_holds(n693) & zb_holds(n695) & zb_holds(n795) & zb_holds(n798) & zb_holds(n802);
    let ok_v36_b9: u16 = ALL & zb_holds(n694) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v36_b9: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v36_b9: u16 = ALL & zb_holds(n808) & zb_holds(n804) & zb_holds(n693) & zb_holds(n695) & zb_holds(n795) & zb_holds(n798) & zb_holds(n802);
    let ok_v37_b10: u16 = ALL & zb_holds(n694) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v37_b10: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v37_b10: u16 = ALL & zb_holds(n808) & zb_holds(n804) & zb_holds(n693) & zb_holds(n695) & zb_holds(n795) & zb_holds(n798) & zb_holds(n802);
    let ok_v38_b11: u16 = ALL & zb_holds(n694) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v38_b11: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v38_b11: u16 = ALL & zb_holds(n808) & zb_holds(n804) & zb_holds(n693) & zb_holds(n695) & zb_holds(n795) & zb_holds(n798) & zb_holds(n802);
    let ok_v40_b12: u16 = ALL & zb_holds(n694) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v40_b12: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v40_b12: u16 = ALL & zb_holds(n808) & zb_holds(n804) & zb_holds(n693) & zb_holds(n695) & zb_holds(n795) & zb_holds(n798) & zb_holds(n802);
    let ok_v41_b13: u16 = ALL & zb_holds(n694) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v41_b13: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v41_b13: u16 = ALL & zb_holds(n808) & zb_holds(n804) & zb_holds(n693) & zb_holds(n695) & zb_holds(n795) & zb_holds(n798) & zb_holds(n802);
    let ok_v42_b14: u16 = ALL & zb_holds(n694) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v42_b14: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v42_b14: u16 = ALL & zb_holds(n808) & zb_holds(n804) & zb_holds(n693) & zb_holds(n695) & zb_holds(n795) & zb_holds(n798) & zb_holds(n802);
    let ok_v48_b15: u16 = ALL & zb_holds(n694) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v48_b15: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v48_b15: u16 = ALL & zb_holds(n693) & zb_holds(n695) & zb_holds(n808) & zb_holds(n804) & zb_holds(n802) & zb_holds(n795) & zb_holds(n798);
    let ok_v49_b16: u16 = ALL & zb_holds(n694) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v49_b16: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v49_b16: u16 = ALL & zb_holds(n808) & zb_holds(n804) & zb_holds(n693) & zb_holds(n695) & zb_holds(n795) & zb_holds(n798) & zb_holds(n802);
    let ok_v50_b17: u16 = ALL & zb_holds(n694) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v50_b17: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v50_b17: u16 = ALL & zb_holds(n808) & zb_holds(n804) & zb_holds(n693) & zb_holds(n695) & zb_holds(n795) & zb_holds(n798) & zb_holds(n802);
    let ok_v52_b18: u16 = ALL & zb_holds(n694) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v52_b18: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v52_b18: u16 = ALL & zb_holds(n808) & zb_holds(n804) & zb_holds(n693) & zb_holds(n695) & zb_holds(n795) & zb_holds(n798) & zb_holds(n802);
    let ok_v53_b19: u16 = ALL & zb_holds(n694) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v53_b19: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v53_b19: u16 = ALL & zb_holds(n808) & zb_holds(n804) & zb_holds(n693) & zb_holds(n695) & zb_holds(n795) & zb_holds(n798) & zb_holds(n802);
    let ok_v54_b20: u16 = ALL & zb_holds(n694) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v54_b20: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v54_b20: u16 = ALL & zb_holds(n808) & zb_holds(n804) & zb_holds(n693) & zb_holds(n695) & zb_holds(n795) & zb_holds(n798) & zb_holds(n802);
    let ok_v56_b21: u16 = ALL & zb_holds(n694) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v56_b21: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v56_b21: u16 = ALL & zb_holds(n808) & zb_holds(n804) & zb_holds(n693) & zb_holds(n695) & zb_holds(n795) & zb_holds(n798) & zb_holds(n802);
    let ok_v57_b22: u16 = ALL & zb_holds(n694) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v57_b22: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v57_b22: u16 = ALL & zb_holds(n808) & zb_holds(n804) & zb_holds(n693) & zb_holds(n695) & zb_holds(n795) & zb_holds(n798) & zb_holds(n802);
    let ok_v58_b23: u16 = ALL & zb_holds(n694) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v58_b23: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v58_b23: u16 = ALL & zb_holds(n808) & zb_holds(n804) & zb_holds(n693) & zb_holds(n695) & zb_holds(n795) & zb_holds(n798) & zb_holds(n802);
    let ok_v0_b24: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n1133);
    let bd_v0_b24: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v0_b24: u16 = ALL & zb_holds(n795) & zb_holds(n1132);
    let ok_v32_b25: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n1133);
    let bd_v32_b25: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v32_b25: u16 = ALL & zb_holds(n795) & zb_holds(n1132);
    let ok_v0_b26: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n1150);
    let bd_v0_b26: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v0_b26: u16 = ALL & zb_holds(n1149);
    let ok_v32_b27: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n1150);
    let bd_v32_b27: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v32_b27: u16 = ALL & zb_holds(n1149);
    let ok_v0_b28: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n1175);
    let bd_v0_b28: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v0_b28: u16 = ALL & zb_holds(n1174);
    let ok_v1_b29: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n1175);
    let bd_v1_b29: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v1_b29: u16 = ALL & zb_holds(n1174);
    let ok_v2_b30: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n1175);
    let bd_v2_b30: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v2_b30: u16 = ALL & zb_holds(n1174);
    let ok_v16_b31: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n1175);
    let bd_v16_b31: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v16_b31: u16 = ALL & zb_holds(n1174);
    let ok_v17_b32: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n1175);
    let bd_v17_b32: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v17_b32: u16 = ALL & zb_holds(n1174);
    let ok_v18_b33: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n1175);
    let bd_v18_b33: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v18_b33: u16 = ALL & zb_holds(n1174);
    let ok_v32_b34: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n1175);
    let bd_v32_b34: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v32_b34: u16 = ALL & zb_holds(n1174);
    let ok_v33_b35: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n1175);
    let bd_v33_b35: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v33_b35: u16 = ALL & zb_holds(n1174);
    let ok_v34_b36: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n1175);
    let bd_v34_b36: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v34_b36: u16 = ALL & zb_holds(n1174);
    let ok_v36_b37: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n1175);
    let bd_v36_b37: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v36_b37: u16 = ALL & zb_holds(n1174);
    let ok_v37_b38: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n1175);
    let bd_v37_b38: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v37_b38: u16 = ALL & zb_holds(n1174);
    let ok_v38_b39: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n1175);
    let bd_v38_b39: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v38_b39: u16 = ALL & zb_holds(n1174);
    let ok_v40_b40: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n1175);
    let bd_v40_b40: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v40_b40: u16 = ALL & zb_holds(n1174);
    let ok_v41_b41: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n1175);
    let bd_v41_b41: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v41_b41: u16 = ALL & zb_holds(n1174);
    let ok_v42_b42: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n1175);
    let bd_v42_b42: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v42_b42: u16 = ALL & zb_holds(n1174);
    let ok_v48_b43: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n1175);
    let bd_v48_b43: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v48_b43: u16 = ALL & zb_holds(n1174);
    let ok_v49_b44: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n1175);
    let bd_v49_b44: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v49_b44: u16 = ALL & zb_holds(n1174);
    let ok_v50_b45: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n1175);
    let bd_v50_b45: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v50_b45: u16 = ALL & zb_holds(n1174);
    let ok_v52_b46: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n1175);
    let bd_v52_b46: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v52_b46: u16 = ALL & zb_holds(n1174);
    let ok_v53_b47: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n1175);
    let bd_v53_b47: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v53_b47: u16 = ALL & zb_holds(n1174);
    let ok_v54_b48: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n1175);
    let bd_v54_b48: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v54_b48: u16 = ALL & zb_holds(n1174);
    let ok_v56_b49: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n1175);
    let bd_v56_b49: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v56_b49: u16 = ALL & zb_holds(n1174);
    let ok_v57_b50: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n1175);
    let bd_v57_b50: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v57_b50: u16 = ALL & zb_holds(n1174);
    let ok_v58_b51: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n1175);
    let bd_v58_b51: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v58_b51: u16 = ALL & zb_holds(n1174);
    let sh0 = KShared0 {
        c87: r_c87,
        c39: r_c39,
        c84: n80,
        c86: n130,
        c280: n363,
        c281: n364,
        c256: n362,
        c85: n131,
    };
    let sh1 = KShared1 {
        c87: n1144,
        c84: n80,
        c86: n130,
        c241: n1136,
        c249: n1140,
        c85: n131,
    };
    let sh2 = KShared2 {
        c87: n1151,
        c39: n1152,
        c84: n80,
        c86: n130,
        c85: n131,
        c38: n1148,
    };
    let sh3 = KShared3 {
        c87: r_c87,
        c39: r_c39,
        c84: n80,
        c86: n130,
        c310: n1170,
        c311: n1171,
        c256: n1166,
        c267: n1167,
        c275: n1168,
        c85: n131,
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
    // 52 distinct button assignments; per outcome they fall
    // into [24, 2, 2, 24] groups that write identical values.
    declined |= live_v0_b0 & (if bd_v0_b0 { ALL } else { !ok_v0_b0 });
    take_0_0 |= live_v0_b0 & ok_v0_b0 & (if bd_v0_b0 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c270: r_c300,
        c271: r_c301,
        c236: n709,
        c272: r_c302,
        c273: r_c303,
        c238: n790,
        c274: n791,
        c241: n708,
        c248: zb_splat(false),
        c249: zb_splat(false),
        c282: n824,
        c283: n793,
        c255: n823,
        h1: n1378, h2: n1379,
    };
    // body 0: buttons 0x00, forks 0x0
    sink.o0(0, take_0_0, &sh0, &o0);
    declined |= live_v1_b1 & (if bd_v1_b1 { ALL } else { !ok_v1_b1 });
    take_0_1 |= live_v1_b1 & ok_v1_b1 & (if bd_v1_b1 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c270: r_c300,
        c271: r_c301,
        c236: n709,
        c272: r_c302,
        c273: r_c303,
        c238: n790,
        c274: n871,
        c241: n708,
        c248: zb_splat(false),
        c249: zb_splat(false),
        c282: n875,
        c283: n873,
        c255: n823,
        h1: n1390, h2: n1391,
    };
    // body 1: buttons 0x01, forks 0x0
    sink.o0(1, take_0_1, &sh0, &o0);
    declined |= live_v2_b2 & (if bd_v2_b2 { ALL } else { !ok_v2_b2 });
    take_0_2 |= live_v2_b2 & ok_v2_b2 & (if bd_v2_b2 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c270: r_c300,
        c271: r_c301,
        c236: n709,
        c272: r_c302,
        c273: r_c303,
        c238: n790,
        c274: n896,
        c241: n708,
        c248: zb_splat(false),
        c249: zb_splat(false),
        c282: n900,
        c283: n898,
        c255: n823,
        h1: n1402, h2: n1403,
    };
    // body 2: buttons 0x02, forks 0x0
    sink.o0(2, take_0_2, &sh0, &o0);
    declined |= live_v16_b3 & (if bd_v16_b3 { ALL } else { !ok_v16_b3 });
    take_0_3 |= live_v16_b3 & ok_v16_b3 & (if bd_v16_b3 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c270: r_c300,
        c271: r_c301,
        c236: n709,
        c272: r_c302,
        c273: r_c303,
        c238: n790,
        c274: n791,
        c241: n904,
        c248: zb_splat(false),
        c249: zb_splat(true),
        c282: n913,
        c283: n906,
        c255: n823,
        h1: n1432, h2: n1433,
    };
    // body 3: buttons 0x10, forks 0x0
    sink.o0(16, take_0_3, &sh0, &o0);
    declined |= live_v17_b4 & (if bd_v17_b4 { ALL } else { !ok_v17_b4 });
    take_0_4 |= live_v17_b4 & ok_v17_b4 & (if bd_v17_b4 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c270: r_c300,
        c271: r_c301,
        c236: n709,
        c272: r_c302,
        c273: r_c303,
        c238: n790,
        c274: n871,
        c241: n904,
        c248: zb_splat(false),
        c249: zb_splat(true),
        c282: n919,
        c283: n917,
        c255: n823,
        h1: n1442, h2: n1443,
    };
    // body 4: buttons 0x11, forks 0x0
    sink.o0(17, take_0_4, &sh0, &o0);
    declined |= live_v18_b5 & (if bd_v18_b5 { ALL } else { !ok_v18_b5 });
    take_0_5 |= live_v18_b5 & ok_v18_b5 & (if bd_v18_b5 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c270: r_c300,
        c271: r_c301,
        c236: n709,
        c272: r_c302,
        c273: r_c303,
        c238: n790,
        c274: n896,
        c241: n904,
        c248: zb_splat(false),
        c249: zb_splat(true),
        c282: n925,
        c283: n923,
        c255: n823,
        h1: n1452, h2: n1453,
    };
    // body 5: buttons 0x12, forks 0x0
    sink.o0(18, take_0_5, &sh0, &o0);
    declined |= live_v32_b6 & (if bd_v32_b6 { ALL } else { !ok_v32_b6 });
    take_0_6 |= live_v32_b6 & ok_v32_b6 & (if bd_v32_b6 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n937,
        c41: n938,
        c270: n941,
        c271: n942,
        c236: n939,
        c272: n943,
        c273: n944,
        c238: n940,
        c274: n791,
        c241: n708,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n950,
        c283: n946,
        c255: n949,
        h1: n1506, h2: n1507,
    };
    // body 6: buttons 0x20, forks 0x0
    sink.o0(32, take_0_6, &sh0, &o0);
    declined |= live_v33_b7 & (if bd_v33_b7 { ALL } else { !ok_v33_b7 });
    take_0_7 |= live_v33_b7 & ok_v33_b7 & (if bd_v33_b7 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n937,
        c41: n938,
        c270: n941,
        c271: n958,
        c236: n939,
        c272: n959,
        c273: n944,
        c238: n940,
        c274: n871,
        c241: n708,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n963,
        c283: n961,
        c255: n949,
        h1: n1526, h2: n1527,
    };
    // body 7: buttons 0x21, forks 0x0
    sink.o0(33, take_0_7, &sh0, &o0);
    declined |= live_v34_b8 & (if bd_v34_b8 { ALL } else { !ok_v34_b8 });
    take_0_8 |= live_v34_b8 & ok_v34_b8 & (if bd_v34_b8 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n937,
        c41: n938,
        c270: n941,
        c271: n958,
        c236: n939,
        c272: n967,
        c273: n944,
        c238: n940,
        c274: n896,
        c241: n708,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n971,
        c283: n969,
        c255: n949,
        h1: n1542, h2: n1543,
    };
    // body 8: buttons 0x22, forks 0x0
    sink.o0(34, take_0_8, &sh0, &o0);
    declined |= live_v36_b9 & (if bd_v36_b9 { ALL } else { !ok_v36_b9 });
    take_0_9 |= live_v36_b9 & ok_v36_b9 & (if bd_v36_b9 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n937,
        c41: n938,
        c270: n979,
        c271: n980,
        c236: n939,
        c272: n981,
        c273: n982,
        c238: n940,
        c274: n791,
        c241: n708,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n986,
        c283: n984,
        c255: n949,
        h1: n1568, h2: n1569,
    };
    // body 9: buttons 0x24, forks 0x0
    sink.o0(36, take_0_9, &sh0, &o0);
    declined |= live_v37_b10 & (if bd_v37_b10 { ALL } else { !ok_v37_b10 });
    take_0_10 |= live_v37_b10 & ok_v37_b10 & (if bd_v37_b10 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n937,
        c41: n938,
        c270: n979,
        c271: n958,
        c236: n939,
        c272: n959,
        c273: n982,
        c238: n940,
        c274: n871,
        c241: n708,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n992,
        c283: n990,
        c255: n949,
        h1: n1584, h2: n1585,
    };
    // body 10: buttons 0x25, forks 0x0
    sink.o0(37, take_0_10, &sh0, &o0);
    declined |= live_v38_b11 & (if bd_v38_b11 { ALL } else { !ok_v38_b11 });
    take_0_11 |= live_v38_b11 & ok_v38_b11 & (if bd_v38_b11 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n937,
        c41: n938,
        c270: n979,
        c271: n958,
        c236: n939,
        c272: n967,
        c273: n982,
        c238: n940,
        c274: n896,
        c241: n708,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n998,
        c283: n996,
        c255: n949,
        h1: n1598, h2: n1599,
    };
    // body 11: buttons 0x26, forks 0x0
    sink.o0(38, take_0_11, &sh0, &o0);
    declined |= live_v40_b12 & (if bd_v40_b12 { ALL } else { !ok_v40_b12 });
    take_0_12 |= live_v40_b12 & ok_v40_b12 & (if bd_v40_b12 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n937,
        c41: n938,
        c270: n979,
        c271: n980,
        c236: n939,
        c272: n981,
        c273: n1001,
        c238: n940,
        c274: n791,
        c241: n708,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n986,
        c283: n1002,
        c255: n949,
        h1: n1610, h2: n1611,
    };
    // body 12: buttons 0x28, forks 0x0
    sink.o0(40, take_0_12, &sh0, &o0);
    declined |= live_v41_b13 & (if bd_v41_b13 { ALL } else { !ok_v41_b13 });
    take_0_13 |= live_v41_b13 & ok_v41_b13 & (if bd_v41_b13 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n937,
        c41: n938,
        c270: n979,
        c271: n958,
        c236: n939,
        c272: n959,
        c273: n1001,
        c238: n940,
        c274: n871,
        c241: n708,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n992,
        c283: n1004,
        c255: n949,
        h1: n1620, h2: n1621,
    };
    // body 13: buttons 0x29, forks 0x0
    sink.o0(41, take_0_13, &sh0, &o0);
    declined |= live_v42_b14 & (if bd_v42_b14 { ALL } else { !ok_v42_b14 });
    take_0_14 |= live_v42_b14 & ok_v42_b14 & (if bd_v42_b14 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n937,
        c41: n938,
        c270: n979,
        c271: n958,
        c236: n939,
        c272: n967,
        c273: n1001,
        c238: n940,
        c274: n896,
        c241: n708,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n998,
        c283: n1006,
        c255: n949,
        h1: n1630, h2: n1631,
    };
    // body 14: buttons 0x2a, forks 0x0
    sink.o0(42, take_0_14, &sh0, &o0);
    declined |= live_v48_b15 & (if bd_v48_b15 { ALL } else { !ok_v48_b15 });
    take_0_15 |= live_v48_b15 & ok_v48_b15 & (if bd_v48_b15 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n937,
        c41: n938,
        c270: n941,
        c271: n942,
        c236: n939,
        c272: n943,
        c273: n944,
        c238: n940,
        c274: n791,
        c241: n904,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n1012,
        c283: n1010,
        c255: n949,
        h1: n1656, h2: n1657,
    };
    // body 15: buttons 0x30, forks 0x0
    sink.o0(48, take_0_15, &sh0, &o0);
    declined |= live_v49_b16 & (if bd_v49_b16 { ALL } else { !ok_v49_b16 });
    take_0_16 |= live_v49_b16 & ok_v49_b16 & (if bd_v49_b16 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n937,
        c41: n938,
        c270: n941,
        c271: n958,
        c236: n939,
        c272: n959,
        c273: n944,
        c238: n940,
        c274: n871,
        c241: n904,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n1023,
        c283: n1021,
        c255: n949,
        h1: n1672, h2: n1673,
    };
    // body 16: buttons 0x31, forks 0x0
    sink.o0(49, take_0_16, &sh0, &o0);
    declined |= live_v50_b17 & (if bd_v50_b17 { ALL } else { !ok_v50_b17 });
    take_0_17 |= live_v50_b17 & ok_v50_b17 & (if bd_v50_b17 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n937,
        c41: n938,
        c270: n941,
        c271: n958,
        c236: n939,
        c272: n967,
        c273: n944,
        c238: n940,
        c274: n896,
        c241: n904,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n1029,
        c283: n1027,
        c255: n949,
        h1: n1686, h2: n1687,
    };
    // body 17: buttons 0x32, forks 0x0
    sink.o0(50, take_0_17, &sh0, &o0);
    declined |= live_v52_b18 & (if bd_v52_b18 { ALL } else { !ok_v52_b18 });
    take_0_18 |= live_v52_b18 & ok_v52_b18 & (if bd_v52_b18 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n937,
        c41: n938,
        c270: n979,
        c271: n980,
        c236: n939,
        c272: n981,
        c273: n982,
        c238: n940,
        c274: n791,
        c241: n904,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n1035,
        c283: n1033,
        c255: n949,
        h1: n1704, h2: n1705,
    };
    // body 18: buttons 0x34, forks 0x0
    sink.o0(52, take_0_18, &sh0, &o0);
    declined |= live_v53_b19 & (if bd_v53_b19 { ALL } else { !ok_v53_b19 });
    take_0_19 |= live_v53_b19 & ok_v53_b19 & (if bd_v53_b19 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n937,
        c41: n938,
        c270: n979,
        c271: n958,
        c236: n939,
        c272: n959,
        c273: n982,
        c238: n940,
        c274: n871,
        c241: n904,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n1041,
        c283: n1039,
        c255: n949,
        h1: n1720, h2: n1721,
    };
    // body 19: buttons 0x35, forks 0x0
    sink.o0(53, take_0_19, &sh0, &o0);
    declined |= live_v54_b20 & (if bd_v54_b20 { ALL } else { !ok_v54_b20 });
    take_0_20 |= live_v54_b20 & ok_v54_b20 & (if bd_v54_b20 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n937,
        c41: n938,
        c270: n979,
        c271: n958,
        c236: n939,
        c272: n967,
        c273: n982,
        c238: n940,
        c274: n896,
        c241: n904,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n1047,
        c283: n1045,
        c255: n949,
        h1: n1734, h2: n1735,
    };
    // body 20: buttons 0x36, forks 0x0
    sink.o0(54, take_0_20, &sh0, &o0);
    declined |= live_v56_b21 & (if bd_v56_b21 { ALL } else { !ok_v56_b21 });
    take_0_21 |= live_v56_b21 & ok_v56_b21 & (if bd_v56_b21 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n937,
        c41: n938,
        c270: n979,
        c271: n980,
        c236: n939,
        c272: n981,
        c273: n1001,
        c238: n940,
        c274: n791,
        c241: n904,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n1035,
        c283: n1049,
        c255: n949,
        h1: n1744, h2: n1745,
    };
    // body 21: buttons 0x38, forks 0x0
    sink.o0(56, take_0_21, &sh0, &o0);
    declined |= live_v57_b22 & (if bd_v57_b22 { ALL } else { !ok_v57_b22 });
    take_0_22 |= live_v57_b22 & ok_v57_b22 & (if bd_v57_b22 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n937,
        c41: n938,
        c270: n979,
        c271: n958,
        c236: n939,
        c272: n959,
        c273: n1001,
        c238: n940,
        c274: n871,
        c241: n904,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n1041,
        c283: n1051,
        c255: n949,
        h1: n1754, h2: n1755,
    };
    // body 22: buttons 0x39, forks 0x0
    sink.o0(57, take_0_22, &sh0, &o0);
    declined |= live_v58_b23 & (if bd_v58_b23 { ALL } else { !ok_v58_b23 });
    take_0_23 |= live_v58_b23 & ok_v58_b23 & (if bd_v58_b23 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n937,
        c41: n938,
        c270: n979,
        c271: n958,
        c236: n939,
        c272: n967,
        c273: n1001,
        c238: n940,
        c274: n896,
        c241: n904,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n1047,
        c283: n1053,
        c255: n949,
        h1: n1764, h2: n1765,
    };
    // body 23: buttons 0x3a, forks 0x0
    sink.o0(58, take_0_23, &sh0, &o0);
    declined |= live_v0_b24 & (if bd_v0_b24 { ALL } else { !ok_v0_b24 });
    take_1_0 |= live_v0_b24 & ok_v0_b24 & (if bd_v0_b24 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: r_c41,
        h1: n1786, h2: n1787,
    };
    // body 24: buttons 0x00, forks 0x0
    sink.o1(0, take_1_0, &sh1, &o1);
    declined |= live_v32_b25 & (if bd_v32_b25 { ALL } else { !ok_v32_b25 });
    take_1_1 |= live_v32_b25 & ok_v32_b25 & (if bd_v32_b25 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n937,
        c41: n938,
        h1: n1790, h2: n1791,
    };
    // body 25: buttons 0x20, forks 0x0
    sink.o1(32, take_1_1, &sh1, &o1);
    declined |= live_v0_b26 & (if bd_v0_b26 { ALL } else { !ok_v0_b26 });
    take_2_0 |= live_v0_b26 & ok_v0_b26 & (if bd_v0_b26 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: r_c20,
        h1: n1810, h2: n1811,
    };
    // body 26: buttons 0x00, forks 0x0
    sink.o2(0, take_2_0, &sh2, &o2);
    declined |= live_v32_b27 & (if bd_v32_b27 { ALL } else { !ok_v32_b27 });
    take_2_1 |= live_v32_b27 & ok_v32_b27 & (if bd_v32_b27 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n937,
        h1: n1812, h2: n1813,
    };
    // body 27: buttons 0x20, forks 0x0
    sink.o2(32, take_2_1, &sh2, &o2);
    declined |= live_v0_b28 & (if bd_v0_b28 { ALL } else { !ok_v0_b28 });
    take_3_0 |= live_v0_b28 & ok_v0_b28 & (if bd_v0_b28 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1158,
        c41: r_c41,
        c300: r_c300,
        c301: r_c301,
        c236: n1159,
        c302: r_c302,
        c303: r_c303,
        c238: n1160,
        c239: n1161,
        c304: n1169,
        c241: n1162,
        c248: n1163,
        c249: n1164,
        c312: n1185,
        c313: n1173,
        c255: n1184,
        h1: n1894, h2: n1895,
    };
    // body 28: buttons 0x00, forks 0x0
    sink.o3(0, take_3_0, &sh3, &o3);
    declined |= live_v1_b29 & (if bd_v1_b29 { ALL } else { !ok_v1_b29 });
    take_3_1 |= live_v1_b29 & ok_v1_b29 & (if bd_v1_b29 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1158,
        c41: r_c41,
        c300: r_c300,
        c301: r_c301,
        c236: n1159,
        c302: r_c302,
        c303: r_c303,
        c238: n1160,
        c239: n1161,
        c304: n1187,
        c241: n1162,
        c248: n1163,
        c249: n1164,
        c312: n1191,
        c313: n1189,
        c255: n1184,
        h1: n1906, h2: n1907,
    };
    // body 29: buttons 0x01, forks 0x0
    sink.o3(1, take_3_1, &sh3, &o3);
    declined |= live_v2_b30 & (if bd_v2_b30 { ALL } else { !ok_v2_b30 });
    take_3_2 |= live_v2_b30 & ok_v2_b30 & (if bd_v2_b30 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1158,
        c41: r_c41,
        c300: r_c300,
        c301: r_c301,
        c236: n1159,
        c302: r_c302,
        c303: r_c303,
        c238: n1160,
        c239: n1161,
        c304: n1192,
        c241: n1162,
        c248: n1163,
        c249: n1164,
        c312: n1196,
        c313: n1194,
        c255: n1184,
        h1: n1918, h2: n1919,
    };
    // body 30: buttons 0x02, forks 0x0
    sink.o3(2, take_3_2, &sh3, &o3);
    declined |= live_v16_b31 & (if bd_v16_b31 { ALL } else { !ok_v16_b31 });
    take_3_3 |= live_v16_b31 & ok_v16_b31 & (if bd_v16_b31 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1158,
        c41: r_c41,
        c300: r_c300,
        c301: r_c301,
        c236: n1159,
        c302: r_c302,
        c303: r_c303,
        c238: n1160,
        c239: n1161,
        c304: n1169,
        c241: n1197,
        c248: n1163,
        c249: n1198,
        c312: n1202,
        c313: n1200,
        c255: n1184,
        h1: n1948, h2: n1949,
    };
    // body 31: buttons 0x10, forks 0x0
    sink.o3(16, take_3_3, &sh3, &o3);
    declined |= live_v17_b32 & (if bd_v17_b32 { ALL } else { !ok_v17_b32 });
    take_3_4 |= live_v17_b32 & ok_v17_b32 & (if bd_v17_b32 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1158,
        c41: r_c41,
        c300: r_c300,
        c301: r_c301,
        c236: n1159,
        c302: r_c302,
        c303: r_c303,
        c238: n1160,
        c239: n1161,
        c304: n1187,
        c241: n1197,
        c248: n1163,
        c249: n1198,
        c312: n1206,
        c313: n1204,
        c255: n1184,
        h1: n1958, h2: n1959,
    };
    // body 32: buttons 0x11, forks 0x0
    sink.o3(17, take_3_4, &sh3, &o3);
    declined |= live_v18_b33 & (if bd_v18_b33 { ALL } else { !ok_v18_b33 });
    take_3_5 |= live_v18_b33 & ok_v18_b33 & (if bd_v18_b33 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1158,
        c41: r_c41,
        c300: r_c300,
        c301: r_c301,
        c236: n1159,
        c302: r_c302,
        c303: r_c303,
        c238: n1160,
        c239: n1161,
        c304: n1192,
        c241: n1197,
        c248: n1163,
        c249: n1198,
        c312: n1210,
        c313: n1208,
        c255: n1184,
        h1: n1968, h2: n1969,
    };
    // body 33: buttons 0x12, forks 0x0
    sink.o3(18, take_3_5, &sh3, &o3);
    declined |= live_v32_b34 & (if bd_v32_b34 { ALL } else { !ok_v32_b34 });
    take_3_6 |= live_v32_b34 & ok_v32_b34 & (if bd_v32_b34 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1213,
        c41: n1214,
        c300: n1219,
        c301: n1220,
        c236: n1215,
        c302: n1221,
        c303: n1222,
        c238: n1216,
        c239: n1217,
        c304: n1169,
        c241: n1162,
        c248: n1218,
        c249: n1164,
        c312: n1228,
        c313: n1224,
        c255: n1227,
        h1: n2026, h2: n2027,
    };
    // body 34: buttons 0x20, forks 0x0
    sink.o3(32, take_3_6, &sh3, &o3);
    declined |= live_v33_b35 & (if bd_v33_b35 { ALL } else { !ok_v33_b35 });
    take_3_7 |= live_v33_b35 & ok_v33_b35 & (if bd_v33_b35 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1213,
        c41: n1214,
        c300: n1219,
        c301: n1229,
        c236: n1215,
        c302: n1230,
        c303: n1222,
        c238: n1216,
        c239: n1217,
        c304: n1187,
        c241: n1162,
        c248: n1218,
        c249: n1164,
        c312: n1234,
        c313: n1232,
        c255: n1227,
        h1: n2046, h2: n2047,
    };
    // body 35: buttons 0x21, forks 0x0
    sink.o3(33, take_3_7, &sh3, &o3);
    declined |= live_v34_b36 & (if bd_v34_b36 { ALL } else { !ok_v34_b36 });
    take_3_8 |= live_v34_b36 & ok_v34_b36 & (if bd_v34_b36 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1213,
        c41: n1214,
        c300: n1219,
        c301: n1229,
        c236: n1215,
        c302: n1235,
        c303: n1222,
        c238: n1216,
        c239: n1217,
        c304: n1192,
        c241: n1162,
        c248: n1218,
        c249: n1164,
        c312: n1239,
        c313: n1237,
        c255: n1227,
        h1: n2062, h2: n2063,
    };
    // body 36: buttons 0x22, forks 0x0
    sink.o3(34, take_3_8, &sh3, &o3);
    declined |= live_v36_b37 & (if bd_v36_b37 { ALL } else { !ok_v36_b37 });
    take_3_9 |= live_v36_b37 & ok_v36_b37 & (if bd_v36_b37 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1213,
        c41: n1214,
        c300: n1240,
        c301: n1241,
        c236: n1215,
        c302: n1242,
        c303: n1243,
        c238: n1216,
        c239: n1217,
        c304: n1169,
        c241: n1162,
        c248: n1218,
        c249: n1164,
        c312: n1247,
        c313: n1245,
        c255: n1227,
        h1: n2088, h2: n2089,
    };
    // body 37: buttons 0x24, forks 0x0
    sink.o3(36, take_3_9, &sh3, &o3);
    declined |= live_v37_b38 & (if bd_v37_b38 { ALL } else { !ok_v37_b38 });
    take_3_10 |= live_v37_b38 & ok_v37_b38 & (if bd_v37_b38 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1213,
        c41: n1214,
        c300: n1240,
        c301: n1229,
        c236: n1215,
        c302: n1230,
        c303: n1243,
        c238: n1216,
        c239: n1217,
        c304: n1187,
        c241: n1162,
        c248: n1218,
        c249: n1164,
        c312: n1251,
        c313: n1249,
        c255: n1227,
        h1: n2104, h2: n2105,
    };
    // body 38: buttons 0x25, forks 0x0
    sink.o3(37, take_3_10, &sh3, &o3);
    declined |= live_v38_b39 & (if bd_v38_b39 { ALL } else { !ok_v38_b39 });
    take_3_11 |= live_v38_b39 & ok_v38_b39 & (if bd_v38_b39 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1213,
        c41: n1214,
        c300: n1240,
        c301: n1229,
        c236: n1215,
        c302: n1235,
        c303: n1243,
        c238: n1216,
        c239: n1217,
        c304: n1192,
        c241: n1162,
        c248: n1218,
        c249: n1164,
        c312: n1255,
        c313: n1253,
        c255: n1227,
        h1: n2118, h2: n2119,
    };
    // body 39: buttons 0x26, forks 0x0
    sink.o3(38, take_3_11, &sh3, &o3);
    declined |= live_v40_b40 & (if bd_v40_b40 { ALL } else { !ok_v40_b40 });
    take_3_12 |= live_v40_b40 & ok_v40_b40 & (if bd_v40_b40 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1213,
        c41: n1214,
        c300: n1240,
        c301: n1241,
        c236: n1215,
        c302: n1242,
        c303: n1256,
        c238: n1216,
        c239: n1217,
        c304: n1169,
        c241: n1162,
        c248: n1218,
        c249: n1164,
        c312: n1247,
        c313: n1257,
        c255: n1227,
        h1: n2130, h2: n2131,
    };
    // body 40: buttons 0x28, forks 0x0
    sink.o3(40, take_3_12, &sh3, &o3);
    declined |= live_v41_b41 & (if bd_v41_b41 { ALL } else { !ok_v41_b41 });
    take_3_13 |= live_v41_b41 & ok_v41_b41 & (if bd_v41_b41 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1213,
        c41: n1214,
        c300: n1240,
        c301: n1229,
        c236: n1215,
        c302: n1230,
        c303: n1256,
        c238: n1216,
        c239: n1217,
        c304: n1187,
        c241: n1162,
        c248: n1218,
        c249: n1164,
        c312: n1251,
        c313: n1258,
        c255: n1227,
        h1: n2140, h2: n2141,
    };
    // body 41: buttons 0x29, forks 0x0
    sink.o3(41, take_3_13, &sh3, &o3);
    declined |= live_v42_b42 & (if bd_v42_b42 { ALL } else { !ok_v42_b42 });
    take_3_14 |= live_v42_b42 & ok_v42_b42 & (if bd_v42_b42 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1213,
        c41: n1214,
        c300: n1240,
        c301: n1229,
        c236: n1215,
        c302: n1235,
        c303: n1256,
        c238: n1216,
        c239: n1217,
        c304: n1192,
        c241: n1162,
        c248: n1218,
        c249: n1164,
        c312: n1255,
        c313: n1259,
        c255: n1227,
        h1: n2150, h2: n2151,
    };
    // body 42: buttons 0x2a, forks 0x0
    sink.o3(42, take_3_14, &sh3, &o3);
    declined |= live_v48_b43 & (if bd_v48_b43 { ALL } else { !ok_v48_b43 });
    take_3_15 |= live_v48_b43 & ok_v48_b43 & (if bd_v48_b43 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1213,
        c41: n1214,
        c300: n1219,
        c301: n1220,
        c236: n1215,
        c302: n1221,
        c303: n1222,
        c238: n1216,
        c239: n1217,
        c304: n1169,
        c241: n1197,
        c248: n1218,
        c249: n1198,
        c312: n1263,
        c313: n1261,
        c255: n1227,
        h1: n2176, h2: n2177,
    };
    // body 43: buttons 0x30, forks 0x0
    sink.o3(48, take_3_15, &sh3, &o3);
    declined |= live_v49_b44 & (if bd_v49_b44 { ALL } else { !ok_v49_b44 });
    take_3_16 |= live_v49_b44 & ok_v49_b44 & (if bd_v49_b44 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1213,
        c41: n1214,
        c300: n1219,
        c301: n1229,
        c236: n1215,
        c302: n1230,
        c303: n1222,
        c238: n1216,
        c239: n1217,
        c304: n1187,
        c241: n1197,
        c248: n1218,
        c249: n1198,
        c312: n1267,
        c313: n1265,
        c255: n1227,
        h1: n2192, h2: n2193,
    };
    // body 44: buttons 0x31, forks 0x0
    sink.o3(49, take_3_16, &sh3, &o3);
    declined |= live_v50_b45 & (if bd_v50_b45 { ALL } else { !ok_v50_b45 });
    take_3_17 |= live_v50_b45 & ok_v50_b45 & (if bd_v50_b45 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1213,
        c41: n1214,
        c300: n1219,
        c301: n1229,
        c236: n1215,
        c302: n1235,
        c303: n1222,
        c238: n1216,
        c239: n1217,
        c304: n1192,
        c241: n1197,
        c248: n1218,
        c249: n1198,
        c312: n1271,
        c313: n1269,
        c255: n1227,
        h1: n2206, h2: n2207,
    };
    // body 45: buttons 0x32, forks 0x0
    sink.o3(50, take_3_17, &sh3, &o3);
    declined |= live_v52_b46 & (if bd_v52_b46 { ALL } else { !ok_v52_b46 });
    take_3_18 |= live_v52_b46 & ok_v52_b46 & (if bd_v52_b46 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1213,
        c41: n1214,
        c300: n1240,
        c301: n1241,
        c236: n1215,
        c302: n1242,
        c303: n1243,
        c238: n1216,
        c239: n1217,
        c304: n1169,
        c241: n1197,
        c248: n1218,
        c249: n1198,
        c312: n1275,
        c313: n1273,
        c255: n1227,
        h1: n2224, h2: n2225,
    };
    // body 46: buttons 0x34, forks 0x0
    sink.o3(52, take_3_18, &sh3, &o3);
    declined |= live_v53_b47 & (if bd_v53_b47 { ALL } else { !ok_v53_b47 });
    take_3_19 |= live_v53_b47 & ok_v53_b47 & (if bd_v53_b47 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1213,
        c41: n1214,
        c300: n1240,
        c301: n1229,
        c236: n1215,
        c302: n1230,
        c303: n1243,
        c238: n1216,
        c239: n1217,
        c304: n1187,
        c241: n1197,
        c248: n1218,
        c249: n1198,
        c312: n1279,
        c313: n1277,
        c255: n1227,
        h1: n2240, h2: n2241,
    };
    // body 47: buttons 0x35, forks 0x0
    sink.o3(53, take_3_19, &sh3, &o3);
    declined |= live_v54_b48 & (if bd_v54_b48 { ALL } else { !ok_v54_b48 });
    take_3_20 |= live_v54_b48 & ok_v54_b48 & (if bd_v54_b48 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1213,
        c41: n1214,
        c300: n1240,
        c301: n1229,
        c236: n1215,
        c302: n1235,
        c303: n1243,
        c238: n1216,
        c239: n1217,
        c304: n1192,
        c241: n1197,
        c248: n1218,
        c249: n1198,
        c312: n1283,
        c313: n1281,
        c255: n1227,
        h1: n2254, h2: n2255,
    };
    // body 48: buttons 0x36, forks 0x0
    sink.o3(54, take_3_20, &sh3, &o3);
    declined |= live_v56_b49 & (if bd_v56_b49 { ALL } else { !ok_v56_b49 });
    take_3_21 |= live_v56_b49 & ok_v56_b49 & (if bd_v56_b49 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1213,
        c41: n1214,
        c300: n1240,
        c301: n1241,
        c236: n1215,
        c302: n1242,
        c303: n1256,
        c238: n1216,
        c239: n1217,
        c304: n1169,
        c241: n1197,
        c248: n1218,
        c249: n1198,
        c312: n1275,
        c313: n1284,
        c255: n1227,
        h1: n2264, h2: n2265,
    };
    // body 49: buttons 0x38, forks 0x0
    sink.o3(56, take_3_21, &sh3, &o3);
    declined |= live_v57_b50 & (if bd_v57_b50 { ALL } else { !ok_v57_b50 });
    take_3_22 |= live_v57_b50 & ok_v57_b50 & (if bd_v57_b50 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1213,
        c41: n1214,
        c300: n1240,
        c301: n1229,
        c236: n1215,
        c302: n1230,
        c303: n1256,
        c238: n1216,
        c239: n1217,
        c304: n1187,
        c241: n1197,
        c248: n1218,
        c249: n1198,
        c312: n1279,
        c313: n1285,
        c255: n1227,
        h1: n2274, h2: n2275,
    };
    // body 50: buttons 0x39, forks 0x0
    sink.o3(57, take_3_22, &sh3, &o3);
    declined |= live_v58_b51 & (if bd_v58_b51 { ALL } else { !ok_v58_b51 });
    take_3_23 |= live_v58_b51 & ok_v58_b51 & (if bd_v58_b51 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1213,
        c41: n1214,
        c300: n1240,
        c301: n1229,
        c236: n1215,
        c302: n1235,
        c303: n1256,
        c238: n1216,
        c239: n1217,
        c304: n1192,
        c241: n1197,
        c248: n1218,
        c249: n1198,
        c312: n1283,
        c313: n1286,
        c255: n1227,
        h1: n2284, h2: n2285,
    };
    // body 51: buttons 0x3a, forks 0x0
    sink.o3(58, take_3_23, &sh3, &o3);
    declined
}
