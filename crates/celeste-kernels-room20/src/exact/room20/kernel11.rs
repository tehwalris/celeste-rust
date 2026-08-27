// GENERATED from a TRACED frame (shape 11). Do not edit.
//
// One input shape, 2 output shapes, 2 distinct button
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
pub const SHAPE: u64 = 12083715741192450336;

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
    pub c328: P8,
    pub c329: P8,
    pub c330: P8,
    pub c331: P8,
    pub c338: P8,
    pub c339: P8,
    pub c340: P8,
    pub c341: P8,
    pub c348: P8,
    pub c349: P8,
    pub c350: P8,
    pub c351: P8,
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
    pub c237: u16,
    pub c326: u16,
    pub c327: u16,
    pub c245: ZN,
    pub c332: ZN,
    pub c333: ZN,
    pub c247: u16,
    pub c334: ZN,
    pub c335: ZN,
    pub c249: ZN,
    pub c250: ZN,
    pub c252: ZN,
    pub c253: ZN,
    pub c256: u16,
    pub c336: u16,
    pub c337: u16,
    pub c258: ZN,
    pub c259: ZN,
    pub c342: ZN,
    pub c343: ZN,
    pub c267: u16,
    pub c344: ZN,
    pub c345: ZN,
    pub c269: ZN,
    pub c271: ZN,
    pub c272: ZN,
    pub c275: u16,
    pub c276: ZN,
    pub c346: u16,
    pub c347: u16,
    pub c278: ZN,
    pub c279: ZN,
    pub c352: ZN,
    pub c353: ZN,
    pub c287: u16,
    pub c354: ZN,
    pub c355: ZN,
    pub c289: ZN,
    pub c291: ZN,
    pub c292: ZN,
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
    pub c237: u32,
    pub c326: u32,
    pub c327: u32,
    pub c245: u32,
    pub c332: u32,
    pub c333: u32,
    pub c247: u32,
    pub c334: u32,
    pub c335: u32,
    pub c249: u32,
    pub c250: u32,
    pub c252: u32,
    pub c253: u32,
    pub c256: u32,
    pub c336: u32,
    pub c337: u32,
    pub c258: u32,
    pub c259: u32,
    pub c342: u32,
    pub c343: u32,
    pub c267: u32,
    pub c344: u32,
    pub c345: u32,
    pub c269: u32,
    pub c271: u32,
    pub c272: u32,
    pub c275: u32,
    pub c276: u32,
    pub c346: u32,
    pub c347: u32,
    pub c278: u32,
    pub c279: u32,
    pub c352: u32,
    pub c353: u32,
    pub c287: u32,
    pub c354: u32,
    pub c355: u32,
    pub c289: u32,
    pub c291: u32,
    pub c292: u32,
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
        c328: match &b.cols[cell("objects[0].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c329: match &b.cols[cell("objects[0].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c330: match &b.cols[cell("objects[0].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c331: match &b.cols[cell("objects[0].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c338: match &b.cols[cell("objects[1].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c339: match &b.cols[cell("objects[1].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c340: match &b.cols[cell("objects[1].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c341: match &b.cols[cell("objects[1].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c348: match &b.cols[cell("objects[2].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c349: match &b.cols[cell("objects[2].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c350: match &b.cols[cell("objects[2].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c351: match &b.cols[cell("objects[2].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
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
        c237: cell("objects[0].collideable")?,
        c326: cell("objects[0].flip.x")?,
        c327: cell("objects[0].flip.y")?,
        c245: cell("objects[0].off")?,
        c332: cell("objects[0].rem.x")?,
        c333: cell("objects[0].rem.y")?,
        c247: cell("objects[0].solids")?,
        c334: cell("objects[0].spd.x")?,
        c335: cell("objects[0].spd.y")?,
        c249: cell("objects[0].spr")?,
        c250: cell("objects[0].start")?,
        c252: cell("objects[0].x")?,
        c253: cell("objects[0].y")?,
        c256: cell("objects[1].collideable")?,
        c336: cell("objects[1].flip.x")?,
        c337: cell("objects[1].flip.y")?,
        c258: cell("objects[1].hide_for")?,
        c259: cell("objects[1].hide_in")?,
        c342: cell("objects[1].rem.x")?,
        c343: cell("objects[1].rem.y")?,
        c267: cell("objects[1].solids")?,
        c344: cell("objects[1].spd.x")?,
        c345: cell("objects[1].spd.y")?,
        c269: cell("objects[1].spr")?,
        c271: cell("objects[1].x")?,
        c272: cell("objects[1].y")?,
        c275: cell("objects[2].collideable")?,
        c276: cell("objects[2].delay")?,
        c346: cell("objects[2].flip.x")?,
        c347: cell("objects[2].flip.y")?,
        c278: cell("objects[2].hide_for")?,
        c279: cell("objects[2].hide_in")?,
        c352: cell("objects[2].rem.x")?,
        c353: cell("objects[2].rem.y")?,
        c287: cell("objects[2].solids")?,
        c354: cell("objects[2].spd.x")?,
        c355: cell("objects[2].spd.y")?,
        c289: cell("objects[2].spr")?,
        c291: cell("objects[2].x")?,
        c292: cell("objects[2].y")?,
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
        c237: match &b.cols[s.c237 as usize] {
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
        c326: match &b.cols[s.c326 as usize] {
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
        c327: match &b.cols[s.c327 as usize] {
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
        c245: match &b.cols[s.c245 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c332: match &b.cols[s.c332 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c333: match &b.cols[s.c333 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c247: match &b.cols[s.c247 as usize] {
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
        c334: match &b.cols[s.c334 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c335: match &b.cols[s.c335 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c249: match &b.cols[s.c249 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c250: match &b.cols[s.c250 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c252: match &b.cols[s.c252 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c253: match &b.cols[s.c253 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c256: match &b.cols[s.c256 as usize] {
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
        c258: match &b.cols[s.c258 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c259: match &b.cols[s.c259 as usize] {
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
        c267: match &b.cols[s.c267 as usize] {
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
        c269: match &b.cols[s.c269 as usize] {
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
        c275: match &b.cols[s.c275 as usize] {
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
        c276: match &b.cols[s.c276 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
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
        c278: match &b.cols[s.c278 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c279: match &b.cols[s.c279 as usize] {
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
        c287: match &b.cols[s.c287 as usize] {
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
        c289: match &b.cols[s.c289 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c291: match &b.cols[s.c291 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c292: match &b.cols[s.c292 as usize] {
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
    (358, "objects[0].flip.x"),
    (359, "objects[0].flip.y"),
    (360, "objects[0].hitbox.h"),
    (361, "objects[0].hitbox.w"),
    (362, "objects[0].hitbox.x"),
    (363, "objects[0].hitbox.y"),
    (246, "objects[0].off"),
    (364, "objects[0].rem.x"),
    (365, "objects[0].rem.y"),
    (248, "objects[0].solids"),
    (366, "objects[0].spd.x"),
    (367, "objects[0].spd.y"),
    (250, "objects[0].spr"),
    (251, "objects[0].start"),
    (253, "objects[0].x"),
    (254, "objects[0].y"),
    (257, "objects[1].collideable"),
    (258, "objects[1].delay"),
    (368, "objects[1].flip.x"),
    (369, "objects[1].flip.y"),
    (370, "objects[1].hitbox.h"),
    (371, "objects[1].hitbox.w"),
    (372, "objects[1].hitbox.x"),
    (373, "objects[1].hitbox.y"),
    (374, "objects[1].rem.x"),
    (375, "objects[1].rem.y"),
    (267, "objects[1].solids"),
    (376, "objects[1].spd.x"),
    (377, "objects[1].spd.y"),
    (269, "objects[1].spr"),
    (270, "objects[1].state"),
    (378, "objects[1].target.x"),
    (379, "objects[1].target.y"),
    (159, "objects[1].type.tile"),
    (273, "objects[1].x"),
    (274, "objects[1].y"),
    (277, "objects[2].collideable"),
    (380, "objects[2].flip.x"),
    (381, "objects[2].flip.y"),
    (279, "objects[2].hide_for"),
    (280, "objects[2].hide_in"),
    (382, "objects[2].hitbox.h"),
    (383, "objects[2].hitbox.w"),
    (384, "objects[2].hitbox.x"),
    (385, "objects[2].hitbox.y"),
    (386, "objects[2].rem.x"),
    (387, "objects[2].rem.y"),
    (288, "objects[2].solids"),
    (388, "objects[2].spd.x"),
    (389, "objects[2].spd.y"),
    (290, "objects[2].spr"),
    (174, "objects[2].type.tile"),
    (292, "objects[2].x"),
    (293, "objects[2].y"),
    (296, "objects[3].collideable"),
    (390, "objects[3].flip.x"),
    (391, "objects[3].flip.y"),
    (298, "objects[3].hide_for"),
    (299, "objects[3].hide_in"),
    (392, "objects[3].hitbox.h"),
    (393, "objects[3].hitbox.w"),
    (394, "objects[3].hitbox.x"),
    (395, "objects[3].hitbox.y"),
    (396, "objects[3].rem.x"),
    (397, "objects[3].rem.y"),
    (307, "objects[3].solids"),
    (398, "objects[3].spd.x"),
    (399, "objects[3].spd.y"),
    (309, "objects[3].spr"),
    (311, "objects[3].x"),
    (312, "objects[3].y"),
    (43, "pause_player"),
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
    SCell::Obj(&[(21, 255), (20, 256), (11, 257), (35, 258), (14, 259), (15, 260), (19, 261), (18, 262), (22, 263), (23, 264), (24, 265), (4, 266), (12, 267), (3, 268), (13, 269), (25, 270), (34, 271), (0, 272), (1, 273), (2, 274)]),
    SCell::Obj(&[(21, 275), (20, 276), (11, 277), (14, 278), (45, 279), (42, 280), (15, 281), (19, 282), (18, 283), (22, 284), (23, 285), (24, 286), (4, 287), (12, 288), (3, 289), (13, 290), (0, 291), (1, 292), (2, 293)]),
    SCell::Obj(&[(21, 294), (20, 295), (11, 296), (14, 297), (45, 298), (42, 299), (15, 300), (19, 301), (18, 302), (22, 303), (23, 304), (24, 305), (4, 306), (12, 307), (3, 308), (13, 309), (0, 310), (1, 311), (2, 312)]),
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
    SCell::Clo(26, &[207]),
    SCell::Clo(25, &[207]),
    SCell::Obj(&[(1, 358), (2, 359)]),
    SCell::Obj(&[(17, 360), (16, 361), (1, 362), (2, 363)]),
    SCell::Clo(24, &[207]),
    SCell::Clo(23, &[207]),
    SCell::Clo(27, &[207]),
    SCell::Clo(28, &[207]),
    SCell::Clo(29, &[207]),
    SCell::Obj(&[(1, 364), (2, 365)]),
    SCell::Obj(&[(1, 366), (2, 367)]),
    SCell::Clo(26, &[208]),
    SCell::Clo(25, &[208]),
    SCell::Obj(&[(1, 368), (2, 369)]),
    SCell::Obj(&[(17, 370), (16, 371), (1, 372), (2, 373)]),
    SCell::Clo(24, &[208]),
    SCell::Clo(23, &[208]),
    SCell::Clo(27, &[208]),
    SCell::Clo(28, &[208]),
    SCell::Clo(29, &[208]),
    SCell::Obj(&[(1, 374), (2, 375)]),
    SCell::Obj(&[(1, 376), (2, 377)]),
    SCell::Obj(&[(1, 378), (2, 379)]),
    SCell::Clo(26, &[209]),
    SCell::Clo(25, &[209]),
    SCell::Obj(&[(1, 380), (2, 381)]),
    SCell::Obj(&[(17, 382), (16, 383), (1, 384), (2, 385)]),
    SCell::Clo(24, &[209]),
    SCell::Clo(23, &[209]),
    SCell::Clo(27, &[209]),
    SCell::Clo(28, &[209]),
    SCell::Clo(29, &[209]),
    SCell::Obj(&[(1, 386), (2, 387)]),
    SCell::Obj(&[(1, 388), (2, 389)]),
    SCell::Clo(26, &[210]),
    SCell::Clo(25, &[210]),
    SCell::Obj(&[(1, 390), (2, 391)]),
    SCell::Obj(&[(17, 392), (16, 393), (1, 394), (2, 395)]),
    SCell::Clo(24, &[210]),
    SCell::Clo(23, &[210]),
    SCell::Clo(27, &[210]),
    SCell::Clo(28, &[210]),
    SCell::Clo(29, &[210]),
    SCell::Obj(&[(1, 396), (2, 397)]),
    SCell::Obj(&[(1, 398), (2, 399)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
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
    (236, 313),
    (237, 314),
    (239, 315),
    (240, 316),
    (241, 317),
    (242, 318),
    (243, 319),
    (244, 320),
    (245, 321),
    (247, 322),
    (249, 323),
    (252, 121),
    (255, 324),
    (256, 325),
    (259, 326),
    (260, 327),
    (261, 328),
    (262, 329),
    (263, 330),
    (264, 331),
    (265, 332),
    (266, 333),
    (268, 334),
    (271, 335),
    (272, 94),
    (275, 336),
    (276, 337),
    (278, 338),
    (281, 339),
    (282, 340),
    (283, 341),
    (284, 342),
    (285, 343),
    (286, 344),
    (287, 345),
    (289, 346),
    (291, 116),
    (294, 347),
    (295, 348),
    (297, 349),
    (300, 350),
    (301, 351),
    (302, 352),
    (303, 353),
    (304, 354),
    (305, 355),
    (306, 356),
    (308, 357),
    (310, 116),
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
    pub c20: ZN,
    pub c86: ZN,
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

/// Outcome 1's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared1 {
    pub c87: ZN,
    pub c39: ZN,
    pub c84: ZN,
    pub c20: ZN,
    pub c41: ZB,
    pub c86: ZN,
    pub c245: ZN,
    pub c253: ZN,
    pub c276: ZN,
    pub c289: ZN,
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
    b.cols[41] = Col::U(AV::Bool(false));
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
    b.cols[358] = Col::U(AV::Bool(false));
    b.cols[359] = Col::U(AV::Bool(false));
    b.cols[360] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[361] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[362] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[363] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[246] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[364] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[365] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[248] = Col::U(AV::Bool(true));
    b.cols[366] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[367] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[250] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[251] = Col::U(AV::Num(P8::from_raw(3145728i32)));
    b.cols[253] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[254] = Col::U(AV::Num(P8::from_raw(3120073i32)));
    b.cols[257] = Col::U(AV::Bool(true));
    b.cols[258] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[368] = Col::U(AV::Bool(false));
    b.cols[369] = Col::U(AV::Bool(false));
    b.cols[370] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[371] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[372] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[373] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[374] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[375] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[267] = Col::U(AV::Bool(false));
    b.cols[376] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[377] = Col::U(AV::Num(P8::from_raw(-262144i32)));
    b.cols[269] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[270] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[378] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[379] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[273] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[274] = Col::U(AV::Num(P8::from_raw(8126464i32)));
    b.cols[277] = Col::U(AV::Bool(true));
    b.cols[380] = Col::U(AV::Bool(false));
    b.cols[381] = Col::U(AV::Bool(false));
    b.cols[279] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[280] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[382] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[383] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[384] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[385] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[386] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[387] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[288] = Col::U(AV::Bool(true));
    b.cols[388] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[389] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[290] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[174] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[292] = Col::U(AV::Num(P8::from_raw(2621440i32)));
    b.cols[293] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[296] = Col::U(AV::Bool(true));
    b.cols[390] = Col::U(AV::Bool(false));
    b.cols[391] = Col::U(AV::Bool(false));
    b.cols[298] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[299] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[392] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[393] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[394] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[395] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[396] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[397] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[307] = Col::U(AV::Bool(true));
    b.cols[398] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[399] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[309] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[311] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[312] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[43] = Col::U(AV::Bool(false));
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
pub const KPART1_0: u64 = 1740538604100517787;
pub const KPART2_0: u64 = 11038548909323932322;

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
        if let Col::N(v) = &mut acc.cols[20] { v.push(sh.c20.lane(i)); }
        if let Col::N(v) = &mut acc.cols[86] { v.push(sh.c86.lane(i)); }
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
pub const KPART1_1: u64 = 15852480138752298020;
pub const KPART2_1: u64 = 14470855529731905178;

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
        if let Col::N(v) = &mut acc.cols[20] { v.push(sh.c20.lane(i)); }
        if let Col::V(v) = &mut acc.cols[41] {
            v.push(if sh.c41.known & (1 << i) != 0 {
                AV::Bool(sh.c41.val & (1 << i) != 0)
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

pub const OUTCOMES: usize = 2;

pub fn acc(i: usize, cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    match i {
        0 => acc0(cart, cache),
        1 => acc1(cart, cache),
        _ => panic!("outcome {} of 2", i),
    }
}

pub fn out_slots(i: usize) -> &'static [(u32, &'static str)] {
    match i {
        0 => OUT_SLOTS_0,
        1 => OUT_SLOTS_1,
        _ => panic!("outcome {} of 2", i),
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
    let r_c237: ZB = ZB { val: rin.c237, known: ALL };
    let r_c245: ZN = rin.c245;
    let r_c247: ZB = ZB { val: rin.c247, known: ALL };
    let r_c249: ZN = rin.c249;
    let r_c250: ZN = rin.c250;
    let r_c252: ZN = rin.c252;
    let r_c253: ZN = rin.c253;
    let r_c256: ZB = ZB { val: rin.c256, known: ALL };
    let r_c258: ZN = rin.c258;
    let r_c259: ZN = rin.c259;
    let r_c267: ZB = ZB { val: rin.c267, known: ALL };
    let r_c269: ZN = rin.c269;
    let r_c271: ZN = rin.c271;
    let r_c272: ZN = rin.c272;
    let r_c275: ZB = ZB { val: rin.c275, known: ALL };
    let r_c276: ZN = rin.c276;
    let r_c278: ZN = rin.c278;
    let r_c279: ZN = rin.c279;
    let r_c287: ZB = ZB { val: rin.c287, known: ALL };
    let r_c289: ZN = rin.c289;
    let r_c291: ZN = rin.c291;
    let r_c292: ZN = rin.c292;
    let r_c326: ZB = ZB { val: rin.c326, known: ALL };
    let r_c327: ZB = ZB { val: rin.c327, known: ALL };
    let r_c332: ZN = rin.c332;
    let r_c333: ZN = rin.c333;
    let r_c334: ZN = rin.c334;
    let r_c335: ZN = rin.c335;
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
    let n81: ZB = zb_not(r_c42);
    let n82: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), r_c88);
    let n83: ZB = zb_not(r_c326);
    let n84: ZB = zb_not(r_c327);
    let n85: bool = P8::from_raw(524288i32) == u.c328;
    let n86: bool = P8::from_raw(524288i32) == u.c329;
    let n87: bool = P8::from_raw(0i32) == u.c330;
    let n88: bool = P8::from_raw(0i32) == u.c331;
    let n89: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c332);
    let n90: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c333);
    let n91: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c334);
    let n92: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c335);
    let n93: ZB = zn_eq(zn_splat(P8::from_raw(1703936i32)), r_c249);
    let n94: ZB = zn_eq(zn_splat(P8::from_raw(3145728i32)), r_c250);
    let n95: ZB = zn_eq(zn_splat(P8::from_raw(524288i32)), r_c252);
    let n96: ZB = zb_not(r_c337);
    let n97: bool = P8::from_raw(524288i32) == u.c338;
    let n98: bool = P8::from_raw(0i32) == u.c340;
    let n99: bool = P8::from_raw(0i32) == u.c341;
    let n100: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c342);
    let n101: ZB = zb_not(r_c347);
    let n102: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c279);
    let n103: bool = P8::from_raw(524288i32) == u.c349;
    let n104: bool = P8::from_raw(0i32) == u.c351;
    let n105: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c352);
    let n106: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c353);
    let n107: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c355);
    let n110: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c84);
    let n111: ZN = zn_rem(n110, zn_splat(P8::from_raw(1966080i32)));
    let n112: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n111);
    let n113: ZB = zb_not(r_c336);
    let n114: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c258);
    let n115: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c259);
    let n116: bool = P8::from_raw(524288i32) == u.c339;
    let n117: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c343);
    let n118: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c344);
    let n119: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c345);
    let n120: ZB = zn_eq(zn_splat(P8::from_raw(1179648i32)), r_c269);
    let n121: ZB = zn_eq(zn_splat(P8::from_raw(2621440i32)), r_c271);
    let n122: ZB = zn_eq(zn_splat(P8::from_raw(7340032i32)), r_c272);
    let n123: ZB = zb_not(r_c346);
    let n124: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c278);
    let n125: bool = P8::from_raw(524288i32) == u.c348;
    let n126: bool = P8::from_raw(0i32) == u.c350;
    let n127: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c354);
    let n128: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c86);
    let n129: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n130: ZB = zn_le(r_c20, zn_splat(P8::from_raw(0i32)));
    let n165: ZB = zn_gt(r_c39, zn_splat(P8::from_raw(0i32)));
    let n166: ZN = zn_sub(r_c39, zn_splat(P8::from_raw(65536i32)));
    let n167: ZB = zn_le(n166, zn_splat(P8::from_raw(0i32)));
    let n169: ZB = zn_eq(zn_splat(P8::from_raw(6815744i32)), r_c291);
    let n170: ZB = zn_eq(zn_splat(P8::from_raw(7340032i32)), r_c292);
    let n171: ZB = zb_not(r_c43);
    let n172: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c85);
    let n173: ZN = zn_rem(n172, zn_splat(P8::from_raw(3932160i32)));
    let n174: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n173);
    let n175: ZN = zsel_n(n174, n128, r_c86);
    let n176: ZN = zsel_n(n112, n175, r_c86);
    let n177: ZN = zsel_n(n112, n173, r_c85);
    let n191: ZB = zb_and(n130, n165);
    let n202: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c245);
    let n203: ZN = zn_div(n202, zn_splat(P8::from_raw(2621440i32)));
    let n204: ZN = zn_sin(n203);
    let n206: ZN = zn_mul(n204, zn_splat(P8::from_raw(163840i32)));
    let n207: ZN = zn_add(zn_splat(P8::from_raw(3145728i32)), n206);
    let n208: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n209: ZN = zsel_n(n129, n208, r_c20);
    let n210: ZN = zsel_n(n129, r_c245, n202);
    let n211: ZN = zsel_n(n129, r_c253, n207);
    let n212: ZB = zn_le(r_c39, zn_splat(P8::from_raw(0i32)));
    let n213: ZB = zn_gt(n166, zn_splat(P8::from_raw(0i32)));
    let n214: ZN = zsel_n(n165, n166, r_c39);
    let n215: ZN = zsel_n(n129, r_c39, n214);
    let n216: ZB = zb_and(n130, n212);
    let n217: ZB = zb_and(n191, n213);
    let n218: ZB = zb_or(n216, n217);
    let n219: ZB = zn_gt(r_c276, zn_splat(P8::from_raw(0i32)));
    let n220: ZN = zn_sub(r_c276, zn_splat(P8::from_raw(65536i32)));
    let n221: ZB = zn_le(n220, zn_splat(P8::from_raw(0i32)));
    let n222: ZN = zsel_n(n219, n220, r_c276);
    let n223: ZB = zn_eq(zn_splat(P8::from_raw(1179648i32)), r_c289);
    let n224: ZN = zsel_n(n221, zn_splat(P8::from_raw(1179648i32)), r_c289);
    let n225: ZN = zsel_n(n219, n224, r_c289);
    let n226: ZN = zsel_n(n223, r_c276, n222);
    let n227: ZN = zsel_n(n223, r_c289, n225);
    let n228: ZN = zsel_n(n129, r_c276, n226);
    let n229: ZN = zsel_n(n129, r_c289, n227);
    let n230: ZB = zb_or(n129, n218);
    let n232: ZW = zw_cellmix_n(20u64, r_c20, 1542469173u64);
    let n233: ZW = zw_cellmix_n(20u64, r_c20, 668265263u64);
    let n234: ZW = zw_add(zw_splat(0u64), n232);
    let n235: ZW = zw_add(zw_splat(0u64), n233);
    let n236: ZW = zw_cellmix_n(39u64, n166, 1542469173u64);
    let n237: ZW = zw_cellmix_n(39u64, n166, 668265263u64);
    let n238: ZW = zw_add(n234, n236);
    let n239: ZW = zw_add(n235, n237);
    let n240: ZW = zw_cellmix_n(84u64, n111, 1542469173u64);
    let n241: ZW = zw_cellmix_n(84u64, n111, 668265263u64);
    let n242: ZW = zw_add(n238, n240);
    let n243: ZW = zw_add(n239, n241);
    let n244: ZW = zw_cellmix_n(85u64, n177, 1542469173u64);
    let n245: ZW = zw_cellmix_n(85u64, n177, 668265263u64);
    let n246: ZW = zw_add(n242, n244);
    let n247: ZW = zw_add(n243, n245);
    let n248: ZW = zw_cellmix_n(86u64, n176, 1542469173u64);
    let n249: ZW = zw_cellmix_n(86u64, n176, 668265263u64);
    let n250: ZW = zw_add(n246, n248);
    let n251: ZW = zw_add(n247, n249);
    let n252: ZW = zw_cellmix_n(87u64, r_c87, 1542469173u64);
    let n253: ZW = zw_cellmix_n(87u64, r_c87, 668265263u64);
    let n254: ZW = zw_add(n250, n252);
    let n255: ZW = zw_add(n251, n253);
    let n256: ZW = zw_cellmix_n(20u64, n209, 1542469173u64);
    let n257: ZW = zw_cellmix_n(20u64, n209, 668265263u64);
    let n258: ZW = zw_add(zw_splat(0u64), n256);
    let n259: ZW = zw_add(zw_splat(0u64), n257);
    let n260: ZW = zw_cellmix_n(39u64, n215, 1542469173u64);
    let n261: ZW = zw_cellmix_n(39u64, n215, 668265263u64);
    let n262: ZW = zw_add(n258, n260);
    let n263: ZW = zw_add(n259, n261);
    let n264: ZW = zw_cellmix_b(41u64, r_c41, 1542469173u64);
    let n265: ZW = zw_cellmix_b(41u64, r_c41, 668265263u64);
    let n266: ZW = zw_add(n262, n264);
    let n267: ZW = zw_add(n263, n265);
    let n268: ZW = zw_add(n266, n240);
    let n269: ZW = zw_add(n267, n241);
    let n270: ZW = zw_add(n268, n244);
    let n271: ZW = zw_add(n269, n245);
    let n272: ZW = zw_add(n270, n248);
    let n273: ZW = zw_add(n271, n249);
    let n274: ZW = zw_add(n272, n252);
    let n275: ZW = zw_add(n273, n253);
    let n276: ZW = zw_cellmix_n(245u64, n210, 1542469173u64);
    let n277: ZW = zw_cellmix_n(245u64, n210, 668265263u64);
    let n278: ZW = zw_add(n274, n276);
    let n279: ZW = zw_add(n275, n277);
    let n280: ZW = zw_cellmix_n(253u64, n211, 1542469173u64);
    let n281: ZW = zw_cellmix_n(253u64, n211, 668265263u64);
    let n282: ZW = zw_add(n278, n280);
    let n283: ZW = zw_add(n279, n281);
    let n284: ZW = zw_cellmix_n(276u64, n228, 1542469173u64);
    let n285: ZW = zw_cellmix_n(276u64, n228, 668265263u64);
    let n286: ZW = zw_add(n282, n284);
    let n287: ZW = zw_add(n283, n285);
    let n288: ZW = zw_cellmix_n(289u64, n229, 1542469173u64);
    let n289: ZW = zw_cellmix_n(289u64, n229, 668265263u64);
    let n290: ZW = zw_add(n286, n288);
    let n291: ZW = zw_add(n287, n289);
    let ok_v0_b0: u16 = ALL & zb_holds(r_c38) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n107) & zb_holds(n127) & zb_holds(r_c287) & zb_holds(n106) & zb_holds(n105) & zb_holds(n102) & zb_holds(n124) & zb_holds(n101) & zb_holds(n123) & zb_holds(r_c275) & zb_holds(n122) & zb_holds(n121) & zb_holds(n120) & zb_holds(n119) & zb_holds(n118) & zb_holds(r_c267) & zb_holds(n117) & zb_holds(n100) & zb_holds(n115) & zb_holds(n114) & zb_holds(n96) & zb_holds(n113) & zb_holds(r_c256) & zb_holds(n95) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n91) & zb_holds(r_c247) & zb_holds(n90) & zb_holds(n89) & zb_holds(n84) & zb_holds(n83) & zb_holds(r_c237) & zb_holds(n81) & zb_holds(n82);
    let bd_v0_b0: bool = !n104 || !n126 || !n103 || !n125 || !n99 || !n98 || !n116 || !n97 || !n88 || !n87 || !n86 || !n85;
    let live_v0_b0: u16 = ALL & zb_holds(n167) & zb_holds(n130) & zb_holds(n165);
    let ok_v0_b1: u16 = ALL & zb_holds(r_c38) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n107) & zb_holds(n127) & zb_holds(r_c287) & zb_holds(n106) & zb_holds(n105) & zb_holds(n102) & zb_holds(n124) & zb_holds(n101) & zb_holds(n123) & zb_holds(r_c275) & zb_holds(n122) & zb_holds(n121) & zb_holds(n120) & zb_holds(n119) & zb_holds(n118) & zb_holds(r_c267) & zb_holds(n117) & zb_holds(n100) & zb_holds(n115) & zb_holds(n114) & zb_holds(n96) & zb_holds(n113) & zb_holds(r_c256) & zb_holds(n95) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n91) & zb_holds(r_c247) & zb_holds(n90) & zb_holds(n89) & zb_holds(n84) & zb_holds(n83) & zb_holds(r_c237) & zb_holds(n81) & zb_holds(n82);
    let bd_v0_b1: bool = !n104 || !n126 || !n103 || !n125 || !n99 || !n98 || !n116 || !n97 || !n88 || !n87 || !n86 || !n85;
    let live_v0_b1: u16 = ALL & zb_holds(n230);
    let sh0 = KShared0 {
        c87: r_c87,
        c39: n166,
        c84: n111,
        c20: r_c20,
        c86: n176,
        c85: n177,
    };
    let sh1 = KShared1 {
        c87: r_c87,
        c39: n215,
        c84: n111,
        c20: n209,
        c41: r_c41,
        c86: n176,
        c245: n210,
        c253: n211,
        c276: n228,
        c289: n229,
        c85: n177,
    };
    let mut take_0_0: u16 = 0;
    let mut take_1_0: u16 = 0;
    // 2 distinct button assignments; per outcome they fall
    // into [1, 1] groups that write identical values.
    declined |= live_v0_b0 & (if bd_v0_b0 { ALL } else { !ok_v0_b0 });
    take_0_0 |= live_v0_b0 & ok_v0_b0 & (if bd_v0_b0 { 0 } else { ALL });
    let o0 = KOut0 {
        h1: n254, h2: n255,
    };
    // body 0: buttons 0x00, forks 0x0
    sink.o0(0, take_0_0, &sh0, &o0);
    declined |= live_v0_b1 & (if bd_v0_b1 { ALL } else { !ok_v0_b1 });
    take_1_0 |= live_v0_b1 & ok_v0_b1 & (if bd_v0_b1 { 0 } else { ALL });
    let o1 = KOut1 {
        h1: n290, h2: n291,
    };
    // body 1: buttons 0x00, forks 0x0
    sink.o1(0, take_1_0, &sh1, &o1);
    declined
}
