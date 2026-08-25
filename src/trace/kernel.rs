//! Render a whole compilable kernel from a TRACED frame.
//!
//! `transpile::kernel::render` does this for the WALK - the recipe
//! pipeline's emitter - and this is deliberately not that function
//! generalized. The walk's renderer carries machinery a traced frame
//! never produces: `Op::Split` fork configurations, `SplitValid`,
//! `fork_depth`, `valid_expr`, and a shape witness to bind against.
//! Those exist because `zi_fork_flr` splits one interval lane into two.
//! A tracer's outcomes come from real branches instead, so generalizing
//! the walk's renderer would mean inheriting assumptions and then
//! noticing them one at a time.
//!
//! Two differences from the walk's generated interface are the point
//! rather than an accident.
//!
//! INPUTS ARE NAMED BY PATH. The walk binds by canonical cell id
//! against a shape witness; this emits the path each input came from, so
//! the binder resolves against whatever block it is handed
//! (`trace::bind`). The cell ids in the struct field names are just
//! names - they agree with the traced input block's numbering, which
//! makes them readable, and nothing depends on that.
//!
//! AN OUTPUT BLOCK IS BUILT, NOT PATCHED. The walk's `acc_init` clones
//! the chunk's structure and overwrites the columns the frame wrote,
//! because in that pipeline the output shape IS the input shape. A
//! traced frame ends in several shapes - room (1,0) f40 has four, and
//! only one of them is the input's - so each outcome carries its own
//! structure and the kernel fills it. Measured there: about 105 pointer
//! cells that the structure carries, ~53 scalars the frame computes, and
//! nothing that passes through from the input block.

use std::collections::BTreeMap;
use std::fmt::Write;

use anyhow::Result;

use celeste_engine::runtime2::{Cell2, Col, AV};

use super::emit::{Bound, Lowered};
use super::verify::Frame;
use crate::transpile::kernel::{render_lines, OutField};

/// The Rust type of an input of this kind, at block-uniform width.
fn uni_ty(kind: &str) -> &'static str {
    match kind {
        "num" => "P8",
        "bool" => "bool",
        _ => "IV",
    }
}

/// The Rust type of an input of this kind, per lane.
fn row_ty(kind: &str) -> &'static str {
    match kind {
        "num" => "ZN",
        "ival" => "ZI",
        _ => "u16",
    }
}

/// Render the kernel for one traced frame.
pub fn render(f: &Frame, b: &Bound, l: &Lowered, title: &str) -> Result<String> {
    let mut o = String::new();

    // cell -> the path it was read from, for the input side.
    let mut in_path: BTreeMap<u32, String> = BTreeMap::new();
    for (i, cell) in f.in_cells.iter().enumerate() {
        in_path.insert(*cell, super::iface::show(&f.iface.slots[i]));
    }

    writeln!(
        o,
        "// GENERATED from a TRACED frame ({}). Do not edit.\n\
         //\n\
         // One input shape, {} output shapes, {} distinct button\n\
         // assignments. See `trace::kernel` for what this interface is\n\
         // and why it is not the walk's.\n\
         #![allow(unused_variables, unused_mut, unused_imports, clippy::all)]\n\
         use celeste_engine::kernel::*;\n\
         use celeste_core::pico8_num::{{Pico8Num as P8, Pico8NumInterval as IV}};\n\
         use celeste_core::cart_data::CartData;\n\
         use celeste_core::collision_cache::CollisionCache;\n\
         use celeste_engine::runtime2::{{Rt2, Col, AV}};\n\
         use celeste_engine::slots::{{build_block, resolve_path, SCell}};\n\
         use std::sync::Arc;\n\
         \n\
         pub struct G<'a> {{ pub cart: &'a CartData, pub cache: &'a CollisionCache }}\n",
        title,
        b.outcomes.len(),
        l.variants.len()
    )?;

    // The block this kernel is FOR, as the engine hashes it.
    //
    // Binding by path is not a shape test: shape 2's paths are a subset
    // of shape 0's - it is the same room with an empty object list - so
    // its `bind` succeeds against a shape-0 block and then computes a
    // frame that assumed no objects. The hash is the whole canonical
    // structure, so it distinguishes them, and it is what the dispatcher
    // keys on. `structure_of` builds this block in canonical order
    // already, which is what makes the hash comparable with one taken
    // after the boundary.
    writeln!(
        o,
        "/// The canonical shape this kernel was traced for.\n\
         pub const SHAPE: u64 = {};\n",
        f.in_rt2.shape_hash_of()
    )?;

    // ---- inputs ----
    let slot_list = |o: &mut String, name: &str, cells: &[(u32, &'static str)]| -> Result<()> {
        writeln!(o, "/// (path, kind) - resolved against a block at bind time.")?;
        writeln!(o, "pub const {}: &[(&str, &str)] = &[", name)?;
        for (cell, kind) in cells {
            writeln!(o, "    ({:?}, {:?}),", in_path[cell].as_str(), kind)?;
        }
        writeln!(o, "];\n")?;
        Ok(())
    };
    slot_list(&mut o, "UNI_SLOTS", &b.uni)?;
    writeln!(o, "/// Block-uniform inputs, in `UNI_SLOTS` order.")?;
    writeln!(o, "pub struct Uni {{")?;
    for (cell, kind) in &b.uni {
        writeln!(o, "    pub c{}: {},", cell, uni_ty(kind))?;
    }
    writeln!(o, "}}\n")?;

    slot_list(&mut o, "ROW_SLOTS", &b.inputs)?;
    writeln!(o, "/// Per-lane inputs, in `ROW_SLOTS` order.")?;
    writeln!(o, "pub struct RowsIn {{")?;
    for (cell, kind) in &b.inputs {
        writeln!(o, "    pub c{}: {},", cell, row_ty(kind))?;
    }
    writeln!(o, "}}\n")?;

    // Where each per-lane input LIVES in the block we were handed.
    // Resolved once per block, in `bind`, rather than once per 16-lane
    // slice: the path walk is cheap but it is not free, and the answer
    // cannot change between slices of one block.
    writeln!(o, "/// Cell ids for `ROW_SLOTS` in the bound block.")?;
    writeln!(o, "pub struct RowSlots {{")?;
    for (cell, _) in &b.inputs {
        writeln!(o, "    pub c{}: u32,", cell)?;
    }
    writeln!(o, "}}\n")?;

    writeln!(
        o,
        "/// Bind to a block by PATH. `None` means this block does not\n\
         /// have a slot the kernel needs, or holds it at the wrong kind -\n\
         /// either way the block takes the interpreter path.\n\
         pub fn bind(b: &Rt2) -> Option<(Uni, RowSlots)> {{\n\
         \x20   let cell = |p: &str| resolve_path(b, p).ok();\n\
         \x20   let u = Uni {{"
    )?;
    for (cell, kind) in &b.uni {
        let path = in_path[cell].as_str();
        let pat = match *kind {
            "num" => "Col::U(AV::Num(n)) => *n",
            "bool" => "Col::U(AV::Bool(v)) => *v",
            other => anyhow::bail!("uniform input kind {}", other),
        };
        writeln!(
            o,
            "        c{c}: match &b.cols[cell({p:?})? as usize] {{ {pat}, _ => return None }},",
            c = cell,
            p = path,
            pat = pat
        )?;
    }
    writeln!(o, "    }};")?;
    writeln!(o, "    let s = RowSlots {{")?;
    for (cell, _) in &b.inputs {
        writeln!(o, "        c{}: cell({:?})?,", cell, in_path[cell].as_str())?;
    }
    writeln!(o, "    }};")?;
    writeln!(o, "    Some((u, s))\n}}\n")?;

    // The SAME walk, reporting instead of refusing.
    //
    // `bind` returns `Option` because it is on the hot path and the
    // caller only has one thing to do with a failure. But under the
    // never-deopt doctrine that failure STOPS the run, and "did not
    // bind" is not a report anyone can act on - which slot, and holding
    // what, is. Kept in step by construction: both are generated from
    // the same two slot lists.
    writeln!(
        o,
        "/// Which slot stopped `bind`. `None` means it would have bound.\n\
         pub fn bind_why(b: &Rt2) -> Option<String> {{\n\
         \x20   for (p, kind) in UNI_SLOTS {{\n\
         \x20       let Ok(c) = resolve_path(b, p) else {{\n\
         \x20           return Some(format!(\"{{}}: no such slot\", p));\n\
         \x20       }};\n\
         \x20       let col = &b.cols[c as usize];\n\
         \x20       let ok = matches!(\n\
         \x20           (*kind, col),\n\
         \x20           (\"num\", Col::U(AV::Num(_))) | (\"bool\", Col::U(AV::Bool(_)))\n\
         \x20       );\n\
         \x20       if !ok {{\n\
         \x20           return Some(format!(\n\
         \x20               \"{{}}: block-uniform {{}} slot holds {{:?}}\",\n\
         \x20               p, kind, col\n\
         \x20           ));\n\
         \x20       }}\n\
         \x20   }}\n\
         \x20   for (p, kind) in ROW_SLOTS {{\n\
         \x20       let Ok(c) = resolve_path(b, p) else {{\n\
         \x20           return Some(format!(\"{{}}: no such slot\", p));\n\
         \x20       }};\n\
         \x20       let col = &b.cols[c as usize];\n\
         \x20       let lanes = (0..b.width).map(|i| col.at(i));\n\
         \x20       let ok = match *kind {{\n\
         \x20           \"num\" => lanes.clone().all(|v| matches!(v, AV::Num(_))),\n\
         \x20           _ => lanes.clone().all(|v| matches!(v, AV::Bool(_))),\n\
         \x20       }};\n\
         \x20       if !ok {{\n\
         \x20           let bad = lanes.enumerate().find(|(_, v)| match *kind {{\n\
         \x20               \"num\" => !matches!(v, AV::Num(_)),\n\
         \x20               _ => !matches!(v, AV::Bool(_)),\n\
         \x20           }});\n\
         \x20           return Some(format!(\n\
         \x20               \"{{}}: per-lane {{}} slot holds {{:?}} at lane {{:?}}\",\n\
         \x20               p, kind, bad.map(|(_, v)| v), bad.map(|(i, _)| i)\n\
         \x20           ));\n\
         \x20       }}\n\
         \x20   }}\n\
         \x20   // Every slot resolves and every lane is the right kind, so\n\
         \x20   // what stopped `rows` is REPRESENTATION: a column whose\n\
         \x20   // values are right but whose storage the gather does not\n\
         \x20   // accept (a `Col::V` of numbers where it wants `Col::N`).\n\
         \x20   // `collapse_uniform_cols` is the usual missing step.\n\
         \x20   None\n\
         }}\n"
    )?;

    writeln!(
        o,
        "/// Gather rows [lo, lo+16) into lane arrays. Short slices pad by\n\
         /// repeating the last row; padded lanes are ignored by width.\n\
         pub fn rows(b: &Rt2, s: &RowSlots, lo: usize) -> Option<RowsIn> {{\n\
         \x20   let at = |i: usize| -> usize {{ (lo + i).min(b.width - 1) }};\n\
         \x20   Some(RowsIn {{"
    )?;
    for (cell, kind) in &b.inputs {
        match *kind {
            "num" => writeln!(
                o,
                "        c{c}: match &b.cols[s.c{c} as usize] {{\n\
                 \x20           Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),\n\
                 \x20           Col::U(AV::Num(n)) => zn_splat(*n),\n\
                 \x20           _ => return None,\n\
                 \x20       }},",
                c = cell
            )?,
            "bool" => writeln!(
                o,
                "        c{c}: match &b.cols[s.c{c} as usize] {{\n\
                 \x20           Col::V(v) => {{\n\
                 \x20               let mut m = 0u16;\n\
                 \x20               for i in 0..W {{\n\
                 \x20                   match v[at(i)] {{\n\
                 \x20                       AV::Bool(true) => m |= 1 << i,\n\
                 \x20                       AV::Bool(false) => {{}}\n\
                 \x20                       _ => return None,\n\
                 \x20                   }}\n\
                 \x20               }}\n\
                 \x20               m\n\
                 \x20           }}\n\
                 \x20           Col::U(AV::Bool(t)) => if *t {{ 0xffff }} else {{ 0 }},\n\
                 \x20           _ => return None,\n\
                 \x20       }},",
                c = cell
            )?,
            // A per-lane INTERVAL. `Col::I` is the raw (lo, hi) pair
            // form; `Col::U(AV::Ival)` is what a block whose lanes all
            // carry the same widened `rem` collapses to, which is every
            // block straight out of the boundary - the widening writes
            // one value for the whole column. A plain number is also
            // accepted, as the degenerate interval it is: a lane can
            // reach a kernel with `rem` exact, before the first
            // boundary widened it.
            "ival" => writeln!(
                o,
                "        c{c}: match &b.cols[s.c{c} as usize] {{\n\
                 \x20           Col::I(v) => ZI {{\n\
                 \x20               lo: ZN::from_array(core::array::from_fn(|i| v[at(i)].0)),\n\
                 \x20               hi: ZN::from_array(core::array::from_fn(|i| v[at(i)].1)),\n\
                 \x20           }},\n\
                 \x20           Col::U(AV::Ival(lo, hi)) => ZI {{ lo: zn_splat(*lo), hi: zn_splat(*hi) }},\n\
                 \x20           Col::N(v) => ZI {{\n\
                 \x20               lo: ZN::from_array(core::array::from_fn(|i| v[at(i)])),\n\
                 \x20               hi: ZN::from_array(core::array::from_fn(|i| v[at(i)])),\n\
                 \x20           }},\n\
                 \x20           Col::U(AV::Num(n)) => ZI {{ lo: zn_splat(*n), hi: zn_splat(*n) }},\n\
                 \x20           _ => return None,\n\
                 \x20       }},",
                c = cell
            )?,
            other => anyhow::bail!("row input kind {}", other),
        }
    }
    writeln!(o, "    }})\n}}\n")?;

    // ---- one output shape per outcome ----
    for (i, out) in l.outs.iter().enumerate() {
        let paths: BTreeMap<u32, String> = f.outs[i]
            .fields
            .iter()
            .zip(f.outs[i].cells.iter())
            .map(|((p, _, _), c)| (*c, super::iface::show(p)))
            .collect();
        writeln!(o, "// ---------------- outcome {} ----------------", i)?;
        writeln!(o, "/// (cell, path) - where each computed value goes.")?;
        writeln!(o, "pub const OUT_SLOTS_{}: &[(u32, &str)] = &[", i)?;
        for OutField { cell, .. } in &out.fields {
            writeln!(o, "    ({}, {:?}),", cell, paths[cell].as_str())?;
        }
        writeln!(o, "];\n")?;
        writeln!(
            o,
            "/// Cells that end the frame as a fresh UnknownBool - next\n\
             /// frame's button inputs. The boundary writes UBool, no data."
        )?;
        writeln!(o, "pub const OUT_UBOOL_{}: &[(u32, &str)] = &[", i)?;
        for (c, p) in f.outs[i].ubool_cells.iter().zip(f.outs[i].ubool.iter()) {
            writeln!(o, "    ({}, {:?}),", c, super::iface::show(p).as_str())?;
        }
        writeln!(o, "];\n")?;

        // THE OUTPUT SHAPE, as generated data. Everything the shape
        // determines - structure, globals, pointer columns - is a
        // compile-time constant of this outcome, so the kernel builds
        // its block rather than patching a copy of the input's.
        let rt2 = &f.outs[i].rt2;
        writeln!(o, "pub const OUT_SHAPE_{}: &[SCell] = &[", i)?;
        for cell in &rt2.structure {
            match cell {
                Cell2::Val => writeln!(o, "    SCell::Val,")?,
                Cell2::Obj(fields) => {
                    let items: Vec<String> =
                        fields.iter().map(|(k, c)| format!("({}, {})", k, c)).collect();
                    writeln!(o, "    SCell::Obj(&[{}]),", items.join(", "))?
                }
                Cell2::Arr(items) => {
                    let items: Vec<String> = items.iter().map(|c| c.to_string()).collect();
                    writeln!(o, "    SCell::Arr(&[{}]),", items.join(", "))?
                }
                Cell2::Clo(fid, caps) => {
                    // The captures are pointers by construction (a
                    // capture holds the object that owns the closure),
                    // and a shape can only carry what is fixed - so a
                    // capture that is not a pointer has no static form
                    // and is refused rather than dropped.
                    let mut ts = Vec::new();
                    for cap in caps.iter() {
                        match cap {
                            Col::U(AV::Ptr(t)) => ts.push(t.to_string()),
                            other => anyhow::bail!(
                                "outcome {} has a closure capture {:?}, which is not a pointer",
                                i,
                                other
                            ),
                        }
                    }
                    writeln!(o, "    SCell::Clo({}, &[{}]),", fid, ts.join(", "))?
                }
                Cell2::Bi(b) => writeln!(o, "    SCell::Bi({}),", b)?,
                Cell2::Unk => writeln!(o, "    SCell::Unk,")?,
            }
        }
        writeln!(o, "];\n")?;
        writeln!(o, "pub const OUT_GLOBALS_{}: &[u32] = &{:?};\n", i, rt2.globals)?;
        writeln!(o, "/// (cell, target) - the pointer topology, fixed by the shape.")?;
        writeln!(o, "pub const OUT_PTRS_{}: &[(u32, u32)] = &[", i)?;
        for (c, col) in rt2.cols.iter().enumerate() {
            if let Col::U(AV::Ptr(t)) = col {
                writeln!(o, "    ({}, {}),", c, t)?;
            }
        }
        writeln!(o, "];\n")?;

        writeln!(o, "/// Outcome {}'s button-INDEPENDENT values.", i)?;
        writeln!(o, "/// Values shared by every button assignment.\n\
                 /// COMPILE-TIME CONSTANTS are absent: their column is\n\
                 /// written once when the block is built, so there is\n\
                 /// nothing to carry per lane.")?;
        writeln!(o, "pub struct KShared{} {{", i)?;
        for OutField { cell, ty, tainted, konst, .. } in &out.fields {
            if !*tainted && konst.is_none() {
                writeln!(o, "    pub c{}: {},", cell, ty)?;
            }
        }
        writeln!(o, "}}\n")?;
        writeln!(o, "/// Outcome {}'s per-assignment values and lane masks.", i)?;
        writeln!(
            o,
            "/// The cells of outcome {i} that DIFFER between button\n\
             /// assignments. Everything else is either constant (written\n\
             /// once when the block is built) or shared (`KShared{i}`).\n\
             ///\n\
             /// No `live`/`deopt` here: which lanes a group writes is its\n\
             /// `take` argument, and declined lanes are accumulated by\n\
             /// `frame` itself.",
            i = i
        )?;
        writeln!(o, "pub struct KOut{} {{", i)?;
        for OutField { cell, ty, tainted, .. } in &out.fields {
            if *tainted {
                writeln!(o, "    pub c{}: {},", cell, ty)?;
            }
        }
        writeln!(
            o,
            "    /// This successor's ROW KEY, both halves, 16 lanes at\n\
             \x20   /// once. Folded by the graph rather than by `append`:\n\
             \x20   /// the fold is sequential over cells but every step is\n\
             \x20   /// a vector, and its button-independent prefix is one\n\
             \x20   /// shared chain across all the assignments instead of\n\
             \x20   /// being recomputed per candidate row.\n\
             \x20   pub h1: ZW,\n\
             \x20   pub h2: ZW,"
        )?;
        writeln!(o, "}}\n")?;
    }

    // ---- building an outcome's block, and filling it ----
    for (i, out) in l.outs.iter().enumerate() {
        writeln!(
            o,
            "/// An EMPTY accumulator with outcome {i}'s shape: structure,\n\
             /// globals and pointers from the constants above, computed\n\
             /// columns starting empty and growing by `append{i}`.\n\
             pub fn acc{i}(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {{\n\
             \x20   let mut b = build_block(OUT_SHAPE_{i}, OUT_GLOBALS_{i}, OUT_PTRS_{i}, 0, cart, cache);",
            i = i
        )?;
        for OutField { cell, ty, konst, .. } in &out.fields {
            // A constant column holds one value for the whole
            // accumulator, so it is written HERE, once, instead of being
            // pushed per row. 44 of outcome 0's 52 fields are like this.
            if let Some(av) = konst {
                writeln!(o, "    b.cols[{}] = Col::U({});", cell, av)?;
                continue;
            }
            let empty = match *ty {
                "ZN" => "Col::N(Vec::new())",
                "ZB" => "Col::V(Vec::new())",
                // A per-lane interval, in the raw (lo, hi) form.
                // `player.rem` leaves a frame as the fork fragment it
                // was narrowed to, which is an interval; the next
                // boundary widens it back.
                "ZI" => "Col::I(Vec::new())",
                other => anyhow::bail!("output type {}", other),
            };
            writeln!(o, "    b.cols[{}] = {};", cell, empty)?;
        }
        writeln!(
            o,
            "    for (cell, _) in OUT_UBOOL_{} {{\n\
             \x20       b.cols[*cell as usize] = Col::U(AV::UBool);\n\
             \x20   }}\n\
             \x20   b\n\
             }}\n",
            i
        )?;

        writeln!(
            o,
            "/// Append this assignment's lanes that TAKE outcome {i} and\n\
             /// that the kernel is willing to keep. A lane in `deopt` is\n\
             /// dropped here and belongs to the interpreter - the caller\n\
             /// has `kv.deopt` and must account for it.\n\
             /// SKIPS a row whose values another configuration already\n\
             /// wrote. The key is over the non-constant cells only - the\n\
             /// rest are one value for the whole accumulator and cannot\n\
             /// tell two rows apart - so it is a handful of mixes rather\n\
             /// than a hundred, computed from values already in\n\
             /// registers. A duplicate caught here costs nothing; one\n\
             /// caught at the boundary has already been written.\n\
             ///\n\
             /// 128-bit like the boundary's own key, because a collision\n\
             /// DROPS a successor rather than merely costing time.\n\
             ///\n\
             /// `org` is the engine-carried origin metadata of this\n\
             /// slice's input lanes (empty = untracked): each written\n\
             /// row records its input lane's origin, and the origin is\n\
             /// mixed into the dedup key so two rows from different\n\
             /// origins never collapse (`Rt2::origin`).\n\
             pub fn append{i}(\n\
             \x20   acc: &mut Rt2, sh: &KShared{i}, kv: &KOut{i}, take: u16,\n\
             \x20   n: usize, seen: &mut RowSet, org: &[u32],\n\
             ) -> u16 {{\n\
             \x20   // Returns the lanes actually WRITTEN, which is `take`\n\
             \x20   // minus the ones another configuration already wrote.\n\
             \x20   // A caller that needs to know which (assignment, lane)\n\
             \x20   // produced row k cannot infer it from `take`.\n\
             \x20   let mut wrote: u16 = 0;\n\
             \x20   let take = take & ((1u32 << n) - 1) as u16;\n\
             \x20   // The key columns come out of their registers ONCE.\n\
             \x20   // `ZW::lane` is a store plus a load, so calling it\n\
             \x20   // inside the loop would do that sixteen times for a\n\
             \x20   // value that does not change.\n\
             \x20   let (h1, h2) = (kv.h1.to_array(), kv.h2.to_array());\n\
             \x20   for i in 0..n {{\n\
             \x20       if take & (1 << i) == 0 {{ continue; }}",
            i = i
        )?;
        // The key is FOLDED BY THE GRAPH now (`Op::Bits` / `Op::Mix`),
        // 16 lanes at a time and with its button-independent prefix
        // shared across every assignment. All that is left here is the
        // table probe, which is inherently scalar: a hash table cannot
        // be vectorized, and at ~2M probes it does not need to be.
        writeln!(
            o,
            "        let key = if org.is_empty() {{ (h1[i], h2[i]) }} else {{\n\
             \x20           // mix64 is a bijection: same row, different\n\
             \x20           // origins can never collide.\n\
             \x20           (mix64(h1[i] ^ mix64(0x517c_c1b7_2722_0a95 ^ org[i] as u64)), h2[i])\n\
             \x20       }};\n\
             \x20       if !seen.insert(key) {{ continue; }}"
        )?;
        for OutField { cell, ty, tainted, konst, .. } in &out.fields {
            if konst.is_some() {
                continue; // written once by `acc`, not per row
            }
            let src = if *tainted { "kv" } else { "sh" };
            match *ty {
                "ZN" => writeln!(
                    o,
                    "        if let Col::N(v) = &mut acc.cols[{c}] {{ v.push({s}.c{c}.lane(i)); }}",
                    c = cell,
                    s = src
                )?,
                // An UNKNOWN boolean is a real abstract value, not a
                // broken one, so it goes in as `UBool` rather than being
                // coerced to a definite bit.
                "ZB" => writeln!(
                    o,
                    "        if let Col::V(v) = &mut acc.cols[{c}] {{\n\
                     \x20           v.push(if {s}.c{c}.known & (1 << i) != 0 {{\n\
                     \x20               AV::Bool({s}.c{c}.val & (1 << i) != 0)\n\
                     \x20           }} else {{ AV::UBool }});\n\
                     \x20       }}",
                    c = cell,
                    s = src
                )?,
                "ZI" => writeln!(
                    o,
                    "        if let Col::I(v) = &mut acc.cols[{c}] {{\n\
                     \x20           v.push(({s}.c{c}.lo.lane(i), {s}.c{c}.hi.lane(i)));\n\
                     \x20       }}",
                    c = cell,
                    s = src
                )?,
                other => anyhow::bail!("output type {}", other),
            }
        }
        writeln!(
            o,
            "        if !org.is_empty() {{ acc.origin.push(org[i]); }}\n\
             \x20       wrote |= 1 << i;\n\
             \x20       acc.width += 1;\n\
             \x20   }}\n\
             \x20   wrote\n\
             }}\n"
        )?;
    }

    // A small dynamic layer over the per-outcome items above. Rust
    // cannot index a struct or a function name by a runtime `usize`, and
    // a caller that loops over outcomes needs to. Generated rather than
    // written by hand so it cannot fall out of step with the count.
    writeln!(o, "pub const OUTCOMES: usize = {};\n", l.outs.len())?;
    let dispatch = |o: &mut String, sig: &str, arm: &str| -> Result<()> {
        writeln!(o, "{} {{", sig)?;
        writeln!(o, "    match i {{")?;
        for k in 0..l.outs.len() {
            writeln!(o, "        {} => {},", k, arm.replace('#', &k.to_string()))?;
        }
        writeln!(o, "        _ => panic!(\"outcome {{}} of {}\", i),", l.outs.len())?;
        writeln!(o, "    }}\n}}\n")?;
        Ok(())
    };
    dispatch(
        &mut o,
        "pub fn acc(i: usize, cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2",
        "acc#(cart, cache)",
    )?;
    dispatch(&mut o, "pub fn out_slots(i: usize) -> &'static [(u32, &'static str)]", "OUT_SLOTS_#")?;

    // ONE uniform entry point per kernel, so a dispatcher can hold
    // kernels for different shapes as plain function pointers. Every
    // shape's `Uni`, `RowsIn` and `KShared` are different types, so
    // anything that exposed them could not be uniform.
    //
    // `None` means this kernel is not for this block - a path that does
    // not resolve, or a slot at the wrong kind. `Some(mask)` is the
    // lanes DECLINED; under the never-deopt doctrine a non-zero mask
    // stops the run rather than falling back.
    writeln!(
        o,
        "struct Append<'a> {{ accs: &'a mut [Rt2], seen: &'a mut [RowSet], n: usize, org: &'a [u32] }}\n"
    )?;
    writeln!(o, "impl<'a> Sink for Append<'a> {{")?;
    for i in 0..l.outs.len() {
        writeln!(
            o,
            "    fn o{i}(&mut self, _mask: u8, take: u16, sh: &KShared{i}, v: &KOut{i}) {{\n\
             \x20       append{i}(&mut self.accs[{i}], sh, v, take, self.n, &mut self.seen[{i}], self.org);\n\
             \x20   }}",
            i = i
        )?;
    }
    writeln!(o, "}}\n")?;
    writeln!(
        o,
        "pub fn step(\n\
         \x20   b: &Rt2, lo: usize, n: usize, accs: &mut [Rt2], seen: &mut [RowSet],\n\
         ) -> Option<u16> {{\n\
         \x20   let (u, s) = bind(b)?;\n\
         \x20   let rin = rows(b, &s, lo)?;\n\
         \x20   let g = G {{ cart: &b.cart, cache: &b.cache }};\n\
         \x20   // The row sets are the CALLER's and reset in O(1) here.\n\
         \x20   // Slice-local because that is where the duplication is:\n\
         \x20   // it comes from configurations agreeing on one lane, and\n\
         \x20   // a lane lives in one slice.\n\
         \x20   seen.iter_mut().for_each(|s| s.next_slice());\n\
         \x20   // Engine-carried origin metadata for this slice's lanes\n\
         \x20   // (empty = untracked); see `Rt2::origin`.\n\
         \x20   let org: &[u32] = if b.origin.is_empty() {{ &[] }} else {{ &b.origin[lo..lo + n] }};\n\
         \x20   let mut sink = Append {{ accs, seen, n, org }};\n\
         \x20   Some(frame(&u, &rin, &g, &mut sink))\n\
         }}\n"
    )?;

    // ---- the body ----
    //
    // GROUP-DRIVEN. Variants that write the same values to an outcome
    // append ONCE, with the union of their lanes, instead of appending
    // identical rows that a dedup then throws away. Outcome 0 has no
    // per-variant cells at all, so its 24 button assignments are one
    // group and produce one row per lane rather than 24.
    //
    // The union is accumulated INCREMENTALLY (`take |= ...` after each
    // variant) and the append is emitted at the group's LAST member, so
    // nothing has to stay live: a variant's masks die immediately, and
    // the group's values are computed once, at the end, from
    // expressions every member shares by definition. Building the union
    // as one expression instead kept 48 masks alive at once and took
    // the build from 70 s to 20+ minutes.
    // A block-undecidable condition zeroes `live`, which would drop
    // every lane of that outcome SILENTLY - the missing-successor
    // failure the whole boolean split exists to prevent. Nothing
    // consumes `bd` today because it is always the literal `false` here;
    // this refuses to emit a kernel where that stops being true, rather
    // than letting it become a quiet hole.
    // A non-false `bd` (block-level obligation) is now WIRED to `declined`
    // above, so a firing block is reported as a coverage gap rather than
    // silently dropped. The abstract kernels have `bd == false` and are
    // unchanged; the lattice kernels carry real block guards.

    let mut groups: Vec<Vec<Option<usize>>> = Vec::new(); // per outcome: variant -> group
    let mut last: Vec<Vec<bool>> = Vec::new();    // per outcome: is last of its group
    let mut n_groups: Vec<usize> = Vec::new();
    // The group signature is the TAINTED cells only, and that is also
    // what makes it sound to emit ONE member's row key for the whole
    // group. A row is (const cells, shared cells, tainted cells); the
    // first two are identical across every assignment by construction,
    // so two assignments with textually equal tainted cells write equal
    // rows - and the key is a pure function of exactly those, folded by
    // the graph. Equal rows, equal key. The last member's key
    // expression names different nodes than the first member's might,
    // but they evaluate to the same word.
    for oi in 0..l.outs.len() {
        let key = |v: &crate::transpile::lower::Variant| -> Option<String> {
            let p = v.per[oi].as_ref()?;
            Some(
                l.outs[oi]
                    .fields
                    .iter()
                    .filter(|f| f.tainted)
                    .map(|f| p.outputs[&f.cell].clone())
                    .collect::<Vec<_>>()
                    .join(";"),
            )
        };
        // `None` where a variant does not write this outcome, so the
        // vector stays parallel to `l.variants` and the indices below
        // still line up.
        let mut seen: BTreeMap<String, usize> = BTreeMap::new();
        let mut g: Vec<Option<usize>> = Vec::new();
        for v in &l.variants {
            g.push(key(v).map(|k| {
                let n = seen.len();
                *seen.entry(k).or_insert(n)
            }));
        }
        let ng = seen.len();
        let mut is_last = vec![false; g.len()];
        for gi in 0..ng {
            if let Some(pos) = g.iter().rposition(|x| *x == Some(gi)) {
                is_last[pos] = true;
            }
        }
        groups.push(g);
        last.push(is_last);
        n_groups.push(ng);
    }

    writeln!(
        o,
        "/// Where a frame's rows go. One call per (outcome, GROUP), not\n\
         /// per outcome per variant: variants that write identical\n\
         /// values are one call whose `take` is the union of their\n\
         /// lanes. `mask` is the group's REPRESENTATIVE assignment -\n\
         /// every member computes the same values, so any of them\n\
         /// identifies the row for a caller that wants to check it\n\
         /// against the graph.\n\
         pub trait Sink {{"
    )?;
    for i in 0..l.outs.len() {
        writeln!(o, "    fn o{i}(&mut self, mask: u8, take: u16, sh: &KShared{i}, v: &KOut{i});", i = i)?;
    }
    writeln!(o, "}}\n")?;

    writeln!(
        o,
        "/// Run one frame over 16 lanes. Returns the lanes DECLINED -\n\
         /// live but not `ok` - which the never-deopt doctrine turns\n\
         /// into a stopped run.\n\
         #[inline(never)]\n\
         pub fn frame(u: &Uni, rin: &RowsIn, g: &G, sink: &mut dyn Sink) -> u16 {{\n\
         \x20   let mut declined: u16 = 0;"
    )?;
    o.push_str(&render_lines(&l.body));
    for (i, out) in l.outs.iter().enumerate() {
        writeln!(o, "    let sh{} = KShared{} {{", i, i)?;
        for OutField { cell, expr, tainted, konst, .. } in &out.fields {
            if !*tainted && konst.is_none() {
                writeln!(o, "        c{}: {},", cell, expr)?;
            }
        }
        writeln!(o, "    }};")?;
    }
    for (i, ng) in n_groups.iter().enumerate() {
        for gi in 0..*ng {
            writeln!(o, "    let mut take_{}_{}: u16 = 0;", i, gi)?;
        }
    }
    writeln!(
        o,
        "    // {} distinct button assignments; per outcome they fall\n\
         \x20   // into {:?} groups that write identical values.",
        l.variants.len(),
        n_groups
    )?;
    for (vi, v) in l.variants.iter().enumerate() {
        for (i, out) in l.outs.iter().enumerate() {
            let Some(p) = v.per[i].as_ref() else {
                continue;
            };
            let gi = groups[i][vi].expect("a variant that writes an outcome has a group");
            if let Some(bd) = &p.bd {
                // Block-level deopt: if `bd` holds, this block is out of the
                // kernel's domain, so every lane it would take is DECLINED
                // (reported by the caller), not silently dropped, and none
                // is taken. plans/specialize.md.
                writeln!(o, "    declined |= {} & (if {} {{ ALL }} else {{ !{} }});", p.live, bd, p.ok)?;
                writeln!(o, "    take_{}_{} |= {} & {} & (if {} {{ 0 }} else {{ ALL }});", i, gi, p.live, p.ok, bd)?;
            } else {
                writeln!(o, "    declined |= {} & !{};", p.live, p.ok)?;
                writeln!(o, "    take_{}_{} |= {} & {};", i, gi, p.live, p.ok)?;
            }
            if !last[i][vi] {
                continue;
            }
            writeln!(o, "    let o{} = KOut{} {{", i, i)?;
            for OutField { cell, tainted, .. } in &out.fields {
                if *tainted {
                    writeln!(o, "        c{}: {},", cell, p.outputs[cell])?;
                }
            }
            let (h1, h2) = p.key.as_ref().ok_or_else(|| {
                anyhow::anyhow!("the traced emitter needs `Emit::row_key`, which was off")
            })?;
            writeln!(o, "        h1: {}, h2: {},", h1, h2)?;
            writeln!(o, "    }};")?;
            writeln!(
                o,
                "    // body {b}: buttons {m:#04x}, forks {fk:#x}\n\
                 \x20   sink.o{i}({m}, take_{i}_{gi}, &sh{i}, &o{i});",
                b = vi,
                fk = v.fork,
                i = i,
                gi = gi,
                m = v.mask
            )?;
        }
    }
    // Nothing to close. The traced emitter resolves every fork at
    // compile time (`Emit::flat_forks`), so a fork configuration is a
    // BODY here rather than a trip through a nest, and the function is
    // straight-line from `let mut declined` to the last `sink.o*`.
    //
    // It used to close `b.forks` loop braces the body had opened.
    debug_assert!(
        !l.body.iter().any(|line| matches!(
            line, crate::transpile::kernel::Line::Raw(r) if r.starts_with("for c")
        )),
        "the traced body opened a fork loop, but nothing closes one any more"
    );
    writeln!(o, "    declined")?;
    writeln!(o, "}}")?;
    Ok(o)
}

/// TYPE-CHECK a rendered kernel, by handing it to rustc.
///
/// `render` returning 6,000 lines only says the emitter emitted
/// something. Whether those lines are Rust - whether they use an engine
/// API that still exists, with the types it still has - is a separate
/// question, and rustc is the only thing that answers it.
///
/// That gap was not theoretical. On 2026-08-23 the lane type became a
/// register, the walk's emitter was updated, `generated_is_current`
/// passed, the whole 279-test gate went green - and the TRACED emitter
/// was still building intervals out of arrays and indexing the row-key
/// columns per lane. Nothing noticed until `check-traced-kernel.sh` ran
/// by hand, because that script is the only thing that compiled it and
/// it is not in the gate.
///
/// `--emit=metadata` so this is type-checking and not codegen: the
/// point is the API surface, and codegen of a 6,000-line body is the
/// expensive part we cannot afford on every commit.
///
/// Links against the rlibs the TEST BINARY was built with, found next to
/// it in `deps/`. Not finding them is a hard failure, not a skip - a
/// check that silently does nothing is worse than no check, because it
/// reads as green.
pub fn typecheck_rendered(src: &str) -> Result<()> {
    use anyhow::Context;
    let exe = std::env::current_exe().context("locating the test binary")?;
    let deps = exe.parent().ok_or_else(|| anyhow::anyhow!("test binary has no parent dir"))?;

    // The newest matching rlib: a `target/` accumulates stale ones, and
    // linking an old `celeste-engine` would check against an API that is
    // no longer there - which is the exact thing this exists to catch.
    let newest = |stem: &str| -> Result<std::path::PathBuf> {
        let mut best: Option<(std::time::SystemTime, std::path::PathBuf)> = None;
        let entries =
            std::fs::read_dir(deps).with_context(|| format!("reading {}", deps.display()))?;
        for e in entries {
            let p = e?.path();
            let name = p.file_name().and_then(|n| n.to_str()).unwrap_or("");
            if name.starts_with(&format!("lib{}-", stem)) && name.ends_with(".rlib") {
                let t = p.metadata()?.modified()?;
                if best.as_ref().is_none_or(|(bt, _)| t > *bt) {
                    best = Some((t, p));
                }
            }
        }
        best.map(|(_, p)| p).ok_or_else(|| {
            anyhow::anyhow!(
                "no lib{}-*.rlib in {} - the type-check cannot run, and skipping it \
                 would read as a pass",
                stem,
                deps.display()
            )
        })
    };

    let dir = std::env::temp_dir().join(format!("celeste-typecheck-{}", std::process::id()));
    std::fs::create_dir_all(&dir)?;
    let file = dir.join("kernel.rs");
    std::fs::write(&file, src)?;

    let mut cmd = std::process::Command::new("rustc");
    cmd.arg("--edition=2021")
        .arg("--crate-type=lib")
        .arg("--crate-name=traced_kernel")
        .arg("--emit=metadata")
        .arg("-o")
        .arg(dir.join("kernel.rmeta"))
        .arg("-L")
        .arg(format!("dependency={}", deps.display()));
    for stem in ["celeste_engine", "celeste_core", "celeste_names"] {
        cmd.arg("--extern").arg(format!("{}={}", stem, newest(stem)?.display()));
    }
    cmd.arg(&file);

    let out = cmd.output().context("running rustc on the rendered kernel")?;
    let _ = std::fs::remove_dir_all(&dir);
    if !out.status.success() {
        let err = String::from_utf8_lossy(&out.stderr);
        // The first few diagnostics; the whole thing is thousands of
        // lines when a template is wrong in every cell.
        let head: Vec<&str> = err.lines().take(40).collect();
        anyhow::bail!(
            "the rendered kernel does not type-check ({} lines of it):\n{}",
            src.lines().count(),
            head.join("\n")
        );
    }
    Ok(())
}

/// One traced frame, everything owned, ready to render or to run.
///
/// The tracer's `Interp` borrows the parsed ASTs, so a caller outside
/// this crate cannot easily set one up. Nothing in here borrows them -
/// `Frame`, `Graph` and `Rt2` are all owned - so the whole thing can be
/// handed across a crate boundary, which is what the out-of-tree check
/// harness needs.
pub struct Reference {
    pub frame: Frame,
    pub graph: crate::transpile::graph::Graph,
    pub bound: Bound,
    pub lowered: Lowered,
    pub cart: std::sync::Arc<celeste_core::cart_data::CartData>,
    pub cache: std::sync::Arc<celeste_core::collision_cache::CollisionCache>,
}

/// Trace the frame this kernel is rendered from: room (1,0), warmed up
/// to the first frame that has a player, pinned to its pm1 key.
pub fn reference_frame() -> Result<Reference> {
    reference_frame_in(std::path::Path::new("."))
}

/// As `reference_frame`, with the repo root given explicitly - the run
/// check is a crate outside the workspace, so its working directory is
/// not the repo root.
pub fn reference_frame_in(root: &std::path::Path) -> Result<Reference> {
    use super::domain::Symbolic;
    use super::interp::Interp;
    use super::verify::{pm1_key, run_one, trace_frame};
    use super::{cart, iface};
    use anyhow::{anyhow, bail};

    let src = cart::sources_in(root)?;
    let top = full_moon::parse(&src).map_err(|e| anyhow!("parse: {:?}", e))?;
    let init = full_moon::parse("_init()").map_err(|e| anyhow!("parse _init: {:?}", e))?;
    let reset = full_moon::parse("__reset_button_states()")
        .map_err(|e| anyhow!("parse reset: {:?}", e))?;
    let fr = full_moon::parse(cart::FRAME_CODE).map_err(|e| anyhow!("parse frame: {:?}", e))?;

    let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
    let cart_data = std::sync::Arc::new(celeste_core::cart_data::CartData::load(root.join("cart"))?);
    let (rx, ry) = celeste_interp::game_runner::start_room();
    let cache = std::sync::Arc::new(celeste_core::collision_cache::CollisionCache::new(
        &cart_data, rx, ry,
    )?);
    it.cache = Some(cache.clone());
    it.cart = Some(cart_data.clone());

    let st = cart::fresh_state::<Symbolic>(&mut it.d);
    let mut st = run_one(&mut it, &top, st)?;
    cart::inject_tile_flag_at(&mut st);
    let mut st = run_one(&mut it, &init, st)?;
    let mut player: Option<super::iface::Path> = None;
    for _ in 0..40 {
        if let Some(p) = super::verify::find_player(&st) {
            player = Some(p);
            break;
        }
        st = run_one(&mut it, &fr, st)?;
    }
    let Some(player) = player else { bail!("no player after 40 frames") };

    let mut roots = vec![player.clone()];
    for g in ["freeze", "has_dashed", "frames", "will_restart", "delay_restart", "max_djump"] {
        roots.push(vec![iface::key(g)]);
    }
    let pin = pm1_key(&player, &st, &it.d)?;
    let frame = trace_frame(&mut it, &reset, &fr, st, &roots, &pin, &[], false)?;

    let graph = std::mem::take(&mut it.d.graph);
    let bound = super::emit::bind(&frame, &graph)?;
    let room = crate::transpile::graph::Room { cart: cart_data.clone(), cache: cache.clone() };
    let lowered = super::emit::lower_frame(
        &bound.graph,
        &bound.inputs,
        &bound.uni,
        &bound.outcomes,
        Some(room),
        bound.forks,
    )?;
    Ok(Reference { frame, graph, bound, lowered, cart: cart_data, cache })
}

/// One kernel per SHAPE the room reaches.
///
/// No pm1 pin: a kernel covers every key of its shape. The pin is worth
/// -6.3% (T13) and costs a kernel per key, and a fully symbolic frame is
/// 2,341 graph nodes against ~1,720 for the largest single outcome of a
/// pinned one - so un-pinning is close to free.
///
/// REFUSES rather than returns a partial set. A shape the walk could not
/// trace, or one it dropped at the cap, is a kernel that will not exist,
/// and under the never-deopt doctrine that is a run that stops. Better
/// to fail here, where the reason is in hand.
/// Every shape's traced frame, BOUND to the engine's numbering, plus the
/// arena they share. The half of `room_kernels_in` that does not lower -
/// separate so a diagnostic can look at the traced graph itself, which
/// is where the question "did the tracer build this node or did a fold"
/// gets answered.
pub(crate) fn room_shapes_in(
    root: &std::path::Path,
    opts: super::shapes::WalkOpts,
) -> Result<(
    Vec<super::shapes::Shape>,
    crate::transpile::graph::Graph,
    std::sync::Arc<celeste_core::cart_data::CartData>,
    std::sync::Arc<celeste_core::collision_cache::CollisionCache>,
)> {
    use super::domain::Symbolic;
    use super::interp::Interp;
    use super::verify::run_one;
    use super::{cart, shapes};
    use anyhow::{anyhow, bail};

    let src = cart::sources_in(root)?;
    let top = full_moon::parse(&src).map_err(|e| anyhow!("parse: {:?}", e))?;
    let init = full_moon::parse("_init()").map_err(|e| anyhow!("parse _init: {:?}", e))?;
    let reset = full_moon::parse("__reset_button_states()")
        .map_err(|e| anyhow!("parse reset: {:?}", e))?;
    let fr = full_moon::parse(cart::FRAME_CODE).map_err(|e| anyhow!("parse frame: {:?}", e))?;

    let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
    let cart_data = std::sync::Arc::new(celeste_core::cart_data::CartData::load(root.join("cart"))?);
    let (rx, ry) = celeste_interp::game_runner::start_room();
    let cache = std::sync::Arc::new(celeste_core::collision_cache::CollisionCache::new(
        &cart_data, rx, ry,
    )?);
    it.cache = Some(cache.clone());
    it.cart = Some(cart_data.clone());

    let st = cart::fresh_state::<Symbolic>(&mut it.d);
    let mut st = run_one(&mut it, &top, st)?;
    cart::inject_tile_flag_at(&mut st);
    let st = run_one(&mut it, &init, st)?;

    let w = shapes::walk(&mut it, &reset, &fr, st, 400, opts)?;
    if !w.refused.is_empty() {
        let why: Vec<String> =
            w.refused.iter().map(|(e, n)| format!("{} x {}", n, e)).collect();
        // The COUNTS matter as much as the reasons: "1 refusal against 12
        // traced shapes" is a hole to fill, "1 against 0" is a tracer
        // that never got started, and the message used to read the same
        // either way.
        bail!(
            "the shape walk could not trace every shape ({} traced, {} left the room, \
             {} unreachable):\n{}",
            w.shapes.len(),
            w.left_room,
            w.unreachable,
            why.join("\n")
        );
    }
    if w.dropped > 0 {
        bail!("the shape walk hit its cap with {} outcomes left", w.dropped);
    }

    let graph = std::mem::take(&mut it.d.graph);
    Ok((w.shapes, graph, cart_data, cache))
}

/// One kernel per heap shape the room reaches.
pub fn room_kernels_in(root: &std::path::Path) -> Result<Vec<Reference>> {
    room_kernels_with(root, super::shapes::WalkOpts::LEVEL0)
}

/// `room_kernels_in` for any walk mode (plans/kernel-ladder.md):
/// `LADDER` produces the rung-agnostic kernels (exact frame outputs,
/// widenings left to the campaign boundary), `EXACT` the exact-rem set
/// (interval slots as plain numbers, no rem forks).
pub fn room_kernels_with(
    root: &std::path::Path,
    opts: super::shapes::WalkOpts,
) -> Result<Vec<Reference>> {
    let (shapes, graph, cart_data, cache) = room_shapes_in(root, opts)?;
    let room = crate::transpile::graph::Room { cart: cart_data.clone(), cache: cache.clone() };
    let mut out = Vec::new();
    for sh in shapes {
        let frame = sh.frame;
        let bound = super::emit::bind(&frame, &graph)?;
        let lowered = super::emit::lower_frame(
            &bound.graph,
            &bound.inputs,
            &bound.uni,
            &bound.outcomes,
            Some(room.clone()),
            bound.forks,
        )
        .map_err(|e| name_cells(&frame, e))?;
        out.push(Reference {
            frame,
            // Every shape was traced into ONE arena so they share
            // subexpressions; each reference keeps a copy because the
            // check harness evaluates against it. Cheap - the whole
            // room's traced graph is a few thousand nodes.
            graph: graph.clone(),
            bound,
            lowered,
            cart: cart_data.clone(),
            cache: cache.clone(),
        });
    }
    Ok(out)
}

/// A block holding `rows` concrete input assignments, one per lane.
///
/// The kernel reads its inputs off a block by path, so checking it
/// against the graph needs a block that holds exactly the values the
/// graph is evaluated at. This builds one: the traced input shape, with
/// each row's values written into the cells the interface named.
pub fn input_block(r: &Reference, rows: &[Vec<super::iface::Conc>]) -> Result<celeste_engine::Rt2> {
    use super::iface::Conc;

    let mut b = celeste_engine::slots::reshape(&r.frame.in_rt2, rows.len());
    // The BLOCK-UNIFORM inputs go in as uniform columns, because that is
    // what the kernel's `bind` accepts - it takes them as `P8`, not as a
    // lane array, and a per-lane column there is a narrowing it refuses.
    // Every row therefore has to agree about them, and this says so
    // rather than silently taking row 0.
    let uni: std::collections::BTreeSet<u32> = r.bound.uni.iter().map(|(c, _)| *c).collect();
    for (i, cell) in r.frame.in_cells.iter().enumerate() {
        if !uni.contains(cell) {
            continue;
        }
        for (k, row) in rows.iter().enumerate() {
            if row[i] != rows[0][i] {
                anyhow::bail!(
                    "row {} disagrees with row 0 about the block-uniform slot {}",
                    k,
                    super::iface::show(&r.frame.iface.slots[i])
                );
            }
        }
        b.cols[*cell as usize] = Col::U(match rows[0][i] {
            Conc::Num(v) => AV::Num(v),
            Conc::Bool(v) => AV::Bool(v),
        });
    }
    for (i, cell) in r.frame.in_cells.iter().enumerate() {
        if uni.contains(cell) {
            continue;
        }
        let col = match rows[0].get(i) {
            Some(Conc::Num(_)) => Col::N(
                rows.iter()
                    .map(|row| match row[i] {
                        Conc::Num(v) => v,
                        Conc::Bool(_) => celeste_core::pico8_num::Pico8Num::from_i16(0),
                    })
                    .collect(),
            ),
            Some(Conc::Bool(_)) => Col::V(
                rows.iter()
                    .map(|row| match row[i] {
                        Conc::Bool(v) => AV::Bool(v),
                        Conc::Num(_) => AV::Nil,
                    })
                    .collect(),
            ),
            None => anyhow::bail!("row is shorter than the interface"),
        };
        b.cols[*cell as usize] = col;
    }
    Ok(b)
}

/// Write a kernel per shape, plus the table a dispatcher indexes.
///
/// Into a directory rather than a file because each shape's `Uni`,
/// `RowsIn` and `KOuts` are different types with the same names - they

/// Restate a lowering failure with its `Cell(n)`s NAMED.
///
/// The emitter works on the bound graph, where an input is a canonical
/// engine cell number and nothing downstream of the tracer knows what a
/// cell means. The interface does, so translating the numbers back is a
/// lookup - and the difference between "arms disagree in Sel(.., Cell(272))"
/// and knowing 272 is a particular object's field is the whole diagnosis.
fn name_cells(f: &super::verify::Frame, e: anyhow::Error) -> anyhow::Error {
    let msg = format!("{:#}", e);
    let mut seen: std::collections::BTreeSet<u32> = Default::default();
    let mut at = msg.as_str();
    while let Some(i) = at.find("Cell(") {
        at = &at[i + 5..];
        let end = at.find(')').unwrap_or(0);
        if let Ok(n) = at[..end].parse::<u32>() {
            seen.insert(n);
        }
    }
    let mut lines = Vec::new();
    for c in seen {
        let name = f
            .in_cells
            .iter()
            .position(|x| *x == c)
            .map(|i| super::iface::show(&f.iface.slots[i]))
            .unwrap_or_else(|| "not an input of this frame".to_string());
        lines.push(format!("  Cell({}) = {}", c, name));
    }
    anyhow::anyhow!("{}\nwhere\n{}", msg, lines.join("\n"))
}

/// coexist only as separate modules.
pub fn write_room_kernels(root: &std::path::Path, dir: &std::path::Path) -> Result<Vec<usize>> {
    let refs = room_kernels_in(root)?;
    write_kernels_from_refs(&refs, dir)
}

/// The RUNG-AGNOSTIC kernel set (plans/kernel-ladder.md): traced with
/// `widen = false`, so a kernel hands the campaign the frame's EXACT
/// rows and the campaign boundary applies whichever precision rung is
/// configured. One set serves every rem rung that carries rem as an
/// interval (Bits(0..=15)); its accumulators must go through
/// `Rt2::boundary_exact`, never `boundary`, or the engine would re-apply
/// Bits(0) on top.
pub fn write_room_kernels_ladder(
    root: &std::path::Path,
    dir: &std::path::Path,
) -> Result<Vec<usize>> {
    let refs = room_kernels_with(root, super::shapes::WalkOpts::LADDER)?;
    write_kernels_from_refs(&refs, dir)
}

/// The EXACT-REM kernel set, for the ladder's top rung (k = 16, rem
/// `Exact`): traced with the interval slots as plain per-lane numbers,
/// so `__split_by_flr` is the identity and the set has no rem forks.
/// Binds only blocks whose rem is a number - which is every block of an
/// exact-rem campaign, and no block of any other rung.
pub fn write_room_kernels_exact(
    root: &std::path::Path,
    dir: &std::path::Path,
) -> Result<Vec<usize>> {
    let refs = room_kernels_with(root, super::shapes::WalkOpts::EXACT)?;
    write_kernels_from_refs(&refs, dir)
}

/// As `write_room_kernels`, but the kernels come from the CONSTANT-LATTICE
/// fixpoint (static objects baked in). Fewer, smaller kernels; each
/// carries the lattice's `pin_guard` in its `ok`, so a lane whose baked
/// fields disagree is declined to a fallback. plans/specialize.md.
pub fn write_room_kernels_lattice(root: &std::path::Path, dir: &std::path::Path) -> Result<Vec<usize>> {
    let mut lw = room_constant_lattice(root)?;
    let room = crate::transpile::graph::Room { cart: lw.cart.clone(), cache: lw.cache.clone() };
    let mut refs: Vec<Reference> = Vec::new();
    let (mut ok_n, mut fail_n) = (0, 0);
    for (i, (_k, f)) in std::mem::take(&mut lw.frames).into_iter().enumerate() {
        let bound = match super::emit::bind(&f, &lw.graph) {
            Ok(b) => b, Err(e) => { fail_n += 1; eprintln!("shape {} bind FAILED: {}", i, format!("{:#}", e).lines().next().unwrap_or("")); continue }
        };
        let lowered = match super::emit::lower_frame(&bound.graph, &bound.inputs, &bound.uni, &bound.outcomes, Some(room.clone()), bound.forks) {
            Ok(l) => l, Err(e) => { fail_n += 1; eprintln!("shape {} lower FAILED: {}", i, format!("{:#}", name_cells(&f, e)).lines().next().unwrap_or("")); continue }
        };
        if let Err(e) = render(&f, &bound, &lowered, &format!("shape {}", i)) {
            fail_n += 1; eprintln!("shape {} render FAILED: {}", i, format!("{:#}", e).lines().next().unwrap_or("")); continue;
        }
        refs.push(Reference { frame: f, graph: lw.graph.clone(), bound, lowered, cart: lw.cart.clone(), cache: lw.cache.clone() });
        ok_n += 1;
    }
    eprintln!("lattice render: {} ok, {} failed", ok_n, fail_n);
    write_kernels_from_refs(&refs, dir)
}

fn write_kernels_from_refs(refs: &[Reference], dir: &std::path::Path) -> Result<Vec<usize>> {
    std::fs::create_dir_all(dir)?;
    let mut sizes = Vec::new();
    let mut sources: Vec<String> = Vec::new();
    let mut decl = String::new();
    let mut table = String::new();
    for (i, r) in refs.iter().enumerate() {
        let src = render(&r.frame, &r.bound, &r.lowered, &format!("shape {}", i))?;
        sizes.push(src.lines().count());
        std::fs::write(dir.join(format!("kernel{}.rs", i)), &src)?;
        sources.push(src);
        writeln!(decl, "#[path = \"kernel{i}.rs\"]\npub mod k{i};", i = i)?;
        writeln!(
            table,
            "    Kernel {{ name: \"shape {i}\", shape: k{i}::SHAPE, \
             outcomes: k{i}::OUTCOMES, acc: k{i}::acc, step: k{i}::step, \
             why: k{i}::bind_why }},",
            i = i
        )?;
    }
    // The set's own content hash, the way the fused artifact carries
    // one. The class kernels get away without it because the compile
    // recipe TEXT is a faithful proxy for them and the campaign
    // fingerprint hashes that; nothing upstream of a traced kernel is a
    // file the fingerprint can read, so the kernels hash themselves.
    // Without it two campaigns with different traced sets would share
    // checkpoints.
    let fingerprint = {
        use std::hash::Hasher;
        let mut h = rustc_hash::FxHasher::default();
        for src in &sources {
            std::hash::Hash::hash(src, &mut h);
        }
        h.finish()
    };
    let mut m = String::new();
    writeln!(
        m,
        "// GENERATED by `trace::kernel::write_room_kernels`. Do not edit.\n\
         //\n\
         // One module per heap SHAPE the room reaches. Each shape's\n\
         // `Uni`, `RowsIn` and `KOuts` are different types with the same\n\
         // names, so they coexist only as separate modules - which is\n\
         // why `step` and `acc` exist: they are the uniform surface a\n\
         // dispatcher can hold as function pointers.\n\
         //\n\
         // `Kernel` is NOT declared here. It lives in\n\
         // `celeste_engine::traced` - a generated copy would have to be\n\
         // kept in step with the frame loops that consume it by hand,\n\
         // and regenerating is not a good moment to discover a\n\
         // signature changed.\n\
         //\n\
         // Naming the ENGINE\'s copy and not `celeste_rust`\'s re-export\n\
         // is what lets this set be checked in below `celeste-rust`: a\n\
         // kernel set that mentions the crate holding the emitters can\n\
         // only live above them.\n\
         #![allow(clippy::all)]\n\
         pub use celeste_engine::traced::Kernel;\n\
         \n\
         {decl}\n\
         pub const KERNELS: &[Kernel] = &[\n\
         {table}];\n\
         \n\
         /// Content hash of every kernel source in this set. Hashed into\n\
         /// the campaign fingerprint so two different traced sets can\n\
         /// never share a checkpoint.\n\
         pub const FINGERPRINT: u64 = {fingerprint};\n",
        decl = decl,
        table = table,
        fingerprint = fingerprint
    )?;
    std::fs::write(dir.join("mod.rs"), m)?;
    Ok(sizes)
}

#[cfg(test)]
mod tests {
    /// The gate that makes the CHECKED-IN traced set safe.
    ///
    /// `crates/celeste-kernels/src/traced/` is committed, so nothing in
    /// the build forces it to match the tracer that claims to produce
    /// it - and a stale kernel there does not fail to compile, it
    /// computes a frame the tracer no longer agrees with. This
    /// regenerates the whole set from the cart, the same way
    /// `regen-generated.sh` does, and compares byte for byte.
    ///
    /// Not `#[ignore]`d. It costs ~6 s, against the ~200 s and ~44 s
    /// that put the other regeneration tests behind `--ignored`, and it
    /// guards exactly what `generated_is_current` guards for the class
    /// kernels - which runs on every commit.
    #[test]
    fn traced_kernels_are_current() {
        let dir = std::env::temp_dir().join(format!("celeste-traced-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        let sizes = super::write_room_kernels(std::path::Path::new("."), &dir)
            .unwrap_or_else(|e| panic!("regenerate the room kernel set (run from the repo root): {:#}", e));

        let committed = std::path::Path::new("crates/celeste-kernels/src/traced");
        let listing = |d: &std::path::Path| -> std::collections::BTreeSet<String> {
            std::fs::read_dir(d)
                .unwrap_or_else(|e| panic!("read {}: {}", d.display(), e))
                .map(|e| e.unwrap().file_name().to_string_lossy().into_owned())
                .collect()
        };
        // Both directions. One shape FEWER than last time leaves a
        // stale `kernelN.rs` behind that still compiles and is still
        // reachable through nothing - comparing only the files the
        // tracer just wrote would pass over it.
        let (fresh_files, on_disk_files) = (listing(&dir), listing(committed));
        assert_eq!(
            fresh_files,
            on_disk_files,
            "the traced kernel set has a different FILE LIST than what is committed \
             ({} shapes now). Run ./regen-generated.sh.",
            sizes.len()
        );

        for name in &fresh_files {
            let a = std::fs::read_to_string(committed.join(name)).unwrap();
            let b = std::fs::read_to_string(dir.join(name)).unwrap();
            if a != b {
                let first = a.lines().zip(b.lines()).position(|(x, y)| x != y).map(|i| i + 1);
                panic!(
                    "crates/celeste-kernels/src/traced/{} is STALE: on disk {} lines, \
                     tracer says {} lines, first differing line {:?}. Run \
                     ./regen-generated.sh and read the diff.",
                    name,
                    a.lines().count(),
                    b.lines().count(),
                    first
                );
            }
        }
        let _ = std::fs::remove_dir_all(&dir);
    }

    /// The RUNG-AGNOSTIC set's staleness gate - the same byte-for-byte
    /// contract as `traced_kernels_are_current`, for
    /// `crates/celeste-kernels/src/ladder` (plans/kernel-ladder.md).
    #[test]
    fn ladder_kernels_are_current() {
        let dir = std::env::temp_dir().join(format!("celeste-ladder-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        let sizes = super::write_room_kernels_ladder(std::path::Path::new("."), &dir)
            .unwrap_or_else(|e| {
                panic!("regenerate the ladder kernel set (run from the repo root): {:#}", e)
            });

        let committed = std::path::Path::new("crates/celeste-kernels/src/ladder");
        let listing = |d: &std::path::Path| -> std::collections::BTreeSet<String> {
            std::fs::read_dir(d)
                .unwrap_or_else(|e| panic!("read {}: {}", d.display(), e))
                .map(|e| e.unwrap().file_name().to_string_lossy().into_owned())
                .collect()
        };
        let (fresh_files, on_disk_files) = (listing(&dir), listing(committed));
        assert_eq!(
            fresh_files,
            on_disk_files,
            "the ladder kernel set has a different FILE LIST than what is committed \
             ({} shapes now). Regenerate with `transpile --room-kernels-ladder \
             crates/celeste-kernels/src/ladder`.",
            sizes.len()
        );

        for name in &fresh_files {
            let a = std::fs::read_to_string(committed.join(name)).unwrap();
            let b = std::fs::read_to_string(dir.join(name)).unwrap();
            if a != b {
                let first =
                    a.lines().zip(b.lines()).position(|(x, y)| x != y).map(|i| i + 1);
                panic!(
                    "crates/celeste-kernels/src/ladder/{} is STALE: on disk {} lines, \
                     tracer says {} lines, first differing line {:?}. Regenerate with \
                     `transpile --room-kernels-ladder crates/celeste-kernels/src/ladder` \
                     and read the diff.",
                    name,
                    a.lines().count(),
                    b.lines().count(),
                    first
                );
            }
        }
        let _ = std::fs::remove_dir_all(&dir);
    }

    /// The EXACT-REM set's staleness gate - same contract, for
    /// `crates/celeste-kernels/src/exact` (plans/kernel-ladder.md).
    #[test]
    fn exact_kernels_are_current() {
        let dir = std::env::temp_dir().join(format!("celeste-exact-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        let sizes = super::write_room_kernels_exact(std::path::Path::new("."), &dir)
            .unwrap_or_else(|e| {
                panic!("regenerate the exact kernel set (run from the repo root): {:#}", e)
            });

        let committed = std::path::Path::new("crates/celeste-kernels/src/exact");
        let listing = |d: &std::path::Path| -> std::collections::BTreeSet<String> {
            std::fs::read_dir(d)
                .unwrap_or_else(|e| panic!("read {}: {}", d.display(), e))
                .map(|e| e.unwrap().file_name().to_string_lossy().into_owned())
                .collect()
        };
        let (fresh_files, on_disk_files) = (listing(&dir), listing(committed));
        assert_eq!(
            fresh_files,
            on_disk_files,
            "the exact kernel set has a different FILE LIST than what is committed \
             ({} shapes now). Regenerate with `transpile --room-kernels-exact \
             crates/celeste-kernels/src/exact`.",
            sizes.len()
        );

        for name in &fresh_files {
            let a = std::fs::read_to_string(committed.join(name)).unwrap();
            let b = std::fs::read_to_string(dir.join(name)).unwrap();
            if a != b {
                let first =
                    a.lines().zip(b.lines()).position(|(x, y)| x != y).map(|i| i + 1);
                panic!(
                    "crates/celeste-kernels/src/exact/{} is STALE: on disk {} lines, \
                     tracer says {} lines, first differing line {:?}. Regenerate with \
                     `transpile --room-kernels-exact crates/celeste-kernels/src/exact` \
                     and read the diff.",
                    name,
                    a.lines().count(),
                    b.lines().count(),
                    first
                );
            }
        }
        let _ = std::fs::remove_dir_all(&dir);
    }



    /// How much of the row amplification is STATICALLY removable?
    ///
    /// `variants` dedups on the whole `(outputs, live, ok)` tuple, so two
    /// button assignments that write identical values and differ only in
    /// which lanes they claim stay separate - and each appends its own
    /// copy of the same row. Grouping PER OUTCOME on `(outputs, ok, bd)`
    /// and unioning the live masks would remove those before they are
    /// written.
    ///
    /// This counts what that would save, from the expressions alone, so
    /// the restructuring is sized before it is built rather than after.
    #[test]
    #[ignore]
    fn how_many_variants_write_the_same_row() {
        let refs = match super::room_kernels_in(std::path::Path::new(".")) {
            Ok(r) => r,
            Err(e) => panic!("{:#}", e),
        };
        for (si, r) in refs.iter().enumerate() {
            let n = r.lowered.variants.len();
            let outcomes = r.lowered.outs.len();
            let mut per: Vec<usize> = Vec::new();
            for oi in 0..outcomes {
                // OUTPUTS only. `ok` is a per-variant variable NAME
                // (`ok_v{mask}`), so including it groups nothing - it
                // was 1.0x on the first attempt for exactly that reason.
                //
                // Dropping it is not a shortcut: the rows a variant
                // appends are `live & !deopt`, and two variants with
                // identical output VALUES append identical rows, so the
                // union of their take masks is exact whatever their `ok`
                // says. Deopt REPORTING still ORs per variant, which is
                // a separate quantity.
                let mut groups: std::collections::BTreeSet<String> = Default::default();
                for v in &r.lowered.variants {
                    let Some(p) = v.per[oi].as_ref() else {
                        continue;
                    };
                    let outs: Vec<String> =
                        p.outputs.iter().map(|(c, e)| format!("{}={}", c, e)).collect();
                    groups.insert(outs.join(";"));
                }
                per.push(groups.len());
            }
            let total: usize = per.iter().sum();
            eprintln!(
                "[dedup] shape {}: {} variants x {} outcomes = {} appends; \
                 grouped {:?} = {} ({:.1}x fewer)",
                si,
                n,
                outcomes,
                n * outcomes,
                per,
                total,
                (n * outcomes) as f64 / total.max(1) as f64
            );
        }
    }

    /// How much of a frame exists only to compute values the boundary
    /// ERASES a moment later?
    ///
    /// `player.rem.x/y` are overwritten with the widened constant, and
    /// the four timers are pinned to 0. The frame computes all of them -
    /// through the fork, in `rem`'s case - writes them into the output
    /// block, and the boundary discards them.
    ///
    /// Philippe's proposal is to compile the widenings INTO the kernel,
    /// at which point those chains are dead and can be deleted. This
    /// counts what that would remove.
    #[test]
    #[ignore]
    fn how_much_of_a_frame_is_erased_immediately() {
        use crate::transpile::graph::NodeId;
        let refs = match super::room_kernels_in(std::path::Path::new(".")) {
            Ok(r) => r,
            Err(e) => panic!("{:#}", e),
        };
        let erased = ["rem.x", "rem.y", "frames", "seconds", "minutes", "deaths"];
        for (si, r) in refs.iter().enumerate() {
            let g = &r.bound.graph;
            let reach = |roots: &[NodeId]| -> usize {
                let mut seen = vec![false; g.len()];
                let mut st = roots.to_vec();
                let mut n = 0;
                while let Some(x) = st.pop() {
                    if seen[x as usize] {
                        continue;
                    }
                    seen[x as usize] = true;
                    n += 1;
                    st.extend(g.get(x).args.iter().copied());
                }
                n
            };
            let mut all: Vec<NodeId> = Vec::new();
            let mut kept: Vec<NodeId> = Vec::new();
            let mut dropped = 0;
            for (oi, o) in r.bound.outcomes.iter().enumerate() {
                for (k, (cell, node, _)) in o.outputs.iter().enumerate() {
                    let _ = cell;
                    all.push(*node);
                    let path = crate::trace::iface::show(&r.frame.outs[oi].fields[k].0);
                    if erased.iter().any(|e| path.ends_with(e)) {
                        dropped += 1;
                    } else {
                        kept.push(*node);
                    }
                }
                all.push(o.live);
                all.push(o.ok);
                kept.push(o.live);
                kept.push(o.ok);
            }
            let (a, k) = (reach(&all), reach(&kept));
            eprintln!(
                "[erase] shape {}: {} nodes reachable from all outputs, {} without the {} \
                 erased cells -> {} nodes ({:.1}%) exist only for values the boundary discards",
                si, a, k, dropped, a - k, 100.0 * (a - k) as f64 / a as f64
            );
        }
    }

    /// What specializing the fork WOULD cost, in nodes - the measurement
    /// that refuted it (2026-08-23).
    ///
    /// The runtime loop runs the tail of the frame once per fork
    /// configuration; specialization runs each configuration's nodes
    /// once, sharing everything the configurations agree on. Which is
    /// cheaper is entirely a question of how much of the frame is
    /// downstream of the split, and that is a reachability fact this
    /// answers before anything is built.
    ///
    /// Reported per shape: the specialized arena's live node count with
    /// the splits left standing (what is emitted today, inside a loop)
    /// against the same count with all 2^forks configurations resolved.
    ///
    /// The room (1,0) answer - 3.92x of a maximum 4.00x, 1.9% shared -
    /// was WRONG TWICE, and the wrongness is the reason this prints as
    /// much as it does now. It ran on the only room that existed, which
    /// forks twice and is the weakest point on the curve; and it blended
    /// all 64 button assignments into the arena, so button divergence
    /// dominated and the fork question was never actually asked.
    ///
    /// Holding the buttons fixed and running room (2,0), the ratio is
    /// flat at ~2.3-3.2x while the configuration count goes 4 -> 16 ->
    /// 256, i.e. sharing goes 40.6% -> 82-86% -> 98.7-99.0%.
    /// Specialization gets CHEAPER, relatively, the more forks there
    /// are.
    ///
    /// Node counts alone do not decide it, so this also reports the
    /// runtime comparison. The loop re-executes every node above its
    /// fork level once per configuration, so its work is
    /// `sum_l count_l * 2^l`; the flat form executes each node once, so
    /// its work IS its node count. Those two are the numbers that
    /// matter, and the level histogram says where a shape sits between
    /// them.
    #[test]
    #[ignore]
    fn what_specializing_the_fork_would_cost() {
        use crate::transpile::graph::{Graph, NodeId};
        let refs = match super::room_kernels_in(std::path::Path::new(".")) {
            Ok(r) => r,
            Err(e) => panic!("{:#}", e),
        };
        for (si, r) in refs.iter().enumerate() {
            let g = &r.bound.graph;
            let forks = r.bound.forks;
            let base: Vec<NodeId> = {
                let mut v = Vec::new();
                for o in r.bound.outcomes.iter() {
                    v.extend(o.outputs.iter().map(|(_, n, _)| *n));
                    v.push(o.live);
                    v.push(o.ok);
                }
                v
            };
            let live_count = |arena: &Graph, roots: &[NodeId]| -> usize {
                crate::transpile::bdd::reachable(arena, roots)
                    .iter()
                    .filter(|b| **b)
                    .count()
            };
            // Today: ONE assignment, splits left as nodes. One rather
            // than all 64, because the question is what the FORK
            // configurations share, and holding the buttons fixed is
            // what isolates it - mixing both makes the ratio a blend of
            // two effects.
            let mut loop_arena = Graph::new();
            let mut loop_roots: Vec<NodeId> = Vec::new();
            {
                let map = g.specialize_config_into(0, None, &mut loop_arena);
                loop_roots.extend(base.iter().map(|x| map[*x as usize]));
            }
            // Specialized: ONE assignment x 2^forks configurations.
            //
            // `let ns: u8 = 1 << forks` was the third member of today's
            // shift-overflow family (see `ChoiceSet`): at 8 forks it
            // wraps to 0 and the inner loop does not run, so the test
            // would have reported perfect sharing for the shapes that
            // share least. It only ever ran on room (1,0), where forks
            // is 2, so it was never wrong in practice - which is
            // exactly how the ChoiceSet one survived too.
            //
            // Enumeration is 2^forks, so it is capped and the cap is
            // ANNOUNCED. A silent skip here would read as "measured and
            // fine" for precisely the shapes the question is about.
            const MAX_FORKS: u8 = 14;
            if forks > MAX_FORKS {
                eprintln!(
                    "[fork] shape {}: {} forks - SKIPPED, 2^{} configurations is past \
                     the {} cap. Not measured, not zero.",
                    si, forks, forks, MAX_FORKS
                );
                continue;
            }
            let ns: u64 = 1u64 << forks;
            let mut flat_arena = Graph::new();
            let mut flat_roots: Vec<NodeId> = Vec::new();
            // Configurations that FOLD AWAY. Resolving a split turns
            // `FragOk` into ordinary arithmetic, and arithmetic folds:
            // a configuration whose every outcome is statically dead is
            // one the flat form never emits, while the runtime loop
            // still enters it and discovers `valid == 0`. So this is
            // both a discount on the flat size and a count of trips the
            // loop takes for nothing.
            let mut dead = 0u64;
            let mut live_roots: Vec<NodeId> = Vec::new();
            // How many DISTINCT successors the configurations produce.
            //
            // Sharing the compute is only half a flat kernel: each
            // configuration also has to WRITE its row, and the write
            // does not hash-cons - it is emitted code. But two
            // configurations that land on the same output tuple are the
            // same successor for every lane (the same argument
            // `specialize_into` makes for buttons), so they need ONE
            // write between them. This counts the tuples, which is the
            // number of write sites a flat kernel actually needs, as
            // against `ns` if none of them coincide.
            let mut tuples: std::collections::HashSet<Vec<NodeId>> =
                std::collections::HashSet::new();
            for c in 0..ns {
                let map = g.specialize_config_into(0, Some(c), &mut flat_arena);
                let alive = r.bound.outcomes.iter().any(|o| {
                    !matches!(
                        flat_arena.get(map[o.live as usize]).op,
                        crate::transpile::graph::Op::ConstBool(false)
                    )
                });
                if alive {
                    live_roots.extend(base.iter().map(|x| map[*x as usize]));
                    tuples.insert(base.iter().map(|x| map[*x as usize]).collect());
                } else {
                    dead += 1;
                }
                flat_roots.extend(base.iter().map(|x| map[*x as usize]));
            }
            let (a, b) = (
                live_count(&loop_arena, &loop_roots),
                live_count(&flat_arena, &flat_roots),
            );
            let b_live = live_count(&flat_arena, &live_roots);
            eprintln!(
                "[fork]   shape {}: {} of {} configurations fold away \
                 ({:.1}%); flat nodes {} -> {} once they are dropped",
                si,
                dead,
                ns,
                100.0 * dead as f64 / ns as f64,
                b,
                b_live,
            );
            eprintln!(
                "[fork]   shape {}: {} distinct successors from {} configurations \
                 ({} write sites, not {})",
                si,
                tuples.len(),
                ns,
                tuples.len(),
                ns,
            );

            // WHERE each fork comes from.
            //
            // `Iface::ival` says the only INTERVAL input is the player's
            // `rem.x`/`rem.y`, so a shape forking 14 times is forking on
            // twelve values that became intervals during the frame. That
            // is either six more objects whose motion genuinely inherits
            // the player's uncertainty, or an imprecision worth
            // narrowing - and the difference is the difference between
            // 16,384 real successors and 16,384 spurious ones. Naming
            // the cells under each fork is what tells them apart.
            let mut in_path: std::collections::BTreeMap<u32, String> =
                std::collections::BTreeMap::new();
            for (i, cell) in r.frame.in_cells.iter().enumerate() {
                in_path.insert(*cell, crate::trace::iface::show(&r.frame.iface.slots[i]));
            }
            let ivals: Vec<String> = r
                .frame
                .iface
                .ival
                .iter()
                .enumerate()
                .filter(|(_, b)| **b)
                .map(|(i, _)| crate::trace::iface::show(&r.frame.iface.slots[i]))
                .collect();
            eprintln!("[fork]   shape {}: interval INPUTS are {:?}", si, ivals);
            for d in 0..forks {
                let split = (0..g.len() as NodeId)
                    .find(|id| matches!(g.get(*id).op, crate::transpile::graph::Op::Split(x) if x == d));
                let Some(split) = split else {
                    eprintln!("[fork]   shape {} fork {}: no Split node (folded away)", si, d);
                    continue;
                };
                let operand = g.get(split).args[0];
                let reach = crate::transpile::bdd::reachable(g, &[operand]);
                let mut objs: Vec<String> = Vec::new();
                let mut n_cells = 0usize;
                for id in 0..g.len() as NodeId {
                    if !reach[id as usize] {
                        continue;
                    }
                    if let crate::transpile::graph::Op::Cell(c) = g.get(id).op {
                        if let Some(p) = in_path.get(&c) {
                            n_cells += 1;
                            // "objects[3].spd.y" -> "objects[3]"
                            let head = match p.find(']') {
                                Some(k) => p[..=k].to_string(),
                                None => p.clone(),
                            };
                            objs.push(head);
                        }
                    }
                }
                objs.sort();
                objs.dedup();
                eprintln!(
                    "[fork]   shape {} fork {}: {} input cells under it, from {:?}",
                    si, d, n_cells, objs
                );
            }

            // The other direction, which is the one that names an
            // object: not "what does this fork read" but "whose
            // position does it write". A fork reads most of the state
            // (every object's motion is guarded by the same globals),
            // so the read cone identifies nothing; the WRITE cone does,
            // because `objects[k].rem.x` is written by exactly the
            // `move` call on object k.
            //
            // `bound.outcomes` and `frame.outs` are the same list in the
            // same order - `bind` renumbers cells, not roots - so the
            // node ids come from the first and the paths from the
            // second.
            let cones = g.split_cones();
            for (oi, o) in r.bound.outcomes.iter().enumerate() {
                let paths = &r.frame.outs[oi].fields;
                assert_eq!(
                    paths.len(),
                    o.outputs.len(),
                    "outcome {} has {} paths but {} outputs - the two lists are \
                     supposed to be the same roots in the same order",
                    oi,
                    paths.len(),
                    o.outputs.len()
                );
                let mut by_obj: std::collections::BTreeMap<String, u64> =
                    std::collections::BTreeMap::new();
                for ((path, _, _), (_, node, _)) in paths.iter().zip(o.outputs.iter()) {
                    let p = crate::trace::iface::show(path);
                    let head = match p.find(']') {
                        Some(k) => p[..=k].to_string(),
                        None => p.clone(),
                    };
                    *by_obj.entry(head).or_insert(0) |= cones[*node as usize];
                }
                for (obj, mask) in by_obj {
                    if mask == 0 {
                        continue;
                    }
                    let which: Vec<u8> = (0..forks).filter(|d| mask & (1 << d) != 0).collect();
                    eprintln!(
                        "[fork]   shape {} outcome {}: {} depends on forks {:?}",
                        si, oi, obj, which
                    );
                }
                // The whole outcome, VALUES AND MASKS. `live` and `ok`
                // matter as much as the fields: an outcome whose values
                // ignore every fork can still be gated on fork
                // validity, and an outcome is only cheap to specialize
                // if the mask is cheap too.
                let whole = o
                    .outputs
                    .iter()
                    .map(|(_, n, _)| cones[*n as usize])
                    .chain([cones[o.live as usize], cones[o.ok as usize]])
                    .fold(0u64, |a, b| a | b);
                let which: Vec<u8> = (0..forks).filter(|d| whole & (1 << d) != 0).collect();
                eprintln!(
                    "[fork]   OUTCOME shape {} outcome {}: {} of {} forks {:?} \
                     -> {} configurations",
                    si,
                    oi,
                    which.len(),
                    forks,
                    which,
                    1u64 << which.len()
                );

                // The number that decides whether flat is BUILDABLE.
                //
                // `emit_body` already specializes 64 button assignments
                // into one arena and dedups them by SIGNATURE into
                // representatives, and each representative becomes an
                // emitted body. Resolving forks the same way makes them
                // another variant dimension - but the existing
                // signature spans EVERY outcome at once, and that is
                // exactly the aggregation that made 16,384
                // configurations look distinct this morning. Per
                // outcome, over only that outcome's own forks, is the
                // version that can collapse.
                //
                // So: 64 buttons x 2^(this outcome's forks), deduped by
                // this outcome's signature alone. That count is how
                // many bodies a flat kernel emits for this outcome.
                // Philippe: "if an outcome is involved in a loop that
                // is irrelevant for its values, I'd expect all the
                // redundant copies the loop creates to collapse."
                //
                // Right, and the enumeration above already only walks
                // an outcome's OWN forks, so there is no such redundancy
                // left to collapse. The question that remains is
                // whether the forks it DOES depend on are in its VALUES
                // or only in its MASK - because those want completely
                // different treatment. 256 distinct value tuples is 256
                // rows and there is nothing to be done. 256 distinct
                // masks over ONE value tuple is one row whose lane mask
                // is an OR, which is a single body.
                let fields_cone = o
                    .outputs
                    .iter()
                    .map(|(_, n, _)| cones[*n as usize])
                    .fold(0u64, |a, b| a | b);
                let pop = |m: u64| -> u32 { m.count_ones() };
                let nc = 1u64 << which.len();
                // The 64-button body count is the expensive half (64 x
                // 2^k specializations of a 20k-node graph); the
                // contradiction count below needs one button. Shape 1
                // is the 14-fork case the question is about, so the
                // expensive half runs there and the cheap half runs
                // everywhere.
                let full = si == 1;
                let mut sigs: std::collections::HashSet<Vec<NodeId>> =
                    std::collections::HashSet::new();
                let mut vals: std::collections::HashSet<Vec<NodeId>> =
                    std::collections::HashSet::new();
                let mut lives: std::collections::HashSet<NodeId> =
                    std::collections::HashSet::new();
                let mut oks: std::collections::HashSet<NodeId> =
                    std::collections::HashSet::new();
                let mut arena = Graph::new();
                for m in 0u8..if full { 64 } else { 1 } {
                    for k in 0..nc {
                        let mut sm = 0u64;
                        for (i, d) in which.iter().enumerate() {
                            if k & (1 << i) != 0 {
                                sm |= 1u64 << d;
                            }
                        }
                        let map = g.specialize_config_into(m, Some(sm), &mut arena);
                        let v: Vec<NodeId> =
                            o.outputs.iter().map(|(_, n, _)| map[*n as usize]).collect();
                        let mut sig = v.clone();
                        sig.push(map[o.ok as usize]);
                        sig.push(map[o.live as usize]);
                        vals.insert(v);
                        lives.insert(map[o.live as usize]);
                        oks.insert(map[o.ok as usize]);
                        sigs.insert(sig);
                    }
                }
                // The GRAPH itself, for the outcomes the question is
                // about. Summary statistics have been wrong six times
                // today; this prints what `live` and `ok` are actually
                // made of - the AND-conjuncts, each with its op and the
                // forks its cone touches - so the structure can be read
                // rather than inferred.
                if si == 1 && which.len() > 2 {
                    fn conj(g: &Graph, n: NodeId, out: &mut Vec<NodeId>) {
                        match &g.get(n).op {
                            crate::transpile::graph::Op::And => {
                                for a in g.get(n).args.clone() {
                                    conj(g, a, out);
                                }
                            }
                            crate::transpile::graph::Op::ConstBool(true) => {}
                            _ => out.push(n),
                        }
                    }
                    for (label, root) in [("live", o.live), ("ok", o.ok)] {
                        let mut cs = Vec::new();
                        conj(g, root, &mut cs);
                        eprintln!(
                            "[fork]   GRAPH shape {} outcome {} {}: {} conjuncts",
                            si,
                            oi,
                            label,
                            cs.len()
                        );
                        for c in cs.iter().take(24) {
                            let m = cones[*c as usize];
                            let f: Vec<u8> =
                                (0..forks).filter(|d| m & (1 << d) != 0).collect();
                            let kids: Vec<String> = g
                                .get(*c)
                                .args
                                .iter()
                                .map(|a| format!("{:?}", g.get(*a).op))
                                .collect();
                            eprintln!(
                                "[fork]     {} {:?}({}) forks {:?}",
                                label,
                                g.get(*c).op,
                                kids.join(", "),
                                f
                            );
                        }
                    }
                }

                // The count above is PRE-SIMPLIFICATION, and the real
                // emitter does not stop there. `Emit::bare` sets
                // `decide: true` for the traced path, so `emit_body`
                // runs ival -> bdd -> ival over the SPECIALIZED arena
                // before it computes any signature - the pass whose own
                // doc records 10,510 nodes going to 4,714 on a traced
                // frame, and whose whole point is that guard algebra
                // only collapses once the choices are constants.
                //
                // Resolving a fork makes it a constant exactly as
                // resolving a button does. So the honest body count is
                // the one taken AFTER that pass, and every number I
                // have reported so far skipped it.
                if si == 1 && !which.is_empty() {
                    let room = crate::transpile::graph::Room {
                        cart: r.cart.clone(),
                        cache: r.cache.clone(),
                    };
                    let mut a3 = Graph::new();
                    let mut roots: Vec<NodeId> = Vec::new();
                    let per = o.outputs.len() + 2;
                    for k in 0..nc {
                        let mut sm = 0u64;
                        for (i, d) in which.iter().enumerate() {
                            if k & (1 << i) != 0 {
                                sm |= 1u64 << d;
                            }
                        }
                        let map = g.specialize_config_into(0, Some(sm), &mut a3);
                        roots.extend(o.outputs.iter().map(|(_, n, _)| map[*n as usize]));
                        roots.push(map[o.ok as usize]);
                        roots.push(map[o.live as usize]);
                    }
                    let raw: std::collections::HashSet<&[NodeId]> =
                        roots.chunks(per).collect();
                    let (g1, m1, _) = crate::transpile::ival::fold(&a3, &roots, Some(&room))
                        .expect("interval fold");
                    let r1: Vec<NodeId> = roots.iter().map(|x| m1[*x as usize]).collect();
                    let (g2, m2, _) =
                        crate::transpile::bdd::simplify_until_stable(&g1, &r1, 1 << 22, 4);
                    let r2: Vec<NodeId> = r1.iter().map(|x| m2[*x as usize]).collect();
                    let (_g3, m3, _) = crate::transpile::ival::fold(&g2, &r2, Some(&room))
                        .expect("interval fold 2");
                    let r3: Vec<NodeId> = r2.iter().map(|x| m3[*x as usize]).collect();
                    let decided: std::collections::HashSet<&[NodeId]> =
                        r3.chunks(per).collect();
                    eprintln!(
                        "[fork]   DECIDED shape {} outcome {}: {} configs, {} distinct \
                         raw -> {} distinct after ival/bdd/ival",
                        si,
                        oi,
                        nc,
                        raw.len(),
                        decided.len()
                    );
                }

                // Philippe: "10,000 of anything seems wrong. There is
                // no way to get 10,000 distinct outcomes from one input
                // frame. My prior is that none of this explosion is
                // real. Strong prior."
                //
                // The test of that: an outcome reachable on ONE control
                // flow path cannot have two paths' forks both live, so
                // most of its 2^k configurations should be
                // CONTRADICTORY - `live` folds to false. Count them. If
                // the count is high the explosion is an artifact of
                // enumerating combinations that cannot co-occur; if it
                // is zero, either they really can co-occur or the
                // folder cannot see that they cannot, and those want
                // very different fixes.
                let mut dead_here = 0u64;
                let mut ok_false = 0u64;
                {
                    let mut a2 = Graph::new();
                    for k in 0..nc {
                        let mut sm = 0u64;
                        for (i, d) in which.iter().enumerate() {
                            if k & (1 << i) != 0 {
                                sm |= 1u64 << d;
                            }
                        }
                        let map = g.specialize_config_into(0, Some(sm), &mut a2);
                        if matches!(
                            a2.get(map[o.live as usize]).op,
                            crate::transpile::graph::Op::ConstBool(false)
                        ) {
                            dead_here += 1;
                        }
                        if matches!(
                            a2.get(map[o.ok as usize]).op,
                            crate::transpile::graph::Op::ConstBool(false)
                        ) {
                            ok_false += 1;
                        }
                    }
                }
                eprintln!(
                    "[fork]   CONTRADICTORY shape {} outcome {}: {} of {} configurations \
                     have live=false, {} have ok=false",
                    si, oi, dead_here, nc, ok_false
                );
                eprintln!(
                    "[fork]   BODIES shape {} outcome {}: 64 buttons x {} configs = {} \
                     -> {} bodies; VALUES {} (cone {} forks), live {} (cone {}), \
                     ok {} (cone {})",
                    si,
                    oi,
                    nc,
                    64 * nc,
                    sigs.len(),
                    vals.len(),
                    pop(fields_cone),
                    lives.len(),
                    pop(cones[o.live as usize]),
                    oks.len(),
                    pop(cones[o.ok as usize]),
                );
            }

            // WHERE the loop's nodes sit, which is what decides its
            // runtime: a node whose split cone's highest bit is `d`
            // lives inside loops 0..=d and is therefore executed up to
            // 2^(d+1) times. Same `level` the emitter places by
            // (`transpile::lower`), so this histogram is literally the
            // shape of the emitted loop nest.
            let live_loop = crate::transpile::bdd::reachable(&loop_arena, &loop_roots);
            let scone = loop_arena.split_cones();
            let mut per_level = vec![0usize; forks as usize + 1];
            for id in 0..loop_arena.len() as NodeId {
                if !live_loop[id as usize] {
                    continue;
                }
                let m = scone[id as usize];
                let lvl = if m == 0 {
                    0
                } else {
                    (crate::transpile::graph::ChoiceSet::BITS - m.leading_zeros()) as usize
                };
                per_level[lvl.min(forks as usize)] += 1;
            }
            // The loop's WORK, with no pruning: each level's nodes run
            // once per configuration of the forks below them. This is an
            // upper bound - `if valid == 0 { continue }` skips
            // configurations no lane reaches - so a flat form cheaper
            // than this is not yet proof, but a flat form cheaper than
            // the loop's SIZE times a handful is a strong hint.
            let loop_work: u64 = per_level
                .iter()
                .enumerate()
                .map(|(l, n)| *n as u64 * (1u64 << l))
                .sum();
            eprintln!(
                "[fork]   shape {} levels {:?}; loop work <= {} node-evals, \
                 flat work = {} ({:.2}x cheaper at most)",
                si,
                per_level,
                loop_work,
                b,
                loop_work as f64 / b.max(1) as f64,
            );
            // The two are equal in WORK - the loop evaluates its tail
            // up to 2^forks times, specialization evaluates 2^forks
            // copies once each - so the ratio is pure code size, and
            // anything near 2^forks means the configurations share
            // nothing and the trade is all cost.
            eprintln!(
                "[fork] shape {}: {} forks, traced graph {} nodes; \
                 emitted live nodes {} (loop) -> {} (specialized), {:.2}x \
                 of a maximum {:.2}x, so {:.1}% shared.",
                si,
                forks,
                g.len(),
                a,
                b,
                b as f64 / a.max(1) as f64,
                ns as f64,
                100.0 * (1.0 - b as f64 / (ns as f64 * a.max(1) as f64)),
            );
        }
    }

}

// ---------------------------------------------------------------------------
// Specialization probe (plans/specialize.md). NOT production: it re-traces one
// shape with the player position, the springs, and a pm1 key pinned, and
// reports how far the graph collapses. Driven by `transpile --spec-probe`.
// ---------------------------------------------------------------------------

/// Pin the player XY, the springs' XY, and a pm1 key on one shape, re-trace,
/// and report the node/fork/body collapse against the unpinned base.
pub fn specialize_probe(
    root: &std::path::Path,
    shape_idx: usize,
    player_xy: (i16, i16),
    spring_xy: &[(i16, i16)],
) -> Result<String> {
    use super::domain::Symbolic;
    use super::interp::Interp;
    use super::iface::{self, Conc, Step};
    use super::verify::{run_one, trace_frame};
    use super::heap::Value;
    use super::{cart, shapes};
    use anyhow::{anyhow, bail};
    use celeste_core::pico8_num::Pico8Num as P8;

    let src = cart::sources_in(root)?;
    let top = full_moon::parse(&src).map_err(|e| anyhow!("parse: {:?}", e))?;
    let init = full_moon::parse("_init()").map_err(|e| anyhow!("parse _init: {:?}", e))?;
    let reset = full_moon::parse("__reset_button_states()").map_err(|e| anyhow!("reset: {:?}", e))?;
    let fr = full_moon::parse(cart::FRAME_CODE).map_err(|e| anyhow!("frame: {:?}", e))?;

    let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
    let cart_data = std::sync::Arc::new(celeste_core::cart_data::CartData::load(root.join("cart"))?);
    let (rx, ry) = celeste_interp::game_runner::start_room();
    let cache = std::sync::Arc::new(celeste_core::collision_cache::CollisionCache::new(&cart_data, rx, ry)?);
    it.cache = Some(cache.clone());
    it.cart = Some(cart_data.clone());

    let st0 = cart::fresh_state::<Symbolic>(&mut it.d);
    let st0 = run_one(&mut it, &top, st0)?;
    let mut st0 = st0;
    cart::inject_tile_flag_at(&mut st0);
    let st0 = run_one(&mut it, &init, st0)?;
    let w = shapes::walk(&mut it, &reset, &fr, st0, 400, shapes::WalkOpts::LEVEL0)?;

    if shape_idx >= w.shapes.len() {
        bail!("shape {} out of range ({} shapes)", shape_idx, w.shapes.len());
    }
    let st = w.shapes[shape_idx].state.clone();
    let base_forks = w.shapes[shape_idx].frame.forks;

    // Locate the player and the spring objects by type.
    let objects_of = |st: &super::state::State<Symbolic>, name: &str| -> Vec<Vec<Step>> {
        let mut out = Vec::new();
        let Some(Value::Table(want)) = iface::get(st, &[iface::key(name)]) else { return out };
        let Some(Value::Table(objects)) = iface::get(st, &[iface::key("objects")]) else { return out };
        let n = st.heap.tables[&objects].arr.len();
        for i in 0..n {
            let base = vec![iface::key("objects"), Step::Idx(i)];
            let mut ty = base.clone();
            ty.push(iface::key("type"));
            if iface::get(st, &ty) == Some(Value::Table(want)) { out.push(base); }
        }
        out
    };
    // Object-identity check: for each spring, does its `collide`
    // closure's captured `obj` point to the same table as the live
    // spring? (CELESTE_SPEC_OBJID)
    if std::env::var("CELESTE_SPEC_OBJID").is_ok() {
        let springs_ck = objects_of(&st, "spring");
        for (i, sp) in springs_ck.iter().enumerate() {
            let live_tid = match iface::get(&st, sp) { Some(Value::Table(t)) => t, _ => { eprintln!("spring {} not a table", i); continue } };
            // the collide closure
            let mut cp = sp.clone(); cp.push(iface::key("collide"));
            let cl = match iface::get(&st, &cp) { Some(Value::Func(c)) => c, other => { eprintln!("spring {} collide = {:?}", i, other); continue } };
            let env = st.heap.closures.get(&cl).map(|c| c.env);
            let cap_obj = env.and_then(|e| st.heap.scopes.get(&e)).and_then(|sc| sc.vars.get("obj")).cloned();
            let cap_tid = match cap_obj { Some(Value::Table(t)) => Some(t), _ => None };
            eprintln!("spring {}: live table {}, collide.env captured obj = {:?} (match: {})",
                i, live_tid, cap_tid, cap_tid == Some(live_tid));
            // also print the captured obj's x vs live x
            if let Some(ct) = cap_tid {
                let cap_x = st.heap.tables.get(&ct).and_then(|t| t.hash.get("x")).cloned();
                let live_x = st.heap.tables.get(&live_tid).and_then(|t| t.hash.get("x")).cloned();
                eprintln!("    captured obj.x node = {:?}, live obj.x node = {:?}", cap_x.map(|v| format!("{:?}", v)), live_x.map(|v| format!("{:?}", v)));
            }
        }
    }
    let players = objects_of(&st, "player");
    let springs = objects_of(&st, "spring");
    let fld = |base: &[Step], f: &[&str]| -> Vec<Step> {
        let mut p = base.to_vec();
        for x in f { p.push(iface::key(x)); }
        p
    };
    let num = |v: i16| Conc::Num(P8::from_i16(v));

    let mut pin: Vec<(Vec<Step>, Conc)> = Vec::new();
    // The player position.
    let nopos = std::env::var("CELESTE_SPEC_NOPOS").is_ok();
    if let Some(pl) = players.first() {
        if !nopos { pin.push((fld(pl, &["x"]), num(player_xy.0))); pin.push((fld(pl, &["y"]), num(player_xy.1))); }
        // pm1 key on the player, canonical "steady" values.
        pin.push((fld(pl, &["dash_time"]), num(0)));
        pin.push((fld(pl, &["p_dash"]), Conc::Bool(false)));
        pin.push((fld(pl, &["p_jump"]), Conc::Bool(false)));
    }
    // Optional extra pin groups, so the graph's fork sources can be
    // isolated without recompiling. CELESTE_SPEC_GROUPS is a comma list:
    //   spd        - pin the player spd.x/spd.y to 0
    //   springall  - pin every spring scalar (freeze the springs)
    //   playerall  - pin every player scalar except rem (the fork input)
    let groups: std::collections::HashSet<String> = std::env::var("CELESTE_SPEC_GROUPS")
        .unwrap_or_default().split(',').map(|s| s.trim().to_string()).collect();
    for (i, sp) in springs.iter().enumerate() {
        if let Some((x, y)) = spring_xy.get(i) {
            pin.push((fld(sp, &["x"]), num(*x)));
            pin.push((fld(sp, &["y"]), num(*y)));
        }
        // CELESTE_SPEC_SFIELDS: dotted spring field paths to pin, e.g.
        // "spr,hide_for,hide_in,delay". spr defaults to 18 (active).
        for spec in std::env::var("CELESTE_SPEC_SFIELDS").unwrap_or_default().split(',') {
            let spec = spec.trim();
            if spec.is_empty() { continue; }
            let parts: Vec<&str> = spec.split('.').collect();
            let path = fld(sp, &parts);
            let v = if spec == "spr" { num(18) } else { num(0) };
            if super::shapes::state_paths(&st).unwrap_or_default().iter().any(|r| r == &path) && !pin.iter().any(|(q,_)| q==&path) {
                match iface::get(&st, &path) {
                    Some(Value::Bool(_)) => pin.push((path, Conc::Bool(false))),
                    Some(Value::Num(_)) => pin.push((path, v)),
                    _ => {}
                }
            }
        }
        if groups.contains("springall") {
            for f in super::shapes::state_paths(&st).unwrap_or_default() {
                if f.starts_with(sp) && !pin.iter().any(|(q, _)| q == &f) {
                    match iface::get(&st, &f) {
                        Some(Value::Bool(_)) => pin.push((f, Conc::Bool(false))),
                        Some(Value::Num(_)) => pin.push((f, num(0))),
                        _ => {}
                    }
                }
            }
        }
    }
    if let Some(pl) = players.first() {
        if groups.contains("spd") {
            pin.push((fld(pl, &["spd", "x"]), num(0)));
            pin.push((fld(pl, &["spd", "y"]), num(0)));
        }
        // CELESTE_SPEC_PFIELDS: dotted player field paths to pin to 0,
        // e.g. "spd.x,spd.y,djump,grace,dash_effect_time".
        for spec in std::env::var("CELESTE_SPEC_PFIELDS").unwrap_or_default().split(',') {
            let mut spec = spec.trim();
            if spec.is_empty() { continue; }
            // trailing "=1" pins a bool to true.
            let want_true = spec.ends_with("=1");
            if want_true { spec = &spec[..spec.len()-2]; }
            let mut numval = 0i16;
            if let Some((f, v)) = spec.split_once(':') { spec = f; numval = v.parse().unwrap_or(0); }
            let parts: Vec<&str> = spec.split('.').collect();
            let path = fld(pl, &parts);
            if super::shapes::state_paths(&st).unwrap_or_default().iter().any(|r| r == &path) && !pin.iter().any(|(q,_)| q==&path) {
                match iface::get(&st, &path) {
                    Some(Value::Bool(_)) => pin.push((path, Conc::Bool(want_true))),
                    Some(Value::Num(_)) => pin.push((path, num(numval))),
                    _ => {}
                }
            }
        }
        if groups.contains("playerall") {
            for f in super::shapes::state_paths(&st).unwrap_or_default() {
                let is_rem = f.starts_with(pl) && f.iter().any(|s| format!("{:?}", s).contains("rem"));
                if f.starts_with(pl) && !is_rem && !pin.iter().any(|(q, _)| q == &f) {
                    match iface::get(&st, &f) {
                        Some(Value::Bool(_)) => pin.push((f, Conc::Bool(false))),
                        Some(Value::Num(_)) => pin.push((f, num(0))),
                        _ => {}
                    }
                }
            }
        }
    }
    // CELESTE_SPEC_ALLGEOM: pin EVERY object's x,y and hitbox to
    // constants (spread positions, default hitbox) - tests whether
    // baking in the static object geometry folds the collision graph.
    if std::env::var("CELESTE_SPEC_ALLGEOM").is_ok() {
        let all: Vec<Vec<Step>> = {
            let mut v = Vec::new();
            if let Some(Value::Table(objs)) = iface::get(&st, &[iface::key("objects")]) {
                let n = st.heap.tables[&objs].arr.len();
                for i in 0..n { v.push(vec![iface::key("objects"), Step::Idx(i)]); }
            }
            v
        };
        let player_bases: Vec<Vec<Step>> = players.clone();
        for (i, ob) in all.iter().enumerate() {
            if player_bases.iter().any(|pb| pb == ob) { continue; }
            for (f, v) in [("x", (i as i16) * 12 + 4), ("y", 40)] {
                let path = fld(ob, &[f]);
                if super::shapes::state_paths(&st).unwrap_or_default().iter().any(|r| r==&path) && !pin.iter().any(|(q,_)| q==&path) {
                    pin.push((path, num(v)));
                }
            }
            // hitboxes are pinned by CELESTE_SPEC_HITBOX, not here.
        }
    }
    // CELESTE_SPEC_HITBOX: pin player hitbox (1,3,6,5) and spring
    // hitbox (0,0,8,8) - the type-fixed values - to test whether the
    // symbolic hitbox is what blocks collide from folding.
    if std::env::var("CELESTE_SPEC_HITBOX").is_ok() {
        if let Some(pl) = players.first() {
            for (f, v) in [("x", 1), ("y", 3), ("w", 6), ("h", 5)] {
                pin.push((fld(pl, &["hitbox", f]), num(v)));
            }
        }
        for sp in &springs {
            for (f, v) in [("x", 0), ("y", 0), ("w", 8), ("h", 8)] {
                pin.push((fld(sp, &["hitbox", f]), num(v)));
            }
        }
    }
    // pm1 globals.
    pin.push((vec![iface::key("freeze")], num(0)));
    pin.push((vec![iface::key("has_dashed")], Conc::Bool(false)));

    // Only keep pins whose path is actually a scalar in this state.
    let roots = shapes::state_paths(&st)?;
    let ival = shapes::ival_paths(&st);
    pin.retain(|(p, _)| roots.iter().any(|r| r == p));

    let pinned_paths: Vec<String> = pin.iter().map(|(p, _)| iface::show(p)).collect();

    // Measure one traced frame: reachable node count, distinct LIVE
    // forks (Op::Split still referenced), and an op census.
    fn measure(g: &crate::transpile::graph::Graph, f: &super::verify::Frame) -> (usize, std::collections::BTreeSet<u8>, Vec<String>) {
        use crate::transpile::graph::Op;
        let mut roots_n: Vec<crate::transpile::graph::NodeId> = Vec::new();
        for o in &f.outs {
            for (_, nd, _) in &o.fields { roots_n.push(*nd); }
            roots_n.push(o.guard);
            roots_n.push(o.ok);
        }
        let reach = crate::transpile::bdd::reachable(g, &roots_n);
        let nodes = reach.iter().filter(|b| **b).count();
        let mut forks = std::collections::BTreeSet::new();
        let mut census: std::collections::BTreeMap<String, usize> = Default::default();
        for id in 0..g.len() {
            if reach[id] {
                let op = &g.get(id as u32).op;
                if let Op::Split(d) | Op::SplitValid(d) = op { forks.insert(*d); }
                *census.entry(format!("{:?}", op).split('(').next().unwrap().to_string()).or_default() += 1;
            }
        }
        let mut v: Vec<_> = census.into_iter().collect();
        v.sort_by_key(|(_, n)| std::cmp::Reverse(*n));
        let top = v.into_iter().take(12).map(|(k, n)| format!("{} {}", k, n)).collect();
        (nodes, forks, top)
    }

    // Base: same shape, NO pins.
    let base = trace_frame(&mut it, &reset, &fr, st.clone(), &roots, &[], &ival, true)
        .map_err(|e| anyhow!("base trace of shape {}: {:#}", shape_idx, e))?;
    let (bn, bf, _) = measure(&it.d.graph, &base);

    // Pinned.
    let f = trace_frame(&mut it, &reset, &fr, st, &roots, &pin, &ival, true)
        .map_err(|e| anyhow!("pinned trace of shape {}: {:#}", shape_idx, e))?;
    let (pn, pf, ptop) = measure(&it.d.graph, &f);

    // Slot -> path, so Cell(i) in the dump can be read.
    if std::env::var("CELESTE_SPEC_SLOTS").is_ok() {
        for (i, sp) in f.iface.slots.iter().enumerate() {
            eprintln!("  slot {} = {}", i, iface::show(sp));
        }
    }
    let mut out = String::new();
    out.push_str(&format!("shape {}: {} outcomes, {} players, {} springs\n", shape_idx, f.outs.len(), players.len(), springs.len()));
    out.push_str(&format!("  pinned: {}\n", pinned_paths.join(", ")));
    out.push_str(&format!("  BASE   : {} nodes, {} live forks {:?}, counter {}\n", bn, bf.len(), bf, base_forks));
    out.push_str(&format!("  PINNED : {} nodes, {} live forks {:?}, counter {}\n", pn, pf.len(), pf, f.forks));
    out.push_str(&format!("  pinned op census: {}\n", ptop.join(", ")));
    // CELESTE_SPEC_CMPS: dump symbolic comparison nodes (box tests etc).
    if std::env::var("CELESTE_SPEC_CMPS").is_ok() {
        use crate::transpile::graph::Op;
        let g = &it.d.graph;
        let mut roots_n: Vec<crate::transpile::graph::NodeId> = Vec::new();
        for o in &f.outs { for (_, nd, _) in &o.fields { roots_n.push(*nd); } roots_n.push(o.guard); roots_n.push(o.ok); }
        let reach = crate::transpile::bdd::reachable(g, &roots_n);
        let mut seen = std::collections::BTreeSet::new();
        for id in 0..g.len() {
            if reach[id] {
                if matches!(g.get(id as u32).op, Op::Gt|Op::Ge|Op::Lt|Op::Le) {
                    let t = super::emit::show_tree(g, id as u32, 4);
                    if seen.insert(t.clone()) { out.push_str(&format!("  cmp: {}\n", t)); }
                }
            }
        }
    }
    // CELESTE_SPEC_BTNFORKS: for each of the 64 button assignments,
    // resolve the buttons (specialize_config_into) and count how many
    // forks remain LIVE. If per-button the count is ~2 (rem x,y), the
    // emitter's 2^fork_depth enumeration is over-counting per config.
    if std::env::var("CELESTE_SPEC_BTNFORKS").is_ok() {
        use crate::transpile::graph::{Graph, Op};
        let g = &it.d.graph;
        let mut roots_n: Vec<crate::transpile::graph::NodeId> = Vec::new();
        for o in &f.outs { for (_, nd, _) in &o.fields { roots_n.push(*nd); } roots_n.push(o.guard); roots_n.push(o.ok); }
        let mut hist: std::collections::BTreeMap<usize, usize> = Default::default();
        for m in 0u8..64 {
            let mut sp = Graph::new();
            let mapped = g.specialize_config_into(m, None, &mut sp);
            let sroots: Vec<_> = roots_n.iter().map(|r| mapped[*r as usize]).collect();
            let reach = crate::transpile::bdd::reachable(&sp, &sroots);
            let mut forks = std::collections::BTreeSet::new();
            for id in 0..sp.len() {
                if reach[id] { if let Op::Split(d) = sp.get(id as u32).op { forks.insert(d); } }
            }
            *hist.entry(forks.len()).or_default() += 1;
        }
        out.push_str(&format!("  live forks per button assignment (fork_depth {}): {:?}\n", f.forks, hist));
    }
    // What each LIVE fork forks on.
    {
        use crate::transpile::graph::Op;
        let g = &it.d.graph;
        let mut roots_n: Vec<crate::transpile::graph::NodeId> = Vec::new();
        for o in &f.outs { for (_, nd, _) in &o.fields { roots_n.push(*nd); } roots_n.push(o.guard); roots_n.push(o.ok); }
        let reach = crate::transpile::bdd::reachable(g, &roots_n);
        for id in 0..g.len() {
            if reach[id] {
                if let Op::Split(d) = g.get(id as u32).op {
                    let operand = g.get(id as u32).args[0];
                    out.push_str(&format!("  fork {}: {}\n", d, super::emit::show_tree(g, operand, 8)));
                }
            }
        }
    }
    Ok(out)
}

/// The per-shape constant lattice by FIXPOINT (plans/specialize.md D1).
///
/// Seeds from the concrete spawn state (all fields constant) and traces
/// forward keeping each shape's known-constant fields CONCRETE (pinned)
/// rather than abstracting them - so a field the frame never changes
/// (spring `spd`, static positions) stays a constant and folds the
/// collisions that depend on it. On reaching a shape, the outcome's
/// constant fields are INTERSECTED into that shape's lattice; a shape
/// whose lattice shrinks is re-processed. Monotone (fields only go
/// constant->abstract), so it terminates.
///
/// Returns `(shape key -> constant field map)` plus a blanked
/// representative state per shape for later re-tracing.
pub fn room_constant_lattice(root: &std::path::Path) -> Result<LatticeWalk> {
    use super::domain::Symbolic;
    use super::interp::Interp;
    use super::verify::{run_one, trace_frame};
    use super::domain::Domain;
    use super::{cart, shapes};
    use anyhow::{anyhow, bail};
    type Cmap = std::collections::BTreeMap<super::iface::Path, super::iface::Conc>;

    let src = cart::sources_in(root)?;
    let top = full_moon::parse(&src).map_err(|e| anyhow!("parse: {:?}", e))?;
    let init = full_moon::parse("_init()").map_err(|e| anyhow!("parse _init: {:?}", e))?;
    let reset = full_moon::parse("__reset_button_states()").map_err(|e| anyhow!("reset: {:?}", e))?;
    let fr = full_moon::parse(cart::FRAME_CODE).map_err(|e| anyhow!("frame: {:?}", e))?;

    let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
    let cart_data = std::sync::Arc::new(celeste_core::cart_data::CartData::load(root.join("cart"))?);
    let (rx, ry) = celeste_interp::game_runner::start_room();
    let cache = std::sync::Arc::new(celeste_core::collision_cache::CollisionCache::new(&cart_data, rx, ry)?);
    it.cache = Some(cache.clone());
    it.cart = Some(cart_data.clone());

    let st = cart::fresh_state::<Symbolic>(&mut it.d);
    let st = run_one(&mut it, &top, st)?;
    let mut st = st;
    cart::inject_tile_flag_at(&mut st);
    let start = run_one(&mut it, &init, st)?;

    let key = |st: &super::state::State<Symbolic>| -> Result<String> { Ok(format!("{:?}", st.shape()?)) };

    let mut lattice: std::collections::BTreeMap<String, Cmap> = Default::default();
    let mut reps: std::collections::BTreeMap<String, super::state::State<Symbolic>> = Default::default();
    let mut forks: std::collections::BTreeMap<String, usize> = Default::default();
    let mut frames: std::collections::BTreeMap<String, super::verify::Frame> = Default::default();
    let mut forkops: std::collections::BTreeMap<String, Vec<String>> = Default::default();

    let sk = key(&start)?;
    lattice.insert(sk.clone(), shapes::field_constants(&start, &it.d)?);
    reps.insert(sk.clone(), start.clone());
    let mut work: Vec<String> = vec![sk];
    let room0 = shapes::room_of(&start, &it.d);
    let mut guard = 0usize;

    while let Some(k) = work.pop() {
        guard += 1;
        if guard > 20000 { bail!("constant-lattice fixpoint did not converge"); }
        let st = reps[&k].clone();
        let roots = shapes::state_paths(&st)?;
        let ival = shapes::ival_paths(&st);
        // Pin the shape's known constants (only those that are real scalar
        // inputs here), everything else abstract.
        let pin: Vec<(super::iface::Path, super::iface::Conc)> = lattice[&k]
            .iter()
            .filter(|(p, _)| roots.iter().any(|r| r == *p))
            .map(|(p, c)| (p.clone(), *c))
            .collect();
        let f = match trace_frame(&mut it, &reset, &fr, st, &roots, &pin, &ival, true) {
            Ok(f) => f,
            Err(_) => continue,
        };
        // Live forks of THIS (converged-so-far) trace of shape k.
        {
            use crate::transpile::graph::Op;
            let mut rn: Vec<crate::transpile::graph::NodeId> = Vec::new();
            for o in &f.outs { for (_, nd, _) in &o.fields { rn.push(*nd); } rn.push(o.guard); rn.push(o.ok); }
            let reach = crate::transpile::bdd::reachable(&it.d.graph, &rn);
            let mut fs = std::collections::BTreeSet::new();
            for id in 0..it.d.graph.len() { if reach[id] { if let Op::Split(d) = it.d.graph.get(id as u32).op { fs.insert(d); } } }
            forks.insert(k.clone(), fs.len());
            if fs.len() > 2 {
                let mut ops = Vec::new();
                for id in 0..it.d.graph.len() {
                    if reach[id] { if let Op::Split(_) = it.d.graph.get(id as u32).op {
                        let operand = it.d.graph.get(id as u32).args[0];
                        ops.push(super::emit::show_tree(&it.d.graph, operand, 5));
                    } }
                }
                forkops.insert(k.clone(), ops);
            }
        }
        // Keep the converged frame for generation (last trace wins).
        for o in &f.outs {
            if it.d.decide(&o.ok) == Some(false) { continue; }
            if shapes::room_of(&o.st, &it.d) != room0 { continue; }
            let tk = key(&o.st)?;
            let fc = shapes::field_constants(&o.st, &it.d)?;
            let changed = match lattice.get_mut(&tk) {
                None => {
                    lattice.insert(tk.clone(), fc);
                    let mut rep = o.st.clone();
                    shapes::blank(&mut rep, &mut it.d)?;
                    reps.insert(tk.clone(), rep);
                    true
                }
                Some(m) => {
                    let before = m.len();
                    m.retain(|p, v| fc.get(p) == Some(v));
                    m.len() != before
                }
            };
            if changed && !work.contains(&tk) { work.push(tk); }
        }
        frames.insert(k.clone(), f);
    }
    let graph = std::mem::take(&mut it.d.graph);
    Ok(LatticeWalk { lattice, reps, forks, frames, graph, cart: cart_data, cache, forkops })
}

/// The output of `room_constant_lattice`.
pub struct LatticeWalk {
    pub lattice: std::collections::BTreeMap<String, std::collections::BTreeMap<super::iface::Path, super::iface::Conc>>,
    pub reps: std::collections::BTreeMap<String, super::state::State<super::domain::Symbolic>>,
    pub forks: std::collections::BTreeMap<String, usize>,
    pub frames: std::collections::BTreeMap<String, super::verify::Frame>,
    pub graph: crate::transpile::graph::Graph,
    pub cart: std::sync::Arc<celeste_core::cart_data::CartData>,
    pub cache: std::sync::Arc<celeste_core::collision_cache::CollisionCache>,
    pub forkops: std::collections::BTreeMap<String, Vec<String>>,
}

/// Report the constant lattice for `transpile --room-consts`.
pub fn room_constants(root: &std::path::Path) -> Result<String> {
    let LatticeWalk { lattice, forks, mut frames, graph, cart, cache, forkops, .. } = room_constant_lattice(root)?;
    let room = crate::transpile::graph::Room { cart: cart.clone(), cache: cache.clone() };
    // Bind+lower each converged frame to get the emitted size.
    let mut lines_by_shape: std::collections::BTreeMap<String, usize> = Default::default();
    for (k, f) in frames.iter_mut() {
        if let Ok(bound) = super::emit::bind(f, &graph) {
            if let Ok(low) = super::emit::lower_frame(&bound.graph, &bound.inputs, &bound.uni, &bound.outcomes, Some(room.clone()), bound.forks) {
                lines_by_shape.insert(k.clone(), low.body.len());
            }
        }
    }
    let mut out = String::new();
    out.push_str(&format!("{} shapes reached (constant-lattice fixpoint)\n", lattice.len()));
    for (i, (k, cm)) in lattice.iter().enumerate() {
        let spd: Vec<String> = cm.keys().map(super::iface::show)
            .filter(|s| s.contains("objects[") && s.contains(".spd")).collect();
        out.push_str(&format!("shape {}: {} const fields, {} forks, {} body lines\n",
            i, cm.len(), forks.get(k).copied().unwrap_or(999), lines_by_shape.get(k).copied().unwrap_or(0)));
        if let Some(ops) = forkops.get(k) {
            for o in ops.iter().take(4) { out.push_str(&format!("    fork: {}\n", o)); }
        }
        let _ = spd;
    }
    out.push_str(&format!("total body lines (lattice): {}\n", lines_by_shape.values().sum::<usize>()));
    Ok(out)
}
