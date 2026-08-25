//! The frame loop, over TRACED kernels.
//!
//! One frame is: dispatch each block to its shape's kernel, run every
//! 16-lane slice of it, collect the outcome accumulators, merge the ones
//! that agree on shape, and put the result through the engine's boundary.
//! Repeat.
//!
//! This is deliberately NOT `compiled::FrameEngine`. That engine is a
//! pair of paths and a policy for choosing between them, and the second
//! path is the interpreter - the thing this campaign exists to stop
//! depending on. What is worth carrying over from it is the mechanical
//! part, and that is what is here: 16-lane slices, one accumulator per
//! output shape, merge before the boundary rather than after, and the
//! boundary itself, which is the engine's and is not reimplemented.
//!
//! ## A frame that cannot run stops the run
//!
//! Two ways it happens, and neither is a degraded mode (CLAUDE.md):
//! a block whose shape has no kernel, and a kernel that declines lanes.
//! Both report what they saw - the shape hash, the lane count - and
//! return an error, so the caller checkpoints and exits rather than
//! absorbing the gap at a thousand times the cost.

use std::collections::BTreeMap;
use std::sync::Arc;

use anyhow::{bail, Context, Result};

use celeste_core::cart_data::CartData;
use celeste_core::collision_cache::CollisionCache;
use celeste_engine::runtime2::{BoundaryIds, Rt2};

use super::dispatch::{Dispatch, Kernel};

/// The width one `step` call covers. The kernels report declined lanes
/// as a `u16`, so this is the mask's width and not a tuning parameter.
const SLICE: usize = 16;

/// What one frame did.
pub struct FrameStat {
    pub frame: usize,
    /// Lanes entering the frame, over all blocks.
    pub rows_in: usize,
    /// Lanes surviving the boundary's dedup.
    pub rows_out: usize,
    pub blocks_out: usize,
    /// Rows APPENDED before the boundary deduped them. A kernel emits one
    /// row per (lane, button assignment, fork configuration) and only the
    /// survivors are the answer, so the ratio to `rows_out` is how much
    /// work the dedup is throwing away - and where a pre-dedup would go.
    pub rows_raw: usize,
    /// DISTINCT raw rows, before the boundary widens anything.
    ///
    /// The gap between `rows_raw` and this is what a plain
    /// exact-duplicate filter in `append` would remove; the gap between
    /// this and `rows_out` is what only the boundary's widenings can
    /// merge. Which of the two dominates decides whether a pre-dedup is
    /// a hash set or a whole `KeyPlan`.
    pub rows_distinct: usize,
    /// Where the frame's time went: running the kernels, merging the
    /// per-shape accumulators, and the boundary (widen-check, canonical
    /// renumber, row hashing, dedup).
    ///
    /// Split because the fix for each is different, and the balance
    /// moves: before constant columns were written once, the kernel half
    /// was dominated by per-row pushes.
    pub t_kernel: std::time::Duration,
    pub t_merge: std::time::Duration,
    pub t_boundary: std::time::Duration,
    /// The surviving rows' keys. The row key already carries the shape
    /// hash, so this set is comparable across blocks and across engines -
    /// it is what a run is checked against.
    pub keys: Vec<(u64, u64)>,
}

pub struct Run {
    dispatch: Dispatch,
    ids: BoundaryIds,
    cart: Arc<CartData>,
    cache: Arc<CollisionCache>,
    blocks: Vec<Rt2>,
    frame: usize,
    /// Count distinct raw rows per frame. A DIAGNOSTIC - it hashes every
    /// candidate row, which is the very work a pre-dedup exists to avoid.
    pub census: bool,
}

impl Run {
    pub fn new(
        kernels: &'static [Kernel],
        start: Rt2,
        cart: Arc<CartData>,
        cache: Arc<CollisionCache>,
    ) -> Result<Self> {
        Ok(Run {
            dispatch: Dispatch::new(kernels)?,
            ids: crate::compiled::boundary_ids(),
            cart,
            cache,
            blocks: vec![start],
            frame: 0,
            census: false,
        })
    }

    pub fn blocks(&self) -> &[Rt2] {
        &self.blocks
    }

    /// One frame. Errors are coverage gaps, and the caller is expected to
    /// treat them as a stop.
    pub fn step(&mut self) -> Result<FrameStat> {
        self.frame += 1;
        let rows_in: usize = self.blocks.iter().map(|b| b.width).sum();

        let mut t_kernel = std::time::Duration::ZERO;
        let mut t_merge = std::time::Duration::ZERO;
        let mut t_boundary = std::time::Duration::ZERO;
        let mut produced: Vec<Rt2> = Vec::new();
        for b in std::mem::take(&mut self.blocks) {
            let k = self.dispatch.find(&b).ok_or_else(|| {
                anyhow::anyhow!(
                    "frame {}: no kernel for shape {:#x} ({} lanes, {} cells); \
                     the shape walk did not reach it",
                    self.frame,
                    b.shape_hash_of(),
                    b.width,
                    b.structure.len()
                )
            })?;
            // One row set per outcome, made ONCE per block and reset
            // per slice: the kernel dedups its own output, and
            // allocating its table per slice cost more than the dedup
            // saved (244 MB of allocation a frame).
            let mut seen: Vec<celeste_engine::kernel::RowSet> =
                (0..k.outcomes).map(|_| celeste_engine::kernel::RowSet::new()).collect();
            let mut accs: Vec<Rt2> =
                (0..k.outcomes).map(|i| (k.acc)(i, self.cart.clone(), self.cache.clone())).collect();
            let mut declined = 0usize;
            let t0 = std::time::Instant::now();
            let mut lo = 0;
            while lo < b.width {
                let n = SLICE.min(b.width - lo);
                let mask = (k.step)(&b, lo, n, &mut accs, &mut seen).ok_or_else(|| {
                    anyhow::anyhow!(
                        "frame {}: {} did not bind a block of its own shape {:#x}: {}",
                        self.frame,
                        k.name,
                        k.shape,
                        (k.why)(&b).unwrap_or_else(|| {
                            "every slot resolves - `bind` and `bind_why` disagree".into()
                        })
                    )
                })?;
                declined += mask.count_ones() as usize;
                lo += n;
            }
            if declined > 0 {
                bail!(
                    "frame {}: {} declined {} of {} lanes - a lane whose `ok` \
                     the kernel could not discharge",
                    self.frame,
                    k.name,
                    declined,
                    b.width
                );
            }
            t_kernel += t0.elapsed();
            produced.extend(accs.into_iter().filter(|a| a.width > 0));
        }

        // Merge before the boundary, not after: dedup is exact only over
        // rows in one block, and two lanes that agree came from different
        // outcomes of different blocks as often as not.
        let mut by_shape: BTreeMap<u64, Vec<Rt2>> = BTreeMap::new();
        for b in produced {
            by_shape.entry(b.shape_hash_of()).or_default().push(b);
        }
        let mut keys: Vec<(u64, u64)> = Vec::new();
        let mut rows_out = 0;
        let rows_raw: usize = by_shape.values().flatten().map(|b| b.width).sum();
        let mut rows_distinct = 0usize;
        if self.census {
            let mut seen: std::collections::HashSet<u64> = Default::default();
            for b in by_shape.values().flatten() {
                for lane in 0..b.width {
                    let mut h: u64 = 0;
                    for (c, cell) in b.structure.iter().enumerate() {
                        if !matches!(cell, celeste_engine::runtime2::Cell2::Val) {
                            continue;
                        }
                        h = h.wrapping_add(celeste_engine::runtime2::cell_mix(
                            c as u64,
                            b.cols[c].at(lane),
                            0x9e37_79b9_7f4a_7c15,
                        ));
                    }
                    seen.insert(h);
                }
            }
            rows_distinct = seen.len();
        }
        for (_, group) in by_shape {
            let t1 = std::time::Instant::now();
            let mut b = Rt2::merge_many(group);
            // Representation, not semantics - but two things downstream
            // insist on it. The boundary's timer pin accepts a uniform
            // number or a value column and panics on a per-lane one, and
            // a kernel's block-uniform inputs (the hitboxes) bind only
            // against `Col::U`. A kernel writes a lane vector whether or
            // not the lanes agree, so nothing else would ever collapse
            // one.
            b.collapse_uniform_cols();
            t_merge += t1.elapsed();
            let t2 = std::time::Instant::now();
            b.boundary(&self.ids);
            t_boundary += t2.elapsed();
            rows_out += b.width;
            keys.extend(b.row_keys.iter().copied());
            self.blocks.push(b);
        }
        keys.sort_unstable();

        Ok(FrameStat {
            frame: self.frame,
            rows_in,
            rows_out,
            blocks_out: self.blocks.len(),
            rows_raw,
            rows_distinct,
            t_kernel,
            t_merge,
            t_boundary,
            keys,
        })
    }
}

/// The block a traced room's run starts from: the state after `_init`,
/// with every scalar at its real value.
pub fn start_block(
    root: &std::path::Path,
) -> Result<(Rt2, Arc<CartData>, Arc<CollisionCache>)> {
    use super::domain::Symbolic;
    use super::interp::Interp;
    use super::verify::run_one;
    use super::cart;
    use anyhow::anyhow;

    let src = cart::sources_in(root)?;
    let top = full_moon::parse(&src).map_err(|e| anyhow!("parse: {:?}", e))?;
    let init = full_moon::parse("_init()").map_err(|e| anyhow!("parse _init: {:?}", e))?;

    let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
    let cart_data = Arc::new(CartData::load(root.join("cart"))?);
    let (rx, ry) = celeste_interp::game_runner::start_room();
    let cache = Arc::new(CollisionCache::new(&cart_data, rx, ry)?);
    it.cache = Some(cache.clone());
    it.cart = Some(cart_data.clone());

    let st = cart::fresh_state::<Symbolic>(&mut it.d);
    let mut st = run_one(&mut it, &top, st)?;
    cart::inject_tile_flag_at(&mut st);
    let st = run_one(&mut it, &init, st)?;

    let b = super::bind::concrete_block(&st, &it.d, cart_data.clone(), cache.clone())
        .context("the state after _init is not a concrete block")?;
    Ok((b, cart_data, cache))
}

#[cfg(test)]
mod tests {
    use std::collections::{BTreeMap, BTreeSet};

    use celeste_engine::runtime2::{Cell2, Rt2, AV, NONE};
    use celeste_names::FIELD_NAMES;

    /// Flat map of every SCALAR leaf field of one lane, keyed by dotted
    /// path. Follows pointers (depth-guarded) and records only scalars,
    /// so shared type tables cancel between two blocks and a diff
    /// isolates the field that actually differs. Ported from the
    /// traced-kernel-check room-(2,0) gate, where it was validated
    /// against 7,364 branched states (plans/specialize.md "CORRECTNESS
    /// CONFIRMED").
    fn scalar_fields_lane(b: &Rt2, lane: usize) -> BTreeMap<String, String> {
        fn nm(id: u32) -> String {
            FIELD_NAMES
                .get(id as usize)
                .map(|s| s.to_string())
                .unwrap_or_else(|| format!("#{}", id))
        }
        fn walk(
            b: &Rt2,
            cell: u32,
            path: &str,
            depth: u32,
            lane: usize,
            out: &mut BTreeMap<String, String>,
        ) {
            if cell == NONE || depth > 16 {
                return;
            }
            match &b.structure[cell as usize] {
                Cell2::Val => match b.cols[cell as usize].at(lane) {
                    AV::Ptr(t) => walk(b, t, path, depth + 1, lane, out),
                    AV::NilPtr | AV::Nil => {}
                    scalar => {
                        out.insert(path.to_string(), format!("{:?}", scalar));
                    }
                },
                Cell2::Obj(fields) => {
                    for (fid, c) in fields {
                        walk(b, *c, &format!("{}.{}", path, nm(*fid)), depth + 1, lane, out);
                    }
                }
                Cell2::Arr(items) => {
                    for (i, it) in items.iter().enumerate() {
                        walk(b, *it, &format!("{}[{}]", path, i), depth + 1, lane, out);
                    }
                }
                _ => {}
            }
        }
        let mut out = BTreeMap::new();
        for gi in 0..b.globals.len() {
            let cell = b.globals[gi];
            if cell != NONE {
                walk(b, cell, &nm(gi as u32), 0, lane, &mut out);
            }
        }
        out
    }

    /// The per-room differential for the non-default rooms: run the
    /// checked-in BASE (level-0) lattice set on the kernel-only frame
    /// loop, the interpreter on the room-aware raw program
    /// (`compile_from_disk`), and compare the per-frame REACHABLE state
    /// set (per-lane field-path -> value signatures).
    ///
    /// Why not row keys: the raw compiled program boxes each object's
    /// `this` upvalue in an extra heap cell that the AST tracer does not
    /// produce, so row keys differ by representation while every
    /// reachable scalar matches (plans/specialize.md, the room-2 oracle
    /// investigation). The reachable-state set is boxing- and
    /// lane-order-independent, and it is what Philippe validated the
    /// room-2 lattice against. Room (1,0) has the stronger row-key-set
    /// gates against the frozen DCE'd program
    /// (`traced_kernels_reproduce_the_interpreter` etc.); the frozen
    /// artifacts for rooms (0,0)/(2,0) do not exist (`bin/freeze` is
    /// deleted), so this is the strongest oracle available per room.
    ///
    /// Any `run.step()` error - an uncovered shape or a DECLINED lane
    /// (a lattice constant the runtime disagrees with) - fails the test.
    fn lattice_room_matches_the_interpreter(
        room: &str,
        kernels: &'static [super::Kernel],
        frames: usize,
    ) {
        std::env::set_var("CELESTE_START_ROOM", room);
        for f in ["lua/celeste-minimal.lua", "cart"] {
            if !std::path::Path::new(f).exists() {
                eprintln!("skipping: {} not found (run from the repo root)", f);
                return;
            }
        }
        let root = std::path::Path::new(".");
        let (start, cart, cache) =
            super::start_block(root).expect("start block for the room");
        let mut run = super::Run::new(kernels, start, cart.clone(), cache.clone())
            .expect("index the room's kernels by shape");

        let program = crate::program::Program::compile_from_disk()
            .expect("compile the room-aware reference program");
        let mut oracle =
            crate::search::run::AbstractRun::start(&program).expect("start the oracle");
        let ids = crate::compiled::boundary_ids();

        let sig_of = |b: &Rt2| -> BTreeSet<Vec<(String, String)>> {
            (0..b.width)
                .map(|l| scalar_fields_lane(b, l).into_iter().collect::<Vec<_>>())
                .collect()
        };
        for frame in 1..=frames {
            run.step().unwrap_or_else(|e| {
                panic!(
                    "kernel frame {}: {:#}\n(a declined block or an uncovered shape \
                     means the lattice over-claimed a constant)",
                    frame, e
                )
            });
            oracle.step().unwrap_or_else(|e| panic!("oracle frame {}: {:#}", frame, e));
            let mut lat: BTreeSet<Vec<(String, String)>> = BTreeSet::new();
            for b in run.blocks() {
                lat.extend(sig_of(b));
            }
            let mut inp: BTreeSet<Vec<(String, String)>> = BTreeSet::new();
            for st in oracle.states() {
                if st.vector_size == 0 {
                    continue;
                }
                let mut ib =
                    crate::compiled::bridge::import_block(st, cart.clone(), cache.clone());
                ib.boundary(&ids);
                inp.extend(sig_of(&ib));
            }
            assert_eq!(
                lat,
                inp,
                "frame {}: reachable STATE-SET differs (not just boxing): \
                 {} lattice-only, {} interp-only (lattice {}, interp {})",
                frame,
                lat.difference(&inp).count(),
                inp.difference(&lat).count(),
                lat.len(),
                inp.len()
            );
        }
    }

    #[test]
    fn room00_lattice_kernels_match_the_interpreter() {
        lattice_room_matches_the_interpreter(
            "0,0",
            celeste_kernels::traced::room00::KERNELS,
            30,
        );
    }

    #[test]
    fn room20_lattice_kernels_match_the_interpreter() {
        lattice_room_matches_the_interpreter(
            "2,0",
            celeste_kernels::traced::room20::KERNELS,
            30,
        );
    }
}
