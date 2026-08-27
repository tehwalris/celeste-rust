//! Does the COMPILED traced kernel compute what the graph says?
//!
//! Everything upstream of here checks a representation. The oracle test
//! checks the traced graph against the interpreter. `check-traced-kernel.sh`
//! checks that the rendered kernel is valid Rust. Neither one runs it.
//!
//! This runs it. The same inputs go through two evaluators of the same
//! graph - `trace::eval`, which walks the nodes, and the kernel, which
//! is those nodes turned into Rust and compiled - and every output cell,
//! every liveness bit and every deopt bit has to agree.
//!
//! The inputs do NOT have to be physically reachable game states. Both
//! sides are evaluating one graph, so any assignment of the input cells
//! is a valid comparison, and the ones that are not reachable are the
//! ones a bug is most likely to survive in.

#[path = "kernel.rs"]
pub mod kernel;

/// Room (1,0)'s kernel set - one module per heap shape, plus the table
/// the dispatcher indexes.
///
/// It used to be installed here as generated source, like `kernel.rs`
/// above. It is CHECKED IN now, in
/// `crates/celeste-kernels-room10/src/traced/room10` (the base variant
/// of the multi-room constant-lattice sets, plans/specialize.md; each
/// room is its own crate so touching one room's kernels does not
/// recompile the others), because `compiled::FrameEngine` has to be
/// able to call it - so this crate reads the same artifact the
/// workspace builds (via the `celeste-kernels` aggregator's `traced::
/// room10` re-export) rather than its own copy. The feature-gated
/// room-(2,0) `lattice` module that used to sit beside it is gone: the
/// room-(2,0) set is checked in as `traced::room20` and gated in the
/// main workspace (`room20_lattice_kernels_match_the_interpreter`).
pub use celeste_kernels::traced::room10 as kernels;

#[cfg(test)]
mod tests {
    use super::kernel;
    use celeste_engine::runtime2::AV;
    use celeste_rust::trace::eval;
    use celeste_rust::trace::iface::Conc;
    use celeste_rust::trace::kernel::{input_block, reference_frame_in};

    /// The button bits of an assignment mask.
    fn bits(m: u8) -> [bool; 6] {
        [
            m & 1 != 0,
            m & 2 != 0,
            m & 4 != 0,
            m & 8 != 0,
            m & 16 != 0,
            m & 32 != 0,
        ]
    }

    /// One lane's inputs, with cell `k` bumped by `d` if it is a number.
    ///
    /// Perturbing is the whole point. Checking only at the values the
    /// frame was traced at would pass for a kernel that had folded every
    /// input away into a constant, which is the one failure the design is
    /// most exposed to.
    fn perturb(base: &[Conc], k: usize, d: i16) -> Vec<Conc> {
        let mut v = base.to_vec();
        if let Some(Conc::Num(x)) = v.get(k).copied() {
            v[k] = Conc::Num(x + celeste_core::pico8_num::Pico8Num::from_i16(d));
        } else if let Some(Conc::Bool(b)) = v.get(k).copied() {
            v[k] = Conc::Bool(!b);
        }
        v
    }

    /// Run one 16-lane block through the kernel.
    ///
    /// Returns each outcome's accumulator and, alongside it, the
    /// (assignment, lane) each appended row came from - which is what
    /// lets the caller evaluate the graph at exactly the point that
    /// produced the row.
    fn run_block(
        r: &celeste_rust::trace::kernel::Reference,
        rows: &[Vec<Conc>],
    ) -> (Vec<celeste_engine::Rt2>, Vec<Vec<(u8, usize)>>) {
        let n = rows.len();
        let block = input_block(r, rows).expect("build the input block");
        let (u, slots) = kernel::bind(&block).expect("bind by path");
        let rin = kernel::rows(&block, &slots, 0).expect("gather rows");
        let g = kernel::G { cart: &r.cart, cache: &r.cache };

        let mut accs: Vec<_> = (0..kernel::OUTCOMES)
            .map(|i| kernel::acc(i, r.cart.clone(), r.cache.clone()))
            .collect();
        let mut from: Vec<Vec<(u8, usize)>> = vec![Vec::new(); kernel::OUTCOMES];
        // The kernel skips a row whose values another configuration
        // already wrote, so the harness tracks the same set or its
        // "which (assignment, lane) produced this row" bookkeeping
        // drifts from what was actually appended.
        let mut seen: Vec<celeste_engine::kernel::RowSet> =
            (0..kernel::OUTCOMES).map(|_| celeste_engine::kernel::RowSet::new()).collect();
        seen.iter_mut().for_each(|s| s.next_slice());

        // The kernel calls this once per (outcome, GROUP), where a group
        // is a set of button assignments that write identical values.
        // `mask` is any member, which is enough to evaluate the graph at
        // the point that produced the row.
        struct H<'a> {
            accs: &'a mut Vec<celeste_engine::Rt2>,
            seen: &'a mut Vec<celeste_engine::kernel::RowSet>,
            from: &'a mut Vec<Vec<(u8, usize)>>,
            n: usize,
        }
        macro_rules! sink_arm {
            ($name:ident, $idx:expr, $sh:ty, $ko:ty, $app:path) => {
                fn $name(&mut self, mask: u8, take: u16, sh: &$sh, v: &$ko) {
                    // `append` REPORTS the lanes it wrote. It skips some
                    // of `take` as duplicates, and which ones cannot be
                    // inferred - assuming the first N bits of `take`
                    // were written is wrong and produced a spurious
                    // mismatch.
                    // `&[]` for the origin metadata: the harness checks
                    // an untagged frame, and `append` treats an empty
                    // slice as "no origins tracked" (the same contract
                    // the engine uses for untagged blocks).
                    let wrote = $app(
                        &mut self.accs[$idx],
                        sh,
                        v,
                        take,
                        self.n,
                        &mut self.seen[$idx],
                        &[],
                    );
                    for lane in 0..16 {
                        if wrote & (1 << lane) != 0 {
                            self.from[$idx].push((mask, lane));
                        }
                    }
                }
            };
        }
        impl<'a> kernel::Sink for H<'a> {
            sink_arm!(o0, 0, kernel::KShared0, kernel::KOut0, kernel::append0);
            sink_arm!(o1, 1, kernel::KShared1, kernel::KOut1, kernel::append1);
            sink_arm!(o2, 2, kernel::KShared2, kernel::KOut2, kernel::append2);
            sink_arm!(o3, 3, kernel::KShared3, kernel::KOut3, kernel::append3);
        }
        let mut h = H { accs: &mut accs, seen: &mut seen, from: &mut from, n };
        kernel::frame(&u, &rin, &g, &mut h);

        (accs, from)
    }

    /// Sixteen lanes: the traced point, then one perturbed slot each,
    /// every slot displaced by `d`.
    ///
    /// Only PER-LANE slots are perturbed. A block-uniform input is one
    /// value for the whole block by definition - the kernel takes it as
    /// a `P8` and refuses a lane array - so varying one across lanes
    /// would not be a harder test, it would be an invalid block.
    fn varying_slots(r: &celeste_rust::trace::kernel::Reference, n: usize) -> Vec<usize> {
        let uni: std::collections::BTreeSet<u32> =
            r.bound.uni.iter().map(|(c, _)| *c).collect();
        (0..n).filter(|i| !uni.contains(&r.frame.in_cells[*i])).collect()
    }

    /// One lane: the base inputs with each named slot displaced.
    fn lane(base: &[Conc], recipe: &[(usize, i16)]) -> Vec<Conc> {
        let mut v = base.to_vec();
        for (slot, d) in recipe {
            v = perturb(&v, *slot, *d);
        }
        v
    }

    /// Pack lane recipes into 16-lane blocks, padding with the base.
    fn blocks(base: &[Conc], recipes: &[Vec<(usize, i16)>]) -> Vec<Vec<Vec<Conc>>> {
        recipes
            .chunks(15)
            .map(|c| {
                let mut rows = vec![base.to_vec()];
                rows.extend(c.iter().map(|r| lane(base, r)));
                while rows.len() < 16 {
                    rows.push(base.to_vec());
                }
                rows
            })
            .collect()
    }

    fn path_of(p: &celeste_rust::trace::iface::Path) -> String {
        celeste_rust::trace::iface::show(p)
    }

    #[test]
    fn the_compiled_kernel_computes_what_the_graph_says() {
        let r = reference_frame_in(std::path::Path::new("..")).expect("trace the reference frame");
        let base = r.frame.iface.init.clone();

        // SEVERAL blocks, at several displacements. One block of +-1
        // jitter put every lane in the SAME outcome, which exercised a
        // quarter of the kernel and looked like a pass. Bigger
        // displacements are what cross a branch, and reaching every
        // outcome is the difference between checking one output shape
        // and four.
        let mut checked = 0usize;
        let mut per_outcome = vec![0usize; kernel::OUTCOMES];
        let slots = varying_slots(&r, base.len());
        let mut recipes: Vec<Vec<(usize, i16)>> = Vec::new();
        // EVERY varying slot, not the first fifteen. The slots are in
        // path order, so cycling `k % len` perturbed `collideable`
        // through `flip.y` and never `x`, `y` or `spd` - the ones a
        // shape-changing branch reads. That is why the first hundred
        // thousand agreeing values were all one outcome.
        for d in [1i16, -1, 4, -4, 16, -16, 60, -60, 120, -120] {
            recipes.extend(slots.iter().map(|s| vec![(*s, d)]));
        }
        // PAIRS, because some outcomes are behind a conjunction that no
        // single displacement can satisfy. Outcome 0 needs `will_restart`
        // set AND `delay_restart` at exactly 1; one slot at a time gets
        // neither of those without losing the other.
        for d in [1i16, -1, 16, -16] {
            for (a, x) in slots.iter().enumerate() {
                for y in slots.iter().skip(a + 1) {
                    recipes.push(vec![(*x, d), (*y, d)]);
                }
            }
        }
        for rows in &blocks(&base, &recipes) {
            let (accs, from) = run_block(&r, rows);
            for i in 0..kernel::OUTCOMES {
                let out = &r.frame.outs[i];
                assert_eq!(
                    accs[i].width,
                    from[i].len(),
                    "outcome {}: appended {} rows but claimed {}",
                    i,
                    accs[i].width,
                    from[i].len()
                );
                per_outcome[i] += from[i].len();
                for (row, (mask, lane)) in from[i].iter().copied().enumerate() {
                    let b = bits(mask);
                    let env = eval::Env {
                        cells: &rows[lane],
                        frees: &b,
                        cart: Some(r.cart.clone()),
                        cache: Some(r.cache.clone()),
                    };
                    for (j, (path, node, _)) in out.fields.iter().enumerate() {
                        let want = eval::eval(&r.graph, *node, &env).unwrap_or_else(|e| {
                            panic!(
                                "outcome {} {}: the graph would not evaluate: {:#}",
                                i, path_of(path), e
                            )
                        });
                        let cell = out.cells[j] as usize;
                        let got = accs[i].cols[cell].at(row);
                        let agree = match (want, got) {
                            (Conc::Num(a), AV::Num(b)) => a == b,
                            (Conc::Bool(a), AV::Bool(b)) => a == b,
                            _ => false,
                        };
                        assert!(
                            agree,
                            "outcome {} {} (cell {}), assignment {:#08b} lane {}: \
                             graph says {:?}, kernel says {:?}",
                            i, path_of(path), cell, mask, lane, want, got
                        );
                        checked += 1;
                    }
                }
            }
        }
        eprintln!(
            "[run] rows per outcome {:?}; {} (row, cell) values agree with the graph",
            per_outcome, checked
        );
        // Say which outcomes were never reached, rather than passing
        // quietly having exercised a quarter of the kernel.
        let missed: Vec<usize> =
            (0..kernel::OUTCOMES).filter(|i| per_outcome[*i] == 0).collect();
        assert!(missed.is_empty(), "no input reached outcome(s) {:?}", missed);
        assert!(checked > 10_000, "only {} values checked", checked);
    }
}
