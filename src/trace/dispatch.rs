//! Which kernel runs a block.
//!
//! One traced room is several kernels - one per heap shape it reaches -
//! and a block arriving at the frame loop has to find its own. The
//! kernels expose a shape-independent surface for exactly this
//! (`trace::kernel`'s `step` and `acc`), so a dispatcher is a table
//! lookup plus the two rules below.
//!
//! ## The key is the SHAPE HASH, not whether `bind` succeeds
//!
//! Binding is by path, and paths do not identify a shape. Room (1,0)
//! reaches a state whose object list is empty, and its kernel's slots -
//! seventeen globals - are a SUBSET of the slots of the shape with a
//! player in it. So that kernel's `bind` succeeds against a block it was
//! not traced for, and then computes a frame that assumed no objects.
//!
//! `Rt2::shape_hash_of` hashes the whole canonical structure, so it
//! separates them. It is also cheap and total: no candidate is run to
//! find out that it was the wrong one.
//!
//! ## A block with no kernel STOPS the run
//!
//! Not a fallback to the interpreter. See CLAUDE.md's never-deopt rule:
//! a missing kernel is a coverage gap, and the useful response is to name
//! the shape and exit so a kernel can be generated for it, not to run the
//! frame a thousand times slower and hide the gap in the wall clock.

use std::collections::HashMap;

use anyhow::{bail, Result};

use celeste_engine::runtime2::Rt2;

/// The kernel surface itself lives in `celeste-engine`, below the
/// emitters, because `compiled::FrameEngine` consumes kernel sets and
/// `trace` produces them - a type naming both would have to sit above
/// both. Re-exported here so the frame loop below reads unchanged.
pub use celeste_engine::traced::Kernel;

pub struct Dispatch {
    by_shape: HashMap<u64, &'static Kernel>,
}

impl Dispatch {
    /// Index a kernel set by shape.
    ///
    /// Two kernels for one shape is a generator bug, not a choice to
    /// make at run time: the frame loop would pick one of them by
    /// whichever the map happened to keep, and the run would depend on a
    /// hash order. So it is refused here, once, rather than tolerated
    /// every frame.
    pub fn new(kernels: &'static [Kernel]) -> Result<Self> {
        Self::new_multi(&[kernels])
    }

    /// One registry over SEVERAL kernel tables - the multi-room sets
    /// (`celeste_kernels::*::SETS`, one table per room). Rooms have
    /// distinct object composition, so their shapes should never
    /// collide; if two rooms' kernels ever hash to one shape, that is a
    /// real generator/hash bug to surface, not a tie to break, and it
    /// is refused exactly like a within-room duplicate.
    pub fn new_multi(sets: &[&'static [Kernel]]) -> Result<Self> {
        let mut by_shape: HashMap<u64, &'static Kernel> = HashMap::new();
        for set in sets {
            for k in set.iter() {
                if let Some(prev) = by_shape.insert(k.shape, k) {
                    bail!(
                        "{} and {} are both for shape {:#x}",
                        prev.name,
                        k.name,
                        k.shape
                    );
                }
            }
        }
        Ok(Dispatch { by_shape })
    }

    /// The kernel for this block, by its canonical shape.
    pub fn find(&self, b: &Rt2) -> Option<&'static Kernel> {
        self.find_by_shape(b.shape_hash_of())
    }

    /// The kernel for a shape hash that has ALREADY been computed.
    ///
    /// `Rt2::shape_hash` is the cached copy `boundary_canonicalize`
    /// wrote, so a block that came off the frontier has it and need not
    /// walk its structure again. `find` above is for a block that has
    /// not been through a boundary - the traced runner's own loop
    /// builds blocks straight out of a kernel accumulator.
    pub fn find_by_shape(&self, shape: u64) -> Option<&'static Kernel> {
        self.by_shape.get(&shape).copied()
    }
}
