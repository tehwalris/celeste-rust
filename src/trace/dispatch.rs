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
use std::sync::Arc;

use anyhow::{bail, Result};

use celeste_core::cart_data::CartData;
use celeste_core::collision_cache::CollisionCache;
use celeste_engine::runtime2::Rt2;

/// One shape's kernel, behind a shape-independent surface.
///
/// The generated `mod.rs` builds a `&'static [Kernel]` of these. The
/// struct lives here rather than in the generated file so that the frame
/// loop, which is not generated, can be written against it.
pub struct Kernel {
    pub name: &'static str,
    /// The canonical shape this kernel was traced for
    /// (`Rt2::shape_hash_of`).
    pub shape: u64,
    /// How many output shapes one frame can end in.
    pub outcomes: usize,
    /// An empty accumulator with outcome `i`'s shape.
    pub acc: fn(usize, Arc<CartData>, Arc<CollisionCache>) -> Rt2,
    /// `None`: not this kernel's shape. `Some(mask)`: the lanes it
    /// declined, which the doctrine says stops the run.
    pub step: fn(&Rt2, usize, usize, &mut [Rt2]) -> Option<u16>,
    /// Which slot would stop `bind`, or `None` if it would bind.
    ///
    /// `step` returns an `Option` because it is the hot path. Under the
    /// never-deopt doctrine a failure there stops the run, and "did not
    /// bind" is not something anyone can act on - which slot, holding
    /// what, is.
    pub why: fn(&Rt2) -> Option<String>,
}

pub struct Dispatch {
    kernels: &'static [Kernel],
    by_shape: HashMap<u64, usize>,
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
        let mut by_shape = HashMap::new();
        for (i, k) in kernels.iter().enumerate() {
            if let Some(j) = by_shape.insert(k.shape, i) {
                bail!(
                    "{} and {} are both for shape {:#x}",
                    kernels[j].name,
                    k.name,
                    k.shape
                );
            }
        }
        Ok(Dispatch { kernels, by_shape })
    }

    pub fn kernels(&self) -> &'static [Kernel] {
        self.kernels
    }

    /// The kernel for this block, by its canonical shape.
    pub fn find(&self, b: &Rt2) -> Option<&'static Kernel> {
        self.by_shape.get(&b.shape_hash_of()).map(|i| &self.kernels[*i])
    }
}
