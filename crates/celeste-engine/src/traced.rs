//! Which kernel runs a block.
//!
//! One traced room is several kernels - one per heap shape it reaches -
//! and a block arriving at a frame loop has to find its own.
//!
//! The TYPE lives here, below the emitters, because two different things
//! produce values of it: `celeste_rust::trace` generates kernel sets,
//! and `celeste_rust::compiled::FrameEngine` consumes them. Anything
//! naming both would otherwise have to be above both.

use std::sync::Arc;

use celeste_core::cart_data::CartData;
use celeste_core::collision_cache::CollisionCache;

use crate::kernel::RowSet;
use crate::runtime2::Rt2;

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
    pub step: fn(&Rt2, usize, usize, &mut [Rt2], &mut [RowSet]) -> Option<u16>,
    /// Which slot would stop `bind`, or `None` if it would bind.
    ///
    /// `step` returns an `Option` because it is the hot path. Under the
    /// never-deopt doctrine a failure there stops the run, and "did not
    /// bind" is not something anyone can act on - which slot, holding
    /// what, is.
    pub why: fn(&Rt2) -> Option<String>,
}

