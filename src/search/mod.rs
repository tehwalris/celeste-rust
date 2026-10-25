//! The search around the forward (`frame::ForwardState`, `storage`); the
//! modules: plans/architecture.md "Code layout".
#![warn(unreachable_pub)]
pub(crate) mod arcs;
pub mod backward;
pub(crate) mod concrete;
pub(crate) mod driver;
pub(crate) mod dual;
pub(crate) mod hull;
pub mod inspect;
pub(crate) mod known;
pub mod level_minus_one;
pub(crate) mod reach;
pub(crate) mod replay;
pub mod rem;
pub mod result;
pub mod robust;
pub mod tree;

pub use driver::{check_known, run, SearchSpec};
