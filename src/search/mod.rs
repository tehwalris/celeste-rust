//! The search around the forward (`frame::ForwardState`, `storage`); the
//! modules: plans/architecture.md "Code layout".
#![warn(unreachable_pub)]
pub mod arc_check;
pub(crate) mod arc_edges;
pub(crate) mod arcs;
pub mod backward;
pub(crate) mod concrete;
pub(crate) mod driver;
pub(crate) mod dual;
pub(crate) mod hull;
pub mod hull_replay;
pub mod inspect;
pub(crate) mod known;
pub mod level_minus_one;
pub mod pos_graph;
pub(crate) mod reach;
pub mod rem;
pub(crate) mod rem_f64;
pub mod result;
pub mod robust;
pub mod tree;
pub mod ui_export;

pub use driver::{check_known, run, SearchSpec};
