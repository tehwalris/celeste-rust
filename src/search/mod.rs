//! The search around the forward (`frame::ForwardState`, `storage`); the
//! modules: plans/architecture.md "Code layout".
pub mod arc_check;
pub mod arc_edges;
pub mod arcs;
pub mod backward;
pub mod concrete;
pub mod driver;
pub mod dual;
pub mod hull;
pub mod hull_replay;
pub mod inspect;
pub mod known;
pub mod level_minus_one;
pub mod pos_graph;
pub mod reach;
pub mod rem;
pub mod rem_f64;
pub mod result;
pub mod robust;
pub mod tree;
pub mod ui_export;

pub use driver::{check_known, run, SearchSpec};
