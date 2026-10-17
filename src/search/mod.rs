//! The search around the forward (`frame::ForwardState`, `storage`):
//! `driver` (its levels, each level's arc phase `solve`, the outcome),
//! `backward` (W over the recorded edges, the optimum), `reach` (remainders
//! pushed forward inside W, and exact), `concrete` (the concrete search, one
//! engine or a pair), `dual`, `known` (a known solution against every
//! pruning step), `arcs` (remainder sets as arcs of the circle), `arc_edges`
//! (the transfers), `hull`, `rem` (+ `rem_f64`), `level_minus_one` (the
//! cost-to-go filter), `pos_graph`, `result` (`result.json`), `robust`,
//! `inspect`, `tree`, `ui_export`; checks of a tree: `arc_check`,
//! `hull_replay`.
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
