//! The search around the forward (`frame::ForwardState`, `storage`):
//!
//!   * `arc_edges`   - per recorded edge, the remainder transfer
//!   * `arcs`        - sets of remainders as arcs of the circle
//!   * `hull`        - a state's remainder hull (the forward's): one arc per axis
//!   * `rem`         - the remainder model of an arithmetic: hulls and transfers
//!                     (`rem_f64`: the doubles')
//!   * `driver`      - THE SEARCH's driver: its levels, the outcome (`run`)
//!   * `arc_dp`      - the arc phase: winning sets, optimum, reach, concrete search
//!   * `dual`        - the dual search: a run valid in both arithmetics
//!   * `known`       - a known solution checked against every pruning step
//!   * `checkpoint`  - the tree's format version and small files (marks, pos graph)
//!   * `pos_graph`   - player-position cells and the position graph
//!   * `result`      - `result.json`: what a search decided, as a record
//!   * `robust`      - a float witness against the host libm's `sin`
//!   * `inspect`     - rows read by named fields, for the diagnostics
//!   * `ui_export`   - `rewrite export-ui`: a finished run -> the web UI's data
//!   * `arc_check`, `hull_replay`, `reach` - checks of a tree: its transfers
//!                     against the reference engine, its states replayed from
//!                     its recorded edges, the exact reach its hulls hold
pub mod arc_check;
pub mod arc_dp;
pub mod arc_edges;
pub mod arcs;
pub mod checkpoint;
pub mod driver;
pub mod dual;
pub mod hull;
pub mod hull_replay;
pub mod inspect;
pub mod known;
pub mod pos_graph;
pub mod reach;
pub mod rem;
pub mod rem_f64;
pub mod result;
pub mod robust;
pub mod ui_export;

pub use driver::{check_known, run, SearchSpec};
