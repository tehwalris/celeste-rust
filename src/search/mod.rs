//! The search around the forward (`frame::ForwardState`, `storage`):
//!
//!   * `driver`      - THE SEARCH's driver: its levels, each level's arc phase
//!                     (`solve`), the outcome (`run`)
//!   * `backward`    - the winning sets W over the recorded edges, the optimum
//!   * `reach`       - remainders pushed forward: inside W (the next level's
//!                     filter), and exact (a check of the hull forward)
//!   * `concrete`    - the concrete search inside W, for one engine or a pair
//!   * `dual`        - the dual search: a run valid in both arithmetics
//!   * `known`       - a known solution checked against every pruning step
//!   * `arcs`        - sets of remainders as arcs of the circle
//!   * `arc_edges`   - per recorded edge, the remainder transfer
//!   * `hull`        - a state's remainder hull (the forward's): one arc per axis
//!   * `rem`         - the remainder model of an arithmetic: hulls and transfers
//!                     (`rem_f64`: the doubles')
//!   * `pos_graph`   - player-position cells and the position graph
//!   * `result`      - `result.json`: what a search decided, as a record
//!   * `robust`      - a float witness against the host libm's `sin`
//!   * `inspect`     - rows read by named fields, for the diagnostics
//!   * `tree`        - a finished tree as the commands that read it open it
//!   * `ui_export`   - `rewrite export-ui`: a finished run -> the web UI's data
//!   * `arc_check`, `hull_replay` - checks of a tree: its transfers against
//!                     the reference engine, its states replayed from its
//!                     recorded edges
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
pub mod pos_graph;
pub mod reach;
pub mod rem;
pub mod rem_f64;
pub mod result;
pub mod robust;
pub mod tree;
pub mod ui_export;

pub use driver::{check_known, run, SearchSpec};
