//! The search around the forward (`frame::ForwardState`, `storage`):
//!
//!   * `checkpoint` - frame checkpoint files: one per (frame, shape piece),
//!                    raw fixed-width columns, rows in id order
//!   * `edges`      - the remainder-free BFS over the recorded graph
//!   * `arc_edges`  - per recorded edge, the remainder transfer
//!   * `arcs`       - sets of remainders as arcs of the circle
//!   * `arc_dp`     - THE SEARCH: winning sets, optimum, concrete search
//!   * `known`      - a known solution checked against every pruning step
//!   * `pos_graph`  - player-position cells and the position graph
//!   * `inspect`    - rows read by named fields, for the diagnostics
//!   * `ui_export`  - `rewrite export-ui`: a finished run -> the web UI's data
pub mod arc_dp;
pub mod arc_edges;
pub mod arcs;
pub mod checkpoint;
pub mod edges;
pub mod inspect;
pub mod known;
pub mod pos_graph;
pub mod ui_export;
