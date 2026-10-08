//! The search around the frame step (`frame::forward_frame`):
//!
//!   * `checkpoint` - frame checkpoint files: one per (frame, shape piece),
//!                    raw fixed-width columns with a per-cell run index
//!   * `door`       - the forward's dedup set of every reached state
//!   * `edges`      - the recorded backward graph and the remainder-free BFS
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
pub mod door;
pub mod edges;
pub mod inspect;
pub mod known;
pub mod pos_graph;
pub mod ui_export;
