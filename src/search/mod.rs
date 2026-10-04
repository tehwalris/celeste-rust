//! The abstract forward search, and the checks that keep it honest.
//!
//! One frame of the search is `compiled::FrameEngine::run_bucket` - one
//! shape's block through its assembled kernel. This module is everything
//! around that:
//!
//!   * `checkpoint`    - the frame checkpoint files: one per (frame, shape),
//!                       rows sorted by (cell, key) with a cell index, raw
//!                       fixed-width columns loadable by cell range
//!   * `pos_graph`     - the position-transition graph, the backward pass's
//!                       predecessor filter (`plans/strategy.md`)
//!   * `ui_export`     - `rewrite export-ui`: a finished run's tree + log
//!                       -> the static data the web UI (`ui/`) renders
//!   * `arcs`          - sets of remainders as arcs of the circle (a frame
//!                       rotates them), for the rotation graph
//!   * `arc_edges`     - per recorded edge, what the frame did to the
//!                       player's remainder (`CELESTE_ARC_EDGES=1`)
//!
//! The forward driver itself is `frame::forward_run` (src/frame.rs), on a
//! `compiled::FrameEngine`.
pub mod arc_edges;
pub mod arcs;
pub mod checkpoint;
pub mod door;
pub mod edges;
pub mod pos_graph;
pub mod ui_export;
