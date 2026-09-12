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
//!
//! The forward driver itself is `frame::forward_run` (src/frame.rs), on a
//! `compiled::FrameEngine`.
pub mod checkpoint;
pub mod pos_graph;
