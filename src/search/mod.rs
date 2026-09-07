//! The abstract forward search, and the checks that keep it honest.
//!
//! One frame of the search is `compiled::FrameEngine::run_frame_block` - the assembled
//! class kernels where they bind and the interpreter where they do not. This
//! module is everything around that:
//!
//!   * `checkpoint`    - block (de)serialization (header + zstd) for the
//!                       sharded frontier
//!   * `pos_graph`     - the position-transition graph, the backward pass's
//!                       predecessor filter (`plans/strategy.md`)
//!
//! The forward driver itself is `frame::forward_run` (src/frame.rs), on a
//! `compiled::FrameEngine`.
pub mod checkpoint;
pub mod pos_graph;
