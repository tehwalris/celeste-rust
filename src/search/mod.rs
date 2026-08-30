//! The abstract forward search, and the checks that keep it honest.
//!
//! One frame of the search is `compiled::FrameEngine::step` - the generated
//! class kernels where they bind and the interpreter where they do not. This
//! module is everything around that:
//!
//!   * `checkpoint`    - batch (de)serialization of boundary states (header +
//!                       zstd) for the sharded frontier and the reference gates
//!   * `pos_graph`     - the position-transition graph, the backward pass's
//!                       predecessor filter (`plans/strategy.md`)
//!
//! The forward driver itself is `frame::forward_run` (src/frame.rs), on a
//! `compiled::FrameEngine`.
pub mod checkpoint;
pub mod pos_graph;
