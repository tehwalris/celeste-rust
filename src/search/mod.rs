//! The abstract forward search, and the checks that keep it honest.
//!
//! One frame of the search is `compiled::FrameEngine::step` - the generated
//! class kernels where they bind and the interpreter where they do not. This
//! module is everything around that:
//!
//!   * `checkpoint`    - save and resume a run, fingerprinted by the engine
//!                       identity so engines never share one
//!   * `pos_graph`     - the position-transition graph, the backward pass's
//!                       predecessor filter (`plans/strategy.md`)
//!   * `state_mapping` - the `State` <-> lane-block correspondence
//!
//! The forward driver itself is `frame::forward_run` (src/frame.rs), on a
//! `compiled::FrameEngine`.
pub mod checkpoint;
pub mod pos_graph;
pub mod state_mapping;
