//! The abstract forward search, and the checks that keep it honest.
//!
//! One frame of the search is `compiled::FrameEngine::step` - the generated
//! class kernels where they bind and the interpreter where they do not. This
//! module is everything around that:
//!
//!   * `run`          - `AbstractRun`, the forward driver: chunking, dedup,
//!                      merge, band and partition filters, and the canonical
//!                      `StateObservation` two runs are compared in
//!   * `differential` - run two programs side by side and report the first
//!                      frame whose observations differ
//!   * `checkpoint`   - save and resume a run, fingerprinted by the engine
//!                      identity so engines never share one
//!   * `sweep`, `sweep_time`, `pos_graph` - the backward half of
//!                      `plans/strategy.md`
//!   * `state_mapping` - the `State` <-> lane-block correspondence
pub mod checkpoint;
pub mod pos_graph;
pub mod run;
pub mod state_mapping;
pub mod sweep;
pub mod sweep_time;
