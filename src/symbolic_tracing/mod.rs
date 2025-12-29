//! Symbolic tracing: execute concrete states while tracking paths,
//! cache traces for reuse, systematically enumerate all paths.

mod path_counter;
mod tracer;
mod cache;
mod runner;

pub use path_counter::PathCounter;
pub use tracer::{TracingInterpreter, TracingResult};
pub use cache::{TraceCache, CachedTrace, CacheStats};
pub use runner::{run_traced, run_frame_traced, RunStats};
