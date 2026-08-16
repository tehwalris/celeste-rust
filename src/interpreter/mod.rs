mod core_interpreter;
pub mod abstraction;
pub mod deopt_collect;
pub mod field_census;
pub mod fixed_env;
mod flow;
pub mod glue;
pub mod heap;
pub mod inspect;
pub mod local_env;
mod op;
/// Test-only: the shared lock for everything whose result depends on the
/// process-global `PARTITION_STRADDLES` toggle (see its doc in op.rs).
#[cfg(test)]
pub use op::partition_straddles_test_lock;
pub mod profiling;
pub mod row_table;
pub mod visited;
pub mod state;
pub mod tracing;
pub mod value;
pub mod merge_dump;
pub mod virtual_merge;
pub mod would_dedup;
pub mod vectorize;
