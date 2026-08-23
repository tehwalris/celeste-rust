mod core_interpreter;
pub mod abstraction;
pub mod deopt_collect;
pub mod fixed_env;
mod flow;
pub mod glue;
pub mod heap;
pub mod inspect;
pub mod local_env;
mod op;
/// The shared lock for everything whose result depends on the
/// process-global `PARTITION_STRADDLES` toggle (see its doc in op.rs).
///
/// Not `#[cfg(test)]`: the tests that need it are in `celeste-rust`, and
/// `cfg(test)` does not cross a crate boundary - the item would be
/// configured out of the dependency exactly when the dependent's tests
/// want it.
pub use op::{partition_straddles_test_lock, set_partition_straddles};
pub mod row_table;
pub mod visited;
pub mod state;
pub mod value;
pub mod merge_dump;
pub mod virtual_merge;
pub mod vectorize;
