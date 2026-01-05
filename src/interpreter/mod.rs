pub mod block_coalesce;
pub mod builtin_resolution;
pub mod call_resolution;
pub mod cfg_analysis;
pub mod cfg_validation;
mod core_interpreter;
pub mod dce;
pub mod deopt_unsafe_builtins;
pub mod heap_elimination;
pub mod fixed_env;
pub mod inlining;
pub mod mem2reg;
mod flow;
pub mod glue;
pub mod heap;
pub mod input_capture;
pub mod inspect;
pub mod local_env;
mod op;
pub mod profiling;
pub mod state;
pub mod tracing;
pub mod value;
pub mod vectorize;

#[cfg(test)]
mod heap_elim_test;
