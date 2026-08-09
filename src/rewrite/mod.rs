//! Rewriting the compiled program into a shape that can be vectorized and
//! eventually compiled.
//!
//! See `plans/rewrite-plan.md` for the whole story. In brief:
//!
//! The abstract interpreter spends most of its time not interpreting. At frame
//! 34, branch-induced state filtering is 33% of runtime and re-merging the
//! fragments afterwards is another 42%, against 23% actual interpretation. All
//! of that is caused by the *shape* of the compiled program: data-dependent
//! branches split the state set, and every fragment then re-enters every callee
//! separately.
//!
//! The fix is to rewrite the program so that it has no calls, no intra-frame
//! heap traffic, and no lane-varying branches - at which point a frame is one
//! straight-line pass over one wide vector of lanes, and can be compiled.
//!
//! The architecture is **untrusted search, trusted check**:
//!
//!   * `program`  - the thing being rewritten; always derived, never edited
//!   * `recipe`   - the ordered list of rewrite instructions, checked into git
//!   * `rules`    - each with an `apply` and an independently-written `verify`
//!   * `validate` - structural invariants, including the dominance check the
//!                  previous attempt lacked
//!   * `verify`   - differential execution against the unmodified program
//!
//! Whatever decides *which* rewrites to make is unconstrained and untrusted,
//! because the verifier checks the result.

pub mod checkpoint;
pub mod class_dead;
pub mod liveness;
pub mod pos_graph;
pub mod print;
pub mod program;
pub mod recipe;
pub mod rules;
pub mod slots;
pub mod state_mapping;
pub mod sweep;
pub mod sweep_census;
pub mod validate;
pub mod verify;
