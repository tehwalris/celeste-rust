//! The frame driver for the reference interpreter (`refdomain::RefDomain`).
//!
//! `Interp<RefDomain>` runs ONE scalar path of a frame; `run_frame_all`
//! enumerates the fork tree depth-first by re-execution (run to a leaf,
//! record the output, `cursor.advance()`, re-run). The outputs are the
//! frame's successor set for one input state. The cursor is the only state
//! carried across reruns: the input is cloned per path, so nothing is
//! snapshotted or backtracked.

use anyhow::{bail, Result};

use full_moon::ast;

use celeste_core::arith::Arithmetic;

use crate::trace::interp::Interp;
use crate::trace::refdomain::{Cursor, RefDomain};
use crate::trace::state::State;

/// Run one frame of `body` from `input` along every fork path; one output state
/// per leaf. For a game frame `body` is
/// `__reset_button_states()\n_update()\n_draw()`, so the buttons are forks too.
///
/// `it` must have the cart's functions registered; its `d.cursor` is the DFS
/// state. A path that REFUSES (e.g. a fork premise a lane cannot satisfy) is
/// an error; a path that RAISES has no successor.
///
/// `unknown` names the input's unknown-boolean fields (`block::from_block`):
/// each is a cursor choice made before the frame runs, as the kernels fork them.
///
/// More than `cap` paths is an error, `TooManyPaths` (every path's output is
/// held: the cap bounds the memory).
pub fn run_frame_all<'a, A: Arithmetic>(
    it: &mut Interp<'a, RefDomain<A>>,
    body: &'a ast::Ast,
    input: &State<RefDomain<A>>,
    unknown: &[(crate::trace::heap::TableId, String)],
    level: crate::abstraction::Level,
    cap: usize,
) -> Result<Vec<State<RefDomain<A>>>> {
    it.d.cursor = Cursor::new();
    let mut outputs = Vec::new();
    let mut paths = 0usize;
    // An unknown fly fruit needs nothing here: its `fly` is an unknown
    // boolean (forked below), `step`/`y` the unknown number and `spd.y`/
    // `rem.y` intervals, whose comparisons the domain forks
    // (`RefDomain::compare`) as the kernels' undecided atoms do. Unknown
    // platforms have no reference form yet.
    if level.platforms {
        anyhow::bail!("the reference engine does not run platforms-unknown levels (`p`, plans/abstractions.md)");
    }
    loop {
        it.d.cursor.reset();
        it.prints.clear();
        let mut st = input.clone();
        for (t, k) in unknown {
            let b = it.d.cursor.choose(2) == 1;
            st.heap.tables.get_mut(t).expect("an unknown field's table").hash.insert(k.clone(), crate::trace::heap::Value::Bool(b));
        }
        if level.floors_near {
            concretize_near_floors(&mut st, &mut it.d)?;
        }
        // A path that RAISES (`Interp::poison`) ends in no state, and only then.
        it.raised.clear();
        let ended = it.exec_block(body.nodes(), st).map_err(|e| e.context(format!("fork path {}", paths + 1)))?;
        match ended.len() {
            0 => anyhow::ensure!(!it.raised.is_empty(), "a frame path ended in no state without a raise"),
            1 => {
                let (mut out, flow) = ended.into_iter().next().expect("one state");
                anyhow::ensure!(!matches!(flow, crate::trace::interp::Flow::Break), "break at chunk toplevel");
                // The absent-as-zero fields, as the tracer's `trace_frame` writes them.
                crate::trace::widen::materialize_absent_fields(&mut out, &mut it.d)?;
                outputs.push(out);
            }
            n => bail!("a frame path ended in {n} states (expected one, or none after a raise)"),
        }
        paths += 1;
        if paths > cap {
            return Err(TooManyPaths(cap).into());
        }
        if !it.d.cursor.advance() {
            break;
        }
    }
    Ok(outputs)
}

/// `run_frame_all` stopped at its path cap.
#[derive(Debug)]
pub struct TooManyPaths(pub usize);

impl std::fmt::Display for TooManyPaths {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "run_frame_all: more than {} fork paths in one frame", self.0)
    }
}

impl std::error::Error for TooManyPaths {}

/// A near level's floors, concretized per path as the kernels read them
/// (`Hook::NearFloor`'s input, `widen::read_near_floors`): a widened `state` (an interval) is each
/// whole number in it, by the cursor, and `collideable` is `state ~= 2` - the
/// cart keeps the two in step, so a stored unknown `collideable` is not a
/// separate choice.
fn concretize_near_floors<A: Arithmetic>(st: &mut State<RefDomain<A>>, d: &mut RefDomain<A>) -> Result<()> {
    use crate::trace::heap::Value;
    use crate::trace::iface;
    use crate::trace::widen::{field, objects_of_type};
    for obj in objects_of_type(st, "fall_floor") {
        let (ps, pc) = (field(&obj, &["state"]), field(&obj, &["collideable"]));
        let Some(Value::Num(v)) = iface::get(st, &ps) else { bail!("{}: not a number", iface::show(&ps)) };
        let int = |n| A::as_i16(n).ok_or_else(|| anyhow::anyhow!("{}: {} is not an integer", iface::show(&ps), A::show(n)));
        let (lo, hi) = A::bounds(v);
        let (lo, hi) = (int(lo)?, int(hi)?);
        let k = lo + d.cursor.choose((hi - lo + 1) as u32) as i16;
        iface::set(st, &ps, Value::Num(A::point(A::from_i16(k))))?;
        iface::set(st, &pc, Value::Bool(k != 2))?;
    }
    Ok(())
}
