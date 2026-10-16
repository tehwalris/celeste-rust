//! The reference engine's edge: one lane of a block <-> a `State<RefDomain>`,
//! through the one converter (`trace::block`), with the reference's values:
//! a number is a point or an interval, a boolean a `bool`.

use anyhow::{bail, Result};
use celeste_core::arith::{Arithmetic, EngineNum};
use celeste_engine::runtime2::{Rt2, AV};

use crate::trace::block::{self, FnInfo, BUTTON_GLOBAL};
use crate::trace::heap::{TableId, Value};
use crate::trace::refdomain::RefDomain;
use crate::trace::state::State;

/// Lane `lane` of a block as a reference state of the block's arithmetic
/// (plus the base state's builtins), and its unknown-boolean fields except
/// the buttons, for the driver to fork (`block::from_block`).
pub fn from_block<N: EngineNum>(
    rt2: &Rt2<N>,
    lane: usize,
    fn_info: &FnInfo,
    base: &State<RefDomain<N::A>>,
) -> Result<(State<RefDomain<N::A>>, Vec<(TableId, String)>)> {
    block::from_block(&mut RefDomain::<N::A>::new(), rt2, lane, fn_info, base)
}

/// Set the six buttons of a reference state from an input byte (bit 0 left,
/// 1 right, 2 up, 3 down, 4 jump, 5 dash). Returns the button table.
pub fn set_buttons<A: celeste_core::arith::Arithmetic>(st: &mut State<RefDomain<A>>, byte: u8) -> Result<TableId> {
    let Some(Value::Table(t)) = st.heap.tables[&st.globals].hash.get(BUTTON_GLOBAL).cloned() else {
        bail!("no {BUTTON_GLOBAL} table")
    };
    let arr = &mut st.heap.tables.get_mut(&t).expect("a live table").arr;
    for (i, b) in arr.iter_mut().enumerate() {
        *b = Value::Bool(byte >> i & 1 == 1);
    }
    Ok(t)
}

/// A reference state as a one-lane block (`block::to_rt2`, no row keys:
/// `frame::Block::canonical` makes them canonical; a tree keys them).
/// `room`: the search's start room (the block's cart and collision cache).
pub fn to_block<N: EngineNum>(st: &State<RefDomain<N::A>>, room: (i16, i16)) -> Result<Rt2<N>> {
    let (cart, cache) = crate::compiled::room_context(room)?;
    block::to_rt2(
        st,
        cart,
        cache,
        |iv| {
            Ok(match <N::A as Arithmetic>::as_point(*iv) {
                Some(n) => AV::Num(n),
                None => {
                    let (lo, hi) = <N::A as Arithmetic>::bounds(*iv);
                    AV::Ival(lo, hi)
                }
            })
        },
        |b| Ok(AV::Bool(*b)),
    )
}
