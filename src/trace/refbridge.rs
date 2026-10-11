//! The reference engine's edge: one lane of a block <-> a `State<RefDomain>`,
//! through the one converter (`trace::block`), with the reference's values:
//! a number is a point or an interval, a boolean a `bool`.

use anyhow::{bail, Result};
use celeste_engine::runtime2::{Rt2, AV};

use crate::trace::block::{self, FnInfo, BUTTON_GLOBAL};
use crate::trace::heap::{TableId, Value};
use crate::trace::refdomain::RefDomain;
use crate::trace::state::State;

/// Lane `lane` of a block as a reference state (plus the base state's
/// builtins), and its unknown-boolean fields except the buttons, for the
/// driver to fork (`block::from_block`).
pub fn from_block(
    rt2: &Rt2,
    lane: usize,
    fn_info: &FnInfo,
    base: &State<RefDomain>,
) -> Result<(State<RefDomain>, Vec<(TableId, String)>)> {
    block::from_block(&mut RefDomain::new(), rt2, lane, fn_info, base)
}

/// Set the six buttons of a reference state from an input byte (bit 0 left,
/// 1 right, 2 up, 3 down, 4 jump, 5 dash). Returns the button table.
pub fn set_buttons(st: &mut State<RefDomain>, byte: u8) -> Result<TableId> {
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
/// `s`: the search's settings (its start room's cart and collision cache).
pub fn to_block(st: &State<RefDomain>, s: &crate::settings::Settings) -> Result<Rt2> {
    let (cart, cache) = crate::compiled::room_context(s)?;
    block::to_rt2(
        st,
        cart,
        cache,
        |iv| {
            Ok(match iv.to_number() {
                Some(n) => AV::Num(n),
                None => AV::Ival(iv.low, iv.high),
            })
        },
        |b| Ok(AV::Bool(*b)),
    )
}
