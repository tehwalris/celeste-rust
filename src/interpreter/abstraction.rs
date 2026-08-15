//! The abstraction layer: how a concrete frame-boundary state becomes an
//! abstract one, and how abstract states are probed.
//!
//! PROOF-CRITICAL. Everything in this module shapes the reachable set the
//! search explores and therefore the optimality proofs:
//!
//! * `RemPrecision` / `make_state_abstract_rem` - the rem widening ladder
//!   (floor-aligned buckets of width 2^-k, each level nesting inside the
//!   previous), plus the fruit `off` widening at interval levels.
//! * `split_rem_straddles` - the per-bucket canonicalization that keeps a
//!   state's rem interval inside one bucket (a straddling interval would
//!   make equal rows hash differently).
//! * `apply_conservative_widenings` - the boundary pins (gameplay-dead
//!   timers, dash_effect_time clamp) certified by `rewrite widencheck`.
//! * `object_shape` - what shape dispatch (`verify::Variant`) routes on.
//! * The lane probes (`count_room_x_lanes`, `room_x_lane_mask`,
//!   `player_xy_per_lane`) that win detection and the tools read.
//!
//! Every widening here must be an OVER-approximation: it may only grow the
//! reachable set, never drop a state a concrete run could visit. If a
//! widening cannot be justified, do not add it - insert a runtime guard
//! and let the run fail loudly instead.

use std::collections::{HashMap, HashSet};
use std::hash::BuildHasherDefault;

use rustc_hash::FxHasher;

use super::{
    heap::HeapId,
    inspect::StateHelper,
    state::State,
    value::{HeapValue, MaybeVector, Value},
};
use crate::pico8_num::{Pico8Num, Pico8NumInterval};

type FxHashMap<K, V> = std::collections::HashMap<K, V, BuildHasherDefault<FxHasher>>;

/// The object-array shape of a state: the sequence of type-table global
/// names of the objects currently alive, in array order - e.g.
/// `["fake_wall", "player"]` for room (0,0) after the spawn finishes.
///
/// Pointers are per-state structure, never lane-varying, so the shape is a
/// property of the whole state: every lane in a state shares it. This is
/// what makes shape dispatch (`verify::Variant`) a per-state decision.
///
/// Loud on structural surprises: an object whose `type` does not resolve
/// unambiguously to a global would make shape dispatch silently misroute,
/// which is worse than a crash.
pub fn object_shape(state: &State) -> anyhow::Result<Vec<String>> {
    use anyhow::anyhow;
    let helper = StateHelper::new(state);
    let arr_id = helper
        .get_objects_array_id()
        .ok_or_else(|| anyhow!("object_shape: no `objects` array global"))?;
    let items: Vec<HeapId> = match helper.load(arr_id) {
        HeapValue::ArrayTable(items) => items.clone(),
        other => {
            return Err(anyhow!("object_shape: `objects` is not an ArrayTable: {:?}", other))
        }
    };
    // Reverse map: heap id of a pointed-to table -> global names pointing at
    // it. Built per call; the global env is small and boundary states are
    // few. Ambiguity (two globals aliasing one type table) is an error at
    // the point of use, not a silent pick - global_env iteration order is
    // not deterministic.
    let mut names: FxHashMap<HeapId, Vec<&str>> = FxHashMap::default();
    for (name, cell) in &state.global_env {
        if let HeapValue::Value(Value::Pointer(target)) = helper.load(*cell) {
            names.entry(*target).or_default().push(name.as_str());
        }
    }
    let mut shape = Vec::with_capacity(items.len());
    for item in items {
        let obj_id = helper
            .unwrap_pointer(helper.load(item))
            .ok_or_else(|| anyhow!("object_shape: objects element is not a pointer"))?;
        let obj = match helper.load(obj_id) {
            HeapValue::ObjectTable(t) => t,
            other => {
                return Err(anyhow!("object_shape: object is not an ObjectTable: {:?}", other))
            }
        };
        let type_ptr = obj
            .get("type")
            .ok_or_else(|| anyhow!("object_shape: object has no `type` field"))?;
        let type_id = helper
            .unwrap_pointer(helper.load(*type_ptr))
            .ok_or_else(|| anyhow!("object_shape: object `type` is not a pointer"))?;
        match names.get(&type_id).map(Vec::as_slice) {
            Some([name]) => shape.push(name.to_string()),
            Some(many) => {
                let mut many: Vec<&str> = many.to_vec();
                many.sort_unstable();
                return Err(anyhow!(
                    "object_shape: object type table is aliased by several globals: {:?}",
                    many
                ));
            }
            None => {
                return Err(anyhow!(
                    "object_shape: object type at heap {:?} matches no global",
                    type_id
                ))
            }
        }
    }
    Ok(shape)
}

/// Marks to apply when making state abstract
pub struct HeapMarks {
    /// Map from mark name to set of heap IDs that should get that mark
    pub marks: HashMap<String, HashSet<HeapId>>,
}

impl HeapMarks {
    pub fn new() -> Self {
        Self {
            marks: HashMap::new(),
        }
    }

    pub fn add_mark(&mut self, mark_name: &str, heap_id: HeapId) {
        self.marks
            .entry(mark_name.to_string())
            .or_insert_with(HashSet::new)
            .insert(heap_id);
    }
}

/// Mark heap locations that should be made abstract.
/// Currently marks player.rem.x and player.rem.y as "player_rem_xy".
pub fn mark_heap(state: &State) -> HeapMarks {
    let mut marks = HeapMarks::new();
    let helper = StateHelper::new(state);

    // Find player objects
    if let Some(objects_array_id) = helper.get_objects_array_id() {
        let players = helper.find_objects_by_type(objects_array_id, "player")
            .expect("get_objects_array_id returned non-ArrayTable");
        for player_heap_id in players {
            if let HeapValue::ObjectTable(player) = helper.load(player_heap_id) {
                // Get player.rem
                if let Some(rem_ptr) = player.get("rem") {
                    if let Some(rem_heap_id) = helper.unwrap_pointer(helper.load(*rem_ptr)) {
                        if let HeapValue::ObjectTable(rem) = helper.load(rem_heap_id) {
                            // Mark rem.x and rem.y
                            for key in ["x", "y"] {
                                if let Some(coord_ptr) = rem.get(key) {
                                    marks.add_mark("player_rem_xy", *coord_ptr);
                                }
                            }
                        }
                    }
                }
                // Mark dash_effect_time for the boundary clamp (see
                // make_state_abstract): it decrements unconditionally every
                // frame, so without a clamp it drifts negative forever and
                // makes otherwise-identical states at different frames
                // distinct, defeating cross-frame visited dedup.
                if let Some(ptr) = player.get("dash_effect_time") {
                    marks.add_mark("player_dash_effect_time", *ptr);
                }
            }
        }
    }

    marks
}

/// Make marked heap values abstract by replacing concrete numbers with intervals.
/// This is the key function for abstract interpretation - it widens concrete values
/// to represent uncertainty (e.g., player's sub-pixel position can be anywhere in [-0.5, 0.5)).
/// The rem precision ladder (plans/refinement-plan.md).
///
/// * `Bits(0)` - the historic widening: rem -> the full interval [-0.5, 0.5).
/// * `Bits(k)`, k in 1..=15 - quantize rem to floor-aligned buckets of width
///   2^-k: the interval `[floor(rem * 2^k) / 2^k, +2^-k)`. Each level nests
///   inside the previous one, so level k-1 over-approximates level k.
/// * `Exact` - no rem widening at all (the concrete rem dynamics).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum RemPrecision {
    Bits(u8),
    Exact,
}

/// The session's rem precision: CELESTE_REM_BITS=k (16 or CELESTE_EXACT_REM
/// mean exact; unset means the historic Bits(0)). Read once.
pub fn rem_precision_from_env() -> RemPrecision {
    static PRECISION: std::sync::OnceLock<RemPrecision> = std::sync::OnceLock::new();
    *PRECISION.get_or_init(|| {
        if std::env::var_os("CELESTE_EXACT_REM").is_some() {
            return RemPrecision::Exact;
        }
        match std::env::var("CELESTE_REM_BITS") {
            Ok(v) => {
                let bits: u8 = v
                    .parse()
                    .unwrap_or_else(|_| panic!("CELESTE_REM_BITS={:?} is not a number", v));
                if bits >= 16 {
                    RemPrecision::Exact
                } else {
                    RemPrecision::Bits(bits)
                }
            }
            Err(_) => RemPrecision::Bits(0),
        }
    })
}

pub fn make_state_abstract(state: State) -> State {
    apply_conservative_widenings(make_state_abstract_rem(state, rem_precision_from_env()))
}

/// Split lanes whose boundary rem interval straddles a bucket boundary of
/// the session precision into one lane per bucket, clipping the interval.
///
/// Why: the boundary widening maps each lane's rem to the bucket(s) covering
/// it. A straddling interval (e.g. [-0.463, 0.037) at 1 bit, after a dash's
/// rounding) would widen to the SPAN of two buckets - sound, but the row is
/// then a different value than the single bucket the same underlying state
/// gets when reached along another path, so row identity fragments and
/// band/witness probes miss legitimate rows. Splitting first makes every
/// boundary row's rem exactly one bucket - canonical identity per level.
///
/// Boundary intervals inherit single-bucket width (they enter the frame as
/// one bucket and the move only shifts and renormalizes them), so a
/// straddle spans at most two adjacent buckets; this is asserted.
pub fn split_rem_straddles(state: State) -> Vec<State> {
    let RemPrecision::Bits(bits) = rem_precision_from_env() else { return vec![state] };
    if bits == 0 {
        return vec![state];
    }
    let width: i32 = 0x1_0000 >> bits;
    let bucket_low = |n: Pico8Num| (n.as_raw_u32() as i32).div_euclid(width) * width;

    let marks = mark_heap(&state);
    let Some(cells) = marks.marks.get("player_rem_xy").cloned() else { return vec![state] };

    let mut work = vec![state];
    for cell in cells {
        let mut next = Vec::with_capacity(work.len());
        for state in work {
            let Some(HeapValue::Value(Value::NumberInterval(mv))) = state.heap.get_opt(cell)
            else {
                // Numbers (and anything else) lie in a single bucket.
                next.push(state);
                continue;
            };
            let intervals: Vec<Pico8NumInterval> = match mv {
                MaybeVector::Scalar(iv) => vec![*iv; state.vector_size.max(1)],
                MaybeVector::Vector(ivs) => ivs.iter().copied().collect(),
            };
            let straddle: Vec<bool> = intervals
                .iter()
                .map(|iv| {
                    let lo = bucket_low(iv.low);
                    let hi = bucket_low(iv.high);
                    assert!(
                        hi - lo <= width,
                        "rem interval {:?} spans more than two buckets at {} bits",
                        iv,
                        bits
                    );
                    hi != lo
                })
                .collect();
            if straddle.iter().all(|s| !s) {
                next.push(state);
                continue;
            }
            let from_raw = |r: i32| Pico8Num::from_parts((r >> 16) as i16, r as u16);
            // A: every lane, straddlers clipped to their LOW bucket.
            let clipped_low: Vec<Pico8NumInterval> = intervals
                .iter()
                .zip(&straddle)
                .map(|(iv, s)| {
                    if *s {
                        Pico8NumInterval::new(iv.low, from_raw(bucket_low(iv.low) + width - 1))
                    } else {
                        *iv
                    }
                })
                .collect();
            let mut low_state = state.clone();
            low_state.heap.set(
                cell,
                HeapValue::Value(Value::NumberInterval(MaybeVector::vector(clipped_low))),
            );
            next.push(low_state);
            // B: only the straddler lanes, clipped to their HIGH bucket.
            let high_state = state.filter_by_mask_clone(
                &straddle,
                crate::interpreter::state::FILTER_SPLIT_FLR,
            );
            let clipped_high: Vec<Pico8NumInterval> = intervals
                .iter()
                .zip(&straddle)
                .filter(|(_, s)| **s)
                .map(|(iv, _)| Pico8NumInterval::new(from_raw(bucket_low(iv.high)), iv.high))
                .collect();
            let mut high_state = high_state;
            high_state.heap.set(
                cell,
                HeapValue::Value(Value::NumberInterval(MaybeVector::vector(clipped_high))),
            );
            next.push(high_state);
        }
        work = next;
    }
    work
}

/// Only the historic rem widening - the baseline abstraction the search has
/// always used. The widen-check (rewrite widencheck) runs the search with
/// this alone and applies `apply_conservative_widenings` post hoc, to certify
/// that the newer widenings are conservative: widening at every boundary
/// must yield exactly the post-hoc-widened exact sets, or the widened field
/// influenced gameplay and the widening changed the reachable set.
pub fn make_state_abstract_rem_only(state: State) -> State {
    make_state_abstract_rem(state, RemPrecision::Bits(0))
}

/// The floor-aligned width-2^-bits bucket containing `n` (bits in 1..=15).
fn rem_bucket(n: Pico8Num, bits: u8) -> Pico8NumInterval {
    let width: i32 = 0x1_0000 >> bits;
    let raw = n.as_raw_u32() as i32;
    let low = raw.div_euclid(width) * width;
    let from_raw = |r: i32| Pico8Num::from_parts((r >> 16) as i16, r as u16);
    Pico8NumInterval::new(from_raw(low), from_raw(low + width - 1))
}

/// Widen player.rem.x/y at `precision` (see `RemPrecision`). Bits(0) is the
/// historic full-interval widening, bit for bit; Exact leaves rem untouched.
pub fn make_state_abstract_rem(mut state: State, precision: RemPrecision) -> State {
    if precision == RemPrecision::Exact {
        return state;
    }
    let marks = mark_heap(&state);

    // The player_rem_xy range: [-0.5, 0.5). 0.5 is 0x8000 fractional.
    let half = Pico8Num::from_parts(0, 0x8000);
    let neg_half = -half;
    let half_below = half.next_smallest(); // 0.5 - epsilon
    let wide_interval = Pico8NumInterval::new(neg_half, half_below);

    // Bucket of a single value at this precision.
    let widen_number = |n: Pico8Num| -> Pico8NumInterval {
        assert!(
            wide_interval.contains_number(n),
            "player_rem value {:?} not in expected interval",
            n
        );
        match precision {
            RemPrecision::Bits(0) => wide_interval,
            RemPrecision::Bits(bits) => rem_bucket(n, bits),
            RemPrecision::Exact => unreachable!(),
        }
    };
    // An interval (mid-frame refinement residue) spans its endpoint buckets.
    let widen_interval = |iv: &Pico8NumInterval| -> Pico8NumInterval {
        assert!(
            wide_interval.contains_interval(iv),
            "player_rem interval {:?} not in expected interval",
            iv
        );
        match precision {
            RemPrecision::Bits(0) => wide_interval,
            RemPrecision::Bits(bits) => {
                Pico8NumInterval::new(rem_bucket(iv.low, bits).low, rem_bucket(iv.high, bits).high)
            }
            RemPrecision::Exact => unreachable!(),
        }
    };

    if let Some(heap_ids) = marks.marks.get("player_rem_xy") {
        for &heap_id in heap_ids {
            let heap_value = state.heap.get(heap_id);
            if let HeapValue::Value(value) = heap_value {
                let new_value = match value {
                    Value::Number(MaybeVector::Scalar(n)) => {
                        Value::NumberInterval(MaybeVector::Scalar(widen_number(*n)))
                    }
                    Value::Number(MaybeVector::Vector(nums)) => Value::NumberInterval(
                        MaybeVector::vector(nums.iter().map(|n| widen_number(*n)).collect()),
                    ),
                    Value::NumberInterval(MaybeVector::Scalar(interval)) => {
                        Value::NumberInterval(MaybeVector::Scalar(widen_interval(interval)))
                    }
                    Value::NumberInterval(MaybeVector::Vector(intervals)) => {
                        Value::NumberInterval(MaybeVector::vector(
                            intervals.iter().map(|iv| widen_interval(iv)).collect(),
                        ))
                    }
                    other => {
                        panic!("Unexpected value type for player_rem: {:?}", other);
                    }
                };
                state.heap.set(heap_id, HeapValue::Value(new_value));
            }
        }
    }

    // Widen each live fruit's bob counter to unknown-within-period: off :=
    // the FULL interval [0, 39] at every non-exact level (this function
    // returns early for Exact, where `off` stays concrete mod 40 - the
    // conservative pin). This joins the refinement ladder exactly like rem:
    // coarse levels over-approximate the bob (sin over the interval covers
    // [-1, 1], so the fruit's y is the whole band and collisions split on
    // UnknownBool), refutations stay sound, and the exact level resolves
    // the phase through the k15 band. Rationale (Philippe, 2026-08-07): the
    // berry is extremely unlikely to be on the optimal path; if it IS, the
    // exact level still decides correctly - a wrong result cannot slip
    // through, only a loose band. The win: all break cohorts collapse into
    // one row family (33.5% of the room-(0,0) frontier at f79 was
    // per-cohort fruit rows).
    {
        let helper = StateHelper::new(&state);
        let mut off_cells: Vec<HeapId> = Vec::new();
        if let Some(arr_id) = helper
            .find_global("objects")
            .and_then(|id| helper.unwrap_pointer(helper.load(id)))
        {
            let fruits = helper
                .find_objects_by_type(arr_id, "fruit")
                .unwrap_or_else(|e| panic!("fruit-off widening: {}", e));
            for obj_id in fruits {
                let HeapValue::ObjectTable(obj) = helper.load(obj_id) else {
                    panic!("fruit-off widening: fruit is not an ObjectTable");
                };
                off_cells.push(*obj.get("off").unwrap_or_else(|| {
                    panic!("fruit-off widening: fruit has no `off` field")
                }));
            }
        }
        let full_period = Pico8NumInterval::new(Pico8Num::from_i16(0), Pico8Num::from_i16(39));
        for cell in off_cells {
            match state.heap.get(cell) {
                HeapValue::Value(Value::Number(_) | Value::NumberInterval(_)) => {
                    state.heap.set(
                        cell,
                        HeapValue::Value(Value::NumberInterval(MaybeVector::Scalar(full_period))),
                    );
                }
                other => panic!("fruit-off widening: off is not numeric: {:?}", other),
            }
        }
    }

    state
}

/// The newer boundary widenings, all justified as behavior-preserving by read
/// censuses (and certifiable dynamically via `rewrite widencheck`): the
/// dash_effect_time clamp, and the gameplay-dead timer-global pins. Applied
/// as part of `make_state_abstract`, and applied post hoc by the widen-check.
pub fn apply_conservative_widenings(mut state: State) -> State {
    let marks = mark_heap(&state);

    // Clamp player.dash_effect_time at 0 from below. The field decrements
    // unconditionally every frame (celeste-minimal.lua:124) and its ONLY read
    // anywhere is `hit.dash_effect_time > 0` (line 471), so every value <= 0
    // is behaviorally identical - the clamp is a no-op in every room, not
    // just this one. Without it the field drifts negative forever and two
    // otherwise-identical states at different frames never compare equal,
    // defeating cross-frame visited dedup.
    if let Some(heap_ids) = marks.marks.get("player_dash_effect_time") {
        let zero = Pico8Num::from_i16(0);
        for &heap_id in heap_ids {
            match state.heap.get(heap_id) {
                HeapValue::Value(Value::Number(mv)) => {
                    let clamped = match mv {
                        MaybeVector::Scalar(n) => {
                            MaybeVector::Scalar(if *n < zero { zero } else { *n })
                        }
                        MaybeVector::Vector(ns) => MaybeVector::vector(
                            ns.iter().map(|n| if *n < zero { zero } else { *n }).collect(),
                        ),
                    };
                    state.heap.set(heap_id, HeapValue::Value(Value::Number(clamped)));
                }
                other => panic!("player dash_effect_time is not a number: {:?}", other),
            }
        }
    }

    // NOTE on p_jump/p_dash (2026-08-06): widening the held-button trails to
    // unknown at the boundary was considered (it would merge the dominated
    // held variants with their released twins) and REJECTED by Philippe: it
    // is an over-approximation - it admits e.g. ground-jump at n followed by
    // wall-jump at n+1, which the concrete game forbids (the press at n
    // forces p_jump=true at n+1). Unlike the rem widening this changes the
    // reachable set asymmetrically, so it stays out.

    // Reduce each live fruit's bob counter modulo its period 40.
    //
    // Read census: `off` is written at fruit.init (this.off=0) and
    // fruit.update (this.off += 1), and READ exactly once - fruit.update's
    // `sin(this.off/40)` (celeste-minimal.lua ~line 425). For nonnegative
    // integers, fixed-point division splits exactly ((off+40)/40 ==
    // off/40 + 1), and `pico8_sin` reduces its argument mod one turn in
    // fixed point, so sin(off/40) == sin((off mod 40)/40) bit-for-bit -
    // pinned exhaustively by test_pico8_sin_period_40_bit_exact. Without
    // this pin, `off` is an embedded frame counter: every fruit-alive row
    // differs across frames and break cohorts, defeating cross-frame
    // visited dedup (measured at 33.5% of the room-(0,0) frontier at f79).
    // No fruit ever exists in room (1,0), so this is a no-op there and the
    // standing (1,0) row hashes are untouched.
    {
        let helper = StateHelper::new(&state);
        let mut off_updates: Vec<(HeapId, Value)> = Vec::new();
        if let Some(arr_id) = helper
            .find_global("objects")
            .and_then(|id| helper.unwrap_pointer(helper.load(id)))
        {
            let fruits = helper
                .find_objects_by_type(arr_id, "fruit")
                .unwrap_or_else(|e| panic!("fruit-off pin: {}", e));
            for obj_id in fruits {
                let HeapValue::ObjectTable(obj) = helper.load(obj_id) else {
                    panic!("fruit-off pin: fruit is not an ObjectTable");
                };
                let off_cell = *obj
                    .get("off")
                    .unwrap_or_else(|| panic!("fruit-off pin: fruit has no `off` field"));
                let reduce = |n: &Pico8Num| -> Pico8Num {
                    let i = n
                        .as_i16()
                        .unwrap_or_else(|| panic!("fruit-off pin: off {:?} is not an integer", n));
                    assert!(i >= 0, "fruit-off pin: off {} is negative", i);
                    Pico8Num::from_i16(i % 40)
                };
                match helper.load(off_cell) {
                    HeapValue::Value(Value::Number(mv)) => {
                        let reduced = match mv {
                            MaybeVector::Scalar(n) => MaybeVector::Scalar(reduce(n)),
                            MaybeVector::Vector(ns) => {
                                MaybeVector::vector(ns.iter().map(reduce).collect())
                            }
                        };
                        off_updates.push((off_cell, Value::Number(reduced)));
                    }
                    // Already widened to unknown-within-period by
                    // make_state_abstract_rem (which runs before this at
                    // non-exact levels; mid-frame the interval drifts to at
                    // most [1, 40]). Validate the bound and leave it - the
                    // next boundary's widening resets it to [0, 39].
                    HeapValue::Value(Value::NumberInterval(mv)) => {
                        let ok = |iv: &Pico8NumInterval| {
                            iv.low >= Pico8Num::from_i16(0) && iv.high <= Pico8Num::from_i16(40)
                        };
                        let all_ok = match mv {
                            MaybeVector::Scalar(iv) => ok(iv),
                            MaybeVector::Vector(ivs) => ivs.iter().all(ok),
                        };
                        assert!(all_ok, "fruit-off pin: widened off outside [0, 40]: {:?}", mv);
                    }
                    other => panic!("fruit-off pin: off is not a number: {:?}", other),
                }
            }
        }
        for (cell, value) in off_updates {
            state.heap.set(cell, HeapValue::Value(value));
        }
    }

    // Pin the gameplay-dead timer globals to 0.
    //
    // `frames`, `seconds`, `minutes` and `deaths` form a closed subsystem in
    // celeste-minimal: they only ever feed each other (the timer cascade and
    // the death counter), never gameplay. The single gameplay read is the
    // key sprite wobble `sin(frames/30)` (celeste-minimal.lua, key.update).
    // For the default room `sin` is absent from the fixed env, so a room
    // where that read executes crashes loudly instead of silently depending
    // on a pinned value. For non-default start rooms `sin` IS registered
    // (game_runner), so the belt there is different: the pin is sound in a
    // room iff no key object exists in it - room (0,0) has none (fruit bobs
    // on its per-object `off` counter, not `frames`). Revisit before any
    // key room. Erasing them at the
    // frame boundary makes the state representation world-still, which is
    // what allows cross-frame visited-set dedup (a state reached at frame n
    // never needs re-expansion later). It also merges died-and-respawned
    // lanes with never-died ones (`deaths` is lane-varying after a death).
    for name in ["frames", "seconds", "minutes", "deaths"] {
        let cell = state
            .global_env
            .get(name)
            .copied()
            .unwrap_or_else(|| panic!("timer global {} missing - pinning would silently not apply", name));
        match state.heap.get(cell) {
            HeapValue::Value(Value::Number(_)) => {
                state.heap.set(
                    cell,
                    HeapValue::Value(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(0)))),
                );
            }
            other => panic!("timer global {} is not a number: {:?}", name, other),
        }
    }

    state
}

/// Lanes whose global `room.x` equals `x`.
///
/// `next_room()` writes the new room index when the player crosses the top of
/// the screen, so for a search confined to one room the lanes whose `room.x`
/// equals the next room's x (`game_runner::win_room_x()`) at a frame boundary
/// are exactly the ones that exited - the win condition. The earliest frame
/// where any appear is the optimal TAS length (under the search's stated
/// abstractions).
///
/// Loud on structural surprises: `room` missing or non-numeric means the
/// probe would silently never fire, which is worse than a crash.
pub fn count_room_x_lanes(state: &State, x: i16) -> usize {
    let helper = StateHelper::new(state);
    // The global cell holds a pointer to the table (globals are boxed like
    // locals); follow the one indirection.
    let cell = helper
        .find_global("room")
        .unwrap_or_else(|| panic!("count_room_x_lanes: no `room` global"));
    let table_id = helper
        .unwrap_pointer(helper.load(cell))
        .unwrap_or_else(|| panic!("count_room_x_lanes: `room` global is not a table pointer"));
    let HeapValue::ObjectTable(room) = helper.load(table_id) else {
        panic!("count_room_x_lanes: `room` does not point at a table");
    };
    let x_id = *room
        .get("x")
        .unwrap_or_else(|| panic!("count_room_x_lanes: room table has no x field"));
    let HeapValue::Value(Value::Number(n)) = helper.load(x_id) else {
        panic!("count_room_x_lanes: room.x is not a number");
    };
    let want = Pico8Num::from_i16(x);
    match n {
        MaybeVector::Scalar(s) => {
            if *s == want {
                state.vector_size
            } else {
                0
            }
        }
        MaybeVector::Vector(v) => v.iter().filter(|s| **s == want).count(),
    }
}

/// Per-lane version of `count_room_x_lanes`: which lanes have `room.x == x`.
pub fn room_x_lane_mask(state: &State, x: i16) -> Vec<bool> {
    let helper = StateHelper::new(state);
    let cell = helper
        .find_global("room")
        .unwrap_or_else(|| panic!("room_x_lane_mask: no `room` global"));
    let table_id = helper
        .unwrap_pointer(helper.load(cell))
        .unwrap_or_else(|| panic!("room_x_lane_mask: `room` global is not a table pointer"));
    let HeapValue::ObjectTable(room) = helper.load(table_id) else {
        panic!("room_x_lane_mask: `room` does not point at a table");
    };
    let x_id = *room
        .get("x")
        .unwrap_or_else(|| panic!("room_x_lane_mask: room table has no x field"));
    let HeapValue::Value(Value::Number(n)) = helper.load(x_id) else {
        panic!("room_x_lane_mask: room.x is not a number");
    };
    let want = Pico8Num::from_i16(x);
    match n {
        MaybeVector::Scalar(s) => vec![*s == want; state.vector_size.max(1)],
        MaybeVector::Vector(v) => v.iter().map(|s| *s == want).collect(),
    }
}

/// Per-lane `(room.x, room.y)`. Object coordinates are ROOM-LOCAL, so a
/// room transition wraps the player's x from ~128 back to ~0; anything that
/// measures distance between two frames has to add `room * 128` first or
/// that one frame looks like a 128-pixel teleport.
pub fn room_xy_per_lane(state: &State) -> Option<Vec<(i16, i16)>> {
    let helper = StateHelper::new(state);
    let table_id = helper.unwrap_pointer(helper.load(helper.find_global("room")?))?;
    let HeapValue::ObjectTable(room) = helper.load(table_id) else { return None };
    let axis = |name: &str| -> Option<Vec<i16>> {
        let HeapValue::Value(Value::Number(n)) = helper.load(*room.get(name)?) else {
            return None;
        };
        Some(match n {
            MaybeVector::Scalar(v) => vec![v.whole_part_as_i16(); state.vector_size.max(1)],
            MaybeVector::Vector(vs) => vs.iter().map(|v| v.whole_part_as_i16()).collect(),
        })
    };
    Some(axis("x")?.into_iter().zip(axis("y")?).collect())
}

/// Per-lane (x, y) whole-pixel positions of the first object (the player or
/// the spawn animation), or `None` when the objects array is empty (dead
/// countdown states) or missing. Used by the per-coordinate saturation dump
/// (`CELESTE_XY_DUMP`); analysis-only, so unexpected shapes return `None`
/// rather than panicking - a missing histogram row is visible in the plot,
/// and this must not take a search down.
pub fn player_xy_per_lane(state: &State) -> Option<Vec<(i16, i16)>> {
    let helper = StateHelper::new(state);
    let arr_id = helper.get_objects_array_id()?;
    // Find the object BY TYPE, not by slot: in multi-object rooms the first
    // slot can be the fake_wall (this silently reported the wall's (8, 32)
    // as "the player" throughout room (0,0) until the witness-position
    // instrumentation exposed it). Fall back to player_spawn so the spawn
    // phase still reports a position.
    let obj_id = helper
        .find_objects_by_type(arr_id, "player")
        .ok()
        .and_then(|v| v.first().copied())
        .or_else(|| {
            helper
                .find_objects_by_type(arr_id, "player_spawn")
                .ok()
                .and_then(|v| v.first().copied())
        })?;
    let HeapValue::ObjectTable(obj) = helper.load(obj_id) else { return None };
    let axis = |name: &str| -> Option<Vec<i16>> {
        let cell = *obj.get(name)?;
        let HeapValue::Value(Value::Number(n)) = helper.load(cell) else { return None };
        Some(match n {
            MaybeVector::Scalar(v) => vec![v.whole_part_as_i16(); state.vector_size.max(1)],
            MaybeVector::Vector(vs) => vs.iter().map(|v| v.whole_part_as_i16()).collect(),
        })
    };
    let xs = axis("x")?;
    let ys = axis("y")?;
    if xs.len() != ys.len() {
        return None;
    }
    Some(xs.into_iter().zip(ys).collect())
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pico8_interval_half() {
        let half = Pico8Num::from_parts(0, 0x8000);
        let neg_half = -half;
        assert_eq!(half + neg_half, Pico8Num::from_i16(0));

        let interval = Pico8NumInterval::new(neg_half, half.next_smallest());
        assert!(interval.contains_number(Pico8Num::from_i16(0)));
        assert!(interval.contains_number(Pico8Num::from_parts(0, 0x4000))); // 0.25
        assert!(interval.contains_number(neg_half));
        assert!(!interval.contains_number(half)); // 0.5 is not included (we use < 0.5)
    }
}

/// A SYNTHETIC win target for cheap end-to-end tests: `CELESTE_WIN_AT_XY=x,y`
/// makes "won" mean "the player is at whole-pixel (x, y)" instead of "the
/// player left the room".
///
/// Why this exists. The real win is a room transition, so any test of the
/// forward/sweep pipeline that produces NON-VACUOUS `g` has to run to the
/// frame where the room is actually exited - 90+ on room (1,0), tens of
/// minutes. Moving the finish line to a position the optimal run passes
/// through EARLY gives the same pipeline, with real wins, at a fraction of
/// the horizon; two such points then bracket a cost extrapolation.
///
/// It is in the campaign fingerprint (`CampaignConfig`). A checkpoint or a
/// `g` produced under a synthetic win describes a different search, and
/// must never be resumable from, or comparable to, a real campaign's.
/// Read once.
pub fn synthetic_win_xy() -> Option<(i16, i16)> {
    static TARGET: std::sync::OnceLock<Option<(i16, i16)>> = std::sync::OnceLock::new();
    *TARGET.get_or_init(|| {
        let raw = std::env::var("CELESTE_WIN_AT_XY").ok()?;
        let (x, y) = raw.split_once(',').unwrap_or_else(|| {
            panic!("CELESTE_WIN_AT_XY must be \"x,y\", got {:?}", raw)
        });
        let parse = |s: &str, which: &str| -> i16 {
            s.trim().parse().unwrap_or_else(|e| {
                panic!("CELESTE_WIN_AT_XY {} coordinate {:?}: {}", which, s, e)
            })
        };
        let target = (parse(x, "x"), parse(y, "y"));
        println!(
            "SYNTHETIC WIN: a lane counts as won at player ({}, {}), NOT at the \
             room exit - this is a test configuration and is in the fingerprint",
            target.0, target.1
        );
        Some(target)
    })
}

/// Which lanes have won, under whichever win condition is configured.
///
/// The single definition every consumer uses - the forward pass's
/// absorbing set, the sweep's B(H) seed, the position graph's exclusion,
/// and the CLI probes - so a synthetic target cannot be honoured by some
/// of them and not others, which would be a silently inconsistent search.
pub fn win_lane_mask(state: &State) -> Vec<bool> {
    match synthetic_win_xy() {
        None => room_x_lane_mask(state, crate::game_runner::win_room_x()),
        Some(target) => match player_xy_per_lane(state) {
            // No player object: nothing here can be at the target.
            None => vec![false; state.vector_size.max(1)],
            Some(xy) => xy.into_iter().map(|p| p == target).collect(),
        },
    }
}

/// Count of `win_lane_mask`.
pub fn count_win_lanes(state: &State) -> usize {
    win_lane_mask(state).into_iter().filter(|w| *w).count()
}

/// How to describe the configured win in a log line, so a synthetic run is
/// never mistaken for a real one when reading output later.
pub fn win_label() -> String {
    match synthetic_win_xy() {
        None => format!("in room ({},_)", crate::game_runner::win_room_x()),
        Some((x, y)) => format!("at SYNTHETIC target ({}, {})", x, y),
    }
}
