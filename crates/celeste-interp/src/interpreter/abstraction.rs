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
//! * The lane probes (`player_xy_per_lane`, `room_xy_per_lane`) that the
//!   position column and the tools read.
//!
//! Every widening here must be an OVER-approximation: it may only grow the
//! reachable set, never drop a state a concrete run could visit. If a
//! widening cannot be justified, do not add it - insert a runtime guard
//! and let the run fail loudly instead.

use std::collections::{HashMap, HashSet};

use super::{
    heap::HeapId,
    inspect::StateHelper,
    state::State,
    value::{HeapValue, MaybeVector, Value},
};
use celeste_core::pico8_num::{Pico8Num, Pico8NumInterval};


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
        let players = helper
            .find_objects_by_type(objects_array_id, "player")
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
                // Get player.spd - the spd-rung widening's target
                // (plans/spd-rung.md). Marked unconditionally; whether
                // anything happens to it is SpdPrecision's decision.
                if let Some(spd_ptr) = player.get("spd") {
                    if let Some(spd_heap_id) = helper.unwrap_pointer(helper.load(*spd_ptr)) {
                        if let HeapValue::ObjectTable(spd) = helper.load(spd_heap_id) {
                            for key in ["x", "y"] {
                                if let Some(coord_ptr) = spd.get(key) {
                                    marks.add_mark("player_spd_xy", *coord_ptr);
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

impl RemPrecision {
    /// Does `self` widen rem AT LEAST as much as `finer` - i.e. is every
    /// `finer` bucket contained in one of `self`'s?
    ///
    /// `Bits(k)`'s buckets are floor-aligned at width 2^-k, so `Bits(k)`
    /// nests inside `Bits(k')` exactly when k >= k', and `Exact` (no
    /// widening) nests inside every `Bits`.
    pub fn coarser_or_equal(self, finer: RemPrecision) -> bool {
        match (self, finer) {
            (RemPrecision::Exact, _) => finer == RemPrecision::Exact,
            (RemPrecision::Bits(_), RemPrecision::Exact) => true,
            (RemPrecision::Bits(a), RemPrecision::Bits(b)) => a <= b,
        }
    }

    /// Every rem level this build can be configured for, coarsest first.
    /// `Bits(16)` and above are read as `Exact` by the env parser, so 0..16
    /// is the whole `Bits` range.
    pub fn all() -> impl Iterator<Item = RemPrecision> {
        (0..16u8)
            .map(RemPrecision::Bits)
            .chain(std::iter::once(RemPrecision::Exact))
    }
}

/// The session rem precision, as ONE settable process global.
///
/// The environment (CELESTE_REM_BITS / CELESTE_EXACT_REM) is the DEFAULT
/// source, read the first time the precision is needed; `set_rem_precision`
/// then overrides it for the rest of the process. This is what lets a single
/// process run several levels (the in-process `ladder`: level 0, then k=1,
/// then k=2) - the old read-once OnceLock could only express one.
///
/// Encoding: 0..=15 = `Bits(n)`, `EXACT` = `Exact`, `UNSET` = not yet read
/// from the env.
static REM_PRECISION: std::sync::atomic::AtomicU8 =
    std::sync::atomic::AtomicU8::new(REM_UNSET);
const REM_UNSET: u8 = 0xFF;
const REM_EXACT: u8 = 0xFE;

fn encode_rem(p: RemPrecision) -> u8 {
    match p {
        RemPrecision::Exact => REM_EXACT,
        RemPrecision::Bits(b) if b >= 16 => REM_EXACT,
        RemPrecision::Bits(b) => b,
    }
}

/// Set the session rem precision for the rest of the process (overriding the
/// env default). Last write wins.
pub fn set_rem_precision(p: RemPrecision) {
    REM_PRECISION.store(encode_rem(p), std::sync::atomic::Ordering::Relaxed);
}

fn rem_precision_from_env_default() -> RemPrecision {
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
}

/// The session's rem precision: the `set_rem_precision` value if one was set,
/// else the env default (CELESTE_REM_BITS=k; 16 or CELESTE_EXACT_REM mean
/// exact; unset means the historic Bits(0)), latched on first read.
pub fn rem_precision_from_env() -> RemPrecision {
    use std::sync::atomic::Ordering::Relaxed;
    match REM_PRECISION.load(Relaxed) {
        REM_UNSET => {
            let p = rem_precision_from_env_default();
            // Racing readers compute the same env default; either store wins.
            REM_PRECISION.store(encode_rem(p), Relaxed);
            p
        }
        REM_EXACT => RemPrecision::Exact,
        b => RemPrecision::Bits(b),
    }
}

/// The spd precision ladder (plans/spd-rung.md) - the rung BELOW level 0
/// that unblocks room (2,0).
///
/// * `WidthLog2(w)` - widen player.spd.x/y to floor-aligned buckets of
///   width 2^w in raw 16.16 units. w=16 is 1 px/frame. Buckets are
///   STATIC and data-independent (floor-alignment on the raw i32), so
///   the scheme covers the entire speed range by construction - no
///   assumption about which speeds occur. Power-of-two floor-aligned
///   buckets NEST, so a coarser level over-approximates a finer one and
///   band coarsening can never straddle.
/// * `Exact` - no spd widening (today's semantics; the env default).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum SpdPrecision {
    WidthLog2(u8),
    Exact,
}

impl SpdPrecision {
    /// Does `self` widen spd AT LEAST as much as `finer`? Buckets are
    /// floor-aligned at width 2^w on the raw 16.16 value, so they nest for
    /// w >= w', and `Exact` nests inside every `WidthLog2`.
    pub fn coarser_or_equal(self, finer: SpdPrecision) -> bool {
        match (self, finer) {
            (SpdPrecision::Exact, _) => finer == SpdPrecision::Exact,
            (SpdPrecision::WidthLog2(_), SpdPrecision::Exact) => true,
            (SpdPrecision::WidthLog2(a), SpdPrecision::WidthLog2(b)) => a >= b,
        }
    }

    /// Every spd level this build can be configured for, coarsest first.
    /// The env parser asserts 8..=20, so that is the whole range.
    pub fn all() -> impl Iterator<Item = SpdPrecision> {
        (8..=20u8)
            .rev()
            .map(SpdPrecision::WidthLog2)
            .chain(std::iter::once(SpdPrecision::Exact))
    }
}

/// The session's spd precision: CELESTE_SPD_WIDTH_LOG2=w (unset means
/// Exact - nothing changes for existing campaigns).
///
/// Still read-once from the env: unlike rem, no code path SETS spd
/// in-process (the ladder refines rem, not spd), so there is nothing for a
/// settable override to express. If an in-process spd ladder ever lands,
/// mirror `set_rem_precision`.
pub fn spd_precision_from_env() -> SpdPrecision {
    static PRECISION: std::sync::OnceLock<SpdPrecision> = std::sync::OnceLock::new();
    *PRECISION.get_or_init(|| match std::env::var("CELESTE_SPD_WIDTH_LOG2") {
        Ok(v) => {
            let w: u8 = v
                .parse()
                .unwrap_or_else(|_| panic!("CELESTE_SPD_WIDTH_LOG2={:?} is not a number", v));
            assert!(
                (8..=20).contains(&w),
                "CELESTE_SPD_WIDTH_LOG2={} outside the sane range 8..=20 \
                 (16 = 1 px/frame buckets)",
                w
            );
            SpdPrecision::WidthLog2(w)
        }
        Err(_) => SpdPrecision::Exact,
    })
}

pub fn make_state_abstract(state: State) -> State {
    erase_provenance_hints(apply_conservative_widenings(make_state_abstract_spd(
        make_state_abstract_rem(state, rem_precision_from_env()),
        spd_precision_from_env(),
    )))
}

/// Canonicalize provenance strings at the frame boundary: `Nil(Some(hint))`
/// becomes `Nil(None)` and `NilPointer(name)` becomes `NilPointer("")`.
///
/// The strings are diagnostic-only: a hint is BORN loading through a
/// `NilPointer` (core_interpreter's Load arm) and CONSUMED only in error
/// messages, and a `NilPointer`'s name likewise reaches nothing but
/// messages and the hint it mints. Game semantics never read either. But
/// both sit in the SHAPE HASH (`ValueShape::Nil(hint)` /
/// `ValueShape::NilPointer(name)`), so two states identical in every
/// gameplay coordinate were explored as two distinct search rows if they
/// reached the same nil along different paths.
///
/// This is a canonicalization, NOT a widening: the states it merges have
/// bit-identical concrete semantics, so there is nothing a refinement rung
/// would need to narrow back.
///
/// It is also what makes the compiled engine's exports and the
/// interpreter's states ONE key space. `compiled::bridge` cannot round-trip
/// the strings (`AV::Nil` and `AV::NilPtr` carry no payload), so its
/// exports were hint-less while the interpreter's states were not - the
/// two searches diverged at the first hinted nil (room (1,0) f25: every
/// row re-keyed, +24 rows against history; see BENCHMARK_DATA.md
/// "The 24-row divergence"). Erasing at the boundary makes the erased form
/// canonical on both paths.
///
/// Trajectory-changing: row keys differ from the first hinted nil onward,
/// which is why FORMAT_VERSION moved to 5.
pub fn erase_provenance_hints(mut state: State) -> State {
    // Boundary states carry no local envs (asserted by the bridge and true
    // at every call site); the heap is the whole story.
    for i in 0..state.heap.len() {
        let id = HeapId::from_raw(i);
        let erased = match state.heap.get_opt(id) {
            Some(HeapValue::Value(Value::Nil(Some(_)))) => Some(HeapValue::Value(Value::Nil(None))),
            Some(HeapValue::Value(Value::NilPointer(s))) if !s.is_empty() => {
                Some(HeapValue::Value(Value::NilPointer(String::new())))
            }
            Some(HeapValue::Closure(gid, caps))
                if caps.iter().any(|c| {
                    matches!(c, Value::Nil(Some(_)))
                        || matches!(c, Value::NilPointer(s) if !s.is_empty())
                }) =>
            {
                let caps = caps
                    .iter()
                    .map(|c| match c {
                        Value::Nil(Some(_)) => Value::Nil(None),
                        Value::NilPointer(s) if !s.is_empty() => Value::NilPointer(String::new()),
                        other => other.clone(),
                    })
                    .collect();
                Some(HeapValue::Closure(gid.clone(), caps))
            }
            _ => None,
        };
        if let Some(v) = erased {
            state.heap.set(id, v);
        }
    }
    state
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
    let RemPrecision::Bits(bits) = rem_precision_from_env() else {
        return vec![state];
    };
    if bits == 0 {
        return vec![state];
    }
    // rem boundary intervals are single-bucket-width by construction
    // (they enter the frame as one bucket and the move only shifts and
    // renormalizes them), so a straddle spans at most TWO buckets.
    split_marked_straddles(state, "player_rem_xy", 0x1_0000 >> bits, 2)
}

/// `split_rem_straddles` for the spd rung: one lane per occupied spd
/// bucket (plans/spd-rung.md). spd boundary intervals are one bucket
/// wide plus at most a frame's worth of accel drift, so the span cap is
/// generous rather than tight; hitting it means the physics moved spd
/// intervals in a way the design did not price, and that should FAIL,
/// not widen silently.
pub fn split_spd_straddles(state: State) -> Vec<State> {
    let SpdPrecision::WidthLog2(w) = spd_precision_from_env() else {
        return vec![state];
    };
    split_marked_straddles(state, "player_spd_xy", 1i32 << w, 8)
}

/// Both boundary straddle splits, composed - the one the boundary
/// pipeline calls.
pub fn split_precision_straddles(state: State) -> Vec<State> {
    split_rem_straddles(state)
        .into_iter()
        .flat_map(split_spd_straddles)
        .collect()
}

/// Split lanes whose interval on any `mark`-ed cell straddles a
/// floor-aligned bucket of `width` into one lane per covered bucket,
/// clipping each to its bucket. Generalizes the historic two-bucket rem
/// split: the first emitted state is EVERY lane clipped to its first
/// bucket, then one state per further bucket depth holding only the
/// lanes that reach it - for a two-bucket straddle this is bit for bit
/// the old (low, high) pair, in the same order, which the room (1,0)
/// f30 byte-identity gate checks.
fn split_marked_straddles(state: State, mark: &str, width: i32, max_span: i32) -> Vec<State> {
    let bucket_low = |n: Pico8Num| (n.as_raw_u32() as i32).div_euclid(width) * width;
    let from_raw = |r: i32| Pico8Num::from_parts((r >> 16) as i16, r as u16);

    let marks = mark_heap(&state);
    let Some(cells) = marks.marks.get(mark).cloned() else {
        return vec![state];
    };

    let mut work = vec![state];
    for cell in cells {
        let mut next = Vec::with_capacity(work.len());
        for state in work {
            let Some(HeapValue::Value(Value::NumberInterval(mv))) = state.heap.get_opt(cell) else {
                // Numbers (and anything else) lie in a single bucket.
                next.push(state);
                continue;
            };
            let intervals: Vec<Pico8NumInterval> = match mv {
                MaybeVector::Scalar(iv) => vec![*iv; state.vector_size.max(1)],
                MaybeVector::Vector(ivs) => ivs.iter().copied().collect(),
            };
            // Buckets covered per lane, 1-based.
            let spans: Vec<i32> = intervals
                .iter()
                .map(|iv| {
                    let span = (bucket_low(iv.high) - bucket_low(iv.low)) / width + 1;
                    assert!(
                        span <= max_span,
                        "{} interval {:?} spans {} buckets of width {:#x} (cap {})",
                        mark,
                        iv,
                        span,
                        width,
                        max_span
                    );
                    span
                })
                .collect();
            let deepest = spans.iter().copied().max().unwrap_or(1);
            if deepest == 1 {
                next.push(state);
                continue;
            }
            // Depth 1: every lane, clipped to its FIRST bucket.
            let clipped_first: Vec<Pico8NumInterval> = intervals
                .iter()
                .zip(&spans)
                .map(|(iv, span)| {
                    if *span > 1 {
                        Pico8NumInterval::new(iv.low, from_raw(bucket_low(iv.low) + width - 1))
                    } else {
                        *iv
                    }
                })
                .collect();
            let mut first_state = state.clone();
            first_state.heap.set(
                cell,
                HeapValue::Value(Value::NumberInterval(MaybeVector::vector(clipped_first))),
            );
            next.push(first_state);
            // Depth k >= 2: only the lanes whose interval reaches bucket k,
            // clipped to it.
            for depth in 2..=deepest {
                let reaches: Vec<bool> = spans.iter().map(|s| *s >= depth).collect();
                let depth_state = state
                    .filter_by_mask_clone(&reaches, crate::interpreter::state::FILTER_SPLIT_FLR);
                let clipped: Vec<Pico8NumInterval> = intervals
                    .iter()
                    .zip(&spans)
                    .filter(|(_, s)| **s >= depth)
                    .map(|(iv, span)| {
                        let start = bucket_low(iv.low) + (depth - 1) * width;
                        let low = if depth == 1 { iv.low } else { from_raw(start) };
                        let high = if depth == *span {
                            iv.high
                        } else {
                            from_raw(start + width - 1)
                        };
                        Pico8NumInterval::new(low, high)
                    })
                    .collect();
                let mut depth_state = depth_state;
                depth_state.heap.set(
                    cell,
                    HeapValue::Value(Value::NumberInterval(MaybeVector::vector(clipped))),
                );
                next.push(depth_state);
            }
        }
        work = next;
    }
    work
}

/// Widen player.spd.x/y to floor-aligned buckets of width 2^w raw
/// (plans/spd-rung.md). Exact is a no-op. The sanity bound is NOT
/// load-bearing - floor-aligned buckets cover every representable value
/// - it exists to catch a heap-shape bug loudly rather than bucket
/// garbage.
pub fn make_state_abstract_spd(mut state: State, precision: SpdPrecision) -> State {
    let SpdPrecision::WidthLog2(w) = precision else {
        return state;
    };
    let width: i32 = 1i32 << w;
    let sane = Pico8NumInterval::new(Pico8Num::from_parts(-16, 0), Pico8Num::from_parts(16, 0));
    let bucket = |raw_low: i32| -> Pico8NumInterval {
        let from_raw = |r: i32| Pico8Num::from_parts((r >> 16) as i16, r as u16);
        Pico8NumInterval::new(from_raw(raw_low), from_raw(raw_low + width - 1))
    };
    let floor_of = |n: Pico8Num| (n.as_raw_u32() as i32).div_euclid(width) * width;
    let widen_number = |n: Pico8Num| -> Pico8NumInterval {
        assert!(
            sane.contains_number(n),
            "player spd {:?} outside +/-16 px/frame",
            n
        );
        bucket(floor_of(n))
    };
    let widen_interval = |iv: &Pico8NumInterval| -> Pico8NumInterval {
        assert!(
            sane.contains_interval(iv),
            "player spd interval {:?} outside +/-16 px/frame",
            iv
        );
        Pico8NumInterval::new(bucket(floor_of(iv.low)).low, bucket(floor_of(iv.high)).high)
    };

    let marks = mark_heap(&state);
    if let Some(heap_ids) = marks.marks.get("player_spd_xy") {
        for &heap_id in heap_ids {
            if let HeapValue::Value(value) = state.heap.get(heap_id) {
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
                    Value::NumberInterval(MaybeVector::Vector(intervals)) => Value::NumberInterval(
                        MaybeVector::vector(intervals.iter().map(widen_interval).collect()),
                    ),
                    other => panic!(
                        "Unexpected value type for player_spd at {:?}: {:?}",
                        heap_id, other
                    ),
                };
                state.heap.set(heap_id, HeapValue::Value(new_value));
            }
        }
    }
    state
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
    //
    // The bob POSITION goes with it. `off` is read in exactly one place -
    // fruit.update's `this.y = this.start + sin(this.off/40)*2.5` (the read
    // census in `apply_conservative_widenings`) - so `y` is the one field
    // derived from it, and widening one without the other does not produce
    // a state the coarse level can have. That asymmetry is not academic:
    // the exact level's `y` is one of the 40 concrete bob positions, so its
    // coarsened row (concrete y, widened off) matches no row of the coarse
    // level (interval y, widened off), and the band filter drops the lane
    // as an "unknown coarse row". Room (2,0) has a fruit alive from room
    // load in EVERY lane, and before this the k16 level came out empty at
    // frame 2 and refuted every horizon. `sin` is in [-1, 1] by definition,
    // so the band is start +/- 2.5 - which is also, bit for bit, what the
    // coarse levels' own interval arithmetic puts there.
    {
        let helper = StateHelper::new(&state);
        let mut off_cells: Vec<HeapId> = Vec::new();
        let mut bob_cells: Vec<(HeapId, HeapId)> = Vec::new();
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
                off_cells.push(
                    *obj.get("off")
                        .unwrap_or_else(|| panic!("fruit-off widening: fruit has no `off` field")),
                );
                let field = |name: &str| {
                    *obj.get(name).unwrap_or_else(|| {
                        panic!("fruit-off widening: fruit has no `{}` field", name)
                    })
                };
                bob_cells.push((field("y"), field("start")));
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
        // sin(off/40) * 2.5, for an unknown phase: the whole bob band.
        let amplitude = Pico8Num::from_parts(2, 0x8000);
        let band_of =
            |start: &Pico8Num| Pico8NumInterval::new(*start - amplitude, *start + amplitude);
        for (y_cell, start_cell) in bob_cells {
            let bands: MaybeVector<Pico8NumInterval> = match state.heap.get(start_cell) {
                HeapValue::Value(Value::Number(MaybeVector::Scalar(start))) => {
                    MaybeVector::Scalar(band_of(start))
                }
                HeapValue::Value(Value::Number(MaybeVector::Vector(starts))) => {
                    MaybeVector::vector(starts.iter().map(band_of).collect())
                }
                other => panic!("fruit-off widening: start is not a number: {:?}", other),
            };
            // The widening must only ever grow the value it replaces.
            let old_y = match state.heap.get(y_cell) {
                HeapValue::Value(value) => value.clone(),
                other => panic!("fruit-off widening: y is not a value: {:?}", other),
            };
            let band_of_lane = |i: usize| match &bands {
                MaybeVector::Scalar(band) => *band,
                MaybeVector::Vector(bands) => bands[i],
            };
            for i in 0..state.vector_size {
                let contained = match &old_y {
                    Value::Number(MaybeVector::Scalar(n)) => band_of_lane(i).contains_number(*n),
                    Value::Number(MaybeVector::Vector(ns)) => {
                        band_of_lane(i).contains_number(ns[i])
                    }
                    Value::NumberInterval(MaybeVector::Scalar(iv)) => {
                        band_of_lane(i).contains_interval(iv)
                    }
                    Value::NumberInterval(MaybeVector::Vector(ivs)) => {
                        band_of_lane(i).contains_interval(&ivs[i])
                    }
                    other => panic!("fruit-off widening: y is not numeric: {:?}", other),
                };
                assert!(
                    contained,
                    "fruit-off widening: lane {} of y {:?} is outside the bob band {:?}",
                    i,
                    old_y,
                    band_of_lane(i)
                );
            }
            state
                .heap
                .set(y_cell, HeapValue::Value(Value::NumberInterval(bands)));
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
                            ns.iter()
                                .map(|n| if *n < zero { zero } else { *n })
                                .collect(),
                        ),
                    };
                    state
                        .heap
                        .set(heap_id, HeapValue::Value(Value::Number(clamped)));
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
                        assert!(
                            all_ok,
                            "fruit-off pin: widened off outside [0, 40]: {:?}",
                            mv
                        );
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
        let cell = state.global_env.get(name).copied().unwrap_or_else(|| {
            panic!(
                "timer global {} missing - pinning would silently not apply",
                name
            )
        });
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

    // NOTE on `has_dashed` (2026-08-15): it is gameplay-dead in any room
    // without a `fly_fruit` (its only read is fly_fruit.update's
    // `if has_dashed`), and pinning it looked like a free way to merge the
    // dashed and never-dashed copies of every state. MEASURED on room
    // (2,0): the pin changes the frame-50 frontier from 5,507,770 lanes to
    // 5,507,769 - one lane. By the time the frontier is large, every state
    // in it has dashed. Not worth a per-room soundness argument.

    state
}

/// Per-lane `(room.x, room.y)`. Object coordinates are ROOM-LOCAL, so a
/// room transition wraps the player's x from ~128 back to ~0; anything that
/// measures distance between two frames has to add `room * 128` first or
/// that one frame looks like a 128-pixel teleport.
pub fn room_xy_per_lane(state: &State) -> Option<Vec<(i16, i16)>> {
    let helper = StateHelper::new(state);
    let table_id = helper.unwrap_pointer(helper.load(helper.find_global("room")?))?;
    let HeapValue::ObjectTable(room) = helper.load(table_id) else {
        return None;
    };
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
/// (the position column); unexpected shapes return `None`
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
    let HeapValue::ObjectTable(obj) = helper.load(obj_id) else {
        return None;
    };
    let axis = |name: &str| -> Option<Vec<i16>> {
        let cell = *obj.get(name)?;
        let HeapValue::Value(Value::Number(n)) = helper.load(cell) else {
            return None;
        };
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

    /// The spd buckets (plans/spd-rung.md): static floor-aligned coverage
    /// including negatives, power-of-two NESTING (finer bucket inside
    /// exactly one coarser bucket - what makes band coarsening
    /// straddle-free and coarse levels over-approximations), and the
    /// 1 px width at w=16.
    #[test]
    fn spd_buckets_are_static_nested_and_cover_negatives() {
        let bucket = |raw: i32, w: u8| -> (i32, i32) {
            let width = 1i32 << w;
            let low = raw.div_euclid(width) * width;
            (low, low + width - 1)
        };
        // 1 px buckets at w=16: -0.3 px/f lands in [-1 px, -epsilon].
        let neg = -(0x0000_4CCC_i32); // ~ -0.3 in 16.16
        let (lo, hi) = bucket(neg, 16);
        assert_eq!(lo, -0x1_0000, "negative speeds floor-align DOWN");
        assert_eq!(hi, -1);
        // Zero sits at the bottom of [0, 1px).
        assert_eq!(bucket(0, 16), (0, 0xFFFF));
        // Nesting: every w=16 bucket lies inside exactly one w=17 bucket.
        for raw in [-0x2_8000i32, -0x1_0000, -1, 0, 0x7FFF, 0x1_2345, 0x5_0000] {
            let (lo16, hi16) = bucket(raw, 16);
            let (lo17, hi17) = bucket(raw, 17);
            assert!(
                lo17 <= lo16 && hi16 <= hi17,
                "w=16 bucket of {:#x} not nested",
                raw
            );
            // And the coarse bucket of the fine bucket's endpoints agrees.
            assert_eq!(bucket(lo16, 17), (lo17, hi17));
            assert_eq!(bucket(hi16, 17), (lo17, hi17));
        }
    }

    /// `coarser_or_equal` is the guard on every artifact one ladder level
    /// shares with another (the position graph today), so it must mean
    /// BUCKET CONTAINMENT and not just "a smaller number". Checked against
    /// the actual widening arithmetic: both ladders are floor-aligned
    /// power-of-two buckets, rem at raw width 2^(16-k) and spd at 2^w, so
    /// containment is nesting in the same sense the test above checks.
    #[test]
    fn coarser_or_equal_means_the_buckets_nest() {
        let bucket = |raw: i32, w: u8| -> (i32, i32) {
            let width = 1i32 << w;
            let low = raw.div_euclid(width) * width;
            (low, low + width - 1)
        };
        let probes = [
            -0x2_8000i32,
            -0x1_0000,
            -0x4CCC,
            -1,
            0,
            0x1234,
            0x7FFF,
            0x1_2345,
        ];
        for a in RemPrecision::all() {
            for b in RemPrecision::all() {
                let (RemPrecision::Bits(ka), RemPrecision::Bits(kb)) = (a, b) else {
                    // Exact is a point, so it nests in everything and
                    // contains only itself.
                    assert_eq!(
                        a.coarser_or_equal(b),
                        b == RemPrecision::Exact || a != RemPrecision::Exact
                    );
                    continue;
                };
                let nests = probes.iter().all(|&r| {
                    let (loa, hia) = bucket(r, 16 - ka);
                    let (lob, hib) = bucket(r, 16 - kb);
                    loa <= lob && hib <= hia
                });
                assert_eq!(a.coarser_or_equal(b), nests, "rem {:?} vs {:?}", a, b);
            }
        }
        for a in SpdPrecision::all() {
            for b in SpdPrecision::all() {
                let (SpdPrecision::WidthLog2(wa), SpdPrecision::WidthLog2(wb)) = (a, b) else {
                    continue;
                };
                let nests = probes.iter().all(|&r| {
                    let (loa, hia) = bucket(r, wa);
                    let (lob, hib) = bucket(r, wb);
                    loa <= lob && hib <= hia
                });
                assert_eq!(a.coarser_or_equal(b), nests, "spd {:?} vs {:?}", a, b);
            }
        }
    }

    /// `SpdPrecision::Exact` must leave any state bit-identical (it is the
    /// default; existing campaigns and gates depend on this being a no-op).
    #[test]
    fn spd_exact_is_a_no_op_and_split_passthrough() {
        // Cheap structural check without building a game state: the
        // widen function returns the input untouched for Exact...
        let state = State::new();
        let out = make_state_abstract_spd(state.clone(), SpdPrecision::Exact);
        assert_eq!(out.vector_size, state.vector_size);
        // ...and the env default (unset in tests) is Exact, so the
        // boundary splitter passes through.
        assert_eq!(spd_precision_from_env(), SpdPrecision::Exact);
        assert_eq!(split_spd_straddles(State::new()).len(), 1);
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
/// A checkpoint or a `g` produced under a synthetic win describes a
/// different search, and must never be resumable from, or comparable to, a
/// real campaign's. Read once.
pub fn synthetic_win_xy() -> Option<(i16, i16)> {
    static TARGET: std::sync::OnceLock<Option<(i16, i16)>> = std::sync::OnceLock::new();
    *TARGET.get_or_init(|| {
        let raw = std::env::var("CELESTE_WIN_AT_XY").ok()?;
        let (x, y) = raw
            .split_once(',')
            .unwrap_or_else(|| panic!("CELESTE_WIN_AT_XY must be \"x,y\", got {:?}", raw));
        let parse = |s: &str, which: &str| -> i16 {
            s.trim()
                .parse()
                .unwrap_or_else(|e| panic!("CELESTE_WIN_AT_XY {} coordinate {:?}: {}", which, s, e))
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

