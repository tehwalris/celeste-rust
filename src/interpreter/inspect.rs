//! Heap inspection utilities for abstract interpretation.
//! Used to mark and transform heap values (e.g., make player position abstract).
//! Also provides state summarization for debugging and visualization.

use std::collections::{BTreeMap, HashSet};
use std::fmt;
use std::hash::BuildHasherDefault;
use std::io::{BufRead, Write};

use rustc_hash::FxHasher;

type FxHashMap<K, V> = std::collections::HashMap<K, V, BuildHasherDefault<FxHasher>>;
type HashMap<K, V> = std::collections::HashMap<K, V>;

/// Error type for heap inspection operations
#[derive(Debug, Clone)]
pub enum InspectError {
    /// Expected an ArrayTable but got something else
    ExpectedArrayTable { heap_id: HeapId, actual: String },
}

impl fmt::Display for InspectError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InspectError::ExpectedArrayTable { heap_id, actual } => {
                write!(f, "expected ArrayTable at {:?}, got {}", heap_id, actual)
            }
        }
    }
}

impl std::error::Error for InspectError {}

use serde::{Deserialize, Serialize};

use super::{
    heap::HeapId,
    state::State,
    value::{HeapValue, MaybeVector, Value},
};
use crate::pico8_num::{Pico8Num, Pico8NumInterval};

// ============================================================================
// State Summary Types (for JSONL dumps)
// ============================================================================

/// A number that can be serialized to JSON (as a string to preserve precision)
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SerializableNum {
    /// The raw 32-bit fixed-point value
    pub raw: i32,
    /// Human-readable representation
    pub display: String,
}

impl From<Pico8Num> for SerializableNum {
    fn from(n: Pico8Num) -> Self {
        // Convert to human-readable fixed-point format
        let whole = n.whole_part_as_i16();
        let frac = n.fraction_part_as_u16();
        let display = if frac == 0 {
            format!("{}", whole)
        } else {
            // Convert fraction to decimal (approx)
            let frac_decimal = (frac as f64) / 65536.0;
            format!("{:.4}", (whole as f64) + frac_decimal)
        };
        Self {
            raw: n.as_raw_u32() as i32,
            display,
        }
    }
}

/// A number or interval
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum NumOrInterval {
    Number { value: SerializableNum },
    Interval { low: SerializableNum, high: SerializableNum },
}

/// Position (x, y) - can be concrete or interval
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Position {
    pub x: NumOrInterval,
    pub y: NumOrInterval,
}

/// Player state summary
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PlayerSummary {
    pub x: NumOrInterval,
    pub y: NumOrInterval,
    pub spd_x: NumOrInterval,
    pub spd_y: NumOrInterval,
}

/// Player spawn state summary
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PlayerSpawnSummary {
    pub x: NumOrInterval,
    pub y: NumOrInterval,
    pub state: NumOrInterval,
    pub delay: NumOrInterval,
}

/// Summary of a single state's key fields
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct StateSummary {
    pub object_count: usize,
    pub player: Option<PlayerSummary>,
    pub player_spawn: Option<PlayerSpawnSummary>,
}

/// A group of states that share the same shape
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct StateGroup {
    /// Shape identifier (hash of the shape for grouping)
    pub shape_hash: u64,
    /// Number of State objects in this group
    pub state_count: usize,
    /// Total vector size (sum of all state.vector_size)
    pub expanded_count: usize,
    /// Summaries for each state in this group
    pub summaries: Vec<StateSummary>,
}

/// Dump of all states for a single frame
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct FrameDump {
    pub frame: u32,
    /// Total State objects this frame
    pub total_states: usize,
    /// Total expanded count (sum of vector_size)
    pub total_expanded: usize,
    /// States grouped by shape
    pub groups: Vec<StateGroup>,
}

// ============================================================================
// State Summary Extraction
// ============================================================================

impl<'a> StateHelper<'a> {
    /// Extract a number from a heap value
    /// For vectors, computes the actual min/max range across all elements
    fn extract_num(&self, heap_id: HeapId) -> Option<NumOrInterval> {
        match self.load(heap_id) {
            HeapValue::Value(Value::Number(mv)) => {
                match mv {
                    MaybeVector::Scalar(n) => Some(NumOrInterval::Number {
                        value: (*n).into()
                    }),
                    MaybeVector::Vector(nums) if !nums.is_empty() => {
                        // Compute actual min/max of all vector elements
                        let min = nums.iter().min().copied().unwrap();
                        let max = nums.iter().max().copied().unwrap();
                        if min == max {
                            Some(NumOrInterval::Number {
                                value: min.into()
                            })
                        } else {
                            Some(NumOrInterval::Interval {
                                low: min.into(),
                                high: max.into(),
                            })
                        }
                    }
                    _ => None,
                }
            }
            HeapValue::Value(Value::NumberInterval(mv)) => {
                match mv {
                    MaybeVector::Scalar(interval) => Some(NumOrInterval::Interval {
                        low: interval.low.into(),
                        high: interval.high.into(),
                    }),
                    MaybeVector::Vector(intervals) if !intervals.is_empty() => {
                        // Compute actual min/max across all intervals
                        let min = intervals.iter().map(|i| i.low).min().unwrap();
                        let max = intervals.iter().map(|i| i.high).max().unwrap();
                        Some(NumOrInterval::Interval {
                            low: min.into(),
                            high: max.into(),
                        })
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }

    /// Extract a number from an object table field
    fn extract_field_num(&self, obj: &FxHashMap<String, HeapId>, field: &str) -> Option<NumOrInterval> {
        let heap_id = *obj.get(field)?;
        self.extract_num(heap_id)
    }

    /// Extract player summary from a player object
    fn extract_player_summary(&self, player_heap_id: HeapId) -> Option<PlayerSummary> {
        let player = match self.load(player_heap_id) {
            HeapValue::ObjectTable(obj) => obj,
            _ => return None,
        };

        let x = self.extract_field_num(player, "x")?;
        let y = self.extract_field_num(player, "y")?;

        // Get spd.x and spd.y
        let spd_ptr = player.get("spd")?;
        let spd_heap_id = self.unwrap_pointer(self.load(*spd_ptr))?;
        let spd = match self.load(spd_heap_id) {
            HeapValue::ObjectTable(obj) => obj,
            _ => return None,
        };
        let spd_x = self.extract_field_num(spd, "x")?;
        let spd_y = self.extract_field_num(spd, "y")?;

        Some(PlayerSummary { x, y, spd_x, spd_y })
    }

    /// Extract player_spawn summary from a player_spawn object
    fn extract_player_spawn_summary(&self, spawn_heap_id: HeapId) -> Option<PlayerSpawnSummary> {
        let spawn = match self.load(spawn_heap_id) {
            HeapValue::ObjectTable(obj) => obj,
            _ => return None,
        };

        let x = self.extract_field_num(spawn, "x")?;
        let y = self.extract_field_num(spawn, "y")?;
        let state = self.extract_field_num(spawn, "state")?;
        let delay = self.extract_field_num(spawn, "delay")?;

        Some(PlayerSpawnSummary { x, y, state, delay })
    }

    /// Get the objects array HeapId (dereferencing the global pointer)
    fn get_objects_array_id(&self) -> Option<HeapId> {
        let global_id = self.find_global("objects")?;
        match self.load(global_id) {
            HeapValue::Value(Value::Pointer(arr_id)) => Some(*arr_id),
            _ => None,
        }
    }

    /// Extract a full state summary
    pub fn extract_summary(&self) -> StateSummary {
        let objects_array_id = self.get_objects_array_id();

        // Count objects
        let object_count = objects_array_id
            .and_then(|arr_id| match self.load(arr_id) {
                HeapValue::ArrayTable(items) => Some(items.len()),
                _ => None,
            })
            .unwrap_or(0);

        // Find player
        let player = objects_array_id
            .and_then(|arr_id| {
                let players = self.find_objects_by_type(arr_id, "player").ok()?;
                players.first().copied()
            })
            .and_then(|id| self.extract_player_summary(id));

        // Find player_spawn
        let player_spawn = objects_array_id
            .and_then(|arr_id| {
                let spawns = self.find_objects_by_type(arr_id, "player_spawn").ok()?;
                spawns.first().copied()
            })
            .and_then(|id| self.extract_player_spawn_summary(id));

        StateSummary {
            object_count,
            player,
            player_spawn,
        }
    }
}

/// Extract a state summary from a state
pub fn extract_state_summary(state: &State) -> StateSummary {
    StateHelper::new(state).extract_summary()
}

/// Create a frame dump from a list of states
/// States are grouped by a simple shape hash (heap length + vector_size for now)
pub fn create_frame_dump(frame: u32, states: &[State]) -> FrameDump {
    let total_states = states.len();
    let total_expanded: usize = states.iter().map(|s| s.vector_size).sum();

    // Group states by a simple shape key (heap_len, vector_size)
    // In the future, use proper shape extraction
    let mut groups_map: BTreeMap<(usize, usize), Vec<(usize, StateSummary)>> = BTreeMap::new();

    for state in states.iter() {
        let key = (state.heap.len(), state.vector_size);
        let summary = extract_state_summary(state);
        groups_map.entry(key).or_default().push((state.vector_size, summary));
    }

    let groups: Vec<StateGroup> = groups_map
        .into_iter()
        .map(|((heap_len, _vec_size), entries)| {
            use std::hash::{Hash, Hasher};
            use std::collections::hash_map::DefaultHasher;

            let mut hasher = DefaultHasher::new();
            heap_len.hash(&mut hasher);
            let shape_hash = hasher.finish();

            let state_count = entries.len();
            let expanded_count: usize = entries.iter().map(|(vs, _)| *vs).sum();
            let summaries: Vec<StateSummary> = entries.into_iter().map(|(_, s)| s).collect();

            StateGroup {
                shape_hash,
                state_count,
                expanded_count,
                summaries,
            }
        })
        .collect();

    FrameDump {
        frame,
        total_states,
        total_expanded,
        groups,
    }
}

/// Write a frame dump as a JSONL line
pub fn write_frame_dump_jsonl(dump: &FrameDump, writer: &mut impl std::io::Write) -> std::io::Result<()> {
    serde_json::to_writer(&mut *writer, dump)?;
    writeln!(writer)?;
    Ok(())
}

/// Helper for navigating and inspecting heap structure
pub struct StateHelper<'a> {
    state: &'a State,
}

impl<'a> StateHelper<'a> {
    pub fn new(state: &'a State) -> Self {
        Self { state }
    }

    /// Get a global variable's heap ID
    pub fn find_global(&self, name: &str) -> Option<HeapId> {
        self.state.global_env.get(name).copied()
    }

    /// Load a heap value
    pub fn load(&self, id: HeapId) -> &HeapValue {
        self.state.heap.get(id)
    }

    /// Load a global as an object table
    pub fn load_global_object(&self, name: &str) -> Option<&FxHashMap<String, HeapId>> {
        let id = self.find_global(name)?;
        match self.load(id) {
            HeapValue::ObjectTable(table) => Some(table),
            _ => None,
        }
    }

    /// Load a global as an array table
    pub fn load_global_array(&self, name: &str) -> Option<&Vec<HeapId>> {
        let id = self.find_global(name)?;
        match self.load(id) {
            HeapValue::ArrayTable(items) => Some(items),
            _ => None,
        }
    }

    /// Get a pointer from a heap value (unwrap Value::Pointer)
    pub fn unwrap_pointer(&self, value: &HeapValue) -> Option<HeapId> {
        match value {
            HeapValue::Value(Value::Pointer(id)) => Some(*id),
            _ => None,
        }
    }

    /// Find objects in an array table that have a specific type.
    /// Returns an error if array_id doesn't point to an ArrayTable.
    pub fn find_objects_by_type(&self, array_id: HeapId, type_name: &str) -> Result<Vec<HeapId>, InspectError> {
        let items = match self.load(array_id) {
            HeapValue::ArrayTable(items) => items,
            other => return Err(InspectError::ExpectedArrayTable {
                heap_id: array_id,
                actual: format!("{:?}", other),
            }),
        };

        // Get the type function's heap ID by dereferencing the global
        let global_type_target = self.find_global(type_name)
            .and_then(|global_heap_id| match self.load(global_heap_id) {
                HeapValue::Value(Value::Pointer(target_id)) => Some(*target_id),
                _ => None,
            });

        let global_type_target = match global_type_target {
            Some(id) => id,
            None => return Ok(vec![]),  // Type not found is valid (no matches)
        };

        let mut results = Vec::new();
        for item_ptr in items {
            if let HeapValue::Value(Value::Pointer(obj_id)) = self.load(*item_ptr) {
                if let HeapValue::ObjectTable(obj) = self.load(*obj_id) {
                    if let Some(type_ptr) = obj.get("type") {
                        if let HeapValue::Value(Value::Pointer(type_heap_id)) = self.load(*type_ptr) {
                            // Check if this matches our type
                            if *type_heap_id == global_type_target {
                                results.push(*obj_id);
                            }
                        }
                    }
                }
            }
        }
        Ok(results)
    }
}

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

/// One-line description of the objects array (count + resolved type names),
/// for premise-failure diagnostics.
pub fn describe_objects(state: &State) -> String {
    let helper = StateHelper::new(state);
    let Some(arr_id) = helper.get_objects_array_id() else {
        return "objects: <no array>".to_string();
    };
    let HeapValue::ArrayTable(items) = helper.load(arr_id) else {
        return "objects: <not an array>".to_string();
    };
    let items = items.clone();
    let mut parts: Vec<String> = Vec::new();
    for item_ptr in &items {
        let type_target = match helper.load(*item_ptr) {
            HeapValue::Value(Value::Pointer(obj_id)) => match helper.load(*obj_id) {
                HeapValue::ObjectTable(obj) => obj.get("type").and_then(|type_ptr| {
                    match helper.load(*type_ptr) {
                        HeapValue::Value(Value::Pointer(t)) => Some(*t),
                        _ => None,
                    }
                }),
                _ => None,
            },
            _ => None,
        };
        let name = type_target
            .and_then(|t| {
                state.global_env.iter().find_map(|(name, gid)| {
                    match helper.load(*gid) {
                        HeapValue::Value(Value::Pointer(p)) if *p == t => Some(name.clone()),
                        _ => None,
                    }
                })
            })
            .unwrap_or_else(|| "?".to_string());
        parts.push(name);
    }
    format!("objects: {} [{}]", items.len(), parts.join(", "))
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

/// Per-lane (x, y) whole-pixel positions of the first object (the player or
/// the spawn animation), or `None` when the objects array is empty (dead
/// countdown states) or missing. Used by the per-coordinate saturation dump
/// (`CELESTE_XY_DUMP`); analysis-only, so unexpected shapes return `None`
/// rather than panicking - a missing histogram row is visible in the plot,
/// and this must not take a search down.
pub fn player_xy_per_lane(state: &State) -> Option<Vec<(i16, i16)>> {
    let helper = StateHelper::new(state);
    let arr_id = helper.get_objects_array_id()?;
    let HeapValue::ArrayTable(items) = helper.load(arr_id) else { return None };
    let first = *items.first()?;
    // One indirection per element: the array slot holds a pointer cell.
    let HeapValue::Value(Value::Pointer(obj_id)) = helper.load(first) else { return None };
    let HeapValue::ObjectTable(obj) = helper.load(*obj_id) else { return None };
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

// ============================================================================
// Full State Serialization (for debugging)
// ============================================================================

/// Dump a single state to JSON string
pub fn state_to_json(state: &State) -> serde_json::Result<String> {
    serde_json::to_string(state)
}

/// Dump a single state to pretty JSON string
pub fn state_to_json_pretty(state: &State) -> serde_json::Result<String> {
    serde_json::to_string_pretty(state)
}

/// Load a state from JSON string
pub fn state_from_json(json: &str) -> serde_json::Result<State> {
    serde_json::from_str(json)
}

/// Dump multiple states to JSONL (one state per line)
pub fn states_to_jsonl(states: &[State], writer: &mut impl std::io::Write) -> std::io::Result<()> {
    for state in states {
        serde_json::to_writer(&mut *writer, state)?;
        writeln!(writer)?;
    }
    Ok(())
}

/// Load multiple states from JSONL
pub fn states_from_jsonl(reader: impl std::io::BufRead) -> std::io::Result<Vec<State>> {
    let mut states = Vec::new();
    for line in reader.lines() {
        let line = line?;
        if line.trim().is_empty() {
            continue;
        }
        let state: State = serde_json::from_str(&line).map_err(|e| {
            std::io::Error::new(std::io::ErrorKind::InvalidData, e)
        })?;
        states.push(state);
    }
    Ok(states)
}

/// Dump states to a JSONL file
pub fn dump_states_to_file(states: &[State], path: &str) -> std::io::Result<()> {
    use std::fs::File;
    use std::io::BufWriter;
    let file = File::create(path)?;
    let mut writer = BufWriter::new(file);
    states_to_jsonl(states, &mut writer)?;
    writer.flush()?;
    Ok(())
}

/// Load states from a JSONL file
pub fn load_states_from_file(path: &str) -> std::io::Result<Vec<State>> {
    use std::fs::File;
    use std::io::BufReader;
    let file = File::open(path)?;
    let reader = BufReader::new(file);
    states_from_jsonl(reader)
}

// ============================================================================
// Checkpoint Serialization (for resumable runs)
// ============================================================================

/// Checkpoint data saved at frame boundaries
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Checkpoint {
    /// Frame number (after this frame was completed)
    pub frame: u32,
    /// All states at this point
    pub states: Vec<State>,
}

/// Save a checkpoint to a zstd-compressed JSONL file
pub fn save_checkpoint(checkpoint: &Checkpoint, path: &str) -> std::io::Result<()> {
    use std::fs::File;
    use std::io::BufWriter;

    let file = File::create(path)?;
    let encoder = zstd::stream::Encoder::new(file, 19)?; // High compression
    let mut writer = BufWriter::new(encoder);

    // Write frame number as first line
    serde_json::to_writer(&mut writer, &checkpoint.frame)?;
    writeln!(writer)?;

    // Write each state as subsequent lines
    for state in &checkpoint.states {
        serde_json::to_writer(&mut writer, state)?;
        writeln!(writer)?;
    }

    let encoder = writer.into_inner().map_err(|e| e.into_error())?;
    encoder.finish()?;
    Ok(())
}

/// Load a checkpoint from a zstd-compressed JSONL file
pub fn load_checkpoint(path: &str) -> std::io::Result<Checkpoint> {
    use std::fs::File;
    use std::io::BufReader;

    let file = File::open(path)?;
    let decoder = zstd::stream::Decoder::new(file)?;
    let reader = BufReader::new(decoder);

    let mut lines = reader.lines();

    // Read frame number from first line
    let frame_line = lines.next()
        .ok_or_else(|| std::io::Error::new(std::io::ErrorKind::InvalidData, "Empty checkpoint file"))??;
    let frame: u32 = serde_json::from_str(&frame_line)
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?;

    // Read states from remaining lines
    let mut states = Vec::new();
    for line in lines {
        let line = line?;
        if line.trim().is_empty() {
            continue;
        }
        let state: State = serde_json::from_str(&line)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?;
        states.push(state);
    }

    Ok(Checkpoint { frame, states })
}

/// Get checkpoint filename for a given frame
pub fn checkpoint_filename(frame: u32) -> String {
    format!("checkpoint_frame{:04}.jsonl.zst", frame)
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

    #[test]
    fn test_state_json_roundtrip() {
        // Create a simple state
        let mut state = State::new();
        state.vector_size = 3;

        // Serialize and deserialize
        let json = state_to_json(&state).expect("serialization failed");
        let restored = state_from_json(&json).expect("deserialization failed");

        assert_eq!(state.vector_size, restored.vector_size);
        assert_eq!(state.heap.len(), restored.heap.len());
    }
}
