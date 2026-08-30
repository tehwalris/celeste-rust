//! Read-only heap inspection: `StateHelper`, state summaries and JSONL
//! dumps for debugging/visualization, and the runner's simple checkpoint
//! format.
//!
//! Nothing in this module influences the search. The abstraction layer -
//! the widenings, rem buckets, straddle splits and shape/lane probes that
//! ARE proof-critical - lives in `super::abstraction`.

use std::collections::BTreeMap;
use std::fmt;
use std::hash::BuildHasherDefault;
use std::io::{BufRead, Write};

use rustc_hash::FxHasher;

type FxHashMap<K, V> = std::collections::HashMap<K, V, BuildHasherDefault<FxHasher>>;

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
use celeste_core::pico8_num::Pico8Num;

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
    pub fn get_objects_array_id(&self) -> Option<HeapId> {
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

// ============================================================================
// Full State Serialization (for debugging)
// ============================================================================

/// Dump a single state to JSON string
pub fn state_to_json(state: &State) -> serde_json::Result<String> {
    serde_json::to_string(state)
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
