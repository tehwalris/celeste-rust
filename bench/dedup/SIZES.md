# Bytes per state, by representation (mental-model sheet; temporary)

Room (6,2) 100% (2300m), level r0sxhf, the visited set at the end of f57:
43.8M states (door f0-56 + f57's new). Sources: DESIGNS.md sections H, H2,
I, I2, J, "Compact". All exact (no merged states).

| layout | representation | B/state |
|---|---|---|
| production door | 16-B hash key + 4-B id, sorted base + index + delta | ~20 (+ overhead) |
| position array -> hash table | v3c: u128 hash key + id, load <= 0.5 | ~58 |
| | v4: 12-B packed slot, load 0.8 | 21.4 |
| | bitcell: (flags+dash, spd.x) -> spd.y mask | 8.0 |
| | compact: per-cell dictionaries, quotiented key, one entry a state | 2.75 |
| within one position, at rest | succinct trie | 1.55-1.68 (12.4-13.4 bits) |
| | gamma/varint over dictionary indices (room / per-cell dicts) | 1.46 / 1.09 (11.7 / 8.7 bits) |
| | zstd -19, each position alone | 0.35 (sample shards 0.18) |
| 8x8 regions, position as bitmask (posmask) | posmask8 hash table (the fast working form) | 1.6-1.7 |
| | sorted (key, mask) array | 0.60 |
| | varint key deltas + raw mask | 0.46 |
| | gamma over dictionary indices | 0.40-0.44 (3.2-3.5 bits) |
| | p8int (interned masks) | 0.37 |
| | sorted array + zstd -1 | 0.16 |
| | varint + zstd -1 / -3 | 0.06 / 0.05 |
| | zstd -19, each region alone | 0.037 |
| whole set at once | best sort order + zstd -19 (8 MB window) | 0.040 |

Scale:
- The same encoding per 8x8 region instead of per position is ~3-10x smaller
  (zstd 0.35 -> 0.037; integer coding ~1.1-1.5 -> ~0.4): the repetition across
  neighbouring positions lands inside one region.
- Fast working tables: 8-58 B/state per position against ~1.7 for posmask8
  (the sweep's live window: ~1.4 MB).
- Against production's door: ~12x smaller in the fast form, ~300x at rest
  (varint + zstd).

Edges, same frame (257.7M edges; DESIGNS.md K1, M), bytes per edge:

| encoding | raw | zstd -1 |
|---|---|---|
| production edge chunks | ~4.3 | - |
| flat, stable ids (region, entry, cell) | 4.76 (6.17 as built in M) | 1.37 (3.74) |
| per region pair, shift + cell mask (census / as built, 100k units) | 1.76 / 2.10 | 0.18 / 0.62 |
