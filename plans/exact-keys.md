# Exact keys: a state's identity is its packed field codes, not a hash (2026-10-10)

Branch `exact-keys` (from `develop` 1f8e4b1). Philippe: "We definitely want
the packed thing like we did in the prototype. Not hashing please."
Research: branch `emit-capture`, `bench/dedup/DESIGNS.md` H, H2, I, I2.

## What changes

Today a state is `(shape, key, cell)` with `key = (mix64(part1 + Σ
cell_mix), mix64(part2 + Σ cell_mix))`: a 128-bit HASH of its non-position
fields (`runtime2::boundary_finish`; the kernels' `AsmBody::key_words16`).
Two different states with equal hashes would be one state, silently. And
the shape is a 64-bit FxHash of the structure, also unchecked.

After: the key is an EXACT packed code. Two states of one shape get the same
key iff every field the hash covered (every `Cell2::Val` cell; a position
coordinate less its low end's whole pixels, `pos_code`) is equal - numbers,
interval bounds (both), unknown bools and numbers, strings, pointers,
nil, at every level. A shape's hash stays its NAME, but the tree stores the
shape's signature (what `shape_hash_of` hashes) and a second structure under
one name is FATAL.

## The key

**Field codes.** A value's exact code (`exact::Code`, ordered): numbers and
intervals as `(lo, hi)` raw 16.16 words (a point is `lo == hi`, so `Num(n)`
and `Ival(n, n)` are one code, as `av_code` has it); `Bool(b)`, `UBool`,
`UNum`, `Str(x)`, `Nil`, `Ptr(p)`, `NilPtr` as their own kinds. Position
coordinates code their `pos_code` value (fraction relative to the low end's
whole pixel; the cell holds the pixels).

**Dictionaries.** Per (shape, cell) a dictionary: code -> index, dense from
0, append-only. Indices never change once given.

**Packing: append-only bit runs (exact, never repacked).** Each field's
index is written in bits of the shape's 127-bit key space. A field whose
dictionary has `n` codes needs `bits(n - 1)` bits (0 for a constant). When a
dictionary grows past its field's bits, the field gets a NEW RUN at the
shape's next free bit (or its last run grows, when that run is the shape's
last). Bits once given never move, and an old index has zeros in the new
bits, so every key ever formed stays valid as the dictionaries grow: no
repacking, no layout versions, stored keys are final. Distinct fields own
disjoint bits and an index is below `2^bits`, so the key is injective on
index tuples, hence on code tuples. A shape needing more than 127 bits is
FATAL (`exact keys: shape ... needs N bits`), never truncated; the [fwd]
log reports the widest shape's bits.

Bit 127 is the PROVISIONAL tag (below), never a layout bit. `Key` stays
`(u64, u64)` (high, low), so ordering is the u128's.

Mixed radix (the prototype's flags digit) was weighed: it saves a few bits
but every dictionary growth changes the radix and forces a repack of every
stored key; bit runs never repack. The prototype's joint flags+dash digit
is a dictionary over a field GROUP; left as a follow-up once the bits are
measured (most shapes are expected far under 64).

**Hash only as a slot function.** Tables (`RegionTable`, the units' lid
tables, `Claims`, the provisional intern) hash the packed key to a slot
(`exact::slot_hash`) and compare keys exactly. A packed key is zero in its
high word for most shapes, so no table may use a key word as its hash.

## Determinism

The dictionaries are READ-ONLY during a wave (the visited set is). A
value the kernels emit that is not in its field's dictionary proves the
state NEW (no stored state holds it). Its key is PROVISIONAL: the exact
content `(shape, partial key over the fields that hit, sorted (cell, code)
of the fields that missed)` is interned in a wave-wide sharded table
(`exact::Provisional`) to an id; the key is `tag | id`. Equal content =>
equal id, so lids and claims stay exact. Ids depend on scheduling; nothing
keeps them.

At the TRANSLATION (start): the provisional states that are requested (a
filtered-out emission adds nothing) give each (shape, field) its new codes,
SORTED BY CODE and appended (indices `n..`); new bits are allocated in
(shape index, cell) order; every provisional key becomes its final packed
key (lids, requests, the units' row buffers). Then the translation runs as
today: requests sorted by (key, cell, ...), entries appended in key order.
The dictionaries after a frame are a function of the frame's new states, so
keys, entry numbers and ids are canonical (independent of threads, units,
claims, resume).

Frame 0 (`seed`): the initial rows' codes added sorted, then keyed.

## Persistence

`FrameMeta` (per frame, and per raise: `meta.r{seq}.bin`) gains:
- `side`: the storage region size S; every reader (resume, resolver)
  refuses a tree whose S differs from the process's;
- `shapes`: `(index, hash, signature)`;
- `codes`: `(shape index, cell, index, code)` - the dictionary additions,
  each with its explicit index;
- `runs`: `(shape index, cell, run number, first bit, bits)` - the bit runs
  given.
Explicit indices and bit positions make the replay order-free (a raise's
additions come after later frames' in time; its file sorts before them):
the reader checks density and disjointness. `checkpoint::FORMAT_VERSION`
12 -> 13 (frame files and every bincode value: old trees are refused).

The key column of the frame files and the entries' keys in the metadata
are the final packed keys.

## Consumers

| what | today | after |
|---|---|---|
| kernels' emission (`run_slice`) | `key_words16` + `mix64(part + h)` | per key field a dictionary lookup per lane, placed into the key; the outcome's uniform part keyed once per (worker, kernel, dictionary generation); any miss -> provisional |
| reference emission (`UnitSink::emit_row`) | the row's precomputed widened hash | the sink keys the row (held-widened, as before) with the tree's dictionaries |
| `RegionTable`, lids, claims | key word as hash | `slot_hash`, exact compare |
| translation, `resolve_lids`, `gather_layer` | hash keys | provisional -> final first |
| `seed`, resume (`restore_visited`), `Resolver` | keys from meta | dictionaries replayed from meta (`KeySpace::apply`) |
| `MarkFilter` (objects ladder), `diag-project` | coarse row hash | the projection keyed in the COARSE tree's key space (lookup only; a missing code = not marked) |
| marks `Visited` + marks files | (shape, key, cell) | plus the key space of the tree they name (saved in the file) |
| `NodeKeys`, `lookup_keys` (concrete search), `known`, `follow` | widened hash | widened row looked up in the tree's key space (`None`: not a node) |
| concrete search dedup (`dead`, `chunk_seen`, `seen`), `known`'s per-step seen | `row_keys_canonical` hash | `exact::ExactRow`: the canonical byte string of the UNWIDENED state (shape name + every key field's code), compared in full; the shape's signature checked in a process-wide registry |
| `KEY_CHECK` | kernel hash == boundary hash | kernel key == key of the boundary row in the frozen dictionaries; for a provisional key, its interned content == the boundary row's (partial, misses) |
| ckhash, the arc gate, `Visited::fingerprint` | hash of (key, cell) | hash of (shape, decoded codes, cell): a fingerprint over exact content (`ContentHash`, "As built") |
| `CELESTE_EMIT_CAPTURE` / `bench-storage` | keys | keys; a miss carries its provisional content so the replay re-interns it |

What still hashes, and why: the slot functions above (equality exact);
fingerprints (over exact data, as checks); the shape NAME (a 64-bit hash,
with the signature stored and compared, so a collision is fatal, not a
merge); the transfer raw-words cache (`xfer_id_raw`, equality exact).

## Verification

- Kept counts and pos graph identical to develop: room (1,0) r0sxh f0-62;
  (6,2) 100% r0sxhf to ~f45; (4,2) r0sxhn to ~f50; a platform room short.
- Bijection: a temporary diagnostic recomputes every stored row's OLD hash
  key (`row_keys_canonical` as on develop) beside the new packed key, over
  every row of those trees: one-to-one, per shape. Then removed.
- Then re-pin `gates/ckhash_room10_f000-044.txt` and
  `gates/arc_room10_win9-101_h35.txt` (fingerprints only; the counts, first
  win, optimum 33 and the witness identical), the known route `--prefer`.
- KEY_CHECK on short forwards, the quick suite, arc-check on the gate tree,
  room (1,0) `--ceiling 99` (same optimum and witness).
- A/B against develop on the (6,2) reference frame: wave time and the emit
  phase (`CELESTE_PHASES`), memory, tree size.

## For `fast-tests`

Any fixture pinning ckhash lines (f, e, d), `[gate]` fingerprints (marks,
W), `bench-storage` f/e lines, marks files, frame files or meta (format 13)
must be re-pinned on this branch; kept counts, pos graphs, optima and
witnesses do not move. `trajectory` keyed lines need `--tree`.

## As built (2026-10-10)

Differences from the design above:

- **Fingerprints hash CONTENT** (`exact::ContentHash`: each key's decoded
  codes), not the packed key. A raise appends its codes after later frames'
  codes, so a raised tree's packed keys differ from a fresh tree's for the
  same states (`gates/raise.sh` failed at h56/h58 on packed-key ckhash, pos
  graphs equal). ckhash (f, e, d), the arc gate, `Visited`'s fingerprints
  (the ladder's filtered-tree cache key) and `bench-storage` use it; the
  raise gate passes. Keys stay deterministic across threads and resume;
  across a raise only the content is.
- **The seed** keys frame 0's rows as the forward keys a reference row
  (held buttons widened), as develop did; the row is stored as it is.
- **Emission speed**: each dictionary slot holds its index already placed
  (the multi-run u128 shifts were the hot spot); fields of at most 4
  numbers are matched lane-parallel.
- **Diagnostics made exact too**: `coarse-census`, `col-census` and
  `spurious` compared u64 hashes of states/projections; they compare the
  codes or the projections themselves. `trajectory`'s keyed lines need
  `--tree` (the tree whose dictionaries the keys index).
- Key widths measured: every shape fits 64 bits so far - (1,0) r0sxh 36,
  (4,2) r0sxhn 33, (6,2) 100% r0sxhf 42 (f56), (2,1) r0sxhnp 30. `Key`
  stays 128-bit (16 B, as the hash was); a u64 key per shape is a
  follow-up (8 B an entry less), as are joint dictionaries over field
  groups (the prototype's flags+dash digit).

## Verification (all against develop 1f8e4b1)

- Kept counts, raw, visited and pos graph identical per frame: (1,0) r0sxh
  f0-62, (6,2) 100% r0sxhf f0-45, (4,2) r0sxhn f0-50, (2,1) r0sxhnp f0-30,
  and (6,2) to f56 (the A/B trees).
- Bijection (a temporary `rewrite exact-bijection`, removed): develop's hash
  keys recomputed on every row of the new trees reproduce develop's ckhash
  at every frame, and old <-> new is one-to-one per (shape, cell) and per
  (shape, region): 7,992,892 / 4,902,559 / 197,509 / 73,770 states, and on
  the gate tree 2,495,881 states and 272,034 entries, reproducing the old
  `gates/ckhash_room10_f000-044.txt` exactly.
- Gates re-pinned (fingerprints only): posgraph identical, per-frame counts,
  marks 11,576, every W count, arc optimum 33, the witness, the known route.
- KEY_CHECK clean: (1,0) r0sxh f0-40, (4,2) r0sxhn f0-45, (6,2) r0sxhf
  f0-36, (2,1) r0sxhnp f0-25. arc-check on the gate tree f1-35: 0 bad.
- 3 vs 32 threads and a resume (f35 -> f50): `ckhash --edges` identical.
  `gates/raise.sh 1,0 55 56 58` OK. Quick suite 176/176; ignored tests 3/3.
- Room (1,0) `--ceiling 99 --prefer`: OPTIMAL 99, the same witness, marks
  8,823,156 (develop the same).

## Measurements (load 20-50 from other agents: wall times are noise)

- Room (6,2) 100% f56 -> f57, `perf -e instructions:u`: `run_slice`
  (kernels + emission) 347G develop, 401G exact keys (+16%); the whole
  process 945G against 1001G (+6%). Wall wave 2.0-4.3 s both (interleaved,
  load 30-44); emit phase 27-58 worker-s both.
- Lids 3,795,565 -> 2,934,825 and 3.44 -> 3.15 B an edge at f57: entries are
  numbered in packed-key order, so neighbouring rows of the frontier (a
  unit) share targets more often.
- Trees: (6,2) f0-56 7.1 -> 6.8 GB; (1,0) `--ceiling 99` 50 -> 42 GB, peak
  13.6 -> 12.5 GB, wall 134 -> 157 s and user 1481 -> 2058 s before the
  emission optimizations (not re-timed after).

## What still hashes, and why

Slot functions with exact equality (`RegionTable`, lids, `Claims`,
`Provisional`, `FieldDict`'s number table, the transfer raw-words cache);
fingerprints (`ContentHash`, the pos graph, edges); a shape's NAME (its
64-bit hash, with the signature stored in the tree and compared - a
collision is fatal); the level -1 table's cache file name (its full key is
stored and compared).
