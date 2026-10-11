# Rows from keys: frame files hold ids, rows are decoded (2026-10-11)

Branch `rows-from-keys` (from `develop` f35583a). Philippe: "Our in-memory
state storage already contains the full states, no? So there's no real
point storing those uncompressed. Investigate."

Answer: yes. With exact keys a stored row is a FUNCTION of its shape's
skeleton, its packed key and its cell, and the id names the key (the
entry's, in the storage metadata) and the cell. The raw value columns
(50-54 B a row), the key column (16 B) and the cell column (4 B) of every
frame file were redundant: 36-69% of a tree. Built here (format 14): a
frame file is the skeleton, the win rows and the id column; rows are
decoded when read, and the forward checks every piece decodes to its rows
exactly before it writes the ids.

## 1. Is a row determined by (shape, key, cell) and the dictionaries?

Every part of a stored row (`Rt2` piece, `checkpoint::save_block` v13):

| part | where it comes from |
|---|---|
| value cells (`Cell2::Val`), all of them | KEY FIELDS: `exact::ShapeKeys` has a dictionary per value cell of the shape's signature; the key holds each one's index (`FieldDict::index_in`, the packing inverted) |
| fields with one code (constants, `konst_av`, `widen_uniform`, dead timers, widened-to-uniform fields) | key fields with 0 bits: the dictionary's one code. `konst_av`/`widen_uniform` are off the kernels' per-lane FOLD only; the key takes them from its uniform prefix (`KEY_CHECK` pins key = boundary key) |
| widened fields (held buttons, remainder, the `n`/`f`/`p` widenings) | stored AT their widened value, and that value is what the key codes: nothing is lost that the tree had not merged already |
| the position object's `x`/`y` | the key holds the fraction relative to the low end's whole pixel (`Code::pos`); the whole pixels are the cell's less the room offset (`pos_graph::room_offset` of the row's own `room.x/y`, themselves key fields) |
| structure (objects, arrays, closures, builtins), globals | the shape (its signature, stored and compared) |
| closure captures | NOT in the signature (only their count), uniform per piece (`save_block` refuses otherwise): kept in the file header |
| the strings table, the non-value cells' uniform columns | per piece, in the header (bytes per file) |
| a numeric column's representation (`N`, `I`, uniform `Num`/`Ival`) | NOT in the key: `Num(n)` and `Ival(n, n)` are one code. Kept per piece and value cell in the header (`decode::Repr`); see "Gaps found" |
| uniform vs per-row column | a function of the values (the layer gather's `collapse_uniform`) |
| win flags | a function of the row (`frame::wins_of`); kept in the header as `(row, cell)` (bytes) |
| key column, cell column | the id's entry key (`storage::meta`) and the id's cell: redundant |
| ids | NOT derivable: they say WHICH states form layer f (the visited set's masks carry no layer). The one per-row datum left |

**The check** (commit ba23ae9, `rewrite decode-check`, format 13): every
stored row of every frame decoded from its ID alone (region -> shape and
entry -> key from the metadata, cell from the id) with its shape's skeleton
(the shape's FIRST file's header, so the skeleton is also shown per shape,
not per file), compared with the stored block exactly (values, column
kinds, row keys, structure with captures, globals, strings):

| fixture | level | rows | shapes | differ | decode ns/row | load ns/row |
|---|---|---|---|---|---|---|
| r10 | (1,0) `r0sxh` | 4,600,309 | 2 | 0 | 154-170 | 55-67 |
| r10arc | (1,0) `r0sxh`, a search | 4,600,098 | 2 | 0 | 187 | 78 |
| r42n | (4,2) `r0sxhn`, level -1 | 3,084,630 | 5 | 0 | 202 | 85 |
| r42x | (4,2) `r0sxh`, level -1 | 978,981 | 4 | 0 | 232 | 96 |
| r62h | (6,2) 100% `r0sxhf`, level -1 | 14,981,185 | 6 | 0 | 220 | 85 |
| r60s | (6,0) `r0sxhfp`, split frame | 1,859,603 | 16 | 0 | 237 | 98 |
| r62h57 | (6,2) 100% f0-f57 | 43,841,605 | 8 | 0 | 217 | 88 |

73.9M rows, 0 differ, frame 0 included (quick profile, one thread, load
7-60; the decoder is a first version: a typed column straight from the
index, ~150-240 ns a row against ~60-100 to gather the raw columns).

**Gaps found** (after the check, by the guard below and the test suite):

1. `Num(n)` vs `Ival(n, n)`: room (2,1) `r0sxhn` split frame, step 49
   (`split_frame_reaches_the_unsplit_frontier_at_a_near_level`): an interval
   column holding a point `[2, 2]`. The code merges them by design
   (producers disagree on column types); the representation is now kept per
   piece (`Repr`: numbers, intervals, other; one byte per numeric value
   cell in the header). None of the seven fixtures had one.
2. REFERENCE-engine emissions (`UnitSink::emit_row`: `forward --reference`
   and tests, never the search) were stored as the CONCRETE row but keyed
   after the widening (`row_key`: the always-on entries - remainder,
   timers, ... - and the held buttons): a v13 tree held, per state, whichever
   concrete row of the class its first claim had, and a resume stepped from
   it. Now the reference sink stores the WIDENED row it keys - the state the
   kernels' tree holds (`forward_extended_frame_by_frame_matches_fresh`
   failed on the guard, passes with it). **A semantic change of the
   reference forward, for Philippe:** it now steps from widened states, as
   the kernels do (the reference engine runs on widened rows already in
   `ref-check`/`arc-check`); before, it stepped from a concrete
   representative chosen by scheduling. Its keys/cells (the `ckhash`
   comparison with the kernels) were always the widened ones.
3. Frame 0 (`wave::seed`) stores the initial row as the engine made it
   and keys it held-widened (`known.rs`: "the tree holds the start as the
   engine made it"). It decodes exactly in every fixture and test (the
   initial row is a fixed point of that widening); the guard would say so
   if a room's were not.

## 2. Disk: what a tree is made of (format 13)

`frames/` split by column (header width x 16/8/4 B for keys/ids/cells, the
rest of the data region the raw value columns), the rest by directory:

| tree | total | edges | value cols | keys | ids | cells | meta | dropped | other |
|---|---|---|---|---|---|---|---|---|---|
| r10 (4.6M states) | 809 MB | 54.2% (95 B/state) | 28.7% (50 B) | 9.1% | 4.6% | 2.3% | 0.9% | - | 0.2% |
| r42n (3.1M) | 470 MB | 44.2% | 35.4% (54 B) | 10.5% | 5.3% | 2.6% | 1.1% | 0.2% | 0.6% |
| r42x (1.0M) | 145 MB | 41.2% | 36.5% (54 B) | 10.8% | 5.4% | 2.7% | 1.3% | 0.1% | 2.0% |
| r62h (15.0M) | 2.67 GB | 49.2% | 30.0% (53 B) | 9.0% | 4.5% | 2.2% | 0.7% | 4.1% | 0.2% |
| r60s (1.9M, split) | 192 MB | 19.5% | 49.3% (51 B) | 15.5% | 7.8% | 3.9% | 1.7% | - | 2.3% |
| r62h57 (43.8M) | 8.62 GB | 54.6% (107 B) | 25.8% (51 B) | 8.1% | 4.1% | 2.0% | 0.6% | 4.7% | 0.1% |
| mod room h170 (495M; another agent's tree, read only) | 92.4 GB | 55.8% (104 B) | 27.8% (52 B) | 8.6% | 4.3% | 2.1% | 1.0% | 0.4% | 0.1% |

Removable (value columns + keys + cells): **36-40%** of the big trees,
46-52% of the object-room fixtures, 69% of the platform fixture. Edges are
the rest: 3.0-3.1 B an edge (r60s 3.7), 20-30 edges a state.

General-purpose compression of what stays, measured on r62h57 f056
(5,857,035 states, 218,442,627 edges): the id column 8 B a row -> zstd -1
0.97, delta varint 1.10, both **0.20 B a row**; the edge files (owner
index + unit blocks) 687 MB = 3.14 B an edge -> zstd -1 120 MB (**0.55 B an
edge**), -3 109 MB (0.50). The research (bench/dedup/DESIGNS.md on
`emit-capture`): bundles (src entry, tgt entry, transfer, shift) -> cell
mask 1.63-2.10 B an edge raw, 0.10-0.62 with zstd -1 (K1, M, N2). (For
reference, the v13 frame files compress 30x with zstd -1: 448 MB -> 15 MB
at f056 - the redundancy this change removes.)

## 3. Who reads stored rows

| reader | what it reads | after |
|---|---|---|
| the next frame's frontier | IN MEMORY (`wave.next`, the layer gather); never the files | unchanged |
| resume, `bench-frame` (`ForwardState::at`) | the last frame's rows; every frame's ids (+ keys: a check, gone) | decoded against the restored visited set (`VisitedSet: TreeKeys`); r62h57 f56: see measurements |
| raise (`load_sources`) | the noted sources' rows of any frame | decoded; a raise works on any tree (it refused a `--trim-rows` one) |
| the backward, `arc_dp`, the marks, the concrete search | ids, widths, win rows; keys via `marks::Resolver` (metadata) | unchanged: never read values |
| known route (`known::check`) | the reference engine's own states; the report's "in the tree" looked rows up by key | the key from the resolver |
| export-ui | cell counts, win rows; marked keys | cells from ids; keys from the resolver |
| ckhash | it loaded every row to take keys and positions | (shape, key, cell) from the ids through the resolver: no row read, faster |
| arc-check, ref-check, rerun-row, widen-check, bounds-audit, follow, spurious, col-census, coarse-census, diag-project, the level -1 probe | rows by id or whole frames | decoded (`load_rows(.., tree)`; `tree_of` / `Resolver::load` once a command). None refuses a tree any more |

Decode cost: ~150-240 ns a row single-threaded in this first version
(above), against ~60-100 ns to gather the raw columns: a reader of all
rows pays ~2-3x the CPU, from a file 6-9x smaller.

## 4. The design (built: format 14)

- **Frame file** (`checkpoint.rs`): `magic | 14 | header | ids`. Header:
  width, structure (with captures), globals, strings, prints, shape hash,
  the non-value cells' columns, `numeric` (per value cell: numbers or
  intervals; `decode::Repr`), the win rows `(row, cell)`. Data: the id
  column, 8 B a row (binary search, `id_at`, the edge files' row ranges
  unchanged). `cell_at`/`row_cells`/`cell_counts` come from the ids.
- **Decode** (`storage::decode::decode_rows`): per value cell its codes
  from the key (a 0-bit field: one code, uniform), the column built in the
  piece's representation; then `room.x/y` from those, the position's whole
  pixels from the cell, its fraction from the key. `TreeKeys` gives an id's
  key: the resume's `VisitedSet` or a reader's `marks::Resolver`.
- **The guard** (`decode::check_piece`, in `checkpoint_frontier`): before
  a piece's ids are written its rows are decoded from their keys and cells
  and compared with the in-memory block exactly (`block_diff`); a
  difference is FATAL ("its rows would be stored lossy"). Every forward,
  every frame, every piece: the check above runs forever, not once.
- **`--trim-rows` is gone** (its flag, `checkpoint::trim`, the `trimmed`
  refusals in raise and `bounds-audit`): there is nothing left to trim, and
  every frame's rows are readable. `CELESTE_TRIM_ROWS` is refused like the
  other old variables, saying why.
- **The reference sink stores widened rows** (gap 2).
- **Format 13 -> 14**: old trees are refused (as every format change);
  fixtures rebuild (`tools/fixtures.sh ensure` does it). A format-13 tree
  converts in place: `rewrite convert-tree --dir D` (every tree under D,
  each in the room its settings record). Each v14 file is decoded back
  and compared with the v13 rows exactly, its stored keys and cells with
  its ids', before any file of the tree is replaced; a failure leaves the
  tree at 13. 2026-10-11: every format-13 tree on the machine but another
  agent's (42 trees: three private fixture copies, four t2 scratch dirs, a
  r62h copy; 244M rows), `ckhash --edges` (`--dropped`) identical before
  (v13 binary) and after, resumes (`bench-frame`) equal to their pins or
  their own next frame; a fixture copy's seven trees (73.7M rows) in 4-7 s
  at 0.6 GB peak.
- `rewrite decode-check` (the format-13 comparison) is gone with the stored
  rows it compared against; its result is the table above, and the guard
  plus `frame::tests::a_resumed_frontier_is_the_forwards_own` (every frame
  of a 36-frame room (1,0) forward: the decoded resume frontier equals the
  in-memory one, block for block, and the files are ~8 B a row) keep it.

Not done (follow-ups, measured above):

- **Compress the ids** (8 -> ~0.2-1.1 B a row: delta varint, or zstd per
  file). Needs `id_at`/`find_row` over blocks (a per-block index) for the
  edges' row ranges and the binary search; ~4% of a tree.
- **Edges** are now ~85-90% of a tree. zstd -1 over today's files alone
  gives 5.7x (0.55 B an edge); the bundled relation 0.1-0.6. The next
  storage step, by far the larger one.
- **Faster decode** if a reader needs it (resume and raise only touch one
  frame or the noted sources): SIMD per field, or decode only the cells a
  diagnostic projects.
- The in-memory frontier could likewise be the new entries' (key, cell)
  plus the skeletons, decoded per unit for the kernels; it is one frame,
  not the problem.

## Measurements (format 14; quick profile, load 30-60 from other agents)

Every fixture rebuilt at format 14 (private dir, `CELESTE_FIXTURES=
/var/tmp/rows-fixtures`) reproduces its pinned `ckhash --edges`
(`--dropped` under level -1) at every frame: states and edges identical.
`check.sh t1`, `t2`, `big` all ok against the unchanged pins (frame lines,
exact kernel metrics, KEY_CHECK, backward, storage replay, ref-check,
arc-check, widen-check, the room (1,0) gates, the objects ladder, the
platform room). The suite with the ignored tests: 195/195.

| tree | v13 | v14 | saved | frames/ v13 -> v14 |
|---|---|---|---|---|
| r10 | 808 MB | 484 MB | 40% | 353 -> 44 MB |
| r42n | 469 MB | 241 MB | 48% | 248 -> 30 MB |
| r42x | 144 MB | 72 MB | 50% | 81 -> 10 MB |
| r62h | 2,666 MB | 1,566 MB | 41% | 1,219 -> 139 MB |
| r60s | 191 MB | 59 MB | 69% | 150 -> 20 MB |
| r62h57 | 8,623 MB | 5,400 MB | 37% | 3,509 -> ~290 MB |

A tree is now its edges (85-95%). Expected on the mod room's 92.4 GB h170
tree: -35.6 GB (38.5%).

Costs (A/B against ba23ae9, interleaved, same load):

- **The guard** (`check_piece`, decoding every new row once): room (1,0)
  `forward --to 56`, the frames' `ckpt` summed: 274-317 ms -> 673-695 ms
  (+0.4 s on a 7-9 s forward, ~5%; a first version spent 1.1 s, comparing
  value by value). The reference frame r62h57 f56/f57 (first version): ckpt
  114-132 -> 224-265 ms of a 2.5-4.3 s frame.
- **A resume's load** (`bench-frame`, `ForwardState::at`): r62h f50 (2.2M
  rows) 0.45-0.71 s -> 0.97-1.07 s; r62h57 f56 (5.9M) 1.6-2.6 s (v13: 0.62 s
  in devloop.md, quieter machine). The frame's files decode one after the
  other: parallel per file would recover most of it (follow-up).
