# Held buttons: `p_jump` / `p_dash` unknown at every non-exact level (2026-09-16)

## Why

The player's `p_jump` and `p_dash` are only the previous frame's button
state, used to detect a PRESS (`lua/celeste-minimal.lua`):

```lua
this.p_jump=false                             -- init (74)
this.p_dash=false                             -- init (75)
local jump = btn(k_jump) and not this.p_jump  -- 109
this.p_jump = btn(k_jump)                     -- 110
local dash = btn(k_dash) and not this.p_dash  -- 112
this.p_dash = btn(k_dash)                     -- 113
```

(The only other `btn(k_jump)` is the title screen, 798.) Yet each doubles the
state count. Room (2,0), exact level 0, f68, 50,674,573 states
(`rewrite coarse-census --erase`):

| erased | states | factor |
|---|---|---|
| `p_jump` | 25,562,913 | 1.98x |
| `p_dash` | 26,428,610 | 1.92x |
| `grace`, `flip`, `djump`, `dash_effect_time` | 49.3M - 50.6M | 1.00x - 1.03x |
| `spd.x` and `spd.y` | 1,906,180 | 26.6x |

## The history, and why this is different

`abstraction.rs:1150-1156` records this widening REJECTED (Philippe): at the
boundary, a held trail made unknown admits e.g. a ground jump at n followed by
a wall jump at n+1 (the real game needs release, then press). That widening
applied at EVERY level, so nothing narrowed it back: the ladder's exact level
kept the spurious retrigger and could report a spurious optimum (CLAUDE.md,
"Never widen a field without a rung that narrows it back").

Here the EXACT rung keeps both fields exact. The widened levels over-approximate
(a held button may retrigger), which is sound for refuting a horizon, and the
exact rung refutes the spurious wins.

## Decisions

- `held = Unknown` at EVERY non-exact level: `r0sxh, r1sxh, ..., r15sxh, rxsx`.
  The first levels are wide spatial bands, and the reduction is worth most there.
- The extra kernel bodies (outcomes whose cone reaches `p_jump`/`p_dash`: x2 or
  x4 configurations) are accepted. Fusion hash-conses bodies, so configurations
  whose outcome does not depend on the flag should collapse into one; measured,
  not assumed.
- Opt-in through `CELESTE_LADDER` first. The default ladder and the three
  pinned room (1,0) gates stay as they are; making it the default is a
  separate step with a re-pin on evidence (CLAUDE.md).

## What the code does today (surveyed 2026-09-16)

- **A kernel cannot read an unknown bool input.** The packer turns `AV::UBool`
  into `false` (`asm_kernel.rs` `InputView::of` ~1178-1181, bool packing
  ~905-913), and `CellRepr::Bool` loads the known plane as the constant true
  (`codegen.rs:837-843`), so `Known(input cell)` is always true. Harmless while
  no input is unknown; silently wrong the moment one is.
- **Tracing the unknown straight through declines.** `jump = btn(k_jump) and
  not p_jump` is fine as Kleene logic, but the jump and no-jump arms have the
  same shape, so the tracer merges them into `Sel(jump, ..)` and conjoins
  `Known(jump)` into `ok` (`state.rs:294-320`). On an unknown lane with jump
  pressed that is live and not ok: a fatal decline. A sound join on undecided
  selects was tried and rejected (`plans/bucket-dispatch.md:40-44`).
- **Storage already handles `UBool`:** the queue's tri-state `TCol::Bool`, the
  checkpoint tag 3, `av_code(UBool)` distinct from true/false, and the kernel's
  `CellMix` of an unknown bool equal to it.
- **The emitted row refuses an undecided bool** (`BodyCols::push_row` assert,
  `asm_kernel.rs:1122-1135`; `BodyCols::of` bails on a uniform `UBool` column).
  Both cite a `widen::fork_bools` that no longer exists.
- **The registry keys on `(rem, spd, pos)`** (`registry_for`,
  `asm_kernel.rs:2072-2090`); `build_registry_for_rung` drops `pos` at
  `Bits(1..15)` and everything at rem Exact.
- **The mark filter only widens when the coarser rem is `Bits`**
  (`widened_keys_rt2`, `frame.rs:1330`).
- **The reference bridge maps any `UnknownBool` to `false`**
  (`refbridge.rs` `conv_value` ~118-121).

## Design

**The level coordinate.** `HeldPrecision { Unknown, Exact }` in `Level`, spec
suffix `h` after the speed token (`r0sxh`, `r1s16xh`). `Unknown` is coarser than
`Exact`; `Level::EXACT` has `Exact`; `parse_ladder` requires the last level
exact. A process atomic joins `set_level` / `current_level`; `Display` adds
`/H?`.

**Kernel output.** At a level with `held = Unknown`, both fields are written as
the uniform constant `AV::UBool` through `emit::bind`'s widen list
(`OutField::widen_uniform`), the route rem takes at level 0: out of the per-lane
key fold, in the template part. States that differ only in them become one row.
No graph op is needed: they are output-only values. `push_row` / `BodyCols::of`
accept `UBool` for widen-uniform cells only.

**Kernel input: a bool fork.** The input cell is declared possibly unknown
(a `ubool` input kind next to `ival`: `CellKind`, emit kind `"ubool"`,
`CellRepr::UBool` loading the known plane). At frame start `fork_bool_inputs`
(`widen.rs`, generic over the domain, the `fork_pos_inputs` pattern) replaces
the cell by `SplitBool(d)`, which resolves per configuration c to
`ConstBool(c)`, and conjoins into the guard the validity `SplitValidBool(d)`,
"this lane's value is unknown or equals c". Inside a body the flag is a
constant, `jump`/`dash` are decided, no `Known` premise survives, and both arms
are emitted as separate rows. A decided lane (the spawn's `false`) keeps only
its own configuration. Every place that lists fork ops takes the new ops:
`split_cones` / `choice_cones`, `specialize_subset_into`, the graph evaluator,
`inherently_bool`, the fork memo and arity.

**The constant lattice.** The two paths are excluded from `field_constants` /
`boundary_widened_paths` at such a level, so the lattice never pins them.

**The registry.** `held` joins the registry key and passes through
`build_registry_for_rung`, `WalkOpts`, `WidenMode` and `widen()`.

**The mark filter.** `Rt2::widen_to` gains a step setting both cells to `UBool`
(new `BoundaryIds` fields `f_p_jump`, `f_p_dash`), applied when the coarser
level has `held = Unknown`, and `widened_keys_rt2` stops gating every widening
on "coarser rem is `Bits`". Lane counts are unchanged, as the filter asserts.

**Collisions are unaffected:** each body runs with the flag constant; only
whether `jump`/`dash` fire branches.

## Steps

1. **Unknown bool inputs loud or correct.** `CellRepr::UBool`; `InputView` and
   `pack_input` pack known masks; the plain `Bool` repr refuses `UBool`;
   `refbridge` refuses `UnknownBool` for these fields. Remove the stale
   `fork_bools` / `partition_pm1` comments.
   *Check:* ASM bool unit tests with unknown input lanes; quick suite.
2. **The bool fork.** `SplitBool` / `SplitValidBool` everywhere fork ops are
   listed; `fork_bool_inputs`, called by `trace_frame` when `held = Unknown`.
   *Check:* a unit test that a frame traced with `p_jump` unknown emits exactly
   the union of the frames traced with it `false` and `true`.
3. **The output widening and the filter.** The widen list, the `push_row`
   relaxation, `widen_to` and `BoundaryIds`, the `widened_keys_rt2` gate.
   *Check:* `CELESTE_KERNEL_KEY_CHECK=1` on a room (1,0) `r0sxh` forward (the
   kernel's keys equal `Rt2::boundary`'s).
4. **The level coordinate, parse, registry key.**
   *Check:* parse and ordering tests; the three pinned gates unchanged.
5. **An oracle test with an assertion.** On sampled room (1,0) rows: the
   `r0sxh` kernel's output set equals the union of the exact-held kernel's
   outputs on both twins, widened with `widen_to`.
6. **Measure.**
   - Room (1,0), `CELESTE_LADDER="r0sxh,r1sxh,...,r15sxh,rxsx"`: states per
     level against today's ladder, level 0's first win against 99, and the
     ladder still reporting 99.
   - Room (2,0) `r0sxh` forward to f68 against exact level 0: states (census:
     about 4x), time per frame, peak memory (50.7M, 81 s, 28.7 GB).
   - Kernel build time and body counts at level 0, before and after.
7. **Update** `abstraction.rs:1150-1156` and CLAUDE.md's ladder section.

## Progress (2026-09-16, evening)

Built as designed, with one simplification and one fix:

- **The input fork has no validity premise.** At a held-unknown level every
  block writes the trails unknown, so forking both values for every lane is
  exact for those lanes and only over-approximates a decided one (the spawn's
  `false`). `widen::fork_held_inputs` emits `Op::SplitInt` over the literal
  `[0, 1]`, one fork per trail (not the memoized `fork_int`, which would have
  tied the two trails together); `Graph::fold` gained the exact `IntFrag` rule
  on a literal interval, so a body's trail is a constant.
- **The output columns.** `acc_template` initialized a column uniform only for
  a lowering constant (`konst_av`), so the trails were stored per row while the
  key folded them as uniform `UBool`: `CELESTE_KERNEL_KEY_CHECK` fired at room
  (1,0) f24. `widen_uniform` now takes precedence.
- Bool inputs that hold an unknown are refused (`InputView::of`, the packer),
  and the reference driver refuses held-unknown levels.

Measured:

- `CELESTE_KERNEL_KEY_CHECK=1` room (1,0) `r0sxh` forward to f30: no mismatch,
  no decline.
- The three pinned gates at the default ladder: identical. Quick suite: 124
  passed.
- Room (1,0) level 0, `r0sxh` against exact:

  | | f30 | f44 | f70 |
  |---|---|---|---|
  | exact kept | 11,774 | 515,997 | about 5.2M |
  | `r0sxh` kept | 5,061 | 141,424 | 1,381,187 |
  | factor | 2.33x | 3.65x | about 3.8x |

- Level 0's first win on room (1,0) stays f89, the same as exact level 0: the
  held retriggers did not make it win earlier.
- Room (1,0), `CELESTE_LADDER="r0sxh,r1sxh,...,r15sxh,rxsx"`, `--ceiling 99`:
  99 confirmed at every level (326 states marked at Exact), 98 refuted at
  level 6, OPTIMAL 99, 1m38s. The widened levels refute nothing the exact
  ladder did not.
- Room (2,0) level 0 to f68, release, 32 threads:

  | f68 | exact `r0sx` | `r0sxh` | factor |
  |---|---|---|---|
  | kept | 50,674,573 | 13,320,182 | 3.80x |
  | visited | 450,368,985 | 119,956,789 | 3.75x |
  | frame | 81.1 s | 16.0 s | 5.1x |
  | peak memory | 28.7 GB | 8.73 GB | 3.3x |
  | run to f68 | 10m12s | 2m21s | 4.3x |

  The factor is 3.78x at f60 and 3.80x at f68: a constant factor, not a slower
  curve.
- Room (2,0) `r0sxh` on to f80: growth does NOT slow, it rises.

  | frame | kept | growth | frame | peak |
  |---|---|---|---|---|
  | f68 | 13,320,182 | x1.114 | 15.3 s | 8.8 GB |
  | f72 | 21,159,387 | x1.128 | 28.8 s | 12.6 GB |
  | f76 | 36,405,697 | x1.150 | 58.8 s | 19.3 GB |
  | f80 | 62,810,806 | x1.158 | 135.0 s | 30.4 GB |

  517,549,244 states visited by f80; the run to f80 took 13m44s. Level 0 alone
  would reach roughly 400M states per frame by f94: surviving to the horizon
  needs states cut before it (a time band, a cost-to-go bound).
- **EXPERIMENT, the time band** (`CELESTE_BAND="94,8"`, `frame::band`): drop a
  player cell when even 8 px per frame up cannot reach the exit (y < -4) by
  f94, with one frame of slack. 8 px is the largest upward move in room
  (2,0)'s recorded transitions, a measurement and not a proven bound, so this
  shows level 0 CAN survive to the horizon, not that the cut is sound. Room
  (2,0) `r0sxh`, 90 GB cap:

  | frame | kept | frame | peak |
  |---|---|---|---|
  | f80 | 60,321,724 | 131.7 s | 30.0 GB |
  | f82 | 68,341,073 | 186.6 s | 35.6 GB |
  | f83 | 67,517,553 | 204.9 s | 37.5 GB |
  | f84 | 55,436,002 | 168.7 s | 37.5 GB |
  | f86 | 12,575,911 | 33.1 s | 37.5 GB |
  | f90 | 2,130,640 | 3.3 s | 37.5 GB |
  | f94 | 75,347 | 0.4 s | 37.5 GB |

  830,750,403 states visited; to f94 in 27m53s; level 0's first win f77. The
  permanent version is a bound grounded in the game code: minimum frames to
  the exit from our own kernels run from each cell (Philippe, 2026-09-16).
- **Room (2,0) search, OPTIMAL 95** (2026-09-16, 41m57s):
  `CELESTE_LADDER="r0sxh,r1sxh,...,r15sxh,rxsx" CELESTE_BAND="95,8"
  rewrite search --room 2,0 --ceiling 95`, 90 GB cap. h95 confirmed at every
  level (marked: level 0 19,245,834; level 5 1,154,364; level 15 7,280; Exact
  4,337). h94: level 0 marked 15,610,354, level 8 68,300, level 9 NO WIN ->
  refuted. The first band had also dropped rows that had already left the room
  (the next room's y read as far from the exit), refuting 95 at level 1; fixed
  in `a2c9e84` before this run.
  CAVEAT: the refutation of 94 rests on the band, and 8 px/frame is not a safe
  bound: a spring snaps the player up to 8 px (`hit.y=this.y-4` from the deepest
  overlap) and the player then moves with `spd.y=-3` in the same frame, 11 px.
  95 itself is the replayed TAS, confirmed through Exact.
  **CLOSED 2026-09-17**: rerun with no band under the level -1 filter
  (`CELESTE_LEVEL_MINUS_ONE="95,5"`, `c75f856`, plans/level-minus-one.md), a
  bound derived from the traced frames whose window premise is checked on every
  row: h95 confirmed through Exact, h94 refuted at level 9, **OPTIMAL 95**, in
  27m27.6s (table 472 s included) at 28.0 GB peak. h95's level-0 marks are the
  band run's exactly (19,245,834): the filter dropped nothing the backward used.

## Later

- **Dominance at the exact rung:** drop a held twin when its released twin is
  already visited. Sound with no spurious states; combining it with the ladder
  needs the mark filter to accept a fine state whose dominating twin's coarse
  key is marked.
