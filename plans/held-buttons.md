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

## Later

- **Dominance at the exact rung:** drop a held twin when its released twin is
  already visited. Sound with no spurious states; combining it with the ladder
  needs the mark filter to accept a fine state whose dominating twin's coarse
  key is marked.
