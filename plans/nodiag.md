# No Diagonal Dashes% (2026-10-04)

`CELESTE_NODIAG=1` (src/trace/cart.rs): the cart's diagonal dash arm (`if
input~=0 then if v_input~=0 then`, inside `if this.djump>0 and dash`) raises
(`nil > 0`); a raise has no successor, so a diagonal dash start is not a move,
exactly, at every level, in the kernels and the reference engine alike. A dash
with no direction held (horizontal, facing) stays legal. The 30 community
nodiag TASes press no diagonal dash. A reference frame path that raises ends
in no state (`refdriver::run_frame_all`), not an error.

References: the community nodiag TAS replayed in the ORIGINAL cart at the
room's any% prologue offset (the earliest exit), our frame counting.

| room | any% (ours) | nodiag ref | ours | how |
|---|---|---|---|---|
| (4,2) 2100m | 71 | 71 (TAS21) | 71 | ladder (any% recipe, L-1 71,5): h70 refuted at level 7 - validation |
| (5,3) 3000m | 79 | 96 (TAS30, offset 31) | 96 (RECHECK PENDING) | arc lower bound 92; concrete DFS: none within 92-95, witness at 96 (PICO-8: exits during f96). The 'none within' runs used a memo keyed on the WIDENED key (unsound, fixed 2026-10-05) - re-running |
| (7,0) 800m | 84 | 99 (TAS8, offset 23) | - | |
| (4,3) 2900m | 85 | 111 (TAS29, offset 29) | **106 - 5 FASTER** | arc-only `rewrite search --level r0sxhn,r0sxh` (L-1 111,5): r0sxhn bound 104, no concrete win at 104; r0sxh bound 106, concrete witness at 106 (392 steps); exits during f106 on a real PICO-8 in celeste-minimal AND the original cart, seeds 0 / 0.5 / rnd. `tas/room_4_3_nodiag_frame_106.txt` |
| (1,1) 1000m | 94 | 94 (TAS10) | - | (validation, not run) |

## The concrete count-up (arc-search --witness)

The arc optimum over coarse objects is only a lower bound. The witness DFS is
EXHAUSTIVE (all 64 inputs, every `rnd` leaf, a fully explored concrete state
remembered per frame) and prunes only by W, which contains every concrete
winner - so "no witness within f" is a proof about the real game, and counting
f up from the arc optimum to the horizon, the first f with a witness is the
concrete optimum. Room (5,3): f92 1.3 s, f93 8 s, f94 48 s, f95 112 s, f96
276 s (100k concrete steps). The dead-state memo must key the EXACT concrete state (it keyed the
level-0 key until 2026-10-05: unsound, every 'none within' is rechecked).
The soundness also rests on the projection of a
concrete state onto a level-0 node matching the tree's keys (a mismatch would
prune a real winner); `rewrite follow` agrees along the spawn and first moves.
