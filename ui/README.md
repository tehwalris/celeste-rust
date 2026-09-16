# Search UI

A phone-first web UI that shows how a `rewrite search` run played out:
the state space as the room's pixel grid, the set sizes, and where the
time went. Static: a Vite build plus the data exported from each finished
checkpoint tree, served by a small node server under `/celeste/` on port
3011 (see `UI-HOSTING.md` at the repo root). Live at
<https://taxw-ux.porgy-vimba.ts.net/celeste/>.

The header carries the title with the run's headline numbers (optimum,
horizons tested, wall time), the **room switch** and the three **tabs**;
on a wide screen they share one row, on a phone the switch and the tabs
share the first and the numbers take a second. The switch is a native
select listing the runs `runs.json` names in GAME order (level index
`room.x%8 + room.y*8`), each with the altitude the game shows on entering
the room - `Room (0,0) · 100 m`, `Room (1,0) · 200 m`, `Room (4,0) · 500 m`.
The labels follow the original cart's `room_title.draw` (celeste.lua;
celeste-minimal.lua strips the drawing): room (3,1) is "old site", level
index 30 "summit", every other room `(1 + level_index) * 100 m`
(`data.ts`, `roomTitle`). The default run is still `runs.json`'s first
entry. Everything below is that run.

## The route

The hash is the whole state of what is on screen, so a link reproduces
it after a reload or on another phone:

```
#space                          the default run's Space tab
#room00/sizes                   another run, another tab
#space?h=98&t=120&g=grid&l=full the Space tab's own state
```

The Space tab writes `h` (the horizon's win frame), `t` (the step, when
stepping by frame) or `p` (the pass, when stepping by pass), `g` (grain),
`m` (3D mode), `l` (look), `s` (speed), `pc` (pacing); the Sizes tab
`h`, `y` (`lin`) and `bm` (the backward metric). Only values that differ
from the default are written, on every settled change (a scrub's end, a
button, a chip - not per frame while playing). An unknown run or tab
falls back to the default.

## What it shows

**Space** (`#space`) - the room, one cell per player pixel, brightness =
states in the cell (log scale against the level's largest cell over the
run, so one frame's brightness is comparable to the next), hue = the
ladder level (blue = level 0, through violet and magenta to orange =
exact), marks in warm white. Nothing is drawn over the room but the win
markers: the titles, the readout and the colour scale sit around it, so
the exits at the top edge and the spawn at the bottom stay visible.

The layout. On a phone: the **stage** card (the horizon picker, the pass
title, the room / grid / 3D view, the readout under it), then the
options, the **ladder** panel and the legend, with the **transport**
fixed to the bottom, one-thumb reachable. On a wide screen (>= 1024 x
620) the Space tab is an app frame that does not scroll: the stage fills
the height the header and the transport leave (the room is as big as
fits, never bigger than the viewport), the transport runs under it with
the scrubber as wide as the stage, and the options, ladder and legend
sit in a side column that scrolls on its own. In the Grid view the panel
count per row is chosen so every panel fits the stage.

Status lives in one place each: the stage's pass title names the *pass*
(level, phase, the level's verdict); the transport's status names the
*position* (the frame or iteration, the counts, the pass number); the
scrubber's bubble names the step while it is held; the readout under the
room names the probed cell (press and drag on a phone, hover with a
mouse), the colour scale, and the wins on screen. The ladder panel lists
every level of the horizon - its result (`win f76`, `no win by f75`,
`not run`) and marked-set size - with the current pass's level tagged
FWD / BWD; tap a level to jump to the start of its forward.

The control model has four axes; every control is bound to exactly one:

| axis | what it is | control |
|---|---|---|
| **horizon** | the win frame H the ladder tested; everything on screen is at H - every level's forward run out to H, its marks from H's backward, levels the ladder never reached at H shown "not run" | the horizon picker at the top of the stage: the OPTIMAL horizon first and the default, then the others highest first, grouped and labelled with their verdict (`h76 · optimal, every level wins`, `h75 · refuted at L9, no win by f75`, `confirmed` for a horizon above the optimum); the picker's dot and border wear the verdict colour. A refuted horizon shows no win by design. (`run.json` lists horizons in the order they RAN - a count-down from a ceiling runs 76 then 75 - so "the last one" is not the answer; `data.ts`, `defaultHorizon` / `horizonOrder`.) Switching horizon opens it at its last step |
| **time** | the position in H's ladder: which pass (level × forward / backward, in the order they ran) and where inside it - a forward sweeps frames 0..H, a backward sweeps iterations H-1 down to 1 | the scrubber (its backdrop is the passes, one band per level with its label, a backward's band dimmer with a warm hairline on top), the step buttons (one frame / iteration), the pass buttons (to the start of the previous / next pass), the ladder panel's rows (to a level's forward), Play (sweeps to the end of the horizon and stops there - the horizon only changes by the picker; from the end it starts over) |
| **grain** | how much of that position is shown at once: *Room* = the current pass, big; *Grid* = every level's panel while the same timeline is walked - only the current pass's level animates, levels already past their passes sit at their final state, levels not yet reached are blank (tap a panel to open it in Room); *Passes* = a step is a whole pass (a forward is its whole reached set, a backward its whole marked set) so the band is seen narrowing pass by pass; *3D* = the room as a WebGL scene you orbit (one finger), tilt from top-down to edge-on (drag up / down), pan and pinch (two fingers), double-tap to reset - *Columns* walks the step timeline with each cell a column as tall as its count (log; the accumulated set dim, the moving set as a bright cap on top; a backward's reached set is a flat slab under its marks), *Stack* walks the pass timeline with one layer of cubes per pass (a forward's reached set in the level's colour, a backward's marked set in warm white, the passes below the current one dimmed), so sweeping the passes stacks the ladder up into its pyramid. `src/view3d.ts` is the renderer (raw WebGL2, instanced boxes, no dependency; ~38k cubes for the full h99 stack) | the Room / Grid / Passes / 3D chips, and Columns / Stack in 3D |
| **look** (2D grains) | *Full*: the accumulated set dim under the moving set bright, the closed levels' marks as dark bands underneath; *Sweep*: just what moves (the frontier, or the states this iteration marks); *Height map* (the default): the levels collapsed into seven bands (L0 / L1 / L2–5 / L6–7 / L8–12 / L13–14 / L15+exact), each cell in the flat colour of the finest band whose set still contains it, forward and backward alike; the broad coarse bands dark and desaturated, lightness and chroma rising with the band so the exact route is the brightest thing on screen (the two thinnest bands get a one-cell halo); the moving set magenta. The last pass is the still that shows the bands narrowing; scrubbing paints them over in order | the Full / Sweep / Height map chips |

The view opens on the answer: the optimal horizon's ladder at its last
step in the Height map look - the exact route, brightest, over every
band the search narrowed through. Play from there sweeps from the top.

**Wins** are drawn in every look as a reticle (a white ring with four
ticks over a dark halo) around the cell a winning state left the room
from; the cell itself stays uncovered. A forward shows the wins found so
far, a backward (and a whole pass, and a finished grid panel) every win
by H - capped at H, since level 0's frames file is shared by every
horizon and holds wins past the earlier ones. The readout names each
(`win: 2 at (28, -2) from f76`), and probing the cell adds `won from
here`. The export places a won state where its player LEFT the room
(`ui_export.rs`, `read_level_frames`, 2026-09-16); an export made before
that put it at the next room's spawn, one room over - room (0,0)'s data
still does, at (136, 128) and (136, 124). No in-room state can have x >=
128 (the cart moves the player back inside at `x > 121`), so those cells
are neither painted nor marked, and the readout says how many won states
it left out: `N won states recorded in the next room by an old export:
exit cell unknown, not drawn`. Re-exporting the run fixes it.

Playback speed is three presets (slow / normal / fast: 15, 60, 250
steps per second; Passes runs one, two, four passes per second), and
pacing is *uniform* (every step the same) or *real time* (each step's
share of the playback is its share of the run's logged time - the
level-0 passes crawl, the high-bit passes flick by - normalised so a
playthrough lasts as long as the uniform one at the same speed). The
legend's "How to read this" holds the longer explanation and the keys:

| key | |
|---|---|
| `space` | play / pause |
| `←` `→` | one step (with shift: ten) |
| `[` `]` | previous / next pass |
| `home` `end` | start / end of the horizon |
| `1` `2` `3` | speed |

A note on the backward: the checkpoint tree written by this run stores
each mark without its distance to the win, so a backward is shown as the
marked set split by the frame each state was first reached at, swept
from the horizon back to frame 1 (marked-so-far dim, this iteration's
layer bright) - the marks against the forward that produced them, not
the exact iteration order. A tree written by the current `Marks::save`
(which stores `dist`) exports `marks_have_dist: true`;
`hHHH_lLL.marks.bin` then carries the per-cell counts by distance.

**Sizes** (`#sizes`) - stat tiles (optimum, wall time, horizons, states
visited, frames, iterations, re-runs), then per horizon: the ladder
curve (marked set and frontier peak per level), frontier size per frame
per level, the backward's marked / re-run / loaded / targets per
iteration, the marked set by layer; level 0's frontier vs visited over
the whole run; and the ladder as a table grouped by horizon. Press or
hover a chart for the crosshair readout. Log / linear y.

**Time** (`#time`) - the waterfall: x is time (the log's per-frame and
per-iteration totals laid end to end after the kernel prebuild; the log
has no wall clock), rows are horizon → level → phase → every frame /
iteration as a column split into emit / own / checkpoint / pos-graph (a
forward frame) or parallel / serial (a backward iteration). Drag across
the overview strip or the waterfall to zoom to a span, pinch or scroll
to zoom, tap a block for its numbers (a horizon / level / phase block
also zooms to itself), "back" pops the previous span, double-tap resets.

## Regenerating the data

The data directory (`/var/tmp/celeste-ui/data`) holds one subdirectory
per run and `runs.json`, the hand-written list of the runs to offer, in
order, the first one the default:

```json
{
  "runs": [
    { "id": "room10", "label": "Room (1,0)" },
    { "id": "room00", "label": "Room (0,0)" }
  ]
}
```

Each run's subdirectory (`id`) is produced by `rewrite export-ui`
(`src/search/ui_export.rs`) from that run's checkpoint tree and its log.
It reads only the checkpoint headers (per-cell counts = the cell index,
win cells = the win list), the marks files, and - to split the marks by
layer - each frame file's `(cell, key)` rows. Room (1,0)'s full ladder
to 99 exports in ~6 s to 8.8 MB; room (0,0)'s ladder to 93 (15
horizons, 64 levels) in ~20 s to 18 MB.

```bash
# 1. Keep the run's log somewhere stable (it is the timing source).
cp /tmp/room10f.log /var/tmp/celeste-ui/room10f.log
cp /tmp/room00_search.log /var/tmp/celeste-ui/room00.log

# 2. Export each run into its own subdirectory (from the repo root: it
#    loads cart/ for the room's tiles).
./one-cargo.sh ./safe-run.sh -- cargo build --profile quick --bin rewrite
./safe-run.sh -- ./target/quick/rewrite export-ui \
    --checkpoint-dir /var/tmp/celeste-checkpoints \
    --log /var/tmp/celeste-ui/room10f.log \
    --out /var/tmp/celeste-ui/data/room10 --room 1,0
./safe-run.sh -- ./target/quick/rewrite export-ui \
    --checkpoint-dir /var/tmp/celeste-search-room00 \
    --log /var/tmp/celeste-ui/room00.log \
    --out /var/tmp/celeste-ui/data/room00 --room 0,0

# 3. List it in /var/tmp/celeste-ui/data/runs.json (above).
```

A run's layout (`run.json` + one binary per horizon/level) is documented
at the top of `src/search/ui_export.rs`; `ui/src/data.ts` is its reader,
with every file fetched as `data/<run>/<file>`. A run that fails to load
shows an error card with the export command to rerun; a run whose
`run.json` lists no horizons says so.

## Building and serving

```bash
cd ui
npm install
ln -sfn /var/tmp/celeste-ui/data public/data   # for `npm run dev` only
npm run typecheck && npm run build              # -> dist/
systemd-run --user --scope -p MemoryMax=2G --quiet node serve.mjs &
curl -s localhost:3011/celeste/ | head
```

`serve.mjs` serves `dist/` under `/celeste/` and the data directory
(`DATA_DIR`, default `/var/tmp/celeste-ui/data`) under `/celeste/data/`,
gzipped. The hashed assets are immutable; `index.html` is never cached
(so a new build is never hidden by an old one); the data is
`no-cache` with `Last-Modified`, answering 304 to the browser's
`If-Modified-Since`, so a re-export shows on reload and an unchanged
multi-MB binary is not re-sent. Restart the server after editing
`serve.mjs`. `npm run dev` runs Vite's dev server on the same port and
prefix (stop `serve.mjs` first).

`node_modules/`, `dist/` and `public/data` are git-ignored; nothing
generated is committed.

## Source layout

```
src/main.ts      the shell: header, run switch, tabs, the hash route
src/ui.ts        DOM helpers, chips (segmented controls), the scrubber,
                 buttons, icons, key/value strips
src/style.css    the visual system (tokens at the top)
src/data.ts      the exported run's reader and the number formatters
src/space.ts     the Space tab (the four axes above)
src/room.ts      the room renderer: count grids composited over the tiles
src/color.ts     the level hues, ramps, height-map bands, phase colours
src/view3d.ts    the 3D grain's WebGL2 renderer
src/sizes.ts     the Sizes tab
src/chart.ts     the line chart
src/timeline.ts  the Time tab (the waterfall)
```
