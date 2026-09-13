# Search UI

A phone-first web UI that shows how one `rewrite search` run played out:
the state space as the room's pixel grid, the set sizes, and where the
time went. Static: a Vite build plus the data exported from a finished
checkpoint tree, served by a small node server under `/celeste/` on port
3011 (see `UI-HOSTING.md` at the repo root). Live at
<https://taxw-ux.porgy-vimba.ts.net/celeste/>.

## What it shows

**Space** (`#space`) - the room, one cell per player pixel, brightness =
states in the cell (log scale), hue = the ladder level (blue = level 0,
through violet and magenta to orange = exact), marks in warm white.

The control model has four axes; every control is bound to exactly one:

| axis | what it is | control |
|---|---|---|
| **horizon** | the win frame H the ladder tested; everything on screen is at H - every level's forward run out to H, its marks from H's backward, levels the ladder never reached at H shown "not run" | the `h89 … h99` strip in the bottom bar (default: the winning horizon); `→ win` jumps back to it |
| **time** | the position in H's ladder: which pass (level × forward / backward, in the order they ran) and where inside it - a forward sweeps frames 0..H, a backward sweeps iterations H-1 down to 1 | the scrubber in the bottom bar (its backdrop is the passes, one band per level, backward half dimmed), `‹ ›` step a pass, Play sweeps and continues into the next horizon at the end, keyboard arrows / space |
| **grain** | how much of that position is shown at once: *Room* = the current pass, big; *Grid* = every level's panel while the same timeline is walked - only the current pass's level animates, levels already past their passes sit at their final state, levels not yet reached are blank (tap a panel to open it in Room); *Passes* = a step is a whole pass (a forward is its whole reached set, a backward its whole marked set) so the band is seen narrowing pass by pass; *3D* = the room as a WebGL scene you orbit (one finger), tilt from top-down to edge-on (drag up / down), pan and pinch (two fingers), double-tap to reset - *Columns* walks the step timeline with each cell a column as tall as its count (log; the accumulated set dim, the moving set as a bright cap on top; a backward's reached set is a flat slab under its marks), *Stack* walks the pass timeline with one layer of cubes per pass (a forward's reached set in the level's colour, a backward's marked set in warm white, the passes below the current one dimmed), so sweeping the passes stacks the ladder up into its pyramid. `src/view3d.ts` is the renderer (raw WebGL2, instanced boxes, no dependency; ~38k cubes for the full h99 stack) | the Room / Grid / Passes / 3D chips, and Columns / Stack in 3D |
| **look** (2D grains) | *Full*: the accumulated set dim under the moving set bright, the closed levels' marks as dark bands underneath, wins as rings; *Sweep only*: just what moves (the frontier, or the states this iteration marks); *Height map*: the levels collapsed into seven bands (L0 / L1 / L2–5 / L6–7 / L8–12 / L13–14 / L15+exact), each cell in the flat colour of the finest band whose set still contains it, forward and backward alike; the broad coarse bands dark and desaturated, lightness and chroma rising with the band so the exact route is the brightest thing on screen (the two thinnest bands get a one-cell halo); the moving set magenta. The last pass is the still that shows the bands narrowing; scrubbing paints them over in order | the Full / Sweep / Height map chips |

Playback speed is three presets (slow / normal / fast: 15, 60, 250
steps per second; Passes runs one, two, four passes per second), and
pacing is *uniform* (every step the same) or *real time* (each step's
share of the playback is its share of the run's logged time - the
level-0 passes crawl, the high-bit passes flick by - normalised so a
playthrough lasts as long as the uniform one at the same speed). The
bottom bar is fixed and one-thumb reachable; the room, then the grain /
look / speed chips and the legend, scroll above it. Press or drag on the
room to read a cell's counts.

A note on the backward: the checkpoint tree written by this run stores
each mark without its distance to the win, so a backward is shown as the
marked set split by the frame each state was first reached at, swept
from the horizon back to frame 1 (marked-so-far dim, this iteration's
layer bright) - the marks against the forward that produced them, not
the exact iteration order. A tree written by the current `Marks::save`
(which stores `dist`) exports `marks_have_dist: true`;
`hHHH_lLL.marks.bin` then carries the per-cell counts by distance.

**Sizes** (`#sizes`) - stat tiles (optimum, wall time, states visited,
re-runs), then per horizon: frontier size per frame per level, the
backward's marked / re-run / loaded / targets per iteration, the marked
set by layer, level 0's frontier vs visited over the whole run, and the
ladder as a table. Press and drag on a chart for the crosshair readout.
Log / linear y.

**Time** (`#time`) - the waterfall: x is time (the log's per-frame and
per-iteration totals laid end to end after the kernel prebuild; the log
has no wall clock), rows are horizon → level → phase → every frame /
iteration as a column split into emit / own / checkpoint / pos-graph (a
forward frame) or parallel / serial (a backward iteration). Pinch or
scroll to zoom, drag to pan, tap a block for its numbers, double-tap to
reset.

## Regenerating the data

The data is produced by `rewrite export-ui` (`src/search/ui_export.rs`)
from a finished run's checkpoint tree and its log. It reads only the
checkpoint headers (per-cell counts = the cell index, win cells = the win
list), the marks files, and - to split the marks by layer - each frame
file's `(cell, key)` rows. Room (1,0)'s full ladder to 99 exports in ~6 s
to 8.8 MB.

```bash
# 1. Keep the run's log somewhere stable (it is the timing source).
cp /tmp/room10f.log /var/tmp/celeste-ui/room10f.log

# 2. Export (from the repo root: it loads cart/ for the room's tiles).
./one-cargo.sh ./safe-run.sh -- cargo build --profile quick --bin rewrite
./safe-run.sh -- ./target/quick/rewrite export-ui \
    --checkpoint-dir /var/tmp/celeste-checkpoints \
    --log /var/tmp/celeste-ui/room10f.log \
    --out /var/tmp/celeste-ui/data --room 1,0
```

The output layout (`run.json` + one binary per horizon/level) is
documented at the top of `src/search/ui_export.rs`; `ui/src/data.ts` is
its reader.

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
gzipped, with `no-cache` on the data so a re-export shows on reload.
`npm run dev` runs Vite's dev server on the same port and prefix (stop
`serve.mjs` first).

`node_modules/`, `dist/` and `public/data` are git-ignored; nothing
generated is committed.
