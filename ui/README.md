# Search UI

A phone-first web UI that shows how a `rewrite search` run played out:
the state space as the room's pixel grid, the set sizes, and where the
time went. Static: a Vite build plus the data exported from each finished
checkpoint tree, served by a small node server under `/celeste/` on port
3011 (see `UI-HOSTING.md` at the repo root). Live at
<https://taxw-ux.porgy-vimba.ts.net/celeste/>.

The header carries the title with the run's headline numbers (optimum,
wall time, horizons), the **run switch** - `Room (1,0)` (the default) or
`Room (0,0)`, the runs `runs.json` lists - and the three **tabs**; on a
wide screen they share one row, on a phone the tabs take a second one.
Everything below is that run.

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
exact), marks in warm white. A colour scale in the room's corner names
the moving set's ramp and the count at its top.

The layout: the stage (the room, the grid of panels, or the 3D view) with
the **transport** under it - fixed to the bottom of a phone, one-thumb
reachable; in the flow on a wide screen, where the options and the
legend sit in a sidebar. Status lives in one place each: the caption on
the room names the *pass* (horizon, level, phase, its verdict); the
transport's status names the *position* (the frame or iteration, the
pass number, the counts); the scrubber's bubble names the step while it
is held.

The control model has four axes; every control is bound to exactly one:

| axis | what it is | control |
|---|---|---|
| **horizon** | the win frame H the ladder tested; everything on screen is at H - every level's forward run out to H, its marks from H's backward, levels the ladder never reached at H shown "not run" | the `h89 … h99` row in the transport (the winning horizon carries a dot; default); the target button jumps back to it |
| **time** | the position in H's ladder: which pass (level × forward / backward, in the order they ran) and where inside it - a forward sweeps frames 0..H, a backward sweeps iterations H-1 down to 1 | the scrubber (its backdrop is the passes, one band per level with its label, a backward's band dimmer with a warm hairline on top), the step buttons (one frame / iteration), the pass buttons (to the start of the previous / next pass), Play (sweeps and continues into the next horizon at the end; from the end of the last one it starts over) |
| **grain** | how much of that position is shown at once: *Room* = the current pass, big; *Grid* = every level's panel while the same timeline is walked - only the current pass's level animates, levels already past their passes sit at their final state, levels not yet reached are blank (tap a panel to open it in Room); *Passes* = a step is a whole pass (a forward is its whole reached set, a backward its whole marked set) so the band is seen narrowing pass by pass; *3D* = the room as a WebGL scene you orbit (one finger), tilt from top-down to edge-on (drag up / down), pan and pinch (two fingers), double-tap to reset - *Columns* walks the step timeline with each cell a column as tall as its count (log; the accumulated set dim, the moving set as a bright cap on top; a backward's reached set is a flat slab under its marks), *Stack* walks the pass timeline with one layer of cubes per pass (a forward's reached set in the level's colour, a backward's marked set in warm white, the passes below the current one dimmed), so sweeping the passes stacks the ladder up into its pyramid. `src/view3d.ts` is the renderer (raw WebGL2, instanced boxes, no dependency; ~38k cubes for the full h99 stack) | the Room / Grid / Passes / 3D chips, and Columns / Stack in 3D |
| **look** (2D grains) | *Full*: the accumulated set dim under the moving set bright, the closed levels' marks as dark bands underneath, wins as rings; *Sweep*: just what moves (the frontier, or the states this iteration marks); *Height map* (the default): the levels collapsed into seven bands (L0 / L1 / L2–5 / L6–7 / L8–12 / L13–14 / L15+exact), each cell in the flat colour of the finest band whose set still contains it, forward and backward alike; the broad coarse bands dark and desaturated, lightness and chroma rising with the band so the exact route is the brightest thing on screen (the two thinnest bands get a one-cell halo); the moving set magenta. The last pass is the still that shows the bands narrowing; scrubbing paints them over in order | the Full / Sweep / Height map chips |

The view opens on the answer: the winning horizon's ladder at its last
step in the Height map look - the exact route, brightest, over every
band the search narrowed through. Play from there sweeps from the top.

Playback speed is three presets (slow / normal / fast: 15, 60, 250
steps per second; Passes runs one, two, four passes per second), and
pacing is *uniform* (every step the same) or *real time* (each step's
share of the playback is its share of the run's logged time - the
level-0 passes crawl, the high-bit passes flick by - normalised so a
playthrough lasts as long as the uniform one at the same speed). Press
or drag on the room (hover with a mouse) to read a cell's counts. The
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
