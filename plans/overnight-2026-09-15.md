# Overnight 2026-09-15: room (2,0) end to end

Mandate (Philippe, going to sleep ~01:00): run room (2,0) to its optimum
and export the animation data; fix memory / formats / whatever blocks it;
decide independently and write the decisions down here.

## Decisions taken

1. **Disk cleanup.** `/` was at 95% (87 GB free). Deleted the old search
   trees that nothing reads any more: `celeste-room00g`, `celeste-room00h`,
   `celeste-search-room00`, `celeste-search-room00b`, `celeste-room10s`,
   `celeste-room10g`, `celeste-room00-batch`, `celeste-checkpoints`,
   `celeste-ck-room00`, `celeste-room00s`, and today's `celeste-h99-pos`
   and `celeste-probe20`. The UI data for rooms (0,0)/(1,0) lives in
   `/var/tmp/celeste-ui/data` and does not need them. Kept:
   `celeste-room20` (last night's exact level-0 tree to f69, RESUMED by
   the run below), `celeste-room10h` (the reference room (1,0) tree the
   probes used), `celeste-b70` (the `bench-frame` tree).
2. **Memory cap 90 GB for this run** (`safe-run.sh --memory 90G`). The
   heap model is exact now (door 24 B/state + frontier ~130 B/row x2 +
   ~12 GB wave transients); the door alone projects to 35-46 GB at f95,
   so ~60 GB would not fit. `free` leaves 101 GB after /tmp, which is
   the rule's ceiling; 90 leaves headroom for the UI server and a shell.
   A disk-backed door (the real fix) is a day of work with its own
   risks; it is the fallback if this OOMs (an OOM at frame X resumes
   from the checkpoint, nothing is lost but the wall time).
3. **Count down from the ceiling 95** (`--ceiling 95`, the replayed
   community TAS): one full ladder at 95, then 94 until refuted. The
   level-0 tree resumes at f69.
4. Position rungs stay off (measured a loss on room (1,0), BENCHMARK_DATA.md).

## Run

    ./safe-run.sh --memory 90G -- ./target/release/rewrite search --room 2,0 \
        --ceiling 95 --checkpoint-dir /var/tmp/celeste-room20 > /var/tmp/celeste-ui/room20.log

Then `export-ui --log /var/tmp/celeste-ui/room20.log --out /var/tmp/celeste-ui/data --room 2,0`
and the replay check `pico8_diff/replay.py --room 2,0 tas/...` of the witness.

## Log

(appended as the night goes)

## 08:20 - the run died, and not for a fixable reason

`EXIT 137` (OOM at the 90 GB cap) during frame 79. The frontier does NOT
saturate: f69 55M kept -> f78 175M kept (+18%/frame), visited 1.48G, door
36.6 GB, frontier 21 GB, f78 took 630 s with 1.0G raw emissions. f95 would
be billions of states. The exact-speed level 0 is not viable for room
(2,0); the f69 "7,500 states per position" was a point on a rising curve.
Tree intact and resumable at f78 (`/var/tmp/celeste-room20`), disk 693 GB
free. The `census` on f078 OOMs at 60 GB (it loads the whole frame); a
streaming per-column census follows.
