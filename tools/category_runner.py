#!/usr/bin/env python3
"""Run a queue of (category, room) searches against the community tasdatabase
and verify every result end to end.

    tools/category_runner.py JOBS.json OUTDIR [--bin REWRITE]

JOBS.json: a list of {"cat": "nodiag", "room": "0,0", "name": "100m",
"offset": 27, "levels": "r0sxhn,r0sxh", "l1": true, "mem": "60G"} (optional:
"flags": more flags for the search, e.g. ["--split-frame"] (an "env" of
CELESTE_* variables is refused, as the binaries refuse them); "tag";
"timeout"; "reuse": a level-0 tree directory, moved in when it exists and moved back
there when the job ends - so jobs can count horizons up on one kept tree;
"to": a horizon H instead of the reference - `--to H`, level -1 at H, and a
refutation is a result (the optimum is above H); "keep_tree": leave the tree
in OUTDIR for a later job's "reuse"; "witness": a file of our inputs to verify,
no search); re-read
before every job, so jobs can be appended while it runs; a job whose key is
already in OUTDIR/results.jsonl is skipped.

Per job:
 0. the room's start as real play enters it: its loading frame from object
    J (pico8_diff/chain.py entry_jank: the category's boot chain; a job's
    "jank": J or "none"), for the search (--loading-jank) and every replay;
    every other start class (`rewrite starts`) a search-only job;
 1. the reference: the community TAS replayed in the ORIGINAL cart behind the
    room's prologue (the earliest offset around `offset` that exits);
 2. `rewrite search --ceiling REF --prefer <the community TAS>` with the
    category's mode (--nodiag / --onlydiag / --gemskip / --hundred) and
    the level -1 filter if `l1` (retried without it if its table refuses);
    `--to H` instead with a job's "to"; its outcome is its `result.json`;
 3. our witness replayed in the original cart (must exit at the optimum; no
    diagonal dash in a nodiag category, every dash diagonal in an onlydiag
    one, as the cart's own dash branch reports them);
 4. the upload (OUTDIR/upload/<cat>/TAS<n>.tas): the first candidate cut
    VALID in REAL PLAY - the boot chain on a real PICO-8 and Celia
    (tools/uct/check_uploads.py), "real_play" in the result; both files
    through UniversalClassicTas for information;
 5. an improvement gets a web UI run (export-ui + runs.json).
Results: one JSON line per job in OUTDIR/results.jsonl.
"""
import argparse
import json
import os
import re
import shutil
import subprocess
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from celeste import ROOT as M, categories, love, pico8, rooms, tasdb  # noqa: E402
sys.path.insert(0, os.path.join(M, "pico8_diff"))
import chain  # noqa: E402
sys.path.insert(0, os.path.join(M, "tools", "uct"))
import check_uploads  # noqa: E402

UI = "/var/tmp/celeste-ui/data"


def job_flags(job):
    """The job's own search flags; an "env" (CELESTE_* variables) is
    refused, as the binaries refuse the environment."""
    if "env" in job:
        raise SystemExit(f"job {job}: \"env\" is gone (the binaries refuse CELESTE_* variables): give its settings in \"flags\"")
    return list(job.get("flags", []))


def replay_exit(room, inputs, seeds, cat, jank=None):
    """Replay `inputs` in the ORIGINAL cart, the room started as the search
    starts it (its loading frame from object `jank`): the frame the room
    changes during (None: never), and the frames."""
    fs = pico8.replay(rooms.parse(room), inputs, len(inputs) + 3, seeds, cat, jank)
    return pico8.exit_frame(fs, rooms.parse(room)), fs


def chest_seed_variants(room, seeds, binary):
    """The seed list with each chest's entry replaced by every class of
    draws: a chest's berry appears at x = start + s, s in [-1, 2), and only
    which side of a pixel it lies on matters to a whole-pixel player
    (`rooms.seed_slots`: the objects the list runs over)."""
    slots = rooms.seed_slots(rooms.parse(room), binary)
    vals = list(seeds) + ["0"] * (len(slots) - len(seeds))
    return [vals[:i] + [s] + vals[i + 1:] for i, kind in enumerate(slots) if kind == "chest" for s in ["-1", "-0.5", "0", "0.5", "1", "1.5"] if vals[i] != s]


def canon_jumps(room, inputs, seeds, cat, jank=None):
    """Keep the jump bit only where a jump FIRES in the minimal cart (the
    search's model). The original cart buffers a press for 4 frames (jbuffer),
    so a press that does nothing in the minimal cart - one copied from a
    community TAS by --prefer - can fire later there. With every press firing
    at once, the buffer never holds one: the same route in both carts. The
    minimal cart is replayed under the file's balloon seeds (without them a
    balloon room's run diverges and keeps a press)."""
    spd_y = {fr.f: fr.spd_y for fr in pico8.replay(rooms.parse(room), inputs, len(inputs) + 1, seeds, cat, jank, original=False)}
    canon, prev = [], 0
    for i, b in enumerate(inputs):
        fired = b & 16 and not prev & 16 and spd_y.get(i + 1) == "0xfffe.0000"
        canon.append((b & ~16) | (16 if fired else 0))
        prev = b
    return canon


def run(job, outdir, binary):
    cat, room, name = job["cat"], job["room"], job["name"]
    # `tag`: a retry of a room with other settings, recorded apart.
    key = f"{cat}/{name}{job.get('tag', '')}"
    jd = os.path.join(outdir, cat, name + job.get("tag", ""))
    shutil.rmtree(jd, ignore_errors=True)
    os.makedirs(jd)
    res = {"key": key, "cat": cat, "room": room, "name": name, "start": time.strftime("%H:%M")}
    entry = tasdb.entry(cat, name)
    if not entry:
        return {**res, "status": "no database entry"}
    dbpath = tasdb.path(cat, entry)
    seeds, dbin = tasdb.parse(open(dbpath).read())
    level = tasdb.level_of_name(name)
    res["db_file"], res["db_frames"] = entry["file"], entry.get("frames")
    # 0. the room's start as REAL PLAY enters it: the transition's loading
    # frame updates its objects from the leaving player's index J (the
    # category's boot chain measures it; a job's "jank" overrides it, "none":
    # the IL load). The search, every replay and the reference start so.
    jank = job.get("jank", "measure")
    if jank == "measure":
        jank = chain.entry_jank(cat, rooms.level_of(rooms.parse(room)))
    jank = None if jank == "none" else jank
    res["jank"] = jank
    # The verdict needs the witness valid at the boot chain's J AND at Celia's
    # (check_uploads); where they are different start classes, Celia's class
    # is also searched (a search-only job) and its optimum recorded.
    if job.get("jank_classes"):
        res["jank_classes"], res["celia_jank"] = job["jank_classes"], job.get("celia_jank")
    # 1. the reference
    ref = None
    for off in range(job["offset"] - 2, job["offset"] + 3):
        e, _ = replay_exit(room, [0] * off + dbin, seeds, cat, jank)
        if e is not None:
            ref, prologue = e, off
            break
    if ref is None:
        # The community file finishes in UniversalClassicTas but not on a real
        # PICO-8 (the tool is a reimplementation): its count is the reference.
        ok, _, n = love.uct(level, dbpath, cat) if level else (False, None, None)
        if not ok and "to" not in job:
            return {**res, "status": "the community TAS does not exit in the original cart nor in UniversalClassicTas"}
        prologue = job["offset"]
        ref = prologue + n if ok else None
        res["ref_source"] = "UniversalClassicTas only (does not finish on a real PICO-8)" if ok else "none: the community TAS finishes nowhere"
    res["ref"], res["prologue"] = ref, prologue
    prefer = os.path.join(jd, "prefer.txt")
    open(prefer, "w").write(",".join(map(str, [0] * prologue + dbin)))
    # 2. the search
    def search(l1):
        flags = categories.search_flags(cat) + job_flags(job)
        flags += ["--loading-jank", str(jank) if jank else "none"]
        # The concrete steps under the community file's balloon seeds ([]: 0):
        # the witness is then one real run under them.
        flags += [f"--concrete-balloon-seeds={','.join(seeds)}"]
        if l1:
            # Level -1 counts search steps: two a frame under the split frame.
            steps = horizon * (2 if "--split-frame" in flags else 1)
            flags += ["--level-minus-one", f"{steps},5"]
        log = os.path.join(jd, "search.log")
        tree = os.path.join(jd, "tree")
        release_tree()
        # `reuse`: a level-0 tree of this room and level (a retry with a
        # longer objects ladder, or a horizon at most the tree's: a tree
        # filtered by level -1 below the horizon is refused, a hull tree
        # cannot be raised), moved in so `search` reuses it.
        if job.get("reuse") and os.path.isdir(job["reuse"]):
            os.makedirs(tree)
            shutil.move(job["reuse"], os.path.join(tree, "level00"))
        cmd = [f"{M}/safe-run.sh", "--memory", job.get("mem", "60G"), "--", binary, "search", "--room", room, "--to" if "to" in job else "--ceiling", str(horizon), "--level", job["levels"], "--prefer", prefer, "--save-marks", os.path.join(jd, "marks"), "--checkpoint-dir", tree] + flags
        t = time.time()
        with open(log, "w") as f:
            rc = subprocess.run(cmd, stdout=f, stderr=subprocess.STDOUT, cwd=M, timeout=job.get("timeout", 4 * 3600)).returncode
        # The search's record (`search::result`): none when it stopped on an
        # error before deciding.
        rec = os.path.join(tree, "result.json")
        return rc, (json.load(open(rec)) if os.path.exists(rec) else None), time.time() - t
    horizon = job.get("to", ref)
    keep = job.get("keep_tree")

    def release_tree():
        """Delete the job's tree; its level 0 goes back to `reuse` (or there for the first time)."""
        tree = os.path.join(jd, "tree")
        back = job.get("reuse")
        if back and os.path.isdir(os.path.join(tree, "level00")) and not os.path.exists(back):
            shutil.move(os.path.join(tree, "level00"), back)
        shutil.rmtree(tree, ignore_errors=True)

    drop_tree = lambda: None if keep else release_tree()
    if job.get("witness"):
        ours = [int(x) for x in re.findall(r"\d+", "".join(l for l in open(job["witness"]) if not l.startswith("#")))]
        opt, res["witness_from"] = len(ours), job["witness"]
    else:
        opt = None
    try:
        if opt is None:
            rc, rec, secs = search(job.get("l1", True))
            if rc != 0 and job.get("l1", True) and rec and rec["verdict"] == "level_minus_one_refused":
                res["l1"] = "refused"
                rc, rec, secs = search(False)
    except subprocess.TimeoutExpired:
        drop_tree()
        return {**res, "status": "search timed out"}
    if opt is None:
        res["search_s"] = round(secs)
        res["peak_gb"] = round(rec["peak_gb"], 2) if rec else None
        res["arc_bounds"] = [l["arc_bound"] for l in rec["levels"] if l["arc_bound"] is not None] if rec else []
        # Under `--to H` a refutation is a result: no win by H at all.
        if rc == 0 and rec and rec["verdict"] in ("refuted", "no_concrete_win") and "to" in job:
            drop_tree()
            return {**res, "status": f"refuted at {horizon}", "detail": rec["verdict"]}
        if rc != 0 or not rec or rec["verdict"] != "optimal":
            drop_tree()
            log = open(os.path.join(jd, "search.log"), errors="replace").read()
            tail = [l for l in log.splitlines() if l.strip()][-3:]
            return {**res, "status": f"search failed (exit {rc}{', ' + rec['verdict'] if rec else ''})", "tail": tail}
        opt, ours = rec["optimal"], rec["witness"]
    res["ours"] = opt
    if job.get("search_only"):
        drop_tree()
        return {**res, "status": "search only: another start class of the room (the widened optimum is the least)"}
    open(os.path.join(jd, "witness.txt"), "w").write(",".join(map(str, ours)))
    # 3. verification in the original cart
    hundred = categories.hundred(cat)
    # The search steps a chest's last shake as every draw (`rnd` is an
    # interval; only the balloons are seeded), so its witness may take the
    # berry where the file's chest seed does not put it: then the other
    # chest seeds are tried, and the upload carries the one that works.
    for sd in [seeds] + (chest_seed_variants(room, seeds, binary) if hundred else []):
        mine, (e, fs) = ours, replay_exit(room, ours, sd, cat, jank)
        if e != opt:
            # A jump press that does nothing in the minimal cart can fire later in
            # the original (its jump buffer): keep only the presses that fire.
            canon = canon_jumps(room, ours, sd, cat, jank)
            e2, fs2 = replay_exit(room, canon, sd, cat, jank)
            res["jump_canonicalized"] = f"original cart exit {e} -> {e2}"
            if e2 == opt:
                mine, e, fs = canon, e2, fs2
        # The berry taken before the exit: a `lifeup` appears where it was.
        berry = any(fr.lifeup and fr.f < e for fr in fs) if e else False
        if e == opt and (berry or not hundred):
            break
    if e == opt and sd != seeds:
        res["chest_seeds"] = f"[{','.join(seeds)}] -> [{','.join(sd)}]"
        seeds = sd
    if e == opt:
        ours = mine
        open(os.path.join(jd, "witness.txt"), "w").write(",".join(map(str, ours)))
    res["ours_original_cart_exit"] = e
    if hundred:
        res["berry"] = berry
    # The dashes as the cart started them (its own dash branch, `dash DIR`).
    started = [fr for fr in fs if fr.dash and (e is None or fr.f <= e)]
    if categories.nodiag(cat):
        res["diagonal_dashes"] = [fr.f for fr in started if fr.dash in pico8.DIAGONAL]
    if categories.onlydiag(cat):
        res["straight_dashes"] = [fr.f for fr in started if fr.dash not in pico8.DIAGONAL]
    lead = next((i for i, b in enumerate(ours) if b), len(ours))
    res["first_input"] = lead
    # 4. The upload, chosen by REAL PLAY (the module doc). It starts at the
    # room's first controllable frame: usually the community file's earliest
    # exiting offset, but a witness may press on the frame before (the player
    # exists and updates on the frame it is created), so that cut is tried
    # too; a cut never drops a press, and the witness ends at the exit (no
    # trailing input). UniversalClassicTas has no loading frame and computes
    # in doubles: information only. When the file's seeds die, the balloons'
    # phases are nudged (nudged_seeds; still optimal: the bound holds for
    # every seed).
    if level:
        ok_db, _, n_db = love.uct(level, dbpath, cat)
        res["uct_db"] = f"{'finished' if ok_db else 'NOT finished'} {n_db} inputs"
        cuts = [c for c in [prologue] + ([lead] if lead < prologue else []) + [prologue - 1] if not any(ours[:c])]
        tries = [(seeds, c) for c in cuts] + [(sd, c) for sd in love.nudged_seeds(seeds, rooms.seed_slots(rooms.parse(room), binary)) for c in cuts]
        # Each candidate once: a room without balloons nudges nothing.
        tries = [t for i, t in enumerate(tries) if t not in tries[:i]]
        # Every candidate's row; without a VALID one the record shows the
        # first (the file's seeds at the reference's cut), not the last tried.
        tried = []
        for sd, cut in tries:
            if sd != seeds and replay_exit(room, ours, sd, cat, jank)[0] != opt:
                continue
            mine = os.path.join(jd, f"ours-{entry['file']}")
            open(mine, "w").write(tasdb.text(sd, ours[cut:]))
            # A replay that fails (PICO-8 printed nothing; a chain.py
            # SystemExit) is this candidate's result, not the runner's end.
            try:
                row = check_uploads.check(mine, False, cat, binary)
            except (Exception, SystemExit) as ex:
                row = f"check failed: {type(ex).__name__}: {ex}"[:300]
            tried.append((cut, sd, row))
            if row.endswith("\tVALID"):
                res["real_play"], res["upload_cut"] = row, cut
                if sd != seeds:
                    res["upload_seeds"] = ",".join(sd)
                up = os.path.join(outdir, "upload", cat)
                os.makedirs(up, exist_ok=True)
                shutil.copy(mine, os.path.join(up, entry["file"]))
                res["upload"] = os.path.join(up, entry["file"])
                ok, st, n = love.uct(level, mine, cat)
                res["uct_ours"] = f"{'finished' if ok else st} {n} inputs (information only)"
                break
        if tried and not res.get("upload"):
            res["real_play"], res["real_play_cut"] = tried[0][2], tried[0][0]
            res["real_play_tried"] = f"{len(tried)} candidates (cuts {sorted({t[0] for t in tried})}, {len({tuple(t[1]) for t in tried})} seed lists), none VALID"
    res["status"] = "no reference" if ref is None else "IMPROVED" if opt < ref else ("tie" if opt == ref else "WORSE?")
    res["verified"] = (e == opt) and not res.get("diagonal_dashes") and res.get("berry", True) and bool(res.get("upload"))
    # 5. the UI
    if ref is not None and opt < ref and not job.get("witness"):
        try:
            paths = os.path.join(jd, "paths")
            # The paths start the room as the search and the replays do (its loading frame).
            subprocess.run([sys.executable, f"{M}/tools/align_tas.py", room, name, str(prologue), os.path.join(jd, "witness.txt"), paths, "--cat", cat, "--seeds", ",".join(seeds)] + (["--jank", str(jank)] if jank else []), cwd=M, capture_output=True, timeout=1800, check=True)
            rid = f"room{room.replace(',', '')}{cat}"
            # The UI's download IS the upload file.
            dl = ["--download", res["upload"]] if res.get("upload") else []
            ex = subprocess.run([f"{M}/safe-run.sh", "--", binary, "export-ui", "--checkpoint-dir", os.path.join(jd, "tree"), "--out", f"{UI}/{rid}", "--room", room, "--arc", os.path.join(jd, "marks"), "--witness", f"{paths}/ours.txt", "--reference", f"{paths}/reference.txt"] + dl, cwd=M, capture_output=True, text=True, timeout=3600)
            if ex.returncode != 0:
                raise RuntimeError("export-ui: " + ex.stderr.strip().splitlines()[-1] if ex.stderr.strip() else "export-ui failed")
            runs = json.load(open(f"{UI}/runs.json"))
            runs["runs"] = [r for r in runs["runs"] if r["id"] != rid]
            tasn = entry["file"].replace(".tas", "")
            label = categories.LABEL.get(cat, cat)
            runs["runs"].append({"id": rid, "label": ui_label(room, label, opt, tasn, ref, res), "category": label, "pick": f"ours {opt} vs TAS {ref}" + ("" if res["verified"] else " (not validated)")})
            json.dump(runs, open(f"{UI}/runs.json", "w"), indent=1)
            res["ui"] = rid
        except Exception as ex:  # the result stands; the UI can be redone
            res["ui_error"] = str(ex)[:200]
    drop_tree()
    return res


def ui_label(room, label, opt, tasn, ref, res):
    r"""The web UI's label of a run that beats the reference: an unverified
    one (`res["verified"]` false) says so and why, never only "exit at ...
    against ...".

    >>> ui_label("0,3", "gemskip100", 141, "TAS25", 142, {"verified": True})
    "Room (0,3) · gemskip100: exit at frame 141 against TAS25's 142 (community TAS drawn as the reference path)"
    >>> ui_label("0,3", "gemskip100", 141, "TAS25", 142, {"verified": False, "ours_original_cart_exit": 141, "real_play": "gemskip100 TAS25 room 0,3\tours 116 db 117\tIL exit 116f berry true\tUCT DID NOT FINISH (died at keypress frame 81) 116\tCelia DID NOT FINISH (DIED at frame 80) 0\tchain exit 116f berry true\tboot exit 116f berry true\tINVALID"})
    "Room (0,3) · gemskip100: exit at frame 141 against TAS25's 142 - NOT VALIDATED: real play INVALID (ours 116 db 117; UCT DID NOT FINISH (died at keypress frame 81) 116; Celia DID NOT FINISH (DIED at frame 80) 0; boot exit 116f berry true) (community TAS drawn as the reference path)"
    """
    head = f"Room ({room}) · {label}: exit at frame {opt} against {tasn}'s {ref}"
    tail = " (community TAS drawn as the reference path)"
    if res.get("verified"):
        return head + tail
    why = []
    if res.get("ours_original_cart_exit") != opt:
        why.append(f"the original cart exits at {res.get('ours_original_cart_exit')}")
    if res.get("diagonal_dashes"):
        why.append(f"diagonal dashes at {res['diagonal_dashes']}")
    if res.get("berry") is False:
        why.append("no berry")
    row = res.get("real_play")
    if row:
        cols = row.split("\t")
        keep = [c for c in cols[1:-1] if c.startswith(("ours", "UCT", "Celia", "boot"))]
        why.append(f"real play {cols[-1]} ({'; '.join(keep)})")
    else:
        why.append("no real-play check")
    return f"{head} - NOT VALIDATED: {'; '.join(why)}{tail}"


def expand(job, cache, binary):
    """A job is run as real play enters its room: its loading frame from the
    J of the category's boot chain (`chain.entry_jank`). Where another play
    of the previous room gives the room another start (`rewrite starts`:
    every feasible J, grouped by the start state), one SEARCH-ONLY job per
    other start, tagged `-J<j>`: the history-WIDENED optimum is the least of
    them all; the main job's witness is valid after the database's previous
    rooms. A job with its own "jank", or a "witness", is run as it is."""
    if "jank" in job or job.get("witness"):
        return [job]
    key = f"{job['cat']}/{job['name']}{job.get('tag', '')}"
    if key not in cache:
        room = rooms.parse(job["room"])
        j = chain.entry_jank(job["cat"], rooms.level_of(room))
        if j is None:
            cache[key] = [{**job, "jank": "none"}]
        else:
            st = rooms.starts(room, categories.search_flags(job["cat"]) + ["--balloon-seeds", "0"], binary)
            classes = st["classes"]
            others = [{**job, "jank": c[0], "tag": job.get("tag", "") + f"-J{c[0]}", "search_only": True} for c in classes if j not in c]
            cache[key] = [{**job, "jank": j, "jank_classes": classes, "celia_jank": st["celia"]}] + others
    return cache[key]


def main():
    ap = argparse.ArgumentParser(description="Run a queue of (category, room) searches and verify every result end to end (the module's doc).")
    ap.add_argument("jobs", help="the jobs file (re-read before every job)")
    ap.add_argument("outdir")
    ap.add_argument("--bin", help="the rewrite binary (default OUTDIR/rewrite: a copy, so a rebuild cannot swap it mid-queue)")
    a = ap.parse_args()
    for var in ("RUNNER_BIN", "CONCRETE_RUN"):
        if var in os.environ:
            raise SystemExit(f"category_runner: {var} is gone: --bin PATH (rewrite search, room and starts)")
    outdir = a.outdir
    binary = a.bin or f"{outdir}/rewrite"
    os.makedirs(outdir, exist_ok=True)
    results = os.path.join(outdir, "results.jsonl")
    cache = {}
    while True:
        done = set()
        if os.path.exists(results):
            done = {json.loads(l)["key"] for l in open(results) if l.strip()}
        todo = [j for job in json.load(open(a.jobs)) for j in expand(job, cache, binary) if f"{j['cat']}/{j['name']}{j.get('tag', '')}" not in done]
        if not todo:
            print("all jobs done", flush=True)
            return
        # OUTDIR/STOP: exit before the next job (a restart that cannot overlap a
        # running search: two searches in one tree corrupt its edge records).
        if os.path.exists(os.path.join(outdir, "STOP")):
            os.remove(os.path.join(outdir, "STOP"))
            print("stopped by OUTDIR/STOP", flush=True)
            return
        job = todo[0]
        print(f"[{time.strftime('%H:%M')}] {job['cat']} {job['name']} {job['room']}", flush=True)
        try:
            res = run(job, outdir, binary)
        except (Exception, SystemExit) as ex:
            # SystemExit too: chain.py and replay.py exit on what they cannot
            # run, and `except Exception` would end the runner with no
            # results line.
            res = {"key": f"{job['cat']}/{job['name']}{job.get('tag', '')}", "status": f"runner error: {type(ex).__name__}: {ex}"[:300]}
        res["end"] = time.strftime("%H:%M")
        with open(results, "a") as f:
            f.write(json.dumps(res) + "\n")
        print("   ", json.dumps(res)[:400], flush=True)


if __name__ == "__main__":
    main()
