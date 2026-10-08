#!/usr/bin/env python3
"""Run a queue of (category, room) searches against the community tasdatabase
and verify every result end to end.

    tools/category_runner.py JOBS.json OUTDIR

JOBS.json: a list of {"cat": "nodiag", "room": "0,0", "name": "100m",
"offset": 27, "levels": "r0sxhn,r0sxh", "l1": true, "mem": "60G"} (optional:
"env" for the search, e.g. {"CELESTE_TRIM_ROWS": "1"}; "tag"; "timeout"; "reuse": a level-0 tree to reuse); re-read
before every job, so jobs can be appended while it runs; a job whose key is
already in OUTDIR/results.jsonl is skipped.

Per job:
 1. the reference: the community TAS replayed in the ORIGINAL cart behind the
    room's prologue (the earliest offset around `offset` that exits);
 2. `rewrite search --ceiling REF --prefer <the community TAS>` with the
    category's mode (CELESTE_NODIAG / CELESTE_GEMSKIP / CELESTE_HUNDRED) and
    the level -1 filter if `l1` (retried without it if its table refuses);
 3. our witness replayed in the original cart (must exit at the optimum; no
    diagonal dash in a nodiag category);
 4. both files through UniversalClassicTas (tools/uct/validate.sh): the clean
    save of ours is the upload file (OUTDIR/upload/<cat>/TAS<n>.tas);
 5. an improvement gets a web UI run (export-ui + runs.json).
Results: one JSON line per job in OUTDIR/results.jsonl.
"""
import json, os, re, shutil, subprocess, sys, time

M = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DB = os.path.expanduser("~/src/github.com/CelesteClassic/tasdatabase")
ORIG = os.path.expanduser("~/src/github.com/tehwalris/celeste_ocaml/celeste.lua")
UI = "/var/tmp/celeste-ui/data"
CAT_LABEL = {"nodiag": "No Diagonal Dashes", "gemskipany": "Gemskip any%", "gemskipnodiag": "Gemskip No Diagonal Dashes", "100": "100%"}


def mode_env(cat):
    env = {}
    if "nodiag" in cat:
        env["CELESTE_NODIAG"] = "1"
    if cat.startswith("gemskip"):
        env["CELESTE_GEMSKIP"] = "1"
    if cat in ("100", "gemskip100"):
        env["CELESTE_HUNDRED"] = "1"
    return env


def replay_args(cat):
    return ["--one-dash"] if cat.startswith("gemskip") else []


def parse_tas(text):
    seeds = text[text.index("[") + 1:text.index("]")].strip().rstrip(",")
    return seeds, [int(x) for x in re.findall(r"\d+", text[text.index("]") + 1:])]


def replay_exit(room, inputs, seeds, cat):
    """The frame the room changes during, replaying `inputs` in the ORIGINAL cart."""
    cmd = [f"{M}/pico8_diff/replay.py", "--lua", ORIG, "--begin-game", "--room", room, "--inputs", ",".join(map(str, inputs)), "--frames", str(len(inputs) + 3)] + replay_args(cat)
    # `[]` in a tasdatabase file: UniversalClassicTas seeds every balloon 0
    # (not PICO-8's rnd), so the replay does too.
    cmd += ["--balloon-seeds", seeds or "0"]
    out = subprocess.run(cmd, capture_output=True, text=True, cwd=M, timeout=900).stdout
    for line in out.splitlines():
        m = re.match(r"f(\d+) room (\S+)", line)
        if m and m.group(2) != room:
            return int(m.group(1)), out
    return None, out


def diag_dashes(inputs):
    bad, prev = [], 0
    for i, b in enumerate(inputs):
        if b & 32 and not prev & 32 and (b & 3) and (b & 12):
            bad.append(i + 1)
        prev = b
    return bad


def canon_jumps(room, inputs, seeds, cat):
    """Keep the jump bit only where a jump FIRES in the minimal cart (the
    search's model). The original cart buffers a press for 4 frames (jbuffer),
    so a press that does nothing in the minimal cart - one copied from a
    community TAS by --prefer - can fire later there. With every press firing
    at once, the buffer never holds one: the same route in both carts. The
    minimal cart is replayed under the file's balloon seeds (concrete_run has
    none: a balloon room's run diverged there and kept a press, gemskip
    2800m 2026-10-07)."""
    cmd = [f"{M}/pico8_diff/replay.py", "--room", room, "--inputs", ",".join(map(str, inputs)), "--frames", str(len(inputs) + 1), "--balloon-seeds", seeds or "0"] + replay_args(cat)
    out = subprocess.run(cmd, capture_output=True, text=True, cwd=M, timeout=900).stdout
    spd_y = {}
    for line in out.splitlines():
        m = re.match(r"f(\d+) .* spd \S+,(\S+)", line)
        if m:
            spd_y[int(m.group(1))] = m.group(2)
    canon, prev = [], 0
    for i, b in enumerate(inputs):
        f = i + 1
        fired = b & 16 and not prev & 16 and spd_y.get(f) == "0xfffe.0000"
        canon.append((b & ~16) | (16 if fired else 0))
        prev = b
    return canon


def uct(level, path, cat):
    env = dict(os.environ)
    if cat.startswith("gemskip"):
        env["UCT_DASHES"] = "1"
    out = subprocess.run([f"{M}/tools/uct/validate.sh", str(level), path], capture_output=True, text=True, env=env, timeout=1800).stdout
    m = re.search(r"(finished, clean save|DID NOT FINISH[^,]*), (\d+) inputs", out)
    saved = os.path.expanduser(f"~/.local/share/love/CelesteTAS/TAS{level}.tas")
    return (m.group(1).startswith("finished") if m else False), (int(m.group(2)) if m else None), saved, out


def run(job, outdir, binary):
    cat, room, name = job["cat"], job["room"], job["name"]
    # `tag`: a retry of a room with other settings, recorded apart.
    key = f"{cat}/{name}{job.get('tag', '')}"
    jd = os.path.join(outdir, cat, name + job.get("tag", ""))
    shutil.rmtree(jd, ignore_errors=True)
    os.makedirs(jd)
    res = {"key": key, "cat": cat, "room": room, "name": name, "start": time.strftime("%H:%M")}
    entry = [e for e in json.load(open(f"{DB}/database.json"))["classic"][cat] if e["name"] == name]
    if not entry:
        return {**res, "status": "no database entry"}
    entry = entry[0]
    dbtext = open(f"{DB}/classic/{cat}/{entry['file']}").read()
    seeds, dbin = parse_tas(dbtext)
    res["db_file"], res["db_frames"] = entry["file"], entry.get("frames")
    # 1. the reference
    ref = None
    for off in range(job["offset"] - 2, job["offset"] + 3):
        e, _ = replay_exit(room, [0] * off + dbin, seeds, cat)
        if e is not None:
            ref, prologue = e, off
            break
    if ref is None:
        # The community file finishes in UniversalClassicTas but not on a real
        # PICO-8 (the tool is a reimplementation): its count is the reference.
        level = int(name.rstrip("m")) // 100 if name.endswith("m") else None
        ok, n, _, _ = uct(level, f"{DB}/classic/{cat}/{entry['file']}", cat) if level else (False, None, None, None)
        if not ok:
            return {**res, "status": "the community TAS does not exit in the original cart nor in UniversalClassicTas"}
        prologue = job["offset"]
        ref = prologue + n
        res["ref_source"] = "UniversalClassicTas only (does not finish on a real PICO-8)"
    res["ref"], res["prologue"] = ref, prologue
    prefer = os.path.join(jd, "prefer.txt")
    open(prefer, "w").write(",".join(map(str, [0] * prologue + dbin)))
    # 2. the search
    def search(l1):
        env = dict(os.environ, **mode_env(cat), **job.get("env", {}))
        # The concrete steps under the community file's balloon seeds ([]: 0):
        # the witness is then one real run under them.
        env["CELESTE_CONCRETE_BALLOON_SEEDS"] = seeds
        if l1:
            # Level -1 counts search steps: two a frame under the split frame.
            steps = ref * (2 if env.get("CELESTE_SPLIT_FRAME") else 1)
            env["CELESTE_LEVEL_MINUS_ONE"] = f"{steps},5"
        log = os.path.join(jd, "search.log")
        tree = os.path.join(jd, "tree")
        shutil.rmtree(tree, ignore_errors=True)
        # `reuse`: a finished level-0 tree of this room, level and horizon (a
        # retry with a longer objects ladder), moved in so `search` reuses it.
        if job.get("reuse") and os.path.isdir(job["reuse"]):
            os.makedirs(tree)
            shutil.move(job["reuse"], os.path.join(tree, "level00"))
        cmd = [f"{M}/safe-run.sh", "--memory", job.get("mem", "60G"), "--", binary, "search", "--room", room, "--ceiling", str(ref), "--level", job["levels"], "--prefer", prefer, "--save-marks", os.path.join(jd, "marks"), "--checkpoint-dir", os.path.join(jd, "tree")]
        t = time.time()
        with open(log, "w") as f:
            rc = subprocess.run(cmd, stdout=f, stderr=subprocess.STDOUT, env=env, cwd=M, timeout=job.get("timeout", 4 * 3600)).returncode
        return rc, open(log, errors="replace").read(), time.time() - t
    try:
        rc, log, secs = search(job.get("l1", True))
        if rc != 0 and job.get("l1", True) and rc != 137 and re.search(r"building the level -1 table|level_minus_one", log):
            res["l1"] = "refused"
            rc, log, secs = search(False)
    except subprocess.TimeoutExpired:
        shutil.rmtree(os.path.join(jd, "tree"), ignore_errors=True)
        return {**res, "status": "search timed out"}
    res["search_s"] = round(secs)
    peak = re.findall(r"peak ([\d.]+) GB", log)
    res["peak_gb"] = peak[-1] if peak else None
    m = re.search(r"OPTIMAL win frame: (\d+)", log)
    if rc != 0 or not m:
        shutil.rmtree(os.path.join(jd, "tree"), ignore_errors=True)
        tail = [l for l in log.splitlines() if l.strip()][-3:]
        return {**res, "status": f"search failed (exit {rc})", "tail": tail}
    opt = int(m.group(1))
    ours = [int(x) for x in re.search(r"concrete optimum \d+ inputs (\S+)", log).group(1).split(",")]
    res["ours"] = opt
    open(os.path.join(jd, "witness.txt"), "w").write(",".join(map(str, ours)))
    # 3. verification in the original cart
    e, out = replay_exit(room, ours, seeds, cat)
    if e != opt:
        # A jump press that does nothing in the minimal cart can fire later in
        # the original (its jump buffer): keep only the presses that fire.
        canon = canon_jumps(room, ours, seeds, cat)
        e2, out2 = replay_exit(room, canon, seeds, cat)
        res["jump_canonicalized"] = f"original cart exit {e} -> {e2}"
        if e2 == opt:
            ours, e, out = canon, e2, out2
            open(os.path.join(jd, "witness.txt"), "w").write(",".join(map(str, ours)))
    res["ours_original_cart_exit"] = e
    if cat in ("100", "gemskip100"):
        # The berry taken before the exit: a `lifeup` appears where it was.
        res["berry"] = any(" lifeup " in l and int(l.split()[0][1:]) < e for l in out.splitlines() if re.match(r"f\d+ ", l)) if e else False
    if "nodiag" in cat:
        res["diagonal_dashes"] = diag_dashes(ours)
    lead = next((i for i, b in enumerate(ours) if b), len(ours))
    res["first_input"] = lead
    # 4. UniversalClassicTas. The upload starts at the room's first
    # controllable frame: usually the community file's earliest exiting
    # offset, but a witness may press on the frame before (the player exists
    # and updates on the frame it is created: room (2,1) nodiag, 2026-10-08),
    # so that cut is tried too, and UCT decides. A cut never drops a press.
    # Last, a frame before the prologue: UCT computes in Lua doubles, not
    # 16.16, and on room (1,2)'s platforms its player is a pixel off PICO-8's
    # from frame 55 (TAS18 too); our 113 does not finish there at the PICO-8
    # alignment, one frame later it does (2026-10-08: the file one zero
    # longer, one frame slower).
    level = int(name.rstrip("m")) // 100 if name.endswith("m") else None
    if level:
        ok_db, n_db, _, _ = uct(level, f"{DB}/classic/{cat}/{entry['file']}", cat)
        res["uct_db"] = f"{'finished' if ok_db else 'NOT finished'} {n_db} inputs"
        for cut in [prologue] + ([lead] if lead < prologue else []) + [prologue - 1]:
            if any(ours[:cut]):
                continue
            mine = os.path.join(jd, f"ours-{entry['file']}")
            open(mine, "w").write(f"[{seeds}]" + ",".join(map(str, ours[cut:])))
            ok, n, saved, _ = uct(level, mine, cat)
            res["uct_ours"], res["upload_cut"] = f"{'finished' if ok else 'NOT finished'} {n} inputs", cut
            if ok:
                up = os.path.join(outdir, "upload", cat)
                os.makedirs(up, exist_ok=True)
                shutil.copy(saved, os.path.join(up, entry["file"]))
                res["upload"] = os.path.join(up, entry["file"])
                break
    res["status"] = "IMPROVED" if opt < ref else ("tie" if opt == ref else "WORSE?")
    res["verified"] = (e == opt) and not res.get("diagonal_dashes") and res.get("uct_ours", "").startswith("finished") and res.get("berry", True)
    # 5. the UI
    if opt < ref:
        try:
            paths = os.path.join(jd, "paths")
            env = dict(os.environ, TAS_CATEGORY=cat, REPLAY_ARGS=" ".join(replay_args(cat)))
            subprocess.run([sys.executable, f"{M}/tools/align_tas.py", room, name, str(prologue), os.path.join(jd, "witness.txt"), seeds or "-", paths], env=env, cwd=M, capture_output=True, timeout=1800, check=True)
            # The UI's download must be the upload file: its seeds as UCT wrote them.
            if res.get("upload"):
                up_seeds, _ = parse_tas(open(res["upload"]).read())
                p = f"{paths}/ours.txt"
                text = open(p).read()
                open(p, "w").write(re.sub(r"(?m)^seeds .*$", f"seeds [{up_seeds}]", text, count=1))
            rid = f"room{room.replace(',', '')}{cat}"
            ex = subprocess.run([binary, "export-ui", "--checkpoint-dir", os.path.join(jd, "tree"), "--log", os.path.join(jd, "search.log"), "--out", f"{UI}/{rid}", "--room", room, "--arc", os.path.join(jd, "marks"), "--witness", f"{paths}/ours.txt", "--reference", f"{paths}/reference.txt"], cwd=M, capture_output=True, text=True, timeout=3600)
            if ex.returncode != 0:
                raise RuntimeError("export-ui: " + ex.stderr.strip().splitlines()[-1] if ex.stderr.strip() else "export-ui failed")
            # The UI's download must equal the upload file.
            if res.get("upload"):
                t = json.load(open(f"{UI}/{rid}/run.json"))["witness"]["tas"]["text"]
                res["ui_download_equals_upload"] = t == open(res["upload"]).read()
            runs = json.load(open(f"{UI}/runs.json"))
            runs["runs"] = [r for r in runs["runs"] if r["id"] != rid]
            tasn = entry["file"].replace(".tas", "")
            runs["runs"].append({"id": rid, "label": f"Room ({room}) · {CAT_LABEL.get(cat, cat)}: exit at frame {opt} against {tasn}'s {ref} (community TAS drawn as the reference path)", "category": CAT_LABEL.get(cat, cat), "pick": f"ours {opt} vs TAS {ref}"})
            json.dump(runs, open(f"{UI}/runs.json", "w"), indent=1)
            res["ui"] = rid
        except Exception as ex:  # the result stands; the UI can be redone
            res["ui_error"] = str(ex)[:200]
    shutil.rmtree(os.path.join(jd, "tree"), ignore_errors=True)
    return res


def main():
    jobs_path, outdir = sys.argv[1], sys.argv[2]
    os.makedirs(outdir, exist_ok=True)
    results = os.path.join(outdir, "results.jsonl")
    while True:
        done = set()
        if os.path.exists(results):
            done = {json.loads(l)["key"] for l in open(results) if l.strip()}
        todo = [j for j in json.load(open(jobs_path)) if f"{j['cat']}/{j['name']}{j.get('tag', '')}" not in done]
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
        binary = os.environ.get("RUNNER_BIN", f"{outdir}/rewrite")
        print(f"[{time.strftime('%H:%M')}] {job['cat']} {job['name']} {job['room']}", flush=True)
        try:
            res = run(job, outdir, binary)
        except Exception as ex:
            res = {"key": f"{job['cat']}/{job['name']}{job.get('tag', '')}", "status": f"runner error: {ex}"[:300]}
        res["end"] = time.strftime("%H:%M")
        with open(results, "a") as f:
            f.write(json.dumps(res) + "\n")
        print("   ", json.dumps(res)[:400], flush=True)


if __name__ == "__main__":
    main()
