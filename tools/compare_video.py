#!/usr/bin/env python3
"""Render a side-by-side-in-time comparison of two TASes of one Celeste
Classic room: the NEW run in full colour, the OLD run's player and berry as a
semi-transparent grayscale ghost on top of it, both input displays, a frame
counter and the margin when the new run exits.

    tools/compare_video.py LEVEL OLD.tas NEW.tas OUT_PREFIX [--title 2800M]
        [--category "NO DIAGONAL DASHES"] [--old-label "TASDATABASE (GHOST)"] [--new-label OURS]
        [--slow 4] [--captures DIR] [--stills DIR]
Gemskip categories: UCT_DASHES=1 in the environment (as tools/category_runner.py).

Writes OUT_PREFIX.mp4 (real time, 30 fps) and OUT_PREFIX_slow.mp4 (the room
entry at real time, then 1/SLOW speed). Both runs are played headlessly in
UniversalClassicTas (tools/uct/validate.sh with UCT_CAPTURE, see
tools/uct/capture.lua), so they get UCT's graphics and its balloon seeds from
the files. Needs numpy, pillow and imageio-ffmpeg (a venv will do).

Checked, not assumed: both captures must start the room identically (the same
number of spawn frames), and each run's frame count (UCT's timer on the frame
before the room changes) must equal its file's input count - 1, the count the
tasdatabase lists.
"""
import argparse, json, os, re, subprocess, sys
import numpy as np
from PIL import Image
import imageio_ffmpeg

M = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
UCT = os.environ.get("UCT", os.path.expanduser("~/src/github.com/gonengazit/UniversalClassicTas"))

PAL = np.array([
    (0, 0, 0), (29, 43, 83), (126, 37, 83), (0, 135, 81), (171, 82, 54), (95, 87, 79), (194, 195, 199), (255, 241, 232),
    (255, 0, 77), (255, 163, 0), (255, 240, 36), (0, 231, 86), (41, 173, 255), (131, 118, 156), (255, 119, 168), (255, 204, 170),
], dtype=np.float32)

W, H = 1080, 1350          # 4:5, phone friendly
S = 8                      # 128 px * 8 = 1024
GX, GY = 28, 298           # the game's top left (even: aligns with yuv420p's 2x2 chroma)
FPS = 30
GHOST_ALPHA, SMOKE_ALPHA = 0.7, 0.35


# ---- capture -------------------------------------------------------------

def capture(level, tas, outdir):
    os.makedirs(outdir, exist_ok=True)
    env = dict(os.environ, UCT_CAPTURE=outdir)
    out = subprocess.run([f"{M}/tools/uct/validate.sh", str(level), tas], env=env, capture_output=True, text=True).stdout
    print(out.strip())
    if "finished, clean save" not in out:
        sys.exit(f"{tas}: did not finish in UniversalClassicTas")


def load(outdir, tas):
    meta = [json.loads(l) for l in open(f"{outdir}/frames.jsonl")]
    raw = np.fromfile(f"{outdir}/frames.bin", dtype=np.uint8).reshape(len(meta), 4, 128, 128, 4)
    if not meta[-1]["exit"] or any(m["exit"] for m in meta[:-1]):
        sys.exit(f"{outdir}: the capture does not end with exactly one room change")
    meta, raw = meta[:-1], raw[:-1]          # the exit frame already shows the reloaded room
    idx = np.rint(raw[..., 0].astype(np.float32) * 15 / 255).astype(np.int64)
    alpha = raw[..., 3] > 127
    text = open(tas).read()
    n_inputs = len(re.findall(r"\d+", text[text.index("]") + 1:]))
    frames = meta[-1]["practice_time"]
    if frames != n_inputs - 1:
        sys.exit(f"{tas}: exits after {frames} frames in UCT, the file has {n_inputs} inputs")
    spawn = next(i for i, m in enumerate(meta) if m["practice_time"] >= 1)
    return {"meta": meta, "screen": idx[:, 0], "player": (idx[:, 1], alpha[:, 1]), "smoke": (idx[:, 2], alpha[:, 2]), "berry": (idx[:, 3], alpha[:, 3]),
            "frames": frames, "spawn": spawn}


# ---- drawing -------------------------------------------------------------

def load_font():
    im = np.array(Image.open(f"{UCT}/CelesteTAS/font.png").convert("RGBA"))
    sep = (im == im[0, 0]).all(axis=2).all(axis=0)
    glyphs, x = {}, 0
    while x < len(sep):
        if sep[x]:
            x += 1
            continue
        s = x
        while x < len(sep) and not sep[x]:
            x += 1
        i = len(glyphs)
        if i < 96:
            glyphs[chr(32 + i)] = (im[:, s:x, :3] != 0).any(axis=2)
        else:
            glyphs[i] = None
    return {k: v for k, v in glyphs.items() if isinstance(k, str)}


FONT = None


def text(img, s, x, y, scale, col):
    """PICO-8 font (UCT's font.png), 4 px advance per glyph, upper left at (x, y)."""
    c = PAL[col] if isinstance(col, int) else np.array(col, dtype=np.float32)
    for ch in s:
        g = FONT.get(ch.lower()) if ch.isalpha() and ch.lower() in FONT else FONT.get(ch)
        if g is not None:
            m = np.repeat(np.repeat(g, scale, 0), scale, 1)
            h, w = m.shape
            img[y:y + h, x:x + w][m] = c
        x += 4 * scale
    return x


def text_w(s, scale):
    return 4 * scale * len(s) - scale


def rect(img, x0, y0, x1, y1, col):
    img[y0:y1, x0:x1] = PAL[col] if isinstance(col, int) else col


def inputs_box(img, x, y, b, scale, on, off, frame_col=0):
    """UCT's input display: a 25x11 box, z x  l [u/d] r as 3x3 squares."""
    rect(img, x, y, x + 25 * scale, y + 11 * scale, frame_col)
    for bit, (dx, dy) in ((16, (2, 7)), (32, (6, 7)), (1, (12, 7)), (4, (16, 3)), (8, (16, 7)), (2, (20, 7))):
        c = on if (b >= 0 and b & bit) else off
        rect(img, x + dx * scale, y + dy * scale, x + (dx + 3) * scale, y + (dy + 3) * scale, c)


GRAY = PAL @ np.array([0.30, 0.59, 0.11], dtype=np.float32)
GRAY_RGB = np.stack([0.45 * 255 + 0.55 * GRAY] * 3, axis=1)  # lifted: reads on the dark background


def game_frame(new, old, i_new, i_old):
    """The 128x128 composite: new screen, old ghost (grayscale), new player on top."""
    rgb = PAL[new["screen"][i_new]].copy()
    for layer, a in ((old["smoke"], SMOKE_ALPHA), (old["berry"], GHOST_ALPHA), (old["player"], GHOST_ALPHA)):
        if i_old is None:
            continue
        idx, m = layer[0][i_old], layer[1][i_old]
        rgb[m] = rgb[m] * (1 - a) + GRAY_RGB[idx[m]] * a
    if i_new is not None:
        idx, m = new["player"][0][i_new], new["player"][1][i_new]
        rgb[m] = PAL[idx[m]]
    return rgb


def compose(args, new, old, t, hud):
    """Video frame for capture index t (both runs aligned at the room load)."""
    n_new, n_old = len(new["meta"]), len(old["meta"])
    i_new = t if t < n_new else None
    i_old = t if t < n_old else None
    if i_new is not None:
        rgb = game_frame(new, old, i_new, i_old)
    else:
        # Ours is out: the old run plays on, dimmed, its player an opaque
        # ghost (which covers exactly the player's own pixels in its screen).
        # Both out (the final hold): the room the old run left.
        rgb = PAL[old["screen"][i_old if i_old is not None else n_old - 1]] * 0.75
        for layer, a in ((old["smoke"], SMOKE_ALPHA), (old["berry"], 1.0), (old["player"], 1.0)):
            if i_old is None:
                break
            idx, m = layer[0][i_old], layer[1][i_old]
            rgb[m] = rgb[m] * (1 - a) + GRAY_RGB[idx[m]] * a
    img = hud.copy()
    img[GY:GY + 128 * S, GX:GX + 128 * S] = np.repeat(np.repeat(rgb, S, 0), S, 1)
    # frame counter
    fm = new["meta"][i_new]["practice_time"] if i_new is not None else old["meta"][i_old]["practice_time"] if i_old is not None else old["frames"]
    label = f"{fm:3d}" if fm > 0 else "  -"
    x = W - 28 - text_w("FRAME", 5)
    text(img, "FRAME", x, 96, 5, 6)
    text(img, label, W - 28 - text_w(label, 14), 140, 14, 7)
    # input displays
    rows = ((new, i_new, 88, 8, 7, 1, args.new_label), (old, i_old, 194, 6, 6, 5, args.old_label))
    for run, i, y, lab_col, on, off, name in rows:
        b = run["meta"][i]["input"] if i is not None else -1
        inputs_box(img, 28, y, b, 8, on, off)
        text(img, name, 28 + 25 * 8 + 28, y + 6, 5, lab_col)
        frames = f"{run['frames']} FRAMES"
        done = i is None
        text(img, ("EXITED AT " if done else "") + frames, 28 + 25 * 8 + 28, y + 50, 4, 11 if done and run is new else 13)
    # the margin, once ours is out
    if i_new is None:
        d = old["frames"] - new["frames"]
        s = f"-{d} FRAMES"
        tw = text_w(s, 14)
        bx, by = GX + (128 * S - tw) // 2, GY + 128 * S * 3 // 4
        img[by - 28:by + 5 * 14 + 28, bx - 36:bx + tw + 36] *= 0.25
        text(img, s, bx, by, 14, 11)
    return img


def base_hud(args):
    img = np.zeros((H, W, 3), dtype=np.float32)
    img[:] = (12, 14, 24)
    rect(img, GX - 4, GY - 4, GX + 128 * S + 4, GY + 128 * S + 4, (60, 64, 80))
    return img


def encode(path, frames_iter):
    ff = imageio_ffmpeg.get_ffmpeg_exe()
    cmd = [ff, "-y", "-loglevel", "error", "-f", "rawvideo", "-pix_fmt", "rgb24", "-s", f"{W}x{H}", "-r", str(FPS), "-i", "-",
           "-c:v", "libx264", "-preset", "slow", "-crf", "16", "-tune", "animation", "-pix_fmt", "yuv420p", "-movflags", "+faststart", path]
    p = subprocess.Popen(cmd, stdin=subprocess.PIPE)
    n = 0
    for img, reps in frames_iter:
        b = np.clip(img, 0, 255).astype(np.uint8).tobytes()
        for _ in range(reps):
            p.stdin.write(b)
            n += 1
    p.stdin.close()
    if p.wait() != 0:
        sys.exit("ffmpeg failed")
    print(f"{path}: {n} frames, {n / FPS:.1f} s, {os.path.getsize(path) / 1e6:.1f} MB")


def main():
    global FONT
    ap = argparse.ArgumentParser()
    ap.add_argument("level", type=int)
    ap.add_argument("old")
    ap.add_argument("new")
    ap.add_argument("out")
    ap.add_argument("--title", default="")
    ap.add_argument("--old-label", default="TASDATABASE (GHOST)")
    ap.add_argument("--new-label", default="OURS")
    ap.add_argument("--category", default="NO DIAGONAL DASHES")
    ap.add_argument("--slow", type=int, default=4)
    ap.add_argument("--captures", help="capture directory (reused if present)")
    ap.add_argument("--stills", help="also write PNGs of a few frames here")
    args = ap.parse_args()
    FONT = load_font()
    cap = args.captures or f"{args.out}_captures"
    runs = {}
    for name, tas in (("old", args.old), ("new", args.new)):
        d = f"{cap}/{name}"
        if not os.path.exists(f"{d}/frames.jsonl"):
            capture(args.level, tas, d)
        runs[name] = load(d, tas)
    old, new = runs["old"], runs["new"]
    if old["spawn"] != new["spawn"]:
        sys.exit(f"the runs start the room differently: {old['spawn']} vs {new['spawn']} spawn frames")
    if (old["screen"][0] != new["screen"][0]).any():
        print("warning: the first frames differ (clouds?)")
    if new["frames"] >= old["frames"]:
        sys.exit("the new run is not faster")
    print(f"old {old['frames']} frames, new {new['frames']} frames, spawn {new['spawn']} frames")
    hud = base_hud(args)
    if args.title:
        text(hud, args.title, GX, 26, 6, 7)
        text(hud, args.category, GX + text_w(args.title, 6) + 30, 32, 4, 13)
    T = len(old["meta"])
    hold_end = 3 * FPS

    def realtime():
        for t in range(T + 1):
            yield compose(args, new, old, t, hud), (hold_end if t == T else 1)

    def slow():
        for t in range(T + 1):
            reps = 1 if t < new["spawn"] - 4 else args.slow
            yield compose(args, new, old, t, hud), (hold_end if t == T else reps)

    if args.stills:
        os.makedirs(args.stills, exist_ok=True)
        for t in sorted({new["spawn"] + 3, (new["spawn"] + len(new["meta"])) // 2, len(new["meta"]) - 1, len(new["meta"]) + 1, T}):
            Image.fromarray(np.clip(compose(args, new, old, t, hud), 0, 255).astype(np.uint8)).save(f"{args.stills}/t{t:03d}.png")
    encode(f"{args.out}.mp4", realtime())
    encode(f"{args.out}_slow.mp4", slow())


if __name__ == "__main__":
    main()
