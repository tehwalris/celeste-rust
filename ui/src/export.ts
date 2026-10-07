// Exporting the room as media: a still (PNG) of what is on screen, and a
// video of the traversal - the room's grid alone (tiles and the coloured
// cells; no paths, labels or controls), at a crisp integer scale.
//
// The video shows at time t the step the live playback would show at t
// (space.ts, `VideoPlan.stepAt`). It is ENCODED, not recorded: each frame is
// drawn and handed to WebCodecs with its timestamp, and mediabunny writes a
// plain MP4 (a MediaRecorder recording in real time wrote a fragmented file
// whose header only knew its first second: players showed a 1-2 s duration
// and stopped seeking there, 2026-10-07).

import { BufferTarget, CanvasSource, Mp4OutputFormat, Output, getFirstEncodableVideoCodec, type VideoCodec } from "mediabunny";
import { button, chips, download, el, show } from "./ui";

/** The pixel scale of an export: an even integer (H.264 needs even sides),
 *  the room's longer side at most 768 px (128 cells -> 6x). */
export function exportScale(w: number, h: number): number {
  return Math.max(2, 2 * Math.floor(384 / Math.max(w, h)));
}

/** Save a canvas as a PNG. */
export function savePng(canvas: HTMLCanvasElement, name: string): Promise<void> {
  return new Promise((resolve, reject) =>
    canvas.toBlob((b) => {
      if (!b) return reject(new Error("the canvas gave no image"));
      download(name, b);
      resolve();
    }, "image/png"),
  );
}

export type Pacing = "uniform" | "real";
export type Range = "all" | "pass";

/** What a video shows: `stepAt(t)` is the step at video time t (s),
 *  for 0 <= t < duration. */
export interface VideoPlan {
  steps: number;
  duration: number;
  stepAt(t: number): number;
  /** e.g. "h106 · level 0 forward … arc backward" */
  what: string;
  /** The file name without its extension. */
  name: string;
}

export interface VideoSource {
  /** The canvas size in pixels. */
  size: { w: number; h: number };
  speeds: { label: string; steps: number }[];
  defaults: { pacing: Pacing; speed: number };
  /** The pass the playhead is in, for the "this pass" range. */
  passLabel(): string;
  plan(range: Range, pacing: Pacing, speed: number): VideoPlan;
  /** Make sure every file the steps need is loaded. */
  ready(): Promise<void>;
  draw(canvas: HTMLCanvasElement, step: number): void;
}

/** The last frame is held this long, so a video does not end on a flash. */
const HOLD_S = 1;

/** The codecs to encode with, in order: H.264 first (what a phone can
 *  share and save to its photos), then VP9 / AV1 in WebM-less MP4. */
const CODECS: VideoCodec[] = ["avc", "vp9", "av1"];

/** Whether this browser can encode video at all (WebCodecs). */
const canEncode = () => typeof VideoEncoder !== "undefined";

const fmtS = (s: number) => (s < 60 ? `${s.toFixed(1)} s` : `${Math.floor(s / 60)} min ${Math.round(s % 60)} s`);
const fmtMB = (b: number) => (b < 1e6 ? `${Math.max(1, Math.round(b / 1e3))} kB` : `${(b / 1e6).toFixed(1)} MB`);

/** Encode `plan` drawn into `canvas`, frame by frame, as a plain
 *  (non-fragmented, fast-start) MP4: every frame is drawn, then handed to
 *  the encoder with its exact timestamp, so the file's duration is right in
 *  its header and nothing depends on the tab's animation timing. It runs as
 *  fast as the encoder does. `progress(t)` is called with the video time;
 *  `signal` cancels (the promise then rejects). */
async function record(src: VideoSource, plan: VideoPlan, canvas: HTMLCanvasElement, fps: number, progress: (t: number) => void, signal: AbortSignal): Promise<Blob> {
  const codec = await getFirstEncodableVideoCodec(CODECS, { width: canvas.width, height: canvas.height });
  if (!codec) throw new Error("This browser cannot encode H.264, VP9 or AV1 video.");
  const output = new Output({ format: new Mp4OutputFormat({ fastStart: "in-memory" }), target: new BufferTarget() });
  const source = new CanvasSource(canvas, { codec, bitrate: 8_000_000, keyFrameInterval: 2 });
  output.addVideoTrack(source, { frameRate: fps });
  await output.start();
  const total = plan.duration + HOLD_S;
  const frames = Math.ceil(total * fps);
  let shown = -1;
  let lastYield = performance.now();
  try {
    for (let k = 0; k < frames; k++) {
      if (signal.aborted) throw new DOMException("cancelled", "AbortError");
      const step = plan.stepAt(Math.min(k / fps, plan.duration - 1e-6));
      if (step !== shown) {
        shown = step;
        src.draw(canvas, step);
      }
      await source.add(k / fps, 1 / fps);
      // Let the page repaint (the preview, the progress bar) now and then.
      if (performance.now() - lastYield > 50) {
        progress((k + 1) / fps);
        await new Promise((r) => setTimeout(r, 0));
        lastYield = performance.now();
      }
    }
    await output.finalize();
  } catch (e) {
    await output.cancel();
    throw e;
  }
  progress(total);
  return new Blob([output.target.buffer!], { type: "video/mp4" });
}

/** The "Export video" dialog: range, pacing, speed; then the recording
 *  with a live preview, a progress bar and Cancel; then Download / Share. */
export function openVideoDialog(src: VideoSource) {
  const dlg = el("dialog", { class: "export-dialog", "aria-label": "export video" });
  const close = () => {
    abort?.abort();
    dlg.close();
  };
  dlg.addEventListener("close", () => {
    abort?.abort();
    if (url) URL.revokeObjectURL(url);
    dlg.remove();
  });

  let range: Range = "all";
  let pacing: Pacing = src.defaults.pacing;
  let speed = src.defaults.speed;
  let abort: AbortController | null = null;
  let url = "";

  const rangeChips = chips<Range>(
    [
      { value: "all", label: "All passes" },
      { value: "pass", label: "This pass", title: src.passLabel() },
    ],
    range,
    (r) => ((range = r), summarize()),
    { label: "range" },
  );
  const pacingChips = chips<Pacing>(
    [
      { value: "uniform", label: "Uniform", title: "every frame / iteration the same time" },
      { value: "real", label: "Real time", title: "each step its share of the run's logged time" },
    ],
    pacing,
    (p) => ((pacing = p), summarize()),
    { label: "pacing" },
  );
  const speedChips = chips<number>(
    src.speeds.map((s, i) => ({ value: i, label: s.label, hint: `${s.steps}/s` })),
    speed,
    (i) => ((speed = i), summarize()),
    { label: "speed" },
  );
  const summary = el("p", { class: "note export-summary" });
  const fpsOf = (i: number) => (src.speeds[i].steps <= 15 ? 30 : 60);
  const ok = canEncode();
  function summarize() {
    const p = src.plan(range, pacing, speed);
    const sp = src.speeds[speed];
    summary.textContent =
      `${p.what}: ${p.steps} steps at ${sp.steps} per second${pacing === "real" ? " on average (each step its share of the logged time)" : ""}` +
      ` → ${fmtS(p.duration)} + ${HOLD_S} s on the last frame. ${src.size.w}×${src.size.h} px, ${fpsOf(speed)} fps, ` +
      (ok ? "MP4." : "but this browser cannot encode video (no WebCodecs).");
    // The first frame as the preview, once the files are in.
    void src.ready().then(() => {
      if (dlg.dataset.phase === "setup") src.draw(canvas, p.stepAt(0));
    });
  }

  const canvas = el("canvas", { class: "export-preview", width: src.size.w, height: src.size.h });
  const bar = el("div", { class: "export-bar" }, [el("i")]);
  const barText = el("div", { class: "export-bar-text" });
  const result = el("div", { class: "export-result" });

  const go = button("Export", () => void start(), "primary");
  go.disabled = !ok;
  const cancel = button("Cancel", () => abort?.abort());
  const closeBtn = button("Close", close);

  const setup = el("div", { class: "export-setup" }, [rangeChips.root, pacingChips.root, speedChips.root, el("div", { class: "chips" }, [el("span", { class: "chips-label", text: "look" }), el("span", { class: "export-fixed", text: "Full, the room's grid only" })]), summary]);
  const actions = el("div", { class: "export-actions" }, [closeBtn, cancel, go]);
  dlg.append(el("h2", { text: "Export video" }), setup, canvas, bar, barText, result, actions);

  const phase = (p: "setup" | "recording" | "done") => {
    show(setup, p !== "recording");
    show(bar, p === "recording");
    show(barText, p !== "setup");
    show(cancel, p === "recording");
    show(go, p !== "recording");
    show(closeBtn, p !== "recording");
    show(result, p === "done");
    go.textContent = p === "done" ? "Export again" : "Export";
    // Once there is a video, Download is the one primary action.
    go.classList.toggle("primary", p !== "done");
    dlg.dataset.phase = p;
  };

  async function start() {
    const plan = src.plan(range, pacing, speed);
    if (url) URL.revokeObjectURL(url);
    url = "";
    result.replaceChildren();
    abort = new AbortController();
    phase("recording");
    barText.textContent = "loading the level files…";
    const total = plan.duration + HOLD_S;
    const fill = bar.firstElementChild as HTMLElement;
    fill.style.width = "0%";
    try {
      await src.ready();
      const blob = await record(
        src,
        plan,
        canvas,
        fpsOf(speed),
        (t) => {
          fill.style.width = `${((100 * t) / total).toFixed(1)}%`;
          barText.textContent = `encoding ${fmtS(t)} of ${fmtS(total)}`;
        },
        abort.signal,
      );
      const name = `${plan.name}.mp4`;
      url = URL.createObjectURL(blob);
      const video = el("video", { src: url, controls: true, playsinline: true, muted: true, loop: true, class: "export-video" });
      const file = new File([blob], name, { type: blob.type });
      const nav = navigator as Navigator & { canShare?: (d: { files: File[] }) => boolean };
      const canShare = !!nav.canShare && nav.canShare({ files: [file] });
      result.replaceChildren(
        video,
        el("div", { class: "export-actions" }, [
          button("Download", () => download(name, blob), "primary", `save ${name}`),
          canShare
            ? button("Share…", () => {
                navigator.share({ files: [file] }).catch(() => undefined);
              })
            : null,
        ].filter((x): x is HTMLButtonElement => x != null)),
      );
      barText.textContent = `${name} · ${fmtMB(blob.size)} · ${fmtS(total)}`;
      dlg.dataset.bytes = String(blob.size);
      phase("done");
    } catch (e) {
      const cancelled = (e as Error).name === "AbortError";
      phase("setup");
      show(barText, true);
      barText.textContent = cancelled ? "cancelled" : `failed: ${(e as Error).message}`;
    } finally {
      abort = null;
    }
  }

  phase("setup");
  barText.textContent = "";
  summarize();
  document.body.append(dlg);
  dlg.showModal();
}
