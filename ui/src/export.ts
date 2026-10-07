// Exporting the room as media: a still (PNG) of what is on screen, and a
// video of the traversal - the room's grid alone (tiles and the coloured
// cells; no paths, labels or controls), at a crisp integer scale.
//
// The video is recorded the way it plays: the canvas is drawn in real time
// while a MediaRecorder records `canvas.captureStream()`, the step shown at
// video time t being the step the live playback would show at t (space.ts,
// `VideoPlan.stepAt`). With `requestFrame` (captureStream(0)) a video frame
// is pushed once per tick at the chosen frame rate, after the draw, so no
// frame is captured half-drawn. A recording therefore takes as long as the
// video lasts; the tab has to stay in front (a hidden tab gets no animation
// frames, and the video would freeze for as long).

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

/** The container / codec to record, by what the browser says it supports:
 *  MP4 first (iOS Safari only has it; it is what a phone can share and
 *  save to its photos), then WebM. */
function pickMime(): string | null {
  if (typeof MediaRecorder === "undefined") return null;
  const candidates = ["video/mp4;codecs=avc1", "video/mp4", "video/webm;codecs=vp9", "video/webm;codecs=vp8", "video/webm"];
  return candidates.find((m) => MediaRecorder.isTypeSupported(m)) ?? null;
}
const extOf = (mime: string) => (mime.startsWith("video/mp4") ? "mp4" : "webm");

const fmtS = (s: number) => (s < 60 ? `${s.toFixed(1)} s` : `${Math.floor(s / 60)} min ${Math.round(s % 60)} s`);
const fmtMB = (b: number) => (b < 1e6 ? `${Math.max(1, Math.round(b / 1e3))} kB` : `${(b / 1e6).toFixed(1)} MB`);

interface Recording {
  blob: Blob;
  mime: string;
}

/** Record `plan` drawn into `canvas`. `progress(t)` is called per frame
 *  with the video time; `signal` cancels (the promise then rejects). */
async function record(src: VideoSource, plan: VideoPlan, canvas: HTMLCanvasElement, fps: number, progress: (t: number) => void, signal: AbortSignal): Promise<Recording> {
  const mime = pickMime();
  if (!mime || typeof canvas.captureStream !== "function") throw new Error("This browser cannot record video (no MediaRecorder or canvas capture).");
  const Track = (window as unknown as { CanvasCaptureMediaStreamTrack?: { prototype: object } }).CanvasCaptureMediaStreamTrack;
  const manual = !!Track && "requestFrame" in Track.prototype;
  const stream = canvas.captureStream(manual ? 0 : fps);
  const track = stream.getVideoTracks()[0] as MediaStreamTrack & { requestFrame?: () => void };
  const rec = new MediaRecorder(stream, { mimeType: mime, videoBitsPerSecond: 8_000_000 });
  const chunks: Blob[] = [];
  rec.ondataavailable = (e) => {
    if (e.data.size) chunks.push(e.data);
  };
  const stopped = new Promise<void>((resolve) => (rec.onstop = () => resolve()));

  let shown = plan.stepAt(0);
  src.draw(canvas, shown);
  rec.start(1000);
  track.requestFrame?.();
  const total = plan.duration + HOLD_S;
  await new Promise<void>((resolve, reject) => {
    let t0 = 0;
    let lastK = 0;
    const tick = (now: number) => {
      if (signal.aborted) return reject(new DOMException("cancelled", "AbortError"));
      if (!t0) t0 = now;
      const t = (now - t0) / 1000;
      const k = Math.floor(t * fps);
      if (k > lastK) {
        lastK = k;
        const step = plan.stepAt(Math.min(k / fps, plan.duration - 1e-6));
        if (step !== shown) {
          shown = step;
          src.draw(canvas, step);
        } else {
          // Chrome captures a frame only from a canvas drawn since the last
          // one: without this a held step (the end's hold, a slow speed) is
          // missing from the video and it ends early. Copying the canvas
          // onto itself is cheap and changes no pixel.
          canvas.getContext("2d")!.drawImage(canvas, 0, 0);
        }
        track.requestFrame?.();
        progress(Math.min(t, total));
      }
      if (t >= total) resolve();
      else requestAnimationFrame(tick);
    };
    requestAnimationFrame(tick);
  }).catch((e) => {
    rec.stop();
    track.stop();
    throw e;
  });
  rec.stop();
  await stopped;
  track.stop();
  return { blob: new Blob(chunks, { type: rec.mimeType || mime }), mime: rec.mimeType || mime };
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
  const mime = pickMime();
  function summarize() {
    const p = src.plan(range, pacing, speed);
    const sp = src.speeds[speed];
    summary.textContent =
      `${p.what}: ${p.steps} steps at ${sp.steps} per second${pacing === "real" ? " on average (each step its share of the logged time)" : ""}` +
      ` → ${fmtS(p.duration)} + ${HOLD_S} s on the last frame. ${src.size.w}×${src.size.h} px, ${fpsOf(speed)} fps, ` +
      (mime ? `${extOf(mime).toUpperCase()}. Recording takes as long as the video: keep this tab in front.` : "but this browser cannot record video.");
    // The first frame as the preview, once the files are in.
    void src.ready().then(() => {
      if (dlg.dataset.phase === "setup") src.draw(canvas, p.stepAt(0));
    });
  }

  const canvas = el("canvas", { class: "export-preview", width: src.size.w, height: src.size.h });
  const bar = el("div", { class: "export-bar" }, [el("i")]);
  const barText = el("div", { class: "export-bar-text" });
  const result = el("div", { class: "export-result" });

  const go = button("Record", () => void start(), "primary");
  go.disabled = !mime;
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
    go.textContent = p === "done" ? "Record again" : "Record";
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
      const rec = await record(
        src,
        plan,
        canvas,
        fpsOf(speed),
        (t) => {
          fill.style.width = `${((100 * t) / total).toFixed(1)}%`;
          barText.textContent = `recording ${fmtS(t)} of ${fmtS(total)}`;
        },
        abort.signal,
      );
      const name = `${plan.name}.${extOf(rec.mime)}`;
      url = URL.createObjectURL(rec.blob);
      const video = el("video", { src: url, controls: true, playsinline: true, muted: true, loop: true, class: "export-video" });
      const file = new File([rec.blob], name, { type: rec.blob.type });
      const nav = navigator as Navigator & { canShare?: (d: { files: File[] }) => boolean };
      const canShare = !!nav.canShare && nav.canShare({ files: [file] });
      result.replaceChildren(
        video,
        el("div", { class: "export-actions" }, [
          button("Download", () => download(name, rec.blob), "primary", `save ${name}`),
          canShare
            ? button("Share…", () => {
                navigator.share({ files: [file] }).catch(() => undefined);
              })
            : null,
        ].filter((x): x is HTMLButtonElement => x != null)),
      );
      barText.textContent = `${name} · ${fmtMB(rec.blob.size)} · ${fmtS(total)}`;
      dlg.dataset.bytes = String(rec.blob.size);
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
