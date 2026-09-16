// Set sizes over the run: the headline figures, then per horizon the
// ladder curve (marked set and frontier peak per level), the frontier per
// frame per level, the backward per iteration, the marked set by layer;
// level 0's frontier against its visited total over the whole run; and
// the ladder as a table (the WCAG twin of every chart here). On a wide
// screen the charts sit two to a row.

import type { Run, LevelRun } from "./data";
import { defaultHorizon, fmtCompact, fmtDuration, fmtInt, fmtMs, horizonOrder, horizonVerdict, levelName } from "./data";
import { levelCss, slots } from "./color";
import { lineChart, legendFor, type Series } from "./chart";
import { chips, el, clear, select } from "./ui";
import type { View } from "./main";

type BwdMetric = "marked" | "rerun" | "loaded" | "targets";
const BWD_METRICS: { value: BwdMetric; label: string; title: string }[] = [
  { value: "marked", label: "marked", title: "states marked at the iteration: they can still win by the horizon" },
  { value: "rerun", label: "re-run", title: "candidate states re-run through the kernel at the iteration" },
  { value: "loaded", label: "loaded", title: "rows loaded from the checkpoint for the iteration" },
  { value: "targets", label: "targets", title: "the marked states of the iteration before, that this one steps into" },
];

export function sizesView(run: Run, onState: () => void): View {
  const root = el("div", { class: "sizes stack" });
  const defH = defaultHorizon(run);
  const hs = run.horizons.map((h) => h.h);
  const fwdFrames = run.horizons.reduce((a, h) => a + h.levels.reduce((b, l) => b + l.fwd.length, 0), 0);
  const bwdIters = run.horizons.reduce((a, h) => a + h.levels.reduce((b, l) => b + l.bwd.length, 0), 0);
  const reruns = run.horizons.reduce((a, h) => a + h.levels.reduce((b, l) => b + (l.reruns ?? 0), 0), 0);
  // Level 0's forward is extended across horizons, a horizon's lines only
  // the frames it added (a count-down's lower horizons add none): its
  // visited total is the largest over every line.
  const l0Visited = Math.max(0, ...run.horizons.flatMap((h) => h.levels.filter((l) => l.level === 0).flatMap((l) => l.fwd.map((x) => x.visited))));
  const levelsRun = run.horizons.reduce((a, h) => a + h.levels.length, 0);
  const stat = (label: string, value: string, unit?: string, fine?: string) =>
    el("div", { class: "stat" }, [
      el("div", { class: "label", text: label }),
      el("div", { class: "value" }, [value, unit ? el("span", { class: "unit", text: unit }) : null]),
      fine ? el("div", { class: "fine", text: fine }) : null,
    ]);
  root.append(
    el("div", { class: "card" }, [
      el("div", { class: "stats" }, [
        stat("optimum", run.optimal != null ? String(run.optimal) : "–", "frames", run.optimal != null ? `every level wins by f${run.optimal}` : "not found"),
        stat("wall time", run.wall_s != null ? fmtDuration(run.wall_s * 1000) : "–", undefined, run.prebuild_s != null ? `after ${fmtMs(run.prebuild_s * 1000)} kernel prebuild` : undefined),
        stat("horizons", String(run.horizons.length), undefined, `h${Math.min(...hs)} to h${Math.max(...hs)} · ${levelsRun} level runs`),
        stat("level-0 states", fmtCompact(l0Visited), undefined, `${fmtInt(l0Visited)} distinct states visited`),
        stat("forward frames", fmtInt(fwdFrames), undefined, "over every level of every horizon"),
        stat("backward iterations", fmtInt(bwdIters)),
        stat("kernel re-runs", fmtCompact(reruns), undefined, `${fmtInt(reruns)} in the backwards`),
      ]),
    ]),
  );

  const st = { h: defH, log: true, bwdMetric: "marked" as BwdMetric };
  const hPicker = select<number>(
    horizonOrder(run).map((i) => ({ value: i, label: `h${run.horizons[i].h} · ${horizonVerdict(run, run.horizons[i]).label}` })),
    st.h,
    (i) => {
      st.h = i;
      build();
      onState();
    },
    { label: "horizon", class: "h-picker" },
  );
  const logChips = chips<string>(
    [
      { value: "log", label: "log" },
      { value: "lin", label: "linear" },
    ],
    st.log ? "log" : "lin",
    (v) => {
      st.log = v === "log";
      build();
      onState();
    },
    { label: "y axis" },
  );
  const metricChips = chips<BwdMetric>(BWD_METRICS, st.bwdMetric, (v) => {
    st.bwdMetric = v;
    build();
    onState();
  });
  root.append(el("div", { class: "card controls" }, [hPicker.root, logChips.root]));
  const charts = el("div", { class: "chart-grid" });
  root.append(charts);

  const seriesOf = (lr: LevelRun, points: [number, number][]): Series => ({ name: `L${lr.level} (${levelName(lr)})`, color: levelCss(lr.level), points });
  const levelLabel = (x: number) => (x === 16 ? "exact" : `L${x}`);

  function build() {
    clear(charts);
    const hr = run.horizons[st.h];
    const H = hr.h;
    const markers = [{ x: H, label: `h${H}` }];
    const verdict = horizonVerdict(run, hr);
    hPicker.root.dataset.kind = verdict.kind;

    // 0. The ladder curve: marked set per level, and the forward's peak.
    const ladder: Series = {
      name: "marked states (the backward's set)",
      color: slots[0],
      points: hr.levels.filter((l) => l.marked != null).map((l) => [l.level, l.marked!] as [number, number]),
    };
    const peak: Series = {
      name: "frontier peak (the forward's widest frame)",
      color: slots[1],
      points: hr.levels.map((l) => [l.level, Math.max(0, ...l.frame_states.slice(0, H + 1))] as [number, number]),
    };
    charts.append(
      el("div", { class: "card" }, [
        el("h2", {}, [`The ladder at h${H}`, el("small", { text: verdict.long })]),
        lineChart({ series: [peak, ladder], yLog: st.log, xLabel: "level", yLabel: "states", xFormat: levelLabel, xTicks: hr.levels.map((l) => l.level) }),
        legendFor([peak, ladder]),
        el("p", { class: "note", text: "The marked set is what the next level has to search inside; the frontier peak is the widest single frame of that level's forward. A refuted level has a peak but no marked set." }),
      ]),
    );

    // 1. Frontier per frame, per level.
    const frontier = hr.levels.map((lr) => seriesOf(lr, lr.frame_states.slice(0, H + 1).map((n, f) => [f, n] as [number, number])));
    charts.append(
      el("div", { class: "card" }, [
        el("h2", {}, ["Frontier per frame", el("small", { text: `h${H}, one line per level` })]),
        lineChart({ series: frontier, yLog: st.log, xLabel: "frame", yLabel: "states first reached at the frame", markers }),
        legendFor(frontier),
        el("p", { class: "note", text: "Each level's forward runs to the horizon (level 0 persists across horizons and is extended a frame at a time). A refuted level's frontier dies out before it wins." }),
      ]),
    );

    // 2. Backward per iteration.
    const bwd = hr.levels
      .filter((lr) => lr.bwd.length)
      .map((lr) => seriesOf(lr, [...lr.bwd].sort((a, b) => a.f - b.f).map((b) => [b.f, b[st.bwdMetric]] as [number, number])));
    const metric = BWD_METRICS.find((m) => m.value === st.bwdMetric)!;
    charts.append(
      el("div", { class: "card" }, [
        el("h2", {}, ["Backward per iteration", el("small", { text: `h${H}, from f${H - 1} down to f1` })]),
        el("div", { class: "controls", style: "margin-bottom:8px" }, [metricChips.root]),
        lineChart({ series: bwd, yLog: st.log, xLabel: "iteration (frame)", yLabel: `${metric.label} states`, markers }),
        legendFor(bwd),
        el("p", { class: "note", text: `${metric.title[0].toUpperCase()}${metric.title.slice(1)}. At iteration f the candidates are the rows of every layer ≤ f in the cells that can step into a target; each is re-run through the kernel and marked if a successor is marked.` }),
      ]),
    );

    // 3. Marked by layer.
    const layer = hr.levels.filter((lr) => lr.marks_by_layer.length).map((lr) => seriesOf(lr, lr.marks_by_layer.slice(0, H + 1).map((n, f) => [f, n] as [number, number])));
    charts.append(
      el("div", { class: "card" }, [
        el("h2", {}, ["Marked states by layer", el("small", { text: `h${H}, by the frame each was first reached at` })]),
        lineChart({ series: layer, yLog: st.log, xLabel: "frame (layer)", yLabel: "marked states in the layer", markers }),
        legendFor(layer),
        el("p", { class: "note", text: "The marked set of a level is the states that can still win by the horizon; split by BFS layer it is the width of the corridor the next level must search, frame by frame." }),
      ]),
    );

    // 4. Level 0: frontier vs visited.
    const l0 = run.horizons.flatMap((h) => h.levels.filter((l) => l.level === 0).flatMap((l) => l.fwd)).sort((a, b) => a.f - b.f);
    const l0f: Series = { name: "frontier (kept after dedup)", color: slots[0], points: l0.map((l) => [l.f, l.kept]) };
    const l0v: Series = { name: "visited (distinct states so far)", color: slots[2], points: l0.map((l) => [l.f, l.visited]) };
    const l0r: Series = { name: "raw (emitted before dedup)", color: slots[3], points: l0.map((l) => [l.f, l.raw]) };
    charts.append(
      el("div", { class: "card" }, [
        el("h2", {}, ["Level 0 over the whole run", el("small", { text: "its forward is extended one frame per horizon" })]),
        lineChart({ series: [l0v, l0r, l0f], yLog: st.log, xLabel: "frame", yLabel: "states" }),
        legendFor([l0f, l0r, l0v]),
      ]),
    );

    // 5. Table, grouped by horizon, in the order they ran.
    const rows: HTMLElement[] = [];
    for (const h of run.horizons) {
      const v = horizonVerdict(run, h);
      rows.push(
        el("tr", { class: "h" }, [
          el("td", { colspan: 7 }, [`horizon ${h.h}`, el("span", { class: `verdict-tag ${v.kind}`, text: v.short }), el("span", { class: "muted", text: ` · ${h.levels.length} level${h.levels.length === 1 ? "" : "s"}` })]),
        ]),
      );
      for (const lr of h.levels) {
        const fwdMs = lr.fwd.reduce((a, b) => a + b.total_ms, 0);
        const bwdMs = lr.bwd.reduce((a, b) => a + b.total_ms, 0);
        rows.push(
          el("tr", {}, [
            el("td", {}, [el("i", { class: "dot", style: `background:${levelCss(lr.level)}` }), `L${lr.level} · ${levelName(lr)}`]),
            el("td", { class: lr.refuted ? "refuted" : "", text: lr.refuted ? "refuted" : lr.first_win != null ? `f${lr.first_win}` : "–" }),
            el("td", { class: lr.marked != null ? "" : "muted", text: lr.marked != null ? fmtInt(lr.marked) : "–" }),
            el("td", { class: lr.reruns != null ? "" : "muted", text: lr.reruns != null ? fmtInt(lr.reruns) : "–" }),
            el("td", { text: `${lr.fwd.length}` }),
            el("td", { text: fmtMs(fwdMs) }),
            el("td", { class: lr.bwd.length ? "" : "muted", text: lr.bwd.length ? `${lr.bwd.length} · ${fmtMs(bwdMs)}` : "–" }),
          ]),
        );
      }
    }
    charts.append(
      el("div", { class: "card wide" }, [
        el("h2", {}, ["The ladder, run by run", el("small", { text: "every level of every horizon, in the order they ran" })]),
        el("div", { class: "table-wrap" }, [
          el("table", { class: "ladder" }, [
            el("thead", {}, [el("tr", {}, ["level", "first win", "marked", "re-runs", "fwd frames", "fwd time", "bwd iters · time"].map((t) => el("th", { text: t })))]),
            el("tbody", {}, rows),
          ]),
        ]),
      ]),
    );
  }

  function params(): string {
    const p = new URLSearchParams();
    if (st.h !== defH) p.set("h", String(run.horizons[st.h].h));
    if (!st.log) p.set("y", "lin");
    if (st.bwdMetric !== "marked") p.set("bm", st.bwdMetric);
    return p.toString();
  }
  function apply(p: URLSearchParams) {
    const hv = p.has("h") ? Number(p.get("h")) : NaN;
    const hi = run.horizons.findIndex((x) => x.h === hv);
    st.h = hi >= 0 ? hi : defH;
    st.log = p.get("y") !== "lin";
    const bm = p.get("bm") as BwdMetric | null;
    st.bwdMetric = bm && BWD_METRICS.some((m) => m.value === bm) ? bm : "marked";
    hPicker.set(st.h);
    logChips.set(st.log ? "log" : "lin");
    metricChips.set(st.bwdMetric);
    build();
  }
  build();
  return { root, params, apply };
}
