// Set sizes over the run: per level of a horizon, the frontier per frame,
// the marked states per backward iteration (or re-runs / candidates), the
// marked set by layer; level 0's frontier against its visited total; and
// the ladder as a table (the WCAG twin of every chart here).

import type { Run, LevelRun } from "./data";
import { fmtCompact, fmtInt, fmtMs, levelName } from "./data";
import { levelCss, slots } from "./color";
import { lineChart, legendFor, type Series } from "./chart";
import { chips, el, clear } from "./ui";
import type { View } from "./main";

export function sizesView(run: Run): View {
  const root = el("div");
  const last = run.horizons[run.horizons.length - 1];
  const l0Last = last.levels.find((l) => l.level === 0);
  const fwdFrames = run.horizons.reduce((a, h) => a + h.levels.reduce((b, l) => b + l.fwd.length, 0), 0);
  const bwdIters = run.horizons.reduce((a, h) => a + h.levels.reduce((b, l) => b + l.bwd.length, 0), 0);
  const reruns = run.horizons.reduce((a, h) => a + h.levels.reduce((b, l) => b + (l.reruns ?? 0), 0), 0);
  const l0Visited = l0Last && l0Last.fwd.length ? l0Last.fwd[l0Last.fwd.length - 1].visited : 0;
  const stat = (label: string, value: string, unit?: string) =>
    el("div", { class: "stat" }, [el("div", { class: "label", text: label }), el("div", { class: "value" }, [value, unit ? el("small", { text: unit }) : null])]);
  root.append(
    el("div", { class: "card" }, [
      el("div", { class: "stats" }, [
        stat("optimal win frame", run.optimal != null ? String(run.optimal) : "-"),
        stat("wall time", run.wall_s != null ? fmtMs(run.wall_s * 1000) : "-"),
        stat("horizons tried", String(run.horizons.length), `h${run.horizons[0].h}–h${last.h}`),
        stat("level-0 states visited", fmtCompact(l0Visited), fmtInt(l0Visited)),
        stat("forward frames", fmtInt(fwdFrames)),
        stat("backward iterations", fmtInt(bwdIters)),
        stat("kernel re-runs (backward)", fmtCompact(reruns), fmtInt(reruns)),
      ]),
    ]),
  );

  const st = { h: run.horizons.length - 1, log: true, bwdMetric: "marked" as "marked" | "rerun" | "loaded" | "targets" };
  const hRow = chips<number>(
    run.horizons.map((x, i) => ({ value: i, label: `h${x.h}` })),
    st.h,
    (i) => {
      st.h = i;
      build();
    },
    { label: "horizon", scroll: true },
  ).root;
  const logRow = chips<string>(
    [
      { value: "log", label: "log scale" },
      { value: "lin", label: "linear" },
    ],
    st.log ? "log" : "lin",
    (v) => {
      st.log = v === "log";
      build();
    },
    { label: "y axis" },
  ).root;
  const metricRow = chips<typeof st.bwdMetric>(
    [
      { value: "marked", label: "marked" },
      { value: "rerun", label: "re-run" },
      { value: "loaded", label: "loaded" },
      { value: "targets", label: "targets" },
    ],
    st.bwdMetric,
    (v) => {
      st.bwdMetric = v;
      build();
    },
    { label: "metric" },
  ).root;
  root.append(el("div", { class: "card stack" }, [hRow, logRow]));
  const charts = el("div");
  root.append(charts);

  const seriesOf = (lr: LevelRun, points: [number, number][]): Series => ({ name: `level ${lr.level} (${levelName(lr)})`, color: levelCss(lr.level), points });

  function build() {
    clear(charts);
    const hr = run.horizons[st.h];
    const H = hr.h;
    const markers = [{ x: H, label: `h${H}` }];

    // 0. The ladder curve: marked set per level, and the forward's peak.
    const ladder: Series = {
      name: "marked states (the backward's set)",
      color: slots[0],
      points: hr.levels.filter((l) => l.marked != null).map((l) => [l.level, l.marked!] as [number, number]),
    };
    const peak: Series = {
      name: "forward frontier peak (largest frame)",
      color: slots[1],
      points: hr.levels.map((l) => [l.level, Math.max(0, ...l.frame_states.slice(0, H + 1))] as [number, number]),
    };
    const levelLabel = (x: number) => (x === 16 ? "exact" : `L${x}`);
    charts.append(
      el("div", { class: "card" }, [
        el("h2", { text: `The ladder at horizon ${H}: set sizes per level` }),
        lineChart({ series: [peak, ladder], yLog: st.log, xLabel: "level", yLabel: "states", xFormat: levelLabel }),
        legendFor([peak, ladder]),
        el("p", { class: "note", text: "The marked set is what the next level has to search inside; the frontier peak is the widest single frame of that level's forward. A refuted level has a peak but no marked set." }),
      ]),
    );

    // 1. Frontier per frame, per level.
    const frontier = hr.levels.map((lr) => seriesOf(lr, lr.frame_states.slice(0, H + 1).map((n, f) => [f, n] as [number, number])));
    charts.append(
      el("div", { class: "card" }, [
        el("h2", { text: `Frontier size per frame at horizon ${H}` }),
        lineChart({ series: frontier, yLog: st.log, xLabel: "frame", yLabel: "states first reached at the frame", markers, tall: true }),
        legendFor(frontier),
        el("p", { class: "note", text: "Each level's forward is run to the horizon (level 0 persists across horizons and is extended a frame at a time). A refuted level's frontier dies out before it wins." }),
      ]),
    );

    // 2. Backward per iteration.
    const bwd = hr.levels
      .filter((lr) => lr.bwd.length)
      .map((lr) => seriesOf(lr, [...lr.bwd].sort((a, b) => a.f - b.f).map((b) => [b.f, b[st.bwdMetric]] as [number, number])));
    charts.append(
      el("div", { class: "card" }, [
        el("h2", { text: `Backward at horizon ${H}: ${st.bwdMetric} per iteration` }),
        metricRow,
        el("div", { style: "height:6px" }),
        lineChart({ series: bwd, yLog: st.log, xLabel: "iteration (frame)", yLabel: st.bwdMetric, markers }),
        legendFor(bwd),
        el("p", { class: "note", text: "The backward runs from frame H-1 down to 1. At iteration f the candidates are the rows of every layer <= f in the cells that can step into a target; each is re-run through the kernel and marked if a successor is marked." }),
      ]),
    );

    // 3. Marked by layer.
    const layer = hr.levels.filter((lr) => lr.marks_by_layer.length).map((lr) => seriesOf(lr, lr.marks_by_layer.slice(0, H + 1).map((n, f) => [f, n] as [number, number])));
    charts.append(
      el("div", { class: "card" }, [
        el("h2", { text: `Marked states by the frame they were first reached at (horizon ${H})` }),
        lineChart({ series: layer, yLog: st.log, xLabel: "frame (layer)", yLabel: "marked states in the layer", markers }),
        legendFor(layer),
        el("p", { class: "note", text: "The marked set of a level is the states that can still win by the horizon; split by BFS layer it is the width of the corridor the next level must search, frame by frame." }),
      ]),
    );

    // 4. Level 0: frontier vs visited.
    const l0 = run.horizons.flatMap((h) => h.levels.filter((l) => l.level === 0).flatMap((l) => l.fwd));
    const l0f: Series = { name: "frontier (kept)", color: slots[0], points: l0.map((l) => [l.f, l.kept]) };
    const l0v: Series = { name: "visited (total so far)", color: slots[2], points: l0.map((l) => [l.f, l.visited]) };
    const l0r: Series = { name: "raw (before dedup)", color: slots[3], points: l0.map((l) => [l.f, l.raw]) };
    charts.append(
      el("div", { class: "card" }, [
        el("h2", { text: "Level 0 forward over the whole run" }),
        lineChart({ series: [l0v, l0r, l0f], yLog: st.log, xLabel: "frame", yLabel: "states" }),
        legendFor([l0f, l0r, l0v]),
      ]),
    );

    // 5. Table.
    const rows: HTMLElement[] = [];
    for (const h of run.horizons) {
      for (const lr of h.levels) {
        const fwdMs = lr.fwd.reduce((a, b) => a + b.total_ms, 0);
        const bwdMs = lr.bwd.reduce((a, b) => a + b.total_ms, 0);
        rows.push(
          el("tr", {}, [
            el("td", { text: `h${h.h} · L${lr.level}` }),
            el("td", { text: levelName(lr) }),
            el("td", { text: lr.refuted ? "refuted" : lr.first_win != null ? `f${lr.first_win}` : "" }),
            el("td", { text: lr.marked != null ? fmtInt(lr.marked) : "" }),
            el("td", { text: lr.reruns != null ? fmtInt(lr.reruns) : "" }),
            el("td", { text: `${lr.fwd.length} / ${fmtMs(fwdMs)}` }),
            el("td", { text: lr.bwd.length ? `${lr.bwd.length} / ${fmtMs(bwdMs)}` : "" }),
          ]),
        );
      }
    }
    charts.append(
      el("div", { class: "card" }, [
        el("h2", { text: "The ladder" }),
        el("div", { class: "table-wrap" }, [
          el("table", { class: "ladder" }, [
            el("thead", {}, [el("tr", {}, ["horizon · level", "precision", "first win", "marked", "re-runs", "fwd frames / time", "bwd iters / time"].map((t) => el("th", { text: t })))]),
            el("tbody", {}, rows),
          ]),
        ]),
      ]),
    );
  }
  build();
  return { root };
}
