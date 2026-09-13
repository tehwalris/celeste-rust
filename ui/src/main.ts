// The shell: the runs on offer (runs.json), the chosen run's run.json,
// the three views, and the route - `#<tab>` for the default run,
// `#<run>/<tab>` for any other.
import "./style.css";
import { chapters, fmtMs, loadRun, loadRuns, type Chapter, type Run, type RunInfo } from "./data";
import { chips, el, clear } from "./ui";
import { spaceView } from "./space";
import { sizesView } from "./sizes";
import { timelineView } from "./timeline";

type Tab = "space" | "sizes" | "time";
const TABS: { id: Tab; label: string }[] = [
  { id: "space", label: "Space" },
  { id: "sizes", label: "Sizes" },
  { id: "time", label: "Time" },
];

/** The route in the hash: `#space`, or `#room00/space`. */
function route(runs: RunInfo[]): { runId: string; tab: Tab } {
  const parts = location.hash.replace(/^#/, "").split("/");
  const runPart = parts.length > 1 ? parts[0] : runs[0].id;
  const tabPart = parts.length > 1 ? parts[1] : parts[0];
  return {
    runId: runs.some((r) => r.id === runPart) ? runPart : runs[0].id,
    tab: TABS.some((t) => t.id === tabPart) ? (tabPart as Tab) : "space",
  };
}
const hashOf = (runs: RunInfo[], runId: string, tab: Tab) => (runId === runs[0].id ? `#${tab}` : `#${runId}/${tab}`);

async function main() {
  const app = document.getElementById("app")!;
  let runs: RunInfo[];
  try {
    runs = await loadRuns();
  } catch (e) {
    clear(app);
    app.append(el("div", { class: "err", text: `Could not load the run list: ${(e as Error).message}\n\nThe data directory needs runs.json and one subdirectory per run (rewrite export-ui --out <data dir>/<run>).` }));
    return;
  }

  const title = el("h1", { text: "Celeste search" });
  const sub = el("span", { class: "sub" });
  const header = el("header", { class: "top" }, [title, sub]);
  const runStrip = el("div", { class: "run-strip" });
  const nav = el("nav", { class: "tabs" });
  const main = el("main");
  clear(app);
  app.append(header, runStrip, nav, main);

  // The loaded run and its views, rebuilt from scratch on a run switch.
  let current: { run: Run; chs: Chapter[]; views: Map<Tab, HTMLElement> } | null = null;
  let tab: Tab | null = null;
  let loads = 0;

  const go = (runId: string, t: Tab) => {
    const h = hashOf(runs, runId, t);
    if (location.hash !== h) history.replaceState(null, "", h);
    void apply(runId, t);
  };

  const buttons = new Map<Tab, HTMLButtonElement>();
  for (const t of TABS) {
    const b = el("button", { type: "button", text: t.label });
    b.addEventListener("click", () => go(current?.run.id ?? runs[0].id, t.id));
    buttons.set(t.id, b);
    nav.append(b);
  }

  const buildRunStrip = (runId: string) => {
    clear(runStrip);
    runStrip.append(
      chips<string>(
        runs.map((r) => ({ value: r.id, label: r.label })),
        new Set([runId]),
        (sel) => go([...sel][0], tab ?? "space"),
        { label: "run" },
      ),
    );
  };

  const build = (t: Tab) => {
    const c = current!;
    let v = c.views.get(t);
    if (!v) {
      v = t === "space" ? spaceView(c.run) : t === "sizes" ? sizesView(c.run) : timelineView(c.run, c.chs);
      c.views.set(t, v);
    }
    return v;
  };

  async function apply(runId: string, t: Tab) {
    if (!current || current.run.id !== runId) {
      const seq = ++loads;
      const label = runs.find((r) => r.id === runId)?.label ?? runId;
      buildRunStrip(runId);
      sub.textContent = "";
      clear(main);
      main.append(el("div", { class: "loading", text: `Loading ${label}…` }));
      let run: Run;
      try {
        run = await loadRun(runId);
      } catch (e) {
        if (seq !== loads) return;
        clear(main);
        main.append(el("div", { class: "err", text: `Could not load ${label}: ${(e as Error).message}\n\nRegenerate it with: rewrite export-ui --log <run log> --out <data dir>/${runId} --room ${runId.replace(/^room(\d)(\d)$/, "$1,$2")}` }));
        return;
      }
      // A later switch superseded this load.
      if (seq !== loads) return;
      current = { run, chs: chapters(run), views: new Map() };
      tab = null;
      // The room is on the run chip; the sub is the run's numbers.
      sub.textContent = `${run.horizons.length} horizons · optimum ${run.optimal ?? "?"} · ${run.wall_s != null ? fmtMs(run.wall_s * 1000) : ""}`;
    }
    if (tab === t) return;
    tab = t;
    clear(main);
    main.append(build(t));
    for (const [id, b] of buttons) b.classList.toggle("on", id === t);
    window.scrollTo({ top: 0 });
  }

  window.addEventListener("hashchange", () => {
    const r = route(runs);
    void apply(r.runId, r.tab);
  });
  const r = route(runs);
  void apply(r.runId, r.tab);
}

main();
