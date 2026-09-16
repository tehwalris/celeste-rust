// The shell: the runs on offer (runs.json), the chosen run's run.json,
// the three views, and the route. The route is the hash:
//
//   #<tab>                     the default run
//   #<run>/<tab>               another run
//   #<run>/<tab>?h=99&t=3364   plus the view's own state (the space view:
//                              horizon, position, grain, look, ...)
//
// so a link reproduces exactly what was on screen. A view reports its
// state through `onState`; the shell writes it into the hash (replace,
// never push - the back button leaves the app, it does not undo scrubs).
//
// The header is one row on a wide screen (title and headline numbers,
// the room, the tabs) and two on a phone (the room and the tabs, then
// the numbers). The room switch lists the runs in GAME order (level
// index) with the altitude the game shows on entering the room; the
// default run is still runs.json's first entry.
import "./style.css";
import { chapters, fmtDuration, loadRun, loadRuns, roomLabel, runsInGameOrder, type Chapter, type Run, type RunInfo } from "./data";
import { el, clear, button, select, type Select } from "./ui";
import { spaceView } from "./space";
import { sizesView } from "./sizes";
import { timelineView } from "./timeline";

type Tab = "space" | "sizes" | "time";
const TABS: { id: Tab; label: string; hint: string }[] = [
  { id: "space", label: "Space", hint: "the room, frame by frame" },
  { id: "sizes", label: "Sizes", hint: "how big the sets were" },
  { id: "time", label: "Time", hint: "where the time went" },
];

/** A view: its element, and optionally its state for the URL. */
export interface View {
  root: HTMLElement;
  /** The view's state as query params (`h=99&t=12`), or "" for the default. */
  params?(): string;
  /** Adopt state from the URL (a back/forward, or a pasted link). */
  apply?(p: URLSearchParams): void;
}

interface Route {
  runId: string;
  tab: Tab;
  params: URLSearchParams;
}

function parseRoute(runs: RunInfo[]): Route {
  const raw = location.hash.replace(/^#/, "");
  const q = raw.indexOf("?");
  const path = q < 0 ? raw : raw.slice(0, q);
  const params = new URLSearchParams(q < 0 ? "" : raw.slice(q + 1));
  const parts = path.split("/").filter(Boolean);
  const runPart = parts.length > 1 ? parts[0] : runs[0].id;
  const tabPart = parts.length > 1 ? parts[1] : parts[0];
  return {
    runId: runs.some((r) => r.id === runPart) ? runPart : runs[0].id,
    tab: TABS.some((t) => t.id === tabPart) ? (tabPart as Tab) : "space",
    params,
  };
}
const hashOf = (runs: RunInfo[], runId: string, tab: Tab, params: string) =>
  `#${runId === runs[0].id ? tab : `${runId}/${tab}`}${params ? "?" + params : ""}`;

function spinner(text: string): HTMLElement {
  return el("div", { class: "loading", role: "status" }, [el("span", { class: "spinner", "aria-hidden": true }), el("span", { text })]);
}

function errorCard(title: string, detail: string, retry?: () => void): HTMLElement {
  const card = el("div", { class: "card err-card", role: "alert" }, [el("h2", { text: title }), el("pre", { text: detail })]);
  if (retry) card.append(button("Try again", retry, "small"));
  return card;
}

async function main() {
  const app = document.getElementById("app")!;
  let runs: RunInfo[];
  try {
    runs = await loadRuns();
  } catch (e) {
    clear(app);
    app.append(
      errorCard(
        "No runs to show",
        `${(e as Error).message}\n\nThe data directory needs runs.json and one subdirectory per run:\n  rewrite export-ui --out <data dir>/<run> --room X,Y`,
        () => location.reload(),
      ),
    );
    return;
  }
  const labelOf = (id: string) => {
    const r = runs.find((x) => x.id === id);
    return r ? roomLabel(r) : id;
  };

  // ---- the header: title, the run, the tabs ----------------------------------
  const title = el("h1", { text: "Celeste search" });
  const sub = el("div", { class: "sub" });
  const brand = el("div", { class: "brand" }, [title, sub]);
  let runPicker: Select<string> | null = null;
  const runBox = el("div", { class: "run-switch" });
  const nav = el("nav", { class: "tabs", "aria-label": "views" });
  const header = el("header", { class: "top" }, [brand, runBox, nav]);
  const main = el("main");
  clear(app);
  app.append(header, main);

  // The loaded run and its views, rebuilt from scratch on a run switch.
  let current: { run: Run; chs: Chapter[]; views: Map<Tab, View> } | null = null;
  let tab: Tab | null = null;
  let loads = 0;

  /** Write the route (and the current view's state) into the hash. */
  let writing = false;
  const writeHash = () => {
    if (!current || !tab) return;
    const v = current.views.get(tab);
    const h = hashOf(runs, current.run.id, tab, v?.params?.() ?? "");
    if (location.hash === h) return;
    writing = true;
    try {
      history.replaceState(null, "", h);
    } catch {
      // Safari rate-limits replaceState; the next write will catch up.
    }
    writing = false;
  };

  const go = (runId: string, t: Tab, params = new URLSearchParams()) => void apply(runId, t, params);

  const tabButtons = new Map<Tab, HTMLButtonElement>();
  for (const t of TABS) {
    const b = el("button", { type: "button", title: t.hint, "aria-current": "false" }, [el("span", { text: t.label })]);
    b.addEventListener("click", () => go(current?.run.id ?? runs[0].id, t.id));
    tabButtons.set(t.id, b);
    nav.append(b);
  }

  const buildRunSwitch = (runId: string) => {
    if (runPicker) {
      runPicker.set(runId);
      return;
    }
    runPicker = select<string>(
      runsInGameOrder(runs).map((r) => ({ value: r.id, label: roomLabel(r) })),
      runId,
      (id) => go(id, tab ?? "space"),
      { label: "room", hideLabel: true, class: "run-picker" },
    );
    runBox.append(runPicker.root);
  };

  const build = (t: Tab): View => {
    const c = current!;
    const have = c.views.get(t);
    if (have) return have;
    const v: View = t === "space" ? spaceView(c.run, writeHash) : t === "sizes" ? sizesView(c.run, writeHash) : timelineView(c.run, c.chs);
    c.views.set(t, v);
    return v;
  };

  async function apply(runId: string, t: Tab, params: URLSearchParams) {
    if (!current || current.run.id !== runId) {
      const seq = ++loads;
      const label = labelOf(runId);
      buildRunSwitch(runId);
      sub.textContent = "";
      document.title = `${label} · Celeste search`;
      clear(main);
      main.append(spinner(`Loading ${label}…`));
      let run: Run;
      try {
        run = await loadRun(runId);
      } catch (e) {
        if (seq !== loads) return;
        clear(main);
        main.append(
          errorCard(
            `Could not load ${label}`,
            `${(e as Error).message}\n\nRegenerate it with:\n  rewrite export-ui --log <run log> --out <data dir>/${runId} --room ${runId.replace(/^room(\d)(\d)$/, "$1,$2")}`,
            () => {
              current = null;
              void apply(runId, t, params);
            },
          ),
        );
        return;
      }
      // A later switch superseded this load.
      if (seq !== loads) return;
      if (run.horizons.length === 0) {
        clear(main);
        main.append(errorCard(`${label} is empty`, "run.json lists no horizons: the log had no ladder in it."));
        return;
      }
      current = { run, chs: chapters(run), views: new Map() };
      tab = null;
      const wall = run.wall_s != null ? fmtDuration(run.wall_s * 1000) : "";
      // The room is on the switch; the sub is the run's headline numbers.
      sub.replaceChildren(el("span", {}, [run.optimal != null ? el("b", { text: `optimum ${run.optimal} frames` }) : "no optimum found"]));
      sub.append(el("span", { text: `${run.horizons.length} horizon${run.horizons.length === 1 ? "" : "s"} tested` }));
      if (wall) sub.append(el("span", { text: `${wall} search` }));
    }
    const v = build(t);
    if (tab !== t) {
      tab = t;
      clear(main);
      main.append(v.root);
      main.dataset.tab = t;
      document.body.dataset.tab = t;
      for (const [id, b] of tabButtons) {
        b.classList.toggle("on", id === t);
        b.setAttribute("aria-current", id === t ? "page" : "false");
      }
      window.scrollTo({ top: 0 });
    }
    if ([...params.keys()].length) v.apply?.(params);
    writeHash();
  }

  window.addEventListener("hashchange", () => {
    if (writing) return;
    const r = parseRoute(runs);
    void apply(r.runId, r.tab, r.params);
  });
  const r = parseRoute(runs);
  void apply(r.runId, r.tab, r.params);
}

main();
