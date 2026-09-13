// The shell: load run.json, build the three views, route by hash.
import "./style.css";
import { chapters, fmtMs, loadRun, type Run } from "./data";
import { el, clear } from "./ui";
import { spaceView } from "./space";
import { sizesView } from "./sizes";
import { timelineView } from "./timeline";

type Tab = "space" | "sizes" | "time";

async function main() {
  const app = document.getElementById("app")!;
  let run: Run;
  try {
    run = await loadRun();
  } catch (e) {
    clear(app);
    app.append(el("div", { class: "err", text: `Could not load the run data: ${(e as Error).message}\n\nRegenerate it with: rewrite export-ui --log <run log> --out /var/tmp/celeste-ui/data` }));
    return;
  }
  const chs = chapters(run);
  clear(app);

  const title = el("h1", { text: `Celeste room (${run.room[0]},${run.room[1]}) search` });
  const sub = el("span", { class: "sub", text: `${run.horizons.length} horizons · optimum ${run.optimal ?? "?"} · ${run.wall_s != null ? fmtMs(run.wall_s * 1000) : ""}` });
  const header = el("header", { class: "top" }, [title, sub]);
  const tabs: { id: Tab; label: string }[] = [
    { id: "space", label: "Space" },
    { id: "sizes", label: "Sizes" },
    { id: "time", label: "Time" },
  ];
  const nav = el("nav", { class: "tabs" });
  const main = el("main");
  app.append(header, nav, main);

  const views = new Map<Tab, HTMLElement>();
  const build = (t: Tab) => {
    let v = views.get(t);
    if (!v) {
      v = t === "space" ? spaceView(run) : t === "sizes" ? sizesView(run) : timelineView(run, chs);
      views.set(t, v);
    }
    return v;
  };
  const buttons = new Map<Tab, HTMLButtonElement>();
  let current: Tab | null = null;
  const show = (t: Tab) => {
    if (current === t) return;
    current = t;
    clear(main);
    main.append(build(t));
    for (const [id, b] of buttons) b.classList.toggle("on", id === t);
    if (location.hash !== `#${t}`) history.replaceState(null, "", `#${t}`);
    window.scrollTo({ top: 0 });
  };
  for (const t of tabs) {
    const b = el("button", { type: "button", text: t.label });
    b.addEventListener("click", () => show(t.id));
    buttons.set(t.id, b);
    nav.append(b);
  }
  const fromHash = (): Tab => {
    const h = location.hash.replace("#", "") as Tab;
    return tabs.some((t) => t.id === h) ? h : "space";
  };
  window.addEventListener("hashchange", () => show(fromHash()));
  show(fromHash());
}

main();
