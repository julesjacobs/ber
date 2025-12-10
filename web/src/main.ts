import { EditorState, StateEffect, StateField } from "@codemirror/state";
import { Decoration, DecorationSet, EditorView, ViewUpdate, keymap, highlightSpecialChars } from "@codemirror/view";
import { basicSetup } from "codemirror";
import { StreamLanguage } from "@codemirror/language";
import { oCaml } from "@codemirror/legacy-modes/mode/mllike";
import { oneDark } from "@codemirror/theme-one-dark";

type Span = {
  startLine: number;
  startCol: number;
  endLine: number;
  endCol: number;
  label?: string | null;
  ty?: string | null;
};

type Mark = { start: number; len: number };
type TypeMark = { loc: Span; id: number };

type TypeTree =
  | {
      kind: "var";
      name: string;
      id: number;
      loc?: Span | null;
      args?: TypeTree[];
    }
  | {
      kind: "con";
      name: string;
      id: number;
      loc?: Span | null;
      args?: TypeTree[];
    };

type TypeView = {
  text: string;
  marks: TypeMark[];
  tree: TypeTree;
};

type ExprInfo = {
  expr: string;
  ty?: TypeView | null;
};

type ExampleMeta = {
  id: string;
  name: string;
  file: string;
};

type Detail = {
  kind: "type_mismatch" | "occurs";
  heading?: string | null;
  got?: TypeView | null;
  expected?: TypeView | null;
  marksGot?: Mark[] | null;
  marksExpected?: Mark[] | null;
  mismatchGotIds?: number[] | null;
  mismatchExpectedIds?: number[] | null;
  exprLeft?: ExprInfo | null;
  exprRight?: ExprInfo | null;
  occursTy?: string | null;
};

type BerResult = {
  ok: boolean;
  output: string;
  spans?: Span[];
  detail?: Detail | null;
};

type BerApi = {
  typecheck: (code: string) => BerResult;
};

declare global {
  interface Window {
    ber?: BerApi;
  }
}

const editorMount = document.getElementById("editor");
const output = document.getElementById("output");
const status = document.getElementById("status");
const debugToggle = document.getElementById("debug-toggle") as HTMLInputElement | null;
const debugPanel = document.getElementById("debug-panel");
const exampleSelect = document.getElementById("example-select") as HTMLSelectElement | null;
const runBtn = document.getElementById("run-btn");

if (!editorMount || !output || !status || !debugPanel || !debugToggle || !exampleSelect) {
  throw new Error("Missing editor/output containers in the DOM");
}

const initialDoc = `type 'a list = Nil | Cons of 'a * 'a list

let bool_not b =
  match b with
  | true -> false
  | false -> true

let bad_fs =
  Cons ((fun x -> Cons(x, Nil)), Cons (bool_not, Nil))`;

let view: EditorView;
let lastResult: BerResult | null = null;
const errorMarks = StateEffect.define<DecorationSet>();
const hoverMarks = StateEffect.define<DecorationSet>();
let examples: ExampleMeta[] = [];
const errorField = StateField.define<DecorationSet>({
  create: () => Decoration.none,
  update(value, tr) {
    const updated = value.map(tr.changes);
    for (const e of tr.effects) {
      if (e.is(errorMarks)) return e.value;
    }
    return updated;
  },
  provide: (f) => EditorView.decorations.from(f),
});

const hoverField = StateField.define<DecorationSet>({
  create: () => Decoration.none,
  update(value, tr) {
    const updated = value.map(tr.changes);
    for (const e of tr.effects) {
      if (e.is(hoverMarks)) return e.value;
    }
    return updated;
  },
  provide: (f) => EditorView.decorations.from(f),
});

const setStatus = (text: string, mode: "ok" | "error" | "pending") => {
  status.textContent = text;
  status.dataset.state = mode;
};

const escapeHtml = (str: string) =>
  str
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&#39;");

const renderMarkedText = (text: string, marks: Mark[], cls: string) => {
  if (!marks.length) return escapeHtml(text);
  const sorted = [...marks].sort((a, b) => a.start - b.start);
  let cursor = 0;
  let html = "";
  for (const m of sorted) {
    const start = Math.max(0, Math.min(text.length, m.start));
    const end = Math.max(start, Math.min(text.length, m.start + m.len));
    if (start > cursor) {
      html += escapeHtml(text.slice(cursor, start));
    }
    html += `<span class="${cls}">${escapeHtml(text.slice(start, end))}</span>`;
    cursor = end;
  }
  if (cursor < text.length) html += escapeHtml(text.slice(cursor));
  return html;
};

let currentSpans: Span[] = [];
let currentHoverSpans: Span[] = [];

const renderTypeView = (
  view: TypeView | null | undefined,
  hoverLocs: Span[],
  highlightIds: Set<number>
) => {
  if (!view || !view.tree) return `<span class="type-text unknown">unknown</span>`;
  const render = (node: TypeTree, prec: number): { text: string; html: string } => {
    if (node.kind === "var") {
      const name = node.name;
      const classes = ["type-frag"];
      const needsHighlight = highlightIds.has(node.id);
      if (needsHighlight) classes.push("type-frag-mismatch");
      const loc = node.loc ?? null;
      const idx = loc ? hoverLocs.push(loc) - 1 : -1;
      const html =
        loc !== null
          ? `<span class="${classes.join(" ")}"${idx >= 0 ? ` data-loc-idx="${idx}"` : ""}>${escapeHtml(name)}</span>`
          : `<span class="${classes.join(" ")}">${escapeHtml(name)}</span>`;
      return { text: name, html };
    }
    const name = node.name;
    const loc = node.loc ?? null;
    const highlighted = highlightIds.has(node.id);
    const wrapSelf = (content: string) => {
      const classes = ["type-frag"];
      if (highlighted) classes.push("type-frag-mismatch");
      const idx = loc ? hoverLocs.push(loc) - 1 : -1;
      if (loc) return `<span class="${classes.join(" ")}"${idx >= 0 ? ` data-loc-idx="${idx}"` : ""}>${content}</span>`;
      return `<span class="${classes.join(" ")}">${content}</span>`;
    };
    const args = node.args ?? [];
    if (name === "->" && args.length === 2) {
      const left = render(args[0], 1);
      const right = render(args[1], 0);
      const op = wrapSelf("->");
      const text = `${left.text} -> ${right.text}`;
      const html = `${left.html} ${op} ${right.html}`;
      const needParen = prec > 0;
      return { text: needParen ? `(${text})` : text, html: needParen ? `(${html})` : html };
    }
    if (name === "*" && args.length === 0) {
      const content = wrapSelf("unit");
      return { text: "unit", html: content };
    }
    if (name === "*" && args.length > 0) {
      const rendered = args.map((a) => render(a, 0));
      const textParts: string[] = [];
      const htmlParts: string[] = [];
      rendered.forEach((r, idx) => {
        if (idx > 0) {
          textParts.push("*");
          htmlParts.push(wrapSelf("*"));
        }
        textParts.push(r.text);
        htmlParts.push(r.html);
      });
      const text = textParts.join(" ");
      const html = htmlParts.join(" ");
      const needParen = prec > 1;
      return { text: needParen ? `(${text})` : text, html: needParen ? `(${html})` : html };
    }
    if (args.length === 0) {
      const content = wrapSelf(name);
      return { text: name, html: content };
    }
    if (args.length === 1) {
      const a = render(args[0], 2);
      const text = `${a.text} ${name}`;
      const html = `${a.html} ${wrapSelf(name)}`;
      const needParen = prec > 1;
      return { text: needParen ? `(${text})` : text, html: needParen ? `(${html})` : html };
    }
    const rendered = args.map((a) => render(a, 0));
    const argsText = rendered.map((r) => r.text).join(", ");
    const argsHtml = rendered.map((r) => r.html).join(", ");
    const baseText = `(${argsText}) ${name}`;
    const baseHtml = `(${argsHtml}) ${wrapSelf(name)}`;
    const needParen = prec > 1;
    return { text: needParen ? `(${baseText})` : baseText, html: needParen ? `(${baseHtml})` : baseHtml };
  };
  const rendered = render(view.tree, 0);
  return `<span class="type-text">${rendered.html}</span>`;
};

const attachHoverHandlers = (container: HTMLElement, hoverLocs: Span[]) => {
  const nodes = container.querySelectorAll<HTMLElement>("[data-loc-idx]");
  nodes.forEach((el) => {
    const idx = Number(el.dataset.locIdx ?? "-1");
    if (!Number.isFinite(idx) || idx < 0 || idx >= hoverLocs.length) return;
    const loc = hoverLocs[idx];
    el.addEventListener("mouseenter", () => {
      el.classList.add("type-frag-hovered");
      const hoverSpan = { ...loc, label: "hover" as const };
      applyHoverHighlights([hoverSpan]);
    });
    el.addEventListener("mouseleave", () => {
      el.classList.remove("type-frag-hovered");
      applyHoverHighlights([]);
    });
  });
};

const renderDebug = () => {
  if (!debugPanel || !debugToggle) return;
  if (!debugToggle.checked) {
    debugPanel.classList.add("hidden");
    return;
  }
  debugPanel.classList.remove("hidden");
  if (!lastResult) {
    debugPanel.textContent = "No result yet.";
    return;
  }
  const safe = (key: string, value: unknown) => {
    if (value instanceof Map) return Object.fromEntries(value);
    return value;
  };
  debugPanel.textContent = JSON.stringify(lastResult, safe, 2);
};

const renderOutput = (result: BerResult) => {
  const ok = result.ok;
  output.dataset.state = ok ? "ok" : "error";
  setStatus(ok ? "Typecheck succeeded" : "Typecheck failed", ok ? "ok" : "error");

  const detail = result.detail;
  if (!ok || detail) {
    if (detail && detail.kind === "type_mismatch") {
      const heading = escapeHtml(detail.heading || "");
      const hoverLocs: Span[] = [];
      const mismatchGotIds = new Set<number>(detail.mismatchGotIds || []);
      const mismatchExpectedIds = new Set<number>(detail.mismatchExpectedIds || []);
      const typeLeft = renderTypeView(detail.got, hoverLocs, mismatchGotIds);
      const typeRight = renderTypeView(detail.expected, hoverLocs, mismatchExpectedIds);
      const exprLeft = escapeHtml(detail.exprLeft?.expr ?? "<unknown>");
      const exprRight = escapeHtml(detail.exprRight?.expr ?? "<unknown>");
      output.innerHTML = `
        <div class="type-heading">Type mismatch at ${heading}</div>
        <div class="type-row compact">
          <code class="expr-snippet got">${exprLeft}</code>
          <span class="type-sep">:</span>
          ${typeLeft}
        </div>
        <div class="type-row compact">
          <code class="expr-snippet expected">${exprRight}</code>
          <span class="type-sep">:</span>
          ${typeRight}
        </div>
      `;
      attachHoverHandlers(output, hoverLocs);
      lastResult = result;
      renderDebug();
      return;
    }
    if (detail && detail.kind === "occurs") {
      const msg = escapeHtml(detail.occursTy ?? "");
      const heading = escapeHtml(detail.heading ? `at ${detail.heading}` : "");
      output.innerHTML = `
        <div class="type-heading">Would require self-referential type ${heading}</div>
        <div class="occurs-box">∞ = ${msg}</div>
      `;
      lastResult = result;
      renderDebug();
      return;
    }
    output.textContent = result.output;
    lastResult = result;
    renderDebug();
    return;
  }

  output.textContent = result.output;
  lastResult = result;
  renderDebug();
};

const sleep = (ms: number) => new Promise((resolve) => setTimeout(resolve, ms));

const waitForBer = async (timeoutMs = 10000): Promise<BerApi> => {
  const start = performance.now();
  while (performance.now() - start < timeoutMs) {
    if (window.ber) return window.ber;
    await sleep(25);
  }
  throw new Error("ber wasm runtime not ready. Did ber_wasm.bc.wasm.js load?");
};

let running = false;
let pending = false;
let nextRunId = 0;
let appliedRunId = 0;

const loadExample = async (id: string) => {
  if (!examples.length) return;
  const ex = examples.find((e) => e.id === id) || examples[0];
  if (!ex) return;
  try {
    const res = await fetch(`./examples/${ex.file}`);
    if (!res.ok) throw new Error(`Failed to load example ${ex.file}`);
    const text = await res.text();
    view.dispatch({
      changes: { from: 0, to: view.state.doc.length, insert: text },
    });
  } catch (err) {
    console.error(err);
  }
};

const populateExamples = async () => {
  if (!exampleSelect) return;
  try {
    const res = await fetch("./examples/index.json");
    if (!res.ok) throw new Error(`Failed to load examples index (${res.status})`);
    examples = (await res.json()) as ExampleMeta[];
  } catch (err) {
    console.error(err);
    examples = [];
  }
  if (!examples.length) {
    exampleSelect.disabled = true;
    exampleSelect.innerHTML = `<option>Examples unavailable</option>`;
    return;
  }
  exampleSelect.disabled = false;
  exampleSelect.innerHTML = examples
    .map((ex) => `<option value="${ex.id}">${ex.name}</option>`)
    .join("");
  exampleSelect.addEventListener("change", (ev) => {
    const target = ev.target as HTMLSelectElement;
    void loadExample(target.value);
  });
  void loadExample(examples[0].id);
};

const runTypecheck = async () => {
  if (running) {
    pending = true;
    return;
  }
  running = true;
  const runId = ++nextRunId;
  setStatus("Typechecking…", "pending");
  try {
    const api = await waitForBer();
    const result = api.typecheck(view.state.doc.toString());
    if (runId < appliedRunId) return;
    appliedRunId = runId;
    renderOutput(result);
    updateHighlights(result.spans ?? []);
  } catch (err) {
    if (runId < appliedRunId) return;
    appliedRunId = runId;
    const message = err instanceof Error ? err.message : String(err);
    renderOutput({ ok: false, output: message });
    updateHighlights([]);
  } finally {
    running = false;
    if (pending) {
      pending = false;
      void runTypecheck();
    }
  }
};

debugToggle.addEventListener("change", () => {
  renderDebug();
});

const triggerTypecheck = () => {
  void runTypecheck();
  return true;
};

const typecheckKeymap = keymap.of([
  { key: "Mod-Enter", preventDefault: true, run: triggerTypecheck },
  { key: "Meta-Enter", preventDefault: true, run: triggerTypecheck },
  { key: "Ctrl-Enter", preventDefault: true, run: triggerTypecheck },
]);

const typecheckDomHandlers = EditorView.domEventHandlers({
  keydown: (event) => {
    const isEnter = event.key === "Enter" || event.key === "Return";
    if (isEnter && (event.metaKey || event.ctrlKey)) {
      event.preventDefault();
      event.stopPropagation();
      void runTypecheck();
      return true;
    }
    return false;
  },
});

const spanToRange = (span: Span) => {
  const doc = view.state.doc;
  const clampLine = (line: number) => Math.max(1, Math.min(doc.lines, line));
  const startLine = clampLine(span.startLine);
  const endLine = clampLine(span.endLine);
  const startLineInfo = doc.line(startLine);
  const endLineInfo = doc.line(endLine);
  const start = Math.min(
    startLineInfo.from + Math.max(0, span.startCol),
    doc.length
  );
  const end = Math.min(
    endLineInfo.from + Math.max(0, span.endCol),
    doc.length
  );
  return { from: Math.min(start, end), to: Math.max(start, end) };
};

const decorateSpans = (spans: Span[], mode: "base" | "hover" = "base") => {
  if (!spans.length) return Decoration.none;
  const marks: Decoration[] = [];
  for (const span of spans) {
    const { from, to } = spanToRange(span);
    if (from === to) continue;
    const cls =
      mode === "hover"
        ? "ber-hover-mark"
        : span.label === "got"
          ? "ber-twiddle ber-twiddle-got"
          : span.label === "expected"
            ? "ber-twiddle ber-twiddle-expected"
            : "ber-twiddle ber-twiddle-generic";
    const parts: string[] = [];
    if (span.label) parts.push(String(span.label));
    if (span.ty) parts.push(String(span.ty));
    const attributes = parts.length ? { title: parts.join(": ") } : undefined;
    const spec = attributes ? { class: cls, attributes } : { class: cls };
    marks.push(Decoration.mark(spec).range(from, to));
  }
  return marks.length ? Decoration.set(marks, true) : Decoration.none;
};

const applyBaseHighlights = (spans: Span[]) => {
  currentSpans = spans;
  const deco = decorateSpans(spans, "base");
  view.dispatch({ effects: errorMarks.of(deco) });
};

const applyHoverHighlights = (spans: Span[]) => {
  currentHoverSpans = spans;
  const deco = decorateSpans(spans, "hover");
  view.dispatch({ effects: hoverMarks.of(deco) });
};

const updateHighlights = (spans: Span[]) => {
  applyHoverHighlights([]);
  applyBaseHighlights(spans);
};

const queueTypecheck = () => {
  setStatus("Typechecking…", "pending");
  void runTypecheck();
};

view = new EditorView({
  parent: editorMount,
  state: EditorState.create({
    doc: initialDoc,
    extensions: [
      errorField,
      hoverField,
      typecheckKeymap,
      typecheckDomHandlers,
      EditorView.updateListener.of((update: ViewUpdate) => {
        if (update.docChanged) {
          queueTypecheck();
        }
      }),
      basicSetup,
      highlightSpecialChars(),
      StreamLanguage.define(oCaml),
      oneDark,
    ],
  }),
});

runTypecheck();
void populateExamples();
