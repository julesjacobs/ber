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

type ParsedHeading = {
  display: string;
  focusSpan: Span | null;
  rangeText: string | null;
  fileLabel: string | null;
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

const parseHeading = (raw: string | null | undefined): ParsedHeading => {
  if (!raw) return { display: "", focusSpan: null, rangeText: null, fileLabel: null };
  const match = raw.match(/^(.*?):(\d+):(\d+)-(\d+):(\d+)$/);
  if (!match) return { display: raw, focusSpan: null, rangeText: null, fileLabel: null };
  const [, file, startLine, startCol, endLine, endCol] = match;
  const rangeText = `${startLine}:${startCol}-${endLine}:${endCol}`;
  const focusSpan: Span = {
    startLine: Number(startLine),
    startCol: Number(startCol),
    endLine: Number(endLine),
    endCol: Number(endCol),
    label: "focus",
  };
  const fileLabel = file === "repl" ? null : file;
  const display = fileLabel ? `${fileLabel}:${rangeText}` : rangeText;
  return { display, focusSpan, rangeText, fileLabel };
};

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
  highlightIds: Set<number>,
  side?: "got" | "expected"
) => {
  if (!view || !view.tree) return `<span class="type-text unknown">unknown</span>`;
  const buildSpan = (content: string, node: TypeTree, loc: Span | null) => {
    const classes = ["type-frag"];
    if (highlightIds.has(node.id)) classes.push("type-frag-mismatch");
    const idx = loc ? hoverLocs.push(loc) - 1 : -1;
    const attrs: string[] = [`class="${classes.join(" ")}"`, `data-type-id="${node.id}"`];
    if (side) attrs.push(`data-type-side="${side}"`);
    if (idx >= 0) attrs.push(`data-loc-idx="${idx}"`);
    return `<span ${attrs.join(" ")}">${escapeHtml(content)}</span>`;
  };
  const render = (node: TypeTree, prec: number): { text: string; html: string } => {
    if (node.kind === "var") {
      const name = node.name;
      const loc = node.loc ?? null;
      const html = buildSpan(name, node, loc);
      return { text: name, html };
    }
    const name = node.name;
    const loc = node.loc ?? null;
    const wrapSelf = (content: string) => buildSpan(content, node, loc);
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
  const attrs = side ? ` data-type-side="${side}"` : "";
  return `<span class="type-text"${attrs}>${rendered.html}</span>`;
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

const clearMismatchArrow = () => {
  const existing = output.querySelector(".type-arrow-layer");
  if (existing) existing.remove();
};

const renderMismatchArrow = (detail: Detail | null | undefined) => {
  clearMismatchArrow();
  if (!detail || detail.kind !== "type_mismatch") return;
  const gotId = detail.mismatchGotIds?.[0];
  const expectedId = detail.mismatchExpectedIds?.[0];
  if (gotId == null || expectedId == null) return;
  requestAnimationFrame(() => {
    const gotContainer = output.querySelector<HTMLElement>('.type-text[data-type-side="got"]');
    const expectedContainer = output.querySelector<HTMLElement>('.type-text[data-type-side="expected"]');
    if (!gotContainer || !expectedContainer) return;
    const gotFrag =
      gotContainer.querySelector<HTMLElement>(`.type-frag[data-type-id="${gotId}"]`) ||
      gotContainer.querySelector<HTMLElement>(".type-frag-mismatch");
    const expectedFrag =
      expectedContainer.querySelector<HTMLElement>(`.type-frag[data-type-id="${expectedId}"]`) ||
      expectedContainer.querySelector<HTMLElement>(".type-frag-mismatch");
    if (!gotFrag || !expectedFrag) return;
    const containerRect = output.getBoundingClientRect();
    if (!containerRect.width || !containerRect.height) return;
    const startRect = gotFrag.getBoundingClientRect();
    const endRect = expectedFrag.getBoundingClientRect();
    const startOffset = 10;
    const endOffset = 8;
    const startX = startRect.left + startRect.width / 2 - containerRect.left;
    const startY = startRect.bottom - containerRect.top + startOffset;
    const endX = endRect.left + endRect.width / 2 - containerRect.left;
    const endTargetY = endRect.top - containerRect.top;
    const endY = endTargetY - endOffset;
    const midY = (startY + endY) / 2;
    const p0 = { x: startX, y: startY };
    const p1 = { x: startX, y: midY };
    const p2 = { x: endX, y: midY };
    const p3 = { x: endX, y: endY };
    const pointOnCurve = (t: number) => {
      const inv = 1 - t;
      const x =
        inv * inv * inv * p0.x +
        3 * inv * inv * t * p1.x +
        3 * inv * t * t * p2.x +
        t * t * t * p3.x;
      const y =
        inv * inv * inv * p0.y +
        3 * inv * inv * t * p1.y +
        3 * inv * t * t * p2.y +
        t * t * t * p3.y;
      return { x, y };
    };
    const tangentAt = (t: number) => {
      const inv = 1 - t;
      const dx =
        3 * ((p1.x - p0.x) * inv * inv + 2 * (p2.x - p1.x) * inv * t + (p3.x - p2.x) * t * t);
      const dy =
        3 * ((p1.y - p0.y) * inv * inv + 2 * (p2.y - p1.y) * inv * t + (p3.y - p2.y) * t * t);
      return { x: dx, y: dy };
    };
    const labelT = 0.5;
    const labelPos = pointOnCurve(labelT);
    const tangent = tangentAt(labelT);
    const tangentLen = Math.hypot(tangent.x, tangent.y) || 1;
    const rightNormal = { x: tangent.y / tangentLen, y: -tangent.x / tangentLen };
    const labelOffset = 16;
    const labelX = labelPos.x + rightNormal.x * labelOffset;
    const labelY = labelPos.y + rightNormal.y * labelOffset;
    const endT = 0.98;
    const endTangent = tangentAt(endT);
    const endTangentLen = Math.hypot(endTangent.x, endTangent.y) || 1;
    const endDir = { x: endTangent.x / endTangentLen, y: endTangent.y / endTangentLen };
    const endNormal = { x: -endDir.y, y: endDir.x };
    const arrowLength = 10;
    const arrowWidth = 7;
    const tipX = endX;
    const tipY = endY;
    const baseX = tipX - endDir.x * arrowLength;
    const baseY = tipY - endDir.y * arrowLength;
    const leftX = baseX + endNormal.x * (arrowWidth / 2);
    const leftY = baseY + endNormal.y * (arrowWidth / 2);
    const rightX = baseX - endNormal.x * (arrowWidth / 2);
    const rightY = baseY - endNormal.y * (arrowWidth / 2);
    const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    svg.classList.add("type-arrow-layer");
    svg.setAttribute("viewBox", `0 0 ${containerRect.width} ${containerRect.height}`);
    svg.setAttribute("width", "100%");
    svg.setAttribute("height", "100%");
    svg.setAttribute("preserveAspectRatio", "xMidYMid meet");
    svg.setAttribute("aria-hidden", "true");
    const path = document.createElementNS("http://www.w3.org/2000/svg", "path");
    path.setAttribute(
      "d",
      `M ${startX} ${startY} C ${startX} ${midY} ${endX} ${midY} ${endX} ${endY}`
    );
    path.setAttribute("class", "type-arrow-path");
    svg.append(path);
    const arrow = document.createElementNS("http://www.w3.org/2000/svg", "path");
    arrow.setAttribute(
      "d",
      `M ${tipX} ${tipY} L ${leftX} ${leftY} L ${rightX} ${rightY} Z`
    );
    arrow.setAttribute("class", "type-arrow-head");
    svg.append(arrow);
    const label = document.createElementNS("http://www.w3.org/2000/svg", "text");
    label.textContent = "expected";
    label.setAttribute("x", `${labelX}`);
    label.setAttribute("y", `${labelY}`);
    label.setAttribute("dominant-baseline", "middle");
    label.setAttribute("text-anchor", "start");
    label.setAttribute("class", "type-arrow-label");
    svg.append(label);
    output.append(svg);
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

const renderOutput = (result: BerResult, headingInfo?: ParsedHeading) => {
  const ok = result.ok;
  output.dataset.state = ok ? "ok" : "error";
  setStatus(ok ? "Typecheck succeeded" : "Typecheck failed", ok ? "ok" : "error");

  const detail = result.detail;
  if (!ok || detail) {
    if (detail && detail.kind === "type_mismatch") {
      const parsedHeading = headingInfo ?? parseHeading(detail.heading);
      const headingLabel =
        parsedHeading.rangeText !== null
          ? `${parsedHeading.fileLabel ? `${escapeHtml(parsedHeading.fileLabel)}:` : ""}<span class="type-location">${escapeHtml(parsedHeading.rangeText)}</span>`
          : escapeHtml(detail.heading || "");
      const hoverLocs: Span[] = [];
      const mismatchGotIds = new Set<number>(detail.mismatchGotIds || []);
      const mismatchExpectedIds = new Set<number>(detail.mismatchExpectedIds || []);
      const typeLeft = renderTypeView(detail.got, hoverLocs, mismatchGotIds, "got");
      const typeRight = renderTypeView(detail.expected, hoverLocs, mismatchExpectedIds, "expected");
      const exprLeft = escapeHtml(detail.exprLeft?.expr ?? "<unknown>");
      const exprRight = escapeHtml(detail.exprRight?.expr ?? "<unknown>");
      output.innerHTML = `
        <div class="type-heading">Type mismatch at ${headingLabel || "unknown location"}</div>
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
      renderMismatchArrow(detail);
      lastResult = result;
      renderDebug();
      return;
    }
    if (detail && detail.kind === "occurs") {
      const parsedHeading = headingInfo ?? parseHeading(detail.heading);
      const msg = escapeHtml(detail.occursTy ?? "");
      const heading =
        parsedHeading.rangeText !== null
          ? `at ${parsedHeading.fileLabel ? `${escapeHtml(parsedHeading.fileLabel)}:` : ""}<span class="type-location">${escapeHtml(parsedHeading.rangeText)}</span>`
          : detail.heading
              ? `at ${escapeHtml(detail.heading)}`
              : "";
      output.innerHTML = `
        <div class="type-heading">Would require self-referential type ${heading}</div>
        <div class="occurs-box">∞ = ${msg}</div>
      `;
      clearMismatchArrow();
      lastResult = result;
      renderDebug();
      return;
    }
    output.textContent = result.output;
    clearMismatchArrow();
    lastResult = result;
    renderDebug();
    return;
  }

  output.textContent = result.output;
  clearMismatchArrow();
  lastResult = result;
  renderDebug();
};

const redrawMismatchArrow = () => {
  if (lastResult?.detail?.kind === "type_mismatch") {
    renderMismatchArrow(lastResult.detail);
  } else {
    clearMismatchArrow();
  }
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
    const headingInfo = result.detail ? parseHeading(result.detail.heading) : undefined;
    renderOutput(result, headingInfo);
    updateHighlights(result.spans ?? [], headingInfo?.focusSpan ?? null);
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
        : span.label === "focus"
          ? "ber-focus-mark"
          : span.label === "got"
          ? "ber-twiddle ber-twiddle-got"
          : span.label === "expected"
            ? "ber-twiddle ber-twiddle-expected"
            : "ber-twiddle ber-twiddle-generic";
    const parts: string[] = [];
    if (span.label && span.label !== "focus") parts.push(String(span.label));
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

const updateHighlights = (spans: Span[], focusSpan?: Span | null) => {
  applyHoverHighlights([]);
  const combined = focusSpan ? [...spans, focusSpan] : spans;
  applyBaseHighlights(combined);
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

window.addEventListener("resize", () => redrawMismatchArrow());

runTypecheck();
void populateExamples();
