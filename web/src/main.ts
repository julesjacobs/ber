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
};

type Mark = { start: number; len: number };

type Detail = {
  kind: "type_mismatch" | "occurs";
  heading?: string | null;
  got?: string | null;
  expected?: string | null;
  marksGot?: Mark[] | null;
  marksExpected?: Mark[] | null;
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
const runBtn = document.getElementById("run-btn");

if (!editorMount || !output || !status) {
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
const errorMarks = StateEffect.define<DecorationSet>();
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

const renderOutput = (result: BerResult) => {
  const ok = result.ok;
  output.dataset.state = ok ? "ok" : "error";
  setStatus(ok ? "Typecheck succeeded" : "Typecheck failed", ok ? "ok" : "error");

  const detail = result.detail;
  if (!ok || detail) {
    if (detail && detail.kind === "type_mismatch") {
      const got = renderMarkedText(
        detail.got || "",
        detail.marksGot || [],
        "type-mark type-mark-got"
      );
      const expected = renderMarkedText(
        detail.expected || "",
        detail.marksExpected || [],
        "type-mark type-mark-expected"
      );
      const heading = escapeHtml(detail.heading || "");
      output.innerHTML = `
        <div class="type-heading">Type mismatch at ${heading}</div>
        <div class="type-row">
          <span class="type-label">Got:</span>
          <span class="type-text got">${got}</span>
        </div>
        <div class="type-row">
          <span class="type-label">Expected:</span>
          <span class="type-text expected">${expected}</span>
        </div>
      `;
      return;
    }
    if (detail && detail.kind === "occurs") {
      const msg = escapeHtml(detail.occursTy ?? "");
      const heading = escapeHtml(detail.heading ? `at ${detail.heading}` : "");
      output.innerHTML = `
        <div class="type-heading">Would require self-referential type ${heading}</div>
        <div class="occurs-box">∞ = ${msg}</div>
      `;
      return;
    }
    output.textContent = result.output;
    return;
  }

  output.textContent = result.output;
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

const decorateSpans = (spans: Span[]) => {
  const marks: Decoration[] = [];
  for (const span of spans) {
    const { from, to } = spanToRange(span);
    if (from === to) continue;
    const cls =
      span.label === "got"
        ? "ber-twiddle ber-twiddle-got"
        : span.label === "expected"
          ? "ber-twiddle ber-twiddle-expected"
          : "ber-twiddle ber-twiddle-generic";
    marks.push(Decoration.mark({ class: cls }).range(from, to));
  }
  return Decoration.set(marks, true);
};

const updateHighlights = (spans: Span[]) => {
  const deco = spans.length ? decorateSpans(spans) : Decoration.none;
  view.dispatch({ effects: errorMarks.of(deco) });
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
