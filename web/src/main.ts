import { EditorState } from "@codemirror/state";
import { EditorView, keymap, highlightSpecialChars } from "@codemirror/view";
import { basicSetup } from "codemirror";
import { StreamLanguage } from "@codemirror/language";
import { oCaml } from "@codemirror/legacy-modes/mode/mllike";
import { oneDark } from "@codemirror/theme-one-dark";

type BerApi = {
  typecheck: (code: string) => { ok: boolean; output: string };
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

const initialDoc = `type 'a option = None | Some of 'a

let map_option = fun f opt ->
  match opt with
  | None -> None
  | Some x -> Some (f x)

let id = fun x -> x

let demo = map_option id (Some 42)`;

let view: EditorView;

const setStatus = (text: string, mode: "ok" | "error" | "pending") => {
  status.textContent = text;
  status.dataset.state = mode;
};

const renderOutput = (text: string, ok: boolean) => {
  output.textContent = text;
  output.dataset.state = ok ? "ok" : "error";
  setStatus(ok ? "Typecheck succeeded" : "Typecheck failed", ok ? "ok" : "error");
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

const runTypecheck = async () => {
  setStatus("Running typecheck…", "pending");
  try {
    const api = await waitForBer();
    const result = api.typecheck(view.state.doc.toString());
    renderOutput(result.output.trim(), result.ok);
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    renderOutput(message, false);
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

view = new EditorView({
  parent: editorMount,
  state: EditorState.create({
    doc: initialDoc,
    extensions: [
      typecheckKeymap,
      typecheckDomHandlers,
      basicSetup,
      highlightSpecialChars(),
      StreamLanguage.define(oCaml),
      oneDark,
    ],
  }),
});

runBtn?.addEventListener("click", (event) => {
  event.preventDefault();
  runTypecheck();
});

runTypecheck();
