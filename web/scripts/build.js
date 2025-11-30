#!/usr/bin/env node

const esbuild = require("esbuild");
const { execSync } = require("child_process");
const fs = require("fs/promises");
const path = require("path");

const watchMode = process.argv.includes("--watch");
const webDir = path.resolve(__dirname, "..");
const repoRoot = path.resolve(webDir, "..");
const distDir = path.join(webDir, "dist");
const buildDir = path.join(repoRoot, "_build", "default", "web");

const copyDir = async (src, dest) => {
  await fs.mkdir(dest, { recursive: true });
  const entries = await fs.readdir(src, { withFileTypes: true });
  for (const entry of entries) {
    const from = path.join(src, entry.name);
    const to = path.join(dest, entry.name);
    if (entry.isDirectory()) {
      await copyDir(from, to);
    } else {
      await fs.copyFile(from, to);
    }
  }
};

const copyStatic = async () => {
  await fs.copyFile(path.join(webDir, "index.html"), path.join(distDir, "index.html"));
  await fs.copyFile(path.join(buildDir, "ber_wasm.bc.wasm.js"), path.join(distDir, "ber_wasm.bc.wasm.js"));
  await copyDir(path.join(buildDir, "ber_wasm.bc.wasm.assets"), path.join(distDir, "ber_wasm.bc.wasm.assets"));
};

const main = async () => {
  await fs.rm(distDir, { recursive: true, force: true });
  await fs.mkdir(distDir, { recursive: true });

  execSync("dune build web/ber_wasm.bc.wasm.js", {
    cwd: repoRoot,
    stdio: "inherit",
    env: { ...process.env, WASM_OF_OCAML: "true" },
  });

  await copyStatic();

  await esbuild.build({
    entryPoints: [path.join(webDir, "src", "main.ts")],
    outfile: path.join(distDir, "bundle.js"),
    bundle: true,
    format: "esm",
    target: "es2020",
    sourcemap: true,
  });

  if (watchMode) {
    console.log("Watch mode is not available with this esbuild build; rerun to refresh assets.");
  } else {
    console.log("Build complete. Serve web/dist/ to try the playground.");
  }
};

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
