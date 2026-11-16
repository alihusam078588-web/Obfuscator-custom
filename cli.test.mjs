import test from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { spawnSync } from "node:child_process";

const cli = new URL("./cli.mjs", import.meta.url);
const run = (args, input) =>
  spawnSync(process.execPath, [cli.pathname, ...args], {
    input,
    encoding: "utf8",
  });

test("help and version", () => {
  assert.equal(run(["--help"]).status, 0);
  assert.match(run(["--version"]).stdout, /obscura 0\.1\.0/);
});
test("full built-in VM regression suite", () => {
  const r = run(["--self-test"]);
  assert.equal(r.status, 0, r.stderr + "\n" + r.stdout);
  const result = JSON.parse(r.stdout);
  assert.equal(result.passed, true);
  assert.ok(result.total >= 100);
  assert.equal(result.failed, 0);
});
test("file input and output", async () => {
  const dir = await mkdtemp(join(tmpdir(), "obscura-"));
  try {
    const input = join(dir, "input.lua"),
      output = join(dir, "output.lua");
    await writeFile(
      input,
      "local function getValues() return 4, 7 end\nlocal a, b = getValues()\nreturn a * 10 + b",
      "utf8",
    );
    const r = run([input, "-o", output]);
    assert.equal(r.status, 0, r.stderr);
    const generated = await readFile(output, "utf8");
    assert.ok(generated.length > 0);
    assert.ok(generated.length > 100);
  } finally {
    await rm(dir, { recursive: true, force: true });
  }
});
test("stdin to stdout", () => {
  const r = run(["-"], "return 6 * 7");
  assert.equal(r.status, 0, r.stderr);
  assert.ok(r.stdout.length > 0);
});
test("verify checks emulator and reference", () => {
  const dirArgs = ["--verify", "-"];
  const r = run(
    dirArgs,
    "local function values() return 4, 7 end\nlocal a, b = values()\nreturn a * 10 + b",
  );
  assert.equal(r.status, 0, r.stderr);
  const result = JSON.parse(r.stdout);
  assert.equal(result.passed, true);
  assert.equal(result.emulatorResult, 47);
  assert.equal(result.referenceResult, 47);
  assert.equal(result.luauCompilerValidated, false);
  assert.equal(result.robloxRuntimeValidated, false);
});
