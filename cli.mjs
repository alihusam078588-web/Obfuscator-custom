import { readFile, writeFile } from "node:fs/promises";
import { stripTypeScriptTypes } from "node:module";
import { pathToFileURL } from "node:url";

const VERSION = "0.1.0";
const HELP = `Obscura VM — standalone Luau subset obfuscator

Usage:
  obscura <input.lua> [-o output.lua]
  obscura - [-o output.lua]
  obscura --verify <input.lua>
  obscura --self-test
  obscura --help

Options:
  -o, --output <file>  Write generated Luau to a file (default: stdout)
  --verify             Compile and compare VM emulator with reference evaluator
  --self-test          Run the complete built-in regression suite
  -h, --help           Show this help
  -v, --version        Show version

Use "-" as the input path to read source from stdin.
Requires Node.js 22.13 or newer. Normal obfuscation is fully local/offline.
`;

async function loadCore() {
  const source = await readFile(new URL("./core.ts", import.meta.url), "utf8");
  const javascript = stripTypeScriptTypes(source, { mode: "strip" });
  const url =
    "data:text/javascript;base64," + Buffer.from(javascript).toString("base64");
  return import(url);
}

function parseArgs(args) {
  const opts = {
    input: null,
    output: null,
    verify: false,
    selfTest: false,
    help: false,
    version: false,
  };
  for (let i = 0; i < args.length; i++) {
    const arg = args[i];
    if (arg === "--help" || arg === "-h") opts.help = true;
    else if (arg === "--version" || arg === "-v") opts.version = true;
    else if (arg === "--verify") opts.verify = true;
    else if (arg === "--self-test") opts.selfTest = true;
    else if (arg === "-o" || arg === "--output") {
      if (!args[i + 1] || args[i + 1].startsWith("-"))
        throw new Error("Missing path after " + arg);
      opts.output = args[++i];
    } else if (arg.startsWith("-") && arg !== "-")
      throw new Error("Unknown option: " + arg);
    else if (opts.input === null) opts.input = arg;
    else throw new Error("Unexpected extra input: " + arg);
  }
  if (opts.verify && (opts.output || opts.selfTest))
    throw new Error("--verify cannot be combined with --output or --self-test");
  if (opts.selfTest && (opts.input !== null || opts.output))
    throw new Error("--self-test takes no input or output path");
  if (!opts.help && !opts.version && !opts.selfTest && opts.input === null)
    throw new Error("No input file specified. Use --help for usage.");
  return opts;
}

async function readInput(file) {
  if (file === "-") {
    const chunks = [];
    for await (const chunk of process.stdin)
      chunks.push(Buffer.isBuffer(chunk) ? chunk : Buffer.from(chunk));
    return Buffer.concat(chunks).toString("utf8");
  }
  return readFile(file, "utf8");
}

async function main() {
  let opts;
  try {
    opts = parseArgs(process.argv.slice(2));
  } catch (error) {
    process.stderr.write(
      "obscura: " + error.message + "\n\nUse --help for usage.\n",
    );
    process.exitCode = 2;
    return;
  }
  if (opts.help) {
    process.stdout.write(HELP);
    return;
  }
  if (opts.version) {
    process.stdout.write("obscura " + VERSION + "\n");
    return;
  }
  const core = await loadCore();
  if (opts.selfTest) {
    const result = core.verifyCases();
    const failed = result.results.filter((x) => !x.passed);
    process.stdout.write(
      JSON.stringify(
        {
          passed: result.passed,
          total: result.total,
          failed: failed.length,
          failures: failed,
        },
        null,
        2,
      ) + "\n",
    );
    if (!result.passed) process.exitCode = 1;
    return;
  }
  const source = await readInput(opts.input);
  if (source.length > 100000)
    throw new Error("Source exceeds the 100 KB prototype limit");
  const compiled = core.compile(source);
  if (opts.verify) {
    const actual = core.emulate(
      compiled.instructions,
      compiled.constants,
      compiled.functionStarts,
    );
    const reference = core.referenceEvaluate(new core.Parser(source).program());
    const passed = core.sameValue(actual, reference);
    process.stdout.write(
      JSON.stringify(
        {
          passed,
          emulatorResult: actual,
          referenceResult: reference,
          luauCompilerValidated: false,
          robloxRuntimeValidated: false,
        },
        null,
        2,
      ) + "\n",
    );
    if (!passed) process.exitCode = 1;
    return;
  }
  if (opts.output) await writeFile(opts.output, compiled.code, "utf8");
  else process.stdout.write(compiled.code + "\n");
}

main().catch((error) => {
  process.stderr.write("obscura: " + (error?.message ?? String(error)) + "\n");
  process.exitCode = 1;
});
