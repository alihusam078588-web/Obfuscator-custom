import { defineApp, object, query, router, string } from "apps";
import {
  compile,
  emulate,
  Parser,
  referenceEvaluate,
  sameValue,
  verifyCases,
} from "./core.js";

const SourceInput = object({
  source: string({
    description:
      "Luau source. Supports two-variable local declarations and parallel assignments, including expanding a direct local function call into two results, local functions, tables, expressions, conditionals, loops, and generic for with pairs(table), ipairs(table), next, table[, initialKey], and previously declared local iterator functions with up to two return values. Closures and varargs are rejected.",
  }),
});
const VerifyInput = object({
  source: string({
    description:
      "Supported Luau source to compile and execute in the reference VM emulator.",
  }),
});

export default defineApp(
  {},
  {
    tools: router({
      obfuscate: query(
        {
          description:
            "Compile supported Luau into table-register VM bytecode. Unsupported syntax is rejected explicitly.",
          input: SourceInput,
        },
        async (_ctx, { source }) => {
          if (source.length > 100000)
            throw new Error("Source exceeds the 100 KB prototype limit");
          const result = compile(source);
          return {
            success: true,
            mode: "table-register-vm",
            output: result.code,
            metrics: {
              registers: result.registerCount,
              instructions: result.instructionCount,
              constants: result.constants.length,
              functions: result.functionStarts.length,
              generatedBytes: result.code.length,
              wrapperLocals: 10,
            },
            limitations: [
              "Supports top-level locals/assignments, compound assignments, scoped do blocks, break/continue, floor division, table constructors/indexing/length, local functions with positional arguments, two-value returns including direct function-call expansion in two-variable declarations and assignments, two-value returns for custom iterators, if/elseif/else, while, repeat/until, numeric for, and generic for with pairs, ipairs, next, or local iterator functions.",
              "Closures, nested function declarations, more than two return values or assignment variables, varargs, method calls, arbitrary iterator expressions, type annotations, and Roblox APIs are not supported yet.",
              "No official Luau compiler or Roblox runtime is connected; generated code still needs external runtime validation.",
            ],
          };
        },
      ),
      verify: query(
        {
          description:
            "Compile supported source and execute VM instructions in a bounded JavaScript reference emulator. This is not Roblox runtime verification.",
          input: VerifyInput,
        },
        async (_ctx, { source }) => {
          const result = compile(source);
          const actual = emulate(
            result.instructions,
            result.constants,
            result.functionStarts,
          );
          const reference = referenceEvaluate(new Parser(source).program());
          const referencePassed = sameValue(actual, reference);
          return {
            syntaxParsed: true,
            bytecodeGenerated: true,
            emulatorPassed: true,
            emulatorResult: actual,
            referencePassed,
            referenceResult: reference,
            luauCompilerValidated: false,
            robloxRuntimeValidated: false,
            note: referencePassed
              ? "VM emulator agrees with the independent AST reference evaluator. Official Luau compiler and Roblox runtime validation are not available in this app environment."
              : "VM emulator and reference evaluator disagree; do not trust this compilation.",
          };
        },
      ),
      selfTest: query(
        {
          description:
            "Run the built-in regression suite for expressions, tables, local functions, all supported loop forms, generic pairs/ipairs, and rejected syntax.",
          input: object({}),
        },
        async () => verifyCases(),
      ),
    }),
  },
);
