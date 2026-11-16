type Token = { kind: string; value: string };
type Expr =
  | { type: "literal"; value: any }
  | { type: "name"; value: string }
  | { type: "unary"; op: string; right: Expr }
  | { type: "binary"; op: string; left: Expr; right: Expr }
  | { type: "index"; table: Expr; key: Expr }
  | { type: "table"; fields: { key: Expr; value: Expr }[] }
  | { type: "call"; name: string; args: Expr[] };
type Stmt =
  | { kind: "local"; name: string; value: Expr }
  | { kind: "localMulti"; names: string[]; values: Expr[] }
  | { kind: "set"; target: Expr; value: Expr }
  | { kind: "setMulti"; targets: Expr[]; values: Expr[] }
  | { kind: "return"; values: Expr[] }
  | {
      kind: "if";
      branches: { condition: Expr; body: Stmt[] }[];
      otherwise: Stmt[];
    }
  | { kind: "while" | "repeat"; condition: Expr; body: Stmt[] }
  | {
      kind: "forNum";
      name: string;
      start: Expr;
      limit: Expr;
      step: Expr;
      body: Stmt[];
    }
  | {
      kind: "forGen";
      names: string[];
      iterator: "pairs" | "ipairs" | "next" | "function";
      functionName: string | null;
      table: Expr;
      cursor: Expr | null;
      body: Stmt[];
    }
  | { kind: "function"; name: string; params: string[]; body: Stmt[] }
  | { kind: "do"; body: Stmt[] }
  | { kind: "break" | "continue" };

const precedence: Record<string, number> = {
  or: 1,
  and: 2,
  "==": 3,
  "~=": 3,
  "<": 3,
  "<=": 3,
  ">": 3,
  ">=": 3,
  "..": 4,
  "+": 5,
  "-": 5,
  "*": 6,
  "/": 6,
  "//": 6,
  "%": 6,
  "^": 8,
};

function tokenize(source: string): Token[] {
  const out: Token[] = [];
  let i = 0;
  while (i < source.length) {
    const c = source[i];
    if (/\s/.test(c)) {
      i++;
      continue;
    }
    if (source.startsWith("--", i)) {
      const end = source.indexOf("\n", i + 2);
      i = end < 0 ? source.length : end + 1;
      continue;
    }
    if (c === "'" || c === '"') {
      const quote = c;
      i++;
      let value = "";
      while (i < source.length && source[i] !== quote) {
        if (source[i] === "\\") {
          i++;
          const e = source[i++];
          const map: Record<string, string> = {
            a: "\x07",
            b: "\b",
            f: "\f",
            n: "\n",
            r: "\r",
            t: "\t",
            v: "\x0b",
            "\\": "\\",
            '"': '"',
            "'": "'",
          };
          if (!(e in map)) throw new Error("Unsupported string escape");
          value += map[e];
        } else value += source[i++];
      }
      if (source[i] !== quote) throw new Error("Unclosed string literal");
      i++;
      out.push({ kind: "string", value });
      continue;
    }
    const number = source.slice(i).match(/^\d+(?:\.\d+)?(?:[eE][+-]?\d+)?/);
    if (number) {
      out.push({ kind: "number", value: number[0] });
      i += number[0].length;
      continue;
    }
    const ident = source.slice(i).match(/^[A-Za-z_][A-Za-z0-9_]*/);
    if (ident) {
      out.push({ kind: "id", value: ident[0] });
      i += ident[0].length;
      continue;
    }
    const op = [
      "...",
      "..=",
      "//=",
      "+=",
      "-=",
      "*=",
      "/=",
      "%=",
      "..",
      "//",
      "==",
      "~=",
      "<=",
      ">=",
    ].find((x) => source.startsWith(x, i));
    if (op) {
      out.push({ kind: "sym", value: op });
      i += op.length;
      continue;
    }
    if ("+-*/%^()=,;<>[]{}.#".includes(c)) {
      out.push({ kind: "sym", value: c });
      i++;
      continue;
    }
    throw new Error("Unsupported token at character " + (i + 1) + ": " + c);
  }
  out.push({ kind: "eof", value: "" });
  return out;
}

export class Parser {
  t: Token[];
  p = 0;
  locals = new Set<string>();
  functions = new Map<string, { params: string[]; body: Stmt[] }>();
  nestedDepth = 0;
  functionDepth = 0;
  loopDepth = 0;
  constructor(source: string) {
    this.t = tokenize(source);
  }
  peek() {
    return this.t[this.p];
  }
  take() {
    return this.t[this.p++];
  }
  eat(v: string) {
    if (this.peek().value === v) {
      this.p++;
      return true;
    }
    return false;
  }
  need(v: string) {
    if (!this.eat(v))
      throw new Error("Expected '" + v + "' near '" + this.peek().value + "'");
  }
  expression(min = 0): Expr {
    const tok = this.take();
    let left: Expr;
    if (tok.value === "-" || tok.value === "not" || tok.value === "#")
      left = {
        type: "unary",
        op: tok.value,
        right: this.expression(tok.value === "not" ? 3 : 7),
      };
    else if (tok.value === "(") {
      left = this.expression();
      this.need(")");
    } else if (tok.kind === "number")
      left = { type: "literal", value: Number(tok.value) };
    else if (tok.kind === "string")
      left = { type: "literal", value: tok.value };
    else if (tok.value === "true" || tok.value === "false")
      left = { type: "literal", value: tok.value === "true" };
    else if (tok.value === "nil") left = { type: "literal", value: null };
    else if (
      tok.kind === "id" &&
      ![
        "local",
        "return",
        "if",
        "then",
        "else",
        "elseif",
        "end",
        "while",
        "do",
        "repeat",
        "until",
        "for",
        "function",
      ].includes(tok.value)
    )
      left = { type: "name", value: tok.value };
    else if (tok.value === "{") {
      const fields: { key: Expr; value: Expr }[] = [];
      let arrayIndex = 1;
      while (!this.eat("}")) {
        if (this.peek().kind === "eof")
          throw new Error("Unclosed table constructor");
        let key: Expr;
        let value: Expr;
        if (this.eat("[")) {
          key = this.expression();
          this.need("]");
          this.need("=");
          value = this.expression();
        } else if (
          this.peek().kind === "id" &&
          this.t[this.p + 1]?.value === "="
        ) {
          key = { type: "literal", value: this.take().value };
          this.need("=");
          value = this.expression();
        } else {
          key = { type: "literal", value: arrayIndex++ };
          value = this.expression();
        }
        fields.push({ key, value });
        if (this.eat("}")) break;
        if (!this.eat(",") && !this.eat(";"))
          throw new Error("Expected ',' or '}' in table constructor");
        if (this.eat("}")) break;
      }
      left = { type: "table", fields };
    } else throw new Error("Expected expression near '" + tok.value + "'");
    while (true) {
      if (this.eat("[")) {
        const key = this.expression();
        this.need("]");
        left = { type: "index", table: left, key };
        continue;
      }
      if (this.eat(".")) {
        const field = this.take();
        if (field.kind !== "id")
          throw new Error("Expected table field name after '.'");
        left = {
          type: "index",
          table: left,
          key: { type: "literal", value: field.value },
        };
        continue;
      }
      if (this.eat("(")) {
        if (left.type !== "name" || !this.functions.has(left.value))
          throw new Error(
            "Only previously declared local functions can be called",
          );
        const args: Expr[] = [];
        if (!this.eat(")")) {
          do {
            args.push(this.expression());
          } while (this.eat(","));
          this.need(")");
        }
        left = { type: "call", name: left.value, args };
        continue;
      }
      const op = this.peek().value;
      const prec = precedence[op];
      if (prec === undefined || prec < min) break;
      this.take();
      const right = this.expression(prec + (op === "^" || op === ".." ? 0 : 1));
      left = { type: "binary", op, left, right };
    }
    return left;
  }
  block(stops: string[], scoped = false): Stmt[] {
    const previousLocals = this.locals;
    if (scoped) this.locals = new Set(previousLocals);
    const declaredHere = new Set<string>();
    const statements: Stmt[] = [];
    while (this.peek().kind !== "eof" && !stops.includes(this.peek().value)) {
      if (this.eat(";")) continue;
      const word = this.peek().value;
      if (word === "break" || word === "continue") {
        this.take();
        if (this.loopDepth === 0)
          throw new Error("'" + word + "' is only valid inside a loop");
        statements.push({ kind: word });
      } else if (word === "local") {
        this.take();
        if (this.eat("function")) {
          if (this.functionDepth > 0 || this.nestedDepth > 0)
            throw new Error(
              "Nested function declarations and closures are not supported",
            );
          const name = this.take();
          if (
            name.kind !== "id" ||
            declaredHere.has(name.value) ||
            this.functions.has(name.value)
          )
            throw new Error(
              "Invalid or duplicate local function: " + name.value,
            );
          this.need("(");
          const params: string[] = [];
          if (!this.eat(")")) {
            do {
              const param = this.take();
              if (param.kind !== "id" || params.includes(param.value))
                throw new Error("Invalid or duplicate function parameter");
              params.push(param.value);
            } while (this.eat(","));
            this.need(")");
          }
          this.functions.set(name.value, { params, body: [] });
          const savedLocals = this.locals;
          this.locals = new Set(params);
          this.functionDepth++;
          const body = this.block(["end"]);
          this.functionDepth--;
          this.need("end");
          this.locals = savedLocals;
          this.locals.add(name.value);
          declaredHere.add(name.value);
          this.functions.set(name.value, { params, body });
          statements.push({ kind: "function", name: name.value, params, body });
        } else {
          const names: string[] = [];
          let name = this.take();
          if (name.kind !== "id") throw new Error("Invalid local declaration");
          names.push(name.value);
          while (this.eat(",")) {
            name = this.take();
            if (name.kind !== "id" || names.includes(name.value))
              throw new Error("Invalid or duplicate local declaration");
            names.push(name.value);
          }
          if (names.length > 2)
            throw new Error(
              "At most two local variables are supported in one declaration",
            );
          for (const n of names) {
            if (declaredHere.has(n) || this.functions.has(n))
              throw new Error("Invalid or duplicate local declaration: " + n);
          }
          const values: Expr[] = [];
          if (this.eat("=")) {
            values.push(this.expression());
            while (this.eat(",")) values.push(this.expression());
          }
          if (values.length > 2)
            throw new Error(
              "At most two initializer expressions are supported",
            );
          for (const n of names) {
            this.locals.add(n);
            declaredHere.add(n);
          }
          if (names.length === 1) {
            statements.push({
              kind: "local",
              name: names[0],
              value: values[0] ?? { type: "literal", value: null },
            });
          } else {
            statements.push({ kind: "localMulti", names, values });
          }
        }
      } else if (word === "return") {
        this.take();
        const values: Expr[] = [];
        if (
          this.peek().kind !== "eof" &&
          !stops.includes(this.peek().value) &&
          this.peek().value !== ";"
        ) {
          values.push(this.expression());
          while (this.eat(",")) values.push(this.expression());
        }
        if (values.length > 2)
          throw new Error("At most two return values are supported");
        statements.push({ kind: "return", values });
        this.eat(";");
        if (this.peek().kind !== "eof" && !stops.includes(this.peek().value))
          throw new Error(
            "Statements after return are not supported in the same block",
          );
        break;
      } else if (word === "if") {
        this.take();
        const branches: { condition: Expr; body: Stmt[] }[] = [];
        let condition = this.expression();
        this.need("then");
        this.nestedDepth++;
        let body = this.block(["elseif", "else", "end"], true);
        this.nestedDepth--;
        branches.push({ condition, body });
        while (this.eat("elseif")) {
          condition = this.expression();
          this.need("then");
          this.nestedDepth++;
          body = this.block(["elseif", "else", "end"], true);
          this.nestedDepth--;
          branches.push({ condition, body });
        }
        let otherwise: Stmt[] = [];
        if (this.eat("else")) {
          this.nestedDepth++;
          otherwise = this.block(["end"], true);
          this.nestedDepth--;
        }
        this.need("end");
        statements.push({ kind: "if", branches, otherwise });
      } else if (word === "do") {
        this.take();
        this.nestedDepth++;
        const body = this.block(["end"], true);
        this.nestedDepth--;
        this.need("end");
        statements.push({ kind: "do", body });
      } else if (word === "while") {
        this.take();
        const condition = this.expression();
        this.need("do");
        this.nestedDepth++;
        this.loopDepth++;
        const body = this.block(["end"], true);
        this.loopDepth--;
        this.nestedDepth--;
        this.need("end");
        statements.push({ kind: "while", condition, body });
      } else if (word === "repeat") {
        this.take();
        const previousLocals = this.locals;
        this.locals = new Set(previousLocals);
        this.nestedDepth++;
        this.loopDepth++;
        const body = this.block(["until"]);
        this.loopDepth--;
        this.nestedDepth--;
        this.need("until");
        const condition = this.expression();
        this.locals = previousLocals;
        statements.push({ kind: "repeat", condition, body });
      } else if (word === "for") {
        this.take();
        const first = this.take();
        if (first.kind !== "id") throw new Error("Invalid for-loop variable");
        const previousLocals = this.locals;
        if (this.eat("=")) {
          this.locals = new Set(previousLocals);
          this.locals.add(first.value);
          const start = this.expression();
          this.need(",");
          const limit = this.expression();
          let step: Expr = { type: "literal", value: 1 };
          if (this.eat(",")) step = this.expression();
          this.need("do");
          this.nestedDepth++;
          this.loopDepth++;
          const body = this.block(["end"], true);
          this.loopDepth--;
          this.nestedDepth--;
          this.need("end");
          this.locals = previousLocals;
          statements.push({
            kind: "forNum",
            name: first.value,
            start,
            limit,
            step,
            body,
          });
        } else {
          const names = [first.value];
          while (this.eat(",")) {
            const name = this.take();
            if (name.kind !== "id" || names.includes(name.value))
              throw new Error("Invalid or duplicate generic-for variable");
            names.push(name.value);
          }
          if (names.length > 2)
            throw new Error(
              "Generic for currently supports one or two loop variables",
            );
          this.need("in");
          const iterator = this.take().value;
          let table: Expr;
          let cursor: Expr | null = null;
          if (iterator === "next") {
            this.need(",");
            table = this.expression();
            if (this.eat(",")) cursor = this.expression();
          } else if (iterator === "pairs" || iterator === "ipairs") {
            this.need("(");
            table = this.expression();
            if (this.peek().value === ",")
              throw new Error(
                iterator + " expects exactly one table expression",
              );
            this.need(")");
          } else if (this.functions.has(iterator)) {
            this.need(",");
            table = this.expression();
            if (this.eat(",")) cursor = this.expression();
          } else {
            throw new Error(
              "Supported generic iterators are pairs(table), ipairs(table), next, table[, initialKey], or a previously declared local iterator function",
            );
          }
          this.need("do");
          this.locals = new Set(previousLocals);
          for (const name of names) this.locals.add(name);
          this.nestedDepth++;
          this.loopDepth++;
          const body = this.block(["end"], true);
          this.loopDepth--;
          this.nestedDepth--;
          this.need("end");
          this.locals = previousLocals;
          statements.push({
            kind: "forGen",
            names,
            iterator:
              iterator === "pairs" ||
              iterator === "ipairs" ||
              iterator === "next"
                ? iterator
                : "function",
            functionName: this.functions.has(iterator) ? iterator : null,
            table,
            cursor,
            body,
          });
        }
      } else if (word === "function") {
        throw new Error(
          "'function' declarations and function calls are not supported in this VM version",
        );
      } else {
        const targets: Expr[] = [this.expression()];
        while (this.eat(",")) targets.push(this.expression());
        if (targets.length > 2)
          throw new Error("At most two assignment targets are supported");
        for (const target of targets) {
          if (target.type !== "name" && target.type !== "index")
            throw new Error("Assignment target must be a local or table index");
          if (target.type === "name" && !this.locals.has(target.value))
            throw new Error(
              "Only assignments to declared locals are supported",
            );
        }
        const assignmentOp = this.take().value;
        if (targets.length > 1 && assignmentOp !== "=")
          throw new Error("Compound assignment supports only one target");
        const values: Expr[] = [this.expression()];
        while (this.eat(",")) values.push(this.expression());
        if (values.length > 2)
          throw new Error("At most two assignment expressions are supported");
        if (targets.length === 1) {
          const target = targets[0],
            right = values[0];
          if (assignmentOp === "=") {
            statements.push({ kind: "set", target, value: right });
          } else {
            const binaryOp: Record<string, string> = {
              "+=": "+",
              "-=": "-",
              "*=": "*",
              "/=": "/",
              "//=": "//",
              "%=": "%",
              "..=": "..",
            };
            const op = binaryOp[assignmentOp];
            if (!op)
              throw new Error(
                "Expected assignment operator, got '" + assignmentOp + "'",
              );
            statements.push({
              kind: "set",
              target,
              value: { type: "binary", op, left: target, right },
            });
          }
        } else {
          if (assignmentOp !== "=")
            throw new Error("Expected '=' for multiple assignment");
          statements.push({ kind: "setMulti", targets, values });
        }
      }
      this.eat(";");
    }
    if (scoped) this.locals = previousLocals;
    return statements;
  }
  program() {
    const result = this.block([]);
    if (this.peek().kind !== "eof")
      throw new Error("Unexpected token: " + this.peek().value);
    return result;
  }
}

function quoteLuau(value: string): string {
  let out = '"';
  for (let i = 0; i < value.length; i++) {
    const code = value.charCodeAt(i);
    const ch = value[i];
    if (ch === "\\") out += "\\\\";
    else if (ch === '"') out += '\\"';
    else if (ch === "\n") out += "\\n";
    else if (ch === "\r") out += "\\r";
    else if (ch === "\t") out += "\\t";
    else if (code < 32 || code === 127)
      out += "\\" + String(code).padStart(3, "0");
    else out += ch;
  }
  return out + '"';
}

export function compile(source: string) {
  const parser = new Parser(source);
  const statements = parser.program();
  const slots = new Map<string, number>();
  let next = 0;
  const constants: any[] = [];
  const instructions: number[][] = [];
  const functionDefs = statements.filter(
    (st): st is Extract<Stmt, { kind: "function" }> => st.kind === "function",
  );
  const functionIds = new Map(
    functionDefs.map((fn, index) => [fn.name, index + 1]),
  );
  const constIndex = (v: any) => {
    const key = JSON.stringify(v);
    let n = constants.findIndex((x) => JSON.stringify(x) === key);
    if (n < 0) {
      constants.push(v);
      n = constants.length - 1;
    }
    return n + 1;
  };
  const emit = (op: number, a = 0, b = 0, c = 0) => {
    instructions.push([op, a, b, c]);
    return instructions.length - 1;
  };
  const patch = (at: number, field: number, value: number) => {
    instructions[at][field] = value;
  };
  const alloc = () => ++next;
  const gen = (node: Expr): number => {
    if (node.type === "name") {
      const slot = slots.get(node.value);
      if (!slot) throw new Error("Unknown local: " + node.value);
      return slot;
    }
    const dst = alloc();
    if (node.type === "literal") {
      if (node.value === null) emit(2, dst);
      else emit(1, dst, constIndex(node.value));
    } else if (node.type === "unary") {
      const r = gen(node.right);
      emit(node.op === "-" ? 15 : node.op === "not" ? 16 : 31, dst, r);
    } else if (node.type === "index") {
      const table = gen(node.table);
      const key = gen(node.key);
      emit(28, dst, table, key);
    } else if (node.type === "table") {
      emit(27, dst);
      for (const field of node.fields) {
        const key = gen(field.key);
        const value = gen(field.value);
        emit(29, dst, key, value);
      }
    } else if (node.type === "call") {
      const functionId = functionIds.get(node.name);
      if (!functionId) throw new Error("Unknown local function: " + node.name);
      const args = node.args.map(gen);
      instructions.push([30, dst, functionId, args.length, ...args]);
    } else {
      if (node.op === "and" || node.op === "or") {
        const left = gen(node.left);
        emit(18, dst, left);
        let condition = left;
        if (node.op === "or") {
          condition = alloc();
          emit(16, condition, left);
        }
        const skipRight = emit(22, condition, 0);
        const right = gen(node.right);
        emit(18, dst, right);
        patch(skipRight, 2, instructions.length);
      } else {
        const a = gen(node.left),
          b = gen(node.right);
        const ops: Record<string, number> = {
          "+": 3,
          "-": 4,
          "*": 5,
          "/": 6,
          "%": 7,
          "^": 8,
          "..": 9,
          "==": 10,
          "~=": 11,
          "<": 12,
          "<=": 13,
          ">": 14,
          ">=": 17,
          "//": 32,
        };
        if (!ops[node.op]) throw new Error("Unsupported operator: " + node.op);
        emit(ops[node.op], dst, a, b);
      }
    }
    return dst;
  };
  const genCallMulti = (node: Expr): [number, number] => {
    if (node.type !== "call")
      throw new Error("Multiple results require a direct function call");
    const functionId = functionIds.get(node.name);
    if (!functionId) throw new Error("Unknown local function: " + node.name);
    const args = node.args.map(gen);
    const first = alloc(),
      second = alloc();
    instructions.push([36, first, second, functionId, args.length, ...args]);
    return [first, second];
  };
  const genValues = (values: Expr[]): number[] => {
    if (values.length === 1 && values[0].type === "call")
      return genCallMulti(values[0]);
    return values.map(gen);
  };
  const loopStack: { breaks: number[]; continues: number[] }[] = [];
  const genBlock = (body: Stmt[]) => {
    for (const st of body) {
      if (st.kind === "function") {
        continue;
      } else if (st.kind === "break" || st.kind === "continue") {
        const loop = loopStack[loopStack.length - 1];
        if (!loop)
          throw new Error("'" + st.kind + "' is only valid inside a loop");
        const jump = emit(21, 0);
        (st.kind === "break" ? loop.breaks : loop.continues).push(jump);
      } else if (st.kind === "do") {
        const saved = new Map(slots);
        genBlock(st.body);
        slots.clear();
        for (const [name, slot] of saved) slots.set(name, slot);
      } else if (st.kind === "local") {
        const slot = alloc();
        slots.set(st.name, slot);
        const value = gen(st.value);
        if (value !== slot) emit(18, slot, value);
      } else if (st.kind === "localMulti") {
        const values = genValues(st.values);
        const targetSlots = st.names.map(() => alloc());
        st.names.forEach((name, index) => slots.set(name, targetSlots[index]));
        targetSlots.forEach((slot, index) => {
          if (values[index] === undefined) emit(2, slot);
          else if (values[index] !== slot) emit(18, slot, values[index]);
        });
      } else if (st.kind === "setMulti") {
        const prepared = st.targets.map((target) => {
          if (target.type === "name") {
            const slot = slots.get(target.value);
            if (!slot) throw new Error("Unknown local: " + target.value);
            return { kind: "name" as const, slot };
          }
          const table = gen(target.table),
            key = gen(target.key);
          return { kind: "index" as const, table, key };
        });
        const values = genValues(st.values).map((source) => {
          const snapshot = alloc();
          emit(18, snapshot, source);
          return snapshot;
        });
        prepared.forEach((target, index) => {
          let valueReg = values[index];
          if (valueReg === undefined) {
            valueReg = alloc();
            emit(2, valueReg);
          }
          if (target.kind === "name") {
            if (target.slot !== valueReg) emit(18, target.slot, valueReg);
          } else emit(29, target.table, target.key, valueReg);
        });
      } else if (st.kind === "set") {
        if (st.target.type === "name") {
          const slot = slots.get(st.target.value);
          if (!slot) throw new Error("Unknown local: " + st.target.value);
          const value = gen(st.value);
          if (value !== slot) emit(18, slot, value);
        } else if (st.target.type === "index") {
          const table = gen(st.target.table);
          const key = gen(st.target.key);
          const value = gen(st.value);
          emit(29, table, key, value);
        }
      } else if (st.kind === "return") {
        if (st.values.length === 0) emit(19);
        else if (st.values.length === 1 && st.values[0].type === "call") {
          const [first, second] = genCallMulti(st.values[0]);
          emit(35, first, second);
        } else if (st.values.length === 1) emit(20, gen(st.values[0]));
        else emit(35, gen(st.values[0]), gen(st.values[1]));
      } else if (st.kind === "while") {
        const start = instructions.length;
        const condition = gen(st.condition);
        const exit = emit(22, condition, 0);
        const saved = new Map(slots);
        const loop = { breaks: [] as number[], continues: [] as number[] };
        loopStack.push(loop);
        genBlock(st.body);
        slots.clear();
        for (const [name, slot] of saved) slots.set(name, slot);
        const back = emit(21, start);
        const end = instructions.length;
        patch(exit, 2, end);
        for (const jump of loop.breaks) patch(jump, 1, end);
        for (const jump of loop.continues) patch(jump, 1, start);
        loopStack.pop();
      } else if (st.kind === "repeat") {
        const start = instructions.length;
        const saved = new Map(slots);
        const loop = { breaks: [] as number[], continues: [] as number[] };
        loopStack.push(loop);
        genBlock(st.body);
        const conditionStart = instructions.length;
        const condition = gen(st.condition);
        slots.clear();
        for (const [name, slot] of saved) slots.set(name, slot);
        emit(22, condition, start);
        const end = instructions.length;
        for (const jump of loop.breaks) patch(jump, 1, end);
        for (const jump of loop.continues) patch(jump, 1, conditionStart);
        loopStack.pop();
      } else if (st.kind === "forNum") {
        const outerSlots = new Map(slots);
        const slot = alloc();
        slots.set(st.name, slot);
        const first = gen(st.start),
          limit = gen(st.limit),
          step = gen(st.step);
        emit(18, slot, first);
        const start = instructions.length;
        const condition = alloc();
        instructions.push([25, condition, slot, limit, step]);
        const exit = emit(22, condition, 0);
        const saved = new Map(slots);
        const loop = { breaks: [] as number[], continues: [] as number[] };
        loopStack.push(loop);
        genBlock(st.body);
        slots.clear();
        for (const [name, oldSlot] of saved) slots.set(name, oldSlot);
        const stepStart = instructions.length;
        instructions.push([26, slot, step, 0]);
        emit(21, start);
        const end = instructions.length;
        patch(exit, 2, end);
        for (const jump of loop.breaks) patch(jump, 1, end);
        for (const jump of loop.continues) patch(jump, 1, stepStart);
        loopStack.pop();
        slots.clear();
        for (const [name, oldSlot] of outerSlots) slots.set(name, oldSlot);
      } else if (st.kind === "forGen") {
        const outerSlots = new Map(slots);
        const tableReg = gen(st.table);
        const cursorReg = alloc();
        if (st.cursor === null) emit(2, cursorReg);
        else {
          const initialCursor = gen(st.cursor);
          emit(18, cursorReg, initialCursor);
        }
        const keySlot = alloc();
        const valueSlot = alloc();
        slots.set(st.names[0], keySlot);
        if (st.names.length > 1) slots.set(st.names[1], valueSlot);
        const fetch = instructions.length;
        if (st.iterator === "function") {
          const functionId = st.functionName
            ? functionIds.get(st.functionName)
            : undefined;
          if (!functionId)
            throw new Error(
              "Unknown generic iterator function: " + st.functionName,
            );
          instructions.push([
            34,
            functionId,
            tableReg,
            cursorReg,
            keySlot,
            valueSlot,
            0,
          ]);
        } else {
          instructions.push([
            33,
            keySlot,
            valueSlot,
            tableReg,
            cursorReg,
            st.iterator === "ipairs" ? 2 : 1,
            0,
          ]);
        }
        const loop = { breaks: [] as number[], continues: [] as number[] };
        loopStack.push(loop);
        genBlock(st.body);
        const continueTarget = fetch;
        for (const jump of loop.continues) patch(jump, 1, continueTarget);
        emit(21, fetch);
        const end = instructions.length;
        patch(fetch, 6, end);
        for (const jump of loop.breaks) patch(jump, 1, end);
        loopStack.pop();
        slots.clear();
        for (const [name, oldSlot] of outerSlots) slots.set(name, oldSlot);
      } else if (st.kind === "if") {
        const genScoped = (block: Stmt[]) => {
          const saved = new Map(slots);
          genBlock(block);
          slots.clear();
          for (const [name, slot] of saved) slots.set(name, slot);
        };
        const exits: number[] = [];
        for (const branch of st.branches) {
          const condition = gen(branch.condition);
          const skip = emit(22, condition, 0);
          genScoped(branch.body);
          exits.push(emit(21, 0));
          patch(skip, 2, instructions.length);
        }
        genScoped(st.otherwise);
        for (const jump of exits) patch(jump, 1, instructions.length);
      }
    }
  };
  genBlock(statements);
  let maxRegisters = next;
  const mainSkip = functionDefs.length ? emit(21, 0) : -1;
  const functionStarts: number[] = [];
  for (const fn of functionDefs) {
    functionStarts.push(instructions.length);
    slots.clear();
    next = 0;
    for (const param of fn.params) slots.set(param, alloc());
    genBlock(fn.body);
    maxRegisters = Math.max(maxRegisters, next);
    emit(19);
  }
  if (mainSkip >= 0) patch(mainSkip, 1, instructions.length);
  const names = [
    "__K" + Math.random().toString(36).slice(2, 8),
    "__I" + Math.random().toString(36).slice(2, 8),
    "__R" + Math.random().toString(36).slice(2, 8),
    "__P" + Math.random().toString(36).slice(2, 8),
    "__X" + Math.random().toString(36).slice(2, 8),
    "__O" + Math.random().toString(36).slice(2, 8),
    "__J" + Math.random().toString(36).slice(2, 8),
    "__S" + Math.random().toString(36).slice(2, 8),
    "__F" + Math.random().toString(36).slice(2, 8),
  ];
  const [K, I, R, P, X, O, J, S, F] = names;
  const pool = constants
    .map((v) => (typeof v === "string" ? quoteLuau(v) : String(v)))
    .join(",");
  const code = [
    "local " + K + "={" + pool + "}",
    "local " +
      I +
      "={" +
      instructions.map((x) => "{" + x.join(",") + "}").join(",") +
      "}",
    "local " + R + "={}",
    "local " + S + "={}",
    "local " +
      F +
      "={" +
      functionStarts.map((v) => String(v + 1)).join(",") +
      "}",
    "local " + P + "=1",
    "while true do",
    "local " + X + "=" + I + "[" + P + "]",
    "if not " + X + " then break end",
    "local " + O + "=" + X + "[1]",
    "local " + J + "=false",
    "if " + O + "==1 then " + R + "[" + X + "[2]]=" + K + "[" + X + "[3]]",
    "elseif " + O + "==2 then " + R + "[" + X + "[2]]=nil",
    "elseif " +
      O +
      "==3 then " +
      R +
      "[" +
      X +
      "[2]]=" +
      R +
      "[" +
      X +
      "[3]]+" +
      R +
      "[" +
      X +
      "[4]]",
    "elseif " +
      O +
      "==4 then " +
      R +
      "[" +
      X +
      "[2]]=" +
      R +
      "[" +
      X +
      "[3]]-" +
      R +
      "[" +
      X +
      "[4]]",
    "elseif " +
      O +
      "==5 then " +
      R +
      "[" +
      X +
      "[2]]=" +
      R +
      "[" +
      X +
      "[3]]*" +
      R +
      "[" +
      X +
      "[4]]",
    "elseif " +
      O +
      "==6 then " +
      R +
      "[" +
      X +
      "[2]]=" +
      R +
      "[" +
      X +
      "[3]]/" +
      R +
      "[" +
      X +
      "[4]]",
    "elseif " +
      O +
      "==7 then " +
      R +
      "[" +
      X +
      "[2]]=" +
      R +
      "[" +
      X +
      "[3]]%" +
      R +
      "[" +
      X +
      "[4]]",
    "elseif " +
      O +
      "==8 then " +
      R +
      "[" +
      X +
      "[2]]=" +
      R +
      "[" +
      X +
      "[3]]^" +
      R +
      "[" +
      X +
      "[4]]",
    "elseif " +
      O +
      "==9 then " +
      R +
      "[" +
      X +
      "[2]]=" +
      R +
      "[" +
      X +
      "[3]].." +
      R +
      "[" +
      X +
      "[4]]",
    "elseif " +
      O +
      "==10 then " +
      R +
      "[" +
      X +
      "[2]]=" +
      R +
      "[" +
      X +
      "[3]]==" +
      R +
      "[" +
      X +
      "[4]]",
    "elseif " +
      O +
      "==11 then " +
      R +
      "[" +
      X +
      "[2]]=" +
      R +
      "[" +
      X +
      "[3]]~=" +
      R +
      "[" +
      X +
      "[4]]",
    "elseif " +
      O +
      "==12 then " +
      R +
      "[" +
      X +
      "[2]]=" +
      R +
      "[" +
      X +
      "[3]]<" +
      R +
      "[" +
      X +
      "[4]]",
    "elseif " +
      O +
      "==13 then " +
      R +
      "[" +
      X +
      "[2]]=" +
      R +
      "[" +
      X +
      "[3]]<=" +
      R +
      "[" +
      X +
      "[4]]",
    "elseif " +
      O +
      "==14 then " +
      R +
      "[" +
      X +
      "[2]]=" +
      R +
      "[" +
      X +
      "[3]]>" +
      R +
      "[" +
      X +
      "[4]]",
    "elseif " +
      O +
      "==15 then " +
      R +
      "[" +
      X +
      "[2]]=-" +
      R +
      "[" +
      X +
      "[3]]",
    "elseif " +
      O +
      "==16 then " +
      R +
      "[" +
      X +
      "[2]]=not " +
      R +
      "[" +
      X +
      "[3]]",
    "elseif " +
      O +
      "==17 then " +
      R +
      "[" +
      X +
      "[2]]=" +
      R +
      "[" +
      X +
      "[3]]>=" +
      R +
      "[" +
      X +
      "[4]]",
    "elseif " + O + "==18 then " + R + "[" + X + "[2]]=" + R + "[" + X + "[3]]",
    "elseif " +
      O +
      "==19 then if #" +
      S +
      "==0 then return end; if " +
      S +
      "[#" +
      S +
      "][7] then " +
      S +
      "[#" +
      S +
      "][3][" +
      S +
      "[#" +
      S +
      "][2]]=nil; " +
      S +
      "[#" +
      S +
      "][3][" +
      S +
      "[#" +
      S +
      "][4]]=nil; " +
      S +
      "[#" +
      S +
      "][3][" +
      S +
      "[#" +
      S +
      "][5]]=nil; " +
      R +
      "=" +
      S +
      "[#" +
      S +
      "][3]; " +
      P +
      "=" +
      S +
      "[#" +
      S +
      "][6]+1 else " +
      S +
      "[#" +
      S +
      "][3][" +
      S +
      "[#" +
      S +
      "][2]]=nil; if " +
      S +
      "[#" +
      S +
      "][4] then " +
      S +
      "[#" +
      S +
      "][3][" +
      S +
      "[#" +
      S +
      "][4]]=nil end; " +
      R +
      "=" +
      S +
      "[#" +
      S +
      "][3]; " +
      P +
      "=" +
      S +
      "[#" +
      S +
      "][1]+1 end; " +
      S +
      "[#" +
      S +
      "]=nil; " +
      J +
      "=true",
    "elseif " +
      O +
      "==20 then if #" +
      S +
      "==0 then return " +
      R +
      "[" +
      X +
      "[2]] end; if " +
      S +
      "[#" +
      S +
      "][7] then local __k=" +
      R +
      "[" +
      X +
      "[2]]; " +
      S +
      "[#" +
      S +
      "][3][" +
      S +
      "[#" +
      S +
      "][2]]=__k; " +
      S +
      "[#" +
      S +
      "][3][" +
      S +
      "[#" +
      S +
      "][4]]=nil; " +
      S +
      "[#" +
      S +
      "][3][" +
      S +
      "[#" +
      S +
      "][5]]=__k; " +
      R +
      "=" +
      S +
      "[#" +
      S +
      "][3]; if __k==nil then " +
      P +
      "=" +
      S +
      "[#" +
      S +
      "][6]+1 else " +
      P +
      "=" +
      S +
      "[#" +
      S +
      "][1]+1 end else " +
      S +
      "[#" +
      S +
      "][3][" +
      S +
      "[#" +
      S +
      "][2]]=" +
      R +
      "[" +
      X +
      "[2]]; if " +
      S +
      "[#" +
      S +
      "][4] then " +
      S +
      "[#" +
      S +
      "][3][" +
      S +
      "[#" +
      S +
      "][4]]=nil end; " +
      R +
      "=" +
      S +
      "[#" +
      S +
      "][3]; " +
      P +
      "=" +
      S +
      "[#" +
      S +
      "][1]+1 end; " +
      S +
      "[#" +
      S +
      "]=nil; " +
      J +
      "=true",
    "elseif " +
      O +
      "==35 then if #" +
      S +
      "==0 then return " +
      R +
      "[" +
      X +
      "[2]] end; if " +
      S +
      "[#" +
      S +
      "][7] then local __k=" +
      R +
      "[" +
      X +
      "[2]]; local __v=" +
      R +
      "[" +
      X +
      "[3]]; " +
      S +
      "[#" +
      S +
      "][3][" +
      S +
      "[#" +
      S +
      "][2]]=__k; " +
      S +
      "[#" +
      S +
      "][3][" +
      S +
      "[#" +
      S +
      "][4]]=__v; " +
      S +
      "[#" +
      S +
      "][3][" +
      S +
      "[#" +
      S +
      "][5]]=__k; " +
      R +
      "=" +
      S +
      "[#" +
      S +
      "][3]; if __k==nil then " +
      P +
      "=" +
      S +
      "[#" +
      S +
      "][6]+1 else " +
      P +
      "=" +
      S +
      "[#" +
      S +
      "][1]+1 end else " +
      S +
      "[#" +
      S +
      "][3][" +
      S +
      "[#" +
      S +
      "][2]]=" +
      R +
      "[" +
      X +
      "[2]]; if " +
      S +
      "[#" +
      S +
      "][4] then " +
      S +
      "[#" +
      S +
      "][3][" +
      S +
      "[#" +
      S +
      "][4]]=" +
      R +
      "[" +
      X +
      "[3]] end; " +
      R +
      "=" +
      S +
      "[#" +
      S +
      "][3]; " +
      P +
      "=" +
      S +
      "[#" +
      S +
      "][1]+1 end; " +
      S +
      "[#" +
      S +
      "]=nil; " +
      J +
      "=true",
    "elseif " + O + "==21 then " + P + "=" + X + "[2]+1; " + J + "=true",
    "elseif " +
      O +
      "==22 then if " +
      R +
      "[" +
      X +
      "[2]]==nil or " +
      R +
      "[" +
      X +
      "[2]]==false then " +
      P +
      "=" +
      X +
      "[3]+1; " +
      J +
      "=true end",
    "elseif " +
      O +
      "==23 then local __a=" +
      R +
      "[" +
      X +
      "[3]]; " +
      R +
      "[" +
      X +
      "[2]]=if __a==nil or __a==false then __a else " +
      R +
      "[" +
      X +
      "[4]]",
    "elseif " +
      O +
      "==24 then local __a=" +
      R +
      "[" +
      X +
      "[3]]; " +
      R +
      "[" +
      X +
      "[2]]=if __a~=nil and __a~=false then __a else " +
      R +
      "[" +
      X +
      "[4]]",
    "elseif " +
      O +
      "==25 then " +
      R +
      "[" +
      X +
      "[2]]=if " +
      R +
      "[" +
      X +
      "[5]]>=0 then " +
      R +
      "[" +
      X +
      "[3]]<=" +
      R +
      "[" +
      X +
      "[4]] else " +
      R +
      "[" +
      X +
      "[3]]>=" +
      R +
      "[" +
      X +
      "[4]]",
    "elseif " +
      O +
      "==26 then " +
      R +
      "[" +
      X +
      "[2]]=" +
      R +
      "[" +
      X +
      "[2]]+" +
      R +
      "[" +
      X +
      "[3]]",
    "elseif " + O + "==27 then " + R + "[" + X + "[2]]={}",
    "elseif " +
      O +
      "==28 then " +
      R +
      "[" +
      X +
      "[2]]=" +
      R +
      "[" +
      X +
      "[3]][" +
      R +
      "[" +
      X +
      "[4]]]",
    "elseif " +
      O +
      "==29 then " +
      R +
      "[" +
      X +
      "[2]][" +
      R +
      "[" +
      X +
      "[3]]]=" +
      R +
      "[" +
      X +
      "[4]]",
    "elseif " +
      O +
      "==30 then " +
      S +
      "[#" +
      S +
      "+1]={" +
      P +
      "," +
      X +
      "[2]," +
      R +
      "}; " +
      R +
      "={}; for __n=1," +
      X +
      "[4] do " +
      R +
      "[__n]=" +
      S +
      "[#" +
      S +
      "][3][" +
      X +
      "[__n+4]] end; " +
      P +
      "=" +
      F +
      "[" +
      X +
      "[3]]; " +
      J +
      "=true",
    "elseif " +
      O +
      "==31 then " +
      R +
      "[" +
      X +
      "[2]]=#" +
      R +
      "[" +
      X +
      "[3]]",
    "elseif " +
      O +
      "==32 then " +
      R +
      "[" +
      X +
      "[2]]=math.floor(" +
      R +
      "[" +
      X +
      "[3]]/" +
      R +
      "[" +
      X +
      "[4]])",
    "elseif " +
      O +
      "==33 then if " +
      X +
      "[6]==1 then local __a,__b=next(" +
      R +
      "[" +
      X +
      "[4]]," +
      R +
      "[" +
      X +
      "[5]]); if __a==nil then " +
      P +
      "=" +
      X +
      "[7]+1; " +
      J +
      "=true else " +
      R +
      "[" +
      X +
      "[2]]=__a; " +
      R +
      "[" +
      X +
      "[3]]=__b; " +
      R +
      "[" +
      X +
      "[5]]=__a end else local __a=(" +
      R +
      "[" +
      X +
      "[5]] or 0)+1; local __b=" +
      R +
      "[" +
      X +
      "[4]][__a]; if __b==nil then " +
      P +
      "=" +
      X +
      "[7]+1; " +
      J +
      "=true else " +
      R +
      "[" +
      X +
      "[2]]=__a; " +
      R +
      "[" +
      X +
      "[3]]=__b; " +
      R +
      "[" +
      X +
      "[5]]=__a end end",
    "elseif " +
      O +
      "==34 then " +
      S +
      "[#" +
      S +
      "+1]={" +
      P +
      "," +
      X +
      "[5]," +
      R +
      "," +
      X +
      "[6]," +
      X +
      "[4]," +
      X +
      "[7],true}; " +
      R +
      "={}; " +
      R +
      "[1]=" +
      S +
      "[#" +
      S +
      "][3][" +
      X +
      "[3]]; " +
      R +
      "[2]=" +
      S +
      "[#" +
      S +
      "][3][" +
      X +
      "[4]]; " +
      P +
      "=" +
      F +
      "[" +
      X +
      "[2]]; " +
      J +
      "=true",
    "elseif " +
      O +
      "==36 then " +
      S +
      "[#" +
      S +
      "+1]={" +
      P +
      "," +
      X +
      "[2]," +
      R +
      "," +
      X +
      "[3]}; " +
      R +
      "={}; for __n=1," +
      X +
      "[5] do " +
      R +
      "[__n]=" +
      S +
      "[#" +
      S +
      "][3][" +
      X +
      "[__n+5]] end; " +
      P +
      "=" +
      F +
      "[" +
      X +
      "[4]]; " +
      J +
      "=true",
    "end",
    "if not " + J + " then " + P + "=" + P + "+1 end",
    "end",
  ].join("\n");
  return {
    code,
    instructions,
    constants,
    functionStarts,
    registerCount: maxRegisters,
    instructionCount: instructions.length,
  };
}

export function emulate(
  instructions: number[][],
  constants: any[],
  functionStarts: number[] = [],
) {
  let r: any[] = [];
  const callStack: {
    pc: number;
    dest: number;
    regs: any[];
    iterator?: boolean;
    dest2?: number;
    controlReg?: number;
    exitTarget?: number;
  }[] = [];
  let pc = 0,
    steps = 0;
  while (pc < instructions.length) {
    if (++steps > 100000) throw new Error("VM instruction limit exceeded");
    const [op, a, b, c, d] = instructions[pc];
    if (op === 1) r[a] = constants[b - 1];
    else if (op === 2) r[a] = null;
    else if (op === 3) r[a] = r[b] + r[c];
    else if (op === 4) r[a] = r[b] - r[c];
    else if (op === 5) r[a] = r[b] * r[c];
    else if (op === 6) r[a] = r[b] / r[c];
    else if (op === 7) r[a] = ((r[b] % r[c]) + r[c]) % r[c];
    else if (op === 8) r[a] = r[b] ** r[c];
    else if (op === 9) {
      if (
        !["string", "number"].includes(typeof r[b]) ||
        !["string", "number"].includes(typeof r[c])
      )
        throw new Error("Attempt to concatenate a non-string/non-number value");
      r[a] = String(r[b]) + String(r[c]);
    } else if (op === 10) r[a] = r[b] === r[c];
    else if (op === 11) r[a] = r[b] !== r[c];
    else if (op === 12) r[a] = r[b] < r[c];
    else if (op === 13) r[a] = r[b] <= r[c];
    else if (op === 14) r[a] = r[b] > r[c];
    else if (op === 15) r[a] = -r[b];
    else if (op === 16)
      r[a] = r[b] === null || r[b] === undefined || r[b] === false;
    else if (op === 17) r[a] = r[b] >= r[c];
    else if (op === 18) r[a] = r[b];
    else if (op === 19) {
      if (!callStack.length) return null;
      const frame = callStack.pop()!;
      if (frame.iterator) {
        frame.regs[frame.dest] = null;
        frame.regs[frame.dest2!] = null;
        frame.regs[frame.controlReg!] = null;
        r = frame.regs;
        pc = frame.exitTarget!;
      } else {
        if (frame.dest2 !== undefined) {
          frame.regs[frame.dest] = null;
          frame.regs[frame.dest2] = null;
        }
        r = frame.regs;
        pc = frame.pc + 1;
      }
      continue;
    } else if (op === 20 || op === 35) {
      const first = r[a];
      const second = op === 35 ? r[b] : null;
      if (!callStack.length) return first ?? null;
      const frame = callStack.pop()!;
      if (frame.iterator) {
        frame.regs[frame.dest] = first;
        frame.regs[frame.dest2!] = second;
        frame.regs[frame.controlReg!] = first;
        r = frame.regs;
        pc =
          first === null || first === undefined
            ? frame.exitTarget!
            : frame.pc + 1;
      } else {
        frame.regs[frame.dest] = first;
        if (frame.dest2 !== undefined) frame.regs[frame.dest2] = second;
        r = frame.regs;
        pc = frame.pc + 1;
      }
      continue;
    } else if (op === 21) {
      pc = a;
      continue;
    } else if (op === 22) {
      if (r[a] === null || r[a] === undefined || r[a] === false) {
        pc = b;
        continue;
      }
    } else if (op === 23)
      r[a] =
        r[b] === null || r[b] === undefined || r[b] === false ? r[b] : r[c];
    else if (op === 24)
      r[a] =
        r[b] !== null && r[b] !== undefined && r[b] !== false ? r[b] : r[c];
    else if (op === 25) r[a] = r[d] >= 0 ? r[b] <= r[c] : r[b] >= r[c];
    else if (op === 26) r[a] = r[a] + r[b];
    else if (op === 27) r[a] = new Map();
    else if (op === 28) {
      if (!(r[b] instanceof Map))
        throw new Error("Attempt to index a non-table value");
      r[a] = r[b].get(r[c]) ?? null;
    } else if (op === 29) {
      if (!(r[a] instanceof Map))
        throw new Error("Attempt to index a non-table value");
      if (r[b] === null || r[b] === undefined)
        throw new Error("Table index is nil");
      if (r[c] === null || r[c] === undefined) r[a].delete(r[b]);
      else r[a].set(r[b], r[c]);
    } else if (op === 30) {
      const frame = { pc, dest: a, regs: r };
      callStack.push(frame);
      const newRegs: any[] = [];
      for (let i = 0; i < c; i++) newRegs[i + 1] = r[instructions[pc][i + 4]];
      r = newRegs;
      pc = functionStarts[b - 1];
      continue;
    } else if (op === 36) {
      const frame = { pc, dest: a, dest2: b, regs: r };
      callStack.push(frame);
      const newRegs: any[] = [];
      for (let i = 0; i < d; i++) newRegs[i + 1] = r[instructions[pc][i + 5]];
      r = newRegs;
      pc = functionStarts[c - 1];
      continue;
    } else if (op === 31) {
      if (!(r[b] instanceof Map))
        throw new Error("Attempt to get length of a non-table value");
      let length = 0;
      while (
        r[b].has(length + 1) &&
        r[b].get(length + 1) !== null &&
        r[b].get(length + 1) !== undefined
      )
        length++;
      r[a] = length;
    } else if (op === 32) r[a] = Math.floor(r[b] / r[c]);
    else if (op === 33) {
      const table = r[c];
      const cursorReg = instructions[pc][4];
      const mode = instructions[pc][5];
      const exitTarget = instructions[pc][6];
      if (!(table instanceof Map))
        throw new Error("Generic for expects a table");
      if (mode === 1) {
        const entries = [...table.entries()];
        let index = 0;
        if (r[cursorReg] !== null && r[cursorReg] !== undefined) {
          const found = entries.findIndex(([key]) => key === r[cursorReg]);
          if (found < 0) throw new Error("Invalid key to next");
          index = found + 1;
        }
        if (index >= entries.length) {
          pc = exitTarget;
          continue;
        }
        const [key, value] = entries[index];
        r[a] = key;
        r[b] = value;
        r[cursorReg] = key;
      } else {
        const index = (r[cursorReg] ?? 0) + 1;
        const value = table.get(index);
        if (value === null || value === undefined) {
          pc = exitTarget;
          continue;
        }
        r[a] = index;
        r[b] = value;
        r[cursorReg] = index;
      }
    } else if (op === 34) {
      const stateReg = b;
      const controlReg = c;
      const keyReg = d;
      const valueReg = instructions[pc][5];
      const exitTarget = instructions[pc][6];
      callStack.push({
        pc,
        dest: keyReg,
        dest2: valueReg,
        controlReg,
        exitTarget,
        regs: r,
        iterator: true,
      });
      const nextRegs: any[] = [];
      nextRegs[1] = r[stateReg];
      nextRegs[2] = r[controlReg];
      r = nextRegs;
      pc = functionStarts[a - 1];
      continue;
    }
    pc++;
  }
  return null;
}

export function referenceEvaluate(statements: Stmt[]) {
  type Scope = Map<string, any>;
  type Result = {
    returned: boolean;
    value: any;
    values?: any[];
    control?: "break" | "continue";
  };
  type Fn = Extract<Stmt, { kind: "function" }>;
  const functions = new Map<string, Fn>();
  const truthy = (v: any) => v !== null && v !== undefined && v !== false;
  const getName = (scopes: Scope[], name: string): any => {
    for (let i = scopes.length - 1; i >= 0; i--)
      if (scopes[i].has(name)) return scopes[i].get(name);
    throw new Error("Unknown local: " + name);
  };
  const setName = (scopes: Scope[], name: string, value: any) => {
    for (let i = scopes.length - 1; i >= 0; i--) {
      if (scopes[i].has(name)) {
        scopes[i].set(name, value);
        return;
      }
    }
    throw new Error("Unknown local: " + name);
  };
  let steps = 0;
  const evalExpr = (node: Expr, scopes: Scope[], depth: number): any => {
    if (++steps > 200000)
      throw new Error("Reference evaluator instruction limit exceeded");
    if (node.type === "literal") return node.value;
    if (node.type === "name") return getName(scopes, node.value);
    if (node.type === "table") {
      const table = new Map<any, any>();
      for (const field of node.fields) {
        const key = evalExpr(field.key, scopes, depth);
        const value = evalExpr(field.value, scopes, depth);
        if (key === null || key === undefined)
          throw new Error("Table index is nil");
        if (value === null || value === undefined) table.delete(key);
        else table.set(key, value);
      }
      return table;
    }
    if (node.type === "index") {
      const table = evalExpr(node.table, scopes, depth);
      const key = evalExpr(node.key, scopes, depth);
      if (!(table instanceof Map))
        throw new Error("Attempt to index a non-table value");
      if (key === null || key === undefined)
        throw new Error("Table index is nil");
      return table.get(key) ?? null;
    }
    if (node.type === "call") {
      if (depth > 500)
        throw new Error("Reference evaluator call-depth limit exceeded");
      const fn = functions.get(node.name);
      if (!fn) throw new Error("Unknown local function: " + node.name);
      const args = node.args.map((arg) => evalExpr(arg, scopes, depth));
      const local: Scope = new Map();
      fn.params.forEach((name, i) => local.set(name, args[i] ?? null));
      const result = runBlock(fn.body, [local], depth + 1);
      return result.returned ? (result.values?.[0] ?? null) : null;
    }
    if (node.type === "unary") {
      const value = evalExpr(node.right, scopes, depth);
      if (node.op === "-") return -value;
      if (node.op === "not") return !truthy(value);
      if (!(value instanceof Map))
        throw new Error("Attempt to get length of a non-table value");
      let length = 0;
      while (
        value.has(length + 1) &&
        value.get(length + 1) !== null &&
        value.get(length + 1) !== undefined
      )
        length++;
      return length;
    }
    if (node.op === "and") {
      const left = evalExpr(node.left, scopes, depth);
      return truthy(left) ? evalExpr(node.right, scopes, depth) : left;
    }
    if (node.op === "or") {
      const left = evalExpr(node.left, scopes, depth);
      return truthy(left) ? left : evalExpr(node.right, scopes, depth);
    }
    const left = evalExpr(node.left, scopes, depth);
    const right = evalExpr(node.right, scopes, depth);
    switch (node.op) {
      case "+":
        return left + right;
      case "-":
        return left - right;
      case "*":
        return left * right;
      case "/":
        return left / right;
      case "//":
        return Math.floor(left / right);
      case "%":
        return left - Math.floor(left / right) * right;
      case "^":
        return left ** right;
      case "..":
        if (
          !["string", "number"].includes(typeof left) ||
          !["string", "number"].includes(typeof right)
        )
          throw new Error(
            "Attempt to concatenate a non-string/non-number value",
          );
        return String(left) + String(right);
      case "==":
        return left === right;
      case "~=":
        return left !== right;
      case "<":
        return left < right;
      case "<=":
        return left <= right;
      case ">":
        return left > right;
      case ">=":
        return left >= right;
      default:
        throw new Error("Unsupported reference operator: " + node.op);
    }
  };
  const evalExprList = (
    values: Expr[],
    scopes: Scope[],
    depth: number,
  ): any[] => {
    if (values.length === 1 && values[0].type === "call") {
      const call = values[0];
      if (depth > 500)
        throw new Error("Reference evaluator call-depth limit exceeded");
      const fn = functions.get(call.name);
      if (!fn) throw new Error("Unknown local function: " + call.name);
      const args = call.args.map((arg) => evalExpr(arg, scopes, depth));
      const local: Scope = new Map();
      fn.params.forEach((name, i) => local.set(name, args[i] ?? null));
      const result = runBlock(fn.body, [local], depth + 1);
      return result.returned ? (result.values ?? [result.value]) : [];
    }
    return values.map((value) => evalExpr(value, scopes, depth));
  };
  const runBlock = (body: Stmt[], scopes: Scope[], depth: number): Result => {
    for (const st of body) {
      if (++steps > 200000)
        throw new Error("Reference evaluator instruction limit exceeded");
      if (st.kind === "function") functions.set(st.name, st);
      else if (st.kind === "local") {
        const value = evalExpr(st.value, scopes, depth);
        scopes[scopes.length - 1].set(st.name, value);
      } else if (st.kind === "localMulti") {
        const values = evalExprList(st.values, scopes, depth);
        st.names.forEach((name, index) =>
          scopes[scopes.length - 1].set(name, values[index] ?? null),
        );
      } else if (st.kind === "setMulti") {
        const prepared = st.targets.map((target) => {
          if (target.type === "name")
            return { kind: "name" as const, name: target.value };
          return {
            kind: "index" as const,
            table: evalExpr(target.table, scopes, depth),
            key: evalExpr(target.key, scopes, depth),
          };
        });
        const values = evalExprList(st.values, scopes, depth);
        prepared.forEach((target, index) => {
          const value = values[index] ?? null;
          if (target.kind === "name") setName(scopes, target.name, value);
          else {
            if (!(target.table instanceof Map))
              throw new Error("Attempt to index a non-table value");
            if (target.key === null || target.key === undefined)
              throw new Error("Table index is nil");
            if (value === null || value === undefined)
              target.table.delete(target.key);
            else target.table.set(target.key, value);
          }
        });
      } else if (st.kind === "set") {
        if (st.target.type === "name") {
          const value = evalExpr(st.value, scopes, depth);
          setName(scopes, st.target.value, value);
        } else if (st.target.type === "index") {
          const table = evalExpr(st.target.table, scopes, depth);
          const key = evalExpr(st.target.key, scopes, depth);
          const value = evalExpr(st.value, scopes, depth);
          if (!(table instanceof Map))
            throw new Error("Attempt to index a non-table value");
          if (key === null || key === undefined)
            throw new Error("Table index is nil");
          if (value === null || value === undefined) table.delete(key);
          else table.set(key, value);
        }
      } else if (st.kind === "return") {
        const values = evalExprList(st.values, scopes, depth);
        return { returned: true, value: values[0] ?? null, values };
      } else if (st.kind === "break" || st.kind === "continue") {
        return { returned: false, value: null, control: st.kind };
      } else if (st.kind === "do") {
        scopes.push(new Map());
        const result = runBlock(st.body, scopes, depth);
        scopes.pop();
        if (result.returned || result.control) return result;
      } else if (st.kind === "if") {
        let chosen: Stmt[] | null = null;
        for (const branch of st.branches) {
          if (truthy(evalExpr(branch.condition, scopes, depth))) {
            chosen = branch.body;
            break;
          }
        }
        if (chosen === null) chosen = st.otherwise;
        scopes.push(new Map());
        const result = runBlock(chosen, scopes, depth);
        scopes.pop();
        if (result.returned || result.control) return result;
      } else if (st.kind === "while") {
        let count = 0;
        while (truthy(evalExpr(st.condition, scopes, depth))) {
          if (++count > 100000)
            throw new Error("Reference while-loop limit exceeded");
          scopes.push(new Map());
          const result = runBlock(st.body, scopes, depth);
          scopes.pop();
          if (result.returned) return result;
          if (result.control === "break") break;
        }
      } else if (st.kind === "repeat") {
        let count = 0;
        do {
          if (++count > 100000)
            throw new Error("Reference repeat-loop limit exceeded");
          scopes.push(new Map());
          const result = runBlock(st.body, scopes, depth);
          if (result.returned) {
            scopes.pop();
            return result;
          }
          const shouldBreak = result.control === "break";
          const done = truthy(evalExpr(st.condition, scopes, depth));
          scopes.pop();
          if (shouldBreak || done) break;
        } while (true);
      } else if (st.kind === "forGen") {
        const table = evalExpr(st.table, scopes, depth);
        if (st.iterator === "function") {
          const fn = st.functionName
            ? functions.get(st.functionName)
            : undefined;
          if (!fn)
            throw new Error(
              "Unknown generic iterator function: " + st.functionName,
            );
          let cursor =
            st.cursor === null ? null : evalExpr(st.cursor, scopes, depth);
          let count = 0;
          scopes.push(new Map());
          while (true) {
            if (++count > 100000)
              throw new Error("Reference custom iterator limit exceeded");
            const iteratorScope: Scope = new Map();
            if (fn.params[0]) iteratorScope.set(fn.params[0], table);
            if (fn.params[1]) iteratorScope.set(fn.params[1], cursor);
            const returned = runBlock(fn.body, [iteratorScope], depth + 1);
            const key = returned.values?.[0] ?? null;
            const value = returned.values?.[1] ?? null;
            if (key === null || key === undefined) break;
            scopes[scopes.length - 1].set(st.names[0], key);
            if (st.names.length > 1)
              scopes[scopes.length - 1].set(st.names[1], value);
            scopes.push(new Map());
            const result = runBlock(st.body, scopes, depth);
            scopes.pop();
            if (result.returned) {
              scopes.pop();
              return result;
            }
            if (result.control === "break") break;
            cursor = key;
          }
          scopes.pop();
        } else {
          if (!(table instanceof Map))
            throw new Error("Generic for expects a table");
          scopes.push(new Map());
          let count = 0;
          if (st.iterator === "ipairs") {
            let key = 1;
            while (true) {
              const value = table.get(key);
              if (value === null || value === undefined) break;
              if (++count > 100000)
                throw new Error("Reference generic-for limit exceeded");
              scopes[scopes.length - 1].set(st.names[0], key);
              if (st.names.length > 1)
                scopes[scopes.length - 1].set(st.names[1], value);
              scopes.push(new Map());
              const result = runBlock(st.body, scopes, depth);
              scopes.pop();
              if (result.returned) {
                scopes.pop();
                return result;
              }
              if (result.control === "break") break;
              key++;
            }
          } else {
            let cursor: any =
              st.cursor === null ? null : evalExpr(st.cursor, scopes, depth);
            while (true) {
              const entries = [...table.entries()];
              let index = 0;
              if (cursor !== null && cursor !== undefined) {
                const found = entries.findIndex(([key]) => key === cursor);
                if (found < 0) throw new Error("Invalid key to next");
                index = found + 1;
              }
              if (index >= entries.length) break;
              if (++count > 100000)
                throw new Error("Reference generic-for limit exceeded");
              const [key, value] = entries[index];
              scopes[scopes.length - 1].set(st.names[0], key);
              if (st.names.length > 1)
                scopes[scopes.length - 1].set(st.names[1], value);
              scopes.push(new Map());
              const result = runBlock(st.body, scopes, depth);
              scopes.pop();
              if (result.returned) {
                scopes.pop();
                return result;
              }
              if (result.control === "break") break;
              cursor = key;
            }
          }
          scopes.pop();
        }
      } else if (st.kind === "forNum") {
        const start = evalExpr(st.start, scopes, depth);
        const limit = evalExpr(st.limit, scopes, depth);
        const step = evalExpr(st.step, scopes, depth);
        if (step === 0) throw new Error("Numeric for step cannot be zero");
        scopes.push(new Map([[st.name, start]]));
        let count = 0;
        for (
          let value = start;
          step > 0 ? value <= limit : value >= limit;
          value += step
        ) {
          if (++count > 100000)
            throw new Error("Reference numeric-for limit exceeded");
          scopes[scopes.length - 1].set(st.name, value);
          scopes.push(new Map());
          const result = runBlock(st.body, scopes, depth);
          scopes.pop();
          if (result.returned) {
            scopes.pop();
            return result;
          }
          if (result.control === "break") break;
        }
        scopes.pop();
      }
    }
    return { returned: false, value: null };
  };
  const result = runBlock(statements, [new Map()], 0);
  return result.returned ? result.value : null;
}

export function sameValue(a: any, b: any): boolean {
  const normalize = (value: any): any => {
    if (value instanceof Map)
      return {
        __table: [...value.entries()].map(([k, v]) => [
          normalize(k),
          normalize(v),
        ]),
      };
    if (Array.isArray(value)) return value.map(normalize);
    return value;
  };
  return JSON.stringify(normalize(a)) === JSON.stringify(normalize(b));
}

export function verifyCases() {
  const cases = [
    {
      name: "arithmetic precedence",
      source: "local x = 2 + 3 * 4\nreturn x",
      expected: 14,
    },
    {
      name: "unary exponent precedence",
      source: "local x = -2 ^ 2\nreturn x",
      expected: -4,
    },
    {
      name: "not comparison precedence",
      source: "local x = not 1 == 2\nreturn x",
      expected: true,
    },
    {
      name: "string constant",
      source: 'local x = "hello"\nreturn x',
      expected: "hello",
    },
    {
      name: "boolean value",
      source: "local x = true\nreturn x",
      expected: true,
    },
    { name: "nil value", source: "local x = nil\nreturn x", expected: null },
    {
      name: "Luau zero truthiness",
      source: "local x = not 0\nreturn x",
      expected: false,
    },
    {
      name: "and truthiness",
      source: "local x = 0 and 4\nreturn x",
      expected: 4,
    },
    {
      name: "or false value",
      source: "local x = false or 7\nreturn x",
      expected: 7,
    },
    {
      name: "if true",
      source: "local x = 1\nif x == 1 then x = 2 end\nreturn x",
      expected: 2,
    },
    {
      name: "if else",
      source: "local x = 1\nif x == 2 then x = 3 else x = 4 end\nreturn x",
      expected: 4,
    },
    {
      name: "elseif",
      source:
        "local x = 2\nlocal y = 0\nif x == 1 then y = 10 elseif x == 2 then y = 20 else y = 30 end\nreturn y",
      expected: 20,
    },
    {
      name: "while loop",
      source: "local x = 0\nwhile x < 4 do x = x + 1 end\nreturn x",
      expected: 4,
    },
    {
      name: "repeat until",
      source: "local x = 0\nrepeat x = x + 1 until x >= 3\nreturn x",
      expected: 3,
    },
    {
      name: "numeric for positive",
      source: "local sum = 0\nfor i = 1, 5 do sum = sum + i end\nreturn sum",
      expected: 15,
    },
    {
      name: "numeric for negative step",
      source:
        "local sum = 0\nfor i = 3, 1, -1 do sum = sum + i end\nreturn sum",
      expected: 6,
    },
    {
      name: "array table constructor",
      source: "local t = {10, 20}\nreturn t[1] + t[2]",
      expected: 30,
    },
    {
      name: "record table constructor",
      source: 'local t = {name = "Ali", value = 5}\nreturn t.value',
      expected: 5,
    },
    {
      name: "table write and dot access",
      source: 'local t = {}\nt["score"] = 7\nreturn t.score',
      expected: 7,
    },
    {
      name: "nested tables",
      source: "local t = {inner = {value = 9}}\nreturn t.inner.value",
      expected: 9,
    },
    {
      name: "mixed table fields",
      source: "local t = {[1] = 4, label = 8}\nreturn t[1] + t.label",
      expected: 12,
    },
    {
      name: "nil table assignment",
      source: "local t = {5}\nt[1] = nil\nreturn t[1]",
      expected: null,
    },
    {
      name: "table length",
      source: "local t = {4, 5, 6}\nreturn #t",
      expected: 3,
    },
    {
      name: "empty table length",
      source: "local t = {}\nreturn #t",
      expected: 0,
    },
    { name: "floor division positive", source: "return 7 // 2", expected: 3 },
    { name: "floor division negative", source: "return -7 // 2", expected: -4 },
    {
      name: "compound arithmetic assignment",
      source: "local x = 2\nx += 3\nx *= 4\nreturn x",
      expected: 20,
    },
    {
      name: "compound concat assignment",
      source: 'local x = "hello"\nx ..= " world"\nreturn x',
      expected: "hello world",
    },
    {
      name: "do block local scope",
      source: "local x = 2\ndo local y = 5; x += y end\nreturn x",
      expected: 7,
    },
    {
      name: "function no explicit return",
      source:
        "local function empty() local x = 1 end\nlocal y = empty()\nreturn y",
      expected: null,
    },
    {
      name: "while break",
      source:
        "local x = 0\nwhile x < 10 do x += 1; if x == 3 then break end end\nreturn x",
      expected: 3,
    },
    {
      name: "while continue",
      source:
        "local x = 0\nlocal sum = 0\nwhile x < 5 do x += 1; if x % 2 == 0 then continue end; sum += x end\nreturn sum",
      expected: 9,
    },
    {
      name: "numeric for break",
      source:
        "local sum = 0\nfor i = 1, 10 do if i == 4 then break end; sum += i end\nreturn sum",
      expected: 6,
    },
    {
      name: "numeric for continue",
      source:
        "local sum = 0\nfor i = 1, 5 do if i == 3 then continue end; sum += i end\nreturn sum",
      expected: 12,
    },
    {
      name: "ipairs generic for",
      source:
        "local t = {4, 5, 6}\nlocal sum = 0\nfor i, value in ipairs(t) do sum += i * value end\nreturn sum",
      expected: 32,
    },
    {
      name: "pairs generic for",
      source:
        "local t = {alpha = 4, beta = 6, [10] = 2}\nlocal sum = 0\nfor key, value in pairs(t) do sum += value end\nreturn sum",
      expected: 12,
    },
    {
      name: "next generic for",
      source:
        "local t = {alpha = 4, beta = 6, gamma = 8}\nlocal sum = 0\nfor key, value in next, t do sum += value end\nreturn sum",
      expected: 18,
    },
    {
      name: "local two values",
      source: "local a, b = 4, 7\nreturn a * 10 + b",
      expected: 47,
    },
    {
      name: "local two names without initializer",
      source: "local a, b\nreturn a == nil and b == nil",
      expected: true,
    },
    {
      name: "local one initializer fills second with nil",
      source: "local a, b = 5\nreturn a == 5 and b == nil",
      expected: true,
    },
    {
      name: "local two initializers",
      source: "local a, b = 3 + 2, 4 * 2\nreturn a + b",
      expected: 13,
    },
    {
      name: "local multi reads outer variables before shadowing",
      source: "local x, y = 4, 7\ndo local x, y = y, x; return x * 10 + y end",
      expected: 74,
    },
    {
      name: "multi assignment swap",
      source: "local a, b = 2, 9\na, b = b, a\nreturn a * 10 + b",
      expected: 92,
    },
    {
      name: "multi assignment with missing value",
      source: "local a, b = 2, 9\na, b = 5\nreturn a * 10 + (b or 0)",
      expected: 50,
    },
    {
      name: "multi assignment table indexes",
      source:
        "local t = {10, 20}\nlocal a, b = 1, 2\nt[a], t[b] = t[b], t[a]\nreturn t[1] * 10 + t[2]",
      expected: 210,
    },
    {
      name: "multi assignment right side before stores",
      source: "local a, b = 1, 2\na, b = b + 1, a + 3\nreturn a * 10 + b",
      expected: 34,
    },
    {
      name: "local multi in nested block",
      source:
        "local total = 0\ndo local a, b = 4, 6; total = a + b end\nreturn total",
      expected: 10,
    },
    {
      name: "custom iterator generic for",
      source:
        "local function iter(state, control)\n local key = (control or 0) + 1\n if key > #state then return nil end\n return key, state[key]\nend\nlocal data = {10, 20, 30}\nlocal total = 0\nfor key, value in iter, data, nil do total += value end\nreturn total",
      expected: 60,
    },
    {
      name: "custom iterator ends without a return",
      source:
        "local function iter(state, control)\n if control then return control + 1 end\nend\nlocal total = 0\nfor key in iter, 10 do total += key end\nreturn total",
      expected: 0,
    },
    {
      name: "custom iterator one return value",
      source:
        "local function iter(state, control)\n local key = (control or 0) + 1\n if key > #state then return nil end\n return key\nend\nlocal data = {10, 20, 30}\nlocal total = 0\nfor key in iter, data do total += key end\nreturn total",
      expected: 6,
    },
    {
      name: "custom iterator break",
      source:
        "local function iter(state, control)\n local key = (control or 0) + 1\n if key > #state then return nil end\n return key, state[key]\nend\nlocal data = {10, 20, 30}\nlocal total = 0\nfor key, value in iter, data do if key == 3 then break end; total += value end\nreturn total",
      expected: 30,
    },
    {
      name: "custom iterator continue",
      source:
        "local function iter(state, control)\n local key = (control or 0) + 1\n if key > #state then return nil end\n return key, state[key]\nend\nlocal data = {10, 20, 30}\nlocal total = 0\nfor key, value in iter, data do if key == 2 then continue end; total += value end\nreturn total",
      expected: 40,
    },
    {
      name: "custom iterator initial control",
      source:
        "local function iter(state, control)\n local key = (control or 0) + 1\n if key > #state then return nil end\n return key, state[key]\nend\nlocal data = {10, 20, 30}\nlocal total = 0\nfor key, value in iter, data, 1 do total += value end\nreturn total",
      expected: 50,
    },
    {
      name: "custom iterator calls helper function",
      source:
        "local function step(value)\n return value + 1\nend\nlocal function iter(state, control)\n local key = step(control or 0)\n if key > #state then return nil end\n return key, state[key]\nend\nlocal data = {3, 4, 5}\nlocal total = 0\nfor key, value in iter, data do total += value end\nreturn total",
      expected: 12,
    },
    {
      name: "custom iterator preserves false key",
      source:
        "local function iter(state, control)\n if control == nil then return false, 7 end\n return nil\nend\nlocal total = 0\nfor key, value in iter, false do total += value end\nreturn total",
      expected: 7,
    },
    {
      name: "nested custom iterator loops",
      source:
        "local function iter(state, control)\n local key = (control or 0) + 1\n if key > #state then return nil end\n return key, state[key]\nend\nlocal data = {2, 3}\nlocal total = 0\nfor i, x in iter, data do for j, y in iter, data do total += x * y end end\nreturn total",
      expected: 25,
    },
    {
      name: "next one loop variable",
      source:
        "local t = {10, 20, 30}\nlocal sum = 0\nfor key in next, t do sum += key end\nreturn sum",
      expected: 6,
    },
    {
      name: "next starts after initial key",
      source:
        'local t = {alpha = 4, beta = 6, gamma = 8}\nlocal sum = 0\nfor key, value in next, t, "alpha" do sum += value end\nreturn sum',
      expected: 14,
    },
    {
      name: "next empty table",
      source:
        "local count = 0\nfor key, value in next, {} do count += 1 end\nreturn count",
      expected: 0,
    },
    {
      name: "next generic break",
      source:
        "local t = {2, 3, 4}\nlocal sum = 0\nfor key, value in next, t do if key == 2 then break end; sum += value end\nreturn sum",
      expected: 2,
    },
    {
      name: "pairs one loop variable",
      source:
        "local t = {10, 20, 30}\nlocal sum = 0\nfor key in pairs(t) do sum += key end\nreturn sum",
      expected: 6,
    },
    {
      name: "ipairs stops at first hole",
      source:
        "local t = {[1] = 5, [3] = 20}\nlocal sum = 0\nfor i, value in ipairs(t) do sum += value end\nreturn sum",
      expected: 5,
    },
    {
      name: "generic for break",
      source:
        "local t = {2, 3, 4, 5}\nlocal sum = 0\nfor i, value in ipairs(t) do if i == 3 then break end; sum += value end\nreturn sum",
      expected: 5,
    },
    {
      name: "generic for continue",
      source:
        "local t = {2, 3, 4, 5}\nlocal sum = 0\nfor i, value in ipairs(t) do if i == 2 then continue end; sum += value end\nreturn sum",
      expected: 11,
    },
    {
      name: "empty pairs",
      source:
        "local sum = 0\nfor key, value in pairs({}) do sum += value end\nreturn sum",
      expected: 0,
    },
    {
      name: "empty ipairs",
      source:
        "local count = 0\nfor i, value in ipairs({}) do count += 1 end\nreturn count",
      expected: 0,
    },
    {
      name: "ipairs continues through false values",
      source:
        "local t = {false, 2, false}\nlocal count = 0\nfor i, value in ipairs(t) do count += 1 end\nreturn count",
      expected: 3,
    },
    {
      name: "generic for preserves shadowed outer local",
      source:
        "local i = 99\nlocal t = {5, 6}\nlocal sum = 0\nfor i, value in ipairs(t) do sum += value end\nreturn sum + i",
      expected: 110,
    },
    {
      name: "nested generic loops",
      source:
        "local a = {2, 3}\nlocal b = {4, 5}\nlocal sum = 0\nfor i, x in ipairs(a) do for j, y in ipairs(b) do sum += x * y end end\nreturn sum",
      expected: 45,
    },
    {
      name: "one-variable ipairs",
      source:
        "local t = {8, 9, 10}\nlocal sum = 0\nfor value in ipairs(t) do sum += value end\nreturn sum",
      expected: 6,
    },
    {
      name: "repeat continue",
      source:
        "local x = 0\nrepeat x += 1; if x < 3 then continue end until x >= 3\nreturn x",
      expected: 3,
    },
    {
      name: "escaped string literals",
      source: 'local x = "quote: \\" slash: \\\\ newline: \\n"\nreturn x',
      expected: 'quote: " slash: \\ newline: \n',
    },
    {
      name: "function call",
      source: "local function add(a, b) return a + b end\nreturn add(4, 5)",
      expected: 9,
    },
    {
      name: "nested function calls",
      source:
        "local function add(a, b) return a + b end\nlocal function twice(x) return add(x, x) end\nreturn twice(6)",
      expected: 12,
    },
    {
      name: "local declaration expands two function returns",
      source:
        "local function getValues() return 4, 7 end\nlocal a, b = getValues()\nreturn a * 10 + b",
      expected: 47,
    },
    {
      name: "parallel assignment expands two function returns",
      source:
        "local function getValues() return 4, 7 end\nlocal a, b = 1, 2\na, b = getValues()\nreturn a * 10 + b",
      expected: 47,
    },
    {
      name: "table targets receive two function returns",
      source:
        "local function getValues() return 4, 7 end\nlocal t = {}\nt[1], t[2] = getValues()\nreturn t[1] * 10 + t[2]",
      expected: 47,
    },
    {
      name: "one function return fills second with nil",
      source:
        "local function getOne() return 5 end\nlocal a, b = getOne()\nreturn a == 5 and b == nil",
      expected: true,
    },
    {
      name: "no function return fills both with nil",
      source:
        "local function getNone() end\nlocal a, b = getNone()\nreturn a == nil and b == nil",
      expected: true,
    },
    {
      name: "nested function forwards two returns",
      source:
        "local function getValues() return 3, 8 end\nlocal function forward() return getValues() end\nlocal a, b = forward()\nreturn a * 10 + b",
      expected: 38,
    },
    {
      name: "ordinary expression list keeps single call result",
      source:
        "local function getValues() return 4, 7 end\nlocal a, b = 1, getValues()\nreturn a * 10 + b",
      expected: 14,
    },
    {
      name: "recursive function",
      source:
        "local function fact(n) if n <= 1 then return 1 else return n * fact(n - 1) end end\nreturn fact(5)",
      expected: 120,
    },
    {
      name: "and short circuit",
      source:
        "local function loop() return loop() end\nreturn false and loop()",
      expected: false,
    },
    {
      name: "or short circuit",
      source: "local function loop() return loop() end\nreturn true or loop()",
      expected: true,
    },
    {
      name: "if block local",
      source: "local x = 1\nif true then local y = 4\n x = x + y end\nreturn x",
      expected: 5,
    },
    {
      name: "shadowed block local",
      source: "local x = 1\nif true then local x = 7\n x = x + 1 end\nreturn x",
      expected: 1,
    },
    {
      name: "while block local",
      source:
        "local x = 0\nwhile x < 3 do local step = 1\n x = x + step end\nreturn x",
      expected: 3,
    },
    {
      name: "repeat local visible to until",
      source:
        "local x = 0\nrepeat local step = 1\n x = x + step until step == 1\nreturn x",
      expected: 1,
    },
    {
      name: "numeric for shadows outer local",
      source:
        "local i = 10\nlocal sum = 0\nfor i = 1, 3 do sum = sum + i end\nreturn sum + i",
      expected: 16,
    },
  ];
  const results = cases.map((test) => {
    const c = compile(test.source);
    const actual = emulate(c.instructions, c.constants, c.functionStarts);
    const reference = referenceEvaluate(new Parser(test.source).program());
    const stringEscapesValid =
      test.name !== "escaped string literals" ||
      c.code.includes('quote: \\" slash: \\\\ newline: \\n');
    const usesGenericFor =
      /\bfor\s+[A-Za-z_]\w*(?:\s*,\s*[A-Za-z_]\w*)?\s+in\b/.test(test.source);
    const usesMultiCall =
      /(?:local\s+)?[A-Za-z_]\w*\s*,\s*[A-Za-z_]\w*\s*=\s*[A-Za-z_]\w*\s*\(/.test(
        test.source,
      );
    const multiCallCodeValid =
      !usesMultiCall ||
      (c.instructions.some((instruction) => instruction[0] === 36) &&
        c.code.includes("==36 then") &&
        c.code.includes("==35 then"));
    const customIterator = test.source.match(
      /\bfor\s+[A-Za-z_]\w*(?:\s*,\s*[A-Za-z_]\w*)?\s+in\s+([A-Za-z_]\w*)\s*,/,
    );
    const usesCustomIterator =
      !!customIterator &&
      new RegExp("\\blocal\\s+function\\s+" + customIterator[1] + "\\b").test(
        test.source,
      );
    const genericCodeValid =
      !usesGenericFor ||
      (usesCustomIterator
        ? c.instructions.some((instruction) => instruction[0] === 34) &&
          c.code.includes("==34 then") &&
          (!/\breturn\s+[^,\n]+,\s*[^\n]+/.test(test.source) ||
            (c.instructions.some((instruction) => instruction[0] === 35) &&
              c.code.includes("==35 then")))
        : c.instructions.some((instruction) => instruction[0] === 33) &&
          c.code.includes("==33 then"));
    return {
      name: test.name,
      passed:
        sameValue(actual, test.expected) &&
        sameValue(actual, reference) &&
        stringEscapesValid &&
        genericCodeValid &&
        multiCallCodeValid,
      expected: test.expected,
      actual,
      reference,
      genericCodeValid,
      multiCallCodeValid,
    };
  });
  const many =
    Array.from(
      { length: 250 },
      (_, i) => "local v" + (i + 1) + " = " + (i + 1),
    ).join("\n") + "\nreturn v250";
  const large = compile(many);
  const largeActual = emulate(
    large.instructions,
    large.constants,
    large.functionStarts,
  );
  const largeReference = referenceEvaluate(new Parser(many).program());
  results.push({
    name: "250 locals",
    passed:
      sameValue(largeActual, 250) && sameValue(largeActual, largeReference),
    expected: 250,
    actual: largeActual,
    reference: largeReference,
  });
  const rejectedCases = [
    { name: "reject three local names", source: "local a, b, c = 1, 2, 3" },
    {
      name: "reject three assignment targets",
      source: "local a, b = 1, 2\na, b, a = 3, 4, 5",
    },
    {
      name: "reject three initializer expressions",
      source: "local a, b = 1, 2, 3",
    },
    {
      name: "reject compound multi assignment",
      source: "local a, b = 1, 2\na, b += 1",
    },
    { name: "reject global function calls", source: 'return print("no")' },
    {
      name: "reject custom generic iterators",
      source: "for key, value in customIterator({}) do end",
    },
    {
      name: "reject unsupported iterator expression",
      source: "for key, value in custom, {}, nil do end",
    },
    { name: "reject more than two return values", source: "return 1, 2, 3" },
    { name: "reject break outside loop", source: "break" },
    { name: "reject continue outside loop", source: "continue" },
    {
      name: "reject nested closures",
      source:
        "local function outer() local function inner() return 1 end return inner() end return outer()",
    },
    {
      name: "reject non-local function declarations",
      source: "function f() return 1 end",
    },
  ];
  for (const test of rejectedCases) {
    let rejected = false;
    try {
      compile(test.source);
    } catch {
      rejected = true;
    }
    results.push({
      name: test.name,
      passed: rejected,
      expected: "rejected",
      actual: rejected ? "rejected" : "compiled",
      reference: null,
    });
  }
  return {
    passed: results.every((x) => x.passed),
    total: results.length,
    results,
  };
}
