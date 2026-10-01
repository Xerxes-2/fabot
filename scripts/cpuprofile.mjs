// Three reads of a V8 `.cpuprofile` (what `npm run profile -- --cpuprofile`
// and `node --cpu-prof` write), each answering one question about one
// function by name:
//
//   under    where its time went: inclusive ms of each callee path under
//            every node named <fnName>, `depth` levels deep (default 2)
//   flat     what its subtree spends itself: self ms by function over the
//            whole subtree under <fnName>, every node counted once
//   callers  who pays for it: inclusive ms of <fnName> by its caller chain,
//            `depth` named callers up (default 2), library frames skipped
//            and recursive nesting counted at its outermost call
//
// Usage: cpuprofile.mjs <file.cpuprofile> (under|flat|callers) <fnName> [depth]
import { readFileSync } from "node:fs";

const usage = "usage: cpuprofile.mjs <file.cpuprofile> (under|flat|callers) <fnName> [depth]";

const fail = (msg) => {
  console.error(msg);
  process.exit(1);
};

const [file, mode, name, depthArg] = process.argv.slice(2);
if (!file || !["under", "flat", "callers"].includes(mode) || !name) fail(usage);
const depth = depthArg === undefined ? 2 : Number(depthArg);
if (!(Number.isInteger(depth) && depth > 0)) fail(`depth must be a positive integer\n${usage}`);

let profile;
try {
  profile = JSON.parse(readFileSync(file, "utf8"));
} catch (err) {
  fail(`cannot read ${file}: ${err.message}`);
}
if (!Array.isArray(profile.nodes) || !Array.isArray(profile.samples) || !Array.isArray(profile.timeDeltas)) {
  fail(`${file} is not a .cpuprofile: it needs nodes, samples and timeDeltas`);
}

const byId = new Map(profile.nodes.map((n) => [n.id, n]));
const parent = new Map();
for (const n of profile.nodes) for (const c of n.children ?? []) parent.set(c, n.id);

// Self time per node: each sample charges the time since the one before it.
const self = new Map();
profile.samples.forEach((id, i) => self.set(id, (self.get(id) ?? 0) + (profile.timeDeltas[i] ?? 0)));

const inclusive = new Map();
const incl = (n) => {
  if (inclusive.has(n.id)) return inclusive.get(n.id);
  let total = self.get(n.id) ?? 0;
  for (const c of n.children ?? []) total += incl(byId.get(c));
  inclusive.set(n.id, total);
  return total;
};
profile.nodes.forEach(incl);

const label = (n) => `${n.callFrame.functionName || "(anonymous)"}:${n.callFrame.lineNumber}`;
const ms = (us) => (us / 1000).toFixed(1).padStart(8);
const named = profile.nodes.filter((n) => n.callFrame.functionName === name);
if (named.length === 0) fail(`no node named ${name} in ${file}`);

// A node nested under another node of the same name is already inside that
// one's inclusive time; counting it again would double a recursive function.
const outermost = named.filter((n) => {
  for (let a = parent.get(n.id); a !== undefined; a = parent.get(a)) {
    if (byId.get(a).callFrame.functionName === name) return false;
  }
  return true;
});

const top = (agg, n) =>
  [...agg].sort((a, b) => b[1] - a[1]).slice(0, n).forEach(([key, us]) => console.log(`${ms(us)} ${key}`));

if (mode === "under") {
  const agg = new Map();
  const walk = (n, level, path) => {
    if (level > depth) return;
    for (const c of n.children ?? []) {
      const child = byId.get(c);
      const key = `${path} > ${label(child)}`;
      agg.set(key, (agg.get(key) ?? 0) + inclusive.get(c));
      walk(child, level + 1, key);
    }
  };
  let total = 0;
  for (const n of outermost) {
    total += inclusive.get(n.id);
    walk(n, 1, name);
  }
  console.log(`${name} total ms ${(total / 1000).toFixed(1)}`);
  top(agg, 40);
} else if (mode === "flat") {
  const agg = new Map();
  const seen = new Set();
  const walk = (n) => {
    if (seen.has(n.id)) return;
    seen.add(n.id);
    agg.set(label(n), (agg.get(label(n)) ?? 0) + (self.get(n.id) ?? 0));
    for (const c of n.children ?? []) walk(byId.get(c));
  };
  named.forEach(walk);
  const total = [...agg.values()].reduce((a, b) => a + b, 0);
  console.log(`${name} subtree ms ${(total / 1000).toFixed(1)}`);
  top(agg, 20);
} else {
  // Frames that are the F# runtime or a collection combinator name no
  // caller anyone would go and change, so the chain steps over them.
  const LIBRARY =
    /^(SetTree|MapTree|FSharp|fold|map|filter|choose|collect|iterate|exists|forAll|ofList|ofSeq|ofArray|toList|toArray|add\d*|contains\d*|tryFind\d*|compare|equals|Equals|CompareTo|Compare|GetHashCode|loop\d*|\(anonymous\)|sortWith|sortBy|List_|Array_|Seq_|singleton|union|unionMany|delay|getEnumerator|GetEnumerator|MoveNext|System\.|Dictionary|memoised|has|get|set|partition|sumBy|minBy|maxBy|groupBy|distinct|count|length|item|append|concat|reverse|head|tail|isEmpty|min|max|remove|difference|intersect|toSet|ofSet|keys|values)/;
  const agg = new Map();
  for (const n of outermost) {
    const chain = [];
    for (let a = parent.get(n.id); a !== undefined && chain.length < depth; a = parent.get(a)) {
      const caller = byId.get(a);
      if (!LIBRARY.test(caller.callFrame.functionName || "(anonymous)")) chain.push(label(caller));
    }
    const key = chain.join(" < ") || "(root)";
    agg.set(key, (agg.get(key) ?? 0) + inclusive.get(n.id));
  }
  const total = [...agg.values()].reduce((a, b) => a + b, 0);
  console.log(`${name} inclusive ms ${(total / 1000).toFixed(1)} by caller`);
  top(agg, 25);
}
