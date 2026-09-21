// The wire gate for `src/App/ObserveMemory.fs` (#294).
//
// `tests/Core.Tests` references Core alone, so every Memory leaf the bot reads
// and writes had no automated gate at all. What that cost on #275: a `null`
// entry under `rivalHeld` threw past `leafOr` and emptied the whole
// `RaidState` — permanently, `saveRaids` writing the emptiness back — and
// off-shape entries decoded to tick 0, because `unbox<int>` compiles to `| 0`.
// Both are exactly what a wire codec exists to survive, and both shipped
// through four review lenses.
//
// The method is **load, then save, then compare the wire**. Every loader
// answers F# values — records, `Map`s, linked lists — that are awkward to
// inspect from JS and easy to inspect wrongly; writing them straight back out
// turns the answer into JSON, exercises the encoder in the same breath, and
// makes "what survived" a diff a human can read. A case says what it feeds and
// what should come back; anything else is a failure with both sides printed.
//
// Run: `npm run wire` (and `npm test`, which runs it after `dotnet test`).
import { strict as assert } from "node:assert";

const MODULE = "../build/fable/ObserveMemory.js";
// `savePositions` takes an F# list, which is a linked structure and not an
// array; this is the one place a case has to build one. The library folder
// carries its version in its name, so it is found rather than spelled — a
// pinned version here would make a Fable bump read as a wire failure.
const fableModules = new URL("../build/fable/fable_modules/", import.meta.url);
const libraryDir = (await import("node:fs/promises"))
  .readdir(fableModules)
  .then((names) => names.find((name) => name.startsWith("fable-library-js")));
const LIST = new URL(`${await libraryDir}/List.js`, fableModules).href;

const home = "W1N1";

// Memory as the shell finds it, with one observe leaf replaced.
const memoryWith = (leaf, value) => {
  const colonies = {};
  colonies[home] = {};
  colonies[home][leaf] = value;
  return { fabot: { observe: { colonies } } };
};

// The same for a leaf that is not per-colony.
const rootMemoryWith = (leaf, value) => ({ fabot: { observe: { [leaf]: value } } });

// JSON with its keys in a stable order, so a diff reads as a diff.
const stable = (value) =>
  JSON.stringify(value, (_, v) =>
    v && typeof v === "object" && !Array.isArray(v)
      ? Object.fromEntries(Object.entries(v).sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0)))
      : v,
  );

const cases = [];
const wire = (name, run) => cases.push({ name, run });

// ---- the Raid log -------------------------------------------------------

// Load a raids leaf and write it straight back: what comes out is what
/// survived the read, in the encoder's own words.
const raidsThrough = async (value) => {
  const { loadRaids, saveRaids } = await import(MODULE);
  globalThis.Memory = memoryWith("raids", value);
  saveRaids(home, loadRaids(home));
  return globalThis.Memory.fabot.observe.colonies[home].raids;
};

const emptyRaids = {
  episodes: [],
  outposts: [],
  rivalHeld: {},
  holds: {},
  threatened: {},
  living: [],
  placed: {},
  hits: {},
};

const episode = {
  opened: 10,
  last: 20,
  roster: [{ id: "h1", owner: "Invader", body: { move: 1 } }],
  closest: { range: 3, room: "W1N2", x: 5, y: 6, t: 15 },
  losses: [{ creep: "w1", t: 12, room: "W1N2", x: 8, y: 49 }],
  damage: 40,
};

const outpost = { room: "W1N2", opened: 10, last: 20, expiry: 500, basis: "collapse-timer" };

for (const [name, leaf] of [
  ["absent", undefined],
  ["null", null],
  // A missing intermediate comes back from the Memory API as this string, and
  // the shell reads the same leaf through `leafOr`.
  ["a string", "Incorrect memory path"],
  ["a number", 7],
]) {
  wire(`raids: a leaf that is ${name} reads empty and writes empty`, async () => {
    assert.equal(stable(await raidsThrough(leaf)), stable(emptyRaids));
  });
}

wire("raids: a well-formed leaf round-trips unchanged", async () => {
  const full = {
    ...emptyRaids,
    episodes: [episode],
    outposts: [outpost],
    rivalHeld: { W1N3: { since: 100, lastLooked: 150 } },
    holds: { W1N4: { holder: "invader", until: 900 } },
    threatened: { W1N5: { until: 800 } },
    living: ["w1", "w2"],
    placed: { w1: { room: "W1N2", x: 8, y: 49 } },
    hits: { "struct-1": 3000 },
  };

  assert.equal(stable(await raidsThrough(full)), stable(full));
});

wire("raids: #275's null latch costs its own entry and not the state", async () => {
  const written = await raidsThrough({
    ...emptyRaids,
    episodes: [episode],
    rivalHeld: { W1N3: null, W1N4: { since: 100, lastLooked: 150 } },
  });

  assert.equal(stable(written.rivalHeld), stable({ W1N4: { since: 100, lastLooked: 150 } }));
  assert.equal(written.episodes.length, 1, "the rest of the leaf survives the bad entry");
});

wire("raids: a stand-down with no expiry costs its row and no other", async () => {
  const written = await raidsThrough({
    ...emptyRaids,
    outposts: [{ room: "W1N9", opened: 1, last: 2, basis: "collapse-timer" }, outpost],
  });

  assert.equal(written.outposts.length, 1, "the row with no expiry is dropped");
  assert.equal(written.outposts[0].room, "W1N2");
});

wire("raids: a stand-down whose basis is not in the vocabulary costs its row", async () => {
  const written = await raidsThrough({
    ...emptyRaids,
    outposts: [{ ...outpost, room: "W1N9", basis: "not-a-basis" }, outpost],
  });

  assert.equal(written.outposts.length, 1);
  assert.equal(written.outposts[0].room, "W1N2");
});

wire("raids: a legacy episode with no damage and no loss tile reads as zero and none", async () => {
  const written = await raidsThrough({
    ...emptyRaids,
    episodes: [{ opened: 10, last: 20, roster: [], losses: [{ creep: "w1", t: 12 }] }],
  });

  assert.equal(written.episodes[0].damage, 0, "a bundle written before ADR 0034 charges nothing");
  assert.equal(written.episodes[0].losses[0].room, undefined, "and a loss written before #376 has no tile");
});

wire("raids: a legacy stand-down with no stronghold flag reads as no bunker", async () => {
  const written = await raidsThrough({ ...emptyRaids, outposts: [outpost] });

  assert.equal(written.outposts[0].stronghold, undefined, "absent means what it meant before #382");
});

// ---- the CPU line -------------------------------------------------------

wire("cpu: an absent leaf reads empty, and a well-formed one round-trips", async () => {
  const { loadCpu, saveCpu } = await import(MODULE);

  globalThis.Memory = rootMemoryWith("cpu", undefined);
  saveCpu(loadCpu());
  assert.equal(stable(globalThis.Memory.fabot.observe.cpu), stable({ ticks: [], spans: [] }));

  const sample = {
    ticks: [{ t: 10, ms: 1.5, entry: 0.1, snapshot: 0.2, decide: 0.3, save: 0.4, execute: 0.5, intents: 3, bucket: 10000, replans: 0 }],
    spans: [],
  };

  globalThis.Memory = rootMemoryWith("cpu", sample);
  saveCpu(loadCpu());
  assert.equal(stable(globalThis.Memory.fabot.observe.cpu), stable(sample));
});

for (const [name, leaf] of [
  ["null", null],
  ["a string", "Incorrect memory path"],
  ["a number", 7],
  ["an array where an object belongs", []],
  ["an object whose ticks are not an array", { ticks: 7 }],
]) {
  wire(`cpu: a leaf that is ${name} reads empty and writes empty`, async () => {
    const { loadCpu, saveCpu } = await import(MODULE);

    globalThis.Memory = rootMemoryWith("cpu", leaf);
    saveCpu(loadCpu());
    assert.equal(stable(globalThis.Memory.fabot.observe.cpu), stable({ ticks: [], spans: [] }));
  });
}

wire("cpu: the coarse spans round-trip, and a leaf without them reads empty", async () => {
  const { loadCpu, saveCpu } = await import(MODULE);

  const sample = { t: 10, ms: 1.5, entry: 0.1, snapshot: 0.2, decide: 0.3, save: 0.4, execute: 0.5, intents: 3, bucket: 10000, replans: 0 };

  // A leaf written before #386 has no `spans` key at all, and absent is the
  // empty record rather than a throw that would cost the ticks beside it.
  globalThis.Memory = rootMemoryWith("cpu", { ticks: [sample] });
  saveCpu(loadCpu());
  assert.deepEqual(globalThis.Memory.fabot.observe.cpu.spans, [], "a line written before the spans keeps its ticks");

  const span = { f: 100, t: 199, n: 100, max: 480.5, sum: 2000.25, b: 3400, r: 2, p: 91920, h: 0, w: 0, ss: 0, sd: 0, sv: 0, sx: 0 };

  globalThis.Memory = rootMemoryWith("cpu", { ticks: [sample], spans: [span] });
  saveCpu(loadCpu());
  assert.equal(stable(globalThis.Memory.fabot.observe.cpu.spans), stable([span]), "and one that has them round-trips key for key");

  // A span written before #389 has no `p`; it reads as zero pops and is
  // written back with the key, which is what "no tick of it was counted" says.
  const { p, ...older } = span;
  globalThis.Memory = rootMemoryWith("cpu", { ticks: [sample], spans: [older] });
  saveCpu(loadCpu());
  assert.equal(stable(globalThis.Memory.fabot.observe.cpu.spans), stable([{ ...older, p: 0 }]), "a span without pops reads as zero");

  // Present and not a number costs the span, as every other span field does.
  globalThis.Memory = rootMemoryWith("cpu", { ticks: [sample], spans: [{ ...older, p: "x" }] });
  saveCpu(loadCpu());
  assert.deepEqual(globalThis.Memory.fabot.observe.cpu.spans, [], "a span whose pops will not read is dropped");
});

wire("cpu: the append keeps the coarse spans current, one row a tick (#390)", async () => {
  const { loadCpu, appendCpu } = await import(MODULE);

  const row = (t) => ({ t, ms: 1.5, entry: 0.1, snapshot: 0.2, decide: 0.3, save: 0.4, execute: 0.5, intents: 3, bucket: 10000, replans: 0 });
  const span = (f, t, n) => ({ f, t, n, max: 1.5, sum: 1.5 * n, b: 10000, r: 0, p: 0, h: 0, w: 0, ss: 0.2 * n, sd: 0.3 * n, sv: 0.4 * n, sx: 0.5 * n });

  // Each state is read off a leaf through `loadCpu`, so it has Core's own
  // types (an F# list is not a JS array); the leaf is then reset to what the
  // previous tick left and the state appended onto it. The fold is Core's
  // and tested there; this is the codec's append.
  const stateOf = (ticks, spans) => {
    globalThis.Memory = rootMemoryWith("cpu", { ticks, spans });
    return loadCpu();
  };

  // A third tick folded into the open span, against a leaf holding two rows and that span.
  const withThird = stateOf([row(1), row(2), row(3)], [span(1, 3, 3)]);
  globalThis.Memory = rootMemoryWith("cpu", { ticks: [row(1), row(2)], spans: [span(1, 2, 2)] });
  appendCpu(withThird);
  assert.equal(stable(globalThis.Memory.fabot.observe.cpu.spans), stable([span(1, 3, 3)]), "the open span is rewritten in place");
  assert.equal(globalThis.Memory.fabot.observe.cpu.ticks.length, 3, "and the row was appended as before");

  // A fourth tick that opened a second span: the leaf holds one span, the state two.
  const withSecond = stateOf([row(1), row(2), row(3), row(4)], [span(1, 3, 3), span(4, 4, 1)]);
  globalThis.Memory = rootMemoryWith("cpu", { ticks: [row(1), row(2), row(3)], spans: [span(1, 3, 3)] });
  appendCpu(withSecond);
  assert.equal(stable(globalThis.Memory.fabot.observe.cpu.spans), stable([span(1, 3, 3), span(4, 4, 1)]), "a span that opened this tick is pushed");

  // A leaf whose spans disagree with the state is written whole.
  globalThis.Memory = rootMemoryWith("cpu", { ticks: [row(1), row(2), row(3)], spans: [] });
  appendCpu(withSecond);
  assert.equal(stable(globalThis.Memory.fabot.observe.cpu.spans), stable([span(1, 3, 3), span(4, 4, 1)]), "a disagreeing leaf is rewritten whole");
});


wire("cpu: heap, memo rows and the span's phase sums round-trip, and absent reads zero (#391)", async () => {
  const { loadCpu, saveCpu } = await import(MODULE);

  const base = { t: 10, ms: 1.5, entry: 0.1, snapshot: 0.2, decide: 0.3, save: 0.4, execute: 0.5, intents: 3, bucket: 10000, replans: 0 };
  const span = { f: 100, t: 199, n: 100, max: 480.5, sum: 2000.25, b: 3400, r: 2, p: 91920, h: 55.3, w: 1400, ss: 900.5, sd: 2100.25, sv: 300, sx: 700 };

  globalThis.Memory = rootMemoryWith("cpu", { ticks: [{ ...base, heap: 41.3, rows: 900 }], spans: [span] });
  saveCpu(loadCpu());
  assert.equal(stable(globalThis.Memory.fabot.observe.cpu.ticks[0]), stable({ ...base, heap: 41.3, rows: 900 }), "the row's heap and rows round-trip");
  assert.equal(stable(globalThis.Memory.fabot.observe.cpu.spans[0]), stable(span), "and the span's six new keys");

  // A legacy row and span carry none of the keys; they read as zero and a
  // zero is not written back, so the row re-encodes as itself.
  const older = { f: 100, t: 199, n: 100, max: 480.5, sum: 2000.25, b: 3400, r: 2 };
  globalThis.Memory = rootMemoryWith("cpu", { ticks: [base], spans: [older] });
  saveCpu(loadCpu());
  assert.equal(stable(globalThis.Memory.fabot.observe.cpu.ticks[0]), stable(base), "no heap, no key");
  assert.equal(stable(globalThis.Memory.fabot.observe.cpu.spans[0]), stable({ ...older, p: 0, h: 0, w: 0, ss: 0, sd: 0, sv: 0, sx: 0 }), "a legacy span reads its new keys as zero");

  // Present and not a number costs the span, as every other span field does.
  globalThis.Memory = rootMemoryWith("cpu", { ticks: [base], spans: [{ ...older, h: "x" }] });
  saveCpu(loadCpu());
  assert.deepEqual(globalThis.Memory.fabot.observe.cpu.spans, [], "a span whose heap will not read is dropped");

  // And the row alike: a non-number heap costs that row and no other.
  globalThis.Memory = rootMemoryWith("cpu", { ticks: [{ ...base, heap: "x" }, { ...base, t: 11, heap: 41.3, rows: 0 }], spans: [] });
  saveCpu(loadCpu());
  assert.equal(stable(globalThis.Memory.fabot.observe.cpu.ticks), stable([{ ...base, t: 11, heap: 41.3, rows: 0 }]), "the bad row is dropped, and a measured tick with no memo rows keeps its zero");
});

wire("cpu: the flood counts ride the row per colony, and a malformed triple is left out", async () => {
  const { loadCpu, saveCpu } = await import(MODULE);

  const base = { t: 10, ms: 1.5, entry: 0.1, snapshot: 0.2, decide: 0.3, save: 0.4, execute: 0.5, intents: 3, bucket: 10000, replans: 0 };
  const counted = { ...base, floods: { W12S28: [12, 9, 9554], W13S28: [3, 3, 812] } };

  globalThis.Memory = rootMemoryWith("cpu", { ticks: [counted], spans: [] });
  saveCpu(loadCpu());
  assert.equal(stable(globalThis.Memory.fabot.observe.cpu.ticks[0]), stable(counted), "the triples round-trip per colony");

  // A row an older bundle wrote carries no `floods` at all, and stays that way.
  globalThis.Memory = rootMemoryWith("cpu", { ticks: [base], spans: [] });
  saveCpu(loadCpu());
  assert.equal(Object.hasOwn(globalThis.Memory.fabot.observe.cpu.ticks[0], "floods"), false, "no counts, no key");

  // A colony whose triple is off the wire shape is dropped, not read as zeros.
  globalThis.Memory = rootMemoryWith("cpu", {
    ticks: [{ ...base, floods: { W12S28: [12, 9, 9554], W13S28: [3, "x", 812], W15S28: [1, 1] } }],
    spans: [],
  });
  saveCpu(loadCpu());
  assert.equal(stable(globalThis.Memory.fabot.observe.cpu.ticks[0].floods), stable({ W12S28: [12, 9, 9554] }), "only the well-formed colony survives");
});

wire("cpu: a span with a field off the shape costs its span and not the line", async () => {
  const { loadCpu, saveCpu } = await import(MODULE);

  const good = { f: 100, t: 199, n: 100, max: 480.5, sum: 2000.25, b: 3400, r: 2 };

  for (const key of ["f", "t", "n", "max", "sum", "b", "r"]) {
    globalThis.Memory = rootMemoryWith("cpu", { ticks: [], spans: [{ ...good, [key]: {} }, good] });
    saveCpu(loadCpu());
    const spans = globalThis.Memory.fabot.observe.cpu.spans;

    assert.equal(spans.length, 1, `a span whose ${key} is not a number is dropped`);
    assert.equal(spans[0].f, 100, "and the well-formed one beside it is kept");
  }
});

wire("cpu: a row that will not decode costs its row and not the line", async () => {
  const { loadCpu, saveCpu } = await import(MODULE);

  const good = { t: 10, ms: 1.5, entry: 0.1, snapshot: 0.2, decide: 0.3, save: 0.4, execute: 0.5, intents: 3, bucket: 10000, replans: 0 };

  globalThis.Memory = rootMemoryWith("cpu", { ticks: [null, good] });
  saveCpu(loadCpu());
  const ticks = globalThis.Memory.fabot.observe.cpu.ticks;

  assert.equal(ticks.length, 1, "the null row is dropped");
  assert.equal(ticks[0].t, 10, "and no tick is invented for it");
});

// ---- the breach log -----------------------------------------------------

wire("breaches: an absent leaf reads empty and a well-formed one round-trips", async () => {
  const { loadBreaches, saveBreaches } = await import(MODULE);

  globalThis.Memory = memoryWith("breaches", undefined);
  saveBreaches(home, loadBreaches(home));
  assert.equal(stable(globalThis.Memory.fabot.observe.colonies[home].breaches), stable({ rows: [] }));

  const rows = {
    rows: [{ kind: "ore-on-the-floor", room: "W1N2", subject: "pile-1", amount: 300, first: 100, last: 120 }],
  };

  globalThis.Memory = memoryWith("breaches", rows);
  saveBreaches(home, loadBreaches(home));
  assert.equal(stable(globalThis.Memory.fabot.observe.colonies[home].breaches), stable(rows));
});

for (const [name, leaf] of [
  ["null", null],
  ["a string", "Incorrect memory path"],
  ["a number", 7],
  ["an object whose rows are not an array", { rows: 7 }],
]) {
  wire(`breaches: a leaf that is ${name} reads empty and writes empty`, async () => {
    const { loadBreaches, saveBreaches } = await import(MODULE);

    globalThis.Memory = memoryWith("breaches", leaf);
    saveBreaches(home, loadBreaches(home));
    assert.equal(stable(globalThis.Memory.fabot.observe.colonies[home].breaches), stable({ rows: [] }));
  });
}

wire("breaches: a row whose kind is not in the vocabulary costs its row", async () => {
  const { loadBreaches, saveBreaches } = await import(MODULE);

  globalThis.Memory = memoryWith("breaches", {
    rows: [
      { kind: "not-a-kind", room: "W1N2", subject: "x", amount: 1, first: 1, last: 2 },
      { kind: "ore-on-the-floor", room: "W1N2", subject: "pile-1", amount: 300, first: 100, last: 120 },
    ],
  });

  saveBreaches(home, loadBreaches(home));
  const rows = globalThis.Memory.fabot.observe.colonies[home].breaches.rows;

  assert.equal(rows.length, 1);
  assert.equal(rows[0].subject, "pile-1");
});

// ---- the reactor reading and the positions leaf -------------------------

// All seven fields are one sample, so this leaf degrades whole rather than
// field by field: a half-read reading would combine a date from one wire shape
// with a store from another, and the programme is scored off the pair.

const reactorThrough = async (value) => {
  const { loadReactor, saveReactor } = await import(MODULE);
  globalThis.Memory = rootMemoryWith("reactor", value);
  saveReactor(loadReactor());
  return globalThis.Memory.fabot.observe.reactor;
};

const emptyReactor = {
  owner: "none",
  storeT: 0,
  continuousWork: 0,
  seen: null,
  bankedT: 0,
  lastDelivery: null,
  dryTicks: 0,
};

for (const [name, leaf] of [
  ["absent", undefined],
  ["null", null],
  ["a string", "Incorrect memory path"],
  ["a number", 7],
  ["missing a field", { owner: "ours", storeT: 10, continuousWork: 1, bankedT: 5 }],
  ["carrying a store that is not a number", { ...emptyReactor, storeT: {} }],
  ["naming an owner nobody knows", { ...emptyReactor, owner: "somebody" }],
]) {
  wire(`reactor: a leaf that is ${name} reads empty and writes empty`, async () => {
    assert.equal(stable(await reactorThrough(leaf)), stable(emptyReactor));
  });
}

wire("reactor: a well-formed reading round-trips key for key", async () => {
  const reading = {
    owner: "ours",
    storeT: 4000,
    continuousWork: 12,
    seen: 560000,
    bankedT: 45000,
    lastDelivery: 560528,
    dryTicks: 375,
  };

  assert.equal(stable(await reactorThrough(reading)), stable(reading));
});

wire("positions: a row that will not read costs its row and no other", async () => {
  const { loadPositions } = await import(MODULE);

  const names = (leaf) => {
    globalThis.Memory = rootMemoryWith("positions", leaf);
    return [...loadPositions().keys()].sort();
  };

  assert.deepEqual(names(undefined), [], "an absent leaf reads empty");
  assert.deepEqual(names("Incorrect memory path"), [], "and so does a leaf of the wrong type");

  const good = { r: "W1N1", x: 5, y: 5 };

  for (const [what, bad] of [
    ["a null row", null],
    ["a row with no room", { x: 5, y: 5 }],
    // A tile read as (0,0) is a corner of the room, which is a wall: the
    // creep would read as having moved a long way, and `CreepInfo.Moved` is
    // what the stall channel is priced off.
    ["a row whose coordinate is not a number", { r: "W1N1", x: {}, y: 5 }],
  ]) {
    assert.deepEqual(names({ w1: bad, w2: good }), ["w2"], `${what} is dropped and the row beside it kept`);
  }
});

// ---- nothing invents a tick ---------------------------------------------

// The second defect class of #275, and the reason this file exists: `unbox<int>`
// is erased by Fable, so an off-shape value does not throw — it compiles to
// `| 0` and becomes the number zero. A tick of zero is not a wrong number, it
// is the *oldest possible* number: `standingDown` reads `tick < Expiry`, so a
// stand-down whose expiry decodes to 0 reads as spent, which is the one
// direction ADR 0043 says the gate may not be wrong in. Every case below feeds
// one field a value of the wrong type and asserts the row is dropped rather
// than kept with a zero in it.
//
// `{}` is the probe value throughout: an object is what a half-written leaf and
// a hand edit through the Memory HTTP API both leave behind, and it is the
// value `unbox` turns into 0 most quietly.

const offShape = [
  ["a stand-down's expiry", (l) => ({ ...l, outposts: [{ ...outpost, expiry: {} }] }), (w) => w.outposts],
  ["an episode's opening tick", (l) => ({ ...l, episodes: [{ ...episode, opened: {} }] }), (w) => w.episodes],
  ["an episode's last-seen tick", (l) => ({ ...l, episodes: [{ ...episode, last: [] }] }), (w) => w.episodes],
  [
    "a closest approach's tick",
    (l) => ({ ...l, episodes: [{ ...episode, closest: { ...episode.closest, t: {} } }] }),
    (w) => w.episodes,
  ],
  // The grain here is the episode and not the loss row: a loss is the
  // episode's own accounting, and an episode that has lost a body it cannot
  // name a tick for is a bundle that will not restate itself. Costing the row
  // instead would leave a window that reads as cheaper than it was.
  [
    "a loss's tick",
    (l) => ({ ...l, episodes: [{ ...episode, losses: [{ creep: "w1", t: {} }] }] }),
    (w) => w.episodes,
  ],
  ["a placed body's coordinate", (l) => ({ ...l, placed: { w1: { room: "W1N2", x: {}, y: 5 } } }), (w) => Object.keys(w.placed)],
  ["a damage baseline", (l) => ({ ...l, hits: { "struct-1": {} } }), (w) => Object.keys(w.hits)],
];

for (const [what, feed, read] of offShape) {
  wire(`ticks: ${what} off the shape costs its row, and invents no zero`, async () => {
    const written = await raidsThrough(feed(emptyRaids));

    assert.deepEqual(read(written), [], `a zero here would read as the oldest tick there is, not as a gap`);
  });
}

wire("ticks: a bad row costs itself and the rows beside it survive", async () => {
  const written = await raidsThrough({
    ...emptyRaids,
    episodes: [{ ...episode, opened: {} }, episode],
    outposts: [{ ...outpost, room: "W1N9", expiry: {} }, outpost],
  });

  assert.equal(written.episodes.length, 1, "one episode is dropped, one kept");
  assert.equal(written.outposts.length, 1);
  assert.equal(written.outposts[0].room, "W1N2", "and the kept stand-down is the well-formed one");
});

wire("ticks: a number under the living roster is not a creep name", async () => {
  const written = await raidsThrough({ ...emptyRaids, living: [1, "w2"] });

  assert.deepEqual(written.living, ["w2"], "a name is a string; a number here would be a body that dies next tick");
});

wire("ticks: a living roster that is not a list reads as nobody, not as a throw", async () => {
  for (const notAList of [undefined, null, 7, "w1", { w1: true }]) {
    const written = await raidsThrough({ ...emptyRaids, episodes: [episode], living: notAList });

    assert.deepEqual(written.living, [], "an unreadable roster is an empty one");
    assert.equal(written.episodes.length, 1, "and it does not take the episode ring with it");
  }
});

// ---- the creep log ------------------------------------------------------

// The largest leaf in Memory and the file's largest decoder, read by `load`
// and written by `save` and `saveChanged` — the incremental writer #370 added,
// whose handshake with the leaf is what a wire gate is for.

const creepLog = {
  log: [{ t: 100, v: { kind: "kept", task: "haul-1" } }],
  lastTask: { kind: "matched", task: "haul-1", factor: "rank" },
  lastMove: [{ kind: "rerouted" }],
};

const creepsThrough = async (value, write) => {
  const { load, save, saveChanged } = await import(MODULE);
  globalThis.Memory = rootMemoryWith("creeps", value);
  const state = load();
  write === "changed" ? saveChanged(state, state) : save(state);
  return globalThis.Memory.fabot.observe.creeps;
};

wire("creeps: an absent leaf reads empty, and a well-formed log round-trips", async () => {
  assert.equal(stable(await creepsThrough(undefined)), stable({}));
  assert.equal(stable(await creepsThrough({ w1: creepLog })), stable({ w1: creepLog }));
});

wire("creeps: a creep whose log will not decode costs that creep alone", async () => {
  const written = await creepsThrough({ w1: null, w2: creepLog });

  assert.deepEqual(Object.keys(written), ["w2"], "one unreadable creep is a gap, not amnesia");
});

wire("creeps: an entry with no tick costs its entry and not the log", async () => {
  const written = await creepsThrough({
    w1: { ...creepLog, log: [{ t: {}, v: { kind: "kept", task: "haul-1" } }, { t: 100, v: { kind: "rerouted" } }] },
  });

  assert.equal(written.w1.log.length, 1, "the entry with no readable tick is dropped");
  assert.equal(written.w1.log[0].t, 100, "and tick zero is not written in its place");
});

wire("creeps: a verdict kind nobody knows costs its row", async () => {
  const written = await creepsThrough({
    w1: { log: [{ t: 100, v: { kind: "not-a-kind" } }], lastMove: [] },
  });

  assert.equal(written.w1.log.length, 0);
});

wire("creeps: saveChanged writes the same wire as save when nothing moved", async () => {
  assert.equal(
    stable(await creepsThrough({ w1: creepLog }, "changed")),
    stable(await creepsThrough({ w1: creepLog })),
    "the incremental writer (#370) and the whole writer agree key for key",
  );
});

wire("creeps: saveChanged falls back to a whole write when the leaf disagrees", async () => {
  const { load, saveChanged } = await import(MODULE);

  globalThis.Memory = rootMemoryWith("creeps", { w1: creepLog });
  const state = load();

  // The leaf now holds a creep count the prior does not, which is the
  // handshake failing: the writer owes a whole write.
  globalThis.Memory.fabot.observe.creeps = { w1: creepLog, w9: creepLog };
  saveChanged(state, state);

  assert.deepEqual(Object.keys(globalThis.Memory.fabot.observe.creeps), ["w1"], "the stale creep is gone");
});

// ---- the verbose list and the positions write ---------------------------

wire("verbose: a string list reads, and anything else reads as off", async () => {
  const { loadVerbose } = await import(MODULE);

  for (const [what, value, size] of [
    ["absent", undefined, 0],
    ["a string list", ["w1", "w2"], 2],
    ["a list with a number in it", ["w1", 2], 0],
    ["an object", { w1: true }, 0],
    ["null", null, 0],
    ["a string", "Incorrect memory path", 0],
    ["a number", 7, 0],
  ]) {
    globalThis.Memory = rootMemoryWith("verbose", value);
    assert.equal(loadVerbose().size, size, `${what} reads as ${size} names`);
  }
});

wire("positions: what savePositions writes is what loadPositions reads", async () => {
  const { loadPositions, savePositions } = await import(MODULE);
  const { ofArray } = await import(LIST);

  globalThis.Memory = rootMemoryWith("positions", undefined);
  savePositions(ofArray([["w1", { Room: "W1N1", X: 5, Y: 6 }]]));

  const written = globalThis.Memory.fabot.observe.positions;
  assert.equal(stable(written), stable({ w1: { r: "W1N1", x: 5, y: 6 } }));

  globalThis.Memory = rootMemoryWith("positions", written);
  assert.equal(loadPositions().size, 1, "and it reads back as the tile it was");
});

// ---- the scoring row ----------------------------------------------------

// `decodeCandidate` and `readNumbers` are reached only through a `scoring`
// verdict, which the verbose channel writes: a row per task the matcher
// weighed, each carrying a rank, a cost and a load, or a rejection reason with
// its own numbers. Nothing in Core sees this leaf, and the operator reads it
// to answer "why did this body take that task" — a rank that decodes to 0 is
// an answer, and the wrong one.

const scoringThrough = async (candidates) => {
  const { load, save } = await import(MODULE);
  globalThis.Memory = rootMemoryWith("creeps", {
    w1: { log: [{ t: 100, v: { kind: "scoring", candidates } }], lastMove: [] },
  });
  save(load());
  const rows = globalThis.Memory.fabot.observe.creeps.w1.log;
  return rows.length === 0 ? null : rows[0].v.candidates;
};

wire("scoring: a scored row and a rejected row both round-trip", async () => {
  const candidates = [
    { task: "haul-1", rank: 3, cost: 12, load: 400 },
    { task: "haul-2", reason: "capacity-full" },
    { task: "haul-3", reason: "too-early", walk: 10, wait: 4 },
  ];

  assert.equal(stable(await scoringThrough(candidates)), stable(candidates));
});

for (const [what, candidate] of [
  ["a rank", { task: "haul-1", rank: {}, cost: 12, load: 400 }],
  ["a cost", { task: "haul-1", rank: 3, cost: {}, load: 400 }],
  ["a load", { task: "haul-1", rank: 3, cost: 12, load: {} }],
  ["a walk", { task: "haul-1", reason: "too-early", walk: {}, wait: 4 }],
]) {
  wire(`scoring: ${what} that is not a number costs the row it sits on`, async () => {
    assert.equal(await scoringThrough([candidate]), null, "the whole scoring verdict goes, and no zero is written");
  });
}

wire("scoring: a reason nobody knows costs its row", async () => {
  assert.equal(await scoringThrough([{ task: "haul-1", reason: "not-a-reason" }]), null);
});

// ---- the two write-only leaves ------------------------------------------

// `saveQuotas` and `saveLayout` have no loader: `scripts/observe.mjs` reads
// them and nothing in F# does, so a renamed key here breaks the operator's
// view silently and no other check sees it. The method above does not apply
// with nothing to load, so these cases assert the documented key set instead.

wire("quotas: silence writes the shape observe.mjs destructures", async () => {
  const { saveQuotas } = await import(MODULE);
  const { QuotasModule_silent } = await import("../build/fable/Core/Types/Verdicts.js");

  globalThis.Memory = memoryWith("quotas", undefined);
  saveQuotas(home, QuotasModule_silent);

  assert.equal(
    stable(globalThis.Memory.fabot.observe.colonies[home].quotas),
    stable({ target: 0, living: 0, casting: 0, rows: [], load: 0, haul: [] }),
  );
});

wire("layout: four empty channels write four empty arrays", async () => {
  const { saveLayout } = await import(MODULE);
  const { empty } = await import(LIST);

  globalThis.Memory = memoryWith("layout", undefined);
  saveLayout(home, empty(), empty(), empty(), empty());

  assert.deepEqual(
    Object.keys(globalThis.Memory.fabot.observe.colonies[home].layout).sort(),
    ["deferred", "refused", "unrouted", "unserved"],
    "the four channels ADR 0028 keeps side by side",
  );
});

// ---- run ----------------------------------------------------------------

let failed = 0;

for (const { name, run } of cases) {
  try {
    // Each case supplies its own Memory; clearing it between them means a
    // case that forgets to reads `undefined` and fails, rather than passing
    // on the leaf the case before it left behind.
    globalThis.Memory = undefined;
    await run();
    console.log(`  ok   ${name}`);
  } catch (error) {
    failed += 1;
    console.log(`  FAIL ${name}`);
    console.log(`       ${error.message.split("\n").join("\n       ")}`);
  }
}

console.log(`\n${cases.length - failed} passed, ${failed} failed`);
process.exit(failed === 0 ? 0 : 1);
