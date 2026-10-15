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

// The Transition log and the CPU line left Memory for the heap and memory
// segments (#639); their codec is pure over the wire (`decodeCpu`,
// `encodeCpu`, `appendCpuOnto`, `decodeLog`, `encodeLog`, `saveChangedOnto`).
// The cases below were written against their old Memory leaves and still hold
// the wire there, threaded through the codec by this shim.
const logCodec = async () => {
  const m = await import(MODULE);
  const leaf = (name) => globalThis.Memory?.fabot?.observe?.[name];
  const put = (name, wire) => {
    globalThis.Memory.fabot ??= {};
    globalThis.Memory.fabot.observe ??= {};
    globalThis.Memory.fabot.observe[name] = wire;
  };
  return {
    loadCpu: () => m.decodeCpu(leaf("cpu")),
    saveCpu: (state) => put("cpu", m.encodeCpu(state)),
    appendCpu: (state) => put("cpu", m.appendCpuOnto(leaf("cpu"), state)),
    load: () => m.decodeLog(leaf("creeps")),
    save: (state) => put("creeps", m.encodeLog(state)),
    saveChanged: (prior, state) => put("creeps", m.saveChangedOnto(leaf("creeps"), prior, state)),
  };
};

// ---- the Raid log -------------------------------------------------------

// Load a raids leaf and write it straight back: what comes out is what
/// survived the read, in the encoder's own words.
const raidsThrough = async (value, declared = []) => {
  const { loadRaids, saveRaids } = await import(MODULE);
  const { ofArray } = await import(LIST);
  globalThis.Memory = memoryWith("raids", value);
  saveRaids(home, ofArray(declared), loadRaids(home));
  return globalThis.Memory.fabot.observe.colonies[home].raids;
};

const emptyRaids = {
  episodes: [],
  reserveShort: false,
  haulFill: 0,
  haulOwed: false,
  living: [],
  placed: {},
  hits: {},
  declared: [],
};

const episode = {
  opened: 10,
  last: 20,
  rooms: ["W1N1", "W1N2"],
  closest: { range: 3, room: "W1N2", x: 5, y: 6, t: 15 },
  losses: [{ creep: "w1", t: 12, room: "W1N2", x: 8, y: 49 }],
  damage: 40,
};

const outpost = { room: "W1N2", opened: 10, last: 20, expiry: 500, basis: "collapse-timer" };

// Two probe logs (#493): one out of its room alive once safe mode showed, one
// still dismantling, its last decided dismantle on `hitAt`.
const probeOut = { probe: "probe-1", first: 900, hits: 1, expires: 2300, fate: "out", ended: 902, safeMode: 901 };
const probeProbing = { probe: "probe-2", first: 950, hits: 4, expires: 2400, fate: "probing", hitAt: 953 };

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
    reserveShort: true,
    digOpen: ["O", "U"],
    haulFill: 0.8125,
    haulOwed: true,
    living: ["w1", "w2"],
    placed: { w1: { room: "W1N2", x: 8, y: 49 } },
    hits: { "struct-1": 3000 },
  };

  assert.equal(stable(await raidsThrough(full)), stable(full));
});

wire("raids: a loss's cast (#612) round-trips, and a loss with none writes none", async () => {
  const cast = { ...episode, losses: [{ ...episode.losses[0], cast: 2 }, { creep: "w3", t: 13 }] };
  const written = await raidsThrough({ ...emptyRaids, episodes: [cast] });
  assert.equal(stable(written.episodes), stable([cast]));
});

wire("ticks: a loss's cast off the shape costs its episode, and invents no zero", async () => {
  const written = await raidsThrough({
    ...emptyRaids,
    episodes: [{ ...episode, losses: [{ ...episode.losses[0], cast: {} }] }],
  });
  assert.deepEqual(written.episodes, []);
});

wire("raids: the declared outposts (#162) are written beside the ring, the leaf's own never read back", async () => {
  const written = await raidsThrough({ ...emptyRaids, declared: ["W9N9"] }, ["W1N2", "W2N1"]);
  assert.deepEqual(written.declared, ["W1N2", "W2N1"]);
});

wire("raids: the danger keys a bundle before the world log wrote are not read and not written back", async () => {
  const written = await raidsThrough({
    ...emptyRaids,
    episodes: [episode],
    outposts: [outpost],
    rivalHeld: { W1N3: { since: 100, lastLooked: 150 } },
    holds: { W1N4: { holder: "invader", until: 900 } },
    threatened: { W1N5: { until: 800 } },
  });

  assert.equal(stable(written), stable({ ...emptyRaids, episodes: [episode] }));
});

wire("raids: the fight records, probes and taken rooms a bundle before the world roster wrote are not read and not written back", async () => {
  const written = await raidsThrough({
    ...emptyRaids,
    episodes: [episode],
    fought: { W1N6: { seen: 700, squad: "duo" } },
    probes: { W18S26: probeOut },
    taken: { W18S26: 930_120 },
  });

  assert.equal(stable(written), stable({ ...emptyRaids, episodes: [episode] }));
});

wire("raids: a legacy episode's roster and killed list are read as none, and its rooms as empty", async () => {
  const { rooms, ...legacy } = episode;
  const written = await raidsThrough({
    ...emptyRaids,
    episodes: [{ ...legacy, roster: [{ id: "h1", owner: "Invader", body: { move: 1 } }], killed: ["h1"] }],
  });

  assert.equal(stable(written.episodes), stable([{ ...legacy, rooms: [] }]));
});

wire("raids: an episode's rooms that are no list, or no names, read as none", async () => {
  const written = await raidsThrough({
    ...emptyRaids,
    episodes: [
      { ...episode, rooms: "W1N2" },
      { ...episode, rooms: [3, "", "W1N3"] },
    ],
  });

  assert.equal(stable(written.episodes.map((e) => e.rooms)), stable([[], ["W1N3"]]));
});

wire("raids: the reserve latch (#505) is true only as a boolean, and a legacy leaf reads unlatched", async () => {
  for (const value of [1, "true", {}, null]) {
    const written = await raidsThrough({ ...emptyRaids, reserveShort: value });
    assert.equal(written.reserveShort, false, `reserveShort ${JSON.stringify(value)}`);
  }

  const { reserveShort, ...legacy } = emptyRaids;
  assert.equal((await raidsThrough(legacy)).reserveShort, false);
});

wire("raids: the ore dig's latch (ADR 0089) keeps the engine's ore letters alone, a shut one round-trips as [], and a missing key or a non-list stays unwritten (read open)", async () => {
  const written = await raidsThrough({ ...emptyRaids, digOpen: ["U", "T", "energy", 3, null, "O", "U"] });
  assert.deepEqual(written.digOpen, ["O", "U"]);

  for (const value of [[], ["T"]]) {
    assert.deepEqual((await raidsThrough({ ...emptyRaids, digOpen: value })).digOpen, [], `digOpen ${JSON.stringify(value)}`);
  }

  for (const value of ["U", { U: true }, null, 1]) {
    const unwritten = await raidsThrough({ ...emptyRaids, digOpen: value });
    assert.equal("digOpen" in unwritten, false, `digOpen ${JSON.stringify(value)}`);
  }

  assert.equal("digOpen" in (await raidsThrough(emptyRaids)), false);
});

wire("raids: the home's last raided tick (#526) round-trips as a number, and anything else or a legacy leaf reads never", async () => {
  assert.equal((await raidsThrough({ ...emptyRaids, homeRaided: 968_300 })).homeRaided, 968_300);

  for (const value of ["968300", {}, null, true]) {
    const written = await raidsThrough({ ...emptyRaids, homeRaided: value });
    assert.equal("homeRaided" in written, false, `homeRaided ${JSON.stringify(value)}`);
  }

  assert.equal("homeRaided" in (await raidsThrough(emptyRaids)), false);
});

wire("raids: the haul overflow (#508) owes only as a boolean, its fill reads 0 off a number and clamps into 0..1, and a legacy leaf reads empty", async () => {
  for (const value of [1, "true", {}, null]) {
    const written = await raidsThrough({ ...emptyRaids, haulOwed: value });
    assert.equal(written.haulOwed, false, `haulOwed ${JSON.stringify(value)}`);
  }

  for (const [value, read] of [
    ["0.9", 0],
    [null, 0],
    [{}, 0],
    [true, 0],
    [-0.5, 0],
    [3, 1],
  ]) {
    const written = await raidsThrough({ ...emptyRaids, haulFill: value });
    assert.equal(written.haulFill, read, `haulFill ${JSON.stringify(value)}`);
  }

  const { haulFill, haulOwed, ...legacy } = emptyRaids;
  const written = await raidsThrough(legacy);
  assert.equal(written.haulFill, 0);
  assert.equal(written.haulOwed, false);
});

// ---- the world danger log ----------------------------------------------

// The world danger log (ADR 0086 S2): one record a room under
// `observe.world.danger`, each part — the stand-down row, the hold, the threat
// memory, the exit watch — read alone. Loaded and written back as the raids
// leaf is.
const dangerThrough = async (leaf) => {
  const { loadDanger, saveDanger } = await import(MODULE);
  const observe = {};
  if (leaf !== undefined) observe.world = { danger: leaf };
  globalThis.Memory = { fabot: { observe } };
  saveDanger(loadDanger());
  return globalThis.Memory.fabot.observe.world.danger;
};

const hold = { holder: "invader", until: 900 };
const threat = { until: 800 };
const heldWatch = {
  run: [[0, 20], [0, 21]],
  exit: [0, 20],
  left: 850,
  leavers: [
    { id: "h-1", x: 1, y: 20, dies: 1400, trip: 100 },
    { id: "h-2", x: 1, y: 22, dies: 900, trip: 150 },
  ],
  until: 950,
};
const seenWatch = { seen: [{ id: "h-1", x: 10, y: 11, dies: 1400, trip: 100 }] };

for (const [name, leaf] of [
  ["absent", undefined],
  ["null", null],
  ["a string", "Incorrect memory path"],
  ["a number", 7],
  ["an array", []],
]) {
  wire(`danger: a leaf that is ${name} reads empty and writes empty`, async () => {
    assert.equal(stable(await dangerThrough(leaf)), stable({}));
  });
}

wire("danger: a well-formed leaf round-trips unchanged", async () => {
  const full = {
    W1N2: { down: outpost, hold, threat, watch: heldWatch },
    W1N3: { watch: seenWatch },
    W1N4: { threat },
    W1N6: { fight: { seen: 700, raiders: ["inv-1", "inv-2"] } },
    W1N7: { fight: { seen: 690 } },
    W18S26: { taken: 930_120, probe: probeOut },
    W17S24: { probe: probeProbing },
  };

  assert.equal(stable(await dangerThrough(full)), stable(full));
});

// #663 S5: an exit watch a bundle before the evidence-ended hold wrote
// carries no trip, no left tick and no leavers: a seen threat reads a
// room's round trip, a hold left at its last tick with no body a look
// decides it on. Leavers that are no list, or a leaver off its shape, cost
// the watch.
wire("danger: a legacy exit watch reads a room's trip and a hold left at its end", async () => {
  const written = await dangerThrough({
    W1N2: { watch: { run: [[0, 20]], exit: [0, 20], until: 950 } },
    W1N3: { watch: { seen: [{ id: "h-1", x: 10, y: 11, dies: 1400 }] } },
    W1N4: { watch: { run: [[0, 20]], exit: [0, 20], leavers: "h-1", until: 950 } },
    W1N5: { watch: { run: [[0, 20]], exit: [0, 20], leavers: [{ id: 7, x: 1, y: 20, dies: 9 }], until: 950 } },
  });

  assert.equal(
    stable(written),
    stable({
      W1N2: { watch: { run: [[0, 20]], exit: [0, 20], left: 950, until: 950 } },
      W1N3: { watch: { seen: [{ id: "h-1", x: 10, y: 11, dies: 1400, trip: 100 }] } },
    }),
  );
});

// #604: a declared enemy's hold is its own holder on the wire.
wire("danger: every holder's hold round-trips, an enemy's included", async () => {
  const full = {
    W1N2: { hold: { holder: "invader", until: 900 } },
    W1N3: { hold: { holder: "rival", until: 900 } },
    W1N4: { hold: { holder: "enemy", until: 900 } },
    // A row whose holder a look has read, beside W1N2's legacy row with none.
    W1N5: { down: { ...outpost, room: "W1N5", basis: "rival-reservation", read: true } },
  };

  assert.equal(stable(await dangerThrough(full)), stable(full));
});

// The parts the world layer's S5 moved out of each colony's log: a fight
// record, a taken tick and a probe log, each read alone.
// #564: the Fight squad is retired; a record a bundle before it wrote carries
// the `squad` it latched, which is read as nothing and dropped.
wire("danger: a fought room with no seen tick costs its part, and a legacy squad key is dropped whatever it holds", async () => {
  const written = await dangerThrough({
    W1N5: { fight: { seen: "soon" }, threat },
    W1N6: { fight: { seen: 700 } },
    W1N7: { fight: { seen: 690, squad: 3 } },
    W1N8: { fight: { seen: 680, squad: "duo", raiders: ["inv-1"] } },
    W1N9: { fight: null },
  });

  assert.equal(
    stable(written),
    stable({
      W1N5: { threat },
      W1N6: { fight: { seen: 700 } },
      W1N7: { fight: { seen: 690 } },
      W1N8: { fight: { seen: 680, raiders: ["inv-1"] } },
    }),
  );
});

wire("danger: a fought room's raiders (the Brief's S7) that are no list, or no ids, read as none known", async () => {
  const written = await dangerThrough({
    W1N6: { fight: { seen: 700, raiders: "inv-1" } },
    W1N7: { fight: { seen: 690, raiders: [3, "", "inv-2"] } },
    W1N8: { fight: { seen: 680, raiders: [] } },
  });

  assert.equal(
    stable(written),
    stable({
      W1N6: { fight: { seen: 700 } },
      W1N7: { fight: { seen: 690, raiders: ["inv-2"] } },
      W1N8: { fight: { seen: 680 } },
    }),
  );
});

wire("danger: a probe log (#493) off its shape, or a taken tick (#496) that is no number, costs its part", async () => {
  const written = await dangerThrough({
    W18S26: { probe: probeOut, taken: 930_120 },
    W17S24: { probe: { ...probeOut, probe: 3 }, taken: "soon" },
    W17S25: { probe: { ...probeOut, fate: "fled" }, hold },
    W17S26: { probe: { ...probeOut, ended: undefined } },
    W17S27: { probe: { ...probeOut, safeMode: "soon" } },
    W17S28: { probe: null, taken: null },
  });

  assert.equal(stable(written), stable({ W18S26: { probe: probeOut, taken: 930_120 }, W17S25: { hold } }));
});

wire("danger: a part off its shape costs that part, and a room left with none is dropped", async () => {
  const written = await dangerThrough({
    W1N9: { down: { room: "W1N9", opened: 1, last: 2, basis: "collapse-timer" }, hold },
    W1N2: { down: outpost },
    W1N5: { hold: { holder: "invader", until: "soon" }, threat: { until: null } },
    W1N6: { watch: { seen: [{ x: 1, y: 2, dies: "soon" }] } },
    W1N7: { watch: { run: [[0, 1], "0,2"], exit: [0, 1], until: 900 } },
    W1N8: null,
  });

  assert.equal(stable(written), stable({ W1N9: { hold }, W1N2: { down: outpost } }));
});

wire("danger: a seen threat without an id (before #561) reads as none and round-trips without one; one whose id is no string costs the watch", async () => {
  // Written back with the room's round trip a row before #663 S5 lacks.
  const legacy = { seen: [{ x: 10, y: 11, dies: 1400 }] };
  const written = await dangerThrough({
    W1N3: { watch: legacy },
    W1N4: { watch: { seen: [{ id: 7, x: 10, y: 11, dies: 1400 }] } },
  });

  assert.equal(stable(written), stable({ W1N3: { watch: { seen: [{ x: 10, y: 11, dies: 1400, trip: 100 }] } } }));
});

wire("danger: a row filed under a room is that room's, whatever it names", async () => {
  const written = await dangerThrough({ W1N3: { down: outpost } });
  assert.equal(written.W1N3.down.room, "W1N3");
});

wire("danger: every stand-down basis round-trips, #441's harass-sighting included", async () => {
  const bases = [
    "collapse-timer",
    "reservation",
    "fallback",
    "rival-reservation",
    "invader-raid",
    "harass-sighting",
    "raid-gone",
    "player-raid-hold",
  ];
  const leaf = Object.fromEntries(
    bases.map((basis, i) => [`W1N${i + 10}`, { down: { ...outpost, room: `W1N${i + 10}`, basis } }]),
  );

  assert.equal(stable(await dangerThrough(leaf)), stable(leaf));
});

wire("danger: a stand-down whose basis is not in the vocabulary costs its row", async () => {
  const written = await dangerThrough({
    W1N9: { down: { ...outpost, room: "W1N9", basis: "not-a-basis" } },
    W1N2: { down: outpost },
  });

  assert.equal(stable(written), stable({ W1N2: { down: outpost } }));
});

wire("danger: a row's raid bodies (#507), player's tick (#513), chase (#510) and bounce round-trip; legacy rows write none", async () => {
  const flagged = { ...outpost, basis: "invader-raid", raiders: ["h1", "h2"], player: 15, chased: true, bounced: true };
  const written = await dangerThrough({ W1N2: { down: flagged }, W1N3: { down: { ...outpost, room: "W1N3" } } });

  assert.equal(stable(written.W1N2.down), stable(flagged));

  for (const key of ["raiders", "player", "chased", "bounced", "stronghold"]) {
    assert.equal(written.W1N3.down[key], undefined, `a legacy row writes no ${key}`);
  }
});

wire("danger: an odd flag reads as none, an off-shape player tick costs the row, a bad raid id the id", async () => {
  const written = await dangerThrough({
    W1N2: { down: { ...outpost, chased: "yes", bounced: 1, raiders: [7, "h2"] } },
    W1N3: { down: { ...outpost, room: "W1N3", player: "soon" } },
  });

  assert.equal(stable(written), stable({ W1N2: { down: { ...outpost, raiders: ["h2"] } } }));
});

// ---- the world raid roster ---------------------------------------------

// The world roster (ADR 0086 S5): `observe.world.roster.<room>.<id>` =
// `{ o, b, f, l, e, d? }`, the owner, the body as first seen, the first and
// last ticks seen there, the tick its life runs out on, and the tick its
// tombstone was first seen (a kill when before `e`).
const rosterThrough = async (leaf) => {
  const { loadRoster, saveRoster } = await import(MODULE);
  globalThis.Memory = { fabot: { observe: leaf === undefined ? {} : { world: { roster: leaf } } } };
  saveRoster(loadRoster());
  return globalThis.Memory.fabot.observe.world.roster;
};

const rosterRow = { o: "giaco", b: { tough: 2, attack: 1, move: 1 }, f: 977_432, l: 977_500, e: 978_900 };

for (const [name, leaf] of [
  ["absent", undefined],
  ["null", null],
  ["a string", "Incorrect memory path"],
  ["a number", 7],
  ["an array", []],
]) {
  wire(`roster: a leaf that is ${name} reads empty and writes an empty object, which stands`, async () => {
    assert.equal(stable(await rosterThrough(leaf)), stable({}));
  });
}

wire("roster: a well-formed leaf round-trips unchanged", async () => {
  const full = {
    W17S29: { TWX: { ...rosterRow, d: 977_501 }, Hauler: { o: "giaco", b: { carry: 10, move: 5 }, f: 977_400, l: 977_450, e: 977_451, d: 977_451 } },
    W16S29: { TWX: rosterRow },
  };

  assert.equal(stable(await rosterThrough(full)), stable(full));
});

wire("roster: a row off its shape, or with no life tick, costs that row, and a room left with none, or that is no object, is dropped", async () => {
  const written = await rosterThrough({
    W17S29: {
      ok: rosterRow,
      noOwner: { ...rosterRow, o: 3 },
      noFirst: { ...rosterRow, f: "soon" },
      noLast: { ...rosterRow, l: undefined },
      noLife: { ...rosterRow, e: undefined },
      badPart: { ...rosterRow, b: { wing: 1 } },
      badCount: { ...rosterRow, b: { move: "1" } },
      nothing: null,
    },
    W16S29: { gone: { ...rosterRow, o: null } },
    W15S29: "W15S29",
  });

  assert.equal(stable(written), stable({ W17S29: { ok: rosterRow } }));
});

wire("roster: a fell tick that is no number reads standing, and a key this bundle does not write is dropped", async () => {
  const written = await rosterThrough({ W17S29: { a: { ...rosterRow, d: "soon" }, b: { ...rosterRow, h: 1 } } });
  assert.equal(stable(written), stable({ W17S29: { a: rosterRow, b: rosterRow } }));
});

wire("danger: a probe log names the colony whose probe it is; one before the world held it names none, and a home that is no string reads as none", async () => {
  const written = await dangerThrough({
    W18S26: { probe: { ...probeProbing, home: "W15S28" } },
    W17S24: { probe: probeProbing },
    W17S25: { probe: { ...probeProbing, home: 7 } },
  });

  assert.equal(
    stable(written),
    stable({ W18S26: { probe: { ...probeProbing, home: "W15S28" } }, W17S24: { probe: probeProbing }, W17S25: { probe: probeProbing } }),
  );
});

// ---- dead colony leaves --------------------------------------------------

wire("prune: every colony leaf of a home no longer declared goes, with the flat legacy raids leaf", async () => {
  const { pruneColonies } = await import(MODULE);
  const { ofArray } = await import(LIST);
  const fresh = () => ({
    fabot: {
      observe: {
        raids: { episodes: [] },
        colonies: { W1N1: { raids: emptyRaids }, W11S29: { raids: emptyRaids }, W11S27: { layout: {} } },
      },
    },
  });

  globalThis.Memory = fresh();
  pruneColonies(ofArray(["W1N1", "W2N2"]));
  assert.deepEqual(Object.keys(globalThis.Memory.fabot.observe.colonies), ["W1N1"]);
  assert.equal("raids" in globalThis.Memory.fabot.observe, false);

  globalThis.Memory = fresh();
  pruneColonies(ofArray([]));
  assert.equal(stable(globalThis.Memory), stable(fresh()), "no home declared: nothing goes");
});

// ---- the CPU line -------------------------------------------------------

wire("cpu: an absent leaf reads empty, and a well-formed one round-trips", async () => {
  const { loadCpu, saveCpu } = await logCodec();

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

wire("cpu: the world lane's split rides beside the colonies' under `world` (#666)", async () => {
  const { loadCpu, saveCpu } = await logCodec();

  const row = {
    t: 10,
    ms: 60.5,
    entry: 0.1,
    snapshot: 14,
    decide: 40,
    save: 3,
    execute: 3.4,
    intents: 90,
    bucket: 10000,
    replans: 0,
    colonies: { world: 21.5, W12S28: 9.25 },
    floods: { world: [40, 2, 31000], W12S28: [3, 1, 900] },
  };

  globalThis.Memory = rootMemoryWith("cpu", { ticks: [row], spans: [] });
  saveCpu(loadCpu());
  assert.equal(stable(globalThis.Memory.fabot.observe.cpu.ticks), stable([row]));
});

for (const [name, leaf] of [
  ["null", null],
  ["a string", "Incorrect memory path"],
  ["a number", 7],
  ["an array where an object belongs", []],
  ["an object whose ticks are not an array", { ticks: 7 }],
]) {
  wire(`cpu: a leaf that is ${name} reads empty and writes empty`, async () => {
    const { loadCpu, saveCpu } = await logCodec();

    globalThis.Memory = rootMemoryWith("cpu", leaf);
    saveCpu(loadCpu());
    assert.equal(stable(globalThis.Memory.fabot.observe.cpu), stable({ ticks: [], spans: [] }));
  });
}

wire("cpu: a light tick's row round-trips its flag, and a row without one reads as a full tick (#443)", async () => {
  const { loadCpu, saveCpu } = await logCodec();

  const full = { t: 10, ms: 60.5, entry: 0.1, snapshot: 14, decide: 40, save: 3, execute: 3.4, intents: 90, bucket: 10000, replans: 0 };
  // A light tick's phases that did not run are zero-width, so the phase
  // decoder reads the row whole.
  const light = { t: 11, ms: 12.5, entry: 0.1, snapshot: 0, decide: 0, save: 0.4, execute: 12, intents: 40, bucket: 10000, replans: 0, light: true };

  globalThis.Memory = rootMemoryWith("cpu", { ticks: [full, light], spans: [] });
  saveCpu(loadCpu());
  assert.equal(
    stable(globalThis.Memory.fabot.observe.cpu.ticks),
    stable([full, light]),
    "the light row keeps its flag, the full row stays without the key",
  );

  // Off-shape flags are a full tick, never a thrown-away row.
  for (const flag of [false, null, "yes", 1]) {
    globalThis.Memory = rootMemoryWith("cpu", { ticks: [{ ...full, light: flag }], spans: [] });
    saveCpu(loadCpu());
    assert.equal(
      stable(globalThis.Memory.fabot.observe.cpu.ticks),
      stable([full]),
      `a flag of ${JSON.stringify(flag)} reads as a full tick`,
    );
  }
});

wire("cpu: a forced full tick's row round-trips its reason, and an off-shape one reads as none", async () => {
  const { loadCpu, saveCpu } = await logCodec();

  const full = { t: 10, ms: 60.5, entry: 0.1, snapshot: 14, decide: 40, save: 3, execute: 3.4, intents: 90, bucket: 10000, replans: 0 };
  const forced = { ...full, t: 12, forced: "border" };
  // #549: a partial forced tick says how many facts forced it and how many
  // colonies replayed; one fact and nobody replayed writes neither key.
  const partial = { ...full, t: 14, forced: "home W1N1 Invader", facts: 3, replayed: 5 };
  // And how many colonies decided their fight alone (`LightTick.fightOnly`).
  const fightOnly = { ...full, t: 16, forced: "home W1N1 Invader", fightOnly: 2 };
  // And the ms of its decide the replay and the movement pass spent (ADR
  // 0091 W7), the rest being the decides.
  const replayed = { ...full, t: 18, forced: "hurt w-1", replayed: 4, replay: 6.25 };

  globalThis.Memory = rootMemoryWith("cpu", { ticks: [full, forced, partial, fightOnly, replayed], spans: [] });
  saveCpu(loadCpu());
  assert.equal(
    stable(globalThis.Memory.fabot.observe.cpu.ticks),
    stable([full, forced, partial, fightOnly, replayed]),
    "the forced rows keep their reason, facts, replayed and fight-only counts and replay ms; the plain full row stays without the keys",
  );

  globalThis.Memory = rootMemoryWith("cpu", {
    ticks: [{ ...full, forced: "border", facts: 1, replayed: 0, fightOnly: 0, replay: 0 }],
    spans: [],
  });
  saveCpu(loadCpu());
  assert.equal(
    stable(globalThis.Memory.fabot.observe.cpu.ticks),
    stable([forced].map((row) => ({ ...row, t: 10 }))),
    "one fact and nothing replayed or fought alone re-encodes without the keys",
  );

  for (const reason of [null, 3, true, {}]) {
    globalThis.Memory = rootMemoryWith("cpu", { ticks: [{ ...full, forced: reason }], spans: [] });
    saveCpu(loadCpu());
    assert.equal(
      stable(globalThis.Memory.fabot.observe.cpu.ticks),
      stable([full]),
      `a reason of ${JSON.stringify(reason)} reads as none, and the row survives`,
    );
  }
});

wire("cpu: a replan's row round-trips what moved each colony's signature, and an absent or off-shape one reads as none", async () => {
  const { loadCpu, saveCpu } = await logCodec();

  const full = { t: 10, ms: 60.5, entry: 0.1, snapshot: 14, decide: 40, save: 3, execute: 3.4, intents: 90, bucket: 10000, replans: 0 };
  const replan = { ...full, t: 12, replans: 2, moved: { W15S28: ["W19S26", "level"], W17S29: ["reset"] } };

  globalThis.Memory = rootMemoryWith("cpu", { ticks: [full, replan], spans: [] });
  saveCpu(loadCpu());
  assert.equal(
    stable(globalThis.Memory.fabot.observe.cpu.ticks),
    stable([full, replan]),
    "the replan row keeps its moved parts per colony, the quiet row stays without the key",
  );

  for (const moved of [null, 3, "W19S26", [], {}]) {
    globalThis.Memory = rootMemoryWith("cpu", { ticks: [{ ...full, moved }], spans: [] });
    saveCpu(loadCpu());
    assert.equal(
      stable(globalThis.Memory.fabot.observe.cpu.ticks),
      stable([full]),
      `a moved of ${JSON.stringify(moved)} reads as none, and the row survives`,
    );
  }

  // A colony whose parts are not all strings is left out; its neighbour stays.
  globalThis.Memory = rootMemoryWith("cpu", {
    ticks: [{ ...full, moved: { W15S28: ["W19S26"], W17S29: [3] } }],
    spans: [],
  });
  saveCpu(loadCpu());
  assert.equal(
    stable(globalThis.Memory.fabot.observe.cpu.ticks),
    stable([{ ...full, moved: { W15S28: ["W19S26"] } }]),
    "the malformed colony is dropped, the well-formed one kept",
  );
});

wire("cpu: the coarse spans round-trip as positional arrays, and a leaf without them reads empty", async () => {
  const { loadCpu, saveCpu } = await logCodec();

  const sample = { t: 10, ms: 1.5, entry: 0.1, snapshot: 0.2, decide: 0.3, save: 0.4, execute: 0.5, intents: 3, bucket: 10000, replans: 0 };

  // A leaf written before #386 has no `spans` key at all, and absent is the
  // empty record rather than a throw that would cost the ticks beside it.
  globalThis.Memory = rootMemoryWith("cpu", { ticks: [sample] });
  saveCpu(loadCpu());
  assert.deepEqual(globalThis.Memory.fabot.observe.cpu.spans, [], "a line written before the spans keeps its ticks");

  // [f, t, n, max, sum, b, r, p, h, w, hmin, x, ss, sd, sv, sx] (#538).
  const span = [100, 199, 100, 480.5, 2000.25, 3400, 2, 91920, 140.5, 719, 65.7, 39.5, 728.969, 2105.733, 221.267, 490.671];

  globalThis.Memory = rootMemoryWith("cpu", { ticks: [sample], spans: [span] });
  saveCpu(loadCpu());
  assert.equal(stable(globalThis.Memory.fabot.observe.cpu.spans), stable([span]), "and one that has them round-trips cell for cell");
  assert.ok(Array.isArray(globalThis.Memory.fabot.observe.cpu.spans[0]), "a plain array, not a typed one");

  // A short array and a cell that is not a number each cost that span alone.
  const bad = [span.slice(0, 15), [...span.slice(0, 7), "x", ...span.slice(8)]];
  globalThis.Memory = rootMemoryWith("cpu", { ticks: [sample], spans: [...bad, span] });
  saveCpu(loadCpu());
  assert.equal(stable(globalThis.Memory.fabot.observe.cpu.spans), stable([span]), "the off-shape spans are dropped, the good one kept");
});

wire("cpu: a keyed span written before #538 is read into the positional form, so a deploy keeps the record", async () => {
  const { loadCpu, saveCpu } = await logCodec();

  const keyed = { f: 100, t: 199, n: 100, max: 480.5, sum: 2000.25, b: 3400, r: 2, p: 91920, h: 140.5, w: 719, hmin: 65.7, x: 39.5, ss: 728.969, sd: 2105.733, sv: 221.267, sx: 490.671 };
  globalThis.Memory = rootMemoryWith("cpu", { ticks: [], spans: [keyed] });
  saveCpu(loadCpu());
  assert.equal(
    stable(globalThis.Memory.fabot.observe.cpu.spans),
    stable([[100, 199, 100, 480.5, 2000.25, 3400, 2, 91920, 140.5, 719, 65.7, 39.5, 728.969, 2105.733, 221.267, 490.671]]),
    "every key lands in its cell",
  );

  // A span older than #389/#391 lacks the later keys; they read as zero.
  const older = { f: 100, t: 199, n: 100, max: 480.5, sum: 2000.25, b: 3400, r: 2 };
  globalThis.Memory = rootMemoryWith("cpu", { ticks: [], spans: [older] });
  saveCpu(loadCpu());
  assert.equal(
    stable(globalThis.Memory.fabot.observe.cpu.spans),
    stable([[100, 199, 100, 480.5, 2000.25, 3400, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0]]),
    "absent later keys are zeros",
  );

  // A required key missing, or any key present and not a number, costs the span.
  for (const off of [{ ...keyed, f: undefined }, { ...keyed, r: undefined }, { ...keyed, h: "x" }]) {
    globalThis.Memory = rootMemoryWith("cpu", { ticks: [], spans: [off, keyed] });
    saveCpu(loadCpu());
    assert.equal(globalThis.Memory.fabot.observe.cpu.spans.length, 1, `${JSON.stringify(off)} is dropped, its neighbour kept`);
  }
});

wire("cpu: only the newest rows keep the per-name splits; an older row keeps its pops (#538)", async () => {
  const { loadCpu, saveCpu, appendCpu } = await logCodec();

  const DETAIL = 20;
  const bare = (t) => ({ t, ms: 1.5, entry: 0.1, snapshot: 0.2, decide: 0.3, save: 0.4, execute: 0.5, intents: 3, bucket: 10000, replans: 0 });
  const whole = (t) => ({
    ...bare(t),
    colonies: { W1N1: 0.3 },
    rooms: { W1N1: 0.1 },
    projects: { W1N1: 0.05 },
    floods: { W1N1: [2, 1, 40 + t] },
    head: 0.02,
  });
  const aged = (t) => ({ ...bare(t), pops: 40 + t });

  // A bare row carrying `pops` round-trips, and a row with floods does not
  // grow the key.
  globalThis.Memory = rootMemoryWith("cpu", { ticks: [aged(1), whole(2)], spans: [] });
  saveCpu(loadCpu());
  assert.equal(stable(globalThis.Memory.fabot.observe.cpu.ticks), stable([aged(1), whole(2)]), "both shapes round-trip");

  // The append rewrites the row that aged out of the detail window bare.
  // The state is what Core's fold leaves: the aged row already bare.
  // The leaf holds ticks 1..21, the newest twenty whole; the state adds
  // tick 22, and tick 2 has aged out of the twenty.
  const before = [aged(1), ...Array.from({ length: DETAIL }, (_, i) => whole(i + 2))];
  const after = [aged(1), aged(2), ...Array.from({ length: DETAIL }, (_, i) => whole(i + 3))];
  globalThis.Memory = rootMemoryWith("cpu", { ticks: after, spans: [] });
  const state = loadCpu();
  globalThis.Memory = rootMemoryWith("cpu", { ticks: before, spans: [] });
  appendCpu(state);
  const rows = globalThis.Memory.fabot.observe.cpu.ticks;
  assert.equal(rows.length, DETAIL + 2, "the newest row is pushed");
  assert.equal(stable(rows[1]), stable(aged(2)), "the row that aged out is rewritten bare, with its pops");
  assert.equal(stable(rows[2]), stable(whole(3)), "the oldest of the newest twenty keeps its splits");
  assert.equal(stable(rows[DETAIL + 1]), stable(whole(DETAIL + 2)), "and so does the newest");
});

wire("probe: the reset tick's drop clears a leftover Memory.__probe (#538)", async () => {
  const { dropProbe } = await import(MODULE);

  globalThis.Memory = { __probe: { ticks: 3, to: 9, window: 300, fns: {} }, fabot: {} };
  dropProbe();
  assert.equal(Object.hasOwn(globalThis.Memory, "__probe"), false, "the key is gone, not set to null");
  assert.ok(globalThis.Memory.fabot, "and nothing beside it");
});

wire("cpu: the append keeps the coarse spans current, one row a tick (#390)", async () => {
  const { loadCpu, appendCpu } = await logCodec();

  const row = (t) => ({ t, ms: 1.5, entry: 0.1, snapshot: 0.2, decide: 0.3, save: 0.4, execute: 0.5, intents: 3, bucket: 10000, replans: 0 });
  const span = (f, t, n) => [f, t, n, 1.5, 1.5 * n, 10000, 0, 0, 0, 0, 0, 0, 0.2 * n, 0.3 * n, 0.4 * n, 0.5 * n];

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

  // At the cap (#394): the state holds 200 spans whether the open span
  // continues or just opened, so the two are told apart by the open span's
  // `From`. A span that opened shifts the oldest out and is pushed; one
  // that continues is rewritten in place.
  const CAP = 200;
  const full = Array.from({ length: CAP }, (_, i) => span(i * 100 + 1, i * 100 + 100, 100));
  const opened = stateOf([row(1), row(2)], [...full.slice(1), span(CAP * 100 + 1, CAP * 100 + 1, 1)]);
  globalThis.Memory = rootMemoryWith("cpu", { ticks: [row(1)], spans: [...full] });
  appendCpu(opened);
  const afterOpen = globalThis.Memory.fabot.observe.cpu.spans;
  assert.equal(afterOpen.length, CAP, "the leaf stays at the cap");
  assert.equal(afterOpen[0][0], 101, "the oldest span left");
  assert.equal(stable(afterOpen[CAP - 2]), stable(full[CAP - 1]), "the last closed span is kept");
  assert.equal(afterOpen[CAP - 1][0], CAP * 100 + 1, "and the one that opened is pushed after it");

  const continued = stateOf([row(1), row(2), row(3)], [...full.slice(1), span(CAP * 100 + 1, CAP * 100 + 2, 2)]);
  globalThis.Memory = rootMemoryWith("cpu", { ticks: [row(1), row(2)], spans: afterOpen });
  appendCpu(continued);
  const afterContinue = globalThis.Memory.fabot.observe.cpu.spans;
  assert.equal(afterContinue.length, CAP, "a continuing span adds nothing");
  assert.equal(afterContinue[CAP - 1][1], CAP * 100 + 2, "and is rewritten in place");
  assert.equal(stable(afterContinue[CAP - 2]), stable(full[CAP - 1]), "with the closed span beside it untouched");

  // A leaf whose last span slot is null or off the shape disagrees and is
  // written whole — never a throw in the loop.
  for (const bad of [null, 7, "x", {}, { f: 20001 }, ["20001"]]) {
    globalThis.Memory = rootMemoryWith("cpu", { ticks: [row(1), row(2)], spans: [span(1, 3, 3), bad] });
    appendCpu(continued);
    assert.equal(globalThis.Memory.fabot.observe.cpu.spans.length, CAP, `a last span that is ${JSON.stringify(bad)} is rewritten whole`);
  }
});


wire("cpu: heap, memo rows and the span's phase sums round-trip, and absent reads zero (#391)", async () => {
  const { loadCpu, saveCpu } = await logCodec();

  const base = { t: 10, ms: 1.5, entry: 0.1, snapshot: 0.2, decide: 0.3, save: 0.4, execute: 0.5, intents: 3, bucket: 10000, replans: 0 };
  const span = [100, 199, 100, 480.5, 2000.25, 3400, 2, 91920, 55.3, 1400, 41.3, 27.5, 900.5, 2100.25, 300, 700];

  globalThis.Memory = rootMemoryWith("cpu", { ticks: [{ ...base, heap: 41.3, rows: 900, ext: 27.5 }], spans: [span] });
  saveCpu(loadCpu());
  assert.equal(stable(globalThis.Memory.fabot.observe.cpu.ticks[0]), stable({ ...base, heap: 41.3, rows: 900, ext: 27.5 }), "the row's heap, rows and off-heap size round-trip");
  assert.equal(stable(globalThis.Memory.fabot.observe.cpu.spans[0]), stable(span), "and the span's new keys");

  // A row #391's bundle wrote has heap and rows but no `ext`: it reads as
  // zero and is written back with the key, as the span's `p` is.
  globalThis.Memory = rootMemoryWith("cpu", { ticks: [{ ...base, heap: 41.3, rows: 900 }], spans: [] });
  saveCpu(loadCpu());
  assert.equal(stable(globalThis.Memory.fabot.observe.cpu.ticks[0]), stable({ ...base, heap: 41.3, rows: 900, ext: 0 }), "a row without the off-heap size reads it as zero");

  // A legacy row carries none of the keys; it reads as zero and a zero is
  // not written back, so the row re-encodes as itself.
  globalThis.Memory = rootMemoryWith("cpu", { ticks: [base], spans: [] });
  saveCpu(loadCpu());
  assert.equal(stable(globalThis.Memory.fabot.observe.cpu.ticks[0]), stable(base), "no heap, no key");

  // And the row alike: a non-number heap costs that row and no other.
  globalThis.Memory = rootMemoryWith("cpu", { ticks: [{ ...base, heap: "x" }, { ...base, t: 11, heap: 41.3, rows: 0 }], spans: [] });
  saveCpu(loadCpu());
  assert.equal(stable(globalThis.Memory.fabot.observe.cpu.ticks), stable([{ ...base, t: 11, heap: 41.3, rows: 0, ext: 0 }]), "the bad row is dropped, and a measured tick with no memo rows keeps its zero");
});

wire("cpu: the flood counts ride the row per colony, and a malformed triple is left out", async () => {
  const { loadCpu, saveCpu } = await logCodec();

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
  const { loadCpu, saveCpu } = await logCodec();

  const good = [100, 199, 100, 480.5, 2000.25, 3400, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0];

  for (let i = 0; i < good.length; i++) {
    const bad = good.map((cell, j) => (j === i ? {} : cell));
    globalThis.Memory = rootMemoryWith("cpu", { ticks: [], spans: [bad, good] });
    saveCpu(loadCpu());
    const spans = globalThis.Memory.fabot.observe.cpu.spans;

    assert.equal(spans.length, 1, `a span whose cell ${i} is not a number is dropped`);
    assert.equal(stable(spans[0]), stable(good), "and the well-formed one beside it is kept");
  }
});

wire("cpu: a row that will not decode costs its row and not the line", async () => {
  const { loadCpu, saveCpu } = await logCodec();

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

// The kinds the bot reads itself by (#630, #629), one row each as the fold
// writes it: a pending `spawn-idle` or `refusal-storm` carries its `window`,
// every other row none.
const selfRows = {
  "light-starved": { kind: "light-starved", room: "", subject: "chase", amount: 20, first: 100, last: 120 },
  "bucket-falling": { kind: "bucket-falling", room: "", subject: "bucket", amount: 400, first: 100, last: 120 },
  fault: { kind: "fault", room: "W1N1", subject: "decide", amount: 3, first: 100, last: 120 },
  "spawn-idle": { kind: "spawn-idle", room: "W1N1", subject: "Spawn1", amount: 2, first: 100, last: 120, window: 150 },
  "body-idle": { kind: "body-idle", room: "W1N2", subject: "guard-1", amount: 150, first: 100, last: 120 },
  "refusal-storm": {
    kind: "refusal-storm",
    room: "W1N1",
    subject: "claimer-1",
    amount: 48,
    first: 100,
    last: 120,
    window: 100,
  },
};

for (const [kind, row] of Object.entries(selfRows)) {
  wire(`breaches: a ${kind} row round-trips in a colony's log and in the world's`, async () => {
    const { loadBreaches, saveBreaches, loadWorldBreaches, saveWorldBreaches } = await import(MODULE);

    globalThis.Memory = memoryWith("breaches", { rows: [row] });
    saveBreaches(home, loadBreaches(home));
    assert.equal(stable(globalThis.Memory.fabot.observe.colonies[home].breaches), stable({ rows: [row] }));

    globalThis.Memory = rootMemoryWith("breaches", { rows: [row] });
    saveWorldBreaches(loadWorldBreaches());
    assert.equal(stable(globalThis.Memory.fabot.observe.breaches), stable({ rows: [row] }));
  });
}

wire("breaches: the world's absent leaf reads empty and writes empty", async () => {
  const { loadWorldBreaches, saveWorldBreaches } = await import(MODULE);

  globalThis.Memory = rootMemoryWith("breaches", undefined);
  saveWorldBreaches(loadWorldBreaches());
  assert.equal(stable(globalThis.Memory.fabot.observe.breaches), stable({ rows: [] }));
});

wire("breaches: a row whose window is not a number costs its row", async () => {
  const { loadBreaches, saveBreaches } = await import(MODULE);

  globalThis.Memory = memoryWith("breaches", {
    rows: [{ ...selfRows["spawn-idle"], window: "150" }, selfRows["body-idle"]],
  });

  saveBreaches(home, loadBreaches(home));
  const rows = globalThis.Memory.fabot.observe.colonies[home].breaches.rows;

  assert.equal(rows.length, 1);
  assert.equal(rows[0].subject, "guard-1");
});

// An incoming nuke's row (#657): the world's log, its impact riding as `nuke`.
const nukeRow = {
  kind: "incoming-nuke",
  room: "W1N1",
  subject: "nuke-1",
  amount: 49_900,
  first: 100,
  last: 200,
  nuke: {
    x: 25,
    y: 25,
    lands: 50_100,
    launch: "W20S20",
    exposed: [
      { id: "spawn-1", kind: "spawn", x: 25, y: 25, hits: 5000, blast: 10_000_000, rampart: 8_000_000, shortfall: 1_995_001 },
      { id: "rampart-1", kind: "rampart", x: 25, y: 25, hits: 8_000_000, blast: 10_000_000, rampart: 0, shortfall: 2_000_001 },
    ],
    creeps: ["worker-1", "worker-2"],
  },
};

wire("breaches: an incoming-nuke row round-trips with its impact", async () => {
  const { loadWorldBreaches, saveWorldBreaches } = await import(MODULE);

  globalThis.Memory = rootMemoryWith("breaches", { rows: [nukeRow] });
  saveWorldBreaches(loadWorldBreaches());
  assert.equal(stable(globalThis.Memory.fabot.observe.breaches), stable({ rows: [nukeRow] }));
});

for (const [name, nuke] of [
  ["a string", "boom"],
  ["missing its landing tick", { ...nukeRow.nuke, lands: undefined }],
  ["an exposed row without hits", { ...nukeRow.nuke, exposed: [{ ...nukeRow.nuke.exposed[0], hits: "5000" }] }],
  ["creeps that are not names", { ...nukeRow.nuke, creeps: [7] }],
]) {
  wire(`breaches: an incoming-nuke row whose impact is ${name} costs its row`, async () => {
    const { loadWorldBreaches, saveWorldBreaches } = await import(MODULE);

    globalThis.Memory = rootMemoryWith("breaches", { rows: [{ ...nukeRow, nuke }, selfRows["bucket-falling"]] });
    saveWorldBreaches(loadWorldBreaches());
    const rows = globalThis.Memory.fabot.observe.breaches.rows;

    assert.equal(rows.length, 1);
    assert.equal(rows[0].subject, "bucket");
  });
}

// ---- the fault ring (#628)---------------------------------------------

const fault = (t, colony = "W1N2") => ({
  t,
  colony,
  phase: "open",
  message: "Conflicting creep intents",
  frames: ["at openWhole (main:10:5)", "at decideWorld (main:20:3)"],
});

const faultsThrough = async (value) => {
  const { loadFaults, saveFaults } = await import(MODULE);
  globalThis.Memory = rootMemoryWith("faults", value);
  saveFaults(loadFaults());
  return globalThis.Memory.fabot.observe.faults;
};

for (const [name, leaf] of [
  ["absent", undefined],
  ["null", null],
  ["a string", "Incorrect memory path"],
  ["a number", 7],
  ["an object whose rows are not an array", { rows: 7 }],
]) {
  wire(`faults: a leaf that is ${name} reads empty and writes empty`, async () => {
    assert.equal(stable(await faultsThrough(leaf)), stable({ rows: [] }));
  });
}

wire("faults: a well-formed ring round-trips unchanged, newest first", async () => {
  const ring = { rows: [fault(20), fault(10, "")] };
  assert.equal(stable(await faultsThrough(ring)), stable(ring));
});

wire("faults: a row off its shape costs its row, a frame that is no string costs its frame", async () => {
  const { message, ...unsaid } = fault(30);
  const rows = (
    await faultsThrough({
      rows: [unsaid, { ...fault(20), t: "20" }, { ...fault(10), frames: ["at f (main:1:1)", 7] }],
    })
  ).rows;

  assert.equal(rows.length, 1, "the row with no message and the row with no tick are dropped");
  assert.equal(rows[0].t, 10);
  assert.deepEqual(rows[0].frames, ["at f (main:1:1)"]);
});

wire("faults: a tick's faults land before the ring's, which keeps the newest 20", async () => {
  const { recordFaults, FaultRow, capFaults } = await import(MODULE);
  const { ofArray } = await import(LIST);
  const row = (t) => new FaultRow(t, "W1N1", "finish", "boom", ofArray(["at f (main:1:1)"]));

  globalThis.Memory = rootMemoryWith("faults", { rows: Array.from({ length: capFaults }, (_, i) => fault(100 - i)) });
  recordFaults(ofArray([row(200), row(200)]));
  const rows = globalThis.Memory.fabot.observe.faults.rows;

  assert.equal(rows.length, capFaults);
  assert.deepEqual(
    rows.slice(0, 3).map((r) => [r.t, r.colony]),
    [
      [200, "W1N1"],
      [200, "W1N1"],
      [100, "W1N2"],
    ],
  );
  assert.equal(rows[capFaults - 1].t, 100 - (capFaults - 3), "the two oldest fell off the end");
});

wire("faults: a tick with no fault leaves the leaf unwritten", async () => {
  const { recordFaults } = await import(MODULE);
  const { ofArray } = await import(LIST);
  globalThis.Memory = rootMemoryWith("cadence", 3);
  recordFaults(ofArray([]));
  assert.equal(globalThis.Memory.fabot.observe.faults, undefined);
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

wire("ticks: a stand-down's expiry, a hold's, a threat's or a watch's tick off the shape costs its part, and invents no zero", async () => {
  const written = await dangerThrough({
    W1N2: { down: { ...outpost, expiry: {} } },
    W1N3: { hold: { ...hold, until: {} } },
    W1N4: { threat: { until: {} } },
    W1N5: { watch: { ...heldWatch, until: {} } },
    W1N6: { watch: { seen: [{ x: 1, y: 2, dies: {} }] } },
  });

  assert.equal(stable(written), stable({}), "a zero here would read as the oldest tick there is, not as a gap");
});

const offShape = [
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
  });

  assert.equal(written.episodes.length, 1, "one episode is dropped, one kept");

  const danger = await dangerThrough({ W1N9: { down: { ...outpost, room: "W1N9", expiry: {} } }, W1N2: { down: outpost } });
  assert.deepEqual(Object.keys(danger), ["W1N2"], "and the kept stand-down is the well-formed one");
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
  const { load, save, saveChanged } = await logCodec();
  globalThis.Memory = rootMemoryWith("creeps", value);
  const state = load();
  write === "changed" ? saveChanged(state, state) : save(state);
  return globalThis.Memory.fabot.observe.creeps;
};

wire("creeps: an absent leaf reads empty, and a well-formed log round-trips", async () => {
  assert.equal(stable(await creepsThrough(undefined)), stable({}));
  assert.equal(stable(await creepsThrough({ w1: creepLog })), stable({ w1: creepLog }));
});

wire("creeps: a release outranked by a struck rampart's Repair round-trips (#481)", async () => {
  const outranked = {
    ...creepLog,
    log: [{ t: 100, v: { kind: "released", task: "upgrade:ctrl-1", reason: "outranked" } }],
  };

  assert.equal(stable(await creepsThrough({ w1: outranked })), stable({ w1: outranked }));
});

wire("creeps: a ranger called off a quiet Guard to a chase round-trips (#510)", async () => {
  const called = {
    ...creepLog,
    log: [{ t: 100, v: { kind: "released", task: "guard:W1N3", reason: "called" } }],
  };

  assert.equal(stable(await creepsThrough({ w1: called })), stable({ w1: called }));
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

wire("creeps: an engine refusal's row and its Refused release round-trip (#629)", async () => {
  const refused = {
    ...creepLog,
    log: [
      { t: 100, v: { kind: "refused", task: "claim:ctrl-1", code: -15, count: 4 } },
      { t: 103, v: { kind: "released", task: "claim:ctrl-1", reason: "refused" } },
      { t: 104, v: { kind: "refused", task: "", code: -12, count: 1 } },
    ],
  };

  assert.equal(stable(await creepsThrough({ w1: refused })), stable({ w1: refused }));
});

wire("creeps: a refusal with no Task string, or a code or count not a number, costs its row", async () => {
  const written = await creepsThrough({
    w1: {
      ...creepLog,
      log: [
        { t: 100, v: { kind: "refused", task: "claim:ctrl-1", code: "-15", count: 4 } },
        { t: 101, v: { kind: "refused", task: "claim:ctrl-1", code: -15 } },
        { t: 102, v: { kind: "refused", code: -15, count: 2 } },
        { t: 103, v: { kind: "refused", task: "claim:ctrl-1", code: -15, count: 2 } },
      ],
    },
  });

  assert.equal(written.w1.log.length, 1, "the three off-shape refusals are dropped");
  assert.equal(written.w1.log[0].t, 103);
  assert.equal(written.w1.log[0].v.task, "claim:ctrl-1", "and no Task is read as \"undefined\"");
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
  const { load, saveChanged } = await logCodec();

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

wire("visuals: the draw list reads as the verbose list does, off its own leaf", async () => {
  const { loadVisuals } = await import(MODULE);

  for (const [what, value, size] of [
    ["absent", undefined, 0],
    ["a room and a creep", ["W13S28", "ranger-1"], 2],
    ["a list with a number in it", ["W13S28", 2], 0],
    ["an object", { W13S28: true }, 0],
    ["null", null, 0],
  ]) {
    globalThis.Memory = rootMemoryWith("visuals", value);
    assert.equal(loadVisuals().size, size, `${what} reads as ${size} names`);
  }

  globalThis.Memory = rootMemoryWith("verbose", ["w1"]);
  assert.equal(loadVisuals().size, 0, "the verbose list is not the draw list");
});

// ---- the live cadence ----------------------------------------------------

wire("cadence: a whole number reads clamped to 2..4, and anything else reads as unset", async () => {
  const { loadCadence } = await import(MODULE);

  for (const [what, value, read] of [
    ["absent", undefined, undefined],
    ["2", 2, 2],
    ["3", 3, 3],
    ["4", 4, 4],
    ["1, under the floor", 1, 2],
    ["0", 0, 2],
    ["-5", -5, 2],
    ["9, over the ceiling", 9, 4],
    ["1e300", 1e300, 4],
    ["Infinity", Infinity, 4],
    ["a fraction", 2.5, undefined],
    ["NaN", NaN, undefined],
    ["a numeric string", "3", undefined],
    ["null", null, undefined],
    ["an object", { cadence: 3 }, undefined],
    ["a list", [3], undefined],
    ["true", true, undefined],
  ]) {
    globalThis.Memory = rootMemoryWith("cadence", value);
    assert.equal(loadCadence(), read, `${what} reads as ${read}`);
  }

  globalThis.Memory = {};
  assert.equal(loadCadence(), undefined, "no fabot subtree reads as unset");
  assert.equal("fabot" in globalThis.Memory, false, "and the read writes nothing");
});

// ---- the live fight-only switch ------------------------------------------

wire("fightOnly: a boolean reads as itself, and anything else reads as unset", async () => {
  const { loadFightOnly } = await import(MODULE);

  for (const [what, value, read] of [
    ["absent", undefined, undefined],
    ["true", true, true],
    ["false", false, false],
    ["a string", "true", undefined],
    ["a number", 1, undefined],
    ["null", null, undefined],
    ["an object", { fightOnly: true }, undefined],
  ]) {
    globalThis.Memory = rootMemoryWith("fightOnly", value);
    assert.equal(loadFightOnly(), read, `${what} reads as ${read}`);
  }

  globalThis.Memory = {};
  assert.equal(loadFightOnly(), undefined, "no fabot subtree reads as unset");
  assert.equal("fabot" in globalThis.Memory, false, "and the read writes nothing");
});

// ---- the switches in effect (#575) ---------------------------------------

wire("switches: each switch in effect and its source, as observe health and cpu read them", async () => {
  const { saveSwitches } = await import(MODULE);
  const { SwitchFrom } = await import("../build/fable/Core/LightTick.js");

  globalThis.Memory = rootMemoryWith("cadence", 3);
  // Fable spreads each tupled parameter into two.
  saveSwitches(3, SwitchFrom.Override, false, SwitchFrom.Tuned);
  assert.equal(
    stable(globalThis.Memory.fabot.observe.switches),
    stable({
      cadence: 3,
      cadenceFrom: "override",
      fightOnly: false,
      fightOnlyFrom: "tuned",
    }),
  );
  assert.equal(globalThis.Memory.fabot.observe.cadence, 3, "the override beside it stands untouched");

  saveSwitches(3, SwitchFrom.Bucket, true, SwitchFrom.Tuned);
  const { cadenceFrom, fightOnly } = globalThis.Memory.fabot.observe.switches;
  assert.equal(cadenceFrom, "bucket");
  assert.equal(fightOnly, true);
});

// ---- the bundle's name (#634) -------------------------------------------

wire("version: the bundle's change and its reset tick, as observe health and cpu read them", async () => {
  const { saveVersion } = await import(MODULE);

  globalThis.Memory = rootMemoryWith("cadence", 3);
  saveVersion("pmvunwvtkxys 56cb9d3e4893+dirty", 42);
  assert.equal(
    stable(globalThis.Memory.fabot.observe.version),
    stable({ build: "pmvunwvtkxys 56cb9d3e4893+dirty", reset: 42 }),
  );
  assert.equal(globalThis.Memory.fabot.observe.cadence, 3, "the leaf beside it stands untouched");

  globalThis.Memory = {};
  saveVersion("unbuilt", 7);
  assert.equal(stable(globalThis.Memory.fabot.observe.version), stable({ build: "unbuilt", reset: 7 }), "an absent subtree is made");
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

// ---- the draw record (#530) ----------------------------------------------

// Each loaded creep's draw: the store and the tick it drew, `{ s, t }`. A row
// that will not read costs itself: read as no record, the load may go to any
// sink, the old behaviour. A tick that is not a number is dropped, never read
// as tick 0 (an expired record would only be harmless by luck).
const drawnThrough = async (value) => {
  const { loadDrawn, saveDrawn } = await import(MODULE);
  globalThis.Memory = rootMemoryWith("drawn", value);
  saveDrawn(loadDrawn());
  return globalThis.Memory.fabot.observe.drawn;
};

wire("drawn: load then save keeps every good row", async () => {
  const leaf = { h1: { s: "sto-1", t: 972170 }, h2: { s: "can-a", t: 972160 } };
  assert.equal(stable(await drawnThrough(leaf)), stable(leaf));
});

wire("drawn: an absent or off-shape leaf reads empty, and a bad row costs its row", async () => {
  assert.equal(stable(await drawnThrough(undefined)), stable({}), "an absent leaf reads empty");
  assert.equal(stable(await drawnThrough("Incorrect memory path")), stable({}), "so does a wrong type");

  const good = { s: "sto-1", t: 972170 };

  for (const [what, bad] of [
    ["a null row", null],
    ["a bare id, the shape before the tick", "sto-1"],
    ["an empty id", { s: "", t: 972170 }],
    ["a tick that is not a number", { s: "sto-1", t: {} }],
    ["a row with no tick", { s: "sto-1" }],
  ]) {
    assert.equal(
      stable(await drawnThrough({ h1: bad, h2: good })),
      stable({ h2: good }),
      `${what} is dropped and the row beside it kept`,
    );
  }
});

// ---- the room latches (#444, #445) --------------------------------------

// Who owns each rival room, and the homes a tower of ours has stood full in:
// heap state the bot also keeps here so a global reset does not forget them.
// The two halves are independent facts, so each degrades on its own, and an
// off-shape entry costs its own entry and no other.

const roomsThrough = async (value) => {
  const { loadRoomLatches, saveRoomLatches } = await import(MODULE);
  globalThis.Memory = rootMemoryWith("rooms", value);
  saveRoomLatches(loadRoomLatches());
  return globalThis.Memory.fabot.observe.rooms;
};

const emptyRooms = { rivals: {}, towered: [], armed: {}, controllers: {} };

// W18S26 as the #489 projection writes it: safe mode until, cooldown until,
// stock, upgrade block until, ticks to downgrade, the tick seen.
const w18s26 = { o: "Trepidimous", l: 6, s: 943747, c: 973747, a: 3, b: 0, d: 119000, t: 923747 };

wire("rooms: a well-formed leaf round-trips key for key", async () => {
  const leaf = {
    rivals: { W17S24: "Trepidimous", W18S26: "Trepidimous" },
    towered: ["W12S28", "W13S28"],
    armed: { W17S26: 923853 },
    controllers: { W18S26: w18s26 },
  };

  assert.equal(stable(await roomsThrough(leaf)), stable(leaf));
});

for (const [name, leaf] of [
  ["absent", undefined],
  ["null", null],
  ["a string", "Incorrect memory path"],
  ["a number", 7],
  ["an array", ["W18S26"]],
  ["an empty object", {}],
]) {
  wire(`rooms: a leaf that is ${name} reads empty and writes empty`, async () => {
    assert.equal(stable(await roomsThrough(leaf)), stable(emptyRooms));
  });
}

wire("rooms: each half degrades alone, and an off-shape entry costs only itself", async () => {
  assert.equal(
    stable(await roomsThrough({ rivals: ["W18S26"], towered: ["W12S28"] })),
    stable({ rivals: {}, towered: ["W12S28"], armed: {}, controllers: {} }),
    "rivals that are not an object read empty; the towers stand",
  );

  assert.equal(
    stable(await roomsThrough({ rivals: { W18S26: "Trepidimous" }, towered: { W12S28: true } })),
    stable({ rivals: { W18S26: "Trepidimous" }, towered: [], armed: {}, controllers: {} }),
    "towers that are not an array read empty; the rivals stand",
  );

  assert.equal(
    stable(
      await roomsThrough({
        rivals: { W1N1: 7, W1N2: null, W1N3: "", W1N4: {}, W18S26: "Trepidimous" },
        towered: [3, null, "", "W12S28"],
      }),
    ),
    stable({ rivals: { W18S26: "Trepidimous" }, towered: ["W12S28"], armed: {}, controllers: {} }),
    "an owner or a room that is not a non-empty string is dropped, its neighbours kept",
  );
});

wire("rooms: the armed rooms (#485) are a room-to-tick object; an older leaf without them reads none", async () => {
  assert.equal(
    stable(await roomsThrough({ rivals: {}, towered: [] })),
    stable(emptyRooms),
    "absent: nothing remembered, and the key written back",
  );

  assert.equal(
    stable(await roomsThrough({ rivals: {}, towered: [], armed: ["W17S26"] })),
    stable(emptyRooms),
    "an array reads empty",
  );

  assert.equal(
    stable(
      await roomsThrough({
        rivals: {},
        towered: [],
        armed: { W17S26: 923853, W16S25: "923853", W15S25: null },
      }),
    ),
    stable({ rivals: {}, towered: [], armed: { W17S26: 923853 }, controllers: {} }),
    "a tick that is not a number costs its own room",
  );
});

wire("rooms: the rival controllers (#489) are a room-to-facts object; an off-shape one costs only itself", async () => {
  assert.equal(
    stable(await roomsThrough({ rivals: {}, towered: [], armed: {} })),
    stable(emptyRooms),
    "absent on an older leaf: nothing remembered, and the key written back",
  );

  assert.equal(
    stable(await roomsThrough({ rivals: {}, towered: [], armed: {}, controllers: [w18s26] })),
    stable(emptyRooms),
    "an array reads empty",
  );

  assert.equal(
    stable(
      await roomsThrough({
        rivals: {},
        towered: [],
        armed: {},
        controllers: {
          W18S26: w18s26,
          W17S24: { ...w18s26, o: "" },
          W22S28: { ...w18s26, s: "943747" },
          W19S29: { ...w18s26, t: undefined },
          W21S23: null,
        },
      }),
    ),
    stable({ rivals: {}, towered: [], armed: {}, controllers: { W18S26: w18s26 } }),
    "an empty owner or a key that is not a number costs its own room",
  );
});

// The world's rival latches (ADR 0086 S2) ride `rivals`: a latched room's
// entry is `{ o?, since, lastLooked }`, an owner alone stays a bare string.

wire("rooms: a latched room round-trips with its owner, or without one, and a legacy bare owner stays bare", async () => {
  const leaf = {
    ...emptyRooms,
    rivals: {
      W18S26: { o: "Trepidimous", since: 900, lastLooked: 960 },
      W16S25: { since: 800, lastLooked: 800 },
      W17S24: "Trepidimous",
    },
  };

  assert.equal(stable(await roomsThrough(leaf)), stable(leaf));
});

wire("rooms: #275's null latch costs its own entry; a latch off its shape costs the latch and keeps the owner", async () => {
  const written = await roomsThrough({
    ...emptyRooms,
    rivals: {
      W1N3: null,
      W1N4: { since: 100, lastLooked: 150 },
      W1N5: { o: "Trepidimous", since: "soon" },
      W1N6: { since: 100 },
    },
  });

  assert.equal(
    stable(written.rivals),
    stable({
      W1N4: { since: 100, lastLooked: 150 },
      W1N5: "Trepidimous",
      W1N6: { since: 100, lastLooked: 100 },
    }),
  );
});


// ---- the scoring row ----------------------------------------------------

// `decodeCandidate` and `readNumbers` are reached only through a `scoring`
// verdict, which the verbose channel writes: a row per task the matcher
// weighed, each carrying a rank, a cost and a load, or a rejection reason with
// its own numbers. Nothing in Core sees this leaf, and the operator reads it
// to answer "why did this body take that task" — a rank that decodes to 0 is
// an answer, and the wrong one.

const scoringThrough = async (candidates) => {
  const { load, save } = await logCodec();
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
    stable({
      target: 0,
      living: 0,
      casting: 0,
      rows: [],
      load: 0,
      haul: [],
      overflow: { fill: 0, owed: false },
      asks: [],
    }),
  );
});

wire("quotas: the haul overflow (#508) writes its fill and whether the extra body is owed", async () => {
  const { saveQuotas } = await import(MODULE);
  const { QuotasModule_silent, Quotas } = await import("../build/fable/Core/Types/Verdicts.js");
  const { HaulOverflow } = await import("../build/fable/Core/Types/Colonies.js");
  const s = QuotasModule_silent;
  const quotas = new Quotas(
    s.Target,
    s.Living,
    s.Casting,
    s.Rows,
    s.HaulerLoad,
    s.HaulerDemand,
    new HaulOverflow(0.8125, true),
    s.Busy,
    s.Casts,
  );

  globalThis.Memory = memoryWith("quotas", undefined);
  saveQuotas(home, quotas);

  assert.equal(
    stable(globalThis.Memory.fabot.observe.colonies[home].quotas.overflow),
    stable({ fill: 0.8125, owed: true }),
  );
});

wire("quotas: the asks (#580) write each cast's task, row, bodies and blocks", async () => {
  const { saveQuotas } = await import(MODULE);
  const { QuotasModule_silent, Quotas, Cast, CastAsk } = await import("../build/fable/Core/Types/Verdicts.js");
  const { CastRow, SquadRole } = await import("../build/fable/Core/Types/Tasks.js");
  // Beside the exit camp's work since #663 S7d.
  const { BodyTemplate } = await import("../build/fable/Core/Types/Views.js");
  const { Task } = await import("../build/fable/Core/Types/Vocabulary.js");
  const { ofArray } = await import(LIST);
  const s = QuotasModule_silent;
  const casts = ofArray([
    new Cast(new Task(12, ["W1N2"]), new CastAsk(new CastRow(0, [BodyTemplate.Ranged]), 2, 3)),
    new Cast(new Task(13, ["W1N1"]), new CastAsk(CastRow.Defender, 1, 1)),
    new Cast(new Task(14, ["W2N2"]), new CastAsk(new CastRow(2, [new SquadRole(2, [])]), 1, 2)),
  ]);
  const quotas = new Quotas(
    s.Target,
    s.Living,
    s.Casting,
    s.Rows,
    s.HaulerLoad,
    s.HaulerDemand,
    s.Overflow,
    s.Busy,
    casts,
  );

  globalThis.Memory = memoryWith("quotas", undefined);
  saveQuotas(home, quotas);

  assert.equal(
    stable(globalThis.Memory.fabot.observe.colonies[home].quotas.asks),
    stable([
      { task: "guard:W1N2", row: "ranger", wanted: 2, blocks: 3 },
      { task: "defend:W1N1", row: "defender", wanted: 1, blocks: 1 },
      { task: "assault:W2N2", row: "medic", wanted: 1, blocks: 2 },
    ]),
  );
});

wire("layout: six empty channels write six empty arrays", async () => {
  const { saveLayout } = await import(MODULE);
  const { empty } = await import(LIST);

  globalThis.Memory = memoryWith("layout", undefined);
  saveLayout(home, empty(), empty(), empty(), empty(), empty(), empty());

  const layout = globalThis.Memory.fabot.observe.colonies[home].layout;
  assert.deepEqual(
    Object.keys(layout).sort(),
    ["deferred", "refused", "roads", "unrouted", "unserved", "waiting"],
    "the channels ADR 0028 keeps side by side, the outpost waits (#247) and the outpost roads (#548) beside them",
  );
  assert.ok(Object.values(layout).every((list) => Array.isArray(list) && list.length === 0));
});

wire("layout: a waiting outpost source (#247) writes its room and whose site holds each Seat", async () => {
  const { saveLayout } = await import(MODULE);
  const { WaitingContainer } = await import("../build/fable/Core/Types/Intents.js");
  const { Pos } = await import("../build/fable/Core/Types/Geometry.js");
  const { empty, ofArray } = await import(LIST);
  const waiting = new WaitingContainer(
    "src-1",
    "W1N2",
    ofArray([new Pos(11, 43)]),
    ofArray([new Pos(10, 45)]),
  );

  globalThis.Memory = memoryWith("layout", undefined);
  saveLayout(home, empty(), empty(), empty(), empty(), ofArray([waiting]), empty());

  assert.equal(
    stable(globalThis.Memory.fabot.observe.colonies[home].layout.waiting),
    stable([
      {
        source: "src-1",
        room: "W1N2",
        ours: [{ x: 11, y: 43 }],
        rival: [{ x: 10, y: 45 }],
      },
    ]),
  );
});

wire("layout: an outpost road (#548) writes its state and its tiles' counts", async () => {
  const { saveLayout } = await import(MODULE);
  const { OutpostRoad, OutpostRoadState } = await import("../build/fable/Core/Types/Intents.js");
  const { empty, ofArray } = await import(LIST);
  // Tag 0 is `Planned`, the first case.
  const road = new OutpostRoad("src-1", "W1N2", new OutpostRoadState(0, []), 40, 12, 10, 6);

  globalThis.Memory = memoryWith("layout", undefined);
  saveLayout(home, empty(), empty(), empty(), empty(), empty(), ofArray([road]));

  assert.equal(
    stable(globalThis.Memory.fabot.observe.colonies[home].layout.roads),
    stable([
      {
        source: "src-1",
        room: "W1N2",
        state: "planned",
        tiles: 40,
        standing: 12,
        pending: 10,
        skipped: 6,
      },
    ]),
  );
});

// ---- the operator-only logs' segments (#639) ----------------------------

const { SEGMENTS, SEGMENT_EVERY, joinSegments, readLog } = await import("./observe-segments.mjs");

// The server as `readLog` meets it: a Memory leaf at `memory` (undefined
// when absent) and segment contents by id; `null` makes a read fail.
const serverWith = (memory, segments) => ({
  segmentReads: 0,
  async userMemoryGet() {
    return memory === null ? { ok: 0, error: "down" } : { ok: 1, data: memory };
  },
  async userMemorySegmentGet(ids) {
    this.segmentReads += 1;
    if (segments === null) return { ok: 0, error: "down" };
    const data = ids.map((id) => segments[id] ?? null);
    return { ok: 1, data: data.length === 1 ? data[0] : data };
  },
});

wire("segments: the operator scripts read the Memory leaf first, else the segments", async () => {
  const log = { w1: { log: [], lastMove: [] } };
  const text = JSON.stringify(log);
  const cut = { 0: text.slice(0, 5), 1: text.slice(5), 2: "", 3: "" };

  const older = serverWith(log, cut);
  assert.deepEqual(await readLog(older, "shard", "creeps"), log, "an older bundle's leaf wins");
  assert.equal(older.segmentReads, 0, "and the segments are not read");

  assert.deepEqual(await readLog(serverWith(undefined, cut), "shard", "creeps"), log, "else the segments, joined");
  assert.equal(await readLog(serverWith(undefined, {}), "shard", "creeps"), undefined, "neither: no log");

  const { gzipSync } = await import("node:zlib");
  const gz = `gz:${gzipSync(Buffer.from(text)).toString("base64")}`;
  assert.deepEqual(await readLog(serverWith(gz, {}), "shard", "creeps"), log, "a gzip'd leaf is read");

  await assert.rejects(readLog(serverWith(undefined, null), "shard", "creeps"), /segment read failed/);
  await assert.rejects(readLog(serverWith(null, cut), "shard", "creeps"), /memory read failed/);
});

// The engine's RawMemory as the bot meets it: `active` ids readable this
// tick (an unwritten one reads ""), and the ids last requested.
const rawMemoryWith = (segments) => {
  const raw = {
    segments: { ...segments },
    asked: undefined,
    setActiveSegments(ids) {
      raw.asked = ids;
    },
  };
  globalThis.RawMemory = raw;
  return raw;
};
const allSegments = () => [...SEGMENTS.creeps, ...SEGMENTS.cpu, ...SEGMENTS.rivals, ...SEGMENTS.ledger];
const allEmpty = () => Object.fromEntries(allSegments().map((id) => [id, ""]));

// A Transition log of `creeps` creeps, `entries` rows each, as the wire holds it.
const bigLog = (creeps, entries) =>
  Object.fromEntries(
    Array.from({ length: creeps }, (_, i) => [
      `creep-${i}`,
      {
        log: Array.from({ length: entries }, (_, j) => ({ t: 1000 + j, v: { kind: "kept", task: `haul-${i}-${j}` } })),
        lastMove: [],
      },
    ]),
  );

wire("segments: the ids and the cadence are the ones observe.mjs reads", async () => {
  const { creepSegments, cpuSegments, rivalSegments, ledgerSegments, segmentEvery } = await import(MODULE);

  assert.deepEqual(Array.from(creepSegments), SEGMENTS.creeps);
  assert.deepEqual(Array.from(cpuSegments), SEGMENTS.cpu);
  assert.deepEqual(Array.from(rivalSegments), SEGMENTS.rivals);
  assert.deepEqual(Array.from(ledgerSegments), SEGMENTS.ledger);
  assert.equal(segmentEvery, SEGMENT_EVERY);
  assert.ok(allSegments().length <= 10, "never more than the engine's ten a tick");
  assert.equal(new Set(allSegments()).size, allSegments().length, "no segment two logs'");
});

wire("segments: a cold heap reads the legacy Memory leaf once and deletes it", async () => {
  const { restoreCpu, encodeCpu, saveCpu, flushSegments } = await import(MODULE);
  const sample = {
    ticks: [{ t: 10, ms: 1.5, entry: 0.1, snapshot: 0.2, decide: 0.3, save: 0.4, execute: 0.5, intents: 3, bucket: 10000, replans: 0 }],
    spans: [],
  };

  const raw = rawMemoryWith({});
  globalThis.Memory = rootMemoryWith("cpu", sample);
  const restored = restoreCpu();
  assert.equal(stable(encodeCpu(restored)), stable(sample), "the leaf's line is the heap's");
  assert.equal(Object.hasOwn(globalThis.Memory.fabot.observe, "cpu"), false, "and the leaf is gone");

  // Written out the same tick, off the cadence: a reset before the next
  // write tick finds the line in its segments.
  saveCpu(restored);
  flushSegments(SEGMENT_EVERY * 7 + 3);
  assert.equal(stable(joinSegments(SEGMENTS.cpu.map((id) => raw.segments[id]))), stable(sample));
  raw.segments = allEmpty();
  flushSegments(SEGMENT_EVERY * 7 + 4);
  assert.equal(raw.segments[SEGMENTS.cpu[0]], "", "and once only: the next tick is off the cadence again");
});

wire("segments: a cut never splits a surrogate pair, and every chunk is within the cap", async () => {
  const { segmentChunks } = await import(MODULE);
  const cap = 100 * 1024;

  // A pair straddling the first cut: its high half is the cap's last unit.
  const text = "a".repeat(cap - 1) + "\u{1F600}" + "b".repeat(10);
  const chunks = segmentChunks(2, text);
  assert.equal(chunks[0].length, cap - 1, "the cut moves one unit earlier");
  assert.equal(chunks[1].codePointAt(0), 0x1f600, "and the pair opens the next chunk whole");
  assert.equal(chunks.join(""), text);

  assert.equal(segmentChunks(1, "a".repeat(cap + 1)), undefined, "past the room: none");
  assert.deepEqual(segmentChunks(3, "xy"), ["xy", "", ""], "the unused chunks are empty");
});

wire("segments: not active, a cold heap asks for them and starts from nothing", async () => {
  const { restoreLog, encodeLog } = await import(MODULE);

  const raw = rawMemoryWith({ 0: "" });
  globalThis.Memory = {};
  assert.equal(restoreLog(), undefined, "None: not an empty log, which would be written over the stored one");
  assert.ok(Array.isArray(raw.asked), "a plain array: the engine refuses a typed one");
  assert.deepEqual(raw.asked, allSegments());

  // Active and never written: an empty log, and nothing asked.
  const fresh = rawMemoryWith(allEmpty());
  const restored = restoreLog();
  assert.notEqual(restored, undefined, "restored");
  assert.equal(stable(encodeLog(restored)), stable({}), "as an empty log");
  assert.equal(fresh.asked, undefined);
});

wire("segments: a log is cut across its segments under the cap and read back whole", async () => {
  const { decodeLog, encodeLog, save, restoreLog, flushSegments } = await import(MODULE);
  const cap = 100 * 1024;
  const wireLog = encodeLog(decodeLog(bigLog(120, 40)));
  assert.ok(JSON.stringify(wireLog).length > cap, "the fixture needs more than one segment");

  globalThis.Memory = {};
  const raw = rawMemoryWith(allEmpty());
  save(decodeLog(wireLog));
  flushSegments(SEGMENT_EVERY + 1);
  assert.equal(raw.segments[0], "", "not a write tick: nothing written");

  flushSegments(SEGMENT_EVERY * 3);
  const chunks = SEGMENTS.creeps.map((id) => raw.segments[id]);
  assert.ok(chunks.every((chunk) => typeof chunk === "string" && chunk.length <= cap), "every chunk within the cap");
  assert.ok(chunks.filter((chunk) => chunk.length > 0).length >= 2, "and the log spans more than one");
  assert.equal(stable(joinSegments(chunks)), stable(wireLog), "observe.mjs reads the log whole");
  assert.equal(raw.asked, undefined, "active segments are not asked for again");

  // A cold heap over those segments restores the same log.
  rawMemoryWith(raw.segments);
  assert.equal(stable(encodeLog(restoreLog())), stable(wireLog), "the bot reads it back whole");
});

wire("segments: a log past its segments' room is not written, and is one fault a heap", async () => {
  const { decodeLog, save, flushSegments } = await import(MODULE);

  globalThis.Memory = {};
  const raw = rawMemoryWith(allEmpty());
  save(decodeLog(bigLog(400, 50)));
  const rows = [...flushSegments(SEGMENT_EVERY * 5)];
  assert.deepEqual(
    SEGMENTS.creeps.map((id) => raw.segments[id]),
    SEGMENTS.creeps.map(() => ""),
    "the segments keep what they held",
  );
  assert.equal(rows.length, 1, "one fault, for the shell's ring, and nothing threw");
  assert.equal(rows[0].Phase, "segments");
  assert.equal(rows[0].Tick, SEGMENT_EVERY * 5);
  assert.equal([...flushSegments(SEGMENT_EVERY * 6)].length, 0, "and not again on this heap");
  // The heap wire is the module's: leave it small for any case after this.
  save(decodeLog({}));
});

// ---- the rival dossier (#653) -------------------------------------------

// One room of a rival's dossier, every key written.
const rivalRoom = (n = 1) => ({
  seen: 1000 * n,
  res: true,
  tReads: 4 * n,
  tLow: 120,
  tLast: 1900,
  refills: [2, 990],
  fireGain: 15000.5,
  fireTicks: 300,
  peaceGain: 800,
  peaceTicks: 1200,
  lowFirst: [3, 2],
  resp: [2, 30, 12, 18, 950],
  unans: [1, 700],
  standing: [1, 640],
  killed: 0,
  unworked: [1, 45, 45, 45, 500],
  lapsed: 980,
  rereserve: [1, 45, 45, 45, 500],
});

const activation = {
  room: "W20S20",
  at: 100,
  noticed: 110,
  rcl: 6,
  ours: 3,
  armed: 2,
  since: 20,
  struck: { spawn: 5, rampart: 1 },
};

const dossier = {
  since: 10,
  latest: 1000,
  aim: { shots: [5, 999], alone: 1, near: [4, 1], weak: [4, 3], heal: [2, 0], armed: [3, 3], edge: [1, 0] },
  safe: [1, 100],
  acts: [activation],
  cool: 70100,
  armed: [6, 998],
  shapes: { "2A2M": 4, "10R5H15M": 2 },
  cross: [3, 997],
  sortie: [1, 997],
  stayed: [1, 960],
  fled: [2, 940],
  fell: [1, 930],
  homeRaid: { 4: [2, 500] },
  outRaid: [1, 600],
  rooms: { W20S20: rivalRoom(1), W21S20: rivalRoom(2) },
};

const rivalsThrough = async (wire) => {
  const { decodeRivals, encodeRivals } = await import(MODULE);
  return encodeRivals(decodeRivals(wire));
};

wire("rivals: a well-formed dossier round-trips unchanged", async () => {
  const full = { Trepidimous: dossier, giaco: { ...dossier, acts: [{ ...activation, since: undefined }] } };
  const back = await rivalsThrough(JSON.parse(JSON.stringify(full)));
  assert.equal(stable(back), stable(JSON.parse(JSON.stringify(full))));
  assert.equal(Object.hasOwn(back.giaco.acts[0], "since"), false, "a safe mode with no visit before it has no since");
});

for (const [name, leaf] of [
  ["absent", undefined],
  ["null", null],
  ["a string", "Incorrect memory path"],
  ["an array", [dossier]],
]) {
  wire(`rivals: a wire that is ${name} reads empty`, async () => {
    assert.equal(stable(await rivalsThrough(leaf)), stable({}));
  });
}

wire("rivals: a dossier off its shape costs that rival alone, and an absent key reads as zero", async () => {
  const back = await rivalsThrough({
    good: dossier,
    badTally: { ...dossier, armed: [1] },
    badRoom: { ...dossier, rooms: { W1N1: { ...rivalRoom(), res: "yes" } } },
    noSince: { ...dossier, since: "10" },
    sparse: { since: 5, latest: 6, rooms: { W2N2: { seen: 6 } } },
  });
  assert.deepEqual(Object.keys(back).sort(), ["good", "sparse"]);
  assert.deepEqual(back.sparse.armed, [0, 0], "a tally absent is none");
  assert.deepEqual(back.sparse.aim.near, [0, 0], "an aim absent is none");
  assert.deepEqual(back.sparse.rooms.W2N2.resp, [0, 0, 0, 0, 0], "a span absent is none");
  assert.equal(back.sparse.rooms.W2N2.res, false);
});

wire("rivals: the dossier at its caps fits its segment", async () => {
  const { capRivals, capRivalRooms, capShapes, capActivations } = await import("../build/fable/Core/Observe.js");
  const big = 9_999_999;
  const room = Object.fromEntries(
    Object.entries(rivalRoom()).map(([key, value]) => [
      key,
      Array.isArray(value) ? value.map(() => big) : typeof value === "number" ? big + 0.5 : value,
    ]),
  );
  const full = {
    ...dossier,
    since: big,
    latest: big,
    acts: Array.from({ length: capActivations }, (_, i) => ({
      ...activation,
      room: `W${100 + i}S100`,
      at: big,
      noticed: big,
      struck: Object.fromEntries(["spawn", "storage", "tower", "rampart", "extension", "terminal", "lab", "link"].map((k) => [k, big])),
    })),
    shapes: Object.fromEntries(Array.from({ length: capShapes }, (_, i) => [`${i}T10A10R10H10W10C10L25M`, big])),
    homeRaid: Object.fromEntries([1, 2, 3, 4, 5, 6, 7, 8].map((level) => [level, [big, big]])),
    rooms: Object.fromEntries(Array.from({ length: capRivalRooms }, (_, i) => [`W${100 + i}S${100 + i}`, room])),
  };
  const all = Object.fromEntries(Array.from({ length: capRivals }, (_, i) => [`a-rival-named-${i}-long`, full]));
  const text = JSON.stringify(await rivalsThrough(all));
  assert.ok(text.length <= 100 * 1024 * SEGMENTS.rivals.length, `${text.length} past its segments`);
});

wire("rivals: written to its segment on the cadence, and a cold heap reads it back", async () => {
  const { restoreRivals, saveRivals, flushSegments, encodeRivals } = await import(MODULE);
  const wireRivals = { Trepidimous: dossier };

  globalThis.Memory = {};
  const raw = rawMemoryWith({ ...allEmpty(), [SEGMENTS.rivals[0]]: JSON.stringify(wireRivals) });
  const restored = restoreRivals();
  assert.notEqual(restored, undefined, "restored");
  assert.equal(stable(encodeRivals(restored.Dossiers)), stable(wireRivals), "the segment's dossier is the heap's");

  saveRivals(restored);
  raw.segments[SEGMENTS.rivals[0]] = "";
  flushSegments(SEGMENT_EVERY * 9 + 1);
  assert.equal(raw.segments[SEGMENTS.rivals[0]], "", "not a write tick: nothing written");
  flushSegments(SEGMENT_EVERY * 9);
  assert.equal(stable(joinSegments(SEGMENTS.rivals.map((id) => raw.segments[id]))), stable(wireRivals));
});

// ---- the operation ledger (#662) ----------------------------------------

// One open row, every key written: the fold's own state beside the counts.
const ledgerRow = {
  k: "sweep",
  r: "W19S26",
  s: 1000,
  l: 1200,
  act: 1198,
  lk: 1196,
  p: { rate: 12.5, value: 3.25, odds: 0.62 },
  cast: [2, 1400, 90],
  join: 1,
  lostCast: [1, 700, 350.5],
  lostLent: [1, 650, 120.25],
  forced: [3, 31.5],
  kills: [2, 1500.25],
  camp: [1, 800.5],
  down: [40, 400],
  ans: [1, 1300],
  resp: 60,
  roster: { "ranger-1-Spawn1": [700, 1200, 1200, 1, 1], "ranger-2-Spawn1": [700, 0, 1100, 0, 0] },
  rfloor: 450,
  marks: { digger: 300.5 },
  camped: ["dancer"],
  answers: { defender: [1100, 1300] },
  afloor: 1050,
};

// A closed row: no odds, no response, none of the fold's state.
const closedRow = {
  k: "hold",
  r: "W12S27",
  s: 900,
  l: 950,
  act: 950,
  lk: 950,
  e: 950,
  p: { rate: 0, value: 10 },
  cast: [0, 0, 0],
  join: 1,
  lostCast: [0, 0, 0],
  lostLent: [0, 0, 0],
  forced: [0, 0],
  kills: [1, 600],
  camp: [0, 0],
  down: [0, 0],
  ans: [0, 0],
};

const ledgerThrough = async (wire) => {
  const { decodeLedger, encodeLedger } = await import(MODULE);
  return encodeLedger(decodeLedger(wire));
};

wire("ledger: a well-formed ledger round-trips unchanged", async () => {
  const full = {
    open: [ledgerRow],
    closed: [closedRow, { ...closedRow, k: "camp", s: 800, e: 850, l: 850, act: 850, lk: 850 }],
    evicted: 3,
  };
  const back = await ledgerThrough(JSON.parse(JSON.stringify(full)));
  assert.equal(stable(back), stable(full));
  assert.equal(Object.hasOwn(back.closed[0], "resp"), false, "a row never answered has no response");
  assert.equal(Object.hasOwn(back.closed[0].p, "odds"), false, "and a row no camp priced no odds");
  assert.equal(Object.hasOwn(back.closed[0], "afloor"), false, "and no floor where none rose");
});

// #663 S7d: an exit camp's row carries the Task it was opened under (`op`),
// what its record's spend is read by; absent or off its shape, none.
wire("ledger: an exit camp's row keeps its Task; absent or off its shape it has none", async () => {
  const camp = { ...closedRow, k: "camp", op: "op:camp@W19S26" };
  const back = await ledgerThrough({ open: [], closed: [camp, { ...closedRow, op: 7 }] });
  assert.equal(back.closed[0].op, "op:camp@W19S26");
  assert.equal(Object.hasOwn(back.closed[1], "op"), false, "a number is no Task");
  assert.equal(Object.hasOwn((await ledgerThrough({ open: [], closed: [closedRow] })).closed[0], "op"), false);
});

wire("ledger: a row's later keys absent read as its last fold's tick, and as none", async () => {
  const { act, lk, lostLent, camp, camped, answers, afloor, rfloor, ...bare } = ledgerRow;
  const back = await ledgerThrough({ open: [bare], closed: [] });
  assert.equal(back.open[0].act, bare.l, "seen or held last on its last fold");
  assert.equal(back.open[0].lk, bare.l, "looked at last on its last fold");
  assert.deepEqual(back.open[0].lostLent, [0, 0, 0]);
  assert.deepEqual(back.open[0].camp, [0, 0]);
  assert.equal(Object.hasOwn(back.open[0], "answers"), false);
  assert.equal(Object.hasOwn(back.open[0], "rfloor"), false);
  assert.equal(back.evicted, 0, "and a ledger with no evictions counted, none");
});

for (const [name, leaf] of [
  ["absent", undefined],
  ["null", null],
  ["a string", "Incorrect memory path"],
  ["an array", [ledgerRow]],
]) {
  wire(`ledger: a wire that is ${name} reads empty`, async () => {
    assert.equal(stable(await ledgerThrough(leaf)), stable({ open: [], closed: [], evicted: 0 }));
  });
}

wire("ledger: a row off its shape costs that row alone, and an absent count reads as zero", async () => {
  const back = await ledgerThrough({
    open: [
      ledgerRow,
      { ...ledgerRow, k: "siege" },
      { ...ledgerRow, cast: [1, 2] },
      { ...ledgerRow, s: "1000" },
      { ...ledgerRow, p: undefined },
      { k: "trespass", r: "W1N1", s: 5, l: 6, p: { rate: 1, value: 2 } },
    ],
    closed: [null, closedRow],
  });
  assert.deepEqual(back.open.map((row) => row.k), ["sweep", "trespass"]);
  assert.deepEqual(back.open[1].cast, [0, 0, 0], "counts absent are none");
  assert.equal(back.open[1].join, 0);
  assert.deepEqual(back.closed.map((row) => row.k), ["hold"]);
});

wire("ledger: a roster entry, a mark, a camped id or an answer off its shape costs itself", async () => {
  const back = await ledgerThrough({
    open: [
      {
        ...ledgerRow,
        roster: { good: [1, 2, 3, 1, 0], short: [1, 2], text: ["1", 2, 3, 1, 0] },
        marks: { good: 5, bad: "5" },
        camped: ["good", 7],
        answers: { good: [1, 2], short: [1], text: ["1", 2] },
      },
    ],
    closed: [],
  });
  assert.deepEqual(Object.keys(back.open[0].roster), ["good"]);
  assert.deepEqual(Object.keys(back.open[0].marks), ["good"]);
  assert.deepEqual(back.open[0].camped, ["good"]);
  assert.deepEqual(Object.keys(back.open[0].answers), ["good"]);
});

wire("ledger: a price the decide did not read is absent, and one off its shape costs the row", async () => {
  const back = await ledgerThrough({
    open: [{ ...ledgerRow, p: {} }, { ...ledgerRow, p: { rate: "x" } }, { ...ledgerRow, p: { value: null } }],
    closed: [],
  });
  assert.equal(back.open.length, 1);
  assert.deepEqual(back.open[0].p, {}, "no rate, value nor odds");
});

wire("ledger: a later count off its shape, or a floor not a number, costs the row", async () => {
  const back = await ledgerThrough({
    open: [
      ledgerRow,
      { ...ledgerRow, lostLent: [1] },
      { ...ledgerRow, camp: "x" },
      { ...ledgerRow, act: "1" },
      { ...ledgerRow, afloor: "x" },
      { ...ledgerRow, rfloor: null },
    ],
    closed: [],
  });
  assert.equal(back.open.length, 1);
});

wire("ledger: the ledger at its caps fits its segment", async () => {
  const { capOpen, capClosed, capRoster, capMarks } = await import("../build/fable/Core/Decide/Ledger.js");
  const big = 9_999_999;
  const row = (i) => ({
    ...ledgerRow,
    r: `W${100 + i}S${100 + i}`,
    s: big,
    l: big,
    e: big,
    act: big,
    lk: big,
    p: { rate: big + 0.25, value: big + 0.25, odds: 0.99 },
    cast: [big, big, big],
    join: big,
    lostCast: [big, big, big + 0.25],
    lostLent: [big, big, big + 0.25],
    forced: [big, big + 0.25],
    kills: [big, big + 0.25],
    camp: [big, big + 0.25],
    down: [big, big + 0.25],
    ans: [big, big],
    resp: big,
    // The roster never past its cap: past it, holders go too, under the
    // floor (`Ledger.fold`'s muster).
    roster: Object.fromEntries(
      Array.from({ length: capRoster }, (_, j) => [`ranger-${big}-Spawn${j}`, [big, 1500, big, 1, 1]]),
    ),
    rfloor: big,
    marks: Object.fromEntries(Array.from({ length: capMarks }, (_, j) => [`5a${j}`.padEnd(24, "f"), big + 0.25])),
    // In id order, as a set writes them.
    camped: Array.from({ length: capMarks }, (_, j) => `5c${String(j).padStart(2, "0")}`.padEnd(24, "f")),
    answers: Object.fromEntries(Array.from({ length: capMarks }, (_, j) => [`5b${j}`.padEnd(24, "f"), [big, big]])),
    afloor: big,
  });
  // A closed row keeps no marks nor camped bodies (`Ledger.fold`), and
  // through the window its roster and answers.
  const full = {
    open: Array.from({ length: capOpen }, (_, i) => row(i)),
    closed: Array.from({ length: capClosed }, (_, i) => {
      const { marks, camped, ...closed } = row(capOpen + i);
      return closed;
    }),
    evicted: big,
  };
  const back = await ledgerThrough(full);
  assert.equal(back.open.length, capOpen);
  assert.equal(back.closed.length, capClosed);
  assert.equal(stable(back), stable(full), "nothing dropped: this is the worst case the bot writes");
  const text = JSON.stringify(back);
  assert.ok(text.length <= 100 * 1024 * SEGMENTS.ledger.length, `${text.length} past its segment`);
});

wire("ledger: written to its segment on the cadence, and a cold heap reads it back", async () => {
  const { restoreLedger, saveLedger, flushSegments, encodeLedger } = await import(MODULE);
  const wireLedger = { open: [ledgerRow], closed: [closedRow], evicted: 2 };

  globalThis.Memory = {};
  const raw = rawMemoryWith({ ...allEmpty(), [SEGMENTS.ledger[0]]: JSON.stringify(wireLedger) });
  const restored = restoreLedger();
  assert.notEqual(restored, undefined, "restored");
  assert.equal(stable(encodeLedger(restored)), stable(wireLedger), "the segment's ledger is the heap's");

  saveLedger(restored);
  raw.segments[SEGMENTS.ledger[0]] = "";
  flushSegments(SEGMENT_EVERY * 11 + 1);
  assert.equal(raw.segments[SEGMENTS.ledger[0]], "", "not a write tick: nothing written");
  flushSegments(SEGMENT_EVERY * 11);
  assert.equal(stable(joinSegments(SEGMENTS.ledger.map((id) => raw.segments[id]))), stable(wireLedger));
});

// ---- the operations (#663 S7a) --------------------------------------------

// `Memory.fabot.ops`: absent or no object is no operations, never a fault,
// and nothing is written; the bot writes `live`, `refused` and `events`
// back onto a leaf it read, and leaves the operator's `specs` and `off`.
// Load, then save the records it read with no events, then compare. A leaf
// whose switch is set is read as off, and the shell writes nothing onto it
// (`Main.operationsDryRun`), which `opsThrough` mirrors.
const opsThrough = async (ops, events = []) => {
  const { loadOps, saveOps } = await import(MODULE);
  const { ofArray, empty } = await import(LIST);
  globalThis.Memory = ops === undefined ? { fabot: {} } : { fabot: { ops } };
  const leaf = loadOps();
  if (leaf != null && !leaf.Off) saveOps(77, leaf.Live, leaf.Garbled, ofArray(events), empty());
  return { leaf, ops: globalThis.Memory.fabot.ops };
};

const opRecord = { i: "op1", s: 100, p: "execute", a: 120, e: 0, l: 0, n: 0 };
const opSpec = { id: "op1", template: "sweep", enemy: "Trepidimous", rooms: ["W19S26"], deadline: 6000 };

for (const [name, leaf] of [
  ["absent", undefined],
  ["null", null],
  ["a string", "Incorrect memory path"],
  ["a number", 7],
  ["true", true],
  ["an array", [{ id: "op1" }]],
]) {
  wire(`ops: a leaf that is ${name} is no operations and is left as it was`, async () => {
    const { leaf: read, ops } = await opsThrough(leaf);
    assert.equal(read, undefined, "no operations");
    assert.equal(stable(ops), stable(leaf));
  });
}

wire("ops: legacy Memory with every other leaf and no ops leaf gains none", async () => {
  const { loadOps, saveOps } = await import(MODULE);
  const { empty } = await import(LIST);
  globalThis.Memory = { fabot: { assignments: { c1: "guard:W1N1" }, observe: { cadence: 3 } } };
  const before = stable(globalThis.Memory);
  assert.equal(loadOps(), undefined);
  saveOps(77, empty(), empty(), empty(), empty());
  assert.equal(stable(globalThis.Memory), before, "untouched");
});

wire("ops: an empty leaf reads as nothing and writes empty lists", async () => {
  const { leaf, ops } = await opsThrough({});
  assert.equal(leaf.Off, false);
  assert.equal(stable(ops), stable({ events: [], live: [], refused: [] }));
});

wire("ops: a well-formed leaf round-trips, the operator's keys untouched", async () => {
  const full = {
    specs: [opSpec],
    off: false,
    live: [opRecord, { ...opRecord, i: "op2", x: "expired" }],
    refused: [{ id: "old", why: "stale" }],
    events: [{ t: 50, i: "op1", k: "began", p: "execute" }],
  };
  const { leaf, ops } = await opsThrough(structuredClone(full));
  assert.equal(leaf.Off, false);
  // Refusals are this tick's, rewritten each tick.
  assert.equal(stable(ops), stable({ ...full, refused: [] }));
});

// Fails closed: any value JavaScript reads as true is off, and an off leaf
// is left exactly as it stood. Before review, `"yes"` read on (a boolean
// alone counted); it reads off now.
for (const [name, off, reads] of [
  ["true", true, true],
  ['"yes"', "yes", true],
  ["1", 1, true],
  ["an object", {}, true],
  ["false", false, false],
  ["0", 0, false],
  ['""', "", false],
  ["null", null, false],
]) {
  wire(`ops: a switch that is ${name} reads ${reads ? "off, and nothing is written" : "on"}`, async () => {
    const stood = { off, specs: [opSpec], live: [opRecord] };
    const { leaf, ops } = await opsThrough(structuredClone(stood));
    assert.equal(leaf.Off, reads);
    if (reads) assert.equal(stable(ops), stable(stood), "untouched");
    else assert.equal(stable(ops.live), stable([opRecord]));
  });
}

wire("ops: a garbled spec row is refused as garbled, by its id where it has one", async () => {
  const { leaf, ops } = await opsThrough({
    specs: [
      opSpec,
      { id: "bad", template: "sweep", enemy: "Trepidimous", rooms: "W1N1" },
      7,
      { ...opSpec, id: "nodl", deadline: "soon" },
      { ...opSpec, id: "frac", deadline: 6000.5 },
      { ...opSpec, id: "huge", deadline: 1e20 },
      { ...opSpec, id: "budget", maxEnergy: 2 ** 31 },
    ],
  });
  assert.equal(
    stable(ops.refused),
    stable([
      { id: "bad", why: "garbled" },
      { id: "?", why: "garbled" },
      { id: "nodl", why: "garbled" },
      { id: "frac", why: "garbled" },
      { id: "huge", why: "garbled" },
      { id: "budget", why: "garbled" },
    ]),
  );
  assert.equal([...leaf.Specs].length, 1, "the one whole spec");
});

wire("ops: a spec's whole numbers within 32 bits read as themselves", async () => {
  const { leaf } = await opsThrough({
    specs: [{ ...opSpec, deadline: 2 ** 31 - 1, maxEnergy: 0, maxBodies: -(2 ** 31 - 1) }],
  });
  const [spec] = leaf.Specs;
  assert.deepEqual([spec.Deadline, spec.MaxEnergy, spec.MaxBodies], [2 ** 31 - 1, 0, -(2 ** 31 - 1)]);
});

wire("ops: a record off its shape costs its own row", async () => {
  const { ops } = await opsThrough({
    live: [opRecord, { ...opRecord, p: "dance" }, { ...opRecord, s: {} }, { ...opRecord, x: "won" }, null, { ...opRecord, i: "" }],
  });
  assert.equal(stable(ops.live), stable([opRecord]));
});

// The room a record is committed to (#663 S7c), `r`: a room name
// round-trips; a record written before it (no `r`) or one whose `r` is off
// its shape reads with no room, its row kept, and is written back without
// one: the next exit chooses the room.
// The tick that room was chosen, `c`: absent or off its shape, the phase's
// entry (`a`), written back beside the room.
wire("ops: a record's committed room round-trips; absent or off its shape it reads as none", async () => {
  const { leaf, ops } = await opsThrough({
    live: [
      { ...opRecord, r: "W19S26", c: 90 },
      { ...opRecord, i: "op2" },
      { ...opRecord, i: "op3", r: 7 },
      { ...opRecord, i: "op4", r: "" },
      { ...opRecord, i: "op5", r: "W18S27", c: "soon" },
    ],
  });
  assert.deepEqual(
    [...leaf.Live].map((record) => [record.Room, record.RoomSince]),
    [
      ["W19S26", 90],
      [undefined, 120],
      [undefined, 120],
      [undefined, 120],
      ["W18S27", 120],
    ],
  );
  assert.equal(
    stable(ops.live),
    stable([
      { ...opRecord, r: "W19S26", c: 90 },
      { ...opRecord, i: "op2" },
      { ...opRecord, i: "op3" },
      { ...opRecord, i: "op4" },
      { ...opRecord, i: "op5", r: "W18S27", c: 120 },
    ]),
  );
});

wire("ops: events append after the kept ones, the newest twenty kept", async () => {
  const prior = Array.from({ length: 25 }, (_, t) => ({ t, i: "op1", k: "recurred", n: t }));
  const { ops } = await opsThrough({ events: [...prior, 7, null] });
  assert.equal(ops.events.length, 20);
  assert.equal(ops.events[0].t, 5, "the oldest fell off");
  assert.equal(ops.events[19].t, 24);
});

wire("ops: each event kind on the wire, stamped with its tick", async () => {
  const { OpEvent, OpPhase, OpEnd } = await import("../build/fable/Core/Types/Operation.js");
  const phase = (tag) => new OpPhase(tag, []);
  const { ops } = await opsThrough({}, [
    new OpEvent(0, ["op1", phase(2), undefined]),
    new OpEvent(1, ["op1", phase(2), phase(3), 1, "W19S26", undefined]),
    new OpEvent(2, ["op1", 300]),
    new OpEvent(3, ["op1", new OpEnd(3, [])]),
    new OpEvent(4, ["op1"]),
  ]);
  assert.equal(
    stable(ops.events),
    stable([
      { t: 77, i: "op1", k: "began", p: "execute" },
      { t: 77, i: "op1", k: "moved", f: "execute", p: "hold", x: 1, r: "W19S26" },
      { t: 77, i: "op1", k: "recurred", n: 300 },
      { t: 77, i: "op1", k: "ended", e: "budget" },
      { t: 77, i: "op1", k: "dropped" },
    ]),
  );
});

// A move of an operation's room carries what decided it: the rate of the
// room it left (`h`), what the room it entered paid (`v`), each to the
// hundredth, and the move's horizon in ticks (`z`).
wire("ops: a room's move carries the numbers that decided it", async () => {
  const { OpEvent, OpPhase, OpMove } = await import("../build/fable/Core/Types/Operation.js");
  const phase = (tag) => new OpPhase(tag, []);
  const { ops } = await opsThrough({}, [
    new OpEvent(1, ["sweep", phase(2), phase(2), 1, "W19S26", new OpMove(7.4012, 19.866, 130)]),
  ]);
  assert.equal(
    stable(ops.events),
    stable([{ t: 77, i: "sweep", k: "moved", f: "execute", p: "execute", x: 1, r: "W19S26", h: 7.4, v: 19.87, z: 130 }]),
  );
});

wire("ops: a defaulted deadline is written into its spec once, never over one given", async () => {
  const { saveOps } = await import(MODULE);
  const { ofArray, empty } = await import(LIST);
  const { ...noDeadline } = opSpec;
  delete noDeadline.deadline;
  globalThis.Memory = { fabot: { ops: { specs: [noDeadline, { ...opSpec, id: "op2" }] } } };
  saveOps(77, empty(), empty(), empty(), ofArray([["op1", 5077], ["op2", 9999]]));
  assert.equal(globalThis.Memory.fabot.ops.specs[0].deadline, 5077);
  assert.equal(globalThis.Memory.fabot.ops.specs[1].deadline, 6000, "a given deadline stands");
});

// ---- the code's operations' latch (#663 S7c) ------------------------------

// `Memory.fabot.ops.code`, the bot's alone: the records of the operations
// code declares (`live`, each a Memory spec's record shape) and this tick's
// refusals of their specs (`refused`); their events join the leaf's
// `events`. Load, save the records read, compare.
const codeThrough = async (ops, refused = []) => {
  const { loadOpsCode, saveOpsCode } = await import(MODULE);
  const { ofArray, empty } = await import(LIST);
  globalThis.Memory = ops === undefined ? { fabot: {} } : { fabot: { ops } };
  const records = loadOpsCode();
  saveOpsCode(77, records, empty(), ofArray(refused));
  return { records: [...records], ops: globalThis.Memory.fabot.ops };
};

const codeRecord = { ...opRecord, i: "sweep", r: "W18S27", c: 110 };

wire("ops code: a well-formed latch round-trips, the operator's keys untouched", async () => {
  const full = {
    specs: [opSpec],
    live: [opRecord],
    code: { live: [codeRecord], refused: [] },
  };
  const { records, ops } = await codeThrough(structuredClone(full));
  assert.deepEqual(
    records.map((record) => [record.Id, record.Room]),
    [["sweep", "W18S27"]],
  );
  assert.equal(stable(ops), stable({ ...full, events: [] }));
});

wire("ops code: this tick's refusals are written beside the latch", async () => {
  const { OpRefusal } = await import("../build/fable/Core/Types/Operation.js");
  const { ops } = await codeThrough({ code: { live: [codeRecord] } }, [
    new OpRefusal("sweep", "ours: W17S25"),
  ]);
  assert.equal(stable(ops.code.refused), stable([{ id: "sweep", why: "ours: W17S25" }]));
});

for (const [name, ops] of [
  ["no code object", {}],
  ["a code that is an array", { code: [codeRecord] }],
  ["a code that is a string", { code: "Incorrect memory path" }],
  ["a code with no live list", { code: { refused: [] } }],
]) {
  wire(`ops code: a leaf with ${name} holds no records, never a fault`, async () => {
    const { records } = await codeThrough(ops);
    assert.deepEqual(records, []);
  });
}

wire("ops code: a record off its shape costs its own row", async () => {
  const { records } = await codeThrough({
    code: { live: [codeRecord, { ...codeRecord, p: "dance" }, null, { ...codeRecord, i: "" }] },
  });
  assert.deepEqual(
    records.map((record) => record.Id),
    ["sweep"],
  );
});

// #663 S7d: an exit camp's record beside the sweep's, in the same shape:
// no room (its exits are worked all at once), its run's spend off the
// ledger, and a run ended abandoned waiting out its period (`n`) to re-arm.
wire("ops code: an exit camp's record round-trips: no room, its run's spend, ended awaiting its next run", async () => {
  const campRecord = { i: "camp", s: 100, p: "retreat", a: 300, e: 3750, l: 3750, n: 1600, x: "abandoned" };
  const full = { code: { live: [codeRecord, campRecord], refused: [] } };
  const { records, ops } = await codeThrough(structuredClone(full));
  assert.deepEqual(
    records.map((record) => [record.Id, record.Room, record.Spent, record.Lost, record.NextDue]),
    [
      ["sweep", "W18S27", 0, 0, 0],
      ["camp", undefined, 3750, 3750, 1600],
    ],
  );
  assert.equal(stable(ops), stable({ ...full, events: [] }));
});

wire("ops code: a Memory with no ops leaf and nothing to write gains none", async () => {
  const before = { fabot: { assignments: { c1: "op:sweep" } } };
  globalThis.Memory = structuredClone(before);
  const { saveOpsCode } = await import(MODULE);
  const { empty } = await import(LIST);
  saveOpsCode(77, empty(), empty(), empty());
  assert.equal(stable(globalThis.Memory), stable(before));
});

wire("ops code: a latch to write makes the ops leaf where none stands", async () => {
  const { OpRecord, OpPhase } = await import("../build/fable/Core/Types/Operation.js");
  const { saveOpsCode, loadOpsCode } = await import(MODULE);
  const { ofArray, empty } = await import(LIST);
  globalThis.Memory = { fabot: {} };
  saveOpsCode(
    77,
    ofArray([new OpRecord("sweep", 100, new OpPhase(2, []), 120, "W18S27", 110, 0, 0, 0, undefined)]),
    empty(),
    empty(),
  );
  assert.equal(
    stable(globalThis.Memory.fabot.ops),
    stable({ code: { live: [codeRecord], refused: [] }, events: [] }),
  );
  assert.deepEqual(
    [...loadOpsCode()].map((record) => record.Room),
    ["W18S27"],
  );
});

wire("ops code: an ops leaf off its shape is the operator's to mend: nothing is written onto it", async () => {
  const { saveOpsCode } = await import(MODULE);
  const { ofArray, empty } = await import(LIST);
  const { OpRecord, OpPhase } = await import("../build/fable/Core/Types/Operation.js");
  globalThis.Memory = { fabot: { ops: "Incorrect memory path" } };
  saveOpsCode(
    77,
    ofArray([new OpRecord("sweep", 100, new OpPhase(2, []), 120, "W18S27", 110, 0, 0, 0, undefined)]),
    empty(),
    empty(),
  );
  assert.equal(globalThis.Memory.fabot.ops, "Incorrect memory path");
});

// ---- the published operations (#663 S7e) ----------------------------------

// A spec row's `live` flag, the operator's `op live`: fails closed, only
// `true` itself is live; the flag is the operator's, never rewritten.
for (const [name, flag, reads] of [
  ["true", true, true],
  ['"true"', "true", false],
  ["1", 1, false],
  ["false", false, false],
  ["absent", undefined, false],
]) {
  wire(`ops: a spec flagged live with ${name} reads ${reads ? "live" : "dry"}, the flag untouched`, async () => {
    const row = flag === undefined ? { ...opSpec } : { ...opSpec, live: flag };
    const { leaf, ops } = await opsThrough({ specs: [row] });
    assert.deepEqual(Array.from(leaf.LiveIds), reads ? ["op1"] : []);
    assert.equal(stable(ops.specs), stable([row]));
  });
}

// `Memory.fabot.ops.readings`: each armed operation's trigger, rewritten
// whole each full tick; none removes the key. Bounded: a reading of two
// exits, landers and all, stays a few hundred bytes.
wire("ops readings: each exit's watch and guards, compact; none removes the key", async () => {
  const { saveOpsReadings } = await import(MODULE);
  const { ofArray, empty } = await import(LIST);
  const { OpReading, ExitReading, Truth } = await import("../build/fable/Core/Types/Operation.js");
  const { HostileInfo } = await import("../build/fable/Core/Types/Sightings.js");
  const { BodyPart } = await import("../build/fable/Core/Types/Vocabulary.js");
  const parts = (n, tag) => Array.from({ length: n }, () => new BodyPart(tag, []));
  const fuse = new HostileInfo("f1", "Trepidimous", { Room: "W18S26", X: 25, Y: 45 },
    ofArray([...parts(18, 2), ...parts(17, 3)]), empty(), 3500, 1500);
  const reading = new OpReading(
    "camp",
    ofArray([
      new ExitReading("W19S26", 340, 200, 3, 2, Truth.Yes, Truth.No, [12.4, 700.6], Truth.Yes, ofArray([fuse])),
      new ExitReading("W18S27", 340, 180, 0, 0, Truth.Unknown, Truth.Unknown, undefined, Truth.Unknown, empty()),
    ]),
    "W19S26: pays 12 against 701; W19S26: loses to 18M17A",
  );
  globalThis.Memory = { fabot: { ops: { specs: [opSpec] } } };
  saveOpsReadings(ofArray([reading]));
  const wireReadings = [
    {
      i: "camp",
      why: "W19S26: pays 12 against 701; W19S26: loses to 18M17A",
      x: [
        { r: "W19S26", age: 340, w: 200, a: 3, b: 2, u: "y", p: "n", l: "y", v: 12, c: 701, by: ["18M17A"] },
        { r: "W18S27", age: 340, w: 180, a: 0, b: 0, u: "?", p: "?", l: "?" },
      ],
    },
  ];
  assert.equal(stable(globalThis.Memory.fabot.ops.readings), stable(wireReadings));
  assert.ok(JSON.stringify(wireReadings).length < 400, "a few hundred bytes");
  assert.equal(stable(globalThis.Memory.fabot.ops.specs), stable([opSpec]), "the operator's keys untouched");
  saveOpsReadings(empty());
  assert.equal("readings" in globalThis.Memory.fabot.ops, false, "none: the key removed");
  globalThis.Memory = { fabot: {} };
  saveOpsReadings(ofArray([reading]));
  assert.equal(globalThis.Memory.fabot.ops, undefined, "no ops leaf: none made");
});

// ---- the exit camps' traffic watch (#674) ---------------------------------

// `Memory.fabot.ops.traffic`, the bot's alone: each watched exit's log,
// `{ s, o, w, b, a, n }` (`ObserveMemory.trafficWire`), written each full
// tick and read back on a cold heap at the tick reading it. Load, save,
// compare. The leaf is the bot's, but a hand edit through the Memory API
// may leave anything there: no edit may make the cold read throw, hang or
// read more than a log holds.
const trafficNow = 1_000_000;
const trafficThrough = async (ops, now = trafficNow) => {
  const { loadTraffic, saveTraffic } = await import(MODULE);
  globalThis.Memory = ops === undefined ? { fabot: {} } : { fabot: { ops } };
  const traffic = loadTraffic(now);
  saveTraffic(traffic);
  return { traffic, ops: globalThis.Memory.fabot.ops };
};

const fuseId = "68f0c1e2a3b4c5d6e7f8a9b0";
const westLog = {
  s: trafficNow - 3000,
  o: "Trepidimous",
  // A full tick every other tick for a hundred ticks, then one forced.
  w: [
    [trafficNow - 300, 100, 2],
    [trafficNow - 99, 1],
  ],
  b: [
    [fuseId, "3R4H7M"],
    ["b2", "2Tc2Aa1A1M"],
  ],
  a: [
    [trafficNow - 99, 0, 25, 0, 1400, 900],
    [trafficNow - 110, 1, 24, 0, 600, 1200],
    [trafficNow - 200, 0, 25, 0, 1400, 999],
  ],
  n: [[fuseId, 25, 0]],
};

wire("traffic: an exit's log round-trips, its bodies whole, the operator's keys untouched", async () => {
  const full = { specs: [opSpec], traffic: { "W18S26>W19S26": westLog } };
  const { traffic, ops } = await trafficThrough(structuredClone(full));
  assert.equal(stable(ops), stable(full));
  const [[key, log]] = [...traffic];
  assert.equal(key, "W18S26>W19S26");
  assert.deepEqual([log.Since, [...log.Watched].length, [...log.Watched][0]], [trafficNow - 3000, 101, trafficNow - 99]);
  const [tick, fuse] = [...log.Arrivals][0];
  assert.deepEqual(
    [tick, fuse.Id, fuse.Owner, { ...fuse.Pos }, [...fuse.Body].length, [...fuse.Boosts].length, fuse.Hits, fuse.TicksToLive],
    [trafficNow - 99, fuseId, "Trepidimous", { Room: "W19S26", X: 25, Y: 0 }, 14, 0, 1400, 900],
  );
  const boosted = [...log.Arrivals][1][1];
  assert.deepEqual(
    [...boosted.Boosts].map((boost) => boost.tag),
    [3, 3, 1, 1, 0, 0],
    "each part's tier, head first",
  );
});

wire("traffic: an older shape, a log with no bodies, landings or standing, reads its watch", async () => {
  const { traffic } = await trafficThrough({ traffic: { "W18S26>W19S26": { s: trafficNow - 3000, w: westLog.w } } });
  const log = traffic.get("W18S26>W19S26");
  assert.deepEqual([log.Since, [...log.Watched].length, [...log.Arrivals].length], [trafficNow - 3000, 101, 0]);
});

for (const [name, leaf] of [
  ["a traffic that is a string", "Incorrect memory path"],
  ["a traffic that is an array", [westLog]],
  ["a traffic that is null", null],
  ["an exit that is no object", { "W18S26>W19S26": [1, 2] }],
  ["an exit with no tick begun", { "W18S26>W19S26": { ...westLog, s: "soon" } }],
  // #674 review: `recall` writes `s` back, so one in the future stuck forever.
  ["an exit begun after now", { "W18S26>W19S26": { ...westLog, s: trafficNow + 1 } }],
  ["an exit begun before tick 0", { "W18S26>W19S26": { ...westLog, s: -5 } }],
  ["an exit begun on no whole tick", { "W18S26>W19S26": { ...westLog, s: trafficNow - 0.5 } }],
  ["an exit whose key names no exit", { W19S26: westLog }],
]) {
  wire(`traffic: ${name} reads as no watch, never a fault`, async () => {
    const { loadTraffic } = await import(MODULE);
    globalThis.Memory = { fabot: { ops: { traffic: leaf } } };
    assert.equal([...loadTraffic(trafficNow)].length, 0);
  });
}

wire("traffic: an ops leaf off its shape reads as no watch, and nothing is written onto it", async () => {
  const { traffic } = await trafficThrough({ traffic: { "W18S26>W19S26": westLog } });
  const { saveTraffic, loadTraffic } = await import(MODULE);
  globalThis.Memory = { fabot: { ops: "Incorrect memory path" } };
  assert.equal([...loadTraffic(trafficNow)].length, 0);
  saveTraffic(traffic);
  assert.equal(globalThis.Memory.fabot.ops, "Incorrect memory path");
});

wire("traffic: a row off its shape costs its own row", async () => {
  const { traffic } = await trafficThrough({
    traffic: {
      "W18S26>W19S26": {
        ...westLog,
        w: [...westLog.w, [trafficNow - 50, 0], [trafficNow - 40, 2, 0], "run", [trafficNow - 30]],
        b: [...westLog.b, ["b3", "3X"], ["", "1M"]],
        a: [
          ...westLog.a,
          [trafficNow - 98, 7, 25, 0, 100, 100],
          [trafficNow - 97, 2, 25, 0, 100, 100],
          [trafficNow - 96, 3, 25, 0, 100, 100],
          [trafficNow - 95, 0, 50, 0, 100, 100],
          [trafficNow - 94, 0, 25, 0, "full", 100],
          [trafficNow + 5, 0, 25, 0, 100, 100],
          // Negative or fractional hits and ticks to live, a fractional tick.
          [trafficNow - 93, 0, 25, 0, -1, 100],
          [trafficNow - 92, 0, 25, 0, 100, -3],
          [trafficNow - 91, 0, 25, 0, 100.5, 100],
          [trafficNow - 90.5, 0, 25, 0, 100, 100],
          null,
        ],
        n: [...westLog.n, ["", 1, 1], ["x", 25]],
      },
    },
  });
  const log = traffic.get("W18S26>W19S26");
  assert.deepEqual(
    [[...log.Watched].length, [...log.Arrivals].map(([tick]) => tick), [...log.Standing].length],
    [101, westLog.a.map(([tick]) => tick), 1],
  );
});

wire("traffic: read back, only the window and at most a body's bouncing worth of landings", async () => {
  const many = 2000;
  const { traffic, ops } = await trafficThrough({
    traffic: {
      "W18S26>W19S26": {
        s: trafficNow - 5000,
        o: "Trepidimous",
        w: [[trafficNow - many, many]],
        b: [[fuseId, "3R4H7M"]],
        a: Array.from({ length: many }, (_, back) => [trafficNow - 1 - back, 0, 25, 0, 1400, 900]),
      },
      "W18S26>W18S27": { s: trafficNow - 5000, w: [[0, 2_000_000_000]] },
    },
  });
  const log = traffic.get("W18S26>W19S26");
  // The window is the 1,500 ticks up to now; the run ends a tick before it.
  assert.deepEqual([[...log.Watched].length, [...log.Arrivals].length], [1499, 750]);
  assert.equal([...traffic.get("W18S26>W18S27").Watched].length, 1500, "a run however long reads a window");
  assert.deepEqual(ops.traffic["W18S26>W19S26"].w, [[trafficNow - 1499, 1499]], "written back as one run");
});

// #674 review: the bound claimed before was one body's and one run's. The
// worst a log holds: the cap's 750 landings by as many distinct bodies of
// 24-character ids, and a window watched in its most broken runs. `runsOf`
// folds the tick after a run's first into it, and a run's next tick at its
// step extends it, so a run holds two ticks and the next begins two on at
// the nearest: 500 runs a window (gaps of 1 and 2 by turns).
const trafficAtBound = async (shapeOf) => {
  const ids = Array.from({ length: 800 }, (_, i) => `${i}`.padStart(24, "f"));
  const w = [];
  for (let tick = trafficNow - 1499; tick + 1 <= trafficNow; tick += 3) w.push([tick, 2]);
  const exit = {
    s: trafficNow - 40_000,
    o: "Trepidimous",
    w,
    b: ids.map((id, i) => [id, shapeOf(i)]),
    a: ids.map((_, i) => [trafficNow - 1 - i, i, 49, 1 + (i % 48), 5000, 1500]),
    n: Array.from({ length: 48 }, (_, i) => [ids[i], 49, 1 + i]),
  };
  const { traffic, ops } = await trafficThrough({ traffic: { "W18S27>W19S27": exit } });
  const log = traffic.get("W18S27>W19S27");
  assert.deepEqual(
    [[...log.Arrivals].length, new Set([...log.Arrivals].map(([, h]) => h.Id)).size, [...log.Watched].length],
    [750, 750, 1000],
    "the cap's landings, each a body of its own; a window's ticks in 500 runs",
  );
  const written = ops.traffic["W18S27>W19S27"];
  assert.deepEqual(
    [written.b.length, written.a.length, written.w.length],
    [750, 750, 500],
    "only the bodies a landing kept names; the runs written back as broken",
  );
  return JSON.stringify(written).length;
};

wire("traffic: at the bound, 750 distinct bodies of a 50-part boosted shape in broken runs: under 70 KB", async () => {
  const bytes = await trafficAtBound(() => "5Tc10Rc10Hc25Mc");
  console.log(`       (an exit at the bound, one shape: ${bytes} B)`);
  assert.ok(bytes < 70_000, `${bytes} B`);
});

// The longest a shape is spelt: fifty parts, each its own run and boosted
// ("1Ta1Mb…", three characters a part), each body's its own.
wire("traffic: at the bound, each body a distinct 50-part shape of the longest text: under 175 KB", async () => {
  const tiers = ["a", "b", "c"];
  const bytes = await trafficAtBound((i) =>
    Array.from({ length: 50 }, (_, p) => `1${p % 2 === 0 ? "T" : "M"}${tiers[Math.floor(i / 3 ** Math.min(p, 6)) % 3]}`).join(""),
  );
  console.log(`       (an exit at the bound, every shape distinct and longest: ${bytes} B)`);
  assert.ok(bytes < 175_000, `${bytes} B`);
});

// #674 review: a window of runs edited to overlap, each a window long, made
// ~2.25 million cells before `List.distinct` on a cold heap.
wire("traffic: runs edited to overlap read a window, the newest runs first, at once", async () => {
  const { loadTraffic } = await import(MODULE);
  const overlapping = Array.from({ length: 5000 }, () => [trafficNow - 1499, 1500]);
  globalThis.Memory = {
    fabot: {
      ops: {
        traffic: {
          "W18S26>W19S26": { s: trafficNow - 3000, w: [[trafficNow - 1499, 10], ...overlapping] },
          // Oldest first as written: the newest run is the last, kept.
          "W18S26>W18S27": {
            s: trafficNow - 3000,
            w: [...Array.from({ length: 2000 }, () => [trafficNow - 2000, 1]), [trafficNow - 5, 3, 2]],
          },
        },
      },
    },
  };
  const started = performance.now();
  const traffic = loadTraffic(trafficNow);
  const ms = performance.now() - started;
  assert.equal([...traffic.get("W18S26>W19S26").Watched].length, 1500);
  assert.deepEqual([...traffic.get("W18S26>W18S27").Watched], [trafficNow - 1, trafficNow - 3, trafficNow - 5]);
  assert.ok(ms < 200, `read in ${ms.toFixed(1)} ms`);
});

// #674 review: `a` and `b` are cut to the cap before they decode.
wire("traffic: landings and bodies edited past the cap: the newest landings read, the rest never decoded", async () => {
  const many = 100_000;
  const { traffic } = await trafficThrough({
    traffic: {
      "W18S26>W19S26": {
        s: trafficNow - 3000,
        o: "Trepidimous",
        w: westLog.w,
        b: Array.from({ length: many }, (_, i) => [`body-${i}`, "3R4H7M"]),
        a: Array.from({ length: many }, (_, i) => [trafficNow - 1 - (i % 1400), i, 25, 0, 1400, 900]),
      },
    },
  });
  const arrivals = [...traffic.get("W18S26>W19S26").Arrivals];
  assert.deepEqual([arrivals.length, arrivals[0][1].Id, arrivals[749][1].Id], [750, "body-0", "body-749"]);
});

wire("traffic: a live-like exit is about a kilobyte", async () => {
  // A window at cadence 2 broken by ten forced ticks; twenty landings by
  // four bodies of 24-character ids, Trepidimous' harassers.
  const ids = Array.from({ length: 4 }, (_, i) => `68f0c1e2a3b4c5d6e7f8a9b${i}`);
  const w = Array.from({ length: 11 }, (_, i) => [trafficNow - 1499 + i * 136, 67, 2]);
  const exit = {
    s: trafficNow - 40_000,
    o: "Trepidimous",
    w,
    b: ids.map((id) => [id, "3R4H7M"]),
    a: Array.from({ length: 20 }, (_, i) => [trafficNow - 1 - i * 70, i % 4, 24 + (i % 3), 0, 1400, 1500 - i * 70]),
    n: [],
  };
  const { ops } = await trafficThrough({ traffic: { "W18S26>W19S26": exit, "W18S26>W18S27": exit } });
  const bytes = JSON.stringify(ops.traffic).length;
  console.log(`       (two live-like exits: ${bytes} B)`);
  assert.ok(bytes < 2_500, `two exits under 2.5 KB, ${bytes} B`);
});

wire("traffic: no watch removes the key, and makes no ops leaf where none stands", async () => {
  const { saveTraffic, loadTraffic } = await import(MODULE);
  const { traffic } = await trafficThrough({ traffic: { "W18S26>W19S26": westLog } });
  globalThis.Memory = { fabot: {} };
  const none = loadTraffic(trafficNow);
  saveTraffic(none);
  assert.equal(globalThis.Memory.fabot.ops, undefined, "nothing to keep: no ops leaf made");
  globalThis.Memory = { fabot: { ops: { specs: [opSpec], traffic: { "W18S26>W19S26": westLog } } } };
  saveTraffic(none);
  assert.equal(stable(globalThis.Memory.fabot.ops), stable({ specs: [opSpec] }), "no exit watched: the key removed");
  globalThis.Memory = { fabot: {} };
  saveTraffic(traffic);
  assert.equal(stable(globalThis.Memory.fabot.ops), stable({ traffic: { "W18S26>W19S26": westLog } }), "a watch to keep: made");
});

// The operator's check (`checkOpSpec`): the shell's own decoder and
// admission over the leaf as stored, so the CLI refuses what the bot would.
wire("ops check: a spec is admitted dry or live, or refused by the bot's own reason", async () => {
  const { checkOpSpec } = await import(MODULE);
  const holders = { W21S23: "Trepidimous", W21S24: "", W20S23: "", W18S28: "Trepidimous", W18S27: "" };
  const row = { id: "c2", template: "exitCamp", enemy: "Trepidimous", rooms: ["W21S23", "W21S24"], deadline: 1005000 };
  const check = (r, live = false, adding = true, stored = null) =>
    checkOpSpec(1000000, "Xerxes-2", holders, stored, r, live, adding);
  const ok = check(row);
  assert.deepEqual([ok.ok, ok.runs, ok.decoded.maxBodies, ok.decoded.deadline], [true, "dry", 1, 1005000]);
  assert.equal(check(row, true).runs, "live");
  for (const [changed, why] of [
    [{ template: "assault" }, "no template: assault"],
    [{ enemy: "Odiodin" }, "an ally: Odiodin"],
    [{ enemy: "Kalgen" }, "a neutral: Kalgen"],
    [{ rooms: ["W21S23", "W20S23", "W25S25"] }, "holder unknown: W25S25"],
    [{ rooms: ["W21S23", "W25S23"] }, "holder unknown: W25S23"],
    [{ maxEnergy: 0 }, "no budget"],
    [{ deadline: 999000 }, "deadline past"],
    [{ id: "camp" }, "a code op's id: camp"],
    [{ rooms: ["W18S28", "W18S27"], id: "c3" }, "code op sweep holds W18S27"],
    [{ template: "sweep", rooms: ["W21S24"] }, "no live sweep"],
    [{ rooms: "W21S23" }, "garbled"],
  ]) {
    const answer = check({ ...row, ...changed }, true);
    assert.deepEqual([answer.ok, answer.why], [false, why], why);
  }
  assert.equal(check(row, false, true, { specs: [row] }).why, "duplicate id");
  assert.equal(check(row, true, false, { specs: [row] }).runs, "live", "`op live` re-checks its own row");
  assert.equal(check({ ...row, id: "c4" }, false, true, { specs: [row] }).why, "op c2 holds W21S23");
  assert.equal(check(row, false, true, { off: true }).off, true, "the switch reported beside");
});

// ---- TileSet under Fable ------------------------------------------------

// A bitset is the one Core type whose meaning rests on JS bit arithmetic:
// bit 31 reads negative, and `dotnet test` never runs the Fable build. Each
// operation is held to a sorted list of grid indices, the order `Set<Pos>`
// enumerates in, over sets that straddle the word boundaries.
wire("TileSet under Fable answers what a sorted index list answers", async () => {
  const T = await import("../build/fable/Core/Types/TileSet.js");
  const G = await import("../build/fable/Core/Types/Geometry.js");
  const { ofArray } = await import(LIST);
  const pos = (i) => new G.Pos(Math.floor(i / 50), i % 50);
  let seed = 97;
  const rnd = (n) => {
    seed = (seed * 1103515245 + 12345) & 0x7fffffff;
    return seed % n;
  };
  const shapes = [[], [0], [31], [2499], [0, 30, 31, 32, 33, 63, 64, 2495, 2496, 2499], [31, 63, 95]];
  for (const n of [5, 60, 900, 2400]) shapes.push(Array.from({ length: n }, () => rnd(2500)));
  const sorted = (a) => [...new Set(a)].sort((x, y) => x - y);
  const tiles = (a) => T.TileSetModule_ofList(ofArray(a.map(pos)));
  const indices = (s) => [...T.TileSetModule_toArray(s)].map((p) => p.X * 50 + p.Y);
  // Lexicographic over the sorted indices, a prefix the lesser: `Set<Pos>`'s order.
  const order = (r, q) => {
    for (let k = 0; k < Math.min(r.length, q.length); k++) if (r[k] !== q[k]) return r[k] < q[k] ? -1 : 1;
    return Math.sign(r.length - q.length);
  };
  for (const a of shapes) {
    const s = tiles(a);
    const r = sorted(a);
    assert.deepEqual(indices(s), r, "the tiles, in Pos order");
    assert.equal(T.TileSetModule_count(s), r.length, "count");
    assert.deepEqual(indices(T.TileSetModule_init((i) => r.includes(i))), r, "init");
    if (r.length) {
      const least = T.TileSetModule_minElement(s);
      assert.equal(least.X * 50 + least.Y, r[0], "minElement");
    }
    const odd = (p) => (p.X * 7 + p.Y) % 2 === 1;
    assert.deepEqual(indices(T.TileSetModule_filter(odd, s)), r.filter((i) => odd(pos(i))), "filter");
    for (let k = 0; k < 50; k++) {
      const i = rnd(2500);
      assert.equal(T.TileSetModule_contains(pos(i), s), r.includes(i), `contains ${i}`);
    }
    for (const i of [0, 31, 32, 63, 2495, 2499, rnd(2500)]) {
      assert.deepEqual(indices(T.TileSetModule_add(pos(i), s)), sorted(r.concat([i])), `add ${i}`);
      assert.deepEqual(indices(T.TileSetModule_remove(pos(i), s)), r.filter((x) => x !== i), `remove ${i}`);
    }
    for (const b of shapes) {
      const t = tiles(b);
      const q = sorted(b);
      assert.deepEqual(indices(T.TileSetModule_union(s, t)), sorted(r.concat(q)), "union");
      assert.deepEqual(indices(T.TileSetModule_intersect(s, t)), r.filter((x) => q.includes(x)), "intersect");
      assert.deepEqual(indices(T.TileSetModule_difference(s, t)), r.filter((x) => !q.includes(x)), "difference");
      assert.equal(T.TileSetModule_isSubset(s, t), r.every((x) => q.includes(x)), "isSubset");
      assert.equal(s.Equals(t), stable(r) === stable(q), "equality");
      assert.equal(Math.sign(s.CompareTo(t)), order(r, q), "comparison");
    }
  }
  assert.throws(() => T.TileSetModule_add(new G.Pos(-1, 0), T.TileSetModule_empty), /off the grid/);
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
