// A captured live tick as an F# arena scenario skeleton (#532): the pure
// half of `capture-tick.mjs`, which does the reading. No I/O here, so
// `cli-smoke.mjs` runs it on a synthetic replay.
//
// The skeleton is written in the arena's own idiom (`tests/Core.Tests/
// Arena.fs`): rooms off their committed captures, controllers, structures
// and sites placed by the builders, bodies by `body` with their hits, life
// and stores, and the Matcher's assignments under `Carried`. What the replay
// cannot say is a TODO in the header, never a guess.

const PARTS = {
  move: "Move",
  work: "Work",
  carry: "Carry",
  attack: "Attack",
  ranged_attack: "RangedAttack",
  heal: "Heal",
  claim: "BodyPart.Claim",
  tough: "Tough",
};

// CONSTRUCTION_COST kinds `withSite` knows; any other site is a TODO.
const SITE_KINDS = new Set(["spawn", "tower", "extension", "rampart", "road", "container", "storage"]);

// Objects the arena has no place for, or holds elsewhere (sources, minerals
// and the controller's tile are the room capture's; roads go to `withRoads`).
const NOT_PLACED = new Set(["source", "mineral", "controller", "creep", "constructionSite", "road", "tombstone", "ruin", "energy"]);

// The engine's NPC owners (`creeps/tick.js`).
const NPCS = { 2: "Invader", 3: "Source Keeper" };

const CREEP_LIFE = 1500;

// CONTROLLER_STRUCTURES (`constants.js`) by controller level.
const SPAWN_LIMIT = [0, 1, 1, 1, 1, 1, 1, 2, 3];
const EXTENSION_LIMIT = [0, 0, 5, 10, 20, 30, 40, 50, 60];
const CLAIM_LIFE = 600;

/// 972617 as `972_617`; F# reads both, the arena tests write the grouped one.
export const fsInt = (n) => {
  const sign = n < 0 ? "-" : "";
  const digits = String(Math.abs(Math.trunc(n)));
  return digits.length < 4 ? `${sign}${digits}` : sign + digits.replace(/\B(?=(\d{3})+(?!\d))/g, "_");
};

const fsString = (s) => JSON.stringify(String(s));

/// A slug as an F# identifier: `w17s25-t972544` as `w17s25T972544`.
export const identifier = (slug) => {
  const words = slug.split(/[^A-Za-z0-9]+/).filter(Boolean);
  const id = words.map((w, i) => (i === 0 ? w[0].toLowerCase() + w.slice(1) : w[0].toUpperCase() + w.slice(1))).join("");
  return /^[0-9]/.test(id) ? `capture${id}` : id || "capture";
};

/// A body, head first, as `parts [ Move, 17; Attack, 17; Move, 1 ]`.
const partsOf = (body) => {
  const runs = [];
  for (const part of body) {
    const name = PARTS[part.type] ?? `(* ${part.type} *) Move`;
    if (runs.length > 0 && runs.at(-1)[0] === name) runs.at(-1)[1]++;
    else runs.push([name, 1]);
  }
  return `parts [ ${runs.map(([name, n]) => `${name}, ${n}`).join("; ")} ]`;
};

/// The parts that still act, counted, `14 ATTACK, 1 MOVE`, in body order.
const liveSummary = (body) => {
  const counts = new Map();
  for (const part of body) if (part.hits > 0) counts.set(part.type, (counts.get(part.type) ?? 0) + 1);
  return [...counts].map(([type, n]) => `${n} ${type.toUpperCase()}`).join(", ") || "none";
};

const pos = (o) => `{ X = ${o.x}; Y = ${o.y} }`;

/// A record copy over `head` with these fields, or `head` alone when none.
const withFields = (head, fields, indent) => {
  if (fields.length === 0) return `${indent}${head}`;
  const pad = `${indent}    `;
  return `${indent}{ ${head} with\n${fields.map((f) => `${pad}${f}`).join("\n")}\n${indent}}`;
};

/// The skeleton's text. `rooms` is `[{ name, href, objects, hasFixture }]`,
/// the primary room first; `ourId` is our user id; `usernames` names the
/// other ids; `assignments` is `{ known: Map<creep, task>, unknown: string[],
/// readAt }` or null when Memory was not read; `command` is the line that
/// captured it.
export function skeleton({ rooms, tick, ourId, usernames, assignments, live = null, slug, command, liveTick }) {
  const time = tick + 1;
  const fn = identifier(slug);
  const sides = new Map(); // F# binding -> username
  const sideOf = (userId) => {
    if (userId === ourId) return "Side.Ours";
    if (Object.hasOwn(NPCS, userId)) return `(Side.Npc ${fsString(NPCS[userId])})`;
    const name = usernames.get(userId) ?? String(userId);
    let binding = identifier(name);
    if (["at", "bodies", "colonies", "a", "body", "room", "arena", "parts"].includes(binding)) binding += "Side";
    sides.set(binding, name);
    return binding;
  };
  const roomBinding = (name) => name.toLowerCase();
  const todos = [];
  const notes = [];
  const fixture = [];
  const bodies = [];
  const homes = [];
  const skipped = new Map();
  // The arena names what it places by its own ids (`kind-x-y`, `spawn-<name>`,
  // `site-kind-x-y`); the room capture keeps the engine's for sources,
  // minerals and the controller. A Task naming an engine id is re-pointed.
  const arenaIds = new Map();
  const unplaced = new Map();
  // `structureOf`'s `kind-x-y` is unique in a room, not across rooms: a
  // second room's clash takes the room name on, as an explicit `Id`.
  const used = new Set();

  for (const r of rooms) {
    if (!r.hasFixture) {
      todos.push(`${r.name} has no committed capture: \`npm run capture-room -- ${r.name}\` first.`);
    }
    const objects = Object.values(r.objects);
    for (const o of objects) {
      if (["source", "mineral", "controller"].includes(o.type)) arenaIds.set(o._id, o._id);
      else if (o.type === "creep") arenaIds.set(o._id, o.name);
      else if (o.type === "road") arenaIds.set(o._id, `road-${o.x}-${o.y}-${r.name}`);
      else unplaced.set(o._id, o.type);
    }
    const lines = [`    let ${roomBinding(r.name)} =`, `        room ${fsString(r.name)}`];
    const controller = objects.find((o) => o.type === "controller");
    if (controller?.user) {
      const owner = controller.user === ourId ? "Ownership.Ours None" : `Ownership.Rival (Some ${fsString(usernames.get(controller.user) ?? controller.user)})`;
      lines.push(`        |> withController ${owner} ${controller.level} ${controller.safeModeAvailable ?? 0}`);
      if ((controller.safeModeCooldown ?? 0) > time) lines.push(`        |> withSafeModeCooldown ${fsInt(controller.safeModeCooldown)}`);
      const safeUntil = (controller.safeMode ?? 0) > time ? controller.safeMode : 0;
      const blocked = (controller.upgradeBlocked ?? 0) > time ? controller.upgradeBlocked : 0;
      const downgrade = controller.downgradeTime ? controller.downgradeTime - time : 0;
      lines.push(
        `        |> withControllerClocks ${fsInt(safeUntil)} ${fsInt(blocked)} ${fsInt(downgrade)} ${fsInt(controller.progress ?? 0)}`,
      );
    } else if (controller?.reservation) {
      const who = controller.reservation.user === ourId ? "us" : (usernames.get(controller.reservation.user) ?? controller.reservation.user);
      notes.push(`${r.name} is reserved by ${who} to t${controller.reservation.endTime}: the arena has no reservations.`);
    }

    const ourSpawns = objects.filter((o) => o.type === "spawn" && o.user === ourId);
    if (ourSpawns.length > 0) {
      // The arena's bank is never debited (its economy refills it), and is
      // the room's energy capacity as well as what it holds (`Arena.factsOf`):
      // so it is the capacity, every spawn's and extension's. What the room
      // held at the tick would price every cast, the guard row's included,
      // off a near-empty bank and stand an outpost down that live held.
      // Only the structures the controller's level keeps active count
      // (CONTROLLER_STRUCTURES): a downgraded room's surplus holds nothing.
      const level = controller?.user === ourId ? controller.level : 0;
      const capacity = (type, fallback, limit) =>
        objects
          .filter((o) => o.type === type && o.user === ourId)
          .map((o) => o.storeCapacityResource?.energy ?? fallback)
          .slice(0, limit)
          .reduce((sum, c) => sum + c, 0);
      const bank =
        capacity("spawn", 300, SPAWN_LIMIT[level] ?? 0) + capacity("extension", 50, EXTENSION_LIMIT[level] ?? 0);
      for (const s of ourSpawns) {
        arenaIds.set(s._id, `spawn-${s.name}`);
        lines.push(`        |> withSpawn ${fsString(s.name)} ${pos(s)} ${fsInt(bank)}`);
        if (s.spawning?.name) {
          notes.push(`${s.name} is spawning ${s.spawning.name} until t${s.spawning.spawnTime}: not placed.`);
        }
      }
      if (controller?.user === ourId) homes.push(r.name);
    }

    const structures = [];
    for (const o of objects) {
      if (NOT_PLACED.has(o.type) || (o.type === "spawn" && o.user === ourId)) {
        if (["tombstone", "ruin", "energy"].includes(o.type)) skipped.set(o.type, (skipped.get(o.type) ?? 0) + 1);
        continue;
      }
      if (o.type === "reactor") {
        arenaIds.set(o._id, o._id);
        const owner = o.user ? `(Some ${sideOf(o.user)})` : "None";
        lines.push(`        |> withReactor ${fsString(o._id)} ${pos(o)} ${owner} ${fsInt(o.store?.T ?? 0)}`);
        continue;
      }
      if (typeof o.hits !== "number") {
        skipped.set(o.type, (skipped.get(o.type) ?? 0) + 1);
        continue;
      }
      let arenaId = `${o.type}-${o.x}-${o.y}`;
      const clash = used.has(arenaId);
      if (clash) arenaId = `${arenaId}-${r.name}`;
      used.add(arenaId);
      arenaIds.set(o._id, arenaId);
      const owner = o.user ? `(Some ${sideOf(o.user)})` : "None";
      const energy = o.store?.energy ?? 0;
      const thorium = o.store?.T ?? 0;
      const fields = [];
      let head;
      if (o.type === "tower") {
        head = `towerOf ${owner.slice(6, -1)} ${pos(o)} ${fsInt(energy)}`;
        if (!o.user) head = `structureOf "tower" None ${fsInt(o.hits)} ${fsInt(o.hitsMax)} ${pos(o)}`;
        else if (o.hits !== 3000) fields.push(`Hits = ${fsInt(o.hits)}`);
      } else {
        head = `structureOf ${fsString(o.type)} ${owner} ${fsInt(o.hits)} ${fsInt(o.hitsMax)} ${pos(o)}`;
        if (energy > 0) fields.push(`Energy = ${fsInt(energy)}`);
      }
      if (clash) fields.push(`Id = ${fsString(arenaId)}`);
      if (thorium > 0) fields.push(`Thorium = ${fsInt(thorium)}`);
      // A stronghold's core; a level-0 core is the arena's default.
      if (o.type === "invaderCore" && o.level > 0) fields.push(`Level = ${o.level}`);
      if (o.type === "rampart") {
        if (o.isPublic) fields.push("IsPublic = true");
        if (o.nextDecayTime) fields.push(`NextDecay = ${fsInt(o.nextDecayTime)}`);
      }
      structures.push({
        key: `${o.type} ${String(o.x).padStart(2)} ${String(o.y).padStart(2)}`,
        text: withFields(head, fields, "                "),
        // Three or more alike but for the tile are one comprehension.
        template: (() => {
          const shared = fields.map((f) => (f.startsWith("Id = ") ? `Id = $"${o.type}-{x}-{y}-${r.name}"` : f));
          return (shared.length === 0 ? head : `{ ${head} with ${shared.join("; ")} }`).replace(pos(o), "{ X = x; Y = y }");
        })(),
        at: `${o.x}, ${o.y}`,
      });
    }
    if (structures.length > 0) {
      structures.sort((a, b) => a.key.localeCompare(b.key));
      const counts = new Map();
      for (const s of structures) if (s.template) counts.set(s.template, (counts.get(s.template) ?? 0) + 1);
      // A list comprehension stands alone: F# will not mix one with plain
      // elements in a list literal.
      const items = [];
      const done = new Set();
      for (const s of structures) {
        if (s.template && counts.get(s.template) >= 3) {
          if (done.has(s.template)) continue;
          done.add(s.template);
          const tiles = structures.filter((t) => t.template === s.template).map((t) => t.at);
          lines.push(`        |> withStructures [ for x, y in [ ${tiles.join("; ")} ] -> ${s.template} ]`);
        } else items.push(s.text);
      }
      if (items.length > 0) lines.push("        |> withStructures", "            [", ...items, "            ]");
    }
    // Roads by tile and hits (#560): the arena walks them at fatigue 1 a
    // part, and the colony's walk reads them as the shell files them.
    const roads = objects
      .filter((o) => o.type === "road")
      .sort((a, b) => a.x - b.x || a.y - b.y)
      .map((o) => `${o.x}, ${o.y}, ${fsInt(o.hits)}`);
    if (roads.length > 0) lines.push(`        |> withRoads [ ${roads.join("; ")} ]`);
    for (const site of objects.filter((o) => o.type === "constructionSite")) {
      const owner = site.user === ourId ? "Side.Ours" : sideOf(site.user);
      const name = site.name ? `Some ${fsString(site.name)}` : "None";
      const call = `withSite ${owner} ${fsString(site.structureType)} ${site.name ? `(${name})` : "None"} ${fsInt(site.progress ?? 0)} ${pos(site)}`;
      if (SITE_KINDS.has(site.structureType)) {
        // `withSite`'s `site-kind-x-y`, room-suffixed on a clash as a
        // structure's is, which `withSite` cannot spell: the record itself.
        let siteId = `site-${site.structureType}-${site.x}-${site.y}`;
        const clash = used.has(siteId);
        if (clash) siteId = `${siteId}-${r.name}`;
        used.add(siteId);
        arenaIds.set(site._id, siteId);
        if (!clash) lines.push(`        |> ${call}`);
        else {
          lines.push(
            "        |> fun r ->",
            "            { r with",
            "                Sites =",
            "                    r.Sites",
            `                    @ [ { Id = ${fsString(siteId)}; Kind = ${fsString(site.structureType)}; At = ${pos(site)}; Owner = ${owner}; Progress = ${fsInt(site.progress ?? 0)}; Total = ${fsInt(site.progressTotal ?? 0)}; Name = ${name} } ]`,
            "            }",
          );
        }
      }
      else todos.push(`${r.name}: a ${site.structureType} site at ${site.x},${site.y} \`withSite\` has no cost for.`);
    }
    fixture.push(lines.join("\n"), "");

    for (const c of objects.filter((o) => o.type === "creep")) {
      const body = c.body ?? [];
      const partSum = body.reduce((sum, p) => sum + (p.hits ?? 0), 0);
      const hitsMax = c.hitsMax ?? body.length * 100;
      const comments = [];
      if (c.hits <= 0) {
        bodies.push(`            // ${c.name} died at t${tick} (hits ${c.hits}/${hitsMax}): not placed.`);
        continue;
      }
      if (c.spawning) continue; // the spawn's note says so
      const ours = c.user === ourId;
      const side = sideOf(c.user);
      const isClaim = body.some((p) => p.type === "claim");
      const ttl = c.ageTime ? c.ageTime - time : null;
      const fields = [];
      if (c.hits !== hitsMax) fields.push(`Hits = ${fsInt(c.hits)}`);
      if (ttl !== null && ttl !== (isClaim ? CLAIM_LIFE : CREEP_LIFE)) fields.push(`TicksToLive = ${fsInt(ttl)}`);
      if ((c.fatigue ?? 0) > 0) fields.push(`Fatigue = ${fsInt(c.fatigue)}`);
      if ((c.store?.energy ?? 0) > 0) fields.push(`Energy = ${fsInt(c.store.energy)}`);
      if ((c.store?.T ?? 0) > 0) fields.push(`Thorium = ${fsInt(c.store.T)}`);
      if (c.hits !== hitsMax) comments.push(`live: ${liveSummary(body)}`);
      if (partSum !== c.hits) comments.push(`its parts read ${partSum} hits, the creep ${c.hits}: the creep's is placed`);
      if (body.some((p) => p.boost)) fields.push(`Boosts = boostsOn (${partsOf(body)}) [ ${body.map((p) => fsString(p.boost ?? "")).join("; ")} ]`);
      if (c.interRoom) comments.push(`crossing to ${c.interRoom.room} ${c.interRoom.x},${c.interRoom.y}`);
      if (ttl === null) comments.push("no ageTime in the replay: a full life");
      const acts = Object.entries(c.actionLog ?? {})
        .filter(([name, at]) => at && name !== "say")
        .map(([name, at]) => (typeof at === "object" && "x" in at ? `${name}→${at.x},${at.y}` : name))
        .join(" ");
      // The engine drives user '2' itself (`invaders/pretick.js`): the arena's
      // `Invader` is that AI, so an invader needs no hand-written script.
      const invader = String(c.user) === "2";
      if (!ours && !invader) comments.push(`TODO script; live: ${acts || "no act"}`);
      if (invader && acts) comments.push(`live: ${acts}`);
      const script = ours ? "None" : invader ? "(Some Invader)" : "(Some Hold)";
      const head = `body ${fsString(c.name)} ${side} (${partsOf(body)}) (at ${fsString(r.name)} ${c.x} ${c.y}) ${script}`;
      bodies.push(...comments.map((text) => `            // ${c.name}: ${text}`), withFields(head, fields, "            "));
    }
  }

  // A body cast by a spawn the capture does not hold belongs live to that
  // spawn's colony (a mother's residents stay hers); in the arena it is
  // adopted by the colony whose room it stands in, and decides differently.
  const spawnNames = new Set(rooms.flatMap((r) => Object.values(r.objects)).filter((o) => o.type === "spawn" && o.user === ourId).map((o) => o.name));
  const strays = new Map();
  for (const c of rooms.flatMap((r) => Object.values(r.objects))) {
    if (c.type !== "creep" || c.user !== ourId || c.hits <= 0) continue;
    const caster = /-(Spawn\w+)$/.exec(c.name ?? "")?.[1] ?? /(Spawn\d+)/.exec(c.name ?? "")?.[1];
    if (caster && !spawnNames.has(caster)) strays.set(caster, [...(strays.get(caster) ?? []), c.name]);
  }
  for (const [caster, names] of strays) {
    todos.push(
      `${names.join(", ")} cast by ${caster}, whose room is not captured: live it is that colony's body, here the room's colony adopts it. Add ${caster}'s room with --rooms.`,
    );
  }

  if (skipped.size > 0) {
    notes.push(`not placed: ${[...skipped].map(([type, n]) => `${n} ${type}`).join(", ")} (the arena has no drops, tombstones or ruins).`);
  }

  const out = [];
  out.push(`// ---- arena capture ${slug} ${"-".repeat(Math.max(4, 58 - slug.length))}`);
  out.push(`// \`${command}\`${liveTick ? `, read at live t${fsInt(liveTick).replaceAll("_", ",")}` : ""}.`);
  out.push(`// The room-history replay at t${fsInt(tick).replaceAll("_", ",")}, the state after that tick ran: arena`);
  out.push(`// tick 0 runs as t${fsInt(time).replaceAll("_", ",")} and decides what our code decided live then.`);
  for (const r of rooms) out.push(`//   ${r.name}  ${r.href}`);
  out.push("// Paste the fixture at module level and the test into its domain's testList");
  out.push("// (docs/agents/arena-capture.md), then name both for what they pin.");
  out.push("// TODO:");
  out.push("//   - every body not ours Holds: script each (its live acts are beside it);");
  if (live) {
    out.push("//   - Healers, ExitWatches and Sightings start empty (heap only); the Raid log,");
    out.push(`//     ArmedSeen and Towered are live Memory's at t${commas(live.readAt)}, less what postdates the tick;`);
  } else {
    out.push("//   - the Raid log (fought, stand-downs), ArmedSeen, Healers and Sightings start");
    out.push("//     empty: `observe raids --colony <home>` is today's, not this tick's (--live-memory);");
  }
  if (!assignments) out.push("//   - Assignments start empty: Memory was not read;");
  for (const t of todos) out.push(`//   - ${t}`);
  for (const n of notes) out.push(`// ${n}`);
  out.push("");
  out.push(`/// TODO: what t${fsInt(tick).replaceAll("_", ",")} is a regression test of.`);
  out.push(`let private ${fn} () : Arena =`);
  // Only bodies place by it: one unused is a warning, and warnings fail the build.
  if (bodies.some((line) => line.includes("(at \""))) {
    out.push("    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }");
  }
  // Bindings first use is in the fixture and the bodies, so print them last-collected but first-placed.
  const sideLines = [...sides].map(([binding, name]) => `    let ${binding} = Side.Player ${fsString(name)}`);
  out.push(...sideLines, "");
  out.push(...fixture);
  if (homes.length > 0) {
    out.push("    // Today's declarations, not the tick's: spell them out if the test must outlive a roster change.");
    out.push("    let colonies =");
    out.push(`        Colony.declared |> List.filter (fun c -> List.contains c.Home [ ${homes.map(fsString).join("; ")} ])`);
  } else {
    out.push("    // TODO: no home of ours was captured, so nothing of ours decides; add it with --rooms.");
    out.push("    let colonies: Colony list = []");
  }
  out.push("", "    let bodies: Body list =", "        [", ...bodies, "        ]", "");
  const arenaCall = `arena ${fsInt(time)} [ ${rooms.map((r) => roomBinding(r.name)).join("; ")} ] colonies bodies`;
  const known = [];
  const unknown = (assignments?.unknown ?? []).map(
    (name) => `${name}: the log holds no task verdict for it that early (or it has died since).`,
  );
  for (const [name, task] of assignments?.known ?? []) {
    const colon = task.indexOf(":");
    const target = colon < 0 ? "" : task.slice(colon + 1);
    if (arenaIds.has(target)) known.push([name, `${task.slice(0, colon)}:${arenaIds.get(target)}`]);
    else if (unplaced.has(target)) unknown.push(`${name}: ${task} names ${unplaced.get(target) === "energy" ? "dropped energy" : `a ${unplaced.get(target)}`}, which the arena does not place.`);
    else known.push([name, task]);
  }
  const fields = [];
  if (known.length > 0 || unknown.length > 0) {
    const f = [];
    f.push(`// The Transition log's last task verdict at or before t${commas(tick)},`);
    f.push(`// read from Memory at live t${commas(assignments.readAt)}.`);
    for (const line of unknown) f.push(`// TODO ${line}`);
    f.push("Assignments =");
    f.push("    Map.ofList");
    f.push("        [");
    for (const [name, task] of known) f.push(`            ${fsString(name)}, ${fsString(task)}`);
    f.push("        ]");
    fields.push(f);
  }
  if (live) fields.push(...liveFields(live, tick, rooms));
  if (fields.length === 0) {
    out.push(`    ${arenaCall}`);
  } else {
    out.push(`    let a = ${arenaCall}`, "");
    out.push("    { a with");
    out.push("        Carried =");
    out.push("            { a.Carried with");
    for (const f of fields) out.push(...f.map((line) => `                ${line}`));
    out.push("            }");
    out.push("    }");
  }
  out.push("");
  out.push("// ptest until the expectation is written; then test.");
  out.push(`[<Tests>]`);
  out.push(`let ${fn}Tests =`);
  out.push(`    testList`);
  out.push(`        ${fsString(`arena capture ${slug}`)}`);
  out.push("        [");
  out.push(`            ptest ${fsString(`${slug}: TODO what happens`)} {`);
  out.push(`                let _, trace = ${fn} () |> run 30`);
  out.push("                // EXPECT: what should happen; if it pins today's behaviour, say which issue flips it.");
  out.push('                failtest $"EXPECT: not written yet\\n{describe trace}"');
  out.push("            }");
  out.push("        ]");
  return out.join("\n") + "\n";
}

const commas = (n) => fsInt(n).replaceAll("_", ",");

const BASES = {
  "collapse-timer": "CollapseTimer",
  reservation: "Reservation",
  fallback: "Fallback",
  "rival-reservation": "RivalReservation",
  "invader-raid": "InvaderRaid",
  "harass-sighting": "HarassSighting",
  "raid-gone": "RaidGone",
  "player-raid-hold": "PlayerRaidHold",
};

const idSet = (ids) => (ids?.length ? `Set.ofList [ ${ids.map(fsString).join("; ")} ]` : "Set.empty");

/// The `Carried` fields a recent tick's live Memory seats (`live`: `{ readAt,
/// raids: { <home>: raw Raid log }, rooms: raw rooms leaf }`), each line
/// relative to the record's indent. A stamp after the tick is no reading
/// of it: such an entry is named in a comment and not seated.
function liveFields(live, tick, rooms) {
  const fields = [];
  const late = [];
  const header = [
    `// From live Memory at t${commas(live.readAt)}, ${live.readAt - tick} ticks after the capture:`,
    "// what it held then, less what is stamped after the tick.",
  ];
  const captured = new Set(rooms.map((r) => r.name));
  const armed = Object.entries(live.rooms?.armed ?? {}).filter(([room]) => captured.has(room));
  for (const [room, t] of armed.filter(([, t]) => t > tick)) late.push(`ArmedSeen ${room} t${commas(t)}`);
  const armedNow = armed.filter(([, t]) => t <= tick);
  const towered = (live.rooms?.towered ?? []).filter((room) => captured.has(room));
  const homes = Object.entries(live.raids ?? {});
  const raidLines = [];
  for (const [home, r] of homes) {
    // A row open at the tick, read later, may carry a later sighting: its
    // LastSeen is clamped to the tick and a later PlayerSince dropped, both
    // named; its Raiders are the later reading's.
    const outposts = (r.outposts ?? [])
      .filter((o) => o.opened <= tick && tick < o.expiry && BASES[o.basis])
      .map((o) => {
        if (o.last <= tick && (o.player == null || o.player <= tick)) return o;
        late.push(`stand-down ${home}/${o.room} seen t${commas(o.last)} (LastSeen clamped to the tick, Raiders as read)`);
        return { ...o, last: Math.min(o.last, tick), player: o.player != null && o.player > tick ? null : o.player };
      });
    // Its stamp is the clock's end: last seen plus `Tuning.ThreatMemory`
    // (a creep's life).
    const seenAt = (v) => v.until - CREEP_LIFE;
    const threatened = Object.entries(r.threatened ?? {}).filter(([, v]) => seenAt(v) <= tick);
    for (const [room, v] of Object.entries(r.threatened ?? {}).filter(([, v]) => seenAt(v) > tick)) {
      late.push(`Threatened ${home}/${room} seen t${commas(seenAt(v))}`);
    }
    const fought = Object.entries(r.fought ?? {}).filter(([, v]) => v.seen <= tick);
    for (const [room, v] of Object.entries(r.fought ?? {}).filter(([, v]) => v.seen > tick)) late.push(`Fought ${home}/${room} t${commas(v.seen)}`);
    const homeRaided = r.homeRaided != null && r.homeRaided <= tick ? r.homeRaided : null;
    if (r.homeRaided != null && r.homeRaided > tick) late.push(`HomeRaided ${home} t${commas(r.homeRaided)}`);
    const parts = [];
    if (outposts.length > 0) {
      parts.push("Outposts =", "    [");
      for (const o of outposts) {
        parts.push(
          `        { Fabot.Core.Observe.OutpostEpisode.RoomName = ${fsString(o.room)}`,
          `          Opened = ${fsInt(o.opened)}`,
          `          LastSeen = ${fsInt(o.last)}`,
          `          Expiry = ${fsInt(o.expiry)}`,
          `          Basis = StandDownBasis.${BASES[o.basis]}`,
          `          Stronghold = ${o.stronghold === true}`,
          `          Raiders = ${idSet(o.raiders)}`,
          `          PlayerSince = ${o.player == null ? "None" : `Some ${fsInt(o.player)}`}`,
          `          Chased = ${o.chased === true} }`,
        );
      }
      parts.push("    ]");
    }
    if (threatened.length > 0) {
      parts.push(
        "Threatened =",
        `    Map.ofList [ ${threatened.map(([room, v]) => `${fsString(room)}, ({ Until = ${fsInt(v.until)} }: Fabot.Core.Observe.ThreatLatch)`).join("; ")} ]`,
      );
    }
    if (fought.length > 0) {
      parts.push(
        "Fought =",
        `    Map.ofList [ ${fought
          .map(([room, v]) => `${fsString(room)}, ({ Seen = ${fsInt(v.seen)}; Squad = ${typeof v.squad === "string" ? `Some ${fsString(v.squad)}` : "None"}; Raiders = ${idSet(v.raiders)} }: FightLatch)`)
          .join("; ")} ]`,
      );
    }
    if (homeRaided != null) parts.push(`HomeRaided = Some ${fsInt(homeRaided)}`);
    if (parts.length > 0) {
      raidLines.push(`        ${fsString(home)},`, "        { Fabot.Core.Observe.RaidState.empty with", ...parts.map((l) => `            ${l}`), "        }");
    }
  }
  const body = [];
  if (armedNow.length > 0) body.push(`ArmedSeen = Map.ofList [ ${armedNow.map(([room, t]) => `${fsString(room)}, ${fsInt(t)}`).join("; ")} ]`);
  if (towered.length > 0) body.push(`Towered = Set.ofList [ ${towered.map(fsString).join("; ")} ]`);
  if (raidLines.length > 0) body.push("Raids =", "    Map.ofList", "        [", ...raidLines.map((l) => `    ${l}`), "        ]");
  const notes = [
    "// Heap only, never in Memory: Healers, ExitWatches and Sightings start empty.",
    ...(late.length > 0 ? [`// Stamped after the tick, not seated unless said: ${late.join(", ")}.`] : []),
  ];
  fields.push([...header, ...notes, ...body]);
  return fields;
}

/// Each creep's task at `tick` off the Transition log
/// (`Memory.fabot.observe.creeps`: `{ <creep>: { log: [{ t, v }] } }`): the
/// last matched or kept verdict at or before it, none after a release. A
/// creep the log has no task verdict for that early is unknown.
export function assignmentsAt(creeps, names, tick) {
  const known = new Map();
  const unknown = [];
  for (const name of names) {
    const log = (creeps?.[name]?.log ?? []).filter((e) => e.t <= tick);
    let state;
    for (const { v } of log) {
      if (v?.kind === "matched" || v?.kind === "kept") state = v.task;
      else if (v?.kind === "released") state = null;
    }
    if (state === undefined) unknown.push(name);
    else if (state !== null) known.set(name, state);
  }
  return { known, unknown };
}
