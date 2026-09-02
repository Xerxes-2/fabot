# Creep standing positions & traffic management — how the Screeps community solves it

Date: 2026-09-02. All claims verified against primary sources (official docs/engine constants, bot source code on their default branches, first-party blog posts) unless marked **unverified**. Motivating bug: a fabot creep finishes Harvest adjacent to a source, gets reassigned Upgrade, and upgrades **in place** because the tile is within range 3 of the controller — permanently squatting the mining spot. Standing tiles are not modeled anywhere in our system.

## Summary

- The community answer has **two independent layers**, and mature bots implement both:
  1. **Seat modeling** — walkable tiles adjacent to a source are a countable, reservable resource ("harvest positions"). Every mature bot precomputes them, caps miner count by them, and pins each miner to a specific tile.
  2. **Traffic management** — collect *all* desired moves for the tick, then resolve conflicts once per room (priority, swap, recursive shove), instead of issuing `moveTo` per creep in isolation. Canonical implementations: screeps-cartographer, sy-harabi's Screeps-Traffic-Manager, Overmind's Movement library, The International's `recurseMoveRequest`.
- **Static/container mining** dissolves the contention problem for sources entirely: one big miner parked on the container tile is the *only* seat consumer (5 WORK drains a source; containers are legal at **every** RCL — the gate is economy, not RCL).
- Our exact failure mode (worker performing a ranged action while squatting a range-1 seat) is solved in the wild two ways: (a) the assignment layer treats standing tiles as reserved resources (The International's `reservedCoords`), (b) the traffic layer shoves creeps that *can* keep working from farther away (Cartographer/Overmind shove creeps only to tiles that keep them within their task's work range — which requires the task to *know* its work range and target).
- For a task-based bot, the strongest precedent is Jon Winsley's role-free task system (same author as Cartographer): tasks carry `MinionIsNear(pos, range)` prerequisites, i.e. **position is part of the task contract**, and matching is Gale–Shapley — very close to fabot's Planner/Matcher split.

## 0. Ground truth from the game engine

Ranges (quotes from the official API source, https://github.com/screeps/docs/blob/master/api/source/Creep.md, rendered at https://docs.screeps.com/api/):

- `harvest`: "The target has to be at an adjacent square to the creep" (range 1).
- `upgradeController`: "The target has to be within 3 squares range of the creep." Same range 3 for `build` and `repair`.
- `transfer` / `withdraw`: adjacent (range 1).

Constants (https://github.com/screeps/common/blob/master/lib/constants.js):

- `SOURCE_ENERGY_CAPACITY: 3000`, `ENERGY_REGEN_TIME: 300` → an owned-room source yields **10 energy/tick**; `HARVEST_POWER: 2` → **5 WORK parts saturate a source** (Overmind spawns +1 spare: `miningPowerNeeded = Math.ceil(energyPerTick / HARVEST_POWER) + 1`, i.e. 6 WORK — see §1).
- `OBSTACLE_OBJECT_TYPES` = spawn, creep, powerCreep, source, mineral, deposit, controller, constructedWall, extension, link, storage, tower, observer, powerSpawn, powerBank, lab, terminal, nuker, factory, invaderCore. **Containers, roads, and ramparts are absent → walkable**; sources and controllers are obstacles. `TERRAIN_MASK_WALL: 1` marks unwalkable terrain.
- `CONTROLLER_STRUCTURES.container: {0: 5, 1: 5, ..., 8: 5}` — **containers are available at every RCL** (even 0). "Container mining is RCL3+" is folklore; the real gate is spawn/energy budget.
- So: **seats per source = adjacent tiles that are not `TERRAIN_MASK_WALL` and not covered by an obstacle structure** — between 1 and 8, typically 1–3 in practice.

## 1. How mature bots model mining spots ("source seats")

### Overmind (bencbartlett/Overmind)

`src/overlords/mining/miner.ts` (https://github.com/bencbartlett/Overmind/blob/master/src/overlords/mining/miner.ts):

- Seat counting caps spawning: `this.minersNeeded = Math.min(Math.ceil(this.miningPowerNeeded / miningPowerEach), this.pos.availableNeighbors(true).length)` — miners needed is work-parts math *capped by walkable neighbor tiles*.
- A single canonical standing tile `harvestPos` is computed: the container's position if one exists, else `calculateContainerPos()` picks the tile at range 1 to the source **on the shortest path to storage/dropoff** (`_.find(path, pos => pos.getRangeTo(this) == 1)`).
- Position is enforced exactly, not by range: `goToMiningSite()` checks `!miner.pos.inRangeToPos(this.harvestPos, 0)` and moves the miner onto that specific tile. Early-game mode (pre-container) relaxes to range 1 of the source.
- Modes: early (multiple small miners, drop/carry), standard (container: harvest + repair container in place), link (transfer to adjacent link when `carry.energy > 0.9 * carryCapacity`).

Historical note from the first-party blog (https://bencbartlett.com/blog/screeps-4-hauling-is-np-hard/): Overmind moved from rigid `miningGroup`s with dedicated haulers to a general logistics network (Gale–Shapley stable matching of requests↔transporters ranked by dq/dt); the rewrite cut creep count ~30% at equal CPU. Lesson stated: rigid role separation created inflexibility.

### The International (The-International-Screeps-Bot/The-International-Open-Source, branch Main)

The most explicit "seat as reservable resource" implementation:

- Precompute per-source seat lists: `findRemoteSourceHarvestPositions` in `src/room/room.ts` collects each adjacent position with `terrain.get(pos.x, pos.y) === TERRAIN_MASK_WALL` filtered out, **sorted by path length to the room anchor** (best seat first). (https://github.com/The-International-Screeps-Bot/The-International-Open-Source/blob/Main/src/room/room.ts)
- Reservation ledger: `roomManager.reservedCoords: Map<string, ReservedCoordTypes>` at room scope. `CreepOps.findCommuneSourceHarvestPos` (`src/room/creeps/creepOps.ts`) returns the creep's remembered seat from `CreepMemoryKeys.packedCoord` if set, else the **first seat not reserved as `ReservedCoordTypes.important`**, then writes the reservation: `creep.room.roomManager.reservedCoords.set(packedCoord, ReservedCoordTypes.important)`. Same pattern for mineral seats.
- The harvester role (`src/room/creeps/roleManagers/commune/sourceHarvester.ts`) then paths to that exact tile with `range: 0` along a precomputed per-source path, and supports being dragged there (`CreepMemoryKeys.getPulled` — the official `Creep.pull` mechanic, https://docs.screeps.com/api/#Creep.pull).
- The traffic layer respects the ledger: `findShoveCoord` in `src/room/creeps/creepPrototypes/creepMoveFunctions.ts` refuses to shove creeps onto reserved coords ("Don't shove onto spawning-reserved coords").

### TooAngel (TooAngel/screeps)

- One static `sourcer` per source in owned rooms, routed by a precomputed path system (`creep.memory.routing.targetId`); it harvests in place, transfers to an adjacent link in base rooms, and builds/repairs a container on remote sources (`src/role_sourcer.js`, https://github.com/TooAngel/screeps/blob/master/src/role_sourcer.js). Seat conflicts are prevented upstream by room planning/routing rather than arbitrated at runtime.
- Conflict handling is role-priority pushing in `Creep.prototype.moveCreep` (`src/prototype_creep_move.js`): when the next path tile is occupied, the blocker may be told to `move(direction)` (e.g. a sourcer/reserver pushes `universal` workers along); in the extreme case an upgrader blocked by a universal/sourcer/upgrader makes the blocker `suicide()`. (https://github.com/TooAngel/screeps/blob/master/src/prototype_creep_move.js) — evidence that ad-hoc pairwise rules get ugly fast.

### Community reference (Screeps wiki, screepers)

- Static Harvesting page (https://wiki.screepspl.us/index.php/Static_Harvesting): static harvester = "a harvester which does not move from the source after arriving"; container mining cuts drop-mining decay losses "by about 90%"; **the miner must actually stand on the container tile** or it degrades to drop mining ("a newly spawned miner stands on top of the assigned container before it starts mining, or it will turn into Drop Mining again"); link mining RCL5+ (3% transfer loss; `CONTROLLER_STRUCTURES.link` confirms 2 links at RCL5).
- Energy page (https://wiki.screepspl.us/Energy/): "harvesting generally starts out at low RCL with generic workers or harvester roles", then transition to static mining with dedicated miner + hauler.

## 2. Traffic management: the tick-end arbitration pattern

The shared shape across all modern implementations: **during the tick, movement calls only register intents; at end of tick, one resolver per room decides who actually moves**, using priorities, swaps, and (recursive) shoving. Issuing `moveTo` immediately per creep (what fabot does) is the pattern all of these exist to replace.

### screeps-cartographer (glitchassassin/screeps-cartographer)

README + docs (https://github.com/glitchassassin/screeps-cartographer, https://screepers.github.io/screeps-cartographer/):

- Loop bracketing: `preTick()` before your logic, `reconcileTraffic()` after; "Traffic management will only manage creeps that use Cartographer to move" — mixing with native `creep.move` breaks it.
- Every movement call takes a `priority` option: "Creeps with a higher priority will be given preference over creeps with a lower priority" when both want the same square. `blockSquare()` evicts creeps from a tile (spawn ramps, construction sites).
- Algorithm (from `src/lib/TrafficManager/reconcileTraffic.ts`, https://github.com/glitchassassin/screeps-cartographer/blob/main/src/lib/TrafficManager/reconcileTraffic.ts):
  - Each intent has a **list of candidate target tiles**, not a single tile. Creeps that registered no move are force-registered with `priority: 0` and `targets: [creep.pos, ...adjacentWalkablePositions(creep.pos, true)]` — i.e. an idle creep "wants" to stay put but *can* be displaced to any adjacent walkable tile. This is how idle creeps get shoved while "allowing them to keep range to a target".
  - Resolve priorities descending; within a priority, creeps with the **fewest remaining candidate tiles first** (most-constrained-first).
  - Swap handling: if the chosen tile is occupied by a creep that wants your tile and has `targets.length < 2`, it is `unshift`ed onto the intent stack for immediate re-resolution — pseudo-recursion via stack.
- CPU note: run expensive logic *after* `reconcileTraffic()` so movement can't be starved when the bucket runs dry.

### sy-harabi's Screeps-Traffic-Manager + write-up

First-party design narrative: "Journey to Solving the Traffic Management Problem" (https://sy-harabi.github.io/Journey-to-Solving-the-Traffic-Management-Problem/). Progression: naive swap/push → recursive shoving + priorities ("each new scenario required additional logic") → reframing as **assignment**: "assigning each creep to a specific position for the next tick", solved as bipartite matching with a modified Ford–Fulkerson: connect each creep to its current position, then for each move intent search augmenting paths; "if a path increases the number of fulfilled intents, we send flow along that path". Near-optimal, not guaranteed optimal (a validation loop "will use more CPU", so he skipped it).

Library (https://github.com/sy-harabi/Screeps-Traffic-Manager): `trafficManager.registerMove(creep, target, priority)`, **`trafficManager.setWorkingArea(creep, pos, range)`** (a displaced creep must stay within `range` of `pos` — the task's work area expressed to the traffic layer), `trafficManager.run(room, costs, threshold)` once per room per tick. This is the traffic manager The International's author community circulated; used by several current bots (**unverified** breadth of adoption).

### Overmind's Movement library

`src/movement/Movement.ts` (https://github.com/bencbartlett/Overmind/blob/master/src/movement/Movement.ts): per-role `MovePriorities` (manager 1, queen 2, ... transport 8, worker 9, default 10; lower = more important); `shouldPush()` — a creep with an active task is only pushed if it **can remain within its task's range** ("push creeps out of the way if they're idling" — idle creeps are always pushable); `pushCreep` → `getPushDirection`, plus `recursivePush` to chain-push when all neighbors are occupied. Pushing happens automatically during movement (no tick-end batch phase) — an older style than Cartographer's, but the key idea is the same: **the shove destination is constrained by the pushee's task target + range**.

### The International's in-tick recursion

`src/room/creeps/creepPrototypes/creepMoveFunctions.ts`: `assignMoveRequest` → `recurseMoveRequest` resolves chains: swap when `TrafficPriorities[this.role] + (needsResources ? 0.1 : 0) > TrafficPriorities[creepAtPos.role] + ...`, `shove(avoidPackedCoords)` picks a `findShoveCoord` avoiding reserved/spawn coords and recursively shoves the occupant.

### Baseline: Traveler (bonzaiferroni/Traveler)

The 2017-era standard (https://github.com/bonzaiferroni/Traveler, `Traveler.ts`): `ignoreCreeps: true` by default (path as if creeps don't exist → single-lane movement works when roads are clear); stuck detection = same coord as last tick (or bouncing on an exit tile); after `DEFAULT_STUCK_VALUE = 2` stuck ticks, ~50% chance to repath with `options.ignoreCreeps = false; options.freshMatrix = true; delete travelData.path`. **It never pushes or swaps** — blockers become `0xff` in the cost matrix and are pathed around. Its rework lineage: NesCafe62/screeps-pathfinding (https://github.com/NesCafe62/screeps-pathfinding) adds real traffic features — push/swap, `priority`, and `getCreepWorkingTarget` so pushed creeps "stay in range of their target if working".

## 3. Which pattern at which stage

- **RCL1–2, generalist workers, no containers** (fabot today): the community went through the same phase. Jon Winsley (Cartographer's author, task-based bot) describes switching "from swarm mining at low levels (sending lots of small minions to harvest, then build/repair/upgrade) to drop mining (filling the Franchises with enough small dedicated Salesmen to tap the source, dropping the energy on the ground for any minion to collect)" already at RCL1/2, with 2-WORK miners (https://www.jonwinsley.com/notes/screeps-logistics-overhaul). At this stage a full traffic manager is overkill; **seat counting + seat reservation is the cheap, sufficient fix** (a handful of creeps, 1–3 seats/source). Cost: one terrain scan per source (cacheable forever — terrain is static) plus a per-tick `Map<packedCoord, creepId>`.
- **Container/static mining** (economically viable ~RCL2–3, legal at any RCL per `CONTROLLER_STRUCTURES.container`): one 5–6 WORK miner parked on the container tile per source. This *removes* mining-spot contention by construction — one seat, one permanent occupant — and is what Overmind/TooAngel/International/wiki all converge on. Prerequisite: room planner chooses the container tile (Overmind: range-1 tile on the path to storage; International: seat sorted by path distance to anchor).
- **Dense rooms / many creeps (RCL4+)**: tick-end traffic arbitration becomes worth its CPU — remote haulers, upgrader clumps at the controller, fastfiller layouts. Cartographer and harabi both stress running it once per room per tick and being mindful of bucket; harabi's README claims "near-zero CPU overhead" via hash-based shuffling (**unverified** — README claim, not independently measured).
- Sequencing evidence: every mature bot has seat reservation *before* (or independent of) a general traffic manager; Traveler-style bots lived for years on "ignore creeps + repath when stuck" alone, which is viable exactly as long as stationary creeps never sit on chokepoints — i.e. as long as standing tiles are managed by *some* other layer.

## 4. Task-based (role-free) precedents: the task carries the position

- **Jon Winsley's Grey Company** (task-based, role-free, same failure domain as fabot):
  - Tasks are `TaskAction`s with `TaskPrerequisite`s, and **position is a prerequisite**: `new MinionIsNear(this.site.pos, 3)` on a Build task; unmet prerequisites expand into sub-tasks (`toMeet()` returns e.g. a `MoveAction { destination: RoomPosition }`) — so movement is an explicit planned step, not an executor fallback (https://www.jonwinsley.com/notes/screeps-task-management).
  - Matching is Gale–Shapley over predicted costs using a `SpeculativeMinion` that simulates position/capacity along the task path (he credits bencbartlett's logistics article).
  - Lesson he states: exhaustive task-tree matching is CPU-heavy; "Managers should try to limit requests, either by issuing tasks to minions directly if the task is well understood" — his miners ("Salesmen") are assigned **directly to franchise seats**, bypassing the general matcher (https://www.jonwinsley.com/notes/screeps-remote-mining-hurdles, https://www.jonwinsley.com/notes/screeps-logistics-overhaul).
- **Overmind**: a Zerg's task carries target position + work range, and the Movement layer consults it (`shouldPush` only displaces a working creep to tiles that keep the task valid). The task ledger and the traffic layer share the same position vocabulary.
- **The International**: not task-based, but its `reservedCoords` room ledger is the cleanest "standing tile as first-class resource" precedent: assignment writes a reservation; movement/shoving reads it.
- Converged lessons: (a) some layer must **own** the standing tile — either the task (Winsley, Overmind) or a room-scope reservation ledger (International); (b) seat count is a **capacity** on the task, bounding how many creeps the matcher may assign; (c) any shove/yield mechanism must be constraint-aware (keep the pushee within its work range), which is only possible if tasks expose target + range to the movement layer.

## Implications for fabot (Planner → Matcher → Executor)

Ordered adoption plan, cheapest first:

1. **Seat model + reservation ledger (fixes the observed bug; do now).**
   - Planner: per source, compute `seats = adjacent tiles with terrain != TERRAIN_MASK_WALL (and no obstacle structure)` — static, cache in room memory/heap. Harvest task capacity = `min(seats, ceil(10 / (2 * workParts)))` per Overmind's formula.
   - Introduce a per-tick (or persistent) `reservedTiles: Map<packedPos, creepName>` at room scope, International-style. Harvest assignment reserves a concrete seat tile; the Intent becomes "harvest source S **from tile T**".
   - Give ranged tasks (Upgrade/Build/Repair, range 3) a standing constraint: **valid standing tile = within range of target AND not in `reservedTiles`**. The Executor, on `OK` while standing on a reserved tile it doesn't own, issues a one-step move to an adjacent valid tile. That alone kills the "upgrade in place on the mining spot" failure: the tile is finally represented.
2. **Static/container mining at the container stage.** Planner picks the container tile (Overmind heuristic: the range-1 tile on the path toward spawn/storage; International: seat with shortest path to anchor), emits a Build(container) task, then a dedicated `HarvestStatic` task whose standing tile *is* the container tile with capacity 1, plus Haul tasks. Contention for that source disappears by construction; wiki warns the miner must actually reach the container tile before harvesting.
3. **Tick-end move arbitration (when rooms get dense).** Restructure the Executor so `moveTo` never fires directly: actions emit **move intents** `(creep, candidateTiles, priority, workingArea?)`; a per-room resolver runs after all intents are collected. This is a *better* fit for fabot's Intent architecture than for role bots — movement stops being incidental and becomes just another arbitrated intent. Port either Cartographer's semantics (priority desc → most-constrained-first → swap via stack; idle creeps auto-registered with `[own tile + adjacent walkable]`) or harabi's augmenting-path matching; both need the task's target+range (`setWorkingArea`) to know where a displaced creep may stand — which step 1 already put on the task.

Step 1 has no library dependency and is a few dozen lines of F#; step 3 is the only one with real algorithmic surface, and both reference implementations (reconcileTraffic.ts, Screeps-Traffic-Manager) are small enough to translate rather than bind.

## Open questions (unverified)

- CPU figures: harabi's "near-zero overhead" and Cartographer's real per-creep cost are README/blog claims; no independent benchmark checked. Measure after step 3 with `Game.cpu.getUsed()`.
- Cartographer's docs site moved (glitchassassin.github.io → screepers.github.io/screeps-cartographer); the per-page URLs (e.g. `pages/trafficManagement.html`) 404 — content above verified from the site root and the source files instead.
- TooAngel's sourcer-count config (`amount` arrays keyed by energy capacity) was read via summary, not line-by-line; treat the "1 sourcer per owned source, more only for remotes" reading as approximate.
- Whether The International still uses its in-repo `recurseMoveRequest` vs an external traffic manager on its current development branch (repo has multiple branches; `Main` was read).
- Overmind's early-mode multi-miner seat handling (pre-container, range-1 relaxation) — read via summary of `miner.ts`; exact tie-breaking between two early miners contending for the best seat not traced.
