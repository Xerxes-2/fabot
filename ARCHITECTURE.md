# fabot v2 — architecture

This is the one document agents read. It says what the modules are, where every
piece of cross-tick state lives, and the few rules that keep it that way. Anything
not here is decided by the code, and the code wins over any other document.

Items marked ❓ are open: the owner decides them before the module is started.

## 0. What v2 is

A Screeps seasonal-server bot in Rust, compiled to WASM and bundled into one JS
file, ported tick for tick from an F#/Fable tree (main's history, before
`14c9617b`). v2 is a
port of v1's economy and a redesign of everything that assumed a unit smaller
than the world.
v1 taught us four things this document is built around:

1. **The unit is the world.** v1 was written for one colony; the assumption
   broke within days and was never replaced, only patched: per-colony decisions
   with cross-colony fixes bolted on (energy aid, shared outposts, a world-level
   fight grafted onto colony views), and seventeen outposts declared by hand in
   source, one commit per expansion. v2 decides once for the world; a colony is
   a resource, and every room, ally and outpost is data.
2. **Stateless re-derivation does not do fights.** A fight is a commitment; a
   decision re-made every tick oscillates, and every patch for the oscillation
   (cooldown, latch, grace, hold-while) is hidden state. v1 ended with ~25
   scattered stores of fight state and eight different "keep" rules. v2 has one
   fight object with a lifecycle.
3. **Pricing belongs to commitments, not to ticks.** v1's best idea — cast the
   cheapest body set that wins the exchange (one ranger kiting a big melee) —
   survives as a pure function called *when a commitment is made*, and its
   predictions are checked against what happened (§10.1).
4. **Nothing is proven by a unit test.** v1 had 154k lines of tests, two tests
   per line of source, and shipped incidents weekly, because scenario
   assertions pin what the bot did, not whether it worked. v2 verifies
   outcomes in a real engine (§10) and keeps unit tests for arithmetic.

The end is set once, at the farthest (v1's end moved several times, and each
move left a layer): many colonies decided as one world, each at RCL8 with
every structure placed and run, every engagement kind in §6.2 fought, within
the CPU limit. Milestones (§13) order the way there; none is an end.

Non-goals: multi-shard, generality across servers, a plugin system, any
abstraction with one caller.

## 1. The tick

```
Game ──World.ofGame──▶ World ──decide──▶ Intents ──Executor──▶ Game
            ▲                    │
         State (Memory + segments + heap, §4) ──── writes back ────┘
```

- `World.ofGame` reads the API once; nothing below it touches `Game`.
- `decide : State -> World -> Intents * State'` is pure at this boundary and
  **deterministic**: no clock, no unseeded random, no reference to anything
  but its arguments. A recorded (State, World) replays the exact decision
  locally; that is the debugger (§10.4) and the benchmark (§9.1) at once.
  Inside, it is written imperatively (§9.2).
- Modules emit spawn asks and move wishes as Intents; `Tick` settles them
  after every module has run: one spawn queue per room, one movement
  arbitration per room over every module's wishes.
- `Executor` turns Intents into API calls; it never decides. Intents are typed
  by **actor** — creep, power creep, tower, lab, terminal, observer, spawn,
  controller — not creep-first with the rest bolted on.

**There is no full/light tick.** v1 alternated whole ticks and then needed 300
lines of "what forces a full tick"; those triggers were really each module's
invalidation rule, scattered. Each module names its own **key** — the facts its
last decision depended on — and re-decides only when the key changes or its due
tick arrives; otherwise its standing output is replayed:

| Module | Decision | Key |
|---|---|---|
| Layout | the plan | census signature |
| Economy | the slots | none — slots are re-derived each tick; a body keeps its slot and its task |
| Fight | an engagement's plan | an engagement event (§6.2) |
| Strategy | open/close operations, declarations | intel changed, an engagement closed, a due tick |
| Move | a path | the next step blocked |

A creep therefore carries a Task and a path and walks the next step each tick.
CPU pressure is one knob: a low bucket stretches the due intervals. Nothing else
degrades.

**Within the tick, nothing is derived twice.** v1 let every consumer compute
its own facts from the view, found the same flood run eighteen times a tick,
and patched it with memos keyed by reference. v2 stages the tick:

```
World ──▶ Derived ──▶ decisions ──▶ Intents
          (once)      (read Derived; never World)
```

- `Derived` is one record built once per tick: hostile reach per room, each
  creep's `Fighter`, the Tracker's groups, store lines, census — whatever more
  than one module reads. A field that may go unused is `Lazy<_>`: computed at
  most once, only if asked. `Tick` builds it before any module runs and hands
  it to each in `Context`; it lives in `core/tick.rs`, below `Fight` and
  `Economy`, so both read it without importing each other.
- Decision functions in `Fight`, `Economy`, `Strategy` take `Derived` (and
  `Atlas`), not `World`. What a function reads is its signature; a `World`
  parameter below the deriving stage is a review flag.
- Tiles of one room are a `TileSet` (a bit a tile) and a value per tile a
  `Table<T>` (indexed `y*50+x`), never a `BTreeSet<Tile>`/`BTreeMap<Tile, _>`:
  a room is 2,500 tiles, the bitmap needs no allocation and walks in the same
  order. `from_exits` as a `BTreeSet` alone was a third of fight's tick.
- **One cache exists**: `Atlas`'s flood table of goal-seeded reverse
  floods, keyed by (room, goal tiles and range — or the exit toward the next
  room —, footprint, pace class: the body's step ticks on road, plain and
  swamp; a seat's price follows from swamp's; the danger it walls, which
  moves only where a hostile's own tile already changes the weights),
  counting hits and misses. A
  room's floods outlive the tick while its weights are unchanged (owner's
  decision): within one tick half the floods repeated, across ticks nearly
  all, and a flood is a pure function of the weights, so a kept one is the
  fresh one and a heap reset only re-floods.
  Room routes are searched once a tick into `Derived`, one tree from each
  room a spawn or body stands in (owner's decision): Economy prices every
  body against every offer, so a search per query cost thousands a tick at
  six colonies, and none is kept across ticks. Floods are
  the only demand-driven derivation. Any other memo, `HashMap` cache or
  lazy cell outside `Derived` is an architecture change (`AGENTS.md`).
- **Counts are a gate** (§9.3): floods, Atlas misses, `Exchange.outcome` calls
  and intel reads per tick on a replay capture are deterministic numbers with
  ceilings; a change that doubles the floods fails `jj ship` without anyone
  profiling it.

**A cold start** (heap empty after a reset or deploy) rebuilds caches from
Memory and segments over as many ticks as the bucket allows, economy first; a
cold tick never spends more than ❓ 60% of the limit.

## 2. Units

- **The world** is the decision unit. One `decide` call, one set of
  engagements, one Strategy.
- **A colony** is a resource: a controller, its spawns, its storage, its
  terminal. It owns no decision. "Which colony serves this" is the nearest by
  walk with a storage, cut off by room-graph hops (§3) for CPU.
- **A room** is a fact in `World` and a row in `intel`; its role (home,
  outpost, errand, avoided, targeted) is data Strategy writes, never a
  constant in source.
- **A creep** is in exactly one of `Fight` or `Economy` each tick (§6.2).
- **Fault isolation is per module**, not per colony: a throw in `Fight` costs
  the fight this tick and is written to `observe.faults`; the economy's intents
  still go out. `Main` wraps each module.

## 3. Modules

| Module | Owns | Reads | Writes | v1 origin |
|---|---|---|---|---|
| `Bindings` | the Screeps API surface | — | — | port verbatim |
| `Engine` | formulas, and the server constants the game exposes no global for | — | — | port verbatim |
| `World` | this tick's facts, one record; last tick's strikes and structures destroyed off the event log of each room we own, reserve or stand in (and a drain's); `rules`, the engine tables the game exposes as globals (`CONTROLLER_STRUCTURES`), read each tick since a season's server may override one | Game | — | port, trim |
| `Atlas` | geometry in two layers: the **room graph** (a node a room's region, left by a stretch of border both rooms open, priced by the flood across it round every tile a keeper strikes; grown from the target's room, so every room's way on is the same whichever room asks and a body never walks back; a keeper room whose lairs no intel places taken only past every other; rooms to avoid) and the **tile grid** per room (terrain, floods, seats, work areas, first steps). Every tile query takes a **footprint** (1×1 today, 2×2 for a formation) | World, `terrain` for rooms out of sight, `intel.rooms` for avoided and keeper rooms | flood table, kept across ticks per room while its weights hold; out of sight, a room's grid is what stood there when last seen (else its terrain) and is kept, floods and all under the same bound, while it stays so (the owner's call: part of the flood table, not a second cache); the rooms whose lairs intel reads otherwise than their names, rebuilt each tick | port, add the graph layer, widen the query |
| `Economy` | slots, their casts and tasks, labs, terminals | World, Atlas, declarations, `State.Economy` | tasks, casts, `State.Economy` | port, trim (§5) |
| `Layout` | construction and tower plans | World, Atlas, declarations (outposts' containers and roads; each colony's plan: built to its `target`, nothing while abandoning) | sites, tower intents | port |
| `Exchange` | who wins: pure fight arithmetic | bodies, terrain | — | port from `Facts.fs:2883–4056, 1038–1582`, `Fighter.fs`, `Arrival.fs`; narrow the input |
| `Fight` | tracker, engagements (one kind per file), casting | World, Atlas, Exchange, declarations (the operator's tactic rules, §6.3), `State.Fight`, `intel.rooms` (a patrol's staleness) | fight intents, casts, declarations, `State.Fight` | **new** (§6) |
| `Tactics` | what one fight body does this tick (§6.4): strikes, steps, the Hunt score, heals, and which tactic a kind picks (§6.3); it imports no Fight | Atlas, Move, the `Sight` and plan Fight hands down | creep intents, wishes | **new**, split from Fight |
| `Strategy` | intel; declarations (homes, outposts, errands, avoided rooms, allies); which operations to open; force allocation across engagements | World, Derived, Exchange, `State.Intel`, `Season`, the open engagements' rooms and pending casts (a colony's reserve) | declarations (a `ColonyPlan` per colony, under the operator's rows), opens/closes engagements, `State.Intel` | **new** (§7); v1 `Operations.fs` bidding informs it |
| `Season` | this season's rules: what scores, its structures, its NPCs, its map quirks | World | declarations to Strategy, season intents | **new**, disposable (§8) |
| `Move` | per-room movement arbitration, run by `Tick` over every module's wishes, and the step functions (chase, kite, flee, formation) | Intents, Atlas | move intents | port `Resolver.fs`, drop fight precedence |
| `Executor` | API calls | Intents | Game | port |
| `Observe` | what the operator reads: health, faults, per-module CPU, engagements, exchange error, the metrics (§10.5) | everything; the metrics: World, assignments, engagements and their casts, the tick's intents and refusals, phases, the declared outposts | `State.Observe`, `metrics` | rewrite small |

Dependency direction is top to bottom. `Fight` and `Economy` do not import each
other; `Season` is imported by `Strategy` only. The seam between them is a
**declaration**: a record Strategy or an engagement writes ("this room may be
mined", "pick up this pile", "this room is avoided", "a claimer is wanted at
X") and Economy or Fight reads. Declarations flow down the table, never up.

**Line ceilings** (`scripts/ceilings.json`, checked by `npm run ceiling`, never raised): `Fight` core
2,500 plus 400 per engagement kind, `Exchange` 2,500, `Economy` 6,000,
`Strategy` 2,000, `Move` 2,000, `Atlas` 3,500, `Season` 1,500, `Observe` 1,000.
❓ confirm once the port lands.

## 4. State

Every value that crosses a tick is listed here, with one owner. A reader that is
not the owner reads it through the owner's module. Adding a row is an
architecture change, not a fix.

**Where it lives.** `Memory` is parsed by the engine on first access every tick,
so its size is a per-tick tax: it holds only what every tick reads. Everything
read on a key change, on demand or by the operator goes in **segments**
(`RawMemory.segments`, read when asked). The heap is a cache and is rebuildable
from the other two (§1, cold start).

Everything v2 keeps in Memory is under `Memory.v2`; v1's keys are dropped on
the first tick that sees them, so the `fabot` blob a takeover finds is parsed
once rather than every tick after (a rollback to v1 finds it gone and builds it
fresh). Segment ids: 1 observe, 2 intel, 3 metrics, 10–14 the
recorder's ring; the rest are assigned here as their stores land.

| Store | Shape | Owner | Lives in | Why |
|---|---|---|---|---|
| `assignments` | creep → `slot|task` | Economy | Memory | a body keeps its slot; its task held while it can be done |
| `fight.engagements` | id → Engagement (§6.2), with the tick it opened, never moved | Fight | Memory | the commitment; read every tick; not the offers (§6.3); a home's deaths since it opened are its raid's |
| `fight.groups` | id → Group (§6.1) | Fight | Memory | hostile identity across ticks |
| `fight.casts` | pending casts by engagement | Fight | Memory | never cast twice |
| `declarations` | homes, outposts, errands, avoided rooms, allies, enemies, claims, the operator's tactic rules; a `ColonyPlan` per colony (stage, build target, reserve) and `flows` (the lab colony's compound stocks), where abandon and labs live, so a colony's lifecycle and what moves between rooms are one plan Economy and Layout read rather than per-purpose fields | Strategy | Memory (the operator's); Strategy's plans rebuilt each tick | what the world is for us, as data |
| `switches` | name → bool: `false` skips a module, `true` opts in (`recorder`) | Main | Memory | turn a module off live |
| `recorder` | the last ticks' read pass (`app/read.js`), Memory and intel rows, deflated; a tick over a segment spans several, so the ring keeps fewer | Main | segments 10–14 | the capture (§9.1) |
| `economy.plan` | per colony: layout memo, census signature | Economy | segment | read on key change |
| `intel.rooms` | room → last seen, owner and level, reserver and the tick its hold lapses, whose WORK body stood there, its keeper lairs and the resource each guards; by username, never stance | Strategy | heap, segment 2 every 100 ticks, the recorder's capture | a declared room out of sight is served as it was last seen (§7.1) |
| `engagements.log` | closed engagements with prediction vs outcome | Observe | segment | §10.1 |
| `observe.*` | health, faults, per-module CPU, counters, this tick's tactic offers (§6.3), the metrics past their threshold | Main, then Observe | segment 1 | operator; calibrates the benchmark |
| `metrics` | the open window's sums; each creep's last task and load, each site's last progress and when, each colony's last controller progress, last tick's engagements and each room's last close; each body's last tile and the one before, its stuck run, the room it last left and when, its ticks to live, whether it was wounded and what last struck it and when, its last energy draw not yet delivered; the closed windows | Observe | heap | what a tick is compared against; a reset loses the open window and the bodies' tracks and restarts the sites' ages |
| `metrics.windows` | 100-tick windows aligned on the game tick, a row a room and one for the world, the last 150 (fewer past the segment's 100 KB) | Observe | segment 3, written as a window closes | `observe metrics`, the soak's `metrics.jsonl` (§10.5) |
| `scouts`, `preyed` | the declared rooms a scout serves, with their rows' age; the rooms an enemy owns, reserves or works, stalest first | Strategy | heap, this tick only | Economy casts scouts, Fight's sweep candidates; derived from intel each tick |
| `sightings` | room → each armed hostile's or Invader core's id → the tick it is gone (seen tick + its ticksToLive, or the core's collapse; a deploying core's deploy plus the longest collapse) and whether it is armed; a room in sight holds only what stands there now. A level-0 core is unarmed: it keeps its core engagement open but stands nothing down | World | heap | vision grace; when a stood-down outpost is clear |
| `standing.*` | per module: last output and the key it was decided on | each module | heap | the replay (§1) |
| `terrain` | room → its raw terrain buffer, read the first tick it is seen and, a few a tick nearest our spawns first, for every room out to `SIEGE_HOPS`; room → its `getRoomStatus`, for each room in sight, beside one or declared | World | heap | terrain never changes, a status only when a novice or respawn area ends (a reset reads it afresh), and reading either is calls across the seam (§9.2); a room whose status is not our spawns' is never routed into or served, since the engine leaves a creep on the exit tile into it; a room with no terrain read is never observed, since one off the shard refuses the observer |

Rules:
- Memory and segments are written once a tick by `Main`, from `State'`. No
  module calls `Memory.*` or `RawMemory.*`.
- No `thread_local!` or `static` outside the driver (`app/driver.rs`) and the
  heap rows above. A cache inside a function's tick scope is fine.
- No room name, player name or object id in source: they are `declarations`.

What v1 had and v2 does not: `danger` (7 parts), stand-down, raid episodes,
exit watches, head latches, exit holds, healer/raider recall, quarries,
refusals, economy carries, camp prices, room latches, roster, hand-declared
outposts. §6 and §7 replace them.

## 5. Economy (port)

A colony's flows become **slots**: each slot casts one body and is the
work that body does, so what is cast and what is offered are one list
(the quota and the pool modelled one economy twice). Its flows: each source's
dig, one carrier pool for its own and its outposts' dig, the ring, the
controller's drink and the ore, a reserver per outpost, workers for its sites
and repairs, and upgraders on the buffer's seats. Casts are slots less the
bodies that stand; a body keeps its slot until the slot goes or the body
dies, so an arriving body never re-matches the others. A hauler is 2:1 or
1:1 CARRY:MOVE, whichever delivers more over a life per energy on its route
(`bodies::carry_per_move`): any ratio between moves no faster than 1:1 off
road. The home's carriers fetch only at home, so the ring never waits on a
body gone to an outpost. The stock is drawn only for its sinks' room past
the loads at home bound for them, which book first: a load never goes back
into the store it came from. A remote is served by the nearest colony
with a storage; a stood-down outpost keeps its slots and its bodies wait at home. The outposts' carriers
take at most half the spawns' time, or a far ring of outposts casts nothing
but haulers; the home's own carriers queue ahead of a fight's casts, since
they refill the bank those are paid from.

The spend follows the **surplus**: dig less the fixed bodies' upkeep, plus
the stock past the reserve over a creep life. It is negative below the
reserve, so spending below it cannot be written, and spending it draws the
stock to the reserve and no further. Sites and repairs take half the income
(all of it before a colony has upgraders, all past a capped controller's
take); the stock past the reserve upgrades.

**The objective is long-run growth, by simple rules first.** A slower start
that ends higher is the better economy; an early regression is accepted when
the long soak (§10.2) improves. Structures are built as their level allows, in
Layout's plan order; bodies are sized to the bank. A rule is replaced by a
priced decision only when a soak shows it losing: a pricing of every
investment by its return was built and measured, cost twice the CPU and 580
lines for no gain, and was not merged.

**A colony keeps a reserve before it spends the surplus**, and Strategy
declares it in the colony's `ColonyPlan` (`strategy::reserve`). Threatened
(a rival player's room within three rooms, a fight open at home, or a fight
cast it owes) it is every tower the level allows firing for n creep lifetimes
(towers × TOWER_ENERGY_COST × CREEP_LIFE_TIME × n), n declared live by the
operator (`observe declare reserve n`), 2 by default (30k at RCL4, 180k at
RCL8); otherwise a third of it. The fight casts it owes are added: a committed
cast is paid before any surplus, as the old bank's order had it. Owner, 2026-11-10: the full reserve cost a
quiet young colony a lifetime of upgrading (main kept its RCL4 stock under
it anyway), and the season's fights come from neighbours, which intel sees.
Open-source bots keep 75k–150k at RCL7–8.

Port with these cuts:
- No fight tasks (`Guard`, `Defend`, `Assault`, `Flee`), no Safety band. v1's
  flee survives in `Move`: `Derived.danger` (each hostile's `hostiles::reach`,
  an enemy tower's falloff range; none in a room we own, whose refills
  feed the towers) is walled for a body outside a fight: it leaves a room
  by another neighbour's way when the graph's is walled, and waits clear of
  it while its target or every way on stands in it. Reached in its target's
  room it walks to the nearest clear tile; reached on its way through it
  leaves by the graph's way, since a hunter follows it anywhere but across.
- No `Facts.fs`. Its ~1,100 economy lines (task ids, stages, hunger, perimeter)
  move into `Economy`; the rest is fight and does not come.
- Outposts, errands and the rooms a colony works come from `declarations`, not
  from `Colonies.fs`.
- Every `#NNN` comment and every branch that exists for one live incident is
  dropped on the way over. A port that keeps a patch names the invariant it
  protects in one line or leaves it behind.

Economy serves declarations it did not make: a `Mine room` from `KeeperDuty`, a
`Pickup pile` from `PowerBank`, a `Boost compound × n` from a `Siege` (labs), a
`Claim room` from Strategy (the claimer and pioneers are economy bodies until a
spawn stands).

The cluster the operator declares (`declarations.labs`: a home and the stock it
keeps of each compound) reacts toward those targets: `labs::plan_for` picks the
reaction, its reagent pair and its outputs from the labs' tiles alone and names
every fill and drain, re-derived each tick from the stocks so no plan is a
state. A lab a cast waits at is out of the plan (§6.5) and what that cast's
parts still ask is `Ground::boost_lab`: the compound and the energy, drawn from
the home's stores by the pool as any other load.

A colony declared `abandon` (owner, 2026-10-18: given up once its mineral is
dug out) digs, builds and upgrades nothing past its downgrade deadline, casts
only its supply floor, and ships what its storage, terminal and labs hold to
the colony whose terminal has most room, energy last. The owner destroys its
structures and unclaims by hand: `destroy()` needs our controller, and an
owned structure left after the unclaim keeps the room in sight and in the
world read every tick (`observe health` prints both commands once it is empty).

## 6. Fight (new)

### 6.1 Tracker — who is out there

```fsharp
type Group = { Id: int; Members: Set<string>; Room: string; LastSeen: int; Centre: Pos }
val step : grace: int -> held: int -> owned: Set<string> -> prev: Group list
           -> hostiles: Creep[] -> tick: int -> Group list * Event list
```

- Armed hostiles are clustered by connected components at Chebyshev distance ≤ 4
  (ranged reach + one step), across a seam when the tiles are adjacent.
  Unarmed hostiles are tracked as singletons. Armed is ATTACK or
  RANGED_ATTACK, HEAL or WORK beside one in the room, and WORK alone in a
  room whose controller is ours (owner's call).
- Identity: next tick's clusters match previous groups by largest member
  overlap. A split keeps the id on the largest fragment; a merge keeps the older
  id. Both are **events**, as is `Left`: a member seen nowhere now (dead, or
  out of sight) while the rest of its group stands. A new id is past every id a group or an
  engagement holds (`held`, derived, not stored), so no id is reused while
  anything holds it.
- A group none of whose members is visible is `Lost` from `LastSeen`; dropped
  after `Tracker.lostGroupTicks` (30, the owner's starting value). This is the only vision grace in the
  fight.
- Pure; property-tested: ids stable under a permutation of the input; no group
  wider than the cluster diameter; no id reused in one tick.

### 6.2 Engagement — what we committed

```fsharp
type Target = Group of int | Room of string | Structure of string | Lairs of string
type Kind = Defend | Claim | Siege | Sweep | KeeperDuty | PowerBank | Stronghold
type Engagement =
    { Id: int; Kind: Kind; Target: Target
      Ours: Set<string>; Asked: Cast list      // committed bodies, and the ones still in an oven
      Plan: Plan                                // tactic, the offers behind it, kill order, formation — set at open (§6.3)
      Predicted: Outcome                        // what Exchange said at open; checked at close (§10.1)
      Phase: string; Since: int                 // the kind's own phase names
      Ignored: int                              // bouncer trips his towers left untouched (§6.2)
      Budget: Budget                            // losses, energy, deadline — before we disengage
      Priority: float                           // Strategy's, for force allocation (§7.3)
      Declares: Declaration list }              // economy work this engagement has opened
```

The record is one; the **phase machine is per kind**, one file each under
`Fight/Kinds/`, 400 lines each. v2.0 implements `Defend`, and `Defend` is the
template every later kind is written against: the simplest kind, with live
samples every day, and where v1's oscillation bugs lived.

Common to every kind:
- `Ours` is committed: a creep in an engagement is **not** in the economy pool
  and is not re-matched. It leaves only when the engagement releases it or it
  dies. A long-lived kind rotates bodies through the generic event *body
  expiring within walk + spawn time* → ask a replacement.
- Transitions fire on **events** — group split/merge/newcomer/left/lost, body died
  or landed, target gone, budget crossed, deadline, another engagement merged
  into this one, Strategy re-prioritised, the affordable set grew (the largest
  energy capacity among the colonies that may cast for an unanswered
  engagement, a home's or an outpost's, rose past what it was priced at) — never on a clock and never on a
  re-priced number. This is what removes the latches.
- A group that appears inside a room an engagement targets belongs to that
  engagement; only a group outside any targeted room can open a `Defend`. Two
  engagements whose groups merge are merged (the older survives).
- An engagement Strategy cannot staff (§7.3) is **starved**, not forming
  forever: `Defend` falls back to towers and ramparts and is marked
  `unanswered`; a `Siege` stays open `unanswered` until the bank grows. A home's unanswered `Defend` also
  spends the shard's one safe mode where no tower stands, or its raid reaches
  a hit spawn, tower, storage or terminal or half the home's spawns or
  extensions, or strikes haulers or has killed them since a player's raid
  opened and leaves the home's haulers under the CARRY that refills it, as a
  claimer in reach of the controller does.
- Lives in Memory. `observe` reads it directly: the engagement *is* the raid log.

`Defend` (v2.0):
- Opened by Strategy when a group threatens: it stands in a room we hold or
  work, or its centre drifts toward one. A level-0 Invader core strikes nothing
  but holds its room's controller: the fight sees it as an armed hostile
  without a body, which no exchange prices and the biggest melee body the bank
  buys razes (v1's guard), so an engagement needs no structure target yet.
- `Forming → Approaching` when every asked body stands (or the group reaches a
  held room first and the standing bodies win alone).
- `Approaching → Fighting` when any of ours is in reach.
- `Fighting → Pursuing` when the group leaves the room and the plan says chase.
- `* → Disengaging` when losses exceed the budget, or a group event re-runs the
  exchange and we no longer win, or the group is `Lost` past the grace.
- `Disengaging → closed` when ours are home or the group is gone.
- A home's `Defend` never disengages (owner's decision): where an outpost's
  would, it stops asking for bodies, its standing bodies hold on our
  ramparts and the towers carry on; it is `unanswered` and closes only when
  the group is gone.
- v2.0's record (`fight/shape.rs`): the asks live in `fight.casts`, `Kind` is
  `Objective`; `Predicted` is the outcome priced each time it is answered
  after standing unanswered. The plan chases while the group stands in
  another room we hold or work.

The other kinds, for the record's sake:
- `Claim`: Strategy picked a room (§7.2); a claimer walks, claims; pioneers are
  economy bodies on a `Build spawn` declaration; closes when the spawn stands.
  Groups in the room are its targets.
- `Siege`: muster → travel (formation) → breach (dismantle/attack under tower
  fire; `Exchange` prices towers and ramparts) → clear → done. Opened on a
  room in `declarations.sieges`, kept in sight by an observer; its target is
  a `Structure` of the towers and the economy, the one its charged towers
  strike least first and of those the one whose raze costs the rival most
  (owner, 2026-10-19), then an enemy's controller, which the biggest CLAIM
  body the bank buys taps as each upgrade block lapses (v1's declaim); it
  closes once neither stands. A safe mode refuses its breach but not its edge,
  so the siege opens on one whose towers still hold energy and drains them
  while it runs, where the pairs its bank buys out-heal the volley they draw;
  a safe mode with no such gate stands the set off as it did before the drain
  (owner, 2026-10-18: a safe mode is spent, not blocked); the end of its drain
  sends the set in on the breach plan, not on `under_fire`. The breach is
  derived, not stored:
  `Exchange.breachPlan` finds the cheapest way in by any exit (the colony's
  walk onto its stretch and the queue through its width, plus each barrier's
  hits over the set's dismantle rate less the repair, no tile where the towers
  outstrike the heal the medics seated there pool), at open and on events,
  and the plan's order is its barriers; the set walks onto its first. A stored breach goes stale the moment the rival builds.
- `Sweep` (v1's sweep): one at a time, on the room in sight an enemy owns,
  reserves or works (`intel`) whose worth, its prey (an enemy's unarmed
  WORK, CARRY or CLAIM bodies outside its barriers, each its cost by its
  life left and its load) and the harvest its exposed sources yield over a
  hunter's life (owner: denial is worth as much as kills), pays most a tick
  of the walk. Its set beats the escort and out-heals the charged towers
  there within that worth; with none, it opens unanswered at the bank it was
  priced at. The engagement is the commitment (v1 stored the room). A room
  worth nothing moves its living set on, never home (owner: a set spends its
  life in the enemy's field): to the candidate a set beats that pays most a
  tick of the set's own walk, else to the room whose staleness pays most a
  tick of it (`atlas::onward`, one rule for any body with work; a room just
  seen is worth nothing, an owned one is priced only in sight, and one where
  the tracker still holds an armed group, in its Lost grace, is not stale:
  owner, 2026-10-19, so a set does not walk straight back to the escort it
  left). A living set moves on only to a
  candidate it beats as it stands, as a move asks no cast. An escort or towers
  that beat it, priced on its arrival or an event, move it on the same way.
  It closes once its bodies are gone, a safe mode stands it off, or no room is
  left to move on to. No prey is remembered out of sight.
- `Drain`: the phase a `Siege` is in while his charged towers out-strike the
  heal the set seats on the body at its breach. A pair per 5,100 of the bank
  (three at most, full speed on plain) works the gate: his border tile the
  pairs' heal holds the volley on, a tile one of his towers reaches and the
  nearest of those. A body without HEAL steps onto it, takes the volley no
  medic can land across the border, and steps back beside it where a HEAL body
  seats; the rest of the set waits in the room next door, never on the tiles
  the pair works, as the entry is the breach plan's to walk round to. It ends
  when his towers are spent or no longer out-strike that heal, and the set
  advances per the breach plan. A drain his towers leave `Ignored` trips
  unanswered — the room's event log without an `EVENT_ATTACK` from a tower onto
  one of ours, reset by one he answered (owner, 2026-10-20: a give-up the drain
  carries, not a profile of the rival) — is given up: the siege holds the edge
  while a pair of its own stands or has tripped, casting nothing more, its
  bodies standing off until they die of age, until his towers answer or the
  safe mode or the declaration ends it. It is not closed, so the room is opened
  on afresh only once the siege is (owner, 2026-10-21). A drain no pair of the
  bank's can work — a gate it cannot heal, or a bank under a pair — is none:
  the set breaches or stands off by the breach's own rules.
- `KeeperDuty`: a perpetual cycle over lairs by respawn time; declares the room
  minable while it holds; rotates on TTL.
- `PowerBank`: break (attack + heal against hit-back) → declare a Pickup at the
  drop → done; deadline is the bank's decay.
- `Stronghold`: `Siege` with the core as target and the waves as groups inside
  the targeted room.

### 6.3 Composition and the exchange

`Exchange` is the v1 arithmetic, ported with its `ColonyView` parameter replaced
by a narrow input:

```fsharp
type Side = { Bodies: Fighter list; Ramparts: Map<Pos, int>; Towers: Tower list }  // rampart hits
type Terrain = { Weight: int; RunOff: int }     // a summary, not the room's tiles
val outcome : stance: Stance -> ours: Side -> theirs: Side -> terrain: Terrain -> Outcome
// Outcome = Win of ticks * Losses | Kite of ticks | Lose; Stance = Hold | Standoff
val cheapestWin : stance: Stance -> ours: Side -> affordable: Body list -> theirs: Side -> terrain: Terrain -> Body list option
```

- `cheapestWin` adds to the bodies already standing (`ours`) before it casts.
- `cheapestWin` is called once at open and once per event that changes a side.
  It chooses the body set; `Predicted` records the outcome for §10.1, and the
  kind chooses the tactic the set fights by (§6.3).
- Under `Hold` the raid comes to our post; one that strikes from past the reach
  of any striker of ours and outlasts our towers picks its range instead, so it is
  priced as a `Standoff`, and unanswered where that does not win (owner's call).
- What a fight weighs (a threat to answer, a structure to raze, a seat to
  stand on) is priced by its consequence in energy a tick: what it razes,
  kills, heals or denies. It is never ranked by body parts or kind. A rule
  keyed on a feature leaves a gap between the feature and the cost, and a rival
  who finds it owns it: Trepidimous's defenders guarded our rangers and let the
  dismantlers raze his home. A wrong choice is a term the price lacks, and is
  fixed there, not with a special case.
- Nothing in `Fight` re-prices per tick. A tactic that needs a number every
  tick reads it off the `Plan` or asks `Atlas`.
- A kind's tactic is **chosen from its candidates**, never derived from the
  outcome: each kind declares theirs beside its phase machine (`fight/kinds/`),
  the price offers each what it wins against what it spends in energy
  (`casting::spent`: the cast, the hits it takes, the room's worth a tick —
  against kills, denied harvest, what razing denies), and `tactics::select`
  picks: the candidates less the ones a rule forbids, a pin where it is still
  offered, else the greatest value less cost, ties to the fewest bodies then the
  declared order. The price orders each kind's candidates; where a kind has one
  viable candidate today (Siege, Sweep), the offer is a record of that price,
  not a comparison, and `Dismantle`, `Bounce` and a sweep's `Close` join with
  their stages. The tick that prices carries the offers behind its choice on
  the `Plan` its heap holds, which `observe engagements` prints from segment 1
  (§4); no tick's Memory does. A `Defend` candidate is offered only where it can fight
  at all: a set that can close does, one that cannot kites, a post is held
  where the raid must come to it, and a set that swings nothing fights as one.

### 6.4 Tactics — what a committed body does

A tactic (`core/tactics`, which imports no Fight) is a function from (engagement, body, world) to that body's intent this
tick. Each is under 150 lines and shared by every kind; v2.0 ports the first
five from v1's step functions, the rest come with their kinds:

| Tactic | Step function (v1 origin) |
|---|---|
| Kite | `Resolver.kiteStep` |
| Close | `Resolver.chaseStep` + glue |
| Hold | stand on rampart / `Atlas` seat |
| Pursue | `Resolver.chaseStep` over seams |
| Retreat | `Resolver.fleeStep` toward home |
| Formation | new: `Move` steps a 2×2 footprint as one body |
| Dismantle | v1 `actionIntents` sapper branch: dismantle the plan's target; the medics seat beside the body his towers struck last tick, as seen, not an assumed focus |
| Drain | new: the pair bounces through the gate (§6.2) |
| Hunt | new (ibex EV): best of nine tiles, worth less netted damage, the worth read over where each mark may stand next tick (one still last tick stays, one that stepped steps on, by `fight.groups`; none under its rampart or over the edge), or onto a mark beside it that may step (v1's glue); strikes most worth in reach; an unarmed mark on the gate, read off its last tick rather than a trajectory, is worth a strike that lands now, never a goal or a step (v1's `catchOdds`) |

A tactic decides the step (a Dismantle its dismantle too). The strike reflex
runs for every body with ATTACK or RANGED_ATTACK, in every phase, inside or
outside an engagement: a swing and
a shot at the first in reach in the kill order, a volley where it outdeals the
shot and no ally or spared stranger stands in its reach. It strikes by stance
(§7.2): an enemy's, an Invader's or a keeper's bodies all, a stranger's only
armed ones, an ally's never. A Hunt's scored shot
stands in for it. Target selection is a static kill order set at open
(healers first, then by damage/hits), re-computed only on events.

The heal reflex (ported `Emitter.healReflex`) runs for every body with HEAL,
inside or outside an engagement. A body's acts pass through the engine's
pipelines (`intents::performed`) before they are emitted, so none is cancelled
unseen.

### 6.5 Casting

An engagement that needs bodies emits `Cast` asks with the engagement id and
Strategy's priority. The spawn queue is `Tick`'s: per room, the economy's
floor asks first and never displaced, then the fight's, then the economy's,
which a fight ask still waiting on the bank holds; neither module reads the
other's asks. The Caster logic that picks which colony spawns ports from v1's
`Caster.fs` mostly as is: the hops, and the queue spread over the colony's
spawns. Beside them v2 prices a ring short of the body: the ticks its economy
takes to refill it, one spawn time for each body the bank is short of.
`fight.casts` remembers what was asked so a body in an oven is not asked
again. One room may hold three spawns; the queue is per room. Every fight body
is cast with one MOVE at the tail (v1's `withMoveLast`), so it walks while any
part lives, and every body a set walks out of our rooms with carries a HEAL
(owner), bar a siege's dismantlers, which
its medics cover: it heals the damage it is shot for instead of losing that
MOVE to it a walk at a time. A home's Defend is priced without one, beside its
towers.

A body the declared cluster's stock covers is cast with its compound in its
name (`fight-<tick>-<engagement>-<slot>-<compound>`): the name is the request,
so no Memory row is added for it. `Tick` settles the body at the home's lab
before its work, and the economy fills that lab from the home's stores. The
hold is a fact of the tick, never a timer: it stands only while the parts still
lack the compound, the home holds it with the energy to spend, and a body in
the room can carry it there — a colony that cannot serve the body leaves it to
fight unboosted.

## 7. Strategy (new)

Strategy is the world's only planner. It has no creeps; it writes declarations
and opens engagements. It runs on its key (§1), not every tick.

### 7.1 Intel

`intel.rooms` (§4) is written from `World` each tick, before any module, for
every visible room. Economy reads a declared claim or outpost out of sight as
its row last saw it, so a room another holds casts no claimer. Strategy owns
the observers: each declared siege every tick, then the stalest rows older
than `STALE` (a creep life, the owner's) in range. Every declared room no
observer covers is a `Scout room` declaration: a living 1-MOVE scout never
idles; as it stands in its room, Economy sends it on by `atlas::onward` (the
row's age over the walk), one room a scout; a new one is cast only for a row past
`STALE` (a room never seen is served as nobody's, its bodies' sight its first row).
❓ After the sweep: rows grow towers and sources with their first reader,
and whether the room graph's avoided set comes from them.

### 7.2 Expansion and declarations

Homes, outposts, errands, avoided rooms, allies, enemies, claims and the
operator's tactic rules are rows in `declarations`, written by Strategy (the
rules by the operator alone) and editable by the operator (`observe declare`;
`observe declare tactics <file.json>` takes a list of `{ rival?, room?, pin?,
forbid? }`, refused where a name is no tactic's or a pin is what a rule that
joins it forbids). The world reader turns every owner into its stance once
(`world::Owner`): a stranger, neither ally nor enemy, is fought where it
threatens us but never hunted; enemies are declared by hand, and a declared siege's owner, as intel last saw it, is one (owner's call). Candidates are priced once when intel changes or a due tick comes:
an outpost by sources, distance, threat and the CPU it costs (v1 withdrew an
outpost for CPU; the budget is explicit here); a claim by the same plus GCL
room; a room is avoided by what intel saw there. A priced candidate that pays
is declared; one that stops paying is withdrawn. No room is ever named in
source.

Each tick, once `Derived` is built and before any module runs, Strategy writes a `ColonyPlan` for every room we
own (today: `Exploit` built to its controller's level) and
`Declarations::planned` merges it under the operator's rows: **an operator row
always wins** and Strategy fills only rooms the operator left undeclared
(owner's call), so a hand decision is never undone by the next plan.

### 7.3 Operations and force allocation

- Each candidate operation (`Siege` on X, a `PowerBank` seen at Y, `KeeperDuty`
  over Z) is priced once — expected gain against bodies, boosts, time and
  `Exchange.outcome` against the defence intel describes — and opened when it
  pays. v1's `Operations.fs` bidding is this section's origin; its code is not.
- **Force is allocated across engagements**, not per engagement. When several
  engagements ask and the spawns or the standing bodies cannot serve all, the
  highest `Priority` (value defended or gained, per body asked) is staffed
  first; the rest are starved (§6.2). Priority is set at open and on events.
  This is the world-level rule that keeps one rival striking three rooms from
  being three independent fights over one force.
- `Defend` is opened at a trivial price (a threat to a held room is answered)
  with priority from the room's worth; v2.0's Strategy is that opener, the
  expansion pricer and the allocator, and nothing else.

## 8. Season (new, disposable)

Every season changes the rules: what scores, which structures exist, which NPCs
walk. `Season/` holds all of it: scoring, the season's structures and
resources, their intents, and the declarations that make the economy and the
fight serve them ("haul Thorium to the reactor" is a declaration). `Strategy`
reads `Season.worth` to price what the season rewards. Nothing else imports
`Season`. Next season, the directory is deleted and rewritten; the rest of the
bot is unchanged. v1's reactor, Thorium and programme code is not ported: it
is this season's `Season/` if anything, and this season ends in weeks.

## 9. Performance

### 9.1 The benchmark comes first

v1's harness never matched live: it ran synthetic colonies, froze the world
between ticks, and counted none of the engine's costs, so every CPU change was
made blind and the profile stayed flat. v2 does no performance work until the
benchmark meets three requirements:

1. **Input is a live capture.** A recorder in the bot (a switch) writes what
   `World.ofGame` read and the Memory string for N consecutive ticks to
   segments; `npm run observe -- capture` pulls them and the harness replays them
   through the built bundle. v1's `capture-tick` read room history after the
   fact; that is not what the bot saw. No synthetic scenario is a benchmark.
2. **The cost model is the engine's.** Memory is `JSON.parse`d from the real
   string on first access and `JSON.stringify`ed at the tick's end; accepted
   intents cost 0.2 each; the harness runs under the live isolate's heap limit
   so GC shows; the first ticks after load are reported apart (cold).
3. **It is calibrated against live.** `Main` records per-module CPU
   (`Game.cpu.getUsed()` deltas) from the first tick of the skeleton, to
   `observe.cpu.modules`. The harness reports the same split. The benchmark is
   **representative** when each module's share is within ❓ 15% of live's on
   the same window; until it is, the harness is fixed, not the code.
   xxscreeps (§10) runs the same isolate and heap limit as live and may
   calibrate better than the node replay; whichever matches live is used.
   Until v2 runs live a soak stands in: `npm run soak -- --capture a-b`
   writes those ticks' input and `npm run bench` replays it (an unchanged
   bundle reads within ±0.4% per module; 600 ticks, 8 rounds, a minute).
   The bench builds `Game` as the official engine does, which xxscreeps does
   not, so ❓ its calibration waits for live; `observe capture` writes the
   live recorder's ring as this capture. `scripts/xxscreeps/ab.mjs` (whole soaks
   side by side, ±0.6%) re-checks a claim in the engine.

A speed claim is an interleaved A/B on the representative benchmark with
identical Memory fingerprints, or a count (floods, lookups, allocations). A
workstation's wall clock alone settles nothing (v1 measured ±6% drift on an
unchanged build).

### 9.2 Rust dialect — defaults, not rules

The boundary is pure; the inside is written as a systems language. Everything
below is the **default** a module's first line is written in; any of it yields
to a measurement on the benchmark above, in either direction:

- Module boundaries take and return owned values, but for `State`, which a
  module writes in place once it cannot fault (`ModuleRun`): returned whole,
  each module cloned it, ~700 allocations a tick on the live capture
  (2026-11-10). Inside a function, `Vec`s, mutable locals and loops on any
  path that runs per creep or per tile, and no allocation a tick can avoid.
- A call across the wasm–JS seam is the seam's cost: on the live-sized capture
  ~90% of what reading the world cost over the F# reader was the number of
  calls, not the work in them (2026-10-17). So `World::of_game`, the only
  reader of the game, takes it from `app/read.js` in one pass, its numbers in
  one `Int32Array` and its strings in one string, decoded in the order written;
  a field goes on that pass, never a call of its own.

### 9.3 Counts are a gate

The bot reports the per-tick counts it has in the observe segment: floods run
and Atlas cache hits/misses today, the tiles a Hunt scores, `Exchange.outcome` calls, `Derived` lazies
forced, intel rows read and Memory bytes as they land. Because `decide` is
deterministic the numbers are exact, so `scripts/ceilings.json` holds ceilings
for them beside the line ceilings — never raised; lowered when a change earns
it — and `npm run scenario` reads them. This is how "the same thing computed
many times" is caught: by a number, at `jj ship`, not by a reviewer noticing. A
count ceiling is the first performance regression test and needs no
representative benchmark to be trusted.

## 10. Verification

The pyramid is inverted: **outcomes in a real engine are the primary test**;
unit tests exist for arithmetic the engine would only obscure. Live is the
final test, and the bot measures itself there.

### 10.1 Exchange is checked against what happened

Every engagement records `Predicted` at open and the actual outcome (ticks,
losses, won/lost) at close, to `engagements.log`. `observe exchange` reports
the prediction error. The same is run offline (§10.2) against generated
opponents: hundreds of random hostile compositions (bodies, boosts, count,
terrain, towers) against `Defend`; a predicted `Win` that lost is an `Exchange`
bug, a large error is a term the model lacks. This is the only real test of
the arithmetic v2 prices its commitments with, and the quality metric of the
"cast exactly enough" lever.

✅ Offline: a scenario reads each engagement from our Memory every tick and
reports its error by `scripts/xxscreeps/prediction.mjs`, which defines an
engagement's outcome and the error once for it and `npm run exchange`;
`predictedWinsLost` fails a fight scenario on an `Exchange` bug. ❓
`engagements.log` and `observe exchange`, live.

### 10.2 xxscreeps is the engine

`xxscreeps` (v1's `npm run xx`) runs vanilla engine rules and the vanilla NPC AI
under isolated-vm, seeded, against the built `dist/main.js`. v2 uses it for
everything v1 used its self-written arena for, and more:

- **Scenario library.** Every live incident becomes a scenario: the room's
  state at tick T (from the recorder or room history) seeded into xxscreeps,
  run N ticks, asserted on outcomes — held, losses ≤ X, the group dead by T+K.
  A fix is verified by its scenario passing and the library not regressing.
  The library runs nightly. `npm run scenario` runs `scripts/xxscreeps/scenarios/`:
  each a start as a recipe (room, seed, late start or node, or the world a
  soak of a pinned commit left at tick T, cached once, so a late-game bound
  costs its last N ticks; the bundle under test runs on from it as a deploy
  would), its ticks and bounds on what the soak measured, scenarios on one
  start sharing one soak; `jj ship` runs those of the modules a change touches.
  The operator's rules are a scenario like any other:
  `tactics-forbid-stands-the-siege-down` is a declared siege whose tactic a
  rule forbids, which prices nothing it may fight and casts nothing.
- **Soak.** The real sector's terrain and rooms, from one spawn for thousands
  of ticks: income curve, expansion cadence, losses to NPC waves, CPU curve.
  ✅ Season 11's own map (`season-world.mjs`) stands in for the template's
  small sector, where a claim may have no sourced neighbour:
  `season-outpost-ring` is a one-source claim with three sourced neighbours
  (remote mining, so a named room needs only one source), `season-late-seat`
  our own RCL8 seat and its block (the late nodes).
  A change to the economy is read off this curve, not off a unit test. It is
  judged on the long horizon (`npm run soak -- --ticks 30000`, where a
  structure's payback, thousands of ticks, has time to show): the RCL and
  progress at the end, the tick each level is reached, the income in the last
  thousand ticks; the first ten thousand ticks alone decide nothing (§5).
  Before it ships, the same over the two extremes of harvest seats
  (`npm run soak:many`: rooms whose sources have one seat each, and rooms
  whose sources have four or more), so no change is tuned to one kind of
  terrain, and 10,000 ticks of N7 on `season-late-seat`, so it is judged on
  the late economy a respawn never reaches, and 10,000 ticks of E1 on
  `season-outpost-ring` with Hivemind seated two rooms off, so exposure and
  losses (§10.5) are read against a rival that contests the outposts
  (Hivemind over TooAngel: it reaches RCL3 there and walks our outposts,
  where TooAngel stays at RCL2; our safe mode kept, as a live colony keeps
  its own). The extremes answer differently
  (a crowd of small bodies works only where seats are many), so they are
  read apart, never as a median.
  A soak is a relative measure: A against B on the same rooms and seed,
  reported as the difference, never as what live will do (live has
  neighbours, season rules and the owner's hand, which no soak calibrates
  against). A win is re-checked on other starts before it is believed (another
  seed, and the bot sitting out its first ticks: the seed only breaks ties,
  and a room with few ties drifts from a small change the seed never
  makes), and a loss smaller than the room's own spread across the starts
  is noise. An RCL8 room's controller progress buys only GCL, so it is
  read apart and never counts as a room's regression. Players' same-tick contested intents resolve in user-id order,
  so a run repeats but is one-sided (the Invader usually moves first); a
  gap between soak and live surfaces as an incident and becomes a scenario.
  That a run repeats is checked by `ab.mjs --a x.js --b x.js` (identical
  fingerprints: N7 and E1 on the season worlds, 2026-11-10).
- **Nodes.** A long soak from a respawn reaches RCL3 in 10,000 ticks and
  never sees links, labs or power. Each node below is a start state built
  from Layout's own plan (the level's allowed structures on their planned
  tiles, a stock in storage, no creeps, so the cold start is exercised too)
  and run a few thousand ticks:

  | Node | Start | What it proves | Read |
  |---|---|---|---|
  | N1 respawn | RCL1, one spawn | the opening, investment order | ticks to RCL2/3, first road and container |
  | N2 RCL3 | 10 extensions, a tower, source containers | road and container returns, tower refill | income, walk share of creep-ticks, upgrade rate |
  | N3 RCL4 | 20 extensions, storage 5k | storage logistics, hauler ratio vs roads | storage net flow, haulers, ticks to RCL5 |
  | N4 RCL5 | 30 extensions, 2 towers, storage 50k, links unlocked | how much hauling links replace | link throughput, haulers (should fall), upgrade rate |
  | N5 RCL6 | terminal, extractor, 3 labs, storage 100k | mining, reactions, upgrade boost, market | ore a tick, boosted upgrade rate, market net |
  | N6 RCL7 | 2 spawns, factory, 6 labs, storage 300k | spawn queueing, compression, big bodies | spawn use, factory output, CPU |
  | N7 RCL8 | the full set, storage 500k | where energy goes past the 15/tick upgrade cap | power processed, surplus use, CPU |
  | E1 outposts | RCL5 + two neutral neighbours | reservation, remote haul, whether an outpost pays | outpost net (after its bodies), reservation held |
  | E2 claim | RCL6 + a neighbour Strategy picks | choosing, claiming, pioneers, aid | ticks to the child's RCL3, mother's income cost |
  | E3 colonies | three colonies at RCL7, 5, 2 | cross-colony work, terminal balancing | total income, cross-colony work share, CPU vs colonies |
  | S1 invaders | N3 + timed Invader waves, home and outposts | `Defend`, towers, recovery | losses, ticks back to income |
  | S2 keepers | RCL7 + a keeper room | `KeeperDuty`, whether keeper mining pays | keeper-room net, losses |
  | S3 stronghold | RCL7 + a stronghold next door | whether to fight it, `Siege` | the decision, gain vs cost |
  | S4 low CPU | N6 with the CPU limit at 20 | degradation, the bucket | bucket curve, income lost to degrading |

  Each push runs the one or two nodes its change touches (about a minute);
  nightly runs them all with the long soak over the seat extremes. Nodes N1–N4 come first
  (the economy and links), N5–N7 with their structures, E with Strategy, S
  with Fight. `npm run soak -- --node N1` … `N5` builds those five (`scripts/plan`; N5 stands its links), and `E1` ✅ N4 with two neighbours declared as outposts (`declarations.outposts`) and an Invader wave in each every 4000 ticks (`--invaders`, xxscreeps' own harvest goal doubling after each raid); each sample's `rooms` line reads an outpost's net after its bodies; `N6` and `N7` ✅ RCL7 and RCL8 with their bodies standing; ❓ the rest, with their structures.
- **Adversaries.** xxscreeps hosts several users. Self-play: the attacker is
  our own bundle with only `Fight` in its modules, an engagement declared at
  start and its squads standing, against the full bot defending; each game
  is also an `Exchange` sample (§10.1). Self-play shares our blind spots (an
  attacker that never drains never tests the defence against a drain), so
  fixed opponents stay in the library: scripted tower drain, dismantle,
  kite and blockade squads, v1, and open-source bots (TooAngel is plain JS;
  Overmind and The International need a build). NPCs do not hold a room;
  this is the only offline test offence has. ✅ A scenario seats opponents
  beside ours (`start.opponents`: a bundle or a bot directory, each in its
  own room; ours without the engine's safe mode); `two-v2-neighbours` seats
  a second v2, a smoke test until v2 has an offensive kind. The scripted
  rivals are one library, `engine/rival/`: `opponent.js`'s { role } picks a
  behaviour list — keep, remote, raid, drain, harass, pass — over its own
  keys, and the roles share one tower aim, one walk and one room economy. A
  raid's bodies and boosts are stood on our room's edge at its tick
  (`opponents[].raid`). Held is: no level or spawn lost and the raid gone
  by the window's end (`defend-holds-*`, N3: a kiting raid,
  a healed pair,
  Invader waves). TooAngel (AGPL, so fetched by its pinned
  tarball and integrity, never vendored: `engine/tooangel/fetch.json`)
  plays from next door in a world cached at tick 11000, its RCL3, and sends
  its melee squad at our home every 1500 ticks (`defend-holds-tooangel-attack`).
  A seated bot reads a pinned CPU and bucket and draws from the world's seed
  (`engine/seat.js`): read off the machine, they throttle it by load. A run
  fetches only with `--fetch`; without the tarball in the cache that
  scenario is skipped, never failed, and `jj ship` never reaches the registry. Hivemind,
  fetched the same way, holds the declared-room world against a siege and
  a sweep, both the offence target and so nightly: `--nightly` runs them,
  and they join the daily library once they pass. `siege-outlasts-safe-mode`,
  30,000 ticks, is nightly for its length alone. The library fails to
  load while a fight kind (`siege-`, `sweep-`, `defend-`) meets no
  open-source bot. ❓ Both fail: reinforcements walk into Hivemind's towers
  alone, and three keep sieges hold their bounds only while its towers
  strike the nearest foe. ❓ self-play with an attacking v2.
- **Exchange calibration** (§10.1), generated: `npm run exchange`.

Coverage, read off xxscreeps 0.2.1's mods and changelog (2026-10-10): invader
AI (`classic/invader`), keeper lairs (`classic/source`), invader cores and
strongholds with bunker4/5 defence, boosted defenders and collapse timers
(`modern/stronghold`), power banks with decay and hit-back
(`modern/powerbank`), power creeps and power spawns (`mmo/powercreep`,
`modern/powerspawn`), labs, factories, nukers, observers, deposits, terminals
and market, portals. `xxscreeps generate-sector` builds a whole sector with
highways and a keeper core; `export`/`import` move a world as a payload JSON;
`manage game pause-tick N` steps N ticks — the soak's world, the scenario
library's seeding and the per-tick assertion respectively.

Speed, measured 2026-11-10, a step's ms a tick with our bot's CPU of it in
brackets, four soaks at once: a respawn on `season-outpost-ring` 1.7 (0.6),
E1 there 3.4 (1.3), N7 on `season-late-seat` 2.9 (1.2), the five-colony live
capture (70 rooms, 124 bodies) 35 (19). The engine's part grows with the
world as the bot's does. **Scenarios for the touched modules run at
`jj ship`; the full library and the soak run nightly.**

Known limits: vanilla rules only (`Season/` is live-only, by design); small NPC
deviations (flee counts armed hostiles only; healer picks by range).
Maintenance is not a risk as of 2026-10: commits three days old, 1,176 in all,
PRs in the 400s; v1 already builds its isolated-vm against Node 24 through
nix.

### 10.3 Unit tests, where the engine would obscure

Two kinds, both under a minute in `npm test`:
1. **Invariants** over generated inputs (a fixed-seed draw, `core/tests`): Memory round-trips; a
   Tracker id is stable under permutation; `Ours` is disjoint from
   `assignments`; no two bodies resolved onto one tile; no idle spawn while a
   cast is affordable.
2. **Pure functions**, table-driven: `Exchange` formulas, `Atlas`, `Tracker`,
   body construction, `Price`.

Not allowed anywhere: a test that asserts a specific creep's action in a
specific scenario. That is a change detector; it pins the bug you have not
found yet.

### 10.4 Replay

Because `decide` is deterministic (§1), a captured tick replays the exact
decision locally. `npm run scenario -- --capture` writes every tick each soak
decides (`capture.jsonl`, objects as changes since the tick before), and
`npm run replay -- --a A.js --b B.js <tag>` stands each tick as the bench does
(§9.1) and has both bundles decide it: identical intents and Memory on every
tick, or the first tick apart with the intents each made and the Memory paths
that differ. Each tick is stood from A's recorded world, so ticks after the
first difference still compare. The library's 274k ticks replay in about a
minute; a refactor's claim is an identical replay, which the soak fingerprints
(every 100 ticks, and the rivals' own noise) cannot make. `--at T` prints one
tick's intents: an incident is debugged by replaying it, not by reasoning
about what the bot might have seen. A capture is only as good as the base
that recorded it: recapture when the base's behaviour changes. A change of
behaviour is then read whole: `npm run scenario -- --bot B.js --base A` prints
each measure beside the base's run of it (better or worse by its bound's side)
and each soak's season score against the base's. One seed is one game, and a fight turns on a
tick: `--seeds N` plays each start on N seeds, and a change is read over them
(four seeds of the fight scenarios run in about eleven minutes).

### 10.5 Metrics

The bot measures its own economy and fight in 100-tick windows (idle share per
pattern, task churn, loads returned to their store, spend split, spawn use, stale sites, engagement flips,
refusals, CPU per phase, deaths by cause, structures lost, hostile dwell,
held steps, edge flips, swings, missed shots), so live and the soak read one number by one
definition: last season most faults were found by the owner watching the
client. Plan gates and soak comparisons read three axes: season output
(controller progress until a season proxy lands), exposure, and losses
(`loss.*`, in energy) over rooms `held`. Exposure counts the ticks a raid could
cost a colony a level: a foe in it or its nearest outposts while it is below
RCL3, has no stocked tower, or cannot fire a safe mode.
`core/observe/metrics.rs` holds the definitions and the thresholds,
each in one table; `observe health` lists a metric past its threshold (never
`Game.notify`), and a scenario bounds one with `{ "metric", "max"|"min" }`.
It is a pure step from the tick's World, assignments, engagements and their casts, intents and refusals
after the executor, and no decision reads it, so `decide` stays
deterministic. Spend is estimated from the intents (WORK × power, capped by
the target and carried energy), not read from the event log, which is a call
a room; `metrics-spend-matches-ledger` holds it within 2% of the soak's engine
ledger per category on E1; `deathsError` holds the deaths (event log read
by World) to the bodies the soak saw struck and gone. Steps are counted over
economy bodies only: a fight body's swing may pay and a set's border wait
flips by design, so the scenarios judge those (`oscillation`, `bounced`).
A held step is Move's own verdict (`Intent::Held`, a wish the arbitration
left standing): the mover never steps into a taken tile, so a body waiting
behind another never shows as a refused step.
A threshold comes from healthy soaks (`npm run metrics:baseline` over
`soak:many`, the nodes and the live-sized world, past the colony's forming):
1.5 × the healthy p99 rounded up (to a multiple of 5 above 5), or 0 for what
must never happen. A metric a healthy run already fires is a fault or a wrong
definition, never a looser bound.
Its CPU budget, 1% of the bot's, is read on a live-sized world (the
soak's `--world` of a live capture); a one-colony node, whose whole tick is
under a millisecond, reads higher.

## 11. Live safety

Correctness is proven live, so the live loop has to be safe:

- Every module behind a **kill switch** in `switches`; the fight can be turned
  off and the economy keeps running.
- Fault isolation per module (§2); faults to `observe.faults`.
- **Deploy canary** (not built yet; `npm run deploy` is build, smoke,
  upload today): `npm run deploy` keeps the previous bundle and records the
  last N ticks' income, idle spawns, deaths, faults and bucket; a worse reading
  over the next N ticks reverts automatically. ❓ N = 200.
- `observe` reads engagements, faults, declarations and exchange error
  directly; nothing is logged twice.

## 12. Process

- **Stop-and-ask has a fallback.** A change that would add a state row, a
  Tuning field, a latch, or any item `AGENTS.md` lists stops and asks the
  owner. When the owner is away, the issue is labelled `needs-owner` and
  parked; the agent takes other work. It is never worked around.
- **Shape audit**, every ❓ 30 issues or monthly: line counts against
  ceilings; §4 against the Memory and segment keys actually written; per-module
  CPU shares; the `engagements.log` prediction error; the count of `Tuning`
  fields. The audit is a report the owner reads, not a gate.
- The owner writes this document. Agents propose changes to it as issues.

## 13. Port manifest

v1 is the `v1` bookmark (from `427ff628`, main before v2 landed), maintenance only.
Read a v1 file with `git show v1:<path>`; nothing of v1 stays in the tree.

Milestones, in order; one is done when its exit holds, not when its list is
empty:

| Milestone | What | Exit |
|---|---|---|
| M1 economy | the rest of v1's economy mechanisms | the seat-extreme soak beats v1's bundle in both classes, on a second seed |
| M1b outposts | declared outposts: reservation, remote harvest and haul, before M2 because RCL6+ structures spend more than a home's two sources give. No fighting before M3: a player's armed hostile in an outpost stands it down while it can reach the room's work — its sources and containers — and no longer; an Invader's raid, whose only aim is that work, stands it down on sight; a stronghold core stands it down outright; and one last seen in a room gone blind stands it down for its life (`sightings`; the owner's choice over porting a guard early, and over a claimer probing a blind room; the split by owner is the owner's too, 2026-10-20, as in v1's ADR 0043: an Invader's raid is read to its life, a player's to where it stands); M3's `Defend` walks its set there once it stands, which brings the room back in sight | node E1 (with timed Invader waves): each outpost nets positive after its bodies and its stand-downs |
| M2 structures | node builder (N1–N4) ✅, links ✅ (the controller's and the longest-way source's first: +5–17k worth over 20k ticks on N4; hub-first broke even, as v1's arithmetic said), node N5 ✅; labs ✅ (the declared cluster reacts toward its targets, and the boost a cast asks for in its name is its sink, §6.5); factory and the RCL8 set wait for a sink (owner's call: one room mines one ore, a reaction needs two, offline has no market) | links: the node beats the economy without them; the rest: judged with their sink, M3's boosts or several colonies |
| M3 defence | `Exchange` ✅, `Defend` ✅ (scenario `defend-kills-home-raid`), the first tactics ✅, casting ✅, the opponents harness ✅ (`two-v2-neighbours`), scripted raids and TooAngel held ✅ (`defend-holds-*`) | self-play and the fixed opponents held; the prediction error within bound (§10.1) |
| M4 live | v2 replaces v1 | ❓ the owner decides from progress |
| M5 reach | `Strategy` over many colonies, the offensive kinds | the end in §0 |

Status: ✅ landed · ⏳ next · blank not started.

| v1 | v2 | Action |
|---|---|---|
| `App/Bindings.fs` | `screeps-game-api`, `app/js.rs` | ✅ plus `cpu.limit` and segment writes |
| `App/World.fs`, `Main.fs` shell | `app/world.rs`, `wire.rs`, `driver.rs`, `core/tick.rs` | ✅ `World`: terrain, controllers, sources, minerals, structures, sites, piles, remains, hostiles, our creeps with bodies and stores; per-module CPU, fault isolation, switches, recorder, observe segment |
| `Types/Rules.fs` `Engine`, `Types/Vocabulary.fs`, `Types/Lists.fs` | `core/engine.rs`, `vocabulary.rs`, `lists.rs` | ✅ `Engine` less policy (`guardCap`) and season (`reactorCapacity`), plus part costs, regen time and level progress; `Vocabulary` less `Task` (Economy's) and with season resources as `Resource.Other` |
| `Core/Atlas.fs`, `Grid.fs`, `Geometry.fs` | `core/atlas/` | ✅ tile index `y*50+x` (the terrain buffer's), the flood, footprint on every query, a minimal room graph, the flood table. Known gaps: a route is chosen without the footprint (a Quad can be routed through a one-tile gap); boosted CARRY is priced unboosted. |
| `Decide/Resolver.fs` | `core/mv/` | ✅ arbitration (two-tier eviction, best chain, displacer's onward last) and `Plan` wishes; re-derived each tick from `Atlas.firstStep` (§1's "path" key is not stored). Fight steps ✅ in `fight/tactics.rs` (kite, close and pursue, retreat), without v1's threat maps; formation stepping comes with `Siege` |
| `Decide/Pool, Matcher, Planner, Quota, Spawns, Price, Bodies` | `core/economy/` | ✅ rewritten as slots (§5, `economy/slots.rs`, `jobs.rs`, `run.rs`): flows to slots, casts, tasks, act in range, wish toward the target; assignments in `Memory.v2.assignments`. Declared outposts (`declarations.outposts`) are worked while no armed one in the room reaches their work, and while no armed one last seen in a blind room outlives the tick. Known gaps: claims are not yet declared; cross-room travel prices each room between at a room's side |
| `Decide/Labs, Layout` | `Economy`, `Layout` | Layout ✅ in the tick after `economy`: a standing plan per room, sites in plan order as the level allows, towers on hostiles. Labs ✅: the cluster's reaction plan per tick and the boost a cast asks for (§5, §6.5) |
| `Types/Colonies.fs` declarations, `allies` | `declarations` data | not ported as code; the current rooms seed the first `declarations`. ✅ `declarations.allies` (player names): an ally's creeps leave `Hostiles` as the world is read, so nothing answers them |
| `Decide/Facts.fs:2883–4056, 1038–1582, 2153–2196`, `Fighter.fs`, `Arrival.fs` | `Exchange` | ✅ `core/exchange/`; `npm run exchange` is §10.1's generated set |
| `Decide/Emitter.fs` strike/heal/glue | `core/tactics` | ✅ the strike (swing, shot, volley where it outdeals the shot) and the heal reflex; glue ported into Hunt (a body beside a mark that may step wishes onto its tile; `Tick` sends that move unarbitrated) |
| `Decide/Caster.fs` | `Fight.Casting` | ✅ a body to the spawn that lands it soonest, a home's Defend the home's, v1's guard, ranger and defender blocks; the lending floors and trespass prices dropped |
| `Decide/Operations.fs`, `Types/Operation.fs` | — | shape only: `OpRecord`'s fields inform `Engagement`, its bidding informs `Strategy`. No code ports; re-read when `Siege` is due |
| `Core/LightTick.fs` | — | not ported; per-module keys (§1) |
| `Decide/Threat.fs`, rest of `Facts.fs`, `Observe.fs` fight parts, `WorldDanger.fs`, `Sightings.fs` latches | — | not ported |
| reactor / Thorium / programme code | — | not ported; `Season/` is per season |
| `tests/**` | — | not ported; §10 |
| `scripts/xxscreeps/*` | `scripts/xx` | port and extend (§10.2); ✅ the scenario library, `scenario.mjs` |
| `scripts/capture-*.mjs` | `observe capture` | ✅ pulls the recorder ring; ✅ the replay harness over a soak's capture, `scripts/bench.mjs` (§9.1) |
| `scripts/bundle, upload, cpuprofile, screeps-api`, `ship.sh` | `scripts/` | ✅ kept; the bundle is `rust.mjs`'s |
| `scripts/observe.mjs` | `scripts/observe.mjs` | ✅ rewritten small: health, cpu, faults, switches, capture, wait |
| `scripts/profile, probe, spar, wire-check, cli-smoke, adr-check` and the rest | — | not ported; `smoke.mjs` and `ceiling.mjs` are new |
| `docs/adr`, `docs/research`, `CONTEXT.md` | — | not ported; this file |
