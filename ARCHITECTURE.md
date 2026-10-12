# fabot v2 — architecture

This is the one document agents read. It says what the modules are, where every
piece of cross-tick state lives, and the few rules that keep it that way. Anything
not here is decided by the code, and the code wins over any other document.

Items marked ❓ are open: the owner decides them before the module is started.

## 0. What v2 is

A Screeps seasonal-server bot in F# compiled by Fable. v2 is a port of v1's
economy and a redesign of everything that assumed a unit smaller than the world.
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
| Economy | the match | creeps born/died, task set, a store crossing a line |
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
  most once, only if asked.
- Decision functions in `Fight`, `Economy`, `Strategy` take `Derived` (and
  `Atlas`), not `World`. What a function reads is its signature; a `World`
  parameter below the deriving stage is a review flag.
- **One tick cache exists**: `Atlas`'s flood table of goal-seeded reverse
  floods, keyed by (room, goal tiles and range — or the exit toward the next
  room —, footprint, pace class: the body's step ticks on road, plain and
  swamp), living in the `Atlas` value of one tick, counting hits and misses.
  Room routes are recomputed per cross-room query; if the counts show it,
  they join `Derived` (an owner decision). Floods are
  the only demand-driven derivation. Any other `TickMemo`, `Dictionary` cache
  or `lazy` outside `Derived` is an architecture change (`AGENTS.md`).
- **Counts are a gate** (§9.3): floods, Atlas misses, `Exchange.outcome` calls
  and intel reads per tick on a replay capture are deterministic numbers with
  ceilings; a change that doubles the floods fails `jj ship` without anyone
  profiling it.

**A cold start** (heap empty after a reset or deploy) rebuilds caches from
Memory and segments over as many ticks as the bucket allows, economy first; a
cold tick never spends more than ❓ 60% of the limit.

## 2. Units

- **The world** is the decision unit. One `decide` call, one pool of tasks, one
  set of engagements, one Strategy.
- **A colony** is a resource: a controller, its spawns, its storage, its
  terminal. It owns no decision. "Whose creep does this" falls out of travel
  cost in the world pool, cut off by room-graph hops (§3) for CPU.
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
| `Engine` | server constants and formulas | — | — | port verbatim |
| `World` | this tick's facts, one record | Game | — | port, trim |
| `Atlas` | geometry in two layers: the **room graph** (which rooms a route crosses, highway cost, rooms to avoid, hops) and the **tile grid** per room (terrain, floods, seats, work areas, first steps). Every tile query takes a **footprint** (1×1 today, 2×2 for a formation) | World, `intel.rooms` for avoided rooms | tick cache | port, add the graph layer, widen the query |
| `Economy` | task pool, planner, matcher, quota, spawns, labs, terminals, market | World, Atlas, declarations, `State.Economy` | tasks, casts, `State.Economy` | port, trim (§5) |
| `Layout` | construction and tower plans | World, Atlas | sites, tower intents | port |
| `Exchange` | who wins: pure fight arithmetic | bodies, terrain | — | port from `Facts.fs:2883–4056, 1038–1582`, `Fighter.fs`, `Arrival.fs`; narrow the input |
| `Fight` | tracker, engagements (one kind per file), tactics, casting | World, Atlas, Exchange, `State.Fight` | fight intents, casts, declarations, `State.Fight` | **new** (§6) |
| `Strategy` | intel; declarations (homes, outposts, errands, avoided rooms, allies); which operations to open; force allocation across engagements | World, Exchange, `State.Intel`, `Season` | declarations, opens/closes engagements, `State.Intel` | **new** (§7); v1 `Operations.fs` bidding informs it |
| `Season` | this season's rules: what scores, its structures, its NPCs, its map quirks | World | declarations to Strategy, season intents | **new**, disposable (§8) |
| `Move` | per-room movement arbitration and the step functions (chase, kite, flee, formation) | Intents, Atlas | move intents | port `Resolver.fs`, drop fight precedence |
| `Executor` | API calls | Intents | Game | port |
| `Observe` | what the operator reads: health, faults, per-module CPU, engagements, exchange error | everything | `State.Observe` | rewrite small |

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

Everything v2 keeps in Memory is under `Memory.v2`; v1's keys are never read
or written, so a rollback to v1 finds its Memory as it left it, and clearing
them is an operator's deliberate step. Segment ids: 1 observe, 10–14 the
recorder's ring; the rest are assigned here as their stores land.

| Store | Shape | Owner | Lives in | Why |
|---|---|---|---|---|
| `assignments` | creep → task id | Economy | Memory | sticky matching |
| `fight.engagements` | id → Engagement (§6.2) | Fight | Memory | the commitment; read every tick |
| `fight.groups` | id → Group (§6.1) | Fight | Memory | hostile identity across ticks |
| `fight.casts` | pending casts by engagement | Fight | Memory | never cast twice |
| `declarations` | homes, outposts, errands, avoided rooms, allies, claims | Strategy | Memory | what the world is for us, as data |
| `switches` | name → bool: `false` skips a module, `true` opts in (`recorder`) | Main | Memory | turn a module off live |
| `recorder` | the last five ticks' `World` and Memory size | Main | segments 10–14 | the capture (§9.1) |
| `economy.plan` | per colony: layout memo, census signature | Economy | segment | read on key change |
| `intel.rooms` | room → last seen, owner, structures summary, defence estimate, sources, controller | Strategy | segment | hundreds of rooms; read on a due tick |
| `engagements.log` | closed engagements with prediction vs outcome | Observe | segment | §10.1 |
| `observe.*` | health, faults, per-module CPU, counters | Main, then Observe | segment 1 | operator; calibrates the benchmark |
| `sightings` | room → last seen tick + ids | World | heap | vision grace |
| `standing.*` | per module: last output and the key it was decided on | each module | heap | the replay (§1) |

Rules:
- Memory and segments are written once a tick by `Main`, from `State'`. No
  module calls `Memory.*` or `RawMemory.*`.
- No `TickMemo` and no `mutable` module-level binding outside `Main` and the
  heap rows above. A cache inside a function's tick scope is fine.
- No room name, player name or object id in source. v1's `Colonies.fs` carried
  seventeen outposts and `allies = ["Odiodin"]`; all of it is `declarations`.

What v1 had and v2 does not: `danger` (7 parts), stand-down, raid episodes,
exit watches, head latches, exit holds, healer/raider recall, quarries,
refusals, economy carries, camp prices, room latches, roster, hand-declared
outposts. §6 and §7 replace them.

## 5. Economy (port)

The task pool + matcher is right for the economy: tasks are independent, bodies
interchangeable, and re-matching on a key change is cheap and does not
oscillate (stickiness via `assignments` is enough). It becomes **one world
pool**; a creep takes the nearest task it fits whatever colony spawned it.

**The objective is long-run growth, by simple rules first.** A slower start
that ends higher is the better economy; an early regression is accepted when
the long soak (§10.2) improves. Structures are built as their level allows, in
Layout's plan order; bodies are sized to the bank. A rule is replaced by a
priced decision only when a soak shows it losing: a pricing of every
investment by its return was built and measured, cost twice the CPU and 580
lines for no gain, and was not merged.

**A colony keeps a reserve before it spends the surplus.** With storage, the
target is every tower the level allows firing for a number of creep
lifetimes (towers × TOWER_ENERGY_COST × CREEP_LIFE_TIME × n). n is a
declaration the operator sets live (`observe declare reserve n`), 2 by
default (30k at RCL4, 180k at RCL8): the season fights daily, and v1,
spending all its surplus, held under 100k at RCL8. Below the target the gap is saved over a lifetime; above it the excess
hires upgraders in proportion. Open-source bots keep 75k–150k at RCL7–8 and
pay walls from a higher threshold.

Port with these cuts:
- No fight tasks (`Guard`, `Defend`, `Assault`, `Flee`), no Safety band, no fight
  cases in the matcher's keep cascade.
- No `Facts.fs`. Its ~1,100 economy lines (task ids, stages, hunger, perimeter)
  move into `Economy`; the rest is fight and does not come.
- `Price` (what a tick of delay costs) stays; it is the economy's pricing and
  it works there.
- Outposts, errands and the rooms a colony works come from `declarations`, not
  from `Colonies.fs`.
- Every `#NNN` comment and every branch that exists for one live incident is
  dropped on the way over. A port that keeps a patch names the invariant it
  protects in one line or leaves it behind.

Economy serves declarations it did not make: a `Mine room` from `KeeperDuty`, a
`Pickup pile` from `PowerBank`, a `Boost compound × n` from a `Siege` (labs), a
`Claim room` from Strategy (the claimer and pioneers are economy bodies until a
spawn stands).

## 6. Fight (new)

### 6.1 Tracker — who is out there

```fsharp
type Group = { Id: int; Members: Set<string>; Room: string; LastSeen: int; Centre: Pos }
val step : prev: Group list -> hostiles: HostileInfo list -> tick: int -> Group list
```

- Armed hostiles are clustered by connected components at Chebyshev distance ≤ 4
  (ranged reach + one step), across a seam when the tiles are adjacent.
  Unarmed hostiles are tracked as singletons.
- Identity: next tick's clusters match previous groups by largest member
  overlap. A split keeps the id on the largest fragment; a merge keeps the older
  id. Both are **events**.
- A group none of whose members is visible is `Lost` from `LastSeen`; dropped
  after `Tuning.LostGroupTicks` (❓ 30). This is the only vision grace in the
  fight.
- Pure; property-tested: ids stable under a permutation of the input; no group
  wider than the cluster diameter; no id reused in one tick.

### 6.2 Engagement — what we committed

```fsharp
type Target = Group of int | Room of string | Structure of string | Lairs of string
type Kind = Defend | Claim | Siege | Drain | KeeperDuty | PowerBank | Stronghold
type Engagement =
    { Id: int; Kind: Kind; Target: Target
      Ours: Set<string>; Asked: Cast list      // committed bodies, and the ones still in an oven
      Plan: Plan                                // tactic, kill order, formation — set at open (§6.3)
      Predicted: Outcome                        // what Exchange said at open; checked at close (§10.1)
      Phase: string; Since: int                 // the kind's own phase names
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
- Transitions fire on **events** — group split/merge/newcomer/lost, body died
  or landed, target gone, budget crossed, deadline, another engagement merged
  into this one, Strategy re-prioritised — never on a clock and never on a
  re-priced number. This is what removes the latches.
- A group that appears inside a room an engagement targets belongs to that
  engagement; only a group outside any targeted room can open a `Defend`. Two
  engagements whose groups merge are merged (the older survives).
- An engagement Strategy cannot staff (§7.3) is **starved**, not forming
  forever: `Defend` falls back to towers and ramparts and is marked
  `unanswered`; an offensive kind closes.
- Lives in Memory. `observe` reads it directly: the engagement *is* the raid log.

`Defend` (v2.0):
- Opened by Strategy when a group threatens: it stands in a room we hold or
  work, or its centre drifts toward one.
- `Forming → Approaching` when every asked body stands (or the group reaches a
  held room first and the standing bodies win alone).
- `Approaching → Fighting` when any of ours is in reach.
- `Fighting → Pursuing` when the group leaves the room and the plan says chase.
- `* → Disengaging` when losses exceed the budget, or a group event re-runs the
  exchange and we no longer win, or the group is `Lost` past the grace.
- `Disengaging → closed` when ours are home or the group is gone.

The other kinds, for the record's sake:
- `Claim`: Strategy picked a room (§7.2); a claimer walks, claims; pioneers are
  economy bodies on a `Build spawn` declaration; closes when the spawn stands.
  Groups in the room are its targets.
- `Siege`: muster → travel (formation) → breach (dismantle/attack under tower
  fire; `Exchange` prices towers and ramparts) → clear → done.
- `Drain`: approach → drain (edge dance, heal) → rotate → …; closes when the
  towers are empty or the deadline passes.
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
type Side = { Bodies: Fighter list; Ramparts: Set<Pos>; Towers: Tower list }
val outcome : ours: Side -> theirs: Side -> terrain: Terrain -> Outcome
// Outcome = Win of ticks * Losses | Kite of ticks | Lose
val cheapestWin : affordable: Body list -> theirs: Side -> terrain: Terrain -> Body list option
```

- `cheapestWin` is called once at open and once per event that changes a side.
  It chooses the body set; the engagement's `Plan` records the tactic the
  outcome implies, and `Predicted` records the outcome for §10.1.
- Nothing in `Fight` re-prices per tick. A tactic that needs a number every
  tick reads it off the `Plan` or asks `Atlas`.

### 6.4 Tactics — what a committed body does

A tactic is a function from (engagement, body, world) to that body's intent this
tick. Each is under 150 lines and shared by every kind; v2.0 ports the first
five from v1's step functions, the rest come with their kinds:

| Tactic | Step function (v1 origin) | Strike |
|---|---|---|
| Kite | `Resolver.kiteStep` | ranged at best range, mass when ≥2 adjacent |
| Close | `Resolver.chaseStep` + glue | melee on the plan's target |
| Hold | stand on rampart / `Atlas` seat | whatever is in reach |
| Pursue | `Resolver.chaseStep` over seams | as Kite/Close |
| Retreat | `Resolver.fleeStep` toward home | none; heal |
| Formation | new: `Move` steps a 2×2 footprint as one body | per slot |
| Dismantle | v1 `actionIntents` sapper branch | dismantle the plan's target |
| EdgeDance | new | step in to draw fire, step out to heal |

Target selection within a tactic is a static kill order set at open (healers
first, then by damage/hits), re-computed only on events. No head latch is
needed because the order does not move between events.

The heal reflex (ported `Emitter.healReflex`) runs for every body with HEAL,
inside or outside an engagement.

### 6.5 Casting

An engagement that needs bodies emits `Cast` asks with the engagement id and
Strategy's priority. The spawn queue takes them above economy casts; the
Caster logic that picks which colony spawns (hops, queue) ports from v1's
`Caster.fs` mostly as is. `fight.casts` remembers what was asked so a body in
an oven is not asked again. One room may hold three spawns; the queue is per
room.

## 7. Strategy (new)

Strategy is the world's only planner. It has no creeps; it writes declarations
and opens engagements. It runs on its key (§1), not every tick.

### 7.1 Intel

`intel.rooms` (segment): every room seen, with owner, structures, towers,
sources, controller, keeper lairs, last seen. Written from `World` each tick a
room is visible; read by Strategy on its due tick and by `Atlas` for the room
graph's avoided set. Scouting is a declaration (`Scout room`) Economy serves
with a cheap body when a row is older than ❓ `Tuning.IntelStale`.

### 7.2 Expansion and declarations

Homes, outposts, errands, avoided rooms, allies and claims are rows in
`declarations`, written by Strategy and editable by the operator (`observe
declare`). Candidates are priced once when intel changes or a due tick comes:
an outpost by sources, distance, threat and the CPU it costs (v1 withdrew an
outpost for CPU; the budget is explicit here); a claim by the same plus GCL
room; a room is avoided by what intel saw there. A priced candidate that pays
is declared; one that stops paying is withdrawn. No room is ever named in
source.

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
   Against the same window's xxscreeps phases `economy` is within 8% of its
   share, `world` and `atlas` about half (❓ the fake's plain properties for
   the engine's getters); ❓ the live recorder still writes the `World`, not
   this capture. `scripts/xxscreeps/ab.mjs` (whole soaks side by side,
   ±0.6%) re-checks a claim in the engine.

A speed claim is an interleaved A/B on the representative benchmark with
identical Memory fingerprints, or a count (floods, lookups, allocations). A
workstation's wall clock alone settles nothing (v1 measured ±6% drift on an
unchanged build).

### 9.2 F# dialect — defaults, not rules

The boundary is pure; the inside is written as a systems language. Everything
below is the **default** a module's first line is written in; any of it yields
to a measurement on the benchmark above, in either direction. The evidence is
Fable 5's emitted JS and `ab.mjs`; node micro-timings ("×") are hints only.
Most impact first:

- Module boundaries take and return immutable records. Inside a function,
  arrays, `ResizeArray`, `Dictionary`, `mutable` locals and `for` loops on any
  path that runs per creep or per tile.
- **Arrays on a per-tile or per-pair loop are read through `Tiles.at` /
  `setAt`.** `arr.[i]`, `arr.[i] <- v`, `for x in arr` and `ResizeArray.[i]`
  compile to a bounds-checked call: 23–43% of the economy (`ab.mjs`).
- **No `seq`, comprehension or stepped range per tick.** `[| for … |]`,
  `[ for … ]`, `[| a .. b |]` and `0 .. 2 .. n` build through Fable's seq
  library (~100× a `ResizeArray` loop); `for x in` a list, seq or collection
  is an enumerator in `try/finally`. A step of 1 or -1 is a plain `for`.
- **Equality is `===` only on a primitive Fable can see.** A union case is
  tested with `.IsCase` or `match`, never `=` (generic `equals`; `ab.mjs`
  −1% bot). `=`, `<>` and `compare` on records, tuples, lists and options,
  inside a non-`inline` generic function, or on an untyped tuple argument
  emit generic `equals`/`compare`: compare fields, match `Some x when`, and
  annotate primitive arguments.
- **A `Dictionary`/`HashSet` key is an `int` or a `string`** (a native JS
  `Map`/`Set`); a record, tuple or union key hashes (~40×). `Map`/`Set` are
  AVL trees, which measured noise against `Dictionary` at the matcher's
  sizes. `ContainsKey` then `.[k]` allocates nothing; `TryGetValue` allocates.
- **Numeric arrays are typed arrays** (`int`, `float`, `byte`); `bool`, `char`
  and reference elements are plain arrays. A sentinel stays only in a numeric
  table; absence elsewhere is an `option`, erased to the value or
  `undefined` (`voption` is the same; a nested or generic `'T option` wraps).
- **`[<Struct>]` does nothing:** the bundle is byte-identical without it, and
  a struct tuple is still a JS array unless destructured where it is built.
  Hot-path records carry `[<NoEquality; NoComparison>]`, which emits nothing
  but makes an accidental deep compare a compile error.
- `inline` with `[<InlineIfLambda>]` erases a passed function; worth it per
  tile only. Fable uncurries top-level and local functions.
- Strings on a hot path: `+` or `$"{x}"`, never `sprintf` (~100×) or a typed
  hole `%d{x}`.
- A `try` inside an expression is an IIFE (~3×): hoist it out of loops.
  `int64` is emulated.
- No closure allocated per tick outlives the tick (v1's `Fresh` lesson,
  measured live; one tick retained measured nothing in `ab.mjs`, an
  accumulating one was not tried): a collection that is stored is built by a
  function that captures nothing.
- `[<Emit>]` stubs take `_`-prefixed params.

### 9.3 Counts are a gate

`scripts/counts <capture>` replays a capture and prints, per tick: floods run,
Atlas cache hits/misses, `Exchange.outcome` calls, `Derived` lazies forced,
intel rows read, Memory bytes. Because `decide` is deterministic the numbers
are exact, so `scripts/ceilings.json` holds ceilings for them beside the line
ceilings — never raised; lowered when a change earns it. This is how "the same
thing computed many times" is caught: by a number, at `jj ship`, not by a
reviewer noticing. A count ceiling is the first performance regression test
and needs no representative benchmark to be trusted.

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

### 10.2 xxscreeps is the engine

`xxscreeps` (v1's `npm run xx`) runs vanilla engine rules and the vanilla NPC AI
under isolated-vm, seeded, against the built `dist/main.js`. v2 uses it for
everything v1 used its self-written arena for, and more:

- **Scenario library.** Every live incident becomes a scenario: the room's
  state at tick T (from the recorder or room history) seeded into xxscreeps,
  run N ticks, asserted on outcomes — held, losses ≤ X, the group dead by T+K.
  A fix is verified by its scenario passing and the library not regressing.
  The library runs nightly. `npm run scenario` runs `scripts/xxscreeps/scenarios/`:
  each a start as a recipe (room, seed, late start or node; ❓ a live room's
  state), its ticks and bounds on what the soak measured, scenarios on one
  start sharing one soak; while it is small, `jj ship` runs all of it.
- **Soak.** The real sector's terrain and rooms, from one spawn for thousands
  of ticks: income curve, expansion cadence, losses to NPC waves, CPU curve.
  A change to the economy is read off this curve, not off a unit test. It is
  judged on the long horizon (`npm run soak -- --ticks 30000`, where a
  structure's payback, thousands of ticks, has time to show): the RCL and
  progress at the end, the tick each level is reached, the income in the last
  thousand ticks; the first ten thousand ticks alone decide nothing (§5).
  Before it ships, the same over the two extremes of harvest seats
  (`npm run soak:many`: rooms whose sources have one seat each, and rooms
  whose sources have four or more), so no change is tuned to one kind of
  terrain. The extremes answer differently (a crowd of small bodies works
  only where seats are many), so they are read apart, never as a median.
  A soak is a relative measure: A against B on the same rooms and seed,
  reported as the difference, never as what live will do (live has
  neighbours, season rules and the owner's hand, which no soak calibrates
  against). A win is re-checked on other starts before it is believed (another
  seed, and the bot sitting out its first ticks: the seed only breaks ties,
  and a room with few ties drifts from a small change the seed never
  makes), and a loss smaller than the room's own spread across the starts
  is noise; a
  gap between soak and live surfaces as an incident and becomes a scenario.
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
  | E3 colonies | three colonies at RCL7, 5, 2 | the world pool across colonies, terminal balancing | total income, cross-colony work share, CPU vs colonies |
  | S1 invaders | N3 + timed Invader waves, home and outposts | `Defend`, towers, recovery | losses, ticks back to income |
  | S2 keepers | RCL7 + a keeper room | `KeeperDuty`, whether keeper mining pays | keeper-room net, losses |
  | S3 stronghold | RCL7 + a stronghold next door | whether to fight it, `Siege` | the decision, gain vs cost |
  | S4 low CPU | N6 with the CPU limit at 20 | degradation, the bucket | bucket curve, income lost to degrading |

  Each push runs the one or two nodes its change touches (about a minute);
  nightly runs them all with the long soak over the seat extremes. Nodes N1–N4 come first
  (the economy and links), N5–N7 with their structures, E with Strategy, S
  with Fight. `npm run soak -- --node N1` … `N4` builds those four (`scripts/plan`); ❓ the rest, with their structures.
- **Adversaries.** xxscreeps hosts several users. Self-play: the attacker is
  our own bundle with only `Fight` in its modules, an engagement declared at
  start and its squads standing, against the full bot defending; each game
  is also an `Exchange` sample (§10.1). Self-play shares our blind spots (an
  attacker that never drains never tests the defence against a drain), so
  fixed opponents stay in the library: scripted tower drain, dismantle,
  kite and blockade squads, v1, and open-source bots (TooAngel is plain JS;
  Overmind and The International need a build). NPCs do not hold a room;
  this is the only offline test offence has. ❓ the self-play harness, built
  with `Defend`.
- **Exchange calibration** (§10.1), generated.

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

Speed, measured 2026-10-10 on v1's `w11s28-invasion` (two rooms, 8 bodies,
5 invaders, three towers, the melee stand-in as our side): **43 ticks in
1,176 ms, ~27 ms/tick** of engine time, in this cloud container. The bot's own
CPU adds to that. So a 300-tick scenario is ~10 s, a library of fifty is
~8 min, a 3,000-tick soak is ~2 min plus the bot: **scenarios for the touched
modules run at `jj ship`; the full library and the soak run nightly.** The
~4 min wall time of that run was v1's `dotnet test` producing the scenario
and nix fetching Node 24, neither of which v2 pays per run.

Known limits: vanilla rules only (`Season/` is live-only, by design); small NPC
deviations (flee counts armed hostiles only; healer picks by range).
Maintenance is not a risk as of 2026-10: commits three days old, 1,176 in all,
PRs in the 400s; v1 already builds its isolated-vm against Node 24 through
nix.

### 10.3 Unit tests, where the engine would obscure

Two kinds, both under a minute in `npm test`:
1. **Invariants** over generated inputs (FsCheck): Memory round-trips; a
   Tracker id is stable under permutation; `Ours` is disjoint from
   `assignments`; no two bodies resolved onto one tile; no idle spawn while a
   cast is affordable.
2. **Pure functions**, table-driven: `Exchange` formulas, `Atlas`, `Tracker`,
   body construction, `Price`.

Not allowed anywhere: a test that asserts a specific creep's action in a
specific scenario. That is a change detector; it pins the bug you have not
found yet.

### 10.4 Replay

Because `decide` is deterministic (§1), the recorder's capture of a live tick
replays the exact decision locally: `scripts/replay <capture> <tick>` prints
the Intents and the State diff. An incident is debugged by replaying it, not
by reasoning about what the bot might have seen.

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
| M2 structures | node builder (N1–N4) ✅, links ✅ (the controller's and the longest-way source's first: +5–17k worth over 20k ticks on N4; hub-first broke even, as v1's arithmetic said), labs, factory, the RCL8 set | each node beats the economy without that structure |
| M3 defence | `Exchange`, `Defend`, the first tactics, casting, the self-play harness | self-play and the fixed opponents held; the prediction error within bound (§10.1) |
| M4 live | v2 replaces v1 | ❓ the owner decides from progress |
| M5 reach | `Strategy` over many colonies, the offensive kinds | the end in §0 |

Status: ✅ landed · ⏳ next · blank not started.

| v1 | v2 | Action |
|---|---|---|
| `App/Bindings.fs` | `App/Bindings.fs` | ✅ verbatim, plus `cpu.limit` and segment writes |
| `App/World.fs`, `Main.fs` shell | `App/World.fs`, `Wire.fs`, `Main.fs`, `Core/Tick.fs` | ✅ `World`: terrain, controllers, sources, minerals, structures, sites, piles, remains, hostiles, our creeps with bodies and stores; per-module CPU, fault isolation, switches, recorder, observe segment |
| `Types/Rules.fs` `Engine`, `Types/Vocabulary.fs`, `Types/Lists.fs` | `Core/Engine.fs`, `Core/Vocabulary.fs`, `Core/Lists.fs` | ✅ `Engine` less policy (`guardCap`) and season (`reactorCapacity`), plus part costs, regen time and level progress; `Vocabulary` less `Task` (Economy's) and with season resources as `Resource.Other` |
| `Core/Atlas.fs`, `Grid.fs`, `Geometry.fs` | `Core/Atlas/Tiles.fs`, `Grid.fs`, `Atlas.fs` | ✅ tile index `y*50+x` (the terrain buffer's), the flood, footprint on every query, a minimal room graph, the flood table. Known gaps: a route is chosen without the footprint (a Quad can be routed through a one-tile gap); travel ticks toward another room cover this room's leg only; boosted CARRY is priced unboosted. `TileSet` waits for its first reader |
| `Decide/Resolver.fs` | `Core/Move/*` | ✅ arbitration (two-tier eviction, best chain, displacer's onward last) and `Plan` wishes; re-derived each tick from `Atlas.firstStep` (§1's "path" key is not stored). Fight steps (kite, chase, flee) and formation stepping come with Fight |
| `Decide/Pool, Matcher, Planner, Quota, Spawns, Price, Bodies` | `Core/Economy/*` | ✅ one world pool, matcher, quota and casts, wired as the `economy` module (`Economy/Run.fs`): match, cast, act in range, wish toward the target; assignments in `Memory.v2.assignments`. The RCL2–4 chain: a harvester on each source's post, haulers from built containers, the controller's buffer and its upgraders, the surplus split between upgraders and workers after the reserve (§5, `declarations.reserve`). Known gaps: outposts and claims are not yet declared (colony rooms only); cross-room travel prices each crossing at a room's side |
| `Decide/Labs, Layout` | `Economy`, `Layout` | Layout ✅ in the tick after `economy`: a standing plan per room, sites in plan order as the level allows, towers on hostiles. Labs ⏳ |
| `Types/Colonies.fs` declarations, `allies` | `declarations` data | not ported as code; the current rooms seed the first `declarations` |
| `Decide/Facts.fs:2883–4056, 1038–1582, 2153–2196`, `Fighter.fs`, `Arrival.fs` | `Exchange` | port, narrow the input type |
| `Decide/Emitter.fs` strike/heal/glue | `Fight.Tactics` | port the functions, not the dispatch |
| `Decide/Caster.fs` | `Fight.Casting` | port |
| `Decide/Operations.fs`, `Types/Operation.fs` | — | shape only: `OpRecord`'s fields inform `Engagement`, its bidding informs `Strategy`. No code ports; re-read when `Siege` is due |
| `Core/LightTick.fs` | — | not ported; per-module keys (§1) |
| `Decide/Threat.fs`, rest of `Facts.fs`, `Observe.fs` fight parts, `WorldDanger.fs`, `Sightings.fs` latches | — | not ported |
| reactor / Thorium / programme code | — | not ported; `Season/` is per season |
| `tests/**` | — | not ported; §10 |
| `scripts/xxscreeps/*` | `scripts/xx` | port and extend (§10.2); ✅ the scenario library, `scenario.mjs` |
| `scripts/capture-*.mjs` | `observe capture` | ✅ pulls the recorder ring; ✅ the replay harness over a soak's capture, `scripts/bench.mjs` (§9.1) |
| `scripts/bundle, upload, cpuprofile, screeps-api`, `ship.sh` | `scripts/` | ✅ kept |
| `scripts/observe.mjs` | `scripts/observe.mjs` | ✅ rewritten small: health, cpu, faults, switches, capture, wait |
| `scripts/profile, probe, spar, wire-check, cli-smoke, adr-check` and the rest | — | not ported; `smoke.mjs` and `ceiling.mjs` are new |
| `docs/adr`, `docs/research`, `CONTEXT.md` | — | not ported; this file |
