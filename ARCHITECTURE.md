# fabot v2 — architecture

The single design document for `fabot` v2. It specifies module boundaries,
cross-tick state ownership, core invariants, and verification. Code decides
anything not stated here.

Items marked `❓` are open owner decisions.

## 0. What v2 is

A Screeps seasonal-server bot in Rust, compiled to WASM and bundled into one JS
file (ported from an F#/Fable tree; `v1` lives on the `v1` bookmark `9677b5e8`
for reference and rollback only).

Four lessons from v1 shape v2:

1. **The unit is the world.** v1 assumed one colony and bolted cross-colony
   fixes and hand-declared outposts on top. v2 decides once for the world; a
   colony is a resource and every room, ally, and outpost is data.
2. **Stateless re-derivation does not do fights.** Re-deciding every tick
   oscillates; patching oscillation with cooldowns, latches, and grace timers
   creates hidden state. v2 models each fight as one `Engagement` with an
   event-driven lifecycle.
3. **Pricing belongs to commitments, not ticks.** Casting the cheapest body set
   that wins (`Exchange::cheapest_win`) runs at commitment boundaries and is
   calibrated against real outcomes (§10.1).
4. **Outcomes in a real engine are the test.** Unit tests are reserved for pure
   arithmetic and structural invariants; behaviour is proven in `sim/` and
   `xxscreeps` (§10).

**Target:** many colonies decided as one world, each at RCL8 with full
structures, fighting every engagement kind in §6.2 within the CPU limit.
**Non-goals:** multi-shard, generality across servers, plugins, single-caller
abstractions.

## 1. The tick

```
Game ──World.ofGame──▶ World ──decide──▶ Actions ──Executor──▶ Game
            ▲                    │
         State (Memory + segments + heap, §4) ──── writes back ────┘
```

- **Pure core (`core/` workspace crates, orchestrated by `fabot-core`):** `World::of_game` reads the JS API once
  in `src/app/`; nothing in `core/` touches `Game`. `core/drive.rs` is the
  shared tick seam called by both the WASM driver (`src/app/driver.rs`) and the
  native simulator (`sim/`).
- **Deterministic `decide`:** `decide(State, World) -> (Settled, Telemetry, State')` has no
  unseeded randomness and reads no clock except for phase timing (never read by
  decisions). Any recorded `(State, World)` replays identically (§10.4). Inside
  module boundaries, code is imperative (§9.2).
- **Central settlement:** a module returns a `ModuleOutput`: engine actions,
  spawn asks, move wishes and telemetry (counts, short casts, traces) that
  never reaches the game. `Tick` settles every module's asks and wishes once
  (one spawn queue and one movement arbitration per room) into `Settled`: the
  actions, and the bodies arbitration left standing for the metrics. Split by
  type, so the executor matches only what reaches the game and `intents`
  imports neither `Fight` nor `Economy`.
- **`Executor`** turns each `Action` (`Creep`, `Spawn`, `Room`, `Tower`,
  `Controller`, `Link`, `Lab`, `Terminal`, `Observe`) into an API call and
  never decides; the driver prints the traces.

**No full/light tick.** Instead of alternating whole ticks, each module names
its own invalidation key and recomputes when the key changes or its due tick
arrives:

| Module | Decision | Key |
|---|---|---|
| `Layout` | colony plan | ground: terrain, rocks, non-own obstacles |
| `Economy` | slots | none — slots re-derive each tick; bodies keep sticky slot + task |
| `Fight` | engagement plan | engagement event (§6.2) |
| `Strategy` | operations, declarations | intel changed, engagement closed, due tick |
| `Move` | step / arbitration | re-derived per tick from `Atlas` floods |

CPU pressure has one knob: a low bucket stretches due intervals. Nothing else
degrades.

**Within the tick, derive once:**

```
World ──▶ Derived ──▶ decisions (read Derived & Atlas, never World) ──▶ ModuleOutput
```

- `Derived` and `Context` (`core/atlas/derived.rs`) are built once per tick
  below `Fight` and `Economy` so modules share facts without importing each
  other; `Declarations` (`core/base/declarations.rs`) sit below every module, and
  only the tick loop and its driver import `tick`.
- Room tile sets use bitmaps (`TileSet`) and per-tile arrays use `Table<T>`
  (`y * 50 + x`), never `BTreeSet<Tile>` / `BTreeMap<Tile, _>`.
- **One demand-driven cache (`Atlas`):** goal-seeded reverse floods per room
  keyed by `(footprint, pace class, goal, danger)`, counting hits and misses.
  Floods outlive the tick while the room's creep-free weights hold (within a tick half
  repeat and across ticks nearly all, and a flood is a pure function of weights
  so a kept one equals a fresh one). A step's flood in a room with a hostile
  on an open tile or danger walls them in a per-tick table never kept, so no
  kept flood reads a creep or danger (`--check` re-floods each hit). Room route trees (`Derived.trees`) are
  searched once a tick per colony because Economy prices every body against
  every offer (per-query search costs thousands a tick at six colonies); trees
  and outpost road walks carry across ticks in `atlas.carried` (§4), keyed by a
  fingerprint of every room they read, never hand-invalidated, and verified
  against fresh computation by `--check`.
- **One search's room pairs (`slots::PairTicks`):** `assign` and `Jobs` each
  keep the ticks of every room pair they price and drop them at their end:
  between rooms a distance reads only the pair, and not walking the tree per
  body and seat saves ~3% of warm economy.
- **Counts are a gate (§9.3):** deterministic per-tick counts (floods, Atlas
  miss rate, Hunt threat lookups) have ceilings in `scripts/ceilings.json`.

**Cold start:** after a heap reset or deploy, caches rebuild from Memory and
segments over multiple ticks (economy first); a cold tick never spends more than
`❓ 60%` of the CPU limit.

## 2. Units

- **World:** the sole decision unit (one `decide`, one engagement set, one
  `Strategy`).
- **Colony:** a resource (controller, spawns, storage, terminal); owns no
  decisions. Service assignment uses nearest walk with a bank (§5), bounded by
  room-graph hops (`REACH_HOPS = 3`, `SIEGE_HOPS = 10`).
- **Room:** a `World` fact and `intel` row; roles (home, outpost, claim,
  avoided, siege) are data in `declarations`, never hardcoded.
- **Creep:** belongs to strictly one of `Fight` (`fight-` name prefix) or
  `Economy` for its entire life.
- **No in-tick fault isolation:** wasm32 is `panic=abort`, so a panic anywhere
  traps the instance; the bundle's `loop` starts a fresh one and that tick's
  intents, Memory, intel and observe are lost (next tick cold). Production
  code denies `unwrap`/`expect` (clippy) instead.

## 3. Modules

| Module | Crate | Owns | Reads | Writes |
|---|---|---|---|---|
| `Bindings` / `app` | `fabot` | Screeps JS API seam, packed `read.js`, `driver.rs` | `Game`, `RawMemory` | `Game`, `RawMemory`, segments |
| `Engine` | `fabot-base` | Game formulas & non-global server constants (vanilla tables from `screeps-game-api`) | — | — |
| `World` | `fabot-base` | Single-pass tick snapshot, event-log strikes/destroys, per-tick `rules` (`CONTROLLER_STRUCTURES`) | `Game` | — |
| `Atlas` | `fabot-atlas` | Room graph (region nodes, border-stretch crossings, keeper-strike avoidance, blind keeper & stronghold penalty) and per-room tile grid (creep-free weights, reverse floods, seats, footprint queries; a hostile's tile is walled only in this tick's step floods and passability, never in a kept flood, so one moving redoes no flood, tree or walk); out-of-sight grids hold what stood when last seen (else terrain) struck where intel places the keepers (`atlas.unseen`) | `World`, `terrain`, `intel.rooms`, `Declarations` (colony plans, for `server`) | flood table, `atlas.carried` |
| `Economy` | `fabot-economy` | Slots, casts, sticky tasks, logistics dispatcher, links, labs, terminals, abandon | `Derived`, `Atlas`, `declarations`, `EconomyState`, Strategy's `scouts` | tasks, casts, `assignments`, `spare` |
| `Layout` | `fabot-layout` | Colony placement plan (Hivemind planner + xxscreeps JPS), rampart ring, construction sites, buried-road destroy, tower firing | `Derived`, `Atlas`, `declarations` | sites, destroys, tower actions, `standing.layout` |
| `Exchange` | `fabot-exchange` | Pure combat arithmetic (`outcome`, `cheapest_win`, breach planning) | fighters, terrain | — |
| `Fight` | `fabot-fight` | Hostile `Tracker`, engagements (`kinds/`: defend, siege, sweep, raid), casting | `Derived`, `Atlas`, `Exchange`, `declarations` (its operations table), `intel`, `FightState` | fight actions, asks, wishes, casts, `FightState` |
| `Tactics` | `fabot-tactics` | Per-creep combat actions, stepping, and the operator's tactic rules (`ruling`, `checked`) (no `Fight` import) | `Atlas`, `Move`, `Sight` + plan | creep actions, wishes |
| `Strategy` | `fabot-strategy` | Intel, observers/scouts, `ColonyPlan` & outpost selection, expansion/abandon, operation opening & global force priority | `World`, `Derived`, `intel`, `Season`, `standing.layout`, `spare`, fight state | `declarations` (`ColonyPlan`, outposts, claims, operations), `intel`, `expansion` |
| `Season` | `fabot-strategy` (private `season`) | Disposable seasonal rules (`worth`, `spent`, season structures/scoring) | `World` | declarations to `Strategy`, season actions |
| `Move` | `fabot-mv` | Room-level movement arbitration (augmenting-path matching) and stepping (`plan`, `blob`) | wishes, `Atlas` | moves & held bodies (`Settled`) |
| `Executor` | `fabot` | Turning settled actions into API calls | `Settled.actions` | `Game`, refusals |
| `Observe` | `fabot-observe` | Operator views (`health`, faults, CPU, counts, tactic offers) & windowed metrics (§10.5) | world, actions, held, telemetry, refusals, state | observe & metrics segments |

`fabot-base` (`core/base`) also holds what every module shares (`tiles`, `grid`,
`wish`, `intents`, `declarations`, intel rows, boost queries, `HashMap`) and
imports no module above it: a crate, so the compiler holds that floor.
`fabot-core` (`core/`) keeps only the tick (`state`, `memory`, `tick`,
`drive`) over the module crates.

**Dependency direction:** top to bottom, enforced at compile time by the Cargo
workspace: each crate depends only on crates earlier in this chain (`‖` joins
crates that do not depend on each other): `fabot-base`, `atlas ‖ exchange`,
`mv`, `layout ‖ tactics`, `economy ‖ fight`, `strategy ‖ observe`, `fabot-core`.
`fabot-economy` and `fabot-fight` cannot import each other; `season` is a
private module of `fabot-strategy`. Declarations flow down only.

**Line, comment, and count ceilings** live in `scripts/ceilings.json` (checked
by `npm run ceiling` and `npm run scenario`). `core/fight/price` (offers, set
shapes, casting) holds its own ceiling apart from the rest of `core/fight`:
fight kept hitting one ceiling, and pricing is the part that changes on its own.

## 4. State

Every cross-tick value lives in this table with one owner. `State` composes
one slice per owner (`EconomyState`, `LayoutState`, `FightState`,
`StrategyState`) and each module's `decide` takes `&mut` only to its own, so
the compiler holds ownership. Adding a row is an architecture change, not a fix.

**Storage tiers:**
- **Memory (`RawMemory`):** parsed once per tick in `core/memory.rs` under
  `Memory.v2` (global `Memory` is never touched; v1 `fabot`/`creeps` keys are
  dropped on first read). Holds only per-tick essential state.
- **Segments (`RawMemory.segments`):** `1` observe, `2` intel, `3` metrics,
  `10–14` recorder ring. Used for infrequent, large, or operator-facing data.
- **Heap:** rebuildable caches only (§1 cold start).

| Store | Shape | Owner | Lives in | Why |
|---|---|---|---|---|
| `assignments` | creep → `Assignment` (slot, task; `slot\|task` text only in `memory.rs`) | Economy | Memory | sticky slot + task across ticks |
| `fight.engagements` | list of `Engagement` (§6.2), with open tick (never moved) | Fight | Memory | active combat commitments (without heap-only `offers`); a home's deaths since open are its raid's |
| `fight.groups` | list of `Group` (§6.1) | Fight | Memory | hostile cluster identity across ticks |
| `fight.casts` | pending casts by engagement | Fight | Memory | prevent double-casting while spawning |
| `fight.due` | colonies with imminent siege successor | Fight | Memory | feeds `ColonyPlan.owes` (§5) |
| `declarations` | operator overrides + per-tick `Strategy` plans (`ColonyPlan`, outposts, claims, operations, flows, allies, enemies, sieges, avoid, tactic rules) | Strategy | Memory (operator); plans rebuilt per tick | world configuration as data |
| `expansion` | in-flight claim, failed rooms (100k fade), abandoned colonies | Strategy | Memory | outlives travel, stock dips, and raze ticks |
| `switches` | module name → bool (`false` disables; `recorder: true` opts in) | Main | Memory | live kill switches |
| `observe.*` | health, faults, per-module CPU, counts, tactic offers, alerts | Observe | segment 1 | operator telemetry & benchmark calibration |
| `intel.rooms` | per-room snapshot (seen tick, owner/level, reservation, worker (another player's WORK body, never an Invader), keeper posts, controller/sources/containers, mineral, stronghold; by username, never stance) | Strategy | heap + segment 2 (every 100 ticks) | serve out-of-sight rooms; price outposts & expansion |
| `metrics.windows` | 100-tick aligned windows (per-room + world, up to 150 windows) | Observe | segment 3 | `observe metrics` & soak `metrics.jsonl` (§10.5) |
| `recorder` | deflated `read.js` pass + Memory + intel ring | Main | segments 10–14 | live capture for benchmark & replay (§9.1) |
| `engagements.log` | closed engagements (`Predicted` vs `Actual`) | Observe | segment `❓` | live `Exchange` error tracking (§10.1) |
| `metrics` | open window sums & per-creep/site/room deltas | Observe | heap | incremental tick deltas |
| `standing.layout` | per-room layout plan & incremental drawing state + ground key | Layout | heap | rebuilds at 4 variations/tick after reset |
| `spare` | colony → spawn ticks/life left by home's own bodies at a steady pace; never an outpost's body or spend | Economy | heap | Strategy prices outposts against it |
| `scouts` | scout targets | Strategy | heap (this tick) | derived from intel for Economy |
| `sightings` | room → armed hostile / core id → expiry tick | World | heap | vision grace for stood-down outposts |
| `atlas.floods` | per-room cached reverse floods (bounded per room) | Atlas | heap | reused while room weights hold |
| `atlas.unseen` | out-of-sight room grid + the keeper posts and blind flag it was struck from | Atlas | heap | keyed by those posts, not by sight alone, so intel arriving after a cold tick strikes a keeper room pre-read blind |
| `atlas.carried` | carried room trees & outpost road walks + room-read fingerprints | Atlas | heap | a search reads no room past its hops so a carried value equals a fresh one while those read the same (saving ~1/10 of decide); drops the tick after going unasked |
| `terrain` | room → `LocalRoomTerrain` (borrowed by `World`) + `getRoomStatus` | World | heap | terrain never changes and status only when novice/respawn ends (prefetched to `SIEGE_HOPS` to avoid seam calls); rooms of foreign status are never routed/served (engine traps creeps on exit) and unread rooms never observed (off-shard refuses observer) |
| `buffers` | emptied `World` lists from last tick | World | heap | capacity reuse without allocation (§9.2) |
| `fixed` | structure id → immutable type/tile/owner/hitsMax | World (`read.js`) | JS heap | avoids repeated getter cost (~1/5 of `world`); checked by `replay --check` |

**Rules:** Memory and segments are written once per tick by `Main` from `State'`;
no `thread_local!`/`static` outside `app/driver.rs` and the heap rows above; no
room/player names or object ids in source.

**Dropped from v1:** multi-part danger tables, stand-down latches, raid
episodes, exit watches/holds, head latches, recall flags, quarries, economy
carries, camp prices, room latches, roster, hand-declared outposts.

## 5. Economy

**Objective:** long-run growth by simple rules first; replace a simple rule with
a priced decision only when a long soak proves it loses (a per-investment return
pricer cost ~2× CPU and 580 lines for no gain and was not merged).

### Slots, casting & bodies

- **Unified slot model:** each flow emits slots (home/outpost dig, home/outpost
  carrier pools, reservers, workers, upgraders, mineral miners, scouts,
  claimers/pioneers). Casts = open slots minus living bodies. Bodies hold their
  slot and task until done or gone, avoiding churn. An outgrown body is weighed
  by the parts it was cast with, dead ones too: a wound a tower heals never
  retires it.
- **Carrier separation & sizing:** home carriers fetch only at home (so the
  spawn ring never waits on outpost trips); outpost carrier slots are numbered
  separately so shrinking pools keep outpost slots filled. Haulers pick 2:1 or
  1:1 `CARRY:MOVE`, whichever delivers more energy/life per energy cost on the
  route (`bodies::carry_per_move`). Small emergency haulers (< half target
  shape) hold their own slot while CARRY is short.
- **Cast priority:** home floor & home carriers queue ahead of fight casts (they
  refill the bank fight casts spend); mineral miner queues before outposts;
  while stock is past reserve (or a pile's draw beats decay), upgraders and
  pre-storage workers queue before outpost haul (the haul only adds to the stock
  they draw down); the outposts' carriers take the spawn time the colony's other
  bodies leave over a life, so a far ring of outposts never casts nothing but
  haulers. While `ColonyPlan.owes` is true (a fight cast owed or imminent siege
  successor), no surplus-spending body is spawned so spawns stay free.
- **Spare (§4 `spare`):** home's own bodies at a steady pace, its floor a
  constant 2 workers whatever this tick's floor: home's repairs and sites move
  with the outposts' haul wear, so a floor read from them made the choice flap.

### Logistics, banks & links

- **Dispatcher (`jobs::rank`):** Hivemind's priority → weight → distance order
  with ring over-booking demotion, plus departures proven by soak: non-ring
  spots are booked down by inbound loads (replacing per-holder penalties);
  harvest-bay containers keep a full bay's reserve for their harvester; a hub
  pile outranks other sources while the ring has room; workers fetching their
  own take the nearest load; held tasks run to completion.
- **Hub pile (`ColonyPlan.pile`):** from RCL2 until storage stands, remotes
  deliver to the planned storage tile (a paved tile blocking no site). Keeps no
  reserve because ground energy decays every tick. While banking on a pile,
  carriers deliver directly to home workers so workers stay on site.
- **Harvest bays & links:** each source post is a Hivemind harvest bay (from
  RCL3, ahead of extension bays); harvesters fill bay extensions before links,
  and spawns draw bay extensions first (`drawn`) so that energy is never hauled.
  Links run Hivemind's single send/tick network (fullest source link → emptiest
  controller link, with hub fallback); carriers bring a controller link by hand
  only while upgraders drink more than links send, and while they do **only
  upgraders draw the controller's buffer**, or a worker's draw is carried back
  from the stock.

### Surplus, reserve & spending

- **Surplus:** `dig − upkeep + (stock − reserve) / life` (for a hub pile, decay
  counts as upkeep over drawdown horizon `√(2 · D · u · life)`). Before a
  storage, an outpost's dig counts only as far as the CARRY on the outposts'
  carrier slots stands (with no reserve to draw down, spend sized to the rest
  drinks the bank that would cast them).
- **Below storage reserve:** surplus is negative below the reserve, so spending
  it draws stock down to the reserve and no further; **below a storage's reserve
  the surplus is the upgraders' alone** (whom carriers feed) — upgrade is the
  score, so it shrinks with the stock rather than stopping while it refills.
  Workers draw only a full load past reserve, so none spawns short of it except
  the downgrade-deadline upgrader and the home-road mender, both permitted to
  draw reserve.
- **Build vs upgrade:** paying structures (spawn, extension, container, storage,
  link, tower, extractor, terminal) preempt upgrading because a structure pays
  only once complete (only RCL3's tower and RCL6's minerals are worth rushing);
  below RCL3, outpost sites wait for the tower: its dig piles on the post (or
  beside the source while no container or site marks one) and is hauled from
  there, while an outpost's harvester with a CARRY on its container site raises
  it with its dig (uncounted by carriers or spend until the container stands).
  Non-paying sites and repairs wait behind the controller (taking stock past
  reserve, or half income before storage).
- **RCL8 cap & terminal aid:** at RCL8, controllers upgrade only inside the
  downgrade deadline; surplus goes to sites, ramparts, labs, and terminal aid
  (`derived::give_aid`): stock past reserve, a 20k margin, and its sites' cost
  ships to the lowest-RCL colony below RCL8, nearest first (first, from any
  colony, to one under half its reserve, furthest short) — flowing only to a
  colony holding less so it never flows back.
- **Repairs:** structures decaying within a creep lifetime are repaired ahead of
  building; stocked colonies dispatch a dedicated 2-WORK outpost mender when an
  outpost road is at risk or its walk lacks one (Hivemind's mine builder counts
  a missing road as work, first paving or lost alike), cast ahead of the far
  haul as Hivemind's is, since the haul's recasts otherwise hold every spawn.
- **Reserve (`strategy::reserve`):** threatened colonies (rival within 3 rooms,
  home fight open, or fight cast owed) keep `towers × TOWER_ENERGY_COST ×
  CREEP_LIFETIME × n` (`n` live-declared via `observe declare reserve`, default
  `2`); quiet colonies keep 1/3 because a full reserve costs a quiet young
  colony a lifetime of upgrading while threats come from neighbours intel sees.
  Owed fight casts and (when GCL allows expanding and `expand::may_start`, since
  one claim flies at a time) one sponsor's 30k expansion margin + one refill
  (`expand::book`) are added to reserve. Builders draw
  reserve only for paying sites and ramparts (`Ground::kept_from_builders`,
  `derived::drawn_for`), because lowering the reserve itself also feeds surplus
  upgraders and drains a raided stock.

### Layout & ramparts

- **Room planner (`layout/`):** full step-for-step port of Hivemind's planner
  (partial ports kept missing its placement order) running on xxscreeps' JPS/A*
  Dial-heap pathfinder (`layout/path.rs`; notices in `THIRD_PARTY_NOTICES`),
  spread at 4 variations/tick in `standing.layout`. Reads terrain/rocks alone.
  Structures build at the RCL Hivemind's room manager first unlocks them
  (towers, spawns, harvest bays from RCL3, extension bays in order; plain roads
  at RCL4 behind storage, swamp roads to sources/controller from RCL2). Key
  departures: near-exit source fallback (3 tiles vs 6); harvest-bay spawn built
  after extension bays; heavier source/controller distance weights (`0.02` vs
  Hivemind's `0.003`/`0.002`) to the first-built spawn because until RCL7 that
  spawn's haul is the economy (Hivemind's weights lost 95k on soak); multi-seat
  source container before extensions; no
  labs below RCL8; nuker last; unreachable cut pockets kept unroaded; source
  links never split a 1-wide post.
- **Ramparts (`strategy::wall`, `ColonyPlan.rampart_hits`):** kept at `ticks
  held × dismantler damage/tick`, where dismantler damage is the largest rival
  dismantler up to our RCL (capped at our level because a taller wall outbids
  the economy holding it and safe mode answers; boosted if rival has labs), and
  `ticks held` is as long as our towers take to kill it through its quad's heals
  from the ring's far end, capped at the time a colony casts its answer twice.
  Quiet colonies keep 10k (1/3 of full at RCL8). Core structures (spawn, tower,
  storage, terminal) get individual ramparts from RCL4 ahead of roads. The
  min-cut **ring** cuts rectangles 4 tiles around spawns, core structures, labs,
  power spawn, nuker, observer, and extension bays, 1 around controller, and 0
  around its link and road (harvest bays without a spawn stay outside), with
  on-ramps, quad-breakers, and decorative walls sealing every tile in ranged
  reach from exits. Built from RCL4, or earlier while a nearby rival quad
  out-heals our towers (`ColonyPlan.ring`); once built, a ring stays and is
  repaired even if the rival leaves. `❓` No scenario yet seats a rival whose
  quad out-heals our towers, so the ring itself is unproven.

### Served declarations, labs & abandon

- **External declarations:** Economy serves `Claim` (claimer, raising and 4
  pioneers until RCL4/storage, all from `atlas::sponsor`: the nearest stocked
  colony not given up, else the nearest whose bank casts a claimer; one rule
  on `Derived` facts that `outposts::busy` holds too), `Scout`, `Boost`, and future
  `Mine`/`Pickup` declarations without owning them.
- **Labs (`economy/labs.rs`):** `labs::plan_for` derives a stateless per-tick
  reaction plan from `declarations.flows` toward target compound stocks; labs
  reserved for active boosts (§6.5) drop out of reactions and are filled by
  carriers via `Ground::boost_lab`. Layout places no lab below RCL8 (labs are
  worth little early); below RCL8 a declared cluster in a new room is inert.
- **Abandon (`economy/abandon.rs`):** declared colonies stop digging, building,
  and upgrading past deadline, ship storage/terminal/lab contents out (energy
  last), destroy own structures (`destroy()` requires our controller, and owned
  structures left after unclaim keep the room visible in every world read), then
  unclaim.
- **Cuts from v1:** no fight tasks in Economy (danger walling lives in `Move`
  via `Derived.danger`, omitted in own rooms so tower refills never flee); no
  `Facts.fs`; no hardcoded colonies/outposts; no one-incident patches.

## 6. Fight

### 6.1 Tracker — who is out there

`tracker::step(grace, held, owned, prev_groups, hostiles, tick) -> (groups, events)`

- **Clustering:** armed hostiles cluster by connected components at Chebyshev
  distance ≤ 4 (including across room borders). Unarmed hostiles are singletons.
  *Armed* = `ATTACK` or `RANGED_ATTACK`, `HEAL`/`WORK` beside one in the room,
  or `WORK` alone in an owned room.
- **Identity & events:** clusters match previous groups by largest member
  overlap. Splits keep the id on the largest fragment; merges keep the older id.
  Emits `Split`, `Merged`, `Left`, `Newcomer`. New ids strictly exceed `held`
  (highest id in groups or engagements).
- **Vision grace:** invisible groups turn `Lost` and expire after
  `LOST_GROUP_TICKS = 30` — the sole vision grace in Fight.

### 6.2 Engagement — what we committed

One `Engagement` struct (`core/fight/shape.rs`: `objective`, `id`, `group`,
`room`, `ours`, `plan { tactic, order, offers }`, `phase`, `opened`, `since`,
`lost`, `budget`, `priority`, `unanswered`, `ignored`, `capacity`, `predicted`),
with one phase machine per kind under `core/fight/kinds/`.

**Common rules:**
- **Committed force:** fight creeps (`fight-` prefix, §2) stay Fight's for life;
  creeps in `ours` are committed to their engagement and move to Fight's free
  pool (never Economy) only when released or when it closes. Long-lived kinds
  rotate replacements on expiry (`walk + spawn time`).
- **Event-only transitions:** transitions fire only on discrete events (tracker
  events, body died/landed, target gone, budget crossed, deadline, merge,
  re-prioritisation, or affordable capacity growing past `capacity`) — never on
  clocks or per-tick re-pricing.
- **Ownership & merging:** groups inside a targeted room join its engagement;
  merging groups merge their engagements into the older id.
- **Starvation (`unanswered`) & safe mode:** unstaffable engagements fall back
  to `unanswered` rather than forming forever. An unanswered home `Defend`
  activates safe mode if no tower stands, a raid reaches core structures / half
  of spawns+extensions, a claimer reaches the controller, or player raids cut
  haulers below refill capacity.

**Kinds implemented:**
- **`Defend`** (`kinds/defend.rs`): opened on threats in or drifting toward the
  table's `Defend` rooms (§7.3: owned, worked, and the rooms their haul
  crosses; enemy workers in outposts and level-0 Invader cores count, as
  structure-less hostiles; Source Keepers never (`hostiles::foe`); the same
  rooms are the ground it pursues on). Phases: `Forming →
  Approaching → Fighting → Pursuing → Disengaging → closed`. An outpost whose
  losses exceed budget while it has lost fewer than two (`OUTPOST_LOSSES = 2`)
  re-forms and re-prices; it closes only once ours are home and `sightings`
  clears (closing sooner reopens on the same raid with a fresh budget and feeds
  it bodies). Blind, with no body of ours standing or asked, it disengages
  and keeps its budget the same way. A body with nothing alive but MOVE,
  TOUGH and CARRY counts as lost to an outpost (nothing there heals it; it can
  end no raid) and no engagement drafts it until healed. Home `Defend` never
  disengages (falls back to `unanswered` on towers/ramparts until the group
  is gone).
- **`Siege`, `Raid` & `Drain`** (`kinds/siege.rs`, `kinds/raid.rs`): opened on
  the table's `Siege` rows, one per `declarations.sieges` room (kept visible by
  observer/scout). Targets towers and economy structures, least struck by
  charged towers first and of those highest raze cost to the rival, then
  declaiming enemy controllers. A **`Raid`** is a ranged blob (`BlobKite`) in an
  unbarriered room, fighting on the line enemy towers fire into the heal it
  pools there. A **`Drain`** phase runs while charged enemy towers out-strike
  breach heal, using **one bouncer/medic pair per 5,100 bank capacity (three
  pairs at most, full speed on plain)** bouncing across the border gate; a safe
  mode refuses a breach but not the border edge, so a siege still opens and
  drains charged towers while safe mode runs if drain pairs out-heal the gate
  volley (spending safe mode rather than letting it block; otherwise safe mode
  stands the set off), then enters on the breach plan once drain ends. If towers
  leave `Ignored` consecutive trips unanswered in the event log (reset on any
  tower attack), the drain gives up and holds the edge without casting more —
  staying open so the room does not reopen afresh. Breach barrier order is
  derived from `Exchange` on open/events, never stored stale.
- **`Sweep`** (`kinds/sweep.rs`): one active at a time on enemy-owned/reserved/
  worked rooms (the table's `Sweep` rows, §7.3), priced by prey plus
  exposed-source harvest over a hunter's life per walk tick (denial is worth as
  much as kills). Living sets spend their life in the enemy's field, rotating
  onward via `atlas::onward` (profitable rooms first, else stalest patrol; rooms
  with a tracker group still in `Lost` grace are not stale so a set never walks
  straight back into an escort it just left) rather than returning home; closes
  on death, safe mode, or no remaining targets.

**Planned kinds:** `Claim` (claimer + Economy pioneers), `KeeperDuty` (lair
rotation declaring room minable), `PowerBank` (break → declare `Pickup`),
`Stronghold` (`Siege` on core with wave groups).

### 6.3 Composition and the exchange

`Exchange` (`core/exchange/`) is pure combat arithmetic over narrow inputs:
`Side { bodies, ramparts, towers, cast_walk, together }`, `Terrain { weight, run_off }`,
`Stance { Hold, Standoff }` → `Outcome { Win(ticks, losses), Kite(ticks), Lose }`
and `cheapest_win` (adds only to already-standing `ours`).

- **Stance:** under `Hold`, raids come to our post; raids out-ranging our
  strikers and outlasting towers price as `Standoff`.
- **Arrival:** a body counts from the tick it lands (a cast's spawn time and
  walk included), so a set fed in one at a time prices worse than one walked
  in `together`, which pays the wait in ticks instead (a body already in the
  raid's reach is struck alone through it). `Defend` prices both arms
  (`Close`/`Kite`/`Hold` vs `Blob*`), rather than holding bodies back until
  they gather (a hold-while, rejected). Its `BlobClose` walks the set as one
  into melee of the kill order's first standing, through the fight.
- **Focus:** the Exchange prices the kill our bodies strike: both rank the
  raid by `fighter::kill_rank`.
- **Losses:** a set's cost charges every body of ours the outcome loses,
  standing or cast, once at its body cost (a survivor at the share of hits it
  lost, never its cast: it fights the next raid), so a death never prices
  below a few ticks saved. `cheapest_win` also charges each cast it buys, by
  design, not a double charge: without the cast a survivor is free and the
  set grows to its cap; without the losses it buys bodies too cheap to survive.
- **Consequence pricing:** every threat, structure, and seat is valued in
  **energy/tick** (razed, killed, healed, or denied) — never by body parts or
  creep kind. Feature-keyed rules leave exploitable gaps (Trepidimous's
  defenders guarded our rangers while dismantlers razed his home); wrong choices
  are fixed by adding the missing term to the price.
- **Tactic selection:** each kind declares candidate tactics; pricing scores
  each candidate as `Offer { tactic, value, cost, bodies }`, and
  `tactics::select` applies operator `forbid`/`pin` rules then picks max
  `value − cost` (ties: fewest bodies, declared order). Offers stay heap-only
  for `observe engagements`.

### 6.4 Tactics — what a committed body does

`core/tactics` (no `Fight` import) maps `(Sight, plan, body)` to actions and wishes:

| Tactic | Behaviour |
|---|---|
| `Kite` | Maintain optimal range while firing |
| `Close` | Close distance + glue onto moving marks; a `Defend` body (`Kite` too, not the blobs) outside its centre's room enters it by any tile and ranks no mark until in. A siege's loose bodies keep its blob's line in |
| `Hold` | Hold rampart / `Atlas` seat |
| `Pursue` | Chase across room borders |
| `Retreat` | Fall back to `ColonyPlan.muster` (off plan roads/structures) |
| `Formation` | Step 2×2 footprint as one body (with `Siege`) |
| `Dismantle` | Dismantle target; medics seat beside the body towers struck last tick |
| `Drain` | Bouncer/medic border-gate volley soaking |
| `Hunt` | ibex-style EV over 9 neighbouring tiles (worth − netted damage + glue) |

**Reflexes:** strike (swing + shot/volley by stance, static open-time kill
order, overridden by `Hunt`'s scored shot) and heal run for every armed/healing
body. All creep acts pass through `intents::performed` so engine pipeline
conflicts never silently cancel an act.

### 6.5 Casting

- **Queue (`Tick::queue`):** per-room priority `Floor → Fight → Economy`; an
  affordable fight ask waiting on energy holds economy asks behind it.
- **Colony choice (`fight/price/casting.rs`):** picks the colony landing the body
  soonest (hops + spawn queue + energy refill time); home `Defend` casts at
  home. Bodies end with one `MOVE` (`with_move_last`) and carry `HEAL` when
  leaving owned rooms (except siege dismantlers covered by medics).
- **Boosts (`core/base/boost.rs`):** if the declared lab cluster covers a compound,
  the cast encodes it in the creep name (`fight-<tick>-<eng>-<slot>-<compound>`).
  `Tick` holds the creep at the boost lab while parts need it, stock covers it,
  and a carrier exists — a stateless tick fact, never a timer.

## 7. Strategy

World-level planner with no creeps; writes `declarations` and steers intel,
expansion, and operation priorities.

### 7.1 Intel

- `intel.rooms` updates every tick from visible rooms and persists to segment 2
  every 100 ticks. Out-of-sight outposts and claims are read from intel.
- **Observers:** watch declared sieges every tick, then stalest rooms older than
  `STALE` (1 creep life) in range.
- **Scouts:** uncovered declared rooms and `threat_radius` rooms (within 3 of
  owned rooms, needed from RCL1 for reserve & outpost selection) emit `Scout`
  declarations. Living 1-MOVE scouts rotate continuously via `atlas::onward`;
  unseen rooms offer nearest ring first and cast ahead of spend. Armed rooms are
  skipped except declared sieges.
- `❓` Grow intel rows with towers/sources on first reader and derive avoided
  rooms from them.

### 7.2 Expansion and declarations

- **Stance (`world::Owner`):** resolved once at read time from `allies`,
  `enemies`, and declared sieges' owners. A stranger is fought where it threatens
  us or works an owned or worked room (an outpost is ours to defend), never
  followed out or hunted.
- **Colony plans & outposts (`strategy::plan`, `strategy::outposts`):**
  rebuilt each tick under operator overrides (**operator rows always win**).
  Outposts run from RCL2 through RCL8 (an RCL8 controller takes 15 of a
  two-source home's 20/tick, leaving nothing to bank for boosts, labs, or
  ramparts without outposts): operator rows first, then standing outposts
  (containers at all sources), then highest net energy/spawn-tick within
  `cpu_limit / 2` and spawn `spare`. Each outpost is priced with the spenders
  its dig casts (2 parts per energy/tick) because a fixed half left spawns idle
  at RCL3–6 and full life past RCL6 starved hauling; a young home reserves half
  its spawns' life for workers so its pile does not decay unspent. Stocked homes
  add one non-standing outpost at a time so spawns absorb one room's bodies at
  once; young homes open all paying ones because waiting on containers delayed
  RCL3. An operation holds only the colonies it casts from (`outposts::busy`):
  a claim's sponsor until its spawn stands; a siege or sweep, from its
  operation row to its engagement's close, every colony Fight's `colony_for`
  picks at rest for some body (`fight::casting::homes`: none nearer out-banks
  it), since between waves no cast is owed; a
  Defend its owed casts (`ColonyPlan.owes`). Held, they add no new outposts
  and sponsor nothing, while the others keep growing.
- **Expansion (`strategy::expand`):** automatic, one in flight. On due ticks
  (100) while GCL allows and `bucket >= 9000`, a non-busy RCL4+ sponsor with
  storage holding `reserve + 30k` scores free 2-source rooms ≥ 2 rooms from
  owned/rival rooms by dig + free neighbours + mineral boost demand
  (`1/(held+1)`) + `season::worth` − crossings − fading failure penalty (a
  nearby stronghold adds no cost because its creeps/towers stay at its core and
  Atlas routes around its room). The best room that passes a full `Layout` fit
  check is declared claimed until its spawn stands and tracked until storage
  stands. Fails on rival claim, 5-life claim timeout, 40k storage timeout (~2×
  the 21k ticks a room two crossings away takes), or expired safe mode without a
  tower (failing applies a 100k-tick penalty within 1 room).
- **Abandon:** Strategy marks a colony `Stage::Abandon` (never the last colony
  or one with an open engagement) when its young expansion failed,
  `season::spent` is true, or at GCL cap a sub-RCL6 colony loses to the best
  free candidate by ≥ 1 source's dig. A given-up room counts as a failure so it
  is not re-claimed soon.

### 7.3 Operations and force allocation

- Candidate operations (`Siege`, `PowerBank`, `KeeperDuty`) are priced once at
  open via `Exchange` against intel and opened only when positive EV.
- **Global force allocation:** engagements compete across the world by
  `Priority` (value per body asked); highest priority is staffed first, lower
  starved (§6.2). This is the world-level rule that keeps one rival striking
  three rooms from being three independent fights over one force.
- **Operations table:** `strategy::plan` writes `declarations.operations`, rows
  of (kind, room, priority): `Defend` on each home (2) and outpost or claim (1)
  and each room on its tree route from the colony that serves it
  (`atlas::server`, Economy's choice too) that no other player owns or, unless
  an ally, reserves (1: Hivemind's remote is under attack while any room its
  haul path crosses is, and its paths never cross an owned room; a Defend in
  a rival's remote fights his guards, a stance the enemies list sets),
  `Siege` on each declared siege (0), `Sweep` on each preyed room, stalest seen
  first (0), whether or not a sweep is open. `Fight` opens only on a row's room
  and ranks by its priority, but keeps its event gate (`stirred`) and prices
  winnability at open (`Exchange`, `drainable`): which rooms and how they rank
  is Strategy's call, whether a set wins there is Fight's (§6.2).

## 8. Season (disposable)

`core/strategy/season.rs`, a private module of `fabot-strategy`, isolates all
seasonal rules: scoring, season structures and resources, `season::worth`
(added to expansion score), and `season::spent` (triggers abandon once mined
out). Vanilla returns `0.0` / `false`. Only `Strategy` imports `Season`; at
season end the file is rewritten without touching the rest of the bot.

## 9. Performance

### 9.1 The benchmark comes first

No performance change ships without the representative benchmark:

1. **Real input:** live capture from the recorder ring (`observe capture`) or,
   offline, a soak capture (`npm run soak -- --capture a-b`). Never synthetic
   scenarios.
2. **Engine cost model:** `bench.mjs` replays under live isolate heap limits
   with Memory JSON parse/stringify, 0.2 CPU/intent, and cold ticks reported
   separately.
3. **Live calibration:** `Main` logs per-module `Game.cpu.getUsed()` deltas to
   `observe.cpu.modules`. The benchmark is representative once each module's
   share is within `❓ 15%` of live (`❓` calibration waits for live capture).

Noise floors: `npm run bench` reads within `±0.4%` per module on an unchanged
bundle and `scripts/xxscreeps/ab.mjs` within `±0.6%` on whole soaks (compared
with v1's `±6%` workstation wall-clock drift). Speed claims require an
interleaved A/B with identical Memory fingerprints, or a deterministic count —
never bare wall-clock.

### 9.2 Rust dialect — defaults, not rules

Pure at module boundaries, systems Rust inside (measurements override defaults):

- **In-place `State`:** modules mutate `State` in place (`ModuleRun`),
  avoiding ~700 allocations/tick from per-module clones; tight
  per-creep/per-tile loops avoid per-tick allocations.
- **Single-pass JS→WASM seam:** ~90% of world-read cost was seam calls rather
  than work, so `src/app/read.js` packs the tick into one `Int32Array` +
  interned string buffer (interning cut `world` by −17%), decoded by
  `core/base/world.rs`; never add per-field JS calls.
- **Copy `world::Id`:** 96-bit left-aligned hex + length (`Copy`, lex-ordered as
  text), avoiding `String` allocations and pointer hashing.
- **`#[inline]` across crates:** rustc inlines a function into another
  crate only when it is `#[inline]` or tiny, and neither the incremental test
  profile nor release LTO makes up for it, so a hot helper another crate calls
  takes `#[inline]` when a profile shows the split took it out of line (each new
  crate boundary repeats that pass); without them the base split cost economy
  +3%, atlas +6%, settle +8% warm and the core suite 2.2 → 3.0 s.

### 9.3 Counts are a gate

Deterministic per-tick counts (floods, Atlas miss rate, Hunt threat lookups) are
reported to segment 1 and enforced against ceilings in `scripts/ceilings.json`
on every `npm run scenario` run.

## 10. Verification

Inverted pyramid: real-engine outcomes are primary; unit tests cover pure
arithmetic and invariants.

### 10.1 Exchange is checked against what happened

Every engagement records `Predicted` at open and compares against actual
outcome at close. Offline, `npm run exchange` and fight scenarios evaluate
predictions via `scripts/xxscreeps/prediction.mjs`; `predictedWinsLost` fails a
scenario as an `Exchange` bug. `❓` Live `engagements.log` segment and
`observe exchange`.

### 10.2 The engines

Two engines complement each other:

- **`sim/` (native Rust on `krebs`, official engine rules):** zero WASM/JS, PGO
  build (`npm run build:sim`), default for economy scenarios and `soak:many`
  (one process per core). Models official backend cron timing (`genInvaders`
  every ~88 ticks, `INSIDE_PASS = 0.66` MLE-fit to Season 11 history), scripted
  waves/level-0 cores, and `--check` cache verification. A season world
  (`season-world.mjs`) carries each sector's centre 3×3 (centre and keeper
  rooms) because `genInvaders` raids only a sector with a live core and
  `genStrongholds` stands one only from a centre room in the world. When `sim`
  and `xxscreeps` disagree, `@screeps/engine` / `@screeps/backend` settles it.
  - `❓` Still on `xxscreeps` (`SIM_START`/`SIM_MEASURES` in `scripts/sim/run.mjs`):
    active rival bots/raids, resumed worlds (`at`/`bundle`), capture intel
    segments, and unported measures (`decay.jsonl`, `waves`, `lost`).
  - `❓` Cut worlds lacking sector bus rooms throw in `genDeposits` (logged and
    skipped as backend cron does).
- **`xxscreeps` (`npm run xx`):** isolated-vm vanilla engine used for active
  rival bots, `--bot`/`--base` comparisons, `--capture`, and `--watch`; with the
  first three, `npm run scenario` skips sim-native scenarios unless `--force-xxscreeps`.

**Scenario library (`npm run scenario`):** every live incident becomes a recipe
scenario with outcome bounds (`scripts/xxscreeps/scenarios/`). `jj ship` runs
scenarios matching touched modules.

**Soak & reading rules (`npm run soak`, `npm run soak:many`):**
- Judged on the 30k-tick horizon by end RCL & progress, the tick each level is
  reached, and income in the last 1,000 ticks (the first 10k ticks alone decide
  nothing).
- `soak:many` runs across seat extremes (1-seat vs ≥4-seat sources, read apart
  and never as a median because a crowd of small bodies only fits many seats),
  `season-outpost-ring` (1-source remote economy), `season-late-seat` (10k ticks
  of N7; RCL8 controller progress buys only GCL so it is read apart and never
  counts as a regression), and 10k ticks of `E1` on `season-outpost-ring` with
  Hivemind two rooms off (`--xxscreeps`, Hivemind over TooAngel because it
  reaches RCL3 and walks outposts while TooAngel stays RCL2) to read exposure
  and losses with safe mode kept.
- `measures.mjs` enforces invariants on every home (no lost road, no level drop,
  ≥3 ramparts post-RCL4, no paying site waiting >10k ticks, zero refusals/faults).
- Soaks are relative A/B comparisons; a win is re-checked on other starts (a
  second seed and delayed start), and any loss smaller than base start-spread is
  noise.

**Nodes** (layout-seeded starting states exercising mid/late game quickly):

| Node | Start | Proves | Read |
|---|---|---|---|
| N1 respawn | RCL1, one spawn | opening, investment order | ticks to RCL2/3, first road & container |
| N2 RCL3 | 10 ext, tower, source containers | road/container return, tower refill | income, walk share, upgrade rate |
| N3 RCL4 | 20 ext, storage 5k | storage logistics, hauler ratio vs roads | storage net flow, haulers, ticks to RCL5 |
| N4 RCL5 | 30 ext, 2 towers, storage 50k, links | how much hauling links replace | link throughput, haulers (fall), upgrade rate |
| N5 RCL6 | terminal, extractor, 3 labs, storage 100k | mining, reactions, boost, market | ore/tick, boosted upgrade rate, market net |
| N6 RCL7 | 2 spawns, factory, 6 labs, storage 300k | spawn queueing, compression, big bodies | spawn use, factory output, CPU |
| N7 RCL8 | full set, storage 500k | surplus spend past 15/tick upgrade cap | power processed, surplus use, CPU |
| E0 outposts | N3 + E1's neighbours, 1 spawn | upgraders before outpost haul while stocked | upgrade vs N3, most stored |
| E1 outposts | RCL5 + 2 neutral neighbours | reservation, remote haul, outpost ROI | outpost net (after bodies), reservation held |
| E2 claim | RCL6 + Strategy-picked neighbour | choosing, claiming, pioneers, aid | ticks to child RCL3, mother income cost |
| E3 colonies | 3 colonies at RCL7, 5, 2 | cross-colony work, terminal balancing | total income, cross-colony share, CPU vs colonies |
| S1 invaders | N3 + timed Invader waves | `Defend`, towers, recovery | losses, ticks back to income |
| S2 keepers | RCL7 + keeper room | `KeeperDuty`, keeper mining ROI | keeper-room net, losses |
| S3 stronghold | RCL7 + neighbouring stronghold | `Siege` decision & execution | decision, gain vs cost |
| S4 low CPU | N6 at CPU limit 20 | degradation, bucket | bucket curve, income lost |

`N1`–`N7` and `E1` are built; `❓` remaining nodes (`E0`, `E2`, `E3`, `S1`–`S4`).

**Adversaries:** scripted rivals (`engine/rival/`) verify plan mechanics;
fetched open-source bots (TooAngel, Hivemind, The International — pinned
tarballs/builds fetched only with `--fetch`, never vendored, deterministic
CPU/bucket; uncached scenarios skip rather than fail and `jj ship` never hits
the network) prove real combat/outpost outcomes. `❓` Self-play with an
attacking v2.

### 10.3 Unit tests, where the engine would obscure

Under 1 minute in `npm test`:
1. **Property invariants** (`core/tests/`): Memory round-trips, Tracker id
   permutation stability, `Ours` ∩ `assignments` = ∅, no overlapping moves, no
   idle spawn while a cast is affordable.
2. **Table-driven pure arithmetic:** `Exchange`, `Atlas`, `Tracker`, bodies,
   pricing.

Never write unit tests pinning a specific creep's action in one scenario.

### 10.4 Replay

- `npm run scenario -- --capture` records per-tick diffs (`capture.jsonl`);
  recapture whenever base behaviour changes.
- `npm run replay -- --a A.js --b B.js <tag>` stands each tick from A's recorded
  world (so ticks past the first diff still compare), checking bit-identical
  intents and Memory, and supports `--at T` to debug incidents directly.
- Behaviour changes compare full outcomes with `--bot B.js --base A` and
  multi-seed `--seeds N`.

### 10.5 Metrics

`core/observe/metrics.rs` aggregates pure 100-tick windows (persisted to segment
3, never read by `decide`) across three plan axes: **output** (controller
progress), **exposure** (ticks a raid could cost a colony a level — a foe in it
or its nearest outposts while below RCL3, lacking a stocked tower, or unable to
fire safe mode), and **losses** (energy). Tracks idle share, churn, returned
loads, spend split (within 2% of engine ledger), spawn use, stale sites,
refusals, CPU/phase, deaths, hostile dwell, and held steps (`Settled.held`). Alert
thresholds (`observe health`) are `1.5 × healthy p99` from
`npm run metrics:baseline` (0 for invariants); a metric a healthy run already
fires is a fault or a wrong definition, never a looser bound. CPU budget ≤ 1% on
a live-sized world.

## 11. Live safety

- Every module sits behind a `switches` kill switch (`Fight` off leaves
  `Economy` running). `Strategy` off chooses nothing: colony plans stay at
  Exploit with no expansion, outpost choice, claim or refill booking, and a
  running abandon pauses; bot-chosen outposts and an in-flight claim drop on
  the next tick, operator-declared ones stay.
- A panic costs the whole tick, never one module (§2).
- **Deploy canary** (not built yet): keep previous bundle, compare `❓ N = 200`
  ticks of income, idle spawns, deaths, faults, and bucket, auto-revert on
  regression.
- `observe` reads live segments directly; nothing is logged twice.

## 12. Process

- **Stop-and-ask fallback:** when an architectural change requires owner
  approval (`AGENTS.md` §Boundaries) and the owner is away, label `needs-owner`
  and park it; never work around it.
- **Shape audit** (`❓` every 30 issues or monthly): report line counts vs
  ceilings, §4 vs actual Memory/segments, per-module CPU shares,
  `engagements.log` error, and `Tuning` count.
- The owner owns this document; agents propose changes via issues (suspended
  during active rewrite period in `AGENTS.md`).

## 13. Port manifest

`v1` lives on the `v1` bookmark (`9677b5e8`, rollback only).

| Milestone | Scope | Exit | Status |
|---|---|---|---|
| M1 economy | core economy slots, logistics, layout | seat-extreme soak beats v1 across seeds | ✅ |
| M1b outposts | remote harvest/haul/reserve; stand-down rule: a player's armed hostile stands down an outpost while it reaches its sources/containers, an Invader raid on sight (its only aim is that work; bodies withdraw while Defend answers, the outpost stays chosen), a stronghold core outright, and a blind room's last-seen hostile for its life (`sightings`) | E1 outposts net positive after Invaders | ✅ |
| M2 structures | nodes N1–N7; links (controller's + longest-way source's first beat hub-first by +5–17k over 20k ticks on N4); labs & boosts; factory and RCL8 set wait for a sink (one room mines one ore, reactions need two, offline has no market) | links beat no-links; labs feed boosts | ✅ |
| M3 defence | `Exchange`, `Defend`, tactics, casting, rival harness | fixed rivals & self-play held; error in bound | ✅ (self-play `❓`) |
| M4 live | v2 replaces v1 live | live since 2026-10-17 | ✅ |
| M5 reach | multi-colony `Strategy`, `Siege`/`Sweep`/`Drain`, offensive kinds | endgame in §0 | ⏳ |

**Remaining known gaps:**
- `Atlas` room graph ignores `Footprint` (2×2 Quad routing through 1-wide gaps);
  boosted `CARRY` priced unboosted.
- `Economy` cross-room travel prices intermediate rooms at one room width;
  single lab cluster and single-compound-per-creep boosts.
- Planned fight kinds (`KeeperDuty`, `PowerBank`, `Stronghold`), live
  `engagements.log`, deploy canary, and remaining `sim/` features (§10.2).
