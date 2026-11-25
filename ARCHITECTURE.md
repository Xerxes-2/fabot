# fabot v2 — architecture

The single design document for `fabot` v2. It specifies module boundaries,
cross-tick state ownership, core invariants, and verification. Code decides
anything not stated here.

Items marked `❓` are open owner decisions.

## 0. What v2 is

A Screeps seasonal-server bot in Rust, compiled to WASM and bundled into one JS
file. `v1` lives on the `v1` bookmark `9677b5e8` for reference and rollback
only.

Four principles shape v2:

1. **The unit is the world.** v2 decides once for the world; a colony is a
   resource and every room, ally, and outpost is data.
2. **Stateless re-derivation does not do fights.** Re-deciding every tick
   oscillates; patching oscillation with cooldowns, latches, and grace timers
   creates hidden state. v2 models each fight as one `Engagement` with an
   event-driven lifecycle.
3. **Pricing belongs to commitments, not ticks.** Casting the cheapest body set
   that wins (`Exchange::cheapest_win`) runs at commitment boundaries and is
   calibrated against real outcomes (§10.1).
4. **Outcomes in a real engine are the test.** Unit tests are reserved for pure
   arithmetic and structural invariants; behaviour is proven in `sim/` (§10).

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
  in `src/app/`; nothing in `core/` touches `Game`. `core/tick/drive.rs` is the
  shared tick seam called by both the WASM driver (`src/app/driver.rs`) and the
  native simulator (`sim/`).
- **Deterministic `decide`:** `decide(State, World) -> (Settled, Telemetry, State')` has no
  unseeded randomness and reads no clock except for phase timing (never read by
  decisions). Any recorded `(State, World)` replays identically (§10.4). Inside
  module boundaries, code is imperative (§9.2).
- **Central settlement:** a module returns a `ModuleOutput`: engine actions,
  spawn asks, move wishes and telemetry (counts, short casts, traces,
  decisions) that never reaches the game. `Tick` settles every module's asks and wishes once
  (one spawn queue and one movement arbitration per room) into `Settled`: the
  actions, and the bodies arbitration left standing for the metrics. Split by
  type, so the executor matches only what reaches the game and `intents`
  imports neither `Fight` nor `Economy`. Settling adds the sign reflex
  (`tick/sign.rs`): any body of ours beside a controller of a room we own,
  reserve or nobody holds signs it when its sign is not ours with the text the
  room's name and kind pick (owned, outpost even while its reservation lapses,
  passing), unless an ally or the server signed it; no engine pipeline holds a
  sign, so it cancels no other act and every module's body signs.
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
- **A distance is a walk (`Atlas::toward`), in one room as across rooms:**
  per target tile, its room's flood carried back along its tree region by
  region, each exit seeded tile by tile with what its landing walks on (v1's
  chain), so a body's distance is one lookup, never a range or a flat fee per
  room. Each walk is kept in `atlas.towards` (§4) by its weights and seeds,
  reused unread while its tree's rooms read as last tick's by address. A
  choice walks its ends nearest by tiles apart first, stopping once that
  bound cannot win (`slots::nearest`): a walk is never shorter.
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
| `Atlas` | `fabot-atlas` | Room graph (region nodes, border-stretch crossings, keeper-strike avoidance, blind keeper penalty, a room intel last saw with towers that fire on us (a live stronghold's, a non-ally player's) never crossed, only reached) and per-room tile grid (creep-free weights, reverse floods, seats, footprint queries; a hostile's tile is walled only in this tick's step floods and passability, never in a kept flood, so one moving redoes no flood, tree or walk); out-of-sight grids hold what stood when last seen (else terrain) struck where intel places the keepers (`atlas.unseen`); a walk can run on terrain alone, as it lies or paved (`bare`) | `World`, `terrain`, `intel.rooms`, `Declarations` (colony plans, for `server`) | flood table, `atlas.carried` |
| `Economy` | `fabot-economy` | Slots, casts, sticky tasks, logistics dispatcher, links, labs, terminals, abandon | `Derived`, `Atlas`, `declarations`, `EconomyState`, Strategy's `scouts` | tasks, casts, `assignments` |
| `Layout` | `fabot-layout` | Colony placement plan (Hivemind planner + xxscreeps JPS), rampart ring, construction sites, buried-road and off-plan wall destroy, tower firing | `Derived`, `Atlas`, `declarations` | sites, destroys, tower actions, `standing.layout` |
| `Exchange` | `fabot-exchange` | Pure combat arithmetic (`outcome`, `cheapest_win`, breach planning, a stronghold's run) | fighters, terrain | — |
| `Fight` | `fabot-fight` | Hostile `Tracker`, engagements (`kinds/`: defend, siege, sweep, raid, stronghold), casting | `Derived`, `Atlas`, `Exchange`, `declarations` (its operations table), `intel`, `FightState` | fight actions, asks, wishes, casts, `FightState` |
| `Tactics` | `fabot-tactics` | Per-creep combat actions, stepping, and the operator's tactic rules (`ruling`, `checked`) (no `Fight` import) | `Atlas`, `Move`, `Sight` + plan | creep actions, wishes |
| `Strategy` | `fabot-strategy` | Intel, observers/scouts, `ColonyPlan` & outpost selection, expansion/abandon, operation opening & global force priority | `World`, `Derived`, `intel`, `Season`, `standing.layout`, `economy::spare` over `EconomyState.assignments` (its colonies' own bodies, priced this tick), fight state | `declarations` (`ColonyPlan`, outposts, claims, operations), `intel`, `expansion`, `piles` |
| `Season` | `fabot-strategy` (private `season`) | Disposable seasonal rules (`worth`, `spent`, season structures/scoring) | `World` | declarations to `Strategy`, season actions |
| `Move` | `fabot-mv` | Room-level movement arbitration (augmenting-path matching) and stepping (`plan`, `blob`) | wishes, `Atlas` | moves & held bodies (`Settled`) |
| `Executor` | `fabot` | Turning settled actions into API calls | `Settled.actions` | `Game`, refusals |
| `Observe` | `fabot-observe` | Operator views (`health`, faults, CPU, counts, tactic offers) & windowed metrics (§10.5) | world, actions, held, telemetry, refusals, state | observe & metrics segments |

`fabot-base` (`core/base`) also holds what every module shares (`tiles`, `grid`,
`wish`, `intents`, `declarations`, intel rows, boost queries, `HashMap`) and
imports no module above it: a crate, so the compiler holds that floor.
`fabot-core` (`core/tick`) keeps only the tick (`state`, `memory`, `tick`,
`drive`) over the module crates.

**Dependency direction:** top to bottom, enforced at compile time by the Cargo
workspace: each crate depends only on crates earlier in this chain (`‖` joins
crates that do not depend on each other): `fabot-base`, `atlas ‖ exchange`,
`mv`, `layout ‖ tactics`, `economy ‖ fight`, `strategy ‖ observe`, `fabot-core`.
`fabot-economy` and `fabot-fight` cannot import each other; `season` is a
private module of `fabot-strategy`. Declarations flow down only.

**Line, comment, and count ceilings** live in `scripts/ceilings.json` (checked
by `npm run ceiling` and `npm run scenario`). `core/fight/price` (offers, set
shapes, casting), `core/fight/scene` (the per-tick read), `core/fight/open`
(what opens a Defend) and `core/fight/tracker` (who is out there, §6.1) hold
ceilings apart from the rest of `core/fight`, as each changes on its own (owner).

## 4. State

Every cross-tick value lives in this table with one owner. `State` composes
one slice per owner (`EconomyState`, `LayoutState`, `FightState`,
`StrategyState`) and each module's `decide` takes `&mut` only to its own, so
the compiler holds ownership. Adding a row is an architecture change, not a fix.

**Storage tiers:**
- **Memory (`RawMemory`):** parsed once per tick in `core/tick/memory.rs` under
  `Memory.v2` (the JS global `Memory` object is never touched; the `RawMemory`
  text is written back as `{"v2":…}` alone, so any other top-level key is
  gone after our first write). Holds only per-tick essential state.
- **Segments (`RawMemory.segments`):** `1` observe, `2` intel, `3` metrics,
  `4` engagements log, `10–14` recorder ring. Used for infrequent, large, or
  operator-facing data.
- **Heap:** rebuildable caches only (§1 cold start).

| Store | Shape | Owner | Lives in | Why |
|---|---|---|---|---|
| `assignments` | creep → `Assignment` (slot, task; `slot\|task` text only in `memory.rs`) | Economy | Memory | sticky slot + task across ticks |
| `fight.engagements` | list of `Engagement` (§6.2), with open tick (never moved), each body's slot, and the plan's boosted slots with the colony each is cast and boosted at (`Plan.boosts`, written when the set is priced and cast) | Fight | Memory | active combat commitments (without heap-only `offers`); a home's deaths since open are its raid's; a body is boosted only as its price chose, at the colony that cast it, which no tick can re-derive (owner; old Memory without it reads as no boosts); `actual` (first contact, the last sighting of a rival whose death was seen, the rival bodies out of sight with no word of their death, each at its last sighting and for at most a creep's lifetime, rivals at contact, energy cast, gave up, a sweep's dead in the rooms it moved on from, its log's dead with `lost`), set on those events while open, read only into `engagements.log` and never decided on (owner) |
| `fight.groups` | list of `Group` (§6.1) | Fight | Memory | hostile cluster identity across ticks |
| `fight.casts` | pending casts by engagement and slot | Fight | Memory | prevent double-casting while spawning |
| `fight.due` | colonies with imminent siege successor | Fight | Memory | feeds `ColonyPlan.owes` and `.fights` (§5) |
| `declarations` | operator overrides + per-tick `Strategy` plans (`ColonyPlan`, outposts, claims, operations, allies, enemies, sieges, avoid, tactic rules) | Strategy | Memory (operator); plans rebuilt per tick | world configuration as data |
| `tuning` | `Tuning` (`core/base/tuning/mod.rs`): judgment constants of economy, expansion and defence economics (never Fight's), a key left out its default, an unknown or out-of-range one (each key's range beside its default) a fault and every default; carried on `Declarations` | Operator writes; Strategy and Economy read | Memory (`observe tuning`, soak's `--tuning` and a seat's `tuning`) | runtime A/B of judgment constants in self-play and soak without a rebuild |
| `expansion` | in-flight claim, failed rooms (100k fade), abandoned colonies | Strategy | Memory | outlives travel, stock dips, and raze ticks |
| `piles` | colony → its hub: pile tile, or built over (`Plan::pile`) | Strategy | Memory | written from the plan whenever one stands, read only while none does (a reset redraws it over ticks), so a colony banks and picks outposts on the cold tick as warm; no `--check`: a heap's first plan apart from the hub kept before it is a live fault, a warm replan updates it; dropped with its colony |
| `switches` | module name → bool (`false` disables; `recorder: true` opts in) | Main | Memory | live kill switches |
| `observe.*` | health, faults, per-module CPU, counts, tactic offers, alerts | Observe | segment 1 | operator telemetry & benchmark calibration |
| `intel.rooms` | per-room snapshot (seen tick, owner/level, reservation, worker (another player's WORK body, never an Invader), keeper posts, controller/sources/containers, mineral, stronghold (core level, deploy and collapse ticks), a tower standing, a fallen stronghold's loot no rampart covers; by username, never stance) | Strategy | heap + segment 2 (every 100 ticks) | serve out-of-sight rooms; price outposts & expansion |
| `metrics.windows` | 100-tick aligned windows (per-room + world, up to 150 windows) | Observe | segment 3 | `observe metrics` & soak `metrics.jsonl` (§10.5) |
| `recorder` | deflated `read.js` pass + Memory + intel ring | Main | segments 10–14 | live capture for benchmark & replay (§9.1) |
| `engagements.log` | the last 200 engagements closed (an unpriced row evicted before any priced one, owner), a JSON row a line: id, kind, room, rivals, tactic, opened, contact, seen, closed, `predicted` (priced only), our dead, energy cast, and how it ended (`disengaged`, `gaveUp`, `left`, `safe`; none: its rival's death seen) | Observe (rows Fight's closes) | segment 4, written on a close (rows closed before a reset's first read wait in heap) | live `Exchange` error tracking (§10.1): an engagement's prediction goes with it, and 200 rows (~230 B each) fit one segment |
| `metrics` | open window sums & per-creep/site/room deltas (a creep's task, load and tile, and the ticks they held) | Observe | heap | incremental tick deltas; a body stuck on its task is read across ticks |
| `standing.layout` | per-room layout plan & incremental drawing state + ground key + `cold` (drawn with no standing held: a reset or a new colony) | Layout | heap | rebuilds at 4 variations/tick after reset; `cold` marks the first plan `piles` is checked against |
| `scouts` | scout targets | Strategy | heap (this tick) | derived from intel for Economy |
| `homes` | the colonies' own economy facts (`derived::Homes`) | Economy | heap (this tick) | read once by `economy::spare` before outposts are picked, finished by Economy's `of_world` |
| `sightings` | room → armed hostile / core id → expiry tick | World | heap | vision grace for stood-down outposts |
| `sightings.last` | per sighting (room, id): its creep's parts, boosts, hits, hits max and tile as last seen (`Glimpse`; none for a core); written on sight, cleared when that creep stands in another room, expires with its sighting; `--check`: in sight it is the creep now and every armed creep there has one, out of sight its creep stands nowhere | World | heap (a stored observation, like the sighting, not a derived memo) | a blind `Defend` prices and seeks the raid it last saw (§6.2) |
| `atlas.floods` | per-room cached reverse floods | Atlas | heap | reused while room weights hold; dropped `FLOOD_TICKS` (300) unasked, so a room keeps the goals asked in that span, at most `MOST_FLOODS` (2000), the least lately asked dropped first |
| `atlas.unseen` | out-of-sight room grid + the keeper posts and blind flag it was struck from | Atlas | heap | keyed by those posts, not by sight alone, so intel arriving after a cold tick strikes a keeper room pre-read blind |
| `atlas.carried` | carried room trees & outpost road walks + room-read fingerprints | Atlas | heap | a search reads no room past its hops so a carried value equals a fresh one while those read the same (saving ~1/10 of decide); drops the tick after going unasked |
| `atlas.towards` | per target tile, room and region, on the room's weights or its terrain alone, as it lies or paved (`bare`): the walk toward it, its weights, seeds and room read | Atlas | heap | a body asks at its choice of work, so a walk is dropped only `TRIP_TICKS` unasked (at most the targets asked in that span × the rooms of their trees); recomputed when its weights or seeds change |
| `atlas.reads` | last tick's room reads around each asked room and hops | Atlas | heap | an unchanged read is handed on as last tick's `Rc`, so a kept walk or tree checks its rooms by address; dropped the tick after going unread |
| `terrain` | room → `LocalRoomTerrain` (borrowed by `World`) + `getRoomStatus` | World | heap | terrain never changes and status only when novice/respawn ends (prefetched to `SIEGE_HOPS` to avoid seam calls); rooms of foreign status are never routed/served (engine traps creeps on exit) and unread rooms never observed (off-shard refuses observer) |
| `buffers` | emptied `World` lists from last tick | World | heap | capacity reuse without allocation (§9.2) |
| `fixed` | structure id → immutable type/tile/owner/hitsMax | World (`read.js`) | JS heap | avoids repeated getter cost (~1/5 of `read.structures`); checked by `replay --check` |

**Rules:** Memory and segments are written once per tick by `Main` from `State'`;
no `thread_local!`/`static` outside `app/driver.rs` and the heap rows above; no
room/player names or object ids in source.

**Not state:** multi-part danger tables, stand-down latches, raid
episodes, exit watches/holds, head latches, recall flags, quarries, economy
carries, camp prices, room latches, roster, hand-declared outposts.

## 5. Economy

**Objective:** long-run growth by simple rules first; replace a simple rule with
a priced decision only when a long soak proves it loses (a per-investment return
pricer would cost ~2× CPU and 580 lines for no gain).

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
- **Cast priority:** a room's queue runs floors (home dig and carriers),
  Defends, other fights, the economy (owner); only a Defend waits on the bank,
  holding what ranks behind it, and floors rank ahead of it since they refill
  the bank it waits on; mineral miner queues before outposts;
  while stock is past reserve (or a pile's draw beats decay), upgraders and
  pre-storage workers queue before outpost haul (the haul only adds to the stock
  they draw down); the outposts' carriers take the spawn time the colony's other
  bodies leave over a life, so a far ring of outposts never casts nothing but
  haulers. While `ColonyPlan.owes` is true (a fight cast owed or imminent siege
  successor), no surplus-spending body is spawned so spawns stay free.
  `❓` A home's first worker ranked ahead of its outposts' dig recovers a home
  whose dig builds its own containers, but delays outpost income at RCL2: no
  ranking yet passes both.
- **Spare (`economy::spare`):** home's own bodies at a steady pace, its floor a
  constant 2 workers whatever this tick's floor: home's repairs and sites move
  with the outposts' haul wear, so a floor read from them would flap the choice.
  Strategy prices it each tick before picking outposts, as it reads none of
  them.

### Logistics, banks & links

- **Dispatcher (`jobs::rank`):** Hivemind's priority → weight → distance order
  with ring over-booking demotion, plus departures proven by soak: non-ring
  spots are booked down by inbound loads;
  harvest-bay containers keep a full bay's reserve for their harvester; a hub
  pile outranks other sources while the ring has room; workers fetching their
  own take the nearest load; held loads book before any fresh pick and run to
  completion; a carrier already holding what a sink a body waits at lacks (at
  least one part's boost) brings it rather than topping up, as Hivemind's
  `delivering` fills to 90% first while that body ages (for every sink kind it
  failed six scenarios); a lab a body stands waiting at outranks the ring unless a spawn
  is open, as the body ages there and a spawning colony's ring never empties;
  while a home harvester or carrier waits on the bank, the controller
  container ranks under an open ring, as Hivemind ties it with extensions on a
  larger weight, so upgraders draining it faster than it fills starve the
  bodies that refill the bank (ungated it failed three scenarios; gated on any
  waiting cast, it cost upgrade on soak).
  The ring's room (`ring_open`) is net of the loads booked for it, where
  Hivemind reads `energyAvailable < capacity`, so a ring fully booked demotes
  no pile; the stock a pile yields to (`Ground::stock`) is storage and terminal
  less the terminal's outgoing shipment, without what transporters carry.
  In a raided room a load is taken only where a walk round the foes reaches
  it (from the body, or from the room's exits for a body elsewhere), where
  Hivemind paths on terrain alone and skips loads in its danger matrix: our
  danger leaves owned rooms out (their towers fire), and a foe parked on a
  pocket's one way in walls a load past its reach.
  `❓` After a wipe the floors cast a harvester and worker at the bank held; a
  200-energy worker's 50 a trip can lag their own recasts, so no full cast pays.
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
  Workers draw only a full load past reserve, so none spawns short of it and
  its body except the downgrade-deadline upgrader and the home-road mender,
  both permitted to draw reserve.
- **Build vs upgrade:** paying structures (spawn, extension, container, storage,
  link, tower, extractor, terminal) preempt upgrading because a structure pays
  only once complete (only RCL3's tower and RCL6's minerals are worth rushing:
  a site's build is owed over a life, the extractor's and terminal's whole
  before a new upgrader);
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
  colony holding less so it never flows back, and never past a mineral
  batch of the taker's terminal room. It sends only from a terminal
  `economy/balance.rs` leaves idle: our colonies take below 10k of each reagent
  their cluster uses and 2.5k of each target boost while they cast or owe a
  cast for any fight (`ColonyPlan.fights`), and give above want + 5k (all of
  a reagent or product they want none of, Hivemind's cutoffs; a boost only
  past 5k, so one sent to a fight stays); a reagent taker asks up to a 5k
  batch (to the giving band), so a reacting lab is not topped up per hundred,
  and a boost its shortfall; furthest short for its want first, so a boost
  stays where it was made until a fighting colony asks.
- **Repairs:** structures decaying within a creep lifetime are repaired ahead of
  building, and any lost within ten rampart decay periods (to decay, haul wear,
  or last tick's strikes) ahead of upgrading too. Soonest lost first, kind
  aside: one lost by the next tick, then by rampart decay periods, nearest
  within one. A held load is spent first: yielding it walked builders off their
  sites.
- **Outpost mender:** a colony at the tower level (where its outposts' sites
  are first owed) dispatches a dedicated 2-WORK outpost mender when an outpost
  road is at risk or its walk lacks one (Hivemind's mine builder counts a
  missing road as work, first paving or lost alike), cast ahead of the far haul
  as Hivemind's is, since the haul's recasts otherwise hold every spawn.
- **Reserve (`strategy::reserve`):** threatened colonies (rival within 3 rooms,
  home fight open, or fight cast owed) keep `towers × TOWER_ENERGY_COST ×
  CREEP_LIFETIME × n` (`n` the `reserveLifetimes` tuning, default `2`); quiet
  colonies keep 1/3 (`quietReserveDivisor`) because a full reserve costs a quiet young
  colony a lifetime of upgrading while threats come from neighbours intel sees.
  Owed fight casts and (when GCL allows expanding and `expand::may_start`, since
  one claim flies at a time) one sponsor's expansion margin (`sponsorMargin`, 30k) + one refill
  (`expand::book`) are added to reserve. Builders draw
  reserve only for paying sites and ramparts (`Ground::kept_from_builders`,
  `derived::drawn_for`), because lowering the reserve itself also feeds surplus
  upgraders and drains a raided stock.

### Layout & ramparts

- **Room planner (`layout/`):** step-for-step copy of Hivemind's planner (a
  partial copy misses its placement order) on xxscreeps' JPS/A* Dial-heap
  pathfinder (`layout/path.rs`; notices in `THIRD_PARTY_NOTICES`),
  spread at 4 variations/tick in `standing.layout`. Reads terrain/rocks alone.
  Structures build at the RCL Hivemind's room manager first unlocks them
  (towers, spawns, harvest bays from RCL3, extension bays in order; plain roads
  at RCL4 behind storage, swamp roads to sources/controller from RCL2). Key
  departures: near-exit source fallback (3 tiles vs 6); harvest-bay spawn built
  after extension bays; heavier source/controller distance weights (`0.02` vs
  Hivemind's `0.003`/`0.002`) to the first-built spawn because until RCL7 that
  spawn's haul is the economy (Hivemind's weights lose 95k on soak); multi-seat
  source container before extensions; no
  labs below RCL8; nuker last; unreachable cut pockets kept unroaded; source
  links never split a 1-wide post; a quad or decorative wall closing a
  structure in gives way, as does one closing in what only an ore's post
  leads to (its digger yields only to a chain two weights heavier) or cuts the
  core off an exit (its outposts may lie only that way), and a pocket of roads and ramparts no walk reaches
  is dropped (Hivemind checks neither, and our kept cut pockets get sealed).
  `❓` The plan is chaotic under any builder or pathfinder change: each
  variation's greedy build carries every road into its later steps (spawn,
  bays, ramparts, towers), and the best variations score close together. So a
  planner change re-baselines with a broad soak. The open fixes are choosing
  the anchor first, canonical roads, or scoring only what a road tie cannot move.
  `❓` The source container's seat (`build::harvest_spot`, Hivemind's most free
  neighbours) is not checked for being the only way between two parts of the
  room; the seat a source is dug from before it (`Atlas::bare_post`) is.
- **Ramparts (`strategy::wall`, `ColonyPlan.rampart_hits`):** kept at `ticks
  held × dismantler damage/tick`, where dismantler damage is the largest rival
  dismantler up to our RCL (capped at our level because a taller wall outbids
  the economy holding it and safe mode answers; boosted if rival has labs), and
  `ticks held` is as long as our towers take to kill it through its quad's heals
  from the ring's far end, capped at the time a colony casts its answer twice.
  Quiet colonies keep `rampartFloor` (10k), or 1/3 of full at RCL8 (`quietWallDivisor`). Core structures (spawn, tower,
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
  `Mine` declarations without owning them, and `Pickup` rows: a room's
  loot in sight (remains' and unwalled containers' non-energy) joins the
  far haul of the nearest colony whose bank has room for a load (none, no
  pickup), every load of it ahead of other loot, with one far carrier kept
  while any is left.
- **Labs (`economy/labs.rs`):** the clusters (`boost::clusters`, each with
  its `Derived.boosts` read by Economy, Fight, and `Tick`) are the colonies not given up with a lab, most
  labs first, ties by name: each with three labs reacts, each boosts its own
  bodies. Targets are derived, not declared: every
  T3 combat boost whose ores the empire digs or holds, in the `BOOSTS` rank
  expansion scores minerals by, each to Hivemind's "high" stock, so the labs
  make what fights spend from what we dig with no operator in the loop.
  `labs::wanted` lists the reactions toward them from the empire's stock,
  depth-first down the crate's `REACTIONS`, each target cut to whole
  reactions the stock carries through to it after the targets before it
  (`labs::makeable`), none drawing an earlier one's stock below its cap.
  `labs::assign` gives each, no two clusters the same, to a cluster its
  reagents reach (its own, and what other colonies would
  give through terminals with room), ranked: the reaction its reagent labs
  already run (both reagents held), so a running reaction stays put, unless
  it reacts under half as fast as the fastest cluster, so a slow one never
  holds a step a fast one is free for; then most output labs; then labs
  holding one reagent; then the nearest colony holding the reagents; then
  the first wanted. Nothing in the rank moves as a send lands, so each tick
  ranks afresh with no memory. A running cluster keeps its reagents and
  what its product reacts with next; an idle one keeps none. Terminals
  balance reagents and boosts (§5 terminal aid), and a product another
  colony is short of leaves the labs a send at a time. Labs
  reserved for active boosts (§6.5) drop out of reactions and are filled by
  carriers via `Ground::boost_lab`. Layout places no lab below RCL8 (labs are
  worth little early).
- **Abandon (`economy/abandon.rs`):** declared colonies stop digging, building,
  and upgrading past deadline, ship storage/terminal/lab contents out (energy
  last), destroy own structures (`destroy()` requires our controller, and owned
  structures left after unclaim keep the room visible in every world read), then
  unclaim.
- **Not in Economy:** fight tasks (danger walling lives in `Move` via
  `Derived.danger`, omitted in own rooms so tower refills never flee),
  hardcoded colonies/outposts, one-incident patches.

## 6. Fight

### 6.1 Tracker — who is out there

`tracker::step(grace, held, owned, prev_groups, hostiles, tick) -> (groups, events)`

- **Clustering:** armed hostiles cluster by connected components at Chebyshev
  distance ≤ 4 (including across room borders). Unarmed hostiles are singletons.
  *Armed* = a live `ATTACK` or `RANGED_ATTACK` (one shot off counts while a
  `HEAL` of its own lives), `HEAL`/`WORK` beside one in the room, or `WORK`
  alone in an owned room: a cripple shot disarmed is v1's trespasser, struck
  where it stands in our rooms but standing nothing down and casting nothing.
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
  rooms are the ground it pursues on, read off the raid's centre, the edge
  tile facing one included, so a bounce over the border is no leaving). Phases: `Forming →
  Approaching → Fighting → Pursuing → Disengaging → closed`. Its loss budget
  is priced as it commits: its first answer to a raid, a newcomer group being
  a new raid. A re-price on a death or tracker event keeps it, or each death
  would lift it past every loss; one while it forms asks no more bodies, so
  a raid growing as it forms is fought by the set first answered. An outpost fights on past its budget by
  `OUTPOST_LOSSES` (2) bodies, then gives up: it disengages, holding no
  body (another answer drafts them; one an open sweep would take in rests
  for it; else they walk home and are recycled, as none would fight for
  it), and closes only once `sightings` clears
  (closing sooner reopens on the same raid with a fresh budget and feeds it
  bodies). Disengaged within that
  bound, it re-forms and re-prices on the budget it kept the tick its raid's
  centre steps from off our ground back onto it (the crossing is the event;
  the centre is all a group keeps from one tick to the next). `❓` A raid
  hovering a tile or two across the border flaps it, each re-form asking its
  shortfall (self-play seed 7: 11 and 9 re-forms in 30k ticks, one Defend
  re-formed, quit and re-formed within 46 ticks); stopping that needs a
  deadline or a wider band (owner: ships as is). Blind, with no body of
  ours standing or asked, it re-forms and re-prices the raid
  `sightings.last` remembers after each loss or age-out while it has lost
  fewer than two, until the sighting expires, and casts and seeks it as a
  seen one; with none remembered, or a re-price casting nothing,
  it disengages and keeps its budget the same way (`❓` a raider that steps
  from a room in sight into one out of sight leaves no sighting, so its room
  going dark next tick closes its Defend blind: keeping it needs the
  glimpse's tick, a §4 change). A home is the owner's exception: it quits at lost >
  budget (no margin) and, given up, answers its raid anew with a fresh budget. `❓` A fight against a core alone
  never sets `predicted`, so each re-answer after a starved one re-prices its
  budget. A body with nothing alive but MOVE,
  TOUGH and CARRY counts as lost to an outpost (nothing there heals it; it can
  end no raid) and no engagement drafts it until healed. A fight body no
  engagement holds rests in any outpost or claim it stands in (one walked
  through on its way home included: where it was freed is no state kept),
  on a tile blocking no walk or work, else at its muster: a raid recast next
  door meets it there (v1's guard). `❓` Absorbing a raid (stand down) vs
  answering it is no explicit Exchange price yet (owner). Home `Defend` never
  disengages (falls back to `unanswered` on towers/ramparts until the group
  is gone). Past its first win's priced ticks from opening, a raid out of
  reach of the ramparts over our structures and struck by nothing of ours
  (razing walls out of reach included) does not come to the post: re-read at
  each re-price, it withholds the post, and a `Hold` re-prices off it the
  first tick it holds. A raid in a home strikes as it stands, so a home's
  set none of whose casts has begun, once its spawn has begun another body
  since they were asked, or stands idle short of a cast's energy, and the
  landing their price gave (their spawn ticks) has passed, cannot arrive in
  time: that deadline sets it `unanswered` and drops its asks, and a later
  event prices it afresh (owner; it formed on casts the floor's refills
  never let begin).
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
- **`Sweep`** (`kinds/sweep.rs`): one a room on enemy-owned/reserved/
  worked rooms (the table's `Sweep` rows, §7.3; an owned room intel last saw
  with a tower is none, a siege's by `declarations.sieges` alone, owner
  decision), harassing his economy, never his guards (owner, 2026-11-24): it
  is worth prey plus exposed-source harvest over a harasser's life per walk
  tick, the output it denies; a body of ours fallen there short of its life
  prices it held no longer than that body lived past its walk, but only
  while its tombstone stands in sight (5 ticks a part). What stops a
  re-cast is losing sight when our last body there dies, then the escort
  check once the killer is seen again; a killer gone before we look leaves
  the room at full worth (no state is kept). A body holds a room while it out-reaches every armed
  body there, a keeper's or a third player's too, at that body's pace on any
  ground and no charged tower stands; the set is the cheapest of
  `HARASSERS` (1A1M, a 1R2M kiter, bare: §6.5's HEAL waived) that holds it
  and that worth pays, so a guarded room is worth nothing and no sweep
  escalates. Rooms open in `atlas::onward`'s order while our spawns cast
  every sweep's harassers within a full body's spawn time a spawn. Its bodies
  strike only economy bodies, and with none containers, scoring each tile
  against what the guards strike. A room it no longer holds, or his safe
  mode covers (never his others), sends the set onward at once. Living sets
  spend their life in the enemy's field, rotating
  onward via `atlas::onward` (profitable rooms first, else stalest patrol, the
  rooms beside his worked ones among them, an unseen one as stale as the
  stalest seen, none another sweep holds or took this tick, a player not an
  enemy holds or reached only past a rival's towers; rooms
  with a tracker group still in `Lost` grace are not stale so a set never walks
  straight back into an escort it just left) rather than returning home; closes
  unanswered, on its first loss in a room (a body disarmed is one, as
  to an outpost Defend, and walks home to be recycled) or with nowhere to go
  (losses start afresh on each room; the log keeps their total). Past the
  deadline a home's `Defend` keeps, a forming set drops its unbegun casts and
  goes on only if its standing bodies hold its room, else closes (its bodies
  stood at home till they aged out).

- **`Stronghold`** (`kinds/stronghold.rs`, L1–L3): opened on the table's
  `Stronghold` row and priced at open off the level intel read, unseen:
  its towers stand beside the core and its garrison never leaves its
  ramparts, so `exchange::stronghold` runs its towers on our set tick by
  tick (PvE is exact), the struck body healing itself alone (the towers take
  the nearest as a set walks in apart), and every count of each candidate
  is priced, as a boosted copy waits on its own lab fills. The candidates are S11's
  leaders' single ranger, derived from the strike (TOUGH whose boost holds
  it, HEAL for what passes, MOVE for plain pace, the rest RANGED), and a
  bare ranger blob. Its value is the loot by level (`coreAmounts` +
  `containerAmounts`) at twice its density as energy, as a commodity draws
  about twice its density in raw resources; each cast costs the share of
  its life its walk and kill take, as a body that wins lives on (owner:
  leaders take L1 with one ranger). Its bodies shoot the core's rampart,
  then the core, from the post out of the garrison's reach the towers
  strike least, healed ahead of every strike; once the core falls its
  towers and garrison strike nothing (`Derived.danger` drops them: a
  garrison outlives a creep's life), and the set shoots any of its garrison
  off a rampart; that garrison opens no Defend either. The ramparts over
  its loot are Economy's: a dismantler (WORK/MOVE, sized to the bank) of
  the colony hauling it breaks `fabot_atlas::loot_wall`'s choice, a wall a
  body reaches round the garrison, as WORK dismantles five times what
  RANGED strikes and a fight set ages out before its loot is free.
  `❓` L4 (fortifier, ranged garrison reaching every post) and L5.

**Planned kinds:** `Claim` (claimer + Economy pioneers), `KeeperDuty` (lair
rotation declaring room minable), `PowerBank` (break → declare `Pickup`).

### 6.3 Composition and the exchange

`Exchange` (`core/exchange/`) is pure combat arithmetic over narrow inputs:
`Side { bodies, ramparts, towers, cast_walk, together }`, `Terrain { weight, run_off }`,
`Stance { Hold, Standoff }` → `Outcome { Win(ticks, losses), Kite(ticks), Lose }`
and `cheapest_win` (adds only to already-standing `ours`).

- **Stance:** under `Hold`, raids come to our post; raids out-ranging our
  strikers and outlasting towers price as `Standoff`. A home with ramparts
  prices `Standoff` beside `Hold`, and a `Hold` that loses is worth nothing:
  a post is kept only where it is the cheaper consequence.
- `❓` (after zone-control defence) Exchange does not price a ranged rival
  shooting off our kiter's MOVE so a full-speed melee catches it.
- `❓` Exchange pools every mate's heal onto the focus whatever the ground, but
  a body pinned by terrain gets only its adjacent mates' heal; a geometry-aware
  heal model or a tactic that avoids pinning fixes it.
- `❓` Our set's hit-back on the raid scales with the swing all its living
  bodies keep, whatever their range, and a body worn down off the focus ends
  no pricing window.
- `❓` (after zone-control defence) Exchange's Kite arm prices a self-healing
  healer, or a raid our rangers could kite off a post, as Lose where kiting
  wins.
- **Arrival:** a body counts from the tick it lands (a cast's spawn time and
  walk included), so a set fed in one at a time prices worse than one walked
  in `together`, which pays the wait in ticks instead (a body already in the
  raid's reach is struck alone through it). `Defend` prices both arms
  (`Close`/`Kite`/`Hold` vs `Blob*`); where only the gathered set wins (its
  bodies standing do not, and every arm walks in together), its casts still
  to land, it forms again and gathers out of its raid's reach until they
  land, then goes in as one (owner, 2026-11-25: a set fed in one at a time
  loses what the gathered set wins). `❓` A raid that grows
  after contact cannot be gathered against. A `Defend`'s `Blob*` set walks as
  one while it fights too, as its price pools its heal: its bodies in the
  raid's room into the reach its tactic names (`BlobClose` melee,
  `BlobKite` a shot) of the kill order's first standing, or, with no body
  that swings, of whichever of the raid its lead reaches round the swings;
  the rest close on the lead short of a swing's reach, each kiting while a
  swing strikes it; the lead waits for neither one struck nor one stopped
  short; a body still walking in closes alone, and pursuit is each body's
  (§6.4). `❓` Rangers in a mixed `BlobClose`
  still walk into melee reach; to be solved with the outpost-defence duo
  redesign.
  `❓` A `Close` set priced landing together reaches the raid apart where
  the raid walks onto its nearer body (a pair 1 tick apart at landing met it
  6 apart); option: price a non-together arm as if the raid closes on the
  nearest body.
  `❓` A raid spawned beside a forming blob scatters it
  (blob-defend-fights-within-its-heal seed 26: strayTicks 264).
- **Focus:** the Exchange prices the kill our bodies strike: both rank the
  raid by `fighter::kill_rank`.
- **Parts:** our bodies strike, heal and hit back only with the parts their
  remaining hits keep alive, head first; the raid is priced whole until it
  dies, since pricing its parts too let rivals that rotate hurt bodies out
  beat a predicted Win (owner, 2026-11-24; bugs 36 → 46).
- **Losses:** a set's cost charges every body of ours the outcome loses,
  standing or cast, once at its body cost (a survivor at the share of hits it
  lost, never its cast: it fights the next raid), so a death never prices
  below a few ticks saved. `cheapest_win` also charges each cast it buys, by
  design, not a double charge: without the cast a survivor is free and the
  set grows to its cap; without the losses it buys bodies too cheap to survive.
- **Towers alone:** with no body of ours fielded, towers that out-damage the
  raid's heal win, within the energy each holds, and if the raid razes no
  keep first.
- `❓` Exchange prices our towers at our body's tile, though a set that
  closes fights where the raid stands, often far weaker; counts a swinger's
  strike-back whether or not the raid swings; and regrows a healed raid's
  hits but not its TOUGH (fixed with `engine::strike_at` and the shared
  `breaks()` target pick).
- **Consequence pricing:** every threat, structure, and seat is valued in
  **energy/tick** (razed, killed, healed, or denied) — never by body parts or
  creep kind. Feature-keyed rules leave exploitable gaps; wrong choices are
  fixed by adding the missing term to the price.
- **Our safe mode:** a hostile that ages out before it ends strikes nothing
  in its life (the engine refuses its attacks and heals), so it is not priced:
  it costs no cast; the ground it holds (a blocked source) is not priced
  either, a gap the owner accepted.
- **Tactic selection:** each kind declares candidate tactics; pricing scores
  each candidate as `Offer { tactic, value, cost, bodies }`, and
  `tactics::select` applies operator `forbid`/`pin` rules then picks max
  `value − cost` (ties: fewest bodies, declared order). Offers stay heap-only
  for `observe engagements`.

### 6.4 Tactics — what a committed body does

`core/tactics` (no `Fight` import) maps `(Sight, plan, body)` to actions and wishes:

| Tactic | Behaviour |
|---|---|
| `Kite` | A body that does not swing walks to a shot's range of its mark round each tile a swing strikes before the body could stand on it and be gone, its rest, legs and the tile's ground counted, from inside one first onto a clear tile beside, else the one struck latest if later than its own (v1's kiteStep, kiteUnsafeFor) |
| `Close` | Close distance + glue onto moving marks; a body that does not swing closes as `Kite` walks, and on a mark that swings stops at a shot's range; a `Defend` body (`Kite` too, not the blobs) outside its centre's room enters it by any tile and ranks no mark until in. A siege's loose bodies keep its blob's line in |
| `Hold` | Hold rampart / `Atlas` seat: one body a rampart, as Exchange's Hold stance seats them |
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

- **Queue (`Tick::queue`):** per-room priority `Floor → home Defend → outpost
  Defend → Fight → Economy` (owner, 2026-11-24); an affordable Defend ask
  waiting on energy holds the asks behind it. An unanswered home Defend is
  priced each tick, as its raid changes under the towers.
  `❓` A cast waits behind a body its spawn already began, which the engine
  cannot cancel: a home raided on the tick an outpost's cast begins is drafted
  after it (defend-prioritises-home-over-outpost seed 1: 65 ticks), and a raid
  that appears unseen waits behind economy bodies already spawning
  (boost-labs-fed-while-the-ring-casts: two haulers, 89 ticks).
- **Colony choice (`fight/price/casting.rs`):** picks the colony landing the body
  soonest (hops + spawn queue + energy refill time); home `Defend` casts at
  home. Bodies end with one `MOVE` (`with_move_last`) and carry `HEAL` when
  walking into rival rooms (except siege dismantlers covered by medics and §6.2
  sweep harassers). On our own ground a `Defend` prices the bare AM and 1R2M
  only while its bank casts no ranged `HEAL` body (a RANGER, 700): a HEAL it
  cannot cast answers no raid, but beside a RANGER bare bodies win a few ticks
  sooner while no outpost heals them, so repeated raids wear them out, a wear
  one exchange does not price (owner, 2026-11-24).
- **Boosts (`core/base/boost/mod.rs`):** `boost::boosted_by` gives each part type
  of a body, by its compound's `BOOSTS` rank, the strongest compound a
  colony's `Stock` covers for all its parts of that type, spending stock and
  energy as it goes and counting the lab fills it waits on (`lab_wait`, 85
  ticks a fill; a compound already in a lab costs none). Boosting is priced
  (owner): `cheapest_win` takes the boosted cast, landing later by its lab
  wait, only where it wins cheaper than the bare one, and the boosted slots
  and their colony go into `Plan.boosts`. A body priced boosted and cast
  elsewhere walks straight out. Each tick `boost::boosting` (`Derived.boosts`,
  per cluster) shares a colony's stock, oldest first, among the bodies a
  forming set's plan boosts there that no fight has touched, and names each
  one's next lab; what they leave, less the casts still to spawn, prices new
  casts. A Defend priced boosted stays Forming while its bodies stand at their
  labs, so no body is held once its set fights. Owner-approved bound: a body
  waits only while its age is within the `lab_wait` of its compounds; past
  that it leaves with what it has. `Tick` holds a body at its lab while the
  lab has room and a carrier exists.
  `❓` A body leaving at that bound unboosted fights in a set priced boosted.

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
  skipped except declared sieges. A towered room (Atlas never crosses it) stays
  in the radius one past its nearest neighbour, and a scout reaches it only as
  its goal once its row is stale, so a tower since gone clears.
- `❓` A towered room is never crossed (Atlas), but an outpost or claim in
  one, or reachable only through one, is not refused by name.

### 7.2 Expansion and declarations

- **Stance (`world::Owner`):** resolved once at read time from `allies`,
  `enemies`, and declared sieges' owners. A stranger is fought where it threatens
  us or works an owned or worked room (an outpost is ours to defend), never
  followed out or hunted.
- **Colony plans & outposts (`strategy::plan`, `strategy::outposts`):** rebuilt
  each tick under operator overrides (**operator rows always win**). Outposts
  run from RCL2 through RCL8 (an RCL8 controller takes 15 of a two-source home's
  20/tick, leaving nothing to bank for boosts, labs, or ramparts without
  outposts): operator rows first, then highest net energy/spawn-tick within
  `cpu_limit / 2` and spawn `spare` of the colony that walks to it
  (`atlas::server`, Economy's choice too; its haul charged on the terrain path,
  unroaded while young and paved once stocked, as Economy casts the carriers
  that path needs only while spawn time is left and the rest of the dig
  decays), a room without containers at all
  sources net of them over one creep life (its haul is priced unroaded, so a
  road repays itself), so a better room displaces a standing one only once it
  repays its setup within a life. A busy home's held-back room keeps its spawn
  time, so busy never swaps which others are worked. Each outpost is priced with
  the spenders its dig casts (2 parts per energy/tick) because a fixed half
  leaves spawns idle at RCL3–6 and full life past RCL6 starves hauling; a young
  home reserves half its spawns' life for workers so its pile does not decay
  unspent. `❓` `outpost_spend_parts` stands in for a margin: a steady-state
  charge with none lets a backlog (a full container doubling its source's
  rate) outgrow the spawn-time cap and never drain, so deriving it needs a
  derived margin (e.g. the backlog rate charged while a container is full) or a
  capped backlog ask. Stocked homes add one non-standing outpost at a time so
  spawns absorb
  one room's bodies at once; young homes open all paying ones because waiting on
  containers delays RCL3. An operation holds only the colonies it casts from
  (`outposts::busy`): a claim's sponsor until its spawn stands; a siege from its
  operation row to its engagement's close, answered or not, and a stronghold
  while answered, every colony Fight's `colony_for` picks at rest for some
  body (`fight::casting::homes`: none nearer out-banks it), since between
  waves no cast is owed; a sweep none (a harasser spawns in a few ticks a
  life); a Defend its owed casts (`ColonyPlan.owes`).
  Held, they add no new outposts and sponsor nothing, while the others keep
  growing.
- **Expansion (`strategy::expand`):** automatic, one in flight. On due ticks
  (100) while GCL allows and `bucket >= expandBucket` (9000), a non-busy RCL4+
  sponsor with storage holding `reserve + sponsorMargin` scores free 2-source rooms
  ≥ 2 rooms from owned rooms and past `expandThreatRooms` (2) from rival rooms by dig + free neighbours + mineral boost demand
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
  first (0), whether or not a sweep is open, `Stronghold` and `Pickup` (0)
  on the deployed L1–L3 within reach of a colony with two lives to its
  collapse that the colony's stock and bank take for less than its loot
  (`kinds::stronghold::take`, priced on the inputs Fight opens with),
  highest level then nearest, and on each fallen stronghold in sight still
  holding loot. A cast set's row stands until its set or the room's core and
  loot are gone, ahead of any rank or collapse, because a commitment is
  priced once, not every tick (owner); a `Stronghold` engagement closes once
  its room is no row. `Fight` opens only on a row's room and ranks by its
  priority, but keeps its event gate (`stirred`) and prices winnability at
  open (`Exchange`, `drainable`): which rooms and how they rank is
  Strategy's call, whether a set wins there is Fight's (§6.2), save a
  stronghold's, which Strategy prices with Fight's own `take`.

## 8. Season (disposable)

`core/strategy/season.rs`, a private module of `fabot-strategy`, isolates all
seasonal rules: scoring, season structures and resources, `season::worth`
(added to expansion score), and `season::spent` (triggers abandon once mined
out). Vanilla returns `0.0` / `false`. Only `Strategy` imports `Season`; at
season end the file is rewritten without touching the rest of the bot.

A killed core's ruin lives to its stronghold's collapse in the vanilla engine
(and krebs); Season 11's radiation took floor(log10 T on the tile) more off
each `decayTime` every tick, so a ruin holding 100k+ T lasted a sixth as long.

## 9. Performance

### 9.1 The benchmark comes first

No performance change ships without the representative benchmark:

1. **Real input:** live capture from the recorder ring (`observe capture`) or,
   offline, a sim capture (`sim --capture a-b`, `npm run scenario -- <name>
   --capture`), the World `decide` read as `Game` shows it. Never synthetic
   scenarios.
2. **Engine cost model:** `bench.mjs` replays under live isolate heap limits
   with Memory JSON parse/stringify, 0.2 CPU/intent, and cold ticks reported
   separately.
3. **Live calibration:** `Main` logs per-module `Game.cpu.getUsed()` deltas to
   `observe.cpu.modules`. The benchmark is representative once each module's
   share is within `❓ 15%` of live (`❓` calibration waits for live capture).

Noise floor: `npm run bench` reads within `±0.4%` per module on an unchanged
bundle. Speed claims require an interleaved A/B with identical Memory
fingerprints, or a deterministic count — never bare wall-clock.

### 9.2 Rust dialect — defaults, not rules

Pure at module boundaries, systems Rust inside (measurements override defaults):

- **In-place `State`:** modules mutate `State` in place (`ModuleRun`),
  avoiding ~700 allocations/tick from per-module clones; tight
  per-creep/per-tile loops avoid per-tick allocations.
- **Single-pass JS→WASM seam:** a per-field world read costs ~90% seam calls
  rather than work, so `src/app/read.js` packs the tick into one `Int32Array` +
  interned string buffer (interning saves 17% of `read.*` and `decode`), decoded by
  `core/base/world/read`; never add per-field JS calls.
- **Copy `world::Id`:** 96-bit left-aligned hex + length (`Copy`, lex-ordered as
  text), avoiding `String` allocations and pointer hashing.
- **`#[inline]` across crates:** rustc inlines a function into another
  crate only when it is `#[inline]` or tiny, and neither the incremental test
  profile nor release LTO makes up for it, so a hot helper another crate calls
  takes `#[inline]` when a profile shows a crate boundary took it out of line
  (missing ones cost economy, atlas and settle 3–8% warm).

### 9.3 Counts are a gate

Deterministic per-tick counts (floods on a warm tick and per colony on a cold
one, Atlas miss rate, Hunt threat lookups) are
reported to segment 1 and enforced against ceilings in `scripts/ceilings.json`
on every `npm run scenario` run.

## 10. Verification

Inverted pyramid: real-engine outcomes are primary; unit tests cover pure
arithmetic and invariants.

### 10.1 Exchange is checked against what happened

Every engagement records `Predicted` at open and compares against actual
outcome at close. Offline, `npm run exchange` and fight scenarios evaluate
predictions via `scripts/sim/prediction.mjs`; `predictedWinsLost` fails a
scenario as an `Exchange` bug. Live, each engagement's row lands in
`engagements.log` (§4) as it closes, and `npm run observe -- exchange` prints
the same table, then each rival's outcomes by tactic; `liveAstray` holds those
rows to the engine's classification in scenarios. A rival that leaves alive is
**left**, neither won nor lost (owner): live, a close where some rival body
was not seen to die (the engine's word: its tombstone, or a siege target's
raze), offline one it outlived.

### 10.2 The engine

- **`sim/` (native Rust on `krebs`, official engine rules):** zero WASM/JS, PGO
  build (`npm run build:sim`), every soak and scenario (one process per core);
  an A/B is two sim binaries (`--bot`, `--base`). Models official backend cron timing (`genInvaders`
  every ~88 ticks, `INSIDE_PASS = 0.66` MLE-fit to Season 11 history), scripted
  waves, level-0 cores and strongholds (`spawnStronghold`, bunker1–5), and
  `--check` cache verification. A season world
  (`season-world.mjs`) carries each sector's centre 3×3 (centre and keeper
  rooms) because `genInvaders` raids only a sector with a live core and
  `genStrongholds` stands one only from a centre room in the world. Where
  `sim` and the game disagree, `@screeps/engine` / `@screeps/backend` settles it.
  `npm run exchange` plays its duels on `sim` too (`sim duel`,
  `sim/src/duel/`), so the prediction error is read over thousands of duels.
  With `--real 1` our side runs the bot's tactics, the one its Defend fields;
  the rival stays a stand-in (the emitter, or with `--rollout R --theirs 1` a
  stress test: his worst reaction his bodies allow), as no rival's strategy can
  be simulated and each differs. The scenarios' scripted rivals play on `sim`
  as a second user (`sim/src/rival/`, one `Player` each), their intents merged
  per room with ours.
  - A world a soak left is checked in as a world JSON (its Memory, and its
    intel rows as segment 2, which `sim` stands), never replayed from a
    pinned bundle.
  - `❓` Cut worlds lacking sector bus rooms throw in `genDeposits` (logged and
    skipped as backend cron does).
- **Watching:** every scenario and soak is watched on `sim` (`npm run
  scenario -- <name> --watch`, `npm run watch:sim`, `sim/src/watch`): the run
  lazycreep shows is the one scenarios and soaks measure.

**Scenario library (`npm run scenario`):** every live incident becomes a recipe
scenario with outcome bounds (`scripts/scenario/scenarios/`). `jj ship` runs
scenarios matching touched modules. A bound guarding a rule holds over every
occurrence in the run (`"each"`: each engagement, safe mode, outpost), not a value
at a tick or in a window, so a trajectory that drifts moves no bound. A decision
no outcome shows is asserted as its module reports it (`Telemetry.decisions`,
`"each": "decision"`): tick-local, written beside the tick's counts in `cpu.jsonl`
and dropped live, so it needs no state, Memory or segment.

**Soak & reading rules (`npm run soak`, `npm run soak:many`):**
- Judged on the 30k-tick horizon by end RCL & progress, the tick each level is
  reached, and income in the last 1,000 ticks (the first 10k ticks alone decide
  nothing).
- `soak:many` runs across seat extremes (1-seat vs ≥4-seat sources, read apart
  and never as a median because a crowd of small bodies only fits many seats),
  `season-outpost-ring` (1-source remote economy), `season-late-seat` (10k ticks
  of N7; RCL8 controller progress buys only GCL so it is read apart and never
  counts as a regression).
- `soak:many -- --reset-every N` runs each sim start beside a twin whose heap is
  lost every N ticks (Memory, segments and world kept, as `src/app/driver.rs`
  meets a global reset); any gap beyond seed noise is a reset bug.
- `soak:labs` (sim, 30k ticks, seeds 1,2) reads the boost chain on
  `young-among-colonies`, its RCL8 homes digging O, U and K, X in one terminal.
- `soak:gen -- --seeds A..B` (sim `--gen`) soaks one sector map.js lays out
  per seed, our room the seed's pick, and holds each to `measures.mjs`'s
  invariants: breadth over maps, a broken seed shrunk in time only, to its first
  broken tick; `watch:sim -- --gen <seed>` shows one.
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
| N5 RCL6 | terminal, extractor, 3 labs, storage 100k | mining, reactions, boost | ore/tick, boosted upgrade rate |
| N6 RCL7 | 2 spawns, factory, 6 labs, storage 300k | spawn queueing, compression, big bodies | spawn use, factory output, CPU |
| N7 RCL8 | full set, storage 500k | surplus spend past 15/tick upgrade cap | power processed, surplus use, CPU |
| E0 outposts | N3 + E1's neighbours, 1 spawn | upgraders before outpost haul while stocked | upgrade vs N3, most stored |
| E1 outposts | RCL5 + 2 neutral neighbours | reservation, remote haul, outpost ROI | outpost net (after bodies), reservation held |
| E2 claim | RCL6 + Strategy-picked neighbour | choosing, claiming, pioneers, aid | ticks to child RCL3, mother income cost |
| E3 colonies | 3 colonies at RCL7, 5, 2 | cross-colony work, terminal balancing | total income, cross-colony share, CPU vs colonies |
| S1 invaders | N3 + timed Invader waves | `Defend`, towers, recovery | losses, ticks back to income |
| S2 keepers | RCL7 + keeper room | `KeeperDuty`, keeper mining ROI | keeper-room net, losses |
| S3 stronghold | RCL7 + neighbouring stronghold | `Stronghold` decision & execution | decision, gain vs cost |
| S4 low CPU | N6 at CPU limit 20 | degradation, bucket | bucket curve, income lost |

`N1`–`N7`, `E0`–`E2` and `S3` (`M7` beside `stronghold-lN-taken`'s stronghold)
are built; `❓` remaining nodes (`E3`, `S1`, `S2`, `S4`).

**Adversaries:** scripted rivals (`sim/src/rival/`) verify plan mechanics. No
test seats an open-source bot (owner): a bot tuned against one says little
about the players a season meets. Self-play: `sim --players` (`sim/src/seat/`)
seats our bot several times in one world, each seat its own `Driver`, Memory
and declarations; the samples follow the first. `❓` Each seat sampled, and a
scenario that scores one seat against another.

### 10.3 Unit tests, where the engine would obscure

Under 1 minute in `npm test`:
1. **Property invariants** (each crate's `tests/`): Memory round-trips, Tracker id
   permutation stability, `Ours` ∩ `assignments` = ∅, no overlapping moves, no
   idle spawn while a cast is affordable.
2. **Table-driven pure arithmetic:** `Exchange`, `Atlas`, `Tracker`, bodies,
   pricing.

Never write unit tests pinning a specific creep's action in one scenario.

### 10.4 Replay

- `npm run scenario -- --capture` records every tick each soak decides
  (`capture.jsonl`); recapture whenever base behaviour changes.
- `npm run replay -- --a A.js --b B.js <tag>` stands each tick from A's recorded
  world (so ticks past the first diff still compare), checking bit-identical
  intents and Memory, and supports `--at T` to debug incidents directly.
- Behaviour changes compare full outcomes with `npm run scenario -- --bot
  <sim> --base <name>` and `--seeds N`.

### 10.5 Metrics

`core/observe/metrics.rs` aggregates pure 100-tick windows (persisted to segment
3, never read by `decide`) across three plan axes: **output** (controller
progress), **exposure** (ticks a raid could cost a colony a level — a foe
outliving the colony's running safe mode, in it or its nearest outposts, while
below RCL3, lacking a stocked tower, or unable to fire safe mode), and
**losses** (energy). Tracks idle share, churn, returned loads, spend split
(within 2% of engine ledger), spawn use, ticks a colony left a slot uncast
by why (`blocked.bank|spawns|fight`), stalls (`stall.ring|task`), stale paying or rampart sites per colony
(one that becomes its structure restarts the colony's waits),
refusals, CPU/phase, deaths, hostile dwell, and held steps (`Settled.held`).
Alert thresholds (`observe health`) are `1.5 × healthy p99` from
`npm run metrics:baseline` (0 for invariants); a metric a healthy run already
fires is a fault or a wrong definition, never a looser bound. CPU budget ≤ 1%
on a live-sized world.

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

| Milestone | Scope | Exit | Status |
|---|---|---|---|
| M1 economy | core economy slots, logistics, layout | seat-extreme soak beats v1 across seeds | ✅ |
| M1b outposts | remote harvest/haul/reserve; stand-down rule: a player's armed hostile stands down an outpost while it reaches its sources/containers and no armed body of ours holds it (strikes as far and stands within its own reach of it: v1's guard), an armed Invader on sight (its only aim is that work; one shot down to no weapon holds nothing, as Defend leaves it; bodies withdraw while Defend answers, the outpost stays chosen), a stronghold core outright, and a blind room's last-seen hostile for its life (`sightings`); a harvester (at its seat or walking to it) walks round only the strikes no guard holds, every other body round all | E1 outposts net positive after Invaders | ✅ |
| M2 structures | nodes N1–N7; links (controller's + longest-way source's first beat hub-first by +5–17k over 20k ticks on N4); labs & boosts; factory and RCL8 set wait for a sink (one room mines one ore, reactions need two, offline has no market) | links beat no-links; labs feed boosts | ✅ |
| M3 defence | `Exchange`, `Defend`, tactics, casting, rival harness | fixed rivals & self-play held; error in bound | ✅ (self-play `❓`) |
| M4 live | v2 replaces v1 live | v2 runs live | ✅ |
| M5 reach | multi-colony `Strategy`, `Siege`/`Sweep`/`Drain`, offensive kinds | endgame in §0 | ⏳ |

**Remaining known gaps:**
- `Atlas` room graph ignores `Footprint` (2×2 Quad routing through 1-wide gaps);
  boosted `CARRY` priced unboosted.
- `Economy` cross-room travel prices intermediate rooms at one room width.
- Planned fight kinds (`KeeperDuty`, `PowerBank`, `Stronghold` L4–L5), deploy canary,
  and remaining `sim/` features (§10.2).
