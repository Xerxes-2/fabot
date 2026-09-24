# fabot — Domain Glossary

Screeps seasonal-server bot, written in F# and compiled to JS via Fable.

## Terms

### World
Everything this tick was seen to hold, read once by the shell (`World.ofGame`): every room a standing [[colony]]'s declaration names or `Game.rooms` answers with, under its own name, plus every creep we own. It holds facts and no conclusions, `decide` never sees one, and the only thing it carries across ticks is each room's [[sighting]]. ADR-0004, ADR-0052.

### Colony view
One colony's whole reading of this tick, cut from the [[world]] by a pure function (`ColonyView.ofWorld`): the rooms it works as one [[spatial projection]], the bodies it holds, its bank and controller, every colony's [[stage]], its [[foreign bodies]], its [[borrowed work]] and the [[sighting]] of each room it works. The only argument `decide` has, one per living colony per tick; it was named **Snapshot** until ADR-0052. ADR-0047, ADR-0052.

### Sighting
What the [[world]] last saw standing in one room and when: the tick vision last answered and the bare ids it answered with (`RoomSighting`), held on the heap and never in Memory. Read for one question only, the Matcher's vision grace (`Tuning.VisionGrace`), which keeps a held assignment through its room going dark and walks its holder to the [[seam]]; nothing is placed or ranked off it. ADR-0004.
_Avoid_: room intel, scouting, cache

### Tuning
Every number this bot chose rather than read off the server, in one record carried on the [[colony view]]; each field states the [[stage]] and bank it was derived at and is pinned by a pairwise test. What is not here is the [[engine constant]]s. ADR-0052.
_Avoid_: config, settings, constants

### Engine constant
A number Screeps fixes, named for the server constant it spells (`Engine`): a number belongs here when changing it would be a lie about the server, and in [[tuning]] when changing it would be a different colony. ADR-0052.
_Avoid_: game constant, magic number

### Intent
A single described action the decision layer wants performed this tick (e.g. "creep X harvests source Y"). Intents are data; they do not touch the Screeps API.

### Executor
The thin imperative shell that turns Intents into Screeps API calls — the only layer allowed to act on the game; the [[world]]'s builder may call read-only game methods to build its projection.

### Task
A unit of work in the task pool (e.g. "deliver 300 energy to spawn") that interchangeable creeps are matched to; a creep has no fixed role. In the pool it carries a [[priority]] and a [[capacity]] the [[planner]] sets and the [[matcher]] reads.

### Priority
Where a pooled Task ranks against every other, lower first — the first component of the [[matcher]]'s key, set by the [[planner]] off the tier ladder (Safety, the [[downgrade deadline]]'s Upgrade, Feeding, the [[storage]]'s draw, surplus, the [[buffer]]'s Refill, the stock's). Tiers stand ten rungs apart so a Task can step up inside its tier by a `Rung` of at most half a tier, and the [[resolver]]'s push weight rounds a rank back to its nearest tier. ADR-0007, ADR-0010, ADR-0012, ADR-0023, ADR-0052, ADR-0057, ADR-0061.
_Avoid_: rank (for the tier itself), weight, urgency

### Capacity
How many creeps a pooled Task admits at once, set by the [[planner]] and counted by the [[matcher]] at [[arrival]]: a total over holders of every shape, a share per set of [[body class]]es, a budget the holders' loads are counted against, and the set of [[post]] tiles whose standing bodies count beside the holders. A Task with no capacity at all is the ordinary case. ADR-0024, ADR-0026, ADR-0045, ADR-0051, ADR-0052, ADR-0054, ADR-0056, ADR-0069, ADR-0071.

### Body class
Which of four shapes a body is, as far as a [[capacity]] is concerned, read off the parts in order: **Fighter** (an ATTACK part), **Heavy** (the [[work-heavy body]]), **Standing** (the [[standing body]]) and **Light** (everything else). A body is exactly one class, so a per-class cap is one fold over the holders. ADR-0006, ADR-0016, ADR-0046, ADR-0052, ADR-0056.

### Harvest
The Task of digging a source or a [[thorium]] deposit, in the [[home room]] or an [[outpost]], pooled for every placed source and judged at [[arrival]] against the [[restock]] wait. Capped by the source's [[seat]]s and, for a [[work-heavy body]], by its [[post]]s; a light body must be half empty, not a [[standing body]] and find rate spare beyond the standing Posts, while a heavy one keeps its Post through the empty window and a store-less [[miner]] digs the deposit alone. ADR-0013, ADR-0016, ADR-0020, ADR-0021, ADR-0024, ADR-0025, ADR-0041, ADR-0042, ADR-0045, ADR-0048, ADR-0051, ADR-0057.

### Refill
The Task of delivering a resource to a store that will take it — the [[refill cluster]], a tower, the [[buffer]], the [[storage]], a [[ferry]]'s sink — with its rank layered by target and the resource carried on the Task. ADR-0010, ADR-0012, ADR-0023, ADR-0054, ADR-0057.

### Refill cluster
The colony's spawn and every extension of it as one [[refill]] Task, keyed by the spawn's id and standing while any member has room; its [[work area]] is the hungry members' rings, the [[emitter]] picks the member at arrival, and its [[capacity]] is a budget of the ring's free energy over the holders' loads. ADR-0054, ADR-0071.
_Avoid_: extension cluster, the spawn ring, anchor

### Withdraw
The Task of taking a resource out of a stocked [[container]], a tombstone or a ruin, or — one tier lower and only while another sink is hungry — the [[storage]]. Applicable to a body with a Carry part, half its store free, no more Work than Move and (at the [[buffer]]) a Work part, to a store that can half fill it; capped by the store's stock over the drawing row's load. ADR-0012, ADR-0016, ADR-0019, ADR-0023, ADR-0026, ADR-0046, ADR-0057, ADR-0071.

### Pickup
The Task of walking to a dropped pile and taking it, pooled from a threshold of a hundred and capped by the pile's amount over one [[hauler unit]] load; a pile carries one resource, and a [[thorium]] pile ranks at the [[storage]]'s draw tier. ADR-0010, ADR-0016, ADR-0026, ADR-0057.

### Build
The Task of spending carried energy into a construction site, surplus work at home (top rung of its tier) and feeding-tier for an [[outpost]]'s queue up to the builders' budget and for every site in a [[nursery]]. Inapplicable to a [[work-heavy body]] except for a [[container]] site on its own [[post]]. ADR-0010, ADR-0016, ADR-0020, ADR-0042, ADR-0047, ADR-0048.

### Repair
The Task of restoring a structure's hits, created below its [[hungry line]] and gone above its [[whole line]]; surplus-tier on the tier's lower rung, and a rescued decaying kind steps up two. ADR-0010, ADR-0034, ADR-0046, ADR-0061.

### Hungry line
Where a [[repair]] Task is created: `Tuning.RepairTrigger` of max hits for the decaying kinds, the [[rampart]]'s floor, full hits for the [[keep]]. ADR-0061.
_Avoid_: repair threshold

### Whole line
Where a held [[repair]] Task is gone: `Tuning.RepairWholeLine` of max for the decaying kinds, and the [[hungry line]] itself for the [[rampart]] and the [[keep]]. It applies only while a creep holds the Repair, so a repair is a job rather than a one-tick top-up. ADR-0061.
_Avoid_: hysteresis

### Reserve
The Task of holding an [[outpost]]'s controller under this colony's [[reservation]]: feeding-tier, applicable to a CLAIM part and no energy state, range 1 beside an obstacle, one holder per controller, pooled for every projected controller that is not ours, not a [[candidate colony]]'s and held by nobody else's CLAIM parts. ADR-0006, ADR-0042, ADR-0047. 🚩

### Claim
The Task of taking a [[candidate colony]]'s controller for our own with CLAIM parts — the first tick of a second [[colony]]. Pooled one per candidate whose controller is projected and takeable; applicability, [[work area]] and cap are [[reserve]]'s. ADR-0047. 🏴

### Reclaim
The Task of claiming the sector Reactor for this player with CLAIM parts, pooled one per declared [[errand]] off the declaration alone, feeding-tier, and fired only on a tick the reactor is not ours — and not an [[ally]]'s while their store is above the handover mark, unless a load of ours stands beside it with room to go in. ADR-0057, ADR-0060, ADR-0069. ☢️

### Flee
The Task of getting out of a [[reach]]: applicable to any creep standing inside one except a [[work-heavy body]] and a Fighter, with the room's safe set as its [[work area]] and no action. One of the two Safety-tier Tasks. ADR-0033, ADR-0056. 🏃

### Planner
The pure step that reads a [[colony view]] and generates this tick's full Task pool from scratch, setting each entry's [[priority]] and [[capacity]] off the [[atlas]]. ADR-0052.

### Matcher
The pure step that assigns creeps to Tasks by greedy matching, remembering only current assignments between ticks (anti-thrash). It knows no kinds of Task: it compares [[priority]] and [[travel cost]] and counts holders against [[capacity]]. ADR-0052.

### Emitter
The pure step that turns the tick's assigned Tasks into each creep's action Intent and [[chat bubble]], judged from tick-start geometry off the same [[atlas]] as the Matcher and Resolver.

### Seat
A walkable tile adjacent to a source, and the capacity unit of Harvest; a Seat under a [[post]] is the garrison's alone. ADR-0024, ADR-0051.

### Post
A tile worth garrisoning with a heavy-WORK body — the [[seat]] under a source [[container]] or its construction site, at most one per source (the one farthest from the controller) — in every projected room, counted room by room. The unit of the [[anchor]] row's quota, body and charge, of a [[work-heavy body]]'s Harvest cap and [[work area]], and the tile whose standing body counts beside a Task's holders. ADR-0012, ADR-0020, ADR-0021, ADR-0024, ADR-0041, ADR-0042, ADR-0051, ADR-0053, ADR-0076.

### Container
A container structure as the [[layout]] places it, per target and never per tile: one source container per source on the [[seat]] nearest its [[trunk]] (an [[outpost]]'s on the Seat with the shortest [[walk]] to the [[seam]], over Seats no site already holds), one controller container in the Upgrade [[work area]], and one mineral container per [[thorium]] deposit from `Tuning.ExtractorLevel`. An outpost's is the switch that admits the room into the economy. ADR-0012, ADR-0023, ADR-0040, ADR-0042, ADR-0057, ADR-0068.

### Buffer
The built [[container]] standing in the home room's controller's Upgrade [[work area]] on no [[post]] (`Atlas.controllerContainers`): the [[upgrader]] row's drink, a [[refill]] sink of the [[hauler unit]], and what a [[ferry]] is hired against in a child. ADR-0019, ADR-0046.
_Avoid_: controller container, upgrade store

### Work Area
The tiles a creep may stand on while performing its Task: the passable tiles within the action's range of the target, narrowed for a [[work-heavy body]] to its source's [[post]]s (or the bare [[seat]]s at home only) and taken less the target room's [[reach]]; the Safety tier's two Tasks derive theirs from the threat facts instead. Unreachable or empty makes the Task inapplicable. ADR-0020, ADR-0033, ADR-0045, ADR-0056.

### Seam
A tile on a room's border and the tile it lands you on in the neighbouring room — never a place to stand, answered off the border layer, and a crossing only when the far room's ground reaches the landing. The joint a cross-room [[walk]] is summed at along a chain of at most `Tuning.MaxHops`, and the anchor an [[outpost]]'s [[container]] pick is measured to. ADR-0036, ADR-0041, ADR-0042, ADR-0058, ADR-0059, ADR-0062.

### Orphan
A [[seam]] crossing whose landing tile has no ground of the far room's beside it, and therefore not a crossing; what makes a band directed. ADR-0062.

### Transit room
A room a [[colony]] projects only because a [[walk]] to one of its [[outpost]]s crosses it: terrain, a border ring and the bodies vision finds there, but no work pooled and no `InvaderRaid` [[stand-down]] opened. ADR-0058, ADR-0065, ADR-0066.

### Move Intent
A creep's movement desire for one tick: a head-and-tail list of candidate standing tiles in preference order, a priority, and the tiles it counts as still working from. Input to the [[resolver]], whose single-step output is what becomes an Intent. ADR-0001, ADR-0008, ADR-0033, ADR-0041.

### Resolver
The pure step that arbitrates Move Intents into single-step moves by a weighted matching over augmenting chains of displacements — one pass per room over every creep of ours standing in it, so the shell folds every colony's intents together. A [[grounded]] creep's tile and a [[foreign bodies]]'s are walls for the tick. ADR-0001, ADR-0008, ADR-0047.

### Grounded
A creep still paying off fatigue this tick: the [[resolver]] neither moves it nor lets anyone claim or displace through its tile, and a traveller stopped by one sidesteps where it can. ADR-0008.

### Travel cost
The cheapest-path cost from a creep to a Task's Work Area over the [[spatial projection]] for that body's load, in half-ticks: terrain weights scaled by the fatigue factor plus the [[occupancy surcharge]] on the creep's own room's leg. A ranking price only; every clock reads the [[walk]]. ADR-0002, ADR-0008, ADR-0010, ADR-0029, ADR-0070.

### Walk
A traffic-blind path priced in whole ticks, no step below one — the [[atlas]]'s clock beside [[travel cost]]'s price, summed leg by leg over the [[seam]]s of a chain. ADR-0029, ADR-0030, ADR-0041, ADR-0058.

### Occupancy surcharge
The extra cost the flood prices onto a step landing on an occupied tile, charged on the creep's own room's leg alone; traffic re-prices a route and never makes a Task inapplicable. ADR-0008, ADR-0010, ADR-0070.

### Workforce target
The number of creeps the colony maintains, derived fresh each tick as the sum of the rows' quotas — [[reserver]]s, [[guard]]s, [[anchor]]s, haulers plus the [[ferry]], [[miner]]s, [[upgrader]]s, workers plus the [[pioneer]]s — with the [[supply floor]] in front, [[expiring]] bodies out and [[casting]] ones in. The rows are asked in order and a row the bank cannot afford yields the tick. ADR-0012, ADR-0026, ADR-0037, ADR-0042, ADR-0046, ADR-0047, ADR-0049, ADR-0050, ADR-0053, ADR-0056, ADR-0057.

### Room energy
One room's shared spawn-energy account (spawn + extensions) — a colony fact, not spawn state. Spawn planning debits it in spawn order so the same energy is never committed twice.

### Home room
The one room this colony owns, spawns from and plans for: the room the [[spatial projection]] is anchored in, the [[layout]] is derived for and the [[trunk]]s radiate from, named by the declaration and never by a spawn. Its bootstrap rules — the bare-[[seat]] fallback and an unposted source's seat count — are its alone. ADR-0020, ADR-0041, ADR-0042, ADR-0045, ADR-0047.
_Avoid_: spawn room (as the definition), main room, home base

### Room position
A tile of a named room (`RoomPos`), the only shape a tile takes once it leaves the grid it indexes; `Pos` stays the bare grid coordinate inside one room's tables. ADR-0041, ADR-0052.
_Avoid_: position (unqualified), coordinate (for the room-joined thing), room position (for a bare `Pos`)

### Spatial projection
A [[colony view]]'s map-shaped view of the rooms the colony works, one projection layered by room name: terrain (memoised per room), entity positions, target kinds, roads, rival site tiles and hits on repairable kinds, with ground on tiles 1..48 and the border rings beside it. Raw data, always present, absent per entry, consulted only through the [[atlas]]. ADR-0004, ADR-0005, ADR-0010, ADR-0031, ADR-0036, ADR-0041.

### Atlas
The per-tick query interface over the [[spatial projection]]: seats, work areas, [[seam]]s, prices, first steps, action permission, and the placement queries the [[layout]] derives from. Total, built fresh each tick, and recalling three tables on the [[census signature]]: the spawn-origin [[walk]]s, the seam walks and the far leg of every cross-room price. ADR-0004, ADR-0005, ADR-0020, ADR-0032, ADR-0033, ADR-0041, ADR-0070.

### Anchor
The heavy-WORK [[body pattern]] cast for a [[post]]: Work up to the saturation of the rock its Post seats, one Carry and minimal Move, cast into the vacant Posts richest first, pinned by [[travel cost]] and working in place. ADR-0006, ADR-0012, ADR-0016, ADR-0020, ADR-0021, ADR-0025, ADR-0026, ADR-0048, ADR-0053.

### Hauler unit
The repeating [Carry; Carry; Move] block hauler bodies are built from, at road parity, living in the [[withdraw]]→[[refill]] cycle. Its quota is the colony's whole haul — each source [[container]]'s round trip to the colony's sinks times its output over carry capacity — rounded up once, with each Thorium mine's haul rounded apart to a body of its own. ADR-0012, ADR-0029, ADR-0030, ADR-0042, ADR-0049.

### Work-heavy body
A living body with strictly more Work parts than Move — a predicate over the parts, never a row name. Its intake is digging from its [[post]], and it walks to nothing else. ADR-0003, ADR-0006, ADR-0016, ADR-0020, ADR-0024, ADR-0033, ADR-0048.

### Standing body
A living body carrying fewer than one Carry part per four Work (`Carry × 4 < Work`): [[build]], [[repair]] and [[refill]] are inapplicable to it, and it fetches from the [[buffer]] alone. ADR-0006, ADR-0046.

### Body pattern
The repeating part block a body is generated from, or for the sized rows the row's minimal cast beside its own sizing rule; a row with no sizing rule of its own is refused at its first cast. A pattern shapes what a creep is good at, never what it is assigned. ADR-0006, ADR-0046, ADR-0050.

### Worker unit
The repeating [Work; Carry; Move] block worker bodies are built from — the generalist, commuting row, hired out of what the [[upgrader]] row leaves of the surplus and floored at two while anything stands in the [[build]] or [[repair]] pool, one otherwise. ADR-0003, ADR-0037, ADR-0046.

### Upgrader
The row that stands beside the [[buffer]] and spends the surplus into the controller: one Carry and `W = M = floor((capacity − 50) / 150)` pairs, a [[standing body]], hired only while a built controller [[container]] stands and the cast reads back as one, its quota the surplus over one body's lifetime cost rounded down. ADR-0016, ADR-0019, ADR-0037, ADR-0046.

### Reserver
The [Claim; Move] row: one per declared [[outpost]] whose controller nobody owns or holds, sized `ceil((5000 − reservation remaining) / 600)` CLAIM parts; one per [[candidate colony]] for the [[claim]]; and one per declared [[errand]] as the [[re-claimer]]. Cast before every other row, and withheld from a room the [[guard]] row still has a gap for. ADR-0006, ADR-0042, ADR-0047, ADR-0057, ADR-0069, ADR-0072.

### Re-claimer
The [[reserver]] row's third face: the `[Claim; Move]` body hired one per declared [[errand]], which walks to the sector Reactor and holds the [[reclaim]] Task for the rest of its life, relieved with a 25-tick overlap (`Tuning.ReclaimerOverlap`) on one permanent seat. ADR-0057, ADR-0060, ADR-0069. ☢️

### Courier
The fixed `[20 Carry; 10 Move]` body that carries one `Tuning.ReactorLoad` — or the remainder, once no more ore is coming — of [[thorium]] from the home [[storage]] to the declared sector Reactor in one delivery slot; a body carrying a delivery is marked by `Facts.carryingADelivery`. ADR-0006, ADR-0067, ADR-0073.
_Avoid_: hauler, runner, delivery creep

### Guard
The `[Tough; Attack×3; Move×5; Heal]` [[body pattern]] cast on contact and never before: one per declared [[outpost]] a [[threat]] stands in (or that the [[raid log]] remembers one in), two where the raid's healing outruns one block's damage, sized to win the exchange, and holding the `Guard of roomName` Task on the threats' range-1 ring at Safety priority. Its [[body class]] is Fighter and its quota does not decay. ADR-0003, ADR-0050, ADR-0054, ADR-0056, ADR-0072. ⚔️
_Avoid_: defender, bodyguard, soldier

### Miner
The store-less [[body pattern]] — Work up to twenty with one Move per `Tuning.MinerWorkPerMove` — that stands on the mineral [[container]] and drops its [[thorium]] into it, one per deposit the colony can dig (owned room, extractor and container standing), cast behind the [[hauler unit]] and ahead of the [[upgrader]]. ADR-0006, ADR-0016, ADR-0057. ⛏
_Avoid_: driller, extractor (which is the structure), remote miner

### Fatigue parity
The body-generation invariant that a worker body padded beyond whole [[worker unit]]s never moves slower than the pure-unit body, empty or loaded; the remainder buys Carry as parity allows, then Move, never Work. ADR-0003.

### Layout
The deterministic full structure plan computed whole from the [[atlas]] and never persisted: the clustered kinds — the spawns beyond the first among them — by one ordering (the oldest spawn's checkerboard, nearest first, [[working ground]] and [[link footing]]s held out) placed at `controller.Level + Tuning.HorizonLookahead` and reserved at the allowance ceiling, plus the [[storage]], the [[link footing]]s, the [[container]]s, the [[trunk]]s and the [[rampart]]s. Recomputed only when its [[census signature]] changes; road sites are placed from RCL3 up. ADR-0011, ADR-0017, ADR-0022, ADR-0027, ADR-0034, ADR-0040, ADR-0063, ADR-0064.

### Trunk
A paved line in the [[layout]] — each source to the controller and to the spawn, plus the Upgrade [[work area]]'s swamps — priced on raw terrain and routed around the reserved tiles; also the line a human paves in an [[outpost]], which the builders' queue orders from the [[seam]] outward. A trunk that routes nothing is recorded on the [[layout record]]. ADR-0011, ADR-0042.

### Working ground
The tiles the colony works from — every source's [[seat]]s, every [[thorium]] deposit's tile and seats, and at home the controller's Upgrade [[work area]] — excluded from the [[layout]]'s clustered ordering and the base of the [[idle ground]]. ADR-0022, ADR-0034, ADR-0042, ADR-0057.

### Idle ground
Where standing idle blocks somebody: the [[working ground]] plus the walkable range-1 ring of each store — the set a body with no [[task]] heads its first step off. ADR-0001, ADR-0022.

### Storage
The colony's stock, placed on the cluster's first pick from RCL4: the deepest [[refill]] target, a [[withdraw]] source pooled only while another sink is hungry (ranked with the containers while the colony is starved), the [[thorium]] sink, and a [[keep]] structure. ADR-0022, ADR-0023, ADR-0034, ADR-0057, ADR-0071.

### Thorium
The season's scoring resource and the one resource beside energy this colony names (`Resource = Energy | Thorium`): a finite deposit read off `FIND_MINERALS`, dug by the [[miner]] through an extractor from `Tuning.ExtractorLevel`, moved by the [[hauler unit]] row at the [[storage]]'s tiers, banked in the [[storage]] and carried to the Reactor by the [[courier]]. A body carries one resource at a time. ADR-0057, ADR-0060, ADR-0067.
_Avoid_: mineral (the engine's word for the object), ore

### Keep
The structures worth defending — the spawn, every tower and the [[storage]]: each ramparted, repaired to full, and a reason to fire the [[safe-mode reflex]] when damaged, from `Independent` up. ADR-0034.
_Avoid_: base, core

### Rampart
The walkable defensive structure the [[layout]] places over every standing [[keep]] structure and every [[post]] a [[container]] stands on, kept above a floor of hits by [[repair]]; a creep on its own rampart is in no [[reach]]. ADR-0034.

### Link footing
A tile the [[layout]] reserves for a link beside each planned source [[container]], the controller container and the [[storage]]: off every [[trunk]] and other footing, nearest the spawn, the one structure footing allowed on [[working ground]], and unfilled at RCL5 because the hauler row is already at its floor. A footing the fold finds no tile for is recorded on the [[layout record]]. ADR-0022, ADR-0027, ADR-0035, ADR-0038, ADR-0042, ADR-0049.

### Colony
A [[home room]] and the [[outpost]]s worked from it — the unit the decision layer and the declaration (`{ Home; Outposts; Mother }`) are written in, declared and never discovered. A **living** colony owns its home and holds a spawn there, and is what `decide` runs over, once each per tick; a creep belongs to the colony whose spawn cast it, with adoption for a creep standing in a room only another colony projects. ADR-0047, ADR-0052.
_Avoid_: base, empire, room group

### Stage
Where a [[colony]] stands in its life, derived every tick by `Colony.stageOf` off ownership, a spawn of ours and the controller's level: [[nursery]], bootstrapping ([[bootstrap window]]) or independent. A room that is no colony of ours has no stage. ADR-0052.
_Avoid_: phase, generation, tier

### Candidate colony
A declared [[colony]] whose home room this colony does not own yet, projected and worked as an [[outpost]] of the mother until the [[claim]] lands. ADR-0047.

### Mother colony
The declared [[colony]] that raises another: it projects, mines and reserves the child's room until the child stands on its own, first through its outpost list and then through the child's `Mother` field, and takes a lost child back for a [[claim]]. ADR-0047.
_Avoid_: parent colony, host colony, parent (as the field)

### Nursery
The first [[stage]] of a child [[colony]]'s life — claimed by us with no spawn of ours standing yet — still projected by its [[mother colony]] as an [[outpost]], with every site in it feeding-tier [[build]] outside the outpost builders' budget. ADR-0047, ADR-0052.
_Avoid_: child colony (as the state), colony under construction

### Bootstrap window
The second [[stage]] of a child [[colony]]'s life, from the tick a spawn stands in it until RCL3, in which its [[mother colony]] still projects its controller, sites and spawn, pools its Upgrade and Builds, hires [[pioneer]]s and lends a [[ferry]]. ADR-0047, ADR-0052.
_Avoid_: bootstrap phase, adolescence, borrowing period

### Borrowed work
What one [[colony]] may take of another's, named on its [[colony view]] and bounded: the home rooms of the children it is raising (their Upgrade and Builds, and a bootstrapping child's [[buffer]] for the [[ferry]]) and of the children it has lost (a [[claim]]). ADR-0047, ADR-0052.
_Avoid_: shared work, cross-colony pool

### Foreign bodies
Where the creeps a [[colony]] does not hold stand in the rooms it works (`ColonyView.Foreign`): priced around by the [[occupancy surcharge]] and treated as walls by a colony arbitrating alone. ADR-0052, ADR-0070.
_Avoid_: other creeps, outsiders, hostiles

### Pioneer
One of the `Tuning.PioneerCount` extra [[worker unit]]s a [[mother colony]] hires while a [[nursery]] of hers stands or a child is in its [[bootstrap window]] — a generalist, not a row, whose job across the [[seam]] reaches it by rank. ADR-0047.

### Ferry
The haul a [[mother colony]] lends a bootstrapping child: `Tuning.FerryLoads` hauler bodies hired against the child's [[buffer]] and priced from her [[storage]], the buffer being a [[refill]] target of hers and never a [[withdraw]] one. ADR-0023, ADR-0047, ADR-0052.
_Avoid_: convoy, supply line, caravan

### Outpost
A room within `Tuning.MaxHops` crossings of the [[home room]] that this colony mines but does not own, declared inside the colony that works it with its sources and controller under the engine's ids, and admitted only where a chain runs both ways; a declaration with no chain is refused and named on the [[layout record]]. The community calls these *remotes*. ADR-0004, ADR-0041, ADR-0042, ADR-0047, ADR-0058, ADR-0062.
_Avoid_: remote, franchise, territory

### Errand
A room a [[colony]] declares because it must walk a body there and act on one named object in it — a room name, the object's id and tile, and nothing else — so a sector centre with no controller can be declared. Nothing else in it is pooled, and an armed non-Source-Keeper hostile in it makes it a [[stand-down]]. ADR-0060, ADR-0075.
_Avoid_: goal, mission, remote target

### Reservation
A neutral controller held by this colony's CLAIM parts, which doubles every source in that room, decays by one a tick and caps at 5,000: the economic precondition of an [[outpost]]. **Held** is the colony's word for a room it owns or reserves; the holder is ours, the NPC Invader's or another player's, never a flag. ADR-0042, ADR-0043.

### Arrival
The tick a creep can first act on a Task — its [[walk]] to the Task's [[work area]] — and the horizon at which every time-aware judgement is made: the [[restock]] wait, [[expiring]], and a [[capacity]]'s count of holders (with a handover window where the Task declares one). ADR-0025, ADR-0026, ADR-0029, ADR-0069.

### Restock
The moment a drained source holds energy again, projected as ticks remaining (zero for a source holding energy now) — the one time fact a [[colony view]] carries about a source. ADR-0013, ADR-0025.

### Expiring
A living creep whose remaining life is at or under its [[lead]] (plus `Tuning.ReclaimerOverlap` for a body standing in its [[errand]] room): excluded from the [[workforce target]]'s living count so its successor is cast, never released for it. The [[courier]] expires economically after its one delivery slot. ADR-0026, ADR-0067, ADR-0069.

### Casting
A body the colony has already paid for that is not yet standing — a creep in a spawn's oven (`RoomFacts.Casting`) — counted inside the [[workforce target]]'s living count and read back to its row off its parts and name prefix. ADR-0026, ADR-0067.
_Avoid_: spawning creep, unborn body, pending creep

### Lead
The time a creep's replacement needs to stand on its tile: the successor body's cast time plus its [[walk]] from beside the spawner, for the body its row would cast for this creep's own [[post]], recalled on the [[census signature]]. ADR-0026, ADR-0029, ADR-0030, ADR-0032, ADR-0053.

### Verdict
The reasoned outcome a decision step returns beside its decision — data, never a log line: the [[matcher]]'s says which Task won a creep and why, what was kept, released or refused (a too-early refusal carrying the [[walk]] and the [[restock]] wait), and the [[resolver]]'s what became of its movement. Manufactured evidence is computed only for the [[verbose list]]. ADR-0009, ADR-0018, ADR-0025, ADR-0029.

### Census signature
The fingerprint of everything a census-derived plan reads — every standing structure, pending site and rival site tile named with its room, the [[home room]]'s level and name, and the rate every projected room's sources are priced at — under which the [[layout]], the hauler quota and the recalled [[walk]] tables are held. Heap only. ADR-0017, ADR-0032, ADR-0044.

### Verbose list
The creep names owed full candidate scoring and reroute attribution, stored under `Memory.fabot.observe` and flipped from the terminal; empty, absent or malformed means off. ADR-0018, ADR-0030.

### Reactor programme
The one flat record of the seasonal scoring line at `Memory.fabot.observe.reactor`, read with `observe.mjs reactor`: one sample of the Reactor's owner, store, `continuousWork` and tick seen, the Thorium banked at home, the last evidenced delivery and the count of dry ticks. ADR-0057, ADR-0060.

### Transition log
The per-creep ring of recent task handovers and movement events, each with its [[verdict]] and tick, written only when something changed and kept only for living creeps. ADR-0009, ADR-0028.

### Raid log
The colony's episodic record of [[hostile]] presence in the rooms it works, under `Memory.fabot.observe.colonies.<home>.raids` and read with `observe.mjs raids` / `outposts`: a ring of raid episodes (window, roster, closest approach, losses, damage, closed by the quiet gap), a ring of [[stand-down]] episodes with their deadlines, the rooms last seen owned by a rival with their look ticks, the controllers held by somebody else's CLAIM parts, and the outposts a [[threat]] was last seen in. A record to be read; the stand-down gate is its one reader. ADR-0028, ADR-0034, ADR-0041, ADR-0043, ADR-0047, ADR-0056, ADR-0065, ADR-0075.

### Layout record
The colony-level channel carrying what the [[layout]] could not deliver this tick, under `Memory.fabot.observe.colonies.<home>.layout` and read with `observe.mjs layout`: the [[link footing]]s with no tile, the [[trunk]]s with no path, the [[container]] picks deferred to a standing container, and the declarations refused for want of a chain. Written every tick, empty lists included; nothing reacts to it. ADR-0011, ADR-0017, ADR-0027, ADR-0035, ADR-0040, ADR-0047.

### CPU line
The per-tick record of what a tick cost, flat and keyed by tick under `Memory.fabot.observe`, read with `observe.mjs cpu`: the milliseconds spent, split at the loop's phase boundaries, plus the count of accepted [[intent]]s. Measured, never budgeted — nothing in the bot reads it back. ADR-0041, ADR-0047.

### Breach log
The colony-level channel carrying what is broken right now, under `Memory.fabot.observe.colonies.<home>.breaches` and read with `observe.mjs breaches`: one row per live invariant violation in the [[thorium]] programme, with its room, object id, the number that makes it actionable and how long it has stood. Measured against what the colony can see; nothing reacts to it. ADR-0004, ADR-0035.

### Chat bubble
The glyph an assigned creep says over its head each tick, one per [[task]] (⛏ Harvest · 📥 Withdraw · 🧲 Pickup · 🔋 Refill · 🔨 Build · 🔧 Repair · ⚡ Upgrade · 🚩 Reserve · 🏴 Claim · 🏃 Flee · ⚔️ Guard). Observability only; unassigned creeps show nothing.

### Safe-mode reflex
The colony reflex that emits `ActivateSafeMode` when a CLAIM-part [[hostile]] stands within range 3 of the home controller, when a [[keep]] structure is below full hits with a hostile in the [[home room]], or — with no tower standing — on the first armed hostile; gated only on stock remaining and safe mode not running. ADR-0007, ADR-0015, ADR-0034.

### Signature reflex
The reflex that writes `Colony.signature`, a human's line, onto any controller the projection places whose sign is not already that line, by any creep of ours within range 1 of it. A reflex and never a [[task]]: it sends nobody anywhere.
_Avoid_: room sign

### Pickup reflex
The colony reflex that emits a pickup Intent for every creep with free capacity and no [[thorium]] aboard standing within range 1 of a dropped energy pile (`Atlas.droppedEnergyIn`), paired one room at a time and deduplicated against the [[pickup]] Task's own act. No movement, no matching, no threshold. ADR-0041, ADR-0057.

### Fire reflex
The colony reflex that has every tower shoot the [[hostile]] nearest to itself each tick one stands in the [[home room]] — attack only, per tower, no focus fire and no energy floor. ADR-0014.

### Ally
A player we have agreed with, by username, declared by hand in `Colony.allies` (#412). Their creeps are no [[hostile]], so nothing fires on them, flees them or stands down for them. On a Reactor they hold, the burn is theirs until a handover: no load is drawn beyond `Tuning.AllyHandoverLead` of their store, and the [[reclaim]] waits for `Tuning.AllyHandover`.
_Avoid_: friend, alliance

### Hostile
A hostile creep as the [[world]] projects it — id, owner, [[room position]] and body parts — swept from every room the colony works and can see, less every [[ally]]'s; out of the [[spatial projection]], gating Tasks only through a [[threat]]'s [[reach]]. An invader core is a structure and rides a field of its own. ADR-0007, ADR-0014, ADR-0028, ADR-0033, ADR-0041, ADR-0043, ADR-0052.

### Threat
A [[hostile]] carrying an ATTACK or RANGED_ATTACK part, read off the parts and never the owner; under safe mode in a room we own nothing is a Threat. Only a Threat has a [[reach]]. ADR-0033.
_Avoid_: enemy, attacker

### Reach
The tiles a [[threat]] can hurt — its weapon range plus a margin of two, less the tiles under our standing [[rampart]]s — filed by the room the Threat stands in. It subtracts from every Task's [[work area]], is what [[flee]] is applicable inside of, and holds the spawn while a tile beside it lies in one. ADR-0033, ADR-0041.
_Avoid_: danger zone, threat radius

### Keeper margin
The tiles masked out of a Source Keeper room's walkable ground within `Tuning.keeperMargin` (six, derived) of every rock a keeper is pinned to, declared by room name and applied to the raw ground and the border ring before any query reads them. The hostile list is untouched; the ground is. ADR-0004, ADR-0060, ADR-0062.
_Avoid_: keeper radius, SK exclusion

### Downgrade deadline
The hard floor on the controller's downgrade timer, half the level's full timer, inside which Upgrade outranks even the feeding tier so the [[safe-mode reflex]] stays fireable. ADR-0007.

### Stand-down
An [[outpost]] or [[errand]] withdrawn from — out of the [[spatial projection]], so nothing pools, counts or walks there — until a tick read off the threat: an invader core's collapse, a rival reservation's end, an armed raid's longest remaining life, or 2,500 ticks where none can be read; a rival's ownership is clockless and re-looked at every `Tuning.RivalRecheck`. Recorded in the [[raid log]], not a route lock except for a [[stronghold]], which is impassable. ADR-0043, ADR-0065, ADR-0066, ADR-0074, ADR-0075.

### Stronghold
An invader core of level 1 or more — towers under ramparts and a garrison — whose room is `StandDown.Impassable` and taken out of every chain until its collapse timer runs out. ADR-0074.

### Disaster fallback
The zero-creep spawning rule: an empty colony spawns bare [[worker unit]]s from whatever [[room energy]] is banked right now, ignoring the remainder — time-to-first-creep outranks spending the bank. ADR-0006.

### Supply floor
The one spawning row that is a floor and not a quota: a colony holding no living body that can put energy into an extension casts one hauler in front of every row, sized from the [[room energy]] banked right now. ADR-0050.

## Avoided terms

- **Role** — creeps are not born with roles; work is Task-based. Don't reintroduce role-based vocabulary.
