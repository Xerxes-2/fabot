# fabot — Domain Glossary

Screeps seasonal-server bot, written in F# and compiled to JS via Fable.

## Terms

### World
Everything this tick was seen to hold, read once by the shell (`World.ofGame`): every room a standing [[colony]]'s declaration names or `Game.rooms` answers with, under its own name, plus every creep we own. It holds facts and no conclusions, `decide` never sees one, and the only thing it carries across ticks is each room's [[sighting]]. ADR-0004, ADR-0052.

### Colony view
One colony's whole reading of this tick, cut from the [[world]] by a pure function (`ColonyView.ofWorld`): the rooms it works as one [[spatial projection]], the bodies it holds (naming those standing in a room it does not project, **unprojected**, which the projection places nowhere), its bank and controller, every colony's [[stage]], its [[foreign bodies]], its [[borrowed work]] and the [[sighting]] of each room it works. The only argument `decide` has, one per living colony per tick; it was named **Snapshot** until ADR-0052. ADR-0047, ADR-0052.

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

### Light tick
A tick that decides nothing: it re-issues every act the last full tick issued or planned whose actor and target still stand — a hauler's act on the tile it stepped onto, an observer's look, a lab's reaction, a swing at a core — leaving range, store and cooldown to the engine (a drop only from the tile it was decided on; never a planner's or a structure's one-shot, nor an act naming an amount), strikes again at each fighter's aim (its target on the full tick, in reach or not) while that target is in sight, and steps each walker one tile along its step plan. It follows each full tick unless a fact the plans did not account for forces the tick full — a raider in a home, a hostile come within a step of striking something of ours a creep fighting it is not, a creep hurt past what the plans fight and heal, born, gone or on a border ring, a changed controller, or a structure's fight — and then only the colonies the fact concerns decide, the rest replaying their plans. Builds no [[world]] and no [[colony view]]. ADR-0082. 💤

### Executor
The thin imperative shell that turns Intents into Screeps API calls — the only layer allowed to act on the game; the [[world]]'s builder may call read-only game methods to build its projection.

### Task
A unit of work in the task pool (e.g. "deliver 300 energy to spawn") that interchangeable creeps are matched to; a creep has no fixed role. In the pool it carries a [[priority]] and a [[capacity]] the [[planner]] sets and the [[matcher]] reads.

### Priority
Where a pooled Task ranks against every other, lower first — the first component of the [[matcher]]'s key, set by the [[planner]] off the tier ladder (Safety, the [[downgrade deadline]]'s Upgrade, the urgent band — a starved [[refill cluster]] or a raided home's towers, and a rung over them a struck rampart (#530) — Feeding, the [[storage]]'s draw, surplus, the [[buffer]]'s Refill, the stock's). Tiers stand ten rungs apart so a Task can step up inside its tier by a `Rung` of at most half a tier, and the [[resolver]]'s push weight rounds a rank back to its nearest tier. For a body with Work the [[matcher]] alone gives back an intake's lift above its tier, a rung per `Tuning.IntakeTravelPerRung` of travel past the body's cheapest intake (#501). ADR-0002, ADR-0007, ADR-0010, ADR-0012, ADR-0023, ADR-0052, ADR-0057, ADR-0061.
_Avoid_: rank (for the tier itself), weight, urgency

### Capacity
How many creeps a pooled Task admits at once, set by the [[planner]] and counted by the [[matcher]] at [[arrival]]: a total over holders of every shape, a share per set of [[body class]]es or of bodies with a Work part, a budget the holders' loads are counted against, and the set of [[post]] tiles whose standing bodies count beside the holders. A Task with no capacity at all is the ordinary case. ADR-0024, ADR-0026, ADR-0045, ADR-0051, ADR-0052, ADR-0054, ADR-0056, ADR-0069, ADR-0071.

### Body class
Which of four shapes a body is, as far as a [[capacity]] is concerned, read off the parts in order: **Fighter** (an ATTACK part), **Heavy** (the [[work-heavy body]]), **Standing** (the [[standing body]]) and **Light** (everything else). A body is exactly one class, so a per-class cap is one fold over the holders. ADR-0006, ADR-0016, ADR-0046, ADR-0052, ADR-0056.

### Harvest
The Task of digging a source or a [[thorium]] deposit, in the [[home room]] or an [[outpost]], pooled for every placed source and judged at [[arrival]] against the [[restock]] wait. Capped by the source's [[seat]]s and, for a [[work-heavy body]], by its [[post]]s; a light body must be half empty, not a [[standing body]] and find rate spare beyond the standing Posts, while a heavy one keeps its Post through the empty window and a store-less [[miner]] digs the deposit alone. ADR-0013, ADR-0016, ADR-0020, ADR-0021, ADR-0024, ADR-0025, ADR-0041, ADR-0042, ADR-0045, ADR-0048, ADR-0051, ADR-0057.

### Refill
The Task of delivering a resource to a store that will take it — the [[refill cluster]], a tower, the [[buffer]], the [[storage]], a [[ferry]]'s sink — with its rank layered by target and the resource carried on the Task. Not to the store an energy load was drawn from, for `Tuning.DrawnHold` ticks after the draw (`CreepInfo.DrawnFrom`, #530). ADR-0010, ADR-0012, ADR-0023, ADR-0054, ADR-0057.

### Refill cluster
The colony's spawn and every extension of it as one [[refill]] Task, keyed by the spawn's id and standing while any member has room; its [[work area]] is the hungry members' rings, the [[emitter]] picks the member at arrival, and its [[capacity]] is a budget of the ring's free energy over the holders' loads. ADR-0054, ADR-0071.
_Avoid_: extension cluster, the spawn ring, anchor

### Withdraw
The Task of taking a resource out of a stocked [[container]], a tombstone or a ruin, or — one tier lower and only while another sink is hungry, an emergency one under the [[emergency reserve]] — the [[storage]]. Applicable to a body with a Carry part, half its store free — or, past half, a store within `Tuning.TopUpReach` whose whole holding fits (the **top-up**, #530) — no more Work than Move and (at the [[buffer]]) a Work part, to a store that can half fill it, or any store at the [[pickup]] threshold where no stocked Storage stands; capped by the store's stock over the drawing row's load, and the Storage's carriers by the room its sinks have left less what their Refills' holders already carry. ADR-0012, ADR-0016, ADR-0019, ADR-0023, ADR-0026, ADR-0046, ADR-0057, ADR-0071.

### Pickup
The Task of walking to a dropped pile and taking it, pooled from a threshold of a hundred and capped by the pile's amount over one [[hauler unit]] load; a pile carries one resource, and a [[thorium]] pile ranks at the [[storage]]'s draw tier. ADR-0010, ADR-0016, ADR-0026, ADR-0057.

### Build
The Task of spending carried energy into a construction site, surplus work at home (top rung of its tier) and feeding-tier for an [[outpost]]'s queue up to the builders' budget and for every site in a [[nursery]]. Inapplicable to a [[work-heavy body]] except for a [[container]] site on its own [[post]]. ADR-0010, ADR-0016, ADR-0020, ADR-0042, ADR-0047, ADR-0048.

### Repair
The Task of restoring a structure's hits, created below its [[hungry line]] and gone above its [[whole line]]; surplus-tier on the tier's lower rung, and a rescued decaying kind steps up two. ADR-0010, ADR-0034, ADR-0046, ADR-0061.

### Hungry line
Where a [[repair]] Task is created: `Tuning.RepairTrigger` of max hits for the decaying kinds, the [[rampart]]'s floor (its tile's declared one, else `Tuning.RampartFloor`), full hits for the [[keep]]. ADR-0061.
_Avoid_: repair threshold

### Whole line
Where a held [[repair]] Task is gone: `Tuning.RepairWholeLine` of max for the decaying kinds, and the [[hungry line]] itself for the [[rampart]] and the [[keep]]. It applies only while a creep holds the Repair, so a repair is a job rather than a one-tick top-up. ADR-0061.
_Avoid_: hysteresis

### Reserve
The Task of holding an [[outpost]]'s controller under this colony's [[reservation]]: feeding-tier, applicable to a CLAIM part and no energy state, range 1 beside an obstacle, one holder per controller, pooled for every projected controller that is not ours, not a [[candidate colony]]'s and held by nobody else's CLAIM parts. ADR-0006, ADR-0042, ADR-0047. 🚩

### Claim
The Task of taking a [[candidate colony]]'s controller for our own with CLAIM parts — the first tick of a second [[colony]]. Pooled one per candidate whose controller is projected and takeable; applicability, [[work area]] and cap are [[reserve]]'s. ADR-0047. 🏴

### Reclaim
The Task of claiming the sector Reactor for this player with CLAIM parts, pooled one per declared [[errand]] off the declaration alone, feeding-tier, and fired only on a tick the reactor is not ours — and neither pooled nor its body cast while an [[ally]] is burning in it (#415). ADR-0057, ADR-0060, ADR-0069. ☢️

### Flee
The Task of getting out of a [[reach]]: applicable to any creep standing inside one whose hostiles out-damage its own heal, except a [[work-heavy body]] standing where it digs — on a [[post]], or at home a bare [[seat]] of a source with none; one walking anywhere takes it like any other body (#515) — with the room's safe set as its [[work area]] (its nearest tiles by the body's own walk), or, in a room with no safe tile (its towers cover it), the ground across the nearest open exit into a room a route enters whose landing no Reach covers; and no action. A body its fatigue holds takes it where a strike lands before it can step off. A fighter takes it too, but a fight Task (Guard, Defend, Assault) it holds or passes outranks it (#533). In the Safety tier. ADR-0033, ADR-0056. 🏃

### Planner
The pure step that reads a [[colony view]] and generates this tick's full Task pool from scratch, setting each entry's [[priority]] and [[capacity]] off the [[atlas]]. ADR-0052.

### Matcher
The pure step that assigns creeps to Tasks by greedy matching — the best pair of any idle body first, so a contested Task goes to the nearest body that ranks it there (#530) — remembering only current assignments between ticks (anti-thrash). It knows no kinds of Task: it compares [[priority]] and [[travel cost]] and counts holders against [[capacity]]. ADR-0052.

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
A tile on a room's border and the tile it lands you on in the neighbouring room — never a place to stay, save to be carried across it, answered off the border layer, and a crossing only when the far room's ground reaches the landing. The joint a cross-room [[walk]] is summed at along a chain of at most `Tuning.MaxHops`, and the anchor an [[outpost]]'s [[container]] pick is measured to. ADR-0036, ADR-0041, ADR-0042, ADR-0058, ADR-0059, ADR-0062.

### Orphan
A [[seam]] crossing whose landing tile has no ground of the far room's beside it, and therefore not a crossing; what makes a band directed. ADR-0062.

### Transit room
A room a [[colony]] projects only because a [[walk]] to one of its [[outpost]]s crosses it, because it is an [[evacuating room]], or because it is a child's outpost its residents watch (see [[trespasser]]): terrain, a border ring and the bodies vision finds there, but no work pooled and no `InvaderRaid` [[stand-down]] opened. ADR-0058, ADR-0065, ADR-0066.

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
The heavy-WORK [[body pattern]] cast for a [[post]]: Work up to the saturation of the rock its Post seats (at the held rate in a room this colony reserves, whatever the reservation reads that tick), one Carry and one Move — one per two Work for an [[outpost]]'s Post (#515) — cast into the vacant Posts richest first, pinned by [[travel cost]] and working in place. ADR-0006, ADR-0012, ADR-0016, ADR-0020, ADR-0021, ADR-0025, ADR-0026, ADR-0048, ADR-0053.

### Hauler unit
The repeating [Carry; Carry; Move] block hauler bodies are built from, at road parity, living in the [[withdraw]]→[[refill]] cycle. Its quota is the colony's whole haul — each source [[container]]'s round trip to the colony's sinks times its output over carry capacity — rounded up once, with each Thorium mine's haul rounded apart to a body of its own, and one body more while the **haul overflow** holds: the fill of the fullest source container whose haul prices a trip, the energy piled at its Seats counted in, smoothed over decided ticks, latched at 75% and dropped under 40% — never without a home [[storage]] with room for a load, never over a flow of zero (#508). Not the worker row's backlog term, which is construction. ADR-0012, ADR-0029, ADR-0030, ADR-0042, ADR-0049.

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
The [Claim; Move] row: one per declared [[outpost]] whose controller nobody owns and no other player holds (the Invader's hold it takes down with `attackController`, #487), sized `ceil((5000 − reservation remaining) / 600)` CLAIM parts; one per [[candidate colony]] for the [[claim]]; and one per declared [[errand]] as the [[re-claimer]]. Cast before every other row, and withheld from a room the [[guard]] row still has a gap for. ADR-0006, ADR-0042, ADR-0047, ADR-0057, ADR-0069, ADR-0072.

### Re-claimer
The [[reserver]] row's third face: the `[Claim; Move]` body hired one per declared [[errand]], which walks to the sector Reactor and holds the [[reclaim]] Task for the rest of its life, relieved with a 25-tick overlap (`Tuning.ReclaimerOverlap`) on one permanent seat. ADR-0057, ADR-0060, ADR-0069. ☢️

### Courier
The fixed `[20 Carry; 10 Move]` body that carries one `Tuning.ReactorLoad` — or the remainder, once no more ore is coming — of [[thorium]] from the home [[storage]] to the declared sector Reactor in one delivery slot; a body carrying a delivery is marked by `Facts.carryingADelivery`. ADR-0006, ADR-0067, ADR-0073.
_Avoid_: hauler, runner, delivery creep

### Guard
The `[Tough; Attack×3; Move×5; Heal]` [[body pattern]] cast on contact and never before: one per declared [[outpost]] a [[threat]] stands in (or that the [[raid log]] remembers one in) or a level-0 invader core does — the bank's biggest body, swinging at the core from its ring (#487) — and one per child's home its [[mother colony]] defends once the child is `Independent`, and never in an [[errand]] room or a home the mother still raises, which the [[ranger]] holds, two where the raid's healing outruns one block's damage, sized to win the exchange, and holding the `Guard of roomName` Task on the threats' range-1 ring at Safety priority. Its swing counts only against a raid every raider of which comes to it — its swing outdeals its gun, or it is rooted, or slower on plain — so a raid holding a shooter as quick as the guard is the [[ranger]] row's where the biggest ranger wins it (#535). A guard striking the body beside it steps onto that body's tile too, which follows it when it leaves. Holders of one Guard none of whom wins alone wait out of reach, on the rally ground, until they stand together. Any body of ours with a weapon strikes the best target in its reach whatever its Task, as the heal reflex heals. A fight begun is fought to its target's death: a holder keeps its Guard while a target of it stands in the room that it can strike, whatever the verdict, until it dies, leaves or outruns it, or the holder, out-damaged and losing, flees. What any body may strike is one diplomacy rule: never a Source Keeper nor an ally; a declared enemy's or an Invader's creep anywhere; anyone else's armed body only in a room we mine or hold. Its [[body class]] is Fighter and its quota does not decay. ADR-0003, ADR-0050, ADR-0054, ADR-0056, ADR-0072, ADR-0077, ADR-0080. ⚔️
_Avoid_: bodyguard, soldier

### Defender
The `[Move; Attack]` [[body pattern]] a home that keeps [[rampart]]s casts for itself (#504): one a raid body outside its line — the tiles a walk from the exits reaches without crossing the declared perimeter — with an ATTACK or RANGED_ATTACK in effect (see [[threat]]), or WORK beside a perimeter rampart, at most `Tuning.DefenderCap`, as many blocks as the bank buys up to `Tuning.DefenderBlocks`, cast ahead of every row and holding the economic rows until it is. It holds the Safety-tier `Defend of roomName` Task, no other body may, and its Work Area is every perimeter rampart beside the raid that no other creep of ours or a lent one stands on, else those nearest the raid; it walks round the tiles outside the line a raid melee reaches in a step and a swing, and swings only from a rampart. A raid inside the line casts none, and a defender with no line to hold flees. A body at the line it never strikes from its rampart (#546, #552) — a shooter keeping its range, a healer, a melee parked out of every rampart's swing or on the exit tile, never one walking in — is the ranger row's too, in the same `Defend`: rangers are cast for those, less the mother's residents holding the home's ground, and shoot from the perimeter ramparts within three of them; where no ranger is cast, held or lent, the defender answers them too. A body bouncing on the home's exit is at the line on the tick it is across. Counted against its own row, never the fleet. ADR-0034. 🧱
_Avoid_: guard (the outposts' row), brawler

### Ranger
The ranged [[body pattern]] an [[errand]] room is held with (#411): `[Move×3; RangedAttack×2; Heal]` a block, kept on the Reactor's ring as a standing garrison of `Tuning.RangerResidents` bodies of `Tuning.RangerResidentBlocks`, raid or not (#419), each sized up to win its raid alone when one stands. The same garrison holds the controller's ring of each child's home its [[mother colony]] raises, from the claim until the child is `Independent` (#447), with `Engine.guardCap` more on top while that home is beaten, one resident loses its raid alone and the largest ranger wins it; under an armed raid there it holds the threats' ring instead. A raid nothing of ours wins — the biggest ranger the bank buys, nor the residents standing there with those called from beside it (ADR-0085) — casts no ranger into a resident room, and the residents already there hold its safe ground (#451). Under a raid with a melee body a ranger kites: its ground is the tiles within three of its target that no melee body reaches in a step and a swing, off the exit tiles and out of dead ends; a raid it out-walks is its [[chase]] (#510). It shoots any armed hostile, rival claimer or raid's healer within three tiles — a claimer beside the controller or Reactor first, then the lowest effective hits, a healer it can break before the rest — and its heal acts beside its shot, pre-healing an adjacent fighter that stands in a Reach. One also works each [[harassment room]], at no fewer than the blocks of the floor it was cast at. Its [[body class]] is Fighter. ADR-0078, ADR-0081. 🏹
_Avoid_: archer, ranged guard

### Chase
A raid a ranged body of ours kites down (#510): every raider in the room shoots nothing and falls behind the ranger on road and plain alike — its weight, destroyed parts included, against its MOVE in effect — and the ranger's damage out-runs every heal acting there. Distance decides it and paper strength does not: a chased raid is never outmatched and opens no [[stand-down]]. In an outpost whose biggest guard body loses it, the room's Guard is a ranger's alone — one, cast for by no row — and a ranger holding the Guard of a quiet harassment room or outpost within a walk is called off it, a resident only beside its quiet home as for a [[trespasser]] (#516). The same Guard, taken by as many rangers as win it, answers a raid that does close but that the rangers standing in the room and those called from beside it win together where the guard loses (ADR-0085); and a raid worn down to what the guard row's cast would win stays with the ranger holding it. A room shut on a raid then seen kitable stays projected as a transit room while the row stands, and the chase is pooled there and nothing else. A chase in sight in a child's outpost beside a home her residents hold is the mother's too, as for a [[trespasser]] (#517). The ranger walks round what each melee reaches in a step and a swing, never the whole [[reach]], so its ring at three stays open, keeps a step and a swing off the exit tile a raid left by, and kites on ground with escape room. A body that can barely move is *stationary*: it opens no stand-down at all, and its Reach is where its fatigue lets it strike within the horizon, its swing alone between steps. ADR-0056, ADR-0078.
_Avoid_: pursuit, hunt

### Trespasser
A body of a declared enemy (`Colony.enemies`, user 2026-10-05) with no weapon in effect (see [[threat]]) and a WORK, CARRY or CLAIM part, or a weapon on paper all shot off (#528): it strikes nothing back. Standing in a room this colony mines — its home or a room of its sources — or on the exit row into one (#516); and for a [[mother colony]], in a child's outpost bordering a home her residents hold, which she projects as a [[transit room]] where the child's own scan works it, and where only a ranger takes the kill (#517). A neutral player's body never is, nor a bare scout. Where no armed [[threat]], held exit, level-0 core, [[chase]] or ranger row answers first, it pools the room's Guard as a kill — a rung up, cast for only where the room is guarded — taken by a [[guard]] body or a [[ranger]], a quiet holder called to it as to a chase. Each trespasser is answered by a row that can strike it (#550): the guard the ones its swing catches — a digger on its seat, a CLAIM body beside its controller, one slower than it — and the ranger the rest, a hauler at the guard's pace, which the guard answers too until a ranger holds the kill. A CLAIM body is struck first, behind any raid. Defence first is rank: a raid's Guard — an outpost raid's, or a garrison's or a chase's where a [[threat]] stands — ranks a rung over the kill, so only a body the raid leaves free takes it (#528). A resident is lent only to a room bordering its quiet home — one whose towers and the bodies a lend leaves there beat every raider but the kill's own that can reach striking range of it before the garrison is back, a room's side each way at a tile a tick (#531) —; no ranger holds a kill beside a home that is not, which recalls the garrison at once. A hauler is waited for at its store, never chased, while no trespasser the guard catches stands in the room; beside one, every trespasser's own tiles are the ground. The kill ends when the trespasser dies or leaves. ADR-0056.
_Avoid_: thief, intruder

### Answerer
The one colony that answers a raided room this tick (the world layer's E1): among the colonies whose row wins the room, else those of them whose biggest ranger kites it, else those whose rangers are among the fewest that win it — one whose row casts for the room first —, the fewest crossings from the room, then the largest bank, then by home name. Chosen with the room's one verdict (`Facts.judge`). Where the verdict is a `Win` outside our homes, it alone casts for the room (`Brief.Answerer`), and it and every colony the `Win` counts — whose row wins it, or whose rangers are among the fewest that win it (`Brief.Counted`) — pool the room's Guard, each sending its own bodies; no other colony pools it. A `Kite`, a `Lose` or a home of ours binds no answerer, and every colony that claims the room pools it and casts for it. ADR-0086.

### Brief
The per-tick configuration behind one combat Task (#518): which hostiles or structures to hit, the bodies fit for it, the ground they hit from, its rung, capacity, row ask (`RowAsk`: which fighting row, how many bodies, what blocks) and win verdict, derived each tick and never stored. Its `Duty` says which Task: a room's Guard, of a flavour (`BriefKind`: outpost raid, level-0 core, held exit, resident, harassment, [[chase]], [[trespasser]]'s kill) classified once per room; the home's Defend; or an [[assault]], whose squad launches through `Formation`. Each tick `Threats.Briefs` holds one per combat Task a source claims, keyed by the Task, read through `Threats.briefOf` (a room's Guard) and `Threats.briefFor`; a raided room's Guard names its [[answerer]], the one colony that casts for it. ADR-0085.
_Avoid_: engagement

### Squad
A fixed composition of fighting bodies an [[assault]] declares and casts whole (#490): sappers, a probe and medics, each one fixed body. Each cast's role is read off its name, each cast counted against its own row and never the fleet the workforce target counts. The Fight squad a raid in a resident room was once answered with (brawler, medic, kiter) is retired (ADR 0090, #564). 🛡️
_Avoid_: team, group, wave

### Fight
A resident room's fight record (`RaidState.Fought`, the world danger log's `fight` part): opened on a tick a raid the room's residents lose stands there — one body of the garrison's size, and the rangers standing there priced together — moved on by every tick a raid stands there after, and dropped `Tuning.FightHoldTicks` after its raid was last seen. A raid seen dead in the room, every body of its last sighting, drops the record at once: the hold is for a raid that left. What it holds is the home's rampart doors private. The `Fight` Task a squad once held off it is retired (ADR 0090, #564).
_Avoid_: attack, assault, engagement

### Assault
A rival's room a [[colony]] declares to send a [[squad]] against (#490): breach tiles, a squad of rows (by default two sappers — `25M 25W` — and two medics), a mode (`Provoke` holds at the breach, `Strike` goes on to the towers and the spawns), the targets past the breach where a human names them (#496) — down, the assault is **taken** (`RaidState.Taken`): its squad leaves and nothing is cast for it until one stands again — the room beside it the squad enters from where a human names one, and whether a human has switched it on. A **probe** squad is one cheap sapper (#493) sent to learn what raises the room's safe mode: it never falls back on its hits, leaves at once when safe mode shows, and its log — first dismantle, dismantles before safe mode, the tick it showed, its fate — is the Raid log's (`RaidState.Probes`). Its Safety-tier `Assault of roomName` Task musters at home, beside the exit its chain leaves by and off the ground beside the spawns, extensions and stores, and launches once every slot is held and its casts stand together, or once its casts stand as one file, each within two of another, and stays launched while every cast is out of home; the squad falls back to rally when a cast drops under half its hits or safe mode runs in the room, seen or remembered. A `Provoke` is pooled whenever it is on, and casts nothing while safe mode runs in its room; a `Strike` only inside its **window** — the rival's controllers as remembered (`RivalSafeMode.barredUntil`) say they cannot raise safe mode in the room until past the squad's casts and walk. ADR-0084.
_Avoid_: siege, raid, strike (a mode, not the whole)

### Miner
The store-less [[body pattern]] — Work up to twenty with one Move per `Tuning.MinerWorkPerMove` — that stands on the mineral [[container]] and drops its [[thorium]] into it, one per deposit the colony can dig (owned room, extractor and container standing), cast behind the [[hauler unit]] and ahead of the [[upgrader]]. ADR-0006, ADR-0016, ADR-0057. ⛏
_Avoid_: driller, extractor (which is the structure), remote miner

### Fatigue parity
The body-generation invariant that a worker body padded beyond whole [[worker unit]]s never moves slower than the pure-unit body, empty or loaded; the remainder buys Carry as parity allows, then Move, never Work. ADR-0003.

### Layout
The deterministic full structure plan computed whole from the [[atlas]] and never persisted: the clustered kinds — the spawns beyond the first among them — by one ordering (the oldest spawn's checkerboard, nearest first, [[working ground]] and [[link footing]]s held out) placed at `controller.Level + Tuning.HorizonLookahead` and reserved at the allowance ceiling, plus the [[storage]], the [[link footing]]s, the [[container]]s, the [[trunk]]s and the [[rampart]]s. Recomputed only when its [[census signature]] changes; road sites are placed from RCL3 up. ADR-0011, ADR-0017, ADR-0022, ADR-0027, ADR-0034, ADR-0040, ADR-0063, ADR-0064.

### Trunk
A paved line in the [[layout]] — each source to the controller and to the spawn, plus the Upgrade [[work area]]'s swamps — priced on raw terrain and routed around the reserved tiles; also the line a human paves in an [[outpost]], a surplus Build there with a rung of its own: under every home Build while the home builds, after it, the colony's one lead site over the controller for one body and the rest on the tier. A trunk that routes nothing is recorded on the [[layout record]]. The **outpost road** is the Layout's one line past the home: from the [[storage]] or the oldest spawn to a tile beside each outpost source's [[container]], along the cheapest chain a fighter walks, any road already standing priced as paved, given new sites only where the [[hauler unit]] ticks it saves, at the rate the reserver row holds, repay its build, decay and wear within `Tuning.RoadPayback`, placed only in rooms the colony works and nobody else holds, `Tuning.OutpostRoadSites` sites at a time and never while the home builds or the player holds `Tuning.OutpostRoadSiteCeiling` sites; every road already standing, hand-laid or not, is kept and repaired. ADR-0011, ADR-0042, ADR-0087.

### Working ground
The tiles the colony works from — every source's [[seat]]s, every [[thorium]] deposit's tile and seats, and at home the controller's Upgrade [[work area]] — excluded from the [[layout]]'s clustered ordering and the base of the [[idle ground]]. ADR-0022, ADR-0034, ADR-0042, ADR-0057.

### Idle ground
Where standing idle blocks somebody: the [[working ground]] plus the walkable range-1 ring of each store — the set a body with no [[task]] heads its first step off. ADR-0001, ADR-0022.

### Storage
The colony's stock, placed on the cluster's first pick from RCL4: the deepest [[refill]] target, a [[withdraw]] source pooled only while another sink is hungry (ranked with the containers while the colony is starved, and only for the emergency sinks under the [[emergency reserve]]), the [[thorium]] sink, and a [[keep]] structure. ADR-0022, ADR-0023, ADR-0034, ADR-0057, ADR-0071.

### Emergency reserve
The [[storage]] energy a colony keeps for spawning and defence, by its controller level (`Tuning.EnergyReserves`; none below RCL4 or with no Storage of its own). A colony under it stays under until it banks one hauler load past it; meanwhile the stock feeds only the [[refill cluster]], the towers and a struck or dying [[rampart]]'s [[repair]], and income banks before anything deeper. ADR-0023.
_Avoid_: floor (the upgrader row's own), buffer

### Thorium
The season's scoring resource (`Resource.Thorium`): a finite deposit read off `FIND_MINERALS`, dug by the [[miner]] through an extractor from `Tuning.ExtractorLevel`, moved by the [[hauler unit]] row at the [[storage]]'s tiers, banked in the [[storage]] and carried to the Reactor by the [[courier]]. A body carries one resource at a time. ADR-0057, ADR-0060, ADR-0067.
_Avoid_: mineral (the engine's word for the object), ore (the room's own, below)

### Ore
A room's ordinary mineral — H, O, U, K, L, Z or X (`Resource.Ore`) — dug for the labs (ADR-0089). A home digs it only once no [[thorium]] deposit stands in the room (the room's one extractor is the Thorium's while any does; one left on a mined-out Thorium tile is destroyed once its container is empty, and the ore's placed) and only while a terminal stands there: by the same [[miner]] into its own mineral [[container]], hauled by the [[hauler unit]] row into the home terminal while that terminal holds under `Tuning.OreTarget` and the [[storage]] stands `Tuning.OreEnergyMargin` over the [[emergency reserve]] — a dig left shut opens only `Tuning.OreTargetBand` under the target and `Tuning.OreEnergyBand` over the margin (`RaidState.DigOpen`; a latch never written reads open, so a deploy shuts no running dig). An [[errand]] room's ore is dug the same way into the home terminal, held or not, while no [[stand-down]] shuts the room (the Reactor's utrium in W15S25, through its neutral extractor), its container seated as an [[outpost]]'s; never its Thorium. A held errand's room is garrisoned by nobody, so any raid there stands it down, and its walk is crossed for walking alone (no sweep). The room's sources are dug only by the bodies building or repairing its mine, where they stand. An [[ally]]'s tombstone, and every drop in a room an ally holds, is never ours to take. An ore miner is cast behind a short [[courier]] row. A body holding ore takes no energy intake. A terminal holding ore the [[lab cluster]] lacks ships it there, no more than it lacks, and keeps that send's fee and a hundred over in energy for it.
_Avoid_: mineral, compound (a lab's product)

### Compound
What the [[lab cluster]] makes out of [[ore]] (`Resource.Compound`): the UH line alone — OH (H and O, boosts nothing), UH (U and H, ATTACK ×2) and UH2O (UH and OH, ATTACK ×3). Held in a store beside the ore (`SpatialInfo.Minerals`); a body holding one takes no energy intake. ADR-0089.
_Avoid_: boost (what a compound gives a part), mineral

### Lab cluster
The one home whose labs make [[compound]]s, and the stock of each it makes to (`Colony.labCluster`: W13S28, UH2O 3,000). Its labs are placed one site at a time on a stamp as near the terminal as the plan leaves room — two reagent labs, every other within range 2 of both — and placed, filled and run only while the home's [[storage]] holds `Tuning.LabEnergyFloor`, or `Tuning.LabEnergyBand` under it while a lab still holds a mineral. Each tick the plan is derived whole from what the terminal and the labs hold (`Labs.planFor`): the next reaction toward the first short target, a short reagent compound made first; reagent labs filled from the terminal and drained of anything else; the output drained of anything else and of its product at half full; nothing remembered. Every other colony's terminal ships it the ore it lacks. A reaction is asked on a decided tick when the output lab is off its cooldown or one tick from it, and replayed on the [[light tick]]. ADR-0089.
_Avoid_: labs (the structures), reactor (the season's)

### Keep
The structures worth defending — the spawn, every tower, the [[storage]] and the terminal (#422): each ramparted, repaired to full, and a reason to fire the [[safe-mode reflex]] when damaged, from `Independent` up. ADR-0034.
_Avoid_: base, core

### Rampart
The walkable defensive structure the [[layout]] places over every standing [[keep]] structure, every [[post]] a [[container]] stands on and every tile of the colony's declared perimeter (#446), kept above a floor of hits by [[repair]] — `Tuning.RampartFloor`, or the floor the colony declares for that tile (`Colony.RampartFloors`) where a choke has to outlast a raid its towers cannot hold — each capped at `Tuning.BuildRampartFloor` while the home's extension or Storage sites stand and no raider has stood at home for `Tuning.BuildFloorRaidTicks` (#526); a creep on its own rampart is in no [[reach]]. ADR-0034.

### Link footing
A tile the [[layout]] reserves for a link beside each planned source [[container]], the controller container and the [[storage]]: off every [[trunk]] and other footing, nearest the spawn, the one structure footing allowed on [[working ground]], and unfilled at RCL5 because the hauler row is already at its floor. A footing the fold finds no tile for is recorded on the [[layout record]]. ADR-0022, ADR-0027, ADR-0035, ADR-0038, ADR-0042, ADR-0049.

### Colony
A [[home room]] and the [[outpost]]s worked from it — the unit the decision layer and the declaration (`{ Home; Outposts; Mother }`) are written in, declared and never discovered. A **living** colony owns its home and holds a spawn there, and is what `decide` runs over, once each per tick; a creep belongs to the colony whose spawn cast it, with adoption for a creep standing in a room only another colony projects. ADR-0047, ADR-0052.
_Avoid_: base, empire, room group

### Stage
Where a [[colony]] stands in its life, derived every tick by `Colony.stageOf` off ownership, a spawn of ours and the controller's level: [[nursery]], bootstrapping ([[bootstrap window]]), weaning (a child at exactly RCL3 whose own tower has not yet stood full: its own rules read independent, its mother still lends; #445) or independent. A room that is no colony of ours has no stage. ADR-0052.
_Avoid_: phase, generation, tier

### Candidate colony
A declared [[colony]] whose home room this colony does not own yet, projected and worked as an [[outpost]] of the mother until the [[claim]] lands. ADR-0047.

### Mother colony
The declared [[colony]] that raises another: it projects, mines and reserves the child's room until the child stands on its own, first through its outpost list and then through the child's `Mother` field, and takes a lost child back for a [[claim]]. While it raises the child it keeps a [[ranger]] garrison in the child's home. Past that it projects the child's home, as a [[transit room]], while the home is beaten — an armed raid out-heals its towers and safe mode is off — and while a [[guard]] it sends there still stands on the way. ADR-0047, ADR-0080.
_Avoid_: parent colony, host colony, parent (as the field)

### Nursery
The first [[stage]] of a child [[colony]]'s life — claimed by us with no spawn of ours standing yet — still projected by its [[mother colony]] as an [[outpost]], with every site in it feeding-tier [[build]] outside the outpost builders' budget — except at controller level 1, where the mother pools its Upgrade on the Feeding tier and its sites wait (#449) — and its [[safe-mode reflex]] fired by the mother. ADR-0047, ADR-0052.
_Avoid_: child colony (as the state), colony under construction

### Bootstrap window
The second [[stage]] of a child [[colony]]'s life, from the tick a spawn stands in it until RCL3, in which its [[mother colony]] still projects its controller, sites and spawn, pools its Upgrade and Builds, hires [[pioneer]]s and lends a [[ferry]]. ADR-0047, ADR-0052.
_Avoid_: bootstrap phase, adolescence, borrowing period

### Borrowed work
What one [[colony]] may take of another's, named on its [[colony view]] and bounded: the home rooms of the children it is raising (their Upgrade and Builds, and a bootstrapping child's [[buffer]] for the [[ferry]]) and of the children it has lost (a [[claim]]), and a beaten child's home it defends (a Guard and nothing else). ADR-0047, ADR-0052, ADR-0080.
_Avoid_: shared work, cross-colony pool

### Foreign bodies
Where the creeps a [[colony]] does not hold stand in the rooms it works (`ColonyView.Foreign`): priced around by the [[occupancy surcharge]] and treated as walls by a colony arbitrating alone. ADR-0052, ADR-0070. A hostile's or ally's body counts as well, rooted or not (#536), and one that stepped since the full tick before also stands on the tile it left (`World.HostileSteps`, #551): a body swaying between two tiles enters the one it is not on.
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
A room a [[colony]] declares because it must walk a body there and act on one named object in it — a room name, the object's id and tile, and nothing else — so a sector centre with no controller can be declared. Nothing else in it is pooled but its [[ore]] mine and its garrison of [[ranger]]s, kept while there is ore to burn in it (#420), which fights a raid where the ranger's reach wins; a raid that reach loses to makes it a [[stand-down]]. ADR-0060, ADR-0077, ADR-0078.
_Avoid_: goal, mission, remote target

### Evacuating room
A [[stand-down]] room of a [[colony]]'s — outpost, errand or salvage — or a [[transit room]] of the walk home from it, projected as a transit room only while a body of that colony stands on that walk (#512): every idle body standing in it walks home round the [[reach]] — and, arrived, on in to the [[shelter]] rather than idle on the exit — nothing is pooled there (not even ore to sweep), and an anchor in it is out of the anchor row's census. A body standing there stays its caster's whoever else projects the room. Not the **stranded** anchor of #506, which is one off every Post that cannot reach a vacant one in time, or one the projection places nowhere. A shut [[harassment room]] is no evacuating room: it stays projected for its ranger regardless. Any other room a body of the colony stands in that its scan set does not hold — a stronghold's room no chain enters, a room a hauler strayed into — is one too, with the rooms beside it and its walk home, held for that body alone: only a chain that starts in one enters them, and the plan signs none (#570). ADR-0043.
_Avoid_: stranded room

### Salvage
A room a [[colony]] declares to take down the structures of ours still standing there (spawn, extension, tower, storage, terminal, extractor, link, rampart), with a Work-and-Move dismantler and no Carry. Its targets are whatever vision shows standing, so the declaration goes inert when the last one falls. ADR-0079.
_Avoid_: demolition, cleanup

### Harassment room
An enemy's remote, declared once for the whole bot (`Colony.harass`) with the one player whose creeps and containers are targets there. Each tick it is cast, among the living [[colony]]s that buy its floor and whose chain reaches it inside the hop budget, by the one fewest crossings away, then the one with the larger bank; while none does, it is refused. Its floor is the ranger blocks its declaration names (`Blocks`), for a room a human knows holds less than the escort the default is sized for, else `Tuning.HarassBlocks` (#457); armed defenders that turn up later are the [[stand-down]]'s to answer. A declaration may name `Via` rooms, a detour round a walled border that the caster projects beside its shortest-walk rectangle. That colony keeps one [[ranger]] there, which waits within reach of the tiles the enemy's creeps must stop at (beside the source and the controller) and shoots them, unarmed ones included, chasing none, and a dismantler takes the enemy's containers down. An ally's creep, a third player's and a Source Keeper are never unarmed targets. A squad the ranger cannot beat makes it a [[stand-down]]. ADR-0081.
_Avoid_: raid target, siege, enemy outpost

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
The colony's episodic record of [[hostile]] presence in the rooms it works, under `Memory.fabot.observe.colonies.<home>.raids` and read with `observe.mjs raids`: a ring of raid episodes (window, the worked rooms the raid stood in, closest approach, losses, damage, closed by the quiet gap; a room the colony only crosses opens none), and the last tick a raider stood in the home (#526). Who the raid was is the world's [[raid roster]], and the [[stand-down]] rows, rival latches, holds, threat memories, fight records, assault probes and taken rooms are the world's [[danger log]] (ADR-0086's S2 and S5). Beside them it carries two per-colony latches that are not about raids: the [[emergency reserve]]'s (#505) and the [[hauler unit]]'s haul overflow reading (#508). A record to be read; the stand-down gate is its one reader. ADR-0028, ADR-0034, ADR-0041, ADR-0043, ADR-0047, ADR-0056, ADR-0065, ADR-0077.

### Danger log
The world's record of the danger in each room, one record a room under `Memory.fabot.observe.world.danger` and read with `observe.mjs outposts`: its [[stand-down]] row (the deadline, its basis, a [[stronghold]] seen, the raid bodies it waits on, a chase), somebody else's hold on its controller, the memory of an armed [[threat]] in it, its exit watch, a resident room's [[fight]] record, and an [[assault]] room's taken tick and last probe. The rival latches (since, last look) sit beside each rival room's owner under `observe.rooms.rivals`. Folded once a tick through every colony in order, each moving only the rooms it saw or prices; every colony's stand-down gate is its projection of the one log. ADR-0086.

### Raid roster
The world's record of who raided: every [[hostile]] our vision saw, room by room and then by id, under `Memory.fabot.observe.world.roster`, with its owner, its body as first seen there, the ticks first and last seen there, the tick its life runs out on, and the tick its tombstone was first seen. A body that fell before its life ran out was killed; one that fell on it died of age (beside us). Allies and Source Keepers are not enrolled. A [[raid log]] episode's roster is these rows in the rooms it names, inside its window. Past the quiet gap a room keeps only rows an episode reads that could hurt, heal or fell, the latest 50. ADR-0086.

### Layout record
The colony-level channel carrying what the [[layout]] could not deliver this tick, under `Memory.fabot.observe.colonies.<home>.layout` and read with `observe.mjs layout`: the [[link footing]]s with no tile, the [[trunk]]s with no path, the [[container]] picks deferred to a standing container, the declarations refused for want of a chain, and the [[outpost]] sources whose every [[seat]] another construction site holds, each Seat under ours or a rival's; beside them each outpost road's state (planned, unpaid, unrouted, no container, dark) and its tiles standing, pending and skipped in a room it crosses or another player holds. Written every tick, empty lists included; nothing reacts to it. ADR-0011, ADR-0017, ADR-0027, ADR-0035, ADR-0040, ADR-0047.

### CPU line
The per-tick record of what a tick cost, flat and keyed by tick under `Memory.fabot.observe`, read with `observe.mjs cpu`: the milliseconds spent, split at the loop's phase boundaries, plus the count of accepted [[intent]]s. Measured, never budgeted — nothing in the bot reads it back. ADR-0041, ADR-0047.

### Breach log
The colony-level channel carrying what is broken right now, under `Memory.fabot.observe.colonies.<home>.breaches` and read with `observe.mjs breaches`: one row per live invariant violation in the [[thorium]] programme, with its room, object id, the number that makes it actionable and how long it has stood. Measured against what the colony can see; nothing reacts to it. ADR-0004, ADR-0035.

### Chat bubble
The glyph an assigned creep says over its head each tick, one per [[task]] (⛏ Harvest · 📥 Withdraw · 🧲 Pickup · 🔋 Refill · 🔨 Build · 🔧 Repair · ⚡ Upgrade · 🚩 Reserve · 🏴 Claim · 🏃 Flee · ⚔️ Guard). Observability only; unassigned creeps show nothing.

### Safe-mode reflex
The colony reflex that emits `ActivateSafeMode` when a CLAIM-part [[hostile]] stands within range 3 of the home controller, when a [[keep]] structure is below full hits with a hostile in the [[home room]], or — with no tower standing — on the first armed hostile unless one armed body of ours standing there, whoever holds it, wins the exchange alone (#448); gated only on stock remaining and safe mode not running. A mother runs it over each [[nursery]] she raises too (#449). ADR-0007, ADR-0015, ADR-0034.

### Signature reflex
The reflex that writes `Colony.signature`, a human's line, onto any controller the projection places whose sign is not already that line, by any creep of ours within range 1 of it. A reflex and never a [[task]]: it sends nobody anywhere.
_Avoid_: room sign

### Pickup reflex
The colony reflex that emits a pickup Intent for every creep with free capacity, no [[thorium]] aboard and no ore-bound Task held (#314) standing within range 1 of a dropped energy pile (`Atlas.droppedEnergyIn`), paired one room at a time and deduplicated against the [[pickup]] Task's own act. No movement, no matching, no threshold. ADR-0041, ADR-0057.

### Fire reflex
The colony reflex that has every tower shoot the [[hostile]] nearest to itself among those worth a shot, each tick one stands in the [[home room]], per tower and with no focus fire. Worth one: an urgent hostile — a claimer, or one whose weapon reaches our [[keep]] or a creep of ours not standing on our own rampart — shot from any energy; or one the towers' energy over `Tuning.TowerRepairReserve` can kill through the heal reaching it (a player's healer on the exit counted at half, as it is carried out every other tick). With none worth a shot, a tower over its reserve repairs the [[rampart]] under attack. ADR-0014.

### Ally
A player we have agreed with, by username, declared by hand in `Colony.allies` (#412). Their creeps are no [[hostile]], so nothing fires on them, flees them or stands down for them. On a Reactor they hold with Thorium in it the burn is theirs: no load is drawn and no [[reclaim]] is pooled or cast; the flag is taken only once their store is empty, with no timed handover (#413, #415).
_Avoid_: friend, alliance

### Hostile
A hostile creep as the [[world]] projects it — id, owner, [[room position]] and body parts — swept from every room the colony works and can see, less every [[ally]]'s; out of the [[spatial projection]], gating Tasks only through a [[threat]]'s [[reach]]. An invader core is a structure and rides a field of its own. ADR-0007, ADR-0014, ADR-0028, ADR-0033, ADR-0041, ADR-0043, ADR-0052.

### Threat
A [[hostile]] fighting with an ATTACK or RANGED_ATTACK part, read off the parts and never the owner; under safe mode in a room we own nothing is a Threat. Every question about a hostile's parts reads them *in effect*: the parts still acting and those the live HEAL reaching it restores inside the *reaction window* — the time our answering body needs to close and strike, half a room's side at a tile a tick — each healer's heal spent on one body a tick, the one in its reach short of the most hits; so a body whose weapons are shot off is no Threat until healing that brings them back in time stands by it, a dying healer barely restores, and a whole one brings back the paper (#531). Only a Threat has a [[reach]]. A kill admits only a body that can strike every target it counts: one that does not outrun it — strictly quicker on road, plain and swamp alike — or that is rooted, a worker on its dig seat, or an Invader penned in its room. A raider that outruns the guard is held off the work, the guard standing beside the room's Posts, not chased. ADR-0033, ADR-0085.
_Avoid_: enemy, attacker

### Reach
The tiles a body of ours stands on that a [[threat]] can strike within the *reach horizon* (`Tuning.ReachHorizon`, two ticks: this full tick and the light one after it) — read off the room's hostile maps, which time each hostile by its pace and its fatigue over the ground and carry it across an exit onto the next room's landing (#514) — less the tiles under our standing [[rampart]]s, and filed by the room each tile lies in. A room's hostile towers (a [[stronghold]]'s, or a player's in the room it owns) strike every tile of it now (#566). It subtracts from every Task's [[work area]], is what [[flee]] is applicable inside of, and holds the spawn while a tile beside it lies in one. A walk round it also goes round the tiles a hostile strikes before the walker could be on them and gone. ADR-0033, ADR-0041.
_Avoid_: danger zone, threat radius

### Shelter
The home's ground every idle body of ours at home but a Fighter walks in to and stays on while an armed [[threat]] stands in a room next door, or a raid [[stand-down]] of this colony's runs there with nothing in sight (#514): the walkable tiles behind the declared perimeter, neither outside the line nor on it, else those five or more from every exit tile, less the home's [[reach]] and the tiles beside a spawn, which a cast needs free. A body steps in only to the nearest such tile, and one already on it does not move. The walk goes round the Reach, a body already standing in one (a [[work-heavy body]] on its [[post]], which has no [[flee]]) stepping on inward through it; a body on it steps nowhere off it. Read each tick off where the body stands, so a walk begun on an exit runs until it arrives. None under the home's own safe mode, and nothing laid on a tick with neither. ADR-0033.

### Keeper margin
The tiles masked out of a Source Keeper room's walkable ground within `Tuning.keeperMargin` (six, derived) of every rock a keeper is pinned to, and one tile less of every lair it is cast on, declared by room name and applied to the raw ground and the border ring before any query reads them; a keeper room with none declared is linked to nothing. The hostile list is untouched; the ground is. ADR-0004, ADR-0060, ADR-0062.
_Avoid_: keeper radius, SK exclusion

### Downgrade deadline
The hard floor on the controller's downgrade timer, half the level's full timer, inside which Upgrade outranks even the feeding tier so the [[safe-mode reflex]] stays fireable. ADR-0007.

### Stand-down
An [[outpost]] or [[errand]] withdrawn from — out of the [[spatial projection]], so nothing pools, counts or walks there, except that while a body of the colony stands in it the room is an [[evacuating room]] its bodies walk home from — until a tick read off the threat, one clock per threat:
- a [[stronghold]]'s core: its collapse (a level-0 core opens none: a guard kills it, #487), or 2,500 ticks where none can be read;
- a rival's reservation: its end;
- a raid (bodies with an ATTACK, RANGED_ATTACK or WORK part in effect) nothing of ours wins — neither the biggest body its casters' row casts (the colonies whose rows cast for it), nor a ranger of theirs kiting it ([[chase]]), nor the rangers standing in it with those called from beside it, of every colony that sees the raid and would send them, each lent by its own colony's rule (the room's one verdict, priced once a tick and read by every colony, ADR-0086): its longest remaining life; a body that can barely move opens none (#510);
- a player's raid alone in an outpost: at most `Tuning.PlayerRaidHold` (200) past its first sighting, never short of a rival's hold (#513);
- a squad in a [[harassment room]]: fifty ticks past its last sighting;
- any raid, sooner: the tick every body is seen gone — dead, disarmed, or a player's in another room the colony projects (#507).

A rival's ownership is clockless and re-looked at every `Tuning.RivalRecheck`. Recorded in the [[danger log]], one row a room for every colony, so a room is shut, or impassable, for each colony alike; each colony's stand-down is its projection of that log onto the rooms it works. Not a route lock except for a [[stronghold]], which is impassable. ADR-0043, ADR-0065, ADR-0066, ADR-0074, ADR-0077, ADR-0086.

### Stronghold
An invader core of level 1 or more — towers under ramparts and a garrison — whose room is `StandDown.Impassable` and taken out of every chain until its collapse timer runs out. ADR-0074.

### Disaster fallback
The zero-creep spawning rule: an empty colony spawns bare [[worker unit]]s from whatever [[room energy]] is banked right now, ignoring the remainder — time-to-first-creep outranks spending the bank. ADR-0006.

### Supply floor
A spawning row that is a floor and not a quota: a colony holding no living body that can put energy into an extension casts one hauler in front of every row, sized from the [[room energy]] banked right now. ADR-0050.

### Harvest floor
The [[supply floor]]'s twin for income (#460): a colony with a Post, no [[anchor]] or miner standing or casting, and extensions short of full casts one Anchor in front of every row, sized from the [[room energy]] banked right now. ADR-0050.

## Avoided terms

- **Role** — creeps are not born with roles; work is Task-based. Don't reintroduce role-based vocabulary.
