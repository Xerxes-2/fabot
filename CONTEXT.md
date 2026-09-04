# fabot — Domain Glossary

Screeps seasonal-server bot, written in F# and compiled to JS via Fable.

## Terms

### Snapshot
The immutable projection of the current tick's game state that the decision layer reads. Contains only the fields decisions need — never the full game object graph.

### Intent
A single described action the decision layer wants performed this tick (e.g. "creep X harvests source Y"). Intents are data; they do not touch the Screeps API.

### Executor
The thin imperative shell that turns Intents into Screeps API calls. The only layer allowed to act on the game (mutating API calls); Snapshot may call read-only game methods to build its projection.

### Task
A unit of work in the task pool (e.g. "deliver 300 energy to spawn"). Creeps are interchangeable executors that get matched to Tasks; a creep has no fixed role.

### Harvest
The Task of digging energy out of a source. Pooled for every placed source, stocked or drained, and judged at [[arrival]] (ADR 0013, revised by ADR 0025): a drained source's Harvest is applicable to a creep only when its [[walk]] covers the [[restock]] wait — so a creep already beside a dry rock is released, exactly as ADR 0013 released it, while a distant one is dispatched the tick its walk equals the wait. One exemption, on ADR 0024's condition: a [[work-heavy body]] standing on a built source [[container]] keeps Harvest through the window — that tile is its job. The Emitter issues no dig while the source is drained. Feeding-tier intake beside [[withdraw]]; capped by the source's [[seat]] count, and for a [[work-heavy body]] by its [[post]] count as well (ADR 0024). Applicability: a Work part and free capacity — widened for a full **work-heavy** creep standing on that source's built [[container]], whose overflow the engine catches (ADR 0012, narrowed to the garrisoning body by ADR 0024: a light body's full store ends its dig wherever it stands, or it would hold the Post for life — never inapplicable, so never released). For a body with more Work than Move parts the [[work area]] is that source's [[post]]s when it has any (ADR 0020): a heavy body digs from the tile that catches its overflow or lets it upgrade in place, and travel cost walks it there — it does not dig, and so does not fill, from a Seat that would strand it.

### Refill
The Task of delivering energy to any energy-hungry structure — spawn, extension, tower, or the controller [[container]]; the Planner filters by free capacity. One generalized Task, but rank layers by target (ADR 0010): spawn-feeding Refill is feeding-tier work, tower Refill is surplus-tier — the colony feeds its own reproduction before its guns — and controller-container Refill sits one tier deeper still, below every surplus Task (ADR 0012): a full creep beside the buffer sinks its load into the controller rather than dumping it back into the container it just drew from, so the buffer is filled by bodies with no surplus work of their own — the hauler row's whole life. The [[storage]] sits one tier deeper still (ADR 0023): the place surplus goes when even the upgrade buffer is full.

### Withdraw
The Task of taking energy out of a stocked [[container]] (ADR 0012) or, one tier lower and only while some other sink is hungry, out of the [[storage]] (ADR 0023). Feeding-tier intake beside Harvest: an empty creep's choice between digging and collecting is made by [[travel cost]], not by rule — for bodies that are not Work-heavy. Applicability: a Carry part, free capacity, and no more Work than Move parts (ADR 0016) — a heavy-Work body's intake is digging, so a stocked container never outbids an unmanned [[post]] — plus, on the controller [[container]] alone, a Work part (ADR 0019): a body that can spend nothing at the controller only sends the buffer's energy back the way it came, and with every other sink full that same container is its only [[refill]] target, so the pair cycled a hauler in and out of one store tick after tick. The intake half of the haul cycle — a filled hauler loses Withdraw applicability and rematches to [[refill]], the same emergent alternation as every other Task pair.

### Build
The Task of spending carried energy into a construction site. Surplus work: same rank tier as Upgrade, below Harvest and spawn-feeding [[refill]] — the economy is fed before anything is constructed.

### Repair
The Task of restoring a decaying structure's hits (ADR 0010) — created when a repairable structure falls below half hits, gone when it is whole. Surplus-tier like Build, same applicability (Work part, carried energy) and the same range. Only repairable kinds ([[trunk]] roads today, containers when they enter) put hits in the [[spatial projection]].

### Planner
The pure step that reads a Snapshot and generates this tick's full Task pool. Runs every tick from scratch — Tasks are never persisted.

### Matcher
The pure step that assigns creeps to Tasks (greedy matching): Assignments in, Assignments out. Current assignments are the only thing remembered between ticks (anti-thrash).

### Emitter
The pure step that turns the tick's assigned Tasks into each assigned creep's action Intent and [[chat bubble]]. Judges actions from tick-start geometry — it consults the same [[atlas]] as the Matcher and Resolver, never resolved positions.

### Seat
A walkable tile adjacent to a source. The capacity unit of Harvest: a source supports at most as many concurrent harvesters as it has Seats — beside which a [[work-heavy body]] is capped by the [[post]] count, the standing room it actually has (ADR 0024).

### Dual Seat
A [[seat]] that also lies inside the controller's Upgrade [[work area]]. A creep standing on one can alternate Harvest and Upgrade without ever moving. One of the two kinds of [[post]]. Derived by the [[atlas]] each tick, never persisted.

### Post
A tile worth garrisoning with a heavy-WORK body (ADR 0012): a [[dual seat]], or the [[seat]] under a source [[container]]. The capacity unit of the [[anchor]] quota — one Anchor per Post — and, since ADR 0024, of Harvest itself for a [[work-heavy body]]: a source admits as many heavy harvesters as it has Posts, so the [[seat]] count never piles two Anchors onto one tile. The one tile such a body may also garrison past a full store. Also the only footing a [[work-heavy body]] harvests that source from, when the source has one (ADR 0020) — the narrowing that makes [[travel cost]] pin an [[anchor]] to a Post instead of leaving it wherever it happened to land. Derived by the [[atlas]] each tick, never persisted.

### Container
A container structure as the [[layout]] places it (ADR 0012): one **source container** per source, on the [[seat]] nearest that source's [[trunk]]; one **controller container** on a buildable work-area tile adjacent to a trunk — the upgrade buffer that lets upgraders work standing still. A repairable kind: its hits enter the [[spatial projection]] and the Repair pool (ADR 0010). A stocked container is a [[withdraw]] target; the controller container is also a [[refill]] target, on the deepest tier but the [[storage]]'s (ADR 0023) — source containers never are.

### Work Area
The set of tiles a creep may stand on while performing its current Task: ordinarily the passable tiles within the action's range of the target, narrowed for a [[work-heavy body]] harvesting a source that has [[post]]s to that source's Posts alone (ADR 0020). Body-aware, not a pure fact about the Task — the same Harvest offers a light body every [[seat]] and a heavy one only the tile worth garrisoning. A creep acts only from inside it; an unreachable Work Area makes the Task inapplicable, narrowed or not. Derived fresh each tick, never persisted.

### Move Intent
A creep's movement desire for one tick: candidate standing tiles plus a priority. Input to the [[resolver]] — not an Intent; the Resolver's output (a single-step move) is what becomes an Intent.

### Resolver
The pure step that arbitrates a room's Move Intents into actual single-step moves (priority first, most-constrained first, swap when contested). Fourth pure step beside Planner, Matcher, and [[emitter]]; movement is never issued outside it. A [[grounded]] creep sits arbitration out: its tile is blocked for the tick and no move Intent is issued to it (ADR 0008).

### Grounded
A creep still paying off fatigue this tick: the engine would answer any move with ERR_TIRED, so the [[resolver]] neither asks it to move nor lets anyone claim or displace through its tile (ADR 0008). Recomputed each tick from the Snapshot's fatigue — a stationary creep drains 2 fatigue a tick, so grounding is always transient.

### Travel cost
The cheapest-path cost from a creep to a Task's Work Area over the [[spatial projection]], for that creep's body: terrain weights (road 1, plain 2, swamp 10 — the engine's own fatigue costs; impassable excluded) scaled by the body's fatigue factor (ADR 0002, revised by ADRs 0006 and 0010), plus the [[occupancy surcharge]] on tiles under standing creeps (ADR 0008). Priced from the load carried right now: carried energy loads Carry parts 50 apiece, and an empty Carry generates no fatigue — the engine's own rule. Breaks rank ties in the Matcher; a Work Area with no travel cost — unreachable or empty — makes the Task inapplicable to that creep: never matched fresh, and a remembered assignment to it is released. Priced in half-ticks (ADR 0010) — and a ranking price and nothing else since ADR 0029: halving it yields no clock, and every time-aware judgement reads the [[walk]] instead.

### Walk
A traffic-blind path priced in whole ticks, no step below one tick (ADR 0029): the [[atlas]]'s clock, beside [[travel cost]]'s ranking price. Same weights, the same body-aware [[work area]] and the same totality — a creep or target the [[spatial projection]] cannot place prices 0, an unreachable Work Area has no walk at all and its readers count from now — with two deliberate differences. Every step costs `max(1, ceil(units / 2))` on travel cost's own unit price: no body crosses a tile faster than a tick however much Move it carries, and the nested rounding is exact, so this is the step's physical time rather than an approximation of it. And today's standing creeps price at nothing: a bystander who moves on next tick is not part of the path, so no [[occupancy surcharge]] can dispatch a creep this tick and recall it the next. A creep's walk to a Task's Work Area is its [[arrival]], priced off the same per-tick flood memo travel cost reads, keyed by pricing so no two of its prices can drift apart — a third, the traffic-blind ranking price the reroute attribution compares against, joined them there (ADR 0030). One rule for every clock in the colony: a successor's walk out of the spawner is the half of a [[lead]] paid after the cast, and the [[hauler unit]] quota's round trip is two walks — one loaded, one empty — summed. Each floods from its own origins and memoises accordingly, but the price of a step is the walk's everywhere, so nothing in the colony turns units into ticks a second way.

### Occupancy surcharge
The extra cost (10, one swamp step — ADR 0008, re-expressed by ADR 0010) the flood prices onto a step landing on a tile some creep occupies this tick. Sends travellers around standing traffic when a lane is cheaper, but the tile stays passable — traffic re-prices a route, it never makes a Task inapplicable, unlike an obstacle.

### Workforce target
The number of creeps the colony maintains, derived fresh each tick from the Snapshot, never persisted. Three addends, each a pattern row's own quota rule (ADR 0012): Anchors — one per [[post]]; haulers — the throughput arithmetic per source [[container]]; workers — unallocated income divided by one worker body's Work drain, so exactly as many upgrade mouths as the surplus feeds. Floored at 2. A source whose Post is provided for retires its other [[seat]]s — the seat count stopped being the base the moment one heavy body could drain a source alone. Spawning fills the gap between the target and the creeps that will still be alive when a replacement could arrive — an [[expiring]] creep is already outside the count (ADR 0026).

### Room energy
One room's shared spawn-energy account (spawn + extensions) — a colony fact, not spawn state. Spawn planning allocates bodies from it in spawn order, debiting as it goes, so the same energy is never committed twice; a spawn whose room banks nothing waits.

### Spatial projection
The Snapshot's map-shaped view of the spawn room — the only one (ADR 0005): the room's name, three-state terrain (plain / swamp / wall), entity positions, what kind of thing each target is (source, controller, a structure or a site of some built kind), which tiles hold a built road (ADR 0010), and current/max hits on repairable kinds only — fields nobody decides on stay out. Raw data, always present (possibly empty); decisions consult it only through the [[atlas]]. A tile absent from the projection is impassable — absence is per-entry, never per-projection (ADR 0004).

### Atlas
The per-tick, task-aware query interface over the [[spatial projection]]: Seats, Work Areas (body-aware for a [[work-heavy body]] harvesting — ADR 0020), travel costs, first steps, action permission, standing candidates, placed creeps — and the placement queries the [[layout]] derives from — room name, target positions, buildable tiles, [[working ground]], structure and road censuses, raw-terrain [[trunk]] paths (ADR 0005, ADR 0011, ADR 0022). Total (ADR 0004): geometry the projection cannot place gets one documented answer per query — it never counts against a [[task]] and never blocks an action. Built fresh each tick; Matcher and Resolver consult the same one.

### Anchor
The heavy-WORK [[body pattern]] cast for a [[post]]: as many Work as saturate one source plus one spare — six — beside one Carry and minimal Move (ADR 0021: past saturation a further Work only drains the source sooner and idles until it regenerates; the spare absorbs an unmanned Post's gap). Its slowness is the point — body-aware [[travel cost]] pins it to a [[post]] — Harvest's [[work area]] for its heavy body is nothing else (ADR 0020), so it neither works nor fills anywhere but the tile worth garrisoning — where it works in place: alternating Harvest and Upgrade on a [[dual seat]], draining the source into the container under it on a source-[[container]] Post (ADR 0006, generalized by ADR 0012). The one Carry is kept on both footings so one sizing rule serves the whole row. Travel cost alone doesn't walk it home: [[withdraw]] is inapplicable to it as a [[work-heavy body]] (ADR 0016), so its only feeding-tier candidate is Harvest and an unmanned Post wins it regardless of distance. Dispatched by [[arrival]] (ADR 0025): a fresh Anchor leaves the spawn while its source is still drained, timed to reach the Post as it [[restock]]s, and a container-Post Anchor holds its tile through every empty window. Replaced by [[arrival]] too (ADR 0026): once [[expiring]], its successor is cast and walks while it still digs. Exempt from [[fatigue parity]], which governs only the [[worker unit]] pattern. One Anchor per Post, inside the [[workforce target]].

### Hauler unit
The repeating [Carry; Carry; Move] block hauler bodies are built from (ADR 0012): 150 energy, full speed loaded *on roads* — the row carries its own parity declaration (road parity, not the plain parity of [[fatigue parity]]), because a hauler's whole life is the [[trunk]]. No Work part, so Harvest, Build, Upgrade and Repair are inapplicable by body; it lives in the [[withdraw]]→[[refill]] cycle. Quota: per source [[container]], round-trip ticks to the spawn (the canonical sink the trunks radiate from) times source output over carry capacity, rounded up. The round trip is two [[walk]]s summed, the leg out loaded and the leg back empty (ADR 0029) — nothing is halved on the total, because each leg is already whole ticks.

### Work-heavy body
A living body with strictly more Work parts than Move — the [[anchor]] row's shape, and readable off the parts alone: [[fatigue parity]] forbids a worker body from reaching it (ADR 0003) and only the Anchor row's floor of two Work over one Move clears it. A predicate over any body, not a [[body pattern]]: what a creep is is decided from what it is made of, never from the row name in its name (ADR 0006). Three gates read it — [[withdraw]] is inapplicable to one (ADR 0016), Harvest's [[work area]] narrows to a [[post]] for one (ADR 0020), and Harvest's Post cap and its full-store garrison reprieve are its alone (ADR 0024) — so its intake is digging, from the tile worth digging on, and that tile is what it competes for.

### Body pattern
The repeating part block a body is generated from; capacity buys as many whole repeats as it can. The [[worker unit]] is one pattern. Which pattern a spawn casts is a colony decision; a pattern shapes what a creep is good at, never what it is assigned — creeps stay interchangeable and matching stays Task-based.

### Worker unit
The repeating [Work; Carry; Move] block worker bodies are built from — the generalist [[body pattern]]: 200 energy, full speed empty, half speed loaded. Capacity buys as many whole units as it can; what's left is remainder.

### Fatigue parity
The body-generation invariant (ADR 0003): a worker body padded beyond whole [[worker unit]]s never moves slower than the pure-unit body, empty or loaded. The remainder buys as much Carry as parity allows, then Move — never Work.

### Layout
The deterministic full structure plan (ADR 0011): every clustered structure (tower first, then extensions) picked by one ordering rule — buildable tiles on the spawn's checkerboard colour, nearest-to-spawn first, [[working ground]] excluded and the [[link footing]]s held out (ADR 0022) — plus the [[storage]] (the first clustered pick, ADR 0022), the [[link footing]]s, the [[container]]s and the [[trunk]] roads, computed whole from the [[atlas]], never persisted to Memory. The clustered horizon stays at RCL4 (20 extensions + tower): the room's cluster is already hemmed in by walls and the controller pocket, so reserving RCL5's ten further extensions would only push the trunks (ADR 0022); Storage and the Link footings are reserved regardless of level because their tiles, once taken by an extension, never come back, and the reserved window carries one spare tile per footing so the picks a footing pushes outward are reserved before the trunks are routed too (ADR 0027). Recomputed only when its [[census signature]] changes (ADR 0017) — same census, same plan. Placement filters the Layout to what the current RCL unlocks and what is missing; the tick a level lands, its sites drop.

### Trunk
A paved line in the [[layout]]: each source to the controller, each source to the spawn, plus the swamp tiles inside the controller's Upgrade [[work area]]. Priced on raw terrain only and routed around the Layout's reserved tiles. The rule is general — short trunks still pave; room-specific exemptions don't get encoded (ADR 0011).

### Working ground
The tiles the colony works from: every source's [[seat]]s and the controller's Upgrade [[work area]]. Excluded from the [[layout]]'s clustered ordering (ADR 0022) — a clustered structure there eats a standing tile the [[anchor]]s and upgraders need, and in this room the cluster's nearest-to-spawn ring reaches the controller pocket by RCL5. A [[link footing]] is the one structure allowed on it. Derived from the [[atlas]] each tick, never persisted.

### Storage
The colony's stock: the one storage structure the [[layout]] places on the cluster's first pick (ADR 0022), built the tick RCL4 lands. Two roles, both ordered so it never outbids the flow (ADR 0023): the deepest [[refill]] target — below even the controller [[container]], so energy reaches it only when every other sink is full — and a [[withdraw]] source one tier below the source containers, pooled only while some sink other than itself has free capacity, so a hauler beside it never cycles energy in and out of the same store. Not a repairable kind (it does not decay) and never the [[trunk]] hub — the spawn stays the canonical sink, and the Storage sits beside it by construction.

### Link footing
A tile the [[layout]] reserves for a link (ADR 0022) while the level that unlocks links is still ahead: one beside each **planned** source [[container]], one beside the controller container, one beside the [[storage]] — planned, not built, because a [[post]] needs a standing container and a Post-anchored rule would reserve nothing at level 0, when the reservation is worth the most. The count is the rule's, never a constant: a room with three sources holds five footings. Each footing is the buildable tile within range 1 of its target that is off every [[trunk]], off the footings' own targets and off the other footings, nearest the spawn, ties by x then y — read back from a standing link too, so the tile does not move the tick its link goes up. The clustered reservation is the one thing it does not dodge, because it outranks it: the tower and the extensions yield by taking their picks with the footings held out, and the reservation is widened by the footing count so the picks they push outward are still tiles the trunks were routed around (ADR 0027). The only structure footing allowed on [[working ground]]: a link there buys the [[anchor]] and the upgraders a transfer without leaving their tile. A link is a built kind and nothing else — no placement [[intent]] ever names one; which footings are filled first, and what the links do, is RCL5's decision, and the footing exists so an extension never claims the tile in the meantime.

### Arrival
The tick a creep can first act on a Task: its [[walk]] to that Task's [[work area]] from where it stands now (ADR 0029) — whole ticks over a path blind to today's traffic, never [[travel cost]] halved. The horizon at which time-aware judgements are made, not the current tick (ADR 0025, ADR 0026): a drained source's Harvest is applicable when the walk covers the [[restock]] wait; a creep is [[expiring]] when its successor's [[lead]] outlasts its life; a Task's capacity counts a holder only when its stay and the candidate's overlap — the holder is still alive when the candidate arrives, and has arrived itself before the candidate dies. Derived each tick from the Snapshot and the [[atlas]], never persisted.

### Restock
The moment a drained source holds energy again — the engine's regeneration, projected as ticks remaining (zero for a source holding energy now). The one time fact the Snapshot carries about a source (ADR 0013, widened by ADR 0025): stocked is a restock of zero, not a field of its own.

### Expiring
A living creep whose remaining life is at or under its [[lead]] — it will be dead before a replacement cast now could stand where it stands (ADR 0026). Excluded from the [[workforce target]]'s living count and its row's gap, so the successor is cast while it still works; never released for it — anti-thrash keeps it on its Task to the last tick. Derived each tick, never persisted.

### Lead
The time a creep's replacement needs to stand on its tile: the successor body's cast time (3 ticks a part) plus that body's [[walk]] out of the spawn, priced over [[travel cost]]'s own weights for the successor's fatigue factor, not the incumbent's (ADR 0026) — whole ticks with no tile below one, and blind to today's traffic as the hauler quota's round trip is (ADR 0029): the path does not start until the body is cast, and the tile it ends on is the one the creep being replaced is standing on. It begins *beside* the spawner, where the engine places a finished creep, never on the spawner's own tile — a step the replacement never walks would buy a lead it cannot use. One rule for every [[body pattern]] — a slow Anchor earns a long lead, a hauler on a [[trunk]] a short one — never a per-row constant. Several spawns resolve as the quota resolves them, at the cheapest; geometry the [[atlas]] cannot price leads nobody.

### Verdict
The reasoned outcome a decision step returns beside its decision — data, never a log line (ADR 0009). The [[matcher]]'s Verdict on a creep says which [[task]] won it and why (rank, [[travel cost]], load, tie-break), that a remembered assignment was kept (anti-thrash, distinct from a fresh match), why an assignment was released, or why an unassigned creep got nothing — including a Task whose time has not come (too early: the body and energy fit, only the [[arrival]] doesn't, ADR 0025); the [[resolver]]'s says what became of its movement ([[grounded]], yielded, reroute). The too-early reason is not a bare word: on a release and on a rejection alike it carries the two numbers the gate compared, the [[walk]] and the [[restock]] wait, because since ADR 0029 no reader recovers the walk by halving a [[travel cost]]. A Verdict that falls out of work already done is always-on; one whose evidence must be manufactured — full candidate scoring, reroute attribution (ADR 0018) — is computed only for creeps on the [[verbose list]]. The Planner and spawn decisions return no Verdicts.

### Census signature
The fingerprint of everything a census-derived plan reads: the kind and position of every standing structure and pending site, the controller level, the room name. While it is unchanged the [[layout]] (and the hauler quota, which reads a subset) is provably identical and is not recomputed (ADR 0017). Held in heap only — a global reset discards it and the next tick recomputes.

### Verbose list
The creep names owed full candidate scoring, stored beside the [[transition log]] under `Memory.fabot.observe` and read fresh each tick — flipped from the terminal through the Memory HTTP API, so an investigation needs no redeploy. Empty (or absent, or malformed) means off; each listed creep's Scoring [[verdict]] carries one Candidate per pooled [[task]]: scored on the full matching key, or rejected at the first gate it failed (inapplicable, capacity-full, unreachable, too-early — the last carrying the [[walk]] and the wait it was judged against, since a rejected row has no cost to read one off). The scored row is not widened to match: only a rejection raises the question of how long the creep still has to wait. A listed creep also gets reroute attribution (ADR 0018) — the manufactured-evidence Verdicts ride this list together. The attribution's traffic-blind route comes off the [[atlas]]'s shared flood memo since ADR 0030, so the flood ADR 0018 called unmemoisable no longer is; the list still gates it, because that decision was about log noise and not about the flood.

### Transition log
The per-creep ring of recent changes — task handovers and movement events, each with its [[verdict]] and tick — written only on the tick something changed. The colony's answer to "why did this creep flip", capped per creep; a quiet creep writes nothing. It keeps only creeps still alive, which is why a raid needs a channel of its own (ADR 0028).

### Raid log
The colony's episodic record of [[hostile]] presence in the spawn rooms (ADR 0028), stored beside the [[transition log]] under `Memory.fabot.observe` and read from the terminal with `observe.mjs raids`. A channel of its own for a structural reason: the Transition log is keyed by creep and prunes a creep's whole timeline the tick it dies, so it cannot record the one event a raid record exists for. An **episode** opens the first tick a spawn room holds any hostile, stays open while hostiles keep appearing, and closes after fifty quiet ticks — the **quiet gap**, long enough to outlast a poke-and-heal cycle, so a squad stepping in and out across 220 ticks is one episode and not forty. Each records its window (opened, and the last tick a hostile actually stood there), the **roster** — one row per distinct hostile id with its owner and part counts, from its first sighting, so a squad reads as five rows — the **closest approach** — the smallest range over the whole episode between any hostile and anything of ours, an owned creep or an owned structure, with the tile and the tick, the number that separates a probe at the room edge from a loss — and our **losses** inside the window, by name and tick: a creep gone while a hostile stood there, stamped at the tick it was last seen alive, because a name is missing the tick after its creep died. Never a creep whose own clock ran out — the record answers what the raid cost, and the Snapshot's ticks-to-live, the same fact [[expiring]] is judged from, tells old age from a kill before it happens. A ring capped at twenty episodes, oldest trimmed first, disposable by construction like its sibling. Damage in hits stays unrecorded until a decision reads it. A record to be read, never a signal sent: nothing in the colony reacts to it.

### Chat bubble
The glyph an assigned creep says over its head each tick, one fixed glyph per [[task]] (⛏ Harvest · 📥 Withdraw · 🔋 Refill · 🔨 Build · 🔧 Repair · ⚡ Upgrade). Observability only, private to our own viewer; unassigned creeps show nothing.

### Safe-mode reflex
The colony reflex (ADR 0007, revised by ADR 0015) that emits `ActivateSafeMode` the tick a CLAIM-part [[hostile]] stands within range 3 of the controller — the claim tap is a range-1 act, so holding until then is free and gives the [[fire reflex]] its window to kill the claimer en route; an unplaced controller falls back to firing on sight. Gated only on stock remaining and safe mode not already running; hostiles without CLAIM never spend the stock.

### Pickup reflex
The colony reflex that emits a pickup Intent for every creep with free carry capacity standing within range 1 of a dropped energy pile — beside its assigned [[task]]'s action, since the engine's pickup conflicts with no other action. A reflex, not a Task: no movement, no matching, no threshold — it only recaptures what is already in reach (death drops, harvest overflow). Energy only; tombstones are not covered. Piles are projected as position and kind alone — no amount, since no decision reads one. Like the [[safe-mode reflex]], it speaks no [[verdict]] and shows no [[chat bubble]].

### Fire reflex
The colony reflex (ADR 0014) that has every tower shoot the [[hostile]] nearest to itself, each tick one stands in the room. Attack only — towers never repair (creep Repair is far cheaper per hit) or heal — and per-tower: no focus fire, no anti-drain gate, no energy floor (a dry tower's shot fails harmlessly; unlike the [[safe-mode reflex]] there is no stock to protect). Like its siblings, it speaks no [[verdict]] and shows no [[chat bubble]].

### Hostile
A hostile creep as the Snapshot projects it: its id, its owner, its position, and its body parts, verbatim — the fields its readers need (ADR 0014, widened by ADR 0028), and the projection grows one only the tick a reader for it exists (ADR 0007's rule that the field list grows when a decision reads it, widened to any reader by ADR 0028). What a hostile can do is decided from what it is made of — CLAIM is the only part that threatens the controller, and the controller is the only thing safe mode is spent on (ADR 0007); where it stands is what the [[fire reflex]] aims at; whose it is, is the [[raid log]]'s roster, the reader the owner grew for and the only one that reads it. Hostiles stay out of the [[spatial projection]]: they block no tiles, price no paths, gate no tasks.

### Downgrade deadline
The hard floor on the controller's downgrade timer: half the level's full timer (ADR 0007). Inside it, Upgrade stops being surplus work and outranks even the feeding tier — a downgrade costs a level and zeroes the safe-mode stock, and the engine refuses safe-mode activation below half minus 5,000, so escalating at half keeps the [[safe-mode reflex]] fireable with the engine's whole grace intact.

### Disaster fallback
The zero-creep spawning rule: an empty colony spawns bare [[worker unit]]s from whatever [[room energy]] is banked right now — as many as the bank affords, up to the workforce deficit — rather than waiting for a full capacity it can never refill. The one body-generation path that ignores the remainder — time-to-first-creep outranks spending the bank.

## Avoided terms

- **Role** — creeps are not born with roles; work is Task-based. Don't reintroduce role-based vocabulary.
