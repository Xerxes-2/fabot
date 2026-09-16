# W15S27 as W15S28's outpost — measured with the repo's own code, over the committed captures

Date: **2026-09-16**. Executes the ticket ADR 0059 left open (*"the declaration
arithmetic in `multihop-outposts.md` §4 was taken under the old rule … W15S27 and
W15S29 … the recommendation that they wait for W15S28 was argued from numbers that
are now 36% and 40% too high"*). W15S28 now stands at RCL6 and borders W15S27, so
the room is re-priced **from W15S28** rather than re-priced from W13S28.

**No API call was made.** Everything below is the repo's own functions run over
captures already in the tree (`tests/Core.Tests/rooms/W15S28.room`, `W15S27.room`,
`W15S26.room`, `W15S25.room`, `W14S28.room`), driven from a temporary Expecto file
`tests/Core.Tests/W15S27ScoutTests.fs` — seven cases, run as
`dotnet test --filter "FullyQualifiedName~w15s27" --logger "console;verbosity=detailed"`,
raw stdout in `/tmp/scout/w15s27.log`. **The file has been deleted and
un-registered from `Core.Tests.fsproj`**; the three gates below are green without it.
Method copied from `third-colony.md` §3 and `w11s29-spawn.md`.

| Gate | Result |
|---|---|
| `npm run format:check` | clean |
| `dotnet build` | clean, 0 warnings |
| `dotnet test` | **1386 / 1386** with the scout file gone (the tree's baseline today; the ticket's "1379" predates `84c71a80` and the two commits after it). With the scout file present it read 1393. |

Functions the numbers come from — nothing below is hand-arithmetic unless it says so:
`Declaration.withinHopBudget` / `Declaration.routable` (`Types/Colonies.fs`),
`RoomName.routesBy`, `Seam.bandBy` / `Seam.joinedBy` (`Types/Geometry.fs`),
`World.ringWalkable` / `World.groundWalkable` / `World.scanOf` (`Types/Sightings.fs`),
`ColonyView.ofWorld` and its `transiting` narrowing (`Types/Views.fs`),
`Atlas.seams` / `routes` / `seats` / `seatTilesOf` / `seamWalkTicks` /
`haulRoundTripTicks` / `castWalkTicks` / `walkTicks` / `workArea` (`Atlas.fs`),
`Decide.decide` and its `Quotas` (`Decide/Entry.fs`), `Quota.haulerDemandOf` through
those Quotas (`Decide/Quota.fs`), `Planner.planTasks` + `Pool.planPool` /
`Pool.priorityOfTier` / `Pool.siteOrder` (`Decide/Planner.fs`, `Decide/Pool.fs`),
`Layout.planLayout` (`Decide/Layout.fs`), `Bodies.bodyFor` (`Decide/Bodies.fs`),
`Keepers.centresIn` / `maskedTilesIn`, `Tuning.keeperMargin` (`Types/Keepers.fs`,
`Types/Rules.fs`).

---

## Summary — question 4 and question 5 first, because the user's plan turns on them

- **Q4. Declaring is the whole difference between a hand-laid site being built and
  being invisible, and the mechanism is not the builders' budget — it is vision.**
  W15S27 is *already* in W15S28's scan set as the transit room of the W15S25 errand,
  and `ColonyView.ofWorld`'s `transiting` sets `ConstructionSites = []` for a transit
  room (`Types/Views.fs:374`). Measured over the same world with the same three
  hand-laid sites standing: **undeclared → `view.ConstructionSites` is empty and
  `Planner.planTasks` pools not one `Build`**; **declared → all three sites are in
  the view and all three are pooled.** Their rungs, read off `PooledTask.Priority`:
  the container site **0** and the road site nearest the home crossing **0**
  (`Feeding`), the third site **20** (`Surplus`, `Pool.priorityOfTier`). That is
  `Tuning.OutpostBuilders = 2` rationing the queue exactly as #266 says, with
  `Pool.siteOrder`'s two keys visible: container first, then `Atlas.seamWalkTicks`
  ascending (`1,40` → 9, `13,29` → 20, `2,1` → 48). **So: yes, declaring changes
  whether the user's sites get built — from "never" to "two at a time, nearest the
  crossing first".**
  **And one warning that outranks the rest: the rock at `14,28` has exactly one
  Seat, `13,29`.** With a hand-laid road site on that single tile, `decide` plans
  **no container in W15S27 at all** (measured: the only container intent left is
  W15S28's own mineral one). ADR 0042 as #244 amends it, in its worst possible
  shape — pave anything you like out there **except `13,29`**, or the room never
  enters the economy.

- **Q5. Paving W15S27 does nothing for the Reactor courier. Not "little" — zero.**
  The courier is `Bodies.courierPattern` = 20 Carry / 10 Move, and
  `Tuning.ReactorLoad = 500` fills ten of those twenty Carry parts, so
  `Grid.fatigueFactorOf` reads **10 fatigue parts against 10 Move parts** — parity,
  which is one tick a tile on plain *and* on road. Measured with `Atlas.walkTicks`
  on the real chain, loaded courier at W15S28's spawn to `Reclaim` the W15S25
  reactor: **198 ticks unpaved, 198 ticks with every one of W15S27's 1,997 walkable
  tiles paved.** The empty return is **170 in every reading**, paved or not: an
  empty body has zero fatigue parts and roads cannot make a tick shorter than a
  tick. The only thing a road buys this body is swamp, and the 28 ticks the loaded
  leg does carry are **not in W15S27**: paving W15S28's own ground alone takes 198 →
  **178**, paving all four rooms takes it to **170**. (The mechanism is alive and
  measurable — a hypothetical *full* 1,000-unit courier goes 398 → 350 when W15S27
  is paved — it just does not fire at half load.) **Pave W15S27 for the hauler if
  you like; do not pave it for the courier.**

- **Q1.** Admissible, and by one chain each way. `hopsBetween` = **1**;
  `Declaration.routable` is **true out and true back** (ADR 0062's both-side rule);
  `routesBy` answers exactly `[["W15S28";"W15S27"]]` and `[["W15S27";"W15S28"]]`, so
  k = 1 and ADR 0059's multi-chain pricing costs this declaration nothing. The band
  is **20 tiles in both directions**, masked and raw alike.

- **Q2.** One source, one Seat, one container: the pick is `13,29`
  (`Decide.decide`'s own `PlaceConstructionSite`), 20 ticks from the Seam
  (`Atlas.seamWalkTicks`). Round trip to the dearest sink is **132** ticks
  (cluster 132 / storage 131 / buffer 130), so the reserved row is `10 × 132 =
  1320` of demand. The colony's hauler row goes **1 → 2** and its anchor row
  **2 → 3**; the whole workforce target goes **6 → 9**.

- **Q3.** The reserver's walk is **89 ticks** (`castWalkTicks [Claim; Move]` from the
  live spawn tile `18,30` to the cheapest of the controller's three Reserve
  tiles). Cadence is ADR 0042's unchanged rule — `ceil((5000 − ticksToEnd) / 600)`
  CLAIM parts, `Engine.claimLifetime = 600`, `Engine.reservationCap = 5000` — so the
  standing cost is `650 / (600 − 89)` = **1.27 energy a tick** for `+5` a tick of
  rock.

- **Q6.** Nothing measurable says danger. The captures carry **no core, hostile or
  reservation data at all** (`capture-room.mjs` records fixed furniture), so the
  empty `view.InvaderCores` below is the fixture's silence and not evidence; the
  live read the user did (`W16S27 core=-`) and `fourth-colony.md`'s t491,507 sweep
  (0 cores within 6 hops) are what stand. On the keeper question there **is** a
  measurement: `Tuning.keeperMargin = 6`, `Keepers.centresIn "W15S27" = []`, and our
  own tiles are **22–23** (the three Reserve tiles) and **40** (the container Seat)
  from the nearest W15S26 centre in the stacked-room metric. Declaring puts no body
  of ours anywhere near a keeper.

- **Q7.** No. An extractor needs `Tuning.ExtractorLevel = 6` on a controller
  **this colony owns**, and `Layout.planLayout` runs on the home room only —
  measured, `decide` emits `Extractor` in W15S28 and never in W15S27. The d2 10,000
  Thorium at `8,24` is a **claim** argument for some future colony, not an outpost
  argument, and is worth nothing to this declaration.

- **Recommendation: declare it.** ~6.3 energy a tick net for one 5,000-energy
  container, one 1,300-ish reserver cycle and one extra hauler, on a one-hop room
  whose whole haul is 132 ticks — the same band the five best one-hop rooms in
  `multihop-outposts.md` §4 occupy. The exact line is in §9.

---

## 1. Is it admissible at all?

All of this is `Declaration.routable` and `RoomName.routesBy` driven over
`World.ringWalkable` / `World.groundWalkable` built from the committed captures —
the same predicate the shell builds, not a copy (the shape `RoomOutpostTests` uses).

| Reading | Function | Answer |
|---|---|---|
| hops | `RoomName.hopsBetween "W15S28" "W15S27"` | **1** |
| name gate | `Declaration.withinHopBudget 3` | true |
| ground gate, out | `Declaration.routable linked 3 "W15S28" "W15S27"` | **true** |
| ground gate, back | `Declaration.routable linked 3 "W15S27" "W15S28"` | **true** |
| chains out | `RoomName.routesBy linked 3 "W15S28" "W15S27"` | `[["W15S28"; "W15S27"]]` |
| chains back | `RoomName.routesBy linked 3 "W15S27" "W15S28"` | `[["W15S27"; "W15S28"]]` |
| band, south side | `Atlas.seams atlas "W15S28" "W15S27"` | **20** |
| band, north side | `Atlas.seams atlas "W15S27" "W15S28"` | **20** |
| band with no keeper mask | `Seam.bandBy` over raw terrain | **20** (the mask takes nothing here) |
| the next crossing up, both ways | `Atlas.seams` W15S27↔W15S26 | **20 / 20**, masked and raw alike |

Three things follow and each matters somewhere below:

1. **One hop, one chain, both directions.** ADR 0059's cost — k far fields and k band
   joins per priced pair — is k = 1 here, so this declaration adds nothing to the
   tick beyond one more room's grids. `third-colony.md` §4's "W15S28 ↔ W15S27 = 20"
   is reproduced tile for tile, which is this document's cross-validation against
   the earlier survey.
2. **ADR 0062's both-side rule passes.** No orphaned landings: the chain out is a
   chain back, which is what `haulRoundTripTicks` needs to answer at all.
3. **The keeper mask is not in play at this border.** It matters one room further on
   (W15S26), and even there the band is 20 both ways over the mask.

## 2. What the outpost is worth, priced the way the colony prices one

The home room is W15S28 at RCL6, spawn on its **live** tile `18,30` (the tile ADR
0064 priced the colony from), grown to its own Layout's answer — 57 paved tiles,
Storage `17,29`, source containers `11,18` and `6,29`, controller buffer `23,29`,
mineral container `28,11` — by standing what `decide` plans and asking again (see
§ substitutions). The 57 reproduces `third-colony.md` §3's "W15S28 … road 57", the
second cross-validation.

| Reading | Function | Answer |
|---|---|---|
| the rock | the capture's own id | `6a8caa95dd4872bccd319011` at `14,28` |
| Seats | `Atlas.seats` | **1** — the tile `13,29`, and nothing else |
| container pick | `Decide.decide` → `PlaceConstructionSite(_, Container)` | **`W15S27 13,29`** |
| pick's walk to the Seam | `Atlas.seamWalkTicks "W15S27" "W15S28" (13,29)` | **20** |
| hauler body at bank 2,300 | `Bodies.bodyFor haulerPattern 2300` | 45 parts, 30 Carry / 15 Move, **1,500** capacity |
| round trip → spawn cluster | `Atlas.haulRoundTripTicks` | **132** |
| round trip → Storage | same | 131 |
| round trip → controller buffer | same | 130 |
| demand, room reserved | `Quota.haulerDemandOf` (through `Decision.Quotas`) | output 10 × **132** = **1,320** |
| demand, room neutral | same | output 5 × 132 = 660 |

`haulerDemandOf` prices the **dearest** sink, which here is the spawn cluster at 132
— unusual, and a consequence of this room's shape: the Storage and the buffer both
sit east of the spawn, nearer the W15S27 crossing than the cluster is.

**What the declaration does to the rows** (same standing world, same tick, the only
difference being whether `Outposts` names the room):

| Reading | undeclared | declared, room neutral | declared, room reserved |
|---|---|---|---|
| hauler | **1** | 1 | **2** |
| anchor | 2 | 3 | **3** |
| reserver | 1 | 2 | **2** |
| whole target | 6 | 8 | **9** |
| colony haul demand | 680 | 1,340 | **2,000** against a 1,500 load |

The hauler step is `ceil(2000 / 1500) = 2`, and #279's remote floor would have forced
the same 2 on its own (`remote = 1320`, `1320 × 2 ≥ 1500`) — so the marginal hauler
is **one whole body for one rock**, and it is the single largest line in the bill.
The reserver row's step from 1 to 2 is the new outpost's own seat; the seat it
already had is the W15S25 re-claimer's, which shares that row (#318).

**One source only, and what that does to the case.** It halves the gross against a
two-source room but it does not halve the cost: the reserver (1.27) and the marginal
hauler (1.50) are paid per *room*, not per rock. Against `multihop-outposts.md` §4's
own yardstick this lands in the one-hop single-source band, not below it:

| Line | Energy a tick | Where the number is from |
|---|---|---|
| gross, reserved | **+10.00** | `Engine.heldOutputPerTick`, measured in the demand row's `output` |
| anchor row, +1 body | −0.47 | `multihop-outposts.md` §0.2 (700 e over `Engine.creepLifetime`); **not re-measured here** |
| reserver | −1.27 | `650 / (600 − 89)`, the walk measured in §3 |
| hauler, +1 body | −1.50 | measured step above × 2,250 e / 1,500 ticks |
| container decay | −0.50 | `remote-mining.md` §1.3, **unverified there and here** |
| **net** | **≈ +6.26** | |

which sits inside §4's one-hop band of 6.31–6.36 and above every two-hop room it
ranked. One-off: the container's 5,000 energy of progress.

## 3. The reserver's cost

| Reading | Function | Answer |
|---|---|---|
| controller | the capture's own id | `6a8caa95dd4872bccd319010` at `6,9` |
| tiles a reserver may stand on | `Atlas.workArea (Reserve _)` | **3** — `5,8`, `5,9`, `6,8` |
| walk, `[Claim; Move]` | `Atlas.castWalkTicks` from `18,30` | **89** (`5,9`), 90 to either other tile |
| walk, the row's body at bank 2,300 | `castWalkTicks (bodyFor reserverPattern 2300)` | **89** — identical, CLAIM/MOVE being fatigue parity |
| to the controller *tile* | `castWalkTicks … (6,9)` | `None` — a controller's tile is an obstacle, which is why the row prices its Work Area and not its target |

Cadence is ADR 0042's, unmoved: `claims = ceil((5000 − ticksToEnd) / 600) |> max 1`,
`Engine.claimLifetime = 600`, `Engine.reservationCap = 5000`. With an 89-tick walk a
CLAIM part buys `600 − 89 = 511` ticks of reservation per 600-tick life, so the
steady state is one block at a time (`600/511 = 1.17`) and the standing cost is
`650 / 511 = 1.27` energy a tick against the `+5` a tick the reservation is worth
(`heldOutputPerTick 10` against `neutralOutputPerTick 5`). It pays for itself
roughly four times over, which is the least interesting number in this document and
the one nobody should re-litigate.

## 4. The road question, stated exactly

**This is the section the user's plan turns on, so it is the code and not the ADR
prose.** The experiment is one world — the three sites below standing in W15S27, the
room reserved, everything else identical — read twice, once with `Outposts = [W15S27]`
and once with `Outposts = []`.

| Site (the human's hand) | tile | `Atlas.seamWalkTicks → W15S28` |
|---|---|---|
| road, far from the home crossing | `2,1` | 48 |
| road, near the home crossing | `1,40` | 9 |
| the colony's own container site | `13,29` | 20 |

| Reading | undeclared | declared |
|---|---|---|
| `view.ConstructionSites` | **empty** | all three ids |
| pooled `Build` tasks (`Planner.planTasks` → `Pool.planPool`) | **none** | all three |
| `13,29` container site | — | priority **0** (`Feeding`) |
| `1,40` road site | — | priority **0** (`Feeding`) |
| `2,1` road site | — | priority **20** (`Surplus`) |
| the home controller's `Upgrade`, for scale | −10 | −10 |

**Why undeclared is not "slow" but "never".** W15S27 is in the scan set either way —
`Colony.roomsProjected` puts it there as the transit room of `Errand.w15s25` — but
`ColonyView.ofWorld` sorts scanned rooms into worked, errand and **transit**, and
`transiting` (`Types/Views.fs:342-375`) returns the room's facts with
`ConstructionSites = []`, `TargetKinds = Map.empty`, `Sources = []`. A site the view
does not carry is a `Build` the Planner never pools, a Task the Matcher never scores
and an Intent nothing emits. The user's hand-laid roads in W15S27 today are not
low-priority; they are **invisible**, and no amount of waiting changes that.

**What declaring buys them, exactly.** `isOutpostSite` (`Decide/Pool.fs:521`) is a
room test and nothing more — not home, not borrowed — so every site the view now
carries enters the outpost queue. `Pool.siteOrder` sorts that queue by
`(is-not-container, seamWalkTicks, id)` and `Tuning.OutpostBuilders = 2` truncates
it; `tierOf` lifts exactly that head to `Feeding` (priority 0) and leaves the tail in
`Surplus` (20), where travel cost keeps a loaded worker beside the home controller.
The measured rungs above are that rule, tile for tile: the container never queues
behind a road, and of the two roads the one **9 ticks from the crossing** is lifted
while the one 48 ticks away waits. As each lifted site finishes, the next one in
takes its place (`fedOutpostSites`), and `builderShare` gives the pair of them one
builder apiece.

**The trap this room is built for, and it is not hypothetical.** The rock has **one**
Seat. `Layout.planOutpostContainers` picks over *"the Seats no other kind's
construction site already holds"* (ADR 0042 as #244 amends it), so a single hand-laid
road site on `13,29` empties the candidate set. Measured: with a road site standing
there, the container intents for the whole tick are `["W15S28 28,11"]` — W15S28's own
mineral container, and **nothing in W15S27**. No container means no Post, no Anchor,
no income and no hauler demand: the declaration would be live and worth zero, in
silence (#244 removed the `-7` line that used to say so). Pave the route by hand as
planned; leave `13,29` alone, or place the container site first and pave around it.

## 5. Does it help the Reactor courier?

The errand chain is `W15S28 → W15S27 → W15S26 → W15S25`, one chain, priced by
`Atlas.walkTicks` for a real body standing beside the spawn with a real load.

- Body: `Bodies.bodyFor courierPattern` = **20 Carry / 10 Move**, 30 parts.
- Load: `Tuning.ReactorLoad = 500` → `fatigueFactorOf` counts
  `min(20, ceil(500/50)) = 10` loaded Carry parts, so **10 fatigue parts against 10
  Move parts**. `Grid.stepUnits`: plain (weight 2) → 20 units against 20 paid a tick
  → 1 tick; road (weight 1) → 10 units → still 1 tick. **Only swamp (weight 10, 100
  units, 5 ticks) is dear, and a road takes it back.**

| Reading (ticks) | loaded, 500 | loaded, 1,000 (hypothetical) | empty |
|---|---|---|---|
| nothing paved → reactor | 198 | 398 | 170 |
| **all 1,997 tiles of W15S27 paved** → reactor | **198** | 350 | **170** |
| W15S28's own ground paved → reactor | 178 | 328 | 170 |
| all four rooms paved → reactor | 170 | 174 | 170 |
| nothing paved → the W15S27 rock | 70 | 142 | 50 |
| **W15S27 paved** → the W15S27 rock | **70** | 122 | **50** |
| W15S28 paved → the W15S27 rock | 50 | 72 | 50 |

Read plainly:

- **Paving W15S27 saves the courier 0 ticks loaded and 0 ticks empty.** Both legs.
- The loaded body does pay 28 ticks of swamp tax over the whole chain, and **20 of
  those 28 are in W15S28's own room** — which live already carries standing roads —
  and the remaining 8 are in W15S26 and W15S25, a Source Keeper room and a sector
  centre, neither of which this colony paves or should.
- The mechanism is not broken and the measurement proves it: the same paving saves
  the *full* 1,000-unit body 48 ticks. `Tuning.ReactorLoad` is deliberately half a
  courier (`Rules.fs`: under the 1,000-unit contact cliff), and half a courier is an
  unfatigued courier.
- **Therefore roads in W15S27 are a hauler argument, never a courier argument.** For
  the hauler — 30 Carry / 15 Move, loaded factor 45 fatigue parts against 15 Move —
  they are worth real ticks, which is the `haulRoundTripTicks` = 132 above, unpaved.

## 6. The hazards, measured

| Reading | Function | Answer |
|---|---|---|
| invader cores in the projection | `ColonyView.InvaderCores` over the capture world | `[]` — **the fixture's silence, not evidence** |
| hostiles | `ColonyView.Hostiles` | `[]`, same caveat |
| keeper margin | `Tuning.keeperMargin Tuning.defaults` | **6** (`Engine.keeperPin + rangedRange + ReachMargin`) |
| keeper centres in W15S27 | `Keepers.centresIn "W15S27"` | **`[]`** — the room is not a keeper room and nothing masks its ground |
| keeper centres in W15S26 | `Keepers.centresIn "W15S26"` | 8 (four lairs, three sources, one mineral) |
| nearest W15S26 centre to the shared edge | rows above the border | **10** |
| masked W15S26 ring tiles facing W15S27 | `Keepers.maskedTilesIn 6 "W15S26"` on `y = 49` | **0** — the mask costs that crossing nothing |
| **our container Seat `13,29` to the nearest centre** | Chebyshev over stacked rooms | **40** |
| **our three Reserve tiles to the nearest centre** | same | **22, 23, 22** |

So: **declaring W15S27 puts no body of ours inside `Rules.keeperMargin` of W15S26's
lairs**, by a factor of three and a half on the closest one. The only body of ours
that ever enters W15S26 is the courier, which crosses it **today** under the
already-declared errand; this declaration adds nobody to that room.

On the core warning in `remote-candidates.md` (*"two rooms out, **and hard against
W16S27's core**"*, and §4's "W16S27 → W15S27 is one step"): the capture format cannot
speak to it — `capture-room.mjs` records terrain, sources, controller and minerals,
and nothing that changes. What the tree does hold is `fourth-colony.md`'s t491,507
sweep: **0 invader cores in 99 rooms within 6 hops**, the W16S25 stronghold that
spawned W16S27's core having collapsed, which matches the user's live read of
`W16S27 core=-`. Two structural notes that are code and not weather:

- **Threat vision in W15S27 does not wait for a declaration.** `transiting` clears a
  transit room's furniture and sites but **not** `Hostiles` or `InvaderCores`, so
  whatever a body of ours sees crossing W15S27 already reaches the view. What the
  declaration adds is ADR 0043's per-outpost **stand-down gate** — the switch that
  withdraws work from the room — which today has nothing to withdraw.
- `W16S27 ↔ W15S27` is an open 24-tile band (`fourth-colony.md` §3), so a future core
  in W16S27 can expand into W15S27 exactly as `remote-candidates.md` feared. The
  answer is the gate above, per ADR 0043, and it only exists once the room is
  declared.

## 7. The Thorium note

**A reserved outpost can never extract it.** `Layout.planLayout` emits `Extractor`
only when `controller.Level >= view.Tuning.ExtractorLevel` (= 6) for the colony's
**own** controller, and the Layout runs over the home room alone (ADR 0042: *"no
roads, and no Layout"*). Measured: `decide` over the declared world places
`Storage / Tower / Extension / Road / Rampart / Extractor` in W15S28 and **only
`Container`** in W15S27; the scout's one hard assertion was that no `Extractor` is
ever placed in the outpost, and it is green.

W15S27's deposit — Thorium **d2 10,000 at `8,24`**, which the capture carries under
`6a901a43b8684d0008338890` — is therefore a **claim argument for a future colony**
and contributes exactly nothing to this declaration's arithmetic. It is filed here so
the next survey does not re-discover it as an outpost argument.

## 8. Which of `multihop-outposts.md` §4's numbers survive

Its W15S27 row reads `W15S27 | W13S28 | 3 hops | 1 source | 146 one-way | 304 round
trip | 154 controller walk | net 4.58`.

- **Dead, both halves.** Wrong home — the room is being declared from W15S28, one hop
  — and wrong pricing: those ticks are the compass chain `W13S27>W14S27` that #288
  replaced with the priced chain, which ADR 0059 records as 36% too high (107, not
  146). Nothing in that row survives as a number for this decision, including the
  §3.2 tie-break table's 107: that too is a W13S28 price, over a three-room chain,
  and irrelevant to a one-hop declaration.
- **What survives is §4's method and §0.2's constants**, and this document re-uses
  them: held 10 / neutral 5, the hauler block at bank 2,300 (30 Carry / 15 Move,
  1,500 capacity — re-measured here), the reserver amortised at `650 / (600 − walk)`,
  the 0.5 container decay (still unverified), and ADR 0049's single rounding with
  #279's remote floor (re-measured here, and it fires).
- **What survives as a conclusion is its ordering.** §4's reading — *"a single-source
  room's net is almost purely a function of distance: 6.31–6.35 at one hop, 4.65–4.81
  at two"* — puts this room at **6.26**, i.e. in the one-hop band, which is the whole
  content of its recommendation that W15S27 *"wait for W15S28"*. The recommendation
  is hereby executed, and its own arithmetic is what justifies it, re-measured.
- **And §5's risk argument survives intact and now favours the room**: its ordering
  rule was *"how many of the trunk's tiles lie outside our own rooms"*. At one hop
  from W15S28 the whole 132-tick round trip is one crossing and one room, against the
  three-room exposure the W13S28 declaration would have bought.

## Substitutions

What the committed captures forced a stand-in for. Each one is a place where the
number would move on the live server, and the direction is named.

1. **The home room is grown, not observed.** `W15S28.room` is terrain and furniture
   only — no spawn, no Storage, no containers, no extensions. The fixture stands a
   spawn on `18,30` (the live tile, per ADR 0064's deploy-tick pricing) at RCL6 with
   a full 2,300 bank, then runs `decide` eight times, standing every Road, Storage
   and Container it plans, until the plan stops moving. The result — **57 roads**,
   Storage `17,29`, source containers `11,18` / `6,29`, buffer `23,29`, mineral
   container `28,11` — reproduces `third-colony.md` §3's independently measured 57,
   which is why the substitution is trusted. Live, the room's standing structures
   differ from its plan (ADR 0064 records 84–116 standing roads against plans of
   25–57 in the other colonies), so **the sink legs below could be a few ticks off in
   either direction**; the 132 is the plan's room, not the live one.
2. **No reservation, then one by hand.** The captures know nothing of ownership, so
   the W15S27 control entry is written by the fixture: `Unowned`, with
   `Reservation = Ours, TicksToEnd = 4000` for the "reserved" readings. That is the
   only way to see the `output 10` rows; the neutral rows are the same world with the
   reservation taken away.
3. **The outpost container is stood, not built.** For the quota readings the pick
   `13,29` is stood as a `Structure Container` rather than raised from a site — the
   hauler quota reads standing containers (ADR 0042's switch, #205's split
   between `postsIn` and `standingPostsIn`), and this document is pricing the steady
   state, not the window before the switch flips.
4. **The hand-laid road sites are the scout's, not a human's.** `2,1` and `1,40` were
   chosen as the first walkable tiles of two bands of the room, to put one site near
   the crossing and one far from it. The *rungs* they land on are the code's; the
   tiles are not a claim about where the user should pave.
5. **"Paved" in §5 means every walkable tile of the room**, 1,997 of them — a pavement
   no human would lay. It is deliberately the most generous possible reading: if the
   maximal pavement saves the courier nothing, no real trunk saves it anything.
6. **The courier stands beside the spawn.** Its walk is priced from `19,30` and
   `17,30`, tiles the engine would put a fresh body down on; a courier caught
   mid-chain would read differently, and the *difference* between paved and unpaved
   is what this section claims, not the absolute 198.
7. **No creeps, no traffic, no threats anywhere else.** Every reading is a quiet tick
   with an empty raid log (`StandDown.none`) and no hostiles. `Atlas.walkTicks` and
   `haulRoundTripTicks` are traffic-blind by design (ADR 0029), so this costs the
   haul numbers nothing; it does mean the guard row reads 0 throughout.
8. **The re-claimer seat is in every row count.** W15S28 declares `Errand.w15s25`, so
   its reserver row already holds one seat before any outpost exists. The `1 → 2`
   step in §2 is the outpost's own seat, not the whole row.

## Not measured

- **The anchor row's body and its 0.47 e/tick.** The row count was measured (2 → 3);
  the body's cost is carried from `multihop-outposts.md` §0.2 and not re-derived.
- **Container decay (0.5 e/tick)** and **road maintenance**. Neither constant exists
  in `Rules.fs`; `multihop-outposts.md` §0.2 marks both unverified and so does this.
  If its road-wear figure is right, a *paved* W15S27 costs roughly 1 e/tick in
  maintenance against two haulers — which, given §5, is spent entirely on the hauler
  and not at all on the courier.
- **The raid tax.** `multihop-outposts.md` §5 measured 1.48 e/tick on W13S29 off a
  live raid log. Nothing in the tree lets that be re-measured for W15S27, and W15S28
  has no raid log of its own worth reading yet. The structural argument (one hop, one
  crossing, the shortest exposure of any candidate) is all this document offers.
- **CPU.** No profile scenario was run. One more one-hop room with k = 1 is the
  cheapest shape ADR 0059 describes, but "cheapest shape" is not a measurement, and
  `fourth-colony.md` §6 already flags the live mean at 40 ms.
- **The live W15S28 census.** Its actual level, bank, standing structures, creep
  count and spawn queue were not read; everything here is RCL6 with a full 2,300 bank
  and the Layout's own plan standing.
- **Whether a *second* rock elsewhere would be a better next declaration.** W15S29 —
  the other room ADR 0059 names — was not priced here. Its capture is in the tree and
  the same scout would answer it in an afternoon.
- **The stand-down's behaviour under a real core in W15S27.** ADR 0043's gate is
  cited, not exercised.

## 9. Recommendation

**Declare it.** One hop, one chain each way, a 20-tile band, a 132-tick round trip,
≈6.3 energy a tick net against one container, one reserver and one extra hauler —
and, the reason the user asked, **it is the only way their hand-laid roads in that
room ever get built**.

Two conditions, both from §4:

1. **Do not let a hand-laid site sit on `13,29`.** It is the rock's only Seat, and a
   site on it silently costs the whole declaration.
2. Expect **two** builders out there at a time, container first, then nearest the
   crossing — not a crowd, and not the whole trunk at once.

The line for `src/Core/Types/Colonies.fs` — ids and tiles are the capture's
(`tests/Core.Tests/rooms/W15S27.room`), in the capture's own order:

```fsharp
    /// W15S28's north outpost, declared 2026-09-16 off
    /// `docs/research/w15s27-outpost.md`, which executes the ticket ADR 0059
    /// left open: one hop and one chain each way over a 20-tile band, one
    /// source at a 132-tick round trip, and — the finding that decided it —
    /// the room is already in this colony's scan set as the W15S25 errand's
    /// transit room, where `transiting` strips its construction sites, so a
    /// human's hand-laid trunk out there is invisible until this line exists.
    /// Its rock has a **single** Seat, `13,29`: a site of any other kind on
    /// that tile plans this room no container at all (ADR 0042 as #244 amends
    /// it), which is the one way this declaration can be live and worth
    /// nothing.
    let w15s27: Outpost =
        {
            RoomName = "W15S27"
            Sources = [ "6a8caa95dd4872bccd319011", { Room = "W15S27"; X = 14; Y = 28 } ]
            Controller = "6a8caa95dd4872bccd319010", { Room = "W15S27"; X = 6; Y = 9 }
        }
```

and, in `Colony.declared`'s third entry, `Outposts = []` becomes
`Outposts = [ Outpost.w15s27 ]` — with the comment there (*"W15S27, W15S29 and
W14S29 are the rooms it will want, and a room worked from a colony that does not
exist is a body bought for nobody"*) updated to say that the colony now exists.

**Which test would need to move: none.** The live-declaration tests read
`Colony.declared` and grow by themselves — `RoomOutpostTests`' *"every outpost the
live declaration names is its capture's"*, *"a chain of real border rings joins every
declared outpost to its home"* and the container-pick case, and `ViewTests`' hop-budget
and no-room-declared-as-both cases — and all of them need only `W15S27.room`, which is
committed. The one thing to check by hand before pushing is that
`ViewTests`' outpost/errand disjointness stays green: W15S27 is a **transit** room of
`Errand.w15s25` and not an errand room, so it is, and the scout confirmed
`ColonyView.Refused` is empty for the declared colony. Any *new* test written for
this — for instance one pinning that a hand-laid road on the single Seat withholds the
container — belongs in `tests/Core.Tests/Decide/OutpostHaulTests.fs` (container,
hauler quota) per `docs/agents/orchestration.md` § Where a new Decide test goes, and
never in a file named after this ticket.
