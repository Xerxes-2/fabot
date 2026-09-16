# The second wave — W14S28, W15S29, W11S28, measured with the repo's own code over the committed captures

Date: **2026-09-16**, the same day `w15s27-outpost.md` landed W15S27 on W15S28.
Three candidates, three different questions: **W14S28** is a reassignment (it is
written in `Colonies.fs` already and declared by nobody), **W15S29** is new for
W15S28, **W11S28** is new for W12S28. The user wants all three at once; this
document prices the *combination* as well as the rooms, and the two things that
decide it — the cumulative cost and the CPU — are reported first.

**No API call was made.** Everything below is the repo's own functions run over
captures already in the tree (`tests/Core.Tests/rooms/W14S28.room`, `W15S29.room`,
`W11S28.room`, and the three homes' plus the chain rooms'), driven from a
temporary Expecto file `tests/Core.Tests/Wave2ScoutTests.fs` — seven cases, run as
`dotnet test --filter "FullyQualifiedName~wave2" --logger "console;verbosity=detailed"`,
raw stdout in `/tmp/scout/wave2.log` (351 `SCOUT` lines). **The file has been
deleted and un-registered from `Core.Tests.fsproj`.** Method copied from
`w15s27-outpost.md`, which is this document's template in every section.

**The capture the brief was unsure of exists.** `tests/Core.Tests/rooms/W15S29.room`
and `W11S28.room` are both committed and complete — terrain, border ring, one
source, a controller and two minerals each — so nothing below is measured short
for want of one, and `scripts/capture-room.mjs` was not run and no tracked file
was added.

| Gate | Result |
|---|---|
| `npm run format:check` | clean |
| `dotnet build` | clean, 0 warnings |
| `dotnet test` | **1388 / 1388** with the scout file gone; **1395** with it present |

Those three gates were taken over a **pristine checkout of `main` (`d33c1374`)** at
`/tmp/fabot-scout`, not in the shared working copy: while this work was running,
another agent's change to `src/Core/Types/Views.fs` and six test fixtures (#354,
the `Reactors` field) was live in the tree and did not build. Nothing in this
document depends on that change, and nothing in this document touched a tracked
file — the scout ran against `d33c1374` and the numbers are `d33c1374`'s.

Functions the numbers come from — nothing below is hand-arithmetic unless it says so:
`Declaration.withinHopBudget` / `Declaration.routable`, `Outpost.roomsProjected`,
`Errand.roomsProjected` (`Types/Colonies.fs`), `RoomName.hopsBetween` / `routesBy` /
`transitBetween`, `Seam.bandBy` / `Seam.joinedBy` (`Types/Geometry.fs`),
`World.ringWalkable` / `World.groundWalkable` (`Types/Sightings.fs`),
`ColonyView.ofWorld` (`Types/Views.fs`), `Atlas.seams` / `routes` / `seats` /
`seatTilesOf` / `seamWalkTicks` / `haulRoundTripTicks` / `castWalkTicks` /
`workArea` / `positionOf` (`Atlas.fs`), `Decide.decide` and its `Quotas`
(`Decide/Entry.fs`), `Quota.haulerDemandOf` through those Quotas (`Decide/Quota.fs`),
`Planner.planTasks` + `Pool.planPool` / `Pool.priorityOfTier` / `Pool.siteOrder`
(`Decide/Planner.fs`, `Decide/Pool.fs`), `Layout.planLayout` (`Decide/Layout.fs`),
`Bodies.bodyFor` / `bodyCost` / `patternTable` (`Decide/Bodies.fs`),
`Keepers.centresIn`, `Tuning.keeperMargin` (`Types/Keepers.fs`, `Types/Rules.fs`),
and `scripts/profile.mjs` for §10.

---

## Summary — the cumulative cost and the CPU first, because they decide it

- **C1. All three at once is the 4,810 failure again, on the smaller colony.**
  Two of the three land on W15S28, and the third lands on the poorest home we
  have. Measured with `Quota.haulerDemandOf` through `Decide.decide`, holding the
  rooms reserved and the outpost containers standing:

  | W15S28 declares | colony haul demand | hauler | anchor | reserver | **target head count** |
  |---|---|---|---|---|---|
  | nothing | 680 | 1 | 2 | 1 | **6** |
  | `[W15S27]` — **today** | 2,000 | 2 | 3 | 2 | **9** |
  | `[W15S27; W15S29]` | 3,170 | 3 | 4 | 3 | **12** |
  | `[W15S27; W14S28]` | 3,570 | 3 | 4 | 3 | **12** |
  | **`[W15S27; W15S29; W14S28]`** | **4,740** | **4** | **5** | **4** | **16** |

  The W13S28 entry in `Colonies.fs` records the live shape that took W14S28 out of
  the list a week ago: *"haul demand 2,790 → 4,810 the tick it was declared, the
  hauler row 2 → 4 … the worker row stood at one of five"*. **4,740 against a
  1,500-unit load, and a head count of 16 where the colony fields 6, is that shape
  inside 1.5%** — on a colony at RCL6 with a 2,300 bank and one
  spawn, where the colony that broke under it was RCL7 with a 5,300 bank and
  625,402 in the storage.
- **C2. What breaks first is the spawn, and it is arithmetic.** One spawn is three
  ticks a part and a body lives `Engine.creepLifetime` = 1,500. Priced off
  `Bodies.bodyFor` / `bodyCost` at each home's own bank (`patternTable` read by
  row name):

  | W15S28 declares | crew parts | **spawn ticks per 1,500** | crew energy | e/tick to stand still |
  |---|---|---|---|---|
  | nothing | 137 | 411 (27%) | 10,200 | 6.80 |
  | `[W15S27]` today | 192 | 576 (38%) | 15,100 | 10.07 |
  | two rooms | 251 | 753 (50%) | 20,000 | 13.33 |
  | **three rooms** | **345** | **1,035 (69%)** | **27,200** | **18.13** |

  **69% of one spawn's whole uptime spent replacing the crew**, before a single
  body is cast for a site, a raid or a claim — and every tick the spawn is busy is
  a tick the extensions are draining rather than filling, which is exactly how the
  live failure reads (*"every body was cast at a 1,365 bank instead of 2,300"*).
  The same table for W13S28 taking W14S28 **back** is worse still: 921 → **1,137
  spawn ticks (76%)** and 23.77 e/tick.
- **C3. So: not three. At most one, and the colony decides which.** W15S28 can
  carry exactly one more declaration (576 → 753 spawn ticks, 9 → 12 bodies);
  W12S28 can carry one (708 → 921); W13S28 can carry none. The only combination
  the numbers allow this week is **one room**, and §11 says which.
- **CPU. Every W15S28 declaration costs about 40% of that colony's `decide`, and
  we are over ADR 0041's trigger already.** `npm run profile`, 100 ticks,
  interleaved runs with the declaration moved in `Colony.declared` between rounds
  (`fourth-colony.md` §10's method), `--scenario reactor` (which *is* W15S28 at
  RCL6 with the W15S25 errand):

  | W15S28's list | `decide`, ms/tick | whole tick, median ms |
  |---|---|---|
  | `[W15S27]` (baseline) | 3.84, 3.94 | 5.01, 5.04, 5.05, 5.89, 6.22 |
  | `+ W15S29` | 5.30, 5.85 | 6.34, 6.57, 6.68, 6.82, 7.31 |
  | `+ W14S28` | 5.52, 5.96 | 6.55, 6.61, 6.99, 7.40, 7.73 |
  | `+ both` | 6.08, 6.07 | 7.19, 7.51, 7.51, 7.85 |

  The intervals do not overlap: **one more W15S28 room is +1.5–2.0 ms of `decide`
  (+40–50%)**, two are +2.2 ms (+57%). Live we stand at a 100-tick mean of ~58 ms
  with a max of 104 ms, against ADR 0041's revisit trigger of *mean > 50 or any
  tick > 80* — **already firing, with #332 and #353 open against it**. A +40%
  charge on the third colony's `decide` is not a charge this bot has the room to
  pay, and that is a legitimate "no" on its own.
- **CPU, the other direction: W11S28 is the cheap one, and measurably so.**
  `--scenario outpost --level 7` is W12S28 at its live level; three interleaved
  rounds: `decide` **2.58 / 2.87 / 3.07** without W11S28 against **3.24 / 3.25 /
  4.12** with it — non-overlapping, but **+0.7 ms (+25%)** and not +2. The reason
  is structural and worth keeping: **W11S28's terrain layer is already in the
  world**, because `Outpost.roomsProjected` puts it in W13S28's scan set as a
  transit room of the W11S29 chain (measured: `W13S28 → W13S29 W11S29 W12S28
  W12S29 W11S28`). Declaring it adds W12S28's own projection, Atlas and pool and
  no new grid.
- **The terminal is not starved by the outpost, and the measurement says so.**
  #349's placement lands at **`W12S28 11,41`** (`Layout.planLayout`,
  `Tuning.TerminalLevel = 6`). Pooled beside W11S28's container site over the same
  tick: the terminal site is **priority 19** (`Surplus` = 20, one home rung up per
  #234) and both outpost container sites are **priority 0** (`Feeding`). So the
  container outranks the terminal — but it outranked it *before* this declaration
  too (W12S27's site reads 0 in the same run), the terminal site is **not** in
  `Pool.siteOrder`'s queue at all (`isOutpostSite` is a not-home test), and what
  the container costs the terminal is one container's build time — **5,000 progress
  against the terminal's 100,000, one twentieth**. What the declaration really
  does to the terminal is pay for it: +6.25 energy a tick net (§7), which is the
  terminal's whole 100,000 in about 16,000 ticks against a ~1,500-tick payback on
  the outpost's own setup.
- **Q1, admissibility.** All four pairs pass, each by exactly one chain each way,
  and `transitBetween` is empty for every one of them — they are all **one hop**.
  (The brief's "W14S28 … 2 from W13S28" is wrong: `RoomName.hopsBetween` answers
  **1**, and `multihop-outposts.md`'s survey row says the same.) Bands, masked and
  raw alike: W15S28↔W14S28 **29**, W13S28↔W14S28 **21**, W15S28↔W15S29 **26**,
  **W12S28↔W11S28 just 2** — the tiles `49,31` and `49,32`, and that is the whole
  door.
- **Q2, the rock and the pick.** One source each. W14S28 `6,8`, **3** Seats, pick
  `6,9`; W15S29 `18,20`, **5** Seats, pick `17,19`; W11S28 `33,15`, **one** Seat,
  pick `32,14` — W15S27's trap again, and `multihop-outposts.md` §4.3 already
  flagged this room for it (*"一只 anchor 死了，接班的那只要等这只的尸体清掉"*).
- **Q3, the haul.** Dearest-sink round trips (`Atlas.haulRoundTripTicks`, the
  hauler cast at the home's own bank): **W14S28 → W15S28 157**, **W14S28 → W13S28
  204**, **W15S29 → W15S28 117**, **W11S28 → W12S28 210**. So W15S29 is the
  cheapest room of the three by a street, and the W14S28 reassignment saves 470
  ticks of demand a cycle by moving the room from W13S28 to W15S28.
- **Q4, the reservers.** `castWalkTicks [Claim; Move]` from each home's live spawn
  tile: **W14S28 81** from W15S28 and **68** from W13S28; **W15S29 53**; **W11S28
  51**. Work Areas of **2**, **1** and **3** tiles — W15S29's controller has
  exactly one tile a reserver may stand on.
- **Q5, keepers.** Nothing is anywhere near one. `Keepers.centresIn` is `[]` for
  all three candidate rooms and for every neighbour of theirs a body could reach;
  `Tuning.keeperMargin` = 6 and the nearest declared keeper room, W15S26, is two
  crossings from the nearest of these three. **No body of ours lands inside the
  margin.**
- **Q6, Thorium.** W14S28's **d3 22,000 at `2,29`** is worth nothing to a
  reservation: `Layout.planLayout` emits an `Extractor` only for a controller this
  colony **owns**, and the Layout runs on the home room alone — measured over the
  declared world, `decide` places `Terminal / Tower / Extension / Extractor /
  Rampart` in W15S28 and **only `Container`** in W14S28. Same for W15S29's d1
  3,000 and W11S28's d2 10,000.

---

## 1. Is each admissible, and how wide is the door?

`Declaration.routable` over `World.ringWalkable` / `World.groundWalkable` built
from the committed captures — the shell's own predicate through
`Seam.joinedBy`, the shape `RoomOutpostTests` uses — and `Atlas.seams` for the
bands. ADR 0062's both-side rule is asked of every pair.

| Pair | hops | routable out / back | chains out / back | `transitBetween` | band out / back (masked) | raw band |
|---|---|---|---|---|---|---|
| W15S28 → W14S28 | **1** | true / true | 1 / 1 | `[]` | **29 / 29** | 29 / 29 |
| W13S28 → W14S28 | **1** | true / true | 1 / 1 | `[]` | **21 / 21** | 21 / 21 |
| W15S28 → W15S29 | **1** | true / true | 1 / 1 | `[]` | **26 / 26** | 26 / 26 |
| W12S28 → W11S28 | **1** | true / true | 1 / 1 | `[]` | **2 / 2** | 2 / 2 |
| *W15S28 → W15S27, for scale* | 1 | true / true | 1 / 1 | `[]` | 20 / 20 | 20 / 20 |

Every chain is `[[home; room]]` in both directions, so ADR 0059's k-chain pricing
costs all four declarations nothing beyond one room's grids, and the keeper mask
takes no tile at any of these borders (masked = raw, everywhere).

Three readings that are not in the table:

- **W12S28 ↔ W11S28 is two tiles**, `49,31 → 0,31` and `49,32 → 0,32`. That
  reproduces `multihop-outposts.md`'s narrow-seam census (*"`W12S28|W11S28` 只有 2
  格（y 31–32）"*) tile for tile, and it is the first cross-validation in this
  document. Two tiles is a door a hauler, a reserver, an anchor and any worker the
  budget sends all share — and the room that a raid would come through.
- **W11S28 has exactly one door.** Its other three borders answer `false` in both
  directions through the same predicate: W11S27 (**false**), W10S28 (**false**,
  the sealed highway column `fourth-colony.md` §3 measured), W11S29 (**false**).
  So the 2-tile band is not one of several ways in; it is the only one.
- **The other two are better connected.** W15S29 opens on W15S28 and W14S29 (both
  `true`) and is walled toward W15S30 and W16S29; W14S28 opens on W13S28, W15S28
  and W14S29 and is walled toward W14S27. For the defensive argument that means
  W11S28 is the cheapest frontage and the tightest bottleneck at once.

## 2. The rocks, the Seats and the container picks

The homes are grown out of their own Layouts the way `w15s27-outpost.md` §2 grows
W15S28 — stand every Road, Storage and Container `decide` plans and ask again
until the plan stops moving — from each colony's **live** spawn tile and level
(ADR 0064: W15S28 `18,30` RCL6, W13S28 `16,12` RCL7, W12S28 `12,40` RCL7).
The road counts that fall out (**57**, **29**, **25**) reproduce ADR 0064's own
table for those three spawns, which is the second cross-validation.

| Reading | W14S28 (from W15S28) | W14S28 (from W13S28) | W15S29 | W11S28 |
|---|---|---|---|---|
| source id / tile | `…3191f9` `6,8` | same | `…319017` `18,20` | `…3195f1` `33,15` |
| `Atlas.seats` | **3** | 3 | **5** | **1** |
| Seat tiles | `6,9` `7,8` `7,9` | same | `17,19`…`19,21` | **`32,14` and nothing else** |
| container pick (`decide`) | **`6,9`** | `6,9` | **`17,19`** | **`32,14`** |
| pick's `seamWalkTicks` home | **13** | **51** | **19** | **32** |
| hauler cast at the bank | 45 parts, 1,500 | 48 parts, 1,600 | 45 / 1,500 | 48 / 1,600 |
| round trip → spawn cluster | 156 | 193 | 111 | 200 |
| round trip → Storage | 157 | 191 | 113 | 203 |
| round trip → controller buffer | 150 | **204** | **117** | **210** |
| **demand, reserved** (`haulerDemandOf`) | **1,570** | **2,040** | **1,170** | **2,100** |
| demand, neutral † | **785** | 1,020 | **585** | 1,050 |
| refused? (`ColonyView.Refused`) | `[]` | `[]` | `[]` | `[]` |

† The neutral row was **measured** for the two rooms W15S28 would hold (785 and
585, beside W15S27's 660, in the all-three-neutral reading of §3); for the other
two columns it is the reserved figure halved, which is what the output rate does
(`heldOutputPerTick` 10 against `neutralOutputPerTick` 5) and what the measured
pairs confirm exactly. Derived, not read.

Three things follow:

1. **W15S29 is the cheapest room of the three and W11S28 the dearest**, and the
   ordering is the round trip and nothing else — the reserver and the anchor cost
   the same everywhere.
2. **The reassignment is worth 470 ticks a cycle.** W14S28 priced from W15S28 is
   1,570 of demand; from W13S28 it is 2,040. The pick is the same tile either way
   (`6,9`); what moves is the walk home — 13 ticks to the W15S28 crossing against
   51 to the W13S28 one.
3. **`multihop-outposts.md` §4.1's round-trip column is understated by about a
   third, and this document's numbers are the reason.** Its rows read *W11S28 …
   单程 66 · 往返 146* and *W14S28 … 单程 65 · 往返 144*. Measured here, the
   **unloaded one-way walk is 68 and 66** — its one-way column is reproduced to
   within two ticks — but `Atlas.haulRoundTripTicks` prices the loaded leg at real
   fatigue (a hauler's Carry parts outnumber its Move two to one, so the loaded
   tile is 2 ticks and the empty one 1) and answers **210** and **204**. That is
   `2 × unloaded` against `3 × unloaded`, and it is why its economics table's
   hauler counts read low. The conclusions of that table survive because the
   hauler *row* steps by 1 either way (below); its round-trip numbers do not.

## 3. What the declaration does to the rows

Same standing world, same tick, the only difference being what `Outposts` names;
rooms reserved, outpost containers standing (ADR 0042's switch reads standing
containers, #205).

**W15S28 (RCL6, bank 2,300, hauler load 1,500, home demand 680):**

| Declares | demand | hauler | anchor | reserver | upgrader | worker | target |
|---|---|---|---|---|---|---|---|
| — | 680 | 1 | 2 | 1 | 0 | 2 | 6 |
| `[27]` today | 2,000 | 2 | 3 | 2 | 1 | 1 | 9 |
| `[27; 29]` | 3,170 | 3 | 4 | 3 | 1 | 1 | 12 |
| `[27; 14]` | 3,570 | 3 | 4 | 3 | 1 | 1 | 12 |
| `[27; 29; 14]` | **4,740** | **4** | 5 | 4 | 1 | 2 | **16** |
| `[27; 29; 14]`, all three neutral | 2,710 | 2 | 5 | 4 | 1 | 1 | 13 |

**W13S28 (RCL7, bank 5,300, load 1,600):** `[W13S29; W11S29]` = 3,850 / hauler 3 /
target **11**; add W14S28 → **5,890 / hauler 4 / target 14**.

**W12S28 (RCL7, bank 5,300, load 1,600):** `[W12S27]` = 1,710 / hauler 2 / target
**8**; add W11S28 → **3,810 / hauler 3 / target 11**.

Read plainly:

- **The W15S27 row reproduces `w15s27-outpost.md` exactly** — a check on this
  document's *harness* rather than on the map, and the one that matters most:
  the same pick `13,29`, the same
  132/131/130 sink legs, the same 1,320 of demand, the same 2,000 colony total
  against a 1,500 load, and the same `hauler 1 → 2, anchor 2 → 3, target 6 → 9`.
- **Every one of these declarations buys exactly one hauler, one anchor and one
  reserver.** The marginal hauler is the largest line in every bill, as it was for
  W15S27.
- **The W13S28 number is the live failure, reproduced.** The `Colonies.fs` entry
  records *2,790 → 4,810* on the tick W14S28 was declared, a step of **+2,020**.
  Modelled here from the grown room the step is **+2,040** (3,850 → 5,890). The
  absolute levels differ because the live colony that day held W13S29 and a
  nursery rather than W13S29 and W11S29, but the **marginal** cost of that room
  from that home is reproduced to within 1%. That is the third cross-validation,
  and it is the one that makes the rest of this document's marginals trustworthy.
- **And the answer to the brief's question is: yes, it recurs, one third smaller.**
  Held by W15S28 the step is **+1,570** rather than +2,040, and the colony it lands
  on has a 1,500 load rather than 1,600 — so the hauler row goes 2 → 3 where
  W13S28's went 3 → 4 (live: 2 → 4). It is the same shape at three-quarters the
  size, on a colony a level smaller.

## 4. The reservers

| Reading | W14S28 ← W15S28 | W14S28 ← W13S28 | W15S29 | W11S28 | *W15S27, for scale* |
|---|---|---|---|---|---|
| controller | `…3191fa` `22,15` | same | `…319018` `12,34` | `…3195f2` `8,16` | `6,9` |
| `Atlas.workArea (Reserve _)` | **2** tiles | 2 | **1** tile | **3** tiles | 3 |
| `castWalkTicks [Claim; Move]` | **81** | **68** | **53** | **51** | 89 |
| same, the row's body at the bank | 81 | 68 | 53 | 51 | 89 |
| amortised `650 / (600 − walk)` | **1.25** | **1.22** | **1.19** | **1.18** | 1.27 |

Cadence is ADR 0042's, unmoved: `Engine.claimLifetime = 600`,
`Engine.reservationCap = 5000`, `heldOutputPerTick = 10` against
`neutralOutputPerTick = 5`. CLAIM/MOVE is fatigue parity, so the row's real cast
walks the same as the two-part probe — measured, not assumed.

**One warning that belongs to W15S29 alone: its controller has a single tile.**
`workArea (Reserve …)` answers one tile for `12,34`, where W15S27's answers three
and W11S28's three. A body standing on it — anybody's — is a reserver that cannot
reach its own controller that tick. It is the `Seats = 1` failure one room over,
in the row that decides whether the rock is worth 10 or 5.

## 5. The bodies, and what one spawn can carry

`Bodies.bodyFor` at each home's own bank, each row's pattern found in
`Bodies.patternTable` by the name the quota row carries; 3 ticks a part is the
engine's spawn rate and `Engine.creepLifetime` = 1,500.

| Body | at bank 2,300 (W15S28) | at bank 5,300 (W12S28, W13S28) |
|---|---|---|
| hauler | 45 parts, 2,250 e, 1,500 capacity | 48 parts, 2,400 e, 1,600 |
| anchor | 8 parts, 700 e | 8 parts, 700 e |
| reserver (bank ceiling) | 6 parts, 1,950 e | 16 parts, 5,200 e |
| worker | 35 parts, 2,300 e | 50 parts, 3,300 e |

| Colony and list | crew parts | spawn ticks / 1,500 | crew energy | e/tick |
|---|---|---|---|---|
| W15S28 — | 137 | 411 | 10,200 | 6.80 |
| W15S28 `[27]` | 192 | 576 | 15,100 | 10.07 |
| W15S28 two rooms | 251 | 753 | 20,000 | 13.33 |
| W15S28 three rooms | **345** | **1,035** | 27,200 | **18.13** |
| W13S28 `[29; 11]` | 307 | 921 | 27,350 | 18.23 |
| W13S28 `+ W14S28` | **379** | **1,137** | 35,650 | **23.77** |
| W12S28 `[27]` | 236 | 708 | 18,700 | 12.47 |
| W12S28 `+ W11S28` | 307 | 921 | 27,350 | 18.23 |

The reserver line is priced at the **bank ceiling** and so overstates: the row's
real cast is `min(bank, claims × 650)` with `claims = ceil((5000 − ticksToEnd) /
600)`, which in the steady state is one block. Take the whole reserver row out and
W15S28's three-room crew is still 987 spawn ticks of 1,500. The conclusion does
not turn on it.

## 6. W12S28's terminal site against W11S28's container site

#349 landed the placement this hour; the question is whether an outpost declared
in the same week starves it. The experiment is one world — W12S28 grown to its own
plan at RCL7, the terminal site standing, the outpost container sites standing —
read twice, once with `Outposts = [W12S27]` and once with `[W12S27; W11S28]`.

| Reading | terminal alone | terminal + W11S28 |
|---|---|---|
| `Layout` terminal pick | **`W12S28 11,41`** | `W12S28 11,41` (unmoved) |
| terminal site, pooled | **priority 19** (`Surplus` 20, one home rung, #234) | **19** |
| W12S27 container site | **priority 0** (`Feeding`) | 0 |
| W11S28 container site | — | **priority 0** |
| home Upgrade, for scale | −10 | −10 |
| rows | reserver 1, anchor 3, hauler 1, worker 2 | reserver 2, anchor 4, hauler 1, worker 2 |
| target | 7 | 9 |

**The answer is no, and the mechanism is worth stating exactly.**

- The terminal site is a **home** site, so it is not in `Pool.siteOrder`'s queue at
  all — `isOutpostSite` is a not-home-and-not-borrowed test (`Decide/Pool.fs:521`),
  and `Tuning.OutpostBuilders = 2` rations only rooms the colony *mines*. The
  outpost container never truncates the terminal out of anything, because the
  terminal was never in the list the budget truncates.
- What the container does do is outrank it, by tier and not by budget: `Feeding`
  0 against `Surplus` 19, which travel cost cannot undo (a rank the whole tier
  shares, #234). But **that was already true with W12S27's site standing**, and
  what the new declaration adds to the queue ahead of the terminal is **5,000
  progress against the terminal's 100,000** — one twentieth of it.
- The rows that grow are the reserver and the anchor, not the worker: **worker
  stays 2** in both readings. The builders' budget is untouched; what the
  declaration buys is bodies for the rock.
- And the direction of the energy is the other way round from the fear.
  The declaration's net is **+6.25 a tick** (§7) against a one-off of about 9,000
  energy (container 5,000 + the first hauler 2,400 + an anchor 700 + the reserver's
  first block), so it pays for itself in roughly 1,500 ticks and then pays the
  terminal's 100,000 in about 16,000 more. On the poorest of the three rooms —
  storage 3,746 at the last live look — an outpost is how the terminal gets built,
  not what stops it.

## 7. The energy account

Same shape and the same constants as `w15s27-outpost.md` §2 and
`multihop-outposts.md` §0.2: gross is `Engine.heldOutputPerTick` as the demand
row's `output` reads it, the anchor's 0.47 is carried and **not re-measured**, the
reserver is amortised at `650 / (600 − walk)` off §4's walks, the hauler is the
**measured** row step (+1 everywhere) times the cast at that home's bank over
1,500, and container decay 0.5 is `remote-mining.md` §1.3's **unverified**
constant.

| Room | holder | gross | anchor | reserver | hauler | decay | **net e/tick** |
|---|---|---|---|---|---|---|---|
| **W15S29** | W15S28 | +10.00 | −0.47 | −1.19 | −1.50 | −0.50 | **+6.34** |
| **W14S28** | W15S28 | +10.00 | −0.47 | −1.25 | −1.50 | −0.50 | **+6.28** |
| **W11S28** | W12S28 | +10.00 | −0.47 | −1.18 | −1.60 | −0.50 | **+6.25** |
| **W14S28** | W13S28 | +10.00 | −0.47 | −1.22 | −1.60 | −0.50 | **+6.21** |
| *W15S27, landed* | W15S28 | +10.00 | −0.47 | −1.27 | −1.50 | −0.50 | *+6.26* |

All four sit inside `multihop-outposts.md` §4's one-hop band of 6.31–6.36 or
within a tenth of it, and its own predictions for two of these rooms (W11S28
**6.35**, W14S28 **6.31**) are reproduced to within 0.10 — the fourth
cross-validation, and the one that says the older survey's *ordering* is still
good even though its round trips are not (§2).

**Which is the point that decides this document: on energy alone all three are
the same room.** A per-room economic argument cannot separate them, so the
separation has to come from the cumulative cost (§C1–C3), the CPU (§10) and the
geometry (§1, §4).

## 8. Hazards

| Reading | Function | W14S28 | W15S29 | W11S28 |
|---|---|---|---|---|
| keeper centres in the room | `Keepers.centresIn` | **`[]`** | **`[]`** | **`[]`** |
| keeper centres in each reachable neighbour | same | 0 (W13S28, W15S28, W14S29) | 0 (W15S28, W14S29) | 0 (its one door, W12S28) |
| `Tuning.keeperMargin` | `Tuning.keeperMargin` | 6 | 6 | 6 |
| invader cores / hostiles in the view | `ColonyView` | `[]` — **the fixture's silence** | `[]` | `[]` |

**No body of ours lands inside `Rules.keeperMargin` of a Source Keeper room under
any of the three declarations**, and unlike W15S27 the question is not even close:
the nearest declared keeper room, W15S26, is two crossings from the nearest of
these three rooms, and `Keepers.centres` declares no other. As in the template,
the empty `InvaderCores` is the capture format's silence and not evidence —
`capture-room.mjs` records fixed furniture — so the standing threat reading is
`fourth-colony.md`'s t491,507 sweep (0 cores in 99 rooms within 6 hops) and
nothing here re-measures it.

Two structural notes that are code and not weather:

- **W11S28's single 2-tile door is the whole of its defence and the whole of its
  logistics.** Every hauler round trip, every reserver, every anchor and any
  builder the budget sends passes `49,31` or `49,32`. ADR 0056's guard is hired
  per raided declared outpost and would have to come through the same two tiles.
- **W14S28 is in nobody's projection today.** Measured: `Outpost.roomsProjected`
  gives W13S28 `[W13S28; W13S29; W11S29; W12S28; W12S29; W11S28]` and W15S28
  `[W15S28; W15S27; W15S25; W15S26]` — the room is dark to both. Its container,
  which the `Colonies.fs` entry says *"is built and will decay"*, is unobservable
  from the captures; if it is still standing, the room's 5,000-energy switch is
  already paid for, and if it is not, it is not. **Unverified either way.**

## 9. The Thorium, in one line each (as `w15s27-outpost.md` §7 states it)

**A reserved outpost can never extract any of it.** `Layout.planLayout` emits an
`Extractor` only when the controller's level reaches `Tuning.ExtractorLevel` = 6
for a controller this colony **owns**, and the Layout runs on the home room alone
(ADR 0042: *"no roads, and no Layout"*). Measured over the declared world,
`decide` places `Terminal / Tower / Extension / Extractor / Rampart` in W15S28 and
**only `Container`** in W14S28; the scout's hard assertion is that no `Extractor`
is ever placed in the outpost, and it is green.

So: **W14S28's d3 22,000 at `2,29`** (`6a901a43b8684d00083388cd`), **W15S29's d1
3,000 at `25,2`** (`6a901a43b8684d0008338892`) and **W11S28's d2 10,000 at
`40,17`** (`6a901a44b8684d00083389d5`) are each a **claim** argument for some
future colony, and contribute exactly nothing to these declarations' arithmetic.
The tiles and ids are the captures'; the densities and amounts are
`fourth-colony.md` §2's live read (a capture records the mineral's id and tile and
never its amount).

## 10. CPU, measured

`npm run profile`, 100 ticks per run, interleaved (a rebuild between every run, the
declaration moved in `Colony.declared` and nothing else), on a copy of the tree at
`/tmp/fabot-cpu` so that no tracked file in the repo was modified. Method from
`fourth-colony.md` §10: per-colony `decide` ms beside the whole tick, several
rounds, and the finding is an interval and not a point.

**`--scenario reactor`** — which *is* the colony in question: W15S28 at RCL6, the
mine dug, the W15S25 errand declared over the masked keeper room.

| W15S28's list | `decide` ms/tick (rounds 4, 5) | whole-tick median (rounds 1–5) |
|---|---|---|
| `[W15S27]` | **3.84, 3.94** | 5.89, 6.22, 5.04, 5.05, 5.01 |
| `[W15S27; W15S29]` | **5.30, 5.85** | 6.34, 7.31, 6.68, 6.57, 6.82 |
| `[W15S27; W14S28]` | **5.52, 5.96** | 6.55, 6.99, 7.73, 6.61, 7.40 |
| `[W15S27; W15S29; W14S28]` | **6.08, 6.07** | 7.19, 7.85, —, 7.51, 7.51 |

The `decide` intervals are disjoint at every step: **+1.5 to +2.0 ms for one more
room (+40–50%), +2.2 ms for two (+57%)**, and the whole-tick medians move with
them (~5.0–6.2 → ~6.3–7.3 → ~6.6–7.7 → ~7.2–7.9).

**`--scenario outpost --level 7`** — W12S28 at its live level, its W12S27 outpost
standing:

| W12S28's list | `decide` ms/tick | whole-tick median |
|---|---|---|
| `[W12S27]` | **3.07, 2.87, 2.58** | 3.94, 3.62, 3.19 |
| `[W12S27; W11S28]` | **3.24, 4.12, 3.25** | 3.96, 4.82, 4.10 |

Disjoint, and small: **+0.7 ms (+25%)**.

**`--scenario pair`** (W12S28 mother at RCL5 with the bootstrapping W13S28) reads
**no separable signal at all** for either W11S28 (mother's `decide` 2.91 / 3.37 /
3.30 against 3.43 / 3.14 / 3.33) or for W13S28 taking W14S28 back (child 1.95 /
2.28 / 2.25 against 2.03 / 1.96 / 1.86). That is a fact about the scenario and not
about the declarations: its mother is pinned at RCL5 (`MOTHER_LEVEL`, `--level`
moves the child) and its child is RCL2, so both colonies carry a fraction of the
creeps and the cross-room candidate Tasks the live rooms do — and #353's finding is
that the cost is `(creeps) × (cross-room candidate Tasks)`. **The `pair` reading is
reported and not used.**

Three things to take from this section:

1. **The cost is not the terrain layer.** W11S28's grid is already in the world
   (W13S28's transit rectangle, §8), and it is still +0.7 ms — so what a
   declaration buys is the projection entry, the Atlas's routes and the pool's and
   quota's cross-room work, which is exactly where #353 put the money
   (`matchCreeps` 35.9%, `pricedAcrossInto` 27.2%).
2. **The bigger the colony, the bigger the charge.** The same one-room step is
   +0.7 ms on the RCL7 `outpost` scenario and +1.5–2.0 ms on the RCL6 `reactor`
   scenario, because the latter carries the errand, the mine and the masked keeper
   room. The live rooms are bigger than either.
3. **We are already over the line.** Live: 100-tick mean ~58 ms, max 104 ms,
   against ADR 0041's *mean > 50 or any tick > 80*, with #332 and #353 open. The
   harness says a W15S28 declaration is a **+40% charge on that colony's decide**.
   There is no reading of these numbers in which we can afford two.

## 11. Recommendation

**One of the three, and not this week for two of them.**

| Room | Holder | Verdict |
|---|---|---|
| **W11S28** | W12S28 | **Declare — and declare this one first.** |
| **W15S29** | W15S28 | **Declare later** — first in the queue behind #353/#332. |
| **W14S28** | W15S28 (never back to W13S28) | **Declare later** — second in that queue, and only if its container is found standing. |

**Why W11S28 and not the other two, when it has the worst haul of the three.**
Every one of these rooms is worth about 6.3 energy a tick (§7), so the question is
never which room is best; it is which colony can pay. Measured:

- **CPU.** W11S28 costs **+0.7 ms** of `decide` on the colony that takes it; either
  W15S28 room costs **+1.5–2.0 ms**, on the colony whose tick is already the
  dearest we run, while ADR 0041's revisit trigger is firing (§10). W11S28's
  terrain is in the world already; the W15S28 rooms' is not.
- **The spawn.** W12S28 goes from 708 spawn ticks of 1,500 to **921 (61%)**;
  W15S28 goes from 576 to **753 (50%)** for one room and 1,035 (69%) for two, at
  RCL6 with a 2,300 bank and having taken W15S27 *today*. The colony that just
  swallowed a declaration should not be handed the next one.
- **The terminal, which was the reason to fear this room, argues for it** (§6):
  the outpost container is one twentieth of the terminal's progress, the worker
  row does not move, and +6.25 a tick is how a room with 3,746 in the storage
  eventually pays 100,000 for a terminal.
- **What W11S28 costs and we should say out loud:** a **single Seat** (`32,14`) —
  so a dead anchor waits for a corpse before the next one can stand, which is
  `multihop-outposts.md` §4.3's own warning — and a **two-tile door**. Both are
  real; neither is a per-tick cost, and the Seat is a reason to keep hands off that
  tile, exactly as `13,29` was in W15S27.

**Why W15S29 waits, and what it waits for.** It is the best *room* of the three —
cheapest haul (1,170), most Seats (5), a 26-tile band, net 6.34 — and it should be
the next declaration this bot makes. It waits on two things and neither is about
the room: **#353/#332** (a +40% charge on W15S28's `decide` while the live tick
mean is 58 ms is unaffordable) and **W15S28's own digestion of W15S27**, whose
container, second hauler and third anchor have not been stood yet. Its one
liability to check on the day: the controller's Work Area is a **single tile**
(§4).

**Why W14S28 waits, and why it must not go back to W13S28.** The `Colonies.fs`
entry's condition — *"the room goes back in the list the day W15S28 stands its own
spawn"* — has been met, but the entry's **reason** has not expired: it was
withdrawn because *"a single spawn cannot raise a nursery two hops out and take on
a second outpost at once"*, and W13S28 is in that state again today, raising
W11S29 three crossings out. Measured, taking W14S28 back would put W13S28 at
**5,890 demand, 4 haulers, a 14-body target and 1,137 spawn ticks of 1,500 (76%)** —
worse than the 4,810 that removed it. Held by W15S28 instead it is 1,570 of
demand rather than 2,040, a 13-tick walk to the crossing rather than 51, and
+6.28 a tick. **So the reassignment is right and the timing is not:** it is
W15S28's room, behind W15S29, and worth re-checking on the day for a standing
container that would make it the cheapest of the three to switch on.

### The exact lines, for the declaration this document recommends

`src/Core/Types/Colonies.fs`, beside the other `Outpost` values (ids and tiles are
the capture's, `tests/Core.Tests/rooms/W11S28.room`, in the capture's own order):

```fsharp
    /// W12S28's west outpost, declared 2026-09-16 off
    /// `docs/research/outpost-wave-2.md`, which is the wave-2 survey's only
    /// "declare now": one hop and one chain each way, one source at a 210-tick
    /// round trip, ≈6.25 energy a tick net — and the cheapest tick of the three
    /// candidates, because this room's terrain layer is already in the world as
    /// a transit room of W13S28's W11S29 chain, so what the declaration adds is
    /// this colony's own projection and not a grid (+0.7 ms of `decide`, against
    /// +1.5–2.0 for either W15S28 candidate, while ADR 0041's revisit trigger is
    /// firing — #332, #353).
    ///
    /// Two liabilities, both geometry. Its band is **two tiles**, `49,31` and
    /// `49,32`, and it is the room's only door: W11S27, W10S28 and W11S29 all
    /// answer `false` both ways through `World.ringWalkable`. And its rock has a
    /// **single** Seat, `32,14` — a site of any other kind on that tile plans
    /// this room no container at all (ADR 0042 as #244 amends it), and a dead
    /// anchor waits for its own corpse before the next one can stand
    /// (`multihop-outposts.md` §4.3).
    ///
    /// It does **not** starve #349's terminal site at `11,41`: that site is a
    /// home site, so it is not in `Pool.siteOrder`'s outpost queue at all, and
    /// what this declaration puts ahead of it is 5,000 progress against the
    /// terminal's 100,000 — one twentieth — while paying +6.25 a tick toward it.
    let w11s28: Outpost =
        {
            RoomName = "W11S28"
            Sources = [ "6a8caac6dd4872bccd3195f1", { Room = "W11S28"; X = 33; Y = 15 } ]
            Controller = "6a8caac6dd4872bccd3195f2", { Room = "W11S28"; X = 8; Y = 16 }
        }
```

and in `Colony.declared`'s **first** entry:

```fsharp
                Outposts =
                    (Outpost.adr0042 |> List.filter (fun o -> o.RoomName = "W12S27"))
                    @ [ Outpost.w11s28 ]
```

with the comment there (*"W12S27 alone since W13S28 stood its own spawn"*) updated
to say the room now works two.

### And the lines for the two that wait, so the day they come is a one-line day

**W14S28 needs no new value at all** — `Outpost.w14s28` is written and its ids and
tiles are the capture's (checked here). The reassignment is one list membership:
W15S28's `Outposts = [ Outpost.w15s27 ]` becomes
`[ Outpost.w15s27; Outpost.w14s28 ]`, and the W13S28 entry's long paragraph loses
its last sentence (*"the room goes back in the list the day W15S28 stands its own
spawn"*) in favour of the finding above: the room goes to **W15S28**, because from
there it is 1,570 of demand against 2,040 and a 13-tick walk to the crossing
against 51, and W13S28 is raising a nursery again.

**W15S29 needs a value**, and it is the capture's:

```fsharp
    let w15s29: Outpost =
        {
            RoomName = "W15S29"
            Sources = [ "6a8caa95dd4872bccd319017", { Room = "W15S29"; X = 18; Y = 20 } ]
            Controller = "6a8caa95dd4872bccd319018", { Room = "W15S29"; X = 12; Y = 34 }
        }
```

with the warning its doc comment owes a reader: the controller `12,34` has a
**one-tile** `workArea (Reserve …)`, and the room is otherwise the best of the
three — 5 Seats, a 26-tile band, a 117-tick round trip, +6.34 a tick.

### Which tests would move

- **`tests/Core.Tests/Decide/OutpostDeclarationTests.fs` — two assertions, and they
  are the only red ones.** *"the north outpost alone"* pins
  `Colony.outpostsOf Colony.declared "W12S28" |> List.map (…RoomName)` = `["W12S27"]`
  → `["W12S27"; "W11S28"]`, and the case below it pins
  `Outpost.roomsProjected outposts "W12S28"` = `["W12S28"; "W12S27"]` →
  `["W12S28"; "W12S27"; "W11S28"]` (one hop, so `transitBetween` adds nothing).
  Both messages need rewording, not just renumbering.
- **`tests/Core.Tests/RoomOutpostTests.fs` — grows by itself and stays green.** Its
  *"every outpost the live declaration names is its capture's"*, *"a chain of real
  border rings joins every declared outpost to its home"* and the container-pick
  case all read `Colony.declared` and need only `W11S28.room`, which is committed —
  and the scout has already run each of those three questions against this
  declaration by hand (`routable` true both ways, ids and tiles the capture's,
  pick `32,14`).
- **`tests/Core.Tests/ViewTests.fs` — green without edits**: the hop-budget case
  (1 hop), the no-room-declared-as-both case (W11S28 is nobody's home and nobody's
  errand) and `ColonyView.Refused` (measured empty for the declared colony).
- **Nothing for either W15S28 candidate, when their day comes.** No test pins
  W15S28's outpost list by name; both rooms' captures are committed.
- Any *new* test written for this — for instance one pinning that a site on the
  single Seat `32,14` withholds the container, or that a 2-tile band still prices a
  round trip — belongs in `tests/Core.Tests/Decide/OutpostHaulTests.fs` (container,
  hauler quota) per `docs/agents/orchestration.md` § Where a new Decide test goes,
  and never in a file named after this ticket.

## Substitutions

Where the committed captures forced a stand-in. Each is a place where the number
would move on the live server, and the direction is named.

1. **All three homes are grown, not observed.** The captures are terrain and
   furniture only, so each home stands a spawn on its live tile (W15S28 `18,30`,
   W13S28 `16,12`, W12S28 `12,40`, per ADR 0064) at its live level with a **full**
   bank, and `decide` is run eight times standing every Road, Storage and Container
   it plans. The resulting road counts (57 / 29 / 25) reproduce ADR 0064's table
   for those spawns, which is why the substitution is trusted. Live, the rooms
   stand 84 / 116 / 109 roads against those plans (ADR 0064), so **every haul leg
   here could be a few ticks short in either direction**.
2. **The banks are the allowance ceiling, not the live balance.** 2,300 for RCL6
   (300 + 40×50) and 5,300 for RCL7 (300 + 50×100). Live, the W14S28 episode
   records bodies being cast at a **1,365** bank; a colony under load casts smaller
   bodies, which makes the hauler rows in §3 optimistic and §C2's spawn duty
   understated.
3. **No reservation, then one by hand.** The captures know nothing of ownership, so
   each candidate's control entry is written by the fixture as `Unowned` with
   `Reservation = Ours, TicksToEnd = 4000` for the reserved rows; the neutral rows
   are the same world with the reservation taken away.
4. **The outpost containers are stood, not built** (ADR 0042's switch reads
   standing containers, #205's `postsIn` / `standingPostsIn` split). §3 prices the
   steady state and not the window before the switch flips; §6 deliberately does
   the opposite, standing them as **sites**, because the terminal question is about
   that window.
5. **W13S28's list in §3 is today's declaration and not the day W14S28 was
   withdrawn.** It holds W13S29 and W11S29 here; live on 2026-09-10 it held W13S29
   and the W15S28 nursery. The *marginal* +2,040 is the comparable number, not the
   3,850 it is added to.
6. **The reserver's crew cost in §5 is the bank ceiling** (6 parts at 2,300, 16 at
   5,300), where the steady-state cast is one 650-energy block. Overstated, and
   §5 says by how much.
7. **The CPU runs are the harness's colonies, not ours.** `reactor` is W15S28 at
   RCL6 with a seeded fleet; `outpost --level 7` is W12S28 with its own; neither
   has our live creep count, and #353's multiplier is creeps × cross-room Tasks. The
   **ratios** are what this document claims (+40% of a colony's `decide`, +25%),
   never the absolute milliseconds.
8. **No creeps, no traffic, no threats anywhere in §1–§9.** Every reading is a
   quiet tick with `StandDown.none` and no hostiles; `Atlas.walkTicks` and
   `haulRoundTripTicks` are traffic-blind by design (ADR 0029), so the guard row
   reads 0 throughout — which is precisely what a 2-tile door would stop being
   during a raid.

## Not measured

- **The anchor row's 0.47 e/tick and container decay's 0.5.** Both carried from
  `multihop-outposts.md` §0.2 / `remote-mining.md` §1.3, both **unverified** there
  and here. Road maintenance likewise.
- **Whether W14S28's container is still standing.** The `Colonies.fs` entry says it
  was built and would decay; no capture can answer it and no live call was made. It
  is worth one `observe` before that declaration, because a standing container is
  5,000 energy and a whole switch already paid for.
- **The raid tax.** `multihop-outposts.md` §5 measured 1.48 e/tick on W13S29 off a
  live raid log. Nothing in the tree lets that be re-measured for any of these
  three, and the structural argument (§8's door counts) is all this document
  offers. W11S28's two-tile door deserves it most.
- **Whether the hand-laid-site question of `w15s27-outpost.md` §4 applies here at
  all.** It does not for W15S29 or W14S28 (neither is in any colony's scan set
  today, so there is nothing invisible to make visible) and it is W13S28's
  projection and not W12S28's that holds W11S28 — so a human's site in W11S28 is
  invisible to the colony that would build it until this declaration exists. That
  mechanism was **not** re-run for these rooms; it is `w15s27-outpost.md` §4's
  measurement, cited.
- **The Matcher's actual assignments.** §6 reads pooled priorities and row counts,
  not which body picks up which Task on the tick; the claim "the terminal loses one
  container's build time" is arithmetic over site costs (5,000 against 100,000),
  not a simulated build.
- **The live census of any of the three homes.** Levels, banks, standing
  structures, creep counts and spawn queues were not read; everything is the
  Layout's plan at the allowance ceiling.
- **`W14S29`**, the fourth room `Colonies.fs` names as one W15S28 "will want". Not
  priced here; its capture is committed and the same scout would answer it.
- **The live CPU after any of this.** §10's ratios are the harness's. What ADR 0041
  asks for is a live `observe cpu` after a deploy, and the honest sequence is
  #353/#332 first, then one declaration, then that reading.
