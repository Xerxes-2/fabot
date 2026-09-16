# W12S29 as W11S29's first outpost — measured with the repo's own code over the committed capture

Asked on 2026-09-17: the fourth colony can "very conveniently remote W12S29",
*provided the CPU problem is handled*. This is that question answered with the
same functions the two earlier outpost surveys used, over
`tests/Core.Tests/rooms/W12S29.room`, which is committed and complete —
`scripts/capture-room.mjs` was not run and no tracked file was added for it.

**Verdict: declare it.** It is the cheapest haul in the whole outpost programme
and it lands on the colony with the most to gain from one. Two liabilities are
real and neither is a reason to refuse; both are written into the declaration.

## Verified against

- `tests/Core.Tests/rooms/W12S29.room` and `W11S29.room` (committed captures:
  terrain, border ring, one source, a controller, two minerals each).
- The live server at t≈505,000 (`/api/game/room-objects`) for the ids, the
  levels, the extension count and the standing containers.
- `observe quotas --colony W11S29` for the colony's own rows and haul demand.

Functions the numbers come from — nothing here is hand-arithmetic unless it says
so: `RoomName.hopsBetween`, `Atlas.haulRoundTripTicks`, `Atlas.ofView`,
`Decide.Bodies.bodyFor` / `bodyCost` / `reserverPattern` / `haulerPattern`,
`TerrainGrid.tryFind`, `Keepers.centresIn`, and `scripts/profile.mjs` for the
CPU arm. Taken from a scout module deleted in the same commit that wrote this
file.

## The numbers

| | W12S29 for W11S29 | W14S28 for W15S28 (the queued one, for scale) |
|---|---|---|
| hops | **1** | 1 |
| haul round trip, container → home sink | **81 ticks** | 190 ticks |
| Seats at the far source (walkable neighbours) | **1** | 3 |
| controller Work Area | 4 tiles | 2 tiles |
| reserver the home can cast at its live bank | `[Claim; Move]`, 650, **1 CLAIM** (bank 800) | `[Claim; Claim; Claim; Move; Move; Move]`, 1,950 (bank 2,300) |
| hauler the home can cast | 750, 10 Carry (500 capacity) | 2,250, 30 Carry |
| container standing today | none | **none** — and §11 of the wave-2 survey made W14S28 conditional on one |

**81 ticks is less than half the cheapest number in either earlier survey**
(W11S28's was 210, W15S29's haul 1,170 in the survey's own units). The reason is
geometry and it is worth stating: W12S29's source sits at `40,43`, hard against
the border W11S29 shares with it, and W11S29's spawn is at `11,30`
(`w11s29-spawn.md`). The room's one source is nearly on the doorstep.

A 500-capacity hauler over an 81-tick round trip moves **6.2 energy a tick**, so
a reserved source's 10/tick needs about 1.6 haulers — against the colony's
current whole haul demand of **380 over one hauler**. This declaration roughly
doubles the fourth colony's income for one extra hauler and change.

## The two liabilities

**1. The far source has a single Seat.** One walkable neighbour, so one anchor
at a time and a dead one waits for its corpse before the next can stand —
`multihop-outposts.md` §4.3's warning, and the same shape as W11S28's single
Seat at `32,14`. It is a reason to keep hands off that tile, not a reason to
refuse the room.

**2. The reserver W11S29 can afford holds the reservation flat and banks
nothing.** At RCL3 with ten extensions the bank is 800, and `Bodies.bodyFor`
answers `[Claim; Move]` — **one** CLAIM. `reserveController` adds one tick per
action and a reservation decays one a tick, so a single-CLAIM reserver standing
its ground holds the room reserved and never accumulates a buffer: any gap in
its presence decays to zero, and an unreserved source pays
`Engine.neutralOutputPerTick` (5) instead of 10.

So the room's full value arrives with RCL4 — twenty extensions, a 1,300 bank,
ADR 0042's two-CLAIM body — and until then the outpost is worth somewhere
between half and all of its 10/tick, depending on how much of the time the one
reserver is standing on the tile. This is a *declared* shortfall rather than a
surprise: the declaration is still positive at the neutral rate.

## CPU, which is what the request was conditional on

The condition cannot be answered with "solved", so here is the state:

- **Live, 100 ticks at t≈505,000:** mean **65.43 ms**, max **150.93**, bucket at
  its **10,000** cap and net-refilling, `replans` ≤ 1 a tick. ADR 0041's revisit
  trigger (mean > 50, or any tick > 80) **is still firing**, and nothing in this
  document stops it firing.
- **What did change is the marginal price of a room.** Measured the same day on
  today's code (`--scenario reactor --level 7`, 100 ticks, three interleaved
  rounds with a rebuild between each, the declaration moved in
  `Colony.declared` between arms): `decide` **2.95, 2.95, 3.23** without a
  second outpost against **3.51, 3.53, 3.68** with it — **+0.55 ms, +18%,
  intervals not overlapping**. The wave-2 survey measured **+1.5–2.0 ms
  (+40–50%)** for the same declaration before #353 and #358 landed. A room's far
  field is now held across ticks and shares its suffix with the chains already
  priced, so a marginal room costs about a third of what it used to.
- **So the honest form of the answer:** one more room is about **+0.6 ms of a 65
  ms tick, under 1%**, and the engine's own allowance is 100 with a full bucket
  behind it. That is affordable. It is not the same claim as "the CPU problem is
  solved" — the tick mean has gone *up* since the wave-2 survey (58 → 65), because
  four colonies run where three did.

## W14S28 still waits, and by the rule invoked half an hour earlier

W15S28 took W15S29 on this same day. The wave-2 survey's own sequencing rule —
*"the colony that just swallowed a declaration should not be handed the next
one"* — is what made W15S29 wait for W15S27's digestion, and it applies to
W14S28 now with the same force. Its own condition is also still unmet: §11 made
it conditional on **its container being found standing**, and the live room has
none. Two hundred and eleven ticks of haul and a container to build is the next
declaration to make, not this one.
