# One declarative Layout, computed whole, truncated at RCL4

> **Amended by #211**: a trunk is priced on **paved length**, swamp surcharged for its construction cost — plain 2, swamp 3 (`Atlas.trunkSwampWeight`) — and no longer at the walking ratio of plain 2 / swamp 10. A trunk is a road, and once paved a swamp tile walks at exactly what a paved plain tile walks at (ADR 0010); the only thing swamp costs a road is the one-off 1,500-against-300 construction, and repair is identical. At the walking ratio the router bought W13S28 a permanent twenty-one-tile detour around five swamps to save ~1,200 energy. The two reasons for raw terrain — the line must not shift as its own roads are built, and must not bend around today's traffic — both stand: no road discount, no occupancy. It is the swamp:plain ratio that was a creep's and not a road's. Work-Area swamps are paved by their own rule, unchanged. W12S28's RCL5 trunk set is byte-identical before and after (measured on the capture); W13S28's drops from 68 sites to 30.

> **Amended by #209**: road **sites** are placed from RCL3 up (`roadLevel`, read off the [[stage]] `Tuning.BootstrapLevel` cuts) and not below it. "Sites place all at once" and the rejection of pacing both stand — what changes is that the road kind now carries a level gate of its own, the same shape the clustered kinds already had (`storageGap level`, `towerGap level`). The reason is in both paragraphs below.

> **Revised by ADR 0039**: the horizon moves to RCL5, and the title's "truncated at RCL4" is now history. ADR 0022's refusal to move it was weighed against the ordering *before* the working ground left it; re-derived after, the ten further extensions and the second tower cost the trunks nothing — both horizons pave the same 25 tiles.

> **Revised by ADR 0022**: the ordering rule now excludes the working ground (every source's Seats and the controller's Upgrade Work Area), and the RCL4 revisit this ADR asked for concluded that the clustered horizon *stays* at RCL4 — only the Storage tile and four Link footings are reserved beyond it, because those are the tiles that never come back once an extension takes them.

Construction planning was a single hard-coded rule (the extension
checkerboard), and each new structure kind — tower at RCL3, roads now —
would have bolted on another special case. Worse, structures placed
without foresight sit where later structures or roads want to be.

We decided construction planning computes **one deterministic Layout for
everything at once**, every tick, from the Atlas alone (no persisted
plan — same statelessness as the Planner):

- **One ordering rule eats all clustered structures.** Buildable tiles on
  the spawn's checkerboard colour, nearest-to-spawn first, exactly as
  extensions do today; the tower simply takes its pick *before* the
  extensions in that ordering. No separate tower-placement rule.
- **Trunks are part of the Layout.** Roads pave the trunk lines — each
  source to the controller *and* each source to the spawn — plus the
  swamp tiles inside the controller's Work Area. Trunk paths are priced
  on raw terrain only — plain 2, swamp 3 since #211: paved length with
  a construction surcharge, not the walk's swamp 10 — and route around
  the Layout's reserved tiles;
  reserved tiles are computed before trunks, so a future extension never
  lands on a road. A trunk ends where the work stands — the controller's
  Work Area edge, a spawn's neighbouring tile — and reservations win
  every overlap: a Work-Area swamp on a reserved tile stays a
  structure's, not a road's.
- **The horizon truncates at RCL4** (20 extensions + tower). The Layout
  is computed for everything up to RCL4 regardless of current level;
  placement is then filtered to what the current RCL unlocks and what is
  missing. The tick RCL3 lands, its sites drop with no new code.
- **Sites place all at once.** No pacing, no budget: a stateless planner
  has no memory to pace with, Build already sits in the surplus tier, and
  the downgrade deadline (ADR 0007) bounds the worst case.
  (Amended, #209.) Within one kind, still all at once — what the road kind
  gained is a **level**, not a rate: none below `roadLevel` (RCL3), the
  whole gap from there up. The plan is unchanged and still computed whole
  at every level, so the trunks go on routing around tomorrow's reserved
  tiles and a Link footing goes on dodging the pavement; what waits is the
  placing. The bound this bullet claimed does not hold for a bootstrapping
  room: W13S28 at RCL1 placed 64 road sites — some 19,000 energy — against
  8 energy a tick of income, with its Upgrade in the same surplus tier and
  further from every worker than the sites were, so nobody upgraded and
  the 200 progress that unlocks five extensions never arrived. That is
  around 2,400 ticks of the whole colony's income laid out ahead of the
  200 progress that doubles the body.
  This **narrows ADR 0010 rather than contradicting it**: a road is worth
  what ADR 0010 prices it at to the starter body too — `1W/2C/2M` loaded
  generates 3 fatigue a tile paved against 6 bare and recovers 4, so the
  road halves its loaded commute exactly as that ADR says. Half a tick a
  loaded step is simply not worth 2,400 ticks of income when the same
  energy buys the level that doubles the body.
  The gate is read off the whole road plan, the Upgrade Work Area's swamps
  included, and that is deliberate: those five tiles in W13S28 are the
  road ADR 0010 was written for, and they are also the dearest in the set
  — a swamp road is five times a plain one to build — so a bootstrapping
  room would spend some 7,500 energy, near a thousand ticks of its income,
  on the buffer it upgrades from before it can afford to upgrade at all.
  They wait with the trunks and arrive with them.

The trunk-line rule is deliberately general: this room's source→spawn
hops are 3–4 steps and barely worth paving, but the policy must hold for
rooms where they are not — room-specific exemptions don't get encoded.

## Considered Options

- **Horizon at RCL8** (the full 60-extension endgame). Rejected: the
  checkerboard reservation would carve a wide no-go zone around the
  spawn, taxing today's trunk routes with detours for structures five
  levels away. Revisit the horizon when RCL4 is in sight.
- **Model-only readiness** (support per-RCL structure lists but compute
  each layout when its level arrives). Rejected: in a stateless,
  deterministic planner the full computation is free, and late layouts
  reintroduce exactly the placement conflicts foresight exists to avoid.
- **Pacing road sites** to limit upgrade bleed. Rejected: needs memory or
  an ordering convention a stateless planner doesn't have; the surplus
  tier and the downgrade deadline already bound the bleed.
  (Amended, #209.) The rejection stands — the fix is a **level gate** and
  not pacing, and it needs neither memory nor an ordering convention: the
  controller level is a fact in the Snapshot, read the way the clustered
  gaps already read it. What #209 corrects is the second clause. The
  surplus tier and the downgrade deadline bound the bleed for a home with
  a Storage and thirty extensions, where Build's competitors are cheap and
  the deadline is 20,000 ticks away; they bound nothing for a colony whose
  whole income is 8 a tick and whose Upgrade is the level itself. The
  level is what pacing was reaching for, and RCL3 is the line already
  drawn: `roadLevel = Tuning.BootstrapLevel`, one field read off the
  other, because "bootstrapped" is the same question in both places
  (ADR 0047 decision 4).
