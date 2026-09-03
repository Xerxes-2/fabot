# One declarative Layout, computed whole, truncated at RCL4

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
  on raw terrain only and route around the Layout's reserved tiles;
  reserved tiles are computed before trunks, so a future extension never
  lands on a road.
- **The horizon truncates at RCL4** (20 extensions + tower). The Layout
  is computed for everything up to RCL4 regardless of current level;
  placement is then filtered to what the current RCL unlocks and what is
  missing. The tick RCL3 lands, its sites drop with no new code.
- **Sites place all at once.** No pacing, no budget: a stateless planner
  has no memory to pace with, Build already sits in the surplus tier, and
  the downgrade deadline (ADR 0007) bounds the worst case.

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
