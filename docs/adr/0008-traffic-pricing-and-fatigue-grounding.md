# Traffic re-prices a route; fatigue grounds a creep

A live jam: builders stalled behind seated harvesters while the console
spammed `MoveCreep failed: -11` (ERR_TIRED) every tick. ADR 0001's
arbitration (as sharpened by the movers-before-stayers fix) kept planning
displacements and swaps for loaded 3-part workers that are tired two ticks
in three — the engine refused every arranged move, and the jam replayed
each tick. Meanwhile the flood pathed straight through standing creeps, so
a traveller never even considered the open lane beside a crowd.

We decided two things, revising ADR 0001's movement picture and ADR 0002's
travel-cost semantics (as already revised by ADR 0006):

1. **Fatigue grounds a creep.** A creep with fatigue outstanding cannot
   step this tick, so the Resolver takes it out of arbitration entirely:
   its tile arrives pre-claimed (blocked), nobody claims or displaces
   through it, and no move Intent the engine would refuse is ever issued.
   Grounding is recomputed from the Snapshot each tick and is always
   transient — a stationary creep drains 2 fatigue a tick — so once the
   creep rests, displacement and swapping resume unchanged.
2. **Traffic re-prices a route.** The flood prices a step landing on a
   tile some creep occupies at 5 extra ticks (the occupancy surcharge).
   Travellers detour around standing traffic whenever a lane is within 5
   ticks of the crowded route; when no lane is, the tile stays passable
   and the traveller waits or displaces as before. The surcharge flows
   into travel cost, so the Matcher also sees crowded approaches as
   dearer — a mild pressure toward less crowded targets; sticky
   assignments (anti-thrash) are unaffected, and since a surcharge can
   never make a cost `None`, traffic can never release an assignment or
   make a Task inapplicable.

## Considered Options

- **Keep issuing moves and let the engine refuse them.** The status quo.
  Rejected: the arbitration plan silently fails as a unit (a swap needs
  both moves to land), the jam replays every tick, and the failure log is
  noise that hides real errors.
- **Treat occupied tiles as impassable** (cartographer-style hard
  obstacles for stuck creeps). Rejected: a Work Area behind a crowd would
  price as unreachable and the Matcher would release the assignment (ADR
  0002 has no range fallback) — traffic must never un-assign work.
- **Penalise only grounded creeps' tiles, not all standing creeps.**
  Rejected for now: a seated harvester is just as much in the way rested
  as tired — displacement costs the displaced creep a shuffle either way —
  and one uniform rule keeps the flood memo shared per (tile, factor).
  Revisit if detours prove too eager.
- **Surcharge in `firstStep` only, keeping `travelCost` terrain-pure.**
  Rejected: the two answers must come from one flood (ADR 0004's one
  Atlas, one pricing), and a Matcher blind to crowds re-creates the pile-up
  the Seat caps exist to prevent.

## Consequences

- The ERR_TIRED spam is gone by construction: no Intent is issued that
  the engine's fatigue rule would refuse.
- A traveller whose only path runs through a grounded creep waits in
  place for up to 2 ticks instead of burning failed moves; with any lane
  within 5 ticks it detours immediately.
- Assignment choice now shifts with traffic. Bounded by the surcharge (5
  ticks, one swamp step) and damped by sticky assignments, but crowded
  Seats look slightly farther than they are.
- Hostile creeps are not yet in the occupancy set (the projection's
  creep positions cover own creeps only); a hostile squatting a lane is
  still pathed through at face value. **Another colony's** creeps are, as
  of #220: a view's `Foreign` tiles enter the occupied grid the flood
  reads, so a traveller is priced around a garrison it can never
  displace — occupancy and never an obstacle, exactly as this decision's
  second Considered Option requires.
- **"Waits in place for up to 2 ticks" is not a promise the arbitration
  could keep** (#219, #216 R2b). It holds where a traveller has nowhere
  to step, which is what the corridor tests pin; it fails where two
  loaded bodies meet head-on in a lane and are tired every other tick,
  because the tick on which both are rested and could swap never comes —
  eight creeps stood in W13S28's north corridor for ten minutes. So a
  traveller's Move Intent now carries a tail: the ground beside both it
  and the step it asked for, a way *around* the tile and never a way back
  down the lane, taken only when the head cannot be had. On the border
  ring the tail is every tile beside the creep and not only those beside
  its step — there is no lane behind a ring creep to back down, and
  standing still there is a bounce across the border rather than a wait
  (#145), which is the rule this generalises rather than narrows. Grounding is
  unchanged and is still a wall; what changed is that a wall is no longer
  the end of the creep's candidate list. The wait this consequence
  describes therefore survives exactly where there is nothing to sidestep
  to, and where there is, a body steps out of the lane and back the tick
  after — two moves against a jam that does not clear on its own.
- **The arbitration's core is a weighted matching** (#216 R2b), which is
  where this decision's "blocked" set now lands: a tile in it is a wall no
  chain runs through, and an occupant with no Move Intent of its own —
  which is what a fatigued creep is — is the same wall found by looking
  rather than by remembering to block it. See ADR 0001's Consequences.
- The flood this decision touches was rewritten from immutable Set/Map
  Dijkstra to flat arrays with a binary heap — an implementation choice,
  not a decision: semantics (including tie-breaking order) are unchanged
  apart from the surcharge.
