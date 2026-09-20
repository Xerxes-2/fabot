# A stronghold is not crossed

> **Status:** accepted

> **Accepted 2026-09-19** on #382; the decision is the user's ("窄修法做吧"), taken after the wider one it replaces was built and reverted. Implemented by the change that accepted it.

ADR 0043's [[stand-down]] withdraws the work a colony declared in one room. ADR 0065 then held that a hostile in a **transit-only** room opens no stand-down at all, such a room having no task, no quota and no guard row to withhold. ADR 0066 held that a stand-down **does not propagate through a route**: an [[errand]] is withheld when its own target room is shut and not when one of its [[transit room]]s is, because *"propagating the outpost's coarse clock to the route would instead stop Reactor supply for as long as unrelated work in a crossing room is withdrawn"*.

Both are right about their own case, and the reasoning of both turns on the same premise: that what makes a room dangerous is dangerous to **work in that room**, which a body merely crossing it does not do.

W15S26 broke the premise. A stronghold expanded there at t578,050 — an invader core of level 4, `bunker4`, 100,000 hits under a 1,000,000-hit rampart, **four towers** each under its own, and a garrison of four `25 MOVE / 25 RANGED_ATTACK` and `25 MOVE / 25 ATTACK` Invaders. A tower reaches every tile of its room. A `[20 Carry; 10 Move]` courier carries 3,000 hits and the [[re-claimer]] 200. The gate had the room correctly shut on the core's collapse timer, and the relay walked a 650-energy body through it every ~409 ticks: `observe raids` recorded two dead on the **same entry tile** 161 ticks apart, for a Reactor whose store had been zero for 26,000 ticks and whose bank had no ore to send.

The first fix tried was the general one — a shut room is not a link — and it is exactly what ADR 0066 rejected, for its own good reason. Two `ViewTests` cases pinned that, one of them named *"a stand-down withholds its target-room errand, not a route crossing"*. It was reverted.

## Decision

**A room holding an invader core of level 1 or more cannot be crossed, for as long as that core's own collapse timer runs.**

1. `InvaderCoreInfo.Level` reaches the projection, because the clock cannot tell the two cores of one stronghold apart: live, the `bunker4` and the level-0 core it expanded into W15S27 carried the **same** collapse tick. A level-0 core has no tower, no rampart and no garrison, and a body walks past it — the season's own evidence, W15S27 being crossed daily while this was written.
2. `OutpostEpisode.Stronghold` records that one was seen while the row stood, so the fact rides the [[raid log]]'s memory. **Read off the row and never off this tick's vision**, which is the whole reason the row exists: stop crossing a room and the colony stops seeing what is in it, so a rule keyed on vision would re-link the room, walk a body in, lose it, see the bunker again and shut it again, for ever. The flag is **sticky for the row's life** and it is a field of its own rather than a `StandDownBasis` case — which is the shape this was first built in and was wrong. `Basis` says which clock the expiry came off, and `sight` overwrites it whenever a later deadline arrives, so a room whose raid outlived its core would have gone back to being crossed with four towers standing in it; a core that offers no collapse tick at all lands on `Reservation` or `Fallback` and would never have been impassable however big its bunker. A property of the room and the provenance of a number are not one fact.
3. `StandDown.Impassable` carries those rooms — a subset of `Shut` and a different question from it — and `World.reachesUnder` is the one combinator the scan set and the refusal report are both built on, so the report cannot name different refusals than the set made and a third reader inherits the rule rather than skipping it. Nothing else is needed: a declaration whose every chain crossed such a room stops being `routable`, `Errand.refused` and `Outpost.refused` withhold it as a unit and name it, and the tick the timer runs out the room re-links and the declaration comes back.

This supersedes ADR 0066 for this one case and leaves the rest of it standing: an ordinary stand-down on a crossing room is still no route lock.

## Consequences

- The relay stops. So does the Reactor programme, for as long as the bunker stands — which is the trade being made, and it is the right one while the store is empty. When there is ore again the arithmetic is the same: a load that cannot cross is a load that does not arrive, and a body that cannot cross is one that does not come back.
- An **outpost** beyond such a room is refused on the same rule, by the same predicate. That is a wider reach than #382's own case and is deliberate: a room a body cannot reach alive is not a room to hire for.
- The refusal is visible. `ColonyView.Refused` names the declaration and its kind, `observe layout` prints it, and `observe outposts` prints the basis as *"a stronghold's own collapse timer — the room is withheld from work and cannot be crossed either"*.
- **Vision is the limit, and it is honest about it** (ADR 0004): a stronghold in a room the colony has never seen is a stronghold it does not know about. The memory covers the room once seen, which is the case that matters — a colony learns about a bunker by crossing into one.
- Not decided here: ADR 0065 says a hostile in a transit-only room opens no stand-down, and yet W15S26's row exists because a **core** opens one on a different basis. Whether that is the intended reading or ADR 0065's own gap is left open, and this decision does not depend on it — what it reads is a row, however the row came to be.
- **A body already standing in such a room is not brought home.** Withholding the declaration stops the next cast; the one in flight loses its Task, stands where it is and goes on counting as living. Getting it out is a movement question this decision does not answer, and the cost is one body per stronghold rather than one every cadence.
- Tests: `ViewTests` pins the carve-out beside the ADR 0066 case it carves out from; `ObserveOutpostTests` pins that a level-4 core makes the room impassable where a level-0 one only shuts it, that a later deadline moves the basis and never the bunker, and that the core's own timer ends it.
