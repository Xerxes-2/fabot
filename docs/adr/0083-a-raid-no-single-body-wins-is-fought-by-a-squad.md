# A raid no single body wins is fought by a squad, priced and launched as a whole

> **Status:** accepted, amended by #453

> **Amended 2026-10-02** by #453 (S2, muster): decision 3's muster is a Safety-tier `Fight of room` Task, pooled beside the room's Guard once a raid the residents lose stands in a resident room on two ticks inside `Tuning.FightConfirmTicks`, and held for `Tuning.FightHoldTicks` after that raid is last seen. The squad it is first pooled with is latched in the Raid log's fight record and read by every rule until the record drops: a raid that shrinks or steps out never re-prices it. Its members are the casts holding it, each role read off the cast's name, and counted against the squad rows alone, never the fleet; a resident joins only after launch, and only into a slot its parts fit. The squad waits on rally ground short of the target, never in a Source Keeper's or a rival's room, and none is cast for a room with no rally ground. It launches only with every slot held by a living member and its casts together, and a lost member sends the survivors back to rally until the squad is whole again; launch is derived from positions and never stored. With the room empty a launched squad holds its resident ring. The #447 relief gives way to the squad only once it launches. Spawn time is not priced.

> **Accepted 2026-10-02** on #452 and #456, the user deciding: we are already fighting Trepidimous; the melee duo is built first; the nearest RCL7 mother in reach casts it, one duo per engagement. Research: `docs/research/squads.md`.

At W17S25 (t880,341) Trepidimous parked `2 × 18M17A + 2 × 11M7H` and a 3-CLAIM tapper on a raised child's controller. Every engage and cast rule we have prices **one body** (ADR 0072's `guardBlocksReach`, "one body and never the row's two summed"; ADR 0080's relief and the #447 resident relief, "while some ranger size wins"). No body we can cast wins that raid alone, so the colony read it as unwinnable and fed residents in one at a time. Several bodies of the same sizes do win it, but only if they arrive together, focus the healers, and either stand up as a melee pair or kite.

We decided **a raid no single body wins is answered by a squad: a fixed composition cast whole, mustered before it engages, and priced as a whole by a bounded tick simulation.**

1. **The price is a simulation, not a closed form** (`Facts.squadFight`, read as `squadWins`). At most 60 ticks and 10 bodies, Source Keepers left out, on the engine's rules: parts stripped from the head, melee strike-back, heal after damage and before the death check, our fire and a raised home's loaded towers on the head of #451's kill order (claimer, broken healer, raid), re-picked each tick as the Emitter does, the raid's on our front. A trade that disarms both sides on one tick is lost. A kite flag zeroes melee both ways; it is priced only for an all-ranged squad at least as fast as the raid's melee, with safe ground in the room. `exchangeWon`'s two clocks cannot see strike-back, kill order or a healer that dies first, and those decide this raid.
2. **The catalogue is fixed and small** (`Bodies.squadCatalogue`): the melee duo (brawler `25M25A` + medic `18M18H`, 8,650), the duo with a kiter (14,250), and three kiters (`16R 24M 8H`, guns before legs, 16,800). The cheapest one that wins within the casting colony's bank and spawn time is the one cast.
3. **Summing bodies is valid only behind muster.** ADR 0072's "one body, never summed" stands wherever bodies arrive one by one (outpost guards against NPC invaders keep `guardBlocksBeat`). It is replaced by `squadWins` only where a squad is mustered complete before it engages (#453, S2); until then `squadWins` is report-only (`observe quotas`).
4. **ADR 0080's relief becomes the squad** once muster exists: a defended or raised home whose raid no single body wins is answered by the catalogue's cheapest winning squad, not by `Engine.guardCap` bodies of the size that would win alone. If no squad wins, the stand-down and safe mode stand as they are.

## Consequences

- Until S2 ships, nothing casts or engages off `squadWins`; `rangersWanted` and `outmatched` still price one body.
- The simulation has no positions, swamp, fatigue, or range-dependent heal; all heal is adjacent. The kite flag is the whole of terrain. It reproduces the research simulator's verdicts on the t880,341 raid with the tapper shot first: the duo standing by t6, 2 × R7 kiting by t56, three kiters kiting by t29.
- Boosts and ranged mass attack are not modelled.
- The raid always strikes our front, so the duo's medic is never its target and shooting their melee first costs the duo nothing here; the order decides the ranger rows.
- The kiter's guns-first order gives up the stand-up resistance §3.2 priced its R8 with: in contact, damage disarms it first.
- The role bodies are not in the pattern table and `patternOfParts` does not read them back yet: a medic would read as a worker. S2 adds the role cuts.

## Considered options

- **Sum the bodies into `exchangeWon`.** Rejected: it ignores strike-back and kill order, and calls the duo a loss and the 2 × R7 kite a win for the wrong reasons.
- **Let any number of single-size rangers in.** Rejected: rangers lose stand-up against melee and the trickle is what killed ours.
