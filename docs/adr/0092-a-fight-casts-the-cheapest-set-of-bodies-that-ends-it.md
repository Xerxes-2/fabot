# A fight casts the cheapest set of bodies that ends it

> **Status:** accepted

> **Accepted 2026-10-08** on #654 (#615 U4 slice C). Supersedes ADR 0085's #550 **Rows** amendment (the trespass's per-row split) and the fixed counts and sizes of the guard and ranger rows (`guardsWanted` one-or-`Engine.guardCap`, `guardBlocksFor`, `rangersWanted`, `rangerBlocksFor`).

## Context

A fight's ask was split by row. The melee guard row and the ranger row each sized their own body off a fixed rule: one guard, two where one block loses (`guardsWanted`); one ranger, two where the bank's biggest loses (`rangersWanted`); the smallest block count `blocksBeat` passes. Which row answered an outpost's raid was a fixed order (`rowAnswer`: the guard where its biggest body trades, else the ranger, else two rangers where the guard's swing catches no raider). A trespass asked each row for the trespassers it could strike (#550), so the W17S26 pair (5W3M miner, 23C23M hauler) cost a guard and a ranger where one ranger kills both. #652's priced kills cast through the same split.

## Decision

One fighter row, `CastRow.Fighter of BodyTemplate`, and one chooser, `Facts.fighterCast`, read by every caller: the room's own row (`rowAnswer`, so the Brief's template and the verdict), the ask (`Threat.fighterAsk`: an outpost's ring, a held exit, a core, a chase's own row, a trespass's kill, the resident and rampart rangers' sizes), and the Caster's price (`castPrice`). A template is a `Bodies` pattern (melee guard block, ranger block); one more is a row in `bodyTemplates`.

The chooser enumerates sets of one template, one block count the bank buys at its capacity, and at most `Engine.guardCap` bodies, and keeps those that end the fight:

- **A raid** (raiders, or in the home a body endangering it): the exchange ends it, trading or kiting (`blocksOutcome` alone, `bodiesOutcome` for more), at the stance the gate prices the body at (a melee body holds its post where a raider keeps off its swing, as `Brief.stanceOf` has it).
- **A kill** (trespassers only): the set kills the most of them (a melee body those its swing catches, a ranged one any it strikes), each before it ages out.

Each set is priced from facts already computed: the life its walk and kill spend of each body's energy (`lifeSpent`; a body that ends a fight sooner is free sooner), the spawn time of each body not yet cast at what a tick of the colony's spawn yields (`spawnTickWorth`), and every tick from the cast to the kill at what the fight costs a tick: the room's sources' yield for a raid (`roomTickWorth`), each trespasser's work for a kill (`harmRate`), whose landed value (`killsLanded`, #621's `killValue` × catch odds) counts for it. Bodies of the colony already cast, standing or in an oven, cost only the life they spend, so a fight a standing body ends casts nothing beside it.

The cheapest set is taken where **every body of it is one `Facts.fits` seats**, fresh, beside the rest of its set: the row and the gate price alike (#647). Where none ends the fight, the melee body holding its post answers (one of the fewest blocks that hold, `bodies` of them where the bank buys only smaller), ending nothing; it is cast without a fits check, as the row cast it before.

Two rules are not prices, each for a reason the price does not carry:

- More than one body of a template only where no one body of it ends the fight: a set's bodies land one at a time, the gate seats one only beside those landed, and a pair priced a hair under one body flipped the ask on the raid's every step, stranding the body already cast.
- More than one melee body is not priced outside a home: `bodiesOutcome` is the shooters' pair (#609) and prices no swing struck back on the bodies the raid does not focus (five 4A4M invaders against two two-block guards came out a trade by it).

**A home of ours is a deliberate exception** (the orchestrator's call in the #654 review: home defence outranks the refactor). Until pair coordination is priced, a home keeps the melee pair its row cast: pairs of melee bodies are candidates there; a melee body of more than one block (one block loses) is cast twice, the old redundancy, as one may land late or fall first; and where no set ends the raid, the pair of the biggest body the bank buys answers where `bodiesOutcome` says it ends it, ending it whether or not the gate seats it fresh (a home is fought for to the death, #602). Without it, a home whose raid only a melee pair beats got no cast that ends it (`castPrice` false), so the Caster and `homeUnheld` dropped the home's own Guard.

## What stays separate

The home's rampart defender (`CastRow.Defender`, #504) and its Defend's split (`defendSplit`): the bodies the line never strikes are a ranged template's by the engine's reach, sized by the chooser over one ranged body. The squad rows. A level-0 core's melee body at the biggest the bank buys (#487). A held exit, an outpost with no raid in sight and a bouncing trespasser cast one melee block (the ring kinds admit a swing). The resident garrison's count (`residentsWanted`, ADR 0090). The trespass's seats stay the split's (`trespassSplit`: who may stand), raised to the cast's count; the cast is what is bought.

## Consequences

- Outside a home, a raid one block loses buys one body of the k blocks that win it, where the old rule bought two bodies of k blocks; where the bank buys no such body, the bodies it buys that hold together. A home still buys the two.
- An outpost's or a chase's template is the cheapest: a one-MOVE melee is cast a ranger; a raid the guard trades and a ranger kites down is cast the ranger and is the row's Win. A raid no set trades is still the verdict's Kite (no cast).
- The W17S26 pair is one ranger. A guarded room's trespass asks one cast.
- An outpost Guard seats, beside the cast's count, the bodies of its template standing in the room or holding it that lose the fight alone, one more than they are, at most `Engine.guardCap`, so a reinforcement is never evicted (#272).
- A cast the exchange prices a win but the gate refuses fresh is no win: W15S28's held guard against #589's trio.
- Open: the template a colony bids for is the asking colony's; a bidder that would cast another template cheaper is not weighed. Hybrid and heal templates are rows not yet added. A set mixing templates is not priced. A shut outpost the view does not project prices its raid's ticks at nothing (its sources are unknown to the view). Pricing a melee pair's coordination, which would retire the home exception. Choosing the mass attack by priced value (deferred, #615).
