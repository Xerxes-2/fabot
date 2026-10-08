# Work is ranked by what a tick of its delay costs

> **Status:** accepted

> **Accepted 2026-10-08** on #622 (a), with #661's item 2 (a site's progress where hostiles walk) folded in. Amends ADR 0010's second decision (Refill's rank layered by target, a tower's in the surplus) and the rung banners on it (#234, #242, #249, #250, #284, #306, #359, #471, #526, #548, the line of 2026-10-05), and ADR 0023's second-tier Withdraw. ADR 0002's key, #501's intake travel charge (what it gives back is now a price's lift over the band it rounds to, `Price.liftOverBand`) and #530's urgent band stand.

## Context

The Pool set each Task's rank off a fixed ladder — Safety, Feeding, StockDraw, Surplus, UpgradeBuffer, Stock, ten ranks apart — and some fifteen rungs a Task could be stepped by inside its tier, each one a live case's patch. The ladder put a tower's Refill in the surplus in peace, so a raid found the towers short: `Raided` flips only once the raid is there. The upgrade buffer sat a tier under the surplus, so W17S29's two upgraders idled some 340 ticks beside an empty buffer while the row still asked for both (t1,026,455-509). A site's progress was worth nothing to finish while hostiles walked the room, and they stomp it.

## Decision

A Task's rank is its price: the energy a tick the colony loses while it waits (`Price`), read off one log scale (`Price.rankOf`): rank 0 is a held source's output a tick, one rank a factor of √2, a band (ten ranks) a factor of thirty-two. Inside one rank travel decides, as the Matcher's key always did; a Task worth √2 more outranks at any walk — the margin that keeps a rank from jittering with each unit a store gains. The old tiers are the bands a rank rounds to for the Resolver's push weight.

What a tick of delay costs, from facts already computed:

- **An intake**: the flow, plus what its store loses by waiting. A source's container its source's rate times the share of the way it stands from the haul's overflow line (`Tuning.HaulFullPercent`) to full; a pile `ceil(amount / 1000)`; a tombstone or a ruin (one projected kind) its store over the 250 ticks a tombstone stands at the most, never more than a full container loses, so the biggest grave ties a full container and travel decides. A dig and a reservation are the flow itself, whichever rock or controller: the caster casts one body a Post and a room, so which one a body takes is travel's.
- **The stock's draw**: what the hungriest sink it serves is worth, never more than the flow, and half of it, for a load out of the stock is energy hauled a second time — but all of it for a ring that starves, where the spawn waits on the walk and not on the hauler's time (ADR 0071's tie, priced).
- **The refill ring**: a tick every spawn waits, the colony's income (`Facts.spawnTickWorth`, the Caster's price of a spawn tick), every row casting at the bank's capacity.
- **A tower**: the raid hazard (one over the ticks since the home's last raider, never under the prior of a creep's life, which is also the hazard where none is on record: a raid long gone leaves the home no safer than one never raided) times the fire its shortfall is, at a tower's worst exchange (`Price.towerExchange`: 150 hits for 10 energy against 100 hits for 50).
- **The buffer**: the cost over a life of the bodies it feeds — the standing row's, which the upgrader row sold at that cost, and any light body holding the home's Upgrade — times the share of the way from the drained line (`Tuning.HaulDrainedPercent`) to empty. The pool feeds every body the row keeps, under the emergency reserve too, where the row hires on the income's share.
- **A Repair** of a decaying kind: its rebuild over the ticks decay leaves it. A road at half its hits is the surplus's own; one near nothing, or a container about to go, outranks a spawn's top-up. The declared line short of its floor: the energy of the hits short, under the home's hazard.
- **A Build**: its growth, plus its progress at the room's raid hazard where hostiles walk (no prior: a room no hostile is known to walk risks nothing).

Bands no price reaches, each an engine fact or a named margin: the fight's own (`Pool.planFightPool`, the world's); the downgrade deadline (the controller and safe mode); the home's defence in a raid (towers, a struck rampart a rank over) and a starved ring (a colony that cannot buy the body that refills it casts nothing). Every priced economy value stands a rank under that band (`Price.economyRank`).

Anchored where no energy price exists, at the band the old ladder gave them: the season's score and the ore and reagents beside it (score is not economy), with their rungs (the mine past its contact cliff, ore that ends, ore on the floor); a Claim and the Reactor's claim; the ferry's sink (the child's need is not on the mother's view); a site's growth (a nursery's or a bootstrapping room's, and the builders' budget's outpost sites at the flow; a home site a rank over the surplus; a room's first tower two; the outpost road's and the Storage-beside-extensions rules); a dying rampart; the claim party's and a borrowed Upgrade; the buffer's floor (its band, where whoever upgrades drinks) and a busy colony's buffer, filled after every sink that spends (the row keeps that body as downgrade insurance, the one `busy` fact both read); the stock's own Refill at the floor and, under the emergency reserve, a rank over the ferry.

## Consequences

- A tower short in peace is filled before the surplus is spent: an empty one with no raid on record ranks near the flow's band.
- The buffer an upgrader idles beside outranks the stock, the surplus and a top-up of a near-full tower; a buffer over the drained line leaves the load to bank.
- The stock's draw ranks with the hungry ring's half, two ranks under a source's container, and over the season's ore while the spawn waits on it (capped at the spawn's room, so the rest of the haul takes the mine); with only surplus work waiting, a body spends the load in hand before topping it up.
- A pile and a container under the overflow line are within a rank and travel decides, where #242's half-a-load rung lifted the pile at any distance; a pile over a thousand, which bleeds two a tick, outranks it.
- Open: the Matcher compares rank, then travel; an exact value ÷ travel key is not taken (travel decides inside one rank). The growth of a finished structure (an extension's bigger bodies, a container's drop-mining) is anchored, not priced. Thorium has no energy price. The emergency reserve's rank over the ferry is a margin's, not a priced defence loss. The tower's exchange and the home's raid prior are first estimates.
