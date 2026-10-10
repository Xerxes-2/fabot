/// The price of a tick of delay: what the world loses, in energy a tick,
/// while a piece of work waits. The one number the pool ranks a Task by and
/// the spawn weighs a body against its cost by. Pure arithmetic; the facts it
/// is applied to are the pool's.
module Fabot.Core.Economy.Price

open Fabot.Core

/// The value the scale is anchored at: what a held source yields a tick, the
/// income flowing. Rank 0.
let heldRate = float Engine.heldOutputPerTick

/// Ranks a halving of value moves. Inside one rank (a factor of √2) travel
/// decides between two Tasks; a Task worth √2 more outranks the other at any
/// walk, which keeps a rank from jittering with every unit a store gains.
let private ranksPerHalving = 2.0

/// Ranks between two bands: a factor of thirty-two in value.
let bandRanks = 10

/// Where the work with no energy price of its own is anchored, one band
/// apart: what the surplus spends on (a site), what it spends last (the
/// controller), and the floor (the stock's own refill, a mineral's dig).
let surplusRank = 2 * bandRanks

let upgradeRank = 3 * bandRanks

let bottomRank = 4 * bandRanks

/// The controller inside its downgrade deadline, a band above the flow: an
/// engine fact (it downgrades, and safe mode needs the timer), not a price.
let deadlineRank = -bandRanks

/// The ceiling every priced value is held under, half a band above the
/// flow: what the fight's and the deadline's bands stand clear of.
let urgentRank = -bandRanks / 2

/// The rank a value a tick stands at: `2·log2(heldRate / value)`, rounded,
/// never under the floor. A value of nothing ranks at the floor.
let rankOf (value: float) : int =
    if not (value > 0.0) then
        bottomRank
    else
        let rank = ranksPerHalving * System.Math.Log(heldRate / value) / System.Math.Log 2.0

        if rank >= float bottomRank then
            bottomRank
        else
            int (System.Math.Floor(rank + 0.5))

/// The value a rank stands for: `rankOf`'s inverse on the scale's points.
let valueOfRank (rank: int) : float =
    heldRate / System.Math.Pow(2.0, float rank / ranksPerHalving)

/// The rank a priced economy value stands at: `rankOf`, held a rank under
/// `urgentRank`.
let economyRank (value: float) : int = max (urgentRank + 1) (rankOf value)

/// A draw out of a store the income flows through (a source's container, a
/// pile, a tombstone): the flow, plus what the store loses by waiting.
let flowDraw (loss: float) : float = heldRate + max 0.0 loss

/// What a source's container loses a tick while it waits: its source's rate
/// times how full it stands. A full one drops every dig on the floor.
let overflowLoss (rate: int) (fill: float) : float = float rate * max 0.0 (min 1.0 fill)

/// ENERGY_DECAY: a dropped pile loses `ceil(amount / 1000)` a tick.
let pileDecay (amount: int) : float =
    if amount <= 0 then 0.0 else float ((amount + 999) / 1000)

/// What a store that ends loses a tick: its energy over the longest a
/// tombstone stands (five ticks a part of a 50-part body), never more than a
/// full source container loses, so the biggest grave ties one and travel
/// decides.
let transientLoss (stored: int) : float =
    min
        heldRate
        (float (max 0 stored)
         / float (Engine.tombstoneDecayPerPart * Engine.maxBodyParts))

/// A draw on the stock: what the hungriest sink it serves is worth, never
/// more than the flow, and half of it, the stock having been hauled once
/// already, but for a bank short of full, where the spawn waits on the walk.
let stockDraw (starved: bool) (hungriest: float) : float =
    let worth = min heldRate (max 0.0 hungriest)
    if starved then worth else worth / 2.0

/// A raid in a creep's life: the hazard a home with none on record is
/// priced under.
let homeRaidPrior = 1.0 / float Engine.creepLifetime

/// What one energy of tower fire is worth: the hits a shot takes at the far
/// end of its range for its energy, over the hits an energy of the cheapest
/// hostile part holds (`150 / 10 × 50 / 100 = 7.5`).
let towerExchange =
    float (Engine.towerAttackAt Engine.towerFalloffRange)
    / float Engine.towerEnergyCost
    * 50.0
    / float Engine.partHits

/// What a tower short of energy costs a tick: the raid hazard times the fire
/// the shortfall is, at the tower's exchange.
let towerShort (hazard: float) (shortfall: int) : float =
    hazard * float (max 0 shortfall) * towerExchange

/// What a decaying structure costs a tick of the life it has left: its
/// rebuild over the ticks until decay takes it.
let rebuildAtRisk (rebuild: int) (hits: int) (decayPerTick: float) : float =
    float (max 0 rebuild) * max 0.0 decayPerTick / float (max 1 hits)

/// What a body returns on its cost: the value it yields over its life, over
/// its energy and its spawn time at what a tick of the spawn is worth.
let bodyReturn (lifetimeValue: float) (energy: int) (spawnTicks: int) (tickWorth: float) : float =
    let cost = float energy + float spawnTicks * tickWorth
    if cost <= 0.0 then 0.0 else lifetimeValue / cost
