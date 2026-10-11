/// A price is what the world loses, in energy a tick, while a piece of work
/// waits: the one number the pool ranks a Task by and the spawn weighs a body by.
module Fabot.Core.Economy.Price

open Fabot.Core

let heldRate = float Engine.heldOutputPerTick

/// Inside one rank travel decides; ranks are coarse (a factor of √2) so a
/// rank does not jitter with every unit a store gains.
let private ranksPerHalving = 2.0

let bandRanks = 10

/// Work with no energy price of its own is anchored a band apart.
let surplusRank = 2 * bandRanks

let upgradeRank = 3 * bandRanks

let bottomRank = 4 * bandRanks

/// Over the stock's own refill: a load no sink takes stands at the
/// controller before it is banked.
let bufferRank = upgradeRank + bandRanks / 2

/// Not a price but an engine fact: the controller downgrades, and safe mode
/// needs the timer.
let deadlineRank = -bandRanks

/// Priced values stay under it, clear of the fight's and the deadline's bands.
let urgentRank = -bandRanks / 2

let rankOf (value: float) : int =
    if not (value > 0.0) then
        bottomRank
    else
        let rank = ranksPerHalving * System.Math.Log(heldRate / value) / System.Math.Log 2.0

        if rank >= float bottomRank then
            bottomRank
        else
            int (System.Math.Floor(rank + 0.5))

let valueOfRank (rank: int) : float =
    heldRate / System.Math.Pow(2.0, float rank / ranksPerHalving)

let economyRank (value: float) : int = max (urgentRank + 1) (rankOf value)

let flowDraw (loss: float) : float = heldRate + max 0.0 loss

/// A full container drops every dig on the floor.
let overflowLoss (rate: int) (fill: float) : float = float rate * max 0.0 (min 1.0 fill)

/// ENERGY_DECAY: a dropped pile loses `ceil(amount / 1000)` a tick.
let pileDecay (amount: int) : float =
    if amount <= 0 then 0.0 else float ((amount + 999) / 1000)

/// Capped at a full source container's loss, so the biggest grave ties one
/// and travel decides.
let transientLoss (stored: int) : float =
    min
        heldRate
        (float (max 0 stored)
         / float (Engine.tombstoneDecayPerPart * Engine.maxBodyParts))

/// Halved because the stock was hauled once already, unless the bank is
/// short and the spawn waits on the walk.
let stockDraw (starved: bool) (hungriest: float) : float =
    let worth = min heldRate (max 0.0 hungriest)
    if starved then worth else worth / 2.0

let homeRaidPrior = 1.0 / float Engine.creepLifetime

/// Priced against the cheapest hostile part (50 energy) at the far end of
/// the tower's range.
let towerExchange =
    float (Engine.towerAttackAt Engine.towerFalloffRange)
    / float Engine.towerEnergyCost
    * 50.0
    / float Engine.partHits

let towerShort (hazard: float) (shortfall: int) : float =
    hazard * float (max 0 shortfall) * towerExchange

let rebuildAtRisk (rebuild: int) (hits: int) (decayPerTick: float) : float =
    float (max 0 rebuild) * max 0.0 decayPerTick / float (max 1 hits)

let bodyReturn (lifetimeValue: float) (energy: int) (spawnTicks: int) (tickWorth: float) : float =
    let cost = float energy + float spawnTicks * tickWorth
    if cost <= 0.0 then 0.0 else lifetimeValue / cost
