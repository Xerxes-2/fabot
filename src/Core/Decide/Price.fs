/// The price of a tick of delay: what the colony loses, in energy a tick, while
/// a piece of work waits — the one number the Pool ranks a Task by and the
/// spawn cascade weighs a body against its cost by. The ranks the Matcher
/// compares and the Resolver pushes with are a function of it (`rankOf`), and
/// the bands those ranks round to (`Tier`) are named here beside it.
[<AutoOpen>]
module Fabot.Core.Decide.Price

open Fabot.Core.Types

/// The bands a rank rounds to: the Resolver's push weight (`weightOfRank`)
/// and the names a reader gives a region of the value scale (`rankOf`).
/// Exported with the constants below because the scale is one fact, and the
/// tests that walk it (#237) read it here.
type Tier =
    /// The fight's own band, above every price: the world's fight pass ranks
    /// its Tasks off their Briefs (`Pool.planFightPool`), never off a value.
    | Safety
    /// A held source's output a tick and the values near it: the income
    /// flowing.
    | Feeding
    /// A thirty-second of it.
    | StockDraw
    /// A thirty-second again: the surplus's work.
    | Surplus
    /// And again.
    | UpgradeBuffer
    /// The floor, where work nothing waits on lands: the stock's own Refill.
    | Stock

/// How far apart two bands stand on the rank scale: ten ranks, a factor of
/// thirty-two in value (`rankOf`), so one rank is a factor of √2. A rank
/// must stay inside the half-band the Resolver rounds by, or it buys its Task
/// a push weight its band does not have (#237): `weightOfRank` rounds a rank
/// to its nearest band and gives a tie to the deeper one.
let tierRungs = 10

/// The whole band order, shallowest first. Exhaustive over Tier on purpose.
/// The downgrade deadline and the home's defence in a raid are the bands
/// above every price; the fight is above them.
let priorityOfTier =
    function
    // One band beneath `deadlineRank`'s: a fleeing creep outbids even a
    // controller about to downgrade.
    | Safety -> -2 * tierRungs
    | Feeding -> 0
    | StockDraw -> tierRungs
    | Surplus -> 2 * tierRungs
    | UpgradeBuffer -> 3 * tierRungs
    | Stock -> 4 * tierRungs

/// One band above the shallowest: where the downgrade deadline puts Upgrade —
/// an engine fact (the controller downgrades, and safe mode needs the timer),
/// never a price.
let deadlineRank = -tierRungs

/// The defence band's rank (#530): the towers while the home is raided and,
/// a rank over it, a struck rampart's Repair; and the ceiling every priced
/// economy value is held under (`economyRank`), so no economy price outbids
/// the home's defence in a raid. A starved refill cluster stands at it too: a
/// colony that cannot buy the body that refills it casts nothing at all.
let urgentRank = priorityOfTier Feeding - tierRungs / 2

/// The step one rank is: a factor of √2 in value.
let priorityStep = 1

/// What a rung is worth on the scale, exhaustive over `Rung` on purpose. The
/// fight's Briefs step their Tasks by rungs; the economy's ranks are prices.
let rankOfRung =
    function
    | OneRungDown -> priorityStep
    | OnTheTier -> 0
    | OneRungUp -> -priorityStep
    | TwoRungsUp -> -2 * priorityStep

/// The value the scale is anchored at: what a held source yields a tick
/// (`Engine.heldOutputPerTick`), the income flowing. Rank 0.
let heldRate = float Engine.heldOutputPerTick

/// The ranks a halving of value moves: two, so a band of ten ranks spans a
/// factor of thirty-two. Inside one rank (a factor of √2) travel decides
/// between two Tasks (the Matcher's key); a Task worth √2 more
/// outranks the other at any walk — the margin that keeps a rank from
/// jittering with every unit a store gains.
let private ranksPerHalving = 2.0

/// The floor of the scale: work nothing waits on, the stock's own Refill.
let bottomRank = priorityOfTier Stock

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

/// The value of a band's own rank: the anchor a Task with no energy price of
/// its own is priced at (the season's score, a lab's reagent).
let anchorOf (tier: Tier) : float = valueOfRank (priorityOfTier tier)

/// The ranks a price stands above the band it rounds to (the band
/// `weightOfRank` gives its push weight: a band owns the ranks from half a
/// band above it to one under half below, a tie going deeper), 0 for a price
/// at or under its band. What the Matcher's intake travel charge may give
/// back (`Pool.matchRank`, #501): a full container's two ranks over the flow,
/// the mine's two over the stock's band, never the band itself.
let liftOverBand (rank: int) : int =
    let aboveLower = ((rank % tierRungs) + tierRungs) % tierRungs

    if aboveLower > tierRungs / 2 then
        (tierRungs - aboveLower) / priorityStep
    else
        0

/// The rank a priced economy value stands at: `rankOf`, held a rank under
/// the defence band (`urgentRank`), which no economy price reaches.
let economyRank (value: float) : int =
    max (urgentRank + priorityStep) (rankOf value)

/// A draw out of a store the income flows through (a source's container, the
/// buffer, a tombstone): the flow, plus what the store loses by waiting.
let flowDraw (loss: float) : float = heldRate + max 0.0 loss

/// What a source's container loses a tick while it waits, as overflow
/// pressure: nothing while it stands under the line a hauled container
/// sawtooths under (`knee`, a fraction), its source's rate once full — every
/// dig then lands on the floor — and the share of the way between. A
/// container under the line keeps what it holds, which a pile does not.
let overflowLoss (knee: float) (rate: float) (stored: int) (capacity: int) : float =
    if capacity <= 0 || knee >= 1.0 then
        0.0
    else
        let fill = float (max 0 (min stored capacity)) / float capacity
        rate * max 0.0 (fill - knee) / (1.0 - knee)

/// ENERGY_DECAY: a dropped pile loses `ceil(amount / 1000)` a tick.
let pileDecay (amount: int) : float =
    if amount <= 0 then 0.0 else float ((amount + 999) / 1000)

/// What a store that ends loses a tick while it waits: a tombstone's or a
/// ruin's energy over the shortest of their longest lives (a tombstone's
/// five ticks a part of a 50-part body; a ruin stands longer, and the
/// projection carries both as one kind), never more than a full source
/// container loses — so the biggest grave ties a full container on value,
/// and travel decides between them.
let transientLoss (stored: int) : float =
    min
        heldRate
        (float (max 0 stored)
         / float (Engine.tombstoneDecayPerPart * Engine.maxBodyParts))

/// A draw on the stock: worth what the hungriest sink it serves is worth,
/// never more than the flow, and half of it — the stock was hauled once
/// already, so a load out of it is half the new flow a load out of a source's
/// container is — but for a ring that starves (`starved`), where it is the
/// walk the spawn waits on and not the hauler's time: the stock is not flow,
/// priced.
let stockDraw (starved: bool) (hungriest: float) : float =
    let worth = min heldRate (max 0.0 hungriest)
    if starved then worth else worth / 2.0

/// The hazard of a raid a tick: one over the ticks since one last stood
/// there, the memoryless estimate, and never under `prior`, the hazard of a
/// room with none on record — a raid long gone leaves a room no safer than
/// one never raided.
let raidHazard (time: int) (lastSeen: int option) (prior: float) : float =
    match lastSeen with
    | Some seen -> max prior (1.0 / float (max 1 (time - seen)))
    | None -> prior

/// A home with no raid on record still expects one in a creep's life: the
/// prior the towers are priced under.
let homeRaidPrior = 1.0 / float Engine.creepLifetime

/// What one energy of tower fire is worth in a raid: the hits a shot takes at
/// the far end of a tower's range for its energy, over the hits an energy of
/// the cheapest hostile part holds — `150 / 10 × 50 / 100 = 7.5`, the
/// exchange a tower makes at its worst.
let towerExchange =
    float (Engine.towerAttackAt Engine.towerFalloffRange)
    / float Engine.towerEnergyCost
    * 50.0
    / float Engine.partHits

/// What a tower short of energy costs a tick (the expected defence loss): the
/// raid hazard times the fire the shortfall is, at the tower's exchange.
let towerShort (hazard: float) (shortfall: int) : float =
    hazard * float (max 0 shortfall) * towerExchange

/// What the bodies that drink only at a store lose while it runs dry: each
/// one's body cost over its life, times the share of the way from the line a
/// hauled store reads drained at (`drained`, a fraction) down to empty. A
/// store over the line idles nobody; an empty one idles them all.
let idleMouths (drained: float) (bodyCosts: int list) (stored: int) (capacity: int) : float =
    if capacity <= 0 || drained <= 0.0 || List.isEmpty bodyCosts then
        0.0
    else
        let fill = float (max 0 (min stored capacity)) / float capacity
        let dry = max 0.0 (drained - fill) / drained
        float (List.sum bodyCosts) / float Engine.creepLifetime * dry

/// What a decaying structure's build costs a tick of what life it has left:
/// the rebuild over the ticks until decay takes it. A road at half its hits
/// is the surplus's own; one near nothing is worth a walk.
let rebuildAtRisk (rebuild: int) (hits: int) (decayPerTick: float) : float =
    float (max 0 rebuild) * max 0.0 decayPerTick / float (max 1 hits)

/// What a site's progress loses a tick where hostiles walk (#661): a hostile
/// stepping onto it destroys it, so the progress already built is at the
/// room's raid hazard.
let stompRisk (progress: int) (hazard: float) : float = float (max 0 progress) * hazard

/// CONSTRUCTION_COST: what a kind's site takes whole, a swamp road five
/// plain ones. Zero for a kind the decision layer never builds.
let constructionCost (kind: BuiltKind) : int =
    match kind with
    | BuiltKind.Spawn -> 15_000
    | BuiltKind.Extension -> 3_000
    | BuiltKind.Tower -> 5_000
    | BuiltKind.Road -> Engine.roadBuildCost
    | BuiltKind.Container -> Engine.containerBuildCost
    | BuiltKind.Storage -> 30_000
    | BuiltKind.Link -> 5_000
    | BuiltKind.Rampart -> 1
    | BuiltKind.Extractor -> 5_000
    | BuiltKind.Terminal -> 100_000
    | BuiltKind.Observer -> 8_000
    | BuiltKind.Lab -> 50_000
    | BuiltKind.Other -> 0

/// What a body returns on what it costs: the value it yields over its life,
/// in energy, over its energy and its spawn time at what a tick of the spawn
/// is worth (`Facts.spawnTickWorth`) — the price the Caster weighs a fighter
/// by (`Facts.castCost`), read for every row.
let bodyReturn (lifetimeValue: float) (energy: int) (spawnTicks: int) (tickWorth: float) : float =
    let cost = float energy + float spawnTicks * tickWorth
    if cost <= 0.0 then 0.0 else lifetimeValue / cost
