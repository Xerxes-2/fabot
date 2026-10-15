/// The server's fixed numbers and formulas. A number the bot chose is a
/// `Tuning` field, owned by the module that reads it.
module Fabot.Core.Engine

open Fabot.Core.Vocabulary

/// MAX_CREEP_SIZE: a larger body is refused.
let maxBodyParts = 50

let harvestPerWork = 2

let carryPartCapacity = 50

let mineralHarvestPerWork = 1

let extractorCooldown = 5

/// The intent pass runs before the object pass, so digs land one tick later.
let mineralHarvestCycle = extractorCooldown + 1

let creepLifetime = 1500

let claimLifetime = 600

/// STRONGHOLD_DECAY_TICKS ×1.1: the longest a deployed core stands.
let strongholdLongestCollapse = 82_500

/// CREEP_SPAWN_TIME.
let spawnTicksPerPart = 3

let partCost (part: BodyPart) : int =
    match part with
    | Work -> 100
    | Carry -> 50
    | Move -> 50
    | Attack -> 80
    | RangedAttack -> 150
    | Heal -> 250
    | Claim -> 600
    | Tough -> 10

let bodyCost (body: BodyPart list) : int = body |> List.sumBy partCost

let boostEnergyPerPart = 20

let boostMineralPerPart = 30

let reservationCap = 5000

/// CONTROLLER_RESERVE. The reservation decays by one a tick, so N parts gain N - 1.
let reservePerClaim = 1

/// NUKE_DAMAGE; a rampart on the tile takes it first.
let nukeDamageCentre = 10_000_000

let nukeDamageRing = 5_000_000

let nukeRadius = 2

let containerCapacity = 2000

let spawnEnergyCapacity = 300

let storageCapacity = 1_000_000

let terminalCapacity = 300_000

let terminalCooldown = 10

let terminalMinSend = 100

let labMineralCapacity = 3_000

let labReactionAmount = 5

let labReactionRange = 2

let reactionTime =
    function
    | Hydroxide -> 20
    | UtriumHydride -> 10
    | UtriumAcid -> 5

let reagentsOf =
    function
    | Hydroxide -> Ore Hydrogen, Ore Oxygen
    | UtriumHydride -> Ore Utrium, Ore Hydrogen
    | UtriumAcid -> Compound Hydroxide, Compound UtriumHydride

/// Chebyshev over the room grid; the observed room is visible the next tick alone.
let observerRange = 10

let controllerDowngrade (level: int) =
    match level with
    | 1 -> 20_000
    | 2 -> 10_000
    | 3 -> 20_000
    | 4 -> 40_000
    | 5 -> 80_000
    | 6 -> 120_000
    | 7 -> 150_000
    | 8 -> 200_000
    | _ -> 0

/// The downgrade timer below which `activateSafeMode` is ERR_TIRED.
let safeModeDowngradeLine (level: int) = controllerDowngrade level / 2 - 5_000

/// `calcTerminalEnergyCost`, over the linear room distance.
let sendFee (range: int) (amount: int) =
    float amount * (1.0 - exp (-float range / 30.0)) |> ceil |> int

let meleeRange = 1

let rangedRange = 3

/// The reach of upgradeController, build and repair.
let upgradeRange = 3

/// A Source Keeper rests within this range of its source or mineral and never pursues.
let keeperPin = 1

/// The username the engine's NPC raiders carry.
let invaderUser = "Invader"

let attackPower = 30

let rangedAttackPower = 10

let dismantlePower = 50

let repairPower = 100

let extensionHits = 1000

let massAttackPowerAt (range: int) =
    match range with
    | 0
    | 1 -> rangedAttackPower
    | 2 -> 4
    | 3 -> 1
    | _ -> 0

let healPower = 12

let rangedHealPower = 4

let towerPowerHeal = 400

let towerPowerAttack = 600

let towerPowerRepair = 800

let towerCapacity = 1000

/// A tower holding less does nothing.
let towerEnergyCost = 10

let towerHits = 3000

let towerOptimalRange = 5

let towerFalloffRange = 20

let towerFalloff = 0.75

let private towerPowerAt (power: int) (range: int) =
    if range <= towerOptimalRange then
        power
    else
        let full = float power
        let r = min range towerFalloffRange

        full
        - full * towerFalloff * float (r - towerOptimalRange)
          / float (towerFalloffRange - towerOptimalRange)
        |> floor
        |> int

let towerHealAt (range: int) = towerPowerAt towerPowerHeal range

let towerAttackAt (range: int) = towerPowerAt towerPowerAttack range

let towerRepairAt (range: int) = towerPowerAt towerPowerRepair range

let partHits = 100

/// The engine destroys parts from the head; a part with any hit left acts.
let liveParts (body: BodyPart list) (hits: int) : BodyPart list =
    let live = (hits + partHits - 1) / partHits
    body |> List.skip (max 0 (List.length body - live))

let private partHitsAt (n: int) (hits: int) (i: int) =
    max 0 (min partHits (hits - partHits * (n - 1 - i)))

/// `calcBodyEffectiveness`. `boosts` is one a part, head first, or empty.
let powerAt (body: BodyPart list) (boosts: Boost list) (hits: int) : BodyPart -> int =
    if List.isEmpty boosts then
        partCountIn (liveParts body hits)
    else
        let live = (hits + partHits - 1) / partHits

        let acting = List.zip body boosts |> List.skip (max 0 (List.length body - live))

        fun part ->
            acting
            |> List.sumBy (fun (p, boost) -> if samePart p part then Boost.multiple p boost else 0)

/// The engine's `_applyDamage`.
let damageTaken (body: BodyPart list) (boosts: Boost list) (hits: int) (damage: int) : int =
    if List.isEmpty boosts || damage <= 0 then
        damage
    else
        let n = List.length body

        let _, reduced =
            List.zip body boosts
            |> List.indexed
            |> List.fold
                (fun (left, reduced) (i, (part, boost)) ->
                    if left <= 0.0 then
                        left, reduced
                    else
                        let ratio = Boost.damageRatio part boost
                        let taken = min (float (partHitsAt n hits i) / ratio) left
                        left - taken, reduced + taken * (1.0 - ratio))
                (float damage, 0.0)

        // Math.round, which rounds a half up.
        damage - int (floor (reduced + 0.5))

let toughHits (body: BodyPart list) (boosts: Boost list) (hits: int) : int =
    if List.isEmpty boosts then
        0
    else
        let n = List.length body

        List.zip body boosts
        |> List.indexed
        |> List.sumBy (fun (i, (part, boost)) ->
            if Boost.damageRatio part boost < 1.0 then
                partHitsAt n hits i
            else
                0)

let damageFor (body: BodyPart list) (boosts: Boost list) (hits: int) (taken: int) : int =
    if List.isEmpty boosts || taken <= 0 then
        taken
    else
        let n = List.length body

        let left, damage =
            List.zip body boosts
            |> List.indexed
            |> List.fold
                (fun (left, damage) (i, (part, boost)) ->
                    let take = min (partHitsAt n hits i) left
                    left - take, damage + float take / Boost.damageRatio part boost)
                (taken, 0.0)

        int (ceil (damage + float left - 1e-9))

/// A source in a neutral room yields half.
let heldOutputPerTick = 10

let neutralOutputPerTick = 5

let sourceRegenTime = 300

let buildPerWork = 5

/// UPGRADE_CONTROLLER_POWER.
let upgradeDrainPerWork = 1

/// CONTROLLER_MAX_UPGRADE_PER_TICK at `controllerMaxLevel`, summed over every
/// creep upgrading it; a creep past the tally upgrades and spends nothing.
let maxUpgradePerTick = 15

let controllerMaxLevel = 8

let controllerLevelProgress (level: int) =
    match level with
    | 1 -> 200
    | 2 -> 45_000
    | 3 -> 135_000
    | 4 -> 405_000
    | 5 -> 1_215_000
    | 6 -> 3_645_000
    | 7 -> 10_935_000
    | _ -> 0

/// The fatigue a weighted part makes stepping onto a road, plain and swamp.
let roadWeight = 1

let plainWeight = 2

let swampWeight = 10

let roadBuildCost = 300

/// CONSTRUCTION_COST_ROAD_SWAMP_RATIO: a swamp road's build, hits and decay.
let roadSwampRatio = 5

/// ROAD_DECAY_AMOUNT every ROAD_DECAY_TIME, on plain.
let roadDecayAmount = 100

let roadDecayTime = 1000

let roadHits = 5000

/// ROAD_WEAROUT: decay-timer ticks a step brings forward, per body part.
let roadWearout = 1

/// CONTAINER_DECAY every CONTAINER_DECAY_TIME, or _OWNED in an owned room.
let containerDecayAmount = 5000

let containerDecayTime = 100

let containerDecayTimeOwned = 500

let containerBuildCost = 5_000

let containerHits = 250_000

let tombstoneDecayPerPart = 5

/// A creep aged out is buried on its last tick: its tombstone's creep has this left.
let agedOutTicksToLive = 1

let rampartDecayAmount = 300

let rampartDecayTime = 100

/// REPAIR_COST: 0.01 energy a hit.
let repairHitsPerEnergy = 100

/// MAX_CONSTRUCTION_SITES, across every room.
let maxConstructionSites = 100

let roomSide = 50

let private spawnRow = [| 0; 1; 1; 1; 1; 1; 1; 2; 3 |]
let private extensionRow = [| 0; 0; 5; 10; 20; 30; 40; 50; 60 |]
let private linkRow = [| 0; 0; 0; 0; 0; 2; 3; 4; 6 |]
let private wallRow = [| 0; 0; 2500 |]
let private storageRow = [| 0; 0; 0; 0; 1 |]
let private towerRow = [| 0; 0; 0; 1; 1; 2; 2; 3; 6 |]
let private eighthRow = [| 0; 0; 0; 0; 0; 0; 0; 0; 1 |]
let private sixthRow = [| 0; 0; 0; 0; 0; 0; 1 |]
let private labRow = [| 0; 0; 0; 0; 0; 0; 3; 6; 10 |]
let private seventhRow = [| 0; 0; 0; 0; 0; 0; 0; 1 |]

/// CONTROLLER_STRUCTURES, keyed by the engine's `STRUCTURE_*` string; sites
/// count with standing structures.
let controllerStructures (structureType: string) (level: int) : int =
    let at (row: int[]) =
        if level < 0 then 0
        elif level >= row.Length then row.[row.Length - 1]
        else row.[level]

    match structureType with
    | "spawn" -> at spawnRow
    | "extension" -> at extensionRow
    | "link" -> at linkRow
    | "road" -> 2500
    | "constructedWall"
    | "rampart" -> at wallRow
    | "storage" -> at storageRow
    | "tower" -> at towerRow
    | "observer"
    | "powerSpawn"
    | "nuker" -> at eighthRow
    | "extractor"
    | "terminal" -> at sixthRow
    | "lab" -> at labRow
    | "container" -> 5
    | "factory" -> at seventhRow
    | _ -> 0
