module Fabot.Core.Tests.DecideTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide

/// A single idle spawn standing in the default room.
let spawn =
    {
        Name = "Spawn1"
        Id = "spawn-1"
        RoomName = "W1N1"
        IsSpawning = false
    }

/// The default room's bank holding the given energy against the given capacity.
let bank energy capacity =
    Map.ofList
        [
            "W1N1",
            {
                Available = energy
                Capacity = capacity
            }
        ]

/// A controller far from its downgrade deadline, stock intact.
let controllerAt level =
    {
        Id = "ctrl-1"
        Level = level
        TicksToDowngrade = 20000
        SafeModeAvailable = 1
        SafeModeActive = false
    }

/// A room this colony owns: the spawn room's control entry, and the rate
/// every source in it is priced at (ADR 0042). Owned and not reserved —
/// the two halves the engine gives the same 3,000 a cycle — because that
/// is what the colony's own room is.
let ownedRoom: RoomControlInfo =
    {
        Owner = Ownership.Ours
        Reservation = None
    }

/// A neutral room nobody holds: seen, and worth half. Not the same fact as
/// a room with no entry at all, which is one the colony cannot see and so
/// cannot price (ADR 0004).
let neutralRoom: RoomControlInfo =
    {
        Owner = Ownership.Unowned
        Reservation = None
    }

/// A room another player has taken: seen, owned, and owned by somebody
/// else (ADR 0043). The third answer to one question, which is why it is a
/// fixture of its own beside the two above rather than a flag on either.
let rivalRoom: RoomControlInfo =
    {
        Owner = Ownership.Rival
        Reservation = None
    }

/// A neutral room under a reservation of the given holder, with the given
/// ticks left on it: ours is what doubles the room, so `false` is the
/// rival's reservation, which reads as none at all for pricing.
let reservedRoom ours ticksToEnd : RoomControlInfo =
    {
        Owner = Ownership.Unowned
        Reservation =
            Some
                {
                    Holder =
                        if ours then
                            ReservationHolder.Ours
                        else
                            ReservationHolder.Rival
                    TicksToEnd = ticksToEnd
                }
    }

/// A neutral room whose reservation the NPC Invader holds — what a
/// level-0 invader core leaves behind it when it `attackController`s a
/// room it expanded into (ADR 0043). The third holder, and a fixture of
/// its own because it prices exactly as a rival's does and, under ADR
/// 0043, withdraws on the opposite rule.
let coreReservedRoom ticksToEnd : RoomControlInfo =
    {
        Owner = Ownership.Unowned
        Reservation =
            Some
                {
                    Holder = ReservationHolder.Invader
                    TicksToEnd = ticksToEnd
                }
    }

/// The control map for a colony holding its own room and nothing else.
/// Both names the fixtures below file a home layer under: `SpatialInfo.empty`
/// and the `spatial` funnel leave `RoomName` unset and file under the empty
/// name, `openRoom` names the room "W1N1", and the control map is keyed by
/// room like every other room-keyed fact (ADR 0041). Holding both keeps one
/// default honest for either funnel; an entry for a room the fixture has no
/// layer for is read by nothing.
let homeControl = Map.ofList [ "", ownedRoom; "W1N1", ownedRoom ]

/// A stocked source: a restock of zero, ready to dig now (ADR 0025).
let source id : SourceInfo = { Id = id; TicksToRestock = 0 }

/// A drained source, the given number of ticks from its restock (ADR 0025).
let drained id ticks : SourceInfo = { Id = id; TicksToRestock = ticks }

/// An energy-hungry structure of the given kind with the given free capacity.
let refillable id freeCapacity kind =
    {
        Id = id
        FreeCapacity = freeCapacity
        Kind = kind
    }

let bareRespawn =
    {
        Time = 42
        Spawns = [ spawn ]
        RoomEnergy = bank 300 300
        Refillables = [ refillable "spawn-1" 0 BuiltKind.Spawn ]
        Sources = [ source "src-a"; source "src-b" ]
        Controller = Some(controllerAt 1)
        RoomControl = homeControl
        ConstructionSites = []
        Creeps = []
        Hostiles = []
        InvaderCores = []
        Spatial = SpatialInfo.empty
    }

/// A creep with the given body's part counts, freshly cast: a full
/// Screeps CREEP_LIFE_TIME to live, so no fixture creep is expiring and no
/// lead has to be priced to read a test (ADR 0026).
let creepWith name energy freeCapacity body =
    {
        Name = name
        TicksToLive = 1500
        Fatigue = 0
        Energy = energy
        FreeCapacity = freeCapacity
        Body = body |> List.countBy id |> Map.ofList
    }

/// The same creep with the given ticks left to live — what puts it inside
/// its row's lead and makes it expiring (ADR 0026).
let withLife ticks (creep: CreepInfo) = { creep with TicksToLive = ticks }

/// A generalist worker-unit creep: one Work, one Carry, one Move.
let worker name energy freeCapacity =
    creepWith name energy freeCapacity [ Work; Carry; Move ]

let spawnIntents intents =
    intents
    |> List.choose (function
        | SpawnCreep(s, b, c) -> Some(s, b, c)
        | _ -> None)

[<Tests>]
let directionCodeTests =
    testList
        "direction codes"
        [
            test "matches the engine's TOP = 1, then clockwise" {
                // These constants leave the program as Creep.move arguments; the
                // table here is the engine's spec, restated so a swapped case fails.
                Expect.equal
                    ([ Top; TopRight; Right; BottomRight; Bottom; BottomLeft; Left; TopLeft ]
                     |> List.map directionCode)
                    [ 1; 2; 3; 4; 5; 6; 7; 8 ]
                    "each Direction maps to its Screeps constant"
            }
        ]

[<Tests>]
let partNameTests =
    testList
        "part names"
        [
            test "matches the engine's spelling, one name per part" {
                // These strings leave the program in spawnCreep bodies and
                // come back in hostile body arrays; the table is the engine's
                // spec, restated so a swapped or misspelt case fails.
                Expect.equal
                    (allBodyParts |> List.map partName)
                    [ "work"; "carry"; "move"; "attack"; "ranged_attack"; "heal"; "claim"; "tough" ]
                    "each BodyPart maps to its Screeps string"
            }
        ]

[<Tests>]
let builtKindTests =
    testList
        "built kinds"
        [
            test "matches the engine's spelling, one name per kind" {
                // These strings come back in `structureType` on structures and
                // sites and leave the program in createConstructionSite; the
                // table is the engine's spec, restated so a swapped or
                // misspelt case fails. Other is not among them: it is what an
                // unmatched string classifies to, not a kind the engine names.
                Expect.equal
                    (allBuiltKinds |> List.map builtKindName)
                    [
                        "spawn"
                        "extension"
                        "tower"
                        "road"
                        "container"
                        "storage"
                        "link"
                        "rampart"
                    ]
                    "each BuiltKind maps to its Screeps string"
            }

            test "Refill keeps the spawn, the extensions and the towers fed" {
                // The rank layer's own kinds (ADR 0010). The controller
                // container and the Storage are Refill targets too, but
                // pooled off the projection's stores (ADR 0012, ADR 0023) —
                // they never enter the Refillables list.
                Expect.equal
                    (allBuiltKinds |> List.filter isRefillable)
                    [ BuiltKind.Spawn; BuiltKind.Extension; BuiltKind.Tower ]
                    "the energy-hungry kinds alone are Refillables"

                Expect.isFalse
                    (isRefillable BuiltKind.Other)
                    "an unmodelled kind is no Refillable: the projection reads no free capacity off it"
            }

            test "each kind is whole at its own line: half of max, a floor, or full" {
                // The whole line per kind (ADR 0034), which is also the list
                // of kinds whose hits the projection carries at all: the
                // decaying roads and containers sit at a fraction of max
                // (ADR 0010), a rampart at its floor, and the Keep at full —
                // it does not decay, so below max means damaged. The numbers
                // themselves are the Repair pool's tunables and are not here.
                Expect.equal
                    (allBuiltKinds |> List.map (fun kind -> kind, wholeLine kind))
                    [
                        BuiltKind.Spawn, Some WholeLine.Full
                        BuiltKind.Extension, None
                        BuiltKind.Tower, Some WholeLine.Full
                        BuiltKind.Road, Some WholeLine.Fraction
                        BuiltKind.Container, Some WholeLine.Fraction
                        BuiltKind.Storage, Some WholeLine.Full
                        BuiltKind.Link, None
                        BuiltKind.Rampart, Some WholeLine.Floor
                    ]
                    "one line per kind, and none for the kinds Repair never touches"

                // The Keep is the list the other two rules hang off (the
                // rampart covering and, from #102, safe mode), so it must be
                // exactly the kinds repaired to full: a Keep kind repaired to
                // half would leave the safe-mode trigger armed for every
                // hostile that wandered through afterwards.
                Expect.equal
                    (allBuiltKinds |> List.filter isKeep)
                    (allBuiltKinds |> List.filter (fun kind -> wholeLine kind = Some WholeLine.Full))
                    "the Keep is exactly the kinds whose whole line is full hits"

                Expect.equal
                    (allBuiltKinds |> List.filter isKeep)
                    [ BuiltKind.Spawn; BuiltKind.Tower; BuiltKind.Storage ]
                    "the spawn, the tower and the Storage are the Keep"

                Expect.equal
                    (allBuiltKinds |> List.filter isStored)
                    [ BuiltKind.Container; BuiltKind.Storage ]
                    "the containers and the Storage alone put a store in the projection"

                // `allBuiltKinds` leaves Other out, so no filter above can
                // say anything about it — and Other is the arm with the worst
                // reach: the projection reads hits and a store off every kind
                // these admit, and an unmodelled structure carries neither.
                Expect.isNone
                    (wholeLine BuiltKind.Other)
                    "an unmodelled kind has no whole line and never enters the Repair pool"

                Expect.isFalse (isKeep BuiltKind.Other) "an unmodelled kind is no Keep structure"

                // The Raid log charges damage on the Keep and its cover
                // (ADR 0034) and on nothing else: a chewed road is the
                // colony's ordinary decay, not a raid's cost.
                Expect.equal
                    (allBuiltKinds |> List.filter isDefence)
                    [ BuiltKind.Spawn; BuiltKind.Tower; BuiltKind.Storage; BuiltKind.Rampart ]
                    "the Keep and the ramparts over it are what a raid's damage is read on"

                // Ownership is asked of every kind that has an owner and a
                // whole line: what stands in a room we took is not
                // automatically ours. The decaying kinds have no owner in
                // the engine, so asking would drop every road and container.
                Expect.equal
                    (allBuiltKinds |> List.filter needsOwner)
                    (allBuiltKinds |> List.filter isDefence)
                    "the ownable repairable kinds are exactly the Keep and the ramparts"

                Expect.isFalse
                    (needsOwner BuiltKind.Road)
                    "a road has no owner to ask about: it would vanish from the projection"

                Expect.isFalse (isDefence BuiltKind.Other) "an unmodelled kind is charged no damage"

                Expect.isFalse
                    (isStored BuiltKind.Other)
                    "an unmodelled kind puts no store in the projection"
            }

            test "a creep stands on a road, a container or a rampart, and on nothing else" {
                // Screeps OBSTACLE_OBJECT_TYPES, as the projection reads it:
                // every kind that is not walkable blocks its tile.
                Expect.equal
                    (allBuiltKinds |> List.filter isWalkable)
                    [ BuiltKind.Road; BuiltKind.Container; BuiltKind.Rampart ]
                    "the three kinds a creep may share a tile with"

                Expect.isFalse
                    (isWalkable BuiltKind.Other)
                    "a kind the decision layer does not model blocks its tile: Other never walks"
            }

            test "a placement Intent's kind widens to the built kind of the same name" {
                // The one crossing between the two vocabularies (#75). The
                // Executor spells a site through it and a projection rebuilt
                // on the .NET side classifies its pending sites through it,
                // so a transposed case would place one kind and describe
                // another with nothing in either layer to catch it.
                Expect.equal
                    ([ Extension; Tower; Road; Container; Storage ] |> List.map builtKindOfPlaceable)
                    [
                        BuiltKind.Extension
                        BuiltKind.Tower
                        BuiltKind.Road
                        BuiltKind.Container
                        BuiltKind.Storage
                    ]
                    "each placeable kind widens to its own built kind"
            }
        ]

[<Tests>]
let bodyTests =
    testList
        "worker body"
        [
            test "a 150 remainder buys two Carry and a Move" {
                // 550 = 2 units + 150: the old whole-unit body stranded 150.
                Expect.equal
                    (workerBodyFor 550)
                    [ Work; Work; Carry; Carry; Carry; Carry; Move; Move; Move ]
                    "remainder is spent at parity: max Carry without moving slower than the pure-unit body"
            }

            test "a 50 remainder buys a Move, not a Carry" {
                // A lone Carry would tip loaded fatigue past the pure-unit body's.
                Expect.equal
                    (workerBodyFor 250)
                    [ Work; Carry; Move; Move ]
                    "the trailing 50 goes to Move"
            }

            test "a 100 remainder buys a Carry/Move pair" {
                Expect.equal
                    (workerBodyFor 500)
                    [ Work; Work; Carry; Carry; Carry; Move; Move; Move ]
                    "a pair keeps parity and adds haul"
            }

            test "an exact multiple stays pure units" {
                Expect.equal
                    (workerBodyFor 800)
                    (List.replicate 4 Work @ List.replicate 4 Carry @ List.replicate 4 Move)
                    "no remainder, no pad"
            }

            test "below one unit cost the floor is one unit" {
                Expect.equal (workerBodyFor 150) [ Work; Carry; Move ] "never below one unit"
            }

            test "every capacity is spent to within a part price, at fatigue parity" {
                for capacity in 200..50..1300 do
                    let body = workerBodyFor capacity

                    let count part =
                        body |> List.filter ((=) part) |> List.length

                    let work, carry, move = count Work, count Carry, count Move

                    Expect.isLessThanOrEqual
                        (bodyCost body)
                        capacity
                        $"affordable at capacity {capacity}"

                    Expect.isLessThan
                        (capacity - bodyCost body)
                        50
                        $"nothing a part could buy is stranded at capacity {capacity}"

                    Expect.isLessThanOrEqual
                        (work + carry)
                        (2 * move)
                        $"loaded parity with the pure-unit body at capacity {capacity}"

                    Expect.isLessThanOrEqual
                        work
                        move
                        $"empty parity with the pure-unit body at capacity {capacity}"
            }

            test "the body never exceeds the 50-part engine cap" {
                // RCL8 capacity: unbounded replication would emit 192 parts,
                // which the engine rejects outright.
                Expect.equal
                    (workerBodyFor 12900)
                    (List.replicate 16 Work @ List.replicate 17 Carry @ List.replicate 17 Move)
                    "16 units plus a Carry/Move pair fill exactly 50 parts"
            }
        ]

[<Tests>]
let patternTableTests =
    testList
        "pattern table"
        [
            test "the worker unit, the Anchor, the hauler and the reserver are the table's rows" {
                // The reserver joined the table the tick its quota did (ADR
                // 0006, ADR 0042): a row arrives with the colony fact that
                // says when it is cast, and `reserverClaimsOf` is that
                // fact. The order here is the declaration's and not the
                // casting order — which runs reserver, Anchor, hauler,
                // worker — because nothing reads this list for a sequence.
                Expect.equal
                    patternTable
                    [
                        {
                            Name = "worker"
                            Block = [ Work; Carry; Move ]
                        }
                        {
                            Name = "anchor"
                            Block = [ Work; Work; Carry; Move ]
                        }
                        {
                            Name = "hauler"
                            Block = [ Carry; Carry; Move ]
                        }
                        {
                            Name = "reserver"
                            Block = [ Claim; Move ]
                        }
                    ]
                    "every body the colony casts comes from these rows"
            }

            test "every row of the table has a sizing rule that can size it" {
                // The generalist rule refuses a shape it cannot size
                // (#155), and that refusal fires inside `decide`, which
                // `Main.loop` calls under no handler — a row declared
                // without a rule of its own would cost every intent of
                // every tick, not one mis-shaped creep. So the table
                // itself is walked here: a row arriving without a sizing
                // rule is red at this gate the moment it is declared,
                // rather than in the colony the moment it is first cast.
                for row in patternTable do
                    Expect.isNonEmpty
                        (bodyFor row 1800)
                        $"the {row.Name} row sizes at an 1,800 bank"
            }

            test "the anchor row spends everything on Work beside one Carry and one Move" {
                // 550 = the RCL2 full bank: 100 buys the Carry/Move pair,
                // the rest is Work — no parity padding (ADR 0006 exempts
                // the Anchor from fatigue parity).
                Expect.equal
                    (bodyFor anchorPattern 550)
                    [ Work; Work; Work; Work; Carry; Move ]
                    "all remaining energy buys Work"
            }

            test "the anchor row never casts below its block" {
                Expect.equal
                    (bodyFor anchorPattern 300)
                    [ Work; Work; Carry; Move ]
                    "two Work keep the Anchor readable off its body (Work > Move)"
            }

            test "the anchor row stops at source saturation plus one spare Work" {
                // ADR 0021: a source regenerates 3,000 energy per 300 ticks
                // and a Work digs 2 a tick, so five Work saturate it; the
                // sixth is slack for an unmanned Post's gap. RCL4's 1,300
                // bank would otherwise buy twelve.
                Expect.equal
                    (bodyFor anchorPattern 1300)
                    [ Work; Work; Work; Work; Work; Work; Carry; Move ]
                    "six Work beside the Carry/Move pair, the rest stays banked"
            }

            test "the anchor cap holds at the richest bank" {
                Expect.equal
                    (bodyFor anchorPattern 12900)
                    [ Work; Work; Work; Work; Work; Work; Carry; Move ]
                    "an RCL8 bank casts the same six-Work Anchor"
            }

            test "the hauler row builds whole blocks and nothing else" {
                // 500 buys three whole blocks (450) and strands the rest:
                // the row's own declaration is road parity, which a padded
                // lone Carry would break — three Move pay six fatigue a
                // tick, seven loaded Carry on a road would generate seven.
                Expect.equal
                    (bodyFor haulerPattern 500)
                    (List.replicate 6 Carry @ List.replicate 3 Move)
                    "capacity buys whole [Carry;Carry;Move] blocks; the remainder stays banked"
            }

            test "the hauler row never casts below its block" {
                Expect.equal
                    (bodyFor haulerPattern 100)
                    [ Carry; Carry; Move ]
                    "the block is the row's minimal cast"
            }

            test "the hauler body never exceeds the 50-part engine cap" {
                Expect.equal
                    (bodyFor haulerPattern 9000)
                    (List.replicate 32 Carry @ List.replicate 16 Move)
                    "sixteen blocks fill 48 parts"
            }

            test "the reserver row builds whole blocks and nothing else" {
                // 1,800 is the colony's live RCL5 bank (#116's deployment
                // note) and 650 a `[Claim; Move]` block, so the bank buys
                // two and strands 500: ADR 0042's own `[2Claim;2Move]`,
                // the body every arithmetic in that ADR is written for.
                // This is the bank's half of the row's rule and the body
                // `bodyFor` answers with, which is what a lead prices its
                // succession off (ADR 0026). What the row actually casts
                // is `min(reservation deficit, this)`, pinned in "the
                // reserver row" below.
                Expect.equal
                    (bodyFor reserverPattern 1800)
                    [ Claim; Claim; Move; Move ]
                    "capacity buys whole [Claim; Move] blocks; the remainder stays banked"
            }

            test "the reserver row never casts below its block" {
                // A CLAIM part is indivisible: a body under one block
                // reserves nothing, and an empty body would price a
                // reserver's succession at zero ticks of cast time.
                Expect.equal
                    (bodyFor reserverPattern 100)
                    [ Claim; Move ]
                    "the block is the row's minimal cast"
            }

            test "the reserver body never exceeds the 50-part engine cap" {
                Expect.equal
                    (bodyFor reserverPattern 100000)
                    (List.replicate 25 Claim @ List.replicate 25 Move)
                    "twenty-five blocks fill the 50 parts exactly"
            }

            test "a row the generalist rule cannot size is refused, not quietly rebuilt" {
                // The fallback counts Work, Carry and Move out of a block
                // and emits only those, so the next table row that is not
                // one of those three — a guard, a healer — used to get a
                // body with none of its own parts in it and no complaint
                // from the compiler (#155). The stop names the row and the
                // part so the fix (its own sizing rule, ADR 0006) is
                // legible from the message alone.
                let guard =
                    {
                        Name = "guard"
                        Block = [ Attack; Move ]
                    }

                let message =
                    try
                        bodyFor guard 1800 |> ignore
                        "no exception"
                    with ex ->
                        ex.Message

                Expect.stringContains
                    message
                    "guard"
                    "the stop names the row that has no sizing rule"

                Expect.stringContains
                    message
                    "Attack"
                    "and the part the generalist rule would have dropped"
            }

            test "a Work/Carry/Move row the table does not name still sizes at parity" {
                // The stop is for the parts the rule cannot place, not for
                // rows the table has not grown yet: a block of Work, Carry
                // and Move is exactly what the generalist rule is written
                // for, whatever the row is called.
                let scout =
                    {
                        Name = "scout"
                        Block = [ Carry; Move ]
                    }

                Expect.equal
                    (bodyFor scout 300)
                    (List.replicate 3 Carry @ List.replicate 3 Move)
                    "three whole blocks, nothing dropped and nothing padded"
            }

            test "a row with no block at all is refused too, on both runtimes" {
                // The other shape the generalist rule cannot size, and the
                // one the two runtimes disagree about: .NET divides by
                // zero on the repeat count while the emitted JS reads
                // `~~(50 / 0)` as no repeats and pads a Carry/Move body
                // out of a row that asked for neither — the silent rebuild
                // #155 exists to end, visible only in the colony. The stop
                // names the row on both.
                let ghost = { Name = "ghost"; Block = [] }

                let message =
                    try
                        bodyFor ghost 1800 |> ignore
                        "no exception"
                    with ex ->
                        ex.Message

                Expect.stringContains message "ghost" "the stop names the row that has no block"

                Expect.stringContains
                    message
                    "no parts at all"
                    "and says which shape it is refusing to size"
            }

            test "spawn planning casts from the pattern table's row" {
                // An established colony at full capacity: the spawned body
                // is the table row sized to capacity, and the creep name
                // carries the row's name — not a hard-coded worker shape.
                let snapshot =
                    { bareRespawn with
                        RoomEnergy = bank 550 550
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                // The row by its name and not by its place in the table:
                // the declaration order is nobody's rule (see the pattern
                // table's own note), so a reordering must not fail here.
                let row = patternTable |> List.find (fun row -> row.Name = workerPattern.Name)

                match spawnIntents intents with
                | [ (_, body, creepName) ] ->
                    Expect.equal body (bodyFor row 550) "body is the row repeated by capacity"
                    Expect.stringStarts creepName $"{row.Name}-" "creep name carries the row's name"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }
        ]

[<Tests>]
let plannerTests =
    testList
        "planner"
        [
            test "one Harvest task per source" {
                let tasks = planTasks bareRespawn noThreats

                let harvests =
                    tasks
                    |> List.choose (function
                        | Harvest sourceId -> Some sourceId
                        | _ -> None)

                Expect.equal
                    harvests
                    [ "src-a"; "src-b" ]
                    "each source gets exactly one Harvest task"
            }

            test "a drained source pools its Harvest task all the same" {
                // ADR 0013's gate, inverted by ADR 0025: the task no longer
                // flickers with the source's stock, because whether a dry
                // rock is worth walking to depends on the walker's body and
                // position — the Matcher's knowledge, not the Planner's.
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a"; drained "src-b" 120 ]
                    }

                let harvests =
                    planTasks snapshot noThreats
                    |> List.choose (function
                        | Harvest sourceId -> Some sourceId
                        | _ -> None)

                Expect.equal
                    harvests
                    [ "src-a"; "src-b" ]
                    "the empty window is a wait to be judged at arrival, not a missing Task"
            }

            test "a controller yields an Upgrade task" {
                let upgrades =
                    planTasks bareRespawn noThreats
                    |> List.choose (function
                        | Upgrade id -> Some id
                        | _ -> None)

                Expect.equal upgrades [ "ctrl-1" ] "the controller gets exactly one Upgrade task"
            }

            test "no Upgrade task without a controller" {
                let tasks = planTasks { bareRespawn with Controller = None } noThreats

                let upgrades =
                    tasks
                    |> List.choose (function
                        | Upgrade id -> Some id
                        | _ -> None)

                Expect.isEmpty upgrades "nothing to upgrade"
            }

            test "each construction site yields a Build task" {
                let snapshot =
                    { bareRespawn with
                        ConstructionSites = [ { Id = "site-1" }; { Id = "site-2" } ]
                    }

                let builds =
                    planTasks snapshot noThreats
                    |> List.choose (function
                        | Build siteId -> Some siteId
                        | _ -> None)

                Expect.equal builds [ "site-1"; "site-2" ] "one Build task per construction site"
            }

            test "a structure missing energy gets a Refill task; a full structure gets none" {
                let snapshot =
                    { bareRespawn with
                        Refillables =
                            [
                                refillable "spawn-1" 50 BuiltKind.Spawn
                                refillable "ext-1" 0 BuiltKind.Extension
                                refillable "ext-2" 50 BuiltKind.Extension
                            ]
                    }

                let refills =
                    planTasks snapshot noThreats
                    |> List.choose (function
                        | Refill structureId -> Some structureId
                        | _ -> None)

                Expect.equal
                    refills
                    [ "spawn-1"; "ext-2" ]
                    "only structures with free capacity need a Refill"
            }

            test "a tower missing energy gets a Refill task; a full tower gets none" {
                // Same generalized Task, same free-capacity filter (ADR 0010) —
                // a tower is just one more energy-hungry structure to the Planner.
                let snapshot =
                    { bareRespawn with
                        Refillables =
                            [
                                refillable "tower-1" 500 BuiltKind.Tower
                                refillable "tower-2" 0 BuiltKind.Tower
                            ]
                    }

                let refills =
                    planTasks snapshot noThreats
                    |> List.choose (function
                        | Refill structureId -> Some structureId
                        | _ -> None)

                Expect.equal
                    refills
                    [ "tower-1" ]
                    "only the tower with free capacity needs a Refill"
            }
        ]

/// The home room's geometry, read back off a projection: the room
/// `RoomName` names, and the one the empty name files when it names none
/// (`SpatialInfo.homeName`). Absent geometry reads as an empty layer, never
/// as a lookup that throws (ADR 0004). The twin of `AtlasTests.homeLayer`;
/// the two suites share no module, as their `spatial` funnels already do
/// not.
let homeLayer (spatial: SpatialInfo) : RoomLayer =
    SpatialInfo.layerOf spatial (SpatialInfo.homeName spatial)

/// The same projection with the home room's layer changed. Since ADR 0041's
/// contract step the tile-shaped containers live under a room name and
/// nowhere else, so a test that used to copy-update the projection itself
/// — `{ colony.Spatial with CreepPositions = … }` — reaches through this
/// instead. It merges into whatever layer is already there, so composing it
/// with `withTargets` and `withOutpost` is order-blind. Apply it after
/// `RoomName` is final: the home name is resolved when it runs, and a
/// projection layered then renamed leaves its geometry filed under the old
/// name.
let withHome (change: RoomLayer -> RoomLayer) (spatial: SpatialInfo) : SpatialInfo =
    { spatial with
        Rooms = Map.add (SpatialInfo.homeName spatial) (change (homeLayer spatial)) spatial.Rooms
    }

/// Synthetic open room: every tile within `radius` of (25,25) is Plain,
/// with the spawn structure "spawn-1" standing at the centre.
let openRoom radius =
    let spawnPos = { X = 25; Y = 25 }

    { SpatialInfo.empty with
        RoomName = Some "W1N1"
        TargetKinds = Map.ofList [ "spawn-1", Structure BuiltKind.Spawn ]
    }
    |> withHome (fun layer ->
        { layer with
            Terrain =
                Map.ofList
                    [
                        for x in 25 - radius .. 25 + radius do
                            for y in 25 - radius .. 25 + radius do
                                { X = x; Y = y }, Plain
                    ]
            TargetPositions = Map.ofList [ "spawn-1", spawnPos ]
            Obstacles = Set.singleton spawnPos
        })

/// The room with extra targets standing (or being built) on given tiles.
let withTargets targets room =
    { room with
        TargetKinds =
            (room.TargetKinds, targets)
            ||> List.fold (fun acc (id, _, kind) -> Map.add id kind acc)
    }
    |> withHome (fun layer ->
        { layer with
            TargetPositions =
                (layer.TargetPositions, targets)
                ||> List.fold (fun acc (id, pos, _) -> Map.add id pos acc)
        })

let placementIntents intents =
    intents
    |> List.choose (function
        | PlaceConstructionSite(room, pos, kind) -> Some(room, pos, kind)
        | _ -> None)

let placedTiles intents =
    placementIntents intents |> List.map (fun (_, pos, _) -> pos)

/// The tiles a plan places one kind of site on, in plan order.
let sitesOfKind kind intents =
    placementIntents intents
    |> List.choose (fun (_, pos, k) -> if k = kind then Some pos else None)

let atLevel level room =
    { bareRespawn with
        Controller = Some(controllerAt level)
        Spatial = room
    }

[<Tests>]
let placementTests =
    testList
        "placement"
        [
            test "RCL2 on open terrain places 5 extensions checkerboard, nearest first" {
                let { Intents = intents } = decide (atLevel 2 (openRoom 3)) Map.empty Set.empty None

                // The nearest checkerboard tile (24,24) is the Storage's pick
                // and (24,26) and (26,24) are the two towers' under the RCL5
                // horizon (ADR 0039) — reservations, not sites: RCL2 allows
                // no tower, but their picks still come first in the one
                // ordering — so the extensions start three tiles in. A golden
                // value of the horizon, not an assertion about the ordering:
                // the rule is unchanged, the list moved.
                Expect.equal
                    (sitesOfKind Extension intents)
                    [
                        { X = 26; Y = 26 }
                        { X = 23; Y = 23 }
                        { X = 23; Y = 25 }
                        { X = 23; Y = 27 }
                        { X = 25; Y = 23 }
                    ]
                    "the last diagonal neighbour, then rank-2 checkerboard tiles"

                for (room, _, kind) in placementIntents intents do
                    Expect.equal room "W1N1" "sites go in the spawn's room"

                    Expect.isTrue
                        (kind = Extension || kind = Rampart)
                        "the extensions the level unlocks, and the spawn's own rampart"
            }

            test "RCL5 on open terrain plans the whole level: 30 extensions, two towers" {
                // The clustered kinds are sized at the horizon and filtered at
                // the current level, so a room standing at the horizon's own
                // level plans everything the engine unlocked there (ADR 0039).
                // A horizon left behind computes a gap of zero here and asks
                // for none of it. The room is a ring wider than the fixtures
                // beside it for the same reason: thirty extensions, two
                // towers, the Storage and the footings want more same-colour
                // tiles than `openRoom 3` has.
                let { Intents = intents } = decide (atLevel 5 (openRoom 5)) Map.empty Set.empty None

                Expect.hasLength
                    (sitesOfKind Extension intents)
                    30
                    "RCL5's whole extension allowance, the ten the level adds included"

                Expect.hasLength
                    (sitesOfKind Tower intents)
                    2
                    "both towers RCL5 allows, the second one the horizon held a tile for"
            }

            test "below RCL2 no placement Intents are emitted" {
                let { Intents = intents } = decide (atLevel 1 (openRoom 3)) Map.empty Set.empty None

                Expect.isEmpty
                    (placementIntents intents)
                    "no extensions allowed at RCL1, and no rampart either: the engine allows neither"
            }

            test "unwalkable tiles are skipped" {
                let room = openRoom 3

                let holed =
                    room
                    |> withHome (fun layer ->
                        { layer with
                            Terrain = Map.add { X = 24; Y = 24 } Wall layer.Terrain
                        })

                let { Intents = intents } = decide (atLevel 2 holed) Map.empty Set.empty None

                Expect.isFalse
                    (List.contains { X = 24; Y = 24 } (placedTiles intents))
                    "wall tile is never chosen"

                Expect.hasLength
                    (sitesOfKind Extension intents)
                    5
                    "the cap is still reached elsewhere"
            }

            test "occupied tiles are skipped" {
                let blocked =
                    openRoom 3
                    |> withTargets [ "rock-1", { X = 24; Y = 24 }, Structure BuiltKind.Other ]

                let { Intents = intents } = decide (atLevel 2 blocked) Map.empty Set.empty None

                Expect.isFalse
                    (List.contains { X = 24; Y = 24 } (placedTiles intents))
                    "occupied tile is never chosen"

                Expect.hasLength
                    (sitesOfKind Extension intents)
                    5
                    "the cap is still reached elsewhere"
            }

            test "built extensions and pending sites count against the cap" {
                let room =
                    openRoom 3
                    |> withTargets
                        [
                            "ext-1", { X = 24; Y = 24 }, Structure BuiltKind.Extension
                            "ext-2", { X = 24; Y = 26 }, Structure BuiltKind.Extension
                            "site-1", { X = 26; Y = 24 }, Site BuiltKind.Extension
                            "site-2", { X = 26; Y = 26 }, Site BuiltKind.Extension
                        ]

                let { Intents = intents } = decide (atLevel 2 room) Map.empty Set.empty None

                Expect.hasLength (sitesOfKind Extension intents) 1 "only the shortfall is placed"
            }

            test "no placement Intents once the allowance is exhausted" {
                let room =
                    openRoom 3
                    |> withTargets
                        [
                            for i in 1..5 ->
                                $"ext-{i}", { X = 22 + i; Y = 22 }, Structure BuiltKind.Extension
                        ]

                let { Intents = intents } = decide (atLevel 2 room) Map.empty Set.empty None
                Expect.isEmpty (sitesOfKind Extension intents) "allowance already used up"
            }

            test "the controller's tile is never chosen" {
                // The controller stands on a free same-colour tile the old
                // Placement projection would have offered to a site.
                let room = openRoom 3 |> withTargets [ "ctrl-1", { X = 24; Y = 24 }, Controller ]

                let { Intents = intents } = decide (atLevel 2 room) Map.empty Set.empty None

                Expect.isFalse
                    (List.contains { X = 24; Y = 24 } (placedTiles intents))
                    "a target's tile is never chosen"

                // One short of RCL2's cap, and that is the horizon's price
                // paid at today's level (ADR 0039): the controller's own
                // Upgrade Work Area is working ground, so this room offers
                // seven tiles, and the second tower's reservation sits ahead
                // of the extensions in the one ordering.
                Expect.hasLength
                    (sitesOfKind Extension intents)
                    4
                    "the cap is not reached: no tile is spare for the second tower's reservation"
            }

            test "no placement Intents without a projected room" {
                let snapshot =
                    { bareRespawn with
                        Controller = Some(controllerAt 2)
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (placementIntents intents) "nothing to plan around"
            }
        ]

/// The trunk fixture (ADR 0011): a broad plain field with the spawn at
/// (25,25), the controller at (35,25), one source embedded in wall terrain
/// at (15,25), two swamps inside the controller's Upgrade Work Area, one
/// far swamp off every trunk line, and two extensions already built on the
/// cluster's nearest tiles.
let trunkRoom =
    let sourcePos = { X = 15; Y = 25 }
    let spawnPos = { X = 25; Y = 25 }
    let controllerPos = { X = 35; Y = 25 }
    let builtExtensions = [ { X = 24; Y = 26 }; { X = 26; Y = 24 } ]
    let areaSwamps = [ { X = 33; Y = 27 }; { X = 34; Y = 24 } ]

    { SpatialInfo.empty with
        RoomName = Some "W1N1"
        TargetKinds =
            Map.ofList
                [
                    "spawn-1", Structure BuiltKind.Spawn
                    "ctrl-1", Controller
                    "src-a", Source
                    "ext-1", Structure BuiltKind.Extension
                    "ext-2", Structure BuiltKind.Extension
                ]
    }
    |> withHome (fun layer ->
        { layer with
            Terrain =
                Map.ofList
                    [
                        for x in 10..40 do
                            for y in 15..35 do
                                let tile = { X = x; Y = y }

                                tile,
                                (if tile = sourcePos then
                                     Wall
                                 elif
                                     List.contains tile areaSwamps || tile = { X = 20; Y = 20 }
                                 then
                                     Swamp
                                 else
                                     Plain)
                    ]
            TargetPositions =
                Map.ofList
                    [
                        "spawn-1", spawnPos
                        "ctrl-1", controllerPos
                        "src-a", sourcePos
                        "ext-1", builtExtensions.[0]
                        "ext-2", builtExtensions.[1]
                    ]
            Obstacles = Set.ofList (spawnPos :: controllerPos :: builtExtensions)
        })

/// The trunk fixture's colony at a controller level.
let trunkColony level =
    { bareRespawn with
        Sources = [ source "src-a" ]
        Controller = Some(controllerAt level)
        Spatial = trunkRoom
    }

/// The trunk fixture without its source: nothing to pave a trunk from.
let noSourceColony level =
    { trunkColony level with
        Sources = []
        Spatial =
            { trunkRoom with
                TargetKinds = Map.remove "src-a" trunkRoom.TargetKinds
            }
            |> withHome (fun layer ->
                { layer with
                    TargetPositions = Map.remove "src-a" layer.TargetPositions
                })
    }

/// The trunk fixture with a second source walled into a pocket: every
/// neighbour of (20,30) is wall terrain except the single Seat east of
/// it — the W12S28-source-B shape (ADR 0012).
let pocketColony level =
    let srcB = { X = 20; Y = 30 }
    let seat = { X = 21; Y = 30 }

    let walled =
        [
            for dx in -1 .. 1 do
                for dy in -1 .. 1 do
                    { X = srcB.X + dx; Y = srcB.Y + dy }
        ]
        |> List.filter (fun tile -> tile <> seat)

    let room =
        trunkRoom
        |> withHome (fun layer ->
            { layer with
                Terrain = (layer.Terrain, walled) ||> List.fold (fun acc t -> Map.add t Wall acc)
            })
        |> withTargets [ "src-b", srcB, Source ]

    { trunkColony level with
        Sources = [ source "src-a"; source "src-b" ]
        Spatial = room
    }

/// The colony with its own road plan already standing: the state the
/// source containers drop in — a container defers to a road site on its
/// tile (one construction site per tile) and coexists with the built road.
let withRoadsBuilt colony =
    let { Intents = intents } = decide colony Map.empty Set.empty None

    { colony with
        Spatial =
            colony.Spatial
            |> withHome (fun layer ->
                { layer with
                    Roads = sitesOfKind Road intents |> Set.ofList
                })
    }

let chebyshev a b = max (abs (a.X - b.X)) (abs (a.Y - b.Y))

/// The clustered structures of a plan: the Storage, the tower and every
/// extension, the tiles one ordering rule picks (ADR 0011, ADR 0022).
let clusterTiles intents =
    sitesOfKind Storage intents
    @ sitesOfKind Tower intents
    @ sitesOfKind Extension intents
    |> Set.ofList

/// The clustered ordering's sort key for a fixture whose spawn stands at
/// (25,25): nearest-to-spawn first, ties by x then y (ADR 0011).
let orderKey tile =
    chebyshev tile { X = 25; Y = 25 }, tile.X, tile.Y

[<Tests>]
let layoutTests =
    testList
        "layout"
        [
            test "RCL2 places the extension gap and every trunk road, no tower" {
                let { Intents = intents } = decide (trunkColony 2) Map.empty Set.empty None

                Expect.isEmpty (sitesOfKind Tower intents) "no tower below RCL3"

                Expect.hasLength
                    (sitesOfKind Extension intents)
                    3
                    "only the gap against the two built extensions is placed"

                let roads = sitesOfKind Road intents |> Set.ofList

                Expect.isTrue
                    (roads |> Set.exists (fun t -> chebyshev t { X = 15; Y = 25 } = 1))
                    "a trunk starts beside the source"

                Expect.isTrue
                    (roads |> Set.exists (fun t -> chebyshev t { X = 25; Y = 25 } = 1))
                    "a trunk ends beside the spawn"

                Expect.isTrue
                    (roads |> Set.exists (fun t -> chebyshev t { X = 35; Y = 25 } <= 3))
                    "a trunk reaches the controller's Work Area"

                Expect.contains roads { X = 33; Y = 27 } "a Work Area swamp is paved"
                Expect.contains roads { X = 34; Y = 24 } "the other Work Area swamp is paved"

                Expect.isFalse
                    (Set.contains { X = 20; Y = 20 } roads)
                    "a swamp off every trunk line is not paved"
            }

            test "the same fixture at RCL3 adds the tower and extensions 6-10 at once" {
                let { Intents = intents } = decide (trunkColony 3) Map.empty Set.empty None

                // (24,24) is the ordering's first free tile and the Storage's
                // reservation (ADR 0022); the tower takes the one after it,
                // and the fixture's two built extensions hold (24,26)/(26,24).
                Expect.equal
                    (sitesOfKind Tower intents)
                    [ { X = 26; Y = 26 } ]
                    "the tower takes the ordering's first free tile after the Storage's"

                let extensions = sitesOfKind Extension intents
                Expect.hasLength extensions 8 "the RCL3 allowance fills against the two built"

                for tile in extensions do
                    Expect.isLessThan
                        (orderKey { X = 26; Y = 26 })
                        (orderKey tile)
                        "the tower's pick comes before every extension in the one ordering"
            }

            test "the same Snapshot recomputes to the identical site set" {
                let first = decide (trunkColony 2) Map.empty Set.empty None
                let second = decide (trunkColony 2) Map.empty Set.empty None

                Expect.equal
                    (placementIntents first.Intents)
                    (placementIntents second.Intents)
                    "the Layout is deterministic — sites never jitter between computations"
            }

            test "trunks route around every horizon reservation" {
                let rcl2 = decide (trunkColony 2) Map.empty Set.empty None
                let rcl4 = decide (trunkColony 4) Map.empty Set.empty None
                let roads = sitesOfKind Road rcl2.Intents |> Set.ofList

                // Read off the horizon's own level, where the whole
                // reservation is on the ground — the second tower and the
                // ten extensions RCL5 adds included (ADR 0039). Below it the
                // check only ever saw the part the level had placed.
                let cluster = clusterTiles (decide (trunkColony 5) Map.empty Set.empty None).Intents

                Expect.equal
                    (sitesOfKind Road rcl4.Intents |> Set.ofList)
                    roads
                    "the road plan is the same at every level — the horizon never moves"

                Expect.isEmpty
                    (Set.intersect roads cluster)
                    "no trunk tile coincides with a reserved structure tile"
            }

            test "a Seat beside the spawn is working ground: no tower, no extension" {
                // The source stands two tiles north of the spawn, so four of
                // its Seats are the cluster's own nearest same-colour tiles.
                let sourcePos = { X = 25; Y = 23 }

                let colony = atLevel 3 (openRoom 6 |> withTargets [ "src-a", sourcePos, Source ])

                let { Intents = intents } = decide colony Map.empty Set.empty None

                let seats =
                    Set.ofList
                        [
                            for x in sourcePos.X - 1 .. sourcePos.X + 1 do
                                for y in sourcePos.Y - 1 .. sourcePos.Y + 1 do
                                    if { X = x; Y = y } <> sourcePos then
                                        { X = x; Y = y }
                        ]

                let cluster = clusterTiles intents

                Expect.isNonEmpty cluster "the cluster still fills, one ring out"

                Expect.isEmpty
                    (Set.intersect cluster seats)
                    "no clustered structure eats a Seat the Anchors stand on"
            }

            test "the Upgrade Work Area is working ground: no tower, no extension" {
                // The controller stands four tiles north of the spawn, so its
                // Upgrade Work Area covers the cluster's nearest same-colour
                // tiles without covering the spawn itself.
                let controllerPos = { X = 25; Y = 21 }

                let colony =
                    atLevel 3 (openRoom 6 |> withTargets [ "ctrl-1", controllerPos, Controller ])

                let { Intents = intents } = decide colony Map.empty Set.empty None

                let upgradeArea =
                    Set.ofList
                        [
                            for x in controllerPos.X - 3 .. controllerPos.X + 3 do
                                for y in controllerPos.Y - 3 .. controllerPos.Y + 3 do
                                    { X = x; Y = y }
                        ]

                let cluster = clusterTiles intents

                Expect.isNonEmpty cluster "the cluster still fills, one ring out"

                Expect.isEmpty
                    (Set.intersect cluster upgradeArea)
                    "no clustered structure eats a tile an upgrader stands on"
            }

            test "without a source only the Work Area swamps are paved, never plain" {
                let { Intents = intents } = decide (noSourceColony 2) Map.empty Set.empty None

                Expect.equal
                    (sitesOfKind Road intents |> Set.ofList)
                    (Set.ofList [ { X = 33; Y = 27 }; { X = 34; Y = 24 } ])
                    "exactly the Work Area's swamp tiles get roads"
            }

            test "built roads and pending road sites are never placed again" {
                let colony = noSourceColony 2

                let snapshot =
                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    Roads = Set.singleton { X = 33; Y = 27 }
                                })
                            |> withTargets
                                [ "road-site-1", { X = 34; Y = 24 }, Site BuiltKind.Road ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.isEmpty
                    (sitesOfKind Road intents)
                    "the gap reads the projection's road census: both tiles are claimed"
            }

            test "each source gets one container on the Seat where its trunk starts" {
                let colony = withRoadsBuilt (trunkColony 2)
                let { Intents = intents } = decide colony Map.empty Set.empty None

                let sourceContainers =
                    sitesOfKind Container intents
                    |> List.filter (fun tile -> chebyshev tile { X = 15; Y = 25 } = 1)

                Expect.hasLength sourceContainers 1 "one container per source"

                Expect.contains
                    (homeLayer colony.Spatial).Roads
                    sourceContainers.Head
                    "the Seat nearest the trunk is the trunk's own first tile"
            }

            test "a container never shares a tile with a planned road site" {
                // One construction site per tile (engine rule): on a fresh
                // plan the source container defers to the trunk road site
                // under it and drops only once that road stands.
                let { Intents = intents } = decide (trunkColony 2) Map.empty Set.empty None
                let roads = sitesOfKind Road intents |> Set.ofList

                for tile in sitesOfKind Container intents do
                    Expect.isFalse
                        (Set.contains tile roads)
                        "the container waits for the road on its tile"
            }

            test "the controller container lands in the Work Area beside a trunk" {
                let { Intents = intents } = decide (trunkColony 2) Map.empty Set.empty None
                let controllerPos = { X = 35; Y = 25 }

                let controllerContainers =
                    sitesOfKind Container intents
                    |> List.filter (fun tile -> chebyshev tile controllerPos <= 3)

                Expect.hasLength controllerContainers 1 "exactly one controller container"

                let tile = controllerContainers.Head
                let roads = sitesOfKind Road intents |> Set.ofList

                Expect.isTrue
                    (roads |> Set.exists (fun road -> chebyshev road tile = 1))
                    "the container sits adjacent to a trunk tile"

                Expect.isFalse (Set.contains tile roads) "the container stays off the road itself"
            }

            test "containers have no RCL gate — level 1 already places both kinds" {
                let { Intents = intents } =
                    decide (withRoadsBuilt (trunkColony 1)) Map.empty Set.empty None

                Expect.hasLength
                    (sitesOfKind Container intents)
                    2
                    "one source container and one controller container"
            }

            test "a one-Seat source gets its container on that Seat" {
                let { Intents = intents } =
                    decide (withRoadsBuilt (pocketColony 2)) Map.empty Set.empty None

                Expect.contains
                    (sitesOfKind Container intents)
                    { X = 21; Y = 30 }
                    "the single Seat is the nearest Seat to the pocket source's trunk"
            }

            test "built containers and pending container sites are never placed again" {
                let colony = withRoadsBuilt (trunkColony 2)
                let planned = decide colony Map.empty Set.empty None

                let standing =
                    match sitesOfKind Container planned.Intents with
                    | [ a; b ] ->
                        [
                            "can-1", a, Structure BuiltKind.Container
                            "can-site-1", b, Site BuiltKind.Container
                        ]
                    | other -> failtest $"expected two planned container sites, got %A{other}"

                let snapshot =
                    { colony with
                        Spatial = colony.Spatial |> withTargets standing
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.isEmpty
                    (sitesOfKind Container intents)
                    "the census claims both tiles: nothing re-drops"

                Expect.equal
                    (sitesOfKind Road intents)
                    (sitesOfKind Road planned.Intents)
                    "standing containers never perturb the road plan"
            }

            test "a container off its source's pick serves that source (#74)" {
                // The pick moves when the trunk moves, and the container
                // standing on the old pick is still the only container this
                // source has (ADR 0040). The target is served wherever the
                // thing serving it sits, so no second site drops beside it.
                let srcPos = { X = 15; Y = 25 }
                let colony = withRoadsBuilt (trunkColony 4)
                let planned = decide colony Map.empty Set.empty None

                let pick =
                    sitesOfKind Container planned.Intents
                    |> List.find (fun tile -> chebyshev tile srcPos <= 1)

                // (14,24) is a Seat of the same source and not the pick —
                // the container a previous plan left standing.
                let orphan = { X = 14; Y = 24 }

                let offPick =
                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withTargets [ "can-old", orphan, Structure BuiltKind.Container ]
                    }

                let after = decide offPick Map.empty Set.empty None

                Expect.isEmpty
                    (sitesOfKind Container after.Intents
                     |> List.filter (fun tile -> chebyshev tile srcPos <= 1))
                    "the standing container serves the source: its own pick is not placed"

                Expect.equal
                    after.Memo.DeferredContainers
                    [
                        {
                            Target = ContainerTarget.Source "src-a"
                            Pick = pick
                            Serving = orphan
                        }
                    ]
                    "the record names the target, the tile the plan picked and the tile serving it"

                // The tick after the plan runs: everything it placed now
                // stands. This is where the defect was paid for — a second
                // container on the pick is a second Post and a second term
                // in the hauler quota, forever.
                let built =
                    { offPick with
                        Spatial =
                            offPick.Spatial
                            |> withTargets
                                [
                                    for i, tile in
                                        List.indexed (sitesOfKind Container after.Intents) ->
                                        $"can-new-{i}", tile, Structure BuiltKind.Container
                                ]
                    }

                Expect.equal
                    (Atlas.posts (Atlas.ofSnapshot built))
                    (Set.singleton orphan)
                    "one Post, on the Seat the container actually stands on"

                Expect.equal
                    (decide built Map.empty Set.empty None).Memo.HaulerQuota
                    2
                    "the hauler row is sized for one source container — the orphan's own term"
            }

            test "a pending container site off the pick serves the source too (#74)" {
                // A site is already going up: judging the target from
                // standing containers alone would drop a second site beside
                // a site, the same defect one tick earlier (ADR 0040).
                let srcPos = { X = 15; Y = 25 }
                let colony = withRoadsBuilt (trunkColony 4)
                let orphan = { X = 14; Y = 24 }

                let offPick =
                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withTargets [ "can-site-old", orphan, Site BuiltKind.Container ]
                    }

                let after = decide offPick Map.empty Set.empty None

                Expect.isEmpty
                    (sitesOfKind Container after.Intents
                     |> List.filter (fun tile -> chebyshev tile srcPos <= 1))
                    "the pending site serves the source: its own pick is not placed"

                Expect.equal
                    (after.Memo.DeferredContainers |> List.map (fun d -> d.Target, d.Serving))
                    [ ContainerTarget.Source "src-a", orphan ]
                    "the deferral is recorded for a pending site as for a standing container"
            }

            test "a container anywhere in the Work Area serves the controller (#74)" {
                let controllerPos = { X = 35; Y = 25 }
                let colony = withRoadsBuilt (trunkColony 4)
                let planned = decide colony Map.empty Set.empty None

                let pick =
                    sitesOfKind Container planned.Intents
                    |> List.find (fun tile -> chebyshev tile controllerPos <= 3)

                // An Upgrade Work Area tile that is not the pick: the
                // controller's container, left where an older plan put it.
                let orphan =
                    [
                        for x in controllerPos.X - 3 .. controllerPos.X + 3 do
                            for y in controllerPos.Y - 3 .. controllerPos.Y + 3 do
                                { X = x; Y = y }
                    ]
                    |> List.find (fun tile -> tile <> pick && tile <> controllerPos)

                let offPick =
                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withTargets [ "can-ctrl", orphan, Structure BuiltKind.Container ]
                    }

                let after = decide offPick Map.empty Set.empty None

                Expect.isEmpty
                    (sitesOfKind Container after.Intents
                     |> List.filter (fun tile -> chebyshev tile controllerPos <= 3))
                    "the standing container serves the controller: its own pick is not placed"

                Expect.equal
                    after.Memo.DeferredContainers
                    [
                        {
                            Target = ContainerTarget.Controller
                            Pick = pick
                            Serving = orphan
                        }
                    ]
                    "the controller's deferral is recorded beside the sources'"
            }

            test "a container on its own pick is served, not deferred (#74)" {
                // The coinciding case: the plan wants exactly the tile the
                // container stands on, so nothing is lost and the record
                // stays empty (ADR 0040).
                let colony = withRoadsBuilt (trunkColony 4)
                let planned = decide colony Map.empty Set.empty None

                let standing =
                    sitesOfKind Container planned.Intents
                    |> List.mapi (fun i tile -> $"can-{i}", tile, Structure BuiltKind.Container)

                let after =
                    decide
                        { colony with
                            Spatial = colony.Spatial |> withTargets standing
                        }
                        Map.empty
                        Set.empty
                        None

                Expect.isEmpty (sitesOfKind Container after.Intents) "nothing re-drops"

                Expect.isEmpty
                    after.Memo.DeferredContainers
                    "a pick that never moved lost nothing and records nothing"
            }
        ]

[<Tests>]
let storageTests =
    testList
        "storage"
        [
            test "RCL4 places one Storage on the ordering's first pick, the tower next" {
                // The cluster's nearest same-colour tile is the Storage's at
                // every level (ADR 0022) — the tower and the extensions take
                // the picks after it.
                let { Intents = intents } = decide (atLevel 4 (openRoom 3)) Map.empty Set.empty None

                Expect.equal
                    (sitesOfKind Storage intents)
                    [ { X = 24; Y = 24 } ]
                    "one Storage, on the ordering's first still-open pick"

                Expect.equal
                    (sitesOfKind Tower intents)
                    [ { X = 24; Y = 26 } ]
                    "the tower's pick is the one after the Storage's"

                for tile in sitesOfKind Extension intents do
                    Expect.isLessThan
                        (orderKey { X = 24; Y = 24 })
                        (orderKey tile)
                        "the Storage's pick comes before every extension in the one ordering"
            }

            test "RCL3 places no Storage yet still holds its tile against the cluster" {
                // The reservation is level-blind (ADR 0022): once an extension
                // takes that tile it never comes back, so it is held from the
                // first tick, levels before the engine allows the Storage.
                let { Intents = intents } = decide (atLevel 3 (openRoom 3)) Map.empty Set.empty None

                Expect.isEmpty
                    (sitesOfKind Storage intents)
                    "the engine allows no Storage below RCL4"

                Expect.isNonEmpty (sitesOfKind Extension intents) "the cluster still fills"

                Expect.isFalse
                    (List.contains { X = 24; Y = 24 } (placedTiles intents))
                    "nothing at all is placed on the reserved first pick"
            }

            test "a standing Storage places none, and its tile leaves the ordering" {
                let standing =
                    openRoom 3
                    |> withTargets [ "sto-1", { X = 24; Y = 24 }, Structure BuiltKind.Storage ]

                let { Intents = intents } = decide (atLevel 4 standing) Map.empty Set.empty None

                Expect.isEmpty
                    (sitesOfKind Storage intents)
                    "the standing census fills the allowance"

                Expect.isFalse
                    (Set.contains { X = 24; Y = 24 } (clusterTiles intents))
                    "a standing structure's tile is not buildable: no cluster pick lands on it"

                // The one thing that is planned onto it: its rampart. A
                // rampart is no footprint, so the Storage's own tile is where
                // it belongs (ADR 0034).
                Expect.contains
                    (sitesOfKind Rampart intents)
                    { X = 24; Y = 24 }
                    "a standing Storage is a Keep structure and gets its cover"
            }

            test "a pending Storage site places none" {
                let pending =
                    openRoom 3
                    |> withTargets [ "sto-site", { X = 24; Y = 24 }, Site BuiltKind.Storage ]

                let { Intents = intents } = decide (atLevel 4 pending) Map.empty Set.empty None

                Expect.isEmpty
                    (sitesOfKind Storage intents)
                    "the pending census fills the allowance too: nothing re-drops"

                Expect.isFalse
                    (List.contains { X = 24; Y = 24 } (placedTiles intents))
                    "a site is a target too: nothing else is planned onto its tile"
            }

            test "the trunks and both container picks keep off the Storage tile" {
                // (24,24) is this fixture's cheapest last step from the
                // source into the spawn: unreserved, the trunk takes it. The
                // reservation is impassable before the trunks are priced, so
                // the lane ends on (24,25) instead. The container picks miss
                // it by construction (ADR 0022): the Storage comes from the
                // clustered ordering, which excludes the working ground,
                // while both container picks draw only from working ground —
                // the Seats and the Upgrade Work Area.
                let colony = withRoadsBuilt (trunkColony 4)
                let { Intents = intents } = decide colony Map.empty Set.empty None

                let storage = sitesOfKind Storage intents |> Set.ofList
                let containers = sitesOfKind Container intents |> Set.ofList

                Expect.isNonEmpty storage "the Storage is planned at RCL4"
                Expect.isNonEmpty containers "the containers drop once their roads stand"

                Expect.isEmpty
                    (Set.intersect storage (homeLayer colony.Spatial).Roads)
                    "no trunk road crosses the Storage's tile"

                Expect.isEmpty
                    (Set.intersect storage containers)
                    "the Storage's tile is never a container's"
            }

            test "the cluster keeps its tiles as the Storage goes from reserved to standing" {
                // The recomputation that can move something: a standing
                // Storage leaves the ordering and frees its slot at once, so
                // the tower and every extension keep the picks they had
                // while the tile was only reserved (ADR 0022).
                let planned = decide (atLevel 4 (openRoom 3)) Map.empty Set.empty None

                let storageTile =
                    match sitesOfKind Storage planned.Intents with
                    | [ tile ] -> tile
                    | other -> failtest $"expected one planned Storage, got %A{other}"

                let built =
                    decide
                        (atLevel
                            4
                            (openRoom 3
                             |> withTargets [ "sto-1", storageTile, Structure BuiltKind.Storage ]))
                        Map.empty
                        Set.empty
                        None

                Expect.equal
                    (sitesOfKind Tower built.Intents)
                    (sitesOfKind Tower planned.Intents)
                    "the tower's pick is the same tile either way"

                Expect.equal
                    (sitesOfKind Extension built.Intents)
                    (sitesOfKind Extension planned.Intents)
                    "and every extension's, in the same order"
            }

            test "a standing Storage changes neither the hauler quota nor the trunk plan" {
                // The spawn stays the trunk hub (ADR 0022): the Storage sits
                // beside it by construction and hires no haul capacity of its
                // own — the quota counts source containers (ADR 0012). One
                // stands on the source's trunk Seat, so the quota is a real
                // number on both sides of the comparison.
                let colony =
                    { trunkColony 4 with
                        Spatial =
                            trunkRoom
                            |> withTargets
                                [ "can-a", { X = 16; Y = 24 }, Structure BuiltKind.Container ]
                    }

                let planned = decide colony Map.empty Set.empty None

                Expect.isGreaterThan
                    planned.Memo.HaulerQuota
                    0
                    "the standing source container hires the haulers the Storage must not"

                let storageTile =
                    match sitesOfKind Storage planned.Intents with
                    | [ tile ] -> tile
                    | other -> failtest $"expected one planned Storage, got %A{other}"

                let standing =
                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    Obstacles = Set.add storageTile layer.Obstacles
                                })
                            |> withTargets [ "sto-1", storageTile, Structure BuiltKind.Storage ]
                    }

                let built = decide standing Map.empty Set.empty None

                Expect.equal
                    built.Memo.HaulerQuota
                    planned.Memo.HaulerQuota
                    "a Storage hires nothing: the quota reads source containers alone"

                Expect.equal
                    (sitesOfKind Road built.Intents)
                    (sitesOfKind Road planned.Intents)
                    "the trunks keep their endpoints — the spawn stays the hub"
            }
        ]

/// Synthetic footing fixture: the source stands three tiles north of the
/// spawn, so its trunk leaves by (24,23) and the source container is
/// planned there, while the Storage takes the ordering's first pick at
/// (24,24). The tile the container's Link footing wants, (23,23), is one
/// of the cluster's own same-colour tiles — the collision the reservation
/// exists to settle (ADR 0022).
/// A room with the whole Keep standing and a Post container at each of
/// the two sources — what the rampart rule covers (ADR 0034). The spawn
/// stands at (25,25) from `openRoom`, the tower and the Storage beside it,
/// and each source's container on the Seat between the source and the
/// spawn, which is working ground and covered all the same.
let keepRoom =
    openRoom 6
    |> withTargets
        [
            "tower-1", { X = 24; Y = 24 }, Structure BuiltKind.Tower
            "sto-1", { X = 26; Y = 26 }, Structure BuiltKind.Storage
            "src-a", { X = 20; Y = 25 }, Source
            "src-b", { X = 30; Y = 25 }, Source
            "con-a", { X = 21; Y = 25 }, Structure BuiltKind.Container
            "con-b", { X = 29; Y = 25 }, Structure BuiltKind.Container
        ]

/// The tiles that room's rule covers, in the plan's own (x, y) order: the
/// Keep — spawn, tower, Storage — and the two Post containers.
let keepCover =
    [
        { X = 21; Y = 25 }
        { X = 24; Y = 24 }
        { X = 25; Y = 25 }
        { X = 26; Y = 26 }
        { X = 29; Y = 25 }
    ]

[<Tests>]
let rampartTests =
    testList
        "ramparts"
        [
            test "every standing Keep structure and Post container is covered, and nothing else" {
                // The rule, whole (ADR 0034): the spawn, the tower and the
                // Storage because they are what a raid is for, the Post
                // containers because a work-heavy body cannot flee its Post.
                // Not the extensions, not the room at large — the equality
                // is what says no rampart lands anywhere else.
                let { Intents = intents } = decide (atLevel 4 keepRoom) Map.empty Set.empty None

                Expect.equal
                    (sitesOfKind Rampart intents)
                    keepCover
                    "five tiles: the Keep and both Posts"
            }

            test "the working-ground exclusion does not reach a rampart" {
                // A Post container stands on a Seat, which the clustered
                // ordering never offers (ADR 0022). A rampart is no
                // footprint — walkable, blocking nothing, taking no tile from
                // the Post it covers — so it is placed there regardless,
                // while the cluster still keeps off (ADR 0034 revising 0022).
                let { Intents = intents } = decide (atLevel 4 keepRoom) Map.empty Set.empty None
                let seats = Set.ofList [ { X = 21; Y = 25 }; { X = 29; Y = 25 } ]

                Expect.isTrue
                    (seats
                     |> Set.forall (fun seat -> List.contains seat (sitesOfKind Rampart intents)))
                    "both Seats under a container are ramparted"

                Expect.isEmpty
                    (Set.intersect seats (clusterTiles intents))
                    "and no clustered structure follows the rampart onto working ground"
            }

            test "a Storage that is only a site is not covered yet" {
                // Standing is the built census: a site is not covered until
                // it is a structure, so the Storage's own tile waits.
                let pending =
                    { keepRoom with
                        TargetKinds = Map.add "sto-1" (Site BuiltKind.Storage) keepRoom.TargetKinds
                    }

                let { Intents = intents } = decide (atLevel 4 pending) Map.empty Set.empty None

                Expect.equal
                    (sitesOfKind Rampart intents)
                    (keepCover |> List.filter (fun tile -> tile <> { X = 26; Y = 26 }))
                    "the four standing things are covered; the site is not"
            }

            test "a tile already ramparted, or already owed a site, emits nothing" {
                // The covering census, both halves — the road gap's own
                // shape: a standing rampart is cover, and a pending one is a
                // tile that needs no second site.
                let standing =
                    keepRoom
                    |> withTargets [ "ram-1", { X = 25; Y = 25 }, Structure BuiltKind.Rampart ]

                let pending =
                    keepRoom |> withTargets [ "ram-1", { X = 24; Y = 24 }, Site BuiltKind.Rampart ]

                let { Intents = afterStanding } =
                    decide (atLevel 4 standing) Map.empty Set.empty None

                let { Intents = afterPending } = decide (atLevel 4 pending) Map.empty Set.empty None

                Expect.equal
                    (sitesOfKind Rampart afterStanding)
                    (keepCover |> List.filter (fun tile -> tile <> { X = 25; Y = 25 }))
                    "the spawn's own rampart stands: nothing is re-placed on it"

                Expect.equal
                    (sitesOfKind Rampart afterPending)
                    (keepCover |> List.filter (fun tile -> tile <> { X = 24; Y = 24 }))
                    "the tower's site is already owed: nothing is re-placed on it"
            }

            test "the set is the rule's: a three-source room ramparts three Posts" {
                let room =
                    keepRoom
                    |> withTargets
                        [
                            "src-c", { X = 25; Y = 20 }, Source
                            "con-c", { X = 25; Y = 21 }, Structure BuiltKind.Container
                        ]

                let colony =
                    { atLevel 4 room with
                        Sources = [ source "src-a"; source "src-b"; source "src-c" ]
                    }

                let { Intents = intents } = decide colony Map.empty Set.empty None

                Expect.equal
                    (sitesOfKind Rampart intents)
                    [
                        { X = 21; Y = 25 }
                        { X = 24; Y = 24 }
                        { X = 25; Y = 21 }
                        { X = 25; Y = 25 }
                        { X = 26; Y = 26 }
                        { X = 29; Y = 25 }
                    ]
                    "three Posts and the Keep, from the rule and not a count"
            }

            test "a container that is no Post is left bare" {
                // The rule covers the Keep and the Posts, and a container off
                // every Seat is neither: the upgrade buffer's own container
                // stands where its upgraders run, and they can flee.
                let room =
                    keepRoom
                    |> withTargets [ "con-far", { X = 25; Y = 29 }, Structure BuiltKind.Container ]

                let { Intents = intents } = decide (atLevel 4 room) Map.empty Set.empty None

                Expect.equal
                    (sitesOfKind Rampart intents)
                    keepCover
                    "a container adjacent to no source gets no cover"
            }

            test "the cover waits for RCL2 and then never grows" {
                // The rule is placed the tick the thing it covers stands and
                // needs no level of its own past the one the engine allows a
                // rampart at — none at RCL1, 2,500 from RCL2 up. Below it
                // every site would be refused, every tick (ADR 0034).
                let at level =
                    let { Intents = intents } =
                        decide (atLevel level keepRoom) Map.empty Set.empty None

                    sitesOfKind Rampart intents

                Expect.isEmpty (at 1) "at RCL1 the engine allows no rampart, so none is planned"
                Expect.equal (at 2) keepCover "at RCL2 the whole cover is planned at once"
                Expect.equal (at 8) keepCover "and RCL8 adds nothing to it"
            }
        ]

let footingRoom = openRoom 6 |> withTargets [ "src-a", { X = 25; Y = 22 }, Source ]

/// The two tiles `footingRoom` holds as Link footings: one beside the
/// planned source container at (24,23), one beside the Storage at (24,24).
/// Two, not four — the count is one per planned source container plus the
/// controller container and the Storage, and this room projects no
/// controller position.
let footingTiles = Set.ofList [ { X = 23; Y = 23 }; { X = 24; Y = 25 } ]

/// The room with a target standing on a tile and blocking it, the way the
/// projection carries a built Storage or link: a target and an obstacle.
let withStanding id pos kind room =
    withTargets [ id, pos, kind ] room
    |> withHome (fun layer ->
        { layer with
            Obstacles = Set.add pos layer.Obstacles
        })

/// A footing fixture whose trunks run through the clustered ring: the
/// source stands against the room's east edge and the controller against
/// its west edge, so the source→controller trunk crosses the whole
/// cluster and the tiles a footing pushes the cluster onto are the ones
/// the trunk would otherwise want (ADR 0022, ADR 0027).
let crossedRoom =
    openRoom 6
    |> withTargets [ "src-a", { X = 30; Y = 26 }, Source ]
    |> withStanding "ctrl-1" { X = 19; Y = 25 } Controller

/// The colony one tick on: every site the Layout just asked for now
/// standing in the projection as a construction site, the obstacle kinds
/// blocking their tile exactly as the engine's own sites do — the state
/// the next tick's plan is computed against. Both halves go through the
/// Core's own tables (#75): a .NET-side projection builder that restated
/// them would drift from the one `buildSpatial` really builds, and the
/// tests would stay green describing a room the bot never sees.
let withPlanPending colony =
    let { Intents = intents } = decide colony Map.empty Set.empty None

    let sites =
        placementIntents intents
        |> List.mapi (fun i (_, pos, kind) -> $"site-{i}", pos, builtKindOfPlaceable kind)

    { colony with
        Spatial =
            withTargets [ for id, pos, kind in sites -> id, pos, Site kind ] colony.Spatial
            |> withHome (fun layer ->
                { layer with
                    Obstacles =
                        (layer.Obstacles, sites)
                        ||> List.fold (fun acc (_, pos, kind) ->
                            if isWalkable kind then acc else Set.add pos acc)
                })
    }

/// W12S28's `10,43` shape, synthesised (#77): the pocket source's only
/// Seat is its container's pick, and every one of the eight tiles beside
/// that pick is spoken for — four wall, the source itself, the one trunk
/// road out, and two standing extensions, which is the live room's own
/// tile table in proportion. The extensions are the live loss: `11,43`
/// took one in the RCL4 burst, planned by a bundle that did not yet hold
/// footings back. Nothing is left for the fold to reserve, so this room's
/// guarantee is short by one — and `pocketColony`, the same room without
/// the seal, is the control that serves all four.
let sealedPocketColony level =
    let colony = pocketColony level
    let sealedTiles = [ { X = 21; Y = 29 }; { X = 21; Y = 31 } ]

    let standingExtensions =
        [ "ext-3", { X = 22; Y = 29 }; "ext-4", { X = 22; Y = 31 } ]

    let walled =
        colony.Spatial
        |> withHome (fun layer ->
            { layer with
                Terrain =
                    (layer.Terrain, sealedTiles)
                    ||> List.fold (fun acc tile -> Map.add tile Wall acc)
            })

    { colony with
        Spatial =
            (walled, standingExtensions)
            ||> List.fold (fun room (id, tile) ->
                withStanding id tile (Structure BuiltKind.Extension) room)
    }

[<Tests>]
let linkFootingTests =
    testList
        "link footing"
        [
            test "every target served records nothing: the empty list is the guarantee holding" {
                // Both of `footingRoom`'s targets get their tile, so the
                // record is empty — and empty is an answer rather than an
                // absence: it is what the Layout channel says while ADR
                // 0022 and ADR 0027's one-footing-per-target still holds
                // (#77, ADR 0035).
                let { Memo = memo } = decide (atLevel 4 footingRoom) Map.empty Set.empty None

                Expect.isEmpty memo.UnservedFootings "both footings stand; nothing is lost"
            }

            test "a served target names the tile the fold reserved for it" {
                // The other half of the record (#106): the fold holds the
                // target, its kind and the tile in scope at the instant it
                // reserves one, and hands all three back. A bare set of
                // tiles would leave the target-to-tile pairing to be
                // rederived by hand — the second derivation the record
                // exists to remove (ADR 0035).
                let { Memo = memo } = decide (atLevel 4 footingRoom) Map.empty Set.empty None

                Expect.equal
                    memo.ServedFootings
                    [
                        {
                            Target = { X = 24; Y = 23 }
                            Kind = FootingKind.SourceContainer
                            Tile = { X = 23; Y = 23 }
                        }
                        {
                            Target = { X = 24; Y = 24 }
                            Kind = FootingKind.Storage
                            Tile = { X = 24; Y = 25 }
                        }
                    ]
                    "both targets, each beside the tile held for its link"

                Expect.equal
                    (memo.ServedFootings |> List.map (fun footing -> footing.Tile) |> Set.ofList)
                    footingTiles
                    "and the tiles are the room's own footings, which no site may take"
            }

            test "the sealed room's four targets split three served to one unserved" {
                // `sealedPocketColony`'s four targets split three to one:
                // the sealed source container is the loss (#77) and the
                // other three stand. Neither list is the whole story alone
                // — the shortfall says which guarantee went and the served
                // record says which tiles the rest hold — and no target is
                // in both, because the fold visits each exactly once.
                let { Memo = memo } = decide (sealedPocketColony 4) Map.empty Set.empty None

                let served = memo.ServedFootings |> List.map (fun footing -> footing.Target)
                let unserved = memo.UnservedFootings |> List.map (fun footing -> footing.Target)

                Expect.hasLength served 3 "the three targets the sealed room can still serve"
                Expect.hasLength unserved 1 "and the one it cannot"

                Expect.isEmpty
                    (served |> List.filter (fun target -> List.contains target unserved))
                    "no target is both served and unserved"
            }

            test "a target with no candidate is recorded by tile and kind, never dropped" {
                // W12S28's `10,43`, synthesised: the pocket source's
                // container pick has wall on five sides, its own source on
                // the sixth, the trunk road out on the seventh and a
                // standing extension on the last, so the fold has nothing
                // to reserve for it. That used to fall through to `taken`
                // and leave the room three footings where the ADRs promise
                // four, with no signal anywhere (#77). The room's other
                // three targets are absent from the list, which is the
                // other half of the claim: the fold still reserves
                // everything it can, and only what it cannot is recorded.
                let { Memo = memo } = decide (sealedPocketColony 4) Map.empty Set.empty None

                Expect.equal
                    memo.UnservedFootings
                    [
                        {
                            Target = { X = 21; Y = 30 }
                            Kind = FootingKind.SourceContainer
                        }
                    ]
                    "one entry: the sealed source container's pick, and nothing else"

                // And the seal is the whole cause. The same room with its
                // pocket open serves all four targets and records nothing,
                // so sealing one pick costs exactly that one footing: the
                // fold reserves everything it still can, which is the other
                // half of the claim above and cannot be read off a list
                // that only ever names losses.
                let { Memo = control } = decide (pocketColony 4) Map.empty Set.empty None

                Expect.isEmpty
                    control.UnservedFootings
                    "unsealed, the same four targets all get their tile"
            }

            test "no site lands on a Link footing, at any level" {
                // The footings are held from level 0, levels before the
                // engine unlocks links, because the tile never comes back
                // once an extension takes it — and past RCL4, where links
                // would be allowed, nothing is placed on them either: Link
                // is a built kind with no placeable counterpart, so the
                // Layout emits no site for one at any level (ADR 0022).
                for level in 1..8 do
                    let { Intents = intents } =
                        decide (atLevel level footingRoom) Map.empty Set.empty None

                    Expect.isEmpty
                        (placedTiles intents
                         |> List.filter (fun tile -> Set.contains tile footingTiles))
                        $"RCL{level}: no extension, tower, road or container site sits on a footing"
            }

            test "a footing outranks the extensions: the cluster fills one tile further out" {
                // (23,23) is the ordering's third free pick and the footing
                // beside the source container. The footing wins it, so the
                // cluster takes the next tile instead — the allowance still
                // fills, it just reaches one ring wider.
                let { Intents = intents } = decide (atLevel 3 footingRoom) Map.empty Set.empty None

                Expect.hasLength
                    (sitesOfKind Extension intents)
                    10
                    "the RCL3 allowance still fills whole"

                Expect.isFalse
                    (Set.contains { X = 23; Y = 23 } (clusterTiles intents))
                    "the footing's tile is out of the clustered picks"

                Expect.contains
                    (clusterTiles intents)
                    { X = 22; Y = 24 }
                    "the pick behind the footing is drawn in: nothing is lost, the cluster moves out"
            }

            test
                "a footing may sit on a Seat: the working ground is off-limits to the cluster alone" {
                // This source's trunk leaves by the Seat at (24,22), so that
                // Seat is the container pick and the footing beside it wants
                // (24,23) — another Seat. A footing is the one structure
                // footing allowed on working ground (ADR 0022); were it to
                // dodge Seats the way the ordering does, it would fall
                // through to (25,23) and cost the cluster that tile.
                let colony =
                    atLevel
                        4
                        (openRoom 6
                         |> withTargets
                             [
                                 "src-a", { X = 23; Y = 23 }, Source
                                 "ctrl-1", { X = 30; Y = 25 }, Controller
                             ])

                let { Intents = intents } = decide colony Map.empty Set.empty None

                Expect.isFalse
                    (List.contains { X = 24; Y = 23 } (placedTiles intents))
                    "the Seat beside the container is held, not built on"

                Expect.contains
                    (clusterTiles intents)
                    { X = 25; Y = 23 }
                    "the cluster keeps the tile a working-ground dodge would have cost it"
            }

            test "the footings hold their tiles as the container and the Storage are built" {
                // The reservation becomes a structure: the source container
                // on (24,23), the Storage on (24,24). Both picks are judged
                // from geometry that does not move when they are built, so
                // the footings beside them do not move either — (23,23) is
                // buildable again on this tick and still nothing takes it.
                let built =
                    footingRoom
                    |> withTargets [ "can-a", { X = 24; Y = 23 }, Structure BuiltKind.Container ]
                    |> withStanding "sto-1" { X = 24; Y = 24 } (Structure BuiltKind.Storage)

                let { Intents = intents } = decide (atLevel 4 built) Map.empty Set.empty None

                Expect.isNonEmpty (sitesOfKind Extension intents) "the cluster still fills"

                Expect.isEmpty
                    (placedTiles intents |> List.filter (fun tile -> Set.contains tile footingTiles))
                    "both footings are still held, target built or reserved"
            }

            test "a standing Storage keeps the footing beside it, and a pending one too" {
                // (23,23) is the footing beside the Storage's pick at
                // (24,24). The tick the Storage is placed its reservation
                // leaves the clustered ordering, so the footing reads the
                // site's — and then the structure's — own tile instead, or
                // the tile it was holding falls to the next extension.
                let room =
                    openRoom 6
                    |> withTargets
                        [
                            "src-a", { X = 22; Y = 26 }, Source
                            "ctrl-1", { X = 30; Y = 25 }, Controller
                        ]

                let planOf colony =
                    let { Intents = intents } = decide (atLevel 4 colony) Map.empty Set.empty None
                    intents

                let planned = planOf room

                let pending =
                    planOf (
                        room |> withTargets [ "sto-1", { X = 24; Y = 24 }, Site BuiltKind.Storage ]
                    )

                let built =
                    planOf (
                        room
                        |> withStanding "sto-1" { X = 24; Y = 24 } (Structure BuiltKind.Storage)
                    )

                Expect.equal
                    (sitesOfKind Storage planned)
                    [ { X = 24; Y = 24 } ]
                    "the Storage is planned on the ordering's first pick"

                for (label, intents) in
                    [ "reserved", planned; "pending", pending; "standing", built ] do
                    Expect.isFalse
                        (List.contains { X = 23; Y = 23 } (placedTiles intents))
                        $"{label}: the footing beside the Storage keeps its tile"
            }

            test "a standing link keeps its own footing: the plan is the tick-before plan" {
                // A link is a target, so the tick it goes up its tile stops
                // being buildable — the footing reads standing links back
                // into its candidates rather than jumping to the next tile
                // and costing the cluster a second one. This room's Storage
                // already stands off in the corner, so the source
                // container's footing is the only one bidding near the
                // cluster and the jump would be visible.
                let room =
                    openRoom 6
                    |> withTargets
                        [
                            "src-a", { X = 25; Y = 21 }, Source
                            "ctrl-1", { X = 26; Y = 28 }, Controller
                        ]
                    |> withStanding "sto-1" { X = 29; Y = 29 } (Structure BuiltKind.Storage)

                let linked =
                    room |> withStanding "link-1" { X = 23; Y = 23 } (Structure BuiltKind.Link)

                let before = decide (atLevel 4 room) Map.empty Set.empty None
                let after = decide (atLevel 4 linked) Map.empty Set.empty None

                Expect.isFalse
                    (List.contains { X = 23; Y = 23 } (placedTiles after.Intents))
                    "the link's tile leaves the ordering: nothing is planned onto it"

                Expect.equal
                    (placementIntents after.Intents)
                    (placementIntents before.Intents)
                    "the link standing on its footing moves nothing else in the plan"
            }

            test "no clustered structure and no trunk want the same tile" {
                // A footing takes one of the cluster's own picks, so the
                // cluster draws one more tile in behind it — and the
                // reservation the trunk flood was routed around is widened
                // by the footing count for exactly that (ADR 0027), so the
                // drawn-in tile is still ground no trunk was allowed to
                // cross. ADR 0011's precedence survives the push: a road
                // never sits where a structure will.
                let { Intents = intents } = decide (atLevel 4 crossedRoom) Map.empty Set.empty None

                Expect.isNonEmpty (sitesOfKind Road intents) "the trunks are paved"

                Expect.isEmpty
                    (Set.intersect (clusterTiles intents) (sitesOfKind Road intents |> Set.ofList))
                    "the cluster the footings pushed out is still inside the reservation"
            }

            test "the footings survive the placement burst: the next tick asks for nothing" {
                // RCL4 places everything its own level unlocks in one
                // burst, so the tick after it every gap at that level is
                // zero and the tiles it took are carried by the sites
                // themselves — except the footings, which no site ever
                // stands on. The widened window is what still holds
                // them (ADR 0027); without it the trunk flood is free to
                // take a footing the moment the cluster is placed, and the
                // Layout emits a road on the tile it had been reserving
                // since level 0, orphaning the roads it just moved off.
                let { Intents = intents } =
                    decide (withPlanPending (atLevel 4 crossedRoom)) Map.empty Set.empty None

                Expect.isEmpty
                    (placementIntents intents)
                    "the whole plan stands where it was asked for: nothing moved, nothing is re-sited"
            }
        ]

/// The trunk fixture cut in two: a wall ridge down x=30 severs the
/// controller and its whole Upgrade Work Area from the rest of the room,
/// leaving the source and the spawn together on the west side. `src-a`
/// still routes its trunk to the spawn and can route none to the Work
/// Area — the per-goal shape #107 records, and the one a record keyed on
/// the source alone would get wrong.
let severedControllerColony level =
    let colony = trunkColony level

    let ridge = [ for y in 15..35 -> { X = 30; Y = y } ]

    { colony with
        Spatial =
            colony.Spatial
            |> withHome (fun layer ->
                { layer with
                    Terrain =
                        (layer.Terrain, ridge)
                        ||> List.fold (fun acc tile -> Map.add tile Wall acc)
                })
    }

/// The pocket fixture with its one Seat walled shut: every one of `src-b`'s
/// eight neighbours is wall, so no goal is reachable from it at all and
/// both its trunks are dropped. `src-a`, in the open, keeps both — the
/// record names the source as well as the goal.
let enclosedSourceColony level =
    let colony = pocketColony level

    { colony with
        Spatial =
            colony.Spatial
            |> withHome (fun layer ->
                { layer with
                    Terrain = Map.add { X = 21; Y = 30 } Wall layer.Terrain
                })
    }

[<Tests>]
let unroutedTrunkTests =
    testList
        "unrouted trunk"
        [
            test "every trunk routed records nothing: the empty list is the guarantee holding" {
                // The trunk fixture's one source reaches both goals, so the
                // record is empty — and empty is an answer rather than an
                // absence, exactly as it is for the footing shortfall it
                // rides beside (#107, ADR 0035).
                let { Memo = memo } = decide (trunkColony 4) Map.empty Set.empty None

                Expect.isEmpty memo.UnroutedTrunks "both trunks route; nothing is lost"
            }

            test "a source that loses one goal and keeps the other records exactly one entry" {
                // The detail a careless record gets wrong. The goals are
                // collected per source, so the loss is per (source, goal):
                // with the controller walled off, `src-a` loses its line to
                // the Upgrade Work Area and keeps the one to the spawn.
                // W12S27 from 6,18 is the live counterexample the other way
                // round (#105), and a record keyed on the source alone
                // would be false in both.
                let colony = severedControllerColony 4
                let { Memo = memo; Intents = intents } = decide colony Map.empty Set.empty None

                Expect.equal
                    memo.UnroutedTrunks
                    [
                        {
                            Source = "src-a"
                            Goal = TrunkGoal.UpgradeArea
                        }
                    ]
                    "one entry: the goal that was lost, named beside the source that lost it"

                // And the trunk it kept is paved, which is what makes the
                // entry a loss of one line rather than of the source: a
                // room that paved nothing would be a different claim.
                Expect.isNonEmpty
                    (sitesOfKind Road intents)
                    "the line to the spawn is still routed and still paved"
            }

            test "a source no goal is reachable from records both its goals" {
                // `src-b` walled in on all eight sides: the router hands
                // back the empty path for each goal in turn, and each is an
                // entry of its own. The spawn carries its id because the
                // spawn list is a list (RCL7 adds a second one), where the
                // Upgrade Work Area is the controller's alone.
                let { Memo = memo } = decide (enclosedSourceColony 4) Map.empty Set.empty None

                Expect.equal
                    memo.UnroutedTrunks
                    [
                        {
                            Source = "src-b"
                            Goal = TrunkGoal.UpgradeArea
                        }
                        {
                            Source = "src-b"
                            Goal = TrunkGoal.Spawn "spawn-1"
                        }
                    ]
                    "both goals, and only the enclosed source's"

                // The open source is the control: sealing one source costs
                // exactly that source's trunks, and the same room with the
                // pocket's Seat open loses nothing at all.
                let { Memo = control } = decide (pocketColony 4) Map.empty Set.empty None

                Expect.isEmpty control.UnroutedTrunks "unsealed, every source reaches every goal"
            }
        ]

/// The trunk colony with one extra target standing (or pending) anywhere.
let withTarget id pos kind colony =
    { colony with
        Spatial = colony.Spatial |> withTargets [ id, pos, kind ]
    }

/// The step-weight grid ADR 0032's guard compares, for the room the caller
/// names. The room is the caller's rather than the fixture's home since
/// #169: the walk table now holds an entry per *goal* room and the far
/// leg's entry is a pure function of that room's grid, so a guard that
/// could only ask about home would pin the pairing in one room while the
/// memo reads every projected one — which is the asymmetry
/// `Atlas.stepWeights` was already given a room parameter for. Read off
/// the projection rather than retyped as a literal: `stepWeights` answers
/// every tile impassable for a room the projection does not carry (ADR
/// 0004, ADR 0041), so a literal that drifted from its fixture would leave
/// the one `sequenceEqual` in the group comparing two empty grids and
/// passing whatever the census did.
let private stepGridOf (room: string) (snapshot: Snapshot) =
    Atlas.stepWeights (Atlas.ofSnapshot snapshot) room

/// The same grid for the colony's own room — the reading every home-room
/// perturbation in the guard group is compared through.
let private homeGridOf (snapshot: Snapshot) =
    stepGridOf (SpatialInfo.homeName snapshot.Spatial) snapshot

/// The same colony with a second room's geometry beside its own: one more
/// entry in `Rooms`, under that room's name (ADR 0041). The kind census
/// stays unlayered and world-unique, exactly as the projection keeps it —
/// which is what makes these fixtures able to ask whether a reader joins a
/// kind to the right room's tile. It adds the outpost's entry and never
/// replaces the map, so the colony's own layer survives it and the helper
/// is order-blind: a `withTarget` composed either side of it is still
/// read.
let withOutpost room targets tiles (colony: Snapshot) =
    { colony with
        Spatial =
            { colony.Spatial with
                Rooms =
                    Map.add
                        room
                        { RoomLayer.empty with
                            Terrain = Map.ofList tiles
                            TargetPositions =
                                targets |> List.map (fun (id, pos, _) -> id, pos) |> Map.ofList
                        }
                        colony.Spatial.Rooms
                TargetKinds =
                    (colony.Spatial.TargetKinds, targets)
                    ||> List.fold (fun acc (id, _, kind) -> Map.add id kind acc)
            }
    }

[<Tests>]
let censusSignatureTests =
    testList
        "census signature"
        [
            // Every census input, perturbed alone, moves the signature —
            // the test surface ADR 0017 demands: a missed input would stall
            // the Layout until a reset instead of failing here.
            test "a structure appearing moves the signature" {
                let perturbed =
                    trunkColony 2
                    |> withTarget "ext-3" { X = 26; Y = 26 } (Structure BuiltKind.Extension)

                Expect.notEqual
                    (censusSignature perturbed)
                    (censusSignature (trunkColony 2))
                    "the standing census is a signature input"
            }

            test "a standing Storage is its own kind in the signature" {
                let standing kind =
                    trunkColony 2 |> withTarget "sto-1" { X = 24; Y = 24 } (Structure kind)

                Expect.notEqual
                    (censusSignature (standing BuiltKind.Storage))
                    (censusSignature (trunkColony 2))
                    "a Storage is a Structure: the standing census carries it (ADR 0022)"

                Expect.notEqual
                    (censusSignature (standing BuiltKind.Storage))
                    (censusSignature (standing BuiltKind.Other))
                    "and it is a kind of its own, not the unmodelled kind it used to project as"
            }

            test "a structure moving moves the signature" {
                let colony = trunkColony 2

                let moved =
                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    TargetPositions =
                                        Map.add "ext-1" { X = 24; Y = 27 } layer.TargetPositions
                                })
                    }

                Expect.notEqual
                    (censusSignature moved)
                    (censusSignature colony)
                    "the census is (kind, position), not a count"
            }

            test "a pending site appearing moves the signature" {
                let perturbed =
                    trunkColony 2 |> withTarget "site-1" { X = 24; Y = 24 } (Site BuiltKind.Road)

                Expect.notEqual
                    (censusSignature perturbed)
                    (censusSignature (trunkColony 2))
                    "the pending census is a signature input"
            }

            test "a structure and a site of the same kind on the same tile differ" {
                let standing =
                    trunkColony 2
                    |> withTarget "can-1" { X = 16; Y = 25 } (Structure BuiltKind.Container)

                let pending =
                    trunkColony 2
                    |> withTarget "can-1" { X = 16; Y = 25 } (Site BuiltKind.Container)

                Expect.notEqual
                    (censusSignature standing)
                    (censusSignature pending)
                    "a site becoming a structure is a census change"
            }

            test "the controller level moves the signature" {
                Expect.notEqual
                    (censusSignature (trunkColony 3))
                    (censusSignature (trunkColony 2))
                    "the level gates allowances, so it is a signature input"
            }

            test "a second room's standing container joins the signature under its own name" {
                // The widening #116's forward note booked and #149 spent
                // (ADR 0042): the hauler quota folds the containers of
                // every projected room and prices each at the rate that
                // room is held at, so both of those are memo inputs now
                // and the signature that gates the memo has to carry them.
                // #121's narrowing to the home layer was right while the
                // memo held nothing but home's; this is the tick it stops
                // being.
                let colony = trunkColony 2

                let ground =
                    [
                        for x in 23..26 do
                            for y in 23..26 -> { X = x; Y = y }, Plain
                    ]

                let joined =
                    colony
                    |> withOutpost
                        "W1N2"
                        [
                            "src-out", { X = 24; Y = 24 }, Source
                            "can-out", { X = 24; Y = 25 }, Structure BuiltKind.Container
                        ]
                        ground

                Expect.notEqual
                    (censusSignature joined)
                    (censusSignature colony)
                    "an outpost's standing container is a census entry: it hires haulers"

                // The room is *in* the entry and not merely implied by the
                // room list, because two rooms hold the same coordinates.
                // Pairwise, one rival at a time: both sides below project
                // W1N2 with the same ground, the same source and the same
                // control, and carry the same container id at the same
                // (24,25) — the only thing that moves is which room's
                // layer places it.
                let bare =
                    colony |> withOutpost "W1N2" [ "src-out", { X = 24; Y = 24 }, Source ] ground

                // The widening itself, with nothing else moving: the same
                // room list, the same held rates, the same home layer —
                // the container standing in W1N2 is the only difference.
                // Joined against the home layer alone (#121's rule) these
                // two sign the same string, and the memo hands back the
                // quota from before the container stood.
                Expect.notEqual
                    (censusSignature joined)
                    (censusSignature bare)
                    "a standing structure in a second room is a census entry of its own"

                Expect.notEqual
                    (censusSignature joined)
                    (censusSignature (
                        bare
                        |> withTarget "can-out" { X = 24; Y = 25 } (Structure BuiltKind.Container)
                    ))
                    "the same container at the same coordinates in the other room is another census"

                // And not the container kind alone. The quota prices that
                // container by a round trip flooded over the outpost's
                // step-weight grid, and `Snapshot.projectVisible` lays a
                // room's `Roads` and `Obstacles` out of the same
                // every-owner structure array the kind census comes from —
                // so a road paved along the haul lane, or a hostile core
                // standing on it, moves a number the memo holds. A
                // standing census filtered down to `Container` outside
                // home would be ADR 0017's signature gap.
                let paved =
                    colony
                    |> withOutpost
                        "W1N2"
                        [
                            "src-out", { X = 24; Y = 24 }, Source
                            "road-out", { X = 24; Y = 26 }, Structure BuiltKind.Road
                        ]
                        ground

                Expect.notEqual
                    (censusSignature paved)
                    (censusSignature bare)
                    "a second room's road prices its haul, so it is a signature input too"

                // And the room list itself, because the rate is signed per
                // projected room: a room that joins carrying nothing is a
                // room the quota can fold a container out of the tick one
                // stands there, and its held rate is what that container
                // would be priced at.
                Expect.notEqual
                    (censusSignature (colony |> withOutpost "W1N2" [] ground))
                    (censusSignature colony)
                    "a room joining the projection brings its own held rate into the signature"

                Expect.notEqual
                    (censusSignature (
                        colony
                        |> withTarget "can-out" { X = 24; Y = 25 } (Structure BuiltKind.Container)
                    ))
                    (censusSignature colony)
                    "the home room's own container still moves it"
            }

            test "the room name moves the signature" {
                let colony = trunkColony 2

                // The same geometry, carried under the new name: the layer
                // is keyed by room (ADR 0041), so a rename that left the
                // tiles filed under the old key would move the signature by
                // emptying the room rather than by naming it — and the
                // input under test would go unmeasured.
                let renamed =
                    { colony with
                        Spatial =
                            { colony.Spatial with
                                RoomName = Some "W2N2"
                                Rooms = Map.ofList [ "W2N2", homeLayer colony.Spatial ]
                            }
                    }

                Expect.notEqual
                    (censusSignature renamed)
                    (censusSignature colony)
                    "terrain is keyed by the room, so the name is a signature input"
            }

            test "who holds the home room moves the signature" {
                // The hauler quota's second load-bearing input since ADR
                // 0042: it prices each container at its source's own
                // output, and that output is read off `RoomControl`. A
                // vision fact riding a census memo has to be signed, or
                // the memo hands back a quota sized for the held rate on
                // the tick the room stops being held — the signature gap
                // ADR 0017 names as its failure mode.
                let colony = trunkColony 2

                let holding control =
                    { colony with
                        RoomControl = homeControl |> Map.map (fun _ _ -> control)
                    }

                Expect.notEqual
                    (censusSignature (holding neutralRoom))
                    (censusSignature (holding ownedRoom))
                    "a room that stopped being held prices its sources at half: a memo input"

                Expect.equal
                    (censusSignature (holding (reservedRoom true 4000)))
                    (censusSignature (holding ownedRoom))
                    "owned or reserved by us is one rate, so the two sign the same"

                Expect.notEqual
                    (censusSignature { colony with RoomControl = Map.empty })
                    (censusSignature (holding neutralRoom))
                    "and no vision at all is a third answer, not the neutral one (ADR 0004)"
            }

            test "the ticks left on a reservation leave the signature alone" {
                // The half of the reservation the quota does *not* read.
                // `TicksToEnd` decays by one every tick, so signing it
                // would throw the Layout and the walk table away on every
                // tick the colony holds an outpost — the memo would never
                // survive its own input.
                let holding control =
                    { trunkColony 2 with
                        RoomControl = homeControl |> Map.map (fun _ _ -> control)
                    }

                Expect.equal
                    (censusSignature (holding (reservedRoom true 4000)))
                    (censusSignature (holding (reservedRoom true 3999)))
                    "the rate is the input, and the countdown under it is not"
            }

            test "everything outside the census leaves the signature alone" {
                let colony = trunkColony 2

                // The bank is perturbed in its Available alone: the
                // Capacity beside it is a function of the standing
                // spawn/extension census and the controller level, so it is
                // covered rather than absent (ADR 0017) — which is what
                // lets the successor body a lead is priced for ride this
                // signature too (ADR 0032).
                let perturbed =
                    { colony with
                        Time = colony.Time + 100
                        RoomEnergy = bank 0 300
                        Sources = [ drained "src-a" 120 ]
                        Creeps = [ worker "w1" 25 25 ]
                        Hostiles =
                            [
                                {
                                    Id = "h1"
                                    Owner = "raider"
                                    RoomName = "W1N1"
                                    Pos = { X = 30; Y = 25 }
                                    Body = [ Attack; Move ]
                                }
                            ]
                        ConstructionSites = [ { Id = "site-9" } ]
                        Spatial =
                            { colony.Spatial with
                                Hits = Map.ofList [ "ext-1", { Hits = 1; HitsMax = 3000 } ]
                                Stores = Map.ofList [ "ext-1", 50 ]
                            }
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 20; Y = 25 } ]
                                })
                            |> withTargets [ "pile-1", { X = 22; Y = 25 }, Dropped ]
                    }

                Expect.equal
                    (censusSignature perturbed)
                    (censusSignature colony)
                    "creeps, stores, hits, drops, hostiles, bank and tick are not census"

                // ADR 0032's guard, the inverse of every test above: the
                // spawn walks behind the leads are recalled on this
                // signature alone, so two Snapshots it calls equal have to
                // lay the same weight grid. A weights input the signature
                // missed would price leads off a stale grid until a global
                // reset, and would fail here rather than in the colony.
                Expect.sequenceEqual
                    (homeGridOf perturbed)
                    (homeGridOf colony)
                    "and the grid the walks flood over is bitwise the same"

                // The same pairing in the room the walk table only started
                // reading with #169: the far leg's entry is a pure function
                // of the *outpost's* grid, so a Snapshot the signature calls
                // equal has to lay that grid bitwise too. Perturbed out
                // there and not at home, or the assertion would be about the
                // home layer twice over: a creep standing in the outpost and
                // a raider beside it are vision facts, and a grid is
                // terrain, roads and obstacles alone.
                let ground =
                    [
                        for x in 23..26 do
                            for y in 23..26 -> { X = x; Y = y }, Plain
                    ]

                let held =
                    colony |> withOutpost "W1N2" [ "src-out", { X = 24; Y = 24 }, Source ] ground

                let seen =
                    { held with
                        Hostiles =
                            [
                                {
                                    Id = "h2"
                                    Owner = "raider"
                                    RoomName = "W1N2"
                                    Pos = { X = 26; Y = 26 }
                                    Body = [ Attack; Move ]
                                }
                            ]
                        Spatial =
                            { held.Spatial with
                                Rooms =
                                    Map.add
                                        "W1N2"
                                        { SpatialInfo.layerOf held.Spatial "W1N2" with
                                            CreepPositions = Map.ofList [ "w1", { X = 25; Y = 25 } ]
                                        }
                                        held.Spatial.Rooms
                            }
                    }

                Expect.equal
                    (censusSignature seen)
                    (censusSignature held)
                    "a creep and a raider in the outpost are no census of that room either"

                Expect.sequenceEqual
                    (stepGridOf "W1N2" seen)
                    (stepGridOf "W1N2" held)
                    "and the grid the far leg floods over is bitwise the same"
            }

            // The three weights inputs beside the terrain, each perturbed
            // alone (ADR 0032). Each test asserts the pairing rather than
            // the signature alone: the perturbation moves the grid the
            // recalled walks flood over, and it moves the signature they
            // are recalled on. A census that held still through one of them
            // would price leads off a grid the room has left.
            test "a built road moves the signature" {
                let colony = trunkColony 2
                let tile = { X = 22; Y = 25 }

                let paved =
                    let placed = colony |> withTarget "road-1" tile (Structure BuiltKind.Road)

                    { placed with
                        Spatial =
                            placed.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    Roads = Set.add tile layer.Roads
                                })
                    }

                Expect.notEqual
                    (homeGridOf paved)
                    (homeGridOf colony)
                    "a road discounts the ground under it"

                Expect.notEqual
                    (censusSignature paved)
                    (censusSignature colony)
                    "and the standing census carries it, so the signature moves with it (ADR 0010)"
            }

            test "an obstacle structure moves the signature" {
                let colony = trunkColony 2
                let tile = { X = 22; Y = 25 }

                let blocked =
                    let placed = colony |> withTarget "twr-1" tile (Structure BuiltKind.Tower)

                    { placed with
                        Spatial =
                            placed.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    Obstacles = Set.add tile layer.Obstacles
                                })
                    }

                Expect.notEqual
                    (homeGridOf blocked)
                    (homeGridOf colony)
                    "an obstacle closes its tile to every flood"

                Expect.notEqual
                    (censusSignature blocked)
                    (censusSignature colony)
                    "and the standing census carries it, so the signature moves with it"
            }

            test "an obstacle site moves the signature" {
                let colony = trunkColony 2
                let tile = { X = 22; Y = 25 }

                let pending =
                    let placed = colony |> withTarget "twr-site" tile (Site BuiltKind.Tower)

                    { placed with
                        Spatial =
                            placed.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    Obstacles = Set.add tile layer.Obstacles
                                })
                    }

                Expect.notEqual
                    (homeGridOf pending)
                    (homeGridOf colony)
                    "the engine refuses a creep its own obstacle site, so it blocks like the structure"

                Expect.notEqual
                    (censusSignature pending)
                    (censusSignature colony)
                    "and the pending census carries it, so the signature moves with it"
            }

            test "an obstacle site in an outpost moves the signature" {
                // The fourth weights input, and the one #169 made
                // load-bearing: the walk table's far-leg entry is a pure
                // function of the *goal* room's grid, so every input of
                // that grid has to be in the signature exactly as the home
                // room's are (ADR 0032). `Snapshot.projectVisible` folds
                // every scanned room's obstacle-kind construction sites
                // into that room's `Obstacles` — the engine refuses a creep
                // its own site wherever it stands — so a pending census
                // read in the home layer alone would leave an outpost's
                // closed tile unsigned, and a lead priced through ground
                // the successor cannot cross would be recalled for the life
                // of the census: ADR 0017's signature gap, in the room the
                // memo has just started reading.
                let colony = trunkColony 2
                let tile = { X = 24; Y = 26 }

                let ground =
                    [
                        for x in 23..26 do
                            for y in 23..26 -> { X = x; Y = y }, Plain
                    ]

                let bare =
                    colony |> withOutpost "W1N2" [ "src-out", { X = 24; Y = 24 }, Source ] ground

                let sited =
                    let placed =
                        colony
                        |> withOutpost
                            "W1N2"
                            [
                                "src-out", { X = 24; Y = 24 }, Source
                                "twr-site-out", tile, Site BuiltKind.Tower
                            ]
                            ground

                    { placed with
                        Spatial =
                            { placed.Spatial with
                                Rooms =
                                    Map.add
                                        "W1N2"
                                        { SpatialInfo.layerOf placed.Spatial "W1N2" with
                                            Obstacles = Set.singleton tile
                                        }
                                        placed.Spatial.Rooms
                            }
                    }

                Expect.notEqual
                    (stepGridOf "W1N2" sited)
                    (stepGridOf "W1N2" bare)
                    "the site closes its tile in the outpost's grid, which the far leg floods"

                Expect.notEqual
                    (censusSignature sited)
                    (censusSignature bare)
                    "so the pending census reaches every projected room, not the home layer alone"

                Expect.notEqual
                    (stepGridOf "W1N2" bare)
                    (stepGridOf "W1N2" colony)
                    "the premise: a room the projection carries no layer for is all impassable (ADR 0004), so neither grid compared above is an empty one passing whatever the census did"
            }
        ]

/// The colony with the given creeps standing on the given tiles. A placed
/// creep beside a placed spawn is all it takes to price a lead, and pricing
/// one floods out of the spawner (ADR 0026).
let staffedColony creeps positions colony =
    { colony with
        Creeps = creeps
        Spatial =
            colony.Spatial
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = Map.ofList positions
                })
    }

/// A memo whose site Intents are a sentinel no computation would produce:
/// reuse is then observable verbatim at the decide seam.
let sentinelMemo snapshot =
    {
        Signature = censusSignature snapshot
        SiteIntents = [ PlaceConstructionSite("W1N1", { X = 1; Y = 1 }, Tower) ]
        UnservedFootings = []
        ServedFootings = []
        UnroutedTrunks = []
        DeferredContainers = []
        HaulerQuota = 0
        Walks = WalkTable()
    }

[<Tests>]
let planMemoTests =
    testList
        "plan memo"
        [
            test "a memo with the matching signature is reused verbatim" {
                let snapshot = trunkColony 2
                let memo = sentinelMemo snapshot
                let decision = decide snapshot Map.empty Set.empty (Some memo)

                Expect.equal
                    (placementIntents decision.Intents)
                    [ "W1N1", { X = 1; Y = 1 }, Tower ]
                    "the memo's site Intents pass through, nothing recomputes"

                Expect.equal decision.Memo memo "the memo rides out unchanged for next tick"
            }

            test "an added structure invalidates the memo" {
                let memo = sentinelMemo (trunkColony 2)

                let perturbed =
                    trunkColony 2
                    |> withTarget "ext-3" { X = 26; Y = 26 } (Structure BuiltKind.Extension)

                let decision = decide perturbed Map.empty Set.empty (Some memo)
                let fresh = decide perturbed Map.empty Set.empty None

                Expect.equal
                    (placementIntents decision.Intents)
                    (placementIntents fresh.Intents)
                    "a stale memo recomputes to exactly the fresh plan"
            }

            test "an added site invalidates the memo" {
                let memo = sentinelMemo (trunkColony 2)

                let perturbed =
                    trunkColony 2 |> withTarget "site-1" { X = 24; Y = 24 } (Site BuiltKind.Road)

                let decision = decide perturbed Map.empty Set.empty (Some memo)
                let fresh = decide perturbed Map.empty Set.empty None

                Expect.equal
                    (placementIntents decision.Intents)
                    (placementIntents fresh.Intents)
                    "a stale memo recomputes to exactly the fresh plan"
            }

            test
                "the Layout's records are census-derived: recalled with the plan, recomputed with it" {
                // #77's record, #106's and #107's join the site Intents and
                // the hauler quota under ADR 0017's standing invitation —
                // same census, same losses — so none is rederived per tick.
                // The sentinel says so in both directions: a memo whose
                // signature holds hands its own empty records back for a
                // room that has in fact lost a footing and reserved three,
                // and a stale one recomputes to what the room really has.
                let colony = sealedPocketColony 4

                let lost =
                    [
                        {
                            Target = { X = 21; Y = 30 }
                            Kind = FootingKind.SourceContainer
                        }
                    ]

                let matching = decide colony Map.empty Set.empty (Some(sentinelMemo colony))

                Expect.isEmpty
                    matching.Memo.UnservedFootings
                    "a matching signature reuses the memo's record; nothing recomputes"

                Expect.isEmpty
                    matching.Memo.ServedFootings
                    "and reuses the served record with it, for a room that reserved three"

                let stale = decide colony Map.empty Set.empty (Some(sentinelMemo (trunkColony 2)))
                let memoless = decide colony Map.empty Set.empty None

                Expect.equal
                    stale.Memo.UnservedFootings
                    lost
                    "a moved signature recomputes the record with the rest of the plan"

                Expect.equal
                    memoless.Memo.UnservedFootings
                    lost
                    "and a memoless tick derives the same loss"

                Expect.hasLength
                    stale.Memo.ServedFootings
                    3
                    "the served record is recomputed too, not left at the stale memo's empty"

                Expect.equal
                    stale.Memo.ServedFootings
                    memoless.Memo.ServedFootings
                    "tile for tile what a memoless tick reserves"
            }

            test "the unrouted trunks ride the memo with the rest of the plan" {
                // #107's record on the same seam, over a room that has in
                // fact lost a trunk: a matching signature hands back the
                // memo's own empty list rather than rederiving the loss,
                // and a moved one recomputes to exactly what a memoless
                // tick finds. ADR 0017's guarantee is that a recalled plan
                // reports what it reported when it was computed, and a
                // record that quietly recomputed itself would break it.
                let colony = enclosedSourceColony 4

                let dropped =
                    [
                        {
                            Source = "src-b"
                            Goal = TrunkGoal.UpgradeArea
                        }
                        {
                            Source = "src-b"
                            Goal = TrunkGoal.Spawn "spawn-1"
                        }
                    ]

                let matching = decide colony Map.empty Set.empty (Some(sentinelMemo colony))

                Expect.isEmpty
                    matching.Memo.UnroutedTrunks
                    "a matching signature reuses the memo's record; nothing recomputes"

                let stale = decide colony Map.empty Set.empty (Some(sentinelMemo (trunkColony 2)))
                let memoless = decide colony Map.empty Set.empty None

                Expect.equal
                    stale.Memo.UnroutedTrunks
                    dropped
                    "a moved signature recomputes the record with the rest of the plan"

                Expect.equal
                    memoless.Memo.UnroutedTrunks
                    dropped
                    "and a memoless tick derives the same loss"
            }

            test "a level-up invalidates the memo" {
                let memo = sentinelMemo (trunkColony 2)
                let decision = decide (trunkColony 3) Map.empty Set.empty (Some memo)
                let fresh = decide (trunkColony 3) Map.empty Set.empty None

                Expect.equal
                    (placementIntents decision.Intents)
                    (placementIntents fresh.Intents)
                    "a stale memo recomputes to exactly the fresh plan"
            }

            test "the memo's hauler quota feeds spawn planning; a stale one is discarded" {
                // The trunk colony has no source containers, so a fresh
                // quota is 0 and the sentinel's 3 is observable: with a
                // matching memo the hauler gap wins the casting order,
                // with a stale one the fresh quota casts a worker again.
                let snapshot =
                    { trunkColony 2 with
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let memo =
                    { sentinelMemo snapshot with
                        HaulerQuota = 3
                    }

                let castNames decision =
                    spawnIntents decision.Intents |> List.map (fun (_, _, name) -> name)

                let reused = decide snapshot Map.empty Set.empty (Some memo)

                Expect.equal
                    (castNames reused)
                    [ "hauler-42-Spawn1" ]
                    "the memo's quota opens a hauler gap the casting order fills first"

                let stale = decide (trunkColony 3) Map.empty Set.empty (Some memo)
                let fresh = decide (trunkColony 3) Map.empty Set.empty None

                Expect.equal
                    (castNames stale)
                    (castNames fresh)
                    "a stale memo's quota is recomputed, never reused"
            }

            test "decide without a memo emits one keyed to this census" {
                let snapshot = trunkColony 2
                let decision = decide snapshot Map.empty Set.empty None

                Expect.equal
                    decision.Memo.Signature
                    (censusSignature snapshot)
                    "the memo carries the census it was computed from"

                let next = decide snapshot Map.empty Set.empty (Some decision.Memo)

                Expect.equal
                    next.Intents
                    decision.Intents
                    "feeding the memo back reproduces the tick verbatim"
            }

            test "the spawn walks ride the memo while the census holds" {
                // ADR 0032. The flood a lead is priced off reads nothing
                // but the census, so the next tick under the same signature
                // fills the table it was handed rather than one of its own:
                // a row the first tick never priced lands beside the first
                // tick's entry instead of in a table nobody keeps.
                let heavy name =
                    creepWith name 0 50 [ Work; Work; Work; Work; Carry; Move ]

                let lone =
                    trunkColony 2 |> staffedColony [ worker "w1" 0 50 ] [ "w1", { X = 22; Y = 25 } ]

                let joined =
                    trunkColony 2
                    |> staffedColony
                        [ worker "w1" 0 50; heavy "a1" ]
                        [ "w1", { X = 22; Y = 25 }; "a1", { X = 23; Y = 25 } ]

                Expect.equal
                    (censusSignature joined)
                    (censusSignature lone)
                    "a creep arriving is not a census change"

                let first = decide lone Map.empty Set.empty None

                Expect.equal
                    first.Memo.Walks.Count
                    1
                    "the tick flooded once out of the spawn, into the table the memo carries"

                let workerFlood =
                    first.Memo.Walks |> Seq.map (fun entry -> entry.Value) |> Seq.exactlyOne

                let second = decide joined Map.empty Set.empty (Some first.Memo)

                Expect.isTrue
                    (obj.ReferenceEquals(second.Memo.Walks, first.Memo.Walks))
                    "an unchanged census hands the same table on"

                Expect.equal
                    second.Memo.Walks.Count
                    2
                    "and the second row's flood lands in it, beside the first tick's"

                Expect.isTrue
                    (second.Memo.Walks
                     |> Seq.exists (fun entry -> obj.ReferenceEquals(entry.Value, workerFlood)))
                    "the row the first tick priced was recalled, not flooded again"

                Expect.equal
                    second.Intents
                    (decide joined Map.empty Set.empty None).Intents
                    "a recalled walk decides exactly what a fresh flood decides"
            }

            test "a moved census drops the whole walk table" {
                // The Layout's own granularity (ADR 0032): a moved
                // signature may have moved the weights or the body the walk
                // is priced for, and telling which is a dependency tracker
                // the memo does not have.
                let staffed = staffedColony [ worker "w1" 0 50 ] [ "w1", { X = 22; Y = 25 } ]

                let first = decide (staffed (trunkColony 2)) Map.empty Set.empty None

                let levelled =
                    decide (staffed (trunkColony 3)) Map.empty Set.empty (Some first.Memo)

                Expect.isFalse
                    (obj.ReferenceEquals(levelled.Memo.Walks, first.Memo.Walks))
                    "a level-up gets a table of its own"

                // The same moved census over a colony with no creep to lead
                // shows what "dropped whole" means: nothing at all rides
                // across, not merely a stale entry priced again.
                let emptied = decide (trunkColony 3) Map.empty Set.empty (Some first.Memo)

                Expect.equal emptied.Memo.Walks.Count 0 "the memo's table went with its signature"
            }
        ]

/// Spatial projection holding exactly the given terrain tiles and target
/// positions; absent tiles are outside the projection (impassable). No
/// creep positions and no obstacles — movement tests add those on top. It
/// files them through `withHome` and inherits its ordering rule, which
/// bites hardest here because this funnel starts from `SpatialInfo.empty`:
/// the home name it resolves is the empty one, so a projection built by
/// this and *then* given a `RoomName` carries its geometry under the empty
/// name while `RoomName` says another, and every reader that asks by
/// *room* — the weight grid, the census signature, the hauler quota —
/// answers off `RoomLayer.empty`. The target-keyed queries still find it,
/// because `SpatialInfo.placementOf` scans every layer, which is what
/// makes the mistake quiet. Name the room first, then build: `openRoom` is
/// what that looks like.
let spatial targets tiles =
    SpatialInfo.empty
    |> withHome (fun layer ->
        { layer with
            Terrain = Map.ofList tiles
            TargetPositions = Map.ofList targets
        })

/// The 8 tiles around a position, all Plain: an open-ground source site.
let openSeats pos =
    [
        for dx in -1 .. 1 do
            for dy in -1 .. 1 do
                if (dx, dy) <> (0, 0) then
                    { X = pos.X + dx; Y = pos.Y + dy }, Plain
    ]

let harvesters assignments sourceId =
    assignments
    |> Map.toList
    |> List.filter (fun (_, tid) -> tid = taskId (Harvest sourceId))
    |> List.map fst

[<Tests>]
let seatTests =
    testList
        "seat capacity"
        [
            test "a single-Seat source gets exactly one of three empty creeps" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50; worker "w3" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] [ { X = 9; Y = 10 }, Plain ]

                    }

                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decide snapshot Map.empty Set.empty None

                Expect.hasLength
                    (harvesters assignments "src-a")
                    1
                    "one Seat supports exactly one harvester"

                let harvestIntents =
                    intents
                    |> List.filter (function
                        | HarvestSource _ -> true
                        | _ -> false)

                Expect.hasLength harvestIntents 1 "surplus creeps emit no Harvest intent"
            }

            test "creeps overflowing a single-Seat source are matched elsewhere" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50; worker "w3" 0 50 ]
                        Spatial =

                            spatial
                                [ "src-a", { X = 10; Y = 10 }; "src-b", { X = 20; Y = 20 } ]
                                ([ { X = 9; Y = 10 }, Plain ] @ openSeats { X = 20; Y = 20 })

                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.hasLength (harvesters assignments "src-a") 1 "the one Seat is filled"

                Expect.hasLength
                    (harvesters assignments "src-b")
                    2
                    "overflow lands on the source with free Seats"
            }

            test "a creep denied a Seat falls through to a lower-rank task" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 25 25; worker "w2" 25 25 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] [ { X = 9; Y = 10 }, Plain ]

                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.hasLength (harvesters assignments "src-a") 1 "the one Seat is filled"

                Expect.contains
                    (assignments |> Map.toList |> List.map snd)
                    (taskId (Upgrade "ctrl-1"))
                    "the denied creep sinks its energy into the controller instead"
            }

            test "Seats are counted from terrain: swamp is a Seat, wall and absent are not" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50; worker "w3" 0 50 ]
                        Spatial =

                            spatial
                                [ "src-a", { X = 10; Y = 10 } ]
                                [
                                    { X = 9; Y = 10 }, Plain
                                    { X = 11; Y = 10 }, Swamp
                                    { X = 10; Y = 9 }, Wall
                                ]

                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.hasLength
                    (harvesters assignments "src-a")
                    2
                    "plain and swamp neighbours are Seats; wall and off-map are not"
            }

            test "oversold remembered assignments are trimmed back to the Seat count" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] [ { X = 9; Y = 10 }, Plain ]

                    }

                let stale =
                    Map.ofList
                        [ "w1", (taskId (Harvest "src-a")); "w2", (taskId (Harvest "src-a")) ]

                let { Assignments = assignments } = decide snapshot stale Set.empty None

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "w1" ]
                    "the cap holds even against remembered oversell"
            }

            test "without a spatial projection Harvest stays uncapped" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50; worker "w3" 0 50 ]
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.hasLength
                    (harvesters assignments "src-a")
                    3
                    "no terrain data means no cap — today's room behaviour"
            }
        ]

[<Tests>]
let partApplicabilityTests =
    testList
        "part-based applicability"
        [
            test "a Work-less body is never matched to Harvest, Build, or Upgrade" {
                // Energy on board and capacity free: only the missing Work
                // part can make these tasks inapplicable.
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        ConstructionSites = [ { Id = "site-1" } ]
                        Controller = Some(controllerAt 2)
                        Creeps = [ creepWith "hauler" 25 25 [ Carry; Move ] ]
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.isEmpty
                    (Map.toList assignments)
                    "a body with no Work part can do none of the Work-part tasks"
            }

            test "a Carry-less body is never matched to Refill" {
                // Energy crafted non-zero so only the missing Carry part
                // can make Refill inapplicable.
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Refillables = [ refillable "spawn-1" 300 BuiltKind.Spawn ]
                        Creeps = [ creepWith "digger" 25 25 [ Work; Move ] ]
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.isEmpty
                    (Map.toList assignments)
                    "a body with no Carry part cannot deliver energy"
            }

            test "a remembered assignment to a task the body cannot do is released" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        ConstructionSites = []
                        Controller = None
                        Creeps = [ creepWith "hauler" 0 50 [ Carry; Move ] ]
                    }

                let remembered = Map.ofList [ "hauler", taskId (Harvest "src-a") ]
                let { Assignments = assignments } = decide snapshot remembered Set.empty None

                Expect.isEmpty
                    (Map.toList assignments)
                    "applicability release covers parts the body lacks"
            }
        ]

/// Spatial projection of a plain corridor x = 10, y = 9..21 with a source
/// at each end (source tiles are walls): "src-far" at (10, 10), "src-near"
/// at (10, 20).
let nearFarCorridor creepPositions =
    spatial
        [ "src-far", { X = 10; Y = 10 }; "src-near", { X = 10; Y = 20 } ]
        [
            for y in 9..21 -> { X = 10; Y = y }, (if y = 10 || y = 20 then Wall else Plain)
        ]
    |> withHome (fun layer ->
        { layer with
            CreepPositions = Map.ofList creepPositions
        })

[<Tests>]
let travelCostTests =
    testList
        "travel-cost matching"
        [
            test
                "live-bug regression: a fresh creep takes the near source regardless of Snapshot order" {
                // The creep stands three steps from the near source, seven
                // from the far one.
                let snapshotWith (sources: SourceInfo list) =
                    { bareRespawn with
                        Sources = sources
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = nearFarCorridor [ "w1", { X = 10; Y = 17 } ]
                    }

                let far: SourceInfo = source "src-far"
                let near: SourceInfo = source "src-near"

                for sources in [ [ far; near ]; [ near; far ] ] do
                    let { Assignments = assignments } =
                        decide (snapshotWith sources) Map.empty Set.empty None

                    Expect.equal
                        (Map.tryFind "w1" assignments)
                        (Some(taskId (Harvest "src-near")))
                        "the cheaper-to-reach source wins the rank tie"
            }

            test "swamp prices the route: a range-nearer target loses to a longer plain path" {
                // One corridor, a source at each end. src-swamp is 3 tiles
                // away by range but behind two swamp tiles (cost 20);
                // src-plain is 5 tiles away over plain ground (cost 8).
                let corridor =
                    [
                        { X = 10; Y = 12 }, Wall
                        { X = 10; Y = 13 }, Swamp
                        { X = 10; Y = 14 }, Swamp
                        { X = 10; Y = 15 }, Plain
                        { X = 10; Y = 16 }, Plain
                        { X = 10; Y = 17 }, Plain
                        { X = 10; Y = 18 }, Plain
                        { X = 10; Y = 19 }, Plain
                        { X = 10; Y = 20 }, Wall
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-swamp"; source "src-plain" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            spatial
                                [ "src-swamp", { X = 10; Y = 12 }; "src-plain", { X = 10; Y = 20 } ]
                                corridor
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 15 } ]
                                })
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Harvest "src-plain")))
                    "true path cost decides, not Chebyshev range"
            }

            test "rank dominates: an adjacent Build never outbids a four-tiles-away Refill" {
                // The hungry spawn sits at the top of the corridor, four
                // steps from the creep; the construction site is close
                // enough to build without moving at all.
                let corridor = [ for y in 10..16 -> { X = 10; Y = y }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "w1" 50 0 ]
                        Spatial =

                            spatial
                                [ "spawn-1", { X = 10; Y = 10 }; "site-1", { X = 10; Y = 16 } ]
                                corridor
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 15 } ]
                                    Obstacles = Set.singleton { X = 10; Y = 10 }
                                })
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Refill "spawn-1")))
                    "travel cost breaks ties within a rank, never across ranks"
            }

            test "a sticky assignment is kept even when a cheaper task exists this tick" {
                // Same corridor as the live-bug regression, but the creep
                // already holds the far source from an earlier tick.
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-far"; source "src-near" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = nearFarCorridor [ "w1", { X = 10; Y = 17 } ]
                    }

                let sticky = Map.ofList [ "w1", (taskId (Harvest "src-far")) ]
                let { Assignments = assignments } = decide snapshot sticky Set.empty None

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Harvest "src-far")))
                    "sticky assignments are never re-evaluated for a closer target"
            }

            test "an unplaced creep is matched as today: Snapshot order decides the tie" {
                // The projection places both sources but not the creep, so
                // no flood can run — the pick falls back to (rank, load).
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-far"; source "src-near" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = nearFarCorridor []
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Harvest "src-far")))
                    "without a creep position, behaviour is unchanged"
            }

            test "an unreachable Work Area makes the Task inapplicable: the creep sinks lower" {
                // The source's one Seat is walled off from the creep; the
                // controller is reachable. The half-full creep could do
                // either, but Harvest is off the table entirely — no
                // range-based fallback march at a wall.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 10; Y = 12 }, Wall
                        { X = 10; Y = 13 }, Plain
                        { X = 10; Y = 14 }, Plain
                        { X = 10; Y = 16 }, Wall
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 25 25 ]
                        Spatial =

                            spatial
                                [ "src-a", { X = 10; Y = 10 }; "ctrl-1", { X = 10; Y = 16 } ]
                                terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                })
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "the unreachable Harvest is not applicable to this creep at all"
            }

            test "body-aware cost: the slow heavy body is matched near, the generalist far" {
                // The near source hides behind two swamp tiles (terrain 20);
                // the far one lies nine plain steps away (terrain 18). By
                // bare terrain weight both creeps would march far. Priced
                // by body, the heavy one (5 fatigue parts on 3 Moves) wades
                // the swamps for 17 + 17 = 34 rather than walk nine plains
                // at ceil(10/3) = 4 apiece for 36 — while the generalist's
                // cost equals terrain, so it still takes the far source.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall // src-near
                        { X = 10; Y = 11 }, Swamp // its only Seat
                        { X = 10; Y = 12 }, Swamp
                        { X = 10; Y = 13 }, Plain // the heavy body stands here
                        { X = 11; Y = 13 }, Plain // the generalist beside it
                        yield! [ for y in 14..22 -> { X = 10; Y = y }, Plain ]
                        { X = 10; Y = 23 }, Wall // src-far
                    ]

                let heavy =
                    creepWith "mule" 0 50 [ Work; Work; Work; Work; Work; Carry; Move; Move; Move ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-near"; source "src-far" ]
                        Creeps = [ heavy; worker "runner" 0 50 ]
                        Spatial =

                            spatial
                                [ "src-near", { X = 10; Y = 10 }; "src-far", { X = 10; Y = 23 } ]
                                terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [
                                                "mule", { X = 10; Y = 13 }
                                                "runner", { X = 11; Y = 13 }
                                            ]
                                })
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "mule" assignments)
                    (Some(taskId (Harvest "src-near")))
                    "the slow body's real travel time keeps it near"

                Expect.equal
                    (Map.tryFind "runner" assignments)
                    (Some(taskId (Harvest "src-far")))
                    "the generalist stays the cheaper traveller to the far source"
            }
        ]

/// A rectangle of Plain tiles, bounds inclusive.
let plainRect x0 x1 y0 y1 =
    [
        for x in x0..x1 do
            for y in y0..y1 -> { X = x; Y = y }, Plain
    ]

let moveIntents intents =
    intents
    |> List.choose (function
        | MoveCreep(name, direction) -> Some(name, direction)
        | _ -> None)

/// Creep action Intents only — spawn and placement Intents filtered out.
let actionIntents intents =
    intents
    |> List.filter (function
        | HarvestSource _
        | TransferEnergyToStructure _
        | BuildSite _
        | UpgradeController _ -> true
        | _ -> false)

[<Tests>]
let movementTests =
    testList
        "movement"
        [
            test "a creep outside its Work Area steps toward the source, acting not yet" {
                // A one-tile-wide plain corridor: x = 10, y = 9..15, with the
                // source tile itself a wall (sources always sit on walls).
                let corridor =
                    [ for y in 9..15 -> { X = 10; Y = y }, Plain ] @ [ { X = 10; Y = 10 }, Wall ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] corridor
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                })
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (moveIntents intents)
                    [ "w1", Top ]
                    "one single-step move Intent up the corridor"

                Expect.isEmpty (actionIntents intents) "out of range: no action Intent yet"
            }

            test "a creep inside its Work Area acts and does not move" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] (openSeats { X = 10; Y = 10 })
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 11 } ]
                                })
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.contains intents (HarvestSource("w1", "src-a")) "seated creep harvests"

                Expect.isEmpty (moveIntents intents) "nowhere to go: no move Intent"
            }

            test "the approach detours around swamp when a plain lane is cheaper" {
                // Straight lane x = 10 is swamp (cost 10 each); the lane at
                // x = 11 is plain and reaches a Seat in as many steps.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 10; Y = 12 }, Swamp
                        { X = 10; Y = 13 }, Swamp
                        { X = 10; Y = 14 }, Plain
                        { X = 11; Y = 11 }, Plain
                        { X = 11; Y = 12 }, Plain
                        { X = 11; Y = 13 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                })
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (moveIntents intents)
                    [ "w1", TopRight ]
                    "the first step leaves the swamp lane for the plain one"
            }

            test
                "a loaded worker's first step lands on the road: the paved detour beats the terrain line" {
                // The terrain line runs straight up the plain lane x = 10,
                // three steps to the Seat at (10,11). A paved arc swings
                // out through x = 11..12 — four steps, one more than the
                // line — to the road Seat at (11,11); the unprojected gap
                // at (11,12)/(11,13) keeps the arc from being cut short.
                // The half-loaded worker prices a road step at 2 and a
                // plain step at 4, so the longer paved detour (8) beats the
                // straight terrain line (12): the road sets the first step.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 10; Y = 12 }, Plain
                        { X = 10; Y = 13 }, Plain
                        { X = 10; Y = 14 }, Plain
                        { X = 11; Y = 14 }, Plain
                        { X = 12; Y = 13 }, Plain
                        { X = 12; Y = 12 }, Plain
                        { X = 11; Y = 11 }, Plain
                    ]

                let paved =
                    Set.ofList
                        [
                            { X = 11; Y = 14 }
                            { X = 12; Y = 13 }
                            { X = 12; Y = 12 }
                            { X = 11; Y = 11 }
                        ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 25 25 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                    Roads = paved
                                })
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (moveIntents intents)
                    [ "w1", Right ]
                    "the first step leaves the terrain line for the paved detour"
            }

            test "a creep in range on a tile it may not keep acts and moves in one tick" {
                // An obstacle structure now sits under the creep (built beneath
                // it), so its tile is no longer Work Area — but the engine
                // judges actions by the tick-start position, so upgrading
                // this tick is still legal while stepping off.
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 50 0 ]
                        Spatial =

                            spatial
                                [ "ctrl-1", { X = 10; Y = 10 } ]
                                [
                                    { X = 10; Y = 10 }, Plain
                                    { X = 10; Y = 11 }, Plain
                                    { X = 10; Y = 12 }, Plain
                                ]
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 12 } ]
                                    Obstacles =
                                        Set.ofList [ { X = 10; Y = 10 }; { X = 10; Y = 12 } ]
                                })
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.contains
                    intents
                    (UpgradeController("w1", "ctrl-1"))
                    "in range at tick start: the action stays legal"

                Expect.equal
                    (moveIntents intents)
                    [ "w1", Top ]
                    "and the creep steps onto the one legal standing tile"
            }

            test "an unreachable Work Area yields no move Intent at all" {
                // The source's Seat exists but the tiles between creep and
                // Seat are outside the projection: no path, so the creep
                // waits instead of thrashing.
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            spatial
                                [ "src-a", { X = 10; Y = 10 } ]
                                [ { X = 10; Y = 11 }, Plain; { X = 10; Y = 14 }, Plain ]
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                })
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.isEmpty (moveIntents intents) "no path: standing still beats oscillating"
                Expect.isEmpty (actionIntents intents) "and the target is out of range"
            }

            test "a builder works from range 3 without closing in" {
                let snapshot =
                    { bareRespawn with
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "w1" 50 0 ]
                        Spatial =

                            spatial
                                [ "site-1", { X = 10; Y = 10 } ]
                                [ for y in 10..13 -> { X = 10; Y = y }, Plain ]
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 13 } ]
                                })
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.contains intents (BuildSite("w1", "site-1")) "range 3 is close enough"
                Expect.isEmpty (moveIntents intents) "no reason to walk closer"
            }

            test "a refiller two tiles out still has to walk to the structure" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                        Spatial =

                            spatial
                                [ "spawn-1", { X = 10; Y = 10 } ]
                                [ for y in 10..12 -> { X = 10; Y = y }, Plain ]
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 12 } ]
                                    Obstacles = Set.singleton { X = 10; Y = 10 }
                                })
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (moveIntents intents)
                    [ "w1", Top ]
                    "transfer needs range 1, so the creep closes in"

                Expect.isEmpty (actionIntents intents) "no transfer from range 2"
            }
        ]

[<Tests>]
let unreachableTests =
    testList
        "unreachable targets"
        [
            test
                "a remembered assignment to an unreachable source is released and its Seat refilled" {
                // src-a's one Seat connects only to w2; w1 sits on a walkable
                // island with no path anywhere, remembering the source from
                // before the wall closed in.
                let terrain =
                    [
                        { X = 10; Y = 11 }, Plain
                        { X = 10; Y = 12 }, Plain
                        { X = 20; Y = 20 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 25 25; worker "w2" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "w1", { X = 20; Y = 20 }; "w2", { X = 10; Y = 12 } ]
                                })
                    }

                let sticky = Map.ofList [ "w1", (taskId (Harvest "src-a")) ]
                let { Assignments = assignments } = decide snapshot sticky Set.empty None

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "w2" ]
                    "the freed Seat goes to the creep that can reach it"

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "the walled-off creep falls through to the next applicable task"
            }

            test "a creep with no reachable applicable task is left unassigned and emits nothing" {
                let terrain = [ { X = 10; Y = 11 }, Plain; { X = 20; Y = 20 }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 20; Y = 20 } ]
                                })
                    }

                let sticky = Map.ofList [ "w1", (taskId (Harvest "src-a")) ]

                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decide snapshot sticky Set.empty None

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    None
                    "the dead-end assignment is released"

                Expect.isEmpty (actionIntents intents) "no action fires at an unreachable target"
                Expect.isEmpty (moveIntents intents) "and no move Intent marches at the wall"
            }

            test "an empty Work Area releases a remembered assignment" {
                // The controller is placed but every tile within upgrade
                // range lies outside the projection: nowhere to stand at all.
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 50 0 ]
                        Spatial =

                            spatial [ "ctrl-1", { X = 10; Y = 10 } ] [ { X = 20; Y = 20 }, Plain ]
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 20; Y = 20 } ]
                                })
                    }

                let sticky = Map.ofList [ "w1", (taskId (Upgrade "ctrl-1")) ]
                let { Assignments = assignments } = decide snapshot sticky Set.empty None

                Expect.equal (Map.tryFind "w1" assignments) None "no Work Area means no assignment"
            }

            test
                "an unplaced creep keeps its assignment: no reachability filtering without geometry" {
                // Same walled-off source, but the projection does not place
                // the creep — nothing can be proven, so nothing is released.
                let terrain = [ { X = 10; Y = 11 }, Plain; { X = 20; Y = 20 }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = spatial [ "src-a", { X = 10; Y = 10 } ] terrain
                    }

                let sticky = Map.ofList [ "w1", (taskId (Harvest "src-a")) ]
                let { Assignments = assignments } = decide snapshot sticky Set.empty None

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "geometry the projection cannot price never releases an assignment"
            }
        ]

/// The tile one step in `direction` from `pos` — mirrors the engine's move.
let stepFrom pos direction =
    match direction with
    | Top -> { pos with Y = pos.Y - 1 }
    | TopRight -> { X = pos.X + 1; Y = pos.Y - 1 }
    | Right -> { pos with X = pos.X + 1 }
    | BottomRight -> { X = pos.X + 1; Y = pos.Y + 1 }
    | Bottom -> { pos with Y = pos.Y + 1 }
    | BottomLeft -> { X = pos.X - 1; Y = pos.Y + 1 }
    | Left -> { pos with X = pos.X - 1 }
    | TopLeft -> { X = pos.X - 1; Y = pos.Y - 1 }

/// Run the Resolver at its own seam: assigned Tasks as data over the
/// snapshot's Atlas; a creep absent from the list is idle. Move Intents
/// only; the movement Verdicts riding beside them are resolveVerdictsOn.
let resolveOn snapshot assigned =
    resolve snapshot (Atlas.ofSnapshot snapshot) noThreats (Map.ofList assigned) Set.empty
    |> fst

/// The Resolver's movement Verdicts at the same seam, with the named
/// creeps on the verbose list (ADR 0018).
let resolveVerdictsVerboseOn snapshot assigned verbose =
    resolve
        snapshot
        (Atlas.ofSnapshot snapshot)
        noThreats
        (Map.ofList assigned)
        (Set.ofList verbose)
    |> snd

/// The same for a quiet colony: nobody on the verbose list.
let resolveVerdictsOn snapshot assigned =
    resolveVerdictsVerboseOn snapshot assigned []

/// Run the Emitter at its own seam, over the same tick-start Atlas.
let emitOn snapshot assigned =
    emit snapshot (Atlas.ofSnapshot snapshot) noThreats (Map.ofList assigned)

/// Two single-Seat sources at the ends of a two-tile corridor; each creep
/// stands on the other's Seat.
let headOnSwap =
    let terrain =
        [
            { X = 10; Y = 10 }, Wall
            { X = 10; Y = 11 }, Plain
            { X = 10; Y = 12 }, Plain
            { X = 10; Y = 13 }, Wall
        ]

    { bareRespawn with
        Sources = [ source "src-a"; source "src-b" ]
        Creeps = [ worker "wa" 0 50; worker "wb" 0 50 ]
        Spatial =

            spatial [ "src-a", { X = 10; Y = 10 }; "src-b", { X = 10; Y = 13 } ] terrain
            |> withHome (fun layer ->
                { layer with
                    CreepPositions =
                        Map.ofList [ "wa", { X = 10; Y = 12 }; "wb", { X = 10; Y = 11 } ]
                })
    }

[<Tests>]
let arbitrationTests =
    testList
        "yield arbitration"
        [
            test
                "squatting regression: the upgrader on the sole Seat yields to the inbound harvester" {
                // Source at (10,10) with (10,11) as its only Seat; controller
                // at (10,14), so the Seat is also at upgrade range 3. The
                // upgrader squats the Seat; the harvester stands one tile out.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 14 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 9; Y = 12 }, Plain
                        { X = 10; Y = 12 }, Plain
                        { X = 11; Y = 12 }, Plain
                        { X = 10; Y = 13 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "har" 0 50; worker "upg" 50 0 ]
                        Spatial =

                            spatial
                                [ "src-a", { X = 10; Y = 10 }; "ctrl-1", { X = 10; Y = 14 } ]
                                terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [
                                                "har", { X = 10; Y = 12 }
                                                "upg", { X = 10; Y = 11 }
                                            ]
                                })
                    }

                let assigned = [ "har", Harvest "src-a"; "upg", Upgrade "ctrl-1" ]
                let moves = resolveOn snapshot assigned |> moveIntents

                Expect.contains moves ("har", Top) "the harvester steps onto the Seat"

                Expect.contains
                    (emitOn snapshot assigned)
                    (UpgradeController("upg", "ctrl-1"))
                    "the displaced upgrader still upgrades this tick"

                match moves |> List.filter (fun (name, _) -> name = "upg") with
                | [ (_, direction) ] ->
                    let dest = stepFrom { X = 10; Y = 11 } direction

                    Expect.isLessThanOrEqual
                        (max (abs (dest.X - 10)) (abs (dest.Y - 14)))
                        3
                        "the upgrader is displaced to a tile still inside its Work Area"
                | other -> failtest $"expected exactly one move for the upgrader, got %A{other}"
            }

            test "head-on swap: two creeps blocking each other exchange tiles" {
                let moves =
                    resolveOn headOnSwap [ "wa", Harvest "src-a"; "wb", Harvest "src-b" ]
                    |> moveIntents

                Expect.equal
                    (moves |> List.sort)
                    [ "wa", Top; "wb", Bottom ]
                    "both creeps move: they swap instead of deadlocking"
            }

            test "pipeline wiring: remembered assignments flow through match, emit, and resolve" {
                // The one arbitration test that still runs the whole decide
                // seam: sticky Assignments survive the Matcher, the Emitter
                // says their glyphs, and the Resolver settles the swap.
                let sticky =
                    Map.ofList
                        [ "wa", (taskId (Harvest "src-a")); "wb", (taskId (Harvest "src-b")) ]

                let {
                        Intents = intents
                        Assignments = next
                    } =
                    decide headOnSwap sticky Set.empty None

                Expect.equal next sticky "the Matcher keeps both remembered assignments"

                Expect.contains
                    intents
                    (SayCreep("wa", "⛏"))
                    "the Emitter's bubbles reach decide's output"

                Expect.equal
                    (moveIntents intents |> List.sort)
                    [ "wa", Top; "wb", Bottom ]
                    "the Resolver's swap reaches decide's output"
            }

            test "an idle creep is displaced by a working creep passing through" {
                // w2 carries no assignment and idles astride the harvester's
                // path.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 10; Y = 12 }, Plain
                        { X = 10; Y = 13 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50; worker "w2" 50 0 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "w1", { X = 10; Y = 13 }; "w2", { X = 10; Y = 12 } ]
                                })
                    }

                let moves = resolveOn snapshot [ "w1", Harvest "src-a" ] |> moveIntents

                Expect.contains moves ("w1", Top) "the working creep claims the idler's tile"

                Expect.isTrue
                    (moves |> List.exists (fun (name, _) -> name = "w2"))
                    "the idler is displaced out of the way"
            }

            test "a contested tile goes to the higher task rank" {
                // One gap at (10,12): the harvester's and the upgrader's
                // cheapest paths both step onto it. Harvest outranks Upgrade,
                // so the upgrader waits in place.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 8 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 10; Y = 12 }, Plain
                        { X = 10; Y = 13 }, Plain
                        { X = 11; Y = 13 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "h" 0 50; worker "u" 50 0 ]
                        Spatial =

                            spatial
                                [ "src-a", { X = 10; Y = 10 }; "ctrl-1", { X = 10; Y = 8 } ]
                                terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "h", { X = 10; Y = 13 }; "u", { X = 11; Y = 13 } ]
                                })
                    }

                let moves =
                    resolveOn snapshot [ "h", Harvest "src-a"; "u", Upgrade "ctrl-1" ]
                    |> moveIntents

                Expect.equal
                    moves
                    [ "h", Top ]
                    "the harvester takes the gap; the outranked upgrader waits"
            }

            test "within a rank the most-constrained creep places first" {
                // Two Seats; h1 sits on the one h2's cheapest path targets.
                // h2 (one candidate tile) outranks h1 (two) inside the same
                // priority, so h1 shuffles along to the free Seat.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 11; Y = 11 }, Plain
                        { X = 9; Y = 12 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "h1" 0 50; worker "h2" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "h1", { X = 10; Y = 11 }; "h2", { X = 9; Y = 12 } ]
                                })
                    }

                let assigned = [ "h1", Harvest "src-a"; "h2", Harvest "src-a" ]

                Expect.equal
                    (resolveOn snapshot assigned |> moveIntents |> List.sort)
                    [ "h1", Right; "h2", TopRight ]
                    "h2 claims the occupied Seat; h1 is displaced to the free one"

                Expect.contains
                    (emitOn snapshot assigned)
                    (HarvestSource("h1", "src-a"))
                    "the displaced harvester still harvests this tick"
            }

            test "a builder blocked by a seated harvester still makes progress" {
                // Corridor y=12, x 8..15. Source at (10,11) seats the
                // harvester mid-corridor; the site sits at the far end. The
                // builder's only path runs through the seated harvester's
                // tile — it must not stand idle while a swap (or an in-area
                // shuffle by the harvester) would let it pass.
                let terrain = [ for x in 8..15 -> { X = x; Y = 12 }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "har" 0 50; worker "bob" 50 0 ]
                        Spatial =

                            spatial
                                [ "src-a", { X = 10; Y = 11 }; "site-1", { X = 15; Y = 12 } ]
                                terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "har", { X = 10; Y = 12 }; "bob", { X = 9; Y = 12 } ]
                                })
                    }

                let assigned = [ "har", Harvest "src-a"; "bob", Build "site-1" ]
                let moves = resolveOn snapshot assigned |> moveIntents

                Expect.isTrue
                    (moves |> List.exists (fun (name, _) -> name = "bob"))
                    "the travelling builder moves instead of stalling behind the seat"

                Expect.contains
                    (emitOn snapshot assigned)
                    (HarvestSource("har", "src-a"))
                    "the harvester still harvests this tick"
            }

            test "a fatigued creep is never asked to move, nor displaced through" {
                // The same one-lane corridor, but the seated harvester is
                // still paying off fatigue: the engine would answer any move
                // with ERR_TIRED, so the Resolver issues none — neither to
                // the harvester nor to the builder whose only path runs
                // through its blocked tile.
                let terrain = [ for x in 8..15 -> { X = x; Y = 12 }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ { worker "har" 0 50 with Fatigue = 4 }; worker "bob" 50 0 ]
                        Spatial =

                            spatial
                                [ "src-a", { X = 10; Y = 11 }; "site-1", { X = 15; Y = 12 } ]
                                terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "har", { X = 10; Y = 12 }; "bob", { X = 9; Y = 12 } ]
                                })
                    }

                let assigned = [ "har", Harvest "src-a"; "bob", Build "site-1" ]

                Expect.isEmpty
                    (resolveOn snapshot assigned |> moveIntents)
                    "no move Intent the engine would refuse with ERR_TIRED"

                Expect.contains
                    (emitOn snapshot assigned)
                    (HarvestSource("har", "src-a"))
                    "the tired harvester still harvests this tick"
            }

            test "a fatigued traveller stands down for the tick instead of failing a move" {
                // The live -11 spam came from loaded travellers: a creep
                // mid-journey with fatigue outstanding used to be issued its
                // next step anyway, which the engine refused every tick.
                let corridor =
                    [ for y in 9..15 -> { X = 10; Y = y }, Plain ] @ [ { X = 10; Y = 10 }, Wall ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ { worker "w1" 0 50 with Fatigue = 4 } ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] corridor
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                })
                    }

                Expect.isEmpty
                    (resolveOn snapshot [ "w1", Harvest "src-a" ] |> moveIntents)
                    "a rested copy of this creep would step Top; the tired one is issued nothing"
            }

            test "a travelling builder detours around a seated harvester when a lane is open" {
                // The corridor grows a parallel lane at y = 13. The straight
                // path runs through the seated harvester's tile; the flood
                // prices that tile dearer for the standing creep, so the
                // builder sidesteps into the lane instead of displacing the
                // Seat.
                let terrain =
                    [ for x in 8..15 -> { X = x; Y = 12 }, Plain ]
                    @ [ for x in 8..15 -> { X = x; Y = 13 }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "har" 0 50; worker "bob" 50 0 ]
                        Spatial =

                            spatial
                                [ "src-a", { X = 10; Y = 11 }; "site-1", { X = 15; Y = 12 } ]
                                terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "har", { X = 10; Y = 12 }; "bob", { X = 9; Y = 12 } ]
                                })
                    }

                let assigned = [ "har", Harvest "src-a"; "bob", Build "site-1" ]

                Expect.equal
                    (resolveOn snapshot assigned |> moveIntents)
                    [ "bob", BottomRight ]
                    "the builder takes the lane; the seated harvester is left alone"
            }

            test "an occupant with no in-area alternative swaps with its displacer" {
                // The upgrader's only in-area standing tile is the Seat
                // itself: every adjacent walkable tile is outside upgrade
                // range. Displaced, it swaps into the harvester's tile.
                let terrain =
                    [
                        { X = 11; Y = 12 }, Wall
                        { X = 13; Y = 12 }, Wall
                        { X = 10; Y = 12 }, Plain
                        { X = 9; Y = 11 }, Plain
                        { X = 9; Y = 12 }, Plain
                        { X = 9; Y = 13 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "har" 0 50; worker "upg" 50 0 ]
                        Spatial =

                            spatial
                                [ "src-a", { X = 11; Y = 12 }; "ctrl-1", { X = 13; Y = 12 } ]
                                terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "har", { X = 9; Y = 12 }; "upg", { X = 10; Y = 12 } ]
                                })
                    }

                let assigned = [ "har", Harvest "src-a"; "upg", Upgrade "ctrl-1" ]

                Expect.equal
                    (resolveOn snapshot assigned |> moveIntents |> List.sort)
                    [ "har", Right; "upg", Left ]
                    "displacer and occupant exchange tiles"

                Expect.contains
                    (emitOn snapshot assigned)
                    (UpgradeController("upg", "ctrl-1"))
                    "the swapped-out upgrader still upgrades from its tick-start tile"
            }
        ]

[<Tests>]
let resolverVerdictTests =
    testList
        "resolver verdicts"
        [
            test "a grounded creep gets a grounded Verdict; the creep behind it yields to it" {
                // The one-lane corridor with a fatigued seated harvester: har
                // sits arbitration out with its tile blocked, and bob — whose
                // only path runs through that tile — stands down for the tick.
                let terrain = [ for x in 8..15 -> { X = x; Y = 12 }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ { worker "har" 0 50 with Fatigue = 4 }; worker "bob" 50 0 ]
                        Spatial =

                            spatial
                                [ "src-a", { X = 10; Y = 11 }; "site-1", { X = 15; Y = 12 } ]
                                terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "har", { X = 10; Y = 12 }; "bob", { X = 9; Y = 12 } ]
                                })
                    }

                Expect.equal
                    (resolveVerdictsOn snapshot [ "har", Harvest "src-a"; "bob", Build "site-1" ])
                    [ Verdict.Grounded "har"; Verdict.Yielded("bob", "har") ]
                    "har is grounded; bob's blocked step names the tired creep holding the tile"
            }

            test "a lone fatigued traveller is grounded, nothing more" {
                let corridor =
                    [ for y in 9..15 -> { X = 10; Y = y }, Plain ] @ [ { X = 10; Y = 10 }, Wall ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ { worker "w1" 0 50 with Fatigue = 4 } ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] corridor
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                })
                    }

                Expect.equal
                    (resolveVerdictsOn snapshot [ "w1", Harvest "src-a" ])
                    [ Verdict.Grounded "w1" ]
                    "grounding is the whole story: no move was asked, none was denied"
            }

            test "a displaced squatter's Verdict names its displacer" {
                // The squatting regression's geometry: the upgrader on the
                // sole Seat is displaced by the inbound harvester.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 14 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 9; Y = 12 }, Plain
                        { X = 10; Y = 12 }, Plain
                        { X = 11; Y = 12 }, Plain
                        { X = 10; Y = 13 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "har" 0 50; worker "upg" 50 0 ]
                        Spatial =

                            spatial
                                [ "src-a", { X = 10; Y = 10 }; "ctrl-1", { X = 10; Y = 14 } ]
                                terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [
                                                "har", { X = 10; Y = 12 }
                                                "upg", { X = 10; Y = 11 }
                                            ]
                                })
                    }

                Expect.equal
                    (resolveVerdictsOn snapshot [ "har", Harvest "src-a"; "upg", Upgrade "ctrl-1" ])
                    [ Verdict.Yielded("upg", "har") ]
                    "the displaced upgrader yields to the harvester; the harvester says nothing"
            }

            test "losing a contested tile to a higher rank is a yield naming the winner" {
                // The contested-gap geometry: Harvest outranks Upgrade, so
                // the upgrader waits in place while the harvester takes the
                // gap it also wanted.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 8 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 10; Y = 12 }, Plain
                        { X = 10; Y = 13 }, Plain
                        { X = 11; Y = 13 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "h" 0 50; worker "u" 50 0 ]
                        Spatial =

                            spatial
                                [ "src-a", { X = 10; Y = 10 }; "ctrl-1", { X = 10; Y = 8 } ]
                                terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "h", { X = 10; Y = 13 }; "u", { X = 11; Y = 13 } ]
                                })
                    }

                Expect.equal
                    (resolveVerdictsOn snapshot [ "h", Harvest "src-a"; "u", Upgrade "ctrl-1" ])
                    [ Verdict.Yielded("u", "h") ]
                    "the outranked upgrader's wait is attributed to the harvester"
            }

            test "the reroute Verdict is manufactured only for a creep on the verbose list" {
                // The two-lane corridor: the builder's straight path runs
                // through the seated harvester's tile, and the surcharge
                // sends it into the parallel lane instead. Nobody yields —
                // the detour is a pricing event, not an arbitration one.
                let terrain =
                    [ for x in 8..15 -> { X = x; Y = 12 }, Plain ]
                    @ [ for x in 8..15 -> { X = x; Y = 13 }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "har" 0 50; worker "bob" 50 0 ]
                        Spatial =

                            spatial
                                [ "src-a", { X = 10; Y = 11 }; "site-1", { X = 15; Y = 12 } ]
                                terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "har", { X = 10; Y = 12 }; "bob", { X = 9; Y = 12 } ]
                                })
                    }

                let assigned = [ "har", Harvest "src-a"; "bob", Build "site-1" ]

                Expect.isEmpty
                    (resolveVerdictsOn snapshot assigned)
                    "a quiet colony pays for no second flood, so it records no reroute"

                Expect.isEmpty
                    (resolveVerdictsVerboseOn snapshot assigned [ "har" ])
                    "the list is read per creep: the detourer is not the one being watched"

                Expect.equal
                    (resolveVerdictsVerboseOn snapshot assigned [ "bob" ])
                    [ Verdict.Rerouted "bob" ]
                    "the lane sidestep is attributed to traffic; the seated harvester says nothing"
            }

            test "a creep simply stepping toward its Work Area produces no movement noise" {
                let corridor =
                    [ for y in 9..15 -> { X = 10; Y = y }, Plain ] @ [ { X = 10; Y = 10 }, Wall ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] corridor
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                })
                    }

                Expect.isEmpty
                    (resolveVerdictsOn snapshot [ "w1", Harvest "src-a" ])
                    "conclusion level means events, not every step"
            }

            test "a clean head-on swap is silent: both creeps settle where they asked" {
                Expect.isEmpty
                    (resolveVerdictsOn headOnSwap [ "wa", Harvest "src-a"; "wb", Harvest "src-b" ])
                    "each traveller got exactly its preferred tile; nothing became of either move"
            }

            test "movement Verdicts ride behind the Matcher's in decide's output" {
                // A fatigued lone traveller at the decide seam: the Matcher
                // speaks first (the fresh match), the Resolver after (the
                // grounding) — one additive list, interleaved downstream.
                let corridor =
                    [ for y in 9..15 -> { X = 10; Y = y }, Plain ] @ [ { X = 10; Y = 10 }, Wall ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ { worker "w1" 0 50 with Fatigue = 4 } ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] corridor
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                })
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [
                        Verdict.Matched("w1", taskId (Harvest "src-a"), MatchFactor.OnlyCandidate)
                        Verdict.Grounded "w1"
                    ]
                    "matcher verdicts first, then the Resolver's, in one list"
            }
        ]

[<Tests>]
let workforceTests =
    testList
        "workforce target"
        [
            // Two sources spaced apart: src-a with three Seats, src-b with
            // two — a Seat total of five.
            let fiveSeats =
                spatial
                    [ "src-a", { X = 10; Y = 10 }; "src-b", { X = 30; Y = 30 } ]
                    [
                        { X = 9; Y = 10 }, Plain
                        { X = 11; Y = 10 }, Plain
                        { X = 10; Y = 9 }, Plain
                        { X = 29; Y = 30 }, Plain
                        { X = 31; Y = 30 }, Plain
                    ]

            test "the Seat total raises the target above the floor" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50 ]
                        Spatial = fiveSeats
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.hasLength
                    (spawnIntents intents)
                    1
                    "five Seats support five creeps; two living is a deficit"
            }

            test "no spawn Intent once the workforce matches the Seat total" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ for i in 1..5 -> worker $"w{i}" 0 50 ]
                        Spatial = fiveSeats
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (spawnIntents intents) "workforce already at target"
            }

            test "a Seat total below the floor leaves the floor in charge" {
                let oneSeat = spatial [ "src-a", { X = 10; Y = 10 } ] [ { X = 9; Y = 10 }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = oneSeat
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.hasLength
                    (spawnIntents intents)
                    1
                    "one Seat cannot lower the target below the floor of two"
            }

            test "an unplaced source contributes no Seats" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50 ]
                        Spatial = spatial [] []
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (spawnIntents intents) "only the floor applies"
            }
        ]

let sayIntents intents =
    intents
    |> List.choose (function
        | SayCreep(name, message) -> Some(name, message)
        | _ -> None)

[<Tests>]
let sayTests =
    testList
        "chat bubbles"
        [
            test "an assigned harvester says the Harvest glyph" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.contains
                    intents
                    (SayCreep("w1", "⛏"))
                    "the bubble shows the creep's current Task"
            }

            test "each Task has its own glyph: Refill, Build, Upgrade" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "w1" 50 0; worker "w2" 50 0; worker "w3" 50 0 ]
                    }

                let sticky =
                    Map.ofList
                        [
                            "w1", (taskId (Refill "spawn-1"))
                            "w2", (taskId (Build "site-1"))
                            "w3", (taskId (Upgrade "ctrl-1"))
                        ]

                let { Intents = intents } = decide snapshot sticky Set.empty None

                Expect.equal
                    (sayIntents intents)
                    [ "w1", "🔋"; "w2", "🔨"; "w3", "⚡" ]
                    "one bubble per assigned creep, glyph matched to its Task"
            }

            test "an unassigned creep says nothing" {
                // Nothing applicable for a full creep: no refill need, no
                // sites, no controller.
                let snapshot =
                    { bareRespawn with
                        Controller = None
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (sayIntents intents) "no Task, no bubble"
            }

            test "a creep still walking toward its target says its glyph anyway" {
                // Out of action range: no action Intent this tick, but the
                // assignment holds — the bubble reports it every tick.
                let corridor =
                    [ for y in 9..15 -> { X = 10; Y = y }, Plain ] @ [ { X = 10; Y = 10 }, Wall ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] corridor
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                })
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.isEmpty (actionIntents intents) "out of range: no action Intent yet"
                Expect.equal (sayIntents intents) [ "w1", "⛏" ] "the bubble still shows the Task"
            }
        ]

/// Project one structure of the given built kind carrying the given hits
/// onto a snapshot — position-less: unpriceable geometry never counts
/// against a Task (ADR 0004), so the pool and matching are exercised
/// without terrain.
let withHits id kind hits hitsMax (snapshot: Snapshot) =
    { snapshot with
        Spatial =
            { snapshot.Spatial with
                TargetKinds = Map.add id (Structure kind) snapshot.Spatial.TargetKinds
                Hits = Map.add id { Hits = hits; HitsMax = hitsMax } snapshot.Spatial.Hits
            }
    }

let repairTasks tasks =
    tasks
    |> List.choose (function
        | Repair structureId -> Some structureId
        | _ -> None)

[<Tests>]
let repairTests =
    testList
        "repair"
        [
            test "a road below half hits yields a Repair task; at half it yields none" {
                let low = bareRespawn |> withHits "road-1" BuiltKind.Road 2499 5000
                let half = bareRespawn |> withHits "road-1" BuiltKind.Road 2500 5000

                Expect.equal
                    (repairTasks (planTasks low noThreats))
                    [ "road-1" ]
                    "below the trigger: one Repair per ailing road"

                Expect.isEmpty
                    (repairTasks (planTasks half noThreats))
                    "at half hits the road is left alone"
            }

            test "a repaired-whole road leaves the pool" {
                let whole = bareRespawn |> withHits "road-1" BuiltKind.Road 5000 5000

                Expect.isEmpty
                    (repairTasks (planTasks whole noThreats))
                    "a whole road needs nothing"
            }

            test "kinds with no whole line never enter the pool on low hits" {
                // The Snapshot projects hits on repairable kinds only, but the
                // kind gate holds in the Planner regardless of what arrives.
                // The extensions are deliberately outside the Keep (ADR
                // 0034): cheap, twenty of them, and no creep lives on one.
                let snapshot =
                    bareRespawn
                    |> withHits "ext-1" BuiltKind.Extension 1 5000
                    |> withHits "link-1" BuiltKind.Link 1 5000
                    |> withHits "rock-1" BuiltKind.Other 1 5000

                Expect.isEmpty
                    (repairTasks (planTasks snapshot noThreats))
                    "an extension, a link and an unmodelled structure are nobody's Repair"
            }

            test "a dented Keep structure enters the pool; a whole one does not" {
                // The Keep is repaired to full (ADR 0034): it does not decay,
                // so below max means it was damaged — the same fact the
                // safe-mode arm reads, which is why a dented Keep is never
                // left standing. This revises ADR 0023's "nothing repairs the
                // Storage".
                let dented =
                    bareRespawn
                    |> withHits "spawn-1" BuiltKind.Spawn 4999 5000
                    |> withHits "tower-1" BuiltKind.Tower 4999 5000
                    |> withHits "sto-1" BuiltKind.Storage 4999 5000

                Expect.equal
                    (repairTasks (planTasks dented noThreats))
                    [ "spawn-1"; "sto-1"; "tower-1" ]
                    "one hit off max is hungry, on every Keep structure"

                let whole =
                    bareRespawn
                    |> withHits "spawn-1" BuiltKind.Spawn 5000 5000
                    |> withHits "tower-1" BuiltKind.Tower 5000 5000
                    |> withHits "sto-1" BuiltKind.Storage 5000 5000

                Expect.isEmpty
                    (repairTasks (planTasks whole noThreats))
                    "a Keep at full hits asks for nothing"
            }

            test "a rampart is hungry below its floor and whole at it" {
                // The floor, not half of max (ADR 0034): a rampart's max is
                // three million at RCL4, so the decaying kinds' fraction
                // would leave it hungry forever. The number restates the
                // tunable, exactly as the road tests restate the half.
                let floor = 100_000
                let max = 3_000_000

                let below = bareRespawn |> withHits "ram-1" BuiltKind.Rampart (floor - 1) max
                let at = bareRespawn |> withHits "ram-1" BuiltKind.Rampart floor max
                let fresh = bareRespawn |> withHits "ram-1" BuiltKind.Rampart 1 max
                let over = bareRespawn |> withHits "ram-1" BuiltKind.Rampart (max / 2) max

                Expect.equal
                    (repairTasks (planTasks below noThreats))
                    [ "ram-1" ]
                    "one hit under the floor is hungry"

                Expect.isEmpty
                    (repairTasks (planTasks at noThreats))
                    "at the floor the rampart is whole"

                Expect.equal
                    (repairTasks (planTasks fresh noThreats))
                    [ "ram-1" ]
                    "a rampart just built stands at 1 hit and is the pool's business at once"

                Expect.isEmpty
                    (repairTasks (planTasks over noThreats))
                    "half of a rampart's max is far over the floor: nothing to do"
            }

            test "a surplus creep is sent to repair: assignment, intent and bubble" {
                // Feeding satisfied — the spawn is full, the creep can carry no
                // more — so the surplus tier is all that is left, and the
                // half-hit road is its only member.
                let snapshot =
                    { bareRespawn with
                        Controller = None
                        Creeps = [ worker "w1" 50 0 ]
                    }
                    |> withHits "road-1" BuiltKind.Road 100 5000

                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Repair "road-1")))
                    "the surplus creep is assigned to the Repair"

                Expect.contains
                    intents
                    (RepairStructure("w1", "road-1"))
                    "the assignment emits the repair intent"

                Expect.equal (sayIntents intents) [ "w1", "🔧" ] "a repairing creep says 🔧"
            }

            test "Repair never poaches from the feeding tier" {
                // A hungry spawn and an ailing road bid for the same loaded
                // creep: the feeding tier wins on rank, not pool order.
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                    }
                    |> withHits "road-1" BuiltKind.Road 100 5000

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("w1", taskId (Refill "spawn-1"), MatchFactor.Rank) ]
                    "the colony feeds itself before it patches roads: rank decided"
            }

            test "Repair never poaches from Harvest either" {
                // A half-loaded creep fits both tiers — room to harvest,
                // energy to spend — and the feeding tier wins on rank.
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Controller = None
                        Creeps = [ worker "w1" 25 25 ]
                    }
                    |> withHits "road-1" BuiltKind.Road 100 5000

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("w1", taskId (Harvest "src-a"), MatchFactor.Rank) ]
                    "the economy is fed before roads are patched: rank decided"
            }

            test "a container below half hits yields a Repair task; at half it yields none" {
                let low = bareRespawn |> withHits "cont-1" BuiltKind.Container 124999 250000
                let half = bareRespawn |> withHits "cont-1" BuiltKind.Container 125000 250000

                Expect.equal
                    (repairTasks (planTasks low noThreats))
                    [ "cont-1" ]
                    "below the trigger: one Repair per ailing container"

                Expect.isEmpty
                    (repairTasks (planTasks half noThreats))
                    "at half hits the container is left alone"
            }

            test "a whole container produces no Repair" {
                let whole = bareRespawn |> withHits "cont-1" BuiltKind.Container 250000 250000

                Expect.isEmpty
                    (repairTasks (planTasks whole noThreats))
                    "a whole container needs nothing"
            }

            test "container Repair is surplus-tier: feeding still wins the creep" {
                // The same duel the road fights: a hungry spawn and an ailing
                // container bid for one loaded creep, and feeding wins on rank.
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                    }
                    |> withHits "cont-1" BuiltKind.Container 100 250000

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("w1", taskId (Refill "spawn-1"), MatchFactor.Rank) ]
                    "the colony feeds itself before it mends containers: rank decided"
            }

            test "a surplus creep mends the container: assignment, intent and bubble" {
                // Feeding satisfied — spawn full, creep full — so the ailing
                // container is the only work left, exactly like a road.
                let snapshot =
                    { bareRespawn with
                        Controller = None
                        Creeps = [ worker "w1" 50 0 ]
                    }
                    |> withHits "cont-1" BuiltKind.Container 100 250000

                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Repair "cont-1")))
                    "the surplus creep is assigned to the container Repair"

                Expect.contains
                    intents
                    (RepairStructure("w1", "cont-1"))
                    "the assignment emits the repair intent"

                Expect.equal (sayIntents intents) [ "w1", "🔧" ] "a repairing creep says 🔧"
            }

            test "an empty creep is inapplicable to Repair" {
                // Nothing to spend: no energy makes Repair unworkable, and the
                // remembered assignment is released rather than kept.
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Creeps = [ worker "w1" 0 50 ]
                    }
                    |> withHits "road-1" BuiltKind.Road 100 5000

                let remembered = Map.ofList [ "w1", taskId (Repair "road-1") ]

                let {
                        Verdicts = verdicts
                        Assignments = assignments
                    } =
                    decide snapshot remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("w1", taskId (Repair "road-1"), ReleaseReason.Inapplicable))
                    "the empty creep's remembered Repair is released"

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    None
                    "nothing else fits an empty creep here"
            }
        ]

[<Tests>]
let verdictTests =
    testList
        "matcher verdicts"
        [
            test "a lone applicable Task wins as the only candidate" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Controller = None
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("w1", taskId (Harvest "src-a"), MatchFactor.OnlyCandidate) ]
                    "one creep, one candidate: the Verdict names the Task and the walkover"
            }

            test "rank decides: Refill outbids Upgrade for a loaded creep" {
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("w1", taskId (Refill "spawn-1"), MatchFactor.Rank) ]
                    "the feeding tier beat the surplus tier: rank decided"
            }

            test "rank layers by target: feeding the spawn outbids feeding the tower" {
                // The tower sits first in the pool, so only the target-layered
                // rank (ADR 0010) — not pool order — can hand the spawn the win.
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Refillables =
                            [
                                refillable "tower-1" 500 BuiltKind.Tower
                                refillable "spawn-1" 50 BuiltKind.Spawn
                            ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("w1", taskId (Refill "spawn-1"), MatchFactor.Rank) ]
                    "the colony feeds its own reproduction before its guns: rank decided"
            }

            test "travel cost decides: the near source wins the rank tie" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-far"; source "src-near" ]
                        Controller = None
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = nearFarCorridor [ "w1", { X = 10; Y = 17 } ]
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("w1", taskId (Harvest "src-near"), MatchFactor.TravelCost) ]
                    "same rank, cheaper path: travel cost decided"
            }

            test "load decides: the second creep spreads to the emptier source" {
                let snapshot =
                    { bareRespawn with
                        Controller = None
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50 ]
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [
                        Verdict.Matched("w1", taskId (Harvest "src-a"), MatchFactor.PoolOrder)
                        Verdict.Matched("w2", taskId (Harvest "src-b"), MatchFactor.Load)
                    ]
                    "w1's tie fell to pool order; w2 avoided the loaded source"
            }

            test "a remembered assignment kept is distinguishable from a fresh match" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-far"; source "src-near" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = nearFarCorridor [ "w1", { X = 10; Y = 17 } ]
                    }

                let sticky = Map.ofList [ "w1", taskId (Harvest "src-far") ]
                let { Verdicts = verdicts } = decide snapshot sticky Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Kept("w1", taskId (Harvest "src-far")) ]
                    "anti-thrash speaks as Kept, never as a fresh Matched"
            }

            test "a Task that left the pool releases with TaskGone" {
                // The remembered Refill target has no free capacity this
                // tick, so the Planner never generates the Task.
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let sticky = Map.ofList [ "w1", taskId (Refill "spawn-1") ]
                let { Verdicts = verdicts } = decide snapshot sticky Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("w1", taskId (Refill "spawn-1"), ReleaseReason.TaskGone))
                    "the release names the vanished Task"
            }

            test "a drained source releases its harvester with TooEarly" {
                // Issue #48: anti-thrash must not pin a creep to a dry
                // rock. The Task stays pooled since ADR 0025, so the
                // release is the arrival gate's rather than TaskGone's, and
                // Inapplicable would make the transition log lie. No
                // projection here, so the walk prices at 0 the way ADR 0004
                // prices unplaced geometry — and the reason says so, beside
                // the wait it was compared against (#88); the same release
                // on real ground is pinned under "restock dispatch".
                let snapshot =
                    { bareRespawn with
                        Sources = [ drained "src-a" 120 ]
                        Controller = None
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let sticky = Map.ofList [ "w1", taskId (Harvest "src-a") ]
                let { Verdicts = verdicts } = decide snapshot sticky Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "w1",
                        taskId (Harvest "src-a"),
                        ReleaseReason.TooEarly(0, 120)
                    ))
                    "an arrival that covers no wait leaves the rock, exactly as ADR 0013 did"
            }

            test "a creep that fills up releases Harvest as Inapplicable and matches fresh" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let sticky = Map.ofList [ "w1", taskId (Harvest "src-a") ]
                let { Verdicts = verdicts } = decide snapshot sticky Set.empty None

                Expect.equal
                    verdicts
                    [
                        Verdict.Released("w1", taskId (Harvest "src-a"), ReleaseReason.Inapplicable)
                        Verdict.Matched("w1", taskId (Refill "spawn-1"), MatchFactor.Rank)
                    ]
                    "the handover carries both halves: why released, what won next"
            }

            test "a body that cannot do its remembered Task releases as Inapplicable" {
                // Part-based, not energy-state: the hauler has room to
                // harvest into but no Work part to harvest with.
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Controller = None
                        Creeps = [ creepWith "hauler" 0 50 [ Carry; Move ] ]
                    }

                let sticky = Map.ofList [ "hauler", taskId (Harvest "src-a") ]
                let { Verdicts = verdicts } = decide snapshot sticky Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "hauler",
                        taskId (Harvest "src-a"),
                        ReleaseReason.Inapplicable
                    ))
                    "the missing Work part releases the assignment as Inapplicable"
            }

            test "a walled-off Work Area releases with Unreachable" {
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 10; Y = 12 }, Wall
                        { X = 10; Y = 13 }, Plain
                        { X = 10; Y = 14 }, Plain
                        { X = 10; Y = 16 }, Wall
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 25 25 ]
                        Spatial =

                            spatial
                                [ "src-a", { X = 10; Y = 10 }; "ctrl-1", { X = 10; Y = 16 } ]
                                terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                })
                    }

                let sticky = Map.ofList [ "w1", taskId (Harvest "src-a") ]
                let { Verdicts = verdicts } = decide snapshot sticky Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("w1", taskId (Harvest "src-a"), ReleaseReason.Unreachable))
                    "no Seat can be reached: the release says so"
            }

            test "a remembered oversell releases with OverCapacity, the loser idles as NoneFree" {
                // One Seat at the source, two creeps remembered on it — an
                // oversell memory can carry across a redeploy. The
                // alphabetically first keeps; nothing else fits the loser.
                let corridor =
                    [ { X = 10; Y = 10 }, Wall ] @ [ for y in 11..14 -> { X = 10; Y = y }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] corridor
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "w1", { X = 10; Y = 12 }; "w2", { X = 10; Y = 13 } ]
                                })
                    }

                let sticky =
                    Map.ofList [ "w1", taskId (Harvest "src-a"); "w2", taskId (Harvest "src-a") ]

                let { Verdicts = verdicts } = decide snapshot sticky Set.empty None

                Expect.equal
                    verdicts
                    [
                        Verdict.Released("w2", taskId (Harvest "src-a"), ReleaseReason.OverCapacity)
                        Verdict.Kept("w1", taskId (Harvest "src-a"))
                        Verdict.Unassigned("w2", IdleReason.NoneFree)
                    ]
                    "the cap releases the oversell and explains the loser's idleness"
            }

            test "an empty pool idles a creep with NoTasks" {
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Unassigned("w1", IdleReason.NoTasks) ]
                    "the Planner generated nothing at all"
            }

            test "a full creep with only Harvest on offer idles as NoneApplicable" {
                let snapshot =
                    { bareRespawn with
                        Controller = None
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Unassigned("w1", IdleReason.NoneApplicable) ]
                    "no Task fit the creep's body or energy state"
            }

            test "an applicable Task with an unreachable Work Area idles as NoneReachable" {
                // The source's one Seat is walled off; nothing else exists.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 10; Y = 12 }, Wall
                        { X = 10; Y = 13 }, Plain
                        { X = 10; Y = 14 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Controller = None
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                })
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Unassigned("w1", IdleReason.NoneReachable) ]
                    "the Task fit and had room, but no path reaches its Work Area"
            }

            test "a dead creep's dropped assignment speaks no Verdict" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Controller = None
                        Creeps = []
                    }

                let sticky = Map.ofList [ "ghost", taskId (Harvest "src-a") ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide snapshot sticky Set.empty None

                Expect.isEmpty (Map.toList assignments) "the dead creep's assignment is dropped"
                Expect.isEmpty verdicts "Verdicts attribute to living creeps only"
            }
        ]

/// The tier fixture: a two-row plain corridor, y = 10..11, x = 9..21,
/// carrying one Refill target per tier — the spawn at (11,10), a tower at
/// (14,10) and the controller container at (18,10), inside the Work Area
/// of the controller standing at (20,10). Spawn, tower and controller
/// stand as obstacles; the second row keeps the corridor open past them.
let tierRoom =
    let corridor =
        [
            for x in 9..21 do
                for y in 10..11 -> { X = x; Y = y }, Plain
        ]

    { spatial [] corridor with
        Stores = Map.ofList [ "can-ctrl", 800 ]
    }
    |> withHome (fun layer ->
        { layer with
            Obstacles = Set.ofList [ { X = 11; Y = 10 }; { X = 14; Y = 10 }; { X = 20; Y = 10 } ]
        })
    |> withTargets
        [
            "spawn-1", { X = 11; Y = 10 }, Structure BuiltKind.Spawn
            "tower-1", { X = 14; Y = 10 }, Structure BuiltKind.Tower
            "can-ctrl", { X = 18; Y = 10 }, Structure BuiltKind.Container
            "ctrl-1", { X = 20; Y = 10 }, Controller
        ]

/// The tier colony with the given hunger: one loaded Carry-only body
/// standing on the buffer, so the deepest tier costs it nothing to reach
/// and every shallower one costs more. Whatever wins, wins against travel
/// cost, and only rank can do that.
let tierColony refillables =
    { bareRespawn with
        Sources = []
        Refillables = refillables
        Creeps = [ creepWith "h1" 100 0 [ Carry; Carry; Move ] ]
        Spatial =
            tierRoom
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = Map.ofList [ "h1", { X = 18; Y = 10 } ]
                })
    }

/// The surplus fixture: one loaded generalist and a hungry tower, in a
/// colony the projection places nothing in — unpriceable geometry never
/// counts against a Task (ADR 0004), so every candidate ties on travel
/// cost and load, and rank is the only thing left that can separate a
/// pair. Each caller adds exactly one rival, so the Verdict's factor is
/// evidence about that rival alone.
let surplusColony =
    { bareRespawn with
        Sources = []
        Controller = None
        Refillables = [ refillable "tower-1" 500 BuiltKind.Tower ]
        Creeps = [ worker "w1" 50 0 ]
    }

[<Tests>]
let rankTierTests =
    testList
        "rank tiers"
        [
            test "the tier order is one sequence: feeding, then surplus, then the buffer" {
                // The Refill target layering (ADR 0010, ADR 0012) read top to
                // bottom by one body, one step at a time: the spawn six steps
                // away outbids the tower three away, and the tower outbids the
                // buffer underfoot. The buffer loses the second step, which is
                // what puts it below the surplus tier rather than beside it —
                // a tie there would hand the win to the container it stands on.
                let hungrySpawn = refillable "spawn-1" 50 BuiltKind.Spawn
                let fullSpawn = refillable "spawn-1" 0 BuiltKind.Spawn
                let hungryTower = refillable "tower-1" 500 BuiltKind.Tower

                let feeding =
                    decide (tierColony [ hungrySpawn; hungryTower ]) Map.empty Set.empty None

                Expect.equal
                    feeding.Verdicts
                    [ Verdict.Matched("h1", taskId (Refill "spawn-1"), MatchFactor.Rank) ]
                    "the colony feeds its own reproduction first: rank decided"

                let surplus =
                    decide (tierColony [ fullSpawn; hungryTower ]) Map.empty Set.empty None

                Expect.equal
                    surplus.Verdicts
                    [ Verdict.Matched("h1", taskId (Refill "tower-1"), MatchFactor.Rank) ]
                    "reproduction fed, the guns outrank the buffer: rank decided"
            }

            test "tower Refill, Build, Repair and Upgrade are one surplus tier" {
                // Pairwise, because the deciding factor is read off the winner
                // and its cheapest rival alone: pool all four at once and the
                // three-way tie hides whichever one left the tier. So each
                // surplus Task meets the tower Refill by itself, and pool order
                // — not rank — has to be what breaks every one of those ties.
                // Build sits beside Upgrade, not above it.
                let verdictsFor colony =
                    (decide colony Map.empty Set.empty None).Verdicts

                let tied =
                    [ Verdict.Matched("w1", taskId (Refill "tower-1"), MatchFactor.PoolOrder) ]

                Expect.equal
                    (verdictsFor
                        { surplusColony with
                            ConstructionSites = [ { Id = "site-1" } ]
                        })
                    tied
                    "Build ties the tower Refill: pool order broke it, not rank"

                Expect.equal
                    (verdictsFor (surplusColony |> withHits "road-1" BuiltKind.Road 100 5000))
                    tied
                    "Repair ties the tower Refill: pool order broke it, not rank"

                Expect.equal
                    (verdictsFor
                        { surplusColony with
                            Controller = Some(controllerAt 1)
                        })
                    tied
                    "Upgrade ties the tower Refill: pool order broke it, not rank"
            }
        ]

[<Tests>]
let verboseScoringTests =
    testList
        "verbose scoring"
        [
            test "a verbose creep's Scoring covers the whole pool, scores and rejections both" {
                // Loaded and full: Harvest cannot fit the energy state, while
                // Refill and Upgrade score on the full key — no projection, so
                // every travel cost prices at 0.
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty (Set.ofList [ "w1" ]) None

                Expect.equal
                    verdicts
                    [
                        Verdict.Scoring(
                            "w1",
                            [
                                Candidate.Rejected(
                                    taskId (Harvest "src-a"),
                                    RejectReason.Inapplicable
                                )
                                Candidate.Scored(taskId (Refill "spawn-1"), 0, 0, 0)
                                Candidate.Scored(taskId (Upgrade "ctrl-1"), 2, 0, 0)
                            ]
                        )
                        Verdict.Matched("w1", taskId (Refill "spawn-1"), MatchFactor.Rank)
                    ]
                    "every pool Task appears once: scored on the key or rejected at its gate"
            }

            test "a full Task rejects as CapacityFull; only the listed creep gets a Scoring" {
                // One Seat at the source, claimed by w1's match before w2's
                // turn: w2's scoring shows the cap, and its upgrade row shows
                // the empty carry. w1 is off the list and speaks no Scoring.
                let corridor =
                    [ { X = 10; Y = 10 }, Wall ] @ [ for y in 11..14 -> { X = 10; Y = y }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] corridor
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "w1", { X = 10; Y = 12 }; "w2", { X = 10; Y = 13 } ]
                                })
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty (Set.ofList [ "w2" ]) None

                Expect.equal
                    verdicts
                    [
                        Verdict.Matched("w1", taskId (Harvest "src-a"), MatchFactor.OnlyCandidate)
                        Verdict.Scoring(
                            "w2",
                            [
                                Candidate.Rejected(
                                    taskId (Harvest "src-a"),
                                    RejectReason.CapacityFull
                                )
                                Candidate.Rejected(
                                    taskId (Upgrade "ctrl-1"),
                                    RejectReason.Inapplicable
                                )
                            ]
                        )
                        Verdict.Unassigned("w2", IdleReason.NoneFree)
                    ]
                    "the cap that idled w2 is named per Task; the unlisted creep stays terse"
            }

            test "a kept creep's own single-Seat Task scores as held, never capacity-full" {
                // The creep's own claim is set aside for its scoring: the
                // Task it holds must read as the winning row, not as
                // rejected against its holder's own seat.
                let corridor =
                    [ { X = 10; Y = 10 }, Wall ] @ [ for y in 11..14 -> { X = 10; Y = y }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] corridor
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 11 } ]
                                })
                    }

                let sticky = Map.ofList [ "w1", taskId (Harvest "src-a") ]
                let { Verdicts = verdicts } = decide snapshot sticky (Set.ofList [ "w1" ]) None

                Expect.equal
                    verdicts
                    [
                        Verdict.Scoring(
                            "w1",
                            [
                                Candidate.Scored(taskId (Harvest "src-a"), 0, 0, 0)
                                Candidate.Rejected(
                                    taskId (Upgrade "ctrl-1"),
                                    RejectReason.Inapplicable
                                )
                            ]
                        )
                        Verdict.Kept("w1", taskId (Harvest "src-a"))
                    ]
                    "the held Task is the scoring's winning row"
            }

            test "a walled-off Work Area rejects as Unreachable" {
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 10; Y = 12 }, Wall
                        { X = 10; Y = 13 }, Plain
                        { X = 10; Y = 14 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Controller = None
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                })
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty (Set.ofList [ "w1" ]) None

                Expect.equal
                    verdicts
                    [
                        Verdict.Scoring(
                            "w1",
                            [
                                Candidate.Rejected(
                                    taskId (Harvest "src-a"),
                                    RejectReason.Unreachable
                                )
                            ]
                        )
                        Verdict.Unassigned("w1", IdleReason.NoneReachable)
                    ]
                    "the scoring pinpoints the gate the idle reason summarises"
            }
        ]

[<Tests>]
let tests =
    testList
        "decide"
        [
            test "an empty creep is matched to a Harvest task and remembered" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decide snapshot Map.empty Set.empty None

                Expect.contains intents (HarvestSource("w1", "src-a")) "empty creep goes harvesting"

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "assignment is remembered"
            }

            test "bare respawn yields exactly one spawn Intent" {
                let { Intents = intents } = decide bareRespawn Map.empty Set.empty None

                match spawnIntents intents with
                | [ (spawnName, body, creepName) ] ->
                    Expect.equal spawnName "Spawn1" "spawns from the only spawn"
                    Expect.isNonEmpty body "body must not be empty"
                    Expect.isNotEmpty creepName "creep needs a name"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "spawn Intent body is affordable at bare-respawn energy" {
                let { Intents = intents } = decide bareRespawn Map.empty Set.empty None

                for (_, body, _) in spawnIntents intents do
                    Expect.isLessThanOrEqual
                        (bodyCost body)
                        300
                        "body cost within bare-respawn energy"
            }

            test "no spawn Intent when energy is below a worker body cost" {
                let snapshot =
                    { bareRespawn with
                        RoomEnergy = bank 100 300
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (spawnIntents intents) "cannot afford a worker"
            }

            test "no spawn Intent while the spawn is already spawning" {
                let snapshot =
                    { bareRespawn with
                        Spawns = [ { spawn with IsSpawning = true } ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (spawnIntents intents) "spawn is busy"
            }

            // Three Seats around src-a: a target of three, so one worker
            // leaves a deficit of two — enough demand for both spawns.
            let threeSeats =
                spatial
                    [ "src-a", { X = 10; Y = 10 } ]
                    [
                        { X = 9; Y = 10 }, Plain
                        { X = 11; Y = 10 }, Plain
                        { X = 10; Y = 9 }, Plain
                    ]

            test "two idle spawns in one room spend the shared bank once" {
                let snapshot =
                    { bareRespawn with
                        Spawns =
                            [
                                spawn
                                { spawn with
                                    Name = "Spawn2"
                                    Id = "spawn-2"
                                }
                            ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = threeSeats
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (spawnName, _, _) ] ->
                    Expect.equal spawnName "Spawn1" "the first spawn in list order takes the budget"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "spawns in different rooms each draw their own bank" {
                let snapshot =
                    { bareRespawn with
                        Spawns =
                            [
                                spawn
                                { spawn with
                                    Name = "Spawn2"
                                    Id = "spawn-2"
                                    RoomName = "W2N2"
                                }
                            ]
                        RoomEnergy =
                            bank 300 300 |> Map.add "W2N2" { Available = 300; Capacity = 300 }
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = threeSeats
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (spawnIntents intents |> List.map (fun (name, _, _) -> name))
                    [ "Spawn1"; "Spawn2" ]
                    "full banks in separate rooms fund one body each"
            }

            test "with zero creeps one bank funds two minimal bodies at once" {
                let snapshot =
                    { bareRespawn with
                        Spawns =
                            [
                                spawn
                                { spawn with
                                    Name = "Spawn2"
                                    Id = "spawn-2"
                                }
                            ]
                        RoomEnergy = bank 550 550
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (spawnIntents intents |> List.map (fun (name, body, _) -> name, body))
                    [ "Spawn1", [ Work; Carry; Move ]; "Spawn2", [ Work; Carry; Move ] ]
                    "the fallback debits the bank per body instead of waiting on the engine"
            }

            test "at 550 capacity the whole capacity is spent" {
                let snapshot =
                    { bareRespawn with
                        RoomEnergy = bank 550 550
                        Creeps = [ worker "worker-1" 0 50 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (_, body, _) ] ->
                    Expect.equal
                        body
                        [ Work; Work; Carry; Carry; Carry; Carry; Move; Move; Move ]
                        "two units plus the 150 remainder as Carry/Carry/Move"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "at 300 capacity the remainder pads the single unit" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "worker-1" 0 50 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (_, body, _) ] ->
                    Expect.equal
                        body
                        [ Work; Carry; Carry; Move; Move ]
                        "one unit plus the 100 remainder as a Carry/Move pair"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "below minimum workforce, spawning waits for full capacity" {
                let snapshot =
                    { bareRespawn with
                        RoomEnergy = bank 400 550
                        Creeps = [ worker "worker-1" 0 50 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.isEmpty
                    (spawnIntents intents)
                    "a living workforce can bank up to a bigger body"
            }

            test "with zero creeps a minimal body is spawned from available energy" {
                let snapshot =
                    { bareRespawn with
                        RoomEnergy = bank 250 550
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (_, body, _) ] ->
                    Expect.equal
                        body
                        [ Work; Carry; Move ]
                        "an empty colony cannot wait for extensions it cannot refill"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "with zero creeps and unaffordable minimal body, no spawn Intent" {
                let snapshot =
                    { bareRespawn with
                        RoomEnergy = bank 150 550
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (spawnIntents intents) "even the fallback needs its unit cost"
            }

            test "one worker is below minimum: a second is spawned" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "worker-1" 0 50 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.hasLength (spawnIntents intents) 1 "a lone worker cannot keep the loop going"
            }

            test "no spawn Intent when workforce is at minimum" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "worker-1" 0 50; worker "worker-2" 0 50 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (spawnIntents intents) "workforce already at minimum"
            }

            test "empty creeps spread across sources instead of piling on one" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50 ]
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None
                let assigned = assignments |> Map.toList |> List.map snd |> List.sort

                Expect.equal
                    assigned
                    [ (taskId (Harvest "src-a")); (taskId (Harvest "src-b")) ]
                    "greedy matching balances load per task"
            }

            test "greedy matching counts kept assignments as load" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 20 30; worker "w2" 0 50 ]
                    }

                let { Assignments = assignments } =
                    decide snapshot (Map.ofList [ "w1", (taskId (Harvest "src-a")) ]) Set.empty None

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "w1 keeps its source"

                Expect.equal
                    (Map.tryFind "w2" assignments)
                    (Some(taskId (Harvest "src-b")))
                    "w2 avoids the occupied source"
            }

            test "assignments pass through unchanged when no creeps died" {
                let assignments = Map.ofList [ "worker-1", (taskId (Harvest "src-a")) ]

                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "worker-1" 0 50 ]
                    }

                let { Assignments = kept } = decide snapshot assignments Set.empty None
                Expect.equal kept assignments "assignments survive the tick"
            }

            test "an assignment sticks across ticks even when greedy would rebalance" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 20 30 ]
                    }

                let assignments = Map.ofList [ "w1", (taskId (Harvest "src-b")) ]

                let {
                        Intents = intents
                        Assignments = kept
                    } =
                    decide snapshot assignments Set.empty None

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Harvest "src-b")))
                    "no thrash: creep stays on its source"

                Expect.contains
                    intents
                    (HarvestSource("w1", "src-b"))
                    "intent follows the sticky assignment"
            }

            test "a creep that fills up is reassigned from Harvest to Refill" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let {
                        Intents = intents
                        Assignments = kept
                    } =
                    decide snapshot (Map.ofList [ "w1", (taskId (Harvest "src-a")) ]) Set.empty None

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Refill "spawn-1")))
                    "full creep switches to delivering"

                Expect.contains
                    intents
                    (TransferEnergyToStructure("w1", "spawn-1"))
                    "delivery intent emitted"
            }

            test "a loaded creep feeds a hungry tower once spawn and extensions are full" {
                // Full feeders leave the pool, so the tower Refill is the one
                // delivery on offer — the same transfer to the creep (ADR 0010).
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Refillables =
                            [
                                refillable "spawn-1" 0 BuiltKind.Spawn
                                refillable "ext-1" 0 BuiltKind.Extension
                                refillable "tower-1" 500 BuiltKind.Tower
                            ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let {
                        Intents = intents
                        Assignments = kept
                    } =
                    decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Refill "tower-1")))
                    "the tower is the delivery that remains"

                Expect.contains
                    intents
                    (TransferEnergyToStructure("w1", "tower-1"))
                    "the same transfer intent feeds a tower"
            }

            test "a creep that empties is reassigned from Refill back to Harvest" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Assignments = kept } =
                    decide
                        snapshot
                        (Map.ofList [ "w1", (taskId (Refill "spawn-1")) ])
                        Set.empty
                        None

                match Map.tryFind "w1" kept with
                | Some tid ->
                    Expect.contains
                        [ taskId (Harvest "src-a"); taskId (Harvest "src-b") ]
                        tid
                        "empty creep goes back to a source"
                | None -> failtest "creep should be reassigned, not idle"
            }

            test "surplus: a full creep with a full spawn switches to upgrading" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let {
                        Intents = intents
                        Assignments = kept
                    } =
                    decide snapshot (Map.ofList [ "w1", (taskId (Harvest "src-a")) ]) Set.empty None

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "nothing to refill, so surplus goes to the controller"

                Expect.contains intents (UpgradeController("w1", "ctrl-1")) "upgrade intent emitted"
            }

            test "a hungry structure beats the controller for a delivering creep" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let { Assignments = kept } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Refill "spawn-1")))
                    "refill outranks upgrade while a structure is missing energy"
            }

            test "an upgrading creep that empties goes back to harvest" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Assignments = kept } =
                    decide
                        snapshot
                        (Map.ofList [ "w1", (taskId (Upgrade "ctrl-1")) ])
                        Set.empty
                        None

                match Map.tryFind "w1" kept with
                | Some tid ->
                    Expect.contains
                        [ taskId (Harvest "src-a"); taskId (Harvest "src-b") ]
                        tid
                        "spent creep returns to a source"
                | None -> failtest "creep should be reassigned, not idle"
            }

            test
                "a full creep with a full spawn and no controller is left unassigned with no intent" {
                let snapshot =
                    { bareRespawn with
                        Controller = None
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let {
                        Intents = intents
                        Assignments = kept
                    } =
                    decide snapshot (Map.ofList [ "w1", (taskId (Harvest "src-a")) ]) Set.empty None

                Expect.isEmpty (Map.toList kept) "no applicable task"

                let creepIntents =
                    intents
                    |> List.filter (function
                        | SpawnCreep _ -> false
                        | _ -> true)

                Expect.isEmpty creepIntents "idle creep emits nothing"
            }

            test "a full creep with a construction site and a full spawn goes building" {
                let snapshot =
                    { bareRespawn with
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let {
                        Intents = intents
                        Assignments = kept
                    } =
                    decide snapshot (Map.ofList [ "w1", (taskId (Harvest "src-a")) ]) Set.empty None

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Build "site-1")))
                    "surplus energy goes into construction"

                Expect.contains intents (BuildSite("w1", "site-1")) "build intent emitted"
            }

            test "an empty creep is never matched to a Build task" {
                let snapshot =
                    { bareRespawn with
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Assignments = kept } =
                    decide snapshot (Map.ofList [ "w1", (taskId (Build "site-1")) ]) Set.empty None

                match Map.tryFind "w1" kept with
                | Some tid ->
                    Expect.contains
                        [ taskId (Harvest "src-a"); taskId (Harvest "src-b") ]
                        tid
                        "empty creep goes harvesting instead"
                | None -> failtest "creep should be reassigned, not idle"
            }

            test "a hungry structure beats a construction site for a delivering creep" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let { Assignments = kept } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Refill "spawn-1")))
                    "refill outranks build while a structure is missing energy"
            }

            test "assignments of dead creeps are dropped" {
                let assignments = Map.ofList [ "ghost", "task-a" ]
                let { Assignments = kept } = decide bareRespawn assignments Set.empty None
                Expect.isEmpty (Map.toList kept) "dead creep's assignment is released"
            }
        ]

let activations intents =
    intents
    |> List.choose (function
        | ActivateSafeMode id -> Some id
        | _ -> None)

/// A hostile creep of the given body standing on the given tile. Its
/// owner and its room are both immaterial to the reflexes — the Raid log
/// is the only reader of either (ADR 0028, ADR 0041) — and the room is
/// spelled the empty string, the name `SpatialInfo.homeName` gives a
/// projection that names none, which is what the colonies assigning
/// `Hostiles` directly here are built on (`bareRespawn`, `spatial`,
/// `towerColony`). A colony that *does* name its room gets that name
/// stamped on by `facing`, so no fixture files a hostile in a room its
/// own projection has no layer for.
let hostileAt id pos body : HostileInfo =
    {
        Id = id
        Owner = "raider"
        RoomName = ""
        Pos = pos
        Body = body
    }

/// A hostile creep of the given body, position immaterial.
let hostile body = hostileAt "h-1" { X = 25; Y = 25 } body

/// The same colony with the given hostiles standing in its room — in
/// **its** room, which is why the name is stamped on here rather than left
/// to `hostileAt` (ADR 0041). The colonies this composes onto are built on
/// `openRoom`, which names its projection "W1N1"; a hostile carrying the
/// empty name would be filed in a room those projections hold no layer
/// for, so every reader that joins a hostile to the geometry around it —
/// the Raid log's closest approach today, the reflexes' Reach at #117 —
/// would measure it against nothing.
let facing hostiles (snapshot: Snapshot) =
    { snapshot with
        Hostiles =
            hostiles
            |> List.map (fun (h: HostileInfo) ->
                { h with
                    RoomName = SpatialInfo.homeName snapshot.Spatial
                })
    }

/// The same colony reading its safe-mode gates off the given controller.
let governedBy controller (snapshot: Snapshot) =
    { snapshot with
        Controller = Some controller
    }

/// A colony whose whole Keep stands at full hits (ADR 0034).
let wholeKeep =
    bareRespawn
    |> withHits "spawn-1" BuiltKind.Spawn 5000 5000
    |> withHits "tower-1" BuiltKind.Tower 5000 5000
    |> withHits "sto-1" BuiltKind.Storage 5000 5000

/// The same Keep one hit off max on the spawn — all the second arm reads:
/// the Keep does not decay, so below max means it was damaged.
let dentedSpawn = wholeKeep |> withHits "spawn-1" BuiltKind.Spawn 4999 5000

[<Tests>]
let safeModeTests =
    testList
        "safe-mode reflex"
        [
            test "an unplaced controller falls back to firing on sight" {
                // No controller tile in the projection means no deadline to
                // measure — the reflex keeps the old conservative answer.
                let snapshot =
                    { bareRespawn with
                        Hostiles = [ hostile [ Claim; Claim; Move; Move ] ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.equal (activations intents) [ "ctrl-1" ] "safe mode fires immediately"
            }

            test "a claimer beyond reach holds the stock — the towers get their window" {
                // attackController is range 1 and judged from tick-start
                // position, so a claimer 4 tiles out cannot tap for at
                // least 3 more ticks; holding costs nothing (ADR 0015).
                let snapshot =
                    { bareRespawn with
                        Spatial = spatial [ "ctrl-1", { X = 25; Y = 25 } ] []
                        Hostiles = [ hostileAt "h-1" { X = 25; Y = 29 } [ Claim; Move ] ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (activations intents) "range 4: the tap cannot land yet"
            }

            test "a claimer at the reach deadline fires safe mode" {
                // Range 3 = the precise deadline (2) plus one tile of margin
                // for a skipped tick (ADR 0015).
                let snapshot =
                    { bareRespawn with
                        Spatial = spatial [ "ctrl-1", { X = 25; Y = 25 } ] []
                        Hostiles = [ hostileAt "h-1" { X = 28; Y = 25 } [ Claim; Move ] ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.equal (activations intents) [ "ctrl-1" ] "the deadline is now"
            }

            test "a hostile without CLAIM parts does not spend the activation" {
                let snapshot =
                    { bareRespawn with
                        Hostiles = [ hostile [ Tough; Attack; RangedAttack; Heal; Move ] ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.isEmpty
                    (activations intents)
                    "fighters cannot touch the controller; the stock is kept"
            }

            test "an empty stock fires nothing" {
                let snapshot =
                    { bareRespawn with
                        Controller =
                            Some
                                { controllerAt 1 with
                                    SafeModeAvailable = 0
                                }
                        Hostiles = [ hostile [ Claim; Move ] ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (activations intents) "nothing to activate with"
            }

            test "safe mode already running is not re-fired" {
                let snapshot =
                    { bareRespawn with
                        Controller =
                            Some
                                { controllerAt 1 with
                                    SafeModeActive = true
                                }
                        Hostiles = [ hostile [ Claim; Move ] ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (activations intents) "the room is already protected"
            }

            test "a dented Keep with a hostile in the room fires" {
                // The second arm (ADR 0034), and it stands on its own: this
                // hostile carries no CLAIM, so the first arm holds its peace
                // and only the damage speaks. Each of the three in turn —
                // the tower is the dismantler test below.
                let fires snapshot =
                    let { Intents = intents } =
                        decide
                            (snapshot |> facing [ hostile [ Tough; Attack; Move ] ])
                            Map.empty
                            Set.empty
                            None

                    activations intents

                Expect.equal
                    (fires dentedSpawn)
                    [ "ctrl-1" ]
                    "the spawn is losing hits with someone here"

                Expect.equal
                    (fires (wholeKeep |> withHits "sto-1" BuiltKind.Storage 4999 5000))
                    [ "ctrl-1" ]
                    "the Storage is the room's largest store, and it is of the Keep"
            }

            test "a dented Keep in an empty room holds the stock" {
                // Damage alone is not a raid: the window between a raid
                // leaving and a worker patching the Keep spends nothing.
                let { Intents = intents } = decide dentedSpawn Map.empty Set.empty None
                Expect.isEmpty (activations intents) "nobody is here to be held off"
            }

            test "a full Keep with a hostile in the room fires nothing" {
                let snapshot = wholeKeep |> facing [ hostile [ Attack; Attack; Move ] ]

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (activations intents) "an intact Keep is not yet certain harm"
            }

            test "a WORK-only hostile in a room with a dented tower fires" {
                // A dismantler hurts a structure without ever qualifying as a
                // Threat — the arm reads any hostile for exactly this case.
                let snapshot =
                    wholeKeep
                    |> withHits "tower-1" BuiltKind.Tower 4999 5000
                    |> facing [ hostile [ Work; Work; Move ] ]

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.equal (activations intents) [ "ctrl-1" ] "a dismantler is doing the harm"
            }

            test "a hungry Post container and a hungry rampart are not the Keep" {
                // Invaders chew containers as a matter of routine, and the
                // stock is not for that (ADR 0034). The rampart matters more:
                // one sits below its floor for most of its life — it decays
                // there — so a Keep arm that read every hungry structure
                // would spend the stock on the first hostile to wander past.
                let snapshot =
                    wholeKeep
                    |> withHits "cont-1" BuiltKind.Container 1 125_000
                    |> withHits "ram-1" BuiltKind.Rampart 50_000 3_000_000
                    |> facing [ hostile [ Work; Move ] ]

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.isEmpty
                    (activations intents)
                    "hits off the Keep's list never spend the stock"
            }

            test "the gates hold over a dented Keep under attack" {
                // The second arm is gated exactly as the first is: there is
                // one pair, not a pair each.
                let dismantled = dentedSpawn |> facing [ hostile [ Work; Move ] ]

                let empty =
                    dismantled
                    |> governedBy
                        { controllerAt 1 with
                            SafeModeAvailable = 0
                        }

                let running =
                    dismantled
                    |> governedBy
                        { controllerAt 1 with
                            SafeModeActive = true
                        }

                let { Intents = onEmpty } = decide empty Map.empty Set.empty None
                let { Intents = onRunning } = decide running Map.empty Set.empty None

                Expect.isEmpty (activations onEmpty) "an empty stock has nothing to spend"
                Expect.isEmpty (activations onRunning) "already protected, whichever arm asks"
            }

            test "a quiet room fires nothing" {
                let { Intents = intents } = decide bareRespawn Map.empty Set.empty None
                Expect.isEmpty (activations intents) "no hostiles, no reflex"
            }
        ]

let shots intents =
    intents
    |> List.choose (function
        | FireTower(tower, target) -> Some(tower, target)
        | _ -> None)

/// A colony whose towers stand on the given tiles, facing the given hostiles.
let towerColony towers hostiles =
    { bareRespawn with
        Hostiles = hostiles
        Spatial =
            { spatial towers [] with
                TargetKinds =
                    towers |> List.map (fun (id, _) -> id, Structure BuiltKind.Tower) |> Map.ofList
            }
    }

[<Tests>]
let fireReflexTests =
    testList
        "fire reflex"
        [
            test "a tower shoots the hostile in the room" {
                let snapshot =
                    towerColony
                        [ "tower-1", { X = 10; Y = 40 } ]
                        [ hostileAt "h-1" { X = 20; Y = 20 } [ Attack; Move ] ]

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.equal (shots intents) [ "tower-1", "h-1" ] "any hostile is fired on"
            }

            test "the nearest hostile is shot — damage decays with range" {
                let snapshot =
                    towerColony
                        [ "tower-1", { X = 10; Y = 40 } ]
                        [
                            hostileAt "h-far" { X = 40; Y = 10 } [ Attack; Move ]
                            hostileAt "h-near" { X = 12; Y = 38 } [ Attack; Move ]
                        ]

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.equal (shots intents) [ "tower-1", "h-near" ] "never waste a decayed shot"
            }

            test "equidistant hostiles tie-break by id — the pick is deterministic" {
                let snapshot =
                    towerColony
                        [ "tower-1", { X = 10; Y = 40 } ]
                        [
                            hostileAt "h-b" { X = 15; Y = 40 } [ Attack; Move ]
                            hostileAt "h-a" { X = 10; Y = 45 } [ Attack; Move ]
                        ]

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.equal (shots intents) [ "tower-1", "h-a" ] "same range: lowest id wins"
            }

            test "each tower picks its own nearest — no focus fire" {
                let snapshot =
                    towerColony
                        [ "tower-1", { X = 10; Y = 40 }; "tower-2", { X = 40; Y = 10 } ]
                        [
                            hostileAt "h-a" { X = 12; Y = 38 } [ Attack; Move ]
                            hostileAt "h-b" { X = 38; Y = 12 } [ Attack; Move ]
                        ]

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (shots intents |> List.sort)
                    [ "tower-1", "h-a"; "tower-2", "h-b" ]
                    "the rule is per-tower, stated once"
            }

            test "a quiet room fires no shot" {
                let snapshot = towerColony [ "tower-1", { X = 10; Y = 40 } ] []
                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (shots intents) "no hostile, no reflex"
            }

            test "a towerless room shoots nothing and keeps the safe-mode rule intact" {
                // A clawless hostile before RCL3: no tower to answer with,
                // and the safe-mode stock stays banked (ADR 0007).
                let snapshot =
                    { bareRespawn with
                        Hostiles = [ hostileAt "h-1" { X = 20; Y = 20 } [ Attack; Move ] ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (shots intents) "no tower, no shot"
                Expect.isEmpty (activations intents) "fighters never spend the stock"
            }
        ]

let pickups intents =
    intents
    |> List.choose (function
        | PickupEnergy(creep, pile) -> Some(creep, pile)
        | _ -> None)

/// A colony around a dropped energy pile at (10, 10) on open ground, with
/// the given creeps standing on the given tiles.
let pileColony creeps positions =
    { bareRespawn with
        Sources = []
        Creeps = creeps
        Spatial =
            { spatial
                  [ "pile-1", { X = 10; Y = 10 } ]
                  [
                      for x in 8..12 do
                          for y in 8..12 -> { X = x; Y = y }, Plain
                  ] with
                TargetKinds = Map.ofList [ "pile-1", Dropped ]
            }
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = Map.ofList positions
                })
    }

/// The same colony with a second room's layer beside its own (ADR 0041):
/// that room's ground, the piles it names, and the creeps standing on its
/// tiles — and its coordinates deliberately collide with `pileColony`'s.
/// A `Pos` carries no room, so a reflex that unioned the two rooms' piles
/// or the two rooms' creeps would pair across the border at range 0 and
/// emit a pickup the engine answers ERR_NOT_IN_RANGE (#166). The creeps
/// still enter `Creeps`, which is the colony's fleet and no room's.
let private withPileRoom room piles positions (colony: Snapshot) =
    { colony with
        Spatial =
            { colony.Spatial with
                Rooms =
                    Map.add
                        room
                        { RoomLayer.empty with
                            Terrain =
                                Map.ofList
                                    [
                                        for x in 8..12 do
                                            for y in 8..12 -> { X = x; Y = y }, Plain
                                    ]
                            TargetPositions = Map.ofList piles
                            CreepPositions = Map.ofList positions
                        }
                        colony.Spatial.Rooms
                TargetKinds =
                    (colony.Spatial.TargetKinds, piles)
                    ||> List.fold (fun kinds (id, _) -> Map.add id Dropped kinds)
            }
    }

[<Tests>]
let pickupReflexTests =
    testList
        "pickup reflex"
        [
            test "an adjacent creep with free capacity picks up" {
                let snapshot = pileColony [ worker "w1" 0 50 ] [ "w1", { X = 10; Y = 11 } ]
                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.equal (pickups intents) [ "w1", "pile-1" ] "in reach and hungry: pick up"
            }

            test "a creep standing on the pile picks up" {
                let snapshot = pileColony [ worker "w1" 0 50 ] [ "w1", { X = 10; Y = 10 } ]
                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.equal (pickups intents) [ "w1", "pile-1" ] "range 0 is within reach"
            }

            test "a full creep leaves the pile alone" {
                let snapshot = pileColony [ worker "w1" 50 0 ] [ "w1", { X = 10; Y = 11 } ]
                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (pickups intents) "no free capacity, nothing to gain"
            }

            test "a pile out of reach draws nobody — the reflex never moves a creep" {
                let snapshot = pileColony [ worker "w1" 0 50 ] [ "w1", { X = 10; Y = 13 } ]
                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (pickups intents) "range 3: recapture only what is in reach"
            }

            test "every adjacent creep picks — the engine settles duplicates" {
                let snapshot =
                    pileColony
                        [ worker "w1" 0 50; worker "w2" 0 50 ]
                        [ "w1", { X = 10; Y = 11 }; "w2", { X = 9; Y = 10 } ]

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (pickups intents |> List.sort)
                    [ "w1", "pile-1"; "w2", "pile-1" ]
                    "zero coordination: both reach, both ask"
            }

            test "the pickup rides beside the task's own action" {
                // The creep sits on a Seat of src-a with the pile also in
                // reach: pickup conflicts with no other action, so both
                // Intents are emitted for the same tick.
                let snapshot =
                    { pileColony [ worker "w1" 0 50 ] [ "w1", { X = 10; Y = 11 } ] with
                        Sources = [ source "src-a" ]
                    }

                let withSource =
                    { snapshot with
                        Spatial =
                            { snapshot.Spatial with
                                TargetKinds = Map.add "src-a" Source snapshot.Spatial.TargetKinds
                            }
                            |> withHome (fun layer ->
                                { layer with
                                    Terrain = Map.add { X = 11; Y = 11 } Wall layer.Terrain
                                    TargetPositions =
                                        Map.add "src-a" { X = 11; Y = 11 } layer.TargetPositions
                                })
                    }

                let { Intents = intents } = decide withSource Map.empty Set.empty None

                Expect.equal (pickups intents) [ "w1", "pile-1" ] "the reflex fires"

                Expect.contains
                    intents
                    (HarvestSource("w1", "src-a"))
                    "the assigned task's action still goes out"
            }

            test "a pile keeps no construction site off its tile" {
                // Layout determinism (ADR 0011): a transient pile must not
                // perturb the ordering, so placement with and without the
                // pile is identical.
                let bare = atLevel 2 (openRoom 3)

                let strewn =
                    atLevel 2 (openRoom 3 |> withTargets [ "pile-1", { X = 24; Y = 24 }, Dropped ])

                let placedWith = decide strewn Map.empty Set.empty None
                let placedWithout = decide bare Map.empty Set.empty None

                Expect.equal
                    (placedTiles placedWith.Intents)
                    (placedTiles placedWithout.Intents)
                    "the Layout does not see piles"
            }

            test "an outpost creep picks up the pile at its own feet" {
                // The live gap (#166): an outpost's Anchor stands on its
                // container, overflows onto the tile it stands on, and the
                // pile is at range 0 for the hauler that comes for the
                // container — 3,000 energy decaying on the ground at
                // t140,810 because both sides of the pairing answered home.
                // The home pile shares the coordinate and stays untouched.
                let snapshot =
                    pileColony [ worker "w-out" 0 50 ] []
                    |> withPileRoom
                        "W1N2"
                        [ "pile-out", { X = 10; Y = 10 } ]
                        [ "w-out", { X = 10; Y = 10 } ]

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (pickups intents)
                    [ "w-out", "pile-out" ]
                    "its own room's pile, and only that one"
            }

            test "a pile at home draws no creep standing in the outpost" {
                // The pairing never crosses a border (ADR 0041): the pile
                // and the creep are bare `Pos`es on one coordinate of two
                // rooms, which is range 0 to `range` and out of the world
                // to the engine.
                let snapshot =
                    pileColony [ worker "w-out" 0 50 ] []
                    |> withPileRoom "W1N2" [] [ "w-out", { X = 10; Y = 10 } ]

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (pickups intents) "same coordinate, different room, no reach"
            }

            test "two rooms each pair their own pile with their own creep" {
                let snapshot =
                    pileColony
                        [ worker "w1" 0 50; worker "w-out" 0 50 ]
                        [ "w1", { X = 10; Y = 11 } ]
                    |> withPileRoom
                        "W1N2"
                        [ "pile-out", { X = 10; Y = 10 } ]
                        [ "w-out", { X = 9; Y = 10 } ]

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (pickups intents |> List.sort)
                    [ "w-out", "pile-out"; "w1", "pile-1" ]
                    "one Intent a room, each creep on the pile of the room it stands in"
            }

            test "a creep the projection places nowhere reaches no pile" {
                // ADR 0004's absence, unchanged by the pairing going per
                // room: a creep in the fleet and in no layer is in no
                // group, so it is measured against nothing rather than
                // against every room's piles at once.
                let snapshot =
                    { pileColony [ worker "w1" 0 50 ] [ "w1", { X = 10; Y = 11 } ] with
                        Creeps = [ worker "w1" 0 50; worker "ghost" 0 50 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.equal (pickups intents) [ "w1", "pile-1" ] "the unplaced creep picks nothing"
            }
        ]

[<Tests>]
let downgradeDeadlineTests =
    testList
        "downgrade deadline"
        [
            test "a controller near downgrade outranks refill for a loaded creep" {
                // A downgrade zeroes the safe-mode stock, so the timer is a
                // hard deadline, not surplus-rank work.
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                        Controller =
                            Some
                                { controllerAt 1 with
                                    TicksToDowngrade = 4000
                                }
                    }

                let { Assignments = kept } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "the deadline escalates Upgrade above the feeding tier"
            }

            test "the deadline scales with level: RCL4 at 15,000 is already urgent" {
                // The engine refuses activateSafeMode below half the level's
                // full timer minus 5,000 — at RCL4 that is 15,000. Escalating
                // at half (20,000) keeps the reflex's activation legal with
                // the whole 5,000-tick grace intact.
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                        Controller =
                            Some
                                { controllerAt 4 with
                                    TicksToDowngrade = 15000
                                }
                    }

                let { Assignments = kept } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "a flat deadline would sleep through RCL4's refusal threshold"
            }

            test "RCL4 above half its timer is not urgent" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                        Controller =
                            Some
                                { controllerAt 4 with
                                    TicksToDowngrade = 25000
                                }
                    }

                let { Assignments = kept } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Refill "spawn-1")))
                    "above half the timer, upgrade stays surplus work"
            }

            test "far from the deadline upgrade stays surplus work" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let { Assignments = kept } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Refill "spawn-1")))
                    "a fresh timer changes nothing"
            }
        ]

/// A room with one Dual Seat: source at (10,10), controller at (13,10).
/// The Seat (11,10) sits at range 2 of the controller — inside its Upgrade
/// Work Area — while (9,10) sits at range 4, an ordinary Seat.
let dualSeatRoom =
    { spatial
          [ "src-a", { X = 10; Y = 10 }; "ctrl-1", { X = 13; Y = 10 } ]
          [ { X = 9; Y = 10 }, Plain; { X = 11; Y = 10 }, Plain ] with
        TargetKinds = Map.ofList [ "src-a", Source; "ctrl-1", Controller ]
    }

/// An Anchor-bodied creep: four Work, one Carry, one Move.
let anchor name energy freeCapacity =
    creepWith name energy freeCapacity [ Work; Work; Work; Work; Carry; Move ]

/// The Dual Seat room, one source, controller in place — the base Anchor scenario.
let dualSeatColony =
    { bareRespawn with
        Sources = [ source "src-a" ]
        Controller = Some(controllerAt 2)
        Spatial = dualSeatRoom
    }

let moveIntentsFor name intents =
    intents
    |> List.filter (function
        | MoveCreep(creep, _) -> creep = name
        | _ -> false)

[<Tests>]
let anchorTests =
    testList
        "anchor"
        [
            test "an Anchor on a plain Seat walks to the Post instead of digging there" {
                // The W12S28 bug (ADR 0020): controller far from every Seat,
                // a built container on the Seat at (9,10) — the source's one
                // Post — and the Anchor standing on the plain Seat (10,11).
                // Harvesting there would fill its single Carry in four ticks
                // and hand it to Upgrade, five tiles away; instead it holds
                // its dig and walks.
                let snapshot =
                    { dualSeatColony with
                        Creeps = [ anchor "a1" 0 50 ]
                        Spatial =
                            { spatial
                                  [
                                      "src-a", { X = 10; Y = 10 }
                                      "ctrl-1", { X = 40; Y = 40 }
                                      "cont-1", { X = 9; Y = 10 }
                                  ]
                                  (openSeats { X = 10; Y = 10 }) with
                                TargetKinds =
                                    Map.ofList
                                        [
                                            "src-a", Source
                                            "ctrl-1", Controller
                                            "cont-1", Structure BuiltKind.Container
                                        ]
                            }
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "a1", { X = 10; Y = 11 } ]
                                })
                    }

                let {
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decide snapshot Map.empty Set.empty None

                Expect.isEmpty
                    (actionIntents intents
                     |> List.filter (function
                         | HarvestSource _ -> true
                         | _ -> false))
                    "off the Post a heavy body does not dig, so it never fills"

                Expect.equal
                    (moveIntentsFor "a1" intents)
                    [ MoveCreep("a1", TopLeft) ]
                    "it steps toward the container Seat"

                Expect.contains
                    verdicts
                    (Verdict.Matched("a1", taskId (Harvest "src-a"), MatchFactor.OnlyCandidate))
                    "and stays matched to Harvest the whole walk"
            }

            test "an Anchor that is already full off-Post commutes once, then settles" {
                // The deployment path ADR 0020 names: a full store off the
                // Post still catches no overflow, so Harvest is inapplicable
                // and the body spends its load at the controller one last
                // time. Nothing pins it until it is empty again — pinned
                // here so the one-time heal stays a decision, not a
                // surprise.
                let room =
                    { spatial
                          [
                              "src-a", { X = 10; Y = 10 }
                              "ctrl-1", { X = 14; Y = 11 }
                              "cont-1", { X = 9; Y = 10 }
                          ]
                          (openSeats { X = 10; Y = 10 }
                           @ [ for x in 11..14 -> { X = x; Y = 11 }, Plain ]) with
                        TargetKinds =
                            Map.ofList
                                [
                                    "src-a", Source
                                    "ctrl-1", Controller
                                    "cont-1", Structure BuiltKind.Container
                                ]
                    }
                    |> withHome (fun layer ->
                        { layer with
                            CreepPositions = Map.ofList [ "a1", { X = 10; Y = 11 } ]
                        })

                let full =
                    { dualSeatColony with
                        Creeps = [ anchor "a1" 50 0 ]
                        Spatial = room
                    }

                let { Verdicts = verdicts } = decide full Map.empty Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Matched("a1", taskId (Upgrade "ctrl-1"), MatchFactor.OnlyCandidate))
                    "a full store off the Post catches no overflow: only Upgrade is left"

                // Emptied at the controller, the same body is pulled home.
                let emptied =
                    { full with
                        Creeps = [ anchor "a1" 0 50 ]
                    }

                let { Intents = intents } = decide emptied Map.empty Set.empty None

                Expect.equal
                    (moveIntentsFor "a1" intents)
                    [ MoveCreep("a1", TopLeft) ]
                    "empty again, it walks to the Post and stays there"
            }

            test "a Dual Seat and banked capacity plan an Anchor body" {
                let snapshot =
                    { dualSeatColony with
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (_, body, creepName) ] ->
                    Expect.equal
                        body
                        [ Work; Work; Carry; Move ]
                        "the Anchor row sized to the 300 bank"

                    Expect.stringStarts creepName "anchor-" "the name carries the anchor row"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "without a Dual Seat only generalists are planned" {
                // Same Seats, controller placed far away: no Seat falls in
                // its Upgrade Work Area, so there is no Dual Seat to cast for.
                let snapshot =
                    { dualSeatColony with
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =
                            dualSeatRoom
                            |> withHome (fun layer ->
                                { layer with
                                    TargetPositions =
                                        Map.ofList
                                            [
                                                "src-a", { X = 10; Y = 10 }
                                                "ctrl-1", { X = 40; Y = 40 }
                                            ]
                                })
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (_, body, creepName) ] ->
                    Expect.equal body (workerBodyFor 300) "the worker row sized to the bank"
                    Expect.stringStarts creepName "worker-" "the name carries the worker row"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "a source-container Post with no Dual Seat casts an Anchor at full bank" {
                // The W12S28 shape: controller far from every Seat, but a
                // built container stands on the Seat at (9,10) — a Post,
                // so the Anchor row comes alive without any Dual Seat.
                let snapshot =
                    { dualSeatColony with
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =
                            { dualSeatRoom with
                                TargetKinds =
                                    dualSeatRoom.TargetKinds
                                    |> Map.add "cont-1" (Structure BuiltKind.Container)
                            }
                            |> withHome (fun layer ->
                                { layer with
                                    TargetPositions =
                                        Map.ofList
                                            [
                                                "src-a", { X = 10; Y = 10 }
                                                "ctrl-1", { X = 40; Y = 40 }
                                                "cont-1", { X = 9; Y = 10 }
                                            ]
                                })
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (_, body, creepName) ] ->
                    Expect.equal
                        body
                        [ Work; Work; Carry; Move ]
                        "the Anchor row sized to the 300 bank"

                    Expect.stringStarts creepName "anchor-" "the container Seat is a Post"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "a capped Anchor is cast the tick its bank holds the body's cost, not a full bank" {
                // ADR 0021: at RCL4 the bank caps at 1,300 but the Anchor
                // row prices at 700 (6W1C1M); waiting for a full bank would
                // hold every Anchor replacement past RCL3 for nothing.
                let snapshot =
                    { dualSeatColony with
                        RoomEnergy = bank 700 1300
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (_, body, creepName) ] ->
                    Expect.equal
                        body
                        [ Work; Work; Work; Work; Work; Work; Carry; Move ]
                        "the capped Anchor row, priced at exactly the bank's holding"

                    Expect.stringStarts creepName "anchor-" "the name carries the anchor row"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "a bank short of the capped Anchor's cost still waits" {
                let snapshot =
                    { dualSeatColony with
                        RoomEnergy = bank 650 1300
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.isEmpty (spawnIntents intents) "650 does not buy 6W1C1M"
            }

            test "the Anchor quota counts Posts: a Dual Seat plus a container Seat want two" {
                // One living Anchor covers the Dual Seat; the built
                // container on the other Seat is a second Post, so the
                // remaining gap is cast from the anchor row, not generalist.
                let snapshot =
                    { dualSeatColony with
                        Creeps = [ anchor "a1" 0 50 ]
                        Spatial =
                            { dualSeatRoom with
                                TargetKinds =
                                    dualSeatRoom.TargetKinds
                                    |> Map.add "cont-1" (Structure BuiltKind.Container)
                            }
                            |> withHome (fun layer ->
                                { layer with
                                    TargetPositions =
                                        layer.TargetPositions |> Map.add "cont-1" { X = 9; Y = 10 }
                                })
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (_, _, creepName) ] ->
                    Expect.stringStarts creepName "anchor-" "the second Post's gap is an Anchor gap"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "a living Anchor fills the quota: the remaining gap goes generalist" {
                let snapshot =
                    { dualSeatColony with
                        Creeps = [ anchor "a1" 0 50 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (_, _, creepName) ] ->
                    Expect.stringStarts creepName "worker-" "the one Dual Seat is already worked"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            // Three Seats — (11,10) the Dual Seat, (9,10) and (9,9) ordinary —
            // and a second idle spawn drawing from the same bank.
            let threeSeatRoom =
                dualSeatRoom
                |> withHome (fun layer ->
                    { layer with
                        Terrain =
                            Map.ofList
                                [
                                    { X = 9; Y = 10 }, Plain
                                    { X = 11; Y = 10 }, Plain
                                    { X = 9; Y = 9 }, Plain
                                ]
                    })

            let secondSpawn =
                { spawn with
                    Name = "Spawn2"
                    Id = "spawn-2"
                }

            test "the Anchor gap is filled before generalist gaps" {
                let snapshot =
                    { dualSeatColony with
                        Spawns = [ spawn; secondSpawn ]
                        RoomEnergy = bank 600 300
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = threeSeatRoom
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (_, firstBody, firstName); (_, _, secondName) ] ->
                    Expect.stringStarts firstName "anchor-" "the Anchor gap is filled first"
                    Expect.equal firstBody [ Work; Work; Carry; Move ] "the Anchor row's body"
                    Expect.stringStarts secondName "worker-" "the generalist fills the remainder"
                | other -> failtest $"expected exactly two SpawnCreep intents, got %A{other}"
            }

            test "planned creeps never exceed the workforce target" {
                // One Post and ten income workers make a target of eleven
                // (ADR 0012, the worker row rounded up by ADR 0037: the
                // Post's 15,000 of lifetime income less the Anchor's 300
                // of amortization over 1 × 1500 is 9.8); ten living leave
                // one gap — the second idle spawn must stay quiet even
                // with energy banked for it.
                let snapshot =
                    { dualSeatColony with
                        Spawns = [ spawn; secondSpawn ]
                        RoomEnergy = bank 600 300
                        Creeps = anchor "a1" 0 50 :: [ for i in 1..9 -> worker $"w{i}" 0 50 ]
                        Spatial = threeSeatRoom
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.hasLength
                    (spawnIntents intents)
                    1
                    "the Anchor quota lives inside the target, never on top of it"
            }

            test "an empty Anchor on its Dual Seat is assigned Harvest without moving" {
                let snapshot =
                    { dualSeatColony with
                        Creeps = [ anchor "a1" 0 50 ]
                        Spatial =
                            dualSeatRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "a1", { X = 11; Y = 10 } ]
                                })
                    }

                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "an empty store calls for Harvest"

                Expect.contains intents (HarvestSource("a1", "src-a")) "the action fires in place"
                Expect.isEmpty (moveIntentsFor "a1" intents) "no movement step is emitted"
            }

            test "a full Anchor on its Dual Seat is assigned Upgrade without moving" {
                let snapshot =
                    { dualSeatColony with
                        Creeps = [ anchor "a1" 50 0 ]
                        Spatial =
                            dualSeatRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "a1", { X = 11; Y = 10 } ]
                                })
                    }

                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "a full store calls for Upgrade"

                Expect.contains
                    intents
                    (UpgradeController("a1", "ctrl-1"))
                    "the action fires in place"

                Expect.isEmpty (moveIntentsFor "a1" intents) "no movement step is emitted"
            }

            test
                "alternation is emergent: a filled-up Anchor's Harvest releases and rematches to Upgrade" {
                let snapshot =
                    { dualSeatColony with
                        Creeps = [ anchor "a1" 50 0 ]
                        Spatial =
                            dualSeatRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "a1", { X = 11; Y = 10 } ]
                                })
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]
                let { Assignments = assignments } = decide snapshot remembered Set.empty None

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "ordinary applicability release + rematch flips the assignment"
            }

            // The Dual Seat room extended east: a plain corridor from
            // (12,10) to (30,10) carrying distant mobile work at its end.
            let corridorEast extraTargets =
                dualSeatRoom
                |> withHome (fun layer ->
                    { layer with
                        TargetPositions =
                            (Map.toList layer.TargetPositions @ extraTargets) |> Map.ofList
                        Terrain =
                            (Map.toList layer.Terrain
                             @ [ for x in 12..30 -> { X = x; Y = 10 }, Plain ])
                            |> Map.ofList
                    })

            test "a distant Build flows to the generalist; the Anchor upgrades in place" {
                let snapshot =
                    { dualSeatColony with
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ anchor "a1" 50 0; worker "g1" 50 0 ]
                        Spatial =
                            corridorEast [ "site-1", { X = 31; Y = 10 } ]
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "a1", { X = 11; Y = 10 }; "g1", { X = 29; Y = 10 } ]
                                })
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "g1" assignments)
                    (Some(taskId (Build "site-1")))
                    "the mobile body takes the distant site"

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "the slow heavy body stays where it is valuable"
            }

            test "a distant Refill flows to the generalist; the empty Anchor harvests" {
                let snapshot =
                    { dualSeatColony with
                        Refillables = [ refillable "spawn-1" 300 BuiltKind.Spawn ]
                        Creeps = [ anchor "a1" 0 50; worker "g1" 50 0 ]
                        Spatial =
                            corridorEast [ "spawn-1", { X = 31; Y = 10 } ]
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "a1", { X = 11; Y = 10 }; "g1", { X = 30; Y = 10 } ]
                                })
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "g1" assignments)
                    (Some(taskId (Refill "spawn-1")))
                    "the loaded mobile body delivers"

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "the empty Anchor works its Seat instead"
            }

            test "the disaster fallback still spawns bare worker units beside a Dual Seat" {
                let snapshot = { dualSeatColony with Creeps = [] }
                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | (_, body, creepName) :: _ ->
                    Expect.equal
                        body
                        [ Work; Carry; Move ]
                        "time-to-first-creep outranks specialisation"

                    Expect.stringStarts creepName "worker-" "the fallback casts the worker row"
                | [] -> failtest "expected the fallback to spawn"
            }
        ]

/// The haul fixture (ADR 0012): a plain corridor y = 10, x = 9..21; the
/// source embedded in wall at (10,10) with Seats (9,10) and (11,10), the
/// controller standing at (20,10); the source container "can-src" on the
/// Seat (11,10) and the controller container "can-ctrl" at (18,10),
/// inside the Upgrade Work Area. The buffer starts stocked, the source
/// container empty.
let haulRoom =
    { spatial [] [ for x in 9..21 -> { X = x; Y = 10 }, (if x = 10 then Wall else Plain) ] with
        Stores = Map.ofList [ "can-src", 0; "can-ctrl", 800 ]
    }
    |> withHome (fun layer ->
        { layer with
            Obstacles = Set.singleton { X = 20; Y = 10 }
        })
    |> withTargets
        [
            "src-a", { X = 10; Y = 10 }, Source
            "ctrl-1", { X = 20; Y = 10 }, Controller
            "can-src", { X = 11; Y = 10 }, Structure BuiltKind.Container
            "can-ctrl", { X = 18; Y = 10 }, Structure BuiltKind.Container
        ]

let haulColony =
    { bareRespawn with
        Sources = [ source "src-a" ]
        Spatial = haulRoom
    }

let withdrawTasks tasks =
    tasks
    |> List.choose (function
        | Withdraw storeId -> Some storeId
        | _ -> None)

let refillTasks tasks =
    tasks
    |> List.choose (function
        | Refill structureId -> Some structureId
        | _ -> None)

[<Tests>]
let logisticsTests =
    testList
        "logistics"
        [
            test "a stocked container yields a Withdraw Task; an empty one yields none" {
                Expect.equal
                    (withdrawTasks (planTasks haulColony noThreats))
                    [ "can-ctrl" ]
                    "the stocked buffer enters the pool; the empty source container does not"
            }

            test
                "the controller container with room is a Refill target; source containers never are" {
                let snapshot =
                    { haulColony with
                        Spatial =
                            { haulRoom with
                                Stores = Map.ofList [ "can-src", 500; "can-ctrl", 800 ]
                            }
                    }

                let tasks = planTasks snapshot noThreats

                Expect.equal
                    (refillTasks tasks)
                    [ "can-ctrl" ]
                    "only the buffer is a Refill target, however stocked the source container"

                Expect.equal
                    (withdrawTasks tasks)
                    [ "can-ctrl"; "can-src" ]
                    "both stocked containers stay Withdraw Tasks"
            }

            test "a full controller container is no Refill target, but stays a Withdraw" {
                let snapshot =
                    { haulColony with
                        Spatial =
                            { haulRoom with
                                Stores = Map.ofList [ "can-ctrl", 2000 ]
                            }
                    }

                let tasks = planTasks snapshot noThreats
                Expect.isEmpty (refillTasks tasks) "no room left to refill"
                Expect.equal (withdrawTasks tasks) [ "can-ctrl" ] "still stocked to draw from"
            }

            test "an empty creep between source and stocked container is matched by travel cost" {
                // At (15,10) the buffer's Work Area is two steps away, the
                // nearest Seat four: collect beats dig. At (12,10) the Seat
                // is one step away: dig beats collect. Same rule both ways.
                let colonyAt pos =
                    { haulColony with
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", pos ]
                                })
                    }

                let near = decide (colonyAt { X = 15; Y = 10 }) Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" near.Assignments)
                    (Some(taskId (Withdraw "can-ctrl")))
                    "the cheaper-to-reach buffer wins the feeding-tier tie"

                Expect.contains
                    near.Verdicts
                    (Verdict.Matched("w1", taskId (Withdraw "can-ctrl"), MatchFactor.TravelCost))
                    "the match speaks its Verdict: travel cost decided"

                let far = decide (colonyAt { X = 12; Y = 10 }) Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" far.Assignments)
                    (Some(taskId (Harvest "src-a")))
                    "nearer the source, digging wins the same tie"
            }

            test "a heavy-Work body never collects: the far Post's Harvest beats the near buffer" {
                // Same geometry where the worker above picks Withdraw — at
                // (15,10) the buffer is two steps, the nearest Seat four.
                // Work > Move makes Withdraw inapplicable (ADR 0016), so
                // the anchor's only feeding-tier candidate is Harvest and
                // the unmanned Post wins regardless of distance.
                let snapshot =
                    { haulColony with
                        Creeps = [ anchor "a1" 0 50 ]
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "a1", { X = 15; Y = 10 } ]
                                })
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "the stocked buffer never outbids the Post for a Work-heavy body"
            }

            test "a kept Withdraw on a heavy-Work body releases as inapplicable and digs" {
                // Deployment heals the live colony without a death: the
                // remembered orbit breaks the first tick the gate lands.
                let snapshot =
                    { haulColony with
                        Creeps = [ anchor "a1" 0 50 ]
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "a1", { X = 15; Y = 10 } ]
                                })
                    }

                let remembered = Map.ofList [ "a1", taskId (Withdraw "can-ctrl") ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide snapshot remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "a1",
                        taskId (Withdraw "can-ctrl"),
                        ReleaseReason.Inapplicable
                    ))
                    "the gate releases the remembered collection"

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "the rematch walks the anchor home"
            }

            test "alternation is emergent: a filled-up creep's Withdraw releases and rematches" {
                // The creep filled up inside the buffer's Work Area — which
                // is also the controller's. Withdraw loses applicability;
                // the rematch sinks the load into Upgrade, never back into
                // the container it just drew from.
                let snapshot =
                    { haulColony with
                        Creeps = [ worker "w1" 50 0 ]
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 17; Y = 10 } ]
                                })
                    }

                let remembered = Map.ofList [ "w1", taskId (Withdraw "can-ctrl") ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide snapshot remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "w1",
                        taskId (Withdraw "can-ctrl"),
                        ReleaseReason.Inapplicable
                    ))
                    "the full store releases Withdraw"

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "the rematch flips to Upgrade, like the Anchor's harvest↔upgrade"
            }

            test "the alternation's other half: an emptied creep's Upgrade releases into Withdraw" {
                let snapshot =
                    { haulColony with
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 17; Y = 10 } ]
                                })
                    }

                let remembered = Map.ofList [ "w1", taskId (Upgrade "ctrl-1") ]
                let { Assignments = assignments } = decide snapshot remembered Set.empty None

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Withdraw "can-ctrl")))
                    "the empty store tops up from the buffer one tile away"
            }

            test "spawn-feeding Refill still outranks the buffer Refill" {
                // The spawn stands mid-corridor, two steps from the loaded
                // creep; the buffer's Work Area costs nothing at all. Rank
                // dominates: reproduction is fed before the buffer.
                let snapshot =
                    { haulColony with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    Obstacles =
                                        Set.ofList [ { X = 14; Y = 10 }; { X = 20; Y = 10 } ]
                                    CreepPositions = Map.ofList [ "w1", { X = 17; Y = 10 } ]
                                })
                            |> withTargets
                                [ "spawn-1", { X = 14; Y = 10 }, Structure BuiltKind.Spawn ]
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Refill "spawn-1")))
                    "the buffer never outbids feeding the spawn"
            }

            test "a loaded Carry-only body is the buffer's Refill worker" {
                // The buffer's tier sits below every surplus Task, so
                // Work-bodied creeps pass it by — but a full hauler-shaped
                // body has no surplus work of its own, and the outflow
                // lands on it.
                let snapshot =
                    { haulColony with
                        Creeps = [ creepWith "h1" 100 0 [ Carry; Carry; Move ] ]
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "h1", { X = 15; Y = 10 } ]
                                })
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Refill "can-ctrl")))
                    "the buffer Refill is live work for a body that can do nothing better"
            }

            test "a seated Withdraw emits the engine withdraw call and speaks 📥" {
                let snapshot =
                    { haulColony with
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 17; Y = 10 } ]
                                })
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.contains
                    intents
                    (WithdrawEnergyFromStructure("w1", "can-ctrl"))
                    "in range at tick start: the Executor-bound Intent fires"

                Expect.contains intents (SayCreep("w1", "📥")) "the Task's own chat bubble"
            }
        ]

/// The stock fixture: the tier corridor with the Storage standing at
/// (16,11), between the tower and the buffer and clear of the working
/// ground the clustered ordering excludes (ADR 0022) — the controller's
/// Upgrade Work Area reaches back only to x = 17. It blocks its own tile
/// the way the projection carries a built one, and nothing but the
/// projection's kind says which structure it is.
let stockRoom =
    tierRoom
    |> withHome (fun layer ->
        { layer with
            Obstacles = Set.add { X = 16; Y = 11 } layer.Obstacles
        })
    |> withTargets [ "sto-1", { X = 16; Y = 11 }, Structure BuiltKind.Storage ]

/// The stock colony with the given hunger and stores: one loaded
/// Carry-only body standing beside the Storage, so the deepest tier of all
/// costs it nothing to reach and every shallower one — the tower one step
/// west, the buffer one step east — costs more. Whatever outbids the
/// stock outbids it against travel cost, and only rank can do that. Each
/// caller leaves the stock exactly one rival, so the Verdict's factor is
/// evidence about that pair alone.
let stockColony refillables stores =
    { bareRespawn with
        Sources = []
        Refillables = refillables
        Creeps = [ creepWith "h1" 100 0 [ Carry; Carry; Move ] ]
        Spatial =
            { stockRoom with Stores = stores }
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = Map.ofList [ "h1", { X = 16; Y = 10 } ]
                })
    }

[<Tests>]
let stockTests =
    testList
        "storage stock"
        [
            test "a Storage with room is a Refill target; a full one is not" {
                // Judged from the projection's kind, as the buffer's tier is
                // (ADR 0023). The buffer is brimming in both colonies, so the
                // stock is the only thing the pool can be reporting on.
                let hungry = stockColony [] (Map.ofList [ "can-ctrl", 2000; "sto-1", 0 ])
                let full = stockColony [] (Map.ofList [ "can-ctrl", 2000; "sto-1", 1000000 ])

                Expect.equal
                    (refillTasks (planTasks hungry noThreats))
                    [ "sto-1" ]
                    "the stock with room pools the deepest Refill of all"

                Expect.isEmpty
                    (refillTasks (planTasks full noThreats))
                    "a full stock pools no Refill: there is nowhere left to put a load"
            }

            test "the upgrade buffer outbids the stock, however close the stock stands" {
                // The hauler stands beside the Storage and a step short of
                // the buffer's Work Area, so travel cost points at the stock
                // and only rank can overrule it: surplus reaches the colony's
                // stock once the upgrade buffer is full and not before (ADR
                // 0023). The tier above the buffer is already pinned by the
                // rank-tier tests, so this one step completes the sequence.
                let { Verdicts = verdicts } =
                    decide
                        (stockColony [] (Map.ofList [ "can-ctrl", 800; "sto-1", 0 ]))
                        Map.empty
                        Set.empty
                        None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("h1", taskId (Refill "can-ctrl"), MatchFactor.Rank) ]
                    "the buffer is filled before the stock: rank decided"
            }

            test "a hungry tower outbids the stock, however close the stock stands" {
                // The buffer is brimming, so the tower is the stock's one
                // rival and the factor is evidence about that pair alone.
                let { Verdicts = verdicts } =
                    decide
                        (stockColony
                            [ refillable "tower-1" 500 BuiltKind.Tower ]
                            (Map.ofList [ "can-ctrl", 2000; "sto-1", 0 ]))
                        Map.empty
                        Set.empty
                        None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("h1", taskId (Refill "tower-1"), MatchFactor.Rank) ]
                    "the guns are fed before the stock: rank decided"
            }

            test "with every other sink full the stock takes the load" {
                // Spawn and tower full, buffer brimming: the deepest tier of
                // all is the one live Refill, and it is served by the same
                // transfer Intent, the same bubble and the same Verdict
                // vocabulary as every other Refill (ADR 0023).
                let colony =
                    stockColony
                        [
                            refillable "spawn-1" 0 BuiltKind.Spawn
                            refillable "tower-1" 0 BuiltKind.Tower
                        ]
                        (Map.ofList [ "can-ctrl", 2000; "sto-1", 500 ])

                let {
                        Intents = intents
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide colony Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Refill "sto-1")))
                    "the load the colony has nowhere else to put sinks into the stock"

                Expect.contains
                    intents
                    (TransferEnergyToStructure("h1", "sto-1"))
                    "the ordinary transfer Intent serves the Storage"

                Expect.contains
                    intents
                    (SayCreep("h1", "🔋"))
                    "the ordinary battery bubble shows it"

                Expect.equal
                    verdicts
                    [ Verdict.Matched("h1", taskId (Refill "sto-1"), MatchFactor.OnlyCandidate) ]
                    "a stock deposit speaks the Verdicts every other Refill speaks"
            }
        ]

[<Tests>]
let stockGateTests =
    testList
        "storage draw gate"
        [
            test "with every other sink full the stock pools no Withdraw" {
                // The gate (ADR 0023): the stock is an intake only while the
                // pool holds a Refill that is not its own. Here the spawn is
                // full and the buffer brimming, so the stock's own Refill —
                // pooled, because the stock has room — is the only one there
                // is. Counting it would gate the Storage open against itself
                // forever, and a hauler beside it would cycle energy in and
                // out of one store.
                let tasks =
                    planTasks
                        (stockColony
                            [ refillable "spawn-1" 0 BuiltKind.Spawn ]
                            (Map.ofList [ "can-ctrl", 2000; "sto-1", 500 ]))
                        noThreats

                Expect.equal (refillTasks tasks) [ "sto-1" ] "the stock's own Refill is pooled"

                Expect.equal
                    (withdrawTasks tasks)
                    [ "can-ctrl" ]
                    "and it is not a sink that opens the stock's own Withdraw"
            }

            test "one hungry extension opens it: exactly one Storage Withdraw" {
                // The Planner reads the refillable census, so a hungry
                // extension anywhere in the colony is the sink the stock is
                // drawn for — one Withdraw for the one Storage, never one
                // per hungry sink.
                let tasks =
                    planTasks
                        (stockColony
                            [ refillable "ext-1" 50 BuiltKind.Extension ]
                            (Map.ofList [ "can-ctrl", 2000; "sto-1", 500 ]))
                        noThreats

                Expect.equal
                    (withdrawTasks tasks)
                    [ "can-ctrl"; "sto-1" ]
                    "the stocked buffer's intake, and one draw on the stock"
            }

            test "the upgrade buffer counts as a sink: the stock feeds it" {
                // Every refillable full and only the buffer with room, so the
                // buffer's Refill is the whole reason the stock opens —
                // stock flows to the upgrade buffer when the sources cannot
                // keep it full (ADR 0023).
                let tasks =
                    planTasks
                        (stockColony
                            [ refillable "spawn-1" 0 BuiltKind.Spawn ]
                            (Map.ofList [ "can-ctrl", 800; "sto-1", 500 ]))
                        noThreats

                Expect.equal
                    (refillTasks tasks)
                    [ "can-ctrl"; "sto-1" ]
                    "the buffer is the one sink other than the stock"

                Expect.equal
                    (withdrawTasks tasks)
                    [ "can-ctrl"; "sto-1" ]
                    "and it opens the draw on the stock"
            }

            test "an empty Storage pools no Withdraw, however hungry the colony" {
                // The stock half of ADR 0012's rule, unchanged: a store with
                // nothing in it is nobody's intake.
                let tasks =
                    planTasks
                        (stockColony
                            [ refillable "ext-1" 50 BuiltKind.Extension ]
                            (Map.ofList [ "can-ctrl", 800; "sto-1", 0 ]))
                        noThreats

                Expect.equal
                    (withdrawTasks tasks)
                    [ "can-ctrl" ]
                    "an open gate draws nothing out of an empty stock"
            }
        ]

/// The draw fixture: a two-row plain corridor, y = 10..11, x = 8..22, with
/// the source walled in at (8,10) and its container on the Seat at (9,10),
/// the Storage off the lane at (17,11), and the upgrade buffer at (21,10)
/// beside the controller at (22,10). The stock and the controller stand as
/// obstacles; the lane runs past both. A creep on the lane at (13,10)
/// stands three plain steps from either store's Work Area — (10,10) beside
/// the source container, (16,10) beside the stock — so travel cost ties
/// the two intakes and nothing but rank can separate them; a creep further
/// east stands inside the stock's Work Area and six steps from the
/// container's, so travel cost points the other way and only rank can
/// override it.
let drawRoom =
    let lane =
        [
            for x in 8..22 do
                for y in 10..11 -> { X = x; Y = y }, (if x = 8 && y = 10 then Wall else Plain)
        ]

    spatial [] lane
    |> withHome (fun layer ->
        { layer with
            Obstacles = Set.ofList [ { X = 17; Y = 11 }; { X = 22; Y = 10 } ]
        })
    |> withTargets
        [
            "src-a", { X = 8; Y = 10 }, Source
            "can-src", { X = 9; Y = 10 }, Structure BuiltKind.Container
            "sto-1", { X = 17; Y = 11 }, Structure BuiltKind.Storage
            "can-ctrl", { X = 21; Y = 10 }, Structure BuiltKind.Container
            "ctrl-1", { X = 22; Y = 10 }, Controller
        ]

/// The draw colony: the draw room with the given stores, one creep on the
/// tile the caller puts it on, and every refillable full — so whatever
/// opens the stock's Withdraw is something the test itself put there.
let drawColony stores (creep: CreepInfo) pos =
    { bareRespawn with
        Sources = [ source "src-a" ]
        Refillables = [ refillable "spawn-1" 0 BuiltKind.Spawn ]
        Creeps = [ creep ]
        Spatial =
            { drawRoom with Stores = stores }
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = Map.ofList [ creep.Name, pos ]
                })
    }

[<Tests>]
let stockDrawTests =
    testList
        "storage draw"
        [
            test "the source container outbids the stock, however near the stock stands" {
                // The tier (ADR 0023): the stock sits one tier below the
                // source containers, so an empty hauler empties the flow's
                // own containers first and draws on the stock only when
                // they are dry. The buffer's own hunger is what opened the
                // stock's Withdraw at all. Twice, because rank beating a
                // tie and rank beating a cheaper rival are two claims: from
                // the lane's middle it is three steps to either Work Area,
                // and from inside the stock's the stock costs nothing at
                // all while the container costs six — ADR 0023's own
                // motivating case, a stock that wins every travel-cost
                // contest and must still lose.
                let drawFrom pos =
                    decide
                        (drawColony
                            (Map.ofList [ "can-src", 500; "can-ctrl", 800; "sto-1", 500 ])
                            (creepWith "h1" 0 100 [ Carry; Carry; Move ])
                            pos)
                        Map.empty
                        Set.empty
                        None

                let equidistant = drawFrom { X = 13; Y = 10 }

                Expect.equal
                    equidistant.Verdicts
                    [ Verdict.Matched("h1", taskId (Withdraw "can-src"), MatchFactor.Rank) ]
                    "the flow is emptied before the stock: rank decided"

                let underfoot = drawFrom { X = 16; Y = 10 }

                Expect.equal
                    underfoot.Verdicts
                    [ Verdict.Matched("h1", taskId (Withdraw "can-src"), MatchFactor.Rank) ]
                    "and it is emptied first from the stock's own doorstep too"
            }

            test "topping up from the stock outbids surplus work" {
                // The tier's other neighbour: the stock is drawn on above
                // everything the colony merely spends energy on, so a
                // half-loaded creep fills up before it spends. The worker
                // stands inside the controller's Work Area and one step from
                // the stock's, so Upgrade is the cheapest rival of the three
                // and the Verdict's factor is evidence about that pair
                // alone.
                let colony =
                    { drawColony
                          (Map.ofList [ "can-src", 0; "can-ctrl", 0; "sto-1", 500 ])
                          (worker "w1" 50 50)
                          { X = 19; Y = 10 } with
                        Sources = []
                    }

                let { Verdicts = verdicts } = decide colony Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("w1", taskId (Withdraw "sto-1"), MatchFactor.Rank) ]
                    "a load worth carrying is worth completing first: rank decided"
            }

            test "the flow's own Refill outbids the stock's draw" {
                // The tier's shallow neighbour, and the price of ordering
                // the stock under the flow (ADR 0023): there is no rank
                // between a container's Withdraw and the spawn Refill it
                // feeds, so a stock one tier below the containers is a tier
                // below the spawn too. The hauler stands in the stock's own
                // Work Area with half a load and the hungry spawn is four
                // steps west — it carries what it has rather than topping
                // up first.
                let colony =
                    { stockColony
                          [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                          (Map.ofList [ "can-ctrl", 2000; "sto-1", 500 ]) with
                        Creeps = [ creepWith "h1" 50 50 [ Carry; Carry; Move ] ]
                    }

                let { Verdicts = verdicts } = decide colony Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("h1", taskId (Refill "spawn-1"), MatchFactor.Rank) ]
                    "the spawn is fed before the stock is drawn on: rank decided"
            }

            test "both halves of the cycle pool on one tick; the tier gap closes it" {
                // What the Planner's gate does not do (ADR 0023): with a
                // sink other than the stock still hungry, a stocked Storage
                // with room pools its Withdraw and its Refill on the same
                // tick, and a part-loaded hauler beside it is applicable to
                // both. What keeps it out of the in-and-out cycle there is
                // the tier gap — the draw at the stock's shallow end, the
                // Refill at the deepest end of all — so it tops up and
                // carries the load away instead of putting it back.
                let colony =
                    { stockColony
                          [ refillable "spawn-1" 0 BuiltKind.Spawn ]
                          (Map.ofList [ "can-ctrl", 800; "sto-1", 500 ]) with
                        Creeps = [ creepWith "h1" 50 50 [ Carry; Carry; Move ] ]
                    }

                let tasks = planTasks colony noThreats

                Expect.contains (withdrawTasks tasks) "sto-1" "the stock is an intake this tick"
                Expect.contains (refillTasks tasks) "sto-1" "and a sink on the very same tick"

                let { Verdicts = verdicts } = decide colony Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("h1", taskId (Withdraw "sto-1"), MatchFactor.Rank) ]
                    "the draw outranks the load's way back in: rank decided"
            }

            test "the containers dry, the hauler draws on the stock for the spawn" {
                // What the stock is for (ADR 0023): the sources cannot
                // feed the spawn, so the stock does. The hauler already
                // stands beside it, and the ordinary withdraw Intent and the
                // ordinary bubble serve the draw — no Intent of the stock's
                // own, no glyph of its own.
                let colony =
                    drawColony
                        (Map.ofList [ "can-src", 0; "can-ctrl", 2000; "sto-1", 500 ])
                        (creepWith "h1" 0 100 [ Carry; Carry; Move ])
                        { X = 16; Y = 10 }

                let {
                        Intents = intents
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide
                        { colony with
                            Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        }
                        Map.empty
                        Set.empty
                        None

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Withdraw "sto-1")))
                    "with nothing in the containers the stock is the intake"

                Expect.contains
                    intents
                    (WithdrawEnergyFromStructure("h1", "sto-1"))
                    "the ordinary withdraw Intent serves the Storage"

                Expect.contains intents (SayCreep("h1", "📥")) "the ordinary inbox bubble shows it"

                Expect.equal
                    verdicts
                    [ Verdict.Matched("h1", taskId (Withdraw "sto-1"), MatchFactor.OnlyCandidate) ]
                    "a stock draw speaks the Verdicts every other Withdraw speaks"
            }

            test "with only the buffer hungry, the stock flows to it and never back" {
                // The other half of the ADR 0019 question, with the stock
                // standing where the buffer stood: the hauler draws on the
                // stock because the buffer has room, and the tick it fills
                // up the buffer outranks the store it just emptied — so the
                // pair alternates instead of cycling, exactly as the source
                // containers and the buffer do.
                let stores = Map.ofList [ "can-src", 0; "can-ctrl", 800; "sto-1", 500 ]
                let beside = { X = 16; Y = 10 }

                let empty =
                    decide
                        (drawColony stores (creepWith "h1" 0 100 [ Carry; Carry; Move ]) beside)
                        Map.empty
                        Set.empty
                        None

                Expect.equal
                    (Map.tryFind "h1" empty.Assignments)
                    (Some(taskId (Withdraw "sto-1")))
                    "the buffer's own hunger is what opens the stock"

                let filled =
                    decide
                        (drawColony stores (creepWith "h1" 100 0 [ Carry; Carry; Move ]) beside)
                        (Map.ofList [ "h1", taskId (Withdraw "sto-1") ])
                        Set.empty
                        None

                Expect.contains
                    filled.Verdicts
                    (Verdict.Released("h1", taskId (Withdraw "sto-1"), ReleaseReason.Inapplicable))
                    "the full store ends the draw, as it ends every other one"

                Expect.contains
                    filled.Verdicts
                    (Verdict.Matched("h1", taskId (Refill "can-ctrl"), MatchFactor.Rank))
                    "and the load goes on to the buffer, not back into the stock"
            }

            test "beside a stock that is both its intake and its sink, a hauler idles" {
                // The ADR 0019 loop in the shape no body gate could cure —
                // the bodies that feed the spawn from the stock are the ones
                // with no Work part — and the gate that closes it: with
                // every other sink full the stock's Withdraw is not pooled
                // at all, so the hauler that would have emptied and refilled
                // one store tick after tick sits still instead. Idling is
                // the honest state; the stock holds energy the colony has
                // nowhere to put.
                let idleOn stores =
                    decide
                        (drawColony
                            stores
                            (creepWith "h1" 0 100 [ Carry; Carry; Move ])
                            { X = 16; Y = 10 })
                        Map.empty
                        Set.empty
                        None

                let withRoom = idleOn (Map.ofList [ "can-src", 0; "can-ctrl", 2000; "sto-1", 500 ])

                Expect.equal
                    (Map.tryFind "h1" withRoom.Assignments)
                    None
                    "a stock that is its own only sink offers no intake"

                Expect.contains
                    withRoom.Verdicts
                    (Verdict.Unassigned("h1", IdleReason.NoneApplicable))
                    "the idle Verdict is the one ADR 0019 left behind"

                let brimming =
                    idleOn (Map.ofList [ "can-src", 0; "can-ctrl", 2000; "sto-1", 1000000 ])

                Expect.equal
                    (Map.tryFind "h1" brimming.Assignments)
                    None
                    "a stock with no room left is no different: still nowhere to carry to"
            }

            test "a Work body draws on the same terms; a Work-heavy body never does" {
                // Nothing about the stock is body-specific (ADR 0023): the
                // ordinary Withdraw gate is the whole rule, so a worker
                // takes the stock exactly as a hauler does, and ADR 0016's
                // comparative gate keeps the Anchor row out of it. The
                // empty buffer is the sink that opens the draw, and holds
                // nothing either body could prefer to it.
                let stores = Map.ofList [ "can-src", 0; "can-ctrl", 0; "sto-1", 500 ]

                let colonyFor creep =
                    { drawColony stores creep { X = 16; Y = 10 } with
                        Sources = []
                    }

                let worked = decide (colonyFor (worker "w1" 0 50)) Map.empty Set.empty None

                Expect.equal
                    worked.Verdicts
                    [ Verdict.Matched("w1", taskId (Withdraw "sto-1"), MatchFactor.OnlyCandidate) ]
                    "a Work part is neither a bar to the stock nor a ticket to it"

                let heavy = decide (colonyFor (anchor "a1" 0 50)) Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "a1" heavy.Assignments)
                    None
                    "a Work-heavy body's intake is digging, whatever the stock holds"

                Expect.contains
                    heavy.Verdicts
                    (Verdict.Unassigned("a1", IdleReason.NoneApplicable))
                    "the idle Verdict names the body gate, as ADR 0016 left it"
            }

            test "the tick the last other sink fills, the holder is released task-gone" {
                // The ADR 0013 shape (ADR 0023): the Task exists while the
                // condition holds and is gone otherwise, so a hauler
                // mid-trip is released through the path every vanishing Task
                // already uses — the stock needs no release reason of its
                // own.
                let colonyWithBuffer buffer =
                    drawColony
                        (Map.ofList [ "can-src", 0; "can-ctrl", buffer; "sto-1", 500 ])
                        (creepWith "h1" 0 100 [ Carry; Carry; Move ])
                        { X = 13; Y = 10 }

                let remembered = Map.ofList [ "h1", taskId (Withdraw "sto-1") ]

                let hungry = decide (colonyWithBuffer 800) remembered Set.empty None

                Expect.contains
                    hungry.Verdicts
                    (Verdict.Kept("h1", taskId (Withdraw "sto-1")))
                    "while one sink still has room the trip stands"

                let filled = decide (colonyWithBuffer 2000) remembered Set.empty None

                Expect.contains
                    filled.Verdicts
                    (Verdict.Released("h1", taskId (Withdraw "sto-1"), ReleaseReason.TaskGone))
                    "the tick it fills, the walk it was on is over"
            }

            test "the accepted churn: a load the buffer will not take goes back to the stock" {
                // ADR 0023 accepts one load of this rather than remembering
                // where a load was drawn from. The hauler filled from the
                // stock while the buffer was hungry and the buffer filled
                // while it walked: its Refill is gone, the stock is the only
                // sink left, and the remainder goes back where it came from
                // rather than nowhere at all.
                let stores = Map.ofList [ "can-src", 0; "can-ctrl", 2000; "sto-1", 500 ]
                let loaded = creepWith "h1" 100 0 [ Carry; Carry; Move ]

                let arrived =
                    decide
                        (drawColony stores loaded { X = 20; Y = 10 })
                        (Map.ofList [ "h1", taskId (Refill "can-ctrl") ])
                        Set.empty
                        None

                Expect.contains
                    arrived.Verdicts
                    (Verdict.Released("h1", taskId (Refill "can-ctrl"), ReleaseReason.TaskGone))
                    "the buffer filled while the hauler walked to it"

                Expect.equal
                    (Map.tryFind "h1" arrived.Assignments)
                    (Some(taskId (Refill "sto-1")))
                    "the stock is the one sink left: the load turns around"

                let back =
                    decide
                        (drawColony stores loaded { X = 16; Y = 10 })
                        (Map.ofList [ "h1", taskId (Refill "sto-1") ])
                        Set.empty
                        None

                Expect.contains
                    back.Intents
                    (TransferEnergyToStructure("h1", "sto-1"))
                    "the ordinary transfer puts the remainder back: nothing is dropped"
            }
        ]

[<Tests>]
let containerPostTests =
    testList
        "container post garrison"
        [
            // The garrison rule (#47, ADR 0012): a full creep standing on a
            // built source container keeps Harvest — the engine drops the
            // overflow into the container underfoot, so the creep
            // effectively has capacity. Everywhere else the ordinary
            // full-store rule stands.
            test "a full Anchor on a built source container keeps its Harvest across ticks" {
                let snapshot =
                    { haulColony with
                        Creeps = [ anchor "a1" 50 0 ]
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "a1", { X = 11; Y = 10 } ]
                                })
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]

                let {
                        Intents = intents
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide snapshot remembered Set.empty None

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "the overflow falls into the container: the Post stays garrisoned"

                Expect.contains
                    verdicts
                    (Verdict.Kept("a1", taskId (Harvest "src-a")))
                    "kept, never released as Inapplicable"

                Expect.contains
                    intents
                    (HarvestSource("a1", "src-a"))
                    "the dig keeps firing past a full store"

                Expect.isEmpty (moveIntentsFor "a1" intents) "no drift off the Post"
            }

            test "a full Anchor on the container matches Harvest fresh, not just from memory" {
                // Both gates — the remembered-assignment release and the
                // fresh judge — must read the same widened rule, or the
                // Anchor would be matched and released in alternate ticks.
                let snapshot =
                    { haulColony with
                        Creeps = [ anchor "a1" 50 0 ]
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "a1", { X = 11; Y = 10 } ]
                                })
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "the feeding-tier dig at cost 0 wins the fresh match too"
            }

            test "a full worker on the container releases Harvest: the garrison is body-aware" {
                // The squat of #67 (ADR 0024): body-blind, this widening let
                // a light body that filled up on the Post keep Harvest for
                // the rest of its life — never Inapplicable, so anti-thrash
                // never let the tile go — while the Anchor cast for that
                // Post read `none-free`. Only a garrisoning body's overflow
                // keeps the dig past a full store.
                let snapshot =
                    { haulColony with
                        Creeps = [ worker "w1" 50 0 ]
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 11; Y = 10 } ]
                                })
                    }

                let remembered = Map.ofList [ "w1", taskId (Harvest "src-a") ]
                let { Verdicts = verdicts } = decide snapshot remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("w1", taskId (Harvest "src-a"), ReleaseReason.Inapplicable))
                    "a light body's full store ends its dig, container or no container"
            }

            test "a full Anchor on a bare Seat still releases Harvest" {
                // (9,10) is a Seat of src-a with no container: harvesting
                // past a full store there spills onto the ground.
                let snapshot =
                    { haulColony with
                        Creeps = [ anchor "a1" 50 0 ]
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "a1", { X = 9; Y = 10 } ]
                                })
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]
                let { Verdicts = verdicts } = decide snapshot remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("a1", taskId (Harvest "src-a"), ReleaseReason.Inapplicable))
                    "no container underfoot: the ordinary full-store rule stands"
            }

            test "a full creep beside the built container still releases Harvest" {
                // (12,10) touches the container at (11,10) but stands off
                // it: adjacency catches nothing — only the tile itself.
                let snapshot =
                    { haulColony with
                        Creeps = [ anchor "a1" 50 0 ]
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "a1", { X = 12; Y = 10 } ]
                                })
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]
                let { Verdicts = verdicts } = decide snapshot remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("a1", taskId (Harvest "src-a"), ReleaseReason.Inapplicable))
                    "the widening reads the creep's own tile, never a neighbour"
            }

            test "a container construction site catches no overflow: Harvest still releases" {
                let snapshot =
                    { haulColony with
                        Creeps = [ anchor "a1" 50 0 ]
                        Spatial =
                            { haulRoom with
                                TargetKinds =
                                    haulRoom.TargetKinds
                                    |> Map.add "can-src" (Site BuiltKind.Container)
                            }
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "a1", { X = 11; Y = 10 } ]
                                })
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]
                let { Verdicts = verdicts } = decide snapshot remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("a1", taskId (Harvest "src-a"), ReleaseReason.Inapplicable))
                    "a pending container is not yet a container"
            }

            test "a built container off the Seats widens nothing: Harvest still releases" {
                // The controller container's tile is no Seat of src-a — a
                // full creep standing on it is nowhere the overflow rule
                // helps, however built the container underfoot.
                let snapshot =
                    { haulColony with
                        Creeps = [ anchor "a1" 50 0 ]
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "a1", { X = 18; Y = 10 } ]
                                })
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]
                let { Verdicts = verdicts } = decide snapshot remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("a1", taskId (Harvest "src-a"), ReleaseReason.Inapplicable))
                    "only that source's own container Seat catches its overflow"
            }
        ]

[<Tests>]
let postCapacityTests =
    testList
        "post capacity"
        [
            // The over-admission half of #67 (ADR 0024): a Work-heavy body's
            // Harvest Work Area is that source's Posts (ADR 0020), so the
            // Seat count admits garrisons to standing room that does not
            // exist. `haulRoom`'s src-a has two Seats and one Post.
            test "a source's Posts cap its heavy harvesters, however many Seats it has" {
                let snapshot =
                    { haulColony with
                        Creeps = [ anchor "a1" 50 0; anchor "a2" 0 50 ]
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "a1", { X = 11; Y = 10 }; "a2", { X = 12; Y = 10 } ]
                                })
                    }

                let remembered =
                    Map.ofList [ "a1", taskId (Harvest "src-a"); "a2", taskId (Harvest "src-a") ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide snapshot remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("a2", taskId (Harvest "src-a"), ReleaseReason.OverCapacity))
                    "one Post seats one garrison: the second Anchor is released, not left to crowd it"

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "a1" ]
                    "the Post's holder keeps the dig"
            }

            test "a fresh heavy body is not matched to a source whose Post is taken" {
                // Both gates read the same cap, or the second Anchor would be
                // released and rematched in alternate ticks.
                let snapshot =
                    { haulColony with
                        Creeps = [ anchor "a1" 0 50; anchor "a2" 0 50 ]
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "a1", { X = 11; Y = 10 }; "a2", { X = 12; Y = 10 } ]
                                })
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "a1" ]
                    "the fresh match stops at the Post count too"
            }

            test "a light body still fills every Seat: the Post cap governs heavy bodies alone" {
                let snapshot =
                    { haulColony with
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50 ]
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "w1", { X = 11; Y = 10 }; "w2", { X = 9; Y = 10 } ]
                                })
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "w1"; "w2" ]
                    "a light body stands on any Seat, so the Seat count is its only cap"
            }

            test "a source with no Post caps heavy harvesters at its Seats" {
                // The pre-container fallback (ADR 0020): with nothing built,
                // a heavy body harvests from any Seat, so a Post cap of zero
                // would strand the colony instead of ordering it.
                let snapshot =
                    { haulColony with
                        Creeps = [ anchor "a1" 0 50; anchor "a2" 0 50 ]
                        Spatial =
                            { haulRoom with
                                TargetKinds =
                                    haulRoom.TargetKinds
                                    |> Map.add "can-src" (Site BuiltKind.Container)
                            }
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "a1", { X = 11; Y = 10 }; "a2", { X = 9; Y = 10 } ]
                                })
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "a1"; "a2" ]
                    "no Post derives no Post cap: both Seats are open"
            }
        ]

/// The restock dispatch corridor: a one-tile lane y = 10 from x = 9 to
/// x = 21 with the source embedded in wall at (10,10), so its Seats are
/// (9,10) and (11,10) and the lane east of the source is the only approach
/// to either. An empty worker-unit body pays a whole tick per plain step,
/// so a creep at (15,10) is four steps and a walk of four ticks from the
/// Seat it can reach (ADR 0029).
let restockRoom =
    spatial [] [ for x in 9..21 -> { X = x; Y = 10 }, (if x = 10 then Wall else Plain) ]
    |> withTargets [ "src-a", { X = 10; Y = 10 }, Source ]

/// The corridor with its one source the given number of ticks from its
/// restock, and one empty creep standing in the lane. The controller is
/// unplaced and its Upgrade is inapplicable to an empty body, so Harvest
/// is the only Task a creep in the lane can hold.
let restockAt name pos ticks =
    { bareRespawn with
        Sources = [ drained "src-a" ticks ]
        Creeps = [ worker name 0 50 ]
        Spatial =
            restockRoom
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = Map.ofList [ name, pos ]
                })
    }

[<Tests>]
let restockTests =
    testList
        "restock dispatch"
        [
            test "a drained source's Harvest is applicable the tick the walk covers the wait" {
                // ADR 0025: the Task is judged at arrival, not at this tick.
                // Four ticks of walking against four ticks of waiting — the
                // creep leaves now and reaches the Seat as the energy lands.
                let snapshot = restockAt "w1" { X = 15; Y = 10 } 4

                let {
                        Assignments = assignments
                        Intents = intents
                    } =
                    decide snapshot Map.empty Set.empty None

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "w1" ]
                    "the walk covers the wait: the dry rock is worth setting out for"

                Expect.isNonEmpty
                    (moveIntentsFor "w1" intents)
                    "dispatched, not idled: the window is spent on the road"
            }

            test "one tick short of covering the wait, the creep stays where it stands" {
                // Zero slack, and the rule is self-correcting: the wait
                // shrinks by one each tick while the walk stays put, so this
                // creep departs next tick and still arrives as the energy
                // does.
                let snapshot = restockAt "w1" { X = 15; Y = 10 } 5

                let {
                        Assignments = assignments
                        Intents = intents
                    } =
                    decide snapshot Map.empty Set.empty None

                Expect.equal (Map.tryFind "w1" assignments) None "four ticks do not cover five"

                Expect.isEmpty
                    (moveIntentsFor "w1" intents)
                    "nothing to walk toward yet: it holds its ground for a tick"
            }

            test "paving one tile of the approach does not shorten the walk" {
                // The floor is per step, not on the total (ADR 0029): a road
                // on (14,10) drops the four-step approach from 8 cost units
                // to 7 — travel cost still ranks a paved route ahead — while
                // the walk stays four ticks, because four tiles are four
                // tiles however they are surfaced. Halving the total would
                // have made it three and sat the creep out of a tick it
                // could have spent walking.
                let snapshot = restockAt "w1" { X = 15; Y = 10 } 4

                let snapshot =
                    { snapshot with
                        Spatial =
                            snapshot.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    Roads = Set.singleton { X = 14; Y = 10 }
                                })
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "w1" ]
                    "four tiles are four ticks, paved or not"
            }

            test "an unreachable drained source rejects as Unreachable, not as too early" {
                // The arrival gate stands behind the reachability gate:
                // geometry the creep cannot cross is reported as such,
                // whatever the source holds. The walk and the travel cost
                // reach the same tiles, so neither gate can shadow the
                // other's answer (ADR 0029).
                let snapshot = restockAt "w1" { X = 17; Y = 10 } 60

                let snapshot =
                    { snapshot with
                        Spatial =
                            snapshot.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    Obstacles = Set.singleton { X = 16; Y = 10 }
                                })
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty (Set.ofList [ "w1" ]) None

                Expect.equal
                    verdicts
                    [
                        Verdict.Scoring(
                            "w1",
                            [
                                Candidate.Rejected(
                                    taskId (Harvest "src-a"),
                                    RejectReason.Unreachable
                                )
                                Candidate.Rejected(
                                    taskId (Upgrade "ctrl-1"),
                                    RejectReason.Inapplicable
                                )
                            ]
                        )
                        Verdict.Unassigned("w1", IdleReason.NoneReachable)
                    ]
                    "the first gate it fails names the rejection, and the idle reason follows it"
            }

            test "a verbose Scoring names the wait: the drained Harvest is rejected TooEarly" {
                // The body and the energy state fit and the Seat is reachable
                // — only the arrival doesn't (ADR 0025), so the row carries
                // its own reason rather than lying as Inapplicable, and the
                // always-on Verdict beside it says the same. The reason is
                // not a bare word (#88): it carries the two numbers the gate
                // compared, four ticks of walk against sixty of wait, so the
                // operator reads the answer off the row instead of halving a
                // cost that no longer means ticks.
                let snapshot = restockAt "w1" { X = 15; Y = 10 } 60

                let { Verdicts = verdicts } = decide snapshot Map.empty (Set.ofList [ "w1" ]) None

                Expect.equal
                    verdicts
                    [
                        Verdict.Scoring(
                            "w1",
                            [
                                Candidate.Rejected(
                                    taskId (Harvest "src-a"),
                                    RejectReason.TooEarly(4, 60)
                                )
                                Candidate.Rejected(
                                    taskId (Upgrade "ctrl-1"),
                                    RejectReason.Inapplicable
                                )
                            ]
                        )
                        Verdict.Unassigned("w1", IdleReason.NoneInTime)
                    ]
                    "a dry source with no garrison shows up as a number of ticks, not a missing row"
            }

            test "the always-on Verdict names the wait even off the verbose list" {
                // ADR 0025, CONTEXT's Verdict entry: the transition log is
                // always on, and none-applicable there would claim the body
                // or the energy state was the problem. Neither is: the creep
                // is simply too far from a source that is not ready yet.
                let snapshot = restockAt "w1" { X = 15; Y = 10 } 60

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Unassigned("w1", IdleReason.NoneInTime) ]
                    "waiting on a restock, not rejected by its body"
            }

            test "a creep on the Seat beside a dry rock is released, walk or no walk" {
                // Issue #48's rule under ADR 0025's gate, on real geometry:
                // standing in the Work Area there is no walk left to cover
                // the wait with, so anti-thrash does not pin the creep to a
                // source that will not feed it for another sixty ticks.
                let snapshot = restockAt "w1" { X = 11; Y = 10 } 60
                let remembered = Map.ofList [ "w1", taskId (Harvest "src-a") ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide snapshot remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("w1", taskId (Harvest "src-a"), ReleaseReason.TooEarly(0, 60)))
                    "an arrival of now covers no wait at all"

                Expect.equal (Map.tryFind "w1" assignments) None "and it is free to work elsewhere"
            }

            test "a release mid-trip carries the same two numbers the rejection does" {
                // #88: a creep released on the road owes the same
                // explanation as one rejected at the gate, so both reasons
                // carry the pair the gate compared. Four tiles out with
                // sixty ticks to go, the release says four and sixty —
                // distinct numbers, neither of them the other, and neither
                // recoverable from a scored row that is not written for a
                // rejected candidate at all.
                let snapshot = restockAt "w1" { X = 15; Y = 10 } 60
                let remembered = Map.ofList [ "w1", taskId (Harvest "src-a") ]

                let { Verdicts = verdicts } = decide snapshot remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("w1", taskId (Harvest "src-a"), ReleaseReason.TooEarly(4, 60)))
                    "why the creep is not on its way is a walk and a wait, not a bare word"
            }

            test "a Work-heavy garrison on a source container keeps Harvest through the window" {
                // The one exemption, on ADR 0024's condition and no other:
                // that tile is the garrison's job whatever the store or the
                // source holds, so the container-Post wobble is gone.
                let snapshot =
                    { haulColony with
                        Sources = [ drained "src-a" 60 ]
                        Creeps = [ anchor "a1" 50 0 ]
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "a1", { X = 11; Y = 10 } ]
                                })
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]

                let {
                        Assignments = assignments
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decide snapshot remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Kept("a1", taskId (Harvest "src-a")))
                    "kept through the empty window, never released as too early"

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "a1" ]
                    "the Post's capacity is held whether the source is drained or not"

                Expect.isEmpty (moveIntentsFor "a1" intents) "no wobble off the Post"
            }

            test "the garrison holding residual energy keeps its Post too" {
                // ADR 0025's motivating symptom, with room left in the store:
                // this Anchor clears the applicability gate on free capacity
                // alone, so only the arrival gate's exemption can keep it —
                // the reprieve is pinned here without ADR 0012's overflow
                // widening standing in for it.
                let snapshot =
                    { haulColony with
                        Sources = [ drained "src-a" 60 ]
                        Creeps = [ anchor "a1" 20 30 ]
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "a1", { X = 11; Y = 10 } ]
                                })
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]

                let {
                        Assignments = assignments
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decide snapshot remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Kept("a1", taskId (Harvest "src-a")))
                    "the wobble every cycle began here: it is kept, not released"

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "a1" ]
                    "still the Post's holder through the window"

                Expect.isEmpty (moveIntentsFor "a1" intents) "and it walks nowhere with its load"
            }

            test "the garrison digs nothing while the source is drained" {
                // The Emitter gate (ADR 0025): the occupancy surcharge can
                // land a creep a tick or two early, and the engine's
                // ERR_NOT_ENOUGH_RESOURCES spam must stay impossible. The
                // garrison stays kept and silent, and digs the tick the
                // energy lands.
                let snapshot =
                    { haulColony with
                        Sources = [ drained "src-a" 1 ]
                        Creeps = [ anchor "a1" 50 0 ]
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "a1", { X = 11; Y = 10 } ]
                                })
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]
                let { Intents = intents } = decide snapshot remembered Set.empty None

                Expect.isEmpty
                    (actionIntents intents)
                    "no dig Intent until the energy is there to dig"

                let restocked =
                    { snapshot with
                        Sources = [ source "src-a" ]
                    }

                let { Intents = intents } = decide restocked remembered Set.empty None

                Expect.contains
                    (actionIntents intents)
                    (HarvestSource("a1", "src-a"))
                    "the tick the energy lands, the same garrison digs"
            }

            test "a Dual Seat Anchor gets no reprieve: it upgrades in place through the window" {
                // The exemption is ADR 0024's condition and no other. On a
                // Dual Seat Upgrade is in place, so the Anchor keeps
                // upgrading as ADR 0013 described and rematches Harvest once
                // its Carry is spent.
                let snapshot =
                    { dualSeatColony with
                        Sources = [ drained "src-a" 60 ]
                        Creeps = [ anchor "a1" 50 10 ]
                        Spatial =
                            dualSeatRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "a1", { X = 11; Y = 10 } ]
                                })
                    }

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]

                let {
                        Assignments = assignments
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decide snapshot remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("a1", taskId (Harvest "src-a"), ReleaseReason.TooEarly(0, 60)))
                    "no container underfoot, so no garrison exemption"

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "the Dual Seat's other half is work it can do standing still"

                Expect.isEmpty
                    (moveIntentsFor "a1" intents)
                    "it upgrades in place, it does not walk"
            }

            test "eight road tiles are eight ticks of waiting, not four" {
                // #79's report, at the gate that made it visible. The lane
                // is paved, so an empty worker unit pays one cost unit a
                // step: eight steps price at 8, which halved read as a
                // four-tick arrival and sent this creep out to cover a wait
                // it could not reach in time. The walk floors each tile at
                // a whole tick — eight tiles, eight ticks — so the gate
                // now covers an eight-tick wait and no more.
                let pavedAt pos ticks =
                    let snapshot = restockAt "w1" pos ticks

                    { snapshot with
                        Spatial =
                            snapshot.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    Roads = Set.ofList [ for x in 11..21 -> { X = x; Y = 10 } ]
                                })
                    }

                let { Assignments = assignments } =
                    decide (pavedAt { X = 19; Y = 10 } 8) Map.empty Set.empty None

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "w1" ]
                    "the walk equals the wait: it leaves now and arrives as the energy does"

                let { Assignments = assignments } =
                    decide (pavedAt { X = 19; Y = 10 } 9) Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    None
                    "one tick short, and eight ticks of road do not cover nine of waiting"
            }

            test "a bystander in the lane does not change the dispatch" {
                // #78 inverted. The lane is one tile wide, so a creep
                // standing in it has nowhere to be walked around: the
                // occupancy surcharge added 10 cost units to this walk,
                // five ticks of phantom arrival, and the gate dispatched a
                // creep on a crowd that had moved on by the next tick —
                // then released it TooEarly. The walk is blind to today's
                // traffic, so the same Snapshot decides the same way with
                // the bystander and without it, at every wait either side
                // of the boundary.
                let decides crowded ticks =
                    let snapshot = restockAt "w1" { X = 15; Y = 10 } ticks

                    let snapshot =
                        if crowded then
                            { snapshot with
                                Creeps =
                                    snapshot.Creeps
                                    @ [ creepWith "b1" 0 100 [ Carry; Carry; Move ] ]
                                Spatial =
                                    snapshot.Spatial
                                    |> withHome (fun layer ->
                                        { layer with
                                            CreepPositions =
                                                layer.CreepPositions
                                                |> Map.add "b1" { X = 14; Y = 10 }
                                        })
                            }
                        else
                            snapshot

                    let { Assignments = assignments } = decide snapshot Map.empty Set.empty None
                    Map.tryFind "w1" assignments

                for ticks in [ 3; 4; 5; 6; 9; 12 ] do
                    Expect.equal
                        (decides true ticks)
                        (decides false ticks)
                        $"a bystander cannot decide a %d{ticks}-tick wait either way"

                Expect.equal
                    (decides true 4)
                    (Some(taskId (Harvest "src-a")))
                    "four ticks of walking still cover four of waiting, crowd or no crowd"

                Expect.equal
                    (decides true 9)
                    None
                    "and the crowd no longer buys the phantom five that dispatched it"
            }
        ]

/// A hauler-unit creep: two Carry, one Move — the hauler row's block.
let hauler name energy freeCapacity =
    creepWith name energy freeCapacity [ Carry; Carry; Move ]

/// The hauler quota fixture: a 3-wide field y = 9..11 from x = 8 to one
/// tile past the spawn, the source embedded in wall at (10,10) with its
/// eight Seats open — a seat-based target roomy enough to leave the
/// hauler row slots — the built source container "can-src" on the Seat
/// (11,10) (a Post), and the spawn structure standing at (spawnX,10).
let quotaRoom spawnX =
    { spatial
          [
              "src-a", { X = 10; Y = 10 }
              "can-src", { X = 11; Y = 10 }
              "spawn-1", { X = spawnX; Y = 10 }
          ]
          [
              for x in 8 .. spawnX + 1 do
                  for y in 9..11 -> { X = x; Y = y }, (if x = 10 && y = 10 then Wall else Plain)
          ] with
        TargetKinds =
            Map.ofList
                [
                    "src-a", Source
                    "can-src", Structure BuiltKind.Container
                    "spawn-1", Structure BuiltKind.Spawn
                ]
    }
    |> withHome (fun layer ->
        { layer with
            Obstacles = Set.singleton { X = spawnX; Y = 10 }
        })

/// The quota fixture's colony: `spawnCount` idle spawns drawing on the one
/// 300-capacity bank holding `available` energy.
let quotaColony spawnX spawnCount available =
    { bareRespawn with
        Spawns =
            [
                for i in 1..spawnCount ->
                    { spawn with
                        Name = $"Spawn{i}"
                        Id = (if i = 1 then "spawn-1" else $"spawn-{i}")
                    }
            ]
        RoomEnergy = bank available 300
        Sources = [ source "src-a" ]
        Spatial = quotaRoom spawnX
    }

let haulerCasts intents =
    spawnIntents intents
    |> List.filter (fun (_, _, name: string) -> name.StartsWith "hauler-")
    |> List.length

/// A room whose source containers are a two- and a three-step paved haul
/// from the spawn — the live W12S28 geometry the round trip's repricing
/// was measured against (ADR 0029), flattened onto one paved lane. Sources embedded in
/// wall at (8,10) and (17,10), their built containers on the Seats (9,10)
/// and (16,10) — two Posts — and the spawn structure standing at (12,10),
/// whose free neighbours are (11,10) and (13,10): two steps from the first
/// container, three from the second.
let shortHaulRoom =
    { spatial
          [
              "src-a", { X = 8; Y = 10 }
              "src-b", { X = 17; Y = 10 }
              "can-a", { X = 9; Y = 10 }
              "can-b", { X = 16; Y = 10 }
              "spawn-1", { X = 12; Y = 10 }
          ]
          [
              for x in 8..17 -> { X = x; Y = 10 }, (if x = 8 || x = 17 then Wall else Plain)
          ] with
        TargetKinds =
            Map.ofList
                [
                    "src-a", Source
                    "src-b", Source
                    "can-a", Structure BuiltKind.Container
                    "can-b", Structure BuiltKind.Container
                    "spawn-1", Structure BuiltKind.Spawn
                ]
    }
    |> withHome (fun layer ->
        { layer with
            Roads = Set.ofList [ for x in 9..16 -> { X = x; Y = 10 } ]
            Obstacles = Set.singleton { X = 12; Y = 10 }
        })

[<Tests>]
let haulerTests =
    testList
        "hauler"
        [
            test "a farther source container hires a larger hauler quota" {
                // Near spawn: 8 steps from the container, [4 Carry; 2 Move]
                // at the 300-capacity bank — each leg a walk (ADR 0029),
                // 2 ticks a loaded step and 1 empty, so 24 round-trip ticks
                // and quota ceil(24 x 10 / 200) = 2. Far spawn: 27 steps —
                // 81 ticks, quota 5. The living Anchor fills the Post, so
                // every remaining specialist gap is a hauler cast, and five
                // idle spawns on a 1500 bank can pay for the larger quota.
                let decideAt spawnX =
                    decide
                        { quotaColony spawnX 5 1500 with
                            Creeps = [ anchor "a1" 0 50 ]
                        }
                        Map.empty
                        Set.empty
                        None

                let near = decideAt 20
                let far = decideAt 39

                Expect.equal (haulerCasts near.Intents) 2 "8 steps ship in two bodies"
                Expect.equal (haulerCasts far.Intents) 5 "27 steps hire five"
            }

            test "the measured room's quota does not move: the fix was not a resizing" {
                // ADR 0029 repriced each leg of the round trip as a walk,
                // and both legs got dearer — the live room's two containers
                // move from 3 and 5 round-trip ticks to 4 and 6. Neither
                // crosses the 20 ticks that would buy a second body against
                // the 200-carry hauler a 300 bank casts — the live room's
                // 16-Carry body wants one only past 80 — so the quota stays
                // one apiece and the fleet is the size it was. The error this corrects bites
                // at remote-mining distances, not at home; a future reader
                // finding the fleet unchanged is looking at the right
                // outcome, not at a fix that failed to land.
                let snapshot =
                    { bareRespawn with
                        Spawns =
                            [
                                for i in 1..4 ->
                                    { spawn with
                                        Name = $"Spawn{i}"
                                        Id = (if i = 1 then "spawn-1" else $"spawn-{i}")
                                    }
                            ]
                        RoomEnergy = bank 1200 300
                        Sources = [ source "src-a"; source "src-b" ]
                        Spatial = shortHaulRoom
                        Creeps = [ anchor "a1" 0 50; anchor "a2" 0 50 ]
                    }

                let atlas = Atlas.ofSnapshot snapshot
                let haulerBody = [ Carry; Carry; Carry; Carry; Move; Move ]

                // Container and spawn alike stand in the colony's own
                // room, which the rooms now ride on the API for (#149): a
                // home round trip is the one-room flood it always was.
                let home = SpatialInfo.homeName snapshot.Spatial

                let roundTrip from =
                    Atlas.haulRoundTripTicks atlas haulerBody home from home { X = 12; Y = 10 }

                Expect.equal
                    (roundTrip { X = 9; Y = 10 })
                    (Some 4)
                    "two paved steps out and back, a tick a tile: 3 ticks became 4"

                Expect.equal
                    (roundTrip { X = 16; Y = 10 })
                    (Some 6)
                    "three paved steps out and back: 5 ticks became 6"

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (haulerCasts intents)
                    2
                    "one hauler per container, exactly as the halved round trip hired"
            }

            test "no source containers hires no haulers" {
                let room = quotaRoom 39

                let snapshot =
                    { quotaColony 39 4 1200 with
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =
                            { room with
                                TargetKinds = Map.remove "can-src" room.TargetKinds
                            }
                            |> withHome (fun layer ->
                                { layer with
                                    TargetPositions = Map.remove "can-src" layer.TargetPositions
                                })
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.equal (haulerCasts intents) 0 "no container, nothing to ship"

                Expect.all
                    (spawnIntents intents)
                    (fun (_, _, name) -> name.StartsWith "worker-")
                    "every cast is the generalist row"
            }

            test "the reserver row leads and is empty here: Anchor, hauler, worker follow" {
                // The 8-step round trip hires two haulers, so the order
                // runs Anchor, both haulers, then the generalist — four
                // casts, one per idle spawn, off the one debited bank.
                //
                // The reserver row runs in front of all three (ADR 0042)
                // and casts nothing at all here: a colony projecting one
                // room has no outpost to declare, so that row's quota is
                // zero and its gap with it. A reserver appearing in this list
                // would mean the gap had been computed unconditionally.
                let snapshot =
                    { quotaColony 20 4 1200 with
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (_, firstBody, firstName)
                    (_, secondBody, secondName)
                    (_, thirdBody, thirdName)
                    (_, fourthBody, fourthName) ] ->
                    Expect.stringStarts firstName "anchor-" "the Post's gap is filled first"
                    Expect.equal firstBody [ Work; Work; Carry; Move ] "the Anchor row's body"
                    Expect.stringStarts secondName "hauler-" "the hauler quota comes second"

                    Expect.equal
                        secondBody
                        [ Carry; Carry; Carry; Carry; Move; Move ]
                        "two whole blocks at the 300 bank"

                    Expect.stringStarts
                        thirdName
                        "hauler-"
                        "the quota is filled before the remainder"

                    Expect.equal thirdBody secondBody "the row casts one body"
                    Expect.stringStarts fourthName "worker-" "the generalist fills the remainder"
                    Expect.equal fourthBody (workerBodyFor 300) "the worker row sized to the bank"
                | other -> failtest $"expected exactly four SpawnCreep intents, got %A{other}"
            }

            test "the disaster fallback still casts a bare worker unit" {
                let snapshot =
                    { quotaColony 20 1 300 with
                        Creeps = []
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (_, body, creepName) ] ->
                    Expect.equal body [ Work; Carry; Move ] "time-to-first-creep outranks the rows"
                    Expect.stringStarts creepName "worker-" "the fallback casts the worker row"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "an empty hauler body is matched into Withdraw" {
                let snapshot =
                    { haulColony with
                        Creeps = [ hauler "h1" 0 100 ]
                        Spatial =
                            { haulRoom with
                                Stores = Map.ofList [ "can-src", 500; "can-ctrl", 800 ]
                            }
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "h1", { X = 12; Y = 10 } ]
                                })
                    }

                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Withdraw "can-src")))
                    "the intake half of the haul cycle: free capacity beside a stocked container"

                Expect.contains
                    intents
                    (WithdrawEnergyFromStructure("h1", "can-src"))
                    "in range at tick start: the withdraw fires"
            }

            test "a hauler's intake is a source container, never the upgrade buffer" {
                // The buffer stands one step away and the source container
                // six, yet a body with no Work part can spend nothing at the
                // controller: drawing from the buffer only sends energy back
                // the way it came.
                let snapshot =
                    { haulColony with
                        Creeps = [ hauler "h1" 0 100 ]
                        Spatial =
                            { haulRoom with
                                Stores = Map.ofList [ "can-src", 500; "can-ctrl", 800 ]
                            }
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "h1", { X = 17; Y = 10 } ]
                                })
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Withdraw "can-src")))
                    "the buffer is inapplicable by body; the far source container is the intake"
            }

            test "the buffer stays intake for a Work body: the worker row still draws" {
                // The gate reads the target's kind beside the body, so the
                // colony's own worker row — four Work, four Carry, four
                // Move — keeps the buffer it upgrades from.
                let snapshot =
                    { haulColony with
                        Creeps =
                            [
                                creepWith
                                    "w1"
                                    0
                                    100
                                    [
                                        Work
                                        Work
                                        Work
                                        Work
                                        Carry
                                        Carry
                                        Carry
                                        Carry
                                        Move
                                        Move
                                        Move
                                        Move
                                    ]
                            ]
                        Spatial =
                            { haulRoom with
                                Stores = Map.ofList [ "can-src", 500; "can-ctrl", 800 ]
                            }
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 17; Y = 10 } ]
                                })
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Withdraw "can-ctrl")))
                    "a body that can spend at the controller draws from the buffer beside it"
            }

            test "with only the buffer stocked, a hauler idles instead of cycling it" {
                // The loop this gate closes: every other sink full, the
                // hauler's only Refill target is the buffer it just drew
                // from, so it emptied and refilled the same container tick
                // after tick without ever delivering.
                let snapshot =
                    { haulColony with
                        Creeps = [ hauler "h1" 0 100 ]
                        Spatial =
                            haulRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "h1", { X = 17; Y = 10 } ]
                                })
                    }

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide snapshot Map.empty Set.empty None

                Expect.equal (Map.tryFind "h1" assignments) None "no intake a hauler may draw from"

                Expect.contains
                    verdicts
                    (Verdict.Unassigned("h1", IdleReason.NoneApplicable))
                    "the idle Verdict names the body gate"
            }

            test "filled, the hauler's Withdraw releases and rematches to Refill" {
                // No Work part: Harvest, Build, Upgrade and Repair are
                // inapplicable by body, so the outflow is the only work
                // left — the same emergent alternation as every Task pair.
                let snapshot =
                    { haulColony with
                        Creeps = [ hauler "h1" 100 0 ]
                        Spatial =
                            { haulRoom with
                                Stores = Map.ofList [ "can-src", 500; "can-ctrl", 800 ]
                            }
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "h1", { X = 12; Y = 10 } ]
                                })
                    }

                let remembered = Map.ofList [ "h1", taskId (Withdraw "can-src") ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide snapshot remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("h1", taskId (Withdraw "can-src"), ReleaseReason.Inapplicable))
                    "the full store releases Withdraw"

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Refill "can-ctrl")))
                    "the rematch flips to the outflow"
            }
        ]

/// The crowding fixture (#161): a three-row plain field y = 9..11,
/// x = 5..35, with two containers standing on the middle row — "can-near"
/// at (10,10) and "can-far" at (30,10). Every creep the tests below stand
/// on it sits one step from the near store's Work Area and seventeen or
/// more from the far one, so travel cost points the whole crowd at one
/// container and only a capacity can send any of it to the other. Three
/// rows and not one, so a waiting hauler is never in another's path: the
/// occupancy surcharge (ADR 0008) prices a queue on a one-tile lane at a
/// swamp step apiece, and a crowd that thins itself by standing in its own
/// way would prove the cap without the cap.
///
/// Two containers and not a container and a Storage, because the two must
/// sit on the *same* tier: a rank between them (ADR 0023) would decide the
/// split before capacity was ever asked.
let crowdField =
    [
        for x in 5..35 do
            for y in 9..11 -> { X = x; Y = y }, Plain
    ]

let crowdRoom nearStock farStock =
    { spatial [] crowdField with
        Stores = Map.ofList [ "can-near", nearStock; "can-far", farStock ]
    }
    |> withTargets
        [
            "can-near", { X = 10; Y = 10 }, Structure BuiltKind.Container
            "can-far", { X = 30; Y = 10 }, Structure BuiltKind.Container
        ]

/// The crowding colony: a 600-capacity bank, where the hauler row casts
/// `[8 Carry; 4 Move]` and one trip is therefore exactly 400 energy — the
/// number every stock below is written against. Its creeps are empty
/// hauler bodies on the tiles given, and it has no source and no placed
/// controller, so the only Tasks a Carry-only body is applicable to are the
/// two Withdraws.
let crowdColony nearStock farStock (creeps: (string * Pos) list) =
    { bareRespawn with
        RoomEnergy = bank 600 600
        Sources = []
        Creeps = [ for name, _ in creeps -> hauler name 0 100 ]
        Spatial =
            crowdRoom nearStock farStock
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = Map.ofList creeps
                })
    }

/// Three empty haulers abreast, one step from the near store's Work Area
/// and equally far from it, so nothing but the Matcher's own order can
/// separate them.
let crowdOfThree =
    [ "h1", { X = 12; Y = 9 }; "h2", { X = 12; Y = 10 }; "h3", { X = 12; Y = 11 } ]

/// The names drawing on one store, in name order.
let drawersOf assignments storeId =
    assignments
    |> Map.toList
    |> List.choose (fun (name, tid) -> if tid = taskId (Withdraw storeId) then Some name else None)

/// The stock-crowding fixture: the same field with one Storage standing at
/// (13,10) — an obstacle, as the projection carries a built one — and the
/// same three haulers abreast, all three inside its Work Area. The colony keeps one hungry spawn
/// so ADR 0023's gate stands open; the haulers are empty, so that Refill is
/// inapplicable to every one of them and the stock's Withdraw is the only
/// Task in the pool they can take.
let stockCrowdColony stock =
    { bareRespawn with
        RoomEnergy = bank 600 600
        Sources = []
        Refillables = [ refillable "spawn-1" 300 BuiltKind.Spawn ]
        Creeps = [ for name, _ in crowdOfThree -> hauler name 0 100 ]
        Spatial =
            { spatial [] crowdField with
                Stores = Map.ofList [ "sto-c", stock ]
            }
            |> withTargets [ "sto-c", { X = 13; Y = 10 }, Structure BuiltKind.Storage ]
            |> withHome (fun layer ->
                { layer with
                    Obstacles = Set.singleton { X = 13; Y = 10 }
                    CreepPositions = Map.ofList [ for name, pos in crowdOfThree -> name, pos ]
                })
    }

/// The upgrade buffer's crowd (#161 under ADR 0019): the same three-row
/// field, the controller standing at (10,10) — an obstacle, as a
/// projected one is — with its buffer container "can-buf" at (12,10),
/// inside the Upgrade Work Area and on no source's Seat, and an ordinary
/// container "can-far" holding the same 900 at (30,10), far outside it.
///
/// The bank is 1,800 — the live RCL5 one, and where the two rows part: the
/// cast hauler carries 1,200 a trip and the cast worker 450. Only a Work
/// body may draw from the buffer (ADR 0019), so the three creeps on its
/// doorstep are cast worker bodies, and they are empty, which leaves the
/// Upgrade beside them and the buffer's own Refill inapplicable and the
/// two Withdraws the whole of the pool they can take.
let bufferCrowd =
    [ "w1", { X = 13; Y = 9 }; "w2", { X = 13; Y = 10 }; "w3", { X = 13; Y = 11 } ]

let bufferCrowdColony bufferStock =
    { bareRespawn with
        RoomEnergy = bank 1800 1800
        Sources = []
        Creeps = [ for name, _ in bufferCrowd -> creepWith name 0 450 (workerBodyFor 1800) ]
        Spatial =
            { spatial [] crowdField with
                Stores = Map.ofList [ "can-buf", bufferStock; "can-far", 900 ]
            }
            |> withTargets
                [
                    "ctrl-1", { X = 10; Y = 10 }, Controller
                    "can-buf", { X = 12; Y = 10 }, Structure BuiltKind.Container
                    "can-far", { X = 30; Y = 10 }, Structure BuiltKind.Container
                ]
            |> withHome (fun layer ->
                { layer with
                    Obstacles = Set.singleton { X = 10; Y = 10 }
                    CreepPositions = Map.ofList bufferCrowd
                })
    }

[<Tests>]
let withdrawCapacityTests =
    testList
        "withdraw capacity"
        [
            test "a container that fills one hauler takes one; the rest walk to the full one" {
                // The defect (#161): the matching key puts cost ahead of
                // `load` (ADR 0002), so without a capacity every empty
                // hauler picks the *nearest* stocked container whatever is
                // in it — three bodies onto 400 energy, two of them home
                // empty, while 2,000 stands unvisited seventeen tiles away.
                // The stock is the cap: `ceil(400 / 400)` is one seat.
                let { Assignments = split } =
                    decide (crowdColony 400 2000 crowdOfThree) Map.empty Set.empty None

                Expect.equal
                    (drawersOf split "can-near")
                    [ "h1" ]
                    "one hauler's worth of stock admits one hauler"

                Expect.equal
                    (drawersOf split "can-far")
                    [ "h2"; "h3" ]
                    "and the crowd it turns away walks to the store that can fill it"

                // The pairwise control: the same three creeps on the same
                // tiles, with nothing changed but the near store's stock.
                // Travel cost still says near for all three, and now the
                // capacity lets it — so the split above is the stock's
                // doing and not the geometry's.
                let { Assignments = whole } =
                    decide (crowdColony 2000 2000 crowdOfThree) Map.empty Set.empty None

                Expect.equal
                    (drawersOf whole "can-near")
                    [ "h1"; "h2"; "h3" ]
                    "stocked for five trips, the near container keeps the whole crowd"
            }

            test "the cap rounds up: one load exactly is one seat, one energy more is two" {
                // The `ceil` (#161), pinned at the boundary the arithmetic
                // turns on: 400 is exactly the cast hauler's load and admits
                // one body, and 401 — a fraction of a second trip — admits
                // the second, because the fraction a floor would drop is
                // energy nobody would be sent for.
                let seatsAt stock =
                    let { Assignments = assignments } =
                        decide
                            (crowdColony stock 2000 (List.truncate 2 crowdOfThree))
                            Map.empty
                            Set.empty
                            None

                    drawersOf assignments "can-near"

                Expect.equal (seatsAt 400) [ "h1" ] "one whole load is one seat"

                Expect.equal
                    (seatsAt 401)
                    [ "h1"; "h2" ]
                    "one energy past it is two: the cap rounds up"

                Expect.equal
                    (seatsAt 800)
                    [ "h1"; "h2" ]
                    "and two whole loads are two, with no third body to prove it wider"
            }

            test "a hauler still walking holds its seat: the second is turned away" {
                // Counted at arrival like every other cap (ADR 0026): the
                // holder is fourteen steps out and has not touched the
                // store, and the candidate is standing on its doorstep. A
                // cap counting only the creeps already on the tile would let
                // the near one in and land both on 400 energy — which is the
                // defect with an extra tick in it.
                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide
                        (crowdColony 400 2000 [ "h1", { X = 25; Y = 10 }; "h2", { X = 11; Y = 10 } ])
                        (Map.ofList [ "h1", taskId (Withdraw "can-near") ])
                        (Set.singleton "h2")
                        None

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Withdraw "can-near")))
                    "the walking holder keeps the store it was already sent to"

                Expect.equal
                    (drawersOf assignments "can-far")
                    [ "h2" ]
                    "and the creep on the doorstep is sent to the far store instead"

                let rejections =
                    verdicts
                    |> List.tryPick (function
                        | Verdict.Scoring("h2", rows) ->
                            rows
                            |> List.filter (function
                                | Candidate.Rejected _ -> true
                                | Candidate.Scored _ -> false)
                            |> Some
                        | _ -> None)

                Expect.equal
                    rejections
                    (Some
                        [
                            Candidate.Rejected(
                                taskId (Withdraw "can-near"),
                                RejectReason.CapacityFull
                            )
                            Candidate.Rejected(taskId (Upgrade "ctrl-1"), RejectReason.Inapplicable)
                        ])
                    "the near store names the cap and no gate before it — not the body, not the price; the Upgrade it has no Work for is the pool's only other loss"
            }

            test "the Storage is not special-cased: the same formula, and at 130k no cap" {
                // ADR 0023's stock is one more store and gets one more
                // reading of the same rule (#161) — a Storage down to one
                // trip's worth admits one drawer, exactly as a container
                // does. What keeps that from starving the haul cycle is the
                // number and not an exemption: a real stock divides into
                // hundreds of trips, so the cap is there and is never the
                // thing that binds.
                let { Assignments = thin } = decide (stockCrowdColony 400) Map.empty Set.empty None

                Expect.equal
                    (drawersOf thin "sto-c")
                    [ "h1" ]
                    "a stock holding one trip's worth admits one hauler"

                let { Assignments = full } =
                    decide (stockCrowdColony 130000) Map.empty Set.empty None

                Expect.equal
                    (drawersOf full "sto-c")
                    [ "h1"; "h2"; "h3" ]
                    "and a colony's real stock caps at 325 trips, which is no cap at all"
            }

            test "the upgrade buffer divides by the worker row that draws from it" {
                // Which row draws is a fact about the store (ADR 0019): no
                // body without a Work part may take the buffer, so its
                // drawers are the worker row and its 900 is two cast
                // workers' loads at this bank. Priced by the hauler the
                // colony would cast instead — 1,200 a trip — the same 900
                // reads `ceil(900 / 1200)` = one seat and sends the second
                // upgrader back to a rock while the energy it came to
                // spend stands beside it (#161).
                let { Assignments = split } =
                    decide (bufferCrowdColony 900) Map.empty Set.empty None

                Expect.equal
                    (drawersOf split "can-buf")
                    [ "w1"; "w2" ]
                    "two worker loads standing in the buffer admit two workers"

                // The same 900 in a store the haul cycle owns, judged for
                // the same three bodies: the divisor is the store's and
                // never the candidate's, so the ordinary container admits
                // one and takes the worker the buffer turned away.
                Expect.equal
                    (drawersOf split "can-far")
                    [ "w3" ]
                    "and an ordinary container's 900 is one hauler load, however the body that walks to it is built"

                // The pairwise control: nothing changed but the buffer's
                // stock, three loads instead of two.
                let { Assignments = whole } =
                    decide (bufferCrowdColony 1350) Map.empty Set.empty None

                Expect.equal
                    (drawersOf whole "can-buf")
                    [ "w1"; "w2"; "w3" ]
                    "three loads keep the whole crowd upgrading standing still, which is what a buffer is for"
            }
        ]

/// The names assigned to one pile, in name order — `drawersOf`'s twin for
/// the Pickup Task (#167).
let pickersOf assignments pileId =
    assignments
    |> Map.toList
    |> List.choose (fun (name, tid) -> if tid = taskId (Pickup pileId) then Some name else None)

/// The pile fixture (#167): the crowding field again, one dropped pile on
/// the middle row at (10,10) holding the given amount, and empty hauler
/// bodies on the given tiles.
///
/// The bank is 150 — one whole hauler block, `[2 Carry; 1 Move]` — so a
/// trip is exactly 100 energy and every capacity below is written against
/// that load. No source, no placed controller and a full spawn, so a
/// Carry-only body is applicable to the pile and to nothing else: what
/// these tests read is the Pickup rule and never a tie against some other
/// Task.
let pileTaskColony amount (creeps: (string * Pos) list) =
    { bareRespawn with
        RoomEnergy = bank 150 150
        Sources = []
        Creeps = [ for name, _ in creeps -> hauler name 0 100 ]
        Spatial =
            { spatial [] crowdField with
                Stores = Map.ofList [ "pile-a", amount ]
            }
            |> withTargets [ "pile-a", { X = 10; Y = 10 }, Dropped ]
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = Map.ofList creeps
                })
    }

/// The same field with a tombstone at (10,10) holding the given energy and
/// nothing else standing anywhere (#167). Deliberately not in `Obstacles`:
/// a tombstone lies on the tile a creep died on and the engine lets
/// another walk over it, so its Work Area includes its own tile.
let tombColony energy (creeps: (string * Pos) list) =
    { bareRespawn with
        RoomEnergy = bank 150 150
        Sources = []
        Creeps = [ for name, _ in creeps -> hauler name 0 100 ]
        Spatial =
            { spatial [] crowdField with
                Stores = Map.ofList [ "tomb-1", energy ]
            }
            |> withTargets [ "tomb-1", { X = 10; Y = 10 }, Tombstone ]
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = Map.ofList creeps
                })
    }

[<Tests>]
let pickupTaskTests =
    testList
        "the pile and the tombstone"
        [
            test "a pile past the threshold hires a hauler ten tiles off; one under it hires nobody" {
                // The live gap's second half (#167): 193 energy of death
                // drop at W13S28 36,21 with nobody near enough for the
                // reflex ever to reach it. A pile at or over the threshold
                // is a Task and gets walked to.
                let walk =
                    decide
                        (pileTaskColony 150 [ "h1", { X = 20; Y = 10 } ])
                        Map.empty
                        Set.empty
                        None

                Expect.equal
                    (Map.tryFind "h1" walk.Assignments)
                    (Some(taskId (Pickup "pile-a")))
                    "150 on the ground is worth ten tiles of walking"

                Expect.isEmpty
                    (pickups walk.Intents)
                    "and out of reach it is walking, not picking: the act waits for arrival"

                Expect.isNonEmpty
                    (moveIntentsFor "h1" walk.Intents)
                    "what a Task buys over the reflex is exactly this step"

                // The pairwise control: the same creep on the same tile
                // with the same everything, and 80 energy on the ground.
                let small =
                    decide (pileTaskColony 80 [ "h1", { X = 20; Y = 10 } ]) Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "h1" small.Assignments)
                    None
                    "under the threshold the pile is the reflex's business and nobody walks"
            }

            test "the threshold is inclusive: a hundred exactly is worth the trip" {
                // Where the tunable turns, pinned on both sides of it: two
                // CARRY parts' worth is the smallest load that pays for a
                // walk made for the pile alone.
                let assignmentAt amount =
                    let { Assignments = assignments } =
                        decide
                            (pileTaskColony amount [ "h1", { X = 20; Y = 10 } ])
                            Map.empty
                            Set.empty
                            None

                    Map.tryFind "h1" assignments

                Expect.equal
                    (assignmentAt 100)
                    (Some(taskId (Pickup "pile-a")))
                    "at the line, pooled"

                Expect.equal (assignmentAt 99) None "one energy short of it, not"
            }

            test "the pile that arrives is picked up once, and the bubble says so" {
                // The Task's own action Intent, at range 1 where the Atlas
                // permits it. The reflex asks for the same act on this
                // tick — its rule is the same range and the same free
                // capacity — so the count is the assertion and not the
                // membership: an arriving picker satisfies both producers,
                // and one creep's one pickup spelt twice would over-report
                // the CPU line's accepted-intent column tick after tick
                // (#167). Two *adjacent creeps* both reaching for one pile
                // stay two asks; this is one creep asking twice.
                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decide
                        (pileTaskColony 150 [ "h1", { X = 10; Y = 11 } ])
                        Map.empty
                        Set.empty
                        None

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Pickup "pile-a")))
                    "standing on its doorstep it still holds the Task"

                Expect.equal
                    (pickups intents)
                    [ "h1", "pile-a" ]
                    "it asks the engine for it, exactly once between the Task and the reflex"

                Expect.contains
                    intents
                    (SayCreep("h1", "🧲"))
                    "one glyph per Task, and this Task has its own"
            }

            test
                "the creep beside a hired picker still asks: the pair is deduplicated, not the pile" {
                // The other side of the count above (#167): what the
                // deduplication drops is one creep's own Intent spelt
                // twice, and nothing else. Two haulers stand at one pile
                // of exactly a hundred, so its capacity is one body: h1 is
                // hired and h2 is not, and h2's reflex pickup is energy
                // the colony recovers for free. A filter written over the
                // pile rather than over the (creep, pile) pair would drop
                // it — which is why the assertion is both names and not a
                // count.
                let { Intents = intents } =
                    decide
                        (pileTaskColony 100 [ "h1", { X = 10; Y = 11 }; "h2", { X = 11; Y = 10 } ])
                        Map.empty
                        Set.empty
                        None

                Expect.equal
                    (pickups intents |> List.sort)
                    [ "h1", "pile-a"; "h2", "pile-a" ]
                    "one ask apiece: the hired picker's own, and the bystander's reflex"
            }

            test "a pile decaying under the threshold releases the hauler still walking to it" {
                // The accepted loss, pinned so it stays a decision
                // (`pickupThreshold`, #167). The threshold gates
                // persistence as well as entry, because the pool is
                // rebuilt creep-blind every tick: a pile at 100 holds its
                // holder, and the same pile one energy lighter — a
                // hundredth of the decay a pile spends on its own, or the
                // first of two hired haulers arriving — is gone, and the
                // walk already spent bought nothing.
                let held = Map.ofList [ "h1", taskId (Pickup "pile-a") ]

                let standing =
                    decide (pileTaskColony 100 [ "h1", { X = 20; Y = 10 } ]) held Set.empty None

                Expect.contains
                    standing.Verdicts
                    (Verdict.Kept("h1", taskId (Pickup "pile-a")))
                    "at the line the walk stands"

                let decayed =
                    decide (pileTaskColony 99 [ "h1", { X = 20; Y = 10 } ]) held Set.empty None

                Expect.contains
                    decayed.Verdicts
                    (Verdict.Released("h1", taskId (Pickup "pile-a"), ReleaseReason.TaskGone))
                    "one energy under it, ten tiles from home, and the trip is over"
            }

            test "the pile's amount is its capacity: 150 admits two of the three haulers" {
                // The Withdraw rule over a pile (#161 read by #167):
                // `ceil(150 / 100)` is two bodies, and travel cost cannot
                // thin the crowd because all three stand one step from the
                // pile's Work Area.
                let { Assignments = split } =
                    decide (pileTaskColony 150 crowdOfThree) Map.empty Set.empty None

                Expect.equal
                    (pickersOf split "pile-a")
                    [ "h1"; "h2" ]
                    "one and a half loads on the ground hire two haulers"

                // The pairwise control: the same three creeps on the same
                // tiles, nothing changed but the amount.
                let { Assignments = whole } =
                    decide (pileTaskColony 300 crowdOfThree) Map.empty Set.empty None

                Expect.equal
                    (pickersOf whole "pile-a")
                    [ "h1"; "h2"; "h3" ]
                    "three loads take the whole crowd"
            }

            test "a Work-heavy body never picks a pile up (ADR 0016)" {
                // The gate that keeps an Anchor at its rock, read over the
                // ground as well as over a container: a heavy body's
                // intake is digging, and a pile is not a dig. Pairwise on
                // the body alone — the same parts, one Move more.
                let bodied body =
                    let colony = pileTaskColony 150 [ "a1", { X = 12; Y = 10 } ]

                    { colony with
                        Creeps = [ creepWith "a1" 0 50 body ]
                    }

                let { Assignments = heavy } =
                    decide (bodied [ Work; Work; Carry; Move ]) Map.empty Set.empty None

                Expect.equal (Map.tryFind "a1" heavy) None "more Work than Move: no Pickup"

                let { Assignments = balanced } =
                    decide (bodied [ Work; Work; Carry; Move; Move ]) Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "a1" balanced)
                    (Some(taskId (Pickup "pile-a")))
                    "the same body at Work <= Move picks it up"
            }

            test "a tombstone is a store: its 408 is withdrawn, and an empty one pools nothing" {
                // The live gap's first half (#167): 408 energy standing in
                // a tombstone in the home room while the colony dug. The
                // Intent is the container's own — the engine's `withdraw`
                // is one method over every store.
                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decide (tombColony 408 [ "h1", { X = 11; Y = 10 } ]) Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Withdraw "tomb-1")))
                    "a store with a clock on it is drawn like any other"

                Expect.contains
                    intents
                    (WithdrawEnergyFromStructure("h1", "tomb-1"))
                    "and the act is withdraw, never pickup"

                // The pairwise control: the same tombstone on the same
                // tile, drawn dry.
                let { Assignments = spent } =
                    decide (tombColony 0 [ "h1", { X = 11; Y = 10 } ]) Map.empty Set.empty None

                Expect.equal (Map.tryFind "h1" spent) None "an empty store is no Task"
            }

            test "a tombstone keeps no construction site off its tile" {
                // Layout determinism (ADR 0011), the rule the piles already
                // had (#167): a tombstone stands wherever a creep happened
                // to die, and a plan that moved with it would be a function
                // of that accident.
                let bare = atLevel 2 (openRoom 3)

                let littered =
                    atLevel
                        2
                        (openRoom 3 |> withTargets [ "tomb-1", { X = 24; Y = 24 }, Tombstone ])

                let placedWith = decide littered Map.empty Set.empty None
                let placedWithout = decide bare Map.empty Set.empty None

                Expect.equal
                    (placedTiles placedWith.Intents)
                    (placedTiles placedWithout.Intents)
                    "the Layout does not see tombstones"
            }

            test "a pile ties a container: one tier, and only cost between them" {
                // The tier (#167): a pile is the haul cycle's own energy
                // lying where it fell, so it feeds on the containers' tier
                // and the choice between the two is travel cost's. Equal
                // cost is the way to read that off one match — a rank
                // either way would have decided it before the price was
                // asked, and the factor says which happened.
                let colony =
                    { bareRespawn with
                        RoomEnergy = bank 150 150
                        Sources = []
                        Creeps = [ hauler "h1" 0 100 ]
                        Spatial =
                            { spatial [] crowdField with
                                Stores = Map.ofList [ "pile-a", 150; "can-far", 400 ]
                            }
                            |> withTargets
                                [
                                    "pile-a", { X = 10; Y = 10 }, Dropped
                                    "can-far", { X = 30; Y = 10 }, Structure BuiltKind.Container
                                ]
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "h1", { X = 20; Y = 10 } ]
                                })
                    }

                let { Verdicts = verdicts } = decide colony Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("h1", taskId (Withdraw "can-far"), MatchFactor.PoolOrder) ]
                    "ten tiles either way: pool order broke the tie, not rank"
            }

            test "a pile outranks the stock underfoot" {
                // The other half of the tier, and the one that has a rank
                // in it (ADR 0023): the Storage is drawn a tier below the
                // flow, so a pile sixteen tiles away beats a stock the
                // creep is standing beside. A pile decays at a thousandth
                // a tick and a stock does not, which is the reason the
                // ordering is right as well as inherited.
                let colony =
                    { bareRespawn with
                        RoomEnergy = bank 150 150
                        Sources = []
                        Refillables = [ refillable "spawn-1" 300 BuiltKind.Spawn ]
                        Creeps = [ hauler "h1" 0 100 ]
                        Spatial =
                            { spatial [] crowdField with
                                Stores = Map.ofList [ "pile-a", 300; "sto-c", 400 ]
                            }
                            |> withTargets
                                [
                                    "pile-a", { X = 30; Y = 10 }, Dropped
                                    "sto-c", { X = 13; Y = 10 }, Structure BuiltKind.Storage
                                ]
                            |> withHome (fun layer ->
                                { layer with
                                    Obstacles = Set.singleton { X = 13; Y = 10 }
                                    CreepPositions = Map.ofList [ "h1", { X = 14; Y = 10 } ]
                                })
                    }

                let { Verdicts = verdicts } = decide colony Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("h1", taskId (Pickup "pile-a"), MatchFactor.Rank) ]
                    "the feeding tier beats the stock draw whatever the distance"
            }

            test "an outpost's pile pools by the rule the home room's does" {
                // The declared outpost is a room of the projection like any
                // other (ADR 0041, ADR 0042): the pool is read off the kind
                // census and the amount, neither of which knows a border.
                // The home pile keeps its own coordinate and no amount, so
                // it stays the reflex's and proves the pairing is not
                // crossing (#166).
                let colony =
                    pileColony [ hauler "h-out" 0 100 ] []
                    |> withPileRoom
                        "W1N2"
                        [ "pile-out", { X = 10; Y = 10 } ]
                        [ "h-out", { X = 12; Y = 10 } ]

                let snapshot =
                    { colony with
                        RoomEnergy = bank 150 150
                        Spatial =
                            { colony.Spatial with
                                Stores = Map.ofList [ "pile-out", 150 ]
                            }
                    }

                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decide snapshot Map.empty Set.empty None

                Expect.equal
                    (pickersOf assignments "pile-out")
                    [ "h-out" ]
                    "the outpost's pile hires the hauler standing in the outpost"

                Expect.contains
                    intents
                    (SayCreep("h-out", "🧲"))
                    "and it walks under the Pickup glyph"

                Expect.isEmpty
                    (pickups intents)
                    "two tiles out: no reflex, and no action Intent until it arrives"
            }
        ]

/// The hauler quota this Snapshot decides, read off the plan memo `decide`
/// returns — the quota's only seam, since the rule itself is private to
/// that pipeline.
let quotaOf snapshot =
    let { Memo = memo } = decide snapshot Map.empty Set.empty None
    memo.HaulerQuota

/// Two rooms whose coordinates collide on purpose (ADR 0041). At home:
/// the controller at (25,22), the buffer container "can-home" two tiles
/// off it, and a source far away at (20,30) — so the buffer is the
/// controller's and no source's. In the outpost: a source at (25,25),
/// range 1 from the home buffer's coordinates, and a container at
/// (25,23), range 1 from the home controller's. Nothing here is nearer
/// than a room boundary to anything it collides with.
let collidingRooms =
    { atLevel
          2
          (openRoom 8
           |> withTargets
               [
                   "ctrl-1", { X = 25; Y = 22 }, Controller
                   "can-home", { X = 25; Y = 24 }, Structure BuiltKind.Container
                   "src-a", { X = 20; Y = 30 }, Source
               ]) with
        Sources = [ source "src-a"; source "src-out" ]
    }
    |> withOutpost
        "W1N2"
        [
            "src-out", { X = 25; Y = 25 }, Source
            "can-out", { X = 25; Y = 23 }, Structure BuiltKind.Container
        ]
        [
            for x in 20..30 do
                for y in 20..30 -> { X = x; Y = y }, Plain
        ]

/// A home container stranded six tiles from the spawn and serving no home
/// source, with an outpost source one tile from its coordinates — the
/// hauler quota's half of the same collision. Six tiles because the quota
/// is a round trip: a container beside the spawn prices at zero and would
/// hire nobody whichever room the source stood in.
let strandedContainer sourceRoom =
    let home =
        openRoom 8
        |> withTargets
            [
                "can-far", { X = 25; Y = 31 }, Structure BuiltKind.Container
                "src-a", { X = 19; Y = 19 }, Source
            ]

    let colony =
        { bareRespawn with
            Sources = [ source "src-a"; source "src-out" ]
            Spatial = home
        }

    let strayed = "src-out", { X = 25; Y = 32 }, Source

    if Some sourceRoom = home.RoomName then
        { colony with
            Spatial = colony.Spatial |> withTargets [ strayed ]
        }
    else
        colony |> withOutpost sourceRoom [ strayed ] []

/// The mirror of the collision: a home source at (25,31) and an outpost
/// container one tile off its coordinates, in the outpost. The quota is
/// flooded over the home room's grid, so a container it does not place
/// must never reach the arithmetic at all.
let outpostContainerColony =
    { bareRespawn with
        Sources = [ source "src-a" ]
        Spatial = openRoom 8 |> withTargets [ "src-a", { X = 25; Y = 31 }, Source ]
    }
    |> withOutpost "W1N2" [ "can-out", { X = 25; Y = 32 }, Structure BuiltKind.Container ] []

[<Tests>]
let roomLayerTests =
    testList
        "room layer"
        [
            test "a container belongs to the source and the controller of its own room" {
                let refills =
                    planTasks collidingRooms noThreats
                    |> List.choose (function
                        | Refill id -> Some id
                        | _ -> None)

                // Room-blind, both judgements invert: the home buffer reads
                // as the outpost source's container and drops out of the
                // pool, and the outpost's container reads as the home
                // controller's buffer and enters it.
                Expect.equal
                    refills
                    [ "can-home" ]
                    "the upgrade buffer is the container in the controller's own room"
            }

            test "an outpost source's coordinates hire no haulers at home" {
                // Pairwise, one rival at a time: the same container, the
                // same source, the same coordinates — only the room the
                // source stands in moves.
                Expect.equal
                    (quotaOf (strandedContainer "W1N2"))
                    0
                    "a source across a room boundary makes no container a source container"

                Expect.isGreaterThan
                    (quotaOf (strandedContainer "W1N1"))
                    0
                    "the same source at home does, and hires for the haul"

                // And the mirror, because the quota picks a room twice
                // over: since #149 it folds the containers of every
                // projected room, but each is judged against the sources
                // of *its own* room — so an outpost container beside a
                // home source's coordinates serves no rock and is priced
                // by nothing. The failure this guards is the container
                // being paired with the home rock and then flooded over
                // home terrain, hiring a fleet for a haul nobody makes.
                Expect.equal
                    (quotaOf outpostContainerColony)
                    0
                    "a container whose own room places no rock it serves hires nobody"
            }

            test "the home room keeps its own targets after a second one has joined" {
                // A target added to the home room after an outpost layer is
                // already in the projection lands beside that layer, never
                // over it — `Rooms` is a map keyed by room name and every
                // funnel here merges into the entry it names. Worth pinning
                // because the failure is silent in the direction a fixture
                // cannot see: a home container the projection dropped
                // produces no Refill and no quota, and reads as "the room
                // rule rejected it" when in fact no reader was ever shown
                // it.
                let late =
                    collidingRooms
                    |> withTarget "can-late" { X = 26; Y = 22 } (Structure BuiltKind.Container)

                let refills =
                    planTasks late noThreats
                    |> List.choose (function
                        | Refill id -> Some id
                        | _ -> None)
                    |> List.sort

                Expect.equal
                    refills
                    [ "can-home"; "can-late" ]
                    "a container added after the outpost joined is still the controller's"
            }

            test "a projection that names no room files and reads under the empty name" {
                // The convention `SpatialInfo.homeName` spells, and the one
                // every fixture here that never sets `RoomName` rests on:
                // tiles and no room name is this colony's own room written
                // without saying so, and the empty name is both where its
                // geometry is filed and where every home query looks for
                // it. Its only pin used to be a test of the bridge, so it
                // went when the bridge did; the convention did not go with
                // it. A site that spelled the unnamed room differently
                // would file the home room under one name and read it under
                // another, and ADR 0004 would answer every home query with
                // the empty set rather than throwing — silent in the one
                // direction a fixture cannot see.
                let unnamed = spatial [ "src-a", { X = 10; Y = 10 } ] [ { X = 9; Y = 10 }, Plain ]

                Expect.equal (SpatialInfo.homeName unnamed) "" "the unnamed room's own name"

                Expect.equal
                    (unnamed.Rooms |> Map.toList |> List.map fst)
                    [ "" ]
                    "and the one room it carries is filed under it"

                Expect.equal
                    (homeLayer unnamed).TargetPositions
                    (Map.ofList [ "src-a", { X = 10; Y = 10 } ])
                    "so a home query reads that geometry back, not an empty layer"

                Expect.stringStarts
                    (censusSignature { bareRespawn with Spatial = unnamed })
                    "|"
                    "and the census signature spells that room the same empty name"
            }
        ]

/// The W12S28 shape (ADR 0012): a 3-wide plain field y = 9..11 from x = 8
/// to 32, two sources embedded in wall at (10,10) and (30,10) with their
/// built containers on the Seats (11,10) and (29,10) — two Posts, no Dual
/// Seat — and the spawn structure at (20,10), eight steps from either
/// container.
let incomeRoom =
    { spatial
          [
              "src-a", { X = 10; Y = 10 }
              "src-b", { X = 30; Y = 10 }
              "can-a", { X = 11; Y = 10 }
              "can-b", { X = 29; Y = 10 }
              "spawn-1", { X = 20; Y = 10 }
          ]
          [
              for x in 8..32 do
                  for y in 9..11 ->
                      { X = x; Y = y }, (if (x = 10 || x = 30) && y = 10 then Wall else Plain)
          ] with
        TargetKinds =
            Map.ofList
                [
                    "src-a", Source
                    "src-b", Source
                    "can-a", Structure BuiltKind.Container
                    "can-b", Structure BuiltKind.Container
                    "spawn-1", Structure BuiltKind.Spawn
                ]
    }
    |> withHome (fun layer ->
        { layer with
            Obstacles = Set.singleton { X = 20; Y = 10 }
        })

/// The W12S28 colony: four idle spawns on the one 300-capacity bank with
/// energy to spare — restraint must come from the target, never from the
/// bank running dry.
let incomeColony =
    { bareRespawn with
        Spawns =
            [
                for i in 1..4 ->
                    { spawn with
                        Name = $"Spawn{i}"
                        Id = (if i = 1 then "spawn-1" else $"spawn-{i}")
                    }
            ]
        RoomEnergy = bank 1200 300
        Sources = [ source "src-a"; source "src-b" ]
        Spatial = incomeRoom
    }

/// The income-based fleet the W12S28 shape pins (ADR 0012): one Anchor
/// per Post (2), the throughput quota (2 haulers per container at 8
/// steps — the round trip is 16 ticks out loaded and 8 back empty, and
/// ceil(24 × 10 / 200) is 2 since ADR 0029 priced each leg as a walk),
/// and the income workers — 2 posted sources × 10 e/tick × the 1500-tick
/// lifetime = 30,000, minus the anchor and hauler rows' replacement
/// amortization (2 × 300 + 4 × 300 = 1,800), over one worker body's Work
/// drain × lifetime (1 × 1500) → ceil(18.8) = 19 (ADR 0037).
let incomeFleet =
    [
        anchor "a1" 0 50
        anchor "a2" 0 50
        hauler "h1" 0 100
        hauler "h2" 0 100
        hauler "h3" 0 100
        hauler "h4" 0 100
    ]
    @ [ for i in 1..19 -> worker $"w{i}" 0 50 ]

/// The same colony at a bank the real W12S28 banks: the geometry is
/// untouched — same sources, same containers, same four idle spawns — and
/// only the bank moves, to 1300 against 1300, so every row's body grows
/// with it. Anchor 6W/1C/1M = 700, hauler 16C/8M = 1200 (16 Carry is 800
/// capacity, so the 24-tick round trip's 240 energy is one hauler a
/// container, halving the row to 2), worker 6W/7C/7M — a Work drain of 6.
/// That drain is the granularity the worker row's rounding is paid in,
/// and it grows with RCL: the fixture the row was pinned at banks 300,
/// where the drain is 1 and a lost fraction is worth 0.8 e/tick.
let richIncomeColony =
    { incomeColony with
        RoomEnergy = bank 1300 1300
    }

/// The rich bank's fleet at a given worker count: the rows its quotas
/// pin — one Anchor per Post (2) beside one hauler per container (2) —
/// and as many workers as the case under it is pinning.
let richIncomeFleet workers =
    [ anchor "a1" 0 50; anchor "a2" 0 50; hauler "h1" 0 100; hauler "h2" 0 100 ]
    @ [ for i in 1..workers -> worker $"w{i}" 0 50 ]

[<Tests>]
let incomeWorkforceTests =
    testList
        "income-based workforce"
        [
            test "the W12S28 fleet is the whole target: 2 Anchors + 4 haulers + 19 workers" {
                // Each posted source retires its 8 Seats: a seat base would
                // add 16 on top and the idle spawns would cast into it.
                let snapshot =
                    { incomeColony with
                        Creeps = incomeFleet
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (spawnIntents intents) "the fleet already matches the target"
            }

            test "amortization is deducted: one worker short casts exactly one worker" {
                // Without the anchor/hauler replacement deduction income
                // would feed 20 workers and this gap would draw two casts.
                let snapshot =
                    { incomeColony with
                        Creeps = List.truncate (List.length incomeFleet - 1) incomeFleet
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (_, _, creepName) ] ->
                    Expect.stringStarts creepName "worker-" "the gap is a worker gap"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "at a 1300 bank the whole fleet is 2 Anchors + 2 haulers + 3 workers" {
                // One Anchor per Post (2), one hauler per container (2 at
                // this bank's 800 carry capacity), and the income workers
                // — 30,000 of lifetime income less 2 × 700 + 2 × 1200 of
                // amortization over the row's 6 × 1500 → ceil(2.911) = 3.
                let snapshot =
                    { richIncomeColony with
                        Creeps = richIncomeFleet 3
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (spawnIntents intents) "the fleet already matches the target"
            }

            test "the worker row rounds up at a Work drain of 6, not down to a body short" {
                // The whole defect at one RCL: 30,000 of lifetime income
                // less 3,800 of anchor and hauler amortization over the
                // worker row's 6 × 1500 is 2.911 bodies. Truncating gives
                // 2 and pins upgrade throughput at 12 e/tick whatever the
                // surplus is; rounding up gives the third body (ADR 0037).
                // The fleet below is two workers, so only the rounded-up
                // target has a gap to cast into.
                let snapshot =
                    { richIncomeColony with
                        Creeps = richIncomeFleet 2
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (_, _, creepName) ] ->
                    Expect.stringStarts creepName "worker-" "the third body is the gap"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }
        ]

/// A rock in the middle of a three-tile field, placed wherever a test puts
/// it: three Seats, all Plain, and nothing else within reach of it. The
/// field is written relative to the rock so the same shape can be dropped
/// into the home room and into an outpost, and the only difference between
/// the two fixtures is which room's layer it lands in — which is the whole
/// of what ADR 0042's narrowing turns on.
let private threeSeatField (rock: Pos) =
    [
        { rock with X = rock.X - 1 }, Plain
        { rock with X = rock.X + 1 }, Plain
        { rock with Y = rock.Y - 1 }, Plain
    ]

/// The W12S28 colony at its whole target with one more source somewhere:
/// the fleet already matches, so any Seat the target counts on top shows
/// up as a spawn Intent and nothing else can.
let private incomeColonyPlus (place: Snapshot -> Snapshot) =
    { incomeColony with
        Creeps = incomeFleet
        Sources = incomeColony.Sources @ [ source "src-out" ]
    }
    |> place

[<Tests>]
let outpostWorkforceTests =
    testList
        "an outpost source and the workforce target"
        [
            // The premise every case below is read against: the W12S28
            // fleet is the whole target, so an empty spawn list means the
            // target did not move and a non-empty one says how far it did.
            let atTarget =
                { incomeColony with
                    Creeps = incomeFleet
                }

            let rock = { X = 40; Y = 40 }

            test "an outpost source with no container leaves the target exactly where it was" {
                // ADR 0042's most important regression, and the reason the
                // constant and this narrowing land in one commit. The
                // unposted-seat rule counts a source's Seats into the target
                // because "its output is spoken for by the seat crews that
                // walk it" — which presumes the walk is cheap. Across a
                // border it is not: the three declared outpost sources carry
                // six Seats between them, five of them swamp, and counted
                // here they would hire six generalists to commute
                // forty-seven to fifty-six tiles to dig them.
                //
                // So a source whose room has no container is a source the
                // quotas cannot see, and the proof of it is that the colony
                // decides what it decides with the room not there at all.
                let withOutpostSource =
                    incomeColonyPlus (
                        withOutpost "W1N2" [ "src-out", rock, Source ] (threeSeatField rock)
                    )

                Expect.isEmpty
                    (spawnIntents (decide atTarget Map.empty Set.empty None).Intents)
                    "the premise: the fleet already matches the target"

                Expect.equal
                    (spawnIntents (decide withOutpostSource Map.empty Set.empty None).Intents)
                    (spawnIntents (decide atTarget Map.empty Set.empty None).Intents)
                    "three Seats a room away hire nobody: the same colony casts the same bodies"
            }

            test "the same rock in the spawn room still contributes its Seats" {
                // The other half of the pair, and the only reading under
                // which the case above says anything: the same source, the
                // same three Plain Seats, the same fleet — filed under the
                // home room's layer rather than an outpost's. ADR 0042
                // narrows the unposted-seat rule to the spawn room and does
                // not repeal it, so this colony hires the three walkers it
                // always did. Without this case a rule that counted no
                // Seats anywhere would pass the one above.
                let atHome =
                    incomeColonyPlus (fun colony ->
                        { colony with
                            Spatial =
                                colony.Spatial
                                |> withTargets [ "src-out", rock, Source ]
                                |> withHome (fun layer ->
                                    { layer with
                                        Terrain =
                                            (layer.Terrain, threeSeatField rock)
                                            ||> List.fold (fun acc (tile, terrain) ->
                                                Map.add tile terrain acc)
                                    })
                        })

                Expect.hasLength
                    (spawnIntents (decide atHome Map.empty Set.empty None).Intents)
                    3
                    "three Seats at home raise the target by three, and the idle spawns cast into it"
            }

            test "an outpost Seat on a home Post's coordinates posts nothing" {
                // The trap ADR 0042 names as the most dangerous in the
                // whole set, at the one query that was still asking it
                // room-blind. A `Pos` carries no room, so testing an
                // outpost source's Seats against the *home* room's Posts
                // answers yes on a bare coordinate collision — and that
                // outpost source then reads as posted with no container
                // under it, puts ten energy a tick of income that does not
                // exist into the base, and the colony hires the workers to
                // spend it.
                //
                // The rock stands at (11,11) of the outpost, so its Seat
                // (11,10) is the very tile the home room's `can-a` stands
                // on. Judged in the source's own room (`Atlas.postsOf`) the
                // collision means nothing whatever.
                let collidingRock = { X = 11; Y = 11 }

                let colliding =
                    incomeColonyPlus (
                        withOutpost
                            "W1N2"
                            [ "src-out", collidingRock, Source ]
                            (threeSeatField collidingRock)
                    )

                Expect.contains
                    (Atlas.posts (Atlas.ofSnapshot atTarget))
                    { X = 11; Y = 10 }
                    "the premise: (11,10) really is a Post of the home room"

                Expect.equal
                    (spawnIntents (decide colliding Map.empty Set.empty None).Intents)
                    (spawnIntents (decide atTarget Map.empty Set.empty None).Intents)
                    "a home container on the coordinates of an outpost Seat is no Post of that source's"
            }

            test "an outpost source moves no tile of the home room's Layout" {
                // The fourth room-blind `Pos` join of the family the ticket's
                // Traps section says to verify rather than assume, and the
                // one that was still open: the Layout plans off
                // `snapshot.Sources`, which since #124 is every scanned
                // room's. An outpost source counted there widens the footing
                // reservation by a slot and moves the clustered picks with
                // it, floods a trunk *at home* from the outpost's
                // coordinates, and plants a container site on a home tile
                // that is a Seat of a source a room away. ADR 0042: "The
                // outpost gets a container and nothing else. No roads, and
                // no Layout."
                //
                // The rock stands at (15,30), which is plain walkable ground
                // of the *home* fixture as well — that is what makes the
                // phantom trunk routable and the collision real rather than
                // theoretical.
                let colony = trunkColony 2
                let rock = { X = 15; Y = 30 }

                let joined =
                    { colony with
                        Sources = colony.Sources @ [ source "src-out" ]
                    }
                    |> withOutpost "W1N2" [ "src-out", rock, Source ] (threeSeatField rock)

                Expect.isNonEmpty
                    (placementIntents (decide colony Map.empty Set.empty None).Intents)
                    "the premise: this colony really does place a plan to move"

                Expect.equal
                    (placementIntents (decide joined Map.empty Set.empty None).Intents)
                    (placementIntents (decide colony Map.empty Set.empty None).Intents)
                    "the same room plans the same tiles: a source a room away is no source of its"
            }
        ]

/// The W12S28 fleet at a given hauler and worker count: one Anchor per
/// home Post — the one row no case below moves — beside the two rows that
/// do. The hauler row is a parameter because it halves with the home
/// room's output: a case that neutralises the spawn room and leaves four
/// haulers standing is reading a fleet two bodies above the quota it says
/// it is sized to.
let private incomeFleetRows haulers workers =
    [ anchor "a1" 0 50; anchor "a2" 0 50 ]
    @ [ for i in 1..haulers -> hauler $"h{i}" 0 100 ]
    @ [ for i in 1..workers -> worker $"w{i}" 0 50 ]

/// The same fleet at the held home room's hauler quota of four, which is
/// every case whose home room stays this colony's own. `incomeFleet` is
/// this fleet at 19 workers, which the first case below re-derives rather
/// than assumes.
let private incomeFleetOf workers = incomeFleetRows 4 workers

/// The W12S28 colony with a **posted** outpost source beside it: the same
/// rock in the same three-Seat field the unposted case above leaves out of
/// every quota, with a container standing on one of its Seats — the switch
/// that admits an outpost into the economy (ADR 0042). The fleet is the
/// caller's, and so is who holds W1N2: everything else is `incomeColony`,
/// unmoved, so a difference between two calls is the reservation and
/// nothing else.
///
/// Its hauler quota is the home room's either way: the quota does fold
/// this container since #149, but W1N2 arrives here with no border ring,
/// so the two rooms share no Seam band, the haul has no price and the
/// container hires nobody (ADR 0004) — the quota's own outpost case is
/// `outpostHaulTests`, on a fixture that lays the rings. Its Anchor row
/// is *three* since #129: the container standing on that Seat makes the
/// rock a Post, and one Anchor per Post counts every projected room's
/// Posts (ADR 0042), so the fleet below carries the outpost's Anchor
/// beside the home room's two. Which leaves the income base as the one
/// addend a reservation moves, and a worker count as the whole reading of
/// it.
let private postedOutpostColony workers (control: (string * RoomControlInfo) list) =
    let rock = { X = 40; Y = 40 }

    let colony =
        incomeColonyPlus (
            withOutpost
                "W1N2"
                [
                    "src-out", rock, Source
                    "can-out", { rock with X = rock.X - 1 }, Structure BuiltKind.Container
                ]
                (threeSeatField rock)
        )

    { colony with
        Creeps = incomeFleetOf workers @ [ anchor "a-out" 0 50 ]
        RoomControl =
            (colony.RoomControl, control)
            ||> List.fold (fun acc (room, holder) -> Map.add room holder acc)
    }

[<Tests>]
let sourceOutputTests =
    testList
        "a source's output and the room that holds it"
        [
            // Ten is the *reserved* rate (ADR 0042). The colony that holds
            // W1N2 counts ten energy a tick from its rock and the colony
            // that does not counts five, and five over a 1,500-tick
            // lifetime is five worker places at this bank's Work drain of
            // one. So the fleet below is sized to the *unreserved* target:
            // the unreserved colony has no gap to cast into and the
            // reserved one does, which is a difference no shared cap and
            // no one-body-per-spawn limit can hide.
            //
            // Unreserved the target is 3 Anchors — the outpost's Post
            // hires one since #129 — + 4 haulers +
            // ceil(((20 + 5) × 1500 − 2100) / 1500) = 24 workers = 31;
            // reserved it is 29 workers and 36.
            let unreservedWorkers = 24

            test "the same outpost source is worth twice as much reserved" {
                Expect.equal
                    (incomeFleetOf 19)
                    incomeFleet
                    "the premise: this is the W12S28 fleet, one worker count at a time"

                Expect.isNonEmpty
                    (Atlas.postsOf
                        (Atlas.ofSnapshot (postedOutpostColony unreservedWorkers []))
                        "src-out")
                    "the premise: the container standing on its Seat makes the rock a Post"

                Expect.isEmpty
                    (spawnIntents
                        (decide
                            (postedOutpostColony unreservedWorkers [ "W1N2", neutralRoom ])
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "unreserved, the rock is worth five a tick and the fleet already matches"

                Expect.isNonEmpty
                    (spawnIntents
                        (decide
                            (postedOutpostColony
                                unreservedWorkers
                                [ "W1N2", reservedRoom true 4000 ])
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "reserved, the same rock is worth ten and the colony hires against it"
            }

            test "a reservation another player holds doubles nothing of ours" {
                // Pairwise, one rival at a time: the same room, the same
                // rock, the same reservation standing on the same
                // controller — only whose it is moves. The engine pays
                // ten a tick in a room a rival holds as readily as in one
                // we hold (docs/research/remote-mining.md §1.1); the
                // colony prices it at five anyway, because a room
                // somebody else holds is one it is withdrawing from.
                Expect.isEmpty
                    (spawnIntents
                        (decide
                            (postedOutpostColony
                                unreservedWorkers
                                [ "W1N2", reservedRoom false 4000 ])
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "another player's reservation prices the rock exactly as none at all does"

                // Five and specifically not nothing. The assertion above
                // is sized to the neutral target, so it would hold just
                // as well if a rival's reservation made the rock
                // *unpriceable* — the answer ADR 0004 reserves for a room
                // with no vision. This fleet is the blind target below,
                // which the neutral rate outgrows and the blind one does
                // not, so the branch is pinned strictly between the two.
                Expect.isNonEmpty
                    (spawnIntents
                        (decide
                            (postedOutpostColony 19 [ "W1N2", reservedRoom false 4000 ])
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "a rival's reservation prices the rock at five and hires, not at nothing"
            }

            test "a room another player owns doubles nothing of ours either" {
                // The other half of "somebody else holds it", and the one
                // the projection could not tell from an unowned room until
                // #133: a rival's *ownership*. ADR 0043's clockless
                // withdrawal is triggered by either half, so either half
                // has to be a fact the Snapshot can state — and stating it
                // must not accidentally read as a hold of ours, which is
                // what this pins.
                //
                // Pairwise against the neutral room, one rival at a time:
                // same room, same rock, same container, same fleet. The
                // engine pays ten a tick in a room a rival owns exactly as
                // in one we own (`sources/tick.js` switches on
                // `roomController.user || roomController.reservation`); the
                // colony prices it at five for the same reason it prices a
                // rival's reservation at five.
                Expect.isEmpty
                    (spawnIntents
                        (decide
                            (postedOutpostColony unreservedWorkers [ "W1N2", rivalRoom ])
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "a rival's ownership prices the rock exactly as nobody's does"

                // Five and specifically not ten, which is the failure a
                // three-state owner exists to make unrepresentable: read
                // as "owned, therefore held", the same rock would be worth
                // ten and this fleet would be five worker places short.
                // The one input that moves between this and the assertion
                // above is whose the controller is.
                Expect.isNonEmpty
                    (spawnIntents
                        (decide
                            (postedOutpostColony unreservedWorkers [ "W1N2", ownedRoom ])
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "owned by us the same rock is worth ten, so the fleet above is the neutral one"

                // Five and specifically not nothing, the same strict
                // bracket the reservation case is pinned in: sized to the
                // blind target, the neutral rate hires and unpriceable
                // does not.
                Expect.isNonEmpty
                    (spawnIntents
                        (decide
                            (postedOutpostColony 19 [ "W1N2", rivalRoom ])
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "a rival's ownership prices the rock at five and hires, not at nothing"
            }

            test "the NPC's reservation prices like a rival's and is not the same fact" {
                // The third holder (ADR 0043). A level-0 invader core
                // `attackController`s the room it expanded into and holds
                // the reservation itself — the measured core two rooms
                // from W12S27 does exactly this
                // (docs/research/remote-mining.md §8.4) — and that
                // reservation is the *only* readable deadline it has,
                // because a level-0 core carries no collapse timer.
                //
                // ADR 0043 reads opposite answers off the NPC's hold and a
                // player's: the NPC's is the clock a stand-down runs to,
                // a player's is the clockless withdrawal that never
                // re-enters. So the two must price the same and must stay
                // tellable apart. Pricing first, pairwise against the
                // rival's reservation, one input at a time.
                let priced control =
                    spawnIntents
                        (decide
                            (postedOutpostColony unreservedWorkers [ "W1N2", control ])
                            Map.empty
                            Set.empty
                            None)
                            .Intents

                Expect.equal
                    (priced (coreReservedRoom 4000))
                    (priced (reservedRoom false 4000))
                    "the NPC's reservation prices the rock exactly as a rival's does"

                Expect.isEmpty
                    (priced (coreReservedRoom 4000))
                    "and that price is five, not the held ten"

                Expect.isNonEmpty
                    (priced (reservedRoom true 4000))
                    "held by us the same rock is worth ten, so the fleet above is the neutral one"

                // And tellable apart, which is the whole reason the holder
                // is a closed three-state rather than a flag. A Snapshot
                // that answered both with one "not ours" would hand the
                // gate ADR 0043 describes an input on which no correct
                // answer exists: the NPC's hold read as a rival's shuts an
                // outpost for the life of the colony, and a rival's read
                // as the NPC's walks back into a room somebody else holds.
                let holderOf (control: RoomControlInfo) =
                    control.Reservation |> Option.map (fun held -> held.Holder)

                Expect.notEqual
                    (holderOf (coreReservedRoom 4000))
                    (holderOf (reservedRoom false 4000))
                    "the NPC's hold and a rival's are two facts, not one"

                Expect.notEqual
                    (holderOf (coreReservedRoom 4000))
                    (holderOf (reservedRoom true 4000))
                    "and neither of them is ours"
            }

            test "an outpost the colony cannot see this tick prices no source" {
                // ADR 0004, entry by entry: who holds a room we cannot look
                // into is not a fact this tick, so the source is
                // unpriceable and enters no quota. Unpriceable is not
                // half — half is what a room we *can* see and nobody holds
                // is worth, and the pair below is what separates the two.
                //
                // What is blind here is the *control* entry alone, which is
                // the one input this test moves. The fixture's container
                // still stands in the projection, so its Post is still in
                // the Anchor row and the fleet still carries `a-out` — live
                // the two arrive and vanish together, because the shell
                // gates the structure census and the control entry on the
                // same `seen` list.
                let blind = postedOutpostColony 19 []

                Expect.isEmpty
                    (spawnIntents (decide blind Map.empty Set.empty None).Intents)
                    "no entry for W1N2: the rock's output prices at nothing and the fleet still matches"

                Expect.isNonEmpty
                    (spawnIntents
                        (decide
                            { blind with
                                RoomControl = Map.add "W1N2" neutralRoom blind.RoomControl
                            }
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "seen and held by nobody, the same rock is worth five and hires"
            }

            test "the colony's own room is priced on its owner, not on a reservation" {
                // The trap #116's prose walks into and ADR 0042's rule does
                // not: taken as "reserved, or half", the spawn room — which
                // is owned and which nothing reserves — would price both its
                // sources at five, halving the income base and the hauler
                // quota together. The engine gives a room with an owner the
                // same 3,000 a cycle it gives a reserved one.
                //
                // Sized to the halved target so the direction is
                // readable, and the hauler row halves with the output it
                // ships: 2 Anchors + 2 haulers, whose amortization is
                // 2 × 300 + 2 × 300 = 1,200, + ceil((10 × 1500 − 1,200) /
                // 1500) = 10 workers = 14. Held it would be 2 + 4 + 19 =
                // 25, which is what the second half reads.
                let halved =
                    { incomeColony with
                        Creeps = incomeFleetRows 2 10
                        RoomControl = homeControl |> Map.map (fun _ _ -> neutralRoom)
                    }

                Expect.isEmpty
                    (spawnIntents (decide halved Map.empty Set.empty None).Intents)
                    "the premise: a neutral spawn room's whole target is these fourteen"

                Expect.isNonEmpty
                    (spawnIntents
                        (decide
                            { halved with
                                RoomControl = homeControl
                            }
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "owned, the same two sources are worth ten each and the fleet is eleven short"
            }

            test "the hauler quota prices each container at its own source's output" {
                // The quota's other reader (ADR 0042), read here on the
                // colony's own room: it folds every projected room's
                // containers and prices each at *that* container's
                // source, so moving the rate under the home room moves
                // the home containers' half of it and nothing else. The
                // outpost half is `outpostHaulTests`, on a fixture with a
                // Seam to cross. ceil(24 × 10 / 200) is two haulers a
                // container and ceil(24 × 5 / 200) is one.
                Expect.equal (quotaOf incomeColony) 4 "the premise: the reserved rate hires four"

                Expect.equal
                    (quotaOf
                        { incomeColony with
                            RoomControl = homeControl |> Map.map (fun _ _ -> neutralRoom)
                        })
                    2
                    "half the output is half the haul, one hauler a container"

                Expect.equal
                    (quotaOf
                        { incomeColony with
                            RoomControl = Map.empty
                        })
                    0
                    "a container whose source's room prices nothing hires nobody (ADR 0004)"
            }

            test "a quota memoised while the room was held is not handed back when it lapses" {
                // ADR 0017's stated failure mode, at the seam that would
                // ship it: the hauler quota rides the census memo, and
                // since ADR 0042 it reads who holds the room — a per-tick
                // vision fact, not a census one. `Main.fs` keeps the memo
                // in heap and hands `decide` last tick's every tick, so a
                // signature blind to the rate would recall four haulers
                // for a room now worth half, and would size the worker
                // row off that amortization too. Every census input here
                // is byte-identical between the two Snapshots: the
                // reservation is the only thing that moved.
                let lapsed =
                    { incomeColony with
                        Creeps = incomeFleetRows 2 10
                        RoomControl = homeControl |> Map.map (fun _ _ -> neutralRoom)
                    }

                let previous = (decide incomeColony Map.empty Set.empty None).Memo

                Expect.equal
                    previous.HaulerQuota
                    4
                    "the premise: held, the two home containers hire four"

                let recalled = decide lapsed Map.empty Set.empty (Some previous)
                let fresh = decide lapsed Map.empty Set.empty None

                Expect.equal
                    recalled.Memo.HaulerQuota
                    fresh.Memo.HaulerQuota
                    "the stale memo recomputes to the fresh quota: half the output, half the haul"

                Expect.equal
                    (spawnIntents recalled.Intents)
                    (spawnIntents fresh.Intents)
                    "so the fleet standing at the halved target casts nothing it does not need"
            }
        ]

[<Tests>]
let invaderCoreTests =
    testList
        "the invader core the Snapshot carries"
        [
            test "a core standing in an outpost moves nothing the colony decides" {
                // ADR 0043's first step, and the whole of what it claims:
                // the threat is projected and read by nobody. The gate
                // that will read it withholds a room from the scan set
                // (#136) and the episode that will carry its deadline is
                // the raid log's (#134); until both land, a core in the
                // projection has to leave every Task, every quota, every
                // cast and every Verdict where it found them — reach and
                // flee included, which is why the comparison below is over
                // the whole decision and not over the spawn Intents alone.
                //
                // The colony under it is the posted outpost at its
                // reserved target: creeps matched, a fleet with a gap, an
                // outpost source in the pool. A quiet fixture would make
                // the equality vacuous, so the premise is asserted first.
                let colony = postedOutpostColony 19 [ "W1N2", reservedRoom true 4000 ]
                let untroubled = decide colony Map.empty Set.empty None

                Expect.isNonEmpty
                    untroubled.Verdicts
                    "the premise: this colony reaches a decision worth comparing"

                Expect.isNonEmpty untroubled.Intents "and emits something for a core to disturb"

                // The whole of `Decision` and not three of its four fields.
                // `Memo` is the field the "no reader" claim is easiest to
                // break through and hardest to notice: a reader folded
                // into `censusSignature` moves `Memo.Signature` alone, so
                // the next tick's `recalled` misses and the Layout and the
                // spawn-walk table are thrown away and reflooded (ADR
                // 0032) — a real behaviour change, and an expensive one,
                // that leaves Intents, Assignments and Verdicts identical
                // on this fixture because both calls are handed no memo
                // and recompute from scratch anyway.
                //
                // One field of the memo cannot ride the record comparison:
                // `Walks` is the mutable `Dictionary` the Atlas fills
                // through the tick, and a Dictionary compares by
                // reference, so two floods of identical walks are unequal
                // on it for a reason that has nothing to do with a core.
                // Its reference is swapped in and its *contents* are
                // compared beside it, which loses nothing.
                let walkRows (memo: PlanMemo) =
                    memo.Walks
                    |> Seq.map (fun entry -> entry.Key, List.ofArray entry.Value)
                    |> List.ofSeq
                    |> List.sortBy fst

                let unchangedWith label cores =
                    let threatened =
                        decide { colony with InvaderCores = cores } Map.empty Set.empty None

                    Expect.equal
                        { threatened with
                            Memo =
                                { threatened.Memo with
                                    Walks = untroubled.Memo.Walks
                                }
                        }
                        untroubled
                        $"{label}: the same decision, memo and census signature and all"

                    Expect.equal
                        (walkRows threatened.Memo)
                        (walkRows untroubled.Memo)
                        $"{label}: the same spawn walks flooded under it"

                unchangedWith
                    "a core whose collapse timer is readable"
                    [
                        ({
                            RoomName = "W1N2"
                            CollapseTick = Some(colony.Time + 64000)
                        }
                        : InvaderCoreInfo)
                    ]

                // The level-0 expansion core of ADR 0043: no stronghold
                // under it, so no collapse timer, so no deadline — the
                // case the reservation and the 2,500-tick fallback exist
                // for, and the one a reader might treat as "no threat".
                unchangedWith
                    "a core carrying no deadline at all"
                    [
                        ({
                            RoomName = "W1N2"
                            CollapseTick = None
                        }
                        : InvaderCoreInfo)
                    ]

                // And one at home, where no outpost gate could ever apply:
                // the list is swept over every room the colony looks into,
                // so the spawn room can hold an entry, and the reflexes
                // that do read the spawn room read hostile *creeps*.
                unchangedWith
                    "a core standing in the colony's own room"
                    [
                        ({
                            RoomName = "W1N1"
                            CollapseTick = Some colony.Time
                        }
                        : InvaderCoreInfo)
                    ]
            }

            test
                "the whole frontier case — a level-0 core and the reservation it took — decides nothing" {
                // Both halves of the fact ADR 0043 reads, together, on the
                // shape actually measured two rooms from W12S27
                // (docs/research/remote-mining.md §8.4): a level-0 core
                // carrying no collapse timer, in a room whose controller
                // it has reserved for itself. The deadline lives only in
                // that reservation, which is why `ReservationHolder`
                // separates the NPC from a rival at all.
                //
                // Read against the same room under a *rival's*
                // reservation and no core: everything either fact could
                // move today is priced off the neutral rate both of them
                // yield, so a decision that differs is a reader — of the
                // holder or of the core — that this ticket says does not
                // exist yet (#134 opens the episode, #136 gates on it).
                let withControl control cores =
                    let colony = postedOutpostColony 19 [ "W1N2", control ]
                    decide { colony with InvaderCores = cores } Map.empty Set.empty None

                let frontier =
                    withControl
                        (coreReservedRoom 4900)
                        [
                            ({
                                RoomName = "W1N2"
                                CollapseTick = None
                            }
                            : InvaderCoreInfo)
                        ]

                let rivalHeld = withControl (reservedRoom false 4900) []

                Expect.isNonEmpty
                    rivalHeld.Verdicts
                    "the premise: this colony reaches a decision worth comparing"

                Expect.equal
                    { frontier with
                        Memo =
                            { frontier.Memo with
                                Walks = rivalHeld.Memo.Walks
                            }
                    }
                    rivalHeld
                    "a core and the NPC's own reservation decide exactly what a rival's reservation does"
            }
        ]

/// The W12S28 colony with its own two source containers taken away: the
/// same two rocks, the same eight Seats apiece, and no Post on either. The
/// only Post left in a projection is whatever an outpost carries — which
/// is the one arrangement where a neutral rate is the *richest* rate the
/// Anchor row hires for, and so the only one where the row's ceiling can
/// be read off a cast body at all.
let private withoutHomePosts (colony: Snapshot) =
    { colony with
        Spatial =
            { colony.Spatial with
                TargetKinds = colony.Spatial.TargetKinds |> Map.remove "can-a" |> Map.remove "can-b"
            }
            |> withHome (fun layer ->
                { layer with
                    TargetPositions =
                        layer.TargetPositions |> Map.remove "can-a" |> Map.remove "can-b"
                })
    }

/// The W12S28 colony at a 1,300 bank with the posted outpost source of
/// `postedOutpostColony` standing beside it: the same rock in the same
/// three-Seat field, its container built, and a fleet of one worker so
/// every Post in the projection is an unfilled Anchor gap. The bank alone
/// would buy twelve Work, so the body the row casts is decided by its
/// ceiling and by nothing else, and 700 of the 1,300 goes on that body —
/// leaving too little for a second, so the tick casts exactly one Anchor
/// whatever the gap.
///
/// Two dials and no others: whether the colony's own room keeps its Posts,
/// and who holds W1N2. Everything the target is built from moves with
/// them, but the *body* reads only the ceiling.
let private anchorCapColony homePosts (control: (string * RoomControlInfo) list) =
    let rock = { X = 40; Y = 40 }

    let colony =
        { incomeColony with
            RoomEnergy = bank 1300 1300
            Sources = incomeColony.Sources @ [ source "src-out" ]
            Creeps = [ worker "w1" 0 50 ]
        }
        |> (if homePosts then id else withoutHomePosts)
        |> withOutpost
            "W1N2"
            [
                "src-out", rock, Source
                "can-out", { rock with X = rock.X - 1 }, Structure BuiltKind.Container
            ]
            (threeSeatField rock)

    { colony with
        RoomControl =
            (colony.RoomControl, control)
            ||> List.fold (fun acc (room, holder) -> Map.add room holder acc)
    }

/// The one Anchor body the tick casts, off `decide`'s own Intents. The row
/// is read off the creep name the casting step stamps, so a tick that cast
/// some other row fails here rather than quietly asserting about a worker.
let private anchorCastBy colony =
    match
        spawnIntents (decide colony Map.empty Set.empty None).Intents
        |> List.filter (fun (_, _, name) -> name.StartsWith "anchor-")
    with
    | [ (_, body, _) ] -> body
    | other -> failtest $"expected exactly one Anchor SpawnCreep intent, got %A{other}"

/// The colony the anchor row's **charge** is legible in, which the cast's
/// own fixture is not: the same W12S28 without its two Posts, at a 1,400
/// bank, with three neutral rocks a room away, each with its container
/// standing — three Posts, three Anchors hired, and every one of them
/// under the neutral ceiling. Its fleet is whole but for the workers, so
/// the one thing a spawn Intent can be here is the income base's own
/// answer.
///
/// Why those two numbers and not the 1,300 of the cast's fixture. The
/// amortization is deducted from income before the surplus is divided into
/// worker places, and the division rounds up over a whole body's Work
/// drain across a lifetime (ADR 0037) — 10,500 energy at this bank — so a
/// charge that moves by 350 an Anchor is invisible unless the surplus
/// straddles a boundary. Three Posts move it by 1,050, and 15 energy a
/// tick over the lifetime leaves 21,450 charged at the cast body against
/// 20,400 charged at the held one: three worker places and two. One
/// Post at 1,300 moves it by 350 against a 9,000-energy place and could
/// not move the target at all.
let private anchorChargeColony workers =
    let rocks = [ { X = 10; Y = 40 }; { X = 20; Y = 40 }; { X = 30; Y = 40 } ]

    let outpost =
        rocks
        |> List.mapi (fun i rock ->
            [
                $"src-out{i}", rock, Source
                $"can-out{i}", { rock with X = rock.X - 1 }, Structure BuiltKind.Container
            ])
        |> List.concat

    let colony =
        { incomeColony with
            RoomEnergy = bank 1400 1400
            Sources = incomeColony.Sources @ [ for i in 0..2 -> source $"src-out{i}" ]
            Creeps =
                [ for i in 1..3 -> anchor $"a{i}" 0 50 ]
                @ [ for i in 1..workers -> worker $"w{i}" 0 50 ]
        }
        |> withoutHomePosts
        |> withOutpost "W1N2" outpost (rocks |> List.collect threeSeatField)

    { colony with
        RoomControl = Map.add "W1N2" neutralRoom colony.RoomControl
    }

[<Tests>]
let anchorWorkCapTests =
    testList
        "the Anchor row's Work ceiling"
        [
            let sixWork = [ Work; Work; Work; Work; Work; Work; Carry; Move ]

            let threeWork = [ Work; Work; Work; Carry; Move ]

            test "the same rock caps the Anchor row at six Work reserved and three unreserved" {
                // ADR 0021's rule, ADR 0042's number: the ceiling is a
                // source's saturation plus one spare, and a source under no
                // reservation regenerates 1,500 over 300 ticks instead of
                // 3,000. Five Work saturate the held rock and two the
                // neutral one, so the ceilings are six and three — and the
                // 1,300 bank standing behind both would buy twelve.
                //
                // One rock, one field, one fleet: only who holds W1N2 moves
                // between the two calls.
                Expect.equal
                    (anchorCastBy (anchorCapColony false [ "W1N2", reservedRoom true 4000 ]))
                    sixWork
                    "reserved, the rock gives ten a tick and the row buys the six Work that dig it"

                Expect.equal
                    (anchorCastBy (anchorCapColony false [ "W1N2", neutralRoom ]))
                    threeWork
                    "unreserved it gives five, and three Work drain it as fast as it fills"
            }

            test "a neutral outpost Post does not shrink the ceiling the home room asks for" {
                // The direction the fold is wrong in, pinned pairwise
                // against the case above: the same neutral W1N2, the same
                // rock, the same field — the colony's own two Posts are the
                // only thing added. A cast is a body and not a posting, and
                // travel cost pins it on whichever Post is nearest once it
                // is alive (ADR 0021's own rejection of sizing by the Post),
                // so the row takes the richest saturation it hires for
                // rather than the poorest. Under-sizing an Anchor for a held
                // rock loses four energy a tick for the body's whole life;
                // over-sizing one for a neutral rock wastes 300 energy once
                // in 1,500 ticks and still digs everything the rock has.
                Expect.equal
                    (anchorCastBy (anchorCapColony true [ "W1N2", neutralRoom ]))
                    sixWork
                    "the home room's held rocks keep the row at six Work whatever stands beside them"
            }

            test "the colony's own room is capped exactly where it always was" {
                // The regression ADR 0042 promises: "unchanged as a rule and
                // changed as a number", and the colony's own number does not
                // move. Owned, with no outpost in the projection at all —
                // the case every existing Anchor test is written on, read
                // here for the ceiling alone.
                Expect.equal
                    (anchorCastBy
                        { incomeColony with
                            RoomEnergy = bank 1300 1300
                            Creeps = [ worker "w1" 0 50 ]
                        })
                    sixWork
                    "two held Posts and a 1,300 bank: the six-Work Anchor of ADR 0021"
            }

            test "a Post the colony cannot price this tick leaves the ceiling where it was" {
                // ADR 0004, entry by entry, and the same separation the
                // source rate keeps: unpriceable is not half. W1N2 carries
                // no control entry here, so nobody knows who holds it —
                // the rock contributes no saturation to the fold rather
                // than the neutral one, and a fold with nothing priceable
                // in it answers the held ceiling, which is the largest the
                // rule gives and the safe direction to be wrong in.
                //
                // Pinned strictly against the neutral case above: seen and
                // held by nobody the same rock casts three Work.
                Expect.equal
                    (anchorCastBy (anchorCapColony false []))
                    sixWork
                    "a rock nobody can price caps nothing, and the row keeps the held ceiling"
            }

            test "the row is charged the body it would cast, not the held one" {
                // The other half of #132's landing note — "the price the row
                // is charged must be the body the row is cast at" — and the
                // half no cast body can show: `workforceTarget` deducts the
                // Anchor row's replacement cost from the income before the
                // surplus is divided into worker places (ADR 0012, ADR
                // 0042), so charging six Work for a row that casts three
                // hires an upgrade mouth fewer than the income really feeds.
                //
                // Read as the income base's cases are read, pairwise across
                // one fleet: three Anchors and nineteen workers is the whole
                // of what this colony's 15 energy a tick pays for, so the
                // tick casts nothing; one worker short of it, the row that
                // is short is the worker row and the tick says so. Charged
                // at the held ceiling the target is 21 instead of 22, and
                // the fleet of 21 below has no gap at all.
                let casts workers =
                    spawnIntents
                        (decide (anchorChargeColony workers) Map.empty Set.empty None).Intents
                    |> List.map (fun (_, _, name) -> name)

                Expect.isEmpty
                    (casts 19)
                    "three Anchors and nineteen workers: the income base is spent and the tick casts nothing"

                match casts 18 with
                | [ creepName ] ->
                    Expect.stringStarts
                        creepName
                        "worker-"
                        "one short of it the worker row is short, which the held charge would not have hired"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }
        ]

/// A lane with one Post at one end and the spawn at the other: the source
/// in wall at (10,10), its built container on the Seat (11,10) — the only
/// tile a Work-heavy body may dig that source from (ADR 0020) — and the
/// spawn structure standing at (21,10), ten plain steps up the lane. Its
/// one free neighbour is (20,10), so that is where a replacement is born
/// and the walk it is led by is nine steps, not ten. Far enough that a
/// replacement's own body, not just its cast time, prices the lead.
let successionRoom =
    { spatial [] [ for x in 9..21 -> { X = x; Y = 10 }, (if x = 10 then Wall else Plain) ] with
        Stores = Map.ofList [ "can-src", 0 ]
    }
    |> withHome (fun layer ->
        { layer with
            Obstacles = Set.singleton { X = 21; Y = 10 }
        })
    |> withTargets
        [
            "src-a", { X = 10; Y = 10 }, Source
            "can-src", { X = 11; Y = 10 }, Structure BuiltKind.Container
            "spawn-1", { X = 21; Y = 10 }, Structure BuiltKind.Spawn
        ]

/// The lane's colony. Its controller is unplaced and every creep below is
/// empty, so the one Task any of them can hold is the lane's Harvest.
let successionColony =
    { bareRespawn with
        Sources = [ source "src-a" ]
        Spatial = successionRoom
    }

/// A succession in the lane: the incumbent Anchor on the Post with the
/// given ticks left to live, its successor nine steps away at (20,10).
let succession incumbent successor life =
    { successionColony with
        Creeps = [ anchor incumbent 0 50 |> withLife life; anchor successor 0 50 ]
        Spatial =
            successionRoom
            |> withHome (fun layer ->
                { layer with
                    CreepPositions =
                        Map.ofList [ incumbent, { X = 11; Y = 10 }; successor, { X = 20; Y = 10 } ]
                })
    }

/// The same lane at an RCL3 bank, where the Anchor row's body is five
/// Work beside its Carry and Move (ADR 0021) — and where both creeps below
/// are that body, so the lead prices exactly the body it leads, as a real
/// succession does. Ten cost units a plain step, 21 ticks in the spawner.
let rcl3Succession incumbent successor life =
    let rcl3Anchor name =
        creepWith name 0 50 [ Work; Work; Work; Work; Work; Carry; Move ]

    { successionColony with
        RoomEnergy = bank 600 600
        Creeps = [ rcl3Anchor incumbent |> withLife life; rcl3Anchor successor ]
        Spatial =
            successionRoom
            |> withHome (fun layer ->
                { layer with
                    CreepPositions =
                        Map.ofList [ incumbent, { X = 11; Y = 10 }; successor, { X = 20; Y = 10 } ]
                })
    }

/// The creeps a tick released and why — the release fold's own output,
/// read without the Task it dropped.
let releases verdicts =
    verdicts
    |> List.choose (function
        | Verdict.Released(creep, _, reason) -> Some(creep, reason)
        | _ -> None)

[<Tests>]
let expiringTests =
    testList
        "expiring creeps"
        [
            test "an expiring creep leaves the count: the colony casts its replacement now" {
                // ADR 0026: spawning fills the gap between the target and
                // the creeps that will still be alive when a replacement
                // could arrive. The W12S28 fleet is whole, but its last
                // worker stands eight steps from the spawn at (20,10) —
                // seven from the tile a replacement is born on. That body
                // is five parts, so 15 ticks in the spawner, and its two
                // Move parts carry it over a plain tile in the walk's
                // one-tick floor: 7 ticks, one a tile (ADR 0029). A lead of
                // 22, so at 22 ticks left the worker is out of the count
                // and its successor is cast while it still works.
                let fleetWithLastWorker life =
                    { incomeColony with
                        Creeps =
                            List.truncate (List.length incomeFleet - 1) incomeFleet
                            @ [ worker "w19" 0 50 |> withLife life ]
                        Spatial =
                            incomeRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w19", { X = 12; Y = 10 } ]
                                })
                    }

                let casts life =
                    let { Intents = intents } =
                        decide (fleetWithLastWorker life) Map.empty Set.empty None

                    spawnIntents intents

                Expect.isEmpty (casts 23) "one tick outside its lead, the worker still counts"

                match casts 22 with
                | [ (_, _, creepName) ] ->
                    Expect.stringStarts creepName "worker-" "at its lead it is counted out"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "an expiring Anchor leaves its row's gap, not just the count" {
                // The lane's one Post is garrisoned, so the colony's next
                // body is a hauler's. Once the garrison is expiring the
                // Anchor row is short again and its successor is cast first
                // — the whole point of counting a row's gap at arrival.
                let casts life =
                    let snapshot =
                        { successionColony with
                            Creeps = [ anchor "a1" 0 50 |> withLife life ]
                            Spatial =
                                successionRoom
                                |> withHome (fun layer ->
                                    { layer with
                                        CreepPositions = Map.ofList [ "a1", { X = 11; Y = 10 } ]
                                    })
                        }

                    let { Intents = intents } = decide snapshot Map.empty Set.empty None
                    spawnIntents intents

                match casts 1500 with
                | [ (_, _, creepName) ] ->
                    Expect.stringStarts creepName "hauler-" "a living Anchor fills the row's gap"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"

                match casts 5 with
                | [ (_, _, creepName) ] ->
                    Expect.stringStarts creepName "anchor-" "an expiring one leaves it open"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "the lead is the replacement's own body: long for an Anchor, short for a hauler" {
                // Nine plain steps from the spawn at (20,10) — eight from
                // the tile a replacement is born on — for two rows of the
                // same colony (ADR 0026). A fresh Anchor is empty and slow
                // — 4 cost units a step, 2 ticks of walk apiece, so 16
                // ticks of walking against 12 in the spawner: a lead of 28.
                // A hauler unit rides the walk's one-tick floor empty: 8
                // ticks of walking against 18 in the spawner, a lead of 26.
                // With 27 ticks left each, only the Anchor is inside its
                // own lead.
                let fleetAtPosts life =
                    { incomeColony with
                        Creeps =
                            incomeFleet
                            |> List.map (fun creep ->
                                if creep.Name = "a1" || creep.Name = "h1" then
                                    withLife life creep
                                else
                                    creep)
                        Spatial =
                            incomeRoom
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "a1", { X = 11; Y = 10 }; "h1", { X = 29; Y = 10 } ]
                                })
                    }

                let casts life =
                    let { Intents = intents } = decide (fleetAtPosts life) Map.empty Set.empty None

                    spawnIntents intents |> List.map (fun (_, _, creepName) -> creepName)

                match casts 27 with
                | [ anchorCast ] ->
                    Expect.stringStarts anchorCast "anchor-" "the Anchor's lead outlasts 27 ticks"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"

                match casts 26 with
                | [ anchorCast; haulerCast ] ->
                    Expect.stringStarts anchorCast "anchor-" "under both leads both rows are short"
                    Expect.stringStarts haulerCast "hauler-" "the hauler's lead is the shorter one"
                | other -> failtest $"expected exactly two SpawnCreep intents, got %A{other}"
            }

            test "the lead is the walk out of the spawner, not the step onto its tile" {
                // The engine places a finished creep on a free neighbour,
                // which for this lane is (20,10): the replacement walks
                // nine steps, not ten. At a 600 bank the Anchor row is five
                // Work over one Move — 10 cost units a plain step, 21 ticks
                // in the spawner — so the lead is 21 + 45 = 66. Charging
                // the step out of the spawner's own tile would make it 71
                // and cast the successor five ticks early, into a Post its
                // predecessor still reads as full.
                let casts life =
                    let snapshot =
                        { rcl3Succession "a1" "a2" life with
                            Creeps =
                                [
                                    creepWith
                                        "a1"
                                        0
                                        50
                                        [ Work; Work; Work; Work; Work; Carry; Move ]
                                    |> withLife life
                                ]
                        }

                    let { Intents = intents } = decide snapshot Map.empty Set.empty None
                    spawnIntents intents |> List.map (fun (_, _, creepName) -> creepName)

                match casts 67 with
                | [ creepName ] ->
                    Expect.stringStarts
                        creepName
                        "hauler-"
                        "one tick outside its lead the Anchor still counts"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"

                match casts 66 with
                | [ creepName ] ->
                    Expect.stringStarts creepName "anchor-" "at its lead the row is short again"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "an expiring creep keeps its Task, whichever name the release fold reaches first" {
                // ADR 0026: an expiring creep is not released — anti-thrash
                // keeps it working to the last tick. The release fold walks
                // creep names in order, so the successor can be judged
                // first, take the slot its predecessor's arrival-priced
                // death frees, and leave the incumbent reading its own Post
                // as full. Both orders keep both creeps.
                let bothKept incumbent successor =
                    let remembered =
                        Map.ofList
                            [
                                incumbent, taskId (Harvest "src-a")
                                successor, taskId (Harvest "src-a")
                            ]

                    let {
                            Assignments = assignments
                            Verdicts = verdicts
                        } =
                        decide (succession incumbent successor 5) remembered Set.empty None

                    Expect.isEmpty
                        (releases verdicts)
                        "the succession is the cap agreeing with the gap, not an oversell"

                    Expect.equal
                        (harvesters assignments "src-a" |> List.sort)
                        (List.sort [ incumbent; successor ])
                        "the incumbent digs to the last tick and the successor walks"

                bothKept "a-old" "z-new"
                bothKept "z-old" "a-new"
            }
        ]

[<Tests>]
let arrivalCapacityTests =
    testList
        "capacity at arrival"
        [
            test "a holder dead before the candidate arrives holds none of the Post" {
                // ADR 0026: the lane's one Post admits one garrison, and
                // the incumbent has 5 ticks left against a successor nine
                // steps — 41 ticks — up the lane. It will be gone before
                // the successor gets there, so it holds none of the cap and
                // the successor leaves now instead of after the death.
                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]

                let { Assignments = assignments } =
                    decide (succession "a1" "a2" 5) remembered Set.empty None

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "a1"; "a2" ]
                    "the Post carries the succession, not two standing garrisons"
            }

            test "a holder that outlives the walk still fills the Post" {
                // The other half of the same gate: a garrison that will
                // still be standing there when the candidate arrives holds
                // the cap exactly as ADR 0024 has it, and the candidate is
                // turned away with nothing free.
                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide (succession "a1" "a2" 1500) remembered Set.empty None

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "a1" ]
                    "one Post, one garrison, for as long as the garrison lives"

                Expect.contains
                    verdicts
                    (Verdict.Unassigned("a2", IdleReason.NoneFree))
                    "the second Anchor is idle for want of standing room, not for want of time"
            }

            test "the succession's margin is spent: the walk and the lead price the same ground" {
                // ADR 0026 read this margin as the occupancy surcharge on
                // the incumbent's own tile — the lead was traffic-blind and
                // the arrival was not, which bought the successor five
                // ticks. ADR 0029 makes the arrival traffic-blind too, and
                // the five ticks are gone: nine steps at five ticks a step
                // is a walk of 45, and the lead over the same lane is 66 —
                // 21 in the spawner and the same 45 of walking. So the
                // incumbent has exactly 45 ticks left the tick its
                // successor stands on the birth tile, and the window is
                // read at equality: 44 admits it, 45 does not. The margin
                // ADR 0026 named is no longer there to spend, and a
                // successor born on the boundary idles the tick before the
                // window opens.
                let admits life =
                    let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]

                    let {
                            Assignments = assignments
                            Verdicts = verdicts
                        } =
                        decide (rcl3Succession "a1" "a2" life) remembered Set.empty None

                    harvesters assignments "src-a", releases verdicts, verdicts

                let harvesting, released, _ = admits 44

                Expect.equal
                    harvesting
                    [ "a1"; "a2" ]
                    "a predecessor gone before the walk ends holds none of the Post"

                Expect.isEmpty released "and the predecessor keeps digging"

                let harvesting, released, verdicts = admits 45

                Expect.equal
                    harvesting
                    [ "a1" ]
                    "still standing there when the successor arrives, the Post reads full"

                Expect.isEmpty released "the incumbent is still never released for it"

                Expect.contains
                    verdicts
                    (Verdict.Unassigned("a2", IdleReason.NoneFree))
                    "and the successor idles until the window opens a tick later"
            }

            test "a holder still walking when the candidate dies holds none of it either" {
                // The window is read from both ends (ADR 0026). This
                // garrison has 35 ticks left against a lead of 30 — it is
                // not expiring, and nothing is being cast to replace it —
                // while the Anchor nine steps up the lane is 41 ticks
                // away. Neither is standing on the tile while the other
                // is, so neither counts against the other, and the release
                // fold reaches the pair in creep-name order without that
                // order deciding anything: a window read only from the
                // candidate's end released whichever of the two the fold
                // came to second.
                let bothKept post far =
                    let remembered =
                        Map.ofList [ post, taskId (Harvest "src-a"); far, taskId (Harvest "src-a") ]

                    let {
                            Assignments = assignments
                            Verdicts = verdicts
                        } =
                        decide (succession post far 35) remembered Set.empty None

                    Expect.isEmpty
                        (releases verdicts)
                        "a garrison nowhere near its own lead is not evicted by a distant candidate"

                    Expect.equal
                        (harvesters assignments "src-a" |> List.sort)
                        (List.sort [ post; far ])
                        "both keep what they were remembered on"

                bothKept "z-post" "a-far"
                bothKept "a-post" "z-far"
            }
        ]


/// The Reach of one tick, read at the seam its three readers share (ADR
/// 0033) — the Threats are derived once from the Snapshot and the Atlas,
/// and this is that derivation, not a second one. The home room's share,
/// since the Reach is filed by room (#138) and these colonies stand their
/// hostiles at home.
let reachIn snapshot =
    Threats.reachIn
        (threatsOf snapshot (Atlas.ofSnapshot snapshot))
        (SpatialInfo.homeName snapshot.Spatial)

/// The open colony facing one hostile of the given body on the given tile.
let facingBody pos body =
    atLevel 2 (openRoom 8) |> facing [ hostileAt "h-1" pos body ]

[<Tests>]
let threatTests =
    testList
        "threats"
        [
            test "a fixture's hostiles stand in the room its own projection names" {
                // ADR 0041 joins a hostile to the geometry around it by room
                // name, and the join is silent when it misses: a hostile
                // filed under a name the projection carries no layer for
                // measures against nothing at all (ADR 0004) rather than
                // failing. These colonies are built on `openRoom`, which
                // names its projection, so the empty default `hostileAt`
                // carries for the unnamed fixtures would be wrong here —
                // wrong today only in the Raid log, and in every reflex the
                // tick #117 gives the Reach a room to read.
                let colony = facingBody { X = 25; Y = 22 } [ Attack; Move ]

                Expect.equal
                    (colony.Hostiles |> List.map (fun h -> h.RoomName))
                    [ "W1N1" ]
                    "the hostile names the room, not the empty default"

                Expect.equal
                    (colony.Hostiles |> List.map (fun h -> h.RoomName))
                    [ SpatialInfo.homeName colony.Spatial ]
                    "and it is the room the projection files its own geometry under"
            }

            test "a Threat is read off the parts: ATTACK or RANGED_ATTACK, nothing else" {
                // ADR 0033: nothing but those two hurts a creep, so nothing
                // else has a Reach. A healer, a scout, a claimer and a
                // dismantler are hostiles the fire reflex shoots and the Raid
                // log records, and they gate no Task.
                let reachOf body =
                    reachIn (facingBody { X = 25; Y = 30 } body)

                Expect.isFalse (Set.isEmpty (reachOf [ Attack; Move ])) "an ATTACK part is a Threat"

                Expect.isFalse
                    (Set.isEmpty (reachOf [ RangedAttack; Move ]))
                    "a RANGED_ATTACK part is a Threat"

                Expect.isEmpty (reachOf [ Heal; Move ]) "a healer reaches nothing"
                Expect.isEmpty (reachOf [ Claim; Move ]) "a claimer is safe mode's business"
                Expect.isEmpty (reachOf [ Work; Work; Move ]) "a dismantler hurts no creep"
                Expect.isEmpty (reachOf [ Tough; Move ]) "armour is not a weapon"
            }

            test "the owner is not consulted: an invader and a raider reach the same tiles" {
                // Same body, same tile, different username: the damage per
                // part is the engine's, not the owner's (ADR 0033).
                let raider = hostileAt "h-1" { X = 25; Y = 30 } [ Attack; Move ]
                let invader = { raider with Owner = "Invader" }

                Expect.equal
                    (reachIn (atLevel 2 (openRoom 8) |> facing [ invader ]))
                    (reachIn (atLevel 2 (openRoom 8) |> facing [ raider ]))
                    "the same Reach whoever owns the creep"
            }

            test "a Reach is the weapon range plus the margin, measured in Chebyshev tiles" {
                // Melee reaches 1 + 2, ranged 3 + 2 — the margin is one tile
                // for the hostile's next step and one for our own tick of lag.
                let melee = reachIn (facingBody { X = 25; Y = 30 } [ Attack; Move ])
                let ranged = reachIn (facingBody { X = 25; Y = 30 } [ RangedAttack; Move ])

                Expect.isTrue
                    (Set.contains { X = 25; Y = 27 } melee)
                    "range 3 is inside a melee Reach"

                Expect.isFalse (Set.contains { X = 25; Y = 26 } melee) "range 4 is outside it"

                Expect.isTrue
                    (Set.contains { X = 22; Y = 27 } melee)
                    "and the Reach is a square: the diagonal at range 3 is in it too"

                Expect.isTrue
                    (Set.contains { X = 25; Y = 25 } ranged)
                    "range 5 is inside a ranged Reach"

                Expect.isFalse (Set.contains { X = 25; Y = 24 } ranged) "range 6 is outside it"

                Expect.isTrue
                    (Set.contains
                        { X = 25; Y = 27 }
                        (reachIn (facingBody { X = 25; Y = 30 } [ Attack; RangedAttack; Move ])))
                    "a body carrying both weapons reaches the farther of them"
            }

            test
                "a tile under one of our standing ramparts is in no Reach; a foreign one covers nothing" {
                // Ownership is readable off the projection's hits alone: it
                // carries them for an ownable kind only when it is ours (ADR
                // 0034), and a rampart somebody else left standing in a room
                // we took covers no creep of ours.
                let room =
                    openRoom 8
                    |> withTargets [ "ramp-1", { X = 25; Y = 28 }, Structure BuiltKind.Rampart ]

                let hostiles = [ hostileAt "h-1" { X = 25; Y = 30 } [ Attack; Move ] ]

                let ours =
                    atLevel 2 room
                    |> withHits "ramp-1" BuiltKind.Rampart 100000 300000
                    |> facing hostiles

                let theirs = atLevel 2 room |> facing hostiles

                Expect.isFalse
                    (Set.contains { X = 25; Y = 28 } (reachIn ours))
                    "our rampart takes its own tile out of the Reach"

                Expect.isTrue
                    (Set.contains { X = 25; Y = 27 } (reachIn ours))
                    "and takes out that tile alone: the tile beside it is still hot"

                Expect.isTrue
                    (Set.contains { X = 25; Y = 28 } (reachIn theirs))
                    "a rampart that is not ours excludes nothing"
            }
        ]

/// The raid lane: a one-tile plain corridor, x = 25 and y = 20..30, with
/// the source "src-a" walled in at (25,19) — its single Seat is the lane's
/// north end, (25,20), and every other tile around it lies outside the
/// projection.
let raidLane creeps =
    spatial
        [ "src-a", { X = 25; Y = 19 } ]
        ([ { X = 25; Y = 19 }, Wall ] @ [ for y in 20..30 -> { X = 25; Y = y }, Plain ])
    |> withHome (fun layer ->
        { layer with
            CreepPositions = Map.ofList creeps
        })

/// A colony over the raid lane: one source, no controller and no hungry
/// structure, so Harvest — and, while a Threat stands in it, Flee — is the
/// whole pool.
let laneColony creeps positions =
    { bareRespawn with
        Sources = [ source "src-a" ]
        Refillables = []
        Controller = None
        Creeps = creeps
        Spatial = raidLane positions
    }

/// The same lane with a built container standing on the Seat: the Post a
/// Work-heavy body garrisons (ADR 0012, ADR 0020).
let postLane creeps positions =
    let colony = laneColony creeps positions

    { colony with
        Spatial =
            colony.Spatial
            |> withTargets [ "can-a", { X = 25; Y = 20 }, Structure BuiltKind.Container ]
    }

/// A garrison body for the Post tests: two Work over one Move is the
/// Work-heavy shape, and its store has room, so Harvest fits its body and
/// its energy state both.
let garrison name =
    creepWith name 0 100 [ Work; Work; Carry; Move ]

[<Tests>]
let threatGateTests =
    testList
        "threat gate"
        [
            test
                "a Harvest whose only Seat is in a Reach is inapplicable, and its holder released Threatened" {
                // The holder stands eight tiles down the lane, well outside
                // the Reach: it is not running from anything, its Task has
                // simply lost the one tile it could have been worked from.
                let colony =
                    laneColony [ worker "w1" 0 100 ] [ "w1", { X = 25; Y = 30 } ]
                    |> facing [ hostileAt "h-1" { X = 25; Y = 22 } [ Attack; Move ] ]

                let {
                        Assignments = kept
                        Verdicts = verdicts
                    } =
                    decide colony (Map.ofList [ "w1", taskId (Harvest "src-a") ]) Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("w1", taskId (Harvest "src-a"), ReleaseReason.Threatened))
                    "the raid's release names the raid, not a Task that vanished"

                Expect.isEmpty (Map.toList kept) "and the Seat is not offered to anyone else"

                Expect.contains
                    verdicts
                    (Verdict.Unassigned("w1", IdleReason.NoneApplicable))
                    "the creep waits rather than walking into the Reach"
            }

            test "the Scoring Verdict rejects a threatened candidate as Threatened" {
                let colony =
                    laneColony [ worker "w1" 0 100 ] [ "w1", { X = 25; Y = 30 } ]
                    |> facing [ hostileAt "h-1" { X = 25; Y = 22 } [ Attack; Move ] ]

                let { Verdicts = verdicts } = decide colony Map.empty (Set.ofList [ "w1" ]) None

                Expect.contains
                    verdicts
                    (Verdict.Scoring(
                        "w1",
                        [
                            Candidate.Rejected(taskId Flee, RejectReason.Inapplicable)
                            Candidate.Rejected(taskId (Harvest "src-a"), RejectReason.Threatened)
                        ]
                    ))
                    "the whole pool, each Task at the gate it failed"
            }

            test "a Work-heavy body on a ramparted Post keeps digging with a Threat beside it" {
                // ADR 0034's exemption, read through the Reach: the tile under
                // our rampart is in no Reach, so the Post is still standing
                // room and the narrowed Work Area is not empty.
                let colony =
                    postLane [ garrison "a1" ] [ "a1", { X = 25; Y = 20 } ]
                    |> facing [ hostileAt "h-1" { X = 25; Y = 22 } [ Attack; Move ] ]

                let ramparted =
                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withTargets
                                [ "ramp-1", { X = 25; Y = 20 }, Structure BuiltKind.Rampart ]
                    }
                    |> withHits "ramp-1" BuiltKind.Rampart 100000 300000

                let {
                        Assignments = kept
                        Verdicts = verdicts
                    } =
                    decide ramparted (Map.ofList [ "a1", taskId (Harvest "src-a") ]) Set.empty None

                Expect.equal
                    (Map.tryFind "a1" kept)
                    (Some(taskId (Harvest "src-a")))
                    "the Anchor keeps its Post"

                Expect.isEmpty
                    (verdicts
                     |> List.filter (function
                         | Verdict.Released _ -> true
                         | _ -> false))
                    "and nothing releases it"
            }

            test "the same body on a bare Post is released Threatened, and is not matched to Flee" {
                // A crawling Anchor neither escapes nor digs (ADR 0033), so
                // Flee is inapplicable to it: it loses the Task and waits.
                let colony =
                    postLane [ garrison "a1" ] [ "a1", { X = 25; Y = 20 } ]
                    |> facing [ hostileAt "h-1" { X = 25; Y = 22 } [ Attack; Move ] ]

                let {
                        Assignments = kept
                        Verdicts = verdicts
                    } =
                    decide colony (Map.ofList [ "a1", taskId (Harvest "src-a") ]) Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("a1", taskId (Harvest "src-a"), ReleaseReason.Threatened))
                    "an unramparted Post in a Reach is no standing room"

                Expect.isEmpty (Map.toList kept) "and a Work-heavy body does not run"

                Expect.contains
                    verdicts
                    (Verdict.Unassigned("a1", IdleReason.NoneApplicable))
                    "it stays and waits, as ADR 0033 says it must"
            }

            test "a body standing on a hot Post digs from neither it nor the cold one it walks to" {
                // Two Posts, one hot: the Task keeps the cold one and stays
                // applicable, so the Anchor holds it — but a creep acts only
                // from the tiles it was judged over, so the dig waits until
                // it has walked out of the Reach.
                let twoPosts =
                    postLane [ garrison "a1" ] [ "a1", { X = 25; Y = 20 } ]
                    |> fun colony ->
                        { colony with
                            Spatial =
                                colony.Spatial
                                |> withHome (fun layer ->
                                    { layer with
                                        Terrain = Map.add { X = 24; Y = 20 } Plain layer.Terrain
                                    })
                                |> withTargets
                                    [ "can-b", { X = 24; Y = 20 }, Structure BuiltKind.Container ]
                        }

                let colony =
                    twoPosts |> facing [ hostileAt "h-1" { X = 28; Y = 20 } [ Attack; Move ] ]

                let {
                        Intents = intents
                        Assignments = kept
                    } =
                    decide colony (Map.ofList [ "a1", taskId (Harvest "src-a") ]) Set.empty None

                Expect.equal
                    (Map.tryFind "a1" kept)
                    (Some(taskId (Harvest "src-a")))
                    "the cold Post keeps the Task applicable"

                Expect.isEmpty
                    (actionIntents intents)
                    "and the hot Post it stands on is no tile to dig from"

                Expect.equal (moveIntents intents) [ "a1", Left ] "it walks to the cold Post"
            }

            test "a Task whose cold tiles cannot be reached is released Unreachable, not held" {
                // The source's other Seat is a walled-off pocket: cold, and
                // no use to a creep on the lane. Reachability is judged over
                // the tiles the Reach left, so the holder is released rather
                // than kept on a Task it can never stand for.
                let pocket creeps positions =
                    let colony = laneColony creeps positions

                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    Terrain = Map.add { X = 24; Y = 18 } Plain layer.Terrain
                                })
                    }

                let colony =
                    pocket [ worker "w1" 0 100 ] [ "w1", { X = 25; Y = 30 } ]
                    |> facing [ hostileAt "h-1" { X = 25; Y = 22 } [ Attack; Move ] ]

                let { Verdicts = verdicts } =
                    decide colony (Map.ofList [ "w1", taskId (Harvest "src-a") ]) Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("w1", taskId (Harvest "src-a"), ReleaseReason.Unreachable))
                    "cold but unreachable is unreachable, and says so"
            }
        ]

/// Two ways into a controller's Upgrade Work Area: the near tile (25,28),
/// straight up the lane from the creep at (25,29), and the far one
/// (23,28), around the corner through (24,29) and (23,29). Both lie at
/// range 3 of the controller at (25,25); the corner tiles do not.
let hotCornerRoom =
    spatial
        [ "ctrl-1", { X = 25; Y = 25 } ]
        [
            { X = 25; Y = 29 }, Plain
            { X = 25; Y = 28 }, Plain
            { X = 24; Y = 29 }, Plain
            { X = 23; Y = 29 }, Plain
            { X = 23; Y = 28 }, Plain
        ]
    |> withHome (fun layer ->
        { layer with
            CreepPositions = Map.ofList [ "u1", { X = 25; Y = 29 } ]
        })

/// The colony over it: one loaded generalist, and Upgrade the whole pool.
let hotCornerColony =
    { bareRespawn with
        Sources = []
        Refillables = []
        Controller = Some(controllerAt 2)
        Creeps = [ worker "u1" 50 0 ]
        Spatial = hotCornerRoom
    }

[<Tests>]
let hotCornerTests =
    testList
        "a partly threatened Work Area"
        [
            test
                "an area with one hot corner stays applicable, and the mover is handed the cold tiles" {
                // ADR 0033's middle case: neither "any tile hot" (which would
                // stop all upgrading over one corner) nor "every tile hot"
                // (which would send the creep to the hot one). The Threat at
                // (27,25) covers the near way in at (25,28) and leaves the far
                // one at (23,28), so the creep turns the corner instead of
                // walking up the lane.
                let {
                        Intents = quiet
                        Assignments = before
                    } =
                    decide hotCornerColony Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "u1" before)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "with nothing to run from the creep upgrades"

                Expect.equal
                    (moveIntents quiet)
                    [ "u1", Top ]
                    "and takes the near way in, one step up the lane"

                let raided =
                    hotCornerColony
                    |> facing [ hostileAt "h-1" { X = 27; Y = 25 } [ Attack; Move ] ]

                let {
                        Intents = intents
                        Assignments = after
                        Verdicts = verdicts
                    } =
                    decide raided Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "u1" after)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "one hot corner does not stop the upgrading"

                Expect.isEmpty
                    (verdicts
                     |> List.filter (function
                         | Verdict.Unassigned _ -> true
                         | _ -> false))
                    "the Task is judged applicable, not threatened"

                Expect.equal
                    (moveIntents intents)
                    [ "u1", Left ]
                    "and the mover walks the long way to the cold tile"
            }
        ]

[<Tests>]
let fleeTests =
    testList
        "flee"
        [
            test "a creep standing in a Reach flees, outbidding even a deadline Upgrade" {
                // Safety ranks beneath the downgrade deadline's -1 (ADR 0033):
                // nothing the colony wants done matters while the creep doing
                // it is being killed.
                let colony =
                    { laneColony [ worker "w1" 50 0 ] [ "w1", { X = 25; Y = 22 } ] with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Controller =
                            Some
                                { controllerAt 2 with
                                    TicksToDowngrade = 10
                                }
                    }
                    |> facing [ hostileAt "h-1" { X = 25; Y = 20 } [ Attack; Move ] ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide colony Map.empty Set.empty None

                Expect.equal (Map.tryFind "w1" assignments) (Some(taskId Flee)) "the creep runs"

                Expect.contains
                    verdicts
                    (Verdict.Matched("w1", taskId Flee, MatchFactor.Rank))
                    "and rank is what decided it against the deadline Upgrade"
            }

            test "a body with no Work part flees too: no part and no capacity are asked" {
                let hauler = creepWith "h1" 50 100 [ Carry; Carry; Move ]

                let colony =
                    { laneColony [ hauler ] [ "h1", { X = 25; Y = 22 } ] with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                    }
                    |> facing [ hostileAt "h-1" { X = 25; Y = 20 } [ Attack; Move ] ]

                let { Assignments = assignments } = decide colony Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId Flee))
                    "a hauler under fire hauls nothing"
            }

            test "the Move Intent walks toward a safe tile, and no action is emitted" {
                // The Threat holds the lane's north end, so every tile within
                // three of it is hot and the safe ground is south: the creep
                // steps that way, and says the Flee glyph while it does.
                let colony =
                    laneColony [ worker "w1" 0 100 ] [ "w1", { X = 25; Y = 22 } ]
                    |> facing [ hostileAt "h-1" { X = 25; Y = 20 } [ Attack; Move ] ]

                let { Intents = intents } = decide colony Map.empty Set.empty None

                Expect.equal (moveIntents intents) [ "w1", Bottom ] "one step away from the Threat"

                Expect.isEmpty
                    (actionIntents intents)
                    "Flee has no action: the Emitter issues movement only"

                Expect.contains (sayIntents intents) ("w1", "🏃") "and the bubble shows the run"
            }

            test "two creeps in a Reach both flee: Flee is uncapped" {
                let colony =
                    laneColony
                        [ worker "w1" 0 100; worker "w2" 0 100 ]
                        [ "w1", { X = 25; Y = 22 }; "w2", { X = 25; Y = 23 } ]
                    |> facing [ hostileAt "h-1" { X = 25; Y = 20 } [ Attack; Move ] ]

                let { Assignments = assignments } = decide colony Map.empty Set.empty None

                Expect.equal
                    (assignments |> Map.toList |> List.sort)
                    [ "w1", taskId Flee; "w2", taskId Flee ]
                    "no worker cap stands between a creep and safety"
            }

            test "the tick it stands outside the Reach it is released Inapplicable and rematches" {
                // Flee ends by its own applicability (ADR 0033): the Threat is
                // still in the room, so the Task is still pooled — this creep
                // is simply no longer inside its Reach.
                let colony =
                    { laneColony [ worker "w1" 50 0 ] [ "w1", { X = 25; Y = 28 } ] with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                    }
                    |> facing [ hostileAt "h-1" { X = 25; Y = 20 } [ Attack; Move ] ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide colony (Map.ofList [ "w1", taskId Flee ]) Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("w1", taskId Flee, ReleaseReason.Inapplicable))
                    "out of the Reach, out of the Task"

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Refill "spawn-1")))
                    "and it goes back to work the same tick"
            }

            test "the Threat leaving takes Flee out of the pool, and its holder with it" {
                // The other half of Flee's ending: no Reach, no Flee — the
                // Task exists while the condition holds (ADR 0013's shape),
                // so a room the raiders have left releases the runner as
                // task-gone and the transition log tells the two apart.
                let colony = laneColony [ worker "w1" 0 100 ] [ "w1", { X = 25; Y = 22 } ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide colony (Map.ofList [ "w1", taskId Flee ]) Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("w1", taskId Flee, ReleaseReason.TaskGone))
                    "nothing left to run from"

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "and the Seat is worked again"
            }
        ]

[<Tests>]
let spawnHoldTests =
    testList
        "the spawn hold"
        [
            test "a Threat beside the spawn holds the cast; one across the room does not" {
                // A creep born into a Reach is a kill delivered (ADR 0033).
                let staffed room =
                    { atLevel 2 room with
                        Creeps = [ worker "w1" 0 100 ]
                        Spatial =
                            room
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 25; Y = 27 } ]
                                })
                    }

                let colony = staffed (openRoom 6)

                let { Intents = quiet } = decide colony Map.empty Set.empty None
                Expect.isNonEmpty (spawnIntents quiet) "a quiet colony casts its deficit"

                let { Intents = beside } =
                    decide
                        (colony |> facing [ hostileAt "h-1" { X = 25; Y = 29 } [ Attack; Move ] ])
                        Map.empty
                        Set.empty
                        None

                Expect.isEmpty
                    (spawnIntents beside)
                    "the doorstep is in the Reach: nothing is cast into it"

                let { Intents = across } =
                    decide
                        (colony |> facing [ hostileAt "h-1" { X = 31; Y = 31 } [ Attack; Move ] ])
                        Map.empty
                        Set.empty
                        None

                Expect.isNonEmpty
                    (spawnIntents across)
                    "a Threat that reaches no tile beside the spawn holds nothing"
            }

            test "the disaster fallback holds too: an empty colony casts nothing under fire" {
                // The one cast that ignores the bank's capacity still does not
                // ignore the Reach — the first creep of an empty colony is the
                // one that can least afford to be born under fire.
                let empty = atLevel 2 (openRoom 6)

                let { Intents = quiet } = decide empty Map.empty Set.empty None

                Expect.isNonEmpty
                    (spawnIntents quiet)
                    "an empty colony casts from whatever is banked"

                let { Intents = raided } =
                    decide
                        (empty |> facing [ hostileAt "h-1" { X = 25; Y = 29 } [ Attack; Move ] ])
                        Map.empty
                        Set.empty
                        None

                Expect.isEmpty (spawnIntents raided) "and holds while the doorstep is hot"
            }
        ]

/// A plain corridor down one column, the shape a two-room fixture needs
/// twice: geometry a reader can count steps along, in a room the flood
/// must not leave (ADR 0041).
let private corridor x y0 y1 =
    [ for y in y0..y1 -> { X = x; Y = y }, Plain ]

/// The same projection with a second room's layer beside the colony's own,
/// under that room's name — the shape an outpost arrives in, and the only
/// one there is since the tile-shaped containers moved under a room name
/// (ADR 0041).
let private withNeighbour room layer (spatial: SpatialInfo) =
    { spatial with
        Rooms = Map.add room layer spatial.Rooms
    }

[<Tests>]
let neighbouringRoomTests =
    testList
        "decide across a border"
        [
            test "a source in the neighbouring room is no Task this creep can be given" {
                // The seam the Atlas's own `travelCost` test cannot reach:
                // the Matcher prices through the Work Area, not through the
                // Task-shaped wrapper, so a guard that sits only on the
                // wrapper leaves the ranking price to be invented off this
                // room's flood. Priced that way the neighbour's source is
                // cost 0 or a handful of units — cheaper than every home
                // rival — and the creep is assigned a Task `mayAct` refuses
                // for the rest of its life, walking inside its own room
                // toward ground it will never stand on. Until #123 sums the
                // legs over the Seam band the honest answer is that the
                // Task does not apply to this creep.
                let home =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                        TargetKinds = Map.ofList [ "src-out", Source ]
                    }
                    |> withHome (fun layer ->
                        { layer with
                            Terrain = Map.ofList (corridor 10 10 17)
                            CreepPositions = Map.ofList [ "w-home", { X = 10; Y = 10 } ]
                        })

                let outpost =
                    { RoomLayer.empty with
                        Terrain = Map.ofList (corridor 10 10 17)
                        TargetPositions = Map.ofList [ "src-out", { X = 10; Y = 18 } ]
                    }

                let snapshot =
                    { bareRespawn with
                        Spawns = []
                        Sources = [ source "src-out" ]
                        Controller = None
                        Refillables = []
                        Creeps = [ worker "w-home" 0 50 ]
                        Spatial = home |> withNeighbour "W2N1" outpost
                    }

                let {
                        Assignments = assignments
                        Intents = intents
                    } =
                    decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w-home" assignments)
                    None
                    "the neighbour's Harvest is inapplicable, so nothing is assigned"

                Expect.isEmpty
                    (moveIntents intents)
                    "and nobody is walked toward a border they cannot cross"
            }

            test "a grounded creep in the neighbouring room grounds nobody here" {
                // ADR 0041's Consequences keep arbitrated movement and the
                // occupancy surcharge single-room, unchanged. The Resolver
                // pre-claims a fatigued creep's tile through a `Set<Pos>`
                // that has no room dimension (ADR 0008), so a creep on the
                // same coordinate of another room would deny a step here
                // on evidence from fifty tiles away. The two projections
                // differ only in what the neighbour holds, and this room
                // decides identically.
                let home =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                        TargetKinds = Map.ofList [ "src-home", Source ]
                    }
                    |> withHome (fun layer ->
                        { layer with
                            Terrain = Map.ofList (corridor 10 10 18)
                            TargetPositions = Map.ofList [ "src-home", { X = 10; Y = 18 } ]
                            CreepPositions = Map.ofList [ "w-home", { X = 10; Y = 10 } ]
                        })

                let colony creeps layer =
                    { bareRespawn with
                        Spawns = []
                        Sources = [ source "src-home" ]
                        Controller = None
                        Refillables = []
                        Creeps = creeps
                        Spatial = home |> withNeighbour "W2N1" layer
                    }

                let assigned = Map.ofList [ "w-home", "harvest:src-home" ]

                let { Intents = alone } =
                    decide (colony [ worker "w-home" 0 50 ] RoomLayer.empty) assigned Set.empty None

                let neighbour =
                    { RoomLayer.empty with
                        Terrain = Map.ofList (corridor 10 10 18)
                        CreepPositions = Map.ofList [ "w-out", { X = 10; Y = 11 } ]
                    }

                let { Intents = crowded } =
                    decide
                        (colony
                            [ worker "w-home" 0 50; { worker "w-out" 0 50 with Fatigue = 5 } ]
                            neighbour)
                        assigned
                        Set.empty
                        None

                Expect.equal
                    (moveIntents alone)
                    [ "w-home", Bottom ]
                    "the premise: with the neighbour empty the home creep steps down its corridor"

                Expect.equal
                    (moveIntents crowded)
                    (moveIntents alone)
                    "and a creep paying off fatigue in another room changes nothing here"
            }
        ]

/// A plain border ring. The Seam query reads the border layer and nothing
/// else (ADR 0041), so a projection without one answers an empty band and
/// prices no crossing at all — which is what the two fixtures above rest
/// on and what the ones below must not. Plain the whole way round, so no
/// crossing is picked out by its terrain.
let private plainRing =
    Map.ofList
        [
            for x in 0..49 do
                for y in 0..49 do
                    if x = 0 || x = 49 || y = 0 || y = 49 then
                        { X = x; Y = y }, Plain
        ]

/// The home half of the fixtures below: the corridor running down from the
/// north border, one worker at (10,2) a step inside it, and one source
/// wherever the test puts it. No controller, no refillable with room and
/// no store, so the whole Task pool is the sources — which is what lets a
/// Matched Verdict's factor name the one comparison that separated them
/// rather than report on some third candidate.
let private northBorderColony (homeSource: Pos) =
    { bareRespawn with
        Spawns = []
        Controller = None
        Refillables = []
        Sources = [ source "src-home" ]
        Creeps = [ worker "w" 0 50 ]
        Spatial =
            { SpatialInfo.empty with
                RoomName = Some "W1N1"
                Borders = Map.ofList [ "W1N1", plainRing ]
                TargetKinds = Map.ofList [ "src-home", Source ]
            }
            |> withHome (fun layer ->
                { layer with
                    Terrain = Map.ofList (corridor 10 1 40)
                    TargetPositions = Map.ofList [ "src-home", homeSource ]
                    CreepPositions = Map.ofList [ "w", { X = 10; Y = 2 } ]
                })
    }

/// The same colony with its outpost beside it, one room north: W1N2's
/// y = 49 row lands on W1N1's y = 0 row, and `Atlas.seams` reads that join
/// out of the two room names alone (ADR 0041) — no fixture here declares
/// an edge, because a declared edge is a second fact that can disagree
/// with the first. The outpost's corridor runs to its own y = 48, so the
/// tile a crossing lands a creep on opens onto ground.
///
/// `None` is the room before anything is laid into it: the whole of what
/// `Snapshot.projectRoom` builds for a room with no vision — its terrain
/// and its border ring, because `Game.map.getRoomTerrain` needs neither —
/// and not one entry more, because everything vision pays for is absent
/// entry by entry until vision returns (ADR 0004).
///
/// That is not the whole of what the shell hands Core for a *declared*
/// room it cannot see: `Outpost.place` lays the declared sources and
/// controller in afterwards, with no vision at all (ADR 0041, #148). So
/// this is the baseline the declaration is added to and never a blind
/// outpost as the colony really projects one — the tests below that want
/// one build it by calling `place`, as the shell does.
let private withNorthOutpost (outpostSource: Pos option) (colony: Snapshot) =
    { colony with
        Sources = colony.Sources @ [ for _ in Option.toList outpostSource -> source "src-out" ]
        Spatial =
            { colony.Spatial with
                Borders = Map.add "W1N2" plainRing colony.Spatial.Borders
                TargetKinds =
                    match outpostSource with
                    | Some _ -> Map.add "src-out" Source colony.Spatial.TargetKinds
                    | None -> colony.Spatial.TargetKinds
            }
            |> withNeighbour
                "W1N2"
                { RoomLayer.empty with
                    Terrain = Map.ofList (corridor 10 40 48)
                    TargetPositions =
                        outpostSource
                        |> Option.map (fun pos -> Map.ofList [ "src-out", pos ])
                        |> Option.defaultValue Map.empty
                }
    }

/// The same colony with a construction site of ours standing in the
/// outpost — the one the container rule places there and nothing else,
/// because that rule is the only thing that places outside the home room
/// (ADR 0042). It arrives in the three pieces the shell hands Core it in:
/// the id-keyed kind census, the outpost layer's own tile, and the
/// `ConstructionSites` entry vision pays for (#150). Merges into whatever
/// layer `withNorthOutpost` already laid, so the two compose in either
/// order.
let private withOutpostSite (site: Pos) (colony: Snapshot) =
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        ConstructionSites = colony.ConstructionSites @ [ { Id = "site-out" } ]
        Spatial =
            { colony.Spatial with
                TargetKinds =
                    Map.add "site-out" (Site BuiltKind.Container) colony.Spatial.TargetKinds
            }
            |> withNeighbour
                "W1N2"
                { outpost with
                    TargetPositions = Map.add "site-out" site outpost.TargetPositions
                }
    }

/// The same worker, carrying a full load: Harvest asks for free capacity
/// and Build asks for carried energy (`applicable`), so a full worker
/// leaves the home source's Task inapplicable and is matched over a pool
/// whose one candidate is the Build — pairwise by construction, with no
/// third rival standing in for either side of a comparison.
let private loaded (colony: Snapshot) =
    { colony with
        Creeps = [ worker "w" 50 0 ]
    }

/// What the tick decided, less the plan memo: the memo carries a mutable
/// walk table whose identity is not a decision, and these three are the
/// whole of what leaves the colony.
let private outcomeOf (colony: Snapshot) =
    let decision = decide colony Map.empty Set.empty None
    decision.Intents, decision.Assignments, decision.Verdicts

/// Which Task won the one worker, and what separated it from its closest
/// rival (ADR 0009's Matched Verdict).
let private matchOf (colony: Snapshot) =
    let { Verdicts = verdicts } = decide colony Map.empty Set.empty None

    verdicts
    |> List.tryPick (function
        | Verdict.Matched("w", task, factor) -> Some(task, factor)
        | _ -> None)

/// The same colony with its own controller standing where the caller puts
/// it: the rival every loaded worker in the real colony always has, and
/// the one the fixtures above leave out so that their Matched factor can
/// name a single comparison. Level 2 and far from its downgrade deadline,
/// so nothing here is ADR 0007's deadline rank in disguise.
let private withHomeController (pos: Pos) (colony: Snapshot) =
    { colony with
        Controller = Some(controllerAt 2)
        Spatial =
            { colony.Spatial with
                TargetKinds = Map.add "ctrl-1" TargetKind.Controller colony.Spatial.TargetKinds
            }
            |> withHome (fun layer ->
                { layer with
                    TargetPositions = Map.add "ctrl-1" pos layer.TargetPositions
                })
    }

[<Tests>]
let outpostTests =
    testList
        "outposts"
        [
            // ADR 0041's central claim, at the seam it is claimed on: an
            // outpost's Task is not steered to the front of the pool or to
            // the back of it, it is ranked. Both Harvests sit on the
            // feeding tier, so what separates them is travel cost — and
            // travel cost crosses the Seam since #123, which is what makes
            // the outpost's Task comparable at all rather than a special
            // case somewhere ahead of the ranking.
            //
            // Pairwise, one rival at a time: this pool holds these two
            // Tasks and nothing else, so the factor a Matched Verdict
            // reports is about this pair and no third candidate stands in
            // for either of them.
            //
            // The ranking and deliberately not the tick that follows it:
            // this test reads the Verdict, which is the half ADR 0041
            // delivers. What the winner does with the tick is #142's, and
            // the case below it drives that.
            test "an outpost Harvest and a home Harvest are ranked in one pool" {
                Expect.equal
                    (matchOf (
                        northBorderColony { X = 10; Y = 38 }
                        |> withNorthOutpost (Some { X = 10; Y = 46 })
                    ))
                    (Some(taskId (Harvest "src-out"), MatchFactor.TravelCost))
                    "the outpost source is the nearer of the two, across the Seam"

                // The same fixture with the two sources swapped over: only
                // how far each one is moves, and the ranking moves with it.
                Expect.equal
                    (matchOf (
                        northBorderColony { X = 10; Y = 4 }
                        |> withNorthOutpost (Some { X = 10; Y = 41 })
                    ))
                    (Some(taskId (Harvest "src-home"), MatchFactor.TravelCost))
                    "the home source is the nearer of the two, and wins the same comparison"
            }

            test "the winner of that comparison is walked toward the Seam, tick after tick" {
                // #142's reproduction, at the seam it was reproduced on.
                // Before it, this fixture answered `Matched ("w",
                // "harvest:src-out", TravelCost)` and then a lone
                // `SayCreep`: the Task had a price and no step, so the
                // creep stood still, said its glyph, and anti-thrash kept
                // it there for the rest of its life — having given up the
                // home source it would otherwise have dug.
                //
                // Now the mover aims at the near side of the crossing the
                // price was paid at. That tile is in the creep's own room,
                // so nothing here is arbitrated across the border: the
                // Resolver settles a step of this room exactly as it always
                // has. This band is plain the whole way round and the
                // corridor meets it at x = 10, so three crossings — x = 9,
                // 10 and 11 — cost this creep the same to the tick, and the
                // band's minimum takes the lowest (X, Y) of them as every
                // other tie in the Atlas is taken. The creep therefore
                // leaves the corridor diagonally, which the engine allows
                // onto an exit exactly as it allows anywhere else.
                let colonyAt pos =
                    let colony =
                        northBorderColony { X = 10; Y = 38 }
                        |> withNorthOutpost (Some { X = 10; Y = 46 })

                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w", pos ]
                                })
                    }

                let assigned = Map.ofList [ "w", taskId (Harvest "src-out") ]

                let {
                        Intents = opening
                        Assignments = assignments
                    } =
                    decide (colonyAt { X = 10; Y = 2 }) Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w" assignments)
                    (Some(taskId (Harvest "src-out")))
                    "the premise: the outpost's Harvest wins the one worker"

                Expect.equal
                    (moveIntents opening)
                    [ "w", Top ]
                    "and it is walked up its own corridor toward the border, not parked on the Task"

                Expect.isEmpty
                    (actionIntents opening)
                    "it may not dig a source a room away, however well priced (ADR 0041)"

                // Driven the way the engine drives it: the creep stands
                // where the last tick's Intent put it, its Assignment handed
                // back, until the step it is given leaves this room's
                // ground — the tick it crosses.
                let ground = Map.ofList (corridor 10 1 40)

                let rec drive pos walked =
                    if List.length walked > 10 then
                        failtest "the creep never reached a crossing"
                    else
                        let { Intents = intents } = decide (colonyAt pos) assigned Set.empty None

                        match moveIntents intents with
                        | [ _, direction ] ->
                            let next = stepFrom pos direction

                            if Map.containsKey next ground then
                                drive next (next :: walked)
                            else
                                List.rev (next :: walked)
                        | _ -> List.rev walked

                Expect.equal
                    (drive { X = 10; Y = 2 } [])
                    [ { X = 10; Y = 1 }; { X = 9; Y = 0 } ]
                    "one tile up the corridor, then onto the exit the price was paid at"
            }

            test "and the tick after the crossing is the far room's: the landed creep walks on" {
                // Where the drive above hands the creep to the engine, and
                // what takes it from there. The engine lifts the creep off
                // (9,0) and files it in W1N2 on that room's border row, and
                // from that tick the Resolver arbitrates W1N2 as a room of
                // its own (#145): its occupants, its blocked tiles and its
                // Move Intents, over that room's tiles and no other's, so
                // the creep that landed gets a step exactly as one standing
                // at home does. Before #145 the far side was deferred, and
                // this case asserted the creep standing on its landing tile
                // holding its Task against anti-thrash, saying its glyph —
                // the trace #142 quotes, one tile past the border.
                //
                // The landing tile is not ground — the ring is no room's
                // floor (ADR 0036) — and the tile beside it is; the mover
                // answers from both, because a flood seeds its start tile
                // whatever that tile's weight, and steps off it onto the
                // room's own ground.
                let landedAt pos =
                    let colony =
                        northBorderColony { X = 10; Y = 38 }
                        |> withNorthOutpost (Some { X = 10; Y = 46 })

                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.empty
                                })
                            |> withNeighbour
                                "W1N2"
                                { RoomLayer.empty with
                                    Terrain = Map.ofList (corridor 10 40 48)
                                    TargetPositions = Map.ofList [ "src-out", { X = 10; Y = 46 } ]
                                    CreepPositions = Map.ofList [ "w", pos ]
                                }
                    }

                let assigned = Map.ofList [ "w", taskId (Harvest "src-out") ]

                // Nothing else leaves the colony on these ticks, and one
                // absence is worth naming: this outpost carries no
                // `RoomControl` entry, so it is a room the colony is not
                // looking into, and ADR 0042's container rule plans into
                // no such room (`planOutpostContainers`). Give the fixture
                // vision and a placement Intent joins the lines below.
                //
                // The tile the crossing above delivers to, two more ring and
                // ground tiles at the top of the corridor, and one a step
                // from the Work Area: each is walked toward the source.
                for pos, expected in
                    [
                        { X = 9; Y = 49 }, TopRight
                        { X = 10; Y = 49 }, Top
                        { X = 10; Y = 48 }, Top
                        { X = 10; Y = 44 }, Bottom
                    ] do
                    let landed = landedAt pos

                    let {
                            Intents = intents
                            Verdicts = verdicts
                        } =
                        decide landed assigned Set.empty None

                    Expect.equal
                        intents
                        [ SayCreep("w", "⛏"); MoveCreep("w", expected) ]
                        $"out of {pos.X},{pos.Y} the creep is walked toward the source, and may not dig yet"

                    Expect.equal
                        verdicts
                        [ Verdict.Kept("w", taskId (Harvest "src-out")) ]
                        "and anti-thrash keeps the Task it is now walking to"

                // Driven the way the engine drives it, from the landing
                // tile: two steps up the corridor and the dig begins.
                let rec drive pos walked =
                    if List.length walked > 10 then
                        failtest "the creep never started digging"
                    else
                        let { Intents = intents } = decide (landedAt pos) assigned Set.empty None

                        match actionIntents intents, moveIntents intents with
                        | [ HarvestSource("w", "src-out") ], [] -> List.rev walked, pos
                        | [], [ _, direction ] ->
                            let next = stepFrom pos direction
                            drive next (next :: walked)
                        | _ -> failtest $"at {pos.X},{pos.Y} the tick neither walked nor dug"

                Expect.equal
                    (drive { X = 9; Y = 49 } [])
                    ([ { X = 10; Y = 48 }; { X = 10; Y = 47 } ], { X = 10; Y = 47 })
                    "off the landing tile onto the corridor, up to the seat, and the source is dug from there"
            }

            test "our site in the outpost is a Build the one pool holds" {
                // #150's reproduction, at the seam it was reproduced on.
                // The container rule placed a site in the outpost, saw it
                // standing on the next tick and correctly declined to place
                // a second — and nothing ever built the first, because the
                // Build pool is `Snapshot.ConstructionSites` mapped one to
                // one and that list was the spawn rooms' alone. A container
                // that is never built is a source that never becomes a
                // Post, so ADR 0042's switch could not close.
                //
                // Nothing in the Build path is outpost-shaped: the Task
                // names the site by id, its Work Area is the site's own
                // room's (ADR 0041, ADR 0020) and its price sums the legs
                // over the Seam (#123), exactly as the outpost Harvest
                // above does. What was missing was the entry, and this is
                // the entry.
                let sited =
                    northBorderColony { X = 10; Y = 38 }
                    |> withNorthOutpost None
                    |> withOutpostSite { X = 10; Y = 43 }
                    |> loaded

                Expect.equal
                    (matchOf sited)
                    (Some(taskId (Build "site-out"), MatchFactor.OnlyCandidate))
                    "the site a room away is a Task this worker is given"

                // The pool is that list and never the kind census: a site
                // the projection places but the shell did not hand over —
                // which is every site in a room the colony cannot see this
                // tick (ADR 0004) — names no Task at all.
                Expect.equal
                    (matchOf { sited with ConstructionSites = [] })
                    None
                    "and a site the Snapshot does not carry is no Task, however well the projection places it"

                // The memo *does* flinch at it, and this is the tick that
                // changed (#169). #121 and #149 left the `pending` half
                // joined against the home layer alone because nothing the
                // memo carried read a site outside home — this rule's own
                // site least of all, since it is recomputed every tick (ADR
                // 0042) — and the throw-away it saved was one Layout and
                // one spawn walk table on the tick the site appeared. The
                // walk table's far leg is now a memo entry over the *goal*
                // room's weight grid, and an obstacle-kind site closes its
                // tile in whatever room it stands in (`projectVisible`), so
                // a pending census stopping at the home layer is ADR 0017's
                // signature gap out there. Signing the half whole rather
                // than only its blocking kinds keeps one rule instead of a
                // second asymmetry to hold in step with the App's obstacle
                // filter; the price is exactly the throw-away above, on the
                // handful of ticks in a colony's life that an outpost
                // container site appears.
                Expect.notEqual
                    (censusSignature sited)
                    (censusSignature (
                        northBorderColony { X = 10; Y = 38 } |> withNorthOutpost None |> loaded
                    ))
                    "an outpost's pending site is a census entry of its own since #169"

                let { Intents = opening } = decide sited Map.empty Set.empty None

                Expect.equal
                    (moveIntents opening)
                    [ "w", Top ]
                    "so the worker is walked up its own corridor toward the border"

                Expect.isEmpty
                    (actionIntents opening)
                    "and may not build a site a room away, however well priced (ADR 0041)"
            }

            test "and the worker that landed in the outpost builds it" {
                // The far half of the same walk, driven the way the engine
                // drives it: #145 arbitrates the outpost as a room of its
                // own, so the creep the engine put down on W1N2's border
                // row gets a step off the ring exactly as one standing at
                // home does, and the tick it stands inside the site's Work
                // Area — build reaches three tiles — the Intent it has
                // been walking toward is emitted.
                //
                // A slow answer and the right one (ADR 0042): whoever
                // holds this Task spends five hits a tick per Work part
                // into a 5,000-hit container. There is no outpost builder
                // row, and this ticket invents none — which creep holds it
                // is the ranking's answer, pinned in the test below.
                let landedAt pos =
                    let colony =
                        northBorderColony { X = 10; Y = 38 }
                        |> withNorthOutpost None
                        |> withOutpostSite { X = 10; Y = 43 }
                        |> loaded

                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.empty
                                })
                            |> withNeighbour
                                "W1N2"
                                { SpatialInfo.layerOf colony.Spatial "W1N2" with
                                    CreepPositions = Map.ofList [ "w", pos ]
                                }
                    }

                let assigned = Map.ofList [ "w", taskId (Build "site-out") ]

                let {
                        Intents = landing
                        Verdicts = verdicts
                    } =
                    decide (landedAt { X = 9; Y = 49 }) assigned Set.empty None

                Expect.equal
                    landing
                    [ SayCreep("w", "🔨"); MoveCreep("w", TopRight) ]
                    "off the landing tile onto the corridor, and no build from a tile out of range"

                Expect.equal
                    verdicts
                    [ Verdict.Kept("w", taskId (Build "site-out")) ]
                    "and anti-thrash keeps the Task it is now walking to"

                let rec drive pos walked =
                    if List.length walked > 10 then
                        failtest "the creep never started building"
                    else
                        let { Intents = intents } = decide (landedAt pos) assigned Set.empty None

                        match actionIntents intents, moveIntents intents with
                        | [ BuildSite("w", "site-out") ], [] -> List.rev walked, pos
                        | [], [ _, direction ] ->
                            let next = stepFrom pos direction
                            drive next (next :: walked)
                        | _ -> failtest $"at {pos.X},{pos.Y} the tick neither walked nor built"

                Expect.equal
                    (drive { X = 9; Y = 49 } [])
                    ([ { X = 10; Y = 48 }; { X = 10; Y = 47 }; { X = 10; Y = 46 } ],
                     { X = 10; Y = 46 })
                    "off the ring onto the corridor, down to build range, and the container rises"
            }

            test "the site outranks the home Upgrade: a loaded worker crosses the Seam for it" {
                // #157, and the reverse of what this very fixture asserted
                // before it. The two cases above hold one worker and no
                // controller, which is what let their factor name the
                // Build's one rival; the colony that really exists has a
                // controller, and while the site was surplus work that
                // controller took every loaded worker every tick. Build and
                // Upgrade shared the surplus tier, so nothing but travel
                // cost separated them, and a loaded worker standing at home
                // is a corridor from its own controller and a Seam plus
                // fifty tiles from the site.
                //
                // Deployed, that was ADR 0042's switch laid down and never
                // closed: the reserver went out (#131), the site went up
                // (#128), and nobody ever built it. #150's answer here —
                // that the builder would be a creep which had walked out
                // for this room's own Harvest and filled up there — never
                // happened either, because the Storage's Withdraw is
                // feeding tier and a few tiles from home while the
                // cross-Seam Harvest is fifty, so no worker made the trip
                // to fill up out there in the first place.
                //
                // So this Build is feeding tier now (`tierOf`): it decides
                // whether the room is in the economy at all, which is the
                // same kind of question the Reserve beside it settles about
                // the rate. The factor is `Rank` and deliberately not
                // `TravelCost` — the site is still much the farther of the
                // two targets and wins anyway. Pairwise, one rival at a
                // time: one Build, one Upgrade, and the home Harvest
                // inapplicable to a body with nothing free to fill.
                let sited =
                    northBorderColony { X = 10; Y = 38 }
                    |> withNorthOutpost None
                    |> withOutpostSite { X = 10; Y = 43 }
                    |> loaded
                    |> withHomeController { X = 10; Y = 5 }

                Expect.equal
                    (matchOf sited)
                    (Some(taskId (Build "site-out"), MatchFactor.Rank))
                    "the switch outranks the sink, however much nearer the sink stands"

                let { Intents = opening } = decide sited Map.empty Set.empty None

                Expect.equal
                    (moveIntents opening)
                    [ "w", Top ]
                    "and the worker is walked up its own corridor toward the Seam it has to cross"

                Expect.isEmpty
                    (actionIntents opening)
                    "having neither built a site a room away nor upgraded the controller beside it"
            }

            test "and the worker already in the outpost still builds it" {
                // The other half of the same colony, unmoved by #157: a
                // creep standing in the outpost is nearer the site than
                // anything at home, so it held this Task on the surplus
                // tier and holds it on the feeding one. What changed is
                // that it is no longer the *only* creep that ever could.
                let landed =
                    let colony =
                        northBorderColony { X = 10; Y = 38 }
                        |> withNorthOutpost None
                        |> withOutpostSite { X = 10; Y = 43 }
                        |> loaded
                        |> withHomeController { X = 10; Y = 5 }

                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.empty
                                })
                            |> withNeighbour
                                "W1N2"
                                { SpatialInfo.layerOf colony.Spatial "W1N2" with
                                    CreepPositions = Map.ofList [ "w", { X = 10; Y = 46 } ]
                                }
                    }

                Expect.equal
                    (matchOf landed)
                    (Some(taskId (Build "site-out"), MatchFactor.Rank))
                    "the creep out there builds, as it did before the tier moved"
            }

            test "a hungry extension still comes first: same tier, and the nearer target wins" {
                // ADR 0010's layering is untouched by #157, and this is
                // what keeps a starving spawn from waiting on a container
                // fifty tiles away with no special case written for it. The
                // spawn and the extensions were always on the feeding tier
                // and the outpost's site has joined them, so what separates
                // the two is travel cost — and a hungry extension underfoot
                // is nearer than a site across a Seam, every time.
                //
                // Pairwise, one rival at a time: no controller in this
                // fixture, so the pool is the Build, the Refill and a home
                // Harvest a full body cannot take.
                let sited =
                    northBorderColony { X = 10; Y = 38 }
                    |> withNorthOutpost None
                    |> withOutpostSite { X = 10; Y = 43 }
                    |> loaded

                Expect.equal
                    (matchOf sited)
                    (Some(taskId (Build "site-out"), MatchFactor.OnlyCandidate))
                    "the premise: with nothing at home to fill, this worker crosses for the site"

                let hungry =
                    { sited with
                        Refillables = [ refillable "ext-1" 50 BuiltKind.Extension ]
                        Spatial =
                            { sited.Spatial with
                                TargetKinds =
                                    Map.add
                                        "ext-1"
                                        (Structure BuiltKind.Extension)
                                        sited.Spatial.TargetKinds
                            }
                            |> withHome (fun layer ->
                                { layer with
                                    TargetPositions =
                                        Map.add "ext-1" { X = 10; Y = 3 } layer.TargetPositions
                                })
                    }

                Expect.equal
                    (matchOf hungry)
                    (Some(taskId (Refill "ext-1"), MatchFactor.TravelCost))
                    "and one extension with room in it takes the same worker back, on cost alone"
            }

            test "a home container site is surplus still: the room is what makes one a switch" {
                // The half of #157 that must not move. What makes the
                // outpost's site a switch is the room it stands in and not
                // the kind it is, and a `Pos` carries no room (ADR 0041) —
                // so a reading that went by the kind census alone would
                // lift every container the Layout ever places (ADR 0040)
                // onto the feeding tier and pull the whole worker row off
                // the controller with it.
                //
                // Discriminating by construction: the home site is the
                // farther of the two targets and the controller the nearer,
                // so on the surplus tier they share the controller wins on
                // cost — and read as a switch the site would have won on
                // rank instead, which is exactly the failure this pins.
                // Pairwise: one Build, one Upgrade.
                let homeSite =
                    let colony =
                        northBorderColony { X = 10; Y = 38 }
                        |> loaded
                        |> withHomeController { X = 10; Y = 5 }

                    { colony with
                        ConstructionSites = [ { Id = "site-home" } ]
                    }
                    |> withTarget "site-home" { X = 10; Y = 30 } (Site BuiltKind.Container)

                Expect.equal
                    (matchOf homeSite)
                    (Some(taskId (Upgrade "ctrl-1"), MatchFactor.TravelCost))
                    "the colony's own container site shares Upgrade's tier and loses on distance"
            }

            test "two builders cross for the site, and the third stays home" {
                // The cap `taskCapacities` puts on this Build (#157). On
                // the feeding tier the site outbids the home Upgrade for
                // every loaded worker at once, and travel cost cannot thin
                // that crowd — a Seam away is a Seam away from every tile
                // of one corridor. Uncapped, the whole worker row walks out
                // together and the home room stops working for the fifty
                // ticks each of them spends crossing.
                //
                // Two is a tunable and the third worker is what reads it:
                // rejected as capacity-full, it falls to the Upgrade it
                // would have taken anyway. Asserted as the whole tally, so
                // a cap that admitted all three or only one both fail.
                // Two is the whole colony's budget and not this site's
                // alone — one site standing is what makes the two numbers
                // agree here; the test below opens a second site and reads
                // them apart.
                let crowd =
                    let colony =
                        northBorderColony { X = 10; Y = 38 }
                        |> withNorthOutpost None
                        |> withOutpostSite { X = 10; Y = 43 }
                        |> withHomeController { X = 10; Y = 5 }

                    { colony with
                        Creeps = [ for name in [ "w1"; "w2"; "w3" ] -> worker name 50 0 ]
                        Spatial =
                            colony.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [
                                                "w1", { X = 10; Y = 2 }
                                                "w2", { X = 10; Y = 3 }
                                                "w3", { X = 10; Y = 4 }
                                            ]
                                })
                    }

                let { Assignments = assignments } = decide crowd Map.empty Set.empty None

                Expect.equal
                    (assignments |> Map.toList |> List.map snd |> List.countBy id |> List.sort)
                    [ taskId (Build "site-out"), 2; taskId (Upgrade "ctrl-1"), 1 ]
                    "two of the three hold the site, and the one left over upgrades"
            }

            test "the whole ring closes at `decide`: cross, build it empty, dig it full, build on" {
                // ADR 0042's switch closing under its own power, end to
                // end and with no new concept in it (#157) — the loop the
                // ticket asks for, driven one tick at a time over the one
                // seam this repo decides at.
                //
                // The colony that really exists: a controller at home, a
                // rock and a container site in the outpost, and one loaded
                // worker standing at home. It crosses because the site now
                // outranks the controller (`tierOf`); it builds until the
                // build empties it; emptied, the Build goes inapplicable
                // and the outpost's own rock — a step away, feeding tier —
                // is the cheapest Task it has (`applicable`, ADR 0013);
                // full again, the site outranks everything once more. No
                // "go home" act and no outpost builder row: the ring is
                // the ordinary ranking, turning.
                //
                // Driven the way the engine drives it: this tick's
                // Assignments handed back as the next tick's, a Move
                // Intent stepped, and a step onto the exit row handed over
                // to the neighbour's own border row, which is exactly what
                // the engine does with a creep that ends its tick there
                // (ADR 0036, #145). What a build spends and a dig collects
                // is the engine's arithmetic and not this seam's, so the
                // two act on the store at their limits — emptied, filled —
                // which is the state the ring turns on.
                let colonyAt room pos carrying =
                    let colony =
                        northBorderColony { X = 10; Y = 38 }
                        |> withNorthOutpost (Some { X = 10; Y = 46 })
                        |> withOutpostSite { X = 10; Y = 44 }
                        |> withHomeController { X = 10; Y = 5 }

                    let standing (name: string) (layer: RoomLayer) =
                        { layer with
                            CreepPositions =
                                if name = room then Map.ofList [ "w", pos ] else Map.empty
                        }

                    { colony with
                        Creeps = [ (if carrying then worker "w" 50 0 else worker "w" 0 50) ]
                        Spatial =
                            colony.Spatial
                            |> withHome (standing "W1N1")
                            |> withNeighbour
                                "W1N2"
                                (SpatialInfo.layerOf colony.Spatial "W1N2" |> standing "W1N2")
                    }

                let rec drive (room, pos, carrying) assigned trail ticks =
                    if ticks = 0 then
                        List.rev trail
                    else
                        let {
                                Intents = intents
                                Assignments = next
                            } =
                            decide (colonyAt room pos carrying) assigned Set.empty None

                        let step state acted =
                            drive state next (acted :: trail) (ticks - 1)

                        match actionIntents intents, moveIntents intents with
                        | [ BuildSite("w", "site-out") ], [] -> step (room, pos, false) "build"
                        | [ HarvestSource("w", "src-out") ], [] -> step (room, pos, true) "harvest"
                        | [], [ _, direction ] ->
                            let next = stepFrom pos direction

                            // The engine's own handover: a creep ending its
                            // tick on the exit row is lifted into the
                            // neighbour and filed on that room's opposite
                            // border row, same column.
                            if room = "W1N1" && next.Y = 0 then
                                step ("W1N2", { next with Y = 49 }, carrying) "cross"
                            else
                                step (room, next, carrying) "walk"
                        | actions, moves ->
                            failtest $"in {room} at {pos.X},{pos.Y}: {actions} and {moves}"

                Expect.equal
                    (drive ("W1N1", { X = 10; Y = 2 }, true) Map.empty [] 10)
                    [
                        "walk"
                        "cross"
                        "walk"
                        "walk"
                        "build"
                        "harvest"
                        "build"
                        "harvest"
                        "build"
                        "harvest"
                    ]
                    "up the corridor, over the Seam, down to the site — and then the ring turns"
            }

            test "the switch is light bodies' work: a full Anchor stays on its Post" {
                // What the feeding tier took away and `applicable` gives
                // back (#157). Travel cost was the only thing keeping a
                // heavy body off a distant site — `applicable`'s own doc
                // says so, "Travel cost pins an Anchor that is at its
                // Post" — and a rank the whole colony shares is exactly
                // what travel cost cannot answer. A full Anchor whose Post
                // carries no standing container yet loses Harvest
                // (`garrisons`), and was then outranked off its own
                // controller and walked fifty tiles at four to seven ticks
                // a step to spend one Carry into a 5,000-progress site,
                // burning a builder place while it went. A heavy body's
                // cross-room work is a Post (ADR 0020), so the gate is
                // ADR 0016's shape: this one Build is inapplicable to it.
                //
                // The two bodies stand on the same tile in the same
                // colony, so what tells them apart is the body and
                // nothing geometric.
                let sited body =
                    let colony =
                        northBorderColony { X = 10; Y = 38 }
                        |> withNorthOutpost None
                        |> withOutpostSite { X = 10; Y = 43 }
                        |> withHomeController { X = 10; Y = 5 }

                    { colony with
                        Creeps = [ body "w" 50 0 ]
                    }

                Expect.equal
                    (matchOf (sited worker))
                    (Some(taskId (Build "site-out"), MatchFactor.Rank))
                    "the premise: a generalist is walked over the Seam for the site"

                Expect.equal
                    (matchOf (sited anchor))
                    (Some(taskId (Upgrade "ctrl-1"), MatchFactor.OnlyCandidate))
                    "and the Anchor beside it has no such Task at all: it spends its Carry where it stands"

                Expect.isEmpty
                    (moveIntents (decide (sited anchor) Map.empty Set.empty None).Intents)
                    "and takes no step toward a border it would spend hundreds of ticks crossing"
            }

            test "what shares this tier and what only looks like it does" {
                // The half of #157's Implementation decisions that is not
                // true as the ticket wrote it, pinned as it really is. The
                // ticket said "Refill still comes first — the home
                // extension / tower is nearer, cost decides"; ADR 0010 is
                // the authority and it puts a **tower** Refill in the
                // surplus tier, not the feeding one, so cost never gets
                // asked. Two pairwise cases, one rival each.
                let sited =
                    northBorderColony { X = 10; Y = 38 }
                    |> withNorthOutpost None
                    |> withOutpostSite { X = 10; Y = 43 }

                // A tower with 500 free, three tiles from the worker,
                // against a site a Seam and fifty tiles away. It loses on
                // rank and distance is never reached — which is ADR 0010's
                // own "a colony feeds its own reproduction before its
                // guns" with this Build counted as reproduction, and a
                // real change of behaviour under a raid at home. The
                // answer for a raid is the stand-down (ADR 0043, #136).
                let tower =
                    { (sited |> loaded) with
                        Refillables = [ refillable "tower-1" 500 BuiltKind.Tower ]
                    }
                    |> withTarget "tower-1" { X = 10; Y = 3 } (Structure BuiltKind.Tower)

                Expect.equal
                    (matchOf tower)
                    (Some(taskId (Build "site-out"), MatchFactor.Rank))
                    "a hungry tower is surplus work and is outranked outright, not beaten on distance"

                // The spawn does share the tier, so cost decides — and
                // cost is answered from where the creep stands. For the
                // one the cap has already parked in the outpost the
                // nearer target is the site, not the spawn: the home room
                // feeds itself through the creeps standing in it and not
                // by any rule.
                let outThere =
                    let colony =
                        { (sited |> loaded) with
                            Refillables = [ refillable "spawn-1" 300 BuiltKind.Spawn ]
                        }
                        |> withTarget "spawn-1" { X = 10; Y = 2 } (Structure BuiltKind.Spawn)

                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.empty
                                })
                            |> withNeighbour
                                "W1N2"
                                { SpatialInfo.layerOf colony.Spatial "W1N2" with
                                    CreepPositions = Map.ofList [ "w", { X = 10; Y = 44 } ]
                                }
                    }

                Expect.equal
                    (matchOf outThere)
                    (Some(taskId (Build "site-out"), MatchFactor.TravelCost))
                    "and a hungry spawn does share it, so the creep already out there builds on rather than walking home"
            }

            test "the two builders are the colony's budget, not each site's" {
                // `taskCapacities`' cap read at colony scale (#157).
                // `planOutpostContainers` places one site per unserved
                // outpost source and places them all on the same tick, so
                // a per-site two over the declaration's three sources is a
                // colony-wide six — the whole worker row, which is the one
                // thing the cap exists to prevent. Spread instead: two
                // sites take one apiece, and the case above, with one site
                // standing, still takes two.
                let crowd =
                    let colony =
                        northBorderColony { X = 10; Y = 38 }
                        |> withNorthOutpost None
                        |> withOutpostSite { X = 10; Y = 43 }
                        |> withHomeController { X = 10; Y = 5 }

                    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

                    { colony with
                        ConstructionSites = colony.ConstructionSites @ [ { Id = "site-out2" } ]
                        Creeps = [ for n in 1..5 -> worker $"w{n}" 50 0 ]
                        Spatial =
                            { colony.Spatial with
                                TargetKinds =
                                    Map.add
                                        "site-out2"
                                        (Site BuiltKind.Container)
                                        colony.Spatial.TargetKinds
                            }
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ for n in 1..5 -> $"w{n}", { X = 10; Y = n + 1 } ]
                                })
                            |> withNeighbour
                                "W1N2"
                                { outpost with
                                    TargetPositions =
                                        Map.add
                                            "site-out2"
                                            { X = 10; Y = 45 }
                                            outpost.TargetPositions
                                }
                    }

                let { Assignments = assignments } = decide crowd Map.empty Set.empty None

                Expect.equal
                    (assignments |> Map.toList |> List.map snd |> List.countBy id |> List.sort)
                    [
                        taskId (Build "site-out"), 1
                        taskId (Build "site-out2"), 1
                        taskId (Upgrade "ctrl-1"), 3
                    ]
                    "two switches open take one builder each, and three of the five stay home"
            }

            test
                "the two rooms are arbitrated apart: a neighbour's creep holds no tile of this room" {
                // #142's acceptance criterion 5, at the seam it is decided
                // on. Each room's arbitration reads that room's creeps and
                // no other's (#145): a `Map<Pos, string>` of occupants has
                // no room on its key, so a creep standing on the same
                // coordinate of the neighbouring room is not an occupant
                // here, is not displaced by this room's travellers, and
                // attributes nothing. Pairwise, one rival at a time — the
                // home traveller and one creep on its next tile, first in
                // the neighbour, then at home — because a pool holding
                // both proves nothing about which of them the traveller
                // was settled against.
                //
                // The bystanders are full, so no Harvest applies to them
                // and they park where they stand, displaceable to any
                // adjacent tile of their own room.
                let colony (homeCreeps: (string * Pos) list) (outpostCreeps: (string * Pos) list) =
                    let colonyOf =
                        northBorderColony { X = 10; Y = 38 }
                        |> withNorthOutpost (Some { X = 10; Y = 46 })

                    { colonyOf with
                        Creeps =
                            worker "w" 0 50
                            :: [ for name, _ in homeCreeps @ outpostCreeps -> worker name 50 0 ]
                        Spatial =
                            colonyOf.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList (("w", { X = 10; Y = 4 }) :: homeCreeps)
                                })
                            // The outpost's corridor runs the whole column
                            // here, so the coordinate the rival stands on is
                            // ground in both rooms and the case is about the
                            // room, never about a tile nobody could stand on.
                            |> withNeighbour
                                "W1N2"
                                { RoomLayer.empty with
                                    Terrain = Map.ofList (corridor 10 1 48)
                                    TargetPositions = Map.ofList [ "src-out", { X = 10; Y = 46 } ]
                                    CreepPositions = Map.ofList outpostCreeps
                                }
                    }

                // The home source is the rival this time, at (10,38) with
                // the worker at (10,4) walking down to it, a step at a time
                // — its next tile is (10,5).
                let assigned = Map.ofList [ "w", taskId (Harvest "src-home") ]

                let outcome homeCreeps outpostCreeps =
                    let {
                            Intents = intents
                            Verdicts = verdicts
                        } =
                        decide (colony homeCreeps outpostCreeps) assigned Set.empty None

                    moveIntents intents,
                    verdicts
                    |> List.filter (function
                        | Verdict.Yielded _
                        | Verdict.Grounded _ -> true
                        | _ -> false)

                Expect.equal
                    (outcome [] [ "o", { X = 10; Y = 5 } ])
                    ([ "w", Bottom ], [])
                    "a neighbour's creep on the next tile's coordinate is no occupant: the traveller steps, nobody yields"

                Expect.equal
                    (outcome [ "h", { X = 10; Y = 5 } ] [])
                    ([ "w", Bottom; "h", Top ], [ Verdict.Yielded("h", "w") ])
                    "a home creep on the next tile is displaced — swapped past the traveller — and yields, as it always has"

                Expect.equal
                    (outcome [ "h", { X = 10; Y = 5 } ] [ "o", { X = 10; Y = 5 } ])
                    ([ "w", Bottom; "h", Top ], [ Verdict.Yielded("h", "w") ])
                    "and the neighbour's creep on the same coordinate changes neither the moves nor the attribution"
            }

            test
                "a grounded creep in the neighbouring room is grounded there, and pre-claims nothing here" {
                // ADR 0008 in the far room: a fatigued creep sits its own
                // room's arbitration out, so it is reported Grounded — the
                // Verdict it was denied while only home was arbitrated —
                // and its tile is blocked in its room only. The home
                // traveller whose next tile shares that coordinate steps
                // regardless; before #145 this half held by the creep not
                // being arbitrated at all, now it holds by the room.
                let colony =
                    let colonyOf =
                        northBorderColony { X = 10; Y = 38 }
                        |> withNorthOutpost (Some { X = 10; Y = 46 })

                    { colonyOf with
                        Creeps = [ worker "w" 0 50; { worker "o" 50 0 with Fatigue = 4 } ]
                        Spatial =
                            colonyOf.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w", { X = 10; Y = 4 } ]
                                })
                            |> withNeighbour
                                "W1N2"
                                { RoomLayer.empty with
                                    Terrain = Map.ofList (corridor 10 1 48)
                                    TargetPositions = Map.ofList [ "src-out", { X = 10; Y = 46 } ]
                                    CreepPositions = Map.ofList [ "o", { X = 10; Y = 5 } ]
                                }
                    }

                let {
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decide colony (Map.ofList [ "w", taskId (Harvest "src-home") ]) Set.empty None

                Expect.equal
                    (moveIntents intents)
                    [ "w", Bottom ]
                    "the home traveller steps onto (10,5) of its own room"

                Expect.contains
                    verdicts
                    (Verdict.Grounded "o")
                    "and the tired creep in the neighbour is grounded there"

                Expect.isEmpty
                    (verdicts
                     |> List.filter (function
                         | Verdict.Yielded _ -> true
                         | _ -> false))
                    "nobody yields to a creep a room away"
            }

            test "a creep on the far room's ring is never settled there: it walks inward" {
                // The ring is no place to stay (ADR 0036, ADR 0041): a
                // creep that ends its tick on the border row is moved out
                // of the room by the engine, so a landed creep the
                // Resolver leaves standing where it is would be bounced
                // back across the border and re-cross the next tick, for
                // as long as it kept losing its step. Two cases, one
                // branch of the mover each. Parked: a full creep with
                // nothing applicable, standing on the landing tile, is
                // walked onto the outpost's ground rather than settled on
                // the ring. Travelling: two landed creeps whose cheapest
                // step is the same ground tile — the one that yields is
                // handed the other ground tile beside it, and steps off
                // the ring instead of staying on it.
                let colonyWith creeps (outpostCreeps: (string * Pos) list) =
                    let colonyOf =
                        northBorderColony { X = 10; Y = 38 }
                        |> withNorthOutpost (Some { X = 10; Y = 46 })

                    { colonyOf with
                        Creeps = creeps
                        Spatial =
                            colonyOf.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.empty
                                })
                            // One landing tile beside the corridor's top,
                            // so the ring has two ground tiles to step
                            // onto and a contested step has somewhere
                            // else to go.
                            |> withNeighbour
                                "W1N2"
                                { RoomLayer.empty with
                                    Terrain =
                                        Map.ofList (
                                            corridor 10 40 48 @ [ { X = 9; Y = 48 }, Plain ]
                                        )
                                    TargetPositions = Map.ofList [ "src-out", { X = 10; Y = 46 } ]
                                    CreepPositions = Map.ofList outpostCreeps
                                }
                    }

                let { Intents = parked } =
                    decide
                        (colonyWith [ worker "a" 50 0 ] [ "a", { X = 9; Y = 49 } ])
                        Map.empty
                        Set.empty
                        None

                Expect.equal
                    (moveIntents parked)
                    [ "a", Top ]
                    "a full creep with no Task steps off the landing tile onto the ground beside it"

                let {
                        Intents = contested
                        Verdicts = verdicts
                    } =
                    decide
                        (colonyWith
                            [ worker "a" 0 50; worker "e" 0 50 ]
                            [ "a", { X = 10; Y = 49 }; "e", { X = 9; Y = 49 } ])
                        (Map.ofList
                            [ "a", taskId (Harvest "src-out"); "e", taskId (Harvest "src-out") ])
                        Set.empty
                        None

                Expect.equal
                    (moveIntents contested)
                    [ "a", TopLeft; "e", TopRight ]
                    "both want (9,48); the first takes it and the second steps onto (10,48) rather than staying on the ring"

                Expect.contains
                    verdicts
                    (Verdict.Yielded("e", "a"))
                    "and the step it gave up is attributed as a yield, as any other is"
            }

            test "a parked outpost creep is displaced onto its own room's ground, never home's" {
                // The ticket's rule — each room's arbitration uses that
                // room's tiles and no other's — at the displacement seam:
                // the tile beside a parked creep is read off the room the
                // creep is filed under. Home has ground at (9,5) and the
                // outpost has none there, so a parked outpost creep pushed
                // off (10,5) by an outpost traveller must be swapped up
                // the corridor, never sent Left onto a coordinate that is
                // only walkable at home.
                let colony =
                    let colonyOf =
                        northBorderColony { X = 10; Y = 38 }
                        |> withNorthOutpost (Some { X = 10; Y = 46 })

                    { colonyOf with
                        Creeps = [ worker "t" 0 50; worker "o" 50 0 ]
                        Spatial =
                            colonyOf.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    Terrain =
                                        Map.ofList (corridor 10 1 40 @ [ { X = 9; Y = 5 }, Plain ])
                                    CreepPositions = Map.empty
                                })
                            |> withNeighbour
                                "W1N2"
                                { RoomLayer.empty with
                                    Terrain = Map.ofList (corridor 10 1 48)
                                    TargetPositions = Map.ofList [ "src-out", { X = 10; Y = 46 } ]
                                    CreepPositions =
                                        Map.ofList
                                            [ "t", { X = 10; Y = 4 }; "o", { X = 10; Y = 5 } ]
                                }
                    }

                let { Intents = intents } =
                    decide colony (Map.ofList [ "t", taskId (Harvest "src-out") ]) Set.empty None

                Expect.equal
                    (moveIntents intents)
                    [ "t", Bottom; "o", Top ]
                    "the traveller takes (10,5) and the parked creep swaps past it up the outpost's corridor"
            }

            test "a neighbour room of bare ground changes nothing" {
                // ADR 0004's totality over a room layer carrying terrain
                // and a border ring and nothing else: every query of it
                // answers empty, so it is unpriceable, enters no Task and
                // blocks no action — "empty" is not a state anything has to
                // model, and the proof is that the tick decides exactly
                // what it decided with no such room at all.
                //
                // Not a blind outpost, which this test claimed to be until
                // #148: a declared room the colony cannot see carries its
                // sources and its controller all the same (`Outpost.place`,
                // ADR 0041), and the tests further down pin what *that*
                // decides. What is left here is the totality property the
                // furniture is laid on top of.
                //
                // Read at the top seam over all three outputs at once,
                // because the ways this could go wrong are not local: a
                // second room's weight grid consulted for a home price, a
                // Seam band admitting a crossing to nowhere, a Task pooled
                // off a layer with nothing in it.
                let colony = northBorderColony { X = 10; Y = 38 }

                Expect.contains
                    (let _, _, verdicts = outcomeOf colony in verdicts)
                    (Verdict.Matched("w", taskId (Harvest "src-home"), MatchFactor.OnlyCandidate))
                    "the premise: this colony decides something, so an equal outcome says something"

                Expect.equal
                    (outcomeOf (colony |> withNorthOutpost None))
                    (outcomeOf colony)
                    "a room with no geometry decides exactly what no room at all decides"
            }

            test "the declared outposts are the two ADR 0042 names, so the shell scans three rooms" {
                // #124 landed this constant empty and pinned the emptiness,
                // because ADR 0041 ships the capability to project a
                // neighbour and deliberately no behaviour. ADR 0042 fills
                // it, and this is where that pin turns over: the rooms, in
                // the order a human wrote them, and the scan set the shell
                // takes from them.
                //
                // Which ids and which tiles is a claim about the committed
                // captures rather than about this list, so it is pinned
                // where the captures are read (`RoomInvariantTests`) and
                // never retyped here — two literals of the same ids would
                // agree with each other and with nothing else.
                Expect.equal
                    (Outpost.declared |> List.map (fun outpost -> outpost.RoomName))
                    [ "W12S27"; "W13S28" ]
                    "the north outpost and the west one"

                Expect.equal
                    (Outpost.roomsProjected Outpost.declared "W12S28")
                    [ "W12S28"; "W12S27"; "W13S28" ]
                    "so the projection covers the spawn room and both of them"
            }

            test "a declared outpost joins the spawn room in the set the shell scans" {
                // Written in the engine's own ids, as a declaration has to
                // be: the projection keys every target by the id the server
                // hands back, so a constant written in the captures'
                // readable short names would match nothing at all on a live
                // server, and would do it in silence (ADR 0004).
                let north =
                    {
                        RoomName = "W12S27"
                        Sources = [ "6a8caabadd4872bccd3194a6", { X = 16; Y = 45 } ]
                        Controller = "6a8caabadd4872bccd3194a5", { X = 37; Y = 43 }
                    }

                let west =
                    {
                        RoomName = "W13S28"
                        Sources =
                            [
                                "6a8caaaddd4872bccd319362", { X = 16; Y = 7 }
                                "6a8caaaddd4872bccd319361", { X = 18; Y = 4 }
                            ]
                        Controller = "6a8caaaddd4872bccd319363", { X = 24; Y = 17 }
                    }

                Expect.equal
                    (Outpost.roomsProjected [ north; west ] "W12S28")
                    [ "W12S28"; "W12S27"; "W13S28" ]
                    "the spawn room, then the declarations in their own order"

                // A declaration naming the spawn room is a human's slip in
                // a constant a human moves (ADR 0039's precedent), and the
                // projection keys rooms by name: scanning that room twice
                // would file one room's geometry under one name twice over
                // rather than say anything about it.
                Expect.equal
                    (Outpost.roomsProjected [ { north with RoomName = "W12S28" } ] "W12S28")
                    [ "W12S28" ]
                    "a room declared twice is scanned once"
            }

            test "a declaration nobody can see this tick still pools its rock, and wins on it" {
                // ADR 0041's deadlock, read at the top seam (#148): *"A
                // source's position needs vision; vision needs a creep
                // there; a creep goes there because a Task exists; the Task
                // exists because the source is in the projection."* #124
                // read ADR 0004's per-entry absence onto the declaration
                // as well, so the outpost's rock entered the pool only on a
                // tick the colony could see the room — and nothing was ever
                // sent to make that tick happen.
                //
                // The room here is shaped exactly as the shell shapes one
                // it cannot see (`Snapshot.projectRoom`): terrain and a
                // border ring, because `Game.map.getRoomTerrain` needs no
                // vision, and not one entry more. Everything the outpost
                // contributes below is the declaration's.
                let declaration =
                    {
                        RoomName = "W1N2"
                        Sources = [ "src-out", { X = 10; Y = 46 } ]
                        // Off the corridor on purpose: what the controller
                        // is doing to this fixture is standing in
                        // `Obstacles`, and a controller on the corridor
                        // would seal it and make the comparison below about
                        // reachability instead of about distance.
                        Controller = "ctrl-out", { X = 11; Y = 44 }
                    }

                let blind = northBorderColony { X = 10; Y = 38 } |> withNorthOutpost None

                let declared =
                    { blind with
                        Sources = Outpost.pooledSources [ "W1N2" ] [ declaration ] blind.Sources
                        Spatial = Outpost.place [ declaration ] blind.Spatial
                    }

                Expect.equal
                    (matchOf blind)
                    (Some(taskId (Harvest "src-home"), MatchFactor.OnlyCandidate))
                    "the premise: undeclared, the blind room offers nothing and the home rock stands alone"

                // The win has to be on the *placed* rock's price, and that
                // needs saying because an unplaced target is not inactive:
                // ADR 0004's escape prices it at 0, which beats every real
                // walk on the same factor. So a `place` that did nothing at
                // all would hand the Verdict below the same task and the
                // same `TravelCost` for the opposite reason. These two
                // lines are what tell the reasons apart: the rock is filed
                // under its own room, and the price that won is a real
                // crossing rather than the escape — the step down to the
                // border, the crossing itself, and two down the outpost's
                // corridor to the Seat at (10,47), four plain tiles at
                // travel cost's 2 apiece (ADR 0010's half-ticks).
                let atlas = Atlas.ofSnapshot declared

                Expect.equal
                    (Atlas.targetRoom atlas "src-out")
                    (Some "W1N2")
                    "the declaration reached the projection: the rock is filed under its own room"

                Expect.equal
                    (Atlas.travelCost atlas "w" (Harvest "src-out"))
                    (Some 8)
                    "and its price is a real crossing, never the escape: four plain steps at 2 apiece"

                // The same pair the ranking test above compares, at the
                // same two tiles — so what moved is only that the outpost's
                // rock is now declared rather than seen, and it is still
                // travel cost that separates the two.
                Expect.equal
                    (matchOf declared)
                    (Some(taskId (Harvest "src-out"), MatchFactor.TravelCost))
                    "declared, the unseen rock is a Task ranked in the one pool — and the nearer of the two"
            }

            test "where vision answers, laying the declaration in changes nothing" {
                // The other half of the rule: a declaration carries only
                // what cannot wait for vision — the ids and the tiles — and
                // is laid *under* what the room's `find` families answered,
                // never over it (ADR 0041). The reservation remaining, the
                // hits, the stores, the creeps and every structure standing
                // are vision's alone and stay vision's.
                //
                // Asserted as an equality on the whole projection rather
                // than field by field: what has to hold is that not one
                // entry moves, and a per-field check would pass while some
                // field nobody thought of was overwritten.
                //
                // The declaration below names the rock one tile off where
                // vision put it, and that disagreement is the whole test.
                // Live the two agree by construction — the ids are the
                // engine's own and a rock does not move — so a declaration
                // that matched vision tile for tile would leave this
                // equality true whichever of the two won, and the rule
                // would be pinned by nothing. Only a conflict can say which
                // truth is authoritative. The one that can really arise is
                // a human's: the constant is moved by hand (ADR 0041), and
                // a mistyped tile must not move a rock the engine is
                // answering for out from under its Seats.
                let declaration =
                    {
                        RoomName = "W1N2"
                        Sources = [ "src-out", { X = 10; Y = 47 } ]
                        Controller = "ctrl-out", { X = 11; Y = 44 }
                    }

                let colony = northBorderColony { X = 10; Y = 38 }

                let seen =
                    { colony with
                        Sources = colony.Sources @ [ drained "src-out" 120 ]
                        Creeps = colony.Creeps @ [ worker "w-out" 0 50 ]
                        Spatial =
                            { colony.Spatial with
                                Borders = Map.add "W1N2" plainRing colony.Spatial.Borders
                                TargetKinds =
                                    colony.Spatial.TargetKinds
                                    |> Map.add "src-out" Source
                                    |> Map.add "ctrl-out" Controller
                                    |> Map.add "cont-out" (Structure BuiltKind.Container)
                                Hits = Map.ofList [ "cont-out", { Hits = 100; HitsMax = 250000 } ]
                                Stores = Map.ofList [ "cont-out", 300 ]
                            }
                            |> withNeighbour
                                "W1N2"
                                { RoomLayer.empty with
                                    Terrain = Map.ofList (corridor 10 40 48)
                                    TargetPositions =
                                        Map.ofList
                                            [
                                                "src-out", { X = 10; Y = 46 }
                                                "ctrl-out", { X = 11; Y = 44 }
                                                "cont-out", { X = 10; Y = 45 }
                                            ]
                                    CreepPositions = Map.ofList [ "w-out", { X = 10; Y = 44 } ]
                                    Obstacles = Set.singleton { X = 11; Y = 44 }
                                }
                    }

                Expect.equal
                    (Outpost.place [ declaration ] seen.Spatial)
                    seen.Spatial
                    "a projection vision already filled gains nothing from the declaration"

                Expect.equal
                    (Outpost.pooledSources [ "W1N2" ] [ declaration ] seen.Sources)
                    seen.Sources
                    "and the seen rock is pooled once, at the engine's restock and not the default"
            }

            test "an unseen rock is pooled at the held-energy default, not at never" {
                // ADR 0025: a restock is a time, and 0 is what a source
                // holding energy reads. The unknown restock takes the same
                // 0 rather than something large, because a drained source's
                // Harvest is judged at the creep's arrival — a walk has to
                // cover the wait — so any other number would be a source no
                // walk could ever cover, which is the vision deadlock again
                // in a second place. What withholds the dig from a rock
                // that turns out to be empty when the creep gets there is
                // the Emitter's own gate, on the tick there is vision to
                // read it from.
                let declaration =
                    {
                        RoomName = "W1N2"
                        Sources = [ "src-out", { X = 10; Y = 46 } ]
                        Controller = "ctrl-out", { X = 11; Y = 44 }
                    }

                Expect.equal
                    (Outpost.pooledSources [ "W1N2" ] [ declaration ] [ source "src-home" ])
                    [ source "src-home"; source "src-out" ]
                    "the seen rocks first, in their order, then the declared one at restock 0"
            }

            test "a declaration for a room the scan set left out places nothing and pools nothing" {
                // The scan set is the one gate on which rooms the colony
                // works (`roomsProjected`), and the stand-down of ADR 0043
                // narrows exactly it: a room withdrawn from does not enter
                // the projection at all. A declaration able to furnish a
                // room the scan left out would be a second gate free to
                // disagree with the first — furniture standing on terrain
                // nobody read.
                let declaration =
                    {
                        RoomName = "W9N9"
                        Sources = [ "src-gone", { X = 10; Y = 46 } ]
                        Controller = "ctrl-gone", { X = 11; Y = 44 }
                    }

                let blind = northBorderColony { X = 10; Y = 38 } |> withNorthOutpost None

                Expect.equal
                    (Outpost.place [ declaration ] blind.Spatial)
                    blind.Spatial
                    "no layer for that room, so no tile of it is placed"

                // The pool passes the same gate, and has to: an unplaced
                // target is not inert. `Atlas.travelCost` answers 0 for
                // geometry the projection cannot place (ADR 0004's escape),
                // so a rock pooled for a room nothing was projected for
                // *wins* its tier on price, and the Emitter aims a Harvest
                // at an object `Game.getObjectById` cannot answer for while
                // anti-thrash holds the creep on it (#142's stuck creep, in
                // a second place). Reachable the tick the colony's last
                // spawn dies — the shell's scan set is empty with no home
                // room — and the shape ADR 0043's stand-down withdraws a
                // room in.
                Expect.equal
                    (Outpost.pooledSources [ "W1N1"; "W1N2" ] [ declaration ] blind.Sources)
                    blind.Sources
                    "and no rock of it is pooled, so the two readings of the constant agree"

                Expect.isEmpty
                    (Outpost.pooledSources [] Outpost.declared [])
                    "an empty scan set — no spawn, so no home room — pools nothing at all"
            }

            test "a declared controller stands in Obstacles, so no Work Area offers its tile" {
                // The third thing a declaration puts in the projection
                // beside the tiles and the kinds: the controller's own tile
                // joins `Obstacles`, exactly as the seen half files it. A
                // controller is an obstacle structure — a reserver stands
                // beside it and never on it — so a Work Area built over
                // ground that ignored it would offer a tile the engine
                // refuses to move onto, and #131's reserver would be
                // assigned there and held there.
                //
                // On plain ground on purpose, and that is the whole reason
                // this fixture exists rather than an assertion over the
                // committed captures: both declared controllers stand on
                // terrain the capture reads as wall, so the weight grid refuses
                // their tiles before `Obstacles` is ever consulted and the
                // rule would be pinned by the terrain rather than by the
                // code (ADR 0036 supplies counterexamples, not cover).
                let declaration =
                    {
                        RoomName = "W1N2"
                        Sources = [ "src-out", { X = 10; Y = 46 } ]
                        Controller = "ctrl-out", { X = 10; Y = 42 }
                    }

                let blind = northBorderColony { X = 10; Y = 38 } |> withNorthOutpost None

                let atlas =
                    Atlas.ofSnapshot
                        { blind with
                            Spatial = Outpost.place [ declaration ] blind.Spatial
                        }

                let area = Atlas.workArea atlas (Upgrade "ctrl-out")

                Expect.isNonEmpty
                    area
                    "the premise: the corridor gives the controller ground to be reserved from"

                Expect.isFalse
                    (Set.contains { X = 10; Y = 42 } area)
                    "and the controller's own tile is not part of it, standing in Obstacles"
            }
        ]

/// The colony with **two** outposts and one Anchor standing beside the
/// wrong one — the live report #159 was filed on, at the seam it is
/// decided at.
///
/// Home is the north corridor with a west arm along row 26 joining it, and
/// the Anchor stands two tiles from the west border and thirty-three from
/// the north one — the asymmetry the live colony had, both numbers walked
/// to the border tile itself. W1N2 across the north border carries a rock
/// with a container standing on one of its Seats; W2N1 across the west
/// carries a rock with nothing on it — the shape the live colony really
/// had, the north container built and the west ones not.
///
/// `northBorderColony`'s own rock is not a third one: `Sources` is
/// replaced, `src-home` is dropped from the kind census and the home
/// layer's `TargetPositions` is emptied, so the position handed to it
/// places nothing and the pool really is the two outpost Harvests.
///
/// The Anchor quota is the colony's Post count (ADR 0042): with the west
/// rock bare that count is one, so the north container hires exactly this
/// one body — and nothing in the Matcher knows which Post it was hired
/// for. It is ranked by `(rank, cost, load)` like every other body, which
/// is what sent the live one west. With a container on the west rock the
/// count is two and this body is the first of them; nothing here casts the
/// second, the fixture having no spawn.
///
/// The pool is those two Harvests and nothing else: no controller, no
/// refillable, no site, and Withdraw and Flee are inapplicable to a
/// Work-heavy body (ADR 0016, ADR 0033). Pairwise by construction, so a
/// Matched Verdict here names this pair and no third candidate stands in
/// for either side of it.
let private twoRockColony (westContainer: (string * Pos) list) =
    let colony = northBorderColony { X = 10; Y = 38 }

    { colony with
        Sources = [ source "src-north"; source "src-west" ]
        Creeps = [ creepWith "anchor" 0 50 [ Work; Work; Carry; Move ] ]
        Spatial =
            { colony.Spatial with
                Borders =
                    colony.Spatial.Borders |> Map.add "W1N2" plainRing |> Map.add "W2N1" plainRing
                TargetKinds =
                    (colony.Spatial.TargetKinds, westContainer)
                    ||> List.fold (fun kinds (id, _) ->
                        Map.add id (Structure BuiltKind.Container) kinds)
                    |> Map.remove "src-home"
                    |> Map.add "src-north" Source
                    |> Map.add "src-west" Source
                    |> Map.add "cont-north" (Structure BuiltKind.Container)
            }
            |> withHome (fun layer ->
                { layer with
                    Terrain =
                        (layer.Terrain, [ for x in 1..10 -> { X = x; Y = 26 } ])
                        ||> List.fold (fun terrain pos -> Map.add pos Plain terrain)
                    TargetPositions = Map.empty
                    CreepPositions = Map.ofList [ "anchor", { X = 2; Y = 26 } ]
                })
            |> withNeighbour
                "W1N2"
                { RoomLayer.empty with
                    Terrain = Map.ofList (corridor 10 40 48)
                    TargetPositions =
                        Map.ofList
                            [ "src-north", { X = 10; Y = 46 }; "cont-north", { X = 10; Y = 45 } ]
                }
            |> withNeighbour
                "W2N1"
                { RoomLayer.empty with
                    Terrain = Map.ofList [ for x in 41..48 -> { X = x; Y = 26 }, Plain ]
                    TargetPositions = Map.ofList (("src-west", { X = 45; Y = 26 }) :: westContainer)
                }
    }

/// Which Task won the one Anchor, and what separated it from its closest
/// rival — `matchOf`'s reading for the body these cases hire (ADR 0009).
let private anchorMatch (colony: Snapshot) =
    let { Verdicts = verdicts } = decide colony Map.empty Set.empty None

    verdicts
    |> List.tryPick (function
        | Verdict.Matched("anchor", task, factor) -> Some(task, factor)
        | _ -> None)

[<Tests>]
let twoOutpostAnchorTests =
    testList
        "an Anchor between two outposts"
        [
            test "an unposted outpost rock is not a rival, however near it stands" {
                // The live failure: the colony's one container stood in the
                // north outpost, the Anchor its Post hired was born in the
                // spawn room, and it walked *west* — to a room with no
                // container at all — because ADR 0020's bare-Seat fallback
                // made those Seats reachable and travel cost had nothing
                // left to say but "nearer".
                //
                // The fix is geometric and not a rank or a quota: the west
                // rock's Work Area for this body is empty, so the Task has
                // no travel cost and never enters the pool. The factor
                // therefore reads `only-candidate` rather than
                // `travel-cost`, which is the whole claim — the near rock
                // is not a rival the far one beat, it is not a candidate.
                Expect.equal
                    (anchorMatch (twoRockColony []))
                    (Some(taskId (Harvest "src-north"), MatchFactor.OnlyCandidate))
                    "the posted rock a room and thirty-eight tiles away is the only Task there is"

                let { Intents = intents } = decide (twoRockColony []) Map.empty Set.empty None

                Expect.equal
                    (moveIntents intents)
                    [ "anchor", Right ]
                    "and the Anchor walks back up the west arm toward the northern Seam"

                Expect.isEmpty
                    (actionIntents intents)
                    "digging nothing on the way: it may not act on a target a room away"
            }

            test "a container standing on the west rock makes it a rival again, and it wins" {
                // The other half, and the only reading under which the case
                // above says anything: nothing here refuses an outpost, or
                // ranks a near room behind a far one. Put a container on the
                // west rock's Seat and that rock is posted, its Work Area is
                // that Post, and travel cost — the one comparison left
                // between two feeding-tier Harvests — sends the Anchor to
                // the near one exactly as it always did.
                Expect.equal
                    (anchorMatch (twoRockColony [ "cont-west", { X = 46; Y = 26 } ]))
                    (Some(taskId (Harvest "src-west"), MatchFactor.TravelCost))
                    "two posted rocks are two candidates, and the near one is cheaper"

                let { Intents = intents } =
                    decide
                        (twoRockColony [ "cont-west", { X = 46; Y = 26 } ])
                        Map.empty
                        Set.empty
                        None

                Expect.equal
                    (moveIntents intents)
                    [ "anchor", Left ]
                    "and the same body walks the other way, two tiles to the western Seam"
            }
        ]

/// A colony with an outpost beside it whose ground and furniture are the
/// case's own: the room's whole floor, and everything placed in it, said
/// here rather than inherited. The container pick is a choice *between*
/// Seats (ADR 0042), so a corridor with one Seat at each end proves
/// nothing about it; these cases lay a floor that makes the Seats differ.
///
/// Both rooms get a plain border ring, because the pick is measured to the
/// Seam and a projection carrying no ring answers an empty band (ADR
/// 0041). No case declares an edge: which border two rooms share is read
/// out of their names.
///
/// The outpost room gets a `RoomControl` entry, held by nobody: that map
/// is one entry per *seen* room, so an entry is how a fixture says the
/// colony is looking into the room this tick — which is what the placement
/// rule waits for, and what the Executor needs to create anything there.
/// Neutral rather than reserved because nothing here reads the rate; the
/// blind room is a case of its own below.
let private withOutpostGround room terrain placed (colony: Snapshot) =
    { colony with
        Sources = colony.Sources @ [ source "src-out" ]
        RoomControl = Map.add room neutralRoom colony.RoomControl
        Spatial =
            { colony.Spatial with
                Borders =
                    colony.Spatial.Borders
                    |> Map.add (SpatialInfo.homeName colony.Spatial) plainRing
                    |> Map.add room plainRing
                TargetKinds =
                    (colony.Spatial.TargetKinds, placed)
                    ||> List.fold (fun kinds (id, _, kind) -> Map.add id kind kinds)
            }
            |> withNeighbour
                room
                { RoomLayer.empty with
                    Terrain = Map.ofList terrain
                    TargetPositions = placed |> List.map (fun (id, pos, _) -> id, pos) |> Map.ofList
                }
    }

/// Every container site the tick asks for, room beside tile, in the order
/// the colony emits them — the whole of what this rule adds to a Decision.
let private containerSites (colony: Snapshot) =
    let { Intents = intents } = decide colony Map.empty Set.empty None

    intents
    |> List.choose (function
        | PlaceConstructionSite(room, pos, Container) -> Some(room, pos)
        | _ -> None)

/// `src-out` sits at (10,44), which no case lays ground on, so its Seats
/// are whichever of its eight neighbours the case does.
let private outpostSource = { X = 10; Y = 44 }

/// Two Seats and two ways out. `(10,45)` is a row nearer the border and
/// its only run to it is three tiles of swamp; `(11,43)` is a row farther
/// and its run is five of plain. Walk and proximity therefore disagree,
/// which is the whole point of the floor: 6 ticks against 16.
let private detourGround =
    [
        { X = 10; Y = 45 }, Plain
        { X = 10; Y = 46 }, Swamp
        { X = 10; Y = 47 }, Swamp
        { X = 10; Y = 48 }, Swamp
        { X = 11; Y = 43 }, Plain
        for y in 44..48 do
            { X = 12; Y = y }, Plain
    ]

[<Tests>]
let outpostContainerTests =
    testList
        "the outpost's source container"
        [
            test "the site lands on the Seat whose walk out to the Seam is shortest" {
                // ADR 0042's own rule, at the seam it is decided on: an
                // outpost has no spawn for a trunk to anchor on, so the
                // pick is anchored on the Seam instead. Measured as a walk
                // and never as a range — the two disagree on this floor by
                // construction, and the Seat the range would pick is the
                // one three swamp tiles from the border.
                let colony =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround "W1N2" detourGround [ "src-out", outpostSource, Source ]

                Expect.equal
                    (Atlas.seatTilesOf (Atlas.ofSnapshot colony) "src-out")
                    (Set.ofList [ { X = 10; Y = 45 }; { X = 11; Y = 43 } ])
                    "the premise: the rock has two Seats, and the nearer one to the border is (10,45)"

                Expect.equal
                    (containerSites colony)
                    [ "W1N2", { X = 11; Y = 43 } ]
                    "the farther Seat wins, because the ground between it and the Seam is cheaper"
            }

            test "the Intent carries the outpost's own room, never the colony's" {
                // The trap the Layout would have walked into: a placement
                // Intent has always carried a room name, and `planLayout`
                // stamps the one room it plans onto every site it emits, so
                // an outpost pick routed through that path would drop a
                // 5,000-energy container on the *home* room's tile of the
                // same coordinates. (11,43) is a real coordinate in both
                // rooms and this asserts which one is named.
                let colony =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround "W1N2" detourGround [ "src-out", outpostSource, Source ]

                // Asserted as the whole list and never with `Expect.all`,
                // which is vacuously true of the empty one: a rule that
                // planned nothing would pass the room-stamping case it
                // exists to pin.
                Expect.equal
                    (containerSites colony)
                    [ "W1N2", { X = 11; Y = 43 } ]
                    "the one site this rule places names the room its source stands in"
            }

            test "a room the colony cannot see this tick is planned nothing" {
                // ADR 0004 entry by entry, the same reading `sourceOutputOf`
                // gives the same rock: with no vision the container census
                // of that room is empty because nobody looked, not because
                // nothing stands there, and an absence is not an answer.
                // The Intent would also be one the Executor can only report
                // as `ActorMissing` — `Game.rooms` holds the seen rooms
                // alone — so a rule that fired here would file an upstream
                // bug against itself once a tick per rock, for ever.
                let seen =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround "W1N2" detourGround [ "src-out", outpostSource, Source ]

                Expect.isNonEmpty
                    (containerSites seen)
                    "the premise: seen, this rock is planned a container"

                Expect.isEmpty
                    (containerSites
                        { seen with
                            RoomControl = Map.remove "W1N2" seen.RoomControl
                        })
                    "and the same tick with the room unseen plans nothing at all"
            }

            test "a source with one Seat is the same rule with one candidate" {
                // W13S28's `16,7` is a single-Seat rock, and "the shortest"
                // has to answer where there is nothing to be shorter than.
                let ground =
                    [
                        { X = 10; Y = 45 }, Swamp
                        for y in 46..48 do
                            { X = 10; Y = y }, Plain
                    ]

                let colony =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround "W1N2" ground [ "src-out", outpostSource, Source ]

                Expect.equal
                    (containerSites colony)
                    [ "W1N2", { X = 10; Y = 45 } ]
                    "the one Seat there is, priced and picked like any other"
            }

            test "Seats that price alike fall to the lowest (X, Y), as every tie here does" {
                // W12S27's `16,45` has three Seats and all three are swamp,
                // so they can price identically — and a plan that answered
                // a different one of them on different ticks would not be
                // one (ADR 0011's determinism). Three swamp Seats over one
                // plain apron, so the three walks are equal by construction
                // and only the tie-break separates them.
                //
                // It also pins the subtraction the walk is measured with:
                // the Seat's own swamp step is charged to whatever walks
                // *in* to it, so three swamp Seats over identical ground
                // tie rather than each carrying five ticks of their own.
                let ground =
                    [
                        { X = 9; Y = 45 }, Swamp
                        { X = 10; Y = 45 }, Swamp
                        { X = 11; Y = 45 }, Swamp
                        for x in 8..12 do
                            for y in 46..48 do
                                { X = x; Y = y }, Plain
                    ]

                let colony =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround "W1N2" ground [ "src-out", outpostSource, Source ]

                Expect.equal
                    (containerSites colony)
                    [ "W1N2", { X = 9; Y = 45 } ]
                    "the three Seats tie, and the lowest X answers"
            }

            test "a Seat's own terrain is not charged to it: the walk is the ground beyond it" {
                // The convention every walk in this colony is measured by
                // (ADR 0029): a walk charges the tiles a creep steps onto
                // and never the tile it already stands on. Here it decides
                // the pick. Two Seats over one symmetric plain apron, so
                // the ground beyond them is identical and only their own
                // terrain differs — the swamp one first in (X, Y) order. A
                // rule that charged a Seat for standing on it would price
                // the swamp Seat five ticks dearer and pick the plain one;
                // this rule ties them and lets the tie-break answer, which
                // is right because whoever hauls from that container starts
                // on it and never pays to arrive.
                let ground =
                    [
                        { X = 9; Y = 45 }, Swamp
                        { X = 11; Y = 45 }, Plain
                        for x in 8..12 do
                            for y in 46..48 do
                                { X = x; Y = y }, Plain
                    ]

                let colony =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround "W1N2" ground [ "src-out", outpostSource, Source ]

                Expect.equal
                    (Atlas.seatTilesOf (Atlas.ofSnapshot colony) "src-out")
                    (Set.ofList [ { X = 9; Y = 45 }; { X = 11; Y = 45 } ])
                    "the premise: two Seats, one swamp and one plain, over the same apron"

                Expect.equal
                    (containerSites colony)
                    [ "W1N2", { X = 9; Y = 45 } ]
                    "the swamp Seat is no dearer than the plain one, so the tie-break answers"
            }

            test "a container already serving the source is planned for no second one" {
                // ADR 0040 holds here as it does at home, and by target
                // rather than by tile: the thing serving the rock is on
                // (10,45), which is not the tile the plan picked, and the
                // rock is served all the same. Standing and pending both,
                // because the plan asks whether another must be built and a
                // site going up answers that.
                let served kind =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround
                        "W1N2"
                        detourGround
                        [ "src-out", outpostSource, Source; "con-out", { X = 10; Y = 45 }, kind ]

                Expect.isEmpty
                    (containerSites (served (Structure BuiltKind.Container)))
                    "a container standing within range 1 of the rock, on a Seat the plan did not pick"

                Expect.isEmpty
                    (containerSites (served (Site BuiltKind.Container)))
                    "and a site pending there, which is a container already being built"
            }

            test "a home container on the pick's coordinates defers nothing" {
                // The room-blind census this rule would have inherited: a
                // `Pos` carries no room (ADR 0041), so a census unioning
                // both rooms' container tiles would read the home room's
                // container as serving an outpost rock fifty tiles away —
                // and would then defer the outpost's container forever,
                // leaving the room with no switch to close (ADR 0042).
                let colony =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround "W1N2" detourGround [ "src-out", outpostSource, Source ]
                    |> withTarget "con-home" { X = 11; Y = 43 } (Structure BuiltKind.Container)

                Expect.equal
                    (containerSites colony)
                    [ "W1N2", { X = 11; Y = 43 } ]
                    "the outpost's rock is unserved: what stands on those coordinates stands at home"
            }

            test "the home room's Layout is not moved by an outpost joining the projection" {
                // ADR 0042: "The outpost gets a container and nothing
                // else. No roads, and no Layout." This rule runs beside the
                // Layout and never inside it, so a colony that gains an
                // outpost plans the same home room it planned without one —
                // the same clustered picks, the same trunks, the same
                // containers, the same footings — and gains exactly one
                // site, in the other room.
                let alone = trunkColony 4

                let withOutpost =
                    alone
                    |> withOutpostGround "W1N2" detourGround [ "src-out", outpostSource, Source ]

                let atHome colony =
                    let { Intents = intents } = decide colony Map.empty Set.empty None

                    placementIntents intents |> List.filter (fun (room, _, _) -> room = "W1N1")

                Expect.isNonEmpty (atHome alone) "the premise: this room has a Layout to move"

                Expect.equal
                    (atHome withOutpost)
                    (atHome alone)
                    "every home site the Layout placed, unmoved and in its own order"

                Expect.equal
                    (containerSites withOutpost |> List.filter (fun (room, _) -> room <> "W1N1"))
                    [ "W1N2", { X = 11; Y = 43 } ]
                    "and the one site the outpost gained is the container, in the outpost"
            }

            test "a room home shares no border with is planned nothing" {
                // Total (ADR 0004): the Seam is read out of the two room
                // names, and two rooms four sectors apart have no band —
                // so the walk that anchors the pick has no anchor, and an
                // unpriceable rule plans nothing rather than planning
                // arbitrarily. W5N5 is not W1N1's neighbour.
                let colony =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround "W5N5" detourGround [ "src-out", outpostSource, Source ]

                Expect.isEmpty
                    (containerSites colony)
                    "no band to price a Seat against, so no Seat is picked"
            }
        ]

/// The colony's own room for the haul below: its spawn standing eleven
/// tiles down a one-wide corridor from the north border, an obstacle as a
/// spawn is, so the only tile a transfer reaches it from on the side the
/// haul arrives on is (25,9). No controller, no refillable with room and
/// no home source — what the hauler quota folds here is the outpost's one
/// container and nothing beside it, so the number this fixture answers is
/// that container's own.
let private haulHome =
    { bareRespawn with
        Controller = None
        Refillables = []
        Sources = [ source "src-out" ]
        RoomEnergy = bank 300 300
        Spatial =
            { SpatialInfo.empty with
                RoomName = Some "W1N1"
                Borders = Map.ofList [ "W1N1", plainRing; "W1N2", plainRing ]
                TargetKinds = Map.ofList [ "spawn-1", Structure BuiltKind.Spawn ]
            }
            |> withHome (fun layer ->
                { layer with
                    Terrain = Map.ofList (corridor 25 1 48)
                    TargetPositions = Map.ofList [ "spawn-1", { X = 25; Y = 10 } ]
                    Obstacles = Set.singleton { X = 25; Y = 10 }
                })
    }

/// The same colony with its outpost one room north: the rock at (25,40) on
/// ground the projection carries none of, and the container standing on
/// the Seat below it — the switch that admits an outpost into the economy
/// (ADR 0042). Who holds W1N2 is the caller's and is the only thing that
/// moves between two calls; `None` is the room the colony sees nobody in,
/// which is a third answer and not the neutral one (ADR 0004).
let private withHaulOutpost (control: RoomControlInfo option) (colony: Snapshot) =
    { colony with
        RoomControl =
            match control with
            | Some held -> Map.add "W1N2" held colony.RoomControl
            | None -> colony.RoomControl
        Spatial =
            { colony.Spatial with
                TargetKinds =
                    colony.Spatial.TargetKinds
                    |> Map.add "src-out" Source
                    |> Map.add "can-out" (Structure BuiltKind.Container)
            }
            |> withNeighbour
                "W1N2"
                { RoomLayer.empty with
                    Terrain = Map.ofList (corridor 25 41 48)
                    TargetPositions =
                        Map.ofList [ "src-out", { X = 25; Y = 40 }; "can-out", { X = 25; Y = 41 } ]
                }
    }

/// The same outpost the tick before its container stands: the rock
/// projected and the room held exactly as above, and `can-out` simply
/// absent, which is what a Seat with nothing built on it is. The standing
/// census is then the only census input that moves between the two.
let private beforeHaulContainer (colony: Snapshot) =
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Spatial =
            { colony.Spatial with
                TargetKinds = colony.Spatial.TargetKinds |> Map.remove "can-out"
            }
            |> withNeighbour
                "W1N2"
                { outpost with
                    TargetPositions = outpost.TargetPositions |> Map.remove "can-out"
                }
    }

[<Tests>]
let outpostHaulTests =
    testList
        "the outpost's container in the hauler quota"
        [
            test "an outpost container hires haul capacity, priced at its own room's rate" {
                // ADR 0042's hauler half, which #127 could not reach: the
                // quota folds every projected room's containers, and the
                // round trip it prices this one at is the Seam join
                // (`Atlas.haulRoundTripTicks`), 51 ticks over this
                // corridor. At the 300 bank the hauler row carries 200, so
                // held the rock ships ten a tick and hires ceil(51 x 10 /
                // 200) = 3, and unheld it ships five and hires 2.
                //
                // Pairwise, one rival at a time: the two colonies differ in
                // who holds W1N2 and in nothing else.
                let held control =
                    quotaOf (haulHome |> withHaulOutpost (Some control))

                Expect.equal
                    (quotaOf haulHome)
                    0
                    "the premise: without the outpost there is no haul"

                Expect.equal (held (reservedRoom true 4000)) 3 "reserved, the rock ships ten a tick"
                Expect.equal (held neutralRoom) 2 "held by nobody, it ships five and hires less"

                Expect.equal
                    (held ownedRoom)
                    (held (reservedRoom true 4000))
                    "owned or reserved by us is one rate, as the engine pays it"

                Expect.equal
                    (quotaOf (haulHome |> withHaulOutpost None))
                    0
                    "and a room the colony cannot see prices no rock at all (ADR 0004)"
            }

            test "a container in a room the projection does not carry hires nobody" {
                // ADR 0004 at the fold's own edge: the container is in the
                // kind census, the colony holds the room, and the
                // projection places neither the container nor its rock —
                // so there is no tile to flood from and no room to flood
                // over. Nothing, and never the home room's arithmetic run
                // over an outpost's coordinates.
                let seen = haulHome |> withHaulOutpost (Some(reservedRoom true 4000))

                let unprojected =
                    { seen with
                        Spatial =
                            { seen.Spatial with
                                Rooms = Map.remove "W1N2" seen.Spatial.Rooms
                            }
                    }

                Expect.equal (quotaOf seen) 3 "the premise: projected, the container hires three"
                Expect.equal (quotaOf unprojected) 0 "unprojected, the same census hires none"
            }

            test "a quota memoised while the outpost was held is not handed back when it lapses" {
                // #127's memo case, in the room it was written for. The
                // hauler quota rides the census memo (ADR 0017) and now
                // reads a *second* room's held rate, so the census
                // signature had to widen to sign every projected room's —
                // and this is what the widening buys. Every census input
                // below is byte-identical between the two Snapshots: the
                // reservation is the only thing that moved.
                let lapsed = haulHome |> withHaulOutpost (Some neutralRoom)

                let previous =
                    (decide
                        (haulHome |> withHaulOutpost (Some(reservedRoom true 4000)))
                        Map.empty
                        Set.empty
                        None)
                        .Memo

                Expect.equal previous.HaulerQuota 3 "the premise: held, the container hires three"

                let recalled = decide lapsed Map.empty Set.empty (Some previous)
                let fresh = decide lapsed Map.empty Set.empty None

                Expect.equal fresh.Memo.HaulerQuota 2 "the premise: lapsed, it hires two"

                Expect.equal
                    recalled.Memo.HaulerQuota
                    fresh.Memo.HaulerQuota
                    "the stale memo recomputes rather than handing back the held rate's fleet"

                Expect.equal
                    (spawnIntents recalled.Intents)
                    (spawnIntents fresh.Intents)
                    "so the fleet the colony casts is the one the halved haul asked for"
            }

            test
                "the quota memoised before an outpost container stood is not handed back once it does" {
                // The other half of the widening, and the one the rate
                // above cannot reach: the census entry itself. The
                // reservation case moves `held`, which is signed per room;
                // this one moves nothing but whether `can-out` stands in
                // W1N2, which only the *standing* census spanning every
                // projected room can see. Joined against the home layer
                // alone the two Snapshots sign one string, so the colony
                // would recall the container-less nothing for ever and ADR
                // 0042's switch would never fire.
                let standing = haulHome |> withHaulOutpost (Some(reservedRoom true 4000))
                let before = standing |> beforeHaulContainer

                let previous = (decide before Map.empty Set.empty None).Memo

                Expect.equal
                    previous.HaulerQuota
                    0
                    "the premise: with no container on the Seat there is no haul to hire for"

                let recalled = decide standing Map.empty Set.empty (Some previous)
                let fresh = decide standing Map.empty Set.empty None

                Expect.equal fresh.Memo.HaulerQuota 3 "the premise: standing, it hires three"

                Expect.equal
                    recalled.Memo.HaulerQuota
                    fresh.Memo.HaulerQuota
                    "the container standing up moves the signature, so the quota is recomputed"

                Expect.equal
                    (spawnIntents recalled.Intents)
                    (spawnIntents fresh.Intents)
                    "and the fleet the colony casts is the one the new haul asked for"
            }
        ]

/// The same haul fixture with an Anchor garrisoning the outpost's
/// container — the succession ADR 0026 owes an outpost's Post as much as a
/// home one (#153). Its ticks to live are the caller's, because that is the
/// whole of what moves between two calls below.
let private withOutpostGarrison life (colony: Snapshot) =
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Creeps = colony.Creeps @ [ anchor "a-out" 0 50 |> withLife life ]
        Spatial =
            colony.Spatial
            |> withNeighbour
                "W1N2"
                { outpost with
                    CreepPositions = Map.add "a-out" { X = 25; Y = 41 } outpost.CreepPositions
                }
    }

/// The colony the cases below read: the haul fixture's two rooms, the
/// outpost held and its container standing, and one Anchor on that
/// container with the given life left.
let private outpostSuccession life =
    haulHome
    |> withHaulOutpost (Some(reservedRoom true 4000))
    |> withOutpostGarrison life

/// The names of the bodies a tick casts, in the order the colony emits
/// them — the row a spawn Intent came from, which is the whole of what the
/// cases below read.
let private castNames (colony: Snapshot) =
    spawnIntents (decide colony Map.empty Set.empty None).Intents
    |> List.map (fun (_, _, name) -> name)

[<Tests>]
let outpostSuccessionTests =
    testList
        "an outpost's Anchor and its lead"
        [
            test "an Anchor a room away is expiring, and its replacement is cast before it dies" {
                // The reproduction #153 opens on. Until it, a lead was
                // priced off the home room's flood alone, so a creep the
                // home room did not place answered 0 and was never expiring
                // — an outpost's garrison held its Post to the last tick,
                // its successor was cast only once it was dead, and the Post
                // stood empty for the cast plus the crossing in every
                // 1,500-tick life while the workforce target went on hiring
                // against the source's nominal output (ADR 0042).
                //
                // Priced over the border the lead is countable a tile at a
                // time. The Anchor row at this 300 bank is two Work over a
                // Carry and a Move (`anchorBodyFor`), so twelve ticks in the
                // spawner and four cost units — two ticks — a plain step.
                // The replacement is born on (25,9), walks eight tiles up to
                // (25,1), steps onto the exit at (25,0), is moved to (25,49)
                // for nothing, steps off onto (25,48) and walks seven more
                // down to the container at (25,41): sixteen tiles of ground
                // at two ticks each, plus the plain exit's own two — 34 of
                // walking and a lead of 46.
                Expect.equal
                    (castNames (outpostSuccession 1500))
                    (castNames (outpostSuccession 47))
                    "one tick outside its lead the garrison still counts, exactly as a fresh one does"

                match castNames (outpostSuccession 47) with
                | [ name ] ->
                    Expect.stringStarts name "hauler-" "the premise: the Anchor row is filled"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"

                match castNames (outpostSuccession 46) with
                | [ name ] ->
                    Expect.stringStarts
                        name
                        "anchor-"
                        "at its lead the outpost's row is short and the successor is cast"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"

                match castNames (outpostSuccession 1) with
                | [ name ] ->
                    Expect.stringStarts
                        name
                        "anchor-"
                        "and a tick from death it is being replaced, not mourned"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "an outpost no crossing reaches leads nobody, however little life is left" {
                // Total (ADR 0004) at the seam the border is joined on: with
                // no ring in the projection the two rooms share no Seam
                // band, so there is no walk to price and no lead — and a
                // lead of 0 leaves the garrison counted living to its last
                // tick, which is the answer unpriceable geometry has always
                // had. Never an arbitrary number, and never the home room's
                // arithmetic run over an outpost's coordinates.
                let unbordered life =
                    let colony = outpostSuccession life

                    { colony with
                        Spatial =
                            { colony.Spatial with
                                Borders = Map.empty
                            }
                    }

                Expect.equal
                    (castNames (unbordered 1))
                    (castNames (unbordered 1500))
                    "the same colony casts the same body whether the garrison is dying or fresh"

                // A worker and not a hauler, because the same missing band
                // leaves the container's round trip unpriceable and its
                // haul unhired (ADR 0004, `outpostHaulTests`). What this
                // case reads is the row it is *not*: the Anchor row is
                // filled, so the garrison is still counted living.
                match castNames (unbordered 1) with
                | [ name ] ->
                    Expect.stringStarts name "worker-" "and the Anchor row reads as filled"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }
        ]

/// The switch's home half: the same corridor down column 25 of W1N1 with
/// the spawn at (25,10), and one home source in the rock beside it at
/// (24,20) with its container standing on the Seat (25,20). A home Post,
/// a home hauler term and a home income share, so this colony is already
/// running an economy well clear of `minWorkforce` before the outpost is
/// asked to add anything to it — which is what lets the pair below read a
/// *difference* rather than a floor.
let private switchHome =
    { bareRespawn with
        Controller = None
        Refillables = []
        Sources = [ source "src-home" ]
        RoomEnergy = bank 300 300
        Spatial =
            { SpatialInfo.empty with
                RoomName = Some "W1N1"
                Borders = Map.ofList [ "W1N1", plainRing; "W1N2", plainRing ]
                TargetKinds =
                    Map.ofList
                        [
                            "spawn-1", Structure BuiltKind.Spawn
                            "src-home", Source
                            "can-home", Structure BuiltKind.Container
                        ]
            }
            |> withHome (fun layer ->
                { layer with
                    Terrain = Map.ofList (({ X = 24; Y = 20 }, Wall) :: corridor 25 1 48)
                    TargetPositions =
                        Map.ofList
                            [
                                "spawn-1", { X = 25; Y = 10 }
                                "src-home", { X = 24; Y = 20 }
                                "can-home", { X = 25; Y = 20 }
                            ]
                    Obstacles = Set.singleton { X = 25; Y = 10 }
                })
    }

/// The same colony with the outpost's rock declared one room north and
/// reserved — projected, priced, and with nothing built on it. This is the
/// tick before the switch: the Harvest is pooled, the room is held, and
/// every quota still reads the home room alone.
let private switchUnposted =
    { switchHome with
        Sources = switchHome.Sources @ [ source "src-out" ]
        RoomControl = Map.add "W1N2" (reservedRoom true 4000) switchHome.RoomControl
        Spatial =
            { switchHome.Spatial with
                TargetKinds = switchHome.Spatial.TargetKinds |> Map.add "src-out" Source
            }
            |> withNeighbour
                "W1N2"
                { RoomLayer.empty with
                    Terrain = Map.ofList (corridor 25 41 48)
                    TargetPositions = Map.ofList [ "src-out", { X = 25; Y = 40 } ]
                }
    }

/// And the tick the switch closes: the container standing on the outpost
/// rock's one Seat, and nothing else in the world different (ADR 0042).
let private switchPosted =
    let outpost = SpatialInfo.layerOf switchUnposted.Spatial "W1N2"

    { switchUnposted with
        Spatial =
            { switchUnposted.Spatial with
                TargetKinds =
                    switchUnposted.Spatial.TargetKinds
                    |> Map.add "can-out" (Structure BuiltKind.Container)
            }
            |> withNeighbour
                "W1N2"
                { outpost with
                    TargetPositions =
                        outpost.TargetPositions |> Map.add "can-out" { X = 25; Y = 41 }
                }
    }

/// The home room's whole target, one row at a time: its one Post's Anchor,
/// the two haulers its container's round trip to the spawn hires, and the
/// ten workers ten energy a tick feeds once the two rows above are
/// amortized — ceil((10 × 1500 − 3 × 300) / 1500) = 10 (ADR 0012, ADR
/// 0037). Thirteen bodies, and every case below reads against it.
let private switchHomeFleet =
    [ anchor "a-home" 0 50; hauler "h-home1" 0 100; hauler "h-home2" 0 100 ]
    @ [ for i in 1..10 -> worker $"w{i}" 0 50 ]

/// What the outpost's container adds, and nothing else: one Anchor for the
/// Post it makes, the three haulers its own round trip across the Seam
/// hires at its own source's held output, and its income share — the
/// worker row goes from ten to nineteen, because twenty a tick less the
/// five rows' amortization over one worker's Work drain is
/// ceil((20 × 1500 − 7 × 300) / 1500) = 19. Thirteen more bodies, which is
/// the whole of ADR 0042's switch stated as a fleet.
let private switchOutpostRows =
    [
        anchor "a-out" 0 50
        hauler "h-out1" 0 100
        hauler "h-out2" 0 100
        hauler "h-out3" 0 100
    ]
    @ [ for i in 11..19 -> worker $"w{i}" 0 50 ]

[<Tests>]
let containerSwitchTests =
    testList
        "the container is the switch"
        [
            // Read against the fleet, one body at a time: a colony standing
            // exactly at its target casts nothing, and the same colony one
            // body short casts one — so a target that moved by n shows up as
            // n bodies and cannot hide inside a spawn's one-cast-a-tick
            // limit.
            let casts colony fleet =
                spawnIntents
                    (decide { colony with Creeps = fleet } Map.empty Set.empty None).Intents

            let short fleet =
                List.truncate (List.length fleet - 1) fleet

            test "an outpost rock with nothing built on it moves no row of the target" {
                // ADR 0042's exclusion, read forward rather than backward:
                // the room is projected, held by us and its rock is pooled
                // for Harvest, and still the colony hires exactly the fleet
                // it hired without it. Until a container stands, an outpost
                // is invisible to every quota.
                //
                // Pairwise, one rival at a time: the two colonies differ in
                // the outpost rock and in nothing else.
                Expect.isEmpty
                    (casts switchHome switchHomeFleet)
                    "the premise: thirteen is the home room's whole target"

                Expect.hasLength
                    (casts switchHome (short switchHomeFleet))
                    1
                    "the premise is tight: one body short and the colony casts"

                Expect.equal
                    (quotaOf switchUnposted)
                    (quotaOf switchHome)
                    "the unposted rock hires no haul"

                Expect.isEmpty
                    (casts switchUnposted switchHomeFleet)
                    "and no Anchor and no worker either: the same thirteen are the whole target"
            }

            test "the container standing is one Anchor, its own haul and its income share" {
                // The switch itself (ADR 0042). One tick's difference — a
                // container standing on the outpost rock's one Seat — and
                // the colony hires thirteen more bodies: the Anchor for the
                // Post the container makes, the three haulers its own round
                // trip hires at its own source's output, and the nine
                // workers the rock's ten a tick feeds once those four rows
                // are amortized.
                Expect.isEmpty
                    (casts switchPosted (switchHomeFleet @ switchOutpostRows))
                    "posted, the target is the home fleet plus the outpost's own rows"

                Expect.hasLength
                    (casts switchPosted (short (switchHomeFleet @ switchOutpostRows)))
                    1
                    "and it is tight: one body short and the colony casts"

                Expect.equal
                    (quotaOf switchPosted - quotaOf switchUnposted)
                    3
                    "three of the thirteen are the container's own hauler term"
            }

            test "the Anchor the container hires is one, from the row the home Posts hire from" {
                // ADR 0042 pins the outpost's Anchor on the *same* row as
                // the home room's, walked to its Post by travel cost like
                // any other body — no remote-miner row, no second sizing
                // rule. So the proof is a swap at a fixed headcount: one
                // body short of the target the colony casts a worker while
                // both Anchors stand, and the same twenty-five bodies with
                // the outpost's Anchor spelled as a worker cast an Anchor
                // instead. Only a row gap can move between the two, because
                // the deficit is one either way.
                let shortFleet = short (switchHomeFleet @ switchOutpostRows)

                let swapped =
                    shortFleet
                    |> List.map (fun creep ->
                        if creep.Name = "a-out" then worker "w20" 0 50 else creep)

                match casts switchPosted shortFleet with
                | [ (_, _, name) ] ->
                    Expect.stringStarts
                        name
                        "worker-"
                        "the premise: with both Anchors it is a worker"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"

                match casts switchPosted swapped with
                | [ (_, _, name) ] -> Expect.stringStarts name "anchor-" "the gap is an Anchor gap"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "a standing outpost container adds no Task; stocked it adds its own Withdraw" {
                // The pool's half of the switch. A rock is pooled for
                // Harvest whichever room it stands in and whether or not it
                // is posted (ADR 0041) — one pool, one ranking — so the
                // container standing adds no Task by standing. It adds one
                // the tick it holds energy, and that Task is a Withdraw on
                // the container itself: nothing here becomes a Refill
                // target, because an outpost's container is no upgrade
                // buffer of a controller a room away (ADR 0010) — the join
                // that answers that is pinned by `roomLayerTests`, on a
                // fixture that has a controller to be wrong about.
                //
                // Standing is not the container's only way into the pool,
                // and the ticket's own Trap names the other: a container is
                // a repairable kind, so once its hits fall under half its
                // max `hungryStructures` pools a cross-room `Repair` for it
                // beside this Withdraw. That is no conflict with ADR 0010 —
                // `isHungry` judges every structure against its own kind's
                // whole line, so an outpost container's decay drags no home
                // container's line with it — and nothing here is decayed.
                Expect.equal
                    (planTasks switchPosted noThreats)
                    (planTasks switchUnposted noThreats)
                    "an empty container standing changes no Task in the pool"

                let stocked =
                    { switchPosted with
                        Spatial =
                            { switchPosted.Spatial with
                                Stores = Map.add "can-out" 500 switchPosted.Spatial.Stores
                            }
                    }

                Expect.equal
                    (List.except (planTasks switchPosted noThreats) (planTasks stocked noThreats))
                    [ Withdraw "can-out" ]
                    "and stocked it adds exactly one Task, the Withdraw of its own store"

                let decayed =
                    { switchPosted with
                        Spatial =
                            { switchPosted.Spatial with
                                Hits =
                                    Map.add
                                        "can-out"
                                        { Hits = 1000; HitsMax = 2500 }
                                        switchPosted.Spatial.Hits
                            }
                    }

                Expect.equal
                    (List.except (planTasks switchPosted noThreats) (planTasks decayed noThreats))
                    [ Repair "can-out" ]
                    "and decayed it adds exactly one more, its own Repair across the Seam"
            }
        ]

/// The whole fleet the switch hires: the home rows and the outpost's,
/// twenty-six bodies standing exactly at `switchPosted`'s target and
/// thirteen over `switchUnposted`'s.
let private switchFleet = switchHomeFleet @ switchOutpostRows

/// The same fleet with the named bodies respelled as generalists — the
/// headcount never moves, so a case reading against it reads a *row gap*
/// and nothing else, the deficit being the same number whichever row the
/// twenty-six bodies were cast from. The spare bodies are named off a
/// prefix of this helper's own, so growing `switchOutpostRows` can never
/// mint a name twice into one fleet.
let private respelled names fleet =
    fleet
    |> List.mapFold
        (fun n (creep: CreepInfo) ->
            if List.contains creep.Name names then
                worker $"gen{n}" 0 50, n + 1
            else
                creep, n)
        1
    |> fst

/// The fleet with both Anchors respelled: twenty-six bodies alive and
/// every Post in the colony standing empty.
let private unmannedPosts = respelled [ "a-home"; "a-out" ] switchFleet

/// The fleet with all five haulers respelled: twenty-six bodies alive and
/// no shipping at all.
let private unshippedFleet =
    respelled [ "h-home1"; "h-home2"; "h-out1"; "h-out2"; "h-out3" ] switchFleet

[<Tests>]
let rowGapTests =
    testList
        "the deficit gates the worker row alone"
        [
            // Read as the switch's own tests are, one body at a time off
            // the one idle spawn `switchHome` stands: a tick casts at most
            // one body, so the list this returns is either empty or names
            // the row whose gap was answered first.
            let casts colony fleet =
                spawnIntents
                    (decide { colony with Creeps = fleet } Map.empty Set.empty None).Intents

            // The premise every case below rests on, asserted where it is
            // used rather than assumed: at `switchUnposted`'s target of
            // thirteen a fleet of twenty-six is far over, and one body
            // fewer is still over — so nothing that follows can be the
            // ordinary deficit hiring.
            let overTarget colony fleet =
                Expect.isEmpty
                    (casts colony (List.truncate (List.length fleet - 1) fleet))
                    "the premise: a body short of this fleet the colony is still over target"

            test "the tick a source unposts, the home room's empty Post is cast for anyway" {
                // #154's reproduction, and the reason the gate moved. The
                // colony loses vision of its outpost for one tick: the
                // source there unposts, and its Anchor place, its haul and
                // its income share leave the target together (ADR 0042,
                // ADR 0004), dropping it under the living count. The home
                // room's Post is empty across both ticks and is a fact
                // about the ground either way — gated on the deficit it
                // went unfilled until ordinary deaths had paid off the
                // whole thirteen-body overshoot, and the colony cast
                // nothing at all, in its own room included, in the
                // meantime.
                //
                // Pairwise, one rival at a time: the two fleets differ in
                // the two Anchors' bodies and in nothing else.
                Expect.isEmpty
                    (casts switchUnposted switchFleet)
                    "with every row manned the same twenty-six cast nothing"

                overTarget switchUnposted switchFleet

                match casts switchUnposted unmannedPosts with
                | [ (_, body, name) ] ->
                    Expect.stringStarts
                        name
                        "anchor-"
                        "the empty Post is filled from the Anchor row"

                    Expect.equal
                        body
                        [ Work; Work; Carry; Move ]
                        "and sized to the bank exactly as that row always is"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "a standing container's hauler gap is filled under the target too" {
                // The same rule on the row beside it (ADR 0012): a source
                // container standing wants its round trip shipped whatever
                // the headcount is, and the tick the target fell the
                // container did not stop standing. Both Anchors stay alive
                // here, so the Anchor row has no gap and the hauler row is
                // the only rival the cast can come from.
                Expect.isEmpty
                    (casts switchUnposted switchFleet)
                    "with every row manned the same twenty-six cast nothing"

                overTarget switchUnposted switchFleet

                match casts switchUnposted unshippedFleet with
                | [ (_, _, name) ] ->
                    Expect.stringStarts
                        name
                        "hauler-"
                        "the home container's own round trip hires it"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "the worker row is the one the deficit is the quota of, and it still stops" {
                // The half of the gate that does not move (ADR 0012): the
                // worker row's quota *is* whatever the target has left over
                // once the specialist rows are counted, so with nothing
                // left over it hires nobody however far the fleet has
                // overshot. Pairwise against the same fleet under a target
                // that reaches it — one room's vision richer, where those
                // twenty-six are the target — and one body short there is a
                // worker.
                Expect.isEmpty
                    (casts switchUnposted switchFleet)
                    "thirteen over target, every row manned, and no generalist"

                overTarget switchUnposted switchFleet

                match casts switchPosted (List.truncate 25 switchFleet) with
                | [ (_, _, name) ] ->
                    Expect.stringStarts
                        name
                        "worker-"
                        "posted, the target reaches the fleet and the remainder is the worker row's"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "a row standing over its quota still holds the worker row down" {
                // What the deficit is and is not (ADR 0012). It gates the
                // worker row; it is not that row's own gap, and the
                // difference shows the tick a specialist row stands over
                // quota. Under `switchUnposted` the Anchor row wants one
                // and the hauler row two: a fleet of two Anchors, five
                // haulers and six workers is thirteen bodies exactly at
                // the target, four of them surplus specialists, and the
                // worker row is four short of its own quota of ten. The
                // surplus holds it there — #154 moves the specialist rows
                // off the deficit and deliberately leaves this half of the
                // gate standing.
                //
                // Pairwise against the same target with the specialist
                // rows at quota, where the whole-fleet gap and the worker
                // row's own gap coincide and one body short is a worker.
                let overSpecialised =
                    switchFleet
                    |> List.filter (fun creep ->
                        not (List.contains creep.Name [ for i in 7..19 -> $"w{i}" ]))

                Expect.hasLength
                    overSpecialised
                    13
                    "the premise: thirteen bodies, standing exactly at the target"

                Expect.isEmpty
                    (casts switchUnposted overSpecialised)
                    "four surplus specialists, and the worker row hires none of its four missing"

                match casts switchUnposted (List.truncate 12 switchHomeFleet) with
                | [ (_, _, name) ] ->
                    Expect.stringStarts
                        name
                        "worker-"
                        "with every specialist row at quota the same shortfall is hired"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test
                "the doorstep hold still comes first: an empty Post is no reason to cast into a Reach" {
                // ADR 0033's gate is asked before anything is priced and
                // this ticket does not move it (#154). The row gap that
                // now outlives a negative deficit is exactly the case that
                // could have walked past it — the hold is the outer
                // question, the deficit an inner one.
                let hot =
                    switchUnposted |> facing [ hostileAt "h-1" { X = 25; Y = 13 } [ Attack; Move ] ]

                Expect.isNonEmpty
                    (casts switchUnposted unmannedPosts)
                    "the premise: quiet, the empty Post is cast for"

                Expect.isEmpty
                    (casts hot unmannedPosts)
                    "and under fire the same empty Post casts nothing"
            }
        ]

/// The ticket's reproduction (#138): the home corridor with `src-home`
/// midway down it and one worker above, and the outpost's corridor with
/// `src-out` near its foot and one worker two tiles short of it. Two rooms
/// whose corridors share a column, so every coordinate the outpost's
/// worker stands on is a coordinate the home room could hold a hostile at
/// — the shape a Reach filed without its room would collapse.
let private twoRoomColony (hostiles: HostileInfo list) =
    let colony =
        northBorderColony { X = 10; Y = 20 }
        |> withNorthOutpost (Some { X = 10; Y = 47 })

    { colony with
        Creeps = [ worker "wh" 0 50; worker "wo" 0 50 ]
        Hostiles = hostiles
        Spatial =
            colony.Spatial
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = Map.ofList [ "wh", { X = 10; Y = 10 } ]
                })
            |> withNeighbour
                "W1N2"
                { RoomLayer.empty with
                    Terrain = Map.ofList (corridor 10 40 48)
                    TargetPositions = Map.ofList [ "src-out", { X = 10; Y = 47 } ]
                    CreepPositions = Map.ofList [ "wo", { X = 10; Y = 45 } ]
                }
    }

/// A hostile filed under the room it stands in — the field `facing`
/// stamps with the home name, set by hand here because these fixtures
/// put a hostile in either room (ADR 0041).
let private hostileIn room pos body =
    { hostileAt "h-1" pos body with
        RoomName = room
    }

[<Tests>]
let layeredThreatTests =
    testList
        "threats by room"
        [
            // The Reach follows the projection's layering (#138, ADR 0041):
            // a hostile's `RoomName` is what its Reach is filed under, and
            // each creep is judged against its own room's share. A room
            // with no entry has an empty Reach, which blocks nothing (ADR
            // 0004) — so the quiet tick is the yardstick every case below
            // is measured against, byte for byte.
            test "a home Threat digs no hole in the outpost: the coordinate is another room's" {
                // The ticket's trace: one melee hostile at (10,45) in W1N1, a
                // tile the home corridor does not even cover, shares its
                // coordinate with the outpost's worker fifty tiles and a
                // border away. Before #138 that worker dropped its source
                // and fled.
                let quiet = twoRoomColony []

                Expect.equal
                    (let _, assignments, _ = outcomeOf quiet in Map.toList assignments)
                    [ "wh", taskId (Harvest "src-home"); "wo", taskId (Harvest "src-out") ]
                    "each worker digs the source of its own room"

                let raided = twoRoomColony [ hostileIn "W1N1" { X = 10; Y = 45 } [ Attack; Move ] ]

                Expect.equal
                    (outcomeOf raided)
                    (outcomeOf quiet)
                    "a Reach in the home room reaches no tile of the outpost"
            }

            test "and the mirror: an outpost Threat digs no hole at home" {
                // The same hostile filed under W1N2, on the coordinate the
                // home worker stands on — pairwise with the case above,
                // because a Reach collapsed onto one set would fail both
                // and a Reach keyed on the home room alone would pass one.
                let quiet = twoRoomColony []

                let raided = twoRoomColony [ hostileIn "W1N2" { X = 10; Y = 10 } [ Attack; Move ] ]

                Expect.equal
                    (outcomeOf raided)
                    (outcomeOf quiet)
                    "a Reach in the outpost reaches no tile of the home room"
            }

            test "an outpost creep flees over its own room's ground, and the home creep works on" {
                // A ranged hostile at (10,42) in W1N2 reaches y 37..47 of
                // that room: the outpost's worker at (10,45) is inside, and
                // the only safe ground its room has left is (10,48), below
                // it. Its Safe set is its own room's walkable ground less
                // that Reach — were it the home room's, as it was before
                // #138, every safe tile would lie in a room the creep's
                // flood cannot enter, and Flee would be priced unreachable.
                let intents, assignments, _ =
                    outcomeOf (
                        twoRoomColony [ hostileIn "W1N2" { X = 10; Y = 42 } [ RangedAttack; Move ] ]
                    )

                Expect.equal
                    (Map.toList assignments)
                    [ "wh", taskId (Harvest "src-home"); "wo", taskId Flee ]
                    "the outpost worker flees; the home worker keeps its dig"

                Expect.equal
                    (moveIntentsFor "wo" intents)
                    [ MoveCreep("wo", Bottom) ]
                    "and runs down its own corridor to the one tile its room has outside the Reach"

                Expect.equal
                    (moveIntentsFor "wh" intents)
                    [ MoveCreep("wh", Bottom) ]
                    "while the home worker walks to its source as on a quiet tick"
            }

            test
                "the spawn hold reads the spawn's own room: an outpost Threat beside its coordinate holds nothing" {
                // ADR 0033's hold, pairwise: the same hostile on the same
                // coordinate beside the spawn, first filed under the
                // outpost, then under the home room.
                let room =
                    atLevel 2 (openRoom 6)
                    |> withOutpost
                        "W1N2"
                        []
                        [
                            for x in 20..30 do
                                for y in 20..30 -> { X = x; Y = y }, Plain
                        ]

                let colony =
                    { room with
                        Creeps = [ worker "w1" 0 100 ]
                        Spatial =
                            room.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 25; Y = 27 } ]
                                })
                    }

                let castsWith hostiles =
                    let { Intents = intents } =
                        decide { colony with Hostiles = hostiles } Map.empty Set.empty None

                    spawnIntents intents

                Expect.isNonEmpty (castsWith []) "a quiet colony casts its deficit"

                Expect.isNonEmpty
                    (castsWith [ hostileIn "W1N2" { X = 25; Y = 29 } [ Attack; Move ] ])
                    "a Threat in the outpost holds no spawn at home"

                Expect.isEmpty
                    (castsWith [ hostileIn "W1N1" { X = 25; Y = 29 } [ Attack; Move ] ])
                    "the same Threat at home holds it"
            }
        ]

/// The outpost the Reserve tests hold: one room across the north border,
/// its controller declared under the engine's own id and laid into the
/// projection the way the shell lays one (`Outpost.place`, ADR 0041) — so
/// it is in the pool with no vision at all, and its tile is an obstacle,
/// which is what puts the reserver beside the controller and never on it.
///
/// No rock declared. The pool these fixtures want is the one Reserve and
/// nothing else, because the Matcher scores a winner against its cheapest
/// rival alone: a second candidate would leave every comparison below
/// reporting on some third Task.
let private reserveDeclaration =
    {
        RoomName = "W1N2"
        Sources = []
        // Off the corridor on purpose: a controller stands in `Obstacles`
        // whether vision found it or a declaration laid it, and one on the
        // corridor would seal the room rather than leave the tiles beside
        // it to stand on.
        Controller = "ctrl-out", { X = 11; Y = 44 }
    }

/// The colony the Reserve tests run in: the home corridor with no Task in
/// it at all, the declared outpost across the border, and the creeps the
/// test names standing in that outpost — filed under its own layer,
/// because a creep is placed in the room it stands in and nowhere else
/// (ADR 0041).
let private reserveColony (creeps: (CreepInfo * Pos) list) =
    let colony = northBorderColony { X = 10; Y = 38 } |> withNorthOutpost None
    let spatial = Outpost.place [ reserveDeclaration ] colony.Spatial
    let outpost = SpatialInfo.layerOf spatial "W1N2"

    { colony with
        Sources = []
        Creeps = creeps |> List.map fst
        Spatial =
            spatial
            |> withNeighbour
                "W1N2"
                { outpost with
                    CreepPositions =
                        creeps |> List.map (fun (creep, pos) -> creep.Name, pos) |> Map.ofList
                }
    }

/// The second outpost, across the *west* border: `Outpost.declared` takes
/// two rooms at once (ADR 0042) and one of them is not enough to tell "one
/// reserver per outpost" apart from "every reserver on whichever
/// controller is nearest". Its controller is off its corridor for the same
/// reason the north one is, and the two tiles left beside it are the
/// declared shape W12S27's `37,43` really has.
let private westReserveDeclaration =
    {
        RoomName = "W2N1"
        Sources = []
        Controller = "ctrl-west", { X = 41; Y = 25 }
    }

/// The colony with both outposts declared and a west arm of home leading
/// to the second: home's `1,26` opens onto W2N1's `48,26` (ADR 0041 reads
/// the join out of the two room names), and the west corridor runs from
/// there to the tiles beside `ctrl-west`. The creeps stand in that arm, a
/// dozen steps from the west controller and some thirty-five from the
/// north one — so travel cost alone prefers the *same* controller for
/// every one of them, which is what makes the per-Task cap the only thing
/// that can spread them.
let private twoOutpostColony (creeps: (CreepInfo * Pos) list) =
    let colony = reserveColony []

    let spatial =
        { colony.Spatial with
            Borders = Map.add "W2N1" plainRing colony.Spatial.Borders
        }
        |> withHome (fun layer ->
            { layer with
                Terrain =
                    (layer.Terrain, [ for x in 1..10 -> { X = x; Y = 26 } ])
                    ||> List.fold (fun terrain pos -> Map.add pos Plain terrain)
                CreepPositions =
                    creeps |> List.map (fun (creep, pos) -> creep.Name, pos) |> Map.ofList
            })
        |> withNeighbour
            "W2N1"
            { RoomLayer.empty with
                Terrain = Map.ofList [ for x in 41..48 -> { X = x; Y = 26 }, Plain ]
            }
        |> Outpost.place [ westReserveDeclaration ]

    { colony with
        Creeps = creeps |> List.map fst
        Spatial = spatial
    }

/// ADR 0042's own reserver: two CLAIM parts and two Move, 1,300 energy.
/// Carrying nothing and with nowhere to put anything — a CLAIM body has no
/// Carry part — so no gate below can be passing on an energy state.
let private reserver name =
    creepWith name 0 0 [ Claim; Claim; Move; Move ]

let private reserveTasks tasks =
    tasks
    |> List.choose (function
        | Reserve controllerId -> Some controllerId
        | _ -> None)

[<Tests>]
let reserveTests =
    testList
        "reserve"
        [
            test
                "an outpost's controller is a Reserve; the colony's own is Upgraded, never reserved" {
                // The pool rule (ADR 0042), read off the projection's kind
                // census: every controller in it but ours. The colony's own
                // is excluded by id — the engine refuses reserveController
                // on a room it owns — so the two controllers here answer
                // the two different Tasks a controller can carry.
                let colony =
                    { bareRespawn with
                        Sources = []
                        Refillables = []
                        Spatial =
                            { SpatialInfo.empty with
                                TargetKinds =
                                    Map.ofList [ "ctrl-1", Controller; "ctrl-out", Controller ]
                            }
                    }

                let tasks = planTasks colony noThreats

                Expect.equal
                    (reserveTasks tasks)
                    [ "ctrl-out" ]
                    "the outpost's controller is the one Reserve in the pool"

                Expect.contains tasks (Upgrade "ctrl-1") "and the colony's own is still Upgraded"

                Expect.isEmpty
                    (reserveTasks (planTasks bareRespawn noThreats))
                    "a colony projecting one room reserves nothing: the pool is the pool it always was"
            }

            test "a CLAIM body is matched to the outpost's Reserve and reserves it" {
                // The whole path in one tick (ADR 0042): the Task is pooled
                // off the declaration, the CLAIM body is the one body it
                // applies to, the Matcher hands it over, and the Emitter
                // issues the reserve. The creep stands at (10,44), one tile
                // from the controller at (11,44) — inside the Work Area
                // already, so the act is this tick's and not a walk's.
                let {
                        Assignments = assignments
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decide
                        (reserveColony [ reserver "r1", { X = 10; Y = 44 } ])
                        Map.empty
                        Set.empty
                        None

                Expect.equal
                    (Map.tryFind "r1" assignments)
                    (Some(taskId (Reserve "ctrl-out")))
                    "the reserver holds the outpost's controller"

                Expect.contains
                    verdicts
                    (Verdict.Matched("r1", taskId (Reserve "ctrl-out"), MatchFactor.OnlyCandidate))
                    "and it is the only Task in the pool it fits"

                Expect.contains
                    intents
                    (ReserveController("r1", "ctrl-out"))
                    "the Intent is the engine's reserve act, aimed at the declared controller"

                Expect.contains
                    intents
                    (SayCreep("r1", "🚩"))
                    "and the bubble carries the Reserve glyph"
            }

            test "a body with no CLAIM part is never matched to Reserve" {
                // Pairwise against the test above: the same colony, the
                // same tile beside the same controller, one body swapped.
                // A generalist can do everything else this colony ever asks
                // and cannot push a reservation up by a tick.
                let {
                        Assignments = assignments
                        Intents = intents
                    } =
                    decide
                        (reserveColony [ worker "w1" 0 50, { X = 10; Y = 44 } ])
                        Map.empty
                        Set.empty
                        None

                Expect.isEmpty
                    (Map.toList assignments)
                    "the one Task in the pool asks for a part this body has none of"

                Expect.isEmpty
                    (intents
                     |> List.filter (function
                         | ReserveController _ -> true
                         | _ -> false))
                    "and nothing reserves anything"
            }

            test "a CLAIM body fits no other Task: without a Reserve it stands still" {
                // ADR 0042's pairing rule, in as many words: every other
                // Task gates on a Work part or a Carry part and a
                // `[2Claim;2Move]` body has neither, so a reserver cast
                // before this Task existed would have stood where it was
                // born for its whole 600-tick life. It is also why the
                // quota may not arrive before the Task (#131).
                let colony =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        ConstructionSites = [ { Id = "site-1" } ]
                        Refillables = [ refillable "spawn-1" 300 BuiltKind.Spawn ]
                        Creeps = [ reserver "r1" ]
                        Spatial =
                            { SpatialInfo.empty with
                                TargetKinds =
                                    Map.ofList
                                        [
                                            "cont-1", Structure BuiltKind.Container
                                            "pile-1", Dropped
                                        ]
                                Stores = Map.ofList [ "cont-1", 500; "pile-1", 150 ]
                            }
                    }
                    |> withHits "road-1" BuiltKind.Road 100 5000

                let pool = planTasks colony noThreats

                Expect.equal
                    (pool |> List.map taskId |> List.sort)
                    (List.sort
                        [
                            taskId (Harvest "src-a")
                            taskId (Withdraw "cont-1")
                            taskId (Pickup "pile-1")
                            taskId (Refill "spawn-1")
                            taskId (Build "site-1")
                            taskId (Repair "road-1")
                            taskId (Upgrade "ctrl-1")
                        ])
                    "the premise: every Task but Reserve and Flee is in the pool"

                let { Assignments = assignments } = decide colony Map.empty Set.empty None

                Expect.isEmpty
                    (Map.toList assignments)
                    "and the CLAIM body is applicable to none of them"
            }

            test "a reserver under fire runs: Safety outranks the tier Reserve sits on" {
                // The one comparison the tier choice actually settles
                // today. Reserve is on the feeding tier — ADR 0042's own
                // argument for casting the row first is that it decides
                // whether the income is five a tick or ten — and Safety
                // sits above every tier of work (ADR 0033), so a reserver
                // being shot at leaves the controller. Both Tasks are in
                // this creep's pool: the Reach takes two of the
                // controller's three standing tiles and leaves one, so
                // Reserve is applicable and loses on rank rather than
                // vanishing.
                let colony = reserveColony [ reserver "r1", { X = 10; Y = 44 } ]

                let raided =
                    { colony with
                        Hostiles = [ hostileIn "W1N2" { X = 10; Y = 41 } [ Attack; Move ] ]
                    }

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide raided Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "r1" assignments)
                    (Some(taskId Flee))
                    "the reserver runs rather than holding the reservation"

                Expect.contains
                    verdicts
                    (Verdict.Matched("r1", taskId Flee, MatchFactor.Rank))
                    "and rank is what separated the two: Safety above Feeding"
            }

            test "one reserver per controller: the second is pushed to the outpost nobody holds" {
                // ADR 0042 casts one reserver *per posted outpost* — "two
                // reservers at 4.33 energy a tick buy three sources their
                // second five" — and a second body on a controller the
                // first already holds buys nothing at all, because a
                // reservation is capped and one body's CLAIM parts are
                // sized to hold it. Travel cost cannot produce that on its
                // own: both bodies stand in the west arm, both price the
                // west controller cheapest, and `load` is only the key's
                // third component, so it never separates two candidates
                // whose costs differ. The per-Task cap is what does, and
                // without it the north outpost is pooled, applicable and
                // matched by nobody for the whole 600-tick life of both
                // creeps — silently, since both report Matched.
                let colony =
                    twoOutpostColony
                        [ reserver "r1", { X = 5; Y = 26 }; reserver "r2", { X = 6; Y = 26 } ]

                let { Assignments = assignments } = decide colony Map.empty Set.empty None

                Expect.equal
                    (assignments |> Map.toList |> List.map snd |> List.sort)
                    [ taskId (Reserve "ctrl-out"); taskId (Reserve "ctrl-west") ]
                    "the two reservers hold the two declared controllers, one each"
            }

            test "a controller in a room this colony owns is not pooled at all" {
                // The other half of #181's fact, at the seam it is decided
                // on: the engine refuses reserveController on a room we
                // own, so that room's controller is not a Task. The pool
                // excluded the colony's own controller by *id*, which said
                // the same thing only while home was the only room the
                // colony owned — the tick a declared outpost is claimed it
                // stops saying it, and a Task no body can execute is one
                // the Matcher fills all the same.
                //
                // Pairwise on the one fact: the same declaration, the same
                // controller, the same projection, ownership the only
                // input that moves.
                let pooledUnder control =
                    let colony = reserveColony []

                    { colony with
                        RoomControl = colony.RoomControl |> Map.add "W1N2" control
                    }
                    |> fun colony -> planTasks colony noThreats
                    |> reserveTasks

                Expect.equal
                    (pooledUnder neutralRoom)
                    [ "ctrl-out" ]
                    "a neutral outpost's controller is the Reserve it always was"

                Expect.isEmpty
                    (pooledUnder ownedRoom)
                    "the same controller, in a room this colony owns, offers a CLAIM body nothing"
            }

            test "the row's one reserver walks past the outpost we own to the one we do not" {
                // #181's live shape, at the seam the bug actually bites:
                // home, a near declared outpost the user has just claimed,
                // and a farther one still neutral. The row hires one body
                // — a room we own is not a room to reserve — and travel
                // cost alone would spend it on the near controller, which
                // is exactly the controller the engine refuses. Nothing
                // between the pool and the Matcher reads ownership, so the
                // pool is where that has to be settled, and this is the
                // test that says so: with the near room owned the body
                // must cross to the far one.
                //
                // Pairwise on ownership, one creep, so the cap cannot be
                // what spreads them: the same colony with the west room
                // neutral keeps the body on the west controller.
                let assignedUnder control =
                    let colony = twoOutpostColony [ reserver "r1", { X = 5; Y = 26 } ]

                    { colony with
                        RoomControl = colony.RoomControl |> Map.add "W2N1" control
                    }
                    |> fun colony -> decide colony Map.empty Set.empty None
                    |> fun result -> Map.tryFind "r1" result.Assignments

                Expect.equal
                    (assignedUnder neutralRoom)
                    (Some(taskId (Reserve "ctrl-west")))
                    "neutral, the near controller is the cheapest walk and the body takes it"

                Expect.equal
                    (assignedUnder ownedRoom)
                    (Some(taskId (Reserve "ctrl-out")))
                    "owned, the near controller is no Task and the body crosses to the neutral one"
            }
        ]

/// A colony standing exactly at its Workforce target with one reserver in
/// it: no Post, no source container and no placed rock, so the target is
/// the floor of two and the two living creeps meet it. One body leaving
/// the count is therefore one cast, which is what makes a lead readable
/// (ADR 0026). The bank is 1,300 — ADR 0042's own reserver body at
/// capacity — and the reserver stands at (25,29), three plain steps from
/// the tile a replacement is born on.
let private leadColony life =
    let room = atLevel 2 (openRoom 6)

    { room with
        RoomEnergy = bank 1300 1300
        Creeps = [ worker "w1" 0 50; reserver "r1" |> withLife life ]
        Spatial =
            room.Spatial
            |> withHome (fun layer ->
                { layer with
                    CreepPositions =
                        Map.ofList [ "w1", { X = 25; Y = 27 }; "r1", { X = 25; Y = 29 } ]
                })
    }

[<Tests>]
let reserverLeadTests =
    testList
        "the reserver row's lead"
        [
            test "a CLAIM body's lead is the reserver row's, not the generalist's" {
                // `patternOf` reads a living body back to the row it was
                // cast from (ADR 0006), and the row is what sizes the
                // replacement a lead prices (ADR 0026). A `[Claim; Move]`
                // body has neither Work nor Carry, so before ADR 0042's row
                // existed it fell through to the generalist and was priced
                // as one.
                //
                // The two arithmetics, at this colony's 1,300 bank: the
                // reserver row casts `[2Claim;2Move]`, four parts, 12 ticks
                // in the spawner, and its two Move carry its two fatigue
                // parts over a plain tile in the walk's one-tick floor — 3
                // ticks for the three steps, a lead of 15. The generalist
                // row at the same bank is twenty parts: 60 ticks in the
                // spawner and the same 3 of walking, a lead of 63. Every
                // life between the two is where the rows disagree.
                let casts life =
                    spawnIntents (decide (leadColony life) Map.empty Set.empty None).Intents

                Expect.isEmpty
                    (casts 30)
                    "at 30 ticks the reserver still counts; read off the generalist row it would not"

                Expect.isEmpty (casts 16) "one tick outside its own row's lead it still counts"

                match casts 15 with
                | [ (_, body, creepName) ] ->
                    Expect.stringStarts
                        creepName
                        "worker-"
                        "at its lead the colony is one short — and casts a generalist, this room declaring no outpost for the reserver row to hire against"

                    Expect.isFalse
                        (List.contains Claim body)
                        "the row that replaces a reserver is the reserver row's quota, and here it is zero"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }
        ]

/// One outpost's furniture in a layer of its own: the rock in a
/// three-Seat field, a controller — what a CLAIM body walks to, and what
/// pools the Reserve Task (#130) — and, when `posted`, a built container
/// on one of those Seats: the switch that makes the rock a Post and
/// admits the room to the economy (#129). Ids carry the room's name, so
/// two outposts stand in one projection without colliding.
let private withOutpostRoom room (rock: Pos) posted (colony: Snapshot) =
    let container =
        if posted then
            [ $"can-{room}", { rock with X = rock.X - 1 }, Structure BuiltKind.Container ]
        else
            []

    { colony with
        Sources = colony.Sources @ [ source $"src-{room}" ]
    }
    |> withOutpost
        room
        ([
            $"src-{room}", rock, Source
            $"ctrl-{room}", { rock with Y = rock.Y + 2 }, Controller
         ]
         @ container)
        (threeSeatField rock)

/// The reserver row's colony: the W12S28 shape at the live RCL5 bank of
/// 1,800 — the level ADR 0042's `[2Claim;2Move]` is priced against — with
/// the named outposts standing beside it and the named holder on each
/// room. Everything else is `incomeColony`, unmoved, so a difference
/// between two calls is the outposts, the fleet or the reservation and
/// nothing else. The bank holds 8,000 against that capacity: restraint in
/// these cases must come from the rows, never from the bank running dry.
let private reserverColony outposts creeps control =
    let colony =
        ({ incomeColony with
            RoomEnergy = bank 8000 1800
            Creeps = creeps
         },
         outposts)
        ||> List.fold (fun acc (room, rock, posted) -> withOutpostRoom room rock posted acc)

    { colony with
        RoomControl =
            (colony.RoomControl, control)
            ||> List.fold (fun acc (room, holder) -> Map.add room holder acc)
    }

/// The north outpost and the west one, diagonal to each other as W12S27
/// and W13S28 are (ADR 0042) — two rooms, so "one reserver per declared
/// outpost" can be told apart from "one reserver".
let private northOutpost posted = "W1N2", { X = 40; Y = 40 }, posted
let private westOutpost posted = "W2N2", { X = 20; Y = 40 }, posted

/// A fleet standing over every row's quota but the reserver's: one Anchor
/// per Post — the home room's two plus one for each posted outpost — four
/// haulers where the row wants two, and forty workers where the income
/// base hires a handful. Every other gap is therefore zero and the
/// whole-fleet deficit is negative, so a `SpawnCreep` in these cases is a
/// reserver or it is a defect.
let private surplusFleet posts =
    [ for i in 1..posts -> anchor $"a{i}" 0 50 ]
    @ [ for i in 1..4 -> hauler $"h{i}" 0 100 ]
    @ [ for i in 1..40 -> worker $"w{i}" 0 50 ]

/// The bodies of this tick's reserver casts, in casting order.
let private reserverCasts intents =
    spawnIntents intents
    |> List.filter (fun (_, _, name: string) -> name.StartsWith "reserver-")
    |> List.map (fun (_, body, _) -> body)

/// The one block the row never casts below, and the body a reservation
/// standing at its 5,000 cap asks for: the deficit is zero and the floor
/// is one.
let private oneBlock = [ Claim; Move ]

/// ADR 0042's own reserver body, which a deficit of one to 1,200 ticks
/// buys: 1,300 energy, 2.17 a tick over a CLAIM part's 600-tick life.
let private twoBlocks = [ Claim; Claim; Move; Move ]

[<Tests>]
let reserverRowTests =
    testList
        "the reserver row"
        [
            test "a declared outpost hires one reserver, posted or not" {
                // The one quota the container switch does *not* gate
                // (#131's correction comment): gating it deadlocks the
                // chain, because a container site needs vision, vision
                // needs a creep in the room, and this is the only creep
                // with a reason to go. ADR 0042's Considered Options is the
                // authority its Consequences clause contradicts — it
                // rejected "mine first, reserve later" precisely so the
                // reservation is standing before the first hauler is sized.
                //
                // Pairwise on the one structure — same room, same rock,
                // same controller, same reservation — because that is the
                // only input that moves.
                let castsWith posted =
                    let fleet = surplusFleet (if posted then 3 else 2)

                    reserverColony [ northOutpost posted ] fleet [ "W1N2", reservedRoom true 5000 ]
                    |> fun colony -> decide colony Map.empty Set.empty None
                    |> fun result -> spawnIntents result.Intents

                for posted in [ true; false ] do
                    match castsWith posted with
                    | [ (_, body, creepName) ] ->
                        Expect.stringStarts
                            creepName
                            "reserver-"
                            $"the one cast is the reserver row's (posted: %b{posted})"

                        Expect.equal body oneBlock "and its body holds a CLAIM part"
                    | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "two declared outposts hire two reservers, one apiece" {
                // Never one rover: `[4Claim;4Move]` is 2,600 energy and so
                // an RCL7 body, and two outposts diagonal to each other
                // share no exit — a rover would spend its 600-tick life
                // crossing the home room (ADR 0042).
                let colony =
                    reserverColony
                        [ northOutpost true; westOutpost true ]
                        (surplusFleet 4)
                        [ "W1N2", reservedRoom true 5000; "W2N2", reservedRoom true 5000 ]

                Expect.equal
                    (reserverCasts (decide colony Map.empty Set.empty None).Intents)
                    [ oneBlock; oneBlock ]
                    "one body per declared outpost, and the four idle spawns cast no third"

                let half =
                    reserverColony
                        [ northOutpost true; westOutpost false ]
                        (surplusFleet 3)
                        [ "W1N2", reservedRoom true 5000; "W2N2", reservedRoom true 5000 ]

                Expect.equal
                    (reserverCasts (decide half Map.empty Set.empty None).Intents)
                    [ oneBlock; oneBlock ]
                    "and the one still waiting for its container is hired for just the same"
            }

            test "a bank that cannot buy one block hires no reserver and still casts" {
                // The row's floor body is 650 — larger than every other
                // row's, and larger than the whole bank below RCL3. Being
                // first in the cascade, a gap it can never fill would stop
                // every row under it forever: `planned` counts intents, so
                // an uncast head row leaves every idle spawn asking for the
                // head row again, in the home room included. So a bank that
                // cannot afford the floor hires the row not at all.
                let intentsAt capacity =
                    let colony =
                        reserverColony
                            [ northOutpost false ]
                            [ worker "w1" 0 50 ]
                            [ "W1N2", neutralRoom ]

                    { colony with
                        RoomEnergy = bank capacity capacity
                    }
                    |> fun colony -> decide colony Map.empty Set.empty None
                    |> fun result -> result.Intents

                Expect.isEmpty
                    (reserverCasts (intentsAt 550))
                    "at an RCL2 bank of 550 the row has no quota at all"

                match spawnIntents (intentsAt 550) with
                | (_, _, creepName) :: _ ->
                    Expect.stringStarts
                        creepName
                        "anchor-"
                        "and the row under it casts rather than the colony freezing"
                | [] -> failtest "expected the rows under the reserver to cast"

                Expect.equal
                    (reserverCasts (intentsAt 650))
                    [ oneBlock ]
                    "one block's worth of capacity is where the row starts hiring"
            }

            test "a living reserver fills the quota; one inside its lead does not" {
                // The quota counts bodies and not rooms (#130): which
                // controller each body ends up holding is the Reserve
                // Task's one-holder-per-controller capacity, so a reserver
                // still walking to its outpost already fills the row's
                // place. Its succession is ADR 0026's existing path and
                // nothing new: inside its lead it leaves the count, and the
                // replacement is cast while it still holds the reservation.
                let colonyWith reservers =
                    reserverColony
                        [ northOutpost true ]
                        (surplusFleet 3 @ reservers)
                        [ "W1N2", reservedRoom true 5000 ]

                let placed life =
                    let colony = colonyWith [ reserver "r1" |> withLife life ]

                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "r1", { X = 22; Y = 10 } ]
                                })
                    }

                Expect.isEmpty
                    (reserverCasts (decide (placed 1500) Map.empty Set.empty None).Intents)
                    "a reserver with a life ahead of it is the row's one body"

                Expect.equal
                    (reserverCasts (decide (placed 5) Map.empty Set.empty None).Intents)
                    [ oneBlock ]
                    "inside its lead it is already outside the count, so the successor is cast"
            }

            test "the reserver row casts in front of the Anchor, hauler and worker rows" {
                // ADR 0042's ordering: the other three rows spend income
                // and this one decides whether the income is five a tick or
                // ten across every source of an outpost at once — and it is
                // the cheapest body on the table, so the row it displaces
                // for a tick waits on 650 energy.
                //
                // The fleet is short in every row at once: two Anchors
                // against three Posts, one hauler against the two-body
                // quota, and a whole-fleet deficit under all of it. Four
                // idle spawns cast one body each, so the whole order is
                // readable in one tick.
                let colony =
                    reserverColony
                        [ northOutpost true ]
                        ([ anchor "a1" 0 50; anchor "a2" 0 50; hauler "h1" 0 100 ])
                        [ "W1N2", reservedRoom true 5000 ]

                match spawnIntents (decide colony Map.empty Set.empty None).Intents with
                | [ (_, firstBody, firstName)
                    (_, _, secondName)
                    (_, _, thirdName)
                    (_, _, fourthName) ] ->
                    Expect.stringStarts firstName "reserver-" "the reservation is cast for first"
                    Expect.equal firstBody oneBlock "at the deficit's own body, not the bank's"
                    Expect.stringStarts secondName "anchor-" "then the empty Post"
                    Expect.stringStarts thirdName "hauler-" "then the throughput quota"
                    Expect.stringStarts fourthName "worker-" "and the generalist last"
                | other -> failtest $"expected exactly four SpawnCreep intents, got %A{other}"
            }

            test "the body grows by a CLAIM part for every 600 ticks the reservation has lost" {
                // ADR 0042's one rule, quota and sizing in the same
                // expression: `ceil((5000 − ticks held) / 600)` CLAIM
                // parts. No state between ticks — the deficit is read off
                // the reservation itself, so the row shrinks to its floor
                // in steady state and comes back bigger on its own the
                // tick a reservation has slipped.
                let castFor held =
                    reserverColony
                        [ northOutpost true ]
                        (surplusFleet 3)
                        [ "W1N2", reservedRoom true held ]
                    |> fun colony -> decide colony Map.empty Set.empty None
                    |> fun result -> reserverCasts result.Intents

                Expect.equal (castFor 5000) [ oneBlock ] "at the cap the deficit is zero: the floor"

                Expect.equal
                    (castFor 4400)
                    [ oneBlock ]
                    "600 ticks lost is one part: one CLAIM holds a reservation up through a whole CLAIM life"

                Expect.equal (castFor 4399) [ twoBlocks ] "the 601st lost tick is the second part"
                Expect.equal (castFor 3800) [ twoBlocks ] "and 1,200 lost is still the second"
            }

            test "the bank truncates the deficit, and the deficit truncates the bank" {
                // The two halves of the sizing rule, each shown cutting the
                // other off. ADR 0042 refuses the bank *as the rule*: at
                // RCL6 a 2,300 bank would buy a third CLAIM for a
                // reservation that caps at 5,000 anyway — which is why the
                // pair below is read at 2,300 and not at today's 1,800,
                // where the two rules agree.
                let castAt capacity held =
                    let colony =
                        reserverColony
                            [ northOutpost true ]
                            (surplusFleet 3)
                            [ "W1N2", reservedRoom true held ]

                    { colony with
                        RoomEnergy = bank 8000 capacity
                    }
                    |> fun colony -> decide colony Map.empty Set.empty None
                    |> fun result -> reserverCasts result.Intents

                Expect.equal
                    (castAt 2300 5000)
                    [ oneBlock ]
                    "a full reservation asks for one block, and the RCL6 bank's three do not overrule it"

                Expect.equal
                    (castAt 2300 0)
                    [ List.replicate 3 Claim @ List.replicate 3 Move ]
                    "a reservation on the floor asks for nine parts and gets the three the bank buys"

                Expect.equal
                    (castAt 8000 0)
                    [ List.replicate 9 Claim @ List.replicate 9 Move ]
                    "at a bank that affords them, the deficit's own nine"
            }

            test "a reservation another player holds leaves this colony holding nothing" {
                // Pairwise, one holder at a time: the same room, the same
                // ticks on the same controller, and only whose it is moves.
                // The colony's hold starts at zero under a rival's
                // reservation, exactly as that room's sources stay at the
                // neutral rate — a hold somebody else owns is not one this
                // row can measure its deficit from.
                //
                // Read at a bank that affords the whole deficit and not at
                // the live 1,800, where two parts and nine both truncate to
                // two and the pair could not tell the `Ours` filter from
                // its absence.
                let castWith control =
                    let colony = reserverColony [ northOutpost true ] (surplusFleet 3) control

                    { colony with
                        RoomEnergy = bank 8000 8000
                    }
                    |> fun colony -> decide colony Map.empty Set.empty None
                    |> fun result -> reserverCasts result.Intents

                let nineBlocks = List.replicate 9 Claim @ List.replicate 9 Move

                Expect.equal
                    (castWith [ "W1N2", reservedRoom true 4000 ])
                    [ twoBlocks ]
                    "1,000 ticks lost of ours is two parts"

                Expect.equal
                    (castWith [ "W1N2", reservedRoom false 4000 ])
                    [ nineBlocks ]
                    "the same 4,000 in a rival's name is a deficit of the whole 5,000: nine parts"

                Expect.equal
                    (castWith [])
                    [ nineBlocks ]
                    "and no reservation at all is that same whole deficit"
            }

            test "every cast this tick carries the largest outstanding demand" {
                // The row casts bodies and the Matcher pairs them to
                // controllers, by travel cost alone and knowing nothing
                // about a deficit (#130). So a demand read room by room
                // would land the *nearer* room's small body on the room
                // that has slipped, and a controller held by one CLAIM
                // against the engine's one tick of decay is frozen where it
                // stands for that body's whole 600-tick life. The row
                // over-buys instead — at most one block per cast, and only
                // while two demands differ.
                let colony =
                    reserverColony
                        [ northOutpost false; westOutpost false ]
                        (surplusFleet 2)
                        [ "W1N2", reservedRoom true 5000; "W2N2", reservedRoom true 2000 ]

                let fiveBlocks = List.replicate 5 Claim @ List.replicate 5 Move

                Expect.equal
                    (reserverCasts
                        (decide
                            { colony with
                                RoomEnergy = bank 8000 8000
                            }
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    [ fiveBlocks; fiveBlocks ]
                    "the room standing at its cap is cast the five blocks the slipped room asked for"
            }

            test "the reserver row is an addend of the target, amortized over a CLAIM life" {
                // The row's two effects on the Workforce target (ADR 0042),
                // both read off one boundary: it adds a place of its own —
                // a CLAIM body is a creep, and a fleet counting it as a
                // generalist would hire an upgrade mouth fewer — and its
                // replacement cost is deducted from the income base like
                // the Anchor and hauler rows'. Unlike theirs it is spread
                // over a **CLAIM body's own 600 ticks** rather than the
                // 1,500 the rest of the sum is written in: ADR 0042 prices
                // this row at 2.17 energy a tick, and over 1,500 it would
                // read as 0.87.
                //
                // The bank is 8,000 so the deficit's whole nine blocks are
                // affordable and the difference is a worker place wide.
                // W1N2 is seen and held by nobody: its rock is worth five,
                // and its reservation is on the floor, so the row asks for
                // its largest body against its smallest income.
                //
                // Income 10 + 10 + 5 = 25 a tick over 1,500 = 37,500.
                // Amortization: 3 Anchors × 700 = 2,100, 2 haulers × 2,400
                // = 4,800, and one 9-block reserver at 5,850 spread over
                // 600 and re-scaled onto 1,500 = 14,625 — 21,525 in all.
                // The surplus 15,975 over a 16-Work body's drain × 1,500 =
                // 24,000 rounds up to one worker (ADR 0037). Charged over
                // 1,500 instead, the same row would leave 24,750 and hire
                // two.
                let fleetOf workers =
                    [ for i in 1..3 -> anchor $"a{i}" 0 50 ]
                    @ [ for i in 1..2 -> hauler $"h{i}" 0 100 ]
                    @ [ reserver "r1" ]
                    @ [ for i in 1..workers -> worker $"w{i}" 0 50 ]

                let atFleet workers =
                    let colony =
                        reserverColony
                            [ northOutpost true ]
                            (fleetOf workers)
                            [ "W1N2", neutralRoom ]

                    decide
                        { colony with
                            RoomEnergy = bank 40000 8000
                        }
                        Map.empty
                        Set.empty
                        None

                Expect.equal
                    (atFleet 1).Memo.HaulerQuota
                    2
                    "the premise: the two home containers hire one body each at this bank"

                Expect.isEmpty
                    (spawnIntents (atFleet 1).Intents)
                    "3 Anchors + 2 haulers + 1 reserver + 1 worker is the whole target: seven"

                match spawnIntents (atFleet 0).Intents with
                | [ (_, _, creepName) ] ->
                    Expect.stringStarts
                        creepName
                        "worker-"
                        "one body short, the generalist row fills the remainder"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "a declared outpost this colony owns hires no reserver at all" {
                // The tick a declared outpost is claimed, its controller
                // stops being reservable: the engine answers
                // ERR_INVALID_TARGET to `reserveController` on a room
                // anybody owns (#181). An owned room carries no reservation
                // for the same reason, so the deficit read off one is the
                // whole 5,000 and this row would cast a 650-energy body at
                // it every 600 ticks forever, each one walking over to fail
                // for its whole life.
                //
                // Pairwise on the one fact — same room, same rock, same
                // absent reservation, same fleet, same bank — because
                // ownership is the only input that moves. Read at a bank
                // that affords the whole nine-block deficit, so the
                // neutral half is a body the truncation could not have
                // produced by accident.
                let castsUnder control =
                    let colony =
                        reserverColony [ northOutpost false ] (surplusFleet 2) [ "W1N2", control ]

                    { colony with
                        RoomEnergy = bank 8000 8000
                    }
                    |> fun colony -> decide colony Map.empty Set.empty None
                    |> fun result -> reserverCasts result.Intents

                Expect.equal
                    (castsUnder neutralRoom)
                    [ List.replicate 9 Claim @ List.replicate 9 Move ]
                    "a room nobody holds is the whole 5,000 of deficit: nine parts"

                Expect.isEmpty
                    (castsUnder ownedRoom)
                    "the same room, owned by this colony, is no longer a room to reserve"
            }
        ]

/// One outpost as ADR 0041 declares it, read off the very tuple the
/// reserver row's fixtures are written in — a room name, its rock and its
/// controller, ids and tiles — spelling the same ids and the same tiles
/// `withOutpostRoom` furnishes that room with. The declaration is what a
/// stand-down subtracts and the furniture is what vision pays for, and
/// the two have to name one room or the gate below would be subtracting
/// something nothing else placed. Taking the room and the rock from
/// `northOutpost`/`westOutpost` rather than retyping them is what keeps
/// that true: one source for the colony both families describe, so a rock
/// moved for one is moved for the other. The `posted` flag rides along
/// unread — `gatedColony` posts every room it works, because an unposted
/// outpost contributes nothing to three of the four rows to begin with
/// and would make the gate's subtraction unreadable.
let private gatedOutpost (room, rock: Pos, _posted) : Outpost =
    {
        RoomName = room
        Sources = [ $"src-{room}", rock ]
        Controller = $"ctrl-{room}", { rock with Y = rock.Y + 2 }
    }

/// The colony the shell assembles for a given declaration under a given
/// shut set (ADR 0043), built the way `Snapshot.build` builds one: the
/// declarations less what the gate withholds (`Outpost.worked`), and then
/// every fact of a worked room — its layer, its rock in the pool, its
/// standing container, who holds it — and *none* of a withheld one.
///
/// That second half is the shell's own rule and not this fixture's
/// invention: the scan set is taken from the declarations that survive the
/// gate, the furniture is laid only into rooms the scan set carries
/// (`Outpost.place`), the rocks are pooled only for those rooms
/// (`Outpost.pooledSources`) and every entry vision pays for is collected
/// over `seen`, which is the scan set filtered by vision. A room the
/// colony does not scan is one it never looks into, so it contributes
/// nothing at all — which is exactly what ADR 0004 has always meant by a
/// room that is not there.
///
/// The reservation stands at its 5,000 cap on every worked room, so the
/// reserver row's deficit is zero and its casts are at the floor: the
/// number of casts is then a count of rooms and never a reading of a
/// deficit.
///
/// Assembled by `reserverColony` and not beside it: the rooms, the bank
/// and the control entries are that fixture's already, so the gate reads
/// over the same colony the reserver row is pinned on rather than a second
/// one free to drift from it.
let private gatedColony declarations shut creeps =
    let worked = Outpost.worked shut declarations

    reserverColony
        (worked
         |> List.map (fun (outpost: Outpost) ->
             outpost.RoomName, outpost.Sources |> List.head |> snd, true))
        creeps
        (worked |> List.map (fun outpost -> outpost.RoomName, reservedRoom true 5000))

/// The two outposts the gate is read over, diagonal to each other as
/// W12S27 and W13S28 are (ADR 0042): one gate each, and one of them is not
/// enough to tell "this room is withheld" from "outposts are withheld".
/// The same two the reserver row hires for, declared instead of furnished.
let private northGated = gatedOutpost (northOutpost true)
let private westGated = gatedOutpost (westOutpost true)

/// A creep standing in a room, placed the way the shell places one: in the
/// layer of the room it stands in (ADR 0041), and nowhere at all when that
/// room is not projected. A creep does give the engine vision of its own
/// room, but the shell reads the rooms it scans and no others, so a
/// stood-down room's tiles go unread and the creep on them is unplaced —
/// unpriceable geometry, which is ADR 0004's own answer and not a state of
/// its own.
let private standingIn room (name, pos) (colony: Snapshot) =
    match Map.tryFind room colony.Spatial.Rooms with
    | None -> colony
    | Some layer ->
        { colony with
            Spatial =
                colony.Spatial
                |> withNeighbour
                    room
                    { layer with
                        CreepPositions = Map.add name pos layer.CreepPositions
                    }
        }

/// Every Task in the pool that names a room's furniture — the rock, the
/// controller and the container `withOutpostRoom` gives it, whose ids all
/// carry the room's name.
let private tasksNaming room colony =
    planTasks colony noThreats
    |> List.map taskId
    |> List.filter (fun id -> (id: string).Contains(room: string))

[<Tests>]
let standDownGateTests =
    testList
        "a stood-down outpost in the pool"
        [
            test "a stood-down outpost pools no Task, counts in no quota and is cast for by nobody" {
                // ADR 0043's whole claim, at the top seam: a room the gate
                // withholds decides exactly what a room nobody declared
                // decides. Nothing downstream was taught about stand-downs
                // — the projection, the Task pool, the four quota rows and
                // the Atlas each see a room that is not there, which is the
                // semantics ADR 0004 paid for long ago.
                //
                // The fleet stands over every row's quota but the
                // reserver's, so a `SpawnCreep` here is a reserver or it is
                // a defect, and the reserver row is the one row a
                // *declaration alone* hires for (#131): one body per
                // declared outpost, container or no container. That makes
                // it the row that can tell "the room left the projection"
                // from "the room left the economy".
                let fleet = surplusFleet 4
                let both = gatedColony [ northGated; westGated ] Set.empty fleet
                let shut = gatedColony [ northGated; westGated ] (Set.singleton "W1N2") fleet
                let never = gatedColony [ westGated ] Set.empty fleet

                Expect.isNonEmpty
                    (tasksNaming "W1N2" both)
                    "the premise: worked, the room's furniture is in the pool"

                Expect.equal
                    (reserverCasts (decide both Map.empty Set.empty None).Intents)
                    [ oneBlock; oneBlock ]
                    "and worked, it is one of two outposts each hiring its own reserver"

                Expect.isEmpty
                    (tasksNaming "W1N2" shut)
                    "shut, no Task in the pool names the room — its rock, its controller and its container are gone with it"

                Expect.equal
                    (reserverCasts (decide shut Map.empty Set.empty None).Intents)
                    [ oneBlock ]
                    "and the one cast left is the other outpost's: nothing is built for a room nothing can enter"

                // Everything else besides, and this one holds by
                // construction rather than by observation: `gatedColony`
                // subtracts the shut set before it assembles anything, so
                // the two Snapshots below are the same value and the
                // equality can only fail if `Outpost.worked` filters by
                // something other than the room's name. That is worth one
                // line and is not the criterion's quota half — a row still
                // counting the shut room could not show up here, because
                // there is no room here for it to count.
                Expect.equal
                    (outcomeOf shut)
                    (outcomeOf never)
                    "a room the gate withholds is subtracted by name, so it assembles the colony a room nobody declared assembles"
            }

            test "the quota rows stop counting the room the gate withholds" {
                // Criterion 1's other half, and the one the equality above
                // cannot reach: it is read on two colonies that really do
                // differ — both declare both rooms, and only the shut set
                // moves — so a row still folding the withheld room's
                // furniture hires a body the colony it is actually working
                // does not want.
                //
                // One row at a time, each against a fleet standing exactly
                // at the shut colony's own quota for it while every other
                // row is over its own, which is the pairwise reading the
                // matcher's cheapest-rival rule asks for everywhere else.
                let castRows anchors haulers workers shut =
                    let fleet =
                        [ for i in 1..anchors -> anchor $"a{i}" 0 50 ]
                        @ [ for i in 1..haulers -> hauler $"h{i}" 0 100 ]
                        @ [ for i in 1..workers -> worker $"w{i}" 0 50 ]

                    gatedColony [ northGated; westGated ] shut fleet
                    |> castNames
                    |> List.map (fun (name: string) -> name.Split('-') |> Array.head)

                let gated anchors haulers workers =
                    castRows anchors haulers workers Set.empty,
                    castRows anchors haulers workers (Set.singleton "W1N2")

                // The Anchor row counts Posts and the withheld room's
                // standing container was one: at three Anchors the colony
                // working both rooms is a body short and the one working
                // the west room alone is already at its target.
                Expect.equal
                    (gated 3 3 40)
                    ([ "reserver"; "reserver"; "anchor" ], [ "reserver" ])
                    "the fourth Post goes with the room, and the Anchor it would have hired goes with it"

                // The workforce target counts each posted source's output
                // and the withheld room's rock was one: at two workers the
                // colony working both rooms hires a third.
                Expect.equal
                    (gated 4 3 2)
                    ([ "reserver"; "reserver"; "worker" ], [ "reserver" ])
                    "the withheld rock's ten a tick leaves the income the worker row is sized off"

                // The fourth row is deliberately not pinned by a cast. At
                // ADR 0042's 1,800 capacity one hauler covers a container's
                // round trip, and this colony's two home containers set the
                // row at two either way — the outposts move it by nothing
                // there is a body's granularity to see. What the row reads
                // is the projection's containers, and the withheld room's
                // is gone with the room, which the Task pool above already
                // shows: no Withdraw names it.
                Expect.equal
                    (gated 4 1 40)
                    ([ "reserver"; "reserver"; "hauler" ], [ "reserver"; "hauler" ])
                    "the hauler row wants its two home bodies on either side of the gate"
            }

            test "two outposts are two gates" {
                // ADR 0043's independent gates: W12S27 standing down does
                // not cost W13S28 its reserver. Pairwise, one room shut at
                // a time, because a gate that withheld "the outposts"
                // rather than a room would pass a test that shut only one.
                let shutting room =
                    let colony =
                        gatedColony [ northGated; westGated ] (Set.singleton room) (surplusFleet 4)

                    tasksNaming "W1N2" colony, tasksNaming "W2N2" colony

                let northShut, westWithNorthShut = shutting "W1N2"
                let northWithWestShut, westShut = shutting "W2N2"

                Expect.isEmpty northShut "the north room is withheld"

                Expect.isNonEmpty westWithNorthShut "while the west one is worked exactly as before"

                Expect.isEmpty westShut "and the other way round"
                Expect.isNonEmpty northWithWestShut "with the north one untouched"
            }

            test "the tick the clock runs out, the outpost is back in the pool" {
                // Re-entry is the clock running out and nothing else (ADR
                // 0043), so the gate is read straight off the log: the two
                // colonies below differ only in the tick `Observe.standDown`
                // was asked at, one either side of the recorded expiry.
                let log =
                    Observe.RaidState.empty
                    |> Observe.foldRaids
                        Observe.capEpisodes
                        Observe.quietGap
                        { incomeColony with
                            Time = 100
                            InvaderCores =
                                [
                                    {
                                        RoomName = "W1N2"
                                        CollapseTick = Some 900
                                    }
                                ]
                        }

                let fleet = surplusFleet 4

                let atTick t =
                    gatedColony [ northGated; westGated ] (Observe.standDown t log) fleet

                Expect.isEmpty
                    (tasksNaming "W1N2" (atTick 899))
                    "one tick short of the expiry the room is still withheld"

                Expect.isNonEmpty
                    (tasksNaming "W1N2" (atTick 900))
                    "on the expiry itself its rock, its controller and its container are in the pool again"

                Expect.equal
                    (reserverCasts (decide (atTick 900) Map.empty Set.empty None).Intents)
                    [ oneBlock; oneBlock ]
                    "and the row hires for it again, the tick it may be entered"
            }

            test "a room another player holds is withheld with no clock at all" {
                // ADR 0043's other trigger, end to end: the fold remembers
                // the room the tick it is seen taken (`RaidState.Foreign`),
                // and the gate withholds it for ever after — there is no
                // expiry, because a room somebody else works has not been
                // made dangerous, it has stopped being ours.
                //
                // Pairwise against the same room seen held by *us*, which
                // is the ordinary steady state of every outpost: one field
                // of one control entry moves.
                let logWith holder =
                    Observe.RaidState.empty
                    |> Observe.foldRaids
                        Observe.capEpisodes
                        Observe.quietGap
                        { incomeColony with
                            Time = 100
                            RoomControl = Map.ofList [ "W1N2", reservedRoom holder 4000 ]
                        }

                let fleet = surplusFleet 4

                let poolAt holder t =
                    gatedColony
                        [ northGated; westGated ]
                        (Observe.standDown t (logWith holder))
                        fleet
                    |> tasksNaming "W1N2"

                Expect.isNonEmpty
                    (poolAt true 101)
                    "held by us the room is worked, which is what every outpost's steady state looks like"

                Expect.isEmpty
                    (poolAt false 101)
                    "held by another player it is withheld the tick after it was seen"

                Expect.isEmpty
                    (poolAt false 1_000_000)
                    "and a million ticks later it is still withheld: this withdrawal carries no clock"
            }

            test "the creep standing in a stood-down outpost is released, on the existing path" {
                // ADR 0043's re-entry rule has a mirror: nothing new
                // withdraws the creeps either. The room's Tasks stop
                // existing, and a creep holding one is released by the
                // release the Matcher has always spoken for an assignment
                // whose Task is gone — no retreat act, no new Verdict, no
                // second rule about where a creep may stand.
                // One creep and no fleet behind it: the release is the
                // subject, and a colony standing at its quotas would have
                // every home Task at capacity, so the creep would read as
                // unassigned for a reason that has nothing to do with the
                // gate.
                let colonyWith shut =
                    gatedColony [ northGated; westGated ] shut [ worker "w-out" 0 50 ]
                    |> standingIn "W1N2" ("w-out", { X = 39; Y = 41 })

                let held = taskId (Harvest "src-W1N2")
                let assignments = Map.ofList [ "w-out", held ]

                let verdictsWith shut =
                    (decide (colonyWith shut) assignments Set.empty None).Verdicts

                Expect.contains
                    (verdictsWith Set.empty)
                    (Verdict.Kept("w-out", held))
                    "the premise: worked, the creep keeps the outpost Harvest it holds"

                Expect.contains
                    (verdictsWith (Set.singleton "W1N2"))
                    (Verdict.Released("w-out", held, ReleaseReason.TaskGone))
                    "shut, the Task is gone and the creep is released by the reason that has always meant that"

                let rematched =
                    verdictsWith (Set.singleton "W1N2")
                    |> List.tryPick (function
                        | Verdict.Matched("w-out", task, _) -> Some task
                        | _ -> None)

                Expect.isSome
                    rematched
                    "and it is matched again on the same tick, not left holding nothing"

                Expect.isFalse
                    ((Option.defaultValue "" rematched).Contains "W1N2")
                    "to a Task of a room the colony is still working"

            // What is *not* pinned here is the walk back, and it is not
            // pinned because it does not happen. A withheld room is not
            // projected (ADR 0043), so it places no creep, so the creep
            // standing in it has no tile: the rematch above is priced on
            // ADR 0004's escape — an unplaced creep prices every Task at 0
            // — rather than on a crossing, `Decide.resolve` builds moves
            // only over the creeps the Atlas places, and nothing aims this
            // one home. The release path is this ticket's claim and it
            // holds; the journey home is a fact about an unplaced creep
            // that ADR 0043's own gate placement makes unreachable, and it
            // is carried out of this ticket as a finding of its own rather
            // than pinned here as if it were the behaviour.
            }
        ]
