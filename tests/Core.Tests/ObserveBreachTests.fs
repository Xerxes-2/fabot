/// The breach log: the live invariants this colony checks every tick, what
/// makes each of them fire, and the age column that tells a delivery
/// landing from a store nobody is answering.
module Fabot.Core.Tests.ObserveBreachTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Observe

/// The room the errand fixtures declare, and the Reactor standing in it: the
/// `Decide` suites' own spelling, read rather than re-spelled, so this
/// fixture cannot drift from the rule it stands in for.
let private errandRoom = Decide.Fixtures.reactorErrand.RoomName
let private reactor = Decide.Fixtures.reactorId

/// A room this colony **owns**: the other half of the pile check's reach, and
/// the half `Facts.inARoomWeOwn` answers off `RoomControl`.
let private owning room (colony: ColonyView) =
    { colony with
        RoomControl =
            Map.add
                room
                {
                    Owner = Ownership.Ours
                    Reservation = None
                    SafeMode = false
                    SafeModeCooldownUntil = 0
                    Sign = None
                }
                colony.RoomControl
    }

/// A dropped Thorium pile standing in a room, as the projection carries one:
/// its kind on the census, its amount in `SpatialInfo.Thorium`, and its tile
/// in that room's layer. All three, because the pile check joins the census
/// to the room and reads the amount off the map beside them.
let private withPile room id amount (colony: ColonyView) =
    let layer = SpatialInfo.layerOf colony.Spatial room

    { colony with
        Spatial =
            { colony.Spatial with
                TargetKinds = Map.add id (Dropped Thorium) colony.Spatial.TargetKinds
                Thorium = Map.add id amount colony.Spatial.Thorium
            }
            |> withNeighbour
                room
                { layer with
                    TargetPositions = Map.add id { X = 26; Y = 43 } layer.TargetPositions
                }
    }

/// A tombstone standing in a room, holding the given ore — the courier that
/// died loaded. The kind names no resource, so `amount` in the Thorium map
/// is the only thing that makes this object ore at all, which is why the
/// control below is a tombstone with no entry in it.
let private withTombstone room id amount (colony: ColonyView) =
    let layer = SpatialInfo.layerOf colony.Spatial room

    { colony with
        Spatial =
            { colony.Spatial with
                TargetKinds = Map.add id Tombstone colony.Spatial.TargetKinds
                Thorium =
                    match amount with
                    | Some units -> Map.add id units colony.Spatial.Thorium
                    | None -> colony.Spatial.Thorium
            }
            |> withNeighbour
                room
                { layer with
                    TargetPositions = Map.add id { X = 27; Y = 43 } layer.TargetPositions
                }
    }

/// The colony with the errand declared and the Reactor visible and ours,
/// holding `held` of its 1,000 (the store on the Reactor's **row** and never
/// in `SpatialInfo.Thorium`).
let private erranding held (colony: ColonyView) =
    { colony with
        // The home this delivery runs from has to be a room the errand is
        // reachable from: `quiet` lives at `raidRoom` (W12S28) while
        // `Decide.Fixtures`' Reactor stands in W1N2, 42 crossings apart, and the
        // running-dry alarm times itself against the walk. Three hops is the live
        // pairing, W15S28 to W15S25.
        Spatial =
            { colony.Spatial with
                RoomName = Some "W1N5"
            }
    }
    |> Decide.Fixtures.withReactorErrand
    |> Decide.Fixtures.withReactorOwner (Some Ownership.Ours)
    |> Decide.Fixtures.withReactorStore held

/// One of ours standing in the errand room with a load of ore aboard — the
/// courier at the end of the paired delivery.
let private courierAt name carried (colony: ColonyView) =
    colony
    |> Decide.Fixtures.standingInErrand [ { ours name with Thorium = carried }, { X = 25; Y = 43 } ]

/// The same body standing **at home**, three crossings from the Reactor.
///
/// Asymmetric on purpose: it conses onto `Creeps` and *replaces* the home
/// layer's `CreepPositions`, so a second body placed at home would be
/// evicted rather than joined.
let private courierAtHome name carried (colony: ColonyView) =
    let laden = { ours name with Thorium = carried }

    { colony with
        Creeps = laden :: colony.Creeps
        Spatial = colony.Spatial |> withCreepsAt [ name, { X = 10; Y = 10 } ]
    }

/// The courier's real body on a named creep: the alarm matches the **shape**
/// `Bodies.courierPattern` casts — twenty Carry and ten Move — and the
/// three-part `ours` body beside it is the control that must not be.
let private withCourierBody name (colony: ColonyView) =
    { colony with
        Creeps =
            colony.Creeps
            |> List.map (fun creep ->
                if creep.Name = name then
                    { creep with
                        Body = Map.ofList [ Carry, 20; Move, 10 ]
                    }
                else
                    creep)
    }

/// One tick of a colony's breach log folded off its view alone, as the
/// shell folds a faulted colony's.
let private foldView cap t (colony: ColonyView) (prior: BreachState) =
    foldBreaches cap t (colonyBreaches ColonyBreachFacts.none colony prior) prior

/// The breaches this view yields on one tick, as (kind, room, subject, amount)
/// rows — the whole of what a row says, so a case that fires the right kind on
/// the wrong object cannot pass.
let private breachesOn t (colony: ColonyView) =
    foldView capBreaches t { colony with Time = t } BreachState.empty
    |> breachRows
    |> List.map (fun row -> row.Breach.Kind, row.Breach.Room, row.Breach.Subject, row.Breach.Amount)

/// The kinds alone, for the cases that are about which check fired.
let private kindsOn t (colony: ColonyView) =
    breachesOn t colony |> List.map (fun (kind, _, _, _) -> kind)

[<Tests>]
let breachKindTests =
    testList
        "breach log: what fires"
        [
            test "a Thorium pile in a room we own is a breach, and one in a stranger's room is not" {
                Expect.equal
                    (breachesOn 100 (quiet |> owning raidRoom |> withPile raidRoom "pile-1" 915))
                    [ BreachKind.OreOnTheFloor, raidRoom, "pile-1", 915 ]
                    "the amount is the T on the floor, which is what makes the row actionable"

                Expect.equal
                    (breachesOn 100 (quiet |> withPile "W9S9" "pile-1" 915))
                    []
                    "a room we neither own nor declared nor cross is somebody else's floor"

                // The third clause of that reach: a room a chain of ours merely crosses is
                // where the ore most often lands, and `Crossed` is the projection's own
                // subtraction rather than a second guess at it.
                let onTheWay =
                    let seen = quiet |> withPile "W9S9" "pile-1" 419

                    { seen with
                        Crossed = Set.singleton "W9S9"
                    }

                Expect.equal
                    (breachesOn 100 onTheWay)
                    [ BreachKind.OreOnTheFloor, "W9S9", "pile-1", 419 ]
                    "the same floor, once the projection says we cross it, is a leak this colony is answerable for"
            }

            test "a pile on the declared Reactor's floor is a breach: that room has no owner at all" {
                // Why this reads `Facts.ourThoriumPiles` rather than "a room we own": the
                // Reactor's room has no controller, and 915 T sat on it for hours while a
                // body of ours stood two tiles away. The projection half is `ViewTests`';
                // wiring this channel found `erranding` had emptied the kind census.
                Expect.equal
                    (breachesOn 100 (quiet |> erranding 500 |> withPile errandRoom "pile-r" 915))
                    [ BreachKind.OreOnTheFloor, errandRoom, "pile-r", 915 ]
                    "the errand room's floor is the one floor of ours that is in nobody's room"
            }

            test "ore in a tombstone is the same breach, and a tombstone holding none is no breach" {
                // The channel swept `Dropped Thorium` alone, so the ore a courier dies
                // with — 175 T at W15S25's (43,6) — was invisible until the tombstone
                // decayed. Same incident a few hundred ticks earlier, same action, so
                // `OreOnTheFloor` and not a kind of its own. Pinned in both rooms the
                // reach names.
                Expect.equal
                    (breachesOn
                        100
                        (quiet |> owning raidRoom |> withTombstone raidRoom "tomb-1" (Some 175)))
                    [ BreachKind.OreOnTheFloor, raidRoom, "tomb-1", 175 ]
                    "the amount is the T in the store, which is what makes the row actionable"

                Expect.equal
                    (breachesOn
                        100
                        (quiet |> erranding 500 |> withTombstone errandRoom "tomb-r" (Some 175)))
                    [ BreachKind.OreOnTheFloor, errandRoom, "tomb-r", 175 ]
                    "and the declared Reactor's room, which is where a courier dies"

                // The pairwise control: the same object in the same room with no entry in
                // the Thorium map. A tombstone is not a breach; ore in one is.
                Expect.isEmpty
                    (breachesOn
                        100
                        (quiet |> owning raidRoom |> withTombstone raidRoom "tomb-1" None))
                    "a tombstone holding no ore is a decaying object and not a loss"
            }

            test "ore a courier cannot place is a breach, and a load that fits is not" {
                let full = quiet |> erranding Engine.reactorCapacity |> courierAt "courier" 500

                Expect.equal
                    (breachesOn 100 full
                     |> List.filter (fun (kind, _, _, _) -> kind = BreachKind.OreUnplaceable))
                    [ BreachKind.OreUnplaceable, errandRoom, "courier", 500 ]
                    "a full Reactor has no room for any of the 500 aboard"

                Expect.equal
                    (breachesOn 100 (quiet |> erranding 700 |> courierAt "courier" 500))
                    [ BreachKind.OreUnplaceable, errandRoom, "courier", 200 ]
                    "300 of the load fits; the breach is the 200 that has nowhere to go"

                Expect.equal
                    (breachesOn 100 (quiet |> erranding 500 |> courierAt "courier" 500))
                    []
                    "a load the store has exactly the room for is the delivery working"
            }

            test "a courier's load is read off the Reactor's row, and no Thorium map beside it" {
                // The regression shape of #354's first incident: `SpatialInfo.Thorium`
                // carries every store a Task can name and deliberately not the Reactor's,
                // and the draw gate read it there anyway — 999 read as 0. The unit test
                // agreed, because the fixture wrote the store where the gate looked.
                let ready = quiet |> erranding 0 |> courierAt "courier" 500

                let inTheWrongMap =
                    { ready with
                        Spatial =
                            { ready.Spatial with
                                Thorium =
                                    Map.add reactor Engine.reactorCapacity ready.Spatial.Thorium
                            }
                    }

                Expect.isFalse
                    (kindsOn 100 inTheWrongMap |> List.contains BreachKind.OreUnplaceable)
                    "a full store written where the projection never writes one raises no alarm"

                Expect.equal
                    (breachesOn
                        100
                        (quiet |> erranding Engine.reactorCapacity |> courierAt "courier" 500)
                     |> List.filter (fun (kind, _, _, _) -> kind = BreachKind.OreUnplaceable))
                    [ BreachKind.OreUnplaceable, errandRoom, "courier", 500 ]
                    "the same number on the Reactor's own row is the breach"
            }

            test "a body of ours elsewhere holding ore is nobody's breach" {
                // The cheapest false positive there is, and the reason the
                // check is narrowed to the bodies standing in the errand room:
                // a [[miner]] at home holding ore bound for its container is
                // not ore with nowhere to go, however full the Reactor is.
                let atHome =
                    { (quiet |> erranding Engine.reactorCapacity) with
                        Creeps = [ { ours "miner" with Thorium = 500 } ]
                    }

                Expect.isFalse
                    (kindsOn 100 atHome |> List.contains BreachKind.OreUnplaceable)
                    "the load of a body that is not at the Reactor has somewhere else to be"
            }

            test "a Reactor of ours standing dry is a breach; one with ore in it is not" {
                Expect.equal
                    (breachesOn 100 (quiet |> erranding 0))
                    [ BreachKind.ReactorStarved, errandRoom, reactor, 0 ]
                    "an empty store is the streak reset, and the row's whole content is the fact"

                Expect.equal
                    (breachesOn 100 (quiet |> erranding 1))
                    [ BreachKind.ReactorRunningDry, errandRoom, reactor, 1 ]
                    "one tonne left and nobody walking is the row that arrives in time, not the starved one"
            }

            test
                "a Reactor whose store is thinner than the courier's lead time is a breach before it starves" {
                // The whole of the design is the threshold: 90 ticks to cast the fixed
                // body plus 150 over three crossings is 240, so 240 fires and 241 does
                // not — the row appears on the last tick an answer still lands.
                Expect.equal
                    (breachesOn 100 (quiet |> erranding 240))
                    [ BreachKind.ReactorRunningDry, errandRoom, reactor, 240 ]
                    "the amount is the ticks of burn left, which counts down while nobody answers"

                Expect.equal
                    (breachesOn 100 (quiet |> erranding 241))
                    []
                    "one tick of margin over the lead time is a Reactor still reachable, and an alarm here would be answered by a courier that stands at the flag burning its 1,500-tick life"
            }

            test
                "a laden courier silences the running-dry row; a courier-shaped body alone does not" {
                // "The store is low **and no ore is moving**", not "and no courier is
                // alive": live (#367) a courier hauled energy for 465 ticks while the
                // store fell 500 -> 0, with this channel reporting `no breaches`. So ore
                // **aboard** (`CreepInfo.Thorium`) answers, matched on shape, never name.
                let walking =
                    quiet |> erranding 10 |> courierAt "courier" 500 |> withCourierBody "courier"

                Expect.equal (breachesOn 100 walking) [] "a load is genuinely in the air"

                // The empty-handed courier is the live shape, and it must cry: either it
                // is walking out to fetch a load, a few ticks of false alarm, or it is
                // doing something else entirely, which is the 465-tick case.
                let empty =
                    quiet |> erranding 10 |> courierAt "courier" 0 |> withCourierBody "courier"

                Expect.equal
                    (breachesOn 100 empty)
                    [ BreachKind.ReactorRunningDry, errandRoom, reactor, 10 ]
                    "a courier carrying nothing is not an answer, whatever its body says"

                Expect.equal
                    (breachesOn 100 (quiet |> erranding 10 |> courierAt "hauler" 500))
                    [ BreachKind.ReactorRunningDry, errandRoom, reactor, 10 ]
                    "a three-part body standing out there is not a courier and carries no load worth a delivery"
            }

            // A load in the air answers the alarm only while it can still land in
            // time: a body that drew at home — three crossings and 150 ticks away —
            // once silenced a store with ten ticks left, and the store reached zero
            // with the load still two rooms out (#377, twice at t559,4xx).
            test "a load too far to land in time is not an answer" {
                let far store =
                    quiet
                    |> erranding store
                    |> courierAtHome "courier" 500
                    |> withCourierBody "courier"

                Expect.equal
                    (breachesOn 100 (far 10))
                    [ BreachKind.ReactorRunningDry, errandRoom, reactor, 10 ]
                    "ten ticks of store against three crossings of walk: the load cannot land and the row stands"

                Expect.equal
                    (breachesOn 100 (far 200))
                    []
                    "two hundred ticks covers the same walk, so the same load is a genuine answer"

                // The near case is the one that must keep working: a body on
                // the Reactor's own ring is no crossings away, so it answers
                // whatever the store holds.
                Expect.equal
                    (breachesOn
                        100
                        (quiet
                         |> erranding 1
                         |> courierAt "courier" 500
                         |> withCourierBody "courier"))
                    []
                    "and a load already standing at the Reactor answers a store with one tick left"
            }

            test "a declared Reactor whose row is not ours is a breach, and is not also starved" {
                let rivals =
                    quiet
                    |> Decide.Fixtures.withReactorErrand
                    |> Decide.Fixtures.withReactorOwner (Some Ownership.Rival)

                Expect.equal
                    (breachesOn 100 rivals)
                    [ BreachKind.ReactorLost, errandRoom, reactor, 0 ]
                    "everything delivered there scores for whoever holds the flag"

                Expect.isFalse
                    (kindsOn 100 rivals |> List.contains BreachKind.ReactorStarved)
                    "a Reactor that is not ours is not a Reactor of ours standing dry"
            }

            test "a Reactor we cannot see yields no row of any kind" {
                // No vision, no row, no reading — and therefore no breach. A declared
                // Reactor with nothing of ours standing out there is the ordinary state
                // between two re-claimers.
                let blind =
                    { (quiet |> erranding 0) with
                        Reactors = []
                    }

                Expect.equal
                    (breachesOn 100 blind)
                    []
                    "neither starved nor lost: the colony has read nothing to be either"
            }

            test "a colony with nothing wrong records nothing" {
                Expect.equal
                    (breachesOn 100 (quiet |> erranding 500))
                    []
                    "the healthy tick is empty"
            }
        ]

[<Tests>]
let breachAgeTests =
    testList
        "breach log: age and the cap"
        [
            test "a breach still standing fifty ticks later is fifty ticks old" {
                // The whole reason this channel folds rather than snapshotting:
                // live, a pile a courier is three ticks from picking up and a
                // pile that is bleeding read exactly alike.
                let colony = quiet |> owning raidRoom |> withPile raidRoom "pile-1" 915

                let state =
                    BreachState.empty
                    |> foldView capBreaches 100 { colony with Time = 100 }
                    |> foldView capBreaches 150 { colony with Time = 150 }

                Expect.equal
                    (standing 150 state |> List.map (fun (breach, age) -> breach.Subject, age))
                    [ "pile-1", 50 ]
                    "the row keeps the tick it opened on and ages against the clock"

                Expect.equal
                    (breachRows state |> List.map (fun row -> row.FirstSeen, row.LastSeen))
                    [ 100, 150 ]
                    "and it dates itself, so the leaf can be read without a clock"
            }

            test "a breach that clears drops out rather than lingering" {
                // This channel answers "what is broken now" and nothing else; the episodic
                // reading of the same ground is the Raid log's. A row that lingered would
                // need a reader who knew which rows were current.
                let broken = quiet |> owning raidRoom |> withPile raidRoom "pile-1" 915

                let state =
                    BreachState.empty
                    |> foldView capBreaches 100 { broken with Time = 100 }
                    |> foldView
                        capBreaches
                        101
                        { (quiet |> owning raidRoom) with
                            Time = 101
                        }

                Expect.equal (breachRows state) [] "the pile was picked up, and the log says so"
            }

            test "a breach that comes back opens a fresh age" {
                // The stated cost of dropping out: a violation that flickers off for one
                // tick loses its age. The right trade for checks that are conditions
                // re-read every tick rather than events.
                let broken = quiet |> owning raidRoom |> withPile raidRoom "pile-1" 915
                let clear = quiet |> owning raidRoom

                let state =
                    BreachState.empty
                    |> foldView capBreaches 100 { broken with Time = 100 }
                    |> foldView capBreaches 101 { clear with Time = 101 }
                    |> foldView capBreaches 102 { broken with Time = 102 }

                Expect.equal
                    (standing 102 state |> List.map snd)
                    [ 0 ]
                    "the returning pile is a new row, not the old one resumed"
            }

            test "the latest reading wins: a growing pile reports what it holds now" {
                let state =
                    BreachState.empty
                    |> foldView
                        capBreaches
                        100
                        { (quiet |> owning raidRoom |> withPile raidRoom "pile-1" 90) with
                            Time = 100
                        }
                    |> foldView
                        capBreaches
                        101
                        { (quiet |> owning raidRoom |> withPile raidRoom "pile-1" 915) with
                            Time = 101
                        }

                Expect.equal
                    (breachRows state |> List.map (fun row -> row.Breach.Amount, row.FirstSeen))
                    [ 915, 100 ]
                    "the amount is this tick's and the age is the first tick's"
            }

            test "the cap keeps the rows that have stood longest" {
                // Every surviving row was last seen on this very tick — a row
                // that stops appearing drops out — so `LastSeen` is tied across
                // the whole log and `FirstSeen` is what decides. Evicting the
                // oldest would silence exactly the breach worth reading.
                let piles ids amount time =
                    (quiet |> owning raidRoom, ids)
                    ||> List.fold (fun colony id -> withPile raidRoom id amount colony)
                    |> fun colony -> { colony with Time = time }

                let state =
                    BreachState.empty
                    |> foldView 2 100 (piles [ "old-a"; "old-b" ] 915 100)
                    |> foldView 2 101 (piles [ "old-a"; "old-b"; "new-c" ] 915 101)

                Expect.equal
                    (breachRows state |> List.map (fun row -> row.Breach.Subject))
                    [ "old-a"; "old-b" ]
                    "the cap drops the breach that has only just appeared"
            }
        ]

/// A tick the CPU line reads: full unless `light`, forced by `forced`,
/// costing `ms`, the bucket as it ended.
let private cpuTick (light: bool) (forced: string option) (ms: float) (bucket: int) : CpuReadings =
    {
        AtEntry = 0.0
        AtSnapshot = ms
        AtDecide = ms
        AtSave = ms
        AtExecute = ms
        Intents = 0
        Bucket = bucket
        Replans = 0
        ColonyDecides = []
        RoomSnapshots = []
        AtRooms = 0.0
        AtProjects = 0.0
        ColonyProjects = []
        ColonyFloods = []
        HeapMb = 0.0
        ExternalMb = 0.0
        MemoRows = 0
        Light = light
        Forced =
            forced
            |> Option.map (fun reason ->
                {
                    Reason = reason
                    Facts = 1
                    Replayed = 0
                    FightOnly = 0
                    ReplayMs = 0.0
                })
        Moved = []
    }

/// The CPU line over ticks 1..n, each tick's reading off its tick.
let private lineOf (n: int) (reading: int -> CpuReadings) =
    [ 1..n ]
    |> List.fold (fun line tick -> foldCpu capCpuTicks tick (reading tick) line) CpuState.empty

/// The world's breaches on one tick, as (kind, room, subject, amount) rows.
let private worldOn (facts: WorldBreachFacts) =
    worldBreaches Tuning.defaults facts
    |> List.map (fun breach -> breach.Kind, breach.Room, breach.Subject, breach.Amount)

/// The world at t100 under a 20 ms allowance, the bundle loaded long before
/// the line begins.
let private worldFacts line =
    {
        Time = 100
        Line = line
        Cadence = Some Tuning.defaults.Cadence
        Limit = 20.0
        LoadedAt = -1_000
        Faults = []
    }

/// The facts of a colony whose economy decided whole, quiet and unheld.
let private decided =
    { ColonyBreachFacts.none with
        Quotas = Some Quotas.silent
    }

/// One of ours whose task cursor went unassigned at `since`, after `task`.
let private idleAfter name task since : ObserveState =
    Map.ofList
        [
            name,
            {
                Entries =
                    [
                        {
                            Tick = since - 50
                            Verdict = Verdict.Kept(name, task)
                        }
                        {
                            Tick = since
                            Verdict = Verdict.Unassigned(name, IdleReason.NoneApplicable)
                        }
                    ]
                LastTask = Some(Verdict.Unassigned(name, IdleReason.NoneApplicable))
                LastScoring = None
                LastMove = []
            }
        ]

/// The same after a harvest at a rock no stand-down shuts.
let private idleSince name since = idleAfter name "harvest:source-1" since

/// A colony with one free spawn beside a bank holding `available` of 300.
let private spawnFree available =
    { quiet with
        Spawns =
            [
                {
                    Name = "Spawn1"
                    Id = "spawn-1"
                    RoomName = raidRoom
                    IsSpawning = false
                    Remaining = 0
                }
            ]
        Bank =
            {
                Available = available
                Capacity = 300
            }
    }

/// Quotas whose spawn plan left `seats` a full bank pays.
let private unfilled seats = { Quotas.silent with Unfilled = seats }

[<Tests>]
let selfBreachTests =
    testList
        "breach log: the bot reads itself (#630)"
        [
            test
                "a window of forced ticks over the limit with no light tick is light-starved; one light tick clears it" {
                // 2026-10-08: a chase forced every tick and the bucket fell 10k to 5k
                // before anyone looked. At cadence 2 the window is twenty ticks.
                let chasing = lineOf 30 (fun _ -> cpuTick false (Some "chase") 30.0 9_000)

                Expect.equal
                    (worldOn (worldFacts chasing))
                    [ BreachKind.LightStarved, "", "chase", 20 ]
                    "the amount is the ticks forcing took in the window, the subject what forced them"

                Expect.isEmpty
                    (worldOn (
                        worldFacts (
                            lineOf 30 (fun tick -> cpuTick (tick = 25) (Some "chase") 30.0 9_000)
                        )
                    ))
                    "a light tick inside the window is the cadence still running"

                Expect.isEmpty
                    (worldOn
                        { worldFacts chasing with
                            Cadence = None
                        })
                    "with light ticks off no light tick is owed"
            }

            test "a forced fight the bucket can pay for is not light-starved" {
                // A long defence forces every tick; at 10 ms of a 20 ms limit the
                // bucket refills, and nothing is starved.
                Expect.isEmpty
                    (worldOn (
                        worldFacts (lineOf 30 (fun _ -> cpuTick false (Some "raid") 10.0 10_000))
                    ))
                    "forced full ticks under the limit cost the bucket nothing"

                // A siege at 22 ms of 20 draws 2 a tick: a full bucket lasts 5,000
                // ticks of it, past the horizon. The same draw at 3,000 lasts 1,500.
                Expect.isEmpty
                    (worldOn (
                        worldFacts (lineOf 30 (fun _ -> cpuTick false (Some "raid") 22.0 10_000))
                    ))
                    "a draw a full bucket outlasts the horizon on is paid for"

                Expect.equal
                    (worldOn (
                        worldFacts (lineOf 30 (fun _ -> cpuTick false (Some "raid") 22.0 3_000))
                    ))
                    [ BreachKind.LightStarved, "", "raid", 20 ]
                    "the same draw on a bucket it empties inside the horizon starves it"
            }

            test "a bucket whose slope empties it inside the horizon is falling; a flat one is not" {
                // Ten a tick from 5,000: 4,000 left at tick 100 is 400 ticks of it.
                Expect.equal
                    (worldOn (
                        worldFacts (
                            lineOf capCpuTicks (fun tick ->
                                cpuTick true None 10.0 (5_000 - 10 * tick))
                        )
                    ))
                    [ BreachKind.BucketFalling, "", "bucket", 400 ]
                    "the amount is the ticks until the bucket stands empty at this slope"

                Expect.isEmpty
                    (worldOn (
                        worldFacts (lineOf capCpuTicks (fun _ -> cpuTick true None 10.0 10_000))
                    ))
                    "a full bucket holding is no fall"

                Expect.isEmpty
                    (worldOn (
                        worldFacts (
                            lineOf (capCpuTicks - 1) (fun tick ->
                                cpuTick true None 10.0 (5_000 - 10 * tick))
                        )
                    ))
                    "a ring short of full is too short to tell a dip from a fall"
            }

            test "a deploy's reset tick and first cadence starve no window of light ticks" {
                // Live t1,030,363: light-starved opened on two deploys running while
                // the window was a net refill; its mean held the 470 ms reset tick.
                // Reset at t11; every tick after forced at 15 ms of 20, the bucket full.
                let deploy (n: int) (ms: float) =
                    lineOf n (fun tick ->
                        cpuTick false (Some "chase") (if tick = 11 then 470.0 else ms) 9_000)

                Expect.equal
                    (worldOn (worldFacts (deploy 30 15.0)))
                    [ BreachKind.LightStarved, "", "chase", 20 ]
                    "read from before the reset, the reset tick alone puts the window over"

                Expect.isEmpty
                    (worldOn
                        { worldFacts (deploy 30 15.0) with
                            LoadedAt = 11
                        })
                    "the reset tick and its first cadence are no window's"

                Expect.equal
                    (worldOn
                        { worldFacts (deploy 40 30.0) with
                            LoadedAt = 11
                        })
                    [ BreachKind.LightStarved, "", "chase", 20 ]
                    "a window wholly after the settle starves as it would on any tick"
            }

            test "a deploy's draw on the bucket is no fall" {
                // The reset at t97 draws 470 off a bucket of 2,000 and the level stays
                // lower: over a ring reaching back before it the slope reads a fall.
                let line =
                    lineOf capCpuTicks (fun tick ->
                        cpuTick true None 10.0 (if tick >= 97 then 1_530 else 2_000))

                Expect.equal
                    (worldOn (worldFacts line) |> List.map (fun (kind, _, _, _) -> kind))
                    [ BreachKind.BucketFalling ]
                    "read from before the reset, the reset's draw is a slope"

                Expect.isEmpty
                    (worldOn { worldFacts line with LoadedAt = 97 })
                    "a ring not wholly after the reset's first cadence is too short to tell"
            }

            test
                "a throw since the load is a fault: the colony's in its room, the tick's in the world's" {
                Expect.equal
                    (foldBreaches
                        capBreaches
                        100
                        (colonyBreaches
                            { ColonyBreachFacts.none with
                                Faults = [ "decide", 99; "decide", 98; "decide", 40 ]
                            }
                            quiet
                            BreachState.empty)
                        BreachState.empty
                     |> openRows 100
                     |> List.map (fun row ->
                         row.Breach.Kind, row.Breach.Room, row.Breach.Subject, row.Breach.Amount))
                    [ BreachKind.Fault, raidRoom, "decide", 3 ]
                    "the step that threw, and how often since the load"

                Expect.equal
                    (worldOn
                        { worldFacts CpuState.empty with
                            Faults = [ "execute", 90 ]
                        })
                    [ BreachKind.Fault, "", "execute", 1 ]
                    "a throw no colony owns is the world's"

                Expect.isEmpty
                    (worldOn
                        { worldFacts CpuState.empty with
                            Time = 90 + faultOpenTicks
                            Faults = [ "execute", 90 ]
                        })
                    "a transient throw a creep's life old has closed, though the ring still lists it"
            }

            test
                "a spawn free beside a full bank while the plan leaves seats opens after the longest cast" {
                let foldCasting casting t colony quotas prior =
                    foldBreaches
                        capBreaches
                        t
                        (colonyBreaches
                            { ColonyBreachFacts.none with
                                Quotas = quotas
                                Casting = casting
                            }
                            { colony with Time = t }
                            prior)
                        prior

                let fold = foldCasting Set.empty

                let pending = BreachState.empty |> fold 100 (spawnFree 300) (Some(unfilled 2))

                Expect.equal
                    (breachRows pending, openRows 100 pending)
                    ([
                        {
                            FirstSeen = 100
                            LastSeen = 100
                            Breach =
                                {
                                    Kind = BreachKind.SpawnIdle
                                    Room = raidRoom
                                    Subject = "Spawn1"
                                    Amount = 2
                                    Window = spawnIdleWindow
                                }
                        }
                     ],
                     [])
                    "a pending row: the seats left, folded for its age"

                let opened =
                    pending
                    |> fold (100 + spawnIdleWindow - 1) (spawnFree 300) None
                    |> fold (100 + spawnIdleWindow) (spawnFree 300) (Some(unfilled 2))

                Expect.equal
                    (openRows (100 + spawnIdleWindow) opened |> List.map _.Breach.Subject)
                    [ "Spawn1" ]
                    "a fight-only tick in between leaves it standing, and a whole cast later it is open"

                Expect.isEmpty
                    (breachRows (
                        opened |> fold (101 + spawnIdleWindow) (spawnFree 300) (Some(unfilled 0))
                    ))
                    "the plan leaving no seat a full bank pays closes it"

                Expect.isEmpty
                    (breachRows (
                        BreachState.empty
                        |> foldCasting
                            (Set.singleton "Spawn1")
                            100
                            (spawnFree 300)
                            (Some(unfilled 2))
                    ))
                    "a spawn this tick's plan casts on is busy"

                Expect.isEmpty
                    (breachRows (BreachState.empty |> fold 100 (spawnFree 299) (Some(unfilled 2))))
                    "a bank still filling is income, not an idle spawn"
            }

            test
                "a spawn whose last cast the engine refused casts nothing, whatever this tick names it for" {
                let idleOn castRefused =
                    colonyBreaches
                        { ColonyBreachFacts.none with
                            Quotas = Some(unfilled 2)
                            Casting = Set.singleton "Spawn1"
                            CastRefused = castRefused
                        }
                        { spawnFree 300 with Time = 100 }
                        BreachState.empty
                    |> List.filter (fun breach -> breach.Kind = BreachKind.SpawnIdle)
                    |> List.map _.Subject

                Expect.equal
                    (idleOn (Set.singleton "Spawn1"))
                    [ "Spawn1" ]
                    "its last cast refused, this one asks the same (#629)"

                Expect.isEmpty (idleOn Set.empty) "a spawn whose last cast the engine took is busy"
            }

            test "an actor refused on every decide for a half-life is a refusal storm" {
                let halfLife = Tuning.defaults.RefusalHalfLife

                let colony t news =
                    { quiet with
                        Time = t
                        Refusals =
                            [
                                { Refusal.one "claimer-1" true "" "claim:c1" -15 (t - 1) with
                                    Fresh = news
                                }
                            ]
                    }

                let fold t news prior =
                    foldBreaches
                        capBreaches
                        t
                        (colonyBreaches ColonyBreachFacts.none (colony t news) prior)
                        prior

                let pending = BreachState.empty |> fold 100 1

                Expect.equal
                    (breachRows pending |> List.map _.Breach, openRows 100 pending)
                    ([
                        {
                            Kind = BreachKind.RefusalStorm
                            Room = raidRoom
                            Subject = "claimer-1"
                            Amount = 1
                            Window = halfLife
                        }
                     ],
                     [])
                    "pending: its price may yet move it"

                let stood = pending |> fold (100 + halfLife) 2

                Expect.equal
                    (openRows (100 + halfLife) stood |> List.map _.Breach.Subject)
                    [ "claimer-1" ]
                    "refused still a half-life on, the price moved nothing"

                Expect.isEmpty
                    (breachRows (stood |> fold (101 + halfLife) 0))
                    "a decide with no refusal since the last closes it"
            }

            test "a body with no Task for a tenth of its life is idle; a fleeing one is not" {
                let colony t =
                    { quiet with
                        Time = t
                        Creeps = [ ours "worker-1" ]
                    }

                let idleWith facts t log =
                    colonyBreaches { facts with Log = log } (colony t) BreachState.empty
                    |> List.map (fun breach ->
                        breach.Kind, breach.Room, breach.Subject, breach.Amount)

                let idleOn = idleWith decided

                Expect.equal
                    (idleOn 350 (idleSince "worker-1" 200))
                    [ BreachKind.BodyIdle, raidRoom, "worker-1", 150 ]
                    "none-applicable since t200, a tenth of 1,500 ticks later"

                Expect.isEmpty
                    (idleOn 349 (idleSince "worker-1" 200))
                    "a tick short of it is a wait"

                let withTask task =
                    idleSince "worker-1" 200
                    |> Map.map (fun _ record ->
                        { record with
                            Entries =
                                record.Entries
                                @ [
                                    {
                                        Tick = 340
                                        Verdict = Verdict.Kept("worker-1", task)
                                    }
                                ]
                            LastTask = Some(Verdict.Kept("worker-1", task))
                        })

                Expect.isEmpty (idleOn 350 (withTask "flee")) "a Flee is a Task"
                Expect.isEmpty (idleOn 350 (withTask "build")) "a new Task closes it"
            }

            test "a body held on purpose is not idle" {
                let idleOf facts (creep: CreepInfo) =
                    colonyBreaches
                        { facts with
                            Log = idleSince creep.Name 200
                        }
                        { quiet with
                            Time = 350
                            Creeps = [ creep ]
                        }
                        BreachState.empty

                Expect.isEmpty
                    (idleOf
                        decided
                        { ours "guard-1" with
                            Body = Map.ofList [ Attack, 2; Move, 2 ]
                        })
                    "a fighter with no Task and no ask open for it is a reserve at its idle home"

                // #647: W13S28 cast guard-1023983-Spawn2 for guard:W13S29, and it idled
                // none-applicable at home from birth, the ask open and no seat for it.
                let guardAsked =
                    { decided with
                        Quotas =
                            Some
                                { Quotas.silent with
                                    Casts =
                                        [
                                            {
                                                For = Guard "W13S29"
                                                Ask =
                                                    {
                                                        Row = CastRow.Guard
                                                        Wanted = 1
                                                        Blocks = 1
                                                    }
                                            }
                                        ]
                                }
                    }

                Expect.equal
                    (idleOf
                        guardAsked
                        { ours "guard-1" with
                            Body = Map.ofList [ Attack, 2; Move, 2 ]
                        }
                     |> List.map (fun breach -> breach.Kind, breach.Subject, breach.Amount))
                    [ BreachKind.BodyIdle, "guard-1", 150 ]
                    "a fighter its colony casts for with no Task is cast-for-but-unseated, and idle"

                // A child fed spends the energy the upgrade would take: the upgrader
                // row is the one it starves, and it starves nobody else.
                let feeding =
                    { decided with
                        Quotas =
                            Some
                                { Quotas.silent with
                                    Busy = [ FeedingChild ]
                                }
                    }

                Expect.isEmpty
                    (idleOf feeding (ours "upgrader-1"))
                    "the starved row's body waits by design"

                Expect.isNonEmpty
                    (idleOf feeding (ours "hauler-1"))
                    "a body of a row the child's feed does not starve still counts"

                let shut =
                    { decided with
                        StoodDown = Set.singleton outpostRoom
                    }

                let idleAfterIn facts (creep: CreepInfo) task =
                    colonyBreaches
                        { facts with
                            Log = idleAfter creep.Name task 200
                        }
                        { quiet with
                            Time = 350
                            Creeps = [ creep ]
                        }
                        BreachState.empty

                Expect.isEmpty
                    (idleAfterIn shut (ours "reserver-1") $"reserve:{outpostRoom}")
                    "a stand-down holds the body whose last Task worked the shut outpost"

                Expect.isNonEmpty
                    (idleAfterIn shut (ours "worker-1") "build:site-1")
                    "and not a body whose work was elsewhere"

                Expect.isEmpty
                    (idleOf ColonyBreachFacts.none (ours "worker-1"))
                    "an economy that did not decide whole says nothing of its holds"

                Expect.isNonEmpty
                    (idleOf
                        { decided with
                            Quotas =
                                Some
                                    { Quotas.silent with
                                        Busy = [ SitesOwed ]
                                    }
                        }
                        (ours "worker-1"))
                    "sites owed are work, and hold nobody"
            }

            test "the cap evicts an idle body before the ore" {
                let idle n =
                    {
                        Kind = BreachKind.BodyIdle
                        Room = raidRoom
                        Subject = $"worker-{n}"
                        Amount = 150
                        Window = 0
                    }

                let pile =
                    {
                        Kind = BreachKind.OreOnTheFloor
                        Room = raidRoom
                        Subject = "pile-1"
                        Amount = 915
                        Window = 0
                    }

                let state =
                    BreachState.empty
                    |> foldBreaches 2 100 [ idle 1; idle 2 ]
                    |> foldBreaches 2 101 [ idle 1; idle 2; pile ]

                Expect.contains
                    (breachRows state |> List.map _.Breach.Subject)
                    "pile-1"
                    "the newest row survives when it is the ore and the older ones idle bodies"
            }
        ]
