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

/// The breaches this view yields on one tick, as (kind, room, subject, amount)
/// rows — the whole of what a row says, so a case that fires the right kind on
/// the wrong object cannot pass.
let private breachesOn t (colony: ColonyView) =
    foldBreaches capBreaches t { colony with Time = t } BreachState.empty
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
                    |> foldBreaches capBreaches 100 { colony with Time = 100 }
                    |> foldBreaches capBreaches 150 { colony with Time = 150 }

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
                    |> foldBreaches capBreaches 100 { broken with Time = 100 }
                    |> foldBreaches
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
                    |> foldBreaches capBreaches 100 { broken with Time = 100 }
                    |> foldBreaches capBreaches 101 { clear with Time = 101 }
                    |> foldBreaches capBreaches 102 { broken with Time = 102 }

                Expect.equal
                    (standing 102 state |> List.map snd)
                    [ 0 ]
                    "the returning pile is a new row, not the old one resumed"
            }

            test "the latest reading wins: a growing pile reports what it holds now" {
                let state =
                    BreachState.empty
                    |> foldBreaches
                        capBreaches
                        100
                        { (quiet |> owning raidRoom |> withPile raidRoom "pile-1" 90) with
                            Time = 100
                        }
                    |> foldBreaches
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
                    |> foldBreaches 2 100 (piles [ "old-a"; "old-b" ] 915 100)
                    |> foldBreaches 2 101 (piles [ "old-a"; "old-b"; "new-c" ] 915 101)

                Expect.equal
                    (breachRows state |> List.map (fun row -> row.Breach.Subject))
                    [ "old-a"; "old-b" ]
                    "the cap drops the breach that has only just appeared"
            }
        ]
