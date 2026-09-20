/// The projection's shape, asked of both sides: of every fixture this
/// suite hands to a decision, and of what `ColonyView.ofWorld` builds out
/// of a world. The table and the check are `SpatialFixtures`'.
module Fabot.Core.Tests.ProjectionShapeTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures

/// The colonies the shared fixture files hand to a decision, by the name a
/// failure should print. The ore path first, then a spread over the other
/// domains: a fixture nobody sweeps is a fixture free to drift.
let private fixtures =
    [
        "mineColony", mineColony
        "mineHaulColony", mineHaulColony
        "consigningColony", consigningColony 10_000 10_000
        "receivingColony", receivingColony 10_000
        "deliveryColony", deliveryColony (Some Ownership.Ours)
        "bareRespawn", bareRespawn
        "dualSeatColony", dualSeatColony
        "trunkColony", trunkColony 6
        "stockColony", Decide.PoolFixtures.stockColony [] (Map.ofList [ "sto-1", 5_000 ])
        "drawColony",
        Decide.PoolFixtures.drawColony
            (Map.ofList [ "can-src", 500; "sto-1", 500 ])
            (worker "w1" 0 100)
            { X = 13; Y = 10 }
        "clusterColony", Decide.PoolFixtures.clusterColony (100, 50, 0) [] []
        "quotaColony", Decide.QuotaFixtures.quotaColony 10 1 1_000
        "richIncomeColony", Decide.QuotaFixtures.richIncomeColony
        "guardColony", Decide.QuotaFixtures.guardColony [] []
        "deadlockColony", Decide.QuotaFixtures.deadlockColony
        "successionColony", Decide.AnchorFixtures.successionColony
        "mineralColony", Decide.LayoutFixtures.mineralColony 6
        "outpostContainerColony", Decide.LayoutFixtures.outpostContainerColony
    ]

// ---- the other side: a world, swept the way the shell sweeps one ----------

let private home = "W1N1"
let private outpost = "W1N2"
let private errand = "W1N3"

/// Plain to every edge, and a ring on all four of them — not a corner of
/// the room: a crossing needs ground of the far room beside its landing,
/// so a room floored in one corner is one no chain can enter, and the
/// errand three rooms out falls out of the scan set with no test saying so.
let private ground =
    TerrainGrid.ofList
        [
            for x in 1 .. Seam.exitEdge - 1 do
                for y in 1 .. Seam.exitEdge - 1 -> { X = x; Y = y }, Plain
        ]

let private ring =
    Map.ofList
        [
            for i in 0 .. Seam.exitEdge do
                yield { X = i; Y = 0 }, Plain
                yield { X = i; Y = Seam.exitEdge }, Plain
                yield { X = 0; Y = i }, Plain
                yield { X = Seam.exitEdge; Y = i }, Plain
        ]

/// One room as the sweep leaves it: the objects it found, each filed in every
/// map `World.ofGame` would file it in and in none it would not.
let private roomOf name owner (targets: (string * Pos * TargetKind) list) stores ore =
    name,
    { RoomFacts.empty with
        Layer =
            { RoomLayer.empty with
                Terrain = ground
                TargetPositions = targets |> List.map (fun (id, pos, _) -> id, pos) |> Map.ofList
            }
        Border = ring
        TargetKinds = targets |> List.map (fun (id, _, kind) -> id, kind) |> Map.ofList
        Stores = Map.ofList stores
        Thorium = Map.ofList ore
        Control =
            Some
                {
                    Owner = owner
                    Reservation = None
                    SafeMode = false
                    Sign = None
                }
    }

/// The home room with one of everything a decision reads a number off.
let private homeFacts =
    roomOf
        home
        Ownership.Ours
        [
            "sto-1", { X = 5; Y = 5 }, Structure BuiltKind.Storage
            "can-1", { X = 6; Y = 5 }, Structure BuiltKind.Container
            "term-1", { X = 7; Y = 5 }, Structure BuiltKind.Terminal
            "min-1", { X = 8; Y = 5 }, Mineral
            "ext-1", { X = 8; Y = 5 }, Structure BuiltKind.Extractor
            "tomb-1", { X = 9; Y = 5 }, Tombstone
            "pile-1", { X = 10; Y = 5 }, Dropped Thorium
            "src-1", { X = 11; Y = 5 }, Source
            "ctrl-1", { X = 12; Y = 5 }, Controller
        ]
        [ "sto-1", 100_000; "can-1", 2_000; "term-1", 5_000; "tomb-1", 50 ]
        [
            "sto-1", 900
            "can-1", 600
            "term-1", 300
            "min-1", 22_000
            "tomb-1", 175
            "pile-1", 400
        ]
    |> fun (name, facts) ->
        name,
        { facts with
            Cooldowns = Map.ofList [ "ext-1", 3 ]
            Controller =
                Some
                    {
                        Id = "ctrl-1"
                        Level = 6
                        TicksToDowngrade = 20_000
                        SafeModeAvailable = 1
                        SafeModeActive = false
                    }
            Spawns =
                [
                    {
                        Name = "Spawn1"
                        Id = "spawn-1"
                        RoomName = home
                        IsSpawning = false
                    }
                ]
            Energy = { Available = 2_300; Capacity = 2_300 }
        }

/// The declared Reactor's room: the ore that fell there, and the Reactor
/// itself — which the sweep files in `Reactors` and `Owners` and **nowhere
/// else**, giving it neither a tile nor a kind.
let private errandFacts =
    roomOf
        errand
        Ownership.Unowned
        [
            "tomb-r", { X = 4; Y = 4 }, Tombstone
            "pile-r", { X = 5; Y = 4 }, Dropped Thorium
        ]
        []
        [ "tomb-r", 500; "pile-r", 200 ]
    |> fun (name, facts) ->
        name,
        { facts with
            Owners = Map.ofList [ "reactor-1", Ownership.Ours ]
            Reactors =
                [
                    {
                        Id = "reactor-1"
                        Owner = ReactorOwner.Ours
                        Thorium = 999
                        ContinuousWork = 8_000
                    }
                ]
        }

let private outpostFacts =
    roomOf
        outpost
        Ownership.Unowned
        [
            "src-out", { X = 5; Y = 5 }, Source
            "ctrl-out", { X = 7; Y = 7 }, Controller
            "can-out", { X = 6; Y = 5 }, Structure BuiltKind.Container
        ]
        [ "can-out", 1_200 ]
        []

let private declared: Colony list =
    [
        {
            Home = home
            Outposts =
                [
                    {
                        RoomName = outpost
                        Sources = [ "src-out", { Room = outpost; X = 5; Y = 5 } ]
                        Controller = "ctrl-out", { Room = outpost; X = 7; Y = 7 }
                    }
                ]
            Errands =
                [
                    {
                        RoomName = errand
                        Target = "reactor-1", { Room = errand; X = 4; Y = 6 }
                    }
                ]
            Mother = None
            Consignee = None
        }
    ]

let private world: World =
    {
        Time = 1_000
        Rooms = Map.ofList [ homeFacts; outpostFacts; errandFacts ]
        Creeps = []
        Sightings = Map.empty
    }

let private sweptView () =
    let holders =
        World.creepColonies Tuning.defaults declared (World.living declared world) Map.empty world

    ColonyView.ofWorld Tuning.defaults declared StandDown.none holders world (List.head declared)

[<Tests>]
let projectionShapeTests =
    testList
        "projection shape"
        [
            test "no shared fixture files a number under an object the sweep never files one under" {
                let offenders =
                    fixtures
                    |> List.collect (fun (name, colony) ->
                        shapeViolations colony |> List.map (shapeViolationLine name))

                Expect.equal
                    offenders
                    []
                    "a fixture that builds a shape the sweep cannot is a test that agrees with the code about something the engine does differently (#355)"
            }

            test "what ofWorld builds out of a conforming world conforms too" {
                // The half a Core test can run (`ColonyView.ofWorld` is Core; the sweep
                // that feeds it is not). The narrowings are filters, so what this refuses
                // is one that moves an entry between maps, or keeps a number after taking
                // away the kind that made it readable.
                let offenders =
                    shapeViolations (sweptView ()) |> List.map (shapeViolationLine "ofWorld")

                Expect.equal offenders [] "the projection the shell builds is one the table admits"
            }

            test "the Reactor's store is not an ore entry, in the world or in the view" {
                // The sweep files a Reactor in `Reactors` and `Owners` and gives it no
                // kind, so a Thorium entry for it is a shape nothing can build.
                let view = sweptView ()

                Expect.isNone
                    (Map.tryFind "reactor-1" view.Spatial.Thorium)
                    "the store is not here, which is where the gate used to look"

                Expect.equal
                    (view.Reactors |> List.map (fun reactor -> reactor.Id, reactor.Thorium))
                    [ "reactor-1", 999 ]
                    "it is here, and it is the 999 the gate has to see"

                Expect.equal
                    (shapeViolations
                        { view with
                            Spatial =
                                { view.Spatial with
                                    Thorium = Map.add "reactor-1" 999 view.Spatial.Thorium
                                }
                        })
                    [
                        {
                            Map = "Thorium"
                            Id = "reactor-1"
                            Kind = None
                        }
                    ]
                    "and writing it into the map the gate used to read is what the check refuses"
            }

            test "a cooldown belongs to an extractor and an ore holding to a store that has one" {
                let view =
                    { bareRespawn with
                        Spatial =
                            { bareRespawn.Spatial with
                                TargetKinds =
                                    Map.ofList
                                        [
                                            "can-1", Structure BuiltKind.Container
                                            "spawn-1", Structure BuiltKind.Spawn
                                            "min-1", Mineral
                                            "pile-1", Dropped Thorium
                                        ]
                                Thorium =
                                    Map.ofList [ "can-1", 600; "min-1", 22_000; "pile-1", 300 ]
                                Cooldowns = Map.ofList [ "spawn-1", 3 ]
                            }
                    }

                Expect.equal
                    (shapeViolations view)
                    [
                        {
                            Map = "Cooldowns"
                            Id = "spawn-1"
                            Kind = Some(Structure BuiltKind.Spawn)
                        }
                    ]
                    "the container, the deposit and the pile all carry ore; only the extractor carries a cooldown"
            }
        ]
