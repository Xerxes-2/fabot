/// The room layer a site is filed under, and the square ring.
module Fabot.Core.Tests.Decide.LayoutRoomTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.LayoutFixtures

[<Tests>]
let roomLayerTests =
    testList
        "room layer"
        [
            test "a container belongs to the source and the controller of its own room" {
                let refills =
                    planTasksOn collidingRooms noThreats
                    |> List.choose (function
                        | Refill(id, _) -> Some id
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

                // The mirror: the quota folds every projected room's
                // containers, each judged against the sources of its own
                // room. The failure guarded is an outpost container paired
                // with the home rock and flooded over home terrain, hiring
                // a fleet for a haul nobody makes.
                Expect.equal
                    (quotaOf outpostContainerColony)
                    0
                    "a container whose own room places no rock it serves hires nobody"
            }

            test "the home room keeps its own targets after a second one has joined" {
                // A target added after an outpost layer is in the projection
                // lands beside that layer, never over it. The failure is
                // silent: a dropped home container produces no Refill and
                // no quota, and reads as "the room rule rejected it".
                let late =
                    collidingRooms
                    |> withTarget "can-late" { X = 26; Y = 22 } (Structure BuiltKind.Container)

                let refills =
                    planTasksOn late noThreats
                    |> List.choose (function
                        | Refill(id, _) -> Some id
                        | _ -> None)
                    |> List.sort

                Expect.equal
                    refills
                    [ "can-home"; "can-late" ]
                    "a container added after the outpost joined is still the controller's"
            }

            test "a projection that names no room files and reads under the empty name" {
                // The convention `SpatialInfo.homeName` spells, which every
                // fixture that never sets `RoomName` rests on: tiles and no
                // room name is this colony's own room, filed and read under
                // the empty name. A site that spelled it differently would
                // file the home room under one name and read it under
                // another, and every home query would answer the empty set
                // rather than throw.
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

[<Tests>]
let squareRingTests =
    testList
        "the square ring"
        [
            test "two bodies on the move meeting on a road ring pass each other (#225)" {
                // #225 (user's geometry): a road ring around one obstacle,
                // two bodies bound opposite ways meet on one edge, both
                // switch to the other edge together, meet again, and so on.
                let ring =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                    }
                    |> withHome (fun layer ->
                        { layer with
                            // A 3×3 block of extensions with a road ring around it, a
                            // plain approach row on each side and wall everywhere else:
                            // the two lanes round the block are equal and the only ways
                            // past, so the occupancy surcharge alone picks the lane.
                            Terrain =
                                TerrainGrid.ofList (
                                    [
                                        for x in 6..18 do
                                            for y in 9..15 -> { X = x; Y = y }, Wall
                                    ]
                                    @ [
                                        for x in 10..14 do
                                            for y in 10..14 do
                                                if x = 10 || x = 14 || y = 10 || y = 14 then
                                                    { X = x; Y = y }, Plain
                                    ]
                                    @ [ for x in 6..9 -> { X = x; Y = 12 }, Plain ]
                                    @ [ for x in 15..18 -> { X = x; Y = 12 }, Plain ]
                                    @ [ { X = 5; Y = 12 }, Wall; { X = 19; Y = 12 }, Wall ]
                                )
                            Roads =
                                Set.ofList
                                    [
                                        for x in 10..14 do
                                            for y in 10..14 do
                                                if x = 10 || x = 14 || y = 10 || y = 14 then
                                                    { X = x; Y = y }
                                    ]
                            Obstacles =
                                Set.ofList
                                    [
                                        for x in 11..13 do
                                            for y in 11..13 -> { X = x; Y = y }
                                    ]
                            TargetPositions =
                                Map.ofList (
                                    [ "src-w", { X = 5; Y = 12 }; "src-e", { X = 19; Y = 12 } ]
                                    @ [
                                        for x in 11..13 do
                                            for y in 11..13 -> $"ext-{x}-{y}", { X = x; Y = y }
                                    ]
                                )
                        })
                    |> fun s ->
                        { s with
                            TargetKinds =
                                (s.TargetKinds,
                                 [
                                     for x in 11..13 do
                                         for y in 11..13 -> $"ext-{x}-{y}"
                                 ])
                                ||> List.fold (fun kinds id ->
                                    Map.add id (Structure BuiltKind.Extension) kinds)
                        }

                let assigned = [ "eb", Harvest "src-e"; "wb", Harvest "src-w" ]

                let fatigueOf variant name tick =
                    match variant with
                    | 1 -> if (name = "eb") = (tick % 2 = 1) then 4 else 0
                    | _ -> 0

                let tickOf variant tick (positions: Map<string, Pos>) =
                    { bareRespawn with
                        Sources = [ source "src-w"; source "src-e" ]
                        Controller = None
                        Refillables = []
                        Creeps =
                            [
                                { worker "eb" 0 50 with
                                    Fatigue = fatigueOf variant "eb" tick
                                    Moved = variant <> 2 && tick > 0
                                }
                                { worker "wb" 0 50 with
                                    Fatigue = fatigueOf variant "wb" tick
                                    Moved = variant <> 2 && tick > 0
                                }
                            ]
                        Spatial =
                            ring
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = positions
                                })
                    }

                let start = Map.ofList [ "eb", { X = 7; Y = 12 }; "wb", { X = 17; Y = 12 } ]

                let arrived (positions: Map<string, Pos>) =
                    positions["eb"].X >= 18 && positions["wb"].X <= 6

                let rec drive variant tick positions trace =
                    if arrived positions then
                        Some tick, List.rev trace
                    elif tick > 30 then
                        None, List.rev trace
                    else
                        let moves =
                            resolveOn (tickOf variant tick positions) assigned |> moveIntents

                        let stepped =
                            (positions, moves)
                            ||> List.fold (fun acc (name, direction) ->
                                Map.add name (stepFrom acc[name] direction) acc)

                        let eb = positions["eb"]
                        let wb = positions["wb"]

                        let line =
                            sprintf
                                "t%d eb=(%d,%d) wb=(%d,%d) moves=%A"
                                tick
                                eb.X
                                eb.Y
                                wb.X
                                wb.Y
                                moves

                        drive variant (tick + 1) stepped (line :: trace)

                let outcome variant =
                    let ticks, trace = drive variant 0 start []
                    ticks, String.concat "\n" trace

                let moving, movingTrace = outcome 0

                Expect.isSome
                    moving
                    (sprintf "two bodies on the move pass each other:\n%s" movingTrace)

                let tired, tiredTrace = outcome 1
                Expect.isSome tired (sprintf "and with alternating fatigue too:\n%s" tiredTrace)

                // The pairwise control, and the loop the user filmed: read as
                // standing traffic (the pre-#225 surcharge), the two bodies
                // price each other's tile, switch lanes together every tick
                // and never pass.
                let standing, _ = outcome 2

                Expect.isNone
                    standing
                    "read as standing traffic they switch lanes together forever — the livelock"
            }
        ]
