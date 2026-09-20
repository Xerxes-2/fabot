/// A whole colony over one tick of `decide`, and its Anchors on their
/// Posts.
module Fabot.Core.Tests.RoomColonyTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Atlas
open Fabot.Core.Decide
open Fabot.Core.Tests.RoomFixtures
open Fabot.Core.Tests.Decide
open Fabot.Core.Tests.RoomInvariantFixtures

/// One tick of `decide` over a whole colony, at three rungs of a colony's
/// life: the child as it was claimed, the child at the level the bootstrap
/// window closes on, and the mother this bot grew up on. Smoke tests, not
/// assertions about the decisions: a colony built from real terrain at that
/// level and bank goes through the whole pipeline without throwing, and no
/// creep of its fleet comes out of it without a Verdict.
[<Tests>]
let colonyTierTests =
    testList
        "a whole colony, one tick of decide"
        [
            for room, level, bank in colonyTiers ->
                test $"{room} at RCL{level} on a {bank} bank" {
                    let snapshot = colonyAt (load room) level bank

                    Expect.isNonEmpty
                        snapshot.Creeps
                        "the premise: a colony with no fleet would make the Verdict check vacuous"

                    let decision = decide snapshot Map.empty Set.empty None
                    let judged = decision.Verdicts |> List.map verdictCreep |> Set.ofList

                    for creep in snapshot.Creeps do
                        Expect.isTrue
                            (Set.contains creep.Name judged)
                            $"{creep.Name} came out of the tick with no Verdict"
                }
        ]

/// The arbitration's invariant: a room's pass settles a **matching** — one
/// creep to a tile, one tile to a creep.
///
/// Pinned on a crowd in a corridor, exhaustively: every way of standing
/// three bodies on the eight tiles of `Fixtures.lane` with either rock as
/// either body's goal — 5,376 arrangements. The colony fixtures below are
/// a smoke check and not the pin: at one or two movers a tick they hold
/// almost no contested tile, and this invariant was once lost under them
/// (the search freed a displaced body's tile before recursing).
[<Tests>]
let arbitrationInjectiveTests =
    testList
        "one settled tile each"
        [
            test "a one-wide lane, every three-body arrangement of it" {
                let tiles = [ for x in 8..15 -> { X = x; Y = 12 } ]
                let names = [ "a"; "b"; "c" ]
                let rocks = [ "src-w"; "src-e" ]

                let settledOn pocket places goals =
                    let view =
                        { Fixtures.bareRespawn with
                            Sources = rocks |> List.map Fixtures.source
                            Controller = None
                            Creeps = names |> List.map (fun name -> Fixtures.worker name 0 50)
                            Spatial =
                                Fixtures.lane pocket
                                |> withHome (fun layer ->
                                    { layer with
                                        CreepPositions = List.zip names places |> Map.ofList
                                    })
                        }

                    let stepOf =
                        Fixtures.resolveOn view (List.zip names goals)
                        |> Fixtures.moveIntents
                        |> Map.ofList

                    List.zip names places
                    |> List.map (fun (creep, pos) ->
                        match Map.tryFind creep stepOf with
                        | Some direction -> Fixtures.stepFrom pos direction
                        | None -> pos)

                let jams =
                    [
                        for pocket in [ false; true ] do
                            for a in tiles do
                                for b in tiles do
                                    for c in tiles do
                                        if a <> b && a <> c && b <> c then
                                            for ga in rocks do
                                                for gb in rocks do
                                                    for gc in rocks do
                                                        let places = [ a; b; c ]

                                                        let goals =
                                                            [ Harvest ga; Harvest gb; Harvest gc ]

                                                        let settled = settledOn pocket places goals

                                                        if
                                                            List.length (List.distinct settled)
                                                            <> List.length settled
                                                        then
                                                            yield pocket, places, goals, settled
                    ]

                Expect.isEmpty jams "every arrangement settles three bodies onto three tiles"
            }

            for room, level, bank in colonyTiers do
                test $"{room} at RCL{level}: no two bodies are settled onto one tile" {
                    let snapshot = colonyAt (load room) level bank
                    let decision = decide snapshot Map.empty Set.empty None

                    let stepOf =
                        decision.Intents
                        |> List.choose (function
                            | MoveCreep(name, direction) -> Some(name, direction)
                            | _ -> None)
                        |> Map.ofList

                    for KeyValue(name, layer) in snapshot.Spatial.Rooms do
                        let settled =
                            layer.CreepPositions
                            |> Map.toList
                            |> List.map (fun (creep, pos) ->
                                match Map.tryFind creep stepOf with
                                | Some direction -> Fixtures.stepFrom pos direction
                                | None -> pos)

                        Expect.hasLength
                            (List.distinct settled)
                            (List.length settled)
                            $"two bodies settled onto one tile of {name}"
                }
        ]

/// Where the fixture's Anchor row stands, which is the one placement a
/// later pairwise test cannot check for itself. An Anchor stationed
/// *beside* its container is a body on a walk, and every quota, cap and
/// Seat rule read off this fixture would be read against a fleet that
/// never digs. W13S28's `16,7` is the counterexample: its one Seat is the
/// container's, so "the nearest free tile" is range 2 from the rock and
/// out of Harvest range altogether.
[<Tests>]
let colonyAnchorPostTests =
    testList
        "a whole colony, its Anchors on their Posts"
        [
            for room, level, bank in colonyTiers ->
                test $"{room} at RCL{level}: every Anchor stands on a Post" {
                    let capture = load room
                    let snapshot = colonyAt capture level bank
                    let layer = snapshot.Spatial.Rooms[room]

                    let containerTiles =
                        snapshot.Spatial.TargetKinds
                        |> Map.toList
                        |> List.choose (fun (id, kind) ->
                            match kind with
                            | Structure BuiltKind.Container -> Map.tryFind id layer.TargetPositions
                            | _ -> None)
                        |> Set.ofList

                    let sources = capture.Sources |> List.map snd

                    let anchors =
                        snapshot.Creeps
                        |> List.filter (fun creep -> creep.Name.StartsWith "anchor-")
                        |> List.map (fun creep -> creep.Name, layer.CreepPositions[creep.Name])

                    Expect.hasLength
                        anchors
                        sources.Length
                        "one Anchor per Post, which is one per source here"

                    for name, pos in anchors do
                        Expect.isTrue
                            (Set.contains pos containerTiles)
                            $"{name} at {pos.X},{pos.Y} stands on no container, so it garrisons no Post"

                        Expect.isTrue
                            (sources
                             |> List.exists (fun source ->
                                 max (abs (source.X - pos.X)) (abs (source.Y - pos.Y)) <= 1))
                            $"{name} at {pos.X},{pos.Y} is out of Harvest range of every source"
                }
        ]
