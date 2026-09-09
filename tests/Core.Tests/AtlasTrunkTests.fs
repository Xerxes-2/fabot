/// Trunk paths, haul round trips, and the walk table the census recalls
/// (ADR 0032).
module Fabot.Core.Tests.AtlasTrunkTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Atlas
open Fabot.Core.Tests.AtlasFixtures

/// Corridor y = 10, x = 10..20, walled off either side: the container tile at
/// (10,10) and the spawn structure standing at (20,10), an obstacle — so the
/// one tile a body can be born on is (19,10), nine steps from the far end.
/// The roads and the standing bodies are the two things a walk over it is
/// priced against, and the two testLists below vary one each.
let private corridorWith roads creeps =
    spatial [ "spawn-1", { X = 20; Y = 10 } ] [ for x in 10..20 -> { X = x; Y = 10 }, Plain ]
    |> withHome (fun layer ->
        { layer with
            Obstacles = Set.singleton { X = 20; Y = 10 }
            Roads = roads
            CreepPositions = Map.ofList creeps
        })
    |> snapshotWith []
    |> ofView

[<Tests>]
let trunkPathTests =
    testList
        "atlas trunkPathHome"
        [
            // A 5-wide corridor of plain ground along y = 10, three rows tall,
            // anchored at a wall tile the way a source is embedded in one.
            let corridor =
                spatial
                    []
                    [
                        yield { X = 10; Y = 10 }, Wall
                        for x in 11..14 do
                            for y in 9..11 do
                                yield { X = x; Y = y }, Plain
                    ]

            test "paves the straight line from beside the anchor to the goal" {
                // A single-row corridor: diagonal steps cost the same as
                // straight ones, so a wider room may legally drift the line.
                let atlas =
                    spatial
                        []
                        [
                            yield { X = 10; Y = 10 }, Wall
                            for x in 11..14 do
                                yield { X = x; Y = 10 }, Plain
                        ]
                    |> snapshotWith []
                    |> ofView

                Expect.equal
                    (trunkPathHome
                        atlas
                        Set.empty
                        { X = 10; Y = 10 }
                        (Set.singleton { X = 14; Y = 10 }))
                    [
                        { X = 11; Y = 10 }
                        { X = 12; Y = 10 }
                        { X = 13; Y = 10 }
                        { X = 14; Y = 10 }
                    ]
                    "the path starts beside the impassable anchor and ends on the goal"
            }

            test "routes around avoided tiles" {
                let atlas = corridor |> snapshotWith [] |> ofView

                let path =
                    trunkPathHome
                        atlas
                        (Set.singleton { X = 12; Y = 10 })
                        { X = 10; Y = 10 }
                        (Set.singleton { X = 14; Y = 10 })

                Expect.isFalse
                    (List.contains { X = 12; Y = 10 } path)
                    "an avoided tile is never paved through"

                Expect.equal (List.last path) { X = 14; Y = 10 } "the goal is still reached"
                Expect.hasLength path 4 "the detour is a same-length diagonal"
            }

            test "prices raw terrain: a built road on a swamp does not attract the line" {
                // The straight line crosses a swamp that already carries a
                // road; normal pricing would make it the cheap lane, but
                // trunk pricing reads the ground under it (ADR 0011).
                let atlas =
                    corridor
                    |> withHome (fun layer ->
                        { layer with
                            Terrain = Map.add { X = 12; Y = 10 } Swamp layer.Terrain
                            Roads = Set.singleton { X = 12; Y = 10 }
                        })
                    |> snapshotWith []
                    |> ofView

                let path =
                    trunkPathHome
                        atlas
                        Set.empty
                        { X = 10; Y = 10 }
                        (Set.singleton { X = 14; Y = 10 })

                Expect.isFalse
                    (List.contains { X = 12; Y = 10 } path)
                    "the swamp is dodged though its road would be cheap to walk"

                Expect.equal (List.last path) { X = 14; Y = 10 } "the goal is still reached"
            }

            test "an obstacle structure is impassable, and no road on it makes it passable" {
                // The trunk prices raw terrain, but "raw" is about the
                // *price* and never about what blocks: a rampart or a spawn
                // standing in the corridor is as impassable to a planned
                // road as a wall (ADR 0011). On (12, 9) and not on the
                // straight line through (12, 10): the flood's heap breaks
                // its ties towards the lower index, so it walks the y = 9
                // row of its own accord and an obstacle parked on (12, 10)
                // proves nothing. Roaded too, against the other start the
                // ticket floated — copying the *walking* grid and undoing
                // the road discount, which would hand a roaded obstacle its
                // terrain price back.
                let atlas =
                    corridor
                    |> withHome (fun layer ->
                        { layer with
                            Obstacles = Set.singleton { X = 12; Y = 9 }
                            Roads = Set.singleton { X = 12; Y = 9 }
                        })
                    |> snapshotWith []
                    |> ofView

                let path =
                    trunkPathHome
                        atlas
                        Set.empty
                        { X = 10; Y = 10 }
                        (Set.singleton { X = 14; Y = 10 })

                Expect.isFalse
                    (List.contains { X = 12; Y = 9 } path)
                    "the obstacle is never paved through"

                Expect.equal (List.last path) { X = 14; Y = 10 } "the goal is still reached"
                Expect.hasLength path 4 "the detour is a same-length diagonal"
            }

            test "the trunk is priced off the home room's ground and nothing else's" {
                // A trunk is a road the Layout plans, and the Layout plans
                // at home (ADR 0041) — so the grid the copy starts from is
                // chosen by `Home` and not by whichever room the projection
                // files first. The outpost sorts before home and walls off
                // every tile of home's corridor, so a copy off the wrong
                // room reaches nothing at all.
                let atlas =
                    { SpatialInfo.empty with
                        RoomName = Some "W2N2"
                    }
                    |> withHome (fun layer ->
                        { layer with
                            Terrain =
                                Map.ofList
                                    [
                                        yield { X = 10; Y = 10 }, Wall
                                        for x in 11..14 do
                                            yield { X = x; Y = 10 }, Plain
                                    ]
                        })
                    |> fun projection ->
                        { projection with
                            Rooms = Map.add "W1N1" RoomLayer.empty projection.Rooms
                        }
                    |> snapshotWith []
                    |> ofView

                Expect.equal
                    (trunkPathHome
                        atlas
                        Set.empty
                        { X = 10; Y = 10 }
                        (Set.singleton { X = 14; Y = 10 }))
                    [
                        { X = 11; Y = 10 }
                        { X = 12; Y = 10 }
                        { X = 13; Y = 10 }
                        { X = 14; Y = 10 }
                    ]
                    "home's corridor is paved, though the room filed first has no ground at all"
            }

            test "an avoided tile off the fifty-by-fifty marks nothing" {
                // The Layout hands its reservations in as bare tiles and the
                // grid index does no checking of its own (#173), so a tile
                // outside the room has to fall out before it is marked
                // rather than index off the end of the grid.
                let atlas = corridor |> snapshotWith [] |> ofView

                Expect.equal
                    (trunkPathHome
                        atlas
                        (Set.ofList [ { X = -1; Y = 10 }; { X = 50; Y = 10 }; { X = 12; Y = -1 } ])
                        { X = 10; Y = 10 }
                        (Set.singleton { X = 14; Y = 10 }))
                    (trunkPathHome
                        atlas
                        Set.empty
                        { X = 10; Y = 10 }
                        (Set.singleton { X = 14; Y = 10 }))
                    "an off-grid reservation reserves nothing and breaks nothing"
            }

            test "unreachable goals pave nothing" {
                let atlas = corridor |> snapshotWith [] |> ofView

                Expect.isEmpty
                    (trunkPathHome
                        atlas
                        Set.empty
                        { X = 10; Y = 10 }
                        (Set.singleton { X = 30; Y = 30 }))
                    "a goal outside the projection is unreachable"
            }

            test "of equally cheap goals the lowest (cost, tile) wins" {
                let atlas = corridor |> snapshotWith [] |> ofView

                let path =
                    trunkPathHome
                        atlas
                        Set.empty
                        { X = 10; Y = 10 }
                        (Set.ofList [ { X = 13; Y = 9 }; { X = 13; Y = 11 } ])

                Expect.equal (List.last path) { X = 13; Y = 9 } "ties break on the tile ordering"
            }
        ]

[<Tests>]
let haulRoundTripTests =
    testList
        "atlas haulRoundTripTicks"
        [
            // Both tiles are the colony's own room's, and these fixtures
            // file it under the empty name (`SpatialInfo.homeName`): since
            // #149 the two rooms ride on the API, because a `Pos` names
            // none (ADR 0041). Same room in and out, this is the flood the
            // rule always ran.
            let atHome atlas body from sink =
                haulRoundTripTicks atlas body (at "" from) (at "" sink)

            test "the loaded leg out and the empty leg back sum to whole ticks" {
                // Each leg is a walk (ADR 0029). [Carry;Carry;Move]
                // loaded on plain: two full Carry x weight 2 over one Move
                // is 4 units a step, ceil(4 / 2) = 2 ticks. Empty Carry
                // rides free, so the leg back sits on the one-tick floor.
                // Nine steps out at 2 and nine back at 1 = 27 ticks, with
                // nothing halved on the total.
                Expect.equal
                    (atHome
                        (corridorWith Set.empty [])
                        [ Carry; Carry; Move ]
                        { X = 10; Y = 10 }
                        { X = 20; Y = 10 })
                    (Some 27)
                    "both legs are priced by the body's own fatigue factor"
            }

            test "a road under the trunk discounts the loaded leg" {
                // Road weight 1 halves the loaded step to 2 units, one
                // tick; the empty leg already rides the floor. Nine steps
                // out and nine back at a tick apiece = 18.
                Expect.equal
                    (atHome
                        (corridorWith (Set.ofList [ for x in 11..19 -> { X = x; Y = 10 } ]) [])
                        [ Carry; Carry; Move ]
                        { X = 10; Y = 10 }
                        { X = 20; Y = 10 })
                    (Some 18)
                    "road parity is worth hiring for"
            }

            test "the pricing is traffic-blind: a standing creep never resizes the fleet" {
                // The quota is capacity planning, not routing: the same
                // corridor with a creep parked mid-lane prices identically
                // — no occupancy surcharge.
                Expect.equal
                    (atHome
                        (corridorWith Set.empty [ "w", { X = 15; Y = 10 } ])
                        [ Carry; Carry; Move ]
                        { X = 10; Y = 10 }
                        { X = 20; Y = 10 })
                    (Some 27)
                    "today's traffic is not tomorrow's throughput"
            }

            test "neither leg prices below the tiles it crosses" {
                // ADR 0029's floor, on this reader too: the round trip is
                // two walks, so it can never price below twice the
                // Chebyshev distance to the sink's nearest goal. The guard
                // that makes reintroducing the trailing halve-and-round-up
                // go red — under it the Move-surplus body below crossed
                // eighteen tiles in nine ticks.
                let floor = 2 * range { X = 10; Y = 10 } { X = 19; Y = 10 }

                for body in
                    [
                        [ Carry; Carry; Move ]
                        [ Carry; Carry; Carry; Carry; Move; Move ]
                        [ Carry; Move; Move; Move ]
                    ] do
                    match
                        atHome
                            (corridorWith Set.empty [])
                            body
                            { X = 10; Y = 10 }
                            { X = 20; Y = 10 }
                    with
                    | Some ticks ->
                        Expect.isGreaterThanOrEqual
                            ticks
                            floor
                            $"%A{body} rounds a nine-tile leg below nine ticks"
                    | None -> failtest $"%A{body} should reach the sink"
            }

            test "an unreachable sink prices no round trip" {
                let gapped =
                    spatial
                        [ "spawn-1", { X = 20; Y = 10 } ]
                        [
                            for x in 10..20 do
                                if x <> 15 then
                                    { X = x; Y = 10 }, Plain
                        ]
                    |> withObstacles [ { X = 20; Y = 10 } ]
                    |> snapshotWith []
                    |> ofView

                Expect.equal
                    (atHome gapped [ Carry; Carry; Move ] { X = 10; Y = 10 } { X = 20; Y = 10 })
                    None
                    "unpriceable geometry hires nobody"
            }
        ]

[<Tests>]
let castWalkTicksTests =
    testList
        "atlas castWalkTicks"
        [
            /// The Anchor row's shape empty: four Work and a Carry over one
            /// Move, so a plain step costs 8 units.
            let anchorBody = [ Work; Work; Work; Work; Carry; Move ]

            /// The room every goal below stands in. The goal's room is the
            /// caller's since #153 — a creep is led wherever it stands, and
            /// these cases lead one at home — and `spatial` builds from
            /// `SpatialInfo.empty`, which names no room, so the corridor is
            /// filed under the empty name (`SpatialInfo.homeName`).
            let home = SpatialInfo.homeName SpatialInfo.empty

            test "the walk is priced for the body given, not for any creep standing there" {
                // The lead's whole point (ADR 0026): an empty Anchor body
                // pays 8 units a plain step, ceil(8 / 2) = 4 ticks, so the
                // nine steps out of the spawner cost 36. A hauler unit over
                // the same ground carries no fatigue empty and rides the
                // walk's one-tick floor: 9.
                Expect.equal
                    (castWalkTicks
                        (corridorWith Set.empty [])
                        anchorBody
                        { X = 20; Y = 10 }
                        (at home { X = 10; Y = 10 }))
                    (Some 36)
                    "a slow body earns a long lead"

                Expect.equal
                    (castWalkTicks
                        (corridorWith Set.empty [])
                        [ Carry; Carry; Move ]
                        { X = 20; Y = 10 }
                        (at home { X = 10; Y = 10 }))
                    (Some 9)
                    "a hauler on the same ground earns a short one"
            }

            test "the walk starts beside the spawner, on the tile the engine places the body on" {
                // The step out of the spawner's own tile is one the
                // replacement never walks: the engine puts a finished creep
                // on a free neighbour. Charging it would buy the lead a
                // whole plain step — 4 ticks for this body — and cast the
                // successor that much too early to be admitted to the tile
                // it is walking to.
                Expect.equal
                    (castWalkTicks
                        (corridorWith Set.empty [])
                        anchorBody
                        { X = 20; Y = 10 }
                        (at home { X = 19; Y = 10 }))
                    (Some 0)
                    "a replacement is born beside the spawner, not moved there"
            }

            test "a road under the walk discounts it, as it discounts travel cost" {
                // Road weight 1 quarters the Anchor's step to 4 units —
                // 2 ticks — over the eight paved tiles it steps onto; the
                // last step onto the unpaved (10,10) still costs 8 units,
                // 4 ticks. 16 + 4 = 20.
                Expect.equal
                    (castWalkTicks
                        (corridorWith (Set.ofList [ for x in 11..19 -> { X = x; Y = 10 } ]) [])
                        anchorBody
                        { X = 20; Y = 10 }
                        (at home { X = 10; Y = 10 }))
                    (Some 20)
                    "the trunk shortens a succession"
            }

            test "the pricing is traffic-blind: today's crowd never moves a succession" {
                // A lead is planning, not routing — the same corridor with
                // a creep parked mid-lane prices identically, with no
                // occupancy surcharge.
                Expect.equal
                    (castWalkTicks
                        (corridorWith Set.empty [ "w", { X = 15; Y = 10 } ])
                        anchorBody
                        { X = 20; Y = 10 }
                        (at home { X = 10; Y = 10 }))
                    (Some 36)
                    "a standing creep is not a detour a replacement will still face"
            }

            test "the cast walk prices no tile below a tick" {
                // ADR 0029's floor, on the lead's reader too: the walk out
                // of the spawner starts beside it, so it can never price
                // below the Chebyshev distance from the birth tile (19,10)
                // to the goal. The guard that makes reintroducing the
                // trailing halving go red — under it a Move-surplus body
                // walked nine tiles in five ticks.
                let floor = range { X = 19; Y = 10 } { X = 10; Y = 10 }

                for body in [ anchorBody; [ Carry; Carry; Move ]; [ Work; Move; Move; Move ] ] do
                    match
                        castWalkTicks
                            (corridorWith Set.empty [])
                            body
                            { X = 20; Y = 10 }
                            (at home { X = 10; Y = 10 })
                    with
                    | Some ticks ->
                        Expect.isGreaterThanOrEqual
                            ticks
                            floor
                            $"%A{body} leads on a walk shorter than its tiles"
                    | None -> failtest $"%A{body} should reach the goal"
            }

            test "an unreachable tile prices no walk" {
                let gapped =
                    spatial
                        [ "spawn-1", { X = 20; Y = 10 } ]
                        [
                            for x in 10..20 do
                                if x <> 15 then
                                    { X = x; Y = 10 }, Plain
                        ]
                    |> withObstacles [ { X = 20; Y = 10 } ]
                    |> snapshotWith []
                    |> ofView

                Expect.equal
                    (castWalkTicks
                        gapped
                        [ Work; Carry; Move ]
                        { X = 20; Y = 10 }
                        (at home { X = 10; Y = 10 }))
                    None
                    "unpriceable geometry leads nobody"
            }

            test "a spawner with no free neighbour prices no walk" {
                // The other half of ADR 0004's totality: there is nowhere
                // for the replacement to be born, so the walk is
                // unpriceable and the row leads nobody.
                let walled =
                    spatial
                        [ "spawn-1", { X = 20; Y = 10 } ]
                        [ for x in 10..20 -> { X = x; Y = 10 }, Plain ]
                    |> withObstacles [ { X = 20; Y = 10 }; { X = 19; Y = 10 } ]
                    |> snapshotWith []
                    |> ofView

                Expect.equal
                    (castWalkTicks
                        walled
                        [ Work; Carry; Move ]
                        { X = 20; Y = 10 }
                        (at home { X = 10; Y = 10 }))
                    None
                    "a spawner that can place nothing leads nobody"
            }
        ]

[<Tests>]
let walkRecallTests =
    testList
        "atlas walk table recall"
        [
            // The castWalkTicks corridor: the spawn structure stands at
            // (20,10), so (19,10) is the one tile a body is born on, and
            // (10,10) lies nine steps further west.
            let corridorSnapshot () =
                spatial
                    [ "spawn-1", { X = 20; Y = 10 } ]
                    [ for x in 10..20 -> { X = x; Y = 10 }, Plain ]
                |> withObstacles [ { X = 20; Y = 10 } ]
                |> snapshotWith []

            let hauler = [ Carry; Carry; Move ]
            let spawnTile = { X = 20; Y = 10 }

            /// The corridor's own room, which is where this goal stands: the
            /// name `spatial` files an unnamed projection's layer under
            /// (`SpatialInfo.homeName`). The cross-border half of the same
            /// table is `crossRoomLeadTests`.
            let home = SpatialInfo.homeName SpatialInfo.empty
            let goal = { X = 10; Y = 10 }

            /// The one flood a corridor's lead pricing lays, read off the
            /// table by value: the array itself, so a table that was read
            /// rather than refilled is visible as the very array the first
            /// Atlas allocated.
            let flood (walks: WalkTable) =
                walks |> Seq.map (fun entry -> entry.Value) |> Seq.exactlyOne

            test "a table handed in is filled once and recalled by the next Atlas over it" {
                // ADR 0032: every input of this flood is in the census, so
                // an Atlas handed a filled table reads the entry instead of
                // running the Dijkstra a second time.
                let walks = WalkTable()
                let first = corridorSnapshot () |> ofViewRecalling walks

                Expect.equal
                    (castWalkTicks first hauler spawnTile (at home goal))
                    (Some 9)
                    "the first Atlas floods to price the lead"

                Expect.equal walks.Count 1 "and leaves the flood in the table it was handed"
                let flooded = flood walks

                let second = corridorSnapshot () |> ofViewRecalling walks

                Expect.equal
                    (castWalkTicks second hauler spawnTile (at home goal))
                    (Some 9)
                    "the recalled walk prices the same lead"

                Expect.equal walks.Count 1 "no second entry under the same key"

                Expect.isTrue
                    (obj.ReferenceEquals(flood walks, flooded))
                    "the second Atlas read the first's flood rather than running its own"
            }

            test "an Atlas with nothing to recall floods for itself" {
                // The other half of the seam: a table with nothing in it is
                // flooded into, so a caller that dropped its memo prices the
                // same lead off its own Dijkstra.
                let fresh = WalkTable()
                let atlas = corridorSnapshot () |> ofViewRecalling fresh

                Expect.equal
                    (castWalkTicks atlas hauler spawnTile (at home goal))
                    (Some 9)
                    "an empty table is flooded into, and prices the lead identically"

                Expect.equal fresh.Count 1 "the flood it ran is left in it"

                Expect.equal
                    (castWalkTicks (corridorSnapshot () |> ofView) hauler spawnTile (at home goal))
                    (Some 9)
                    "and the plain entry point, which lays its own table, agrees"
            }
        ]

[<Tests>]
let controllerContainerTests =
    testList
        "atlas controllerContainers"
        [
            test "the buffer is a built container in the Upgrade area off every Seat" {
                // Source at (10,10), controller at (14,10): "can-src" sits on
                // the Seat (11,10), "can-ctrl" at (13,10) inside the Upgrade
                // Work Area and on no Seat.
                let atlas =
                    { spatial
                          [
                              "src-a", { X = 10; Y = 10 }
                              "ctrl-1", { X = 14; Y = 10 }
                              "can-src", { X = 11; Y = 10 }
                              "can-ctrl", { X = 13; Y = 10 }
                          ]
                          [ for x in 11..13 -> { X = x; Y = 10 }, Plain ] with
                        TargetKinds =
                            Map.ofList
                                [
                                    "src-a", Source
                                    "ctrl-1", Controller
                                    "can-src", Structure BuiltKind.Container
                                    "can-ctrl", Structure BuiltKind.Container
                                ]
                    }
                    |> snapshotWith []
                    |> ofView

                Expect.equal
                    (controllerContainers atlas)
                    (Set.singleton "can-ctrl")
                    "the source container is intake, not buffer, however near the controller"
            }

            test "a container site, a far container and a controllerless room are no buffer" {
                let atlasWith kinds ctrlPos =
                    { spatial
                          ([ "can-ctrl", { X = 13; Y = 10 } ]
                           @ (ctrlPos |> Option.toList |> List.map (fun p -> "ctrl-1", p)))
                          [ for x in 11..20 -> { X = x; Y = 10 }, Plain ] with
                        TargetKinds = Map.ofList kinds
                    }
                    |> snapshotWith []
                    |> ofView

                Expect.equal
                    (controllerContainers (
                        atlasWith
                            [ "ctrl-1", Controller; "can-ctrl", Site BuiltKind.Container ]
                            (Some { X = 14; Y = 10 })
                    ))
                    Set.empty
                    "a pending container buffers nothing"

                Expect.equal
                    (controllerContainers (
                        atlasWith
                            [ "ctrl-1", Controller; "can-ctrl", Structure BuiltKind.Container ]
                            (Some { X = 20; Y = 10 })
                    ))
                    Set.empty
                    "out of the Upgrade Work Area, a container is nobody's buffer"

                Expect.equal
                    (controllerContainers (
                        atlasWith [ "can-ctrl", Structure BuiltKind.Container ] None
                    ))
                    Set.empty
                    "no controller, no buffer — the empty answer opens the gate (ADR 0004)"
            }
        ]
