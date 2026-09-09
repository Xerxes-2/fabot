/// What the projection places and what the Atlas answers about it.
module Fabot.Core.Tests.AtlasPlacementTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Atlas
open Fabot.Core.Tests.AtlasFixtures

[<Tests>]
let placementQueryTests =
    testList
        "atlas placement queries"
        [
            test "homeRoom passes the projection's room through, absent when empty" {
                let named =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                    }
                    |> snapshotWith []
                    |> ofView

                Expect.equal (homeRoom named) (Some "W1N1") "the projection names its room"

                let bare = SpatialInfo.empty |> snapshotWith [] |> ofView
                Expect.equal (homeRoom bare) None "an empty projection covers no room"
            }

            test "positionOf finds a projected target and misses an absent one" {
                let atlas =
                    spatial [ "spawn-1", { X = 25; Y = 25 } ] [] |> snapshotWith [] |> ofView

                Expect.equal
                    (positionOf atlas "spawn-1")
                    (Some(at "" { X = 25; Y = 25 }))
                    "a projected target has a tile"

                Expect.equal (positionOf atlas "ghost") None "an unprojected target has none"
            }

            test "buildableTiles excludes walls and every target's tile, in (X, Y) order" {
                // Plain and swamp qualify; the wall, the structure's tile and
                // the site's tile do not; a creep does not block placement.
                let atlas =
                    { spatial
                          [ "ext-1", { X = 10; Y = 11 }; "site-1", { X = 11; Y = 10 } ]
                          [
                              { X = 10; Y = 10 }, Plain
                              { X = 10; Y = 11 }, Plain
                              { X = 11; Y = 10 }, Plain
                              { X = 11; Y = 11 }, Swamp
                              { X = 12; Y = 10 }, Wall
                          ] with
                        TargetKinds =
                            Map.ofList
                                [
                                    "ext-1", Structure BuiltKind.Extension
                                    "site-1", Site BuiltKind.Extension
                                ]
                    }
                    |> withCreepsAt [ "w", { X = 10; Y = 10 } ]
                    |> snapshotWith [ worker "w" ]
                    |> ofView

                Expect.equal
                    (buildableTilesIn atlas (atlasHome atlas))
                    [ { X = 10; Y = 10 }; { X = 11; Y = 11 } ]
                    "free plain and swamp tiles only, sorted by (X, Y)"
            }

            test "buildableTiles orders by X before Y, not by Y before X" {
                // The (X, Y) order is this query's own contract — the key
                // order the terrain layer's `Map<Pos, _>` gave before #177,
                // which the grid's `x * roomSide + y` index reproduces. No
                // Layout consumer leans on it: `planLayout`'s ordering
                // re-sorts on `(range, X, Y)` and the footing candidates go
                // through a set, so ADR 0011's determinism downstream is
                // carried by those and this pins the contract itself. Two
                // tiles the two orders disagree about, which the
                // neighbouring pairs above cannot tell apart.
                let atlas =
                    spatial [] [ { X = 11; Y = 9 }, Plain; { X = 10; Y = 12 }, Plain ]
                    |> snapshotWith []
                    |> ofView

                Expect.equal
                    (buildableTilesIn atlas (atlasHome atlas))
                    [ { X = 10; Y = 12 }; { X = 11; Y = 9 } ]
                    "the lower X comes first though its Y is higher"
            }

            test "buildableTiles scans the home room's ground and nothing else's" {
                // The Layout builds where it is anchored (ADR 0041), and a
                // grid is chosen by room name before any tile is read, so
                // the name has to be `Home` and not whichever room the
                // projection files first. The outpost is named to sort
                // before home and offers tiles home has not got.
                let atlas =
                    { SpatialInfo.empty with
                        RoomName = Some "W2N2"
                    }
                    |> withHome (fun layer ->
                        { layer with
                            Terrain =
                                Map.ofList [ { X = 10; Y = 10 }, Plain; { X = 10; Y = 11 }, Swamp ]
                        })
                    |> fun projection ->
                        { projection with
                            Rooms =
                                Map.add
                                    "W1N1"
                                    { RoomLayer.empty with
                                        Terrain = Map.ofList [ { X = 20; Y = 20 }, Plain ]
                                    }
                                    projection.Rooms
                        }
                    |> snapshotWith []
                    |> ofView

                Expect.equal
                    (buildableTilesIn atlas (atlasHome atlas))
                    [ { X = 10; Y = 10 }; { X = 10; Y = 11 } ]
                    "home's two tiles, and never the other room's coordinate"
            }

            test "isSwamp reads the home room's ground and nothing else's" {
                // The Layout's road plan asks it per tile of the Upgrade
                // Work Area; every answer that is not "swamp here" is one
                // answer (ADR 0004). A bare `Pos` names no room (ADR 0041),
                // so the room has to come from `Home` and not from whichever
                // room the projection happens to file first: the outpost
                // here is named to sort *before* home and contradicts it on
                // both tiles the two share.
                let atlas =
                    { SpatialInfo.empty with
                        RoomName = Some "W2N2"
                    }
                    |> withHome (fun layer ->
                        { layer with
                            Terrain =
                                Map.ofList
                                    [
                                        { X = 10; Y = 10 }, Swamp
                                        { X = 10; Y = 11 }, Plain
                                        { X = 10; Y = 12 }, Wall
                                    ]
                        })
                    |> fun projection ->
                        { projection with
                            Rooms =
                                Map.add
                                    "W1N1"
                                    { RoomLayer.empty with
                                        Terrain =
                                            Map.ofList
                                                [
                                                    { X = 10; Y = 10 }, Plain
                                                    { X = 10; Y = 11 }, Swamp
                                                    { X = 30; Y = 30 }, Swamp
                                                ]
                                    }
                                    projection.Rooms
                        }
                    |> snapshotWith []
                    |> ofView

                Expect.isTrue
                    (isSwampIn atlas (atlasHome atlas) { X = 10; Y = 10 })
                    "swamp terrain is swamp, though the other room calls the coordinate plain"

                Expect.isFalse
                    (isSwampIn atlas (atlasHome atlas) { X = 10; Y = 11 })
                    "plain is not, though the other room calls the coordinate swamp"

                Expect.isFalse (isSwampIn atlas (atlasHome atlas) { X = 10; Y = 12 }) "wall is not"

                Expect.isFalse
                    (isSwampIn atlas (atlasHome atlas) { X = 30; Y = 30 })
                    "a tile home's layer does not carry is not swamp, whatever the outpost's is"

                Expect.isFalse
                    (isSwampIn atlas (atlasHome atlas) { X = -1; Y = 10 })
                    "nor is a tile off the fifty-by-fifty"
            }

            test "a swamp under a road is still swamp: isSwamp reads terrain, not the walking price" {
                // The road pass discounts the walking grid to 1; the Layout
                // plans its swamp roads off the ground under them, so a
                // paved swamp must still read as swamp or the plan would
                // stop maintaining the road it just built (ADR 0011).
                let atlas =
                    spatial [] [ { X = 10; Y = 10 }, Swamp ]
                    |> withRoads [ { X = 10; Y = 10 } ]
                    |> snapshotWith []
                    |> ofView

                Expect.isTrue
                    (isSwampIn atlas (atlasHome atlas) { X = 10; Y = 10 })
                    "the road does not pave the ground away"
            }

            test
                "droppedEnergyIn lists a room's placed piles in id order; buildableTiles ignores them" {
                // A pile is a target the reflex reads, not a thing standing
                // on the tile: it never keeps a construction site off it.
                /// The room this funnel files its geometry under: the
                /// projection names none, so it is filed under the empty
                /// name (`SpatialInfo.homeName`).
                let home = SpatialInfo.homeName SpatialInfo.empty

                let atlas =
                    { spatial
                          [ "pile-b", { X = 10; Y = 11 }; "pile-a", { X = 10; Y = 10 } ]
                          [ { X = 10; Y = 10 }, Plain; { X = 10; Y = 11 }, Plain ] with
                        TargetKinds = Map.ofList [ "pile-a", Dropped; "pile-b", Dropped ]
                    }
                    |> snapshotWith []
                    |> ofView

                Expect.equal
                    (droppedEnergyIn atlas home)
                    [ "pile-a", at home { X = 10; Y = 10 }; "pile-b", at home { X = 10; Y = 11 } ]
                    "both piles placed, id order"

                Expect.isEmpty
                    (droppedEnergyIn atlas "W9N9")
                    "a room the projection does not carry places no pile (ADR 0004)"

                Expect.equal
                    (buildableTilesIn atlas (atlasHome atlas))
                    [ { X = 10; Y = 10 }; { X = 10; Y = 11 } ]
                    "pile tiles stay buildable"
            }

            test "a standing link is a built kind: its tile is censused and no longer buildable" {
                // Link is a projection kind with no placeable counterpart
                // (ADR 0022): the Layout never asks for one, it only needs
                // to see the ones that stand, so a link on a footing does
                // not send the footing looking for another tile.
                let atlas =
                    { spatial
                          [ "link-1", { X = 10; Y = 10 }; "sto-1", { X = 10; Y = 11 } ]
                          [ { X = 10; Y = 10 }, Plain; { X = 10; Y = 11 }, Plain ] with
                        TargetKinds =
                            Map.ofList
                                [
                                    "link-1", Structure BuiltKind.Link
                                    "sto-1", Structure BuiltKind.Storage
                                ]
                    }
                    |> snapshotWith []
                    |> ofView

                Expect.equal
                    (linkTilesIn atlas (atlasHome atlas))
                    (Set.singleton { X = 10; Y = 10 })
                    "the link's tile, and no other kind's"

                Expect.equal
                    (storageTilesIn atlas (atlasHome atlas))
                    (Set.singleton { X = 10; Y = 11 })
                    "the Storage's tile, the anchor its own footing is read from"

                Expect.isEmpty
                    (buildableTilesIn atlas (atlasHome atlas))
                    "both stand on their tiles: neither takes a site"
            }

            test
                "a placed container is a target, not an obstacle: repairable in place, unbuildable under" {
                // Container at (10,10) on a fully projected 7x7 plain square,
                // carrying hits and store as the projection now does.
                let tiles =
                    [
                        for x in 7..13 do
                            for y in 7..13 -> { X = x; Y = y }, Plain
                    ]

                let atlas =
                    { spatial [ "cont-1", { X = 10; Y = 10 } ] tiles with
                        TargetKinds = Map.ofList [ "cont-1", Structure BuiltKind.Container ]
                        Hits = Map.ofList [ "cont-1", { Hits = 100; HitsMax = 250000 } ]
                        Stores = Map.ofList [ "cont-1", 800 ]
                    }
                    |> withCreepsAt [ "w", { X = 7; Y = 7 } ]
                    |> snapshotWith [ worker "w" ]
                    |> ofView

                let area = workArea atlas (Repair "cont-1")

                Expect.hasLength
                    area
                    49
                    "the full range-3 square stands: a container blocks no tile, its own included"

                Expect.equal
                    (travelCost atlas "w" (Repair "cont-1"))
                    (Some 0)
                    "the corner creep already stands inside the Work Area"

                Expect.isFalse
                    (List.contains { X = 10; Y = 10 } (buildableTilesIn atlas (atlasHome atlas)))
                    "the container's tile takes no construction site"
            }

            test "an unplaced container gets the documented answers: empty area, free pricing" {
                // Hits arrive without a position — unpriceable geometry never
                // counts against a Task (ADR 0004).
                let atlas =
                    { spatial [] [ { X = 10; Y = 10 }, Plain ] with
                        TargetKinds = Map.ofList [ "cont-1", Structure BuiltKind.Container ]
                        Hits = Map.ofList [ "cont-1", { Hits = 100; HitsMax = 250000 } ]
                    }
                    |> withCreepsAt [ "w", { X = 10; Y = 10 } ]
                    |> snapshotWith [ worker "w" ]
                    |> ofView

                Expect.equal
                    (workArea atlas (Repair "cont-1") |> tilesHome atlas)
                    Set.empty
                    "nowhere to stand"

                Expect.equal
                    (travelCost atlas "w" (Repair "cont-1"))
                    (Some 0)
                    "an unplaced target prices at 0, never against the Task"

                Expect.isTrue
                    (mayActFor atlas "w" (Repair "cont-1"))
                    "an unplaced target never blocks the action"
            }

            test "extension censuses count exactly the built and pending extensions" {
                let atlas =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                        TargetKinds =
                            Map.ofList
                                [
                                    "spawn-1", Structure BuiltKind.Spawn
                                    "ext-1", Structure BuiltKind.Extension
                                    "ext-2", Structure BuiltKind.Extension
                                    "road-1", Structure BuiltKind.Other
                                    "site-1", Site BuiltKind.Extension
                                    "site-2", Site BuiltKind.Other
                                    "src-a", Source
                                    "ctrl-1", Controller
                                ]
                    }
                    |> withHome (fun layer ->
                        { layer with
                            TargetPositions =
                                Map.ofList
                                    [
                                        "spawn-1", { X = 20; Y = 20 }
                                        "ext-1", { X = 21; Y = 20 }
                                        "ext-2", { X = 22; Y = 20 }
                                        "road-1", { X = 23; Y = 20 }
                                        "site-1", { X = 24; Y = 20 }
                                        "site-2", { X = 25; Y = 20 }
                                        "src-a", { X = 26; Y = 20 }
                                        "ctrl-1", { X = 27; Y = 20 }
                                    ]
                        })
                    |> snapshotWith []
                    |> ofView

                Expect.equal
                    (builtIn atlas "W1N1" BuiltKind.Extension)
                    2
                    "only standing extensions are built"

                Expect.equal
                    (pendingIn atlas "W1N1" BuiltKind.Extension)
                    1
                    "only sites that will become extensions are pending"
            }

            test "the kind censuses count one room's own structures and not a neighbour's (#140)" {
                // The Layout's gap rule is `allowed at RCL − built −
                // pending`, and the allowance is a fact about *this*
                // room's controller — so the census subtracted from it has
                // to be this room's. Until #216 R3 the six counts read the
                // flat, id-keyed kind census and answered for every room
                // the projection carried, which ADR 0052 decision 7's
                // borrowing made reachable: a mother carries a
                // bootstrapping child's construction sites so her workers
                // may build them (`ColonyView.borrowed` keeps every
                // `Site _`), and the child's extension sites came off her
                // own allowance.
                //
                // Pairwise on the room the site is filed under, with the
                // kind census flat and identical either way — that is the
                // half ADR 0041 leaves unlayered, and the join to a named
                // room's positions is what separates them.
                let kinds =
                    Map.ofList
                        [
                            "ext-home", Structure BuiltKind.Extension
                            "site-home", Site BuiltKind.Extension
                            "ext-child", Structure BuiltKind.Extension
                            "site-child", Site BuiltKind.Extension
                            "tower-child", Structure BuiltKind.Tower
                            "tower-site-child", Site BuiltKind.Tower
                            "storage-child", Structure BuiltKind.Storage
                            "storage-site-child", Site BuiltKind.Storage
                        ]

                let child =
                    { RoomLayer.empty with
                        TargetPositions =
                            Map.ofList
                                [
                                    "ext-child", { X = 30; Y = 30 }
                                    "site-child", { X = 31; Y = 30 }
                                    "tower-child", { X = 32; Y = 30 }
                                    "tower-site-child", { X = 33; Y = 30 }
                                    "storage-child", { X = 34; Y = 30 }
                                    "storage-site-child", { X = 35; Y = 30 }
                                ]
                    }

                let atlas =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                        TargetKinds = kinds
                        Rooms = Map.ofList [ "W2N1", child ]
                    }
                    |> withHome (fun layer ->
                        { layer with
                            TargetPositions =
                                Map.ofList
                                    [
                                        "ext-home", { X = 20; Y = 20 }
                                        "site-home", { X = 21; Y = 20 }
                                    ]
                        })
                    |> snapshotWith []
                    |> ofView

                Expect.equal
                    (builtIn atlas "W1N1" BuiltKind.Extension,
                     pendingIn atlas "W1N1" BuiltKind.Extension)
                    (1, 1)
                    "home counts its own extension and its own site, and neither of the child's"

                Expect.equal
                    (builtIn atlas "W2N1" BuiltKind.Extension,
                     pendingIn atlas "W2N1" BuiltKind.Extension)
                    (1, 1)
                    "and the child's counts are the child's, off the layer they are filed in"

                Expect.equal
                    (builtIn atlas "W1N1" BuiltKind.Tower, pendingIn atlas "W1N1" BuiltKind.Tower)
                    (0, 0)
                    "the tower census is the named room's too"

                Expect.equal
                    (builtIn atlas "W1N1" BuiltKind.Storage,
                     pendingIn atlas "W1N1" BuiltKind.Storage)
                    (0, 0)
                    "and so is the Storage's"
            }
        ]
