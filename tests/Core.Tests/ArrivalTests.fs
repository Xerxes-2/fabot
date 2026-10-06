/// Arrival time (`Arrival`): the hostile maps a room's floods lay, and
/// one of our bodies' own, timed by fatigue and distance alone.
module Fabot.Core.Tests.ArrivalTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Tests.AtlasFixtures

/// The room every case reads, and the room west of it: W13S28's east exit
/// lands on W12S28's west column.
let private home = "W12S28"
let private west = "W13S28"

/// Every exit tile of a room, plain.
let private plainRing =
    [
        for along in 1..48 do
            yield { X = along; Y = 0 }, Plain
            yield { X = along; Y = 49 }, Plain
            yield { X = 0; Y = along }, Plain
            yield { X = 49; Y = along }, Plain
    ]

/// Both rooms plain, rings and ground, with `home` dressed by `dress`.
let private rooms (dress: RoomLayer -> RoomLayer) =
    let spatial = bordered [ home, plainRing; west, plainRing ]

    { spatial with
        RoomName = Some home
        Rooms = spatial.Rooms |> Map.add home (dress spatial.Rooms.[home])
    }

/// `home`'s ground with these tiles swamp.
let private swampAt (tiles: Pos list) (layer: RoomLayer) =
    { layer with
        Terrain =
            tiles
            |> List.fold (fun grid tile -> TerrainGrid.add tile Swamp grid) layer.Terrain
    }

/// A wall down column `x` of the ground, but for row `gap`.
let private wallDown (x: int) (gap: int option) (terrain: TerrainGrid) =
    [ 1..48 ]
    |> List.filter (fun y -> Some y <> gap)
    |> List.fold (fun grid y -> TerrainGrid.add { X = x; Y = y } Wall grid) terrain

let private atlasOf creeps spatial =
    spatial |> snapshotWith creeps |> Atlas.ofView

/// A hostile of Trep's (an Invader's where the owner says so).
let private hostile owner id room (x, y) body =
    {
        Id = id
        Owner = owner
        Pos = RoomPos.at room { X = x; Y = y }
        Body = body
        Boosts = []
        Hits = Engine.partHits * List.length body
        TicksToLive = 1000
    }

let private trep = hostile "Trep"

/// A world that sees each room with these hostiles, at these fatigues.
let private seeing (rooms: (string * HostileInfo list * (string * int) list) list) =
    { WorldView.empty with
        Rooms =
            rooms
            |> List.map (fun (room, hostiles, fatigue) ->
                room,
                {
                    Hostiles = hostiles
                    HostileFatigue = Map.ofList fatigue
                    InvaderCores = []
                    Towers = []
                    Fallen = []
                    Control =
                        {
                            Owner = Ownership.Unowned
                            Reservation = None
                            SafeMode = false
                            SafeModeCooldownUntil = 0
                            Sign = None
                        }
                })
            |> Map.ofList
    }

/// The one class map of a strike range, read at a tile.
let private strikeAt (maps: ArrivalMaps) (strike: int) (x, y) =
    maps.Classes
    |> List.filter (fun map -> map.Class.Strike = strike)
    |> List.exactlyOne
    |> fun map -> Arrival.ticksAt map.Ticks { X = x; Y = y }

let private melee = Engine.meleeRange
let private ranged = Engine.rangedRange

[<Tests>]
let arrivalTests =
    testList
        "arrival"
        [
            test "a 1:1 melee stands on swamp the tick after its step, and leaves it five ticks on" {
                // A swamp column at x = 12 between the melee at (10,25) and
                // the east of the room.
                let atlas = rooms (swampAt [ for y in 1..48 -> { X = 12; Y = y } ]) |> atlasOf []

                let world = seeing [ home, [ trep "m" home (10, 25) [ Attack; Move ] ], [] ]
                let maps = Arrival.inRoom atlas world home

                Expect.equal
                    ([ 11; 12; 13; 14 ] |> List.map (fun x -> strikeAt maps melee (x, 25)))
                    [ Some 0; Some 1; Some 2; Some 7 ]
                    "its swing now, a step on, onto the swamp, and off it five ticks later"
            }

            test "a shooter's map is its walk dilated by its range, and a hybrid is in both classes" {
                let atlas = rooms (swampAt [ for y in 1..48 -> { X = 12; Y = y } ]) |> atlasOf []

                let world =
                    seeing
                        [
                            home,
                            [ trep "h" home (10, 25) [ Attack; RangedAttack; Move; Move ] ],
                            []
                        ]

                let maps = Arrival.inRoom atlas world home

                Expect.equal
                    ([ 13; 14; 15; 16 ] |> List.map (fun x -> strikeAt maps ranged (x, 25)))
                    [ Some 0; Some 1; Some 2; Some 7 ]
                    "three further out than the swing, tick for tick"

                Expect.equal (strikeAt maps melee (13, 25)) (Some 2) "and its swing has its own map"
            }

            test "hostiles walk our roads: a body two ticks a plain tile takes one a paved one" {
                let world = seeing [ home, [ trep "m" home (10, 25) [ Attack; Attack; Move ] ], [] ]

                let paved =
                    rooms (fun layer ->
                        { layer with
                            Roads = Set.ofList [ for x in 11..20 -> { X = x; Y = 25 } ]
                        })
                    |> atlasOf []

                let bare = rooms id |> atlasOf []

                Expect.equal
                    (strikeAt (Arrival.inRoom paved world home) melee (21, 25))
                    (Some 10)
                    "ten road steps"

                Expect.equal
                    (strikeAt (Arrival.inRoom bare world home) melee (21, 25))
                    (Some 19)
                    "one tick to step, two a plain tile after"
            }

            test "a fatigued hostile waits its fatigue out before its first step" {
                let atlas = rooms id |> atlasOf []
                let body = [ Attack; Move ]

                let tired =
                    Arrival.inRoom
                        atlas
                        (seeing [ home, [ trep "m" home (10, 25) body ], [ "m", 4 ] ])
                        home

                let fresh =
                    Arrival.inRoom atlas (seeing [ home, [ trep "m" home (10, 25) body ], [] ]) home

                Expect.equal (strikeAt tired melee (11, 25)) (Some 0) "it swings where it stands"

                Expect.equal
                    (strikeAt tired melee (12, 25))
                    (Some 3)
                    "two ticks of fatigue, then the step"

                Expect.equal (strikeAt fresh melee (12, 25)) (Some 1) "a step away when rested"
            }

            test "a hostile never walks through a private rampart of ours, and does a public one" {
                // A wall down x = 12 but for (12,25), where our rampart stands.
                let walled isPublic =
                    let spatial =
                        rooms (fun layer ->
                            { layer with
                                Terrain =
                                    [
                                        for y in 1..48 do
                                            if y <> 25 then
                                                yield { X = 12; Y = y }
                                    ]
                                    |> List.fold
                                        (fun grid tile -> TerrainGrid.add tile Wall grid)
                                        layer.Terrain
                                TargetPositions = Map.ofList [ "rampart-1", { X = 12; Y = 25 } ]
                            })

                    { spatial with
                        TargetKinds = Map.ofList [ "rampart-1", Structure BuiltKind.Rampart ]
                        Hits = Map.ofList [ "rampart-1", { Hits = 1000; HitsMax = 1000 } ]
                        PublicRamparts = if isPublic then Set.singleton "rampart-1" else Set.empty
                    }
                    |> atlasOf []

                let world = seeing [ home, [ trep "m" home (10, 25) [ Attack; Move ] ], [] ]

                Expect.equal
                    (strikeAt (Arrival.inRoom (walled false) world home) melee (14, 25))
                    None
                    "walled out"

                Expect.equal
                    (strikeAt (Arrival.inRoom (walled true) world home) melee (14, 25))
                    (Some 3)
                    "through the open rampart, a tile a tick"
            }

            test "a construction site of ours walls only us: a hostile walks over it" {
                // The wall's gap at (12,25) holds our extension site, an
                // obstacle in the walking grid.
                let atlas =
                    let spatial =
                        rooms (fun layer ->
                            { layer with
                                Terrain = wallDown 12 (Some 25) layer.Terrain
                                TargetPositions = Map.ofList [ "site-1", { X = 12; Y = 25 } ]
                                Obstacles = Set.singleton { X = 12; Y = 25 }
                            })

                    { spatial with
                        TargetKinds = Map.ofList [ "site-1", Site BuiltKind.Extension ]
                    }
                    |> atlasOf []

                let world = seeing [ home, [ trep "m" home (10, 25) [ Attack; Move ] ], [] ]

                Expect.equal
                    (strikeAt (Arrival.inRoom atlas world home) melee (14, 25))
                    (Some 3)
                    "over the site"
            }

            test "a road through a wall is a tunnel, walked at a road's pace" {
                let atlas =
                    rooms (fun layer ->
                        { layer with
                            Terrain = wallDown 12 None layer.Terrain
                            Roads = Set.singleton { X = 12; Y = 25 }
                        })
                    |> atlasOf []

                let world = seeing [ home, [ trep "m" home (10, 25) [ Attack; Move ] ], [] ]

                Expect.equal
                    (strikeAt (Arrival.inRoom atlas world home) melee (14, 25))
                    (Some 3)
                    "through the tunnel"
            }

            test
                "an Invader, which no exit carries off, walks the ring round a wall; a player's body cannot" {
                // The wall runs the room's whole height: only the exit rows
                // pass it.
                let atlas =
                    rooms (fun layer ->
                        { layer with
                            Terrain = wallDown 12 None layer.Terrain
                        })
                    |> atlasOf []

                let mapsOf owner =
                    Arrival.inRoom
                        atlas
                        (seeing [ home, [ hostile owner "m" home (10, 25) [ Attack; Move ] ], [] ])
                        home

                Expect.isSome (strikeAt (mapsOf "Invader") melee (14, 25)) "round by the exit row"

                Expect.isNone
                    (strikeAt (mapsOf "Trep") melee (14, 25))
                    "carried off at the first exit"
            }

            test "a body with no MOVE never arrives: it strikes its weapon's range and no further" {
                let atlas = rooms id |> atlasOf []

                let maps =
                    Arrival.inRoom
                        atlas
                        (seeing [ home, [ trep "m" home (10, 25) [ Attack ] ], [] ])
                        home

                Expect.equal (strikeAt maps melee (11, 25)) (Some 0) "beside it"
                Expect.equal (strikeAt maps melee (12, 25)) None "never past it"
            }

            test
                "a player's hostile next door lands on our exits the tick it stands on its own, an Invader never" {
                let atlas = rooms id |> atlasOf []

                // Two steps from W13S28's east exit: on (49,25) at tick 2,
                // which puts it on our (0,25) at tick 2.
                let player =
                    Arrival.inRoom
                        atlas
                        (seeing [ west, [ trep "m" west (47, 25) [ Attack; Move ] ], [] ])
                        home

                Expect.equal
                    ([ 1; 2 ] |> List.map (fun x -> strikeAt player melee (x, 25)))
                    [ Some 2; Some 3 ]
                    "struck from the landing at tick 2, a step in at tick 3"

                let invader =
                    Arrival.inRoom
                        atlas
                        (seeing
                            [ west, [ hostile "Invader" "i" west (47, 25) [ Attack; Move ] ], [] ])
                        home

                Expect.isEmpty invader.Classes "an Invader stays in its room"
            }

            test
                "a step onto an exit costs no fatigue, so a slow body leaves its landing the tick it lands" {
                // 14A1M, fourteen ticks a plain tile, beside W13S28's east
                // exit: onto it and across at tick 0, on our (0,25) at 1,
                // on (1,25) at 2, which strikes (2,25).
                let atlas = rooms id |> atlasOf []

                let maps =
                    Arrival.inRoom
                        atlas
                        (seeing
                            [
                                west,
                                [ trep "m" west (48, 25) (List.replicate 14 Attack @ [ Move ]) ],
                                []
                            ])
                        home

                Expect.equal
                    (strikeAt maps melee (2, 25))
                    (Some 2)
                    "not after fourteen ticks on the landing"
            }

            test "a swamp exit and a swamp landing hold a body no longer than plain ones" {
                let swampColumn x =
                    Map.map (fun (tile: Pos) terrain -> if tile.X = x then Swamp else terrain)

                let atlas =
                    let spatial = rooms id

                    { spatial with
                        Borders =
                            spatial.Borders
                            |> Map.add west (swampColumn 49 spatial.Borders.[west])
                            |> Map.add home (swampColumn 0 spatial.Borders.[home])
                    }
                    |> atlasOf []

                let maps =
                    Arrival.inRoom
                        atlas
                        (seeing [ west, [ trep "m" west (48, 25) [ Attack; Move ] ], [] ])
                        home

                Expect.equal (strikeAt maps melee (2, 25)) (Some 2) "as over plain"
            }

            test
                "an unseen neighbour of remembered danger keeps its landings risky until it is seen" {
                let atlas = rooms id |> atlasOf []

                let dark =
                    { WorldView.empty with
                        Danger = Set.singleton west
                    }

                let maps = Arrival.inRoom atlas dark home

                Expect.equal
                    ([ 3; 4 ] |> List.map (fun x -> Arrival.soonestAt maps { X = x; Y = 25 }))
                    [ Some 0; Some 1 ]
                    "shot from the landing now, a tile a tick in from it"

                let seen =
                    { seeing [ west, [], [] ] with
                        Danger = Set.singleton west
                    }

                Expect.isNone (Arrival.inRoom atlas seen home).Unseen "seen again, and empty"
            }

            test "one of our bodies stands on a tile by its gait, after the fatigue it carries" {
                let creep =
                    { creepWith "r" 0 [ RangedAttack; Move ] with
                        Fatigue = 4
                    }

                let atlas =
                    rooms (fun layer ->
                        { layer with
                            CreepPositions = Map.ofList [ "r", { X = 10; Y = 25 } ]
                        })
                    |> atlasOf [ creep ]

                let body = Arrival.ofBody atlas creep |> Option.get

                Expect.equal body.Room home "its own room"

                Expect.equal
                    ([ 10; 11; 12 ]
                     |> List.map (fun x -> Arrival.ticksAt body.Ticks { X = x; Y = 25 }))
                    [ Some 0; Some 3; Some 4 ]
                    "two ticks of fatigue, then a tile a tick"
            }

            test "two reads of a room's maps under the world's table are one build" {
                let atlas = rooms id |> atlasOf []

                let world =
                    seeing [ home, [ trep "m" home (10, 25) [ Attack; Move ] ], [] ]
                    |> WorldView.sharingSafeGround

                Expect.isTrue
                    (obj.ReferenceEquals(
                        Arrival.inRoom atlas world home,
                        Arrival.inRoom atlas world home
                    ))
                    "the second read is the first's maps"
            }
        ]
