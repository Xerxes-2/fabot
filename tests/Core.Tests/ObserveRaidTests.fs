/// The Raid log's episode fold: the windows it opens and closes, the roster
/// it unions, the closest approach it measures, the losses it keeps that the
/// Transition log has already pruned, and the damage it charges.
module Fabot.Core.Tests.ObserveRaidTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Observe

[<Tests>]
let episodeTests =
    testList
        "raid fold: episodes"
        [
            test "a colony nobody is raiding records nothing" {
                let state = RaidState.empty |> raidTick 10 quiet |> raidTick 11 quiet

                // Bar the dig latch, which every decided tick writes.
                Expect.equal
                    state
                    { RaidState.empty with
                        DigOpen = Some Set.empty
                    }
                    "a tick with no hostile and no open episode leaves the log as it found it"
            }

            test "the first hostile opens an episode and the ticks that follow extend it" {
                let state =
                    RaidState.empty
                    |> raidTick 10 (raid squad)
                    |> raidTick 11 (raid squad)
                    |> raidTick 12 (raid squad)

                Expect.equal
                    (windows state)
                    [ 10, 12 ]
                    "one episode, opened once and carried to the last tick a hostile stood there"
            }

            test "a squad that steps out and back inside the quiet gap is one episode" {
                // The same creeps re-entering: t15 is exactly five ticks after the last
                // sighting, so the return is still the raid that is already open.
                let state =
                    (RaidState.empty, [ 10..15 ])
                    ||> List.fold (fun state t ->
                        state |> raidTick t (if t = 10 || t = 15 then raid squad else quiet))

                Expect.equal
                    (windows state)
                    [ 10, 15 ]
                    "a re-entry inside the gap extends the raid rather than opening a second"
            }

            test "a return after the gap has elapsed opens a second episode" {
                let state =
                    (RaidState.empty, [ 10..16 ])
                    ||> List.fold (fun state t ->
                        state |> raidTick t (if t = 10 || t = 16 then raid squad else quiet))

                Expect.equal
                    (windows state)
                    [ (10, 10); (16, 16) ]
                    "a gap wider than the quiet gap is a departure, and the next visit is a new raid"
            }

            test "the quiet gap is the colony's tunable: the same six ticks are one raid or two" {
                // `Tuning.QuietGap`, pairwise over the one field on one history: a squad
                // seen at t10 and again at t16. At a five-tick gap the second sighting
                // opens a second episode; at a seven-tick gap it is the raid still open.
                let episodes gap =
                    (RaidState.empty, [ 10..16 ])
                    ||> List.fold (fun state t ->
                        let colony = if t = 10 || t = 16 then raid squad else quiet

                        let seen =
                            { colony with
                                Time = t
                                Tuning = { colony.Tuning with QuietGap = gap }
                            }

                        foldRaids
                            3
                            (colony.Creeps |> List.map (fun c -> c.Name) |> Set.ofList)
                            seen
                            (Fabot.Core.Decide.Planner.outpostFactsOf
                                seen
                                (Fabot.Core.Atlas.ofView seen))
                                .Declared
                            state)
                    |> windows

                Expect.equal
                    (episodes 5)
                    [ (10, 10); (16, 16) ]
                    "five ticks of silence is a departure and the return is a second raid"

                Expect.equal
                    (episodes 7)
                    [ 10, 16 ]
                    "seven, and the very same history is one raid the squad never left"
            }

            test "the ring keeps the newest episodes and drops the oldest" {
                // Four raids, each well clear of the quiet gap; the cap is three.
                let state =
                    (RaidState.empty, [ 10; 20; 30; 40 ])
                    ||> List.fold (fun state t -> state |> raidTick t (raid squad))

                Expect.equal
                    (windows state)
                    [ (20, 20); (30, 30); (40, 40) ]
                    "only the newest cap-many raids survive"
            }

            test "an empty log folds like a fresh boot" {
                // What a discarded subtree costs: the episodes it held, and
                // nothing else — the next hostile simply opens a raid.
                let state = RaidState.empty |> raidTick 10 (raid squad)

                Expect.equal (windows state) [ 10, 10 ] "the first sighting simply opens an episode"
            }

            test "an episode names the rooms it works that its raid stood in" {
                let state =
                    RaidState.empty
                    |> raidTick 10 (raid squad)
                    |> raidTick 11 (raid outpostSquad |> withDeclaredOutpost outpostRoom)

                Expect.equal
                    (episodeRooms state)
                    [ Set.ofList [ raidRoom; outpostRoom ] ]
                    "the home and the outpost, where the world roster's rows of the raid are read"
            }

            // Characterisation (the world layer's S0, `docs/adr/0086`), flipped
            // at S5: the roster is the world's, and a room the colony only
            // crosses opens nothing in its log. Live t973,5xx: W17S29's 105.8 KB
            // of `episodes` were W17S25's siege, seen only in its transit rooms.
            test "a raid seen in a room the colony only crosses opens no episode, and no stand-down" {
                let transit = "W13S27"

                let crossing =
                    { quiet with
                        Crossed = Set.singleton transit
                        Hostiles = [ armedIn transit ]
                    }

                let state = RaidState.empty |> raidTick 10 crossing

                Expect.isEmpty
                    state.Episodes
                    "the transit room's raider is in no episode of this colony's"

                Expect.isEmpty state.Outposts "and the colony prices no stand-down there"

                Expect.equal
                    (Roster.fold 10 (seenRooms [ transit, crossing.Hostiles, [] ]) Roster.empty
                     |> rosterRows
                     |> List.map (fun (room, id, _) -> room, id))
                    [ transit, "raid-1" ]
                    "it is enrolled once, in the world roster"
            }
        ]

[<Tests>]
let rosterTests =
    let rosterOf (ticks: (int * WorldView) list) =
        (Roster.empty, ticks)
        ||> List.fold (fun roster (t, watch) -> Roster.fold t watch roster)

    testList
        "raid roster: the world's"
        [
            test
                "one row per hostile id in its room, with its owner, its part counts and its window" {
                let twx = raider "TWX" "giaco" { X = 38; Y = 47 } [ Tough; Tough; Attack; Move ]
                let ccv = raider "Ccv" "giaco" { X = 39; Y = 47 } [ RangedAttack; Heal; Move ]

                let roster =
                    rosterOf
                        [
                            10, seenRooms [ raidRoom, [ twx; ccv ], [] ]
                            11, seenRooms [ raidRoom, [], [] ]
                            12, seenRooms [ raidRoom, [ twx ], [] ]
                            13, seenRooms [ raidRoom, [ twx; ccv ], [] ]
                        ]

                Expect.equal
                    (rosterRows roster)
                    [
                        raidRoom,
                        "Ccv",
                        {
                            Owner = "giaco"
                            Body = Map.ofList [ Move, 1; RangedAttack, 1; Heal, 1 ]
                            First = 10
                            Last = 13
                            Dies = 13 + Engine.creepLifetime
                            Fell = None
                        }
                        raidRoom,
                        "TWX",
                        {
                            Owner = "giaco"
                            Body = Map.ofList [ Move, 1; Attack, 1; Tough, 2 ]
                            First = 10
                            Last = 13
                            Dies = 13 + Engine.creepLifetime
                            Fell = None
                        }
                    ]
                    "a squad reads as one row a creep, however often it re-enters"
            }

            test "a row keeps the body that entered the room, not what the tower left of it" {
                let whole = raider "TWX" "giaco" { X = 38; Y = 47 } [ Tough; Tough; Attack; Move ]
                let chewed = raider "TWX" "giaco" { X = 38; Y = 48 } [ Attack; Move ]

                let roster =
                    rosterOf
                        [
                            10, seenRooms [ raidRoom, [ whole ], [] ]
                            11, seenRooms [ raidRoom, [ chewed ], [] ]
                        ]

                Expect.equal
                    (rosterRows roster |> List.map (fun (_, _, row) -> row.Body))
                    [ Map.ofList [ Move, 1; Attack, 1; Tough, 2 ] ]
                    "the first sighting wins: the roster answers what came, not what survived"
            }

            test
                "one hostile in two rooms is a row in each, and neither an ally nor a keeper is enrolled" {
                let twx = raider "TWX" "giaco" { X = 38; Y = 47 } [ Attack; Move ]

                let ally = raider "Ally" (Set.minElement Colony.allies) { X = 1; Y = 1 } [ Move ]

                let keeper = raider "Keeper" "Source Keeper" { X = 2; Y = 2 } [ Attack; Move ]

                let roster =
                    rosterOf
                        [
                            10, seenRooms [ raidRoom, [ twx; ally; keeper ], [] ]
                            11,
                            seenRooms
                                [
                                    outpostRoom,
                                    [
                                        { twx with
                                            Pos = RoomPos.at outpostRoom { X = 38; Y = 0 }
                                        }
                                    ],
                                    []
                                ]
                        ]

                Expect.equal
                    (rosterRows roster
                     |> List.map (fun (room, id, row) -> room, id, row.First, row.Last))
                    [ outpostRoom, "TWX", 11, 11; raidRoom, "TWX", 10, 10 ]
                    "room by room, and only the raiders"
            }

            // #507: the four Trepidimous bodies died at W17S25's ramparts and the
            // log said nothing of it. And live, giaco's haulers died of age beside
            // W17S29 and the log counted them killed: a death is a kill only for a
            // body that died before its time.
            test
                "a tombstone stamps its rows once, and a kill is a body that fell before its life ran out" {
                let twx = raider "TWX" "giaco" { X = 38; Y = 47 } [ Tough; Attack; Move ]

                let hauler =
                    { raider "Hauler" "giaco" { X = 30; Y = 30 } [ Carry; Move ] with
                        TicksToLive = 2
                    }

                let roster =
                    rosterOf
                        [
                            // TWX is hit in the outpost and dies at home.
                            10,
                            seenRooms
                                [
                                    outpostRoom,
                                    [
                                        { twx with
                                            Pos = RoomPos.at outpostRoom { X = 38; Y = 1 }
                                        }
                                    ],
                                    []
                                    raidRoom, [ hauler ], []
                                ]
                            11, seenRooms [ raidRoom, [ twx; { hauler with TicksToLive = 1 } ], [] ]
                            12, seenRooms [ raidRoom, [], [ "TWX"; "Hauler"; "not-a-raider" ] ]
                            13, seenRooms [ raidRoom, [], [ "TWX"; "Hauler" ] ]
                        ]

                Expect.equal
                    (rosterRows roster |> List.map (fun (room, id, row) -> room, id, row.Fell))
                    [
                        outpostRoom, "TWX", Some 12
                        raidRoom, "Hauler", Some 12
                        raidRoom, "TWX", Some 12
                    ]
                    "every row of a fallen id, on t12, once however long the tombstones lie"

                Expect.equal
                    (rosterRows roster
                     |> List.filter (fun (_, _, row) -> killed row)
                     |> List.map (fun (_, id, _) -> id)
                     |> List.distinct)
                    [ "TWX" ]
                    "the raider fell with its life ahead of it, once whichever room; the hauler's ran out on t12"
            }

            test "a row no episode reads is dropped once it is older than the quiet gap" {
                let twx = raider "TWX" "giaco" { X = 38; Y = 47 } [ Attack; Move ]

                let roster =
                    rosterOf
                        [
                            10, seenRooms [ raidRoom, [ twx ], [] ]
                            10,
                            seenRooms
                                [
                                    outpostRoom,
                                    [
                                        { twx with
                                            Id = "Passer"
                                            Pos = RoomPos.at outpostRoom { X = 5; Y = 5 }
                                        }
                                    ],
                                    []
                                ]
                        ]

                let episode =
                    {
                        Opened = 10
                        LastSeen = 10
                        Rooms = Set.singleton raidRoom
                        Closest = None
                        Losses = []
                        Damage = 0
                    }

                let gap = Tuning.defaults.QuietGap

                Expect.equal
                    (Roster.prune (10 + gap) gap [] roster |> rosterRows |> List.length)
                    2
                    "inside the gap every row stands"

                Expect.equal
                    (Roster.prune (11 + gap) gap [ episode ] roster
                     |> rosterRows
                     |> List.map (fun (room, id, _) -> room, id))
                    [ raidRoom, "TWX" ]
                    "past it, the row an episode reads stands, and the transit room's goes"
            }

            // Live t988,7xx: W17S29's episode, open 11,300 ticks beside giaco's
            // hauler lane, kept 1,668 rows, about 5 ms a full tick.
            test
                "past the quiet gap a room keeps only bodies that could hurt, heal or fell, the latest RoomCap of them" {
                let gap = Tuning.defaults.QuietGap
                let armed i = raiderIn raidRoom i [ Attack; Move ]

                let hauler i =
                    { raiderIn raidRoom (1_000 + i) [ Carry; Move ] with
                        Id = $"hauler-{i}"
                    }

                let roster =
                    rosterOf (
                        [ 1 .. Roster.roomCap + 10 ]
                        |> List.map (fun i ->
                            i, seenRooms [ raidRoom, [ armed i; hauler i ], [ "hauler-1" ] ])
                    )

                let episode =
                    {
                        Opened = 1
                        LastSeen = Roster.roomCap + 10
                        Rooms = Set.singleton raidRoom
                        Closest = None
                        Losses = []
                        Damage = 0
                    }

                let tick = Roster.roomCap + 10 + gap + 1
                let kept = Roster.prune tick gap [ episode ] roster |> rosterRows

                Expect.equal
                    (kept |> List.map (fun (_, id, _) -> id) |> List.sort)
                    ([ 11 .. Roster.roomCap + 10 ] |> List.map (fun i -> $"raid-{i}") |> List.sort)
                    "the latest fifty armed rows: no hauler that lived, and the fallen hauler seen first goes with the earliest"

                let fallen =
                    Map.ofList
                        [ raidRoom, Map.ofList [ "hauler-1", roster.[raidRoom].["hauler-1"] ] ]
                    |> Roster.prune tick gap [ episode ]
                    |> rosterRows
                    |> List.map (fun (_, id, _) -> id)

                Expect.equal fallen [ "hauler-1" ] "a fallen body stays, armed or not"
            }
        ]

[<Tests>]
let approachTests =
    testList
        "raid fold: closest approach"
        [
            test
                "the closest approach is the smallest range to anything of ours, with its tile and tick" {
                // The bottom exit band is a non-event; the same squad at the
                // left door is not, and the record must separate them.
                let far = raider "TWX" "giaco" { X = 38; Y = 47 } [ Attack; Move ]
                let near = raider "TWX" "giaco" { X = 12; Y = 42 } [ Attack; Move ]

                let state =
                    RaidState.empty
                    |> raidTick 10 { placed with Hostiles = [ far ] }
                    |> raidTick 11 { placed with Hostiles = [ near ] }
                    |> raidTick 12 { placed with Hostiles = [ far ] }

                Expect.equal
                    (state.Episodes |> List.map (fun e -> e.Closest))
                    [
                        Some
                            {
                                Range = 2
                                Pos = RoomPos.at raidRoom { X = 12; Y = 42 }
                                Tick = 11
                            }
                    ]
                    "the minimum over the episode, on the tile and the tick it was reached"
            }

            test "a tie keeps the tick the raid first reached its closest" {
                // Both tiles sit at range 2 — (12,42) from the spawn, (8,42)
                // from both. Nothing gets nearer, so the second sighting must
                // not overwrite the first.
                let first = raider "TWX" "giaco" { X = 12; Y = 42 } [ Attack; Move ]
                let again = raider "TWX" "giaco" { X = 8; Y = 42 } [ Attack; Move ]

                let state =
                    RaidState.empty
                    |> raidTick 10 { placed with Hostiles = [ first ] }
                    |> raidTick 11 { placed with Hostiles = [ again ] }

                Expect.equal
                    (state.Episodes |> List.map (fun e -> e.Closest))
                    [
                        Some
                            {
                                Range = 2
                                Pos = RoomPos.at raidRoom { X = 12; Y = 42 }
                                Tick = 10
                            }
                    ]
                    "an equal range is not a nearer one: the tile and tick already recorded stand"
            }

            test "an owned creep counts as much as an owned structure" {
                let state =
                    RaidState.empty
                    |> raidTick
                        10
                        { placed with
                            Hostiles = [ raider "TWX" "giaco" { X = 9; Y = 46 } [ Attack; Move ] ]
                        }

                Expect.equal
                    (state.Episodes |> List.map (fun e -> e.Closest))
                    [
                        Some
                            {
                                Range = 2
                                Pos = RoomPos.at raidRoom { X = 9; Y = 46 }
                                Tick = 10
                            }
                    ]
                    "the nearer of the two owned tiles wins, and here that one is a creep of ours"
            }

            test "a colony the projection cannot place records no approach" {
                let state = RaidState.empty |> raidTick 10 (raid squad)

                Expect.equal
                    (state.Episodes |> List.map (fun e -> e.Closest))
                    [ None ]
                    "absence is per-entry: an unmeasurable approach is None, never a zero range"
            }

            test "one of ours in another room is not a raider at range 0" {
                // A `Pos` carries no room, so a creep of ours standing on the raider's
                // coordinates in the outpost would read as touching it. The raider is at
                // the bottom exit band, 28 tiles off the spawn, so a room-blind union
                // would show up as an unmissable 0.
                let outpost =
                    { RoomLayer.empty with
                        CreepPositions = Map.ofList [ "w2", { X = 38; Y = 47 } ]
                    }

                let colony =
                    { placed with
                        Creeps = [ ours "w1"; ours "w2" ]
                        Hostiles = [ raider "TWX" "giaco" { X = 38; Y = 47 } [ Attack; Move ] ]
                        Spatial =
                            { placed.Spatial with
                                Rooms = Map.add "W12S27" outpost placed.Spatial.Rooms
                            }
                    }

                let state = RaidState.empty |> raidTick 10 colony

                Expect.equal
                    (state.Episodes |> List.map (fun e -> e.Closest))
                    [
                        Some
                            {
                                Range = 28
                                Pos = RoomPos.at raidRoom { X = 38; Y = 47 }
                                Tick = 10
                            }
                    ]
                    "the spawn in the raider's own room is the nearest thing of ours there"
            }

            test "a raider in a room the projection places nothing of ours in measures nothing" {
                // The other half: the room is read off the raider, not assumed to be the
                // colony's. Everything of ours stands in W12S28, so a raider filed under
                // the outpost has nothing to close on — absence, not a zero range.
                // Declared, so the raid is the colony's (a room it only crosses
                // opens no episode).
                let elsewhere =
                    { raider "TWX" "giaco" { X = 9; Y = 46 } [ Attack; Move ] with
                        Pos = RoomPos.at outpostRoom { X = 9; Y = 46 }
                    }

                let state =
                    RaidState.empty
                    |> raidTick
                        10
                        ({ placed with Hostiles = [ elsewhere ] } |> withDeclaredOutpost outpostRoom)

                Expect.equal
                    (state.Episodes |> List.map (fun e -> e.Closest))
                    [ None ]
                    "the same tile that measured range 2 at home measures nothing from the outpost"
            }
            // The approach is measured against armed hostiles alone: live, a `1 MOVE`
            // scout at range 1 on the Reactor's ring named an episode while an invader
            // three rooms away did the killing (#376).
            test "an unarmed scout at range 1 is no approach; the armed raider further off is" {
                let scout = raider "SCOUT" "odiodin" { X = 9; Y = 45 } [ Move ]

                let scouted = RaidState.empty |> raidTick 10 { placed with Hostiles = [ scout ] }

                Expect.equal
                    (scouted.Episodes |> List.map (fun e -> e.Closest))
                    [ None ]
                    "the scout opens the episode and is on its roster, but measures no approach"

                let both =
                    RaidState.empty
                    |> raidTick
                        10
                        { placed with
                            Hostiles = scout :: squad
                        }

                Expect.equal
                    (both.Episodes
                     |> List.map (fun e -> e.Closest |> Option.map (fun c -> c.Range, c.Pos)))
                    [
                        Some(
                            RoomPos.range
                                (RoomPos.at raidRoom { X = 38; Y = 47 })
                                (RoomPos.at raidRoom { X = 10; Y = 40 })
                            |> Option.get,
                            RoomPos.at raidRoom { X = 38; Y = 47 }
                        )
                    ]
                    "with the armed raider beside it, the approach is the raider's own range and tile"
            }
        ]

[<Tests>]
let lossTests =
    testList
        "raid fold: losses"
        [
            test "a creep that goes missing under a raider is stamped at the tick it was last alive" {
                // A name is missing the tick after its creep died, so t11's
                // reading is a death during t10 — and t10 is inside the
                // window the episode records.
                let state =
                    RaidState.empty
                    |> raidTick
                        10
                        { (raid squad) with
                            Creeps = [ ours "w1"; ours "w2" ]
                        }
                    |> raidTick
                        11
                        { (raid squad) with
                            Creeps = [ ours "w1" ]
                        }

                Expect.equal
                    (losses state)
                    [
                        {
                            Creep = "w2"
                            Tick = 10
                            Where = None
                        }
                    ]
                    "the loss the Transition log prunes is the one this channel exists to keep"
            }

            // The tile the body last stood on rides the loss, read off the prior
            // tick's placement — the tick it is missing the projection no longer
            // places it. The episode names no room, so this is the only way a reader
            // can tell which of the colony's rooms a body died in.
            test "a loss carries the tile the body last stood on, and none when it was never placed" {
                let withOurs names positions =
                    { (raid squad) with
                        Creeps = names |> List.map ours
                        Spatial =
                            { (raid squad).Spatial with
                                Rooms =
                                    Map.ofList
                                        [
                                            raidRoom,
                                            { RoomLayer.empty with
                                                CreepPositions = Map.ofList positions
                                            }
                                        ]
                            }
                    }

                let state =
                    RaidState.empty
                    |> raidTick
                        10
                        (withOurs
                            [ "w1"; "w2" ]
                            [ "w1", { X = 9; Y = 44 }; "w2", { X = 12; Y = 41 } ])
                    |> raidTick 11 (withOurs [ "w1" ] [ "w1", { X = 9; Y = 44 } ])

                Expect.equal
                    (losses state)
                    [
                        {
                            Creep = "w2"
                            Tick = 10
                            Where = Some(RoomPos.at raidRoom { X = 12; Y = 41 })
                        }
                    ]
                    "the loss names the tile w2 stood on the tick before it went missing"

                let unplaced =
                    RaidState.empty
                    |> raidTick 10 (withOurs [ "w1"; "w2" ] [])
                    |> raidTick 11 (withOurs [ "w1" ] [])

                Expect.equal
                    (losses unplaced |> List.map (fun loss -> loss.Where))
                    [ None ]
                    "a body the projection never placed is stamped with no tile (ADR 0004)"
            }

            test "the kill read on the first quiet tick is still the raid's" {
                // The poke-and-heal shape of #66: the squad kills and steps
                // straight back out, so the name goes missing on a tick with
                // no hostile in the room. It died under the last sighting.
                let state =
                    RaidState.empty
                    |> raidTick
                        10
                        { (raid squad) with
                            Creeps = [ ours "w1"; ours "w2" ]
                        }
                    |> raidTick 11 { quiet with Creeps = [ ours "w1" ] }

                Expect.equal
                    (losses state)
                    [
                        {
                            Creep = "w2"
                            Tick = 10
                            Where = None
                        }
                    ]
                    "the reading lags the death by a tick, and the tick it lands on is the sighting"
            }

            test "a creep gone deeper into the quiet gap is not charged to the raid" {
                // Two ticks after the last sighting: whatever took this creep,
                // no hostile was standing there when it was last seen alive.
                let state =
                    RaidState.empty
                    |> raidTick
                        10
                        { (raid squad) with
                            Creeps = [ ours "w1"; ours "w2" ]
                        }
                    |> raidTick 12 { quiet with Creeps = [ ours "w1" ] }

                Expect.equal
                    (losses state)
                    []
                    "the window is opened-to-last-seen, and attrition outside it is not the raid's"
            }

            test "a creep whose own clock ran out is not a loss" {
                // Ordinary old age lands inside a raid window often enough to
                // pad it: a creep on 1,500-tick life, a raid over 200. The
                // ColonyView's TicksToLive tells the two apart before the fact.
                let state =
                    RaidState.empty
                    |> raidTick
                        10
                        { (raid squad) with
                            Creeps = [ ours "w1"; spent "w2" ]
                        }
                    |> raidTick
                        11
                        { (raid squad) with
                            Creeps = [ ours "w1" ]
                        }

                Expect.equal
                    (losses state)
                    []
                    "the record answers what the raid cost, and this one the raiders never touched"
            }

            test "a creep gone on the seam between two raids is charged to neither" {
                // The gap elapses on the very tick a fresh hostile arrives:
                // the creep was last alive during the old raid's silence, and
                // the episode opening now has not seen it at all.
                let state =
                    RaidState.empty
                    |> raidTick
                        10
                        { (raid squad) with
                            Creeps = [ ours "w1"; ours "w2" ]
                        }
                    |> raidTick
                        16
                        { (raid squad) with
                            Creeps = [ ours "w1" ]
                        }

                Expect.equal
                    (windows state)
                    [ (10, 10); (16, 16) ]
                    "the gap made this a second raid"

                Expect.equal
                    (losses state)
                    []
                    "a fresh episode has no baseline of its own, so it opens owing nothing"
            }

            test "a creep that dies of old age outside any episode is recorded nowhere" {
                let state =
                    RaidState.empty
                    |> raidTick
                        10
                        { quiet with
                            Creeps = [ ours "w1"; ours "w2" ]
                        }
                    |> raidTick 11 { quiet with Creeps = [ ours "w1" ] }

                Expect.equal
                    state
                    { RaidState.empty with
                        DigOpen = Some Set.empty
                    }
                    "peacetime attrition opens no episode and leaves no loss behind"
            }

            test "a creep another colony adopted is not a loss: it left the fleet, not the world" {
                // A name can leave a ColonyView two ways — its creep died, or the colony
                // next door adopted the body for the tick — and only the first is what the
                // raid cost. Pairwise against the loss above: the same name gone at t11,
                // once still in `Game.creeps` and once not.
                let crossed =
                    RaidState.empty
                    |> raidTickIn
                        [ "w1"; "w2" ]
                        10
                        { (raid squad) with
                            Creeps = [ ours "w1"; ours "w2" ]
                        }
                    |> raidTickIn
                        [ "w1"; "w2" ]
                        11
                        { (raid squad) with
                            Creeps = [ ours "w1" ]
                        }

                Expect.equal
                    (losses crossed)
                    []
                    "the body is standing in the colony next door, and the raid is charged nothing for it"

                let killed =
                    RaidState.empty
                    |> raidTickIn
                        [ "w1"; "w2" ]
                        10
                        { (raid squad) with
                            Creeps = [ ours "w1"; ours "w2" ]
                        }
                    |> raidTickIn
                        [ "w1" ]
                        11
                        { (raid squad) with
                            Creeps = [ ours "w1" ]
                        }

                Expect.equal
                    (losses killed)
                    [
                        {
                            Creep = "w2"
                            Tick = 10
                            Where = None
                        }
                    ]
                    "and the same absence with the name gone from the world is the loss it always was"
            }

            test
                "a creep crossing back and forth all raid is charged once for each death, and never for a crossing" {
                // `Losses` appends, so a hauler shuttling across the [[seam]] during a
                // 200-tick siege would otherwise file a fresh phantom kill on every tick
                // it left the fleet.
                let shuttle =
                    RaidState.empty
                    |> raidTickIn
                        [ "w1"; "hauler" ]
                        10
                        { (raid squad) with
                            Creeps = [ ours "w1"; ours "hauler" ]
                        }
                    |> raidTickIn
                        [ "w1"; "hauler" ]
                        11
                        { (raid squad) with
                            Creeps = [ ours "w1" ]
                        }
                    |> raidTickIn
                        [ "w1"; "hauler" ]
                        12
                        { (raid squad) with
                            Creeps = [ ours "w1"; ours "hauler" ]
                        }
                    |> raidTickIn
                        [ "w1"; "hauler" ]
                        13
                        { (raid squad) with
                            Creeps = [ ours "w1" ]
                        }

                Expect.equal (losses shuttle) [] "four ticks, two crossings, nothing owed"
            }
        ]

/// Every episode's recorded damage, oldest episode first.
let damages (state: RaidState) =
    state.Episodes |> List.map (fun e -> e.Damage)

[<Tests>]
let damageTests =
    testList
        "raid fold: damage"
        [
            test "the hits lost over an episode are summed tick over tick" {
                // The raid's cost in hits, folded from the previous tick's the way the
                // losses are folded from its names.
                let state =
                    RaidState.empty
                    |> raidTick 10 (raid squad |> withHits "ram-1" BuiltKind.Rampart 100_000)
                    |> raidTick 11 (raid squad |> withHits "ram-1" BuiltKind.Rampart 99_400)
                    |> raidTick 12 (raid squad |> withHits "ram-1" BuiltKind.Rampart 98_000)

                Expect.equal
                    (damages state)
                    [ 2_000 ]
                    "600 hits and then 1,400, charged to the one open episode"
            }

            test "a repair is not negative damage" {
                // Decreases summed, increases ignored: the record answers
                // what the raid took off, not where the hits stood at the
                // end — a rampart raised back over its floor mid-raid must
                // not subtract the damage that made it necessary.
                let state =
                    RaidState.empty
                    |> raidTick 10 (raid squad |> withHits "ram-1" BuiltKind.Rampart 100_000)
                    |> raidTick 11 (raid squad |> withHits "ram-1" BuiltKind.Rampart 90_000)
                    |> raidTick 12 (raid squad |> withHits "ram-1" BuiltKind.Rampart 100_000)

                Expect.equal (damages state) [ 10_000 ] "the repair leaves the total where it was"
            }

            test "a probe that touches nothing records no damage" {
                let state =
                    RaidState.empty
                    |> raidTick 10 (raid squad |> withHits "spawn-1" BuiltKind.Spawn 5_000)
                    |> raidTick 11 (raid squad |> withHits "spawn-1" BuiltKind.Spawn 5_000)

                Expect.equal (damages state) [ 0 ] "an episode that cost nothing says so"
            }

            test "the Keep and the ramparts are charged; the decaying kinds are not" {
                // The measure is the Keep's and its cover's. A road wearing down under a
                // raid is ordinary decay, and charging it would drown the number.
                let dented kind hits = raid squad |> withHits "s-1" kind hits

                let over kind first second =
                    RaidState.empty
                    |> raidTick 10 (dented kind first)
                    |> raidTick 11 (dented kind second)
                    |> damages

                Expect.equal (over BuiltKind.Spawn 5_000 4_000) [ 1_000 ] "the spawn is charged"
                Expect.equal (over BuiltKind.Tower 5_000 4_000) [ 1_000 ] "the tower is charged"
                Expect.equal (over BuiltKind.Storage 5_000 4_000) [ 1_000 ] "the Storage is charged"

                Expect.equal
                    (over BuiltKind.Rampart 100_000 99_000)
                    [ 1_000 ]
                    "and the ramparts over them"

                Expect.equal (over BuiltKind.Road 5_000 4_000) [ 0 ] "a chewed road is not the Keep"

                Expect.equal
                    (over BuiltKind.Container 5_000 4_000)
                    [ 0 ]
                    "and neither is a chewed container, ramparted or not"
            }

            test "damage is not charged across the seam between two episodes" {
                // The baseline is carried only while an episode is open, and
                // a freshly opened one is charged nothing on its opening
                // tick: whatever the hits did while nobody was in the room
                // belongs to no raid.
                let state =
                    RaidState.empty
                    |> raidTick 10 (raid squad |> withHits "ram-1" BuiltKind.Rampart 100_000)
                    |> raidTick 11 (raid squad |> withHits "ram-1" BuiltKind.Rampart 99_000)
                    |> raidTick 20 (raid squad |> withHits "ram-1" BuiltKind.Rampart 50_000)

                Expect.equal
                    (windows state)
                    [ (10, 11); (20, 20) ]
                    "the quiet gap closed the first episode before the second opened"

                Expect.equal
                    (damages state)
                    [ 1_000; 0 ]
                    "the 49,000 hits that went missing between the two are charged to neither"
            }

            test "a rampart raised mid-episode is no damage on the tick it stands" {
                // A structure the baseline does not carry costs nothing: the
                // fold reads decreases, and appearing is not one.
                let state =
                    RaidState.empty
                    |> raidTick 10 (raid squad)
                    |> raidTick 11 (raid squad |> withHits "ram-1" BuiltKind.Rampart 1)

                Expect.equal (damages state) [ 0 ] "a rampart at 1 hit has lost nothing yet"
            }

            test "the decay of a quiet gap is charged to no raid" {
                // An episode stays open through the quiet gap, and a rampart ticks down 300
                // hits every 100 ticks. Damage is read over the window the losses are, so
                // the gap's own decay never lands in the record.
                let state =
                    RaidState.empty
                    |> raidTick 10 (raid squad |> withHits "ram-1" BuiltKind.Rampart 100_000)
                    |> raidTick 11 (raid squad |> withHits "ram-1" BuiltKind.Rampart 99_000)
                    |> raidTick 12 (quiet |> withHits "ram-1" BuiltKind.Rampart 98_700)
                    |> raidTick 13 (quiet |> withHits "ram-1" BuiltKind.Rampart 98_400)
                    |> raidTick 14 (quiet |> withHits "ram-1" BuiltKind.Rampart 98_100)

                Expect.equal
                    (windows state)
                    [ (10, 11) ]
                    "the episode is still open, and its window still ends at the last sighting"

                Expect.equal
                    (damages state)
                    [ 1_300 ]
                    "the raid's 1,000 and the 300 read one tick late; the rest of the gap is decay"
            }

            test "a raid a room away opens an episode and is charged none of this room's decay" {
                // An outpost's raider opens a colony episode, and damage is the one field
                // that cannot follow it: the Keep and its ramparts stand in the colony's
                // own room, so a window held open from next door would charge 3 hits a
                // tick per rampart of ordinary decay as what the raid cost.
                let decaying hostiles t =
                    raid hostiles
                    |> withDeclaredOutpost outpostRoom
                    |> withHits "ram-1" BuiltKind.Rampart (100_000 - 300 * t)

                let over hostiles =
                    (RaidState.empty, [ 0..2 ])
                    ||> List.fold (fun state t -> raidTick (10 + t) (decaying hostiles t) state)

                let away = over outpostSquad

                Expect.equal
                    (windows away)
                    [ (10, 12) ]
                    "the premise: the outpost's raider opens an episode and holds it open"

                Expect.equal (damages away) [ 0 ] "and the decay at home is charged to nobody"

                // Pairwise, the same body on the same tile filed at home: the room is the
                // only difference between the two runs.
                Expect.equal
                    (damages (over squad))
                    [ 600 ]
                    "the same raider here is charged every hit the window covers"
            }

            test "the baseline is dropped in peacetime" {
                // Carried exactly as `Living` is: hits lost while no episode
                // is open are charged to nobody, and the next raid opens
                // against the hits it finds.
                let state =
                    RaidState.empty
                    |> raidTick 10 (quiet |> withHits "ram-1" BuiltKind.Rampart 100_000)

                Expect.equal state.Hits Map.empty "a quiet tick keeps no baseline"

                let raiding =
                    RaidState.empty
                    |> raidTick 10 (raid squad |> withHits "ram-1" BuiltKind.Rampart 100_000)

                Expect.equal
                    raiding.Hits
                    (Map.ofList [ "ram-1", 100_000 ])
                    "an open episode carries this tick's hits into the next"
            }
        ]

/// An enemy remote the colony harasses (#432), one crossing from home.
let private harassRoom = "W12S29"

let private harassing =
    Fabot.Core.Tests.Decide.Fixtures.casting
        {
            RoomName = harassRoom
            Enemy = "Trepidimous"
            Stand = RoomPos.at harassRoom { X = 25; Y = 25 }
            Controller = RoomPos.at harassRoom { X = 40; Y = 40 }
            Via = []
            Blocks = None
        }

/// The same room cast and shut: off the worked list, still in the cast set.
let private shutOut (colony: ColonyView) = { colony with Harass = [] }

/// Trepidimous's raid squad as it stood outside W17S29: `18M17A` and two
/// `11M7H`, 510 damage and 168 heal a tick.
let private trepidimousSquad =
    [
        List.replicate 18 Move @ List.replicate 17 Attack
        List.replicate 11 Move @ List.replicate 7 Heal
        List.replicate 11 Move @ List.replicate 7 Heal
    ]
    |> List.mapi (fun i body ->
        { raiderIn harassRoom i body with
            Owner = "Trepidimous"
            TicksToLive = 700
        })

[<Tests>]
let harassStandDownTests =
    testList
        "raid fold: a harassment room's stand-down"
        [
            test
                "a squad the biggest ranger loses to shuts the room until HarassClearTicks after it was last seen there" {
                let colony =
                    { (harassing quiet) with
                        Hostiles = trepidimousSquad
                        Bank = { Available = 5600; Capacity = 5600 }
                    }

                // The squad crosses once and moves on; the room is in sight.
                let state =
                    RaidState.empty
                    |> raidTick 100 colony
                    |> raidTick 101 (shutOut colony |> fun c -> { c with Hostiles = [] })

                Expect.contains
                    (shutAt 149 state)
                    harassRoom
                    "withdrawn while the squad may be near"

                Expect.isFalse
                    (Set.contains harassRoom (shutAt 150 state))
                    "and harassed again fifty ticks after it was last seen, not when its 700-tick life runs out"

                Expect.equal
                    (standDowns state)
                    [ harassRoom, 100, 100, 150, StandDownBasis.HarassSighting ]
                    "the clock is the last sighting's, and says so"
            }

            // Characterisation (the world layer's S5): B8 of its inventory, the
            // clock left in the old caster's log when `harassCasters` picked
            // another colony, was closed by S2's world log already. This pins
            // it, and passes on S2 by design.
            test "the clock is the world log's: a new caster moves the row the old one opened" {
                let colony =
                    { (harassing quiet) with
                        Hostiles = trepidimousSquad
                        Bank = { Available = 5600; Capacity = 5600 }
                    }

                let row (danger: Danger) =
                    danger.Rooms
                    |> Map.tryFind harassRoom
                    |> Option.bind _.Down
                    |> Option.map (fun down -> down.Opened, down.LastSeen, down.Expiry, down.Basis)

                let first, opened = (RaidState.empty, Danger.empty) |> dangerTick [] 100 colony

                // The next caster, its own log empty, sees the squad again.
                let _, moved = (RaidState.empty, opened) |> dangerTick [] 130 (shutOut colony)

                Expect.equal
                    (row moved)
                    (Some(100, 130, 180, StandDownBasis.HarassSighting))
                    "one row: opened by the first caster, moved by the next"

                let _, left =
                    (first, moved)
                    |> dangerTick
                        []
                        131
                        { quiet with
                            Hostiles = trepidimousSquad
                        }

                Expect.equal
                    (row left)
                    (row moved)
                    "and the old caster, casting it no more, leaves the clock be"
            }

            test "the squad seen again while the room is shut moves the clock to that sighting" {
                let colony =
                    { (harassing quiet) with
                        Hostiles = trepidimousSquad
                        Bank = { Available = 5600; Capacity = 5600 }
                    }

                // Shut at 100, and the squad still standing there at 130: the
                // room is off the worked list and still cast.
                let state = RaidState.empty |> raidTick 100 colony |> raidTick 130 (shutOut colony)

                Expect.contains
                    (shutAt 179 state)
                    harassRoom
                    "shut fifty ticks past the later sighting"

                Expect.isFalse
                    (Set.contains harassRoom (shutAt 180 state))
                    "and open on the fiftieth"
            }

            test "a dark tick neither extends nor erases a harassment room's clock" {
                let colony =
                    { (harassing quiet) with
                        Hostiles = trepidimousSquad
                        Bank = { Available = 5600; Capacity = 5600 }
                    }

                // Nothing of ours stands there once it is shut: no hostile, no
                // controller, for every tick up to and past the deadline.
                let dark = { shutOut colony with Hostiles = [] }

                let state =
                    [ 101..160 ]
                    |> List.fold
                        (fun state t -> raidTick t dark state)
                        (RaidState.empty |> raidTick 100 colony)

                Expect.equal
                    (standDowns state)
                    [ harassRoom, 100, 100, 150, StandDownBasis.HarassSighting ]
                    "the row stands as the last sighting wrote it"

                Expect.contains (shutAt 149 state) harassRoom "shut through the blind ticks"

                Expect.isFalse
                    (Set.contains harassRoom (shutAt 150 state))
                    "and open on the deadline"
            }

            test "a sighting there replaces an older raid-life clock and never outlives a core's" {
                let colony =
                    { (harassing quiet) with
                        Hostiles = trepidimousSquad
                        Bank = { Available = 5600; Capacity = 5600 }
                    }

                let standing basis expiry =
                    { RaidState.empty with
                        Outposts =
                            [
                                {
                                    RoomName = harassRoom
                                    Opened = 100
                                    LastSeen = 100
                                    Expiry = expiry
                                    Basis = basis
                                    Stronghold = false
                                    Raiders = Set.empty
                                    PlayerSince = None
                                    Chased = false
                                    Bounced = false
                                }
                            ]
                    }

                // The live case: a 1,338-tick deadline for a squad seen once.
                Expect.equal
                    (standing StandDownBasis.InvaderRaid 1_438
                     |> raidTick 200 (shutOut colony)
                     |> standDowns)
                    [ harassRoom, 100, 200, 250, StandDownBasis.HarassSighting ]
                    "the raid's life is not the room's clock any more"

                Expect.equal
                    (standing StandDownBasis.CollapseTimer 5_000
                     |> raidTick 200 (shutOut colony)
                     |> standDowns)
                    [ harassRoom, 100, 200, 5_000, StandDownBasis.CollapseTimer ]
                    "a core's clock is the core's, and the squad does not cut it short"
            }

            test "an outpost under a harassing squad is held the player window past its opening" {
                let colony =
                    { (withDeclaredOutpost harassRoom quiet) with
                        Hostiles = trepidimousSquad
                        Bank = { Available = 5600; Capacity = 5600 }
                    }

                let state =
                    RaidState.empty
                    |> raidTick 100 colony
                    |> raidTick 101 { colony with Hostiles = [] }

                Expect.contains
                    (shutAt 299 state)
                    harassRoom
                    "withdrawn past the harassment clear (#513)"

                Expect.isFalse
                    (Set.contains harassRoom (shutAt 300 state))
                    "and worked again at its opening plus 200, the squad still alive"

                Expect.equal
                    (standDowns state |> List.map (fun (_, _, _, _, basis) -> basis))
                    [ StandDownBasis.PlayerRaidHold ]
                    "off the player's-raid window"
            }

            test "a room shut on a raid's clock is a raided outpost, one shut on a core's is not" {
                // #514: what lays the home's shelter once the raid is out of sight.
                let episode room basis =
                    {
                        RoomName = room
                        Opened = 100
                        LastSeen = 100
                        Expiry = 500
                        Basis = basis
                        Stronghold = false
                        Raiders = Set.empty
                        PlayerSince = None
                        Chased = false
                        Bounced = false
                    }

                let state =
                    { RaidState.empty with
                        Outposts =
                            [
                                episode "W1N2" StandDownBasis.PlayerRaidHold
                                episode "W2N1" StandDownBasis.InvaderRaid
                                episode "W1N0" StandDownBasis.CollapseTimer
                            ]
                    }

                Expect.equal
                    (standDown Tuning.defaults 200 state).RaidedOutposts
                    (Set.ofList [ "W1N2"; "W2N1" ])
                    "the two raids, not the core"

                Expect.isEmpty
                    (standDown Tuning.defaults 500 state).RaidedOutposts
                    "and neither once its clock has run"
            }

            test "the enemy's escort the ranger beats is fought, not withdrawn from" {
                let escort =
                    { raiderIn harassRoom 1 [ Move; Move; Move; RangedAttack; Heal ] with
                        Owner = "Trepidimous"
                    }

                let colony =
                    { (harassing quiet) with
                        Hostiles = [ escort ]
                        Bank = { Available = 2100; Capacity = 2100 }
                    }

                Expect.isEmpty
                    (RaidState.empty |> raidTick 100 colony |> shutAt 101)
                    "three ranger blocks beat a 3M1RA1H"
            }

            // #510: a ranged body holds the range on any melee-only body slower
            // than it, so the distance and not the paper decides.
            test "a melee squad that cannot close on the ranger is kited, not withdrawn from" {
                let bodies (list: BodyPart list list) =
                    list
                    |> List.mapi (raiderIn harassRoom)
                    |> List.map (fun hostile -> { hostile with Owner = "Trepidimous" })

                let squadOf hostiles =
                    { (harassing quiet) with
                        Hostiles = bodies hostiles
                        Bank = { Available = 800; Capacity = 800 }
                    }

                // Five on two: behind a ranger on a road as on plain.
                let heavy = List.replicate 5 [ Attack; Attack; Attack; Attack; Attack; Move; Move ]

                let fleet =
                    List.replicate 5 [ Attack; Attack; Attack; Attack; Move; Move; Move; Move ]

                let shooters = List.replicate 5 [ RangedAttack; RangedAttack; Move ]

                Expect.isEmpty
                    (RaidState.empty |> raidTick 100 (squadOf heavy) |> shutAt 101)
                    "five ATTACK on two MOVE out-swing one ranger block and never catch it"

                Expect.contains
                    (RaidState.empty
                     |> raidTick 100 (squadOf (heavy @ [ [ Heal; Heal; Move; Move ] ]))
                     |> shutAt 101)
                    harassRoom
                    "a heal that out-runs the ranger's 20 a tick makes the kite endless: weighed on paper"

                Expect.contains
                    (RaidState.empty |> raidTick 100 (squadOf fleet) |> shutAt 101)
                    harassRoom
                    "the same blows on legs as quick as the ranger's still shut the room"

                Expect.contains
                    (RaidState.empty |> raidTick 100 (squadOf (heavy @ shooters)) |> shutAt 101)
                    harassRoom
                    "a squad that shoots back is weighed on paper, slow or not"
            }

            test "the enemy's reservation of the room it harasses is no stand-down" {
                let colony =
                    harassing quiet |> visible harassRoom (heldBy ReservationHolder.Rival 4_000)

                Expect.isEmpty
                    (RaidState.empty |> raidTick 100 colony |> shutAt 101)
                    "the enemy holding its remote is why the room is declared"

                // Beside it, a room nobody harasses under the same hold is shut.
                Expect.contains
                    (RaidState.empty
                     |> raidTick
                         100
                         (visible harassRoom (heldBy ReservationHolder.Rival 4_000) quiet)
                     |> shutAt 101)
                    harassRoom
                    "the same hold anywhere else is the stand-down it always was"
            }

            test
                "a room the squad shut stays ours: the enemy's reservation stretches no stand-down, and the squad is no approach" {
                let ranger = ours "ranger-1"

                // Our ranger standing beside the squad, under the enemy's hold.
                let colony =
                    { (harassing quiet) with
                        Creeps = [ ranger ]
                        Hostiles = trepidimousSquad
                        Bank = { Available = 5600; Capacity = 5600 }
                        Spatial =
                            { quiet.Spatial with
                                Rooms =
                                    Map.ofList
                                        [
                                            harassRoom,
                                            { RoomLayer.empty with
                                                CreepPositions =
                                                    Map.ofList [ ranger.Name, { X = 26; Y = 25 } ]
                                            }
                                        ]
                            }
                    }
                    |> visible harassRoom (heldBy ReservationHolder.Rival 4_000)

                // The tick after: the gate has shut the room, so it is off
                // the worked list and still cast.
                let state = RaidState.empty |> raidTick 100 colony |> raidTick 101 (shutOut colony)

                Expect.contains (shutAt 150 state) harassRoom "the squad's stand-down still runs"

                Expect.isFalse
                    (Set.contains harassRoom (shutAt 151 state))
                    "and ends fifty ticks after the squad was last seen, not at the end of the enemy's reservation"

                Expect.equal
                    (state.Episodes |> List.map (fun e -> e.Closest))
                    [ None ]
                    "the squad standing in the shut room measures no approach on us"
            }

            test "our own attack on the enemy's creeps is no approach on us" {
                let ranger = ours "ranger-1"

                let colony =
                    { (harassing quiet) with
                        Creeps = [ ranger ]
                        Hostiles =
                            [
                                { raiderIn harassRoom 1 [ Move; RangedAttack ] with
                                    Owner = "Trepidimous"
                                }
                            ]
                        Spatial =
                            { quiet.Spatial with
                                Rooms =
                                    Map.ofList
                                        [
                                            harassRoom,
                                            { RoomLayer.empty with
                                                CreepPositions =
                                                    Map.ofList [ ranger.Name, { X = 26; Y = 25 } ]
                                            }
                                        ]
                            }
                    }

                Expect.equal
                    ((RaidState.empty |> raidTick 100 colony).Episodes
                     |> List.map (fun e -> e.Closest))
                    [ None ]
                    "the episode records the roster, and measures no approach"
            }
        ]

/// A resident room's raid record (`foldFights`), on `fightingMother`'s child
/// home at this tick, under this raid.
let private fightRecordAt time raid =
    { Decide.Fixtures.fightingMother raid [] with
        Time = time
    }

/// A raid of this many ATTACK, a MOVE each, in the child's home.
let private meleeOf n =
    [
        Decide.Fixtures.hostileIn
            "W1N2"
            { X = 26; Y = 40 }
            (List.replicate n Move @ List.replicate n Attack)
    ]

/// The ids of a raid's raiders, as a fight record keeps them.
let private raidersOf (raid: HostileInfo list) =
    raid
    |> List.filter (HostileInfo.raider raid)
    |> List.map (fun hostile -> hostile.Id)
    |> Set.ofList

/// One record of the child's home.
let private recordOf seen raid =
    Map.ofList
        [
            "W1N2",
            {
                Seen = seen
                Raiders = raidersOf raid
            }
        ]

[<Tests>]
let fightRecordTests =
    testList
        "raid fold: the fight record"
        [
            test
                "opened by a raid the residents lose, moved on by any raid after, and dropped FightHoldTicks after its raid was last seen" {
                let hold = Tuning.defaults.FightHoldTicks
                let lost = Decide.Fixtures.w17s25RaidIn "W1N2"

                let opened = foldFights (fightRecordAt 1_000 lost) Map.empty

                Expect.equal opened (recordOf 1_000 lost) "a raid the residents lose opens it"

                Expect.isEmpty
                    (foldFights (fightRecordAt 1_000 (meleeOf 2)) Map.empty)
                    "one they win opens nothing"

                Expect.equal
                    (foldFights (fightRecordAt 1_100 []) opened)
                    opened
                    "a quiet tick keeps it as it was"

                Expect.equal
                    (foldFights (fightRecordAt (1_000 + hold) (meleeOf 2)) opened)
                    (recordOf (1_000 + hold) (meleeOf 2))
                    "any raid moves it on while it stands, a shrunk one too"

                Expect.isEmpty
                    (foldFights (fightRecordAt (1_001 + hold) []) opened)
                    "and it is dropped a tick past FightHoldTicks"
            }

            // The Brief's S7 (live t970,260, W17S25): an invader the residents
            // and the tower killed in the room within a few ticks held the
            // record for FightHoldTicks. The hold is for a raid that left.
            test "a raid killed in the room drops its record at once; one that stepped out keeps it" {
                let lost = Decide.Fixtures.w17s25RaidIn "W1N2"
                let latched = recordOf 1_000 lost
                let dead = raidersOf lost |> Set.toList

                Expect.isEmpty
                    (foldFights
                        { fightRecordAt 1_003 [] with
                            Fallen = dead
                        }
                        latched)
                    "every raider of its last sighting lies dead there: no hold"

                Expect.equal
                    (foldFights (fightRecordAt 1_003 []) latched)
                    latched
                    "gone with no body seen dead: the hold stands"

                Expect.equal
                    (foldFights
                        { fightRecordAt 1_003 [] with
                            Fallen = List.take 1 dead
                        }
                        latched)
                    latched
                    "one dead and the rest gone: it may yet come back"
            }

            // The Brief's S7 (the same live case): the record opens only on a
            // raid the residents lose, and the residents standing in the room are
            // priced together, where one body of the garrison's size was.
            test "a raid one resident loses but the two standing in the room win opens no record" {
                let raid = meleeOf 12

                let resident name x =
                    Decide.Fixtures.creepWith
                        name
                        0
                        0
                        (Fabot.Core.Decide.Bodies.bodyFor
                            Fabot.Core.Decide.Bodies.rangerPattern
                            4900),
                    RoomPos.at "W1N2" { X = x; Y = 36 }

                let at bodies =
                    { Decide.Fixtures.fightingMother raid bodies with
                        Time = 1_000
                        LoadedTowers = Map.empty
                    }

                Expect.isNonEmpty
                    (foldFights (at []) Map.empty)
                    "the premise: one garrison-sized body loses it"

                Expect.isNonEmpty
                    (foldFights (at [ resident "r-1" 24 ]) Map.empty)
                    "and so does one resident standing there"

                Expect.isEmpty
                    (foldFights (at [ resident "r-1" 24; resident "r-2" 26 ]) Map.empty)
                    "the two standing there win it together: no record"
            }

            // The world layer's S5: the records are one world map, so a colony
            // that is not resident in a room never judges its record off its own
            // vision; the clocks end it whoever folds.
            test "a record of a room this colony is not resident in moves by its clock alone" {
                let hold = Tuning.defaults.FightHoldTicks
                let lost = Decide.Fixtures.w17s25RaidIn "W1N2"
                let latched = recordOf 1_000 lost

                let neighbour time =
                    { quiet with
                        Time = time
                        Fallen = raidersOf lost |> Set.toList
                    }

                Expect.equal
                    (foldFights (neighbour 1_003) latched)
                    latched
                    "every raider lies dead where it can see, and the record is not its to drop"

                Expect.isEmpty
                    (foldFights (neighbour (1_001 + hold)) latched)
                    "but the hold runs out for it as for anybody"
            }

            // B6 of the world layer's inventory: a garrisoned child home's record
            // was the mother's, and the child could not see it.
            test
                "the mother's record of her child's home is the world log's, which every colony's gate reads" {
                let lost = Decide.Fixtures.w17s25RaidIn "W1N2"

                let _, danger =
                    Danger.foldColony
                        3
                        Set.empty
                        Set.empty
                        (fightRecordAt 1_000 lost)
                        []
                        RaidState.empty
                        Danger.empty

                Expect.equal
                    (gateAt 1_001 danger).Fought
                    (recordOf 1_000 lost)
                    "the child's gate holds the record the mother opened"
            }
        ]

/// `fightingMother`'s child room under a probe's Provoke (#493), a rampart
/// on its breach, at this tick: the probe with this life left on this tile,
/// or gone, and safe mode up in the room or not.
let private probedAt time (life: int) (tile: RoomPos option) (safeMode: bool) =
    let probe =
        Decide.Fixtures.creepWith "probe-1" 0 0 Fabot.Core.Decide.Bodies.probePattern.Block
        |> Decide.Fixtures.withLife life

    { Decide.Fixtures.fightingMother [] (tile |> Option.map (fun at -> probe, at) |> Option.toList) with
        Time = time
        Assaults =
            [
                {
                    Assault =
                        { Assault.w18s26 with
                            RoomName = "W1N2"
                            Entry = None
                        }
                    Targets = Some [ "rampart-1", { X = 20; Y = 30 } ]
                    Taken = false
                    Towers = []
                    SafeMode = safeMode
                    BarredUntil = None
                }
            ]
    }

let private inTheRoom = Some(RoomPos.at "W1N2" { X = 20; Y = 31 })

let private dismantles = [ DismantleStructure("probe-1", "rampart-1") ]

let private probeLogOf (state: RaidState) = Map.tryFind "W1N2" state.Probes

[<Tests>]
let probeLogTests =
    testList
        "raid fold: the probe log"
        [
            test
                "a probe's first dismantle opens its room's log; its dismantles, and the light ticks' replays of one, count until safe mode shows" {
                let opened =
                    RaidState.empty |> foldProbes (probedAt 100 1_000 inTheRoom false) dismantles

                Expect.equal
                    (probeLogOf opened)
                    (Some
                        {
                            Probe = "probe-1"
                            FirstHit = 100
                            Hits = 1
                            HitAt = Some 100
                            SafeModeAt = None
                            Fate = Probing
                            Expires = 1_100
                            Home = "W1N1"
                        })
                    "opened on the first dismantle"

                let replayed = opened |> foldProbes (probedAt 103 997 inTheRoom false) dismantles

                Expect.equal
                    (probeLogOf replayed |> Option.map (fun log -> log.Hits))
                    (Some 4)
                    "t101 and t102 light, each replaying it, and t103's own"

                let raised = replayed |> foldProbes (probedAt 104 996 inTheRoom true) []

                Expect.equal
                    (probeLogOf raised |> Option.map (fun log -> log.Hits, log.SafeModeAt, log.Fate))
                    (Some(4, Some 104, Probing))
                    "safe mode seen at t104 after four dismantles, the probe still in the room"

                let out =
                    raised
                    |> foldProbes
                        (probedAt 105 995 (Some(RoomPos.at "W1N1" { X = 20; Y = 1 })) true)
                        []

                Expect.equal
                    (probeLogOf out |> Option.map (fun log -> log.Fate))
                    (Some(Out 105))
                    "out of the room alive at t105"

                Expect.equal
                    (out |> foldProbes (probedAt 106 0 None true) [] |> probeLogOf)
                    (probeLogOf out)
                    "and settled: its going later is not its fate"
            }

            test "a probe gone with its life left was killed; one gone at its life's end expired" {
                let opened life =
                    RaidState.empty |> foldProbes (probedAt 100 life inTheRoom false) dismantles

                Expect.equal
                    (opened 1_000
                     |> foldProbes (probedAt 102 0 None false) []
                     |> probeLogOf
                     |> Option.map (fun log -> log.Fate))
                    (Some(Died 102))
                    "killed"

                Expect.equal
                    (opened 2
                     |> foldProbes (probedAt 102 0 None false) []
                     |> probeLogOf
                     |> Option.map (fun log -> log.Fate))
                    (Some(Expired 102))
                    "of age"
            }

            test "only a probe's dismantle of the room's target opens a log" {
                Expect.isNone
                    (RaidState.empty
                     |> foldProbes
                         (probedAt 100 1_000 inTheRoom false)
                         [ DismantleStructure("sapper-1", "rampart-1") ]
                     |> probeLogOf)
                    "a sapper's opens none"

                Expect.isNone
                    (RaidState.empty
                     |> foldProbes
                         (probedAt 100 1_000 inTheRoom false)
                         [ DismantleStructure("probe-1", "container-1") ]
                     |> probeLogOf)
                    "nor a probe's of something else"
            }

            test
                "an assault room seen with its targets down is recorded taken from that tick, kept while dark, and dropped once one stands again (#496)" {
                let seenAt time (targets: (string * Pos) list option) (taken: bool) =
                    let view = probedAt time 1_000 None false

                    { view with
                        Assaults =
                            view.Assaults
                            |> List.map (fun facts ->
                                { facts with
                                    // Taken is an assault's with declared targets alone.
                                    Assault =
                                        { facts.Assault with
                                            Targets = [ { X = 25; Y = 41 } ]
                                        }
                                    Targets = targets
                                    Taken = taken
                                })
                    }

                let opened = foldTaken (seenAt 100 (Some []) true) Map.empty
                Expect.equal opened (Map.ofList [ "W1N2", 100 ]) "taken at t100"

                Expect.equal
                    (foldTaken (seenAt 101 (Some []) true) opened)
                    opened
                    "seen again: still from t100"

                Expect.equal (foldTaken (seenAt 102 None true) opened) opened "dark: kept"

                Expect.isEmpty
                    (foldTaken (seenAt 103 (Some [ "link-1", { X = 25; Y = 41 } ]) false) opened)
                    "the link rebuilt: dropped"
            }

            // The world layer's S5: a rival room's probe and its taken tick are
            // the room's, not the assaulter's.
            test "the probe log and the taken room are the world log's, laid into every colony's" {
                let view = probedAt 100 1_000 inTheRoom false

                let state, danger =
                    Danger.foldColony 3 Set.empty Set.empty view [] RaidState.empty Danger.empty
                    |> Danger.probing view dismantles

                let laid = Danger.into danger RaidState.empty

                Expect.equal (probeLogOf laid) (probeLogOf state) "the log the assaulter folded"
                Expect.isSome (probeLogOf laid) "and it holds one"

                let taken =
                    { view with
                        Assaults =
                            view.Assaults
                            |> List.map (fun facts ->
                                { facts with
                                    Assault =
                                        { facts.Assault with
                                            Targets = [ { X = 20; Y = 30 } ]
                                        }
                                    Targets = Some []
                                    Taken = true
                                })
                    }

                let _, danger =
                    Danger.foldColony 3 Set.empty Set.empty taken [] RaidState.empty Danger.empty

                Expect.equal
                    (Danger.into danger RaidState.empty).Taken
                    (Map.ofList [ "W1N2", 100 ])
                    "and so is the tick its targets were first seen down"
            }

            // W18S26 is declared twice live: w18s26Link with its target, w18s26
            // (a probe's line) with none. One record a room, so each colony moves
            // only the part that is its own.
            test
                "a second colony assaulting the room moves neither the first's probe log nor its taken tick" {
                let assaulter time intents (state, danger) =
                    let view = probedAt time 1_000 inTheRoom false

                    let view =
                        { view with
                            Assaults =
                                view.Assaults
                                |> List.map (fun facts ->
                                    { facts with
                                        Assault =
                                            { facts.Assault with
                                                Targets = [ { X = 20; Y = 30 } ]
                                            }
                                        Taken = true
                                    })
                        }

                    Danger.foldColony 3 Set.empty Set.empty view [] state danger
                    |> Danger.probing view intents

                let other time (state, danger) =
                    let view = probedAt time 1_000 None false

                    let view =
                        { view with
                            Spatial =
                                { view.Spatial with
                                    RoomName = Some "W9N9"
                                }
                        }

                    Danger.foldColony 3 Set.empty Set.empty view [] state danger
                    |> Danger.probing view []

                let _, first = assaulter 100 dismantles (RaidState.empty, Danger.empty)
                let _, after = other 101 (RaidState.empty, first)

                let parts (danger: Danger) =
                    danger.Rooms
                    |> Map.tryFind "W1N2"
                    |> Option.map (fun entry -> entry.Taken, entry.Probe)

                Expect.isSome
                    (parts first |> Option.bind snd)
                    "the premise: the first colony's probe opened the log"

                Expect.equal
                    (parts first |> Option.bind fst)
                    (Some 100)
                    "and its targets were seen down"

                Expect.equal
                    (parts after)
                    (parts first)
                    "the other colony, holding no probe and declaring no target, moves neither"
            }
        ]

[<Tests>]
let reserveLatchTests =
    testList
        "the emergency reserve's latch"
        [
            test
                "the fold latches a colony under its reserve until it banks one hauler load past it (#505)" {
                // RCL4 keeps 10,000; the 300 bank's hauler carries 200. The
                // latch the view carries is the one the last fold wrote.
                let stocked energy latched =
                    { quiet with
                        Controller = Some(Fabot.Core.Tests.Decide.Fixtures.controllerAt 4)
                        Bank = { Available = 300; Capacity = 300 }
                        ReserveShort = latched
                        Spatial =
                            { quiet.Spatial with
                                TargetKinds =
                                    Map.add
                                        "sto-1"
                                        (Structure BuiltKind.Storage)
                                        quiet.Spatial.TargetKinds
                                Stores = Map.add "sto-1" energy quiet.Spatial.Stores
                            }
                    }

                let latch energy latched =
                    (raidTick 100 (stocked energy latched) RaidState.empty).ReserveShort

                Expect.isTrue (latch 9_999 false) "under the reserve: latched"
                Expect.isTrue (latch 10_100 true) "latched, a hundred past the line is still under"
                Expect.isFalse (latch 10_200 true) "a hauler load past it: released"
                Expect.isFalse (latch 10_100 false) "unlatched, the line is the reserve itself"
            }

            test "the fold writes the ores dug this tick as the next tick's dig latch" {
                let ore = Fabot.Core.Tests.Decide.Fixtures.oreColony

                Expect.equal
                    (raidTick 100 ore RaidState.empty).DigOpen
                    (Some(Set.ofList [ Oxygen ]))
                    "the oxygen dug this tick is latched open"

                Expect.equal
                    (raidTick
                        100
                        (ore |> Fabot.Core.Tests.Decide.Fixtures.withOreIn "term-1" Oxygen 6_000)
                        RaidState.empty)
                        .DigOpen
                    (Some Set.empty)
                    "a terminal at its target latches it shut"
            }
        ]

[<Tests>]
let haulOverflowLatchTests =
    testList
        "the haul overflow reading"
        [
            test
                "the raid log carries the last decided tick's haul overflow reading to the next tick's gate (#508)" {
                let overflow = { Fill = 0.8; Owed = true }

                let logged =
                    { RaidState.empty with
                        HaulOverflow = overflow
                    }

                Expect.equal
                    (raidTick 100 quiet logged).HaulOverflow
                    overflow
                    "the raid fold leaves it as the decision wrote it"

                Expect.equal
                    (standDown Tuning.defaults 100 logged).HaulOverflow
                    overflow
                    "and the gate hands it to the view"
            }
        ]
