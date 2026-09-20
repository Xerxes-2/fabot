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

                Expect.equal
                    state
                    RaidState.empty
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
                            (Fabot.Core.Decide.Planner.outpostFactsOf seen)
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
        ]

[<Tests>]
let rosterTests =
    testList
        "raid fold: roster"
        [
            test "one row per hostile id, with its owner and its part counts" {
                let twx = raider "TWX" "giaco" { X = 38; Y = 47 } [ Tough; Tough; Attack; Move ]
                let ccv = raider "Ccv" "giaco" { X = 39; Y = 47 } [ RangedAttack; Heal; Move ]

                let state =
                    RaidState.empty
                    |> raidTick 10 (raid [ twx; ccv ])
                    |> raidTick 11 quiet
                    |> raidTick 12 (raid [ twx ])
                    |> raidTick 13 (raid [ twx; ccv ])

                Expect.equal
                    (rosters state)
                    [
                        "Ccv",
                        {
                            Owner = "giaco"
                            Body = Map.ofList [ Move, 1; RangedAttack, 1; Heal, 1 ]
                        }
                        "TWX",
                        {
                            Owner = "giaco"
                            Body = Map.ofList [ Move, 1; Attack, 1; Tough, 2 ]
                        }
                    ]
                    "a squad reads as one row a creep, however often it re-enters"
            }

            test "a row keeps the body that entered the room, not what the tower left of it" {
                let whole = raider "TWX" "giaco" { X = 38; Y = 47 } [ Tough; Tough; Attack; Move ]
                let chewed = raider "TWX" "giaco" { X = 38; Y = 48 } [ Attack; Move ]

                let state =
                    RaidState.empty |> raidTick 10 (raid [ whole ]) |> raidTick 11 (raid [ chewed ])

                Expect.equal
                    (rosters state)
                    [
                        "TWX",
                        {
                            Owner = "giaco"
                            Body = Map.ofList [ Move, 1; Attack, 1; Tough, 2 ]
                        }
                    ]
                    "the first sighting wins: the roster answers what came, not what survived"
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
                let elsewhere =
                    { raider "TWX" "giaco" { X = 9; Y = 46 } [ Attack; Move ] with
                        Pos = RoomPos.at "W12S27" { X = 9; Y = 46 }
                    }

                let state = RaidState.empty |> raidTick 10 { placed with Hostiles = [ elsewhere ] }

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
                    RaidState.empty
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
                    raid hostiles |> withHits "ram-1" BuiltKind.Rampart (100_000 - 300 * t)

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
