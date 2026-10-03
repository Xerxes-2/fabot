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
                            (Fabot.Core.Decide.Planner.outpostFactsOf seen).Declared
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

            test "an outpost under the same raid keeps the raid's life" {
                let colony =
                    { (withDeclaredOutpost harassRoom quiet) with
                        Hostiles = trepidimousSquad
                        Bank = { Available = 5600; Capacity = 5600 }
                    }

                let state =
                    RaidState.empty
                    |> raidTick 100 colony
                    |> raidTick 101 { colony with Hostiles = [] }

                Expect.contains (shutAt 799 state) harassRoom "withdrawn while the squad lives"

                Expect.isFalse
                    (Set.contains harassRoom (shutAt 800 state))
                    "and worked again when it cannot"

                Expect.equal
                    (standDowns state |> List.map (fun (_, _, _, _, basis) -> basis))
                    [ StandDownBasis.InvaderRaid ]
                    "off the raid's life"
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

[<Tests>]
let fightRecordTests =
    testList
        "raid fold: the fight record"
        [
            test
                "opened unlatched by a raid the residents lose, and latched with its squad on a second sighting inside FightConfirmTicks" {
                let confirm = Tuning.defaults.FightConfirmTicks
                let lost = Decide.Fixtures.w17s25RaidIn "W1N2"

                let opened = foldFights (fightRecordAt 1_000 lost) Map.empty

                Expect.equal
                    opened
                    (Map.ofList [ "W1N2", { Seen = 1_000; Squad = None } ])
                    "a raid the residents lose opens it, latching nothing"

                Expect.isEmpty
                    (foldFights (fightRecordAt 1_000 (meleeOf 2)) Map.empty)
                    "one they win opens nothing"

                Expect.equal
                    (foldFights (fightRecordAt (1_000 + confirm) lost) opened)
                    (Map.ofList
                        [
                            "W1N2",
                            {
                                Seen = 1_000 + confirm
                                Squad = Some "duo"
                            }
                        ])
                    "seen again inside FightConfirmTicks, it latches the squad the Fight is pooled with"

                Expect.equal
                    (foldFights (fightRecordAt (1_001 + confirm) lost) opened)
                    (Map.ofList [ "W1N2", { Seen = 1_001 + confirm; Squad = None } ])
                    "seen again later, it opens afresh"

                Expect.isEmpty
                    (foldFights (fightRecordAt (1_001 + confirm) []) opened)
                    "and with no second sighting it is dropped"
            }

            test
                "a latched record keeps its squad: moved on while it holds, left alone while it bars a cast" {
                let hold = Tuning.defaults.FightHoldTicks
                let lost = Decide.Fixtures.w17s25RaidIn "W1N2"

                let latched = Map.ofList [ "W1N2", { Seen = 1_000; Squad = Some "3×kiter" } ]

                let at seen =
                    Map.ofList [ "W1N2", { Seen = seen; Squad = Some "3×kiter" } ]

                Expect.equal
                    (foldFights (fightRecordAt 1_100 []) latched)
                    latched
                    "a quiet tick keeps it as it was"

                Expect.equal
                    (foldFights (fightRecordAt (1_000 + hold) (meleeOf 2)) latched)
                    (at (1_000 + hold))
                    "any raid moves it on while it holds the Fight, a shrunk one too, and the squad stays"

                Expect.equal
                    (foldFights (fightRecordAt (1_001 + hold) lost) latched)
                    latched
                    "while it bars a second cast, a raid moves nothing"

                Expect.isEmpty
                    (foldFights (fightRecordAt (1_000 + 2 * hold) []) latched)
                    "and it is dropped once the bar runs out"
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
        ]
