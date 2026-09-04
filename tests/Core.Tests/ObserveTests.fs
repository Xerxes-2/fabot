module Fabot.Core.Tests.ObserveTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Observe

/// A creep's recorded timeline as (tick, verdict) pairs, oldest first.
let timeline name (state: ObserveState) =
    match Map.tryFind name state with
    | None -> []
    | Some log -> log.Entries |> List.map (fun e -> e.Tick, e.Verdict)

/// Fold one tick with everyone in the verdicts alive, default cap.
let tick t living verdicts state =
    fold capPerCreep t (Set.ofList living) verdicts state

[<Tests>]
let appendTests =
    testList
        "observe fold: change detection"
        [
            test "a first Verdict opens the creep's timeline, stamped with the tick" {
                let state =
                    Map.empty
                    |> tick 5 [ "a" ] [ Verdict.Matched("a", "harvest:src-1", MatchFactor.Rank) ]

                Expect.equal
                    (timeline "a" state)
                    [ 5, Verdict.Matched("a", "harvest:src-1", MatchFactor.Rank) ]
                    "the fresh match is the creep's first entry"
            }

            test "a Kept of the same Task appends nothing after the match that won it" {
                // Kept is the anti-thrash steady state: the creep still holds the
                // Task the logged match explains, so there is no change to record.
                let state =
                    Map.empty
                    |> tick 5 [ "a" ] [ Verdict.Matched("a", "harvest:src-1", MatchFactor.Rank) ]
                    |> tick 6 [ "a" ] [ Verdict.Kept("a", "harvest:src-1") ]
                    |> tick 7 [ "a" ] [ Verdict.Kept("a", "harvest:src-1") ]

                Expect.equal
                    (timeline "a" state)
                    [ 5, Verdict.Matched("a", "harvest:src-1", MatchFactor.Rank) ]
                    "quiet ticks write nothing"
            }

            test "an identical Unassigned repeats nothing; a changed reason appends" {
                let state =
                    Map.empty
                    |> tick 5 [ "a" ] [ Verdict.Unassigned("a", IdleReason.NoTasks) ]
                    |> tick 6 [ "a" ] [ Verdict.Unassigned("a", IdleReason.NoTasks) ]
                    |> tick 7 [ "a" ] [ Verdict.Unassigned("a", IdleReason.NoneApplicable) ]

                Expect.equal
                    (timeline "a" state)
                    [
                        5, Verdict.Unassigned("a", IdleReason.NoTasks)
                        7, Verdict.Unassigned("a", IdleReason.NoneApplicable)
                    ]
                    "only the reason change is a recorded event"
            }

            test "a handover records the release and the fresh match in one tick, in order" {
                let state =
                    Map.empty
                    |> tick 5 [ "a" ] [ Verdict.Matched("a", "harvest:src-1", MatchFactor.Rank) ]
                    |> tick
                        9
                        [ "a" ]
                        [
                            Verdict.Released("a", "harvest:src-1", ReleaseReason.TaskGone)
                            Verdict.Matched("a", "refill:spawn-1", MatchFactor.OnlyCandidate)
                        ]

                Expect.equal
                    (timeline "a" state)
                    [
                        5, Verdict.Matched("a", "harvest:src-1", MatchFactor.Rank)
                        9, Verdict.Released("a", "harvest:src-1", ReleaseReason.TaskGone)
                        9, Verdict.Matched("a", "refill:spawn-1", MatchFactor.OnlyCandidate)
                    ]
                    "the handover reads from → to with its reason"
            }
        ]

[<Tests>]
let movementTests =
    testList
        "observe fold: movement episodes"
        [
            test "a grounding spanning several ticks is one entry" {
                let state =
                    Map.empty
                    |> tick 5 [ "a" ] [ Verdict.Kept("a", "t"); Verdict.Grounded "a" ]
                    |> tick 6 [ "a" ] [ Verdict.Kept("a", "t"); Verdict.Grounded "a" ]
                    |> tick 7 [ "a" ] [ Verdict.Kept("a", "t"); Verdict.Grounded "a" ]

                Expect.equal
                    (timeline "a" state)
                    [ 5, Verdict.Kept("a", "t"); 5, Verdict.Grounded "a" ]
                    "an unbroken episode records only its start"
            }

            test "a quiet tick between groundings starts a new episode" {
                // Movement Verdicts are episodic: a tick with none means the creep
                // moved freely, so a repeat afterwards is a fresh event.
                let state =
                    Map.empty
                    |> tick 5 [ "a" ] [ Verdict.Kept("a", "t"); Verdict.Grounded "a" ]
                    |> tick 6 [ "a" ] [ Verdict.Kept("a", "t") ]
                    |> tick 7 [ "a" ] [ Verdict.Kept("a", "t"); Verdict.Grounded "a" ]

                Expect.equal
                    (timeline "a" state)
                    [ 5, Verdict.Kept("a", "t"); 5, Verdict.Grounded "a"; 7, Verdict.Grounded "a" ]
                    "each grounding episode gets its own entry"
            }

            test "a yield to a different counterpart is a change" {
                let state =
                    Map.empty
                    |> tick 5 [ "a"; "b"; "c" ] [ Verdict.Yielded("a", "b") ]
                    |> tick 6 [ "a"; "b"; "c" ] [ Verdict.Yielded("a", "c") ]

                Expect.equal
                    (timeline "a" state)
                    [ 5, Verdict.Yielded("a", "b"); 6, Verdict.Yielded("a", "c") ]
                    "who holds the tile is part of the event"
            }

            test "a reroute beside a yield persists without re-appending" {
                // Both movement Verdicts can ride one tick; a tick repeating the
                // same pair is the same episode continuing.
                let pair = [ Verdict.Rerouted "a"; Verdict.Yielded("a", "b") ]

                let state = Map.empty |> tick 5 [ "a"; "b" ] pair |> tick 6 [ "a"; "b" ] pair

                Expect.equal
                    (timeline "a" state)
                    [ 5, Verdict.Rerouted "a"; 5, Verdict.Yielded("a", "b") ]
                    "the continuing pair appends nothing"
            }

            test "matcher and resolver events interleave in one timeline in tick order" {
                let state =
                    Map.empty
                    |> tick 5 [ "a" ] [ Verdict.Matched("a", "t1", MatchFactor.Rank) ]
                    |> tick 6 [ "a" ] [ Verdict.Kept("a", "t1"); Verdict.Grounded "a" ]
                    |> tick
                        7
                        [ "a" ]
                        [
                            Verdict.Released("a", "t1", ReleaseReason.Unreachable)
                            Verdict.Unassigned("a", IdleReason.NoneReachable)
                        ]

                Expect.equal
                    (timeline "a" state)
                    [
                        5, Verdict.Matched("a", "t1", MatchFactor.Rank)
                        6, Verdict.Grounded "a"
                        7, Verdict.Released("a", "t1", ReleaseReason.Unreachable)
                        7, Verdict.Unassigned("a", IdleReason.NoneReachable)
                    ]
                    "one chronology holds task and movement events"
            }
        ]

[<Tests>]
let scoringTests =
    testList
        "observe fold: the verbose scoring channel"
        [
            test "an unchanged scoring appends nothing; a changed row appends" {
                let stable = Candidate.Rejected("t2", RejectReason.CapacityFull)
                let before = Verdict.Scoring("a", [ Candidate.Scored("t1", 0, 3, 1); stable ])
                let after = Verdict.Scoring("a", [ Candidate.Scored("t1", 0, 4, 1); stable ])

                let state =
                    Map.empty
                    |> tick 5 [ "a" ] [ before ]
                    |> tick 6 [ "a" ] [ before ]
                    |> tick 7 [ "a" ] [ after ]

                Expect.equal
                    (timeline "a" state)
                    [ 5, before; 7, after ]
                    "only the tick a row moved is a recorded event"
            }

            test "re-flipping verbose on records the unchanged scoring afresh" {
                // Scorings are episodic: a tick without one means off the
                // list, so turning verbose back on always records — the
                // investigator's confirmation the flip took effect.
                let scoring = Verdict.Scoring("a", [ Candidate.Scored("t", 0, 0, 0) ])

                let state =
                    Map.empty
                    |> tick 5 [ "a" ] [ scoring ]
                    |> tick 6 [ "a" ] []
                    |> tick 7 [ "a" ] [ scoring ]

                Expect.equal
                    (timeline "a" state)
                    [ 5, scoring; 7, scoring ]
                    "each verbose episode opens with a recorded scoring"
            }

            test "scoring rides its own channel: the steady Kept stays quiet around it" {
                // Flipping verbose on mid-investigation must not make the
                // unchanged assignment re-append as if it were news.
                let scoring = Verdict.Scoring("a", [ Candidate.Scored("t", 0, 0, 0) ])

                let state =
                    Map.empty
                    |> tick 5 [ "a" ] [ Verdict.Matched("a", "t", MatchFactor.Rank) ]
                    |> tick 6 [ "a" ] [ scoring; Verdict.Kept("a", "t") ]
                    |> tick 7 [ "a" ] [ scoring; Verdict.Kept("a", "t") ]

                Expect.equal
                    (timeline "a" state)
                    [ 5, Verdict.Matched("a", "t", MatchFactor.Rank); 6, scoring ]
                    "the scoring lands once and displaces no task-channel judgement"
            }
        ]

[<Tests>]
let ringTests =
    testList
        "observe fold: ring cap"
        [
            test "sustained churn holds the cap; oldest entries fall off first" {
                // Alternate between two idle reasons: every tick is a change.
                let reasonAt t =
                    if t % 2 = 0 then
                        IdleReason.NoTasks
                    else
                        IdleReason.NoneApplicable

                let state =
                    (Map.empty, [ 1..9 ])
                    ||> List.fold (fun state t ->
                        state
                        |> fold 3 t (Set.ofList [ "a" ]) [ Verdict.Unassigned("a", reasonAt t) ])

                Expect.equal
                    (timeline "a" state)
                    [
                        7, Verdict.Unassigned("a", reasonAt 7)
                        8, Verdict.Unassigned("a", reasonAt 8)
                        9, Verdict.Unassigned("a", reasonAt 9)
                    ]
                    "only the newest cap-many entries survive"
            }

            test "an unchanged Kept stays quiet even after churn evicts its match from the ring" {
                // Movement churn under a tiny cap pushes the Matched entry off
                // the ring; the steady Kept must still append nothing — change
                // detection judges against the creep's story, not against
                // whatever the ring happens to retain.
                let state =
                    Map.empty
                    |> (fun s ->
                        fold
                            2
                            1
                            (Set.ofList [ "a" ])
                            [ Verdict.Matched("a", "t", MatchFactor.Rank) ]
                            s)
                    |> (fun s ->
                        fold
                            2
                            2
                            (Set.ofList [ "a" ])
                            [ Verdict.Kept("a", "t"); Verdict.Grounded "a" ]
                            s)
                    |> (fun s -> fold 2 3 (Set.ofList [ "a" ]) [ Verdict.Kept("a", "t") ] s)
                    |> (fun s ->
                        fold
                            2
                            4
                            (Set.ofList [ "a" ])
                            [ Verdict.Kept("a", "t"); Verdict.Yielded("a", "b") ]
                            s)
                    |> (fun s -> fold 2 5 (Set.ofList [ "a" ]) [ Verdict.Kept("a", "t") ] s)

                Expect.equal
                    (timeline "a" state)
                    [ 2, Verdict.Grounded "a"; 4, Verdict.Yielded("a", "b") ]
                    "the ring holds only the movement events; no spurious Kept re-appends"
            }
        ]

[<Tests>]
let pruneTests =
    testList
        "observe fold: pruning and prior state"
        [
            test "a dead creep's timeline is pruned" {
                let state =
                    Map.empty
                    |> tick 5 [ "a"; "b" ] [ Verdict.Kept("a", "t"); Verdict.Kept("b", "t") ]
                    |> tick 6 [ "b" ] [ Verdict.Kept("b", "t") ]

                Expect.isFalse (Map.containsKey "a" state) "the dead creep's log is gone"
                Expect.isTrue (Map.containsKey "b" state) "the survivor keeps its log"
            }

            test "a Verdict for a creep not alive writes nothing" {
                let state = Map.empty |> tick 5 [] [ Verdict.Kept("ghost", "t") ]

                Expect.equal state Map.empty "no timeline opens for a creep that is not alive"
            }

            test "empty prior state folds like a fresh boot" {
                let state = Map.empty |> tick 5 [ "a" ] [ Verdict.Grounded "a" ]

                Expect.equal
                    (timeline "a" state)
                    [ 5, Verdict.Grounded "a" ]
                    "the first tick simply appends"
            }
        ]

/// A colony nobody is raiding: the Raid fold reads hostiles, our creeps
/// and the tiles of what is ours, so everything else stays empty.
let quiet: Snapshot =
    {
        Time = 100
        Spawns = []
        RoomEnergy = Map.empty
        Refillables = []
        Sources = []
        Controller = None
        ConstructionSites = []
        Creeps = []
        Hostiles = []
        Spatial = SpatialInfo.empty
    }

/// A hostile creep of the given owner and body standing on a tile.
let raider id owner pos body : HostileInfo =
    {
        Id = id
        Owner = owner
        Pos = pos
        Body = body
    }

/// One of ours with a full life ahead of it; immaterial but for its name.
/// Whatever becomes of it, its own clock is not what did it.
let ours name : CreepInfo =
    {
        Name = name
        TicksToLive = 500
        Fatigue = 0
        Energy = 0
        FreeCapacity = 50
        Body = Map.ofList [ Work, 1; Carry, 1; Move, 1 ]
    }

/// One of ours on the last tick of its life: the engine's counter runs out
/// on it, so it is gone next tick whatever the raiders do.
let spent name = { ours name with TicksToLive = 1 }

/// The squad of #66, cut to the one creep the lifecycle tests need.
let squad = [ raider "TWX" "giaco" { X = 38; Y = 47 } [ Tough; Attack; Move ] ]

/// A colony holding just these hostiles.
let raid hostiles = { quiet with Hostiles = hostiles }

/// #66's room in miniature, with something of ours to measure against:
/// the tower's spawn at (10,40) and one of our creeps out at (9,44).
let placed =
    { quiet with
        Refillables =
            [
                {
                    Id = "spawn-1"
                    FreeCapacity = 0
                    Kind = BuiltKind.Spawn
                }
            ]
        Creeps = [ ours "w1" ]
        Spatial =
            { SpatialInfo.empty with
                TargetPositions = Map.ofList [ "spawn-1", { X = 10; Y = 40 } ]
                CreepPositions = Map.ofList [ "w1", { X = 9; Y = 44 } ]
            }
    }

/// Fold one Raid-log tick over a colony at the given tick, with a small
/// ring cap and a short quiet gap so both are exercised in a few ticks
/// rather than a few hundred.
let raidTick t (colony: Snapshot) state =
    foldRaids 3 5 { colony with Time = t } state

/// The recorded episodes as (opened, last-seen) windows, oldest first.
let windows (state: RaidState) =
    state.Episodes |> List.map (fun e -> e.Opened, e.LastSeen)

/// Every episode's roster rows, oldest episode first.
let rosters (state: RaidState) =
    state.Episodes |> List.collect (fun e -> Map.toList e.Roster)

/// Every episode's recorded losses, oldest episode first.
let losses (state: RaidState) =
    state.Episodes |> List.collect (fun e -> e.Losses)

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
                // #66's shape: the same creeps re-entering over and over.
                // The gap is five ticks here, and t15 is exactly five ticks
                // after the last sighting, so the return is still the raid
                // that is already open.
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
                                Pos = { X = 12; Y = 42 }
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
                                Pos = { X = 12; Y = 42 }
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
                                Pos = { X = 9; Y = 46 }
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
                    [ { Creep = "w2"; Tick = 10 } ]
                    "the loss the Transition log prunes is the one this channel exists to keep"
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
                    [ { Creep = "w2"; Tick = 10 } ]
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
                // Snapshot's TicksToLive tells the two apart before the fact.
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
        ]

/// The colony with one structure of the given kind standing at the given
/// hits — what the damage fold reads: the kind decides whether it is
/// charged, the number is what moves tick over tick.
let withHits id kind hits (colony: Snapshot) =
    { colony with
        Spatial =
            { colony.Spatial with
                TargetKinds = Map.add id (Structure kind) colony.Spatial.TargetKinds
                Hits = Map.add id { Hits = hits; HitsMax = 3_000_000 } colony.Spatial.Hits
            }
    }

/// Every episode's recorded damage, oldest episode first.
let damages (state: RaidState) =
    state.Episodes |> List.map (fun e -> e.Damage)

[<Tests>]
let damageTests =
    testList
        "raid fold: damage"
        [
            test "the hits lost over an episode are summed tick over tick" {
                // What ADR 0028 deferred until a decision read hits (ADR
                // 0034): the raid's cost in hits, folded from the previous
                // tick's the way the losses are folded from its names.
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
                // The measure is the Keep's and its cover's (ADR 0034). A
                // road wearing down under a raid is the colony's ordinary
                // decay, and charging it would drown the number the record
                // exists for.
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
                // An episode stays open through the quiet gap, and a rampart
                // ticks down 300 hits every 100 ticks whoever is watching.
                // Damage is read over the window the losses are — a hostile
                // standing there, or the tick straight after a sighting — so
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
