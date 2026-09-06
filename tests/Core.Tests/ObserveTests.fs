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

/// The room the raid fixtures project — the colony's own, which is the
/// only room `Snapshot.Hostiles` sweeps (ADR 0028). Named rather than left
/// to `SpatialInfo.homeName`'s empty string, because the closest approach
/// now joins a hostile's room to the projection's layer (ADR 0041) and a
/// fixture whose two halves agreed by both being blank would prove nothing.
let raidRoom = "W12S28"

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
        // The Raid fold prices no source, so who holds the room is nothing
        // it reads (ADR 0042).
        RoomControl = Map.empty
        ConstructionSites = []
        Creeps = []
        Hostiles = []
        // The raid fold reads hostile *creeps* here; an invader core is a
        // structure, opens the log's other family and is folded from this
        // list (ADR 0043) — empty for every fixture the spawn room's raid
        // is measured through, which is what makes those measurements a
        // regression against the second family arriving.
        InvaderCores = []
        Spatial =
            { SpatialInfo.empty with
                RoomName = Some raidRoom
            }
        // The Raid fold reads no declaration: which rooms a human means to
        // own decides Tasks and quotas (ADR 0047), and the log records what
        // happened in a room rather than what is planned for one.
        ColonyHomes = []
    }

/// A hostile creep of the given owner and body standing on a tile of the
/// colony's own room.
let raider id owner pos body : HostileInfo =
    {
        Id = id
        Owner = owner
        RoomName = raidRoom
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
            { quiet.Spatial with
                // Under the room's own name, which is the only place tiles
                // live since ADR 0041 — and the name the hostiles above
                // stand in, because the closest approach joins the two
                // before it measures anything.
                Rooms =
                    Map.ofList
                        [
                            raidRoom,
                            { RoomLayer.empty with
                                TargetPositions = Map.ofList [ "spawn-1", { X = 10; Y = 40 } ]
                                CreepPositions = Map.ofList [ "w1", { X = 9; Y = 44 } ]
                            }
                        ]
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

            test "one of ours in another room is not a raider at range 0" {
                // What layering the projection would otherwise cost this
                // record (ADR 0041): a `Pos` carries no room, so a creep of
                // ours standing on the raider's coordinates in the outpost
                // reads as touching it without either creep leaving its
                // room. The raider is at the bottom exit band, 28 tiles off
                // the spawn — the non-event the first test above uses — so
                // a room-blind union would show up as an unmissable 0.
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
                                Pos = { X = 38; Y = 47 }
                                Tick = 10
                            }
                    ]
                    "the spawn in the raider's own room is the nearest thing of ours there"
            }

            test "a raider in a room the projection places nothing of ours in measures nothing" {
                // The other half of the same rule, and the one that says
                // the room is read off the raider rather than assumed to be
                // the colony's: everything of ours stands in W12S28, so a
                // raider filed under the outpost has nothing to close on —
                // ADR 0004's absence, not a zero range.
                let elsewhere =
                    { raider "TWX" "giaco" { X = 9; Y = 46 } [ Attack; Move ] with
                        RoomName = "W12S27"
                    }

                let state = RaidState.empty |> raidTick 10 { placed with Hostiles = [ elsewhere ] }

                Expect.equal
                    (state.Episodes |> List.map (fun e -> e.Closest))
                    [ None ]
                    "the same tile that measured range 2 at home measures nothing from the outpost"
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

/// The outpost the threat fixtures stand in: a room of the scan set that
/// is not the spawn room, so the two families are told apart by the room
/// a record names and never by both being blank.
let outpostRoom = "W12S27"

/// An invader core standing in a room, with or without a collapse timer to
/// read a deadline off. A level-0 core — the measured case on this
/// colony's frontier — carries none.
let core room collapse : InvaderCoreInfo =
    {
        RoomName = room
        CollapseTick = collapse
    }

/// A colony that can see these cores, and nothing else going on.
let seen cores = { quiet with InvaderCores = cores }

/// The room as vision answers for it: nobody owns it, and this is what
/// stands on its controller. `RoomControl` carries an entry only for a
/// room the colony can see (ADR 0004), so putting one there is how a
/// fixture says the colony is looking.
let visible room reservation (colony: Snapshot) =
    { colony with
        RoomControl =
            colony.RoomControl
            |> Map.add
                room
                {
                    Owner = Ownership.Unowned
                    Reservation = reservation
                }
    }

/// A reservation on that controller, carrying the engine's own *relative*
/// count of what is left to run on it.
let heldBy holder ticks =
    Some { Holder = holder; TicksToEnd = ticks }

/// The room as vision answers for it when another player owns the
/// controller outright — the other half of ADR 0043's clockless
/// withdrawal, beside a rival's reservation.
let ownedByRival room (colony: Snapshot) =
    { colony with
        RoomControl =
            colony.RoomControl
            |> Map.add
                room
                {
                    Owner = Ownership.Rival
                    Reservation = None
                }
    }

/// The recorded stand-downs as (room, opened, last seen, expiry, basis),
/// oldest first — the whole of what the outpost family records.
let standDowns (state: RaidState) =
    state.Outposts
    |> List.map (fun e -> e.RoomName, e.Opened, e.LastSeen, e.Expiry, e.Basis)

[<Tests>]
let outpostTests =
    testList
        "raid fold: outpost episodes"
        [
            test "an invader core opens a stand-down that runs to its collapse timer" {
                // The best of ADR 0043's three deadlines, and the only one
                // the engine hands over already absolute — the shell added
                // this tick to `ticksRemaining` on the way in (#133).
                let state = RaidState.empty |> raidTick 100 (seen [ core outpostRoom (Some 900) ])

                Expect.equal
                    (standDowns state)
                    [ outpostRoom, 100, 100, 900, StandDownBasis.CollapseTimer ]
                    "the room, the tick it opened on, and the tick read off the threat itself"
            }

            test "a core with no collapse timer runs to the end of the reservation it took" {
                // A level-0 core has no stronghold to collapse and carries
                // no timer, so the only deadline it has is the hold it took
                // with `attackController`. `TicksToEnd` is relative, so the
                // tick is this one plus it: stored as read it would be a
                // deadline four thousand ticks after the epoch.
                let state =
                    RaidState.empty
                    |> raidTick
                        100
                        (seen [ core outpostRoom None ]
                         |> visible outpostRoom (heldBy ReservationHolder.Invader 4000))

                Expect.equal
                    (standDowns state)
                    [ outpostRoom, 100, 100, 4100, StandDownBasis.Reservation ]
                    "four thousand ticks left on the hold is a deadline at tick 4,100, not at tick 4,000"
            }

            test "a hold shorter than the fallback is no deadline at all" {
                // ADR 0043's amendment, taken in #136 because this is the
                // ticket where a short clock first became observable: the
                // reservation branch may only ever answer *later* than the
                // fallback.
                //
                // A core outlives the hold it takes — it re-reserves the
                // controller the tick the hold lapses — so the end of a
                // reservation is never the end of the core, and a hold
                // with a handful of ticks left says only what the core did
                // last tick. The engine hands out exactly that: a core
                // that has just taken a controller nobody reserved holds it
                // for three ticks. Read literally that is a three-tick
                // stand-down, which is the "immediately" ADR 0043's own
                // user story says no path may reach.
                //
                // Pairwise, one number at a time, on either side of the
                // 2,500-tick fallback: only the length of the hold moves.
                let heldFor ticks =
                    RaidState.empty
                    |> raidTick
                        100
                        (seen [ core outpostRoom None ]
                         |> visible outpostRoom (heldBy ReservationHolder.Invader ticks))
                    |> standDowns

                Expect.equal
                    (heldFor 3)
                    [ outpostRoom, 100, 100, 2600, StandDownBasis.Fallback ]
                    "a three-tick hold is read as unreadable, and the clock is the one the colony chose"

                Expect.equal
                    (heldFor 300)
                    [ outpostRoom, 100, 100, 2600, StandDownBasis.Fallback ]
                    "and so is a three-hundred-tick one: below the fallback the read never shortens the gate"

                Expect.equal
                    (heldFor standDownFallback)
                    [ outpostRoom, 100, 100, 100 + standDownFallback, StandDownBasis.Reservation ]
                    "at the fallback's own length the hold reads through, and says so"

                // The basis is the operator's half of the amendment: the
                // number the two answers give at the boundary is the same,
                // and "shut until 2,600" and "shut until 2,600 because
                // nothing could be read" are different answers (#117). So
                // the floor is not a `max` over the tick with the reason
                // left standing — a stand-down naming a reservation names
                // the tick that reservation really ends on.
                let basisOf rows =
                    rows |> List.map (fun (_, _, _, _, basis) -> basis)

                Expect.notEqual
                    (basisOf (heldFor 300))
                    (basisOf (heldFor standDownFallback))
                    "the two sides of the floor are told apart by the reason, not only by the tick"
            }

            test "with neither deadline readable the clock is the expansion period" {
                // Nothing is unreadable here by accident: a core with no
                // timer in a room nothing holds is the shape the fallback
                // exists for, and no path may answer "indefinitely" or
                // "now".
                let state = RaidState.empty |> raidTick 100 (seen [ core outpostRoom None ])

                Expect.equal
                    (standDowns state)
                    [ outpostRoom, 100, 100, 2600, StandDownBasis.Fallback ]
                    "2,500 ticks on from the sighting, and the record says it was chosen and not read"
            }

            test "only the invader's own hold is a clock" {
                // Pairwise, one holder at a time: a rule reading "not ours"
                // would take a rival's reservation for the core's and shut
                // the room until a tick that says nothing about the core,
                // and ADR 0043 answers those two oppositely. Both fall back
                // rather than reading a deadline off a hold that is not the
                // threat's.
                let held holder =
                    RaidState.empty
                    |> raidTick
                        100
                        (seen [ core outpostRoom None ] |> visible outpostRoom (heldBy holder 300))
                    |> standDowns

                Expect.equal
                    (held ReservationHolder.Rival)
                    [ outpostRoom, 100, 100, 2600, StandDownBasis.Fallback ]
                    "a player's hold is the clockless withdrawal, never a deadline for this one"

                Expect.equal
                    (held ReservationHolder.Ours)
                    [ outpostRoom, 100, 100, 2600, StandDownBasis.Fallback ]
                    "and the colony's own hold says nothing about the core standing in it"
            }

            test "a core still standing there extends the stand-down and re-reads its clock" {
                let state =
                    RaidState.empty
                    |> raidTick 100 (seen [ core outpostRoom None ])
                    |> raidTick 110 (seen [ core outpostRoom None ])

                Expect.equal
                    (standDowns state)
                    [ outpostRoom, 100, 110, 2610, StandDownBasis.Fallback ]
                    "one episode, its window carried to the last sighting and its clock read at it"
            }

            test "a re-read never shortens a stand-down that is already running" {
                // The gate may be wrong in one direction only (ADR 0043's
                // Consequences): a stale stand-down costs an outpost's
                // income until its clock runs out, and the failure it
                // prevents costs a creep a cycle. A later sighting can
                // land on a worse deadline than the one already recorded
                // — the core drains our hold and takes its own, freshly
                // at a handful of ticks, or our reserver takes it back and
                // the read falls through to the fallback — and reading
                // that in would cut the stand-down short, which is the
                // other direction. The same rule `deadlines` applies to
                // two cores in one tick, applied across ticks.
                let shortened =
                    RaidState.empty
                    |> raidTick
                        100
                        (seen [ core outpostRoom None ]
                         |> visible outpostRoom (heldBy ReservationHolder.Invader 4000))
                    |> raidTick 110 (seen [ core outpostRoom None ])

                Expect.equal
                    (standDowns shortened)
                    [ outpostRoom, 100, 110, 4100, StandDownBasis.Reservation ]
                    "the window still extends to the sighting, and the clock and the reason it was read off both stand"

                let lengthened =
                    RaidState.empty
                    |> raidTick 100 (seen [ core outpostRoom None ])
                    |> raidTick
                        110
                        (seen [ core outpostRoom None ]
                         |> visible outpostRoom (heldBy ReservationHolder.Invader 4000))

                Expect.equal
                    (standDowns lengthened)
                    [ outpostRoom, 100, 110, 4110, StandDownBasis.Reservation ]
                    "and a longer deadline is taken, with the basis of the tick that won"
            }

            test "a tick without vision moves no clock and closes no stand-down" {
                // The dangerous case (#117): losing vision reads exactly
                // like peace, and the quiet gap here is five ticks, so the
                // spawn family would have closed this episode six times
                // over. This family is exempt — the colony stops looking
                // the moment it withdraws, so silence is never evidence.
                let standing = RaidState.empty |> raidTick 100 (seen [ core outpostRoom None ])

                let blind =
                    (standing, [ 101..130 ])
                    ||> List.fold (fun state t -> state |> raidTick t quiet)

                Expect.equal
                    (standDowns blind)
                    [ outpostRoom, 100, 100, 2600, StandDownBasis.Fallback ]
                    "thirty blind ticks leave the record exactly as the last tick with vision left it"
            }

            test "a room seen clear stands down all the same, until its clock runs out" {
                // Re-entry is a clock running out and never a second look
                // (ADR 0043). A tick with vision and no core is not
                // evidence the core is gone — it is what a creep passing
                // the wrong tile sees — and even a true one does not open
                // the gate early.
                let state =
                    RaidState.empty
                    |> raidTick 100 (seen [ core outpostRoom None ])
                    |> raidTick 101 (quiet |> visible outpostRoom None)

                Expect.equal
                    (standDowns state)
                    [ outpostRoom, 100, 100, 2600, StandDownBasis.Fallback ]
                    "the room looking clear neither closed the episode nor moved its expiry"
            }

            test "the clock runs out, and the next core seen opens a second stand-down" {
                let state =
                    RaidState.empty
                    |> raidTick 100 (seen [ core outpostRoom (Some 105) ])
                    |> raidTick 105 (seen [ core outpostRoom (Some 130) ])

                Expect.equal
                    (standDowns state)
                    [
                        (outpostRoom, 100, 100, 105, StandDownBasis.CollapseTimer)
                        (outpostRoom, 105, 105, 130, StandDownBasis.CollapseTimer)
                    ]
                    "the expiry is the first tick the room may be re-entered, so a sighting on it is a new stand-down and the spent one stays in the ring"
            }

            test "each outpost's clock is its own" {
                let other = "W13S28"

                let state =
                    RaidState.empty
                    |> raidTick 100 (seen [ core outpostRoom (Some 500); core other (Some 700) ])
                    |> raidTick 110 (seen [ core other (Some 700) ])

                Expect.equal
                    (standDowns state)
                    [
                        (outpostRoom, 100, 100, 500, StandDownBasis.CollapseTimer)
                        (other, 100, 110, 700, StandDownBasis.CollapseTimer)
                    ]
                    "the tick that saw one room and not the other moved that room's episode alone"
            }

            test "a ring full of raids evicts no stand-down that is running" {
                // One ring shared between the families would drop the
                // episode driving the gate and reopen the room in the
                // middle of a stand-down (#117). The cap is three here and
                // four raids overflow it.
                let state =
                    (RaidState.empty |> raidTick 10 (seen [ core outpostRoom (Some 500) ]),
                     [ 20; 30; 40; 50 ])
                    ||> List.fold (fun state t -> state |> raidTick t (raid squad))

                Expect.equal
                    (standDowns state)
                    [ outpostRoom, 10, 10, 500, StandDownBasis.CollapseTimer ]
                    "the stand-down is still there with its clock untouched"

                Expect.equal
                    (windows state)
                    [ (30, 30); (40, 40); (50, 50) ]
                    "while the raid ring trims to the cap exactly as it did before"
            }

            test "a stand-down still running survives a ring overflowing past it" {
                // Six spent stand-downs in one room around one long-running
                // one somewhere else, against a cap of three. The overflow
                // is paid out of the finished rows and never out of the one
                // holding a room shut.
                let other = "W13S28"

                let state =
                    RaidState.empty
                    |> raidTick 10 (seen [ core outpostRoom (Some 11) ])
                    |> raidTick 12 (seen [ core outpostRoom (Some 13) ])
                    |> raidTick 14 (seen [ core outpostRoom (Some 15) ])
                    |> raidTick 16 (seen [ core other (Some 5000) ])
                    |> raidTick 18 (seen [ core outpostRoom (Some 19) ])
                    |> raidTick 20 (seen [ core outpostRoom (Some 21) ])
                    |> raidTick 22 (seen [ core outpostRoom (Some 23) ])

                Expect.equal
                    (standDowns state)
                    [
                        (other, 16, 16, 5000, StandDownBasis.CollapseTimer)
                        (outpostRoom, 20, 20, 21, StandDownBasis.CollapseTimer)
                        (outpostRoom, 22, 22, 23, StandDownBasis.CollapseTimer)
                    ]
                    "the oldest row is the one still standing down, and it is the one row the trim would not take"
            }

            test "a core in an outpost leaves the spawn room's raid exactly as it was" {
                // The regression #117 asks for. The two families share a
                // Memory leaf and nothing else, so every step the raid fold
                // takes — the window, the roster, the closest approach, the
                // losses and the damage, plus both baselines it carries
                // between ticks — must read the same with a core standing
                // next door as without one. Compared as whole states rather
                // than through the projections, so a field no list here
                // reads is covered too.
                let sequence colony =
                    (RaidState.empty, [ 10..14 ])
                    ||> List.fold (fun state t ->
                        { colony with
                            Creeps = if t < 12 then [ ours "w1"; ours "w2" ] else [ ours "w1" ]
                        }
                        |> withHits "ram-1" BuiltKind.Rampart (100_000 - 200 * t)
                        |> fun tick -> state |> raidTick t tick)

                let alone = sequence { placed with Hostiles = squad }

                let beside =
                    sequence
                        { placed with
                            Hostiles = squad
                            InvaderCores = [ core outpostRoom None ]
                        }

                Expect.equal
                    { beside with Outposts = [] }
                    alone
                    "the raid reads byte for byte the same, roster, approach, losses, damage and baselines alike"

                Expect.equal
                    (standDowns beside)
                    [ outpostRoom, 10, 14, 2514, StandDownBasis.Fallback ]
                    "while the core standing next door recorded a stand-down of its own"
            }
        ]

[<Tests>]
let clocklessTests =
    testList
        "raid fold: the withdrawal with no clock"
        [
            test "a room another player holds is remembered, and no episode opens for it" {
                // ADR 0043's other trigger, and the community's one
                // unanimous abandonment rule. It is not a threat, so there
                // is no threat to read a deadline off and nothing for a
                // basis to explain: the record is the room's name and
                // that is the whole of it.
                //
                // Owned and reserved are one fact to this rule — the room
                // is being worked by somebody else — so both are pinned,
                // one at a time.
                let owned = RaidState.empty |> raidTick 100 (quiet |> ownedByRival outpostRoom)

                let reserved =
                    RaidState.empty
                    |> raidTick
                        100
                        (quiet |> visible outpostRoom (heldBy ReservationHolder.Rival 4000))

                Expect.equal
                    owned.RivalHeld
                    (Map.ofList [ outpostRoom, 100 ])
                    "an owner that is not us, against the tick the look was taken on"

                Expect.equal
                    reserved.RivalHeld
                    (Map.ofList [ outpostRoom, 100 ])
                    "and a reservation that is not ours read the same way"

                Expect.isEmpty owned.Outposts "no episode opened: there is no clock to run"
                Expect.isEmpty reserved.Outposts "nor for the reservation"
            }

            test "the NPC's hold is a clock and never an exit" {
                // Pairwise against the rival above, one holder at a time —
                // the whole reason `ReservationHolder` is three states and
                // not a "not ours" flag (#133). A core reserving the room
                // it stands in must not also withdraw the colony from that
                // room for ever: its hold is the clocked family's deadline
                // and the clockless set must not see it.
                let held holder =
                    (RaidState.empty
                     |> raidTick 100 (quiet |> visible outpostRoom (heldBy holder 4000)))
                        .RivalHeld

                Expect.isEmpty
                    (held ReservationHolder.Invader)
                    "the Invader's hold withdraws nothing on its own"

                Expect.isEmpty (held ReservationHolder.Ours) "and neither does our own"

                Expect.equal
                    (held ReservationHolder.Rival)
                    (Map.ofList [ outpostRoom, 100 ])
                    "while the third holder is the one that does"
            }

            test "the conclusion is held through every tick nobody is looking" {
                // The load-bearing half, and the reason this is persisted
                // at all rather than read off each tick's Snapshot: the
                // gate's own effect is to withdraw the creeps that paid for
                // the vision that judged it. A rule re-read from nothing
                // would reopen the room the tick after it shut it, and the
                // colony would walk back into somebody else's room for
                // ever — `standingDown`'s oscillation, arriving through
                // the other trigger.
                let taken = RaidState.empty |> raidTick 100 (quiet |> ownedByRival outpostRoom)

                let blind =
                    (taken, [ 101..130 ]) ||> List.fold (fun state t -> state |> raidTick t quiet)

                Expect.equal
                    blind.RivalHeld
                    (Map.ofList [ outpostRoom, 100 ])
                    "thirty blind ticks leave the last look's conclusion, and its tick, exactly where they stood"
            }

            test "the tick recorded is the look that shut the gate, not the last look" {
                // The trace half of the record (#117's US-20): the number
                // beside the room is the tick the withdrawal began, so an
                // operator can line an income drop up against it months
                // later. A second look that finds the room still taken is
                // not a second withdrawal and must not restamp it — and
                // nothing measures against the tick, so keeping the first
                // costs nothing and moving it would cost the only date
                // there is.
                let twice =
                    RaidState.empty
                    |> raidTick 100 (quiet |> ownedByRival outpostRoom)
                    |> raidTick 140 (quiet |> ownedByRival outpostRoom)

                Expect.equal
                    twice.RivalHeld
                    (Map.ofList [ outpostRoom, 100 ])
                    "the tick the gate shut on, forty ticks after a second look agreed with it"
            }

            test "a room taken again after it was freed is dated by the second withdrawal" {
                // The other side of the rule above: the tick is the
                // *current* withdrawal's, not the room's first ever, so a
                // room that came back and was taken again dates from the
                // taking that is holding it now.
                let again =
                    RaidState.empty
                    |> raidTick 100 (quiet |> ownedByRival outpostRoom)
                    |> raidTick 120 (quiet |> visible outpostRoom None)
                    |> raidTick 300 (quiet |> ownedByRival outpostRoom)

                Expect.equal
                    again.RivalHeld
                    (Map.ofList [ outpostRoom, 300 ])
                    "the look that shut it this time, and not the one whose gate has been cleared"
            }

            test "only a tick with vision takes a room back out" {
                // "Until it is seen again" is the rule ADR 0043 gives, and
                // it is written on vision in both directions: a look that
                // finds the room free is as good evidence as the look that
                // found it taken.
                //
                // In the live colony that second look is not something the
                // bot can arrange — a room this holds shut is not scanned,
                // so nothing goes there to see it — which makes the
                // clockless withdrawal effectively permanent until a human
                // moves the declaration. That is ADR 0043's own reading of
                // it: not a threat that passes, but a room that stopped
                // being ours to work.
                let freed =
                    RaidState.empty
                    |> raidTick 100 (quiet |> ownedByRival outpostRoom)
                    |> raidTick 101 (quiet |> visible outpostRoom None)

                Expect.isEmpty freed.RivalHeld "the room the colony can see is nobody else's again"
            }

            test "a colony nobody has taken anything from remembers nothing" {
                // The home room is in `RoomControl` on every tick with
                // vision and is the colony's own, so a rule reading
                // "somebody holds this" the wrong way round would withdraw
                // the colony from itself.
                let home =
                    RaidState.empty
                    |> raidTick
                        100
                        { quiet with
                            RoomControl =
                                Map.ofList
                                    [
                                        raidRoom,
                                        {
                                            Owner = Ownership.Ours
                                            Reservation = None
                                        }
                                    ]
                        }

                Expect.isEmpty home.RivalHeld "the room we own is not a room somebody took"
            }
        ]

[<Tests>]
let gateTests =
    testList
        "the stand-down gate"
        [
            test "a running clock shuts its room, and the tick it runs out opens it" {
                // The gate reads the log the way `observe.mjs outposts`
                // reads it: shut while the tick is short of the expiry,
                // and the expiry is the first tick the room may be
                // re-entered.
                let state = RaidState.empty |> raidTick 100 (seen [ core outpostRoom (Some 900) ])

                Expect.equal
                    (standDown 899 state)
                    (Set.singleton outpostRoom)
                    "the tick before the clock runs out the room is still withheld"

                Expect.isEmpty
                    (standDown 900 state)
                    "on the expiry itself the room is back in the set the shell scans"

                Expect.isEmpty (standDown 5000 state) "and stays there"
            }

            test "each outpost's gate is its own" {
                // ADR 0043's independent gates: W12S27 standing down says
                // nothing about W13S28. Two rooms, one core each, two
                // clocks that run out at different ticks.
                let other = "W13S28"

                let state =
                    RaidState.empty
                    |> raidTick 100 (seen [ core outpostRoom (Some 200); core other (Some 900) ])

                Expect.equal
                    (standDown 150 state)
                    (Set.ofList [ outpostRoom; other ])
                    "both clocks running, both rooms withheld"

                Expect.equal
                    (standDown 500 state)
                    (Set.singleton other)
                    "the room whose clock ran out is back on its own, and the other is still shut"
            }

            test "a room in another player's hands is shut by no clock at all" {
                // The two triggers meet in one set, and only here: the
                // clocked family carries an expiry the gate compares
                // against, the clockless one carries nothing to compare.
                let state = RaidState.empty |> raidTick 100 (quiet |> ownedByRival outpostRoom)

                Expect.equal
                    (standDown 101 state)
                    (Set.singleton outpostRoom)
                    "shut the tick after it was seen taken"

                Expect.equal
                    (standDown 1_000_000 state)
                    (Set.singleton outpostRoom)
                    "and shut a million ticks later: there is no clock for this one to run out"
            }

            test "an empty log withholds nothing" {
                // The colony's ordinary state, and the one it has run in
                // since ADR 0042 filled the declaration: no outpost has
                // ever held a core, so the gate is open and the shell
                // scans every room a human declared.
                Expect.isEmpty
                    (standDown 100 RaidState.empty)
                    "nothing is recorded, nothing is shut"
            }
        ]

/// The CPU line as (tick, ms) pairs, oldest first — the shape
/// `observe.mjs cpu` reads a mean and a max off.
let private line (state: CpuState) =
    state.Ticks |> List.map (fun sample -> sample.Tick, sample.Ms)

/// Each row's phase split, oldest first — `None` for a row written by a
/// bundle that did not measure the boundaries (#170).
let private splits (state: CpuState) =
    state.Ticks |> List.map (fun sample -> sample.Phases)

/// A tick that cost `ms` in total and whose boundaries were all read at the
/// end of it. The total is the last reading, so the rows these tests fold
/// carry exactly the costs they carried before the phases arrived, and the
/// window's shape stays the one the trigger is judged over.
let private costing (ms: float) =
    {
        AtEntry = 0.0
        AtSnapshot = ms
        AtDecide = ms
        AtSave = ms
        AtExecute = ms
        Intents = 0
    }

[<Tests>]
let cpuTests =
    testList
        "observe fold: the CPU line"
        [
            test "every tick writes a row, quiet or not, oldest first" {
                // Unlike the Transition log there is no change detection:
                // two ticks that cost the same are two rows, because the
                // distribution is the whole point (ADR 0041).
                let state =
                    CpuState.empty
                    |> foldCpu capCpuTicks 100 (costing 21.0)
                    |> foldCpu capCpuTicks 101 (costing 21.0)

                Expect.equal
                    (line state)
                    [ 100, 21.0; 101, 21.0 ]
                    "both ticks are recorded, in the order they ran"
            }

            test "a tick that finished no loop leaves a gap, not a row" {
                // The row carries its own tick, so a tick the loop threw on
                // — writing nothing — is visible as a missing number rather
                // than as a cheap tick that never happened.
                let state =
                    CpuState.empty
                    |> foldCpu capCpuTicks 100 (costing 21.0)
                    |> foldCpu capCpuTicks 102 (costing 19.5)

                Expect.equal
                    (line state)
                    [ 100, 21.0; 102, 19.5 ]
                    "tick 101 is absent; nothing is invented for it"
            }

            test "the ring keeps the newest cap-many ticks" {
                let state =
                    (CpuState.empty, [ 1..5 ])
                    ||> List.fold (fun state t -> foldCpu 3 t (costing (float t)) state)

                Expect.equal
                    (line state)
                    [ 3, 3.0; 4, 4.0; 5, 5.0 ]
                    "the oldest rows fall off the front, the sibling channels' convention"
            }

            test "a cost is kept to the microsecond" {
                // Finer than the profiler's own 100µs sampling interval, so
                // nothing a reader could act on is lost; the digits past it
                // are Memory paid for noise.
                let state =
                    CpuState.empty
                    |> foldCpu capCpuTicks 100 (costing 21.2345674)
                    |> foldCpu capCpuTicks 101 (costing 8.0009)

                Expect.equal
                    (line state)
                    [ 100, 21.235; 101, 8.001 ]
                    "each cost rounds to three decimal places"
            }

            test "the readings are differenced into phases, the entry alone" {
                // The shape of a live tick the day the split was built: an
                // engine prelude already spent before `loop` runs, then the
                // Snapshot, `decide`, the Memory writes and the Executor's
                // intents (#170). The engine's counter is cumulative and
                // every phase is a difference — except the entry, which is
                // the prelude itself and is carried as it was read.
                let state =
                    CpuState.empty
                    |> foldCpu
                        capCpuTicks
                        141584
                        {
                            AtEntry = 0.4
                            AtSnapshot = 3.4
                            AtDecide = 44.2
                            AtSave = 46.0
                            AtExecute = 49.4
                            Intents = 44
                        }

                Expect.equal
                    (splits state)
                    [
                        Some
                            {
                                Entry = 0.4
                                Snapshot = 3.0
                                Decide = 40.8
                                Save = 1.8
                                Execute = 3.4
                                Intents = 44
                            }
                    ]
                    "each phase is the ground it covers, not the counter it ended at"

                Expect.equal
                    (line state)
                    [ 141584, 49.4 ]
                    "the tick's total is the last reading — the number the trigger has always judged"
            }

            test "a phase is kept to the microsecond, like the total" {
                // The differences are rounded the same way the total is, so
                // a phase never arrives with the float noise of a
                // subtraction: the digits Memory pays for are the ones a
                // reader could act on.
                let state =
                    CpuState.empty
                    |> foldCpu
                        capCpuTicks
                        100
                        {
                            AtEntry = 0.1234564
                            AtSnapshot = 1.2345674
                            AtDecide = 2.0009
                            AtSave = 2.0015
                            AtExecute = 3.9999996
                            Intents = 1
                        }

                Expect.equal
                    (splits state)
                    [
                        Some
                            {
                                Entry = 0.123
                                Snapshot = 1.111
                                Decide = 0.766
                                Save = 0.001
                                Execute = 1.998
                                Intents = 1
                            }
                    ]
                    "every phase rounds to three decimal places"
            }

            test "a tick the engine took no intent on says nothing was taken" {
                // Zero is a measurement here, unlike an absent phase group:
                // a tick with no accepted intent is the one shape that
                // proves the engine's 0.2-per-intent charge is not what the
                // tick cost.
                let state = CpuState.empty |> foldCpu capCpuTicks 100 (costing 21.0)

                Expect.equal
                    (splits state |> List.map (Option.map (fun phases -> phases.Intents)))
                    [ Some 0 ]
                    "the count rides the row at zero rather than going missing"
            }

            test "a row written before the phases keeps its absence" {
                // What the ring holds for the first hundred ticks after the
                // split is deployed, and what a rollback puts back in it.
                // The old row keeps its total — the window the trigger is
                // read over never shortens — and its phases stay absent
                // rather than being filled with zeros, which would say the
                // Snapshot cost nothing rather than that nobody measured it.
                let unsplit =
                    {
                        Ticks = [ { Tick = 99; Ms = 6.1; Phases = None } ]
                    }

                let state = unsplit |> foldCpu capCpuTicks 100 (costing 21.0)

                Expect.equal
                    (line state)
                    [ 99, 6.1; 100, 21.0 ]
                    "the older row rides on with the cost it was written with"

                Expect.equal
                    (splits state |> List.map Option.isSome)
                    [ false; true ]
                    "absence is preserved, and only the new row is split"
            }
        ]
