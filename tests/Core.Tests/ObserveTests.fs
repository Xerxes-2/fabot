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
                            Verdict.Released(
                                "a",
                                "t1",
                                ReleaseReason.Rejected RejectReason.Unreachable
                            )
                            Verdict.Unassigned("a", IdleReason.NoneReachable)
                        ]

                Expect.equal
                    (timeline "a" state)
                    [
                        5, Verdict.Matched("a", "t1", MatchFactor.Rank)
                        6, Verdict.Grounded "a"
                        7,
                        Verdict.Released("a", "t1", ReleaseReason.Rejected RejectReason.Unreachable)
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

/// The room the raid fixtures project — the colony's own, and the only
/// room `ColonyView.Hostiles` swept until #201 widened it to every room the
/// colony works and can see. Named rather than left to
/// `SpatialInfo.homeName`'s empty string, because the closest approach
/// joins a hostile's room to the projection's layer (ADR 0041) and a
/// fixture whose two halves agreed by both being blank would prove nothing.
let raidRoom = "W12S28"

/// A room of the scan set that is not the colony's own: where the
/// stand-down family's cores stand, and — since #201 — where a raider the
/// sweep now reaches can stand too. The two families are told apart by the
/// room a record names and never by both being blank.
let outpostRoom = "W12S27"

/// A colony nobody is raiding: the Raid fold reads hostiles, our creeps
/// and the tiles of what is ours, so everything else stays empty.
let quiet: ColonyView =
    {
        Time = 100
        Spawns = []
        Bank = { Available = 0; Capacity = 0 }
        Refillables = []
        Sources = []
        Controller = None
        // The Raid fold prices no source, so who holds the room is nothing
        // it reads (ADR 0042).
        RoomControl = Map.empty
        HeldOutposts = Set.empty
        ThreatenedOutposts = Set.empty
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
        Declared = []
        // Nor a [[stage]], for the same reason: what a colony is old
        // enough to do (ADR 0052 decision 3) is not what happened to it.
        Stages = Map.empty
        // Another colony's bodies are in no raid of this one's: the log
        // counts the creeps this colony lost (ADR 0028), and a body it
        // does not hold is not one of them (ADR 0052 decision 1).
        Foreign = Set.empty
        // And nothing is borrowed: what one colony may take of a child's
        // room decides Tasks, and the log records what happened.
        Borrowed = { Rooms = [] }
        // And nothing refused: the log records what happened, and a room
        // no Seam reaches has nobody in it for anything to happen to (#243).
        Refused = []
        // And no [[errand]]: an errand is a room a human declared because
        // one named object out there has to be acted on (ADR 0060 decision
        // 1), and this fixture declares none — so no Reclaim is pooled and
        // no seat of the reserver row is the re-claimer's (#318).
        Errands = []
        Consignee = None
        Crossed = Set.empty
        Reactors = []
        // And nothing remembered of a room it cannot see: the log records
        // what happened, and a vision grace changes which assignment a tick
        // holds and never what a tick did (#151).
        Sightings = Map.empty
        // The numbers this bot ships with (ADR 0052 decision 5): a
        // fixture starts from them and the tests that are *about* a
        // tunable move the one field they are about.
        Tuning = Tuning.defaults
        // Nothing in the oven: a fixture's rows count what is alive, and
        // the casting cascade's own tests are the ones that put a body
        // here (#156).
        Casting = []
    }

/// A hostile creep of the given owner and body standing on a tile of the
/// colony's own room.
let raider id owner pos body : HostileInfo =
    {
        Id = id
        Owner = owner
        Pos = RoomPos.at raidRoom pos
        Body = body
        TicksToLive = Engine.creepLifetime
    }

/// One of ours with a full life ahead of it; immaterial but for its name.
/// Whatever becomes of it, its own clock is not what did it.
let ours name : CreepInfo =
    {
        Name = name
        TicksToLive = 500
        Hits = { Hits = 300; HitsMax = 300 }
        Fatigue = 0
        Energy = 0
        Thorium = 0
        FreeCapacity = 50
        Moved = false
        Body = Map.ofList [ Work, 1; Carry, 1; Move, 1 ]
    }

/// One of ours on the last tick of its life: the engine's counter runs out
/// on it, so it is gone next tick whatever the raiders do.
let spent name = { ours name with TicksToLive = 1 }

/// The squad of #66, cut to the one creep the lifecycle tests need.
let squad = [ raider "TWX" "giaco" { X = 38; Y = 47 } [ Tough; Attack; Move ] ]

/// The same body on the same tile, a room away: the raider #201's widened
/// sweep reaches (`ColonyView.Hostiles` covers every room the colony works
/// and can see). The room is the only difference from `squad`, which is
/// what makes the pair able to ask which of the fold's answers are the
/// colony's and which are one room's.
let outpostSquad =
    squad
    |> List.map (fun hostile ->
        { hostile with
            Pos = RoomPos.at outpostRoom (RoomPos.pos hostile.Pos)
        })

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

/// The colony at the given tick under a five-tick [[quiet gap]], so an
/// episode's close is exercised in a few ticks rather than in fifty. The
/// gap is the colony's own tunable since #216 R4 (ADR 0052 decision 5), so
/// a fixture that wants a short one says so on the view.
let atShortGap t (colony: ColonyView) =
    { colony with
        Time = t
        Tuning = { colony.Tuning with QuietGap = 5 }
    }

/// Fold one Raid-log tick over a colony at the given tick, with a small
/// ring cap and a short quiet gap so both are exercised in a few ticks
/// rather than a few hundred.
///
/// The world holds exactly this colony's creeps, which is the one-colony
/// world every test but `adoptionTests` below is written in: there is
/// nobody else to have adopted a name that left the ColonyView, so a name
/// that leaves it left the world.
let raidTick t (colony: ColonyView) state =
    let alive = colony.Creeps |> List.map (fun creep -> creep.Name) |> Set.ofList
    foldRaids 3 alive (atShortGap t colony) state

/// The same fold with the world said separately from the colony: what the
/// shell hands in since #191, where a ColonyView carries one colony's fleet
/// and `Game.creeps` carries everyone's (ADR 0047).
let raidTickIn alive t (colony: ColonyView) state =
    foldRaids 3 (Set.ofList alive) (atShortGap t colony) state

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

            test "the quiet gap is the colony's tunable: the same six ticks are one raid or two" {
                // `Tuning.QuietGap` (ADR 0052 decision 5), pairwise over
                // the one field on one history: a squad seen at t10 and
                // again at t16. At a five-tick gap the second sighting is
                // six ticks of silence later and opens a second episode; at
                // a seven-tick gap it is the raid that is still open.
                //
                // A tunable and not an engine number: what it decides is
                // whether an absence is a squad healing off-room or a squad
                // that has left (#66's poke-and-heal, ~220 ticks of one
                // raid), and the fifty this bot ships with is that
                // judgement rather than anything the server says.
                let episodes gap =
                    (RaidState.empty, [ 10..16 ])
                    ||> List.fold (fun state t ->
                        let colony = if t = 10 || t = 16 then raid squad else quiet

                        foldRaids
                            3
                            (colony.Creeps |> List.map (fun c -> c.Name) |> Set.ofList)
                            { colony with
                                Time = t
                                Tuning = { colony.Tuning with QuietGap = gap }
                            }
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
                                Pos = RoomPos.at raidRoom { X = 38; Y = 47 }
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
                        Pos = RoomPos.at "W12S27" { X = 9; Y = 46 }
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
                // ADR 0047 decision 2 through this channel. Since #191 a
                // ColonyView carries one colony's creeps, so a name can leave
                // it two ways — its creep died, or the colony next door
                // adopted the body for the tick it stands in a room only
                // that colony projects. Only the first is what the raid
                // cost, and the world's own list is what tells them apart.
                //
                // Pairwise against the loss above it, one fact moved: the
                // same name gone from the same ColonyView at t11, once still
                // in `Game.creeps` and once not.
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
                    [ { Creep = "w2"; Tick = 10 } ]
                    "and the same absence with the name gone from the world is the loss it always was"
            }

            test
                "a creep crossing back and forth all raid is charged once for each death, and never for a crossing" {
                // The shape that made this worth a rule rather than a
                // sentence: `Losses` appends, so a hauler shuttling across
                // the [[seam]] during a 200-tick siege would file a fresh
                // phantom kill on every tick it left the fleet.
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

/// The colony with one structure of the given kind standing at the given
/// hits — what the damage fold reads: the kind decides whether it is
/// charged, the number is what moves tick over tick.
let withHits id kind hits (colony: ColonyView) =
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

            test "a raid a room away opens an episode and is charged none of this room's decay" {
                // #201 widened the sweep to every room the colony works, so
                // an outpost's raider opens a colony episode — that is what
                // the widening is for, and the record the operator reads it
                // off. Damage is the one field that cannot follow it: the
                // Keep and its ramparts stand in the colony's own room (ADR
                // 0034), so a window held open from next door would charge
                // 3 hits a tick per rampart of ordinary decay as what a
                // raid that never touched the Keep cost — noise that is the
                // whole of the number rather than the rounding error the
                // field's own doc prices it as.
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

                // Pairwise, the same body on the same tile filed at home:
                // the room is the only difference between the two runs, so
                // nothing but the room the damage is measured in separates
                // them.
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

/// A hostile of the raid standing in a room other than the colony's own, with
/// a full Invader life: what ADR 0043's clock reads for a raid with no core in
/// it (#257). Each carries an id of its own, a raid being a roster.
let raiderIn room i body : HostileInfo =
    {
        Id = $"raid-{i}"
        Owner = "Invader"
        Pos = RoomPos.at room { X = 25; Y = 25 }
        Body = body
        TicksToLive = Engine.creepLifetime
    }

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
let visible room reservation (colony: ColonyView) =
    { colony with
        RoomControl =
            colony.RoomControl
            |> Map.add
                room
                {
                    Owner = Ownership.Unowned
                    Reservation = reservation
                    SafeMode = false
                }
    }

/// A reservation on that controller, carrying the engine's own *relative*
/// count of what is left to run on it.
let heldBy holder ticks =
    Some { Holder = holder; TicksToEnd = ticks }

/// The room as vision answers for it when another player owns the
/// controller outright — ADR 0043's clockless withdrawal, and since #165 the
/// whole of it: a rival's reservation beside this one is a clock.
let ownedByRival room (colony: ColonyView) =
    { colony with
        RoomControl =
            colony.RoomControl
            |> Map.add
                room
                {
                    Owner = Ownership.Rival
                    Reservation = None
                    SafeMode = false
                }
    }

/// The outpost room as a declared one: its controller is placed and classified
/// in the projection, which is the fact both the guard row and the raid
/// stand-down use to distinguish work from a room the colony merely crosses.
let withDeclaredOutpost room (colony: ColonyView) =
    let controller = $"ctrl-{room}"
    let layer = SpatialInfo.layerOf colony.Spatial room

    { colony with
        Spatial =
            { colony.Spatial with
                Rooms =
                    Map.add
                        room
                        { layer with
                            TargetPositions =
                                Map.add controller { X = 25; Y = 25 } layer.TargetPositions
                        }
                        colony.Spatial.Rooms
                TargetKinds = Map.add controller Controller colony.Spatial.TargetKinds
            }
    }

/// The recorded stand-downs as (room, opened, last seen, expiry, basis),
/// oldest first — the whole of what the outpost family records.
let standDowns (state: RaidState) =
    state.Outposts
    |> List.map (fun e -> e.RoomName, e.Opened, e.LastSeen, e.Expiry, e.Basis)

/// The rooms the gate withholds from the work at a tick, under the numbers the
/// bot ships. Since #165 the gate answers with two sets and this is the one ADR
/// 0043 wrote every pin below against: which rooms the colony does not work.
let shutAt tick state =
    (standDown Tuning.defaults tick state).Shut

/// The gate's other half (#165): the latched rooms this tick takes one look
/// into — a subset of `shutAt`'s answer and never a room leaving it.
let recheckedAt tick state =
    (standDown Tuning.defaults tick state).Rechecked

/// The gate's third set (#333), which withholds no room at all: the outposts
/// somebody else's reservation was standing on at the last look and whose hold
/// this tick is still short of. What the view hands to
/// `Planner.reservableControllers` on the ticks vision answers for nothing.
let heldAt tick state =
    (standDown Tuning.defaults tick state).HeldOutposts

/// The gate's fourth set (#366), which withholds no room either: the declared
/// outposts an armed [[threat]] was standing in at the last look and whose
/// memory this tick is still short of. What the view hands to
/// `Planner.guardedOutposts` and `Quota.guardsWanted` on the ticks the raid has
/// killed everything of ours that could see the room.
let threatenedAt tick state =
    (standDown Tuning.defaults tick state).ThreatenedOutposts

/// An armed raider in a room: ADR 0033's Threat, the only hostile #366's
/// memory is written for.
let armedIn room = raiderIn room 1 [ Move; Attack ]

/// The same raid with no weapon on it: a healer is a hostile the [[fire
/// reflex]] shoots and the [[raid log]] records, and no reason to buy a guard.
let healerIn room = raiderIn room 2 [ Move; Heal ]

/// A declared outpost the colony is **looking into** this tick, holding
/// whatever hostiles the case names — the two facts #366's memory is written
/// from, said together because either alone writes nothing.
let lookingAt room hostiles (colony: ColonyView) =
    { (colony |> withDeclaredOutpost room |> visible room None) with
        Hostiles = hostiles
    }

[<Tests>]
let outpostTests =
    testList
        "raid fold: outpost episodes"
        [
            test "a raid two guards cannot beat stands the room down to its own life" {
                // #257. ADR 0043 clocked a stand-down off an invader *core*
                // and off nothing else, so a raid of plain creeps offered no
                // deadline at all: W13S29 stayed open through one, the rows
                // went on hiring into it, and in three hundred ticks it took
                // two reservers and a guard while the invaders kept full
                // health. An Invader in a room nobody owns never suicides, so
                // what it has left is exactly what it will spend and that is
                // the clock.
                //
                // Read only for a raid the guard row's cap cannot beat, which
                // is what keeps the withdrawal from cancelling ADR 0056 before
                // it fights: shutting a room takes it out of the scan set, so a
                // raid that shut it on sight would hide its own hostiles and
                // buy no guard at all. Pairwise on the raid's size alone.
                let raidIn hostiles =
                    { (quiet |> withDeclaredOutpost outpostRoom) with
                        Hostiles = hostiles |> List.mapi (raiderIn outpostRoom)
                    }

                let overwhelming = List.replicate 5 [ Attack; Attack; Attack; Move; Move; Move ]

                Expect.equal
                    (standDowns (RaidState.empty |> raidTick 100 (raidIn overwhelming)))
                    [ outpostRoom, 100, 100, 1600, StandDownBasis.InvaderRaid ]
                    "five attackers beat two blocks, so the room is left for the fifteen hundred they have"

                Expect.isEmpty
                    (standDowns (
                        RaidState.empty
                        |> raidTick
                            100
                            (raidIn
                                [
                                    [
                                        Tough
                                        Tough
                                        Move
                                        Move
                                        Move
                                        Move
                                        Move
                                        RangedAttack
                                        Work
                                        Attack
                                    ]
                                ])
                    ))
                    "and the lone smallMelee two blocks beat opens no stand-down: that room is a fight"
            }

            test "two melee blocks cannot use self-heal to win an equal exchange" {
                let raid attacks =
                    { (quiet |> withDeclaredOutpost outpostRoom) with
                        Hostiles =
                            List.replicate
                                2
                                (List.replicate attacks Attack @ List.replicate (10 - attacks) Move)
                            |> List.mapi (raiderIn outpostRoom)
                    }

                Expect.isEmpty
                    (standDowns (RaidState.empty |> raidTick 100 (raid 2)))
                    "two blocks kill 2,000 hits before 120 damage kills them"

                Expect.equal
                    (standDowns (RaidState.empty |> raidTick 100 (raid 3)))
                    [ outpostRoom, 100, 100, 1600, StandDownBasis.InvaderRaid ]
                    "equal 180 damage and 2,000 hits is not a win; fictitious self-heal must not keep the room open"
            }

            test "a transit-room hostile opens no stand-down, regardless of owner" {
                // #324. W15S26 is projected only because the Errand's chain
                // crosses it. Its hostiles stay in the view so a walker can
                // Flee, but this gate can neither garrison the room nor
                // withhold any work in it. The ordinary Invader is the
                // asymmetric case: filtering only the expected Source Keeper
                // would make this half fail.
                let transitRoom = "W15S26"
                let overwhelming = List.replicate 5 [ Attack; Attack; Attack; Move; Move; Move ]

                let raidBy owner =
                    { quiet with
                        Hostiles =
                            overwhelming
                            |> List.mapi (raiderIn transitRoom)
                            |> List.map (fun hostile -> { hostile with Owner = owner })
                    }

                for owner in [ "Source Keeper"; "Invader"; "Shibdib" ] do
                    Expect.isEmpty
                        (standDowns (RaidState.empty |> raidTick 100 (raidBy owner)))
                        $"{owner} in a room the colony only crosses is no stand-down"
            }

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
                    (heldFor Tuning.defaults.StandDownFallback)
                    [
                        outpostRoom,
                        100,
                        100,
                        100 + Tuning.defaults.StandDownFallback,
                        StandDownBasis.Reservation
                    ]
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
                    (basisOf (heldFor Tuning.defaults.StandDownFallback))
                    "the two sides of the floor are told apart by the reason, not only by the tick"
            }

            test
                "the fallback clock is the colony's tunable, and it is both the floor and the answer" {
                // `Tuning.StandDownFallback` (ADR 0052 decision 5),
                // pairwise over the one field: it is read twice in the same
                // rule — as the deadline a threat gave no readable clock
                // for, and as the floor under a hold too short to believe —
                // so moving it has to move both answers together or the
                // second reading is a literal wearing the first one's name.
                let shut fallback ticks =
                    let colony =
                        seen [ core outpostRoom None ]
                        |> visible outpostRoom (heldBy ReservationHolder.Invader ticks)

                    RaidState.empty
                    |> raidTick
                        100
                        { colony with
                            Tuning =
                                { colony.Tuning with
                                    StandDownFallback = fallback
                                }
                        }
                    |> standDowns

                Expect.equal
                    (shut 2500 300)
                    [ outpostRoom, 100, 100, 2600, StandDownBasis.Fallback ]
                    "at the shipped 2,500 a three-hundred-tick hold is unreadable and the colony's own clock answers"

                Expect.equal
                    (shut 200 300)
                    [ outpostRoom, 100, 100, 400, StandDownBasis.Reservation ]
                    "at a fallback of 200 the same hold clears the floor and reads through as a reservation"

                Expect.equal
                    (shut 200 100)
                    [ outpostRoom, 100, 100, 300, StandDownBasis.Fallback ]
                    "and the fallback is still the answer under its own floor: 200 ticks from now, said as the colony's choice"
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
                // and ADR 0043 answers those two differently. Both fall back
                // rather than reading a deadline off a hold that is not the
                // threat's.
                //
                // Since #165 a rival's hold is a deadline of its own — 300
                // ticks here — and this is where the two families meet: the
                // ring keeps the **later** of the reads a tick produces for
                // one room, so the rival's short clock can never cut a core's
                // stand-down short.
                let held holder =
                    RaidState.empty
                    |> raidTick
                        100
                        (seen [ core outpostRoom None ] |> visible outpostRoom (heldBy holder 300))
                    |> standDowns

                Expect.equal
                    (held ReservationHolder.Rival)
                    [ outpostRoom, 100, 100, 2600, StandDownBasis.Fallback ]
                    "a player's 300-tick hold is no deadline for the core, and never shortens its clock"

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
                let sequence (colony: ColonyView) =
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
                // **Ownership alone since #165.** Owned and reserved are one
                // fact to the economics (ADR 0042) and two facts to a gate
                // that has to say when the room comes back: the engine ends a
                // reservation and ends nothing about an owner. So the
                // reservation moved to the ring, and the pairwise contrast is
                // one room, one look, one field of the control entry apart.
                let owned = RaidState.empty |> raidTick 100 (quiet |> ownedByRival outpostRoom)

                let reserved =
                    RaidState.empty
                    |> raidTick
                        100
                        (quiet |> visible outpostRoom (heldBy ReservationHolder.Rival 4000))

                Expect.equal
                    owned.RivalHeld
                    (Map.ofList [ outpostRoom, { Since = 100; LastLooked = 100 } ])
                    "an owner that is not us, against the tick the look was taken on"

                Expect.isEmpty
                    reserved.RivalHeld
                    "and a reservation that is not ours latches nothing: it runs out on its own"

                Expect.isEmpty owned.Outposts "no episode opened: there is no clock to run"

                Expect.equal
                    (standDowns reserved)
                    [ outpostRoom, 100, 100, 4100, StandDownBasis.RivalReservation ]
                    "while the reservation is an episode of the ring, clocked to the hold's own end"
            }

            test "the NPC's hold is a clock and never an exit" {
                // Pairwise, one holder at a time — the whole reason
                // `ReservationHolder` is three states and not a "not ours"
                // flag (#133). Since #165 no holder latches: a reservation of
                // anybody's ends on a tick the engine counts down, and the
                // three answers are three *clocks* — the NPC's read off the
                // core standing there, the rival's off the hold itself, and
                // ours no clock at all because the room is being worked by us.
                let folded holder =
                    RaidState.empty
                    |> raidTick 100 (quiet |> visible outpostRoom (heldBy holder 4000))

                let held holder = (folded holder).RivalHeld

                Expect.isEmpty
                    (held ReservationHolder.Invader)
                    "the Invader's hold withdraws nothing on its own"

                Expect.isEmpty (held ReservationHolder.Ours) "and neither does our own"

                Expect.isEmpty
                    (held ReservationHolder.Rival)
                    "nor does the third: since #165 the latch is ownership's alone"

                // The NPC's hold is read off the *core* and never off the
                // controller (`deadlineOf`), so a hold with no core standing
                // in the room opens nothing — where a rival's hold is read off
                // the controller itself and opens an episode on the spot.
                Expect.isEmpty
                    (standDowns (folded ReservationHolder.Invader))
                    "no core is standing there, so the NPC's hold is nobody's deadline this tick"

                Expect.equal
                    (standDowns (folded ReservationHolder.Rival))
                    [ outpostRoom, 100, 100, 4100, StandDownBasis.RivalReservation ]
                    "while the rival's hold is a stand-down running to the end of that hold"

                Expect.isEmpty
                    (standDowns (folded ReservationHolder.Ours))
                    "and our own hold is the steady state of every outpost, not a withdrawal"
            }

            test "the conclusion is held through every tick nobody is looking" {
                // The load-bearing half, and the reason this is persisted
                // at all rather than read off each tick's ColonyView: the
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
                    (Map.ofList [ outpostRoom, { Since = 100; LastLooked = 100 } ])
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
                    (Map.ofList [ outpostRoom, { Since = 100; LastLooked = 100 } ])
                    "the tick the gate shut on, forty ticks after a second look agreed with it — and no look fell due in between (#275)"
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
                    (Map.ofList [ outpostRoom, { Since = 300; LastLooked = 300 } ])
                    "the look that shut it this time, and not the one whose gate has been cleared"
            }

            test "only a tick with vision takes a room back out" {
                // "Until it is seen again" is the rule ADR 0043 gives, and
                // it is written on vision in both directions: a look that
                // finds the room free is as good evidence as the look that
                // found it taken.
                //
                // In the live colony that second look used to be something
                // the bot could not arrange — a room this holds shut is not
                // scanned, so nothing went there to see it — which made the
                // withdrawal permanent until a human hand-edited the leaf.
                // #165 arranges it: the gate re-admits a latched room to the
                // **scan** once every `Tuning.RivalRecheck` ticks, and this
                // fold is what such a look lands in. The rule here is
                // unchanged and is why that was enough — a look that finds
                // the room free has always taken it back out.
                let freed =
                    RaidState.empty
                    |> raidTick 100 (quiet |> ownedByRival outpostRoom)
                    |> raidTick 101 (quiet |> visible outpostRoom None)

                Expect.isEmpty freed.RivalHeld "the room the colony can see is nobody else's again"
            }

            test "the look that falls due moves the stride, and never the date the gate shut on" {
                // #275. The stride is measured between *looks*, so the tick a
                // look was taken on is the one the record has to carry — and
                // it is carried beside the shutting tick rather than over it,
                // because the two answer different questions: one dates an
                // income drop for an operator (#117's US-20), the other says
                // when the colony next questions its own conclusion.
                //
                // The look is stamped whether or not vision answered. The gate
                // re-admits the room to the scan for that tick and the colony
                // may well be blind in it — which is the common case, the
                // withdrawal itself having taken the vision away — and a look
                // stamped only when it saw something would leave a blind room
                // re-admitted on every tick from the stride onwards.
                let recheck = Tuning.defaults.RivalRecheck
                let state = RaidState.empty |> raidTick 100 (quiet |> ownedByRival outpostRoom)

                Expect.equal
                    state.RivalHeld
                    (Map.ofList [ outpostRoom, { Since = 100; LastLooked = 100 } ])
                    "the look that shut the gate is the last look taken, so far"

                // Forty ticks late, which is the whole of what this ticket
                // fixes: a tick the gate was not evaluated on delays the look
                // rather than cancelling it.
                let looked = state |> raidTick (100 + recheck + 40) quiet

                Expect.equal
                    looked.RivalHeld
                    (Map.ofList
                        [
                            outpostRoom,
                            {
                                Since = 100
                                LastLooked = 100 + recheck + 40
                            }
                        ])
                    "the blind look is taken all the same, and the date of the withdrawal stands"

                Expect.isEmpty
                    (recheckedAt (100 + recheck + 41) looked)
                    "so the tick after the look, nothing is looked into"

                Expect.equal
                    (recheckedAt (100 + 2 * recheck + 40) looked)
                    (Set.singleton outpostRoom)
                    "and the next look falls a stride after the look, not a stride after the shutting"
            }

            test "a look with vision that agrees moves the stride and clears nothing" {
                // The look that finds the rival still there: the latch stands,
                // its date stands, and the stride runs again from this look.
                // The one thing that separates it from the blind look above is
                // that it could have cleared the latch and did not.
                let recheck = Tuning.defaults.RivalRecheck

                let agreed =
                    RaidState.empty
                    |> raidTick 100 (quiet |> ownedByRival outpostRoom)
                    |> raidTick (100 + recheck) (quiet |> ownedByRival outpostRoom)

                Expect.equal
                    agreed.RivalHeld
                    (Map.ofList
                        [
                            outpostRoom,
                            {
                                Since = 100
                                LastLooked = 100 + recheck
                            }
                        ])
                    "still theirs, shut since the same tick, and looked into on the stride"
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
                                            SafeMode = false
                                        }
                                    ]
                        }

                Expect.isEmpty home.RivalHeld "the room we own is not a room somebody took"
            }
        ]

[<Tests>]
let holdTests =
    testList
        "raid fold: the reservation somebody else is standing on"
        [
            test "a hold that is not ours is recorded against the tick it runs out on" {
                // #333's record, and the three answers `ReservationHolder`
                // gives read one at a time. What goes in the leaf is the
                // fact the reserver row now refuses to hire against — the
                // engine answers ERR_INVALID_TARGET on a controller anybody
                // but us holds — so the channel can say *why* a declared
                // outpost is being mined and not reserved.
                //
                // The tick is absolute, the engine's countdown being
                // relative: 100 + 4,000. Stored as read it would date a hold
                // to the start of the world.
                let recorded holder =
                    (RaidState.empty
                     |> raidTick 100 (quiet |> visible outpostRoom (heldBy holder 4000)))
                        .Holds

                Expect.equal
                    (recorded ReservationHolder.Invader)
                    (Map.ofList
                        [
                            outpostRoom,
                            {
                                Holder = ReservationHolder.Invader
                                Until = 4100
                            }
                        ])
                    "the Invader's hold — W12S27's own case, the core long since collapsed"

                Expect.equal
                    (recorded ReservationHolder.Rival)
                    (Map.ofList
                        [
                            outpostRoom,
                            {
                                Holder = ReservationHolder.Rival
                                Until = 4100
                            }
                        ])
                    "and another player's, which the engine refuses us in exactly the same words"

                Expect.isEmpty
                    (recorded ReservationHolder.Ours)
                    "our own hold is the steady state of every outpost and is recorded nowhere"

                Expect.isEmpty
                    (RaidState.empty |> raidTick 100 (quiet |> visible outpostRoom None)).Holds
                    "and an unreserved controller is the room the row hires for: no entry either"
            }

            test "the hold outlives the vision that read it, and ends itself" {
                // The two rules the record is kept on, which are the latch's
                // first and the ring's second. A tick without vision leaves
                // the conclusion standing — the colony is blind in most of
                // these rooms most of the time, and re-reading the entry off
                // nothing would clear it the tick after it was written. And
                // the entry ends on the tick it named, no look being needed
                // to know a countdown has run out (ADR 0043's re-entry rule).
                let taken =
                    RaidState.empty
                    |> raidTick
                        100
                        (quiet |> visible outpostRoom (heldBy ReservationHolder.Invader 4000))

                let blind =
                    (taken, [ 101..130 ]) ||> List.fold (fun state t -> state |> raidTick t quiet)

                Expect.equal
                    blind.Holds
                    taken.Holds
                    "thirty blind ticks leave the last look's conclusion exactly where it stood"

                let atEnd =
                    (taken, [ 4099; 4100 ]) ||> List.fold (fun state t -> state |> raidTick t quiet)

                Expect.isEmpty
                    atEnd.Holds
                    "and on the tick the engine's countdown reaches, the record retires itself"

                let short = taken |> raidTick 4099 quiet

                Expect.equal
                    short.Holds
                    taken.Holds
                    "one tick short of it the hold is still standing, blind or not"
            }

            test "a look that finds the controller free takes the room back out" {
                // The other direction, on the same evidence rule: a tick
                // with vision decides the room either way, so the entry that
                // matters is the last look's and never the first. Pinned
                // *before* the clock runs out, or the expiry above would be
                // what cleared it and this would pass on the wrong reason.
                let freed =
                    RaidState.empty
                    |> raidTick
                        100
                        (quiet |> visible outpostRoom (heldBy ReservationHolder.Invader 4000))
                    |> raidTick 101 (quiet |> visible outpostRoom None)

                Expect.isEmpty
                    freed.Holds
                    "the controller the colony can see is nobody else's again"

                // And a hold re-read is re-clocked: the engine counts down at
                // one a tick, so the same hold read 40 ticks later names the
                // same absolute tick, and a *fresh* hold names a later one.
                let reread =
                    RaidState.empty
                    |> raidTick
                        100
                        (quiet |> visible outpostRoom (heldBy ReservationHolder.Invader 4000))
                    |> raidTick
                        140
                        (quiet |> visible outpostRoom (heldBy ReservationHolder.Invader 3960))

                Expect.equal
                    reread.Holds
                    (Map.ofList
                        [
                            outpostRoom,
                            {
                                Holder = ReservationHolder.Invader
                                Until = 4100
                            }
                        ])
                    "the same hold, forty ticks of it spent, still ends on the tick it always did"
            }

            test "the Invader's hold withdraws nothing, and a rival's withdraws the room as well" {
                // The line this record is drawn on, and the whole reason it
                // is a third shape rather than a row of the ring — but the
                // line is the **Invader's** alone, and saying it of "somebody
                // else" would be false of the other half of the very
                // predicate the record is folded on (`heldByOther`).
                //
                // The Invader's leftover hold is the new case: no core stands
                // over it, ADR 0043's ring is clocked off cores, and #165's
                // rival clause does not answer for the NPC. So the room is
                // mined and only the *reservation* is refused. Written as a
                // stand-down it would have withdrawn the room, which is the
                // architectural question #333 leaves for a human.
                //
                // A rival's identical hold is a clocked stand-down already
                // (#165) — the same control entry opens an episode and the
                // gate shuts the room — so for that holder the record says
                // *why* a room that is withheld anyway is also unreservable,
                // and nothing about a room the colony goes on mining. Pinned
                // pairwise, one holder apart, because the sentence in the
                // docs is written one way and is true only one way.
                let heldByWhom holder =
                    RaidState.empty
                    |> raidTick 100 (quiet |> visible outpostRoom (heldBy holder 4000))

                let invader = heldByWhom ReservationHolder.Invader
                let rival = heldByWhom ReservationHolder.Rival

                Expect.isEmpty invader.Outposts "the Invader's leftover hold opens no episode"

                Expect.isEmpty invader.RivalHeld "and latches nothing: the engine ends this hold"

                Expect.isEmpty (shutAt 101 invader) "so the gate withholds that room from nothing"

                Expect.equal
                    (heldAt 101 invader)
                    (Set.singleton outpostRoom)
                    "what it does narrow is the reservation, on every blind tick of the hold"

                Expect.isNonEmpty
                    rival.Outposts
                    "a rival's identical hold is #165's episode: the same entry opens a stand-down"

                Expect.equal
                    (shutAt 101 rival)
                    (Set.singleton outpostRoom)
                    "so that room is withheld from the work, and is not one the colony goes on mining"

                Expect.equal
                    (heldAt 101 rival)
                    (Set.singleton outpostRoom)
                    "and it is in this set too: the two families name one room and say different things"
            }

            test "the gate stops holding a reservation on the tick the engine's countdown reaches" {
                // What the gate does with the record, which is the half the
                // rule the ticket is about actually reads
                // (`Planner.reservableControllers` through
                // `ColonyView.HeldOutposts`). The set is the standing holds
                // and nothing else: `tick < Until`, the same test the fold
                // retires an entry on and `observe.mjs` prints one under, so
                // a leaf the fold has not caught up with — no tick with
                // vision since the hold ended — cannot withhold a
                // reservation the engine would now accept.
                let taken =
                    RaidState.empty
                    |> raidTick
                        100
                        (quiet |> visible outpostRoom (heldBy ReservationHolder.Invader 4000))

                Expect.equal
                    (heldAt 4099 taken)
                    (Set.singleton outpostRoom)
                    "one tick short of the end the hold stands, blind ever since it was read"

                Expect.isEmpty
                    (heldAt 4100 taken)
                    "and on the tick the countdown reaches, the room is ours to reserve again"

                Expect.isEmpty
                    (heldAt 101 RaidState.empty)
                    "a colony with no log at all holds nothing out of its own pool"
            }
        ]

[<Tests>]
let threatMemoryTests =
    testList
        "raid fold: the guard's memory of a raided outpost"
        [
            test "an armed threat seen in a declared outpost is remembered through the blind ticks" {
                // #366. The guard row hires on a threat seen in an outpost,
                // and the bodies that vision comes from — the anchor, the
                // hauler, the reserver — are exactly what the raid kills, so
                // the room goes dark and the row that bought a 15-ATTACK-part
                // body stops asking for it. This is #333's answer in the guard
                // row: the conclusion is written down on the tick with vision
                // and read on the ticks without one.
                let looked =
                    RaidState.empty
                    |> raidTick 100 (quiet |> lookingAt outpostRoom [ armedIn outpostRoom ])

                Expect.equal
                    (looked.Threatened |> Map.tryFind outpostRoom)
                    (Some
                        {
                            Until = 100 + Tuning.defaults.ThreatMemory
                        })
                    "the look writes the room down against its own clock: this tick plus ThreatMemory"

                // The blind tick is the whole point: nothing of ours stands in
                // the room any more, so there is no control entry and no
                // hostile on the view, and the record has to survive that.
                let blind = looked |> raidTick 140 quiet

                Expect.equal
                    (blind.Threatened |> Map.tryFind outpostRoom)
                    (Some { Until = 100 + Engine.creepLifetime })
                    "a tick with no vision in the room leaves the conclusion exactly as it found it"

                // The clock is a backstop and not a schedule (#369). It was 300
                // — the guard's cast plus its walk — and that is the right size
                // for *sending* a guard and the wrong size for *remembering*:
                // while the room is dark an expiry cannot mean the raid ended,
                // only that we stopped remembering, and the row then reads a
                // room full of invader as clear. W15S29 killed four of W15S28's
                // bodies that way in one day, one unarmed body at a time.
                Expect.equal
                    (threatenedAt 999 blind)
                    (Set.singleton outpostRoom)
                    "700 ticks past the old memory's end, with nobody looking, the room is still remembered"

                Expect.equal
                    (threatenedAt (99 + Engine.creepLifetime) blind)
                    (Set.singleton outpostRoom)
                    "one tick short of the backstop it still answers"

                Expect.isEmpty
                    (threatenedAt (100 + Engine.creepLifetime) blind)
                    "and on the tick no raider seen then could still be alive it is forgotten, with no look taken at all"

                Expect.isEmpty
                    (threatenedAt 101 RaidState.empty)
                    "a colony with no log remembers no raid: absence classifies nothing (ADR 0004)"
            }

            test "a look that finds the outpost clear forgets the raid on the tick it takes" {
                // The other direction of the same rule, and the one that keeps
                // this a memory rather than a second [[stand-down]]: a tick
                // with vision decides the room either way. The guard's own
                // arrival is what usually takes this look, which is why the
                // memory may be generous — the cost of it being too long is one
                // body's walk into a room that turns out to be clear.
                let remembered =
                    RaidState.empty
                    |> raidTick 100 (quiet |> lookingAt outpostRoom [ armedIn outpostRoom ])

                let cleared = remembered |> raidTick 160 (quiet |> lookingAt outpostRoom [])

                Expect.isEmpty
                    cleared.Threatened
                    "the room is seen clear 240 ticks before the memory would have run out, and the entry goes with the look"

                Expect.isEmpty (threatenedAt 161 cleared) "so the gate answers for nothing"

                // Vision and no Threat is a clearing; vision and a Threat is a
                // fresh write, which is what keeps a raid that outlives the
                // memory from being forgotten while it is being watched.
                let stillThere =
                    remembered
                    |> raidTick 160 (quiet |> lookingAt outpostRoom [ armedIn outpostRoom ])

                Expect.equal
                    (stillThere.Threatened |> Map.tryFind outpostRoom)
                    (Some { Until = 160 + Engine.creepLifetime })
                    "and a look that finds it still standing there moves the clock to this tick's"
            }

            test
                "a healer alone is remembered nowhere, and neither is a room the colony merely crosses" {
                // Two narrowings in one case, both of them the rule's own
                // spelling rather than this fixture's. ADR 0033's Threat test:
                // a hostile with no ATTACK or RANGED_ATTACK part reaches
                // nothing and is no reason to buy a body, so it writes no
                // memory a guard row could act on. And the room: the guard row
                // is per **declared outpost**, so a raid at home is the
                // [[keep]]'s (ADR 0034) and one in a room the colony does not
                // declare hires nobody (#324).
                let healerSeen =
                    RaidState.empty
                    |> raidTick 100 (quiet |> lookingAt outpostRoom [ healerIn outpostRoom ])

                Expect.isEmpty
                    healerSeen.Threatened
                    "a healer standing in the outpost is a hostile with no reach and buys no guard"

                let atHome =
                    RaidState.empty
                    |> raidTick
                        100
                        { (quiet |> visible raidRoom None) with
                            Hostiles = [ armedIn raidRoom ]
                        }

                Expect.isEmpty
                    atHome.Threatened
                    "and an armed raid in the colony's own room is no outpost's memory"

                let undeclared =
                    RaidState.empty
                    |> raidTick
                        100
                        { (quiet |> visible outpostRoom None) with
                            Hostiles = [ armedIn outpostRoom ]
                        }

                Expect.isEmpty
                    undeclared.Threatened
                    "nor is a room with no controller of ours projected in it: no declaration, no guard row"
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
                    (shutAt 899 state)
                    (Set.singleton outpostRoom)
                    "the tick before the clock runs out the room is still withheld"

                Expect.isEmpty
                    (shutAt 900 state)
                    "on the expiry itself the room is back in the set the shell scans"

                Expect.isEmpty (shutAt 5000 state) "and stays there"
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
                    (shutAt 150 state)
                    (Set.ofList [ outpostRoom; other ])
                    "both clocks running, both rooms withheld"

                Expect.equal
                    (shutAt 500 state)
                    (Set.singleton other)
                    "the room whose clock ran out is back on its own, and the other is still shut"
            }

            test "a room in another player's hands is shut by no clock at all" {
                // The two triggers meet in one set, and only here: the
                // clocked family carries an expiry the gate compares
                // against, the clockless one carries nothing to compare.
                let state = RaidState.empty |> raidTick 100 (quiet |> ownedByRival outpostRoom)

                Expect.equal
                    (shutAt 101 state)
                    (Set.singleton outpostRoom)
                    "shut the tick after it was seen taken"

                Expect.equal
                    (shutAt 1_000_000 state)
                    (Set.singleton outpostRoom)
                    "and shut a million ticks later: there is no clock for this one to run out"
            }

            test "a room another player reserved comes back when the reservation runs out" {
                // #165's first half, at the gate: a passing claimer is a
                // routine event where an owner is not, and the engine is
                // already counting its hold down. The room stands down for
                // exactly that hold and re-enters with **no vision needed** —
                // which is the whole of ADR 0043's "re-entry is a clock
                // running out, not a look", now reached through the trigger
                // that used to latch.
                //
                // Pairwise against the owner above, one field of one control
                // entry apart: 200 ticks of a rival's reservation seen at
                // t100, against the same room owned outright.
                let reserved =
                    RaidState.empty
                    |> raidTick
                        100
                        (quiet |> visible outpostRoom (heldBy ReservationHolder.Rival 200))

                Expect.equal
                    (shutAt 299 reserved)
                    (Set.singleton outpostRoom)
                    "the tick before the hold ends the room is still withheld"

                // t300 and not t301: `TicksToEnd` is the engine's own
                // countdown to the tick the reservation is *gone*, and
                // `Expiry` is the first tick the room may be re-entered —
                // the same reading `deadlineOf` gives the Invader's hold,
                // where 4,000 ticks at t100 records 4,100.
                Expect.isEmpty
                    (shutAt 300 reserved)
                    "on the tick the hold ends the room is back in the pool, nobody having looked"

                Expect.isEmpty (recheckedAt 300 reserved) "and no latch was ever taken to re-check"
            }

            test "a latched room is looked into again every RivalRecheck ticks" {
                // #165's second half. The latch is ownership's and stays
                // ownership's — the room is withheld from the work on every
                // tick below — but the gate hands the shell one room to look
                // into on the ticks a whole `RivalRecheck` after the look
                // that shut it, so the tick with vision that clears a latch
                // can arrive at all. Outside those ticks the room is
                // withdrawn and unscanned, which is ADR 0043 unchanged.
                let state = RaidState.empty |> raidTick 100 (quiet |> ownedByRival outpostRoom)
                let recheck = Tuning.defaults.RivalRecheck

                Expect.equal
                    (shutAt (100 + recheck) state)
                    (Set.singleton outpostRoom)
                    "the recheck tick withholds the room from the work exactly as every other tick does"

                Expect.isEmpty
                    (recheckedAt (100 + recheck - 1) state)
                    "one tick short of the stride, nothing is looked into"

                Expect.equal
                    (recheckedAt (100 + recheck) state)
                    (Set.singleton outpostRoom)
                    "on the stride itself the room is re-admitted to the scan"

                // The look is one tick long because taking it stamps the
                // stride, so the test for that has to fold the tick the look
                // was taken on — the gate alone, asked twice about a log
                // nothing wrote to in between, is being asked about a look
                // that is still owed (#275, and the test below).
                let looked = state |> raidTick (100 + recheck) (quiet |> ownedByRival outpostRoom)

                Expect.isEmpty
                    (recheckedAt (100 + recheck + 1) looked)
                    "and the look is one tick long"

                Expect.equal
                    (recheckedAt (100 + 2 * recheck) looked)
                    (Set.singleton outpostRoom)
                    "a look that changed nothing leaves the next one a whole stride away"

                Expect.isEmpty
                    (recheckedAt 100 state)
                    "the look that shut the gate is not itself a recheck"

                // The knob at a second value, which is what makes it a
                // tunable and not a constant in disguise (ADR 0052 decision
                // 5). The exact-multiple rule had its own trap here — 5,000 is
                // a multiple of 1,000, so a colony tuned to 1,000 would have
                // looked on the shipped stride too — and the elapsed rule
                // (#275) has none: any value below the shipped one separates
                // the two at a tick between them. 3,000 is that, and the pair
                // below asks 3,100: a stride of 3,000 has elapsed there and a
                // stride of 5,000 has not, so the second assertion fails
                // outright if the constant is read in place of the knob.
                let sooner =
                    { Tuning.defaults with
                        RivalRecheck = 3000
                    }

                Expect.equal
                    (standDown sooner 3_100 state).Rechecked
                    (Set.singleton outpostRoom)
                    "a colony tuned to look oftener looks on its own stride"

                Expect.isEmpty
                    (recheckedAt 3_100 state)
                    "and on that tick the shipped stride is not due yet: the knob is read, not a constant"

                Expect.isEmpty
                    (standDown
                        { Tuning.defaults with
                            RivalRecheck = 0
                        }
                        (100 + recheck)
                        state)
                        .Rechecked
                    "a stride of zero is this rule switched off, not a tick divided by nothing"
            }

            test "a tick the gate was never evaluated on delays the look, never forfeits it" {
                // #275. The stride used to be an exact-multiple test —
                // `(tick - since) % RivalRecheck = 0` — a gate that has to be
                // asked on precisely the right tick or not at all. A tick's
                // evaluation is not guaranteed: the loop can throw before the
                // log is written, the engine cuts a tick short when the bot is
                // out of CPU and the bucket is empty, and a deploy lands in the
                // middle of one. Under the old test every tick lost that way
                // cost a whole 5,000 ticks of an outpost's income, silently,
                // because the next tick the gate answered on was another stride
                // away. A look that is owed stays owed.
                let recheck = Tuning.defaults.RivalRecheck
                let state = RaidState.empty |> raidTick 100 (quiet |> ownedByRival outpostRoom)

                Expect.isEmpty
                    (recheckedAt (100 + recheck - 1) state)
                    "one tick short of the stride the look is not owed yet"

                Expect.equal
                    (recheckedAt (100 + recheck + 1) state)
                    (Set.singleton outpostRoom)
                    "the tick after the stride, with nothing having looked, the look is still owed"

                Expect.equal
                    (recheckedAt (100 + recheck + 3_000) state)
                    (Set.singleton outpostRoom)
                    "and three thousand ticks after it, nowhere near a multiple of the stride"

                Expect.equal
                    (recheckedAt (100 + 3 * recheck - 1) state)
                    (Set.singleton outpostRoom)
                    "two whole strides of missed ticks are a late look and not a lost one"
            }

            test "a look stamped ahead of the clock is owed now, and the fold stamps the real tick" {
                // #275's own hazard, which the exact multiple it replaces did
                // not have. An elapsed test has no upper bound, so a
                // `LastLooked` in the future absorbs every tick until the
                // clock catches it up and a stride passes on top — a latch
                // silenced for longer than the stride, which is exactly the
                // unfalsifiable gate #165 bought its way out of. Two ways in,
                // both real: a mistyped hand edit of this leaf, which is the
                // documented way out of a stuck latch, and a private server
                // rolled back behind the tick the log was written on. No look
                // is taken on a tick that has not happened, so a stamp ahead
                // of the clock is a wrong number and not a record.
                let ahead =
                    { RaidState.empty with
                        RivalHeld =
                            Map.ofList [ outpostRoom, { Since = 100; LastLooked = 9_000_000 } ]
                    }

                Expect.equal
                    (recheckedAt 100_000 ahead)
                    (Set.singleton outpostRoom)
                    "the look is owed on the first tick the gate is asked, not nine million ticks out"

                Expect.equal
                    (shutAt 100_000 ahead)
                    (Set.singleton outpostRoom)
                    "and the room is withheld from the work through it, as on every other tick"

                // The gate corrects in one tick; the leaf corrects with it,
                // because the fold stamps the tick the look actually fell due
                // on over the impossible one.
                let healed = ahead |> raidTick 100_000 (quiet |> ownedByRival outpostRoom)

                Expect.equal
                    healed.RivalHeld
                    (Map.ofList [ outpostRoom, { Since = 100; LastLooked = 100_000 } ])
                    "the impossible stamp is written over by the look that was taken, the date standing"

                Expect.isEmpty
                    (recheckedAt 100_001 healed)
                    "so the stride runs from the real look and the room is not re-read every tick"
            }

            test "a clocked stand-down is never re-checked, and an empty log never looks" {
                // The recheck belongs to the latch alone: a clocked
                // stand-down needs no look, because its own clock takes the
                // room back (ADR 0043), and scanning it early would cost a
                // room read for an answer nothing acts on.
                let clocked = RaidState.empty |> raidTick 100 (seen [ core outpostRoom (Some 900) ])

                Expect.equal
                    (shutAt 800 clocked)
                    (Set.singleton outpostRoom)
                    "the core's clock is still running"

                Expect.isEmpty
                    (recheckedAt (100 + Tuning.defaults.RivalRecheck) clocked)
                    "and the stride falls due on nothing: there is no latch here to question"

                Expect.isEmpty
                    (recheckedAt 5_100 RaidState.empty)
                    "an empty log looks into nothing on any tick"
            }

            test "an empty log withholds nothing" {
                // The colony's ordinary state, and the one it has run in
                // since ADR 0042 filled the declaration: no outpost has
                // ever held a core, so the gate is open and the shell
                // scans every room a human declared.
                Expect.isEmpty (shutAt 100 RaidState.empty) "nothing is recorded, nothing is shut"
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
        Bucket = 10_000
        Replans = 0
        ColonyDecides = []
        RoomSnapshots = []
        AtRooms = 0.0
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

            test "each colony's decide is differenced against the boundary before it (#370)" {
                // The live shape the day this was built: four colonies, the
                // `decide` phase running from 15.3 to 56.3 ms, and a reader
                // who could not say which of the four a 140 ms spike had come
                // out of. The readings arrive cumulative — one
                // `Game.cpu.getUsed` after each colony — so the first is
                // differenced against the phase's own start and each of the
                // rest against the colony before it.
                let state =
                    CpuState.empty
                    |> foldCpu
                        capCpuTicks
                        100
                        {
                            AtEntry = 0.4
                            AtSnapshot = 15.3
                            AtDecide = 56.3
                            AtSave = 60.9
                            AtExecute = 69.0
                            Intents = 78
                            Bucket = 10_000
                            Replans = 0
                            ColonyDecides =
                                [ "W12S28", 27.3; "W13S28", 38.1; "W11S29", 45.0; "W15S28", 55.9 ]
                            RoomSnapshots = []
                            AtRooms = 0.0
                        }

                Expect.equal
                    (state.Ticks |> List.map (fun sample -> sample.Colonies))
                    [ [ "W12S28", 12.0; "W13S28", 10.8; "W11S29", 6.9; "W15S28", 10.9 ] ]
                    "the first against `AtSnapshot`, each of the rest against the colony before it"

                // And the remainder is readable rather than hidden: what the
                // phase cost less what the colonies did is the movement
                // arbitration and the two Memory reads `decide` is handed, so
                // neither number is derived from the other and a reader can
                // subtract them.
                let phases = state.Ticks |> List.exactlyOne |> (fun sample -> sample.Phases)

                Expect.equal
                    (phases |> Option.map (fun p -> p.Decide))
                    (Some 41.0)
                    "the phase stays the tick's own, 41.0 ms against the colonies' 40.6"
            }

            test
                "a bundle that measured no colony writes no split, which is what an older row reads as" {
                // `Phases` needs its `option` because a measured zero and an
                // unmeasured phase are different claims. This does not: the
                // empty list is the right answer both for a row written before
                // the split existed and for a tick in which no colony decided,
                // and the split is only ever read against `Phases.Decide`,
                // which says whether there was anything to attribute.
                let state = CpuState.empty |> foldCpu capCpuTicks 100 (costing 21.0)

                Expect.equal
                    (state.Ticks |> List.map (fun sample -> sample.Colonies))
                    [ [] ]
                    "no reading, no attribution — and the row is still in the window the trigger is read off"
            }


            test "the snapshot's rooms are differenced from the prelude" {
                // The rooms' counterpart to the colonies' split, and it starts
                // one boundary earlier: `snapshot` begins where the prelude's
                // reading was taken, because nothing runs between them. A
                // reader that differenced the first room against `AtSnapshot`
                // would price it against the *end* of its own phase and report
                // a negative millisecond — which is the shape of mistake the
                // colonies' split could not make, since `AtSnapshot` really is
                // the boundary before the first colony.
                let state =
                    CpuState.empty
                    |> foldCpu
                        capCpuTicks
                        100
                        {
                            AtEntry = 3.0
                            AtSnapshot = 18.0
                            AtDecide = 50.0
                            AtSave = 54.0
                            AtExecute = 60.0
                            Intents = 40
                            Bucket = 10_000
                            Replans = 0
                            ColonyDecides = []
                            AtRooms = 4.0
                            RoomSnapshots = [ "W15S28", 9.0; "W15S27", 12.5; "W15S26", 18.0 ]
                        }

                Expect.equal
                    (state.Ticks |> List.map (fun sample -> sample.Rooms))
                    [ [ "W15S28", 5.0; "W15S27", 3.5; "W15S26", 5.5 ] ]
                    "each room against the room swept before it, the first against `AtRooms`"

                // And they sum to **less** than the phase, on purpose: 18.0 -
                // 3.0 is 15.0 while 5.0 + 3.5 + 5.5 is 14.0, and the missing
                // 1.0 is the head the sweep does before the first room —
                // enumerating `Game.rooms`, grouping every creep by the room it
                // stands in, reading the declarations. Charging that head to
                // whichever room happened to be swept first is what this
                // reading did on its first live window: it priced W11S28, an
                // outpost with one rock, at 2.35 ms against the four-spawn home
                // room beside it at 1.23. The remainder is left readable rather
                // than folded into a room, exactly as `decide`'s is.
                Expect.equal
                    (state.Ticks |> List.collect (fun sample -> sample.Rooms) |> List.sumBy snd)
                    14.0
                    "the rooms sum to the sweep, and the sweep is less than the phase"
            }

            test "the readings are differenced into phases, the entry alone" {
                // The shape of a live tick the day the split was built: an
                // engine prelude already spent before `loop` runs, then the
                // ColonyView, `decide`, the Memory writes and the Executor's
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
                            // The margin and the replan count ride the same
                            // row (#357): a full bucket and a tick that kept
                            // every colony's plan, which is the shape a phase
                            // split is read against.
                            Bucket = 9_872
                            Replans = 0
                            ColonyDecides = []
                            RoomSnapshots = []
                            AtRooms = 0.0
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
                                Bucket = 9_872
                                Replans = 0
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
                            // Neither of these is a duration, so neither is
                            // rounded: an integer count of banked milliseconds
                            // and an integer count of colonies.
                            Bucket = 4_213
                            Replans = 2
                            ColonyDecides = []
                            RoomSnapshots = []
                            AtRooms = 0.0
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
                                Bucket = 4_213
                                Replans = 2
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
                // ColonyView cost nothing rather than that nobody measured it.
                let unsplit =
                    {
                        Ticks =
                            [
                                {
                                    Tick = 99
                                    Ms = 6.1
                                    Phases = None
                                    Colonies = []
                                    Rooms = []
                                }
                            ]
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

/// The room the errand fixtures declare, and the Reactor standing in it: the
/// `Decide` suites' own spelling, read rather than re-spelled (#318's rule for
/// the two domains that already share it). A second spelling here would be a
/// fixture free to drift away from the rule it stands in for — and the
/// regression this channel exists to catch is precisely a fixture that agreed
/// with the code about a shape `World` never builds.
let private errandRoom = Decide.Fixtures.reactorErrand.RoomName
let private reactor = Decide.Fixtures.reactorId

/// A room this colony **owns**: the other half of the pile check's reach, and
/// the half `Facts.inARoomWeOwn` answers off `RoomControl`.
let private owning room (colony: ColonyView) =
    { colony with
        RoomControl =
            Map.add
                room
                {
                    Owner = Ownership.Ours
                    Reservation = None
                    SafeMode = false
                }
                colony.RoomControl
    }

/// A dropped Thorium pile standing in a room, as the projection carries one:
/// its kind on the census, its amount in `SpatialInfo.Thorium`, and its tile in
/// that room's layer (ADR 0041). All three, because the pile check joins the
/// census to the room and reads the amount off the map beside them.
let private withPile room id amount (colony: ColonyView) =
    let layer = SpatialInfo.layerOf colony.Spatial room

    { colony with
        Spatial =
            { colony.Spatial with
                TargetKinds = Map.add id (Dropped Thorium) colony.Spatial.TargetKinds
                Thorium = Map.add id amount colony.Spatial.Thorium
            }
            |> withNeighbour
                room
                { layer with
                    TargetPositions = Map.add id { X = 26; Y = 43 } layer.TargetPositions
                }
    }

/// A tombstone standing in a room, holding the given ore — the courier that
/// died loaded (#359). The same three facts as the pile above and one
/// difference: the kind names no resource, so `amount` in the Thorium map is
/// the only thing that makes this object ore at all, which is why the control
/// below is a tombstone with no entry in it.
let private withTombstone room id amount (colony: ColonyView) =
    let layer = SpatialInfo.layerOf colony.Spatial room

    { colony with
        Spatial =
            { colony.Spatial with
                TargetKinds = Map.add id Tombstone colony.Spatial.TargetKinds
                Thorium =
                    match amount with
                    | Some units -> Map.add id units colony.Spatial.Thorium
                    | None -> colony.Spatial.Thorium
            }
            |> withNeighbour
                room
                { layer with
                    TargetPositions = Map.add id { X = 27; Y = 43 } layer.TargetPositions
                }
    }

/// The colony with the errand declared and the Reactor visible and ours,
/// holding `held` of its 1,000 (#354's fixtures, which put the store on the
/// Reactor's **row** and never in `SpatialInfo.Thorium`).
let private erranding held (colony: ColonyView) =
    { colony with
        // The home this delivery runs from, and it has to be a room the errand
        // is actually reachable from (#361). `quiet` lives at `raidRoom`
        // (W12S28) while `Decide.Fixtures`' Reactor stands in W1N2 — **42 room
        // crossings apart**, a declaration `Errand.routable` would refuse and
        // no courier could ever walk. It never mattered until a check read the
        // distance: the running-dry alarm times itself against the walk, and
        // against 42 hops every store in these fixtures reads as too late to
        // save. Three hops is the live pairing, W15S28 to W15S25.
        Spatial =
            { colony.Spatial with
                RoomName = Some "W1N5"
            }
    }
    |> Decide.Fixtures.withReactorErrand
    |> Decide.Fixtures.withReactorOwner (Some Ownership.Ours)
    |> Decide.Fixtures.withReactorStore held

/// One of ours standing in the errand room with a load of ore aboard — the
/// courier at the end of the paired delivery (ADR 0060 decision 3).
let private courierAt name carried (colony: ColonyView) =
    colony
    |> Decide.Fixtures.standingInErrand [ { ours name with Thorium = carried }, { X = 25; Y = 43 } ]

/// The courier's real body on a named creep (#361): the alarm matches the
/// **shape** `Bodies.courierPattern` casts — twenty Carry and ten Move — so a
/// fixture that wants to be seen as a courier has to carry it, and the
/// three-part `ours` body beside it is the control that must not be.
let private withCourierBody name (colony: ColonyView) =
    { colony with
        Creeps =
            colony.Creeps
            |> List.map (fun creep ->
                if creep.Name = name then
                    { creep with
                        Body = Map.ofList [ Carry, 20; Move, 10 ]
                    }
                else
                    creep)
    }

/// The breaches this view yields on one tick, as (kind, room, subject, amount)
/// rows — the whole of what a row says, so a case that fires the right kind on
/// the wrong object cannot pass.
let private breachesOn t (colony: ColonyView) =
    foldBreaches capBreaches t { colony with Time = t } BreachState.empty
    |> breachRows
    |> List.map (fun row -> row.Breach.Kind, row.Breach.Room, row.Breach.Subject, row.Breach.Amount)

/// The kinds alone, for the cases that are about which check fired.
let private kindsOn t (colony: ColonyView) =
    breachesOn t colony |> List.map (fun (kind, _, _, _) -> kind)

[<Tests>]
let breachKindTests =
    testList
        "breach log: what fires"
        [
            test "a Thorium pile in a room we own is a breach, and one in a stranger's room is not" {
                Expect.equal
                    (breachesOn 100 (quiet |> owning raidRoom |> withPile raidRoom "pile-1" 915))
                    [ BreachKind.OreOnTheFloor, raidRoom, "pile-1", 915 ]
                    "the amount is the T on the floor, which is what makes the row actionable"

                Expect.equal
                    (breachesOn 100 (quiet |> withPile "W9S9" "pile-1" 915))
                    []
                    "a room we neither own nor declared nor cross is somebody else's floor"

                // The third clause of that reach (#360). A room a chain of ours
                // merely crosses is not somebody else's floor — it is where the
                // ore most often lands, the delivery route being three
                // crossings and the courier oldest on the loaded leg — and
                // until this it was the one place a leak could bleed out
                // unnamed. `Crossed` is what says so, and it is the projection's
                // own subtraction rather than a second guess at it.
                let onTheWay =
                    let seen = quiet |> withPile "W9S9" "pile-1" 419

                    { seen with
                        Crossed = Set.singleton "W9S9"
                    }

                Expect.equal
                    (breachesOn 100 onTheWay)
                    [ BreachKind.OreOnTheFloor, "W9S9", "pile-1", 419 ]
                    "the same floor, once the projection says we cross it, is a leak this colony is answerable for"
            }

            test "a pile on the declared Reactor's floor is a breach: that room has no owner at all" {
                // #354's second half, which is the reason this check reads
                // `Facts.ourThoriumPiles` rather than "a room we own": the
                // Reactor's room has no controller, so it is owned by nobody,
                // and 915 T sat on it for hours while a body of ours stood two
                // tiles away. A check written against ownership alone would
                // have been silent through the very incident it is for.
                //
                // This case pins the rule; that the **projection** can build
                // the shape is pinned next door in `ViewTests` ("ore on the
                // errand room's floor is the one thing beside the declaration
                // that rides"), and the two were written together because they
                // were false apart: wiring this channel is what found that
                // `erranding` had emptied the kind census the Pickup #354
                // added sweeps, so the rule reached nothing live and its own
                // fixture hid that (#356, #355).
                Expect.equal
                    (breachesOn 100 (quiet |> erranding 500 |> withPile errandRoom "pile-r" 915))
                    [ BreachKind.OreOnTheFloor, errandRoom, "pile-r", 915 ]
                    "the errand room's floor is the one floor of ours that is in nobody's room"
            }

            test "ore in a tombstone is the same breach, and a tombstone holding none is no breach" {
                // #359. The channel swept `Dropped Thorium` alone, so the ore a
                // courier dies with — 175 T at W15S25's (43,6) — was invisible
                // to it until the tombstone decayed and dropped the store as
                // piles. Covering the tombstone directly is those ticks, and the
                // decay is why it is `OreOnTheFloor` and not a kind of its own:
                // it is the same incident a few hundred ticks earlier, reported
                // to an operator who would take the same action.
                //
                // Pinned in both rooms the reach names, because they are two
                // clauses: a room we own, and a room we declared an errand in —
                // the second being the one with no controller, where 915 T of
                // the pile's own incident bled unnamed. The projection half of
                // both is `ViewTests`' pair of tombstone cases, written with
                // this one for #355's and #356's reason.
                Expect.equal
                    (breachesOn
                        100
                        (quiet |> owning raidRoom |> withTombstone raidRoom "tomb-1" (Some 175)))
                    [ BreachKind.OreOnTheFloor, raidRoom, "tomb-1", 175 ]
                    "the amount is the T in the store, which is what makes the row actionable"

                Expect.equal
                    (breachesOn
                        100
                        (quiet |> erranding 500 |> withTombstone errandRoom "tomb-r" (Some 175)))
                    [ BreachKind.OreOnTheFloor, errandRoom, "tomb-r", 175 ]
                    "and the declared Reactor's room, which is where a courier dies"

                // The pairwise control: the same object in the same room with no
                // entry in the Thorium map — a tombstone of a body that was
                // carrying energy, or none. A tombstone is not a breach; ore in
                // one is.
                Expect.isEmpty
                    (breachesOn
                        100
                        (quiet |> owning raidRoom |> withTombstone raidRoom "tomb-1" None))
                    "a tombstone holding no ore is a decaying object and not a loss"
            }

            test "ore a courier cannot place is a breach, and a load that fits is not" {
                let full = quiet |> erranding Engine.reactorCapacity |> courierAt "courier" 500

                Expect.equal
                    (breachesOn 100 full
                     |> List.filter (fun (kind, _, _, _) -> kind = BreachKind.OreUnplaceable))
                    [ BreachKind.OreUnplaceable, errandRoom, "courier", 500 ]
                    "a full Reactor has no room for any of the 500 aboard"

                Expect.equal
                    (breachesOn 100 (quiet |> erranding 700 |> courierAt "courier" 500))
                    [ BreachKind.OreUnplaceable, errandRoom, "courier", 200 ]
                    "300 of the load fits; the breach is the 200 that has nowhere to go"

                Expect.equal
                    (breachesOn 100 (quiet |> erranding 500 |> courierAt "courier" 500))
                    []
                    "a load the store has exactly the room for is the delivery working"
            }

            test "a courier's load is read off the Reactor's row, and no Thorium map beside it" {
                // The regression shape of #354's first incident, copied from
                // `ErrandTests`' "the draw reads the Reactor's own row":
                // `SpatialInfo.Thorium` carries every store a Task can name and
                // deliberately not the Reactor's, and the draw gate read it
                // there anyway — 999 read as 0, the gate never closed, and the
                // ore reached the floor. What made it invisible is what this
                // case pins: the unit test agreed with the gate, because the
                // fixture wrote the store where the gate looked.
                let ready = quiet |> erranding 0 |> courierAt "courier" 500

                let inTheWrongMap =
                    { ready with
                        Spatial =
                            { ready.Spatial with
                                Thorium =
                                    Map.add reactor Engine.reactorCapacity ready.Spatial.Thorium
                            }
                    }

                Expect.isFalse
                    (kindsOn 100 inTheWrongMap |> List.contains BreachKind.OreUnplaceable)
                    "a full store written where the projection never writes one raises no alarm"

                Expect.equal
                    (breachesOn
                        100
                        (quiet |> erranding Engine.reactorCapacity |> courierAt "courier" 500)
                     |> List.filter (fun (kind, _, _, _) -> kind = BreachKind.OreUnplaceable))
                    [ BreachKind.OreUnplaceable, errandRoom, "courier", 500 ]
                    "the same number on the Reactor's own row is the breach"
            }

            test "a body of ours elsewhere holding ore is nobody's breach" {
                // The cheapest false positive there is, and the reason the
                // check is narrowed to the bodies standing in the errand room:
                // a [[miner]] at home holding ore bound for its container is
                // not ore with nowhere to go, however full the Reactor is.
                let atHome =
                    { (quiet |> erranding Engine.reactorCapacity) with
                        Creeps = [ { ours "miner" with Thorium = 500 } ]
                    }

                Expect.isFalse
                    (kindsOn 100 atHome |> List.contains BreachKind.OreUnplaceable)
                    "the load of a body that is not at the Reactor has somewhere else to be"
            }

            test "a Reactor of ours standing dry is a breach; one with ore in it is not" {
                Expect.equal
                    (breachesOn 100 (quiet |> erranding 0))
                    [ BreachKind.ReactorStarved, errandRoom, reactor, 0 ]
                    "an empty store is the streak reset, and the row's whole content is the fact"

                Expect.equal
                    (breachesOn 100 (quiet |> erranding 1))
                    [ BreachKind.ReactorRunningDry, errandRoom, reactor, 1 ]
                    "one tonne left and nobody walking is the row that arrives in time, not the starved one"
            }

            test
                "a Reactor whose store is thinner than the courier's lead time is a breach before it starves" {
                // #361, and the whole of the design is the threshold. 90 ticks
                // to cast the fixed body plus 150 over three crossings is 240,
                // so 240 fires and 241 does not, and the row appears on the
                // last tick an answer still lands rather than on the tick the
                // streak is already gone.
                Expect.equal
                    (breachesOn 100 (quiet |> erranding 240))
                    [ BreachKind.ReactorRunningDry, errandRoom, reactor, 240 ]
                    "the amount is the ticks of burn left, which counts down while nobody answers"

                Expect.equal
                    (breachesOn 100 (quiet |> erranding 241))
                    []
                    "one tick of margin over the lead time is a Reactor still reachable, and an alarm here would be answered by a courier that stands at the flag burning its 1,500-tick life"
            }

            test
                "a laden courier silences the running-dry row; a courier-shaped body alone does not" {
                // The condition is not "the store is low", it is "the store is
                // low **and no ore is moving**".
                //
                // The first version of this said "and no courier is alive", and
                // #367 is what that cost: the delivery draw ranked below
                // ordinary energy hauling, so a courier lived for 465 ticks
                // hauling energy while the store fell 500 -> 0 and a
                // 15,582-tick streak broke, and this channel reported `no
                // breaches` the whole way down. A body of the right shape is
                // not a delivery in progress.
                //
                // So what answers the alarm is ore **aboard** — a fact of the
                // view (`CreepInfo.Thorium`) rather than an assignment, which
                // keeps this channel out of the Matcher's business (ADR 0025)
                // and out of reach of a body that is doing something else.
                // Matched on the body's shape and never its name, so the alarm
                // and the quota that hires cannot come to disagree about what a
                // courier is.
                let walking =
                    quiet |> erranding 10 |> courierAt "courier" 500 |> withCourierBody "courier"

                Expect.equal (breachesOn 100 walking) [] "a load is genuinely in the air"

                // The empty-handed courier is the live shape, and it must cry:
                // it is either walking out to fetch a load, which costs a few
                // ticks of false alarm, or it is doing something else entirely,
                // which is the 465-tick case this exists for. An alarm whose
                // whole value is arriving early errs this way.
                let empty =
                    quiet |> erranding 10 |> courierAt "courier" 0 |> withCourierBody "courier"

                Expect.equal
                    (breachesOn 100 empty)
                    [ BreachKind.ReactorRunningDry, errandRoom, reactor, 10 ]
                    "a courier carrying nothing is not an answer, whatever its body says"

                Expect.equal
                    (breachesOn 100 (quiet |> erranding 10 |> courierAt "hauler" 500))
                    [ BreachKind.ReactorRunningDry, errandRoom, reactor, 10 ]
                    "a three-part body standing out there is not a courier and carries no load worth a delivery"
            }

            test "a declared Reactor whose row is not ours is a breach, and is not also starved" {
                let rivals =
                    quiet
                    |> Decide.Fixtures.withReactorErrand
                    |> Decide.Fixtures.withReactorOwner (Some Ownership.Rival)

                Expect.equal
                    (breachesOn 100 rivals)
                    [ BreachKind.ReactorLost, errandRoom, reactor, 0 ]
                    "everything delivered there scores for whoever holds the flag"

                Expect.isFalse
                    (kindsOn 100 rivals |> List.contains BreachKind.ReactorStarved)
                    "a Reactor that is not ours is not a Reactor of ours standing dry"
            }

            test "a Reactor we cannot see yields no row of any kind" {
                // ADR 0004 taken to its conclusion on an alarm channel: no
                // vision, no row, no reading — and therefore no breach. A
                // declared Reactor with nothing of ours standing out there is
                // the ordinary state between two re-claimers, and a channel
                // that read absence as a violation would cry wolf on every one
                // of those ticks.
                let blind =
                    { (quiet |> erranding 0) with
                        Reactors = []
                    }

                Expect.equal
                    (breachesOn 100 blind)
                    []
                    "neither starved nor lost: the colony has read nothing to be either"
            }

            test "a colony with nothing wrong records nothing" {
                Expect.equal
                    (breachesOn 100 (quiet |> erranding 500))
                    []
                    "the healthy tick is empty"
            }
        ]

[<Tests>]
let breachAgeTests =
    testList
        "breach log: age and the cap"
        [
            test "a breach still standing fifty ticks later is fifty ticks old" {
                // The whole reason this channel folds rather than snapshotting:
                // live, a pile a courier is three ticks from picking up and a
                // pile that is bleeding read exactly alike.
                let colony = quiet |> owning raidRoom |> withPile raidRoom "pile-1" 915

                let state =
                    BreachState.empty
                    |> foldBreaches capBreaches 100 { colony with Time = 100 }
                    |> foldBreaches capBreaches 150 { colony with Time = 150 }

                Expect.equal
                    (standing 150 state |> List.map (fun (breach, age) -> breach.Subject, age))
                    [ "pile-1", 50 ]
                    "the row keeps the tick it opened on and ages against the clock"

                Expect.equal
                    (breachRows state |> List.map (fun row -> row.FirstSeen, row.LastSeen))
                    [ 100, 150 ]
                    "and it dates itself, so the leaf can be read without a clock"
            }

            test "a breach that clears drops out rather than lingering" {
                // This channel answers "what is broken now" and nothing else;
                // the episodic reading of the same ground is the Raid log's
                // (ADR 0028). A row that lingered would need a reader who knew
                // which rows were current, which is every stale dashboard.
                let broken = quiet |> owning raidRoom |> withPile raidRoom "pile-1" 915

                let state =
                    BreachState.empty
                    |> foldBreaches capBreaches 100 { broken with Time = 100 }
                    |> foldBreaches
                        capBreaches
                        101
                        { (quiet |> owning raidRoom) with
                            Time = 101
                        }

                Expect.equal (breachRows state) [] "the pile was picked up, and the log says so"
            }

            test "a breach that comes back opens a fresh age" {
                // The stated cost of dropping out: a violation that flickers
                // off for one tick loses its age. That is the right trade for
                // four checks that are conditions the projection re-reads every
                // tick rather than events, and it is pinned so the next reader
                // meets it here rather than in the field.
                let broken = quiet |> owning raidRoom |> withPile raidRoom "pile-1" 915
                let clear = quiet |> owning raidRoom

                let state =
                    BreachState.empty
                    |> foldBreaches capBreaches 100 { broken with Time = 100 }
                    |> foldBreaches capBreaches 101 { clear with Time = 101 }
                    |> foldBreaches capBreaches 102 { broken with Time = 102 }

                Expect.equal
                    (standing 102 state |> List.map snd)
                    [ 0 ]
                    "the returning pile is a new row, not the old one resumed"
            }

            test "the latest reading wins: a growing pile reports what it holds now" {
                let state =
                    BreachState.empty
                    |> foldBreaches
                        capBreaches
                        100
                        { (quiet |> owning raidRoom |> withPile raidRoom "pile-1" 90) with
                            Time = 100
                        }
                    |> foldBreaches
                        capBreaches
                        101
                        { (quiet |> owning raidRoom |> withPile raidRoom "pile-1" 915) with
                            Time = 101
                        }

                Expect.equal
                    (breachRows state |> List.map (fun row -> row.Breach.Amount, row.FirstSeen))
                    [ 915, 100 ]
                    "the amount is this tick's and the age is the first tick's"
            }

            test "the cap keeps the rows that have stood longest" {
                // Every surviving row was last seen on this very tick — a row
                // that stops appearing drops out — so `LastSeen` is tied across
                // the whole log and `FirstSeen` is what decides. Evicting the
                // oldest would silence exactly the breach worth reading.
                let piles ids amount time =
                    (quiet |> owning raidRoom, ids)
                    ||> List.fold (fun colony id -> withPile raidRoom id amount colony)
                    |> fun colony -> { colony with Time = time }

                let state =
                    BreachState.empty
                    |> foldBreaches 2 100 (piles [ "old-a"; "old-b" ] 915 100)
                    |> foldBreaches 2 101 (piles [ "old-a"; "old-b"; "new-c" ] 915 101)

                Expect.equal
                    (breachRows state |> List.map (fun row -> row.Breach.Subject))
                    [ "old-a"; "old-b" ]
                    "the cap drops the breach that has only just appeared"
            }
        ]
