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
