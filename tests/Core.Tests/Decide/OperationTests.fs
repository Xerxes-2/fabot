/// The operation (#663 M7): a spec's guards, phases and exits, its record's
/// latch moved on by the interpreter's pure step, and the decoder that
/// admits or refuses a Memory spec.
module Fabot.Core.Tests.Decide.OperationTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Decide

/// Facts with every listed room priced at its rate, every room seen since
/// the reset, and nothing else.
let private factsAt (now: int) (rates: (string * float) list) : OpFacts =
    let rates = Map.ofList rates

    {
        Now = now
        Rate = fun room -> Map.tryFind room rates
        Worth =
            fun room ->
                if rates.ContainsKey room then
                    Some RoomWorth.empty
                else
                    None
        Warm = fun _ -> true
        Arrived = fun _ -> Truth.Unknown
        Assembled = fun _ -> Truth.Unknown
        Loses = fun _ -> Truth.Unknown
        Holds = fun _ -> Truth.Unknown
        Challenge = fun room -> Map.tryFind room rates
        HomeThreatened = Truth.No
    }

let private limits =
    {
        MaxBodies = 4
        MaxEnergy = 10_000
        Deadline = 5_000
        MaxLossRatio = 1.0
    }

let private sweep =
    Operation.Templates.sweep "s1" "Trepidimous" [ "W1N1"; "W2N1" ] limits

let private recordAt (phase: OpPhase) (since: int) : OpRecord =
    {
        Id = "s1"
        Started = 0
        Phase = phase
        Since = since
        Room = None
        RoomSince = since
        Spent = 0
        Lost = 0
        NextDue = 0
        Ended = None
    }

/// Every spec admitted, nothing refused or kept.
let private admitting (specs: OperationSpec list) : OpAdmission =
    {
        Admitted = specs
        Refused = []
        Kept = Set.empty
    }

/// A spec of one phase and the given exits, a second phase to move into.
let private withExits (exits: (OpGuard * OpNext) list) : OperationSpec =
    { sweep with
        Phases =
            [
                {
                    Phase = OpPhase.Muster
                    Jobs = []
                    Exits = exits
                    Urgent = []
                }
                {
                    Phase = OpPhase.Execute
                    Jobs = []
                    Exits = []
                    Urgent = []
                }
            ]
    }

let private standingAt (now: int) : OpStanding =
    {
        Now = now
        Ours = fun room -> room = "W5N5"
        Holder =
            function
            | "W6N6" -> RoomHolder.Player "Odiodin"
            | "W7N7" -> RoomHolder.Player "somebody"
            | "W8N8" -> RoomHolder.Player "Trepidimous"
            | "W4N4" -> RoomHolder.Unknown
            | _ -> RoomHolder.Nobody
    }

let private standing = standingAt 1_000

let private wire: OpSpecWire =
    {
        Id = "op1"
        Template = "sweep"
        Enemy = "Trepidimous"
        Rooms = [ "W8N8"; "W9N9" ]
        Deadline = None
        MaxEnergy = None
        MaxBodies = None
    }

/// A room's facts this tick, its controller held as given.
let private looked (owner: Ownership) (reserver: string option) : RoomFacts =
    { RoomFacts.empty with
        Control =
            Some
                {
                    Owner = owner
                    Reservation =
                        reserver
                        |> Option.map (fun username ->
                            {
                                Holder = Colony.reservationHolder (Some "me") username
                                TicksToEnd = 1_000
                                Username = username
                            })
                    SafeMode = false
                    SafeModeCooldownUntil = 0
                    Sign = None
                }
    }

let private sighted (tick: int) (rival: string option) : RoomSighting =
    {
        Tick = tick
        Targets = lazy Set.empty
        Rival = rival
    }

[<Tests>]
let operationStepTests =
    testList
        "the operation interpreter's step (#663 S7a)"
        [
            test "no ops: no records, no events, and nothing priced" {
                let untouched: OpFacts =
                    {
                        Now = 100
                        Rate = fun _ -> failwith "priced with no op"
                        Worth = fun _ -> failwith "priced with no op"
                        Warm = fun _ -> failwith "read with no op"
                        Arrived = fun _ -> failwith "read with no op"
                        Assembled = fun _ -> failwith "read with no op"
                        Loses = fun _ -> failwith "read with no op"
                        Holds = fun _ -> failwith "read with no op"
                        Challenge = fun _ -> failwith "priced with no op"
                        HomeThreatened = Truth.Unknown
                    }

                Expect.equal (Operations.step untouched (admitting []) []) ([], []) "nothing moves"

                Expect.equal
                    (Operation.admit standing [] [])
                    (admitting [])
                    "nothing admitted, refused or kept"
            }

            test "the sweep begins in its first phase, its room the one a cast earns most in" {
                let records, events =
                    Operations.step
                        (factsAt 100 [ "W1N1", 2.0; "W2N1", 3.0 ])
                        (admitting [ sweep ])
                        []

                Expect.equal
                    records
                    [
                        { recordAt OpPhase.Execute 100 with
                            Started = 100
                            Room = Some "W2N1"
                        }
                    ]
                    "begun at once, committed to its room"

                Expect.equal
                    events
                    [ OpEvent.Began("s1", OpPhase.Execute, Some "W2N1") ]
                    "one event"
            }

            test
                "the sweep's transition table: no room pays, hold; one pays again, execute; its deadline, expired" {
                let at now rates phase =
                    Operations.advance (factsAt now rates) sweep (Some(recordAt phase 0))
                    |> fst
                    |> Option.map (fun r -> r.Phase, r.Ended)

                Expect.equal
                    (at 10 [ "W1N1", 0.0; "W2N1", 0.0 ] OpPhase.Execute)
                    (Some(OpPhase.Hold, None))
                    "nothing pays: hold"

                Expect.equal
                    (at 10 [ "W1N1", 1.0; "W2N1", 0.0 ] OpPhase.Execute)
                    (Some(OpPhase.Execute, None))
                    "one pays: stay"

                Expect.equal
                    (at 10 [ "W1N1", 1.0; "W2N1", 0.0 ] OpPhase.Hold)
                    (Some(OpPhase.Execute, None))
                    "one pays again: execute"

                Expect.equal
                    (at 5_000 [ "W1N1", 1.0 ] OpPhase.Execute)
                    (Some(OpPhase.Execute, Some OpEnd.Expired))
                    "its deadline: expired, hard"
            }

            // The S7c review: Execute -> Hold on one tick's 0, then Hold ->
            // Execute on the plain argmax, flipped the room in another shape.
            test
                "one tick at nothing for the room committed to, its quarry still there, moves neither the room nor the phase" {
                let committed =
                    { recordAt OpPhase.Execute 40 with
                        Room = Some "W1N1"
                        RoomSince = 20
                    }

                let flat =
                    { factsAt 50 [ "W1N1", 0.0; "W2N1", 0.0 ] with
                        Holds = fun room -> if room = "W1N1" then Truth.Yes else Truth.No
                        Loses = fun _ -> Truth.No
                    }

                let moved, events = Operations.advance flat sweep (Some committed)

                Expect.equal
                    moved
                    (Some committed)
                    "unmoved: the room, its commitment and the phase"

                Expect.isEmpty events "silent"

                Expect.equal
                    (Operations.workedRoom 50 moved)
                    (Some "W1N1")
                    "still worked: its Brief laid, its group seated"
            }

            test
                "across a hold the committed room is kept, and taken up again unless another outpays it on evidence" {
                let held =
                    { recordAt OpPhase.Hold 40 with
                        Room = Some "W1N1"
                        RoomSince = 20
                    }

                // W2N1 pays more this tick on bodies seen there once: no
                // evidence it outpays the room committed to.
                let back =
                    { factsAt 50 [ "W1N1", 1.0; "W2N1", 3.0 ] with
                        Holds = fun room -> if room = "W1N1" then Truth.Yes else Truth.No
                        Loses = fun _ -> Truth.No
                        Challenge = fun room -> if room = "W2N1" then Some 0.5 else Some 1.0
                    }

                Expect.equal
                    (Operations.advance back sweep (Some held)
                     |> fst
                     |> Option.map (fun r -> r.Phase, r.Room, r.RoomSince))
                    (Some(OpPhase.Execute, Some "W1N1", 20))
                    "the committed room, its commitment kept"

                Expect.equal
                    (Operations.advance { back with Challenge = back.Rate } sweep (Some held)
                     |> fst
                     |> Option.map (fun r -> r.Phase, r.Room, r.RoomSince))
                    (Some(OpPhase.Execute, Some "W2N1", 50))
                    "outpaid on evidence: the other room, committed from now"
            }

            test "Unknown never transitions, and hides the exits after it" {
                let spec =
                    withExits
                        [
                            OpGuard.Assembled, OpNext.To OpPhase.Execute
                            OpGuard.Always, OpNext.Finish OpEnd.Abandoned
                        ]

                let moved, events =
                    Operations.advance (factsAt 10 []) spec (Some(recordAt OpPhase.Muster 0))

                Expect.equal moved (Some(recordAt OpPhase.Muster 0)) "unmoved"
                Expect.isEmpty events "no event"

                // A room out of sight prices nothing: its rate is Unknown.
                let unpriced = withExits [ OpGuard.RateAbove 0.0, OpNext.To OpPhase.Execute ]

                Expect.equal
                    (Operations.advance
                        (factsAt 10 [ "W1N1", 0.0 ])
                        unpriced
                        (Some(recordAt OpPhase.Muster 0))
                     |> fst)
                    (Some(recordAt OpPhase.Muster 0))
                    "a room unpriced: no move"

                Expect.equal
                    (Operations.truth
                        (factsAt 10 [])
                        unpriced
                        (recordAt OpPhase.Muster 0)
                        (OpGuard.Not OpGuard.SafeModeIn))
                    Truth.Unknown
                    "not unknown is unknown"
            }

            test "the quarry guards are Unknown in a room not seen since the reset" {
                let worth =
                    { RoomWorth.empty with
                        Values = Map.ofList [ "h1", 100.0 ]
                    }

                let facts (warm: string -> bool) =
                    { factsAt 10 [ "W1N1", 1.0; "W2N1", 1.0 ] with
                        Worth =
                            fun room -> if room = "W1N1" then Some worth else Some RoomWorth.empty
                        Warm = warm
                    }

                let quarry warm =
                    Operations.truth
                        (facts warm)
                        sweep
                        (recordAt OpPhase.Execute 0)
                        OpGuard.QuarryIn

                Expect.equal (quarry (fun _ -> false)) Truth.Unknown "neither seen"
                Expect.equal (quarry (fun room -> room = "W1N1")) Truth.Yes "a body in sight there"

                Expect.equal
                    (quarry (fun room -> room = "W2N1"))
                    Truth.Unknown
                    "the room with it unseen"

                let dark =
                    { facts (fun _ -> true) with
                        Worth =
                            fun _ ->
                                Some
                                    { worth with
                                        Dark = Set.ofList [ "h1" ]
                                    }
                    }

                Expect.equal
                    (Operations.truth dark sweep (recordAt OpPhase.Execute 0) OpGuard.QuarryGone)
                    Truth.Unknown
                    "only a body out of sight: unknown"
            }

            test "a room is warm once sighted since the reset, never on a reset's seeded sighting" {
                let world =
                    { World.empty with
                        Sightings =
                            Map.ofList
                                [
                                    "W1N1", sighted 0 (Some "Trepidimous")
                                    "W2N1", sighted 990 None
                                    "W3N1", sighted 1_005 None
                                ]
                    }

                let warm = Operations.warmSince 1_000 world

                Expect.equal
                    ([ "W1N1"; "W2N1"; "W3N1"; "W4N1" ] |> List.map warm)
                    [ false; false; true; false ]
                    "only a sighting at or after the reset"
            }

            test "Ended is absorbing" {
                let ended =
                    { recordAt OpPhase.Execute 0 with
                        Ended = Some OpEnd.Expired
                    }

                Expect.equal
                    (Operations.advance (factsAt 9_999 [ "W1N1", 0.0 ]) sweep (Some ended))
                    (Some ended, [])
                    "unmoved, silent"
            }

            test "at most one jump a tick" {
                let chain =
                    { withExits [ OpGuard.Always, OpNext.To OpPhase.Execute ] with
                        Phases =
                            [
                                {
                                    Phase = OpPhase.Muster
                                    Jobs = []
                                    Exits = [ OpGuard.Always, OpNext.To OpPhase.Execute ]
                                    Urgent = []
                                }
                                {
                                    Phase = OpPhase.Execute
                                    Jobs = []
                                    Exits = [ OpGuard.Always, OpNext.Finish OpEnd.Done ]
                                    Urgent = []
                                }
                            ]
                    }

                let begun, _ = Operations.advance (factsAt 10 []) chain None
                Expect.equal (begun |> Option.map _.Phase) (Some OpPhase.Muster) "begun, no jump"

                let first, events = Operations.advance (factsAt 11 []) chain begun

                Expect.equal (first |> Option.map _.Phase) (Some OpPhase.Execute) "one jump"

                Expect.equal
                    events
                    [ OpEvent.Moved("s1", OpPhase.Muster, OpPhase.Execute, 0, None) ]
                    "its event"

                let second, _ = Operations.advance (factsAt 12 []) chain first
                Expect.equal (second |> Option.bind _.Ended) (Some OpEnd.Done) "the next tick's"

                // Past the deadline, it expires whatever exit it could take:
                // the deadline is hard.
                let late, _ =
                    Operations.advance (factsAt 6_000 []) chain (Some(recordAt OpPhase.Muster 0))

                Expect.equal
                    (late |> Option.map (fun r -> r.Phase, r.Ended))
                    (Some(OpPhase.Muster, Some OpEnd.Expired))
                    "expired, not moved"
            }

            test "past the deadline with no exit taken, it expires" {
                let spec = withExits [ OpGuard.InPhaseFor 10_000, OpNext.To OpPhase.Execute ]

                Expect.equal
                    (Operations.advance (factsAt 5_000 []) spec (Some(recordAt OpPhase.Muster 0)))
                    (Some
                        { recordAt OpPhase.Muster 0 with
                            Ended = Some OpEnd.Expired
                        },
                     [ OpEvent.Ended("s1", OpEnd.Expired) ])
                    "expired"
            }

            test "a Memory op on its deadline tick ends through step, never refused nor dropped" {
                let begin' = Operation.admit standing [] [ wire ]
                let begun, _ = Operations.step (factsAt 1_000 []) begin' []

                let atDeadline = Operation.admit (standingAt 6_000) begun [ wire ]

                Expect.equal
                    (atDeadline.Admitted |> List.map (fun spec -> spec.Limits))
                    (begin'.Admitted |> List.map (fun spec -> spec.Limits))
                    "admitted on its deadline tick, its limits as on its first"

                let records, events = Operations.step (factsAt 6_000 []) atDeadline begun

                Expect.equal
                    (records |> List.map _.Ended)
                    [ Some OpEnd.Expired ]
                    "ended, its record kept"

                Expect.equal events [ OpEvent.Ended("op1", OpEnd.Expired) ] "no Dropped"

                // A tick on, the spec still listed: refused as ended, its record kept.
                let after = Operation.admit (standingAt 6_001) records [ wire ]
                Expect.equal after.Refused [ { Id = "op1"; Why = "id ended" } ] "id ended"

                Expect.equal
                    (Operations.step (factsAt 6_001 []) after records)
                    (records, [])
                    "kept, silent"
            }

            test "a spent budget stops it, as an event" {
                let spent =
                    { recordAt OpPhase.Execute 0 with
                        Spent = limits.MaxEnergy
                    }

                let _, events =
                    Operations.step (factsAt 10 [ "W1N1", 1.0 ]) (admitting [ sweep ]) [ spent ]

                Expect.equal events [ OpEvent.Ended("s1", OpEnd.Budget) ] "ended on its budget"
            }

            test "an Every schedule recurs past its window and waits for its next run" {
                let every =
                    { withExits [] with
                        Schedule = OpSchedule.Every(100, 30)
                    }

                let begun, _ = Operations.advance (factsAt 10 []) every None
                Expect.equal (begun |> Option.map _.NextDue) (Some 110) "next due a period on"

                let recurred, events = Operations.advance (factsAt 40 []) every begun

                Expect.equal (recurred |> Option.map _.Started) (Some 110) "waits"
                Expect.equal events [ OpEvent.Recurred("s1", 110) ] "its event"

                Expect.equal
                    (Operations.advance (factsAt 50 []) every recurred)
                    (recurred, [])
                    "waiting: unmoved"
            }

            test "an Every schedule runs only with a period and a window inside it" {
                Expect.equal
                    ([ 0, 0; 10, 11; 10, -1; 10, 10; 10, 0 ]
                     |> List.map (OpSchedule.Every >> Operation.validSchedule))
                    [ false; false; false; true; true ]
                    "period > 0, 0 <= window <= period"
            }

            test "a When schedule begins on its guard's first Yes" {
                let spec =
                    { withExits [] with
                        Schedule = OpSchedule.When(OpGuard.RateAbove 5.0)
                    }

                Expect.equal
                    (Operations.advance (factsAt 10 [ "W1N1", 1.0 ]) spec None)
                    (None, [])
                    "not yet"

                Expect.isSome
                    (Operations.advance (factsAt 10 [ "W1N1", 6.0 ]) spec None |> fst)
                    "begun"
            }

            test "a record whose spec is gone is dropped" {
                Expect.equal
                    (Operations.step (factsAt 10 []) (admitting []) [ recordAt OpPhase.Execute 0 ])
                    ([], [ OpEvent.Dropped "s1" ])
                    "dropped"
            }
        ]

[<Tests>]
let operationDecodeTests =
    testList
        "a Memory spec admitted or refused (#663 S7a)"
        [
            test "the sweep template, its deadline defaulted, its budget the template's ceiling" {
                match Operation.decode standing None wire with
                | Ok spec ->
                    Expect.equal spec.Limits.Deadline 6_000 "now and the default span"

                    Expect.equal
                        spec.Limits.MaxBodies
                        Operation.Templates.sweepBodies
                        "the template's bodies"

                    Expect.equal
                        spec.Limits.MaxEnergy
                        (Operation.ceiling Operation.Templates.sweepBodies Operation.defaultSpan)
                        "the ceiling"

                    Expect.equal
                        spec.Target
                        (OpTarget.Among(RoomClass.Listed [ "W8N8"; "W9N9" ]))
                        "its rooms"
                | Error why -> failtest $"%A{why}"
            }

            test "a running op's limits are priced from its first tick, not re-priced as it runs" {
                let at (now: int) =
                    Operation.decode
                        (standingAt now)
                        (Some 1_000)
                        { wire with Deadline = Some 6_000 }
                    |> Result.map _.Limits

                Expect.equal (at 5_999) (at 1_000) "the same budget late in its life"
                Expect.isOk (at 7_000) "past its deadline, still admitted: step ends it"
            }

            test "a parameter only narrows: a budget past the ceiling is the ceiling" {
                match
                    Operation.decode
                        standing
                        None
                        { wire with
                            MaxEnergy = Some System.Int32.MaxValue
                            MaxBodies = Some 99
                        }
                with
                | Ok spec ->
                    Expect.equal
                        spec.Limits.MaxBodies
                        Operation.Templates.sweepBodies
                        "bodies capped"

                    Expect.equal
                        spec.Limits.MaxEnergy
                        (Operation.ceiling Operation.Templates.sweepBodies Operation.defaultSpan)
                        "budget capped"
                | Error why -> failtest $"%A{why}"
            }

            test "never an ally, a neutral, ours, or a room an ally or a neutral holds" {
                let refusal (changed: OpSpecWire) =
                    match Operation.decode standing None changed with
                    | Error why -> Some why
                    | Ok _ -> None

                let unsafe why = Some(OpRefused.Unsafe why)
                let holds why = Some(OpRefused.Holds why)

                Expect.equal
                    (refusal { wire with Enemy = "Odiodin" })
                    (unsafe "an ally: Odiodin")
                    "an ally"

                Expect.equal
                    (refusal { wire with Enemy = "somebody" })
                    (unsafe "a neutral: somebody")
                    "a neutral"

                Expect.equal (refusal { wire with Rooms = [ "W5N5" ] }) (unsafe "ours: W5N5") "ours"

                Expect.equal
                    (refusal { wire with Rooms = [ "W6N6" ] })
                    (unsafe "an ally's: W6N6")
                    "an ally's room"

                Expect.equal
                    (refusal { wire with Rooms = [ "W7N7" ] })
                    (unsafe "a neutral's: W7N7")
                    "a neutral's room"

                Expect.equal
                    (refusal { wire with Rooms = [ "W4N4"; "W6N6" ] })
                    (unsafe "an ally's: W6N6")
                    "an unsafe room before an unknown one"

                Expect.equal
                    (refusal { wire with Rooms = [ "W4N4" ] })
                    (holds "holder unknown: W4N4")
                    "a holder nothing we keep knows"

                Expect.equal
                    (refusal { wire with Rooms = [ "nowhere" ] })
                    (holds "no room: nowhere")
                    "no room"

                Expect.equal
                    (refusal { wire with Rooms = [ "W05N5" ] })
                    (holds "no room: W05N5")
                    "a name off its one spelling: W05N5 is W5N5's alias"

                Expect.equal (refusal { wire with Rooms = [] }) (holds "no rooms") "none"

                Expect.equal
                    (refusal { wire with Template = "nuke" })
                    (holds "no template: nuke")
                    "no template"

                Expect.equal
                    (refusal { wire with MaxEnergy = Some 0 })
                    (holds "no budget")
                    "no budget"

                Expect.equal
                    (refusal { wire with Deadline = Some 1_000 })
                    (holds "deadline past")
                    "past"

                Expect.equal
                    (refusal
                        { wire with
                            Deadline = Some(1_000 + Operation.maxSpan + 1)
                        })
                    (holds "deadline too far")
                    "too far"

                Expect.isNone
                    (refusal
                        { wire with
                            Deadline = Some(1_000 + Operation.maxSpan)
                        })
                    "the longest span"
            }

            test
                "the holder off what we keep: a look, the sightings, the declared rivals, the dossier" {
                let world =
                    { World.empty with
                        Rooms =
                            Map.ofList
                                [
                                    "W1N1", looked Ownership.Unowned (Some "Odiodin")
                                    "W2N1", looked Ownership.Unowned (Some "Kalgen")
                                    "W3N1", looked Ownership.Unowned None
                                    "W4N1", looked Ownership.Unowned (Some "me")
                                ]
                        Sightings = Map.ofList [ "W6N1", sighted 900 (Some "Trepidimous") ]
                    }

                let remembered =
                    function
                    | "W7N1" -> Some "Kalgen"
                    | _ -> None

                let standing = Operations.standingOf 1_000 (Set.ofList [ "W5N1" ]) remembered world

                Expect.equal
                    ([ "W1N1"; "W2N1"; "W3N1"; "W6N1"; "W7N1"; "W9S26"; "W8N1" ]
                     |> List.map standing.Holder)
                    [
                        RoomHolder.Player "Odiodin"
                        RoomHolder.Player "Kalgen"
                        RoomHolder.Nobody
                        RoomHolder.Player "Trepidimous"
                        RoomHolder.Player "Kalgen"
                        RoomHolder.Player "Kalgen"
                        RoomHolder.Unknown
                    ]
                    "seen, sighted, remembered, declared (Kalgen's W9S26), else unknown"

                Expect.equal
                    ([ "W4N1"; "W5N1"; "W3N1" ] |> List.map standing.Ours)
                    [ true; true; false ]
                    "ours: reserved by us this tick, or declared"

                let refusal room =
                    match Operation.decode standing None { wire with Rooms = [ room ] } with
                    | Error(OpRefused.Unsafe why)
                    | Error(OpRefused.Holds why) -> why
                    | Ok _ -> "admitted"

                Expect.equal
                    ([ "W1N1"; "W2N1"; "W9S26"; "W8N1"; "W3N1" ] |> List.map refusal)
                    [
                        "an ally's: W1N1"
                        "a neutral's: W2N1"
                        "a neutral's: W9S26"
                        "holder unknown: W8N1"
                        "admitted"
                    ]
                    "Odiodin's reservation, Kalgen's reservation and room, an ally's room out of sight"
            }

            test "admitted in order: a duplicate id refused, and no more live than the cap" {
                let wires =
                    [ 1 .. Operation.liveCap + 1 ]
                    |> List.map (fun i -> { wire with Id = $"op{i}" })

                let admission = Operation.admit standing [] (wires @ [ { wire with Id = "op1" } ])

                Expect.equal
                    (admission.Admitted |> List.map _.Id)
                    [ for i in 1 .. Operation.liveCap -> $"op{i}" ]
                    "the first"

                Expect.equal
                    admission.Refused
                    [
                        {
                            Id = $"op{Operation.liveCap + 1}"
                            Why = "live-op cap"
                        }
                        { Id = "op1"; Why = "duplicate id" }
                    ]
                    "the rest"

                // An ended op holds no live seat.
                let ended =
                    { recordAt OpPhase.Execute 0 with
                        Id = "op1"
                        Ended = Some OpEnd.Done
                    }

                let admission = Operation.admit standing [ ended ] wires

                Expect.equal
                    (List.length admission.Admitted)
                    Operation.liveCap
                    "every one but the ended"

                Expect.isTrue (admission.Kept.Contains "op1") "the ended record kept"

                // A running op keeps its seat before a newcomer listed ahead of it.
                let running =
                    { recordAt OpPhase.Execute 0 with
                        Id = $"op{Operation.liveCap + 1}"
                    }

                let admission = Operation.admit standing [ running ] wires

                Expect.contains
                    (admission.Admitted |> List.map _.Id)
                    running.Id
                    "the running one seated"

                Expect.equal
                    admission.Refused
                    [
                        {
                            Id = $"op{Operation.liveCap}"
                            Why = "live-op cap"
                        }
                    ]
                    "the last newcomer"
            }

            test "a refusal keeps a running record unmoved, unless its spec turned unsafe" {
                let runningAs (id: string) =
                    { recordAt OpPhase.Execute 0 with
                        Id = id
                    }

                let records = [ runningAs "unknown"; runningAs "ally"; runningAs "typo" ]

                let wires =
                    [
                        { wire with
                            Id = "unknown"
                            Rooms = [ "W4N4" ]
                        }
                        { wire with
                            Id = "ally"
                            Rooms = [ "W6N6" ]
                        }
                        { wire with
                            Id = "typo"
                            Template = "swep"
                        }
                    ]

                let admission = Operation.admit standing records wires
                let kept, events = Operations.step (factsAt 1_000 []) admission records

                Expect.equal (kept |> List.map _.Id) [ "unknown"; "typo" ] "held, unmoved"
                Expect.equal events [ OpEvent.Dropped "ally" ] "the ally's room dropped"
            }

            test "the sweep template's every exit leads to a phase it has, on a schedule that runs" {
                Expect.isTrue (Operation.validSchedule sweep.Schedule) "its schedule"

                for phase in sweep.Phases do
                    for _, next in phase.Exits do
                        match next with
                        | OpNext.To into ->
                            Expect.isTrue
                                (sweep.Phases |> List.exists (fun p -> p.Phase = into))
                                $"{into}"
                        | _ -> ()
            }
        ]

/// One answer of the sweep's: in W1N1, a group of two, a strike seating two.
let private answer: SweepAnswer =
    {
        Room = Some "W1N1"
        Group = set [ "a"; "b" ]
        Scoped = Set.empty
        Jobs = [ JobKind.Strike ]
        Seats = 2
    }

/// The shadow's answer for the same tick: the phase's declared jobs.
let private shadowed =
    { answer with
        Jobs = [ JobKind.Strike; JobKind.Camp; JobKind.Ring; JobKind.Muster ]
    }

[<Tests>]
let sweepShadowTests =
    testList
        "the sweep's shadow (#663 S7b)"
        [
            test "the code declares the sweep over every live candidate, against their one enemy" {
                Expect.equal
                    (Colony.operations |> Result.map (List.collect _.Rooms))
                    (Ok(Colony.harass |> List.map _.RoomName))
                    "the live candidates"

                for candidates in [ Colony.harass; Colony.harassDeclared ] do
                    match Colony.sweepOf candidates with
                    | Ok [ wire ] ->
                        Expect.all candidates (fun h -> h.Enemy = wire.Enemy) "one enemy"
                    | Ok [] -> Expect.isEmpty candidates "none with no candidate"
                    | other -> failtest $"one sweep: {other}"
            }

            test "candidates of two enemies are refused by name, never read as the first's" {
                let mixed: Harass list =
                    [
                        {
                            RoomName = "W8N8"
                            Enemy = "Trepidimous"
                            Via = []
                        }
                        {
                            RoomName = "W9N9"
                            Enemy = "Shibdib"
                            Via = []
                        }
                    ]

                Expect.equal
                    (Colony.sweepOf mixed)
                    (Error "sweep candidates of two enemies: Trepidimous, Shibdib")
                    "refused"

                Expect.equal
                    (Operations.declaredSweep standing (Colony.sweepOf mixed))
                    (None,
                     [
                         {
                             Id = "sweep"
                             Why = "sweep candidates of two enemies: Trepidimous, Shibdib"
                         }
                     ])
                    "no spec, one refusal"
            }

            test
                "a code spec runs with no deadline nor budget but what it names; an unknown template is refused" {
                match Operation.ofCode standing wire with
                | Ok(spec, []) ->
                    Expect.equal
                        (spec.Limits.Deadline, spec.Limits.MaxEnergy)
                        (System.Int32.MaxValue, System.Int32.MaxValue)
                        "open-ended"
                | other -> failtest $"admitted whole: {other}"

                Expect.equal
                    (Operation.ofCode standing { wire with Template = "siege" })
                    (Error
                        {
                            Id = "op1"
                            Why = "no template: siege"
                        })
                    "unknown"
            }

            test
                "a code spec passes the Memory spec's safety: an ally or a neutral as enemy refuses it, a room that is ours, an ally's or a neutral's is dropped, an unknown holder stays" {
                Expect.equal
                    (Operation.ofCode standing { wire with Enemy = "Odiodin" })
                    (Error { Id = "op1"; Why = "an ally: Odiodin" })
                    "an ally"

                Expect.equal
                    (Operation.ofCode standing { wire with Enemy = "somebody" })
                    (Error
                        {
                            Id = "op1"
                            Why = "a neutral: somebody"
                        })
                    "a neutral"

                match
                    Operation.ofCode
                        standing
                        { wire with
                            Rooms = [ "W5N5"; "W6N6"; "W7N7"; "W4N4"; "W8N8" ]
                        }
                with
                | Ok(spec, dropped) ->
                    Expect.equal
                        spec.Target
                        (OpTarget.Among(RoomClass.Listed [ "W4N4"; "W8N8" ]))
                        "kept"

                    Expect.equal
                        (dropped |> List.map _.Why)
                        [ "ours: W5N5"; "an ally's: W6N6"; "a neutral's: W7N7" ]
                        "each dropped room refused"
                | Error refused -> failtest $"refused whole: {refused}"
            }

            test "a fault is recorded once a heap: a repeat of its message is not recorded again" {
                let first = Operation.firstSeen Set.empty "boom"
                Expect.equal first (Some(set [ "boom" ])) "the first recorded"
                Expect.isNone (Operation.firstSeen first.Value "boom") "the repeat not"
                Expect.isSome (Operation.firstSeen first.Value "bang") "another message is"
            }

            test
                "the shadow agrees where every answer is the sweep's, its phase's declared jobs a superset of those laid" {
                Expect.isEmpty (Operations.disagreements 10 shadowed answer) "agreed"
            }

            test
                "another room is one disagreement: the jobs and the seats are compared only in one room" {
                let examples =
                    Operations.disagreements 10 { shadowed with Room = None; Seats = 0 } answer

                Expect.equal
                    (examples |> List.map (fun e -> e.Kind, e.Room, e.Shadow, e.Sweep))
                    [ Divergence.Room, "W1N1", "none", "W1N1" ]
                    "the room alone"

                Expect.stringStarts
                    (List.head examples).Why
                    "the spec works no room"
                    "why: the spec's own phase"
            }

            test
                "in one room each answer that differs is one example, the group as the bodies only one side counts" {
                let examples =
                    Operations.disagreements
                        10
                        { shadowed with
                            Group = set [ "a"; "c" ]
                            Seats = 1
                        }
                        { answer with
                            Jobs = [ JobKind.HoldExit; JobKind.Strike ]
                        }

                Expect.equal
                    (examples |> List.map (fun e -> e.Kind, e.Shadow, e.Sweep))
                    [
                        Divergence.Group, "c", "b"
                        Divergence.Jobs, "Strike,Camp,Ring,Muster", "HoldExit,Strike"
                        Divergence.Seats, "1", "2"
                    ]
                    "each kind once"

                Expect.equal (examples |> List.map _.Tick |> List.distinct) [ 10 ] "stamped"
            }

            test
                "a group difference made only of bodies the shadow counts from a colony not casting their room is the roster's scope" {
                let wider =
                    { shadowed with
                        Group = set [ "a"; "b"; "c" ]
                        Scoped = set [ "c" ]
                    }

                Expect.equal
                    (Operations.disagreements 10 wider answer |> List.map _.Kind)
                    [ Divergence.Scope ]
                    "scope"

                Expect.equal
                    (Operations.disagreements 10 { wider with Group = set [ "a"; "c" ] } answer
                     |> List.map _.Kind)
                    [ Divergence.Group ]
                    "a body the sweep counts and the shadow does not is a group difference"
            }

            test
                "the log counts each kind, keeps the latest newest first, one example a standing disagreement, at most its cap" {
                let room tick =
                    Operations.disagreements tick { shadowed with Room = None } answer

                let log =
                    [ 1..3 ]
                    |> List.fold
                        (fun log tick -> Operations.logged log answer (room tick))
                        Operation.shadowEmpty

                Expect.equal (log.Compared, log.Working) (3, 3) "three compared, each worked"
                Expect.equal log.Counts (Map.ofList [ Divergence.Room, 3 ]) "counted"
                Expect.equal (log.Last |> List.map _.Tick) [ 3 ] "one example, the latest"

                let many =
                    [ 1..12 ]
                    |> List.fold
                        (fun log tick ->
                            Operations.logged
                                log
                                { answer with Room = None }
                                (Operations.disagreements
                                    tick
                                    { shadowed with Seats = tick }
                                    answer))
                        Operation.shadowEmpty

                Expect.equal many.Working 0 "no room worked"

                Expect.equal
                    (many.Last |> List.map _.Tick)
                    [ 12..-1..5 ]
                    "the cap's newest, newest first"
            }
        ]
