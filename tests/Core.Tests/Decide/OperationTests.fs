/// The operation (#663 M7): a spec's guards, phases and exits, its record's
/// latch moved on by the interpreter's pure step, and the decoder that
/// admits or refuses a Memory spec.
module Fabot.Core.Tests.Decide.OperationTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Decide

module Observe = Fabot.Core.Observe

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
        // No group: a move lands at once.
        Arrival = fun room -> Map.tryFind room rates |> Option.map (fun _ -> 0)
        Sighted = fun _ -> true
        HomeThreatened = Truth.No
        ExitsUsed = lazy Truth.Unknown
        Pays = lazy Truth.Unknown
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
        Bids = []
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
                        Arrival = fun _ -> failwith "priced with no op"
                        Sighted = fun _ -> failwith "read with no op"
                        HomeThreatened = Truth.Unknown
                        ExitsUsed = lazy (failwith "read with no op")
                        Pays = lazy (failwith "priced with no op")
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

            // Live W19S26 after S7c: the hold entered on nothing paying dropped
            // the room, and the execute after it took the plain argmax.
            test
                "execute in a room, hold when nothing pays, execute again: the same room, though another pays a little more one tick" {
                let executing =
                    { recordAt OpPhase.Execute 40 with
                        Room = Some "W1N1"
                        RoomSince = 20
                    }

                let spent =
                    { factsAt 50 [ "W1N1", 0.0; "W2N1", 0.0 ] with
                        Holds = fun _ -> Truth.No
                        Loses = fun _ -> Truth.No
                    }

                let held = Operations.advance spent sweep (Some executing) |> fst

                Expect.equal
                    (held |> Option.map (fun r -> r.Phase, r.Room, r.RoomSince))
                    (Some(OpPhase.Hold, Some "W1N1", 20))
                    "the hold keeps the room and its commitment"

                // W1N1 pays again; W2N1 a little more on bodies seen there
                // once: no evidence it outpays.
                let back =
                    { factsAt 60 [ "W1N1", 1.0; "W2N1", 1.1 ] with
                        Holds = fun _ -> Truth.Yes
                        Loses = fun _ -> Truth.No
                        Challenge = fun room -> if room = "W2N1" then Some 0.0 else Some 1.0
                    }

                Expect.equal
                    (Operations.advance back sweep held
                     |> fst
                     |> Option.map (fun r -> r.Phase, r.Room, r.RoomSince))
                    (Some(OpPhase.Execute, Some "W1N1", 20))
                    "back to the room it was committed to"

                // Exhausted with another paying: the hold's room moves on.
                let elsewhere =
                    { factsAt 50 [ "W1N1", 0.0; "W2N1", 1.0 ] with
                        Holds = fun room -> if room = "W1N1" then Truth.No else Truth.Yes
                        Loses = fun _ -> Truth.No
                    }

                Expect.equal
                    (Operations.advance elsewhere sweep (Some executing)
                     |> fst
                     |> Option.map (fun r -> r.Phase, r.Room, r.RoomSince))
                    (Some(OpPhase.Execute, Some "W2N1", 50))
                    "exhausted, another paying: that room, committed from now"
            }

            // The S7c fix's review: above, W2N1's challenge is its rate, so
            // `Outpaid` takes the room. Live, an exhausted room's successor
            // often has no body seen on two full ticks (challenge nothing)
            // while its plain rate pays: the exhausted branch of the entry.
            test
                "exhausted, another paying on its rate though on no evidence: that room, committed from now" {
                let executing =
                    { recordAt OpPhase.Execute 40 with
                        Room = Some "W1N1"
                        RoomSince = 20
                    }

                let spent =
                    { factsAt 50 [ "W1N1", 0.0; "W2N1", 1.0 ] with
                        Holds = fun room -> if room = "W1N1" then Truth.No else Truth.Yes
                        Loses = fun _ -> Truth.No
                        Challenge = fun _ -> Some 0.0
                    }

                Expect.equal
                    (Operations.truth spent sweep executing OpGuard.Outpaid)
                    Truth.No
                    "the premise: nothing outpays it on evidence"

                Expect.equal
                    (Operations.advance spent sweep (Some executing)
                     |> fst
                     |> Option.map (fun r -> r.Phase, r.Room, r.RoomSince))
                    (Some(OpPhase.Execute, Some "W2N1", 50))
                    "the room moves on its exhaustion"

                // From a hold, the same: the room it kept is spent, W2N1 pays.
                let holding = { executing with Phase = OpPhase.Hold }

                Expect.equal
                    (Operations.advance spent sweep (Some holding)
                     |> fst
                     |> Option.map (fun r -> r.Phase, r.Room, r.RoomSince))
                    (Some(OpPhase.Execute, Some "W2N1", 50))
                    "out of the hold into W2N1"
            }

            // Live W19S26, bundle f837bfb7, t1,065,819-832: eleven
            // `execute>execute` events through `Outpaid`, the room unchanged
            // in each, every one re-committing it from that tick.
            test
                "an exit into the phase it is in with the room it holds is no transition: no event, its commitment kept" {
                let executing =
                    { recordAt OpPhase.Execute 40 with
                        Room = Some "W1N1"
                        RoomSince = 20
                    }

                // Exhausted, and the target's best is the room it holds: the
                // exhausted exit re-enters Execute with W1N1.
                let spent =
                    { factsAt 50 [ "W1N1", 1.0; "W2N1", 0.0 ] with
                        Holds = fun room -> if room = "W1N1" then Truth.No else Truth.Yes
                        Loses = fun _ -> Truth.No
                    }

                Expect.equal
                    (Operations.exitOf
                        spent
                        sweep
                        executing
                        (sweep.Phases |> List.find (fun p -> p.Phase = OpPhase.Execute)).Exits
                     |> Option.map fst)
                    (Some 2)
                    "the premise: the exhausted exit into Execute is taken"

                Expect.equal
                    (Operations.advance spent sweep (Some executing))
                    (Some executing, [])
                    "unmoved, silent"
            }

            test "outpaid, the room it moves to is the one that outpaid it, never the one it holds" {
                let executing =
                    { recordAt OpPhase.Execute 40 with
                        Room = Some "W1N1"
                        RoomSince = 20
                    }

                // W2N1's challenge beats W1N1's rate, though W1N1's rate is
                // the plain argmax: the shape the live events had.
                let outpaid =
                    { factsAt 50 [ "W1N1", 1.0; "W2N1", 0.5 ] with
                        Holds = fun _ -> Truth.Yes
                        Loses = fun _ -> Truth.No
                        Challenge = fun room -> if room = "W2N1" then Some 1.5 else Some 1.0
                    }

                Expect.equal
                    (Operations.advance outpaid sweep (Some executing)
                     |> fst
                     |> Option.map (fun r -> r.Phase, r.Room, r.RoomSince))
                    (Some(OpPhase.Execute, Some "W2N1", 50))
                    "into W2N1, committed from now"
            }

            // Live W18S27 <-> W19S26, bundle 66de1701, t1,070,572-608: eleven
            // `Outpaid` moves, one to seven ticks apart, while no body crossed
            // between the rooms. The group: a ranger standing in W18S27 and
            // one walking from W17S29, its slowest body some 115 ticks from
            // W18S27's miner and 138 from W19S26's. Which reading turned the
            // comparison on each tick the replay cannot say (our looks are
            // not in it); here it turns on exactly the live ticks, priced as
            // the arena prices the two rooms off the capture: W19S26 at 20
            // in sight and 3.5 on evidence out of it, W18S27 at 7.5 to 12.
            test
                "t1,070,567-610: a comparison turning on each live move's tick never moves the latch before the move could land" {
                let live =
                    [ 572, "W19S26"; 579, "W18S27"; 581, "W19S26"; 582, "W18S27"; 584, "W19S26" ]
                    @ [ 588, "W18S27"; 590, "W19S26"; 592, "W18S27"; 593, "W19S26" ]
                    @ [ 606, "W18S27"; 608, "W19S26" ]
                    |> List.map (fun (t, room) -> 1_070_000 + t, room)

                // The room paying more on a tick: the one live had just moved to.
                let ahead (now: int) =
                    live
                    |> List.filter (fun (t, _) -> t <= now)
                    |> List.tryLast
                    |> Option.map snd
                    |> Option.defaultValue "W18S27"

                let spec =
                    Operation.Templates.sweep
                        "sweep"
                        "Trepidimous"
                        [ "W18S27"; "W19S26" ]
                        { limits with
                            Deadline = System.Int32.MaxValue
                        }

                let factsOn (walked: int -> string -> int) (now: int) =
                    let rate room =
                        match room, ahead now with
                        | "W19S26", "W19S26" -> 20.0
                        | "W19S26", _ -> 3.5
                        | _, "W19S26" -> 7.5
                        | _ -> 12.0

                    { factsAt now [ "W18S27", rate "W18S27"; "W19S26", rate "W19S26" ] with
                        Holds = fun _ -> Truth.Yes
                        Loses = fun _ -> Truth.No
                        Arrival = fun room -> Some(walked now room)
                    }

                // The slowest body a tile a tick nearer both rooms' quarries.
                let walking (now: int) (room: string) =
                    let k = now - 1_070_567
                    if room = "W18S27" then max 0 (115 - k) else max 0 (138 - k)

                let start =
                    { recordAt OpPhase.Execute 1_070_552 with
                        Id = "sweep"
                        Room = Some "W18S27"
                    }

                let moves (walked: int -> string -> int) =
                    ((Some start, []), [ 1_070_567..1_070_610 ])
                    ||> List.fold (fun (record, moved) now ->
                        let next, _ = Operations.advance (factsOn walked now) spec record
                        let room = next |> Option.bind _.Room

                        next,
                        (if room <> (record |> Option.bind _.Room) then
                             (now, Option.defaultValue "" room) :: moved
                         else
                             moved))
                    |> snd
                    |> List.rev

                Expect.equal
                    (moves (fun _ _ -> 0))
                    live
                    "the premise: read with no horizon (main's rule), the latch moves on each live tick"

                Expect.isEmpty
                    (moves walking)
                    "with the group's walk to each quarry: held, all 44 ticks"
            }

            test
                "a room paying for the walk its move adds takes the latch once it has paid for the ticks the move takes, never before" {
                let spec = sweep

                let executing =
                    { recordAt OpPhase.Execute 40 with
                        Room = Some "W1N1"
                        RoomSince = 20
                    }

                // The group at W1N1's quarry, W2N1's ten ticks off: W2N1
                // paying 3 against 1 earns 30 over its ten, against W1N1's
                // 10 over them and 10 over the walk the move adds.
                let factsOn (paid: float) (now: int) =
                    { factsAt now [ "W1N1", 1.0; "W2N1", paid ] with
                        Holds = fun _ -> Truth.Yes
                        Loses = fun _ -> Truth.No
                        Arrival = fun room -> if room = "W2N1" then Some 10 else Some 0
                    }

                let run (paid: float) =
                    ((Some executing, []), [ 50..70 ])
                    ||> List.fold (fun (record, events) now ->
                        let next, moved = Operations.advance (factsOn paid now) spec record
                        next, events @ (moved |> List.map (fun event -> now, event)))
                    |> snd

                Expect.equal
                    (run 3.0)
                    [
                        60,
                        OpEvent.Moved(
                            "s1",
                            OpPhase.Execute,
                            OpPhase.Execute,
                            1,
                            Some "W2N1",
                            Some { Held = 1.0; Paid = 3.0; Horizon = 10 }
                        )
                    ]
                    "bidding from t50, it moves at t60, its ten ticks paid, with the numbers that decided it"

                Expect.isEmpty
                    (run 1.9)
                    "paying under twice W1N1 from W1N1's quarry: the walk the move adds is not paid, it stays"

                // Equidistant, the move adds no walk: any surplus that holds
                // for its horizon pays.
                let level (now: int) =
                    { factsOn 1.1 now with
                        Arrival = fun _ -> Some 10
                    }

                Expect.equal
                    (Operations.advance
                        (level 60)
                        spec
                        (Some { executing with Bids = [ "W2N1", 50 ] })
                     |> fst
                     |> Option.bind _.Room)
                    (Some "W2N1")
                    "equidistant, a surplus held for its horizon takes the latch"
            }

            // The ship-gate review: one count for the top bid alone was reset
            // by every swap of two rooms that both outbid the held one, so
            // neither ever moved it.
            test
                "two rooms both outbidding the held one, swapping places every tick: each bid stands on its own, and the better takes the latch after its horizon" {
                let spec =
                    Operation.Templates.sweep "s1" "Trepidimous" [ "W1N1"; "W2N1"; "W3N1" ] limits

                let executing =
                    { recordAt OpPhase.Execute 40 with
                        Room = Some "W1N1"
                        RoomSince = 20
                    }

                let factsOn (now: int) =
                    let high, low = if now % 2 = 0 then "W2N1", "W3N1" else "W3N1", "W2N1"

                    { factsAt now [ "W1N1", 1.0; high, 3.0; low, 2.5 ] with
                        Holds = fun _ -> Truth.Yes
                        Loses = fun _ -> Truth.No
                        Arrival = fun room -> if room = "W1N1" then Some 0 else Some 10
                    }

                let moves =
                    ((Some executing, []), [ 50..70 ])
                    ||> List.fold (fun (record, moved) now ->
                        let next, events = Operations.advance (factsOn now) spec record

                        next,
                        moved
                        @ (events
                           |> List.choose (function
                               | OpEvent.Moved(_, _, _, _, room, _) -> Some(now, room)
                               | _ -> None)))
                    |> snd

                Expect.equal
                    moves
                    [ 60, Some "W2N1" ]
                    "both bid from t50; at t60, both stood ten ticks, the one paying more that tick"
            }

            // The ship-gate review: a room seen only on an observer's looks
            // bid only on them, and its count restarted in between. A tick
            // nothing sees the room neither confirms its bid nor resets it.
            test
                "a better room seen every fifth tick takes the latch after its horizon; one seen paying once, then seen not paying, never does" {
                let executing =
                    { recordAt OpPhase.Execute 40 with
                        Room = Some "W1N1"
                        RoomSince = 20
                    }

                // W2N1 in sight on every fifth tick, where it pays `seen now`;
                // between looks its bodies are priced from memory at 3, and on
                // evidence at 0.5 (none sampled twice).
                let factsOn (seen: int -> float) (now: int) =
                    let sighted = now % 5 = 0
                    let plain = if sighted then seen now else 3.0
                    let paid = if sighted then seen now else 0.5

                    { factsAt now [ "W1N1", 1.0; "W2N1", plain ] with
                        Holds = fun _ -> Truth.Yes
                        Loses = fun _ -> Truth.No
                        Challenge = fun room -> if room = "W2N1" then Some paid else Some 1.0
                        Arrival = fun room -> if room = "W2N1" then Some 10 else Some 0
                        Sighted = fun room -> room = "W1N1" || sighted
                    }

                let moves (seen: int -> float) =
                    ((Some executing, []), [ 50..80 ])
                    ||> List.fold (fun (record, moved) now ->
                        let next, events = Operations.advance (factsOn seen now) sweep record

                        next,
                        moved
                        @ (events
                           |> List.choose (function
                               | OpEvent.Moved(_, _, _, _, room, _) -> Some(now, room)
                               | _ -> None)))
                    |> snd

                Expect.equal
                    (moves (fun _ -> 3.0))
                    [ 60, Some "W2N1" ]
                    "seen out-paying at t50, never seen not to: it moves at t60, its ten ticks stood"

                Expect.isEmpty
                    (moves (fun now -> if now = 50 then 3.0 else 1.0))
                    "seen paying at t50, seen not paying at t55: the bid falls, and memory alone starts none"
            }

            test "an exhausted room moves to one that pays at once, whatever a bid has yet to prove" {
                let executing =
                    { recordAt OpPhase.Execute 40 with
                        Room = Some "W1N1"
                        RoomSince = 20
                        Bids = [ "W2N1", 49 ]
                    }

                let spent =
                    { factsAt 50 [ "W1N1", 0.0; "W2N1", 1.0 ] with
                        Holds = fun room -> if room = "W1N1" then Truth.No else Truth.Yes
                        Loses = fun _ -> Truth.No
                        Arrival = fun room -> if room = "W2N1" then Some 30 else Some 0
                    }

                Expect.equal
                    (Operations.truth spent sweep executing OpGuard.Outpaid)
                    Truth.No
                    "the premise: W2N1 has bid for one tick of its thirty"

                let moved, events = Operations.advance spent sweep (Some executing)

                Expect.equal
                    (moved |> Option.map (fun r -> r.Phase, r.Room, r.RoomSince, r.Bids))
                    (Some(OpPhase.Execute, Some "W2N1", 50, []))
                    "into W2N1 on its exhaustion, committed from now, no bid carried over"

                Expect.equal
                    events
                    [
                        OpEvent.Moved(
                            "s1",
                            OpPhase.Execute,
                            OpPhase.Execute,
                            2,
                            Some "W2N1",
                            Some { Held = 0.0; Paid = 1.0; Horizon = 30 }
                        )
                    ]
                    "its event: the room it left paid nothing"
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
                    [ OpEvent.Moved("s1", OpPhase.Muster, OpPhase.Execute, 0, None, None) ]
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

/// An exit camp on W1N3's exits into W1N2 and W2N3 (#663 S7d).
let private camp =
    Operation.Templates.exitCamp "c1" "Trepidimous" [ "W1N3"; "W1N2"; "W2N3" ] limits

/// An exit camp's facts: prey uses its exits, it pays, and each of its rooms
/// reads its exchange, its muster and its arrival as given.
let private campAt
    (now: int)
    (used: Truth)
    (pays: Truth)
    (loses: string -> Truth)
    (assembled: Truth)
    (arrived: Truth)
    : OpFacts =
    { factsAt now [] with
        Loses = loses
        Assembled = fun _ -> assembled
        Arrived = fun _ -> arrived
        ExitsUsed = lazy used
        Pays = lazy pays
    }

let private campRecord (phase: OpPhase) (started: int) : OpRecord =
    { recordAt phase started with
        Id = "c1"
        Started = started
        NextDue = started + Operation.Templates.campSpan
    }

[<Tests>]
let exitCampSpecTests =
    testList
        "the exit camp's spec: its trigger, its phases and its exits (#663 S7d)"
        [
            test
                "its shape: one body an exit, its rooms the exits' landings, armed on a priced trigger" {
                Expect.equal
                    camp.Target
                    (OpTarget.Exits
                        [ { Home = "W1N3"; Camp = "W1N2" }; { Home = "W1N3"; Camp = "W2N3" } ])
                    "the home's exits into the rooms beside it"

                Expect.equal camp.Limits.MaxBodies 2 "one body an exit"

                Expect.equal
                    (camp.Phases |> List.map _.Phase)
                    [ OpPhase.Muster; OpPhase.Approach; OpPhase.Execute; OpPhase.Retreat ]
                    "muster, approach, execute, retreat"

                Expect.equal
                    camp.Schedule
                    (OpSchedule.Armed(
                        OpGuard.All
                            [ OpGuard.ExitsUsed; OpGuard.Pays; OpGuard.Not OpGuard.ExchangeLoses ],
                        Operation.Templates.campSpan,
                        Operation.Templates.campSpan
                    ))
                    "prey uses its exits, it pays, it wins: re-armed a lifetime after each run began"
            }

            test "its trigger: all three must hold, and Unknown never starts it" {
                let begins used pays (loses: Truth) =
                    Operations.advance
                        (campAt 100 used pays (fun _ -> loses) Truth.No Truth.No)
                        camp
                        None
                    |> fst
                    |> Option.map (fun record -> record.Phase)

                Expect.equal
                    (begins Truth.Yes Truth.Yes Truth.No)
                    (Some OpPhase.Muster)
                    "all three: begun"

                Expect.isNone (begins Truth.No Truth.Yes Truth.No) "no prey crossing: not"
                Expect.isNone (begins Truth.Yes Truth.No Truth.No) "not paying: not"
                Expect.isNone (begins Truth.Yes Truth.Yes Truth.Yes) "losing its exchange: not"
                Expect.isNone (begins Truth.Unknown Truth.Yes Truth.No) "crossings unknown: not"
                Expect.isNone (begins Truth.Yes Truth.Unknown Truth.No) "its price unknown: not"
                Expect.isNone (begins Truth.Yes Truth.Yes Truth.Unknown) "its exchange unknown: not"
            }

            test
                "losing the exchange at any exit retreats it; no longer paying ends it done; Unknown moves nothing" {
                let moved pays (loses: string -> Truth) =
                    Operations.advance
                        (campAt 200 Truth.Yes pays loses Truth.Yes Truth.Yes)
                        camp
                        (Some(campRecord OpPhase.Execute 100))
                    |> fun (record, events) ->
                        record |> Option.map (fun r -> r.Phase, r.Ended), events

                Expect.equal
                    (moved Truth.Yes (fun room -> if room = "W2N3" then Truth.Yes else Truth.No))
                    (Some(OpPhase.Retreat, None),
                     [ OpEvent.Moved("c1", OpPhase.Execute, OpPhase.Retreat, 0, None, None) ])
                    "one exit lost: retreat"

                Expect.equal
                    (moved Truth.No (fun _ -> Truth.No))
                    (Some(OpPhase.Execute, Some OpEnd.Done), [ OpEvent.Ended("c1", OpEnd.Done) ])
                    "the crossings decayed past its cost: done"

                Expect.equal
                    (moved Truth.Unknown (fun _ -> Truth.No))
                    (Some(OpPhase.Execute, None), [])
                    "its price unknown: held"

                Expect.equal
                    (moved Truth.No (fun room -> if room = "W1N2" then Truth.Unknown else Truth.No))
                    (Some(OpPhase.Execute, None), [])
                    "an exchange unknown ahead of the price: held"

                Expect.equal
                    (Operations.advance
                        (campAt 201 Truth.Yes Truth.Yes (fun _ -> Truth.No) Truth.Yes Truth.Yes)
                        camp
                        (Some(campRecord OpPhase.Retreat 100))
                     |> fst
                     |> Option.bind _.Ended)
                    (Some OpEnd.Abandoned)
                    "its retreat lays no work and ends the run"
            }

            test "muster, approach, execute: cast, then there" {
                let phaseAfter (phase: OpPhase) (assembled: Truth) (arrived: Truth) =
                    Operations.advance
                        (campAt 200 Truth.Yes Truth.Yes (fun _ -> Truth.No) assembled arrived)
                        camp
                        (Some(campRecord phase 100))
                    |> fst
                    |> Option.map _.Phase

                Expect.equal
                    (phaseAfter OpPhase.Muster Truth.No Truth.No)
                    (Some OpPhase.Muster)
                    "uncast: mustering"

                Expect.equal
                    (phaseAfter OpPhase.Muster Truth.Yes Truth.No)
                    (Some OpPhase.Approach)
                    "cast: on its way"

                Expect.equal
                    (phaseAfter OpPhase.Approach Truth.Yes Truth.No)
                    (Some OpPhase.Approach)
                    "walking"

                Expect.equal
                    (phaseAfter OpPhase.Approach Truth.Yes Truth.Yes)
                    (Some OpPhase.Execute)
                    "there"
            }

            test
                "re-armed: a run ended waits out its period, then its trigger again; its window ends a run; its deadline for good" {
                let ended (how: OpEnd) =
                    { campRecord OpPhase.Execute 100 with
                        Ended = Some how
                    }

                let at now pays record =
                    Operations.advance
                        (campAt now Truth.Yes pays (fun _ -> Truth.No) Truth.No Truth.No)
                        camp
                        (Some record)

                let due = 100 + Operation.Templates.campSpan

                Expect.equal
                    (at (due - 1) Truth.Yes (ended OpEnd.Done))
                    (Some(ended OpEnd.Done), [])
                    "before it is due: ended"

                Expect.equal
                    (at due Truth.No (ended OpEnd.Abandoned))
                    (Some(ended OpEnd.Abandoned), [])
                    "due, its trigger no: waits on"

                match at due Truth.Yes (ended OpEnd.Abandoned) with
                | Some record, [ OpEvent.Began("c1", OpPhase.Muster, None) ] ->
                    Expect.equal
                        (record.Started, record.Spent, record.Lost, record.Ended, record.NextDue)
                        (due, 0, 0, None, due + Operation.Templates.campSpan)
                        "due, its trigger yes: a new run, its spend afresh"
                | other -> failtest $"a new run: %A{other}"

                Expect.equal
                    (at due Truth.Yes (ended OpEnd.Budget) |> fst |> Option.bind _.Ended)
                    None
                    "a run's budget spent re-arms as well"

                Expect.equal
                    (Operations.advance
                        (campAt due Truth.Yes Truth.Yes (fun _ -> Truth.No) Truth.No Truth.No)
                        { camp with
                            Limits = { camp.Limits with Deadline = due }
                        }
                        (Some(ended OpEnd.Expired))
                     |> fst
                     |> Option.bind _.Ended)
                    (Some OpEnd.Expired)
                    "expired: for good"

                Expect.equal
                    (Operations.advance
                        (campAt due Truth.Yes Truth.Yes (fun _ -> Truth.No) Truth.Yes Truth.Yes)
                        camp
                        (Some(campRecord OpPhase.Execute 100))
                     |> snd)
                    [ OpEvent.Ended("c1", OpEnd.Done) ]
                    "a run past its window ends done"
            }

            test
                "code's camp is anchored on its home: an ally's home refuses it whole, an unknown one casts nothing" {
                let wire: OpSpecWire =
                    {
                        Id = "c1"
                        Template = "exitCamp"
                        Enemy = "Trepidimous"
                        Rooms = [ "W6N6"; "W9N9" ]
                        Deadline = None
                        MaxEnergy = None
                        MaxBodies = None
                    }

                Expect.equal
                    (Operation.ofCode standing wire |> Result.map ignore)
                    (Error { Id = "c1"; Why = "an ally's: W6N6" })
                    "an ally's home: refused, never read as a camp on W9N9"

                Expect.equal
                    (Operations.declared
                        standing
                        [
                            { wire with
                                Rooms = [ "W8N8"; "W5N5"; "W9N9" ]
                            }
                        ])
                    ([ { wire with Rooms = [ "W8N8"; "W9N9" ] } ],
                     [ { Id = "c1"; Why = "ours: W5N5" } ])
                    "a room of ours dropped, its home kept first"

                Expect.equal
                    (Operations.declared standing [ { wire with Rooms = [ "W4N4"; "W9N9" ] } ])
                    ([],
                     [
                         {
                             Id = "c1"
                             Why = "holder unknown: W4N4"
                         }
                     ])
                    "its home's holder unknown: nothing cast"
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
                    | "W7N1" -> Some(RoomHolder.Player "Kalgen")
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

            test
                "out of sight a room keeps the holder its last look found: a rival's lapsed remote castable, an ally's refused" {
                // The look: Trepidimous's remote unreserved, Odiodin's reserved.
                let seen =
                    { World.empty with
                        Rooms =
                            Map.ofList
                                [
                                    "W1N1", looked Ownership.Unowned None
                                    "W2N1", looked Ownership.Unowned (Some "Odiodin")
                                ]
                    }

                let looks =
                    [ "W1N1"; "W2N1" ]
                    |> List.choose (fun room ->
                        Operations.holderSeen seen room |> Option.map (fun h -> room, h))
                    |> Map.ofList

                // The dossier: Trepidimous's reservations, one seen gone and
                // one standing at the last look.
                let rivals: Observe.Rivals =
                    Observe.Rivals.ofDossiers (
                        Map.ofList
                            [
                                "Trepidimous",
                                { Observe.Dossier.opened 900 with
                                    Rooms =
                                        Map.ofList
                                            [
                                                "W1N1",
                                                { Observe.RivalRoom.empty with
                                                    LapsedAt = 950
                                                }
                                                "W3N1",
                                                { Observe.RivalRoom.empty with
                                                    LapsedAt = 950
                                                }
                                                "W4N1",
                                                { Observe.RivalRoom.empty with
                                                    Seen = 960
                                                    Reserving = true
                                                }
                                            ]
                                }
                            ]
                    )

                let sweep =
                    { wire with
                        Template = "sweep"
                        Rooms = [ "W1N1"; "W2N1"; "W3N1"; "W4N1"; "W5N1" ]
                    }

                let castable (remembered: string -> RoomHolder option) =
                    let standing = Operations.standingOf 1_000 Set.empty remembered World.empty
                    let admitted, refused = Operations.declared standing [ sweep ]
                    admitted |> List.collect _.Rooms, refused |> List.map _.Why

                Expect.equal
                    (castable (Observe.Rivals.lastHolder looks (Some rivals)))
                    ([ "W1N1"; "W3N1"; "W4N1" ], [ "an ally's: W2N1"; "holder unknown: W5N1" ])
                    "the looks this heap, else the dossier's lapse and reservation; never seen is unknown"

                Expect.equal
                    (castable (Observe.Rivals.lastHolder Map.empty (Some rivals)))
                    ([ "W1N1"; "W3N1"; "W4N1" ], [ "holder unknown: W2N1"; "holder unknown: W5N1" ])
                    "a cold heap: the ally's room is in no dossier, so unknown and still refused"
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

/// An exit camp on W8N8 (Trepidimous's) landing in two rooms beside it, as
/// the operator publishes one, its deadline pinned.
let private beside = RoomName.adjacent "W8N8"

let private campWire: OpSpecWire =
    {
        Id = "c1"
        Template = "exitCamp"
        Enemy = "Trepidimous"
        Rooms = "W8N8" :: List.take 2 beside
        Deadline = Some 6_000
        MaxEnergy = None
        MaxBodies = None
    }

/// The specs published over `standing` with no code spec and no record.
let private publishing (live: Set<string>) (wires: OpSpecWire list) =
    Operation.publish standing false [] [] [] live wires

/// Each refusal of a lone spec published live.
let private refusedLive (wire: OpSpecWire) : string list =
    (publishing (Set.singleton wire.Id) [ wire ]).Refused |> List.map _.Why

[<Tests>]
let operationPublishTests =
    testList
        "a published operation: dry until flagged live, never sharing a room (#663 S7e)"
        [
            test
                "unflagged it runs dry; flagged live it comes back a wire with the limits its decode priced" {
                let dry = publishing Set.empty [ campWire ]

                Expect.equal (dry.Dry.Admitted |> List.map _.Id) [ "c1" ] "admitted to run dry"
                Expect.equal dry.Live [] "nothing to lay"

                let live = publishing (Set.singleton "c1") [ campWire ]

                Expect.equal live.Dry.Admitted [] "not run dry as well"

                Expect.equal
                    live.Live
                    [
                        { campWire with
                            MaxBodies = Some 2
                            MaxEnergy = Some(Operation.ceiling 2 Operation.Templates.campSpan)
                        }
                    ]
                    "one body an exit, a run's budget the template's ceiling, the deadline as given"

                Expect.equal live.Refused [] "nothing refused"
            }

            test "the decoder's refusals, by the decoder's reason" {
                let cases =
                    [
                        { campWire with Template = "assault" }, "no template: assault"
                        { campWire with Enemy = "Odiodin" }, "an ally: Odiodin"
                        { campWire with Enemy = "Kalgen" }, "a neutral: Kalgen"
                        { campWire with
                            Rooms = [ "W8N8"; "W6N6" ]
                        },
                        "an ally's: W6N6"
                        { campWire with
                            Rooms = [ "W8N8"; "W7N7" ]
                        },
                        "a neutral's: W7N7"
                        { campWire with
                            Rooms = [ "W5N5"; "W5N6" ]
                        },
                        "ours: W5N5"
                        { campWire with
                            Rooms = [ "W8N8"; "W1N1" ]
                        },
                        "not beside W8N8: W1N1"
                        { campWire with Rooms = [ "W8N8" ] }, "no room beside W8N8"
                        { campWire with MaxEnergy = Some 0 }, "no budget"
                        { campWire with MaxEnergy = Some -5 }, "no budget"
                        { campWire with MaxBodies = Some 0 }, "no bodies"
                        { campWire with Deadline = Some 900 }, "deadline past"
                        { campWire with
                            Template = "sweep"
                            Rooms = [ "W9N9" ]
                        },
                        "no live sweep"
                    ]

                for wire, why in cases do
                    Expect.equal (refusedLive wire) [ why ] why
            }

            test
                "a spec sharing a code spec's id or room, or an earlier one's room, holds its record" {
                let code =
                    [
                        { campWire with
                            Id = "camp"
                            Rooms = [ "W8N8"; List.head beside ]
                        }
                    ]

                let record (id: string) =
                    { recordAt OpPhase.Execute 900 with
                        Id = id
                    }

                let published =
                    Operation.publish
                        standing
                        false
                        code
                        []
                        [ record "camp"; record "c2" ]
                        (Set.ofList [ "camp"; "c2"; "c3"; "c4" ])
                        [
                            { campWire with Id = "camp" }
                            { campWire with
                                Id = "c2"
                                Rooms = [ "W9N9"; "W9N8" ]
                            }
                            { campWire with
                                Id = "c3"
                                Template = "sweep"
                                Rooms = [ "W8N8" ]
                            }
                            { campWire with
                                Id = "c4"
                                Rooms = [ "W9N9"; "W10N9" ]
                            }
                        ]

                Expect.equal
                    published.Refused
                    [
                        {
                            Id = "camp"
                            Why = "a code op's id: camp"
                        }
                        {
                            Id = "c3"
                            Why = "code op camp holds W8N8"
                        }
                        { Id = "c4"; Why = "op c2 holds W9N9" }
                    ]
                    "one fight a room: code's first, then Memory's in order"

                Expect.equal
                    (published.Live |> List.map _.Id)
                    [ "c2" ]
                    "the one that shares nothing"

                Expect.isTrue
                    (Set.isSubset (Set.ofList [ "camp"; "c3"; "c4" ]) published.LiveKept)
                    "a clash holds: each record kept, unmoved"
            }

            test "the code's camp and a published one on the same rival home never both run" {
                let wire =
                    { campWire with
                        Id = "mine"
                        Rooms = [ "W18S26"; "W18S25" ]
                    }

                let published =
                    Operation.publish
                        standing
                        false
                        Colony.operations
                        []
                        []
                        (Set.singleton "mine")
                        [ wire ]

                Expect.equal published.Live [] "not laid"

                Expect.equal
                    (published.Refused |> List.map _.Why)
                    [ "code op camp holds W18S26" ]
                    "its home the code camp's"
            }

            // Ship-gate review: the live flag is read by id, so a row with
            // none ran live on a later row of its id flagged live.
            test "an id on two rows refuses both: neither runs, dry or live, on the other's flag" {
                let unflagged = campWire

                let flagged =
                    { campWire with
                        Rooms = [ "W9N9"; "W9N8" ]
                    }

                let published = publishing (Set.singleton "c1") [ unflagged; flagged ]

                Expect.equal
                    (published.Live, published.Dry.Admitted)
                    ([], [])
                    "nothing laid, nothing run dry"

                Expect.equal
                    published.Refused
                    [ { Id = "c1"; Why = "duplicate id" }; { Id = "c1"; Why = "duplicate id" } ]
                    "every row refused"

                Expect.equal published.Watched [] "nor watched"
                Expect.isTrue (published.LiveKept.Contains "c1") "its record held"
            }

            test "the world watches the live specs laid or held, never one refused outright" {
                let published =
                    publishing
                        (Set.ofList [ "c1"; "c2"; "c3"; "c4" ])
                        [
                            campWire
                            { campWire with
                                Id = "c2"
                                Rooms = [ "W8N8"; List.item 2 beside ]
                            }
                            { campWire with
                                Id = "c3"
                                Template = "sweep"
                                Rooms = [ "W9N9" ]
                            }
                            { campWire with
                                Id = "c4"
                                Rooms = [ "W9N9"; "W6N6" ]
                            }
                        ]

                Expect.equal
                    (published.Watched |> List.map _.Id)
                    [ "c1"; "c2" ]
                    "c1 laid, c2 held by its clash on W8N8; the live sweep and the ally's room unwatched"
            }

            test "the kill switch admits nothing and keeps every live record" {
                let published =
                    Operation.publish standing true [] [] [] (Set.singleton "c1") [ campWire ]

                Expect.equal
                    (published.Dry.Admitted, published.Live, published.Refused)
                    ([], [], [])
                    "nothing"

                Expect.equal published.LiveKept (Set.singleton "c1") "its record kept"
            }

            test
                "the code's latch keeps code's records and live ones standing; a live one gone is dropped" {
                let record (id: string) =
                    { recordAt OpPhase.Execute 900 with
                        Id = id
                    }

                let published =
                    Operation.publish
                        standing
                        false
                        [
                            { campWire with
                                Id = "camp"
                                Rooms = [ "W1N1"; "W1N2" ]
                            }
                        ]
                        []
                        []
                        (Set.singleton "c1")
                        [ campWire ]

                Expect.equal
                    (Operations.retire
                        [ { campWire with Id = "camp" } ]
                        published
                        [ record "camp"; record "c1"; record "gone" ])
                    ([ record "camp"; record "c1" ], [ OpEvent.Dropped "gone" ])
                    "code's kept, the live one kept, the one whose spec left dropped"
            }
        ]

[<Tests>]
let sweepSpecTests =
    testList
        "the sweep's code spec (#663 S7b, S7c)"
        [
            test
                "the code declares the sweep as one operation, its candidates its rooms against its one enemy" {
                let candidates = Colony.candidatesOf Colony.operations

                // #663 S7d: an exit camp names its home first, which is no
                // candidate; the rooms its exits land in are, once each.
                Expect.equal
                    (candidates |> List.map _.RoomName)
                    (Colony.operations
                     |> List.collect (fun wire ->
                         match wire.Template, wire.Rooms with
                         | "exitCamp", _ :: camps -> camps
                         | _, rooms -> rooms)
                     |> List.distinct)
                    "a candidate a room, an exit camp's home none"

                Expect.equal
                    (candidates
                     |> List.choose (fun h ->
                         Colony.harassDeclared
                         |> List.tryFind (fun d -> d.RoomName = h.RoomName)
                         |> Option.map (fun d -> d.Via = h.Via)))
                    (candidates |> List.map (fun _ -> true))
                    "each with its declared detour"

                for candidates in [ candidates; Colony.harassDeclared ] do
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

            test
                "the shell casts only what the code spec's safety admits: a room that is ours is dropped, refused by name" {
                let admitted, refused =
                    Operations.declared
                        standing
                        [
                            { wire with
                                Id = "sweep"
                                Rooms = [ "W5N5"; "W8N8" ]
                            }
                        ]

                Expect.equal (admitted |> List.collect _.Rooms) [ "W8N8" ] "ours dropped"
                Expect.equal refused [ { Id = "sweep"; Why = "ours: W5N5" } ] "and refused"

                Expect.equal
                    (Operations.declared standing [ { wire with Enemy = "Odiodin" } ])
                    ([], [ { Id = "op1"; Why = "an ally: Odiodin" } ])
                    "against an ally: nothing cast"
            }
        ]
