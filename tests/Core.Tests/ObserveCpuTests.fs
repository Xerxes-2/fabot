/// The CPU line (ADR 0041): the per-tick sample, where the tick's cost went,
/// and the revisit trigger the totals are read against.
module Fabot.Core.Tests.ObserveCpuTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Observe

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
        AtProjects = 0.0
        ColonyProjects = []
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
                            AtProjects = 0.0
                            ColonyProjects = []
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
                            AtProjects = 0.0
                            ColonyProjects = []
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

                // And the head is carried rather than left to be inferred: 4.0
                // - 3.0. A reader handed only `snapshot` and the rooms could
                // subtract head and tail *together* and would not know which of
                // the two to go after — and on the first live window the head
                // alone was 1.9 ms, more than any single room.
                Expect.equal
                    (state.Ticks |> List.map (fun sample -> sample.SweepHead))
                    [ 1.0 ]
                    "the head is the sweep's start less the prelude's reading"
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
                            AtProjects = 0.0
                            ColonyProjects = []
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
                            AtProjects = 0.0
                            ColonyProjects = []
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
                                    SweepHead = 0.0
                                    Projects = []
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
