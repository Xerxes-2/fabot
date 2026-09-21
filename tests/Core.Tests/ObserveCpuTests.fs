/// The CPU line: the per-tick sample, where the tick's cost went, and the
/// revisit trigger the totals are read against.
module Fabot.Core.Tests.ObserveCpuTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Observe

/// The CPU line as (tick, ms) pairs, oldest first — the shape
/// `observe.mjs cpu` reads a mean and a max off.
let private line (state: CpuState) =
    state.Ticks |> List.map (fun sample -> sample.Tick, sample.Ms)

/// Each row's phase split, oldest first — `None` for a row written by a
/// bundle that did not measure the boundaries.
let private splits (state: CpuState) =
    state.Ticks |> List.map (fun sample -> sample.Phases)

/// A tick that cost `ms` in total and whose boundaries were all read at the
/// end of it, so the rows carry exactly the costs they carried before the
/// phases arrived.
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
        ColonyFloods = []
        HeapMb = 0.0
        ExternalMb = 0.0
        MemoRows = 0
    }

/// The same reading with a bucket and a replan count of its own, which is
/// what the coarse spans keep beside the milliseconds.
let private costingAt (ms: float) bucket replans =
    { costing ms with
        Bucket = bucket
        Replans = replans
    }

/// Fold a run of ticks, one reading each, from an empty line.
let private folded readings =
    readings
    |> List.fold
        (fun state (tick, reading) -> foldCpu capCpuTicks tick reading state)
        CpuState.empty

[<Tests>]
let cpuSpanTests =
    testList
        "observe fold: the CPU line's coarse spans"
        [
            test "a span carries the worst tick of its window, not its mean" {
                // The engine's per-tick ceiling is 500 ms and a wall, so a window whose
                // mean is comfortable and whose worst tick is not is exactly the shape a
                // post-mortem is looking for.
                let state =
                    folded
                        [ for t in 1..10 -> t, costingAt (if t = 7 then 480.0 else 20.0) 10_000 0 ]

                match state.Spans with
                | [ span ] ->
                    Expect.equal span.Max 480.0 "the worst tick stands on its own"
                    Expect.equal span.Ticks 10 "ten ticks folded in"

                    Expect.equal
                        span.Sum
                        (9.0 * 20.0 + 480.0)
                        "and the sum is kept whole, so the mean is the reader's to take"
                | other -> failtest $"expected one open span, got %A{other}"
            }

            test "a span keeps the lowest bucket it saw" {
                // A span whose floor is the full 10,000 never spent more than the
                // allowance; one whose floor is near zero is the shape an outage leaves
                // behind. The bucket dips and recovers inside the window — 10,000 then
                // 2,000 then 9,000 — so the floor and the last reading differ.
                let state =
                    folded
                        [
                            1, costingAt 20.0 10_000 0
                            2, costingAt 20.0 2_000 0
                            3, costingAt 20.0 9_000 0
                        ]

                match state.Spans with
                | [ span ] -> Expect.equal span.Bucket 2_000 "the floor and not the last reading"
                | other -> failtest $"expected one open span, got %A{other}"
            }

            test "a span closes at its width and the next one opens" {
                let state = folded [ for t in 1 .. spanTicks + 3 -> t, costingAt 20.0 10_000 0 ]

                match state.Spans with
                | [ closed; opening ] ->
                    Expect.equal closed.Ticks spanTicks "the first span is full"
                    Expect.equal closed.From 1 "and it is the older one"
                    Expect.equal opening.Ticks 3 "the rest are in the one still filling"
                    Expect.equal opening.From (spanTicks + 1) "which opens on the tick after"
                | other -> failtest $"expected one closed span and one open, got %A{other}"
            }

            test "the record reaches back hours where the fine ring reaches minutes" {
                // `capCpuTicks` holds a hundred ticks, about five minutes; two outages
                // were hours old before anyone read the channel.
                Expect.isGreaterThan
                    (spanTicks * capCpuSpans)
                    (capCpuTicks * 100)
                    "the coarse record covers two orders of magnitude more ticks than the fine one"
            }

            test "a tick behind the open span opens a fresh one rather than widening it" {
                // A global reset with a stale leaf, or a hand-edited one: `Ticks` against
                // `To - From` is how a reader tells a gap from a run.
                let state = folded [ 1, costing 20.0; 2, costing 20.0; 1, costing 20.0 ]

                Expect.equal
                    (List.length state.Spans)
                    2
                    "the tick that went backwards starts its own span"
            }

            test "the spans survive the trim that shortens the fine ring" {
                // The two records are kept to their own lengths.
                let state = folded [ for t in 1..150 -> t, costingAt 20.0 10_000 0 ]

                Expect.equal (List.length state.Ticks) capCpuTicks "the fine ring is trimmed"

                Expect.equal
                    (state.Spans |> List.sumBy (fun s -> s.Ticks))
                    150
                    "and no tick is lost from the coarse one"
            }
        ]

[<Tests>]
let cpuTests =
    testList
        "observe fold: the CPU line"
        [
            test "every tick writes a row, quiet or not, oldest first" {
                // Unlike the Transition log there is no change detection: two ticks that
                // cost the same are two rows, because the distribution is the point.
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
                // The row carries its own tick, so a tick the loop threw on is visible as
                // a missing number rather than as a cheap tick that never happened.
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
                // Finer than the profiler's own 100µs sampling interval; the digits past
                // it are Memory paid for noise.
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
                // The readings arrive cumulative — one `Game.cpu.getUsed` after each
                // colony — so the first is differenced against the phase's own start and
                // each of the rest against the colony before it.
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
                            ColonyFloods = []
                            HeapMb = 0.0
                            ExternalMb = 0.0
                            MemoRows = 0
                        }

                Expect.equal
                    (state.Ticks |> List.map (fun sample -> sample.Colonies))
                    [ [ "W12S28", 12.0; "W13S28", 10.8; "W11S29", 6.9; "W15S28", 10.9 ] ]
                    "the first against `AtSnapshot`, each of the rest against the colony before it"

                // The remainder is readable rather than hidden: the phase less what the
                // colonies did is the movement arbitration and the two Memory reads, so
                // neither number is derived from the other.
                let phases = state.Ticks |> List.exactlyOne |> (fun sample -> sample.Phases)

                Expect.equal
                    (phases |> Option.map (fun p -> p.Decide))
                    (Some 41.0)
                    "the phase stays the tick's own, 41.0 ms against the colonies' 40.6"
            }

            test "the span keeps its worst heap and memo rows and its phase sums" {
                // #391. An hour-long climb a reset cures shows as heap and
                // rows rising span by span; the phase sums say which phase
                // the hour went to once the fine ring has forgotten it.
                let reading heap rows =
                    { costing 40.0 with
                        AtSnapshot = 10.0
                        AtDecide = 30.0
                        AtSave = 33.0
                        AtExecute = 40.0
                        HeapMb = heap
                        MemoRows = rows
                        ExternalMb = heap / 2.0
                    }

                let state =
                    CpuState.empty
                    |> foldCpu capCpuTicks 100 (reading 41.26 900)
                    |> foldCpu capCpuTicks 101 (reading 55.04 1400)
                    |> foldCpu capCpuTicks 102 (reading 48.0 1100)

                Expect.equal
                    (state.Ticks |> List.map (fun sample -> sample.HeapMb, sample.MemoRows))
                    [ 41.3, 900; 55.0, 1400; 48.0, 1100 ]
                    "each row carries its heap to a tenth of a megabyte and its rows"

                let span = state.Spans |> List.exactlyOne

                Expect.equal
                    (span.MaxHeapMb, span.MaxMemoRows)
                    (55.0, 1400)
                    "the span keeps the largest of each, not the last"

                Expect.equal
                    (span.MinHeapMb, span.MaxExternalMb)
                    (41.3, 27.5)
                    "and the heap's floor and the off-heap peak beside them (#393)"

                // A span an older bundle opened has no floor; the first
                // measured tick sets it rather than being min'd against zero.
                let continued =
                    { state with
                        Spans = state.Spans |> List.map (fun span -> { span with MinHeapMb = 0.0 })
                    }
                    |> foldCpu capCpuTicks 103 (reading 50.0 1000)

                Expect.equal
                    (continued.Spans |> List.map (fun span -> span.MinHeapMb))
                    [ 50.0 ]
                    "a floor of zero means unmeasured, not a heap of nothing"

                Expect.equal
                    (span.SnapshotSum, span.DecideSum, span.SaveSum, span.ExecuteSum)
                    (30.0, 60.0, 9.0, 21.0)
                    "and the phase sums over its three ticks"
            }

            test "the flood counts are differenced per colony from the tick's reset" {
                // The shell zeroes `Grid.Counters` before the first colony decides and
                // reads them after each, so the counts are differenced like the
                // milliseconds — `ColonyDecides`' fold, on three integers.
                let state =
                    CpuState.empty
                    |> foldCpu
                        capCpuTicks
                        100
                        { costing 40.0 with
                            ColonyFloods =
                                [
                                    "W12S28", { Floods = 12; Free = 9; Pops = 9_554 }
                                    "W13S28",
                                    {
                                        Floods = 15
                                        Free = 12
                                        Pops = 10_366
                                    }
                                    "W11S29",
                                    {
                                        Floods = 15
                                        Free = 12
                                        Pops = 10_366
                                    }
                                ]
                        }

                Expect.equal
                    (state.Ticks |> List.map (fun sample -> sample.Floods))
                    [
                        [
                            "W12S28", { Floods = 12; Free = 9; Pops = 9_554 }
                            "W13S28", { Floods = 3; Free = 3; Pops = 812 }
                            "W11S29", FloodCounts.zero
                        ]
                    ]
                    "each colony's own flooding, and a colony that flooded nothing reads as zero, not as the total"

                Expect.equal
                    (state.Spans |> List.map (fun span -> span.MaxPops))
                    [ 10_366 ]
                    "the span keeps the tick's total pops as its worst"
            }

            test
                "the span's worst pops is the max over its ticks, and an older span folds on at its own" {
                // The coarse record's reading, kept the way `Max` is: the largest single
                // tick, never a mean.
                let counting pops =
                    { costing 20.0 with
                        ColonyFloods = [ "W12S28", { Floods = 1; Free = 1; Pops = pops } ]
                    }

                let state =
                    CpuState.empty
                    |> foldCpu capCpuTicks 100 (counting 9_000)
                    |> foldCpu capCpuTicks 101 (counting 91_920)
                    |> foldCpu capCpuTicks 102 (counting 9_500)

                Expect.equal
                    (state.Spans |> List.map (fun span -> span.Ticks, span.MaxPops))
                    [ 3, 91_920 ]
                    "one span, three ticks, the spike's pops kept"

                let uncounted = CpuState.empty |> foldCpu capCpuTicks 100 (costing 20.0)

                Expect.equal
                    (uncounted.Spans |> List.map (fun span -> span.MaxPops))
                    [ 0 ]
                    "a tick that read no counter is a span at zero pops"
            }

            test
                "a bundle that measured no colony writes no split, which is what an older row reads as" {
                // `Phases` needs its `option` because a measured zero and an unmeasured
                // phase are different claims. This does not: the empty list is the right
                // answer both for a row written before the split existed and for a tick
                // in which no colony decided.
                let state = CpuState.empty |> foldCpu capCpuTicks 100 (costing 21.0)

                Expect.equal
                    (state.Ticks |> List.map (fun sample -> sample.Colonies))
                    [ [] ]
                    "no reading, no attribution — and the row is still in the window the trigger is read off"
            }


            test "the snapshot's rooms are differenced from the prelude" {
                // The rooms' split starts one boundary earlier: `snapshot` begins where
                // the prelude's reading was taken, because nothing runs between them. A
                // reader that differenced the first room against `AtSnapshot` would report
                // a negative millisecond.
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
                            ColonyFloods = []
                            HeapMb = 0.0
                            ExternalMb = 0.0
                            MemoRows = 0
                            RoomSnapshots = [ "W15S28", 9.0; "W15S27", 12.5; "W15S26", 18.0 ]
                        }

                Expect.equal
                    (state.Ticks |> List.map (fun sample -> sample.Rooms))
                    [ [ "W15S28", 5.0; "W15S27", 3.5; "W15S26", 5.5 ] ]
                    "each room against the room swept before it, the first against `AtRooms`"

                // They sum to **less** than the phase, on purpose: 18.0 - 3.0 is 15.0
                // while 5.0 + 3.5 + 5.5 is 14.0, and the missing 1.0 is the head the
                // sweep does before the first room. Charging it to whichever room was
                // swept first priced W11S28, an outpost with one rock, at 2.35 ms against
                // the four-spawn home room beside it at 1.23.
                Expect.equal
                    (state.Ticks |> List.collect (fun sample -> sample.Rooms) |> List.sumBy snd)
                    14.0
                    "the rooms sum to the sweep, and the sweep is less than the phase"

                // The head is carried rather than inferred: 4.0 - 3.0. A reader handed
                // only `snapshot` and the rooms could not tell head from tail, and on the
                // first live window the head alone was 1.9 ms, more than any single room.
                Expect.equal
                    (state.Ticks |> List.map (fun sample -> sample.SweepHead))
                    [ 1.0 ]
                    "the head is the sweep's start less the prelude's reading"
            }

            test "the readings are differenced into phases, the entry alone" {
                // An engine prelude already spent before `loop` runs, then the ColonyView,
                // `decide`, the Memory writes and the Executor's intents. The counter is
                // cumulative and every phase a difference — except the entry, which is the
                // prelude itself and is carried as read.
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
                            // The margin and the replan count ride the same row: a full bucket and a
                            // tick that kept every colony's plan.
                            Bucket = 9_872
                            Replans = 0
                            ColonyDecides = []
                            RoomSnapshots = []
                            AtRooms = 0.0
                            AtProjects = 0.0
                            ColonyProjects = []
                            ColonyFloods = []
                            HeapMb = 0.0
                            ExternalMb = 0.0
                            MemoRows = 0
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
                // The differences are rounded the same way the total is, so a phase never
                // arrives with the float noise of a subtraction.
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
                            // Neither of these is a duration, so neither is rounded.
                            Bucket = 4_213
                            Replans = 2
                            ColonyDecides = []
                            RoomSnapshots = []
                            AtRooms = 0.0
                            AtProjects = 0.0
                            ColonyProjects = []
                            ColonyFloods = []
                            HeapMb = 0.0
                            ExternalMb = 0.0
                            MemoRows = 0
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
                // Zero is a measurement here, unlike an absent phase group: a tick with no
                // accepted intent is what proves the engine's 0.2-per-intent charge is not
                // what the tick cost.
                let state = CpuState.empty |> foldCpu capCpuTicks 100 (costing 21.0)

                Expect.equal
                    (splits state |> List.map (Option.map (fun phases -> phases.Intents)))
                    [ Some 0 ]
                    "the count rides the row at zero rather than going missing"
            }

            test "a row written before the phases keeps its absence" {
                // What the ring holds for the first hundred ticks after the split is
                // deployed, and what a rollback puts back. The old row keeps its total and
                // its phases stay absent rather than filled with zeros, which would say
                // the ColonyView cost nothing rather than that nobody measured it.
                let unsplit =
                    {
                        Spans = []
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
                                    Floods = []
                                    HeapMb = 0.0
                                    ExternalMb = 0.0
                                    MemoRows = 0
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
