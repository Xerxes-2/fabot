/// The operation ledger's fold (`Ledger.fold`, #662): one row per operation,
/// our spend against the rival's loss, opened, resumed and closed off what
/// each full tick's decide priced.
module Fabot.Core.Tests.Decide.LedgerTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests.Decide.Fixtures

let private room = "W20S20"

let private prediction: Ledger.Prediction =
    {
        Rate = Some 4.0
        Value = Some 2.5
        Odds = None
    }

/// An operation seen this tick, its kind in `room`, beside the room's
/// reading: the bodies priced there, the rival's armed bodies standing
/// there by id at the tick their spawn began and their cost, the remote's
/// yield and whether a body works it; the room in sight.
let private seen kind values armed yieldOf worked : Ledger.Seen * Ledger.Reading =
    {
        Kind = kind
        Room = room
        Predicted = lazy prediction
        Camp = []
    },
    {
        Values = Map.ofList values
        Rival = Some "Trepidimous"
        Armed =
            armed
            |> List.map (fun (id, began, cost) ->
                id, ({ Began = began; Cost = cost }: Ledger.Armed))
            |> Map.ofList
        Yield = yieldOf
        Worked = worked
        InSight = true
    }

/// A body of ours holding the operation's Guard, standing in `at`.
let private holder name life at : Ledger.Holder =
    {
        Name = name
        Cost = 1000
        SpawnTicks = 30
        Life = life
        Room = at
    }

/// One full tick: the operations seen with their room's reading, the
/// holders of `room`'s Guard, the creeps of ours alive and the quarries
/// remembered.
let private tick
    time
    (sightings: (Ledger.Seen * Ledger.Reading) list)
    holders
    living
    quarries
    : Ledger.Tick =
    {
        Time = time
        Window = 50
        Living = Set.ofList living
        Quarries = Map.ofList quarries
        Fallen = Set.empty
        Forcing = []
        Observed =
            {
                Seen = List.map fst sightings
                Readings =
                    sightings
                    |> List.map (fun (op, reading) -> op.Room, lazy (Some reading))
                    |> Map.ofList
                Holders = Map.ofList [ room, holders ]
            }
    }

/// A remembered body, known gone as `gone`.
let private quarry id gone : string * Track =
    let body = hostileAt id { X = 25; Y = 25 } [ Work; Move ]

    id,
    {
        Body =
            { body with
                Pos = RoomPos.at room (RoomPos.pos body.Pos)
            }
        Seen = 100
        Cargo = 0
        Trail = []
        Crossings = None
        Gone = gone
    }

/// One tick's fold, which must throw nowhere.
let private foldOn (tick: Ledger.Tick) (ledger: Ledger.Ledger) =
    let folded, faults = Ledger.fold tick ledger
    Expect.isEmpty faults "no row threw"
    folded

let private folds (ticks: Ledger.Tick list) =
    ticks |> List.fold (fun ledger tick -> foldOn tick ledger) Ledger.empty

let private only (rows: Ledger.Row list) =
    Expect.equal (List.length rows) 1 "one row"
    List.head rows

[<Tests>]
let tests =
    testList
        "operation ledger (#662)"
        [
            test
                "an operation seen opens one row, keyed by its kind, room and first tick, priced then" {
                let ledger =
                    folds
                        [
                            tick 100 [ seen Ledger.OperationKind.Sweep [] [] 0.0 false ] [] [] []
                            tick 102 [ seen Ledger.OperationKind.Sweep [] [] 0.0 false ] [] [] []
                        ]

                let row = only ledger.Open

                Expect.equal
                    row.Key
                    {
                        Kind = Ledger.OperationKind.Sweep
                        Room = room
                        Start = 100
                    }
                    "the first tick it was seen"

                Expect.equal row.Predicted prediction "its price at the open"
                Expect.equal row.Last 102 "folded on the latest tick"
            }

            test
                "a row neither seen nor held closes; seen again within the window it resumes, past it a new one opens" {
                let sweep = seen Ledger.OperationKind.Sweep [] [] 0.0 false

                let closed = folds [ tick 100 [ sweep ] [] [] []; tick 102 [] [] [] [] ]
                Expect.isEmpty closed.Open "nothing holds it"
                Expect.equal (only closed.Closed).Ended (Some 102) "closed on the tick it went"

                let resumed = foldOn (tick 140 [ sweep ] [] [] []) closed
                Expect.equal (only resumed.Open).Key.Start 100 "a bounce is the same operation"
                Expect.isEmpty resumed.Closed "and leaves the closed rows"

                let fresh = foldOn (tick 160 [ sweep ] [] [] []) closed
                Expect.equal (only fresh.Open).Key.Start 160 "past the window, a new one"
            }

            test "a contested room's raid that turns to prey is one row, of the kind it opened as" {
                let ledger =
                    folds
                        [
                            tick 100 [ seen Ledger.OperationKind.Hold [] [] 0.0 false ] [] [] []
                            tick 102 [ seen Ledger.OperationKind.Trespass [] [] 0.0 false ] [] [] []
                        ]

                Expect.equal (only ledger.Open).Key.Kind Ledger.OperationKind.Hold "one fight"
            }

            test "a row unseen stays open while a body still holds its Task" {
                let ledger =
                    folds
                        [
                            tick 100 [ seen Ledger.OperationKind.Hold [] [] 0.0 false ] [] [] []
                            tick 102 [] [ holder "guard-1" 1400 room ] [ "guard-1" ] []
                        ]

                Expect.equal (only ledger.Open).Last 102 "the group has not left"
            }

            test "a body that fell on a forced tick, which folds nothing, is a kill on the next" {
                let values = [ "digger", 300.0 ]

                let ledger =
                    folds
                        [
                            tick
                                100
                                [ seen Ledger.OperationKind.Trespass values [] 0.0 false ]
                                []
                                []
                                []
                            // The forced tick read it fallen; the track is gone since.
                            { tick
                                  104
                                  [ seen Ledger.OperationKind.Trespass [] [] 0.0 false ]
                                  []
                                  []
                                  [] with
                                Fallen = Set.singleton "digger"
                            }
                        ]

                Expect.equal (only ledger.Open).Kills 1 "the digger"
            }

            test "the forced ticks since the last fold share their CPU among the rows they name" {
                let hold = seen Ledger.OperationKind.Hold [] [] 0.0 false

                let forcing cpu : Ledger.Forcing =
                    {
                        Rooms = Set.singleton room
                        Creeps = Set.empty
                        Cpu = cpu
                    }

                let row =
                    only
                        (folds
                            [
                                tick 100 [ hold ] [] [] []
                                { tick 104 [ hold ] [] [] [] with
                                    Forcing = [ forcing 5.0; forcing 7.0 ]
                                }
                            ])
                            .Open

                Expect.equal row.Forced 2 "two forced ticks"
                Expect.floatClose Accuracy.medium row.ForcedCpu 12.0 "their CPU"
            }

            test "a full open list evicts the row seen or held longest ago, and counts it" {
                let at (n: int) = $"W{n}S1"

                let seenIn (n: int) : Ledger.Seen =
                    {
                        Kind = Ledger.OperationKind.Hold
                        Room = at n
                        Predicted = lazy prediction
                        Camp = []
                    }

                // Every row stands, held by a body each; the first opened ten
                // ticks before the rest.
                let tickOf time (seen: Ledger.Seen list) (held: int list) : Ledger.Tick =
                    {
                        Time = time
                        Window = 50
                        Living = held |> List.map (fun n -> $"guard-{n}") |> Set.ofList
                        Quarries = Map.empty
                        Fallen = Set.empty
                        Forcing = []
                        Observed =
                            {
                                Seen = seen
                                Readings = Map.empty
                                Holders =
                                    held
                                    |> List.map (fun n -> at n, [ holder $"guard-{n}" 1400 (at n) ])
                                    |> Map.ofList
                            }
                    }

                let all = [ 1 .. Ledger.capOpen ]

                let full =
                    folds [ tickOf 90 [ seenIn 1 ] [ 1 ]; tickOf 100 (all |> List.map seenIn) all ]

                let next = foldOn (tickOf 120 [ seenIn 99 ] (all @ [ 99 ])) full
                Expect.equal next.Open.Length Ledger.capOpen "still at the cap"

                Expect.isTrue
                    (next.Open |> List.exists (fun row -> row.Key.Room = at 99))
                    "the new row opened"

                Expect.equal
                    (next.Closed |> List.map _.Key.Room)
                    [ at 1 ]
                    "the stalest went to the closed rows"

                Expect.equal next.Evicted 1 "and is counted"
            }

            test
                "a body cast after the open is the operation's cast; one alive before it is lent; each counted once" {
                let hold = seen Ledger.OperationKind.Hold [] [] 0.0 false
                let old = holder "guard-old" 900 room
                // Born at 1450 of a 1500 life, its 30-tick spawn began at 140.
                let young = holder "guard-new" 1450 room

                let ledger =
                    folds
                        [
                            tick 100 [ hold ] [ old ] [ "guard-old" ] []
                            tick 190 [ hold ] [ old; young ] [ "guard-old"; "guard-new" ] []
                            tick 192 [ hold ] [ old ] [ "guard-old"; "guard-new" ] []
                            tick 194 [ hold ] [ old; young ] [ "guard-old"; "guard-new" ] []
                        ]

                let row = only ledger.Open

                Expect.equal
                    (row.Cast, row.CastEnergy, row.SpawnTicks)
                    (1, 1000, 30)
                    "the young body"

                Expect.equal row.Joined 1 "the old one, lent"
            }

            test
                "a holder gone before its life ran out is lost at its cost and the life it had left; one aged out is not" {
                let hold = seen Ledger.OperationKind.Hold [] [] 0.0 false

                let ledger =
                    folds
                        [
                            tick
                                100
                                [ hold ]
                                [ holder "guard-1" 750 room; holder "guard-2" 2 room ]
                                [ "guard-1"; "guard-2" ]
                                []
                            tick 102 [ hold ] [] [] []
                        ]

                let row = only ledger.Open
                Expect.equal (row.LentLost, row.LentLostCost) (1, 1000) "the first fell, lent"
                Expect.equal row.CastLost 0 "none cast for it"

                Expect.floatClose
                    Accuracy.medium
                    row.LentLostLife
                    (1000.0 * 748.0 / 1500.0)
                    "its life left, at its cost"
            }

            test
                "a body cast for the operation that falls is a cast loss: its energy is its spend already" {
                let hold = seen Ledger.OperationKind.Hold [] [] 0.0 false
                // Its 30-tick spawn began at 140.
                let young = holder "guard-new" 1450 room

                let ledger =
                    folds
                        [
                            tick 100 [ hold ] [] [] []
                            tick 190 [ hold ] [ young ] [ "guard-new" ] []
                            tick 192 [ hold ] [] [] []
                        ]

                let row = only ledger.Open
                Expect.equal (row.Cast, row.CastEnergy) (1, 1000) "cast for it"
                Expect.equal (row.CastLost, row.LentLost) (1, 0) "and lost, cast"
                Expect.equal row.LentLostLife 0.0 "no lent life to add to the spend"
            }

            test
                "a body priced there that fell is a kill at the value it was last priced at; one aged out is none" {
                let values = [ "digger", 300.0; "hauler", 120.0 ]

                let ledger =
                    folds
                        [
                            tick
                                100
                                [ seen Ledger.OperationKind.Trespass values [] 0.0 false ]
                                []
                                []
                                []
                            tick
                                102
                                [ seen Ledger.OperationKind.Trespass [] [] 0.0 false ]
                                []
                                []
                                [
                                    quarry "digger" (Some Gone.Fallen)
                                    quarry "hauler" (Some Gone.Aged)
                                ]
                            tick
                                104
                                [ seen Ledger.OperationKind.Trespass [] [] 0.0 false ]
                                []
                                []
                                [ quarry "digger" (Some Gone.Fallen) ]
                        ]

                let row = only ledger.Open
                Expect.equal row.Kills 1 "the digger, once"
                Expect.floatClose Accuracy.medium row.KillValue 300.0 "at its value"
                Expect.isEmpty row.Marks "nothing left marked"
            }

            test
                "a rival remote stands unworked while a holder stands in it: the ticks between folds at its yield" {
                let down worked =
                    seen Ledger.OperationKind.Sweep [] [] 20.0 worked

                let inRoom = [ holder "ranger-1" 1400 room ]
                let away = [ holder "ranger-1" 1400 "W21S20" ]

                let ledger =
                    folds
                        [
                            tick 100 [ down false ] inRoom [ "ranger-1" ] []
                            tick 103 [ down false ] inRoom [ "ranger-1" ] []
                            tick 105 [ down true ] inRoom [ "ranger-1" ] []
                            tick 107 [ down false ] away [ "ranger-1" ] []
                        ]

                let row = only ledger.Open
                Expect.equal row.Downtime 3 "worked at 105, and nobody stood there at 107"
                Expect.floatClose Accuracy.medium row.DowntimeValue 60.0 "at its yield"
            }

            test
                "an armed body cast before the open is no answer; one cast after is, once, with the ticks to the first" {
                let armed bodies =
                    seen Ledger.OperationKind.Sweep [] bodies 0.0 false

                let ledger =
                    folds
                        [
                            tick 100 [ armed [ "escort", 50, 1300 ] ] [] [] []
                            // Cast before the open, arriving late: no answer.
                            tick 120 [ armed [ "escort", 50, 1300; "early", 90, 900 ] ] [] [] []
                            tick
                                130
                                [ armed [ "escort", 50, 1300; "defender", 110, 2000 ] ]
                                []
                                []
                                []
                            tick 140 [ armed [ "defender", 110, 2000 ] ] [] [] []
                        ]

                let row = only ledger.Open
                Expect.equal (row.Answered, row.AnsweredCost) (1, 2000) "the defender, once"
                Expect.equal row.Response (Some 30) "thirty ticks after the open"
            }

            test "past the roster's cap no holder enlists twice, and the roster stays at its cap" {
                let hold = seen Ledger.OperationKind.Hold [] [] 0.0 false
                let count = Ledger.capRoster + 4
                // Born on one tick, each a tick older at every fold.
                let guards time =
                    [ for i in 1..count -> holder $"guard-{i}" (900 - (time - 100)) room ]

                let names = guards 100 |> List.map _.Name

                let ledger =
                    folds
                        [
                            tick 100 [ hold ] (guards 100) names []
                            tick 102 [ hold ] (guards 102) names []
                            tick 104 [ hold ] (guards 104) names []
                        ]

                let row = only ledger.Open
                Expect.equal row.Joined count "each lent once"
                Expect.equal row.Roster.Count Ledger.capRoster "at its cap"
            }

            test "past the answers' cap no answer is counted twice" {
                let wave n =
                    [ for i in 1..n -> $"defender-{i}", 100 + i, 1000 ]

                let armed bodies =
                    seen Ledger.OperationKind.Sweep [] bodies 0.0 false

                let count = Ledger.capMarks + 4

                let ledger =
                    folds
                        [
                            tick 100 [ armed [] ] [] [] []
                            tick 130 [ armed (wave count) ] [] [] []
                            tick 132 [ armed (wave count) ] [] [] []
                            tick 134 [ armed (wave count) ] [] [] []
                        ]

                let row = only ledger.Open
                Expect.equal row.Answered count "each once"
                Expect.equal row.AnsweredCost (count * 1000) "at its cost once"
                Expect.equal row.Answers.Count Ledger.capMarks "the ids kept at the cap"
            }

            test "an answer killed is a kill, and leaves the answers' cost" {
                let reading values bodies =
                    seen Ledger.OperationKind.Sweep values bodies 0.0 false

                let ledger =
                    folds
                        [
                            tick 100 [ reading [] [] ] [] [] []
                            tick
                                130
                                [ reading [ "defender", 700.0 ] [ "defender", 110, 2000 ] ]
                                []
                                []
                                []
                            tick
                                132
                                [ reading [] [] ]
                                []
                                []
                                [ quarry "defender" (Some Gone.Fallen) ]
                        ]

                let row = only ledger.Open
                Expect.equal (row.Kills, row.KillValue) (1, 700.0) "killed"
                Expect.equal (row.Answered, row.AnsweredCost) (1, 0) "answered, its cost a kill's"
            }

            test "a sweep's camp is a job of its row: its bodies' kills counted apart" {
                let sweep, reading =
                    seen Ledger.OperationKind.Sweep [ "dancer", 400.0; "miner", 300.0 ] [] 0.0 false

                let camping = { sweep with Camp = [ "dancer" ] }

                let ledger =
                    folds
                        [
                            tick 100 [ camping, reading ] [] [] []
                            tick
                                102
                                [ camping, reading ]
                                []
                                []
                                [
                                    quarry "dancer" (Some Gone.Fallen)
                                    quarry "miner" (Some Gone.Fallen)
                                ]
                        ]

                let row = only ledger.Open

                Expect.equal
                    (row.Kills, row.CampKills)
                    (2, 1)
                    "both the sweep's, the dancer the camp's"

                Expect.equal row.CampValue 400.0 "at its value"
            }

            test "a room the decide priced no worth for this tick records nothing new" {
                let sweep, reading =
                    seen Ledger.OperationKind.Sweep [ "digger", 300.0 ] [] 20.0 false

                let opened =
                    folds
                        [
                            tick
                                100
                                [ sweep, reading ]
                                [ holder "ranger-1" 1400 room ]
                                [ "ranger-1" ]
                                []
                        ]

                let unpriced =
                    let at =
                        tick
                            130
                            [ sweep, reading ]
                            [ holder "ranger-1" 1400 room ]
                            [ "ranger-1" ]
                            [ quarry "digger" None ]

                    { at with
                        Observed =
                            { at.Observed with
                                Readings = Map.ofList [ room, lazy None ]
                            }
                    }

                let row = only (foldOn unpriced opened).Open
                Expect.equal (row.Answered, row.Downtime) (0, 0) "no answer, no downtime read"
                Expect.equal row.Marks.Count 1 "the marks of the tick that priced it stand"
            }

            test "a row whose fold throws stands as it was, and names itself" {
                let sweep, reading = seen Ledger.OperationKind.Sweep [] [] 0.0 false
                let opened = folds [ tick 100 [ sweep, reading ] [] [] [] ]

                let throwing =
                    let at = tick 102 [ sweep, reading ] [] [] []

                    { at with
                        Observed =
                            { at.Observed with
                                Readings = Map.ofList [ room, lazy (failwith "bad reading") ]
                            }
                    }

                let folded, faults = Ledger.fold throwing opened
                Expect.equal (only folded.Open).Last 100 "folded nothing"
                Expect.equal (faults |> List.map snd) [ "bad reading" ] "and said why"
            }

            test "a closed row keeps its roster and answers through the window, and none past it" {
                let ledger =
                    folds
                        [
                            tick
                                100
                                [
                                    seen
                                        Ledger.OperationKind.Trespass
                                        [ "digger", 300.0 ]
                                        []
                                        0.0
                                        false
                                ]
                                []
                                []
                                []
                            tick
                                120
                                [
                                    seen
                                        Ledger.OperationKind.Trespass
                                        [ "digger", 300.0 ]
                                        [ "defender", 110, 1300 ]
                                        0.0
                                        false
                                ]
                                []
                                []
                                []
                            tick 122 [] [] [] [ quarry "digger" None ]
                        ]

                let closed = only ledger.Closed
                Expect.isFalse closed.Answers.IsEmpty "still resumable"
                Expect.isTrue closed.Marks.IsEmpty "its marks read afresh on a resume"

                let later = foldOn (tick 200 [] [] [] []) ledger
                Expect.isTrue (only later.Closed).Answers.IsEmpty "past the window, stripped"
            }
        ]
