/// The Reservation, and a stood-down outpost's place in the pool.
module Fabot.Core.Tests.Decide.OutpostReserveTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.OutpostFixtures

/// `Observe.foldRaids` with the outpost chain's answers derived off the same
/// view (#383), as the shell hands the same value to both halves.
let private foldRaidsOf alive (view: ColonyView) prior =
    Observe.foldRaids Observe.capEpisodes alive view (Planner.outpostFactsOf view) prior

[<Tests>]
let reserveTests =
    testList
        "reserve"
        [
            test
                "an outpost's controller is a Reserve; the colony's own is Upgraded, never reserved" {
                // Every controller in the kind census but ours, excluded by id: the
                // engine refuses reserveController on a room it owns.
                let colony =
                    { bareRespawn with
                        Sources = []
                        Refillables = []
                        Spatial =
                            { SpatialInfo.empty with
                                TargetKinds =
                                    Map.ofList [ "ctrl-1", Controller; "ctrl-out", Controller ]
                            }
                    }

                let tasks = planTasksOn colony noThreats

                Expect.equal
                    (reserveTasks tasks)
                    [ "ctrl-out" ]
                    "the outpost's controller is the one Reserve in the pool"

                Expect.contains tasks (Upgrade "ctrl-1") "and the colony's own is still Upgraded"

                Expect.isEmpty
                    (reserveTasks (planTasksOn bareRespawn noThreats))
                    "a colony projecting one room reserves nothing: the pool is the pool it always was"
            }

            test "a CLAIM body is matched to the outpost's Reserve and reserves it" {
                // The whole path in one tick. The creep stands at (10,44), one tile
                // from the controller at (11,44), so the act is this tick's, not a walk's.
                let {
                        Assignments = assignments
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decideOn (reserveColony [ reserver "r1", { X = 10; Y = 44 } ])

                Expect.equal
                    (Map.tryFind "r1" assignments)
                    (Some(taskId (Reserve "ctrl-out")))
                    "the reserver holds the outpost's controller"

                Expect.contains
                    verdicts
                    (Verdict.Matched("r1", taskId (Reserve "ctrl-out"), MatchFactor.OnlyCandidate))
                    "and it is the only Task in the pool it fits"

                Expect.contains
                    intents
                    (ReserveController("r1", "ctrl-out"))
                    "the Intent is the engine's reserve act, aimed at the declared controller"

                Expect.contains
                    intents
                    (SayCreep("r1", "🚩"))
                    "and the bubble carries the Reserve glyph"
            }

            test "a body with no CLAIM part is never matched to Reserve" {
                // Pairwise against the test above: one body swapped. A generalist
                // cannot push a reservation up by a tick.
                let {
                        Assignments = assignments
                        Intents = intents
                    } =
                    decideOn (reserveColony [ worker "w1" 0 50, { X = 10; Y = 44 } ])

                Expect.isEmpty
                    (Map.toList assignments)
                    "the one Task in the pool asks for a part this body has none of"

                Expect.isEmpty
                    (intents
                     |> List.filter (function
                         | ReserveController _ -> true
                         | _ -> false))
                    "and nothing reserves anything"
            }

            test "a CLAIM body fits no other Task: without a Reserve it stands still" {
                // Every other Task gates on a Work or Carry part and a `[2Claim;2Move]`
                // body has neither, so a reserver cast before this Task existed would
                // have stood where it was born for its whole 600-tick life (#131).
                let colony =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        ConstructionSites = [ { Id = "site-1"; Left = siteOwes } ]
                        Refillables = [ refillable "spawn-1" 300 BuiltKind.Spawn ]
                        Creeps = [ reserver "r1" ]
                        Spatial =
                            { SpatialInfo.empty with
                                TargetKinds =
                                    Map.ofList
                                        [
                                            "cont-1", Structure BuiltKind.Container
                                            "pile-1", (Dropped Energy)
                                        ]
                                Stores = Map.ofList [ "cont-1", 500; "pile-1", 150 ]
                            }
                    }
                    |> withHits "road-1" BuiltKind.Road 100 5000

                let pool = planTasksOn colony noThreats

                Expect.equal
                    (pool |> List.map taskId |> List.sort)
                    (List.sort
                        [
                            taskId (Harvest "src-a")
                            taskId (Withdraw("cont-1", Energy))
                            taskId (Pickup("pile-1", Energy))
                            taskId (Refill("spawn-1", Energy))
                            taskId (Build "site-1")
                            taskId (Repair "road-1")
                            taskId (Upgrade "ctrl-1")
                        ])
                    "the premise: every Task but Reserve and Flee is in the pool"

                let { Assignments = assignments } = decideOn colony

                Expect.isEmpty
                    (Map.toList assignments)
                    "and the CLAIM body is applicable to none of them"
            }

            test "a reserver under fire runs: Safety outranks the tier Reserve sits on" {
                // Reserve is on the feeding tier and Safety sits above every tier of
                // work. Both Tasks are in this creep's pool: the Reach takes two of the
                // controller's three standing tiles and leaves one, so Reserve loses
                // on rank rather than vanishing.
                let colony = reserveColony [ reserver "r1", { X = 10; Y = 44 } ]

                let raided =
                    { colony with
                        Hostiles = [ hostileIn "W1N2" { X = 10; Y = 41 } [ Attack; Move ] ]
                    }

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decideOn raided

                Expect.equal
                    (Map.tryFind "r1" assignments)
                    (Some(taskId Flee))
                    "the reserver runs rather than holding the reservation"

                Expect.contains
                    verdicts
                    (Verdict.Matched("r1", taskId Flee, MatchFactor.Rank))
                    "and rank is what separated the two: Safety above Feeding"
            }

            test "one reserver per controller: the second is pushed to the outpost nobody holds" {
                // A second body on a controller the first already holds buys nothing.
                // Travel cost cannot produce that: both bodies stand in the west arm
                // and price the west controller cheapest, and `load` is only the
                // key's third component. The per-Task cap is what does, and without
                // it the north outpost is matched by nobody for 600 ticks, silently.
                let colony =
                    twoOutpostColony
                        [ reserver "r1", { X = 5; Y = 26 }; reserver "r2", { X = 6; Y = 26 } ]

                let { Assignments = assignments } = decideOn colony

                Expect.equal
                    (assignments |> Map.toList |> List.map snd |> List.sort)
                    [ taskId (Reserve "ctrl-out"); taskId (Reserve "ctrl-west") ]
                    "the two reservers hold the two declared controllers, one each"
            }

            test "a Reserve holder alive at the candidate's arrival still blocks it" {
                // The equality boundary holds for ordinary bounded Tasks; only the
                // Reactor relay has a handover window. The candidate is three ticks
                // from the ring and the incumbent has exactly three left.
                let incumbent = reserver "a-old" |> withLife 3
                let candidate = reserver "z-new"

                let { Assignments = assignments } =
                    reserveColony [ incumbent, { X = 10; Y = 44 }; candidate, { X = 10; Y = 40 } ]
                    |> decideOn

                Expect.equal
                    (assignments |> Map.toList)
                    [ "a-old", taskId (Reserve "ctrl-out") ]
                    "equality still spends the Reserve's one seat on the incumbent"
            }

            test "a controller in a room this colony owns is not pooled at all" {
                // #181: the engine refuses reserveController on a room we own. The pool
                // excluded the colony's own controller by *id*, which said the same
                // thing only while home was the only room owned; a Task no body can
                // execute is one the Matcher fills all the same.
                //
                // Pairwise: ownership the only input that moves.
                let pooledUnder control =
                    let colony = reserveColony []

                    { colony with
                        RoomControl = colony.RoomControl |> Map.add "W1N2" control
                    }
                    |> fun colony -> planTasksOn colony noThreats
                    |> reserveTasks

                Expect.equal
                    (pooledUnder neutralRoom)
                    [ "ctrl-out" ]
                    "a neutral outpost's controller is the Reserve it always was"

                Expect.isEmpty
                    (pooledUnder ownedRoom)
                    "the same controller, in a room this colony owns, offers a CLAIM body nothing"
            }

            test "the row's one reserver walks past the outpost we own to the one we do not" {
                // #181's live shape: a near declared outpost just claimed and a farther
                // one still neutral. The row hires one body and travel cost alone would
                // spend it on the near controller, the one the engine refuses. Nothing
                // between the pool and the Matcher reads ownership.
                //
                // Pairwise on ownership, one creep, so the cap cannot be what spreads them.
                let assignedUnder control =
                    let colony = twoOutpostColony [ reserver "r1", { X = 5; Y = 26 } ]

                    { colony with
                        RoomControl = colony.RoomControl |> Map.add "W2N1" control
                    }
                    |> fun colony -> decideOn colony
                    |> fun result -> Map.tryFind "r1" result.Assignments

                Expect.equal
                    (assignedUnder neutralRoom)
                    (Some(taskId (Reserve "ctrl-west")))
                    "neutral, the near controller is the cheapest walk and the body takes it"

                Expect.equal
                    (assignedUnder ownedRoom)
                    (Some(taskId (Reserve "ctrl-out")))
                    "owned, the near controller is no Task and the body crosses to the neutral one"
            }

            test "a controller somebody else reserves is no Task and hires nobody" {
                // #333, measured live at W12S27 over t411,226-t411,878: a core collapsed
                // and its stand-down ended, but the reservation it took outlives it by
                // `CONTROLLER_RESERVE_MAX`, 4,999 ticks. `reserveController` is refused
                // every tick against a held controller, and the row re-cast
                // `[claim x3, move x3]` at 1,950 energy a body against an empty Storage.
                //
                // The pool offers exactly the controllers the row hires for, or a
                // reserver bought for the *other* outpost would be handed this one.
                //
                // **One holder at a time**, each pinned against its own unreserved entry.
                let castsUnder control =
                    reserverColony [ northOutpost true ] (surplusFleet 3) [ "W1N2", control ]
                    |> decideOn
                    |> fun result -> reserverCasts result.Intents

                let pooledUnder control =
                    reserverColony [ northOutpost true ] (surplusFleet 3) [ "W1N2", control ]
                    |> fun colony -> planTasksOn colony noThreats
                    |> reserveTasks

                // The Invader's leftover hold, at the ticks the ticket was filed at.
                Expect.isEmpty
                    (castsUnder (coreReservedRoom 4_999))
                    "the Invader's reservation outliving its core hires no reserver"

                Expect.isEmpty
                    (pooledUnder (coreReservedRoom 4_999))
                    "and its controller is no Reserve, so no other room's reserver is sent to it"

                Expect.equal
                    (castsUnder neutralRoom |> List.length)
                    1
                    "the same room unreserved hires the one body it always did"

                Expect.equal
                    (pooledUnder neutralRoom)
                    [ "ctrl-W1N2" ]
                    "and its controller is the Reserve it always was"

                // A rival's hold, alone. A rival's reservation is also a clocked
                // stand-down (#165) and would withdraw the room on the *next* tick;
                // this is the same tick.
                Expect.isEmpty
                    (castsUnder (reservedRoom false 4_999))
                    "another player's reservation reads the same way — the engine refuses us alike"

                Expect.isEmpty
                    (pooledUnder (reservedRoom false 4_999))
                    "and takes its controller out of the pool with it"

                Expect.equal
                    (castsUnder (reservedRoom true 4_000) |> List.length)
                    1
                    "our own hold is the steady state of every outpost and is not touched"

                Expect.equal
                    (pooledUnder (reservedRoom true 4_000))
                    [ "ctrl-W1N2" ]
                    "the room we are already holding is the room we go on holding"
            }

            test "a room with no control entry is still the room the row exists to go and see" {
                // The direction this read may **not** be wrong in (#131's deadlock): a
                // declared outpost nothing looked into carries no `RoomControl` entry.
                // Read as "somebody might hold it" nobody would walk there and the
                // entry would never appear.
                //
                // Pinned beside the hold above because a predicate written the other
                // way round would pass every case in the test above.
                let casts =
                    reserverColony [ northOutpost true ] (surplusFleet 3) []
                    |> decideOn
                    |> fun result -> reserverCasts result.Intents

                Expect.equal
                    (List.length casts)
                    1
                    "blind, the outpost hires the one body whose walk is what buys the look"
            }

            test "a hold the colony has already read is not re-bought the tick its reserver dies" {
                // #333's actual **spend**: `RoomControl` carries this tick's vision
                // alone and W12S27 holds no body of ours but the reserver itself, so a
                // rule read off vision erases itself: the reserver arrives, its Task
                // vanishes, and on the first dark tick after it dies the row buys the
                // deficit again. Live: `reserver-411079`, then `reserver-411698`, 619
                // ticks apart, the reservation unmoved between them.
                //
                // The record closes it: the look wrote the hold's end into the raid
                // log, `Observe.standDown` hands it forward, the blind ticks read it.
                // Driven from the fold because the thing under test is that the two
                // halves agree about which tick the hold ends.
                let log =
                    Observe.RaidState.empty
                    // No world roster: one tick folded off an empty log has no `Living`
                    // baseline, so nothing reads as a loss (#191).
                    |> foldRaidsOf
                        Set.empty
                        { incomeColony with
                            Time = 100
                            RoomControl = Map.ofList [ "W1N2", coreReservedRoom 4_999 ]
                        }

                let blindAt t =
                    { reserverColony [ northOutpost true ] (surplusFleet 3) [] with
                        HeldOutposts = (Observe.standDown Tuning.defaults t log).HeldOutposts
                    }

                Expect.isEmpty
                    (reserverCasts (decideOn (blindAt 101)).Intents)
                    "the tick after the look, blind again, the row hires nobody: the record answers"

                Expect.isEmpty
                    (reserveTasks (planTasksOn (blindAt 5_098) noThreats))
                    "and the controller is out of the pool for every blind tick of the hold"

                Expect.equal
                    (reserverCasts (decideOn (blindAt 5_099)).Intents |> List.length)
                    1
                    "on the tick the engine's own countdown runs out the row hires again, unlooked at"
            }

            test "a look that finds the controller free overrules the record beside it" {
                // Vision first and the record second: the record is the *previous*
                // tick's conclusion, so a hold that ended early (somebody else's
                // `attackController`, a server rolled back) opens the room on the tick
                // the look is taken.
                //
                // Pinned pairwise against the same stale record read blind.
                let castsUnder control =
                    { reserverColony [ northOutpost true ] (surplusFleet 3) control with
                        HeldOutposts = Set.singleton "W1N2"
                    }
                    |> decideOn
                    |> fun result -> reserverCasts result.Intents

                Expect.equal
                    (castsUnder [ "W1N2", neutralRoom ] |> List.length)
                    1
                    "a tick with vision on a free controller hires, whatever the last look wrote"

                Expect.isEmpty
                    (castsUnder [])
                    "and blind, that same record is the whole of the answer"
            }
        ]

[<Tests>]
let standDownGateTests =
    testList
        "a stood-down outpost in the pool"
        [
            test "a stood-down outpost pools no Task, counts in no quota and is cast for by nobody" {
                // A room the gate withholds decides exactly what a room nobody declared
                // decides; nothing downstream was taught about stand-downs.
                //
                // The fleet stands over every row's quota but the reserver's, so a
                // `SpawnCreep` here is a reserver or a defect, and the reserver row is
                // the one row a *declaration alone* hires for (#131): it can tell "the
                // room left the projection" from "the room left the economy".
                let fleet = surplusFleet 4
                let both = gatedColony [ northGated; westGated ] Set.empty fleet
                let shut = gatedColony [ northGated; westGated ] (Set.singleton "W1N2") fleet
                let never = gatedColony [ westGated ] Set.empty fleet

                Expect.isNonEmpty
                    (tasksNaming "W1N2" both)
                    "the premise: worked, the room's furniture is in the pool"

                Expect.equal
                    (reserverCasts (decideOn both).Intents)
                    [ oneBlock; oneBlock ]
                    "and worked, it is one of two outposts each hiring its own reserver"

                Expect.isEmpty
                    (tasksNaming "W1N2" shut)
                    "shut, no Task in the pool names the room — its rock, its controller and its container are gone with it"

                Expect.equal
                    (reserverCasts (decideOn shut).Intents)
                    [ oneBlock ]
                    "and the one cast left is the other outpost's: nothing is built for a room nothing can enter"

                // This one holds by construction: `gatedColony` subtracts the shut set
                // before it assembles anything, so the equality can only fail if
                // `Outpost.worked` filters by something other than the room's name.
                Expect.equal
                    (outcomeOf shut)
                    (outcomeOf never)
                    "a room the gate withholds is subtracted by name, so it assembles the colony a room nobody declared assembles"
            }

            test "the quota rows stop counting the room the gate withholds" {
                // Read on two colonies that really do differ: both declare both rooms,
                // only the shut set moves. One row at a time, each against a fleet
                // standing exactly at the shut colony's own quota for it.
                let castRows anchors haulers workers shut =
                    let fleet =
                        [ for i in 1..anchors -> anchor $"a{i}" 0 50 ]
                        @ [ for i in 1..haulers -> hauler $"h{i}" 0 100 ]
                        @ [ for i in 1..workers -> worker $"w{i}" 0 50 ]

                    gatedColony [ northGated; westGated ] shut fleet
                    |> castNames
                    |> List.map (fun (name: string) -> name.Split('-') |> Array.head)

                let gated anchors haulers workers =
                    castRows anchors haulers workers Set.empty,
                    castRows anchors haulers workers (Set.singleton "W1N2")

                // The Anchor row counts Posts and the withheld room's container was one.
                Expect.equal
                    (gated 3 3 40)
                    ([ "reserver"; "reserver"; "anchor" ], [ "reserver" ])
                    "the fourth Post goes with the room, and the Anchor it would have hired goes with it"

                // The workforce target counts each posted source's output.
                Expect.equal
                    (gated 4 1 3)
                    ([ "reserver"; "reserver"; "worker" ], [ "reserver" ])
                    "the withheld rock's ten a tick leaves the income the worker row is sized off"

                // The fourth row is not pinned by a cast: at 1,800 capacity one hauler
                // covers both home containers together, so the outposts move the row by
                // nothing a body's granularity can see. The pool above already shows
                // the withheld container gone: no Withdraw names it.
                Expect.equal
                    (gated 4 0 40)
                    ([ "reserver"; "reserver"; "hauler" ], [ "reserver"; "hauler" ])
                    "the hauler row wants its one home body on either side of the gate"
            }

            test "two outposts are two gates" {
                // Pairwise, one room shut at a time: a gate that withheld "the
                // outposts" rather than a room would pass a test that shut only one.
                let shutting room =
                    let colony =
                        gatedColony [ northGated; westGated ] (Set.singleton room) (surplusFleet 4)

                    tasksNaming "W1N2" colony, tasksNaming "W2N2" colony

                let northShut, westWithNorthShut = shutting "W1N2"
                let northWithWestShut, westShut = shutting "W2N2"

                Expect.isEmpty northShut "the north room is withheld"

                Expect.isNonEmpty westWithNorthShut "while the west one is worked exactly as before"

                Expect.isEmpty westShut "and the other way round"
                Expect.isNonEmpty northWithWestShut "with the north one untouched"
            }

            test "the tick the clock runs out, the outpost is back in the pool" {
                // The gate is read straight off the log: the two colonies differ only
                // in the tick `Observe.standDown` was asked at.
                let log =
                    Observe.RaidState.empty
                    // No world roster: nothing can be read as a loss (#191).
                    |> foldRaidsOf
                        Set.empty
                        { incomeColony with
                            Time = 100
                            InvaderCores =
                                [
                                    {
                                        RoomName = "W1N2"
                                        CollapseTick = Some 900
                                        Level = 0
                                    }
                                ]
                        }

                let fleet = surplusFleet 4

                let atTick t =
                    gatedColony
                        [ northGated; westGated ]
                        (Observe.standDown Tuning.defaults t log).Shut
                        fleet

                Expect.isEmpty
                    (tasksNaming "W1N2" (atTick 899))
                    "one tick short of the expiry the room is still withheld"

                Expect.isNonEmpty
                    (tasksNaming "W1N2" (atTick 900))
                    "on the expiry itself its rock, its controller and its container are in the pool again"

                Expect.equal
                    (reserverCasts (decideOn (atTick 900)).Intents)
                    [ oneBlock; oneBlock ]
                    "and the row hires for it again, the tick it may be entered"
            }

            test "a room another player holds is withheld with no clock at all" {
                // The fold remembers the room the tick it is seen taken
                // (`RaidState.RivalHeld`); a room somebody else **owns** has no expiry.
                //
                // Pairwise against the same room seen held by *us*.
                let logWith control =
                    Observe.RaidState.empty
                    // No world roster: nothing can be read as a loss (#191).
                    |> foldRaidsOf
                        Set.empty
                        { incomeColony with
                            Time = 100
                            RoomControl = Map.ofList [ "W1N2", control ]
                        }

                let fleet = surplusFleet 4

                let poolAt control t =
                    gatedColony
                        [ northGated; westGated ]
                        (Observe.standDown Tuning.defaults t (logWith control)).Shut
                        fleet
                    |> tasksNaming "W1N2"

                Expect.isNonEmpty
                    (poolAt (reservedRoom true 4000) 101)
                    "held by us the room is worked, which is what every outpost's steady state looks like"

                Expect.isEmpty
                    (poolAt rivalRoom 101)
                    "owned by another player it is withheld the tick after it was seen"

                Expect.isEmpty
                    (poolAt rivalRoom 1_000_000)
                    "and a million ticks later it is still withheld: this withdrawal carries no clock"
            }

            test "a room another player reserved is back in the pool when that hold ends" {
                // #165: a rival's *reservation* is a clocked stand-down, not the latch
                // beside it, so everything comes back on the tick the engine's own
                // countdown reaches, with nobody having gone to look.
                //
                // Pairwise against the room owned outright.
                let log =
                    Observe.RaidState.empty
                    // No world roster, for the reason the test above gives.
                    |> foldRaidsOf
                        Set.empty
                        { incomeColony with
                            Time = 100
                            RoomControl = Map.ofList [ "W1N2", reservedRoom false 4000 ]
                        }

                let fleet = surplusFleet 4

                let atTick t =
                    gatedColony
                        [ northGated; westGated ]
                        (Observe.standDown Tuning.defaults t log).Shut
                        fleet

                Expect.isEmpty
                    (tasksNaming "W1N2" (atTick 101))
                    "the tick after the reservation was seen the room is withheld"

                Expect.isEmpty
                    (tasksNaming "W1N2" (atTick 4099))
                    "and stays withheld for every tick of the hold the engine is counting down"

                Expect.isNonEmpty
                    (tasksNaming "W1N2" (atTick 4100))
                    "on the tick that hold ends its rock, its controller and its container are pooled again"

                Expect.equal
                    (reserverCasts (decideOn (atTick 4100)).Intents)
                    [ oneBlock; oneBlock ]
                    "and the row hires for it again, no look having been needed"
            }

            test "the look a re-check buys decides nothing" {
                // #165's second half: on the re-check tick the colony reads the room's
                // control entry and nothing else. The room is in no layer, its rock in
                // no pool and its controller no Task: "re-admitted to the scan set
                // only", pinned where a reader that widened it would go red.
                let fleet = surplusFleet 4
                let shut = gatedColony [ northGated; westGated ] (Set.singleton "W1N2") fleet

                let looked =
                    { shut with
                        RoomControl = Map.add "W1N2" rivalRoom shut.RoomControl
                    }

                let withoutLook = decideOn shut
                let withLook = decideOn looked

                Expect.isNonEmpty
                    withoutLook.Verdicts
                    "the premise: this colony reaches a decision worth comparing"

                Expect.equal
                    { withLook with
                        Memo =
                            { withLook.Memo with
                                Walks = withoutLook.Memo.Walks
                                SeamWalks = withoutLook.Memo.SeamWalks
                                FarFields = withoutLook.Memo.FarFields
                            }
                    }
                    withoutLook
                    "the same decision, memo and census signature and all"
            }

            test "a look the loop never took is still owed, and the room it frees comes back" {
                // #275: the stride between looks used to be an exact-multiple test, so
                // a throw before the log was written, an empty-bucket tick or a deploy
                // landing mid-tick cost an outpost a full stride. The look is owed from
                // the stride onwards, so the first tick the gate *is* evaluated pays it.
                let latched =
                    Observe.RaidState.empty
                    // No world roster, for the reason the tests above give.
                    |> foldRaidsOf
                        Set.empty
                        { incomeColony with
                            Time = 100
                            RoomControl = Map.ofList [ "W1N2", rivalRoom ]
                        }

                // The tick the look fell due on is one the loop never ran, and so are
                // the 1,233 after it: nowhere near a multiple of the stride.
                let late = 100 + Tuning.defaults.RivalRecheck + 1_234

                Expect.equal
                    (Observe.standDown Tuning.defaults late latched).Rechecked
                    (Set.singleton "W1N2")
                    "the look the gate never got to take is still owed on the tick it is asked"

                // The shell reads the room's controller and nothing else of it
                // (`ColonyView.ofWorld`). Here the rival has gone.
                let freed =
                    latched
                    |> foldRaidsOf
                        Set.empty
                        { incomeColony with
                            Time = late
                            RoomControl = Map.ofList [ "W1N2", neutralRoom ]
                        }

                // Named apart from `poolAt` above, which takes a control and a tick
                // against one fixed log.
                let workedFrom log t =
                    gatedColony
                        [ northGated; westGated ]
                        (Observe.standDown Tuning.defaults t log).Shut
                        (surplusFleet 4)
                    |> tasksNaming "W1N2"

                Expect.isEmpty
                    (workedFrom latched late)
                    "on the tick of the look itself the room is still withheld from the work"

                Expect.isNonEmpty
                    (workedFrom freed (late + 1))
                    "and the tick after a look that found nobody there, its rock is pooled again"
            }

            test "the creep standing in a stood-down outpost is released, on the existing path" {
                // Nothing new withdraws the creeps: the room's Tasks stop existing and
                // the Matcher's release for a gone Task does the rest.
                // One creep and no fleet behind it: a colony at its quotas would have
                // every home Task at capacity, and the creep would read as unassigned
                // for a reason that has nothing to do with the gate.
                let colonyWith shut =
                    gatedColony [ northGated; westGated ] shut [ worker "w-out" 0 50 ]
                    |> standingIn "W1N2" ("w-out", { X = 39; Y = 41 })

                let held = taskId (Harvest "src-W1N2")
                let assignments = Map.ofList [ "w-out", held ]

                let verdictsWith shut =
                    (decideFrom assignments (colonyWith shut)).Verdicts

                Expect.contains
                    (verdictsWith Set.empty)
                    (Verdict.Kept("w-out", held))
                    "the premise: worked, the creep keeps the outpost Harvest it holds"

                Expect.contains
                    (verdictsWith (Set.singleton "W1N2"))
                    (Verdict.Released("w-out", held, ReleaseReason.TaskGone))
                    "shut, the Task is gone and the creep is released by the reason that has always meant that"

                let rematched =
                    verdictsWith (Set.singleton "W1N2")
                    |> List.tryPick (function
                        | Verdict.Matched("w-out", task, _) -> Some task
                        | _ -> None)

                Expect.isSome
                    rematched
                    "and it is matched again on the same tick, not left holding nothing"

                Expect.isFalse
                    ((Option.defaultValue "" rematched).Contains "W1N2")
                    "to a Task of a room the colony is still working"

            // The walk back is not pinned because it does not happen: a withheld
            // room is not projected, so the creep standing in it has no tile, the
            // rematch is priced on the unplaced creep's 0 and `Decide.resolve`
            // builds moves only over the creeps the Atlas places. The journey home
            // is carried out of this ticket as a finding of its own.
            }
        ]
