/// The Reservation, and a stood-down outpost's place in the pool
/// (ADR 0043).
module Fabot.Core.Tests.Decide.OutpostReserveTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.OutpostFixtures

[<Tests>]
let reserveTests =
    testList
        "reserve"
        [
            test
                "an outpost's controller is a Reserve; the colony's own is Upgraded, never reserved" {
                // The pool rule (ADR 0042), read off the projection's kind
                // census: every controller in it but ours. The colony's own
                // is excluded by id — the engine refuses reserveController
                // on a room it owns — so the two controllers here answer
                // the two different Tasks a controller can carry.
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

                let tasks = planTasks colony noThreats

                Expect.equal
                    (reserveTasks tasks)
                    [ "ctrl-out" ]
                    "the outpost's controller is the one Reserve in the pool"

                Expect.contains tasks (Upgrade "ctrl-1") "and the colony's own is still Upgraded"

                Expect.isEmpty
                    (reserveTasks (planTasks bareRespawn noThreats))
                    "a colony projecting one room reserves nothing: the pool is the pool it always was"
            }

            test "a CLAIM body is matched to the outpost's Reserve and reserves it" {
                // The whole path in one tick (ADR 0042): the Task is pooled
                // off the declaration, the CLAIM body is the one body it
                // applies to, the Matcher hands it over, and the Emitter
                // issues the reserve. The creep stands at (10,44), one tile
                // from the controller at (11,44) — inside the Work Area
                // already, so the act is this tick's and not a walk's.
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
                // Pairwise against the test above: the same colony, the
                // same tile beside the same controller, one body swapped.
                // A generalist can do everything else this colony ever asks
                // and cannot push a reservation up by a tick.
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
                // ADR 0042's pairing rule, in as many words: every other
                // Task gates on a Work part or a Carry part and a
                // `[2Claim;2Move]` body has neither, so a reserver cast
                // before this Task existed would have stood where it was
                // born for its whole 600-tick life. It is also why the
                // quota may not arrive before the Task (#131).
                let colony =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        ConstructionSites = [ { Id = "site-1" } ]
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

                let pool = planTasks colony noThreats

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
                // The one comparison the tier choice actually settles
                // today. Reserve is on the feeding tier — ADR 0042's own
                // argument for casting the row first is that it decides
                // whether the income is five a tick or ten — and Safety
                // sits above every tier of work (ADR 0033), so a reserver
                // being shot at leaves the controller. Both Tasks are in
                // this creep's pool: the Reach takes two of the
                // controller's three standing tiles and leaves one, so
                // Reserve is applicable and loses on rank rather than
                // vanishing.
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
                // ADR 0042 casts one reserver *per posted outpost* — "two
                // reservers at 4.33 energy a tick buy three sources their
                // second five" — and a second body on a controller the
                // first already holds buys nothing at all, because a
                // reservation is capped and one body's CLAIM parts are
                // sized to hold it. Travel cost cannot produce that on its
                // own: both bodies stand in the west arm, both price the
                // west controller cheapest, and `load` is only the key's
                // third component, so it never separates two candidates
                // whose costs differ. The per-Task cap is what does, and
                // without it the north outpost is pooled, applicable and
                // matched by nobody for the whole 600-tick life of both
                // creeps — silently, since both report Matched.
                let colony =
                    twoOutpostColony
                        [ reserver "r1", { X = 5; Y = 26 }; reserver "r2", { X = 6; Y = 26 } ]

                let { Assignments = assignments } = decideOn colony

                Expect.equal
                    (assignments |> Map.toList |> List.map snd |> List.sort)
                    [ taskId (Reserve "ctrl-out"); taskId (Reserve "ctrl-west") ]
                    "the two reservers hold the two declared controllers, one each"
            }

            test "a controller in a room this colony owns is not pooled at all" {
                // The other half of #181's fact, at the seam it is decided
                // on: the engine refuses reserveController on a room we
                // own, so that room's controller is not a Task. The pool
                // excluded the colony's own controller by *id*, which said
                // the same thing only while home was the only room the
                // colony owned — the tick a declared outpost is claimed it
                // stops saying it, and a Task no body can execute is one
                // the Matcher fills all the same.
                //
                // Pairwise on the one fact: the same declaration, the same
                // controller, the same projection, ownership the only
                // input that moves.
                let pooledUnder control =
                    let colony = reserveColony []

                    { colony with
                        RoomControl = colony.RoomControl |> Map.add "W1N2" control
                    }
                    |> fun colony -> planTasks colony noThreats
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
                // #181's live shape, at the seam the bug actually bites:
                // home, a near declared outpost the user has just claimed,
                // and a farther one still neutral. The row hires one body
                // — a room we own is not a room to reserve — and travel
                // cost alone would spend it on the near controller, which
                // is exactly the controller the engine refuses. Nothing
                // between the pool and the Matcher reads ownership, so the
                // pool is where that has to be settled, and this is the
                // test that says so: with the near room owned the body
                // must cross to the far one.
                //
                // Pairwise on ownership, one creep, so the cap cannot be
                // what spreads them: the same colony with the west room
                // neutral keeps the body on the west controller.
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
        ]

[<Tests>]
let standDownGateTests =
    testList
        "a stood-down outpost in the pool"
        [
            test "a stood-down outpost pools no Task, counts in no quota and is cast for by nobody" {
                // ADR 0043's whole claim, at the top seam: a room the gate
                // withholds decides exactly what a room nobody declared
                // decides. Nothing downstream was taught about stand-downs
                // — the projection, the Task pool, the four quota rows and
                // the Atlas each see a room that is not there, which is the
                // semantics ADR 0004 paid for long ago.
                //
                // The fleet stands over every row's quota but the
                // reserver's, so a `SpawnCreep` here is a reserver or it is
                // a defect, and the reserver row is the one row a
                // *declaration alone* hires for (#131): one body per
                // declared outpost, container or no container. That makes
                // it the row that can tell "the room left the projection"
                // from "the room left the economy".
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

                // Everything else besides, and this one holds by
                // construction rather than by observation: `gatedColony`
                // subtracts the shut set before it assembles anything, so
                // the two views below are the same value and the
                // equality can only fail if `Outpost.worked` filters by
                // something other than the room's name. That is worth one
                // line and is not the criterion's quota half — a row still
                // counting the shut room could not show up here, because
                // there is no room here for it to count.
                Expect.equal
                    (outcomeOf shut)
                    (outcomeOf never)
                    "a room the gate withholds is subtracted by name, so it assembles the colony a room nobody declared assembles"
            }

            test "the quota rows stop counting the room the gate withholds" {
                // Criterion 1's other half, and the one the equality above
                // cannot reach: it is read on two colonies that really do
                // differ — both declare both rooms, and only the shut set
                // moves — so a row still folding the withheld room's
                // furniture hires a body the colony it is actually working
                // does not want.
                //
                // One row at a time, each against a fleet standing exactly
                // at the shut colony's own quota for it while every other
                // row is over its own, which is the pairwise reading the
                // matcher's cheapest-rival rule asks for everywhere else.
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

                // The Anchor row counts Posts and the withheld room's
                // standing container was one: at three Anchors the colony
                // working both rooms is a body short and the one working
                // the west room alone is already at its target.
                Expect.equal
                    (gated 3 3 40)
                    ([ "reserver"; "reserver"; "anchor" ], [ "reserver" ])
                    "the fourth Post goes with the room, and the Anchor it would have hired goes with it"

                // The workforce target counts each posted source's output
                // and the withheld room's rock was one: at three workers
                // the colony working both rooms hires a fourth.
                Expect.equal
                    (gated 4 1 3)
                    ([ "reserver"; "reserver"; "worker" ], [ "reserver" ])
                    "the withheld rock's ten a tick leaves the income the worker row is sized off"

                // The fourth row is deliberately not pinned by a cast. At
                // ADR 0042's 1,800 capacity one hauler covers both home
                // containers' round trips together (ADR 0049), and this
                // colony's two home containers set the row at one either
                // way — the outposts move it by nothing there is a body's
                // granularity to see. What the row reads is the
                // projection's containers, and the withheld room's is gone
                // with the room, which the Task pool above already shows:
                // no Withdraw names it.
                Expect.equal
                    (gated 4 0 40)
                    ([ "reserver"; "reserver"; "hauler" ], [ "reserver"; "hauler" ])
                    "the hauler row wants its one home body on either side of the gate"
            }

            test "two outposts are two gates" {
                // ADR 0043's independent gates: W12S27 standing down does
                // not cost W13S28 its reserver. Pairwise, one room shut at
                // a time, because a gate that withheld "the outposts"
                // rather than a room would pass a test that shut only one.
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
                // Re-entry is the clock running out and nothing else (ADR
                // 0043), so the gate is read straight off the log: the two
                // colonies below differ only in the tick `Observe.standDown`
                // was asked at, one either side of the recorded expiry.
                let log =
                    Observe.RaidState.empty
                    // No world roster: one tick folded off an empty log
                    // has no `Living` baseline, so nothing can be read as a
                    // loss whatever `Game.creeps` holds (#191).
                    |> Observe.foldRaids
                        Observe.capEpisodes
                        Set.empty
                        { incomeColony with
                            Time = 100
                            InvaderCores =
                                [
                                    {
                                        RoomName = "W1N2"
                                        CollapseTick = Some 900
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
                // ADR 0043's other trigger, end to end: the fold remembers
                // the room the tick it is seen taken (`RaidState.RivalHeld`),
                // and the gate withholds it for ever after — there is no
                // expiry, because a room somebody else **owns** has not been
                // made dangerous, it has stopped being ours.
                //
                // Pairwise against the same room seen held by *us*, which
                // is the ordinary steady state of every outpost: one control
                // entry moves.
                let logWith control =
                    Observe.RaidState.empty
                    // No world roster: one tick folded off an empty log
                    // has no `Living` baseline, so nothing can be read as a
                    // loss whatever `Game.creeps` holds (#191).
                    |> Observe.foldRaids
                        Observe.capEpisodes
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
                // #165, end to end at the same seam as the two tests above:
                // a rival's *reservation* is a clocked stand-down and not the
                // latch beside it, so the Tasks, the furniture and the
                // reserver row all come back on the tick the engine's own
                // countdown reaches — with nobody having gone to look, which
                // is the whole point of a clock (ADR 0043).
                //
                // Pairwise against the room owned outright, one control entry
                // apart: the latch above is still a latch.
                let log =
                    Observe.RaidState.empty
                    // No world roster, for the reason the test above gives.
                    |> Observe.foldRaids
                        Observe.capEpisodes
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
                // #165's second half at the top seam. On the one tick in
                // every `Tuning.RivalRecheck` the gate re-admits a latched
                // room to the **scan**, the colony reads that room's control
                // entry — and a control entry alone is what the whole
                // re-admission amounts to: the room is in no layer, its rock
                // is in no pool and its controller is no Task, so every
                // reader of `RoomControl` asks it about a room the projection
                // already carries and finds this one nowhere (ADR 0004).
                // "Re-admitted to the scan set only, and not to the Task or
                // quota set" is that sentence, pinned where a reader that
                // widened it would go red.
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
                            }
                    }
                    withoutLook
                    "the same decision, memo and census signature and all"
            }

            test "a look the loop never took is still owed, and the room it frees comes back" {
                // #275, end to end. The stride between looks used to be an
                // exact-multiple test, so the gate had to be asked on one
                // precise tick or the whole 5,000 went by again: a throw before
                // the log was written, a tick the engine cut short with an
                // empty bucket, or a deploy landing mid-tick cost an outpost a
                // full stride of income, and nothing anywhere said so. The look
                // is owed from the stride onwards instead, so the first tick
                // the gate *is* evaluated on pays it.
                let latched =
                    Observe.RaidState.empty
                    // No world roster, for the reason the tests above give.
                    |> Observe.foldRaids
                        Observe.capEpisodes
                        Set.empty
                        { incomeColony with
                            Time = 100
                            RoomControl = Map.ofList [ "W1N2", rivalRoom ]
                        }

                // The tick the look fell due on is one the loop never ran, and
                // so are the 1,233 after it. Nowhere near a multiple of the
                // stride, which is exactly what the old test needed.
                let late = 100 + Tuning.defaults.RivalRecheck + 1_234

                Expect.equal
                    (Observe.standDown Tuning.defaults late latched).Rechecked
                    (Set.singleton "W1N2")
                    "the look the gate never got to take is still owed on the tick it is asked"

                // What the shell does with that answer: it reads the room's
                // controller and nothing else of it (`ColonyView.ofWorld`).
                // Here the rival has gone, which is the one thing that can
                // clear the latch.
                let freed =
                    latched
                    |> Observe.foldRaids
                        Observe.capEpisodes
                        Set.empty
                        { incomeColony with
                            Time = late
                            RoomControl = Map.ofList [ "W1N2", neutralRoom ]
                        }

                // Named apart from the `poolAt` a few tests up, which takes a
                // control and a tick against one fixed log: one name carrying
                // two signatures in one file reads as the same helper twice.
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
                // ADR 0043's re-entry rule has a mirror: nothing new
                // withdraws the creeps either. The room's Tasks stop
                // existing, and a creep holding one is released by the
                // release the Matcher has always spoken for an assignment
                // whose Task is gone — no retreat act, no new Verdict, no
                // second rule about where a creep may stand.
                // One creep and no fleet behind it: the release is the
                // subject, and a colony standing at its quotas would have
                // every home Task at capacity, so the creep would read as
                // unassigned for a reason that has nothing to do with the
                // gate.
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

            // What is *not* pinned here is the walk back, and it is not
            // pinned because it does not happen. A withheld room is not
            // projected (ADR 0043), so it places no creep, so the creep
            // standing in it has no tile: the rematch above is priced on
            // ADR 0004's escape — an unplaced creep prices every Task at 0
            // — rather than on a crossing, `Decide.resolve` builds moves
            // only over the creeps the Atlas places, and nothing aims this
            // one home. The release path is this ticket's claim and it
            // holds; the journey home is a fact about an unplaced creep
            // that ADR 0043's own gate placement makes unreachable, and it
            // is carried out of this ticket as a finding of its own rather
            // than pinned here as if it were the behaviour.
            }
        ]
