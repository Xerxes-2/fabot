/// A raid in a raised child's outpost: what the resident sees of it, who is
/// judged beside whom over the fleet, and who casts its answer.
module Fabot.Core.Tests.Decide.OutpostChildTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.OutpostFixtures


/// The view with W1N4 seen this tick (#576): the hostiles standing there,
/// these Invader cores, and these towers not ours firing there.
let private seenBeyond
    (towers: (string * Pos) list)
    (cores: InvaderCoreInfo list)
    (colony: ColonyView)
    =
    { colony with
        World =
            { colony.World with
                Rooms =
                    colony.World.Rooms
                    |> Map.add
                        "W1N4"
                        {
                            Hostiles = colony.Hostiles |> List.filter (fun h -> h.Pos.Room = "W1N4")
                            HostileFatigue = Map.empty
                            HostileCargo = Map.empty
                            RivalRamparts = Map.empty
                            InvaderCores = cores
                            Towers = towers
                            OurTowers = []
                            Fallen = []
                            Control = neutralRoom
                        }
            }
    }

/// An Invader core of this level in W1N4: a stronghold's from level 1.
let private coreBeyond level : InvaderCoreInfo =
    {
        Id = "core-1"
        RoomName = "W1N4"
        Tile = { X = 30; Y = 30 }
        CollapseTick = None
        Level = level
    }


/// The same with the resident gone and a melee guard of ours standing in
/// the outpost instead.
let private guardedBeyond hostiles =
    { watchingBeyond hostiles with
        Creeps = (watchingBeyond hostiles).Creeps |> List.filter (fun c -> c.Name <> "r-1")
    }
    |> withOutpostLayer "W1N3" (fun layer ->
        { layer with
            CreepPositions = Map.empty
        })
    |> withGuards [ guard "g-1", { X = 25; Y = 44 } ]

/// The child herself in `watchingBeyond`'s frame (the world layer's S3): her
/// home W1N3, her outpost W1N4 beside it with its controller in her
/// projection, a bank of `energy`, and her own bodies standing at home. The
/// view the room's one verdict is priced over beside the mother's.
let private childBeside (mother: ColonyView) energy (ours: (CreepInfo * Pos) list) =
    let home = SpatialInfo.layerOf mother.Spatial "W1N3"
    let outpost = SpatialInfo.layerOf mother.Spatial "W1N4"

    { mother with
        Creeps = List.map fst ours
        Bank = bank energy energy
        Borrowed = BorrowedWork.empty
        Crossed = Set.empty
        Spatial =
            { mother.Spatial with
                RoomName = Some "W1N3"
                TargetKinds = mother.Spatial.TargetKinds |> Map.add "ctl-W1N4" Controller
            }
            |> withNeighbour
                "W1N3"
                { home with
                    CreepPositions =
                        (home.CreepPositions, ours)
                        ||> List.fold (fun tiles (creep, pos) -> Map.add creep.Name pos tiles)
                }
            |> withNeighbour
                "W1N4"
                { outpost with
                    TargetPositions =
                        outpost.TargetPositions |> Map.add "ctl-W1N4" { X = 40; Y = 40 }
                }
    }

/// Synapse673 (#518): 13 RANGED_ATTACK, 7 MOVE, no heal, standing in a room.
let private synapse room =
    { hostileIn room { X = 25; Y = 20 } (List.replicate 7 Move @ List.replicate 13 RangedAttack) with
        Id = "Synapse673"
        Owner = "Trepidimous"
    }

/// A 10M7R3H ranger-row body: one loses Synapse673, two win it.
let private smallResident name =
    creepWith
        name
        0
        0
        (List.replicate 10 Move @ List.replicate 7 RangedAttack @ List.replicate 3 Heal)

/// `Facts.judge` over these colonies with the world's fleet laid into each
/// view, as `WorldView.ofWorld` lays it in the shell: every body any of them
/// places, by the room it stands in.
let private judgeOverFleet held (colonies: Facts.Judged list) =
    let fleet =
        colonies
        |> List.collect (fun colony ->
            colony.Cut.Creeps
            |> List.choose (fun creep ->
                SpatialInfo.creepPlacementOf colony.Cut.Spatial creep.Name
                |> Option.map (fun at -> creep.Name, (at.Room, (creep, RoomPos.pos at)))))
        |> List.distinctBy fst
        |> List.map snd
        |> List.groupBy fst
        |> List.map (fun (room, placed) -> room, List.map snd placed)
        |> Map.ofList

    colonies
    |> List.map (fun colony ->
        { colony with
            Cut =
                { colony.Cut with
                    World =
                        { colony.Cut.World with
                            Bodies = lazy fleet
                        }
                }
        })
    |> Facts.judge held

/// The mother's view and the child's with the world's verdicts priced over
/// both (`Facts.judge`).
let private judgedBoth (mother: ColonyView) (child: ColonyView) =
    let deciding view : Facts.Judged =
        {
            Cut = view
            Owns = Set.empty
            Decides = true
        }

    match judgeOverFleet Map.empty [ deciding mother; deciding child ] with
    | [ mother; child ] -> mother, child
    | _ -> failwith "two views in, two out"

/// The bodies each colony casts for a Task (`Caster.assign`), every view a
/// colony asking what it asks alone (`Quota.castsOf`), a
/// crossing a room apart; a colony casting none for it left out.
let private castsFor (task: Task) (views: ColonyView list) : (string * int) list =
    let crossings (home: string) (room: string) =
        RoomName.offsetOf home room
        |> Option.map (fun (dx, dy) -> max (abs dx) (abs dy))

    views
    |> List.map (fun view ->
        let atlas = Atlas.ofView view

        ({
            Home = SpatialInfo.homeName view.Spatial
            View = view
            Asks =
                Quota.castsOf
                    view
                    (threatsOf view atlas)
                    (Planner.outpostFactsOf view atlas).Guarded
                    (fun _ -> false)
        }
        : Caster.Bidder))
    |> Caster.assign crossings Map.empty Map.empty
    |> Map.toList
    |> List.choose (fun (home, casts) ->
        match
            casts
            |> List.filter (fun cast -> cast.For = task)
            |> List.sumBy (fun cast -> cast.Ask.Wanted)
        with
        | 0 -> None
        | bodies -> Some(home, bodies))

/// The child's view of `judgedBoth`.
let private judgedBeside (mother: ColonyView) (child: ColonyView) = snd (judgedBoth mother child)

/// The rooms a colony's stand-down shuts off this tick's sightings alone.
let private shutOf (colony: ColonyView) =
    Observe.RaidState.empty
    |> Observe.foldRaids
        Observe.capEpisodes
        Set.empty
        colony
        (Planner.outpostFactsOf colony (Atlas.ofView colony)).Declared
    |> Observe.standDown Tuning.defaults (colony.Time + 1)
    |> fun gate -> gate.Shut

[<Tests>]
let childOutpostTests =
    testList
        "a resident sees into its raised home's outposts (#517)"
        [
            test "a trespasser in the child's outpost beside the home calls the quiet resident" {
                let colony = watchingBeyond [ diggerAt "W1N4" { X = 25; Y = 42 } ]

                Expect.isSome
                    (pooledOf colony |> entryFor (Guard "W1N4"))
                    "the kill is pooled there"

                let called = decide colony residentHeld Set.empty None

                Expect.contains
                    called.Verdicts
                    (Verdict.Released("r-1", taskId (Guard "W1N3"), ReleaseReason.Called))
                    "the resident is let go of its quiet ring"

                Expect.equal
                    (Map.tryFind "r-1" called.Assignments)
                    (Some(taskId (Guard "W1N4")))
                    "and takes the kill"
            }

            test "the W18S25 shape: a crippled melee in the child's outpost is the resident's chase" {
                let crippled =
                    { crippledAt { X = 25; Y = 42 } with
                        Pos = { Room = "W1N4"; X = 25; Y = 42 }
                    }

                let called = decide (watchingBeyond [ crippled ]) residentHeld Set.empty None

                Expect.equal
                    (Map.tryFind "r-1" called.Assignments)
                    (Some(taskId (Guard "W1N4")))
                    "the resident takes the chase"
            }

            // #576: towers not ours strike every tile of their room, the kite
            // ground and the ring alike, and no body of ours out-heals one.
            // The cripple is no kite and no win there for any body, and the
            // resident is not walked in after it: the chase lets go at the
            // border.
            test "a cripple stepped into a room a rival's tower fires in is no chase" {
                let crippled =
                    { crunchyAt { X = 25; Y = 42 } with
                        Pos = { Room = "W1N4"; X = 25; Y = 42 }
                    }

                let colony =
                    watchingBeyond [ crippled ] |> seenBeyond [ "tower-1", { X = 10; Y = 10 } ] []

                Expect.equal (Facts.answer colony "W1N4") Answer.Lose "no kite, no win, no rangers"

                Expect.isNone (pooledOf colony |> entryFor (Guard "W1N4")) "the chase is not pooled"

                Expect.equal
                    (Map.tryFind "r-1" (decide colony residentHeld Set.empty None).Assignments)
                    (Some(taskId (Guard "W1N3")))
                    "the resident stays on its home's Guard"
            }

            test "a cripple in an Invader stronghold's room under its towers is no chase" {
                let crippled =
                    { crippledAt { X = 25; Y = 42 } with
                        Pos = { Room = "W1N4"; X = 25; Y = 42 }
                        Owner = "Invader"
                    }

                let colony =
                    watchingBeyond [ crippled ]
                    |> seenBeyond
                        [ "tower-1", { X = 10; Y = 10 }; "tower-2", { X = 12; Y = 10 } ]
                        [ coreBeyond 3 ]

                Expect.equal (Facts.answer colony "W1N4") Answer.Lose "no kite, no win, no rangers"

                Expect.isNone (pooledOf colony |> entryFor (Guard "W1N4")) "the chase is not pooled"

                Expect.equal
                    (Map.tryFind "r-1" (decide colony residentHeld Set.empty None).Assignments)
                    (Some(taskId (Guard "W1N3")))
                    "the resident stays on its home's Guard"
            }

            test "a cripple in an outpost beside a level-0 core, no tower standing, is still chased" {
                let crippled =
                    { crippledAt { X = 25; Y = 42 } with
                        Pos = { Room = "W1N4"; X = 25; Y = 42 }
                    }

                let colony = watchingBeyond [ crippled ] |> seenBeyond [] [ coreBeyond 0 ]

                Expect.equal (Facts.answer colony "W1N4") Answer.Kite "the resident kites it"

                Expect.equal
                    (Map.tryFind "r-1" (decide colony residentHeld Set.empty None).Assignments)
                    (Some(taskId (Guard "W1N4")))
                    "and takes the chase"
            }

            // #616: a fighter is the fleet's, so the mother's guard that
            // kills the trespasser hunts it as any colony's would.
            test "a melee guard of ours hunts a trespasser across the child's border" {
                let colony = guardedBeyond [ diggerAt "W1N4" { X = 25; Y = 42 } ]

                Expect.isSome
                    (pooledOf colony |> entryFor (Guard "W1N4"))
                    "the premise: the kill is pooled"

                Expect.equal
                    (Map.tryFind "g-1" (decideOn colony).Assignments)
                    (Some(taskId (Guard "W1N4")))
                    "the guard takes the kill"
            }

            test "a transit room that is no child's outpost pools no kill" {
                let colony =
                    { watchingBeyond [ diggerAt "W1N4" { X = 25; Y = 42 } ] with
                        Borrowed = (raisingBeyond []).Borrowed
                    }

                Expect.isNone
                    (pooledOf colony |> entryFor (Guard "W1N4"))
                    "a room crossed is mined by nobody of ours"
            }

            // The Brief's S7, rule 2, the live case (#518, t964,802): Synapse673,
            // 13 RANGED_ATTACK 7 MOVE and no heal, in the child's outpost beside
            // her home, where the mother (bank 5,600) keeps two 21M14R7H
            // residents; the child's bank is 850. Neither colony's guard row
            // answers it: the mother's casts for no child's outpost, the
            // child's one block loses. The one verdict counts the residents in
            // both views: the mother calls them, and the child does not stand
            // the outpost down.
            test
                "a ranged harasser in the child's outpost: the mother calls her residents and the child stays open" {
                let harasser room =
                    { hostileIn
                          room
                          { X = 25; Y = 20 }
                          (List.replicate 7 Move @ List.replicate 13 RangedAttack) with
                        Id = "Synapse673"
                        Owner = "Trepidimous"
                    }

                let resident name =
                    creepWith
                        name
                        0
                        0
                        (List.replicate 21 Move
                         @ List.replicate 14 RangedAttack
                         @ List.replicate 7 Heal)

                let shut (colony: ColonyView) =
                    Observe.RaidState.empty
                    |> Observe.foldRaids
                        Observe.capEpisodes
                        Set.empty
                        colony
                        (Planner.outpostFactsOf colony (Atlas.ofView colony)).Declared
                    |> Observe.standDown Tuning.defaults (colony.Time + 1)
                    |> fun gate -> gate.Shut

                // The mother's view: W1N3 the child's home she keeps the
                // residents in, W1N4 the child's outpost beside it.
                let mother =
                    { watchingBeyond [ harasser "W1N4" ] with
                        Bank = bank 5600 5600
                    }
                    |> fun colony ->
                        { colony with
                            Creeps =
                                (colony.Creeps |> List.filter (fun creep -> creep.Name <> "r-1"))
                                @ [ resident "r-1"; resident "r-2" ]
                        }
                    |> standingIn "W1N3" ("r-2", { X = 27; Y = 26 })

                Expect.isTrue
                    (blocksBeat guardPattern Stance.Hold mother "W1N4" (guardBlocksReach mother))
                    "the premise: the mother's guard row would win it, were the room hers"

                Expect.equal (Facts.answer mother "W1N4") Answer.Win "answered by the residents"

                let held = Map.ofList [ "r-1", taskId (Guard "W1N3"); "r-2", taskId (Guard "W1N3") ]

                let called = decide mother held Set.empty None

                Expect.isTrue
                    ([ "r-1"; "r-2" ]
                     |> List.exists (fun name ->
                         Map.tryFind name called.Assignments = Some(taskId (Guard "W1N4"))))
                    "a resident takes the harasser"

                // The child's view beside her mother's (the world layer's S3):
                // her bank 850 and no body of her own; the room's one verdict
                // priced over both.
                let child (mother: ColonyView) =
                    judgedBeside mother (childBeside mother 850 [])

                Expect.isFalse
                    (blocksBeat
                        guardPattern
                        Stance.Hold
                        (child mother)
                        "W1N4"
                        (guardBlocksReach (child mother)))
                    "the premise: the child's guard row loses it"

                Expect.equal
                    (Facts.answer (child mother) "W1N4")
                    Answer.Win
                    "the one verdict counts the mother's residents"

                Expect.isEmpty (shut (child mother)) "so the child does not stand the outpost down"
                Expect.equal (guardQuotaOf (child mother)) (Some 0) "nor casts a guard for it"

                let away =
                    { mother with
                        Creeps =
                            mother.Creeps
                            |> List.filter (fun creep -> creep.Name <> "r-1" && creep.Name <> "r-2")
                    }

                Expect.equal
                    (shut (child away))
                    (Set.singleton "W1N4")
                    "with no resident at home it is stood down"
            }

            // The Brief's S8 (#518, follow-up 1): the child priced her mother's
            // residents beside her own rangers, the mother hers alone, so a raid
            // only the two together win was the child's Win and the mother's
            // Lose, and nobody covered the outpost. The child now prices the
            // residents lent from next door as the mother does, apart from her
            // own: the mother calls, or the child shuts, never neither.
            // Flipped at the world layer's S3 (`docs/adr/0086`): the room's one
            // verdict prices both colonies' bodies, each lent by its own
            // colony's rule, and the raid is a Win. Its S4 gate below.
            test
                "a raid only the child's ranger and the mother's resident win together is one Win, fought by both and cast for by nobody" {
                let harasser room =
                    { hostileIn
                          room
                          { X = 25; Y = 20 }
                          (List.replicate 7 Move @ List.replicate 13 RangedAttack) with
                        Id = "Synapse673"
                        Owner = "Trepidimous"
                    }

                let resident name =
                    creepWith
                        name
                        0
                        0
                        (List.replicate 10 Move
                         @ List.replicate 7 RangedAttack
                         @ List.replicate 3 Heal)

                let shut (colony: ColonyView) =
                    Observe.RaidState.empty
                    |> Observe.foldRaids
                        Observe.capEpisodes
                        Set.empty
                        colony
                        (Planner.outpostFactsOf colony (Atlas.ofView colony)).Declared
                    |> Observe.standDown Tuning.defaults (colony.Time + 1)
                    |> fun gate -> gate.Shut

                // The mother's view: one resident in the child's home W1N3.
                let mother =
                    { watchingBeyond [ harasser "W1N4" ] with
                        Bank = bank 5600 5600
                    }
                    |> fun colony ->
                        { colony with
                            Creeps =
                                (colony.Creeps |> List.filter (fun creep -> creep.Name <> "r-1"))
                                @ [ resident "r-1" ]
                        }

                Expect.equal
                    (Facts.answer mother "W1N4")
                    Answer.Lose
                    "the premise: the mother's resident loses it alone"

                // The child's view: her own ranger at home W1N3, beside her
                // outpost W1N4, where the mother's resident stands too.
                let child = childBeside mother 850 [ ranger "r-own", { X = 24; Y = 26 } ]

                Expect.equal
                    (Facts.answer child "W1N4")
                    Answer.Lose
                    "the premise: the child's ranger loses it alone"

                let hers =
                    { child with
                        Creeps = child.Creeps @ [ resident "r-1" ]
                    }

                Expect.equal
                    (Facts.answer hers "W1N4")
                    Answer.Win
                    "the premise: the two together win it, were both hers"

                let judged = judgedBeside mother child

                Expect.equal
                    (Facts.answer judged "W1N4")
                    Answer.Win
                    "priced once over both colonies' bodies, the raid is a Win"

                Expect.isEmpty (shut judged) "and the child stands nothing down"

                // The world layer's S4: both colonies decide off the judged
                // views. Each colony the `Win` counts pools the room's Guard
                // and sends its own body, so neither sends half the answer;
                // the two standing win it, so no colony casts another (W6 S2:
                // the Caster's ask, where the answerer's gate stood).
                let judgedMother, judgedChild = judgedBoth mother child
                let held = Map.ofList [ "r-1", taskId (Guard "W1N3") ]

                Expect.isSome
                    (pooledOf judgedChild |> entryFor (Guard "W1N4"))
                    "the child pools guard:W1N4"

                Expect.isSome
                    (pooledOf judgedMother |> entryFor (Guard "W1N4"))
                    "the mother, whose resident it counts, pools it too"

                Expect.isEmpty
                    (castsFor (Guard "W1N4") [ judgedMother; judgedChild ])
                    "and no colony casts for it: the bodies standing win it"

                let childDecides = decide judgedChild held Set.empty None
                let motherDecides = decide judgedMother held Set.empty None

                Expect.equal
                    (Map.tryFind "r-own" childDecides.Assignments)
                    (Some(taskId (Guard "W1N4")))
                    "the child sends her own ranger"

                Expect.equal
                    (Map.tryFind "r-1" motherDecides.Assignments)
                    (Some(taskId (Guard "W1N4")))
                    "the mother sends her resident"

                Expect.isNone
                    (Map.tryFind "r-1" childDecides.Assignments)
                    "each colony assigns only its own body"
            }

            // The Brief's S8 (#518): whether a resident may be lent to a kill is
            // its lender's rule whichever colony asks. Raids in two rooms
            // beside the child's home: to the mother each breaks the home's
            // quiet for the other, so she calls nobody; the child, whose own
            // resident rooms leave her home out, read it quiet, called the
            // residents a Win and stood nothing down. Since the world layer's
            // S3 the residents are priced in the mother's view alone, by
            // HomeHold (`Facts.judge`).
            test
                "raids beside the child's home on two sides: the mother lends nobody, and the child stands her outpost down" {
                let harasser room id =
                    { hostileIn
                          room
                          { X = 25; Y = 20 }
                          (List.replicate 7 Move @ List.replicate 13 RangedAttack) with
                        Id = id
                        Owner = "Trepidimous"
                    }

                let resident name =
                    creepWith
                        name
                        0
                        0
                        (List.replicate 21 Move
                         @ List.replicate 14 RangedAttack
                         @ List.replicate 7 Heal)

                let shut (colony: ColonyView) =
                    Observe.RaidState.empty
                    |> Observe.foldRaids
                        Observe.capEpisodes
                        Set.empty
                        colony
                        (Planner.outpostFactsOf colony (Atlas.ofView colony)).Declared
                    |> Observe.standDown Tuning.defaults (colony.Time + 1)
                    |> fun gate -> gate.Shut

                // The mother's view: the child's home W1N3 between W1N4, her
                // outpost, and W2N3; her one resident the body that leaves,
                // so nothing of the fleet stays (HomeHold).
                let mother hostiles =
                    { watchingBeyond hostiles with
                        Bank = bank 5600 5600
                    }
                    |> fun colony ->
                        { colony with
                            Creeps =
                                (colony.Creeps |> List.filter (fun creep -> creep.Name <> "r-1"))
                                @ [ resident "r-1" ]
                        }

                Expect.equal
                    (Facts.answer (mother [ harasser "W1N4" "h-a" ]) "W1N4")
                    Answer.Win
                    "the premise: one raid, and the residents answer it"

                Expect.equal
                    (Facts.answer (mother [ harasser "W1N4" "h-a"; harasser "W2N3" "h-b" ]) "W1N4")
                    Answer.Lose
                    "two, and the mother lends nobody to either"

                // The child's view beside the mother's: her home W1N3 between
                // W1N4, her outpost, and W2N3; the room's one verdict priced
                // over both.
                let child hostiles =
                    let mother = mother hostiles
                    judgedBeside mother (childBeside mother 850 [])

                Expect.equal
                    (Facts.answer (child [ harasser "W1N4" "h-a" ]) "W1N4")
                    Answer.Win
                    "the premise: one raid, and the one verdict counts the residents"

                let both = child [ harasser "W1N4" "h-a"; harasser "W2N3" "h-b" ]

                Expect.equal
                    (Facts.answer both "W1N4")
                    Answer.Lose
                    "two, and the mother's rule lends nobody, whoever reads the room"

                Expect.contains (shut both) "W1N4" "and the child stands her outpost down"
            }

            // The world layer's S3: the room's own answer, its row's cast or
            // its ranger's kite, is the colony's whose rows cast for it; a
            // colony that only crosses it casts nothing there.
            test
                "a child's outpost the mother only crosses is the child's row's: the mother's bank wins nothing there" {
                let harasser =
                    { hostileIn
                          "W1N4"
                          { X = 25; Y = 20 }
                          (List.replicate 7 Move @ List.replicate 13 RangedAttack) with
                        Id = "Synapse673"
                        Owner = "Trepidimous"
                    }

                let mother =
                    { watchingBeyond [ harasser ] with
                        Bank = bank 5600 5600
                        Borrowed = (raisingBeyond []).Borrowed
                    }
                    |> fun colony ->
                        { colony with
                            Creeps = colony.Creeps |> List.filter (fun creep -> creep.Name <> "r-1")
                        }

                let child = childBeside mother 850 []

                Expect.equal
                    (Facts.answer mother "W1N4")
                    Answer.Win
                    "the premise: alone, the mother prices her own row's cast a Win"

                Expect.equal (Facts.answer child "W1N4") Answer.Lose "and the child hers a Lose"

                let judged = judgedBeside mother child

                Expect.equal
                    (Facts.answer judged "W1N4")
                    Answer.Lose
                    "the one verdict is the child's row's"

                Expect.equal
                    (Facts.verdictOf judged "W1N4").Rows
                    (Map.ofList [ "W1N3", { Ranger = false; Wins = false } ])
                    "the mother's row weighed nowhere"

                Expect.equal (shutOf judged) (Set.singleton "W1N4") "so the child stands it down"
            }

            // The world layer's S4: a neighbour's body winning a room never
            // strips it of the colony whose own Guard it is. The child's row
            // loses her outpost, the mother's resident beside it wins it
            // alone; the child still pools her outpost's Guard and asks for
            // it, or nobody of hers would guard the room her miners keep
            // working.
            test
                "a neighbour's ranger winning the child's outpost alone leaves the child pooling and guarding it" {
                let mother =
                    { watchingBeyond [ synapse "W1N4" ] with
                        Bank = bank 5600 5600
                    }
                    |> fun colony ->
                        { colony with
                            Creeps =
                                (colony.Creeps |> List.filter (fun creep -> creep.Name <> "r-1"))
                                @ [
                                    creepWith
                                        "r-1"
                                        0
                                        0
                                        (List.replicate 21 Move
                                         @ List.replicate 14 RangedAttack
                                         @ List.replicate 7 Heal)
                                ]
                        }

                let mother, child = judgedBoth mother (childBeside mother 850 [])

                Expect.equal
                    (Facts.answer child "W1N4", (Facts.verdictOf child "W1N4").Rows.["W1N3"].Wins)
                    (Answer.Win, false)
                    "the premise: the mother's resident wins it alone, the child's row loses"

                let atlas = Atlas.ofView child

                Expect.isSome
                    (pooledOf child |> entryFor (Guard "W1N4"))
                    "the child pools her outpost's Guard"

                Expect.contains
                    (Planner.outpostFactsOf child atlas).Guarded
                    "W1N4"
                    "and guards it, whoever wins it: its ask is hers (`Quota.castsOf`), the Caster's to cast"

                Expect.isSome
                    (pooledOf mother |> entryFor (Guard "W1N4"))
                    "the mother pools it for her resident"
            }

            // The user's complaint, a cripple nobody chases: the owner's row
            // wins its shut outpost with no body of hers standing, so her row
            // casts the body that does (the stand-down is an economy gate).
            test
                "a row that wins its own shut outpost with nothing standing casts for it: the Win binds the child and her chase asks her guard row (#571)" {
                let raid =
                    hostileIn "W1N4" { X = 25; Y = 20 } [ Attack; Attack; Attack; Attack; Move ]

                let shut (view: ColonyView) =
                    { view with
                        RaidedOutposts = Set.singleton "W1N4"
                    }

                let mother = shut (watchingBeyond [ raid ])
                let child = shut (childBeside mother 5600 [])

                Expect.isTrue
                    (Facts.rowAnswer child "W1N4").Wins
                    "the premise: the child's row, cast, would win it"

                let _, child =
                    match
                        judgeOverFleet
                            Map.empty
                            [
                                {
                                    Cut = mother
                                    Owns = Set.empty
                                    Decides = true
                                }
                                {
                                    Cut = child
                                    Owns = Set.ofList [ "W1N3"; "W1N4" ]
                                    Decides = true
                                }
                            ]
                    with
                    | [ mother; child ] -> mother, child
                    | _ -> failwith "two views in, two out"

                Expect.equal
                    (Facts.answer child "W1N4", (Facts.verdictOf child "W1N4").Casting)
                    (Answer.Win, Set.singleton "W1N3")
                    "the Win is the child's row's, which casts for her own room, shut or not"

                let threats = threatsOf child (Atlas.ofView child)
                let brief = Threats.briefOf threats "W1N4"

                Expect.equal
                    (brief.Asks |> List.map (fun ask -> ask.Row))
                    [ CastRow.Guard ]
                    "and her chase asks her guard row for the body that wins it"

                Expect.isTrue (Threats.wants threats (Guard "W1N4") >= 1) "at least one"
            }

            // #573's shape: the owner can neither cast a winning row nor a
            // ranger that kites the healed cripple, and the mother's resident
            // next door kites it but loses it standing. The fleet's kite is
            // the room's.
            test
                "a healed cripple in a shut outpost its owner cannot answer is the kite of any colony's ranger standing by" {
                // Fourteen ATTACK and three HEAL on one MOVE: a one-block
                // ranger's shot never out-heals it, a big one's does.
                let cripple =
                    hostileIn
                        "W1N4"
                        { X = 25; Y = 20 }
                        (List.replicate 14 Attack @ List.replicate 3 Heal @ [ Move ])

                let shut (view: ColonyView) =
                    { view with
                        RaidedOutposts = Set.singleton "W1N4"
                    }

                let mother =
                    watchingBeyond [ cripple ]
                    |> fun colony ->
                        { colony with
                            Creeps =
                                (colony.Creeps |> List.filter (fun creep -> creep.Name <> "r-1"))
                                @ [
                                    creepWith
                                        "r-1"
                                        0
                                        0
                                        (List.replicate 21 Move
                                         @ List.replicate 14 RangedAttack
                                         @ List.replicate 7 Heal)
                                ]
                        }
                    |> shut

                let child = shut (childBeside mother 300 [])

                let judgedWith (mother: ColonyView) (child: ColonyView) =
                    match
                        judgeOverFleet
                            Map.empty
                            [
                                {
                                    Cut = mother
                                    Owns = Set.empty
                                    Decides = true
                                }
                                {
                                    Cut = child
                                    Owns = Set.ofList [ "W1N3"; "W1N4" ]
                                    Decides = true
                                }
                            ]
                    with
                    | [ mother; child ] -> mother, child
                    | _ -> failwith "two views in, two out"

                let judgedMother, judged = judgedWith mother child

                Expect.equal
                    (Facts.answer judged "W1N4")
                    Answer.Kite
                    "the mother's resident kites it: a Kite, binding nobody"

                Expect.equal
                    (Map.tryFind "r-1" (decide judgedMother Map.empty Set.empty None).Assignments)
                    (Some(taskId (Guard "W1N4")))
                    "and the resident next door takes the chase"

                // A tower not ours seen firing in the room, in the one world
                // every colony reads: no body standing by out-heals it, so
                // none kites or wins the raid there (#576).
                let towered (view: ColonyView) =
                    { view with
                        World =
                            { view.World with
                                Rooms =
                                    view.World.Rooms
                                    |> Map.add
                                        "W1N4"
                                        {
                                            Hostiles = [ cripple ]
                                            HostileFatigue = Map.empty
                                            HostileCargo = Map.empty
                                            RivalRamparts = Map.empty
                                            InvaderCores = []
                                            Towers = [ "tower-1", { X = 10; Y = 10 } ]
                                            OurTowers = []
                                            Fallen = []
                                            Control = neutralRoom
                                        }
                            }
                    }

                Expect.equal
                    (Facts.answer (snd (judgedWith (towered mother) (towered child))) "W1N4")
                    Answer.Lose
                    "under a tower not ours, the resident's kite is no answer"
            }

            // A raid fled into a room no row casts for: a colony that only
            // passes a small ranger beside it is no answer, its row's paper
            // Win aside, or that ranger would go in alone and lose.
            test
                "a raid in a room no row casts for binds no colony whose ranger beside it neither wins nor kites it" {
                let child =
                    let beside =
                        childBeside
                            (watchingBeyond [])
                            5600
                            [ smallResident "c-1", { X = 25; Y = 2 } ]

                    { beside with
                        Hostiles = [ synapse "W1N4" ]
                        Spatial =
                            { beside.Spatial with
                                TargetKinds = Map.remove "ctl-W1N4" beside.Spatial.TargetKinds
                            }
                    }

                Expect.isFalse
                    (List.contains
                        "W1N4"
                        (Planner.outpostFactsOf child (Atlas.ofView child)).Declared)
                    "the premise: W1N4 is no outpost of hers"

                let verdictHeld held =
                    match
                        judgeOverFleet
                            held
                            [
                                {
                                    Cut = child
                                    Owns = Set.singleton "W1N3"
                                    Decides = true
                                }
                            ]
                    with
                    | [ judged ] -> Facts.verdictOf judged "W1N4"
                    | _ -> failwith "one view in, one out"

                let verdict = verdictHeld Map.empty

                Expect.isTrue
                    (verdict.Rows |> Map.tryFind "W1N3" |> Option.exists (fun row -> row.Wins))
                    "the premise: her row, cast, would win it"

                Expect.equal
                    (verdict.Answer, verdict.Own)
                    (Answer.Lose, Set.empty)
                    "but nothing of hers casts for the room or takes the raid: no Win binds her"

                // The binding's hysteresis: a body already holding the room's
                // Guard binds her until the Guard lets it go, so a tick its live
                // parts dip under the price does not reshuffle the pools.
                let held = verdictHeld (Map.ofList [ "c-1", taskId (Guard "W1N4") ])

                Expect.equal
                    (held.Answer, held.Own)
                    (Answer.Win, Set.singleton "W1N3")
                    "a ranger of hers already on the raid binds her row's Win"
            }

            // The world layer's S3: a raid the room's own row answers is no
            // other colony's chase.
            test
                "a raid the child's row wins is no chase of the mother's, whose residents would win it beside" {
                let harasser =
                    { hostileIn
                          "W1N4"
                          { X = 25; Y = 20 }
                          (List.replicate 7 Move @ List.replicate 13 RangedAttack) with
                        Id = "Synapse673"
                        Owner = "Trepidimous"
                    }

                let resident name =
                    creepWith
                        name
                        0
                        0
                        (List.replicate 21 Move
                         @ List.replicate 14 RangedAttack
                         @ List.replicate 7 Heal)

                // The mother crossing the child's outpost, a bank too small
                // for any row of hers to win it, and two residents beside it.
                let mother =
                    { watchingBeyond [ harasser ] with
                        Bank = bank 850 850
                        Borrowed = (raisingBeyond []).Borrowed
                    }
                    |> fun colony ->
                        { colony with
                            Creeps =
                                (colony.Creeps |> List.filter (fun creep -> creep.Name <> "r-1"))
                                @ [ resident "r-1"; resident "r-2" ]
                        }
                    |> standingIn "W1N3" ("r-2", { X = 27; Y = 26 })

                let child = childBeside mother 5600 []

                Expect.isSome
                    (pooledOf mother |> entryFor (Guard "W1N4"))
                    "the premise: alone, the mother chases it with her residents"

                Expect.equal
                    (Facts.verdictOf (judgedBeside mother child) "W1N4").Own
                    (Set.singleton "W1N3")
                    "the child's row wins it"

                Expect.isNone
                    (pooledOf (fst (judgedBoth mother child)) |> entryFor (Guard "W1N4"))
                    "under the one verdict the mother pools no chase there"
            }

            // The world layer's S3 review: a body counts toward a room's
            // verdict only where its own colony would send it — a colony that
            // sees the raid in a room it works or crosses, deciding this tick
            // or already holding the room's Guard.
            test
                "a raid only the child's ranger and a resident beside wins is Lose where the mother works no room of the raid" {
                // The mother sees the raid in the one world every colony reads, so
                // what drops her resident is that she works no room of it,
                // not that she is blind to it.
                let mother =
                    (raisingBeyond [])
                    |> fun colony ->
                        { colony with
                            Creeps =
                                (colony.Creeps |> List.filter (fun creep -> creep.Name <> "r-1"))
                                @ [ smallResident "r-1" ]
                            Hostiles = [ synapse "W1N4" ]
                        }
                    |> seenBeyond [] []

                let child =
                    { childBeside mother 850 [ ranger "r-own", { X = 24; Y = 26 } ] with
                        Hostiles = [ synapse "W1N4" ]
                    }

                Expect.isFalse
                    (Map.containsKey "W1N4" mother.Spatial.Rooms)
                    "the premise: the mother neither works nor crosses W1N4"

                Expect.equal
                    (Facts.answer
                        { child with
                            Creeps = child.Creeps @ [ smallResident "r-1" ]
                        }
                        "W1N4")
                    Answer.Win
                    "and the two together win it, were both hers"

                let judged = judgedBeside mother child

                Expect.equal
                    (Facts.answer judged "W1N4")
                    Answer.Lose
                    "the resident, whom nobody would send, is no body of the verdict"

                Expect.equal (shutOf judged) (Set.singleton "W1N4") "so the child stands it down"
            }

            test
                "a neighbour that sees no raid beside a child's home lends its ranger nothing: the mother's resident stays outmatched" {
                let mother =
                    { raisingBeyond [ synapse "W1N3" ] with
                        Bank = bank 850 850
                    }
                    |> fun colony ->
                        { colony with
                            Creeps =
                                (colony.Creeps |> List.filter (fun creep -> creep.Name <> "r-1"))
                                @ [ smallResident "r-1" ]
                        }

                let neighbour =
                    { raisingBeyond [] with
                        Creeps = [ smallResident "n-1" ]
                        Hostiles = []
                        Borrowed = BorrowedWork.empty
                        Spatial =
                            { SpatialInfo.empty with
                                RoomName = Some "W2N3"
                                Rooms =
                                    Map.ofList
                                        [
                                            "W2N3",
                                            { RoomLayer.empty with
                                                CreepPositions =
                                                    Map.ofList [ "n-1", { X = 2; Y = 26 } ]
                                            }
                                        ]
                            }
                    }

                Expect.equal
                    (Facts.answer mother "W1N3")
                    Answer.Lose
                    "the premise: the resident loses it alone"

                Expect.equal
                    (Facts.answer
                        ({ mother with
                            Creeps = mother.Creeps @ [ smallResident "r-2" ]
                         }
                         |> standingIn "W1N3" ("r-2", { X = 27; Y = 26 }))
                        "W1N3")
                    Answer.Win
                    "and two such bodies win it"

                let judged =
                    judgeOverFleet
                        Map.empty
                        [
                            {
                                Cut = mother
                                Owns = Set.empty
                                Decides = true
                            }
                            {
                                Cut = neighbour
                                Owns = Set.empty
                                Decides = true
                            }
                        ]

                Expect.equal
                    (judged |> List.map (fun view -> Facts.answer view "W1N3"))
                    [ Answer.Lose; Answer.Lose ]
                    "the neighbour, who sees no raid there, lends nobody: still outmatched"
            }

            test
                "a colony replaying its plan lends only the bodies already holding the room's Guard" {
                let mother =
                    { watchingBeyond [ synapse "W1N4" ] with
                        Bank = bank 5600 5600
                    }
                    |> fun colony ->
                        { colony with
                            Creeps =
                                (colony.Creeps |> List.filter (fun creep -> creep.Name <> "r-1"))
                                @ [ smallResident "r-1" ]
                        }

                let child = childBeside mother 850 [ ranger "r-own", { X = 24; Y = 26 } ]

                let answerHeld held =
                    judgeOverFleet
                        held
                        [
                            {
                                Cut = mother
                                Owns = Set.empty
                                Decides = false
                            }
                            {
                                Cut = child
                                Owns = Set.empty
                                Decides = true
                            }
                        ]
                    |> List.map (fun view -> Facts.answer view "W1N4")

                Expect.equal
                    (answerHeld (Map.ofList [ "r-1", taskId (Guard "W1N3") ]))
                    [ Answer.Lose; Answer.Lose ]
                    "a resident its replayed plan keeps at home is lent to nobody"

                Expect.equal
                    (answerHeld (Map.ofList [ "r-1", taskId (Guard "W1N4") ]))
                    [ Answer.Win; Answer.Win ]
                    "one its plan already sent there counts"
            }

            // The world layer's S3 review: a home is priced by its own colony
            // and the mother's resident row alone.
            // The S3 re-review: the child pools no Guard at her own home (her
            // line is her Defend's), so her row's biggest cast answers nothing
            // there; a raised child's home is the mother's resident row's.
            test
                "a raised child's home is the mother's resident row's: the child's bank wins nothing at her own home" {
                let mother =
                    { raisingBeyond [ synapse "W1N3" ] with
                        Bank = bank 850 850
                    }
                    |> fun colony ->
                        { colony with
                            Creeps =
                                (colony.Creeps |> List.filter (fun creep -> creep.Name <> "r-1"))
                                @ [ smallResident "r-1" ]
                        }

                Expect.equal
                    (Facts.answer mother "W1N3")
                    Answer.Lose
                    "the premise: the resident loses it alone"

                Expect.equal
                    (Facts.answer (childBeside mother 5600 []) "W1N3")
                    Answer.Win
                    "and, alone, the child prices her own row's cast a Win"

                for energy in [ 2300; 5600 ] do
                    let mother, child = judgedBoth mother (childBeside mother energy [])

                    Expect.equal
                        [ Facts.answer mother "W1N3"; Facts.answer child "W1N3" ]
                        [ Answer.Lose; Answer.Lose ]
                        $"under one verdict, at a child's bank of {energy}, the raid is still outmatched"
            }

            // The world layer's S4: a home's raid is its own colony's Defend
            // and its garrison's Guard, two Tasks (#552): the mother keeps her
            // garrison's Guard though the child's rangers alone are the
            // fewest that win.
            test
                "a raid at a raised child's home her rangers win leaves the mother her garrison's Guard" {
                let mother =
                    { raisingBeyond [ synapse "W1N3" ] with
                        Bank = bank 850 850
                    }
                    |> fun colony ->
                        { colony with
                            Creeps =
                                (colony.Creeps |> List.filter (fun creep -> creep.Name <> "r-1"))
                                @ [ smallResident "r-1" ]
                        }

                let mother, child =
                    judgedBoth
                        mother
                        (childBeside
                            mother
                            850
                            [
                                smallResident "a-own", { X = 24; Y = 26 }
                                smallResident "b-own", { X = 24; Y = 27 }
                            ])

                Expect.equal
                    (Facts.verdictOf child "W1N3").Bodies
                    [ "a-own", "W1N3"; "b-own", "W1N3" ]
                    "the premise: the child's two rangers are the fewest that win it"

                Expect.isSome
                    (pooledOf mother |> entryFor (Guard "W1N3"))
                    "the mother pools her garrison's Guard"
            }

            test
                "a child's home a neighbour only crosses is the child's to price: the neighbour's bank wins nothing there" {
                let neighbour =
                    { raisingBeyond [ synapse "W1N3" ] with
                        Bank = bank 5600 5600
                        Borrowed = BorrowedWork.empty
                    }
                    |> fun colony ->
                        { colony with
                            Creeps = colony.Creeps |> List.filter (fun creep -> creep.Name <> "r-1")
                        }

                let child = childBeside neighbour 850 []

                Expect.equal
                    (Facts.answer neighbour "W1N3")
                    Answer.Win
                    "the premise: alone, the neighbour prices its own row's cast a Win"

                Expect.equal (Facts.answer child "W1N3") Answer.Lose "and the child hers a Lose"

                Expect.equal
                    (Facts.answer (judgedBeside neighbour child) "W1N3")
                    Answer.Lose
                    "the one verdict is the home's own colony's"
            }

            // A home is quiet for a lend while no hostile can reach striking
            // range of it inside the reaction window (#531), not while no
            // raider stands next door: live t972,544, Crunchy047 in W18S25
            // stepping once in seventeen ticks and Wraith403 in W17S26 each
            // kept the residents from the other's kill.
            test
                "a slow raid and a disarmed one beside the home lend the residents; a fast raider two rooms off does not" {
                let harasser =
                    { hostileIn
                          "W1N4"
                          { X = 25; Y = 20 }
                          (List.replicate 7 Move @ List.replicate 13 RangedAttack) with
                        Id = "Synapse673"
                        Owner = "Trepidimous"
                    }

                let resident name =
                    creepWith
                        name
                        0
                        0
                        (List.replicate 21 Move
                         @ List.replicate 14 RangedAttack
                         @ List.replicate 7 Heal)

                // The mother's view: the child's home W1N3, the kill in W1N4
                // beyond it, W2N3 on its other side; her one resident the
                // body that leaves, so nothing of the fleet stays (HomeHold).
                let mother others =
                    { watchingBeyond (harasser :: others) with
                        Bank = bank 5600 5600
                    }
                    |> fun colony ->
                        { colony with
                            Creeps =
                                (colony.Creeps |> List.filter (fun creep -> creep.Name <> "r-1"))
                                @ [ resident "r-1" ]
                        }

                let slow =
                    { crunchyAt { X = 25; Y = 20 } with
                        Pos = RoomPos.at "W2N3" { X = 25; Y = 20 }
                    }

                let disarmed =
                    { disarmedAt "Trepidimous" "W2N3" { X = 30; Y = 20 } with
                        Id = "Virtual458"
                    }

                Expect.isTrue (HostileInfo.armed [ slow ] slow) "the premise: the slow one is armed"

                Expect.equal
                    (Facts.answer (mother [ slow; disarmed ]) "W1N4")
                    Answer.Win
                    "seventeen ticks a step and no weapon reach no home in the window: the residents are lent"

                // Two rooms west of the home, a tile a tick: 77 tiles to the
                // home's middle less its reach, inside the 100-tick window.
                let fast =
                    { hostileIn
                          "W3N3"
                          { X = 45; Y = 25 }
                          (List.replicate 10 Move @ List.replicate 10 RangedAttack) with
                        Id = "Runner"
                        Owner = "Trepidimous"
                    }

                Expect.equal
                    (Facts.answer (mother [ fast ]) "W1N4")
                    Answer.Lose
                    "a fast armed raider that reaches the home before the residents are back keeps them home"
            }

            // A raider breaks a home's quiet only if it arrives inside the
            // lend's window and what the home keeps would not beat it (#531):
            // live t972,544, Wraith403 heals its gun back in three ticks and
            // walks a tile a tick, but one loaded tower out-shoots its HEAL.
            test
                "a raider arriving inside the window breaks the quiet only where the home's towers lose to it" {
                let harasser =
                    { hostileIn
                          "W1N4"
                          { X = 25; Y = 20 }
                          (List.replicate 7 Move @ List.replicate 13 RangedAttack) with
                        Id = "Synapse673"
                        Owner = "Trepidimous"
                    }

                let resident name =
                    creepWith
                        name
                        0
                        0
                        (List.replicate 21 Move
                         @ List.replicate 14 RangedAttack
                         @ List.replicate 7 Heal)

                let mother towers others =
                    { watchingBeyond (harasser :: others) with
                        Bank = bank 5600 5600
                    }
                    |> withOurTowers "W1N3" (List.replicate towers { X = 25; Y = 25 })
                    |> fun colony ->
                        { colony with
                            Creeps =
                                (colony.Creeps |> List.filter (fun creep -> creep.Name <> "r-1"))
                                @ [ resident "r-1" ]
                        }

                let wraith = wraithAt "W2N3" { X = 25; Y = 20 }

                Expect.equal
                    (Facts.answer (mother 0 [ wraith ]) "W1N4")
                    Answer.Lose
                    "with no tower it reaches a home nothing holds: nobody is lent"

                Expect.equal
                    (Facts.answer (mother 1 [ wraith ]) "W1N4")
                    Answer.Win
                    "one loaded tower beats it, so the home is quiet and the residents are lent"
            }

            // HomeHold: what a garrisoned home keeps is the
            // fleet's bodies left there, so one of two residents may leave
            // while the other beats the raider arriving, and neither once the
            // other is matched out (the world lane's FightMatch counts it).
            test
                "HomeHold keeps a garrison home while the bodies left lose, and releases it while they win" {
                let resident name =
                    creepWith
                        name
                        0
                        0
                        (List.replicate 21 Move
                         @ List.replicate 14 RangedAttack
                         @ List.replicate 7 Heal)

                let harasser =
                    { hostileIn
                          "W1N4"
                          { X = 25; Y = 20 }
                          (List.replicate 7 Move @ List.replicate 13 RangedAttack) with
                        Id = "Synapse673"
                        Owner = "Trepidimous"
                    }

                let mother =
                    { watchingBeyond [ harasser; wraithAt "W2N3" { X = 25; Y = 20 } ] with
                        Bank = bank 5600 5600
                    }
                    |> fun colony ->
                        { colony with
                            Creeps =
                                (colony.Creeps |> List.filter (fun creep -> creep.Name <> "r-1"))
                                @ [ resident "r-1"; resident "r-2" ]
                        }
                    |> standingIn "W1N3" ("r-1", { X = 25; Y = 26 })
                    |> standingIn "W1N3" ("r-2", { X = 27; Y = 26 })

                let r1 = mother.Creeps |> List.find (fun creep -> creep.Name = "r-1")

                let unmapped _ = None

                Expect.isTrue
                    (Facts.homeHoldReleases
                        mother
                        "W1N4"
                        (Some "W1N3")
                        (fun _ _ -> false)
                        unmapped
                        r1)
                    "r-2 staying beats Wraith403 arriving: r-1 may go"

                Expect.isFalse
                    (Facts.homeHoldReleases
                        mother
                        "W1N4"
                        (Some "W1N3")
                        (fun body _ -> body.Name = "r-2")
                        unmapped
                        r1)
                    "with r-2 matched out the home keeps nothing but no tower: r-1 stays"

                Expect.isFalse
                    (Facts.homeHoldReleases
                        mother
                        "W1N6"
                        (Some "W1N3")
                        (fun _ _ -> false)
                        unmapped
                        r1)
                    "and a garrison body standing at home leaves only for a room beside it"
            }

            // One window for one question (#531): a raider's arms for the
            // quiet are read at the window of its arrival, the lend's, never
            // at the shorter one to close and strike.
            test
                "a raider whose gun heals back after the strike window but inside the lend's breaks the quiet" {
                let harasser =
                    { hostileIn
                          "W1N4"
                          { X = 25; Y = 20 }
                          (List.replicate 7 Move @ List.replicate 13 RangedAttack) with
                        Id = "Synapse673"
                        Owner = "Trepidimous"
                    }

                let resident name =
                    creepWith
                        name
                        0
                        0
                        (List.replicate 21 Move
                         @ List.replicate 14 RangedAttack
                         @ List.replicate 7 Heal)

                let mother others =
                    { watchingBeyond (harasser :: others) with
                        Bank = bank 5600 5600
                    }
                    |> fun colony ->
                        { colony with
                            Creeps =
                                (colony.Creeps |> List.filter (fun creep -> creep.Name <> "r-1"))
                                @ [ resident "r-1" ]
                        }

                // Its gun at the head of eleven parts, back past 1,000 hits:
                // 42 ticks of its own 12 from 500.
                let mending =
                    { hostileIn
                          "W2N3"
                          { X = 45; Y = 25 }
                          ([ RangedAttack ]
                           @ List.replicate 6 Tough
                           @ List.replicate 3 Move
                           @ [ Heal ]) with
                        Id = "Mender"
                        Owner = "Trepidimous"
                        Hits = 500
                    }

                Expect.isFalse
                    (HostileInfo.armed [ mending ] mending)
                    "the premise: unarmed at the window to close and strike"

                Expect.isTrue
                    (HostileInfo.partsWithin Reaction.lend [ mending ] mending
                     |> List.contains RangedAttack)
                    "the premise: armed at the lend's"

                Expect.equal
                    (Facts.answer (mother [ mending ]) "W1N4")
                    Answer.Lose
                    "so it keeps the residents home"
            }
        ]
