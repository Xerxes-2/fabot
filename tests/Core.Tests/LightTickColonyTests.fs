/// Which colonies a forced tick decides: who can hurt us this tick, the
/// reset's split, the deciders of each forcing fact and the plans the rest
/// replay, and a colony's fight decided alone.
module Fabot.Core.Tests.LightTickColonyTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.LightTick
open Fabot.Core.Tests.LightTickFixtures

/// Four colonies, each projecting its home and one outpost.
let private fourColonies =
    [
        "W1N1", Set.ofList [ "W1N1"; "W2N1" ]
        "W3N1", Set.ofList [ "W3N1"; "W4N1" ]
        "W5N1", Set.ofList [ "W5N1"; "W6N1" ]
        "W7N1", Set.ofList [ "W7N1"; "W8N1" ]
    ]

let private hostileIn roomName x y owner armed : GlanceHostile =
    {
        Id = "h-1"
        Tile = RoomPos.at roomName { X = x; Y = y }
        Owner = owner
        Armed = armed
        Heals = false
        Weapon = if armed then Some Engine.meleeRange else None
        Gait = mobile
        Breaks = false
        Hits = 1000
    }

[<Tests>]
let hurtingTests =
    testList
        "who can hurt us this tick"
        [
            test
                "an armed body within reach of a creep of ours can; one far off, a scout beside it and an ally cannot" {
                let near =
                    { hostile 12 10 "Trepidimous" true with
                        Id = "near"
                    }

                let far =
                    { hostile 45 45 "Trepidimous" true with
                        Id = "far"
                    }

                let scout =
                    { hostile 11 10 "Trepidimous" false with
                        Id = "scout"
                    }

                let ally =
                    { hostile 11 11 "Odiodin" true with
                        Id = "ally"
                    }

                let seen =
                    { quiet with
                        Hostiles = [ near; far; scout; ally ]
                    }

                Expect.equal
                    (LightTick.hurting seen |> List.map _.Id)
                    [ "near" ]
                    "the armed body in reach alone"
            }

            test "a body in a room under our safe mode can hurt nothing" {
                let seen =
                    { quiet with
                        Hostiles = [ hostile 12 10 "Trepidimous" true ]
                        Controllers = Map.ofList [ room, (5, true) ]
                    }

                Expect.isEmpty (LightTick.hurting seen) "safe mode holds it off"
            }

            test "a dismantler beside a structure of ours can, beside only a creep it cannot" {
                let breaker =
                    { hostile 31 30 "Trepidimous" false with
                        Breaks = true
                    }

                Expect.equal
                    (LightTick.hurting { quiet with Hostiles = [ breaker ] } |> List.length)
                    1
                    "beside the spawn"

                Expect.isEmpty
                    (LightTick.hurting
                        { quiet with
                            Hostiles = [ { breaker with Tile = tile 11 10 } ]
                        })
                    "beside the digger alone"
            }
        ]

[<Tests>]
let resetSplitTests =
    testList
        "reset split"
        [
            test
                "a reset with one threatened colony decides it first, then the rest in order up to half" {
                let seen =
                    { quiet with
                        Hostiles = [ hostileIn "W8N1" 25 25 "Trepidimous" true ]
                    }

                let first, rest = LightTick.resetSplit 0.5 seen Set.empty fourColonies

                Expect.equal
                    first
                    [ "W7N1"; "W1N1" ]
                    "the threatened colony, then the first in order"

                Expect.equal
                    rest
                    [ "W3N1"; "W5N1" ]
                    "the remaining colonies decide on the next tick"
            }

            test "an ally's or a keeper's armed body threatens nobody, and the split stays in order" {
                let seen =
                    { quiet with
                        Hostiles =
                            [
                                hostileIn "W8N1" 25 25 "Odiodin" true
                                hostileIn "W6N1" 25 25 "Source Keeper" true
                            ]
                    }

                let first, rest = LightTick.resetSplit 0.5 seen Set.empty fourColonies

                Expect.equal first [ "W1N1"; "W3N1" ] "the first half in order"
                Expect.equal rest [ "W5N1"; "W7N1" ] "the second half"
            }

            test "an unarmed hostile that takes nothing down, beside our creep, threatens no colony" {
                // User, 2026-10-04: a body that cannot hurt a creep is no reason
                // to decide.
                let seen =
                    { quiet with
                        Creeps =
                            Map.ofList
                                [
                                    "miner",
                                    { creepAt 10 10 1000 with
                                        Tile = RoomPos.at "W6N1" { X = 10; Y = 10 }
                                    }
                                ]
                        Hostiles = [ hostileIn "W6N1" 12 10 "Trepidimous" false ]
                    }

                let first, _ = LightTick.resetSplit 0.5 seen Set.empty fourColonies

                Expect.equal first [ "W1N1"; "W3N1" ] "the colonies in their order, none picked out"
            }

            test "more threatened colonies than half all decide first" {
                let seen =
                    { quiet with
                        Hostiles =
                            [
                                hostileIn "W2N1" 25 25 "Invader" true
                                hostileIn "W4N1" 25 25 "Invader" true
                                hostileIn "W8N1" 25 25 "Invader" true
                            ]
                    }

                let first, rest = LightTick.resetSplit 0.5 seen Set.empty fourColonies

                Expect.equal first [ "W1N1"; "W3N1"; "W7N1" ] "every threatened colony"
                Expect.equal rest [ "W5N1" ] "only the quiet one waits"
            }

            test "a colony with a room the world danger log holds danger in decides first, unseen" {
                // A shut outpost watched by vision alone: nothing of the raid is
                // in sight on the reset tick, and the log still says whose it is.
                let first, rest =
                    LightTick.resetSplit 0.5 quiet (Set.ofList [ "W6N1" ]) fourColonies

                Expect.equal
                    first
                    [ "W5N1"; "W1N1" ]
                    "the endangered colony, then the first in order"

                Expect.equal rest [ "W3N1"; "W7N1" ] "the rest wait"
            }

            test "a lone colony decides on the reset tick" {
                let first, rest =
                    LightTick.resetSplit 0.5 quiet Set.empty [ List.head fourColonies ]

                Expect.equal first [ "W1N1" ] "one colony is its own half"
                Expect.isEmpty rest "nothing waits"
            }

            test "the tick after a reset that deferred colonies is full" {
                let deferring =
                    { LightTick.lastFull quiet Map.empty [] with
                        Deferred = Set.ofList [ "W5N1" ]
                    }

                Expect.equal
                    (LightTick.forced deferring quiet)
                    (Some LightForce.Deferred)
                    "a quiet world still decides the deferred colonies"
            }
        ]

/// Two colonies: W1N1 (and its outpost W2N1) holding the digger and the
/// hauler, W3N1 (and W4N1) holding a miner standing in W4N1 and a ranger
/// standing in W5N1, a room neither answers for.
let private pair =
    [ "W1N1", Set.ofList [ "W1N1"; "W2N1" ]; "W3N1", Set.ofList [ "W3N1"; "W4N1" ] ]

let private split: Glance =
    { quiet with
        Objects = Set.add "src-b" quiet.Objects
        Creeps =
            quiet.Creeps
            |> Map.add
                "miner"
                {
                    Tile = RoomPos.at "W4N1" { X = 10; Y = 10 }
                    Hits = 1000
                    Inward = []
                    Heals = 0
                    Reach = 0
                    Gait = None
                }
            |> Map.add
                "ranger"
                {
                    Tile = RoomPos.at "W5N1" { X = 10; Y = 10 }
                    Hits = 500
                    Inward = []
                    Heals = 0
                    Reach = 0
                    Gait = None
                }
    }

let private holders =
    Map.ofList [ "digger", "W1N1"; "hauler", "W1N1"; "miner", "W3N1"; "ranger", "W3N1" ]

/// Each colony's plan off `split`: the digger harvesting and the hauler a
/// step into a walk; the miner harvesting, the observer looking.
let private planned: LastFull =
    let planOf creeps steps intents =
        LightTick.plan split (Set.ofList creeps) steps [] [] intents [] Map.empty Map.empty

    LightTick.ofPlans
        split
        Set.empty
        [
            "W1N1",
            planOf
                [ "digger"; "hauler" ]
                (Map.ofList [ "hauler", [ tile 20 20; tile 21 20 ] ])
                [ HarvestSource("digger", "src-a") ]
            "W3N1",
            planOf
                [ "miner"; "ranger" ]
                Map.empty
                [ HarvestSource("miner", "src-b"); ObserveRoom("obs-1", "W6N1") ]
        ]

[<Tests>]
let decidersTests =
    testList
        "forced tick decides by colony"
        [
            test
                "a hostile decides the colonies answering its room and those whose bodies stand there" {
                Expect.equal
                    (LightTick.deciders
                        pair
                        holders
                        planned
                        split
                        [ LightForce.Hostile("W2N1", "Invader") ])
                    (Set.ofList [ "W1N1" ])
                    "W1N1's outpost: W1N1 alone"

                Expect.equal
                    (LightTick.deciders
                        pair
                        holders
                        planned
                        split
                        [ LightForce.Hostile("W5N1", "Invader") ])
                    (Set.ofList [ "W3N1" ])
                    "a room nobody answers for, where W3N1's ranger stands"
            }

            test
                "a raider at a home decides its colony and every colony with a fighter standing there" {
                // #552: W15S28's residents in W17S25 replayed a plan made
                // before the healer bounced back in.
                let standing reach =
                    { split with
                        Creeps =
                            split.Creeps
                            |> Map.add
                                "ranger"
                                {
                                    Tile = RoomPos.at "W1N1" { X = 10; Y = 10 }
                                    Hits = 500
                                    Inward = []
                                    Heals = 0
                                    Reach = reach
                                    Gait = None
                                }
                    }

                let resident = standing Engine.rangedRange

                Expect.equal
                    (LightTick.deciders
                        pair
                        holders
                        planned
                        (standing 0)
                        [ LightForce.Home("W1N1", "Trepidimous") ])
                    (Set.ofList [ "W1N1" ])
                    "a body with no weapon and no HEAL there decides nothing of W3N1's"

                Expect.equal
                    (LightTick.deciders
                        pair
                        holders
                        planned
                        split
                        [ LightForce.Home("W1N1", "Trepidimous") ])
                    (Set.ofList [ "W1N1" ])
                    "the home alone"

                Expect.equal
                    (LightTick.deciders
                        pair
                        holders
                        planned
                        resident
                        [ LightForce.Home("W1N1", "Trepidimous") ])
                    (Set.ofList [ "W1N1"; "W3N1" ])
                    "and W3N1, whose ranger stands in it"
            }

            test "a creep's fact decides its holder, wherever it stands" {
                Expect.equal
                    (LightTick.deciders pair holders planned split [ LightForce.OnBorder "miner" ])
                    (Set.ofList [ "W3N1" ])
                    "the miner's colony"

                let crossing =
                    { split with
                        Creeps =
                            split.Creeps
                            |> Map.add
                                "miner"
                                {
                                    Tile = tile 15 15
                                    Hits = 900
                                    Inward = []
                                    Heals = 0
                                    Reach = 0
                                    Gait = None
                                }
                    }

                Expect.equal
                    (LightTick.deciders
                        pair
                        holders
                        planned
                        crossing
                        [ LightForce.HitsLost "miner" ])
                    (Set.ofList [ "W3N1" ])
                    "hurt crossing W1N1: its own colony decides its flight"

                Expect.equal
                    (LightTick.deciders
                        pair
                        (Map.remove "hauler" holders |> Map.add "next" "W3N1")
                        planned
                        split
                        [ LightForce.CreepsChanged [ "hauler" ] ])
                    (Set.ofList [ "W1N1" ])
                    "a body gone: the colony whose plan held it"

                Expect.equal
                    (LightTick.deciders
                        pair
                        (Map.add "next" "W3N1" holders)
                        planned
                        split
                        [ LightForce.CreepsChanged [ "next" ] ])
                    (Set.ofList [ "W3N1" ])
                    "a body born: its holder"
            }

            test
                "a fight decides its colony; a fact no colony owns, everyone; a colony with no plan always decides" {
                Expect.equal
                    (LightTick.deciders pair holders planned split [ LightForce.Fought "W3N1" ])
                    (Set.ofList [ "W3N1" ])
                    "the colony whose tower fought"

                Expect.equal
                    (LightTick.deciders
                        pair
                        holders
                        planned
                        split
                        [ LightForce.ControllerChanged "W9N9" ])
                    (Set.ofList [ "W1N1"; "W3N1" ])
                    "nobody's room: everyone"

                Expect.equal
                    (LightTick.deciders
                        (pair @ [ "W7N1", Set.ofList [ "W7N1" ] ])
                        holders
                        planned
                        split
                        [ LightForce.Fought "W3N1" ])
                    (Set.ofList [ "W3N1"; "W7N1" ])
                    "W7N1 holds no plan"
            }

            test
                "a faulted colony is deferred on the first fault of a run alone, and a clean decide ends the run" {
                // #628: a colony that throws every tick forces one full
                // tick, then replays until the cadence's.
                let runs, deferred =
                    LightTick.faultRuns Map.empty (Set.ofList [ "W3N1" ]) (Set.ofList [ "W1N1" ])

                Expect.equal deferred (Set.ofList [ "W3N1" ]) "the first fault defers"

                let again, deferredAgain =
                    LightTick.faultRuns runs (Set.ofList [ "W3N1" ]) (Set.ofList [ "W1N1" ])

                Expect.isEmpty deferredAgain "the second in a row does not"
                Expect.equal (Map.tryFind "W3N1" again) (Some 2) "the run counts on"

                let idle, _ = LightTick.faultRuns again Set.empty Set.empty

                Expect.equal
                    (Map.tryFind "W3N1" idle)
                    (Some 2)
                    "a tick it does not decide stands the run"

                let clean, _ = LightTick.faultRuns again Set.empty (Set.ofList [ "W3N1" ])
                Expect.isEmpty clean "a clean decide ends it"

                let _, afresh = LightTick.faultRuns clean (Set.ofList [ "W3N1" ]) Set.empty
                Expect.equal afresh (Set.ofList [ "W3N1" ]) "and the next fault defers again"
            }

            test "a faulted colony's carried plan forces no tick for its fight or its chase" {
                // A tower's fight forces its colony's next tick, and a chase
                // whose quarry walks off forces its chaser's: carried, either
                // would re-decide the faulted colony every tick.
                let carry (record: LastFull) glance =
                    LightTick.ofPlans
                        glance
                        Set.empty
                        [ "", LightTick.faultedPlan glance (Some(solo record)) ]

                let fought = LightTick.lastFull quiet Map.empty [ FireTower("tower", "h") ]

                Expect.equal
                    (LightTick.forced fought quiet)
                    (Some(LightForce.Fought ""))
                    "the premise: it forces"

                Expect.isNone (LightTick.forced (carry fought quiet) quiet) "carried, it does not"

                let chasing =
                    { quiet with
                        Creeps = Map.ofList [ "ranger", creepAt 12 10 1000 ]
                        Hostiles = [ hostile 13 10 "Trepidimous" false ]
                    }

                let walkedOff =
                    { chasing with
                        Hostiles = [ hostile 14 10 "Trepidimous" false ]
                    }

                let chase =
                    LightTick.lastFull chasing Map.empty [ RangedAttackCreep("ranger", "h-1") ]
                    |> aiming [ RangedAttackCreep("ranger", "h-1") ] Map.empty

                Expect.equal
                    (LightTick.forced chase walkedOff)
                    (Some(LightForce.Chase "ranger"))
                    "the premise: the quarry walked off forces the chase"

                Expect.isNone
                    (LightTick.forced (carry chase chasing) walkedOff)
                    "carried, it does not"
            }

            test
                "a faulted colony with no plan carries an empty one, so a forced tick does not decide it for that" {
                let carried =
                    LightTick.ofPlans
                        split
                        Set.empty
                        [ "W1N1", planned.Plans.["W1N1"]; "W7N1", LightTick.faultedPlan split None ]

                Expect.equal
                    (LightTick.deciders
                        (pair @ [ "W7N1", Set.ofList [ "W7N1" ] ])
                        holders
                        carried
                        split
                        [ LightForce.Fought "W1N1" ])
                    (Set.ofList [ "W1N1"; "W3N1" ])
                    "W3N1 holds no plan and decides; W7N1's empty plan keeps it out"

                Expect.isEmpty
                    (LightTick.replay (Set.ofList [ "W7N1" ]) holders carried split)
                    "and it replays nothing"

                Expect.equal
                    (LightTick.faultedPlan split (Some planned.Plans.["W3N1"]))
                    planned.Plans.["W3N1"]
                    "a colony with a plan carries the one it replays"
            }

            test "a replayed colony issues its own plan for its own bodies, and its looks" {
                Expect.equal
                    (LightTick.replay (Set.ofList [ "W3N1" ]) holders planned split)
                    [ HarvestSource("miner", "src-b"); ObserveRoom("obs-1", "W6N1") ]
                    "the miner and the observer; nothing of W1N1's"

                Expect.equal
                    (LightTick.replay (Set.ofList [ "W1N1" ]) holders planned split)
                    [ HarvestSource("digger", "src-a"); MoveCreep("hauler", Right) ]
                    "W1N1's harvest and step"

                Expect.equal
                    (LightTick.replay
                        (Set.ofList [ "W3N1" ])
                        (Map.add "hauler" "W3N1" holders)
                        planned
                        split)
                    [ HarvestSource("miner", "src-b"); ObserveRoom("obs-1", "W6N1") ]
                    "a body W3N1 adopted whose plan was W1N1's stands"

                Expect.equal
                    (LightTick.intents planned split)
                    [
                        HarvestSource("digger", "src-a")
                        MoveCreep("hauler", Right)
                        HarvestSource("miner", "src-b")
                        ObserveRoom("obs-1", "W6N1")
                    ]
                    "a light tick replays every plan"
            }

            test "#562 a replayed step claims the tile it lands on, unless the step fails anyway" {
                let homes = Set.ofList [ "W1N1" ]

                // The bodies W1N1 holds, whose plans replay.
                let claims tired glance replayed =
                    LightTick.claimed (Set.ofList [ "digger"; "hauler" ]) tired glance replayed

                Expect.equal
                    (claims Set.empty split (LightTick.replay homes holders planned split))
                    (Set.singleton (tile 21 20))
                    "the tile the replayed step lands on is the deciding colonies' claim, and no act is"

                let step = [ MoveCreep("hauler", Right) ]

                Expect.isEmpty
                    (claims (Set.ofList [ "hauler" ]) split step)
                    "a fatigued walker claims nothing"

                Expect.isEmpty
                    (claims
                        Set.empty
                        { split with
                            Hostiles = [ hostile 21 20 "Invader" false ]
                        }
                        step)
                    "nor a step onto a hostile"

                let ahead =
                    { split with
                        Creeps = Map.add "digger" (creepAt 21 20 1000) split.Creeps
                    }

                Expect.isEmpty
                    (claims Set.empty ahead step)
                    "nor onto a replayed body that takes no step"

                Expect.equal
                    (claims Set.empty ahead (MoveCreep("digger", Right) :: step))
                    (Set.ofList [ tile 21 20; tile 22 20 ])
                    "a replayed body stepping on out of it leaves the claim standing"
            }

            test "a forced tick's fresh plan claims a body before the plan it carried" {
                // A body a deciding colony adopted is that colony's alone: two
                // plans replaying one body would lay two acts on its channels.
                let fresh =
                    LightTick.plan
                        split
                        (Set.ofList [ "hauler" ])
                        Map.empty
                        []
                        []
                        [ PickupPile("hauler", "pile") ]
                        []
                        Map.empty
                        Map.empty

                let carried = planned.Plans["W1N1"]
                let record = LightTick.ofPlans split Set.empty [ "W3N1", fresh; "W1N1", carried ]

                Expect.equal
                    record.Plans["W1N1"].Creeps
                    (Set.ofList [ "digger" ])
                    "the carried plan less the hauler"

                Expect.isEmpty record.Plans["W1N1"].Steps "and its step plan"

                Expect.equal
                    (LightTick.intents record split)
                    [ HarvestSource("digger", "src-a"); PickupPile("hauler", "pile") ]
                    "the hauler's pickup alone"
            }
        ]

/// The fighter test `LightTick.fightOnly` reads off the views: W3N1's ranger.
let private ranger name = name = "ranger"

[<Tests>]
let fightOnlyTests =
    testList
        "a forced tick decides a colony's fight alone"
        [
            test "every fact a fight's: the colony decides its fight alone" {
                let alone reasons =
                    LightTick.fightOnly ranger pair holders planned split reasons

                Expect.equal
                    (alone [ LightForce.Home("W3N1", "Trepidimous") ])
                    (Set.ofList [ "W3N1" ])
                    "a raider at home"

                Expect.equal
                    (alone [ LightForce.Hostile("W5N1", "Invader") ])
                    (Set.ofList [ "W3N1" ])
                    "a hostile new in reach, where its ranger stands"

                Expect.equal
                    (alone [ LightForce.Fought "W3N1" ])
                    (Set.ofList [ "W3N1" ])
                    "its tower fought"

                Expect.equal
                    (alone [ LightForce.HitsLost "ranger" ])
                    (Set.ofList [ "W3N1" ])
                    "its fighter hurt"

                Expect.equal
                    (alone [ LightForce.Chase "ranger" ])
                    (Set.ofList [ "W3N1" ])
                    "its fighter's quarry walking off (#619)"
            }

            test "an economy fact decides its colony whole" {
                let alone reasons =
                    LightTick.fightOnly ranger pair holders planned split reasons

                let home = LightForce.Home("W3N1", "Trepidimous")

                Expect.isEmpty
                    (alone [ home; LightForce.OnBorder "miner" ])
                    "a raid beside a body on the ring"

                Expect.isEmpty
                    (alone [ home; LightForce.CreepsChanged [ "miner" ] ])
                    "beside a body gone"

                Expect.isEmpty
                    (alone [ home; LightForce.ControllerChanged "W3N1" ])
                    "beside its controller"

                Expect.isEmpty
                    (LightTick.fightOnly
                        ranger
                        (pair @ [ "W7N1", Set.ofList [ "W7N1" ] ])
                        holders
                        planned
                        split
                        [ LightForce.Fought "W7N1" ])
                    "a colony with no plan to replay decides whole"
            }

            test
                "a hurt economy body decides its colony's fight alone: its answer is the fight's, its flight the decide's hand-back (#669)" {
                // The Invader stood beside the miner as the plans were made;
                // it struck since, and no plan strikes it.
                let raided =
                    { split with
                        Hostiles = [ hostileIn "W4N1" 11 11 "Invader" true ]
                    }

                let seen = LightTick.ofPlans raided Set.empty (planned.Plans |> Map.toList)

                let hurt =
                    { raided with
                        Creeps =
                            raided.Creeps
                            |> Map.change
                                "miner"
                                (Option.map (fun miner -> { miner with Hits = 900 }))
                    }

                let reasons = LightTick.forcing seen hurt

                Expect.equal
                    reasons
                    [ LightForce.HitsLost "miner" ]
                    "premise: the miner's lost hits alone"

                Expect.equal
                    (LightTick.fightOnly ranger pair holders seen hurt reasons)
                    (Set.ofList [ "W3N1" ])
                    "the miner's colony decides its fight alone"

                Expect.equal
                    (LightTick.fightOnly
                        ranger
                        pair
                        holders
                        seen
                        hurt
                        [ LightForce.Home("W3N1", "Trepidimous"); LightForce.HitsLost "digger" ])
                    (Set.ofList [ "W1N1"; "W3N1" ])
                    "another colony's hurt body is that colony's fight"
            }

            test
                "an economy body a hostile can newly strike decides its colony whole: Flee is the economy's" {
                let raided =
                    { split with
                        Hostiles = [ hostileIn "W4N1" 11 11 "Invader" true ]
                    }

                Expect.isEmpty
                    (LightTick.fightOnly
                        ranger
                        pair
                        holders
                        planned
                        raided
                        [ LightForce.Home("W3N1", "Trepidimous") ])
                    "the miner stands beside it"

                let seen = LightTick.ofPlans raided Set.empty (planned.Plans |> Map.toList)

                Expect.equal
                    (LightTick.fightOnly
                        ranger
                        pair
                        holders
                        seen
                        raided
                        [ LightForce.Home("W3N1", "Trepidimous") ])
                    (Set.ofList [ "W3N1" ])
                    "one the plans already saw reach it is replayed"
            }

            test
                "an economy body its replayed step walks into a hostile's reach decides its colony whole" {
                // The hauler stands at (20,20) and its plan steps it to
                // (21,20). A melee raider three off reaches neither tile now
                // nor after its own step from where the hauler stands; both
                // stepping in, it strikes on the next tick, too late for Flee.
                let raided x =
                    { split with
                        Hostiles = [ hostileIn room x 20 "Invader" true ]
                    }

                let alone glance =
                    LightTick.fightOnly
                        ranger
                        pair
                        holders
                        planned
                        glance
                        [ LightForce.Home("W1N1", "Invader") ]

                Expect.isEmpty (alone (raided 23)) "two off the tile the hauler lands on"

                Expect.equal
                    (alone (raided 24))
                    (Set.ofList [ "W1N1" ])
                    "three off it: the fight alone"
            }

            test
                "a fight-only colony's economy replays its plan, and its fighters' plan is the fight's" {
                Expect.equal
                    (LightTick.replayBodies
                        (Set.ofList [ "W3N1" ])
                        (Set.ofList [ "miner" ])
                        planned
                        split)
                    [ HarvestSource("miner", "src-b"); ObserveRoom("obs-1", "W6N1") ]
                    "the miner's harvest and the observer's look"

                let carried = planned.Plans["W3N1"]

                let fresh =
                    LightTick.plan
                        { split with Time = 7 }
                        (Set.ofList [ "ranger" ])
                        (Map.ofList [ "ranger", [ RoomPos.at "W5N1" { X = 11; Y = 10 } ] ])
                        []
                        []
                        [ FireTower("tower-1", "h-1"); RangedAttackCreep("ranger", "h-1") ]
                        [ RangedAttackCreep("ranger", "h-1") ]
                        Map.empty
                        Map.empty

                let merged = LightTick.withFight (Set.ofList [ "ranger" ]) fresh carried

                Expect.equal merged.Creeps (Set.ofList [ "miner"; "ranger" ]) "both halves' bodies"

                Expect.equal
                    merged.Acts
                    [
                        HarvestSource("miner", "src-b")
                        ObserveRoom("obs-1", "W6N1")
                        RangedAttackCreep("ranger", "h-1")
                    ]
                    "the economy's acts, then the fighter's; no tower's"

                Expect.isTrue merged.Fought "its tower fought this tick"
                Expect.equal merged.At carried.At "the economy's later acts keep their plan's tick"
                Expect.equal merged.Aims [ RangedAttackCreep("ranger", "h-1") ] "the fighter's aim"
                Expect.isTrue (Map.containsKey "ranger" merged.Steps) "the fighter's step plan"
            }
        ]
