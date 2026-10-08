/// An outpost raid's engagement: who a kill admits and lends, the row that
/// reaches it, a fight fought to its target's death, and a contested outpost.
module Fabot.Core.Tests.Decide.OutpostEngagementTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.OutpostFixtures

/// The raided outpost floored out to its borders, and W1N3 beyond it, a room
/// of nobody's, floored too: no source, no controller, nothing of ours.
let private beyond hostiles =
    let colony = declaredRaid hostiles
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    let floor =
        TerrainGrid.ofList
            [
                for x in 1..48 do
                    for y in 1..48 -> { X = x; Y = y }, Plain
            ]

    { colony with
        Spatial =
            { colony.Spatial with
                Borders = colony.Spatial.Borders |> Map.add "W1N3" plainRing
            }
            |> withNeighbour "W1N2" { outpost with Terrain = floor }
            |> withNeighbour "W1N3" { RoomLayer.empty with Terrain = floor }
    }

/// A digger in the outpost and a raider beyond its north border whose Reach
/// spans (20..30, 0..3) — it lands on y = 0 at tick 2 and shoots three from
/// there (movement S2) —, our guard `g-1` at (19,1) just west of it.
let private trespassPastAReach digger =
    beyond
        [
            diggerAt "W1N2" digger
            { hostileIn "W1N3" { X = 25; Y = 47 } smallMelee with
                Id = "h-2"
            }
        ]
    |> withGuards [ guard "g-1", { X = 19; Y = 1 } ]

[<Tests>]
let killAdmissionTests =
    testList
        "who a kill admits, who is lent to it, and how it is walked to"
        [
            test
                "a raider in a room beside the resident's home, not the kill's, keeps the resident home" {
                let digger = diggerAt "W1N2" { X = 25; Y = 10 }

                let raider =
                    { hostileIn "W1N4" { X = 25; Y = 25 } smallMelee with
                        Id = "h-2"
                    }

                let heldOf hostiles =
                    (decide (raisingBeyond hostiles) residentHeld Set.empty None).Assignments
                    |> Map.tryFind "r-1"

                Expect.equal (heldOf [ digger ]) (Some(taskId (Guard "W1N2"))) "the premise: lent"

                Expect.equal
                    (heldOf [ digger; raider ])
                    (Some(taskId (Guard "W1N3")))
                    "a raider next door to its home is a home that is not quiet"
            }

            // #602: a Guard lost where its body is struck is let go, but
            // never a home of ours': the resident struck in the child home it
            // defends, losing on paper, keeps fighting there, and never runs
            // off to its own home.
            test
                "a resident struck in the child home it defends, losing on paper, keeps its Guard there" {
                let escort =
                    { hostileIn
                          "W1N3"
                          { X = 26; Y = 26 }
                          (List.replicate 3 RangedAttack
                           @ List.replicate 7 Move
                           @ List.replicate 4 Heal) with
                        Id = "h-escort"
                    }

                let result = decide (raisingBeyond [ escort ]) residentHeld Set.empty None

                Expect.equal
                    (Map.tryFind "r-1" result.Assignments)
                    (Some(taskId (Guard "W1N3")))
                    "it holds the child's Guard"
            }

            test "a resident standing in its home is never a fresh taker of a kill not beside it" {
                let takes hostiles =
                    (decideOn (raisingBeyond hostiles)).Assignments |> Map.tryFind "r-1"

                Expect.equal
                    (takes [ diggerAt "W1N2" { X = 25; Y = 10 } ])
                    (Some(taskId (Guard "W1N2")))
                    "the premise: unheld, it takes a kill beside its home"

                Expect.notEqual
                    (takes [ diggerAt "W1N1" { X = 25; Y = 5 } ])
                    (Some(taskId (Guard "W1N1")))
                    "and not one two rooms off"
            }

            test "a chase in sight admits only a ranger whose own parts kite the raid down" {
                // Five RANGED_ATTACK on one MOVE: slower than the plodder, so it
                // cannot hold the range on it.
                let slow = creepWith "r-slow" 0 0 (List.replicate 5 RangedAttack @ [ Move ])

                let takes creep =
                    (decideOn (
                        declaredRaid [ plodderAt { X = 25; Y = 44 } ]
                        |> withGuards [ creep, { X = 22; Y = 47 } ]
                    ))
                        .Assignments
                    |> Map.tryFind creep.Name

                Expect.equal (takes (ranger "r-1")) (Some(taskId (Guard "W1N2"))) "the premise"

                Expect.notEqual
                    (takes slow)
                    (Some(taskId (Guard "W1N2")))
                    "a slow ranger is refused"
            }

            // A target is out of a chase only if it outruns the chaser on
            // road, plain and swamp alike (#531, the user 2026-10-05: a real
            // opponent runs into dead ends, swamp and exit bounces).
            // Synapse673 keeps a 1:1 guard's pace on road and falls behind it
            // on plain, so the guard chases it.
            test
                "a ranged raider slower on plain than the guard is chased, and the outpost stays open; with no guard castable it stands down" {
                let synapse =
                    { hostileIn
                          "W1N2"
                          { X = 25; Y = 45 }
                          (List.replicate 7 Move @ List.replicate 13 RangedAttack) with
                        Id = "Synapse673"
                        Owner = "Trepidimous"
                    }

                let shut (colony: ColonyView) =
                    Observe.RaidState.empty
                    |> Observe.foldRaids
                        Observe.capEpisodes
                        Set.empty
                        colony
                        (Planner.outpostFactsOf colony (Atlas.ofView colony)).Declared
                    |> Observe.standDown Tuning.defaults (colony.Time + 1)
                    |> fun gate -> gate.Shut

                // Two guard blocks out-trade it, and the guard standing there
                // is the two-block body the row casts: one block loses it
                // alone, and is not sent (#615 U3).
                let colony =
                    { declaredRaid [ synapse ] with
                        Bank = bank 1500 1500
                    }
                    |> withGuards
                        [ creepWith "g-1" 0 0 (bodyFor guardPattern 1500), { X = 20; Y = 41 } ]

                Expect.isTrue
                    (blocksBeat guardPattern Stance.Hold colony "W1N2" (guardBlocksReach colony))
                    "the premise: the guard row's body out-trades it"

                let ground = groundOf colony

                Expect.isNonEmpty ground "the guard has ground"

                Expect.isTrue
                    (ground |> Set.forall (fun tile -> rangeTo (RoomPos.pos synapse.Pos) tile <= 1))
                    "the ring round the raider: a chase, not the post"

                Expect.equal
                    (Map.tryFind "g-1" (decideOn colony).Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the guard takes the room"

                Expect.isEmpty (shut colony) "and the outpost is not stood down"

                let bare =
                    { declaredRaid [ synapse ] with
                        Bank = bank 300 300
                    }

                Expect.equal (shut bare) (Set.singleton "W1N2") "with no guard castable it is"
            }

            // `Gait.outruns`, the kill's one catch rule (#531): strictly quicker
            // on road, plain and swamp alike. Characterisation (movement S0,
            // `docs/adr/0088`): catchability read off two gaits alone, blind to
            // the ground between. Flips at S6, pursuit on evidence.
            test
                "a kill admits no body its target outruns on every ground; equal pace on one is a chase" {
                let hauler =
                    { hostileIn
                          "W1N2"
                          { X = 25; Y = 44 }
                          (List.replicate 23 Carry @ List.replicate 23 Move) with
                        Owner = "Trepidimous"
                    }

                // Five RANGED_ATTACK on one MOVE: three ticks a road tile.
                let slow = creepWith "r-slow" 0 0 (List.replicate 5 RangedAttack @ [ Move ])

                let colony =
                    declaredRaid [ hauler ]
                    |> withGuards [ guard "g-1", { X = 20; Y = 41 }; slow, { X = 22; Y = 47 } ]

                let atlas = Atlas.ofView colony
                let brief = Threats.briefOf (threatsOf colony atlas) "W1N2"

                Expect.equal
                    (brief.Duty, Brief.isKill brief)
                    (Duty.Guard BriefKind.Contested, true)
                    "the premise: a contested room's prey, a kill"

                Expect.isFalse
                    (brief.Admits slow)
                    "an empty hauler outruns the slow ranger on every ground"

                Expect.isTrue
                    (brief.Admits(guard "g-1"))
                    "the 1:1 guard keeps its pace on road: worth the chase"
            }

            test "a chase out of sight in a room with no source walks the ranger in off the border" {
                let colony =
                    { beyond [] with
                        ChasedOutposts = Set.singleton "W1N3"
                    }
                    |> withGuards [ ranger "r-1", { X = 25; Y = 25 } ]

                Expect.isNone
                    (Threats.guardGroundIn (threatsOf colony (Atlas.ofView colony)) "W1N3")
                    "the premise: no ground of its own"

                Expect.equal
                    ((decideOn colony).Assignments |> Map.tryFind "r-1")
                    (Some(taskId (Guard "W1N3")))
                    "it is still walked to"
            }

            test "a trespass in a home under safe mode is capped at the home's guard count" {
                // Safe mode makes the raid no Threat, so the trespass is open;
                // the count still prices the raid, two healers over one block.
                let colony =
                    { declaredRaid
                          [
                              diggerAt "W1N1" { X = 25; Y = 5 }
                              { hostileIn "W1N1" { X = 25; Y = 6 } smallMelee with
                                  Id = "h-2"
                              }
                          ] with
                        RoomControl =
                            (declaredRaid []).RoomControl
                            |> Map.add "W1N1" { ownedRoom with SafeMode = true }
                    }

                let colony =
                    { colony with
                        Hostiles =
                            colony.Hostiles
                            @ [
                                for i in 1..2 ->
                                    { hostileIn "W1N1" { X = 25; Y = 7 } smallHealer with
                                        Id = $"heal-{i}"
                                    }
                            ]
                    }

                Expect.equal
                    (pooledHolding [] (Guard "W1N1") colony)
                    (Some(safetyRank OneRungUp, Capacity.fighters 2))
                    "a kill, for the two guards the raid prices"
            }

            test "a trespass's killer walks round a Reach on its way, and plans the step it takes" {
                // A raider beyond the border reaches (20..30, 0..3) of the
                // outpost; the straight walk east from (19,1) to the digger
                // past it crosses it.
                let colony = trespassPastAReach { X = 32; Y = 1 }
                let reach = Threats.reachIn (threatsOf colony (Atlas.ofView colony)) "W1N2"

                Expect.contains
                    reach
                    { X = 20; Y = 1 }
                    "the premise: the Reach lies across the walk"

                let decision = decideOn colony

                Expect.equal
                    (Map.tryFind "g-1" decision.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the guard takes the kill"

                let steps =
                    moveIntents decision.Intents
                    |> List.filter (fst >> (=) "g-1")
                    |> List.map (snd >> stepFrom { X = 19; Y = 1 })

                match steps, Map.tryFind "g-1" decision.Steps with
                | [ step ], Some(first :: next :: _) ->
                    Expect.isGreaterThan step.Y 1 "it steps down, under the Reach"
                    Expect.isFalse (Set.contains step reach) "and out of it"

                    // The light tick replays the plan's second tile from its first.
                    Expect.equal
                        first
                        (RoomPos.at "W1N2" step)
                        "its step plan is the step it issued"

                    Expect.isFalse (Set.contains (RoomPos.pos next) reach) "and the next one round"
                | other -> failtest $"one step and a plan: {other}"
            }

            // Characterisation (movement S0, `docs/adr/0088`): with the Reach a
            // wall and no way round, the walker waits at its edge
            // (`moveIntentFor.round`). Kept at S2 and S3: a trespass's killer
            // walks round the whole Reach (`Avoid.Reach`), the arrival map's
            // now; S3 moved only the kite's walk.
            test "a trespass's killer waits at the edge of a Reach that covers the kill's ground" {
                // The same raider reaches every tile beside a digger at (27,1):
                // the kill's ground less the Reach is empty, so there is no
                // way round to it.
                let colony = trespassPastAReach { X = 27; Y = 1 }
                let decision = decideOn colony

                Expect.equal
                    (Map.tryFind "g-1" decision.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the guard takes the kill"

                let steps =
                    moveIntents decision.Intents
                    |> List.filter (fst >> (=) "g-1")
                    |> List.map (snd >> stepFrom { X = 19; Y = 1 })

                Expect.isEmpty steps "it waits rather than step in"
                Expect.isNone (Map.tryFind "g-1" decision.Steps) "and plans no step"
            }
        ]

/// Trepidimous' kiter (#609): 3 RANGED_ATTACK, 4 HEAL, 7 MOVE — 30 a tick at
/// three, 48 healed.
let private trepKiter =
    List.replicate 3 RangedAttack @ List.replicate 4 Heal @ List.replicate 7 Move

/// The 4RA2H ranger an 1,800 bank buys: two ranger blocks.
let private twoBlockRanger = Bodies.rangerPattern.Block @ Bodies.rangerPattern.Block

/// The W11S28 invasion's shape (#535, live t973,670): a small mixed body
/// that closes, two shooters that keep range, a healer behind each.
let private invasion =
    [
        { hostileIn "W1N2" { X = 25; Y = 42 } smallMelee with
            Id = "mixed"
        }
        { hostileIn "W1N2" { X = 22; Y = 44 } smallRanged with
            Id = "shooter-1"
        }
        { hostileIn "W1N2" { X = 28; Y = 44 } smallRanged with
            Id = "shooter-2"
        }
        { hostileIn "W1N2" { X = 22; Y = 45 } smallHealer with
            Id = "healer-1"
        }
        { hostileIn "W1N2" { X = 28; Y = 45 } smallHealer with
            Id = "healer-2"
        }
    ]

[<Tests>]
let engagementTests =
    testList
        "an outpost raid's engagement: the row that reaches it, the target in reach, the step after it, together"
        [
            test "the W11S28 shape asks the ranger row, not two melee guards" {
                let colony =
                    { declaredRaid invasion with
                        Bank = bank 5600 5600
                    }

                let atlas = Atlas.ofView colony
                let brief = Threats.briefOf (threatsOf colony atlas) "W1N2"

                Expect.equal
                    (brief.Asks |> List.map (fun ask -> ask.Row))
                    [ CastRow.Fighter BodyTemplate.Ranged ]
                    "the guard's swing reaches no shooter, so the ranger row is asked"

                Expect.equal
                    (brief.Verdict.Value)
                    Answer.Win
                    "and the ranger wins it: the room stays open"

                Expect.isTrue
                    (brief.Admits(creepWith "ranger-1" 0 0 (bodyFor Bodies.rangerPattern 5600)))
                    "a ranger takes it"

                Expect.isFalse (brief.Admits(guard "g-1")) "and a melee guard does not"

                let mixedOnly =
                    { declaredRaid (List.take 1 invasion) with
                        Bank = bank 5600 5600
                    }

                Expect.equal
                    ((Threats.briefOf (threatsOf mixedOnly (Atlas.ofView mixedOnly)) "W1N2").Asks
                     |> List.map (fun ask -> ask.Row))
                    [ CastRow.Fighter BodyTemplate.Melee ]
                    "a raid that closes on the guard is still the guard's"

                let poor =
                    { declaredRaid invasion with
                        Bank = bank 550 550
                    }

                Expect.equal
                    ((Threats.briefOf (threatsOf poor (Atlas.ofView poor)) "W1N2").Asks
                     |> List.map (fun ask -> ask.Row))
                    [ CastRow.Fighter BodyTemplate.Melee ]
                    "a bank that casts no ranger block is never asked for one (#546): the guard's"
            }

            // #609 (W17S26, live t1,012,579): Trepidimous' 3RA4H7M kiter heals
            // 48 a tick; the 4RA2H body an 1,800 bank buys deals 40 and loses
            // alone, two deal 80 and win. Priced one body, the room went to
            // the melee guard, which never catches it.
            test
                "a kiter the bank's biggest ranger loses to alone and two beat asks the ranger row for two" {
                let colony =
                    { declaredRaid [ hostileIn "W1N2" { X = 25; Y = 42 } trepKiter ] with
                        Bank = bank 1800 1800
                    }

                let threats = threatsOf colony (Atlas.ofView colony)
                let brief = Threats.briefOf threats "W1N2"

                Expect.equal
                    (brief.Asks |> List.map (fun ask -> ask.Row, ask.Wanted threats))
                    [ CastRow.Fighter BodyTemplate.Ranged, Engine.guardCap ]
                    "the ranger row is asked, for the pair the verdict prices"

                Expect.equal
                    brief.Verdict.Value
                    Answer.Win
                    "and the pair wins it: the room stays open"

                Expect.isFalse (brief.Admits(guard "g-1")) "a melee guard does not take it"
            }

            // #609: the same kiter in a room the stand-down shuts is the chase,
            // and the chase casts the pair as the outpost does.
            test "the chase of a kiter one ranger loses to casts the pair" {
                let shut =
                    { declaredRaid [ hostileIn "W1N2" { X = 25; Y = 42 } trepKiter ] with
                        RaidedOutposts = Set.singleton "W1N2"
                        Bank = bank 1800 1800
                    }
                    |> withGuards [ creepWith "r-1" 0 0 twoBlockRanger, { X = 22; Y = 47 } ]

                let threats = threatsOf shut (Atlas.ofView shut)
                let brief = Threats.briefOf threats "W1N2"

                Expect.equal
                    brief.Duty
                    (Duty.Guard BriefKind.Chase)
                    "the premise: the room is the chase"

                Expect.equal
                    (brief.Asks |> List.map (fun ask -> ask.Row, ask.Wanted threats))
                    [ CastRow.Fighter BodyTemplate.Ranged, Engine.guardCap ]
                    "its row's ask is the pair"
            }

            // #609 (`Facts.fits`): a body struck by a raid it loses alone
            // counts a partner holding the fight that lands while it still
            // stands above its floor, priced at the hits it has left then.
            test
                "a struck body wins beside a partner that lands before it falls, and not one that lands after" {
                let raid = [ hostileIn "W1N2" { X = 25; Y = 42 } trepKiter ]
                let struck = creepWith "r-1" 0 0 twoBlockRanger
                let partner = creepWith "r-2" 0 0 twoBlockRanger
                let world = kitedBy raid [ struck, { X = 25; Y = 45 } ]

                let wins (body: CreepInfo) beside =
                    Facts.fits world Stance.Standoff "W1N2" 2 true body beside

                Expect.isFalse (wins struck []) "the premise: alone it loses (40 against 48 heal)"

                Expect.isTrue
                    (wins struck [ partner, true, Some 60 ])
                    "whole, it outlasts a partner sixty ticks out (6 a tick net)"

                Expect.isFalse
                    (wins struck [ partner, false, Some 60 ])
                    "one not holding the fight, off it, is no partner"

                Expect.isFalse
                    (wins struck [ partner, true, None ])
                    "nor one no walk prices, off the fight"

                // A third of its hits and its live parts: two shots, two heals.
                let worn =
                    { creepWith "r-1" 0 0 [ RangedAttack; RangedAttack; Heal; Heal ] with
                        Hits = { Hits = 400; HitsMax = 1_200 }
                    }

                Expect.isTrue
                    (wins worn [ partner, true, Some 2 ])
                    "worn, a partner within the horizon still counts"

                Expect.isFalse
                    (wins worn [ partner, true, Some 60 ])
                    "but one sixty ticks out lands after its heals are shot off and it falls"
            }

            // #624 (from #615 U3's review): a body lost anyway spends itself
            // only on damage a kill keeps, and a kill is only a finisher's
            // that survives its own exchange, or our towers'.
            test
                "a body lost anyway spends itself only beside a finisher that outlives the raid, or our towers" {
                let raid =
                    [
                        { hostileIn
                              "W1N2"
                              { X = 25; Y = 42 }
                              (List.replicate 12 Attack @ List.replicate 12 Move) with
                            Id = "m-1"
                        }
                        { hostileIn "W1N2" { X = 25; Y = 43 } smallHealer with
                            Id = "h-1"
                        }
                    ]

                let fourBlocks = List.concat (List.replicate 4 Bodies.rangerPattern.Block)

                // Five ticks to live: it ages out before it falls, so leaving
                // saves nothing, and its 80 a tick outpaces the 60 heal.
                let dying =
                    { creepWith "r-1" 0 0 fourBlocks with
                        TicksToLive = 5
                    }

                let partner = creepWith "r-2" 0 0 fourBlocks
                let world = kitedBy raid [ dying, { X = 25; Y = 45 }; partner, { X = 26; Y = 45 } ]

                let fits colony beside =
                    Facts.fits colony Stance.Standoff "W1N2" 2 true dying beside

                Expect.isFalse
                    (fits world [ partner, true, Some 0 ])
                    "the two lose together, and the partner alone, out-healing the heal by 20, falls to the 360 swing long before: what the dying one strikes is healed back"

                Expect.isTrue
                    (fits (world |> withOurTowers "W1N2" [ { X = 25; Y = 30 } ]) [])
                    "a tower out-damaging the heal alone finishes the raid: its 100 hits are kept"
            }

            test
                "the W11S28 shape as the engine's Invaders asks the ranger row: penned, its shooters still flee" {
                // An Invader with no ATTACK flees every hostile inside three
                // (`findAttack.js`): penned in its room, it keeps its range
                // there all the same (#568's spar, a guard kited to death).
                let npc = invasion |> List.map (fun h -> { h with Owner = "Invader" })

                let colony =
                    { declaredRaid npc with
                        Bank = bank 5600 5600
                    }

                let atlas = Atlas.ofView colony
                let brief = Threats.briefOf (threatsOf colony atlas) "W1N2"

                Expect.equal
                    (brief.Asks |> List.map (fun ask -> ask.Row))
                    [ CastRow.Fighter BodyTemplate.Ranged ]
                    "the guard's swing reaches no Invader shooter, so the ranger row is asked"

                Expect.isFalse (brief.Admits(guard "g-1")) "and a melee guard is not sent into it"

                let mixedOnly =
                    { declaredRaid (List.take 1 npc) with
                        Bank = bank 5600 5600
                    }

                Expect.equal
                    ((Threats.briefOf (threatsOf mixedOnly (Atlas.ofView mixedOnly)) "W1N2").Asks
                     |> List.map (fun ask -> ask.Row))
                    [ CastRow.Fighter BodyTemplate.Melee ]
                    "an Invader carrying ATTACK closes, so it is still the guard's"
            }

            test
                "a guard with a target beside it and its head three off swings at the one beside it" {
                // The kill order's head is the raider nearest the Post (25,41).
                let head =
                    { hostileIn "W1N2" { X = 25; Y = 43 } smallMelee with
                        Id = "h-head"
                    }

                let near =
                    { hostileIn "W1N2" { X = 28; Y = 46 } smallMelee with
                        Id = "h-near"
                    }

                // A guard that wins the two: an 800 one loses them, so it
                // runs from the one beside it (#602) and swings at nothing.
                let colony =
                    declaredRaid [ head; near ]
                    |> withGuards
                        [ creepWith "g-1" 0 0 (bodyFor guardPattern 2400), { X = 28; Y = 45 } ]

                let result = decideFrom (Map.ofList [ "g-1", taskId (Guard "W1N2") ]) colony

                Expect.equal
                    (result.Aims |> List.tryHead)
                    (Some(AttackCreep("g-1", "h-head")))
                    "the premise: the head of its order is the one three off"

                Expect.contains
                    result.Intents
                    (AttackCreep("g-1", "h-near"))
                    "it swings at the one beside it"

                Expect.isFalse
                    (List.contains (AttackCreep("g-1", "h-head")) result.Intents)
                    "and not at the head"
            }

            test "a melee guard beside its target steps onto the target's tile with its swing" {
                let colony = declaredRaid raiders |> withGuards [ guard "g-1", beside ]
                let result = decideFrom (Map.ofList [ "g-1", taskId (Guard "W1N2") ]) colony

                Expect.contains result.Intents (AttackCreep("g-1", "h-1")) "the swing lands"

                Expect.equal
                    (moveIntents result.Intents |> List.filter (fst >> (=) "g-1") |> List.map snd)
                    [ (directionTo beside { X = 25; Y = 42 }).Value ]
                    "and it steps onto the target's tile: a target that leaves is followed, one that stays costs a failed move"
            }

            test "two guards that lose alone and win together wait out of reach for each other" {
                // A smallMelee beside two smallHealers: one 800-bank guard loses
                // it, two win. One healer no longer does: the melee's swing is
                // struck back by the guard's 90 (#624). The bank now buys a
                // guard that wins alone, so the room is the guard row's `Win`;
                // the two held were cast smaller.
                let raid = raiders @ healers 2

                let declared hostiles =
                    { declaredRaid hostiles with
                        Bank = bank 1500 1500
                    }

                // Both still at home, apart: neither has entered the fight.
                let colony at =
                    declared raid
                    |> withBodyAtHome (guard "g-1") { X = 25; Y = 30 }
                    |> withBodyAtHome (guard "g-2") at

                let holding = [ "g-1", Guard "W1N2"; "g-2", Guard "W1N2" ]

                let apart = heldBriefOf holding "W1N2" (colony atSpawn)

                Expect.equal
                    (apart.Stances |> Map.keys |> Set.ofSeq)
                    (set [ "g-1"; "g-2" ])
                    "both are held off the fight"

                let rally = apart.Stances |> Map.find "g-1"

                Expect.isTrue
                    (not (Set.isEmpty rally) && rally |> Set.forall (fun tile -> tile.Room = "W1N1"))
                    $"on the rally ground short of the room: {rally}"

                // #663 S5: the muster is a job ahead of the ring, priced by
                // the one rule the sweep's group musters by (`Facts.muster`):
                // the first come alone loses to the raid, so waiting costs
                // nothing it would keep.
                let muster = List.head apart.Jobs

                Expect.equal
                    (muster.Kind, muster.Tiles)
                    (JobKind.Muster, rally)
                    "its first job: the muster"

                Expect.isGreaterThan
                    muster.Value.Value
                    0.0
                    "the first come's loss over the wait, a tick"

                // Its light-tick rule: a mustering holder waits; only a new
                // armed body in reach forces a decide.
                let waiting = colony atSpawn

                Expect.equal
                    (Emitter.forces
                        waiting
                        (threatsOfHeld
                            waiting
                            (Atlas.ofView waiting)
                            (heldTaskFacts
                                waiting
                                (holding
                                 |> List.map (fun (name, task) -> name, taskId task)
                                 |> Map.ofList)))
                        (Map.ofList holding))
                    (Map.ofList [ "g-1", JobForce.Waits; "g-2", JobForce.Waits ])
                    "both wait"

                // The rule alone: a first come the raid beats loses its body,
                // and waiting costs nothing it would keep, however much the
                // room yields; a group of one waits for nobody.
                let one lands =
                    {
                        Facts.Fighter = Fighter.ofCreep (guard "g-1")
                        Facts.Lands = lands
                        Facts.Energy = 800.0
                    }

                Expect.equal
                    (Facts.muster (colony atSpawn) "W1N2" [ one 0; one 10 ])
                    (Some 80.0)
                    "its 800 energy over the ten ticks the second takes"

                Expect.isNone
                    (Facts.muster (colony atSpawn) "W1N2" [ one 0 ])
                    "alone, nobody to wait for"

                // The review of #663 S5: a lone holder waits only for a body
                // the facts say will land — one in an oven, or one an idle
                // spawn the bank funds now. With the bank empty no cast
                // begins, so nobody is coming and it does not wait for ever.
                // The bank drained, its spawn idle: no cast begins.
                let lone =
                    { declared raid with
                        Bank = bank 0 1500
                    }
                    |> withBodyAtHome (guard "g-1") { X = 25; Y = 30 }

                Expect.isEmpty
                    (heldBriefOf [ "g-1", Guard "W1N2" ] "W1N2" lone).Stances
                    "the bank empty: no cast is coming, nobody waits"

                // A second guard in the oven, ten ticks from standing: coming.
                let ovened =
                    { lone with
                        Casting =
                            [
                                {
                                    Name = "guard-oven"
                                    Body = Bodies.guardPattern.Block
                                }
                            ]
                        Spawns =
                            lone.Spawns
                            |> List.map (fun spawn ->
                                { spawn with
                                    IsSpawning = true
                                    Remaining = 10
                                })
                    }

                Expect.equal
                    ((heldBriefOf [ "g-1", Guard "W1N2" ] "W1N2" ovened).Stances
                     |> Map.keys
                     |> List.ofSeq)
                    [ "g-1" ]
                    "a guard in the oven lands: the lone holder waits for it"

                let decision =
                    decideFrom
                        (holding |> List.map (fun (name, task) -> name, taskId task) |> Map.ofList)
                        (colony atSpawn)

                Expect.equal
                    (Map.tryFind "g-1" decision.Assignments, Map.tryFind "g-2" decision.Assignments)
                    (Some(taskId (Guard "W1N2")), Some(taskId (Guard "W1N2")))
                    $"each keeps the Guard while it waits: {decision.Verdicts}"

                // Launched without state: one in the room is engaged, and never
                // pulled back out of reach, nor the other held.
                let engaged =
                    declared raid
                    |> withGuards [ guard "g-1", beside ]
                    |> withBodyAtHome (guard "g-2") atSpawn

                Expect.isEmpty
                    (heldBriefOf holding "W1N2" engaged).Stances
                    "a guard in reach of its raider is never pulled out of it"

                Expect.contains
                    (decideFrom
                        (holding |> List.map (fun (name, task) -> name, taskId task) |> Map.ofList)
                        engaged)
                        .Intents
                    (AttackCreep("g-1", "h-1"))
                    "and swings"

                let alone =
                    declared raiders
                    |> withGuards [ guard "g-1", beside ]
                    |> withBodyAtHome (guard "g-2") atSpawn

                Expect.isEmpty
                    (heldBriefOf holding "W1N2" alone).Stances
                    "a guard that wins alone goes in at once"

                let together =
                    let tiles = rally |> Set.toList |> List.map RoomPos.pos

                    declared raid
                    |> withBodyAtHome (guard "g-1") tiles.[0]
                    |> withBodyAtHome (guard "g-2") tiles.[1]

                Expect.isEmpty
                    (heldBriefOf holding "W1N2" together).Stances
                    "once both stand on the rally ground they go in together"

                // Moved by #663 S5 (one muster rule for #535 and the sweep's):
                // a file strung out short of the rally ground walks on to it
                // and waits there for its last (it went in as one file, #491's
                // squad rule): the ground lies on its walk in, so nothing is
                // pulled back, and the first come is not sent in alone.
                let walking =
                    declared raid
                    |> withBodyAtHome (guard "g-1") { X = 25; Y = 7 }
                    |> withBodyAtHome (guard "g-2") { X = 25; Y = 9 }

                Expect.equal
                    ((heldBriefOf holding "W1N2" walking).Stances |> Map.keys |> Set.ofSeq)
                    (set [ "g-1"; "g-2" ])
                    "holders strung out short of the rally ground walk on to it and wait there"

                // One on the ground and the other a step off it beside its
                // partner have both landed (the #609 capture's crowded ground).
                let beside =
                    let tiles = rally |> Set.toList |> List.map RoomPos.pos

                    declared raid
                    |> withBodyAtHome (guard "g-1") tiles.[0]
                    |> withBodyAtHome (guard "g-2") { tiles.[0] with Y = tiles.[0].Y + 2 }

                Expect.isEmpty
                    (heldBriefOf holding "W1N2" beside).Stances
                    "a holder beside its partner on the rally ground has landed with it"
            }

            // Live t974,024 (W18S25): Bang590 and Sky320 priced against the
            // residents together, and one ranger lent walked in alone.
            test
                "a chase two lent residents win together: the first lent waits out of reach for the second" {
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
                        (List.replicate 10 Move
                         @ List.replicate 7 RangedAttack
                         @ List.replicate 3 Heal)

                let mother (r1: string * Pos) =
                    { watchingBeyond [ harasser ] with
                        Bank = bank 5600 5600
                    }
                    |> fun colony ->
                        { colony with
                            Creeps =
                                (colony.Creeps |> List.filter (fun creep -> creep.Name <> "r-1"))
                                @ [ resident "r-1"; resident "r-2" ]
                        }
                    |> withOutpostLayer "W1N3" (fun layer ->
                        { layer with
                            CreepPositions = Map.empty
                        })
                    |> standingIn (fst r1) ("r-1", snd r1)
                    |> standingIn "W1N3" ("r-2", { X = 27; Y = 40 })

                let holding = [ "r-1", Guard "W1N4"; "r-2", Guard "W1N4" ]
                // The first lent is on its way, near the crossing, the second
                // still by the controller: neither has entered the chase.
                let lentIn = mother ("W1N3", { X = 25; Y = 10 })
                let brief = heldBriefOf holding "W1N4" lentIn

                Expect.equal
                    (brief.Duty, brief.Verdict.Value)
                    (Duty.Guard BriefKind.Chase, Answer.Win)
                    "the premise: a chase the residents win"

                Expect.equal
                    (brief.Capacity(threatsOf lentIn (Atlas.ofView lentIn))
                     |> Capacity.capOf CapScope.Fighters)
                    (Some 2)
                    "the call takes the two the verdict priced"

                Expect.equal
                    (brief.Stances
                     |> Map.tryFind "r-1"
                     |> Option.map (Set.map (fun tile -> tile.Room)))
                    (Some(set [ "W1N3" ]))
                    "the first lent is held back beside the crossing until the second stands with it"

                let decision =
                    decideFrom
                        (holding |> List.map (fun (name, task) -> name, taskId task) |> Map.ofList)
                        lentIn

                Expect.equal
                    (Map.tryFind "r-1" decision.Assignments)
                    (Some(taskId (Guard "W1N4")))
                    "and keeps the chase while it waits"

                Expect.isEmpty
                    (heldBriefOf holding "W1N4" (mother ("W1N3", { X = 26; Y = 40 }))).Stances
                    "and once the two stand together they go in"
            }
        ]

[<Tests>]
let pursuitTests =
    testList
        "a fight begun is fought to its target's death (#535)"
        [
            // Live 2026-10-06, W15S27: invader_W15S27_250 sat at 10 hits, one
            // MOVE alive, for ~250 ticks beside an idle guard and froze an
            // anchor: shot disarmed, it pooled no Guard.
            test "a guard whose invader is disarmed mid-fight keeps its Guard and kills it" {
                let invader =
                    { hostileIn
                          "W1N2"
                          { X = 25; Y = 44 }
                          (List.replicate 3 RangedAttack @ List.replicate 5 Move) with
                        Owner = "Invader"
                        Hits = 10
                    }

                // A room we hold the fight in but mine nothing in: only the
                // held Guard makes the cripple ours (`Threats.Engaged`).
                let unmined =
                    { declaredRaid [ invader ] with
                        Sources = []
                    }
                    |> withGuards [ guard "g-1", { X = 25; Y = 45 } ]

                let held = Map.ofList [ "g-1", taskId (Guard "W1N2") ]
                let result = decideFrom held unmined

                Expect.equal
                    (Map.tryFind "g-1" result.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the guard keeps the fight it began"

                Expect.contains
                    result.Intents
                    (AttackCreep("g-1", "h-1"))
                    "and swings at the cripple"

                Expect.notEqual
                    (Map.tryFind "g-1" (decideOn unmined).Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "where nothing of ours is mined, a body that held no fight begins none"

                let mined =
                    declaredRaid [ invader ] |> withGuards [ guard "g-1", { X = 25; Y = 45 } ]

                Expect.equal
                    (Map.tryFind "g-1" (decideOn mined).Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "where we mine, any guard takes the cripple: a trespass's kill"
            }

            // Virtual458 (Trepidimous) in W18S25: every ATTACK and all but one
            // MOVE shot off, 74 hits, limping away from the ranger holding it.
            test
                "Virtual458's shape, disarmed and limping off, is chased to death by the ranger holding it" {
                let virtual458 =
                    { hostileIn
                          "W1N2"
                          { X = 25; Y = 42 }
                          (List.replicate 17 Attack @ List.replicate 17 Move) with
                        Owner = "Trepidimous"
                        Hits = 74
                    }

                let colony =
                    declaredRaid [ virtual458 ] |> withGuards [ ranger "r-1", { X = 25; Y = 45 } ]

                let result = decideFrom (Map.ofList [ "r-1", taskId (Guard "W1N2") ]) colony

                Expect.equal
                    (Map.tryFind "r-1" result.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the ranger keeps it"

                Expect.contains result.Intents (RangedAttackCreep("r-1", "h-1")) "and shoots it"
            }

            test
                "a stronghold Invader under its own rampart holds no guard; the same Invader off it does" {
                let invaderAt (pos: Pos) =
                    { hostileIn "W1N2" pos smallMelee with
                        Owner = "Invader"
                    }

                // A rampart not ours (no hits projected for it) on (25,44).
                let withRampart (colony: ColonyView) =
                    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

                    { colony with
                        Spatial =
                            { colony.Spatial with
                                TargetKinds =
                                    Map.add
                                        "ramp-core"
                                        (Structure BuiltKind.Rampart)
                                        colony.Spatial.TargetKinds
                            }
                            |> withNeighbour
                                "W1N2"
                                { outpost with
                                    TargetPositions =
                                        Map.add
                                            "ramp-core"
                                            { X = 25; Y = 44 }
                                            outpost.TargetPositions
                                }
                    }

                let holds (pos: Pos) =
                    // A bank whose guard wins it: the fight is not a lost one.
                    let colony =
                        { declaredRaid [ invaderAt pos ] with
                            Bank = bank 1500 1500
                        }
                        |> withRampart
                        |> withGuards [ guard "g-1", { X = 25; Y = 46 } ]

                    let atlas = Atlas.ofView colony

                    let held = heldTaskFacts colony (Map.ofList [ "g-1", taskId (Guard "W1N2") ])

                    Threats.holdsFight
                        colony
                        atlas
                        (threatsOfHeld colony atlas held)
                        (guard "g-1")
                        (Guard "W1N2")

                Expect.isFalse
                    (holds { X = 25; Y = 44 })
                    "under the rampart: a strike lands on it, held for nothing"

                Expect.isTrue (holds { X = 25; Y = 45 }) "off it: struck, held"
            }

            test "a disarmed target that outruns its holder is let go" {
                // A neutral's raider shot down to four MOVE: it outruns a guard
                // left one MOVE, and walks at the pace of a whole one.
                let quick =
                    { hostileIn "W1N2" { X = 25; Y = 44 } ([ Attack ] @ List.replicate 4 Move) with
                        Owner = "giaco"
                        Hits = 400
                    }

                let lame = creepWith "g-1" 0 0 [ Tough; Attack; Attack; Attack; Heal; Move ]
                let held = Map.ofList [ "g-1", taskId (Guard "W1N2") ]

                let outrun =
                    decideFrom
                        held
                        (declaredRaid [ quick ] |> withGuards [ lame, { X = 25; Y = 45 } ])

                Expect.notEqual
                    (Map.tryFind "g-1" outrun.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the lame guard is let go"

                let kept =
                    decideFrom
                        held
                        (declaredRaid [ quick ] |> withGuards [ guard "g-1", { X = 25; Y = 45 } ])

                Expect.equal
                    (Map.tryFind "g-1" kept.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "and a whole guard, which it does not outrun, keeps it"
            }

            test "a fight against an armed raid the holder loses is not kept: it flees" {
                // An undeclared room pools no Guard for an armed raid; nothing
                // disarmed is pursued, so the holder out-damaged runs (#533).
                let colony =
                    raidedOutpost (raiders @ healers 1) |> withGuards [ guard "g-1", beside ]

                let result = decideFrom (Map.ofList [ "g-1", taskId (Guard "W1N2") ]) colony

                Expect.equal (Map.tryFind "g-1" result.Assignments) (Some(taskId Flee)) "it flees"
            }
        ]

/// W17S26 at t1,007,995 (#604): Trepidimous reserves our outpost, one
/// 18M17A of his stands guard and a 5W3M digs it, under the given hold.
let private contestedUnder control =
    let colony =
        declaredRaid
            [
                { hostileIn
                      "W1N2"
                      { X = 25; Y = 42 }
                      (List.replicate 18 Move @ List.replicate 17 Attack) with
                    Owner = "Trepidimous"
                }
                seatedDigger
            ]

    { colony with
        RoomControl = Map.add "W1N2" control colony.RoomControl
    }

[<Tests>]
let contestedOutpostTests =
    testList
        "an outpost a declared enemy reserves and works (#604)"
        [
            test
                "his guard and his digger are the outpost's Guard, and his hold is a Reserve to attack" {
                let pool = pooledOf (contestedUnder (enemyReservedRoom 1_900))

                Expect.isSome
                    (entryFor (Guard "W1N2") pool)
                    "the outpost's Guard is pooled against him"

                Expect.isSome
                    (entryFor (Reserve "ctl-out") pool)
                    "and the controller is ours to attack"

                Expect.isNone
                    (entryFor
                        (Reserve "ctl-out")
                        (pooledOf (contestedUnder (reservedRoom false 1_900))))
                    "a neutral's hold is refused the reserver, as ever (#333)"
            }
        ]
