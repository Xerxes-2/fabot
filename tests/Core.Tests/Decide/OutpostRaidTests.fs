/// An outpost with a hostile standing in it: the Guard that answers, and
/// the container that switches the room back.
module Fabot.Core.Tests.Decide.OutpostRaidTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.OutpostFixtures

[<Tests>]
let raidedOutpostTests =
    testList
        "an outpost with an armed hostile standing in it"
        [
            test "the crew flees and the garrison, which cannot, is matched to nothing at all" {
                // What a raid costs an outpost before the guard row answers it: the
                // haulers run, the anchor cannot and stays.
                //
                // Pairwise against the same fixture with no hostile in it.
                let assignmentsOf hostiles =
                    (decide (raidedOutpost hostiles) Map.empty Set.empty None).Assignments

                let quiet = assignmentsOf []
                let raided = assignmentsOf raiders

                Expect.equal
                    (Map.tryFind "a-out" quiet)
                    (Some(taskId (Harvest "src-out")))
                    "the premise: on a quiet tick the garrison digs the rock it stands on"

                Expect.isEmpty
                    (quiet |> Map.filter (fun _ tid -> tid = taskId Flee))
                    "and nobody runs from a room with nothing in it (ADR 0033)"

                Expect.equal
                    (Map.tryFind "h-out1" quiet, Map.tryFind "h-out2" quiet)
                    (Some(taskId (Withdraw("can-out", Energy))),
                     Some(taskId (Withdraw("can-out", Energy))))
                    "and the crew empties the container that Post feeds"

                Expect.equal
                    (Map.tryFind "h-out1" raided, Map.tryFind "h-out2" raided)
                    (Some(taskId Flee), Some(taskId Flee))
                    "both haulers drop that haul for Flee: the Safety tier outranks every other"

                // Neither Flee nor work: Flee is inapplicable to a work-heavy body and
                // every Seat of its rock (the row of field under it) lies inside the
                // Reach. This is the hole the guard row is cast into.
                Expect.equal
                    (Map.tryFind "a-out" raided)
                    None
                    "and the Anchor is matched to nothing at all: it neither runs nor digs"
            }

            test "the garrison's Harvest is released Threatened rather than simply lost" {
                // Read off the Verdicts with the quiet tick's assignment already held.
                // Every Seat of `src-out` is inside this raid's Reach, and the
                // container the haul reads is on one of them.
                let releasesOf hostiles =
                    let held =
                        Map.ofList
                            [
                                "a-out", taskId (Harvest "src-out")
                                "h-out1", taskId (Withdraw("can-out", Energy))
                            ]

                    (decideFrom held (raidedOutpost hostiles)).Verdicts
                    |> List.choose (function
                        | Verdict.Released(creep, task, reason) -> Some(creep, task, reason)
                        | _ -> None)

                Expect.isEmpty (releasesOf []) "the premise: a quiet tick releases nobody"

                Expect.equal
                    (releasesOf raiders)
                    [
                        "a-out",
                        taskId (Harvest "src-out"),
                        ReleaseReason.Rejected RejectReason.Threatened
                        "h-out1",
                        taskId (Withdraw("can-out", Energy)),
                        ReleaseReason.Rejected RejectReason.Threatened
                    ]
                    "the raid takes the rock's Seats and the container's ground, and says so"
            }
        ]

[<Tests>]
let guardTaskTests =
    testList
        "the Guard of a raided outpost"
        [
            test "the raid pools one Guard, keyed on the room and ranked with Flee" {
                // Keyed on the **room**, `guard:W1N2` and never the invader's id, so a
                // multi-creep raid pools one Task and not five. Pairwise against the
                // same geometry with nothing in it.
                let quiet = pooledOf (declaredRaid [])
                let raided = pooledOf (declaredRaid raiders)

                Expect.isNone
                    (entryFor (Guard "W1N2") quiet)
                    "the premise: a quiet outpost is no fight and pools none"

                Expect.equal
                    (raided
                     |> List.map (fun entry -> taskId entry.Task)
                     |> List.filter (fun id -> id.StartsWith "guard:")
                     |> List.distinct)
                    [ "guard:W1N2" ]
                    "the raid pools exactly one, under the room's own name"

                // A raid's Guard is two rungs over the Safety tier (#528), over a
                // trespass's kill; Flee, which admits no fighter, on it.
                Expect.equal
                    (raided |> entryFor (Guard "W1N2") |> Option.map (fun e -> e.Priority))
                    (raided
                     |> entryFor Flee
                     |> Option.map (fun e -> e.Priority + rankOfRung TwoRungsUp))
                    "and it ranks two rungs over Flee"
            }

            test "the cap is the room's own quota, one guard and then two" {
                // The Fighter share is `guardsWanted` for that room. Read pairwise off
                // the raid alone: a lone `smallMelee` against the same raid carrying
                // two healers, whose 120 out-heals the 90 one `guardPattern` block
                // deals (#272), one guard of ours standing in both readings.
                let capOf colony =
                    pooledOf colony
                    |> entryFor (Guard "W1N2")
                    |> Option.map (fun entry -> entry.Capacity |> Capacity.capOf CapScope.Fighters)

                Expect.equal
                    (capOf (declaredRaid raiders |> withGuards [ guard "g-1", beside ]))
                    (Some(Some 1))
                    "a raid that heals nothing admits one Fighter"

                Expect.equal
                    (capOf (
                        declaredRaid (raiders @ healers 2) |> withGuards [ guard "g-1", beside ]
                    ))
                    (Some(Some 2))
                    "and a raid healing 120 against the 90 one guard block deals admits the second"
            }

            test "a Fighter standing in the raided room is matched to the Guard" {
                // Pairwise against the same guard in the same room with nothing to
                // fight, which is matched to nothing at all.
                let raided =
                    decide
                        (declaredRaid raiders |> withGuards [ guard "g-1", beside ])
                        Map.empty
                        Set.empty
                        None

                let quiet =
                    decide
                        (declaredRaid [] |> withGuards [ guard "g-1", beside ])
                        Map.empty
                        Set.empty
                        None

                Expect.equal
                    (Map.tryFind "g-1" quiet.Assignments)
                    None
                    "the premise: with no raid there is no Guard, and no other Task takes a body with no Work and no Carry"

                Expect.equal
                    (Map.tryFind "g-1" raided.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the raid gives it the one Task it is for"

                Expect.equal
                    (Map.tryFind "h-out1" raided.Assignments)
                    (Some(taskId Flee))
                    "and the crew still runs: the guard answers the raid, it does not cancel it"

                Expect.contains
                    (sayIntents raided.Intents)
                    ("g-1", "⚔️")
                    "the bubble carries the Guard's own glyph"
            }

            test "the Emitter does not let self-heal suppress a melee swing" {
                // Screeps resolves `attack` and `heal` in one fixed intent
                // pipeline, with heal to the right: scheduling both returns OK
                // for both calls but executes only the heal. At the decision
                // seam, therefore, the guard swings while it is in range and
                // self-heals only while there is no swing to suppress.
                let intentsFrom tile =
                    (decide
                        (declaredRaid raiders
                         |> withGuards
                             [
                                 { guard "g-1" with
                                     Hits = { Hits = 999; HitsMax = 1000 }
                                 },
                                 tile
                             ])
                        Map.empty
                        Set.empty
                        None)

                let inSwing = intentsFrom beside

                // Off the raid's ground entirely: the y = 48 row the flee cases run
                // onto, outside the reach and four tiles from the invader.
                let walking = intentsFrom { X = 28; Y = 48 }

                for decision in [ inSwing; walking ] do
                    Expect.isOk
                        (Fabot.Core.IntentPlan.create decision.Intents)
                        "the complete guard turn is executable, including movement and speech"

                Expect.equal
                    (attacksOf inSwing.Intents)
                    [ "g-1", "h-1" ]
                    "standing on the invader's ring, the guard swings at it"

                Expect.isEmpty
                    (healsOf inSwing.Intents)
                    "a self-heal would suppress the melee swing in the engine"

                Expect.isEmpty
                    (attacksOf walking.Intents)
                    "four tiles away it swings at nothing: the act is not issued out of range"

                Expect.equal
                    (healsOf walking.Intents)
                    [ "g-1", "g-1" ]
                    "with no swing to suppress, the guard heals while it walks"

                Expect.equal
                    (Map.tryFind "g-1" walking.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the body still holds the Task it is walking to"

                Expect.isNonEmpty
                    (moveIntentsFor "g-1" walking.Intents)
                    "and the mover walks it there: the Emitter issues no movement of its own"
            }

            test
                "with two Threats the one beside the Post is the target, and with no Post the nearest" {
                // The Threat nearest a Post of that room, ties by id: the engine's own
                // `findAttack.js` chases the closest hostile by path.
                //
                // Two invaders, both within range 1 of the guard at (26,43): `inv-2` at
                // (25,42) a tile from the Post at (25,41), `inv-1` at (27,44) three
                // tiles from it, and the ids run the other way.
                let invader id pos =
                    { hostileIn "W1N2" pos smallMelee with
                        Id = id
                    }

                let raid =
                    [ invader "inv-2" { X = 25; Y = 42 }; invader "inv-1" { X = 27; Y = 44 } ]

                let attacksFrom tile colony =
                    (decide (colony |> withGuards [ guard "g-1", tile ]) Map.empty Set.empty None)
                        .Intents
                    |> attacksOf

                let posted = declaredRaid raid
                // The same room the tick before its container stands: no Post, so
                // nothing to stand in front of.
                let postless = beforeHaulContainer posted

                Expect.equal
                    (attacksFrom { X = 26; Y = 43 } posted)
                    [ "g-1", "inv-2" ]
                    "the Post decides it, over an id order that says otherwise"

                Expect.equal
                    (attacksFrom { X = 26; Y = 43 } postless)
                    [ "g-1", "inv-1" ]
                    "with no Post the guard's own tile decides, and equal distances tie by id"

                Expect.equal
                    (attacksFrom beside postless)
                    [ "g-1", "inv-2" ]
                    "which is a distance and not the id: one tile away wins over three"

                // **The range gate is on the candidates, not on the pick.** (28,44) is
                // a ring tile of `inv-1` and three from `inv-2`: nearest-the-Post then
                // filtered by range would swing at nothing for as long as it stood.
                Expect.equal
                    (attacksFrom { X = 28; Y = 44 } posted)
                    [ "g-1", "inv-1" ]
                    "out of reach of the Post's own invader, the guard hits the one it can reach"
            }

            test "the cap refuses the second Fighter while the room wants one" {
                // The room wants one guard this tick, so the second body in the same
                // ring is refused by the number the Planner set, named so a verbose
                // reading tells "the room is full" from "this body cannot fight".
                let colony =
                    declaredRaid raiders
                    |> withGuards [ guard "g-1", beside; guard "g-2", besideToo ]

                let decision = decide colony Map.empty (Set.singleton "g-2") None

                Expect.equal
                    (Map.tryFind "g-1" decision.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the first Fighter takes the fight"

                Expect.notEqual
                    (Map.tryFind "g-2" decision.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "and the second does not hold it beside him"

                Expect.contains
                    (rejectionsFor "g-2" decision.Verdicts |> Option.defaultValue [])
                    (taskId (Guard "W1N2"), RejectReason.CapacityFull)
                    "the cap is what refused it, and the scoring says so"
            }

            test "the escalation does not evict the reinforcement it just bought" {
                // #272: the count is a function of the **raid**, so it cannot retract
                // on the arrival of the body it asked for. Priced against the guards
                // standing there, the same two-healer raid admitted two while one
                // guard stood and one the tick the second arrived, and the arriving
                // body was `CapacityFull`-evicted onto a Flee whose safe set is this
                // same room: 750 energy of body that never issued an `AttackCreep`.
                // The damage is one block of the row's own body, a constant, so this
                // holds at whatever the fixture banks.
                let capOf colony =
                    pooledOf colony
                    |> entryFor (Guard "W1N2")
                    |> Option.map (fun entry -> entry.Capacity |> Capacity.capOf CapScope.Fighters)

                let raid = declaredRaid (raiders @ healers 2)

                Expect.equal
                    (capOf (raid |> withGuards [ guard "g-1", beside ]))
                    (Some(Some 2))
                    "the premise: 120 healed against the 90 one guard block deals admits two"

                Expect.equal
                    (capOf (raid |> withGuards [ guard "g-1", beside; guard "g-2", besideToo ]))
                    (Some(Some 2))
                    "and the second guard standing in the ring does not close the cap behind it"

                let decision =
                    decide
                        (raid |> withGuards [ guard "g-1", beside; guard "g-2", besideToo ])
                        Map.empty
                        (Set.singleton "g-2")
                        None

                Expect.equal
                    (Map.tryFind "g-1" decision.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the first Fighter keeps the fight"

                Expect.equal
                    (Map.tryFind "g-2" decision.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "and the second holds it beside him rather than fleeing the room it is standing in"

                Expect.isEmpty
                    (rejectionsFor "g-2" decision.Verdicts
                     |> Option.defaultValue []
                     |> List.filter (fun (task, reason) ->
                         task = taskId (Guard "W1N2") && reason = RejectReason.CapacityFull))
                    "with nothing left to reject it for: the cap is the raid's number and it did not move"
            }

            test "a hauler and a worker are refused the fight they are standing in" {
                // The body gate shuts every body with no ATTACK part out before the
                // number is counted. Both classes, because both stand in this room: a
                // hauler is a `Light`, the Anchor a `Heavy`.
                let colony = declaredRaid raiders |> withGuards [ guard "g-1", beside ]

                let decision = decide colony Map.empty (Set.ofList [ "h-out1"; "a-out" ]) None

                Expect.contains
                    (rejectionsFor "h-out1" decision.Verdicts |> Option.defaultValue [])
                    (taskId (Guard "W1N2"), RejectReason.Inapplicable)
                    "no ATTACK part, no fight"

                Expect.contains
                    (rejectionsFor "a-out" decision.Verdicts |> Option.defaultValue [])
                    (taskId (Guard "W1N2"), RejectReason.Inapplicable)
                    "and the work-heavy body standing on the Post is refused it too, Work being no weapon"

                Expect.equal
                    (Map.tryFind "g-1" decision.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "while the one body that carries one holds it"
            }

            test "the guard the row cast at home is priced across the Seam and walks it" {
                // The row hires at the spawn, so the guard begins a room and a Seam
                // away from the ring it was bought for. A Work Area filed under the
                // raided room and priced over the creep's own room alone would reject
                // this body `Unreachable` on every tick, and its non-decaying `Living`
                // would suppress the next cast: 750 energy standing at the oven.
                //
                // Pairwise against the same body inside the room.
                let across =
                    decide
                        (declaredRaid raiders |> withBodyAtHome (guard "g-home") atSpawn)
                        Map.empty
                        (Set.singleton "g-home")
                        None

                let inside =
                    decide
                        (declaredRaid raiders |> withGuards [ guard "g-home", beside ])
                        Map.empty
                        Set.empty
                        None

                Expect.equal
                    (Map.tryFind "g-home" inside.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the premise: standing on the ring, the body holds the fight"

                Expect.equal
                    (Map.tryFind "g-home" across.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "and a border away it holds the same fight — the walk is a price, not a refusal"

                Expect.isEmpty
                    (rejectionsFor "g-home" across.Verdicts
                     |> Option.defaultValue []
                     |> List.filter (fun (task, reason) ->
                         task = taskId (Guard "W1N2") && reason = RejectReason.Unreachable))
                    "and it is not rejected Unreachable: the ring across a Seam is priced over the Seam"

                // The other gate that could refuse a body a border away: the cross-room
                // threat reading (#147) sits *beneath* the Safety tier, so the ring the
                // raid laid is not read as ground the raid took. Judged by its ground
                // every Guard would be threatened on every tick one existed.
                Expect.isEmpty
                    (rejectionsFor "g-home" across.Verdicts
                     |> Option.defaultValue []
                     |> List.filter (fun (task, reason) ->
                         task = taskId (Guard "W1N2") && reason = RejectReason.Threatened))
                    "nor Threatened: its own ring is not ground the raid took from it"

                Expect.equal
                    (moveIntentsFor "g-home" across.Intents)
                    [ MoveCreep("g-home", Direction.Top) ]
                    "the mover walks it there, up the corridor toward the crossing into W1N2"
            }

            test "the room clears and the Guard goes with it: the holder is released TaskGone" {
                // The Task is a per-tick fact read off vision, as the row's quota is:
                // the holder is released under the reason that says the work is gone.
                let held = Map.ofList [ "g-1", taskId (Guard "W1N2") ]

                let releasesOf hostiles =
                    (decide
                        (declaredRaid hostiles |> withGuards [ guard "g-1", beside ])
                        held
                        Set.empty
                        None)
                        .Verdicts
                    |> List.choose (function
                        | Verdict.Released(creep, task, reason) -> Some(creep, task, reason)
                        | _ -> None)

                Expect.isEmpty
                    (releasesOf raiders)
                    "the premise: while the invader stands, the guard keeps the fight"

                Expect.equal
                    (releasesOf [])
                    [ "g-1", taskId (Guard "W1N2"), ReleaseReason.TaskGone ]
                    "and the tick it is gone the Task is gone, not merely threatened"
            }

            test
                "every other Task in the raided room reads Threatened, and the Guard is the one that does not" {
                // Read off one verbose scoring, the one place the whole pool is judged
                // for one body.
                //
                // This raid's Reach covers the rock's three Seats and the container's
                // ground. A Guard's Work Area is *made* of Reach tiles, so under the
                // gate unamended it would be inapplicable to everyone; the subtraction
                // is skipped for the Safety tier, both of whose areas are derived off
                // the tick's `Threats` rather than a target's surroundings.
                //
                // Flee is the tier's other half and is disjoint by body class, so this
                // one body sees one Task rejected for every gate and exactly one left.
                let colony = declaredRaid raiders |> withGuards [ guard "g-1", beside ]
                let decision = decide colony Map.empty (Set.singleton "g-1") None
                let rejections = rejectionsFor "g-1" decision.Verdicts |> Option.defaultValue []

                Expect.contains
                    rejections
                    (taskId (Harvest "src-out"), RejectReason.Threatened)
                    "the rock's every Seat is in the Reach, so its Harvest is gone for this body"

                Expect.contains
                    rejections
                    (taskId (Withdraw("can-out", Energy)), RejectReason.Threatened)
                    "and so is the ground the container is drawn from"

                Expect.contains
                    rejections
                    (taskId Flee, RejectReason.Inapplicable)
                    "and the tier's other Task is refused the body, not the ground: a Fighter does not run"

                Expect.isEmpty
                    (rejections |> List.filter (fun (task, _) -> task = taskId (Guard "W1N2")))
                    "the Guard is on no rejected row at all: the tier's area keeps its Reach tiles"

                Expect.equal
                    (Map.tryFind "g-1" decision.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "so the one body standing in a room where nothing else can be worked holds the fight"
            }
        ]

/// The raided outpost as the raid leaves it: **dark**, nothing of ours
/// standing in it, no control entry, and the last look's armed threat
/// remembered (#366). That is the live shape: the anchor, the hauler and
/// the reserver are the vision, so the tick they die all four go at once.
/// The room's declaration, its rock and its ground stay where they were.
let private blinded (colony: ColonyView) =
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        RoomControl = Map.remove "W1N2" colony.RoomControl
        Creeps =
            colony.Creeps
            |> List.filter (fun creep -> not (Map.containsKey creep.Name outpost.CreepPositions))
        Spatial =
            colony.Spatial
            |> withNeighbour
                "W1N2"
                { outpost with
                    CreepPositions = Map.empty
                }
    }

/// The same room with the last look's conclusion on the view, which is what
/// `Observe.standDown` hands `ColonyView.ofWorld` on those ticks.
let private remembering (colony: ColonyView) =
    { colony with
        ThreatenedOutposts = Set.singleton "W1N2"
    }

[<Tests>]
let idleGuardTests =
    testList
        "an idle guard away from home"
        [
            test "walks home, where every guarded room is within the hop budget" {
                // #416: an idle guard left in an outpost cannot price a Guard four
                // crossings out; from home it can.
                let guard = creepWith "g" 0 0 Bodies.guardPattern.Block

                let colony =
                    { (northBorderColony { X = 10; Y = 38 } |> withNorthOutpost None) with
                        Creeps = [ guard ]
                    }
                    |> fun colony ->
                        { colony with
                            Spatial =
                                colony.Spatial
                                |> withNeighbour
                                    "W1N2"
                                    { SpatialInfo.layerOf colony.Spatial "W1N2" with
                                        CreepPositions =
                                            Map.ofList [ guard.Name, { X = 10; Y = 46 } ]
                                    }
                        }

                Expect.contains
                    (decideOn colony).Intents
                    (MoveCreep(guard.Name, Bottom))
                    "with no Task it steps toward the border home lies behind"
            }
        ]

[<Tests>]
let blindGuardTests =
    testList
        "the Guard of an outpost the raid has gone dark in"
        [
            test "the remembered raid pools its Guard, capped at one" {
                // #366: "vision in a guarded outpost is the guard" is circular while
                // that guard is still in the oven; the vision in an *unguarded*
                // outpost is what the raid kills. Live W11S28 went dark 70 ticks after
                // the guard was cast and the Task vanished under the body walking to it.
                //
                // Pairwise on the memory and nothing else.
                let dark = blinded (declaredRaid [])
                let remembered = remembering dark

                Expect.isNone
                    (pooledOf dark |> entryFor (Guard "W1N2"))
                    "the premise, and the bug: blind and remembering nothing, no Guard is pooled at all"

                Expect.isSome
                    (pooledOf remembered |> entryFor (Guard "W1N2"))
                    "and with the armed Threat of the last look remembered, the fight is pooled again"

                Expect.equal
                    (pooledOf remembered
                     |> entryFor (Guard "W1N2")
                     |> Option.map (fun entry -> entry.Capacity |> Capacity.capOf CapScope.Fighters))
                    (Some(Some 1))
                    "at one body: the two-guard clause prices a raid, and there is no raid on the view to price"
            }

            test "the guard already paid for is walked into the room it cannot see" {
                // The cost: a 15-ATTACK-part body `idle (none-applicable)` at home
                // while a 2-ATTACK-part invader keeps the outpost. `Threats.ringIn` is
                // empty for a room no Threat stands in, so the fallback is the walkable
                // ring of the **declared** source tiles (`Atlas.sourceRingIn`), where
                // our anchors stand and therefore where the hunting is.
                //
                // Pairwise against the same blind room with nothing remembered.
                let decideWith colony =
                    decide
                        (colony |> withBodyAtHome (guard "g-home") atSpawn)
                        Map.empty
                        (Set.singleton "g-home")
                        None

                let forgotten = decideWith (blinded (declaredRaid []))
                let remembered = decideWith (remembering (blinded (declaredRaid [])))

                Expect.equal
                    (Map.tryFind "g-home" forgotten.Assignments)
                    None
                    "the premise: with the raid forgotten the body is matched to nothing at all"

                Expect.equal
                    (Map.tryFind "g-home" remembered.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "and with it remembered the body holds the fight it was bought for"

                Expect.equal
                    (moveIntentsFor "g-home" remembered.Intents)
                    [ MoveCreep("g-home", Direction.Top) ]
                    "the mover walks it up the corridor toward the crossing, as it does for a seen raid"

                Expect.isEmpty
                    (attacksOf remembered.Intents)
                    "and it swings at nothing: a guard with no visible target issues no attack"

                Expect.isEmpty
                    (rejectionsFor "g-home" remembered.Verdicts
                     |> Option.defaultValue []
                     |> List.filter (fun (task, _) -> task = taskId (Guard "W1N2")))
                    "on no rejected row: neither unreachable nor inapplicable, which an empty ring made it"
            }

            test
                "the fallback ground is the declared source ring, and arrival hands it back to the Reach" {
                // The geometry, so the case above cannot be green on a walk to nowhere:
                // `Outpost.Sources` places `src-out` at (25,40) without vision, and the
                // ring is the y = 41 row of this fixture's field, the container's own
                // tile included, a container being no obstacle.
                //
                // The tick the guard arrives the room is lit and `Threats.ringIn`
                // answers, so the ordinary ring takes over on the same tick.
                let dark = remembering (blinded (declaredRaid []))
                let atlas = Atlas.ofView dark

                Expect.equal
                    (Atlas.sourceRingIn atlas "W1N2")
                    ([ { X = 24; Y = 41 }; { X = 25; Y = 41 }; { X = 26; Y = 41 } ]
                     |> List.map (RoomPos.at "W1N2")
                     |> Set.ofList)
                    "the rock's three walkable neighbours, standing in the declaration and not in vision"

                let seen = declaredRaid raiders
                let ring = Threats.ringIn (threatsOf seen (Atlas.ofView seen)) "W1N2"

                Expect.isNonEmpty
                    ring
                    "the tick vision answers, the Threat's own ring exists and is what the Task is worked from"

                Expect.isFalse
                    (Set.isSubset ring (Atlas.sourceRingIn atlas "W1N2"))
                    "and it is a different set: the fallback is the geometry, the ring is the fight"
            }

            test "a guard standing in the dark room keeps the Task, and vision clears it" {
                // The arrival tick from the body's side (#366): a tick **with** vision
                // decides the room either way, and the tick vision shows the room
                // clear the fold drops the memory, which keeps this a memory and not
                // a second stand-down.
                let standing =
                    remembering (blinded (declaredRaid []))
                    |> withGuards [ guard "g-1", { X = 24; Y = 41 } ]

                Expect.equal
                    (Map.tryFind "g-1" (decide standing Map.empty Set.empty None).Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "on a declared source's ring in a room it cannot see past, the body holds the fight"

                // The same tick with the room lit and empty: the latch is still on the
                // view, because the fold that clears it runs on the observation.
                let lit =
                    { standing with
                        RoomControl =
                            standing.RoomControl
                            |> Map.add
                                "W1N2"
                                {
                                    Owner = Ownership.Unowned
                                    Reservation = None
                                    SafeMode = false
                                    SafeModeCooldownUntil = 0
                                    Sign = None
                                }
                    }

                Expect.isNone
                    (pooledOf lit |> entryFor (Guard "W1N2"))
                    "and the tick vision answers for the room, the clear look wins over the memory"
            }
        ]

/// The two-room shape #147 was filed on: a body of ours **at home** and a
/// Task whose ground is a raided room across the Seam. The reading below
/// must sit beneath the Safety tier; the guard half of that ordering is
/// pinned in `guardTaskTests` above, read for `Threatened` beside
/// `Unreachable`.
[<Tests>]
let crossSeamThreatTests =
    testList
        "a Task across the Seam in a raided room"
        [
            test "the home worker is not sent into a raid its own crew is running out of" {
                // **#147, reproduced and fixed.** `threatened` read the creep-relative
                // Work Area, which is empty across a border by construction, and an
                // empty area is not "threatened" but unplaceable. So on the tick this
                // outpost's crew was fleeing, a worker at home was matched to that rock
                // and walked toward the invader: a wasted crossing ending in
                // `NoneApplicable`.
                //
                // Pairwise on the raid and on nothing else.
                let atHome hostiles =
                    decide
                        (declaredRaid hostiles |> withBodyAtHome (worker "w-home" 0 50) atSpawn)
                        Map.empty
                        Set.empty
                        None

                let quiet = atHome []
                let raided = atHome raiders

                Expect.equal
                    (Map.tryFind "w-home" quiet.Assignments)
                    (Some(taskId (Withdraw("can-out", Energy))))
                    "the premise: with the room quiet the body is offered the outpost's work and crosses for it"

                Expect.equal
                    (moveIntentsFor "w-home" quiet.Intents)
                    [ MoveCreep("w-home", Direction.Top) ]
                    "the premise is tight: that is a walk up the corridor toward the crossing"

                Expect.equal
                    (Map.tryFind "w-home" raided.Assignments)
                    None
                    "and under the raid the same Task is gone for it too: the ground is the target room's"

                // #514: the raid next door lays the home's shelter, and the
                // spawn's doorstep is no part of it, so the body steps one tile
                // up onto it and stops there.
                Expect.equal
                    (moveIntentsFor "w-home" raided.Intents)
                    [ MoveCreep("w-home", Direction.Top) ]
                    "it steps off the spawn's doorstep onto the shelter"

                let settled =
                    decide
                        (declaredRaid raiders
                         |> withBodyAtHome (worker "w-home" 0 50) { atSpawn with Y = atSpawn.Y - 1 })
                        Map.empty
                        Set.empty
                        None

                Expect.isEmpty
                    (moveIntentsFor "w-home" settled.Intents)
                    "and nothing walks it on across the Seam"
            }
        ]

[<Tests>]
let containerSwitchTests =
    testList
        "the container is the switch"
        [
            // Read against the fleet, one body at a time: a colony exactly at its
            // target casts nothing and the same colony one body short casts one,
            // so a target that moved by n cannot hide inside a spawn's
            // one-cast-a-tick limit.
            let casts colony fleet =
                spawnIntents
                    (decide { colony with Creeps = fleet } Map.empty Set.empty None).Intents

            let short fleet =
                List.truncate (List.length fleet - 1) fleet

            test "an outpost rock with nothing built on it moves no row of the target" {
                // The room is projected, held by us and its rock is pooled for Harvest,
                // and still the colony hires exactly the fleet it hired without it.
                //
                // Pairwise: the two colonies differ in the outpost rock and nothing else.
                Expect.isEmpty
                    (casts switchHome switchHomeFleet)
                    "the premise: six is the home room's whole target"

                Expect.hasLength
                    (casts switchHome (short switchHomeFleet))
                    1
                    "the premise is tight: one body short and the colony casts"

                Expect.equal
                    (quotaOf switchUnposted)
                    (quotaOf switchHome)
                    "the unposted rock hires no haul"

                Expect.isEmpty
                    (casts switchUnposted switchHomeFleet)
                    "and no Anchor and no worker either: the same six are the whole target"
            }

            test "the container standing is one Anchor, its own haul and its income share" {
                // One tick's difference, a container on the rock's one Seat, and the
                // colony hires six more bodies: the Anchor, the one hauler its round
                // trip adds to the rounded-once pool, and the four workers the rock's
                // output feeds once those rows are amortized.
                Expect.isEmpty
                    (casts switchPosted (switchHomeFleet @ switchOutpostRows))
                    "posted, the target is the home fleet plus the outpost's own rows"

                Expect.hasLength
                    (casts switchPosted (short (switchHomeFleet @ switchOutpostRows)))
                    1
                    "and it is tight: one body short and the colony casts"

                Expect.equal
                    (quotaOf switchPosted - quotaOf switchUnposted)
                    1
                    "one of the six is what the container's own haul adds to the pool"
            }

            test "the Anchor the container hires is one, from the row the home Posts hire from" {
                // The outpost's Anchor is on the *same* row as the home room's, so the
                // proof is a swap at a fixed headcount: one body short the colony casts
                // a worker while both Anchors stand, and the same eleven bodies with
                // the outpost's Anchor spelled as a worker cast an Anchor instead.
                let shortFleet = short (switchHomeFleet @ switchOutpostRows)

                let swapped =
                    shortFleet
                    |> List.map (fun creep ->
                        if creep.Name = "a-out" then worker "w9" 0 50 else creep)

                match casts switchPosted shortFleet with
                | [ (_, _, name) ] ->
                    Expect.stringStarts
                        name
                        "worker-"
                        "the premise: with both Anchors it is a worker"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"

                match casts switchPosted swapped with
                | [ (_, _, name) ] -> Expect.stringStarts name "anchor-" "the gap is an Anchor gap"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "a standing outpost container adds no Task; stocked it adds its own Withdraw" {
                // A rock is pooled for Harvest whether or not it is posted, so the
                // container standing adds no Task by standing. Stocked it adds a
                // Withdraw on itself and never a Refill target: an outpost's container
                // is no upgrade buffer of a controller a room away (the join is pinned
                // by `roomLayerTests`, on a fixture with a controller to be wrong about).
                //
                // A container is a repairable kind, so under half its max
                // `hungryStructures` pools a cross-room `Repair` beside this Withdraw;
                // `isHungry` judges every structure against its own kind's whole line.
                Expect.equal
                    (planTasksOn switchPosted noThreats)
                    (planTasksOn switchUnposted noThreats)
                    "an empty container standing changes no Task in the pool"

                let stocked =
                    { switchPosted with
                        Spatial =
                            { switchPosted.Spatial with
                                Stores = Map.add "can-out" 500 switchPosted.Spatial.Stores
                            }
                    }

                Expect.equal
                    (List.except
                        (planTasksOn switchPosted noThreats)
                        (planTasksOn stocked noThreats))
                    [ Withdraw("can-out", Energy) ]
                    "and stocked it adds exactly one Task, the Withdraw of its own store"

                let decayed =
                    { switchPosted with
                        Spatial =
                            { switchPosted.Spatial with
                                Hits =
                                    Map.add
                                        "can-out"
                                        { Hits = 1000; HitsMax = 2500 }
                                        switchPosted.Spatial.Hits
                            }
                    }

                Expect.equal
                    (List.except
                        (planTasksOn switchPosted noThreats)
                        (planTasksOn decayed noThreats))
                    [ Repair "can-out" ]
                    "and decayed it adds exactly one more, its own Repair across the Seam"
            }
        ]

/// The outpost's field run out to its west border (#450): the y 41..48 rows
/// laid from x = 0, exit tiles and all, so a hold's ground can be read
/// against tiles the terrain would let a body stand on.
let private westField (colony: ColonyView) =
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Spatial =
            colony.Spatial
            |> withNeighbour
                "W1N2"
                { outpost with
                    Terrain =
                        TerrainGrid.ofList
                            [
                                for x in 0..30 do
                                    for y in 41..48 -> { X = x; Y = y }, Plain
                            ]
                }
    }

/// The west side's exit tiles beside that field.
let private westRun = [ for y in 41..48 -> { X = 0; Y = y } ]

/// The view of a tick the raid has just left by the west exit: the world's
/// hold on that run, as `ColonyView.ofWorld` hands it on.
let private heldWest (colony: ColonyView) =
    { colony with
        ExitHolds =
            Map.ofList
                [
                    "W1N2",
                    {
                        Run = westRun
                        Exit = { X = 0; Y = 44 }
                        Until = colony.Time + 50
                    }
                ]
    }

/// One room's ring, W1N2's border walled but for a west exit run of
/// y 20..24, as the world reads it off the terrain.
let private westBorder =
    plainRing
    |> Map.map (fun tile terrain ->
        if tile.X = 0 && tile.Y >= 20 && tile.Y <= 24 then
            terrain
        else
            Wall)

/// W17S26's west border as #561 met it: an exit run down x = 0 past y 6,
/// walled elsewhere.
let private w17s26Border =
    plainRing
    |> Map.map (fun tile terrain ->
        if tile.X = 0 && tile.Y >= 2 && tile.Y <= 9 then
            terrain
        else
            Wall)

/// The corner the north and west borders meet at, walled but for a west run
/// of y 2..6 and a north run of x 1..5: (0,1) is wall, so from (1,1) the
/// nearest exit is (1,0) on the north side, one step, and the west run's
/// nearest is (0,2), a diagonal.
let private cornerBorder =
    plainRing
    |> Map.map (fun tile terrain ->
        if
            (tile.X = 0 && tile.Y >= 2 && tile.Y <= 6)
            || (tile.Y = 0 && tile.X >= 1 && tile.X <= 5)
        then
            terrain
        else
            Wall)

/// W1N2 seen at `time` with these hostiles in it, over this border.
let private watchedOn border time hostiles : World =
    { World.empty with
        Time = time
        Rooms =
            Map.ofList
                [
                    "W1N2",
                    { RoomFacts.empty with
                        Border = border
                        Hostiles = hostiles
                    }
                ]
        Sightings =
            Map.ofList
                [
                    "W1N2",
                    {
                        Tick = time
                        Targets = lazy Set.empty
                        Rival = None
                    }
                ]
    }

/// W1N2 seen at `time` over the west border.
let private watched time hostiles = watchedOn westBorder time hostiles

/// The same room at `time` with nothing of ours to see it by.
let private dark time : World =
    { watched time [] with
        Sightings = Map.empty
    }

let private kiterAt pos ttl =
    { hostileIn "W1N2" pos [ RangedAttack; Move ] with
        TicksToLive = ttl
    }

/// The same world with these creeps' tombstones lying in W1N2.
let private withFallen (fallen: string list) (world: World) : World =
    { world with
        Rooms =
            world.Rooms
            |> Map.change "W1N2" (Option.map (fun facts -> { facts with Fallen = fallen }))
    }

/// The same world with W2N2, the room across W1N2's west border, seen too,
/// these hostiles standing in it.
let private withWestSeen hostiles (world: World) : World =
    { world with
        Rooms =
            world.Rooms
            |> Map.add
                "W2N2"
                { RoomFacts.empty with
                    Hostiles = hostiles
                }
        Sightings =
            world.Sightings
            |> Map.add
                "W2N2"
                {
                    Tick = world.Time
                    Targets = lazy Set.empty
                    Rival = None
                }
    }

let private watchOf (world: World) = Map.tryFind "W1N2" world.ExitWatches

/// The watch carried through a run of ticks, from none.
let private watchThrough (worlds: World list) : World =
    worlds
    |> List.fold
        (fun (previous: World) world -> World.watchExits Tuning.defaults previous.ExitWatches world)
        World.empty

/// The hold the watch has standing, if any.
let private holdOf (world: World) =
    match watchOf world with
    | Some(ExitWatch.Held hold) -> Some hold
    | _ -> None

/// The raid memory carried through a run of ticks, from none.
let private recallThrough (worlds: World list) : World =
    worlds
    |> List.fold
        (fun (previous: World) world -> World.recallRaiders previous.Raiders world)
        World.empty

/// The ids W1N2's raid memory holds, and the tick it was seen.
let private recalledIn (world: World) =
    Map.tryFind "W1N2" world.Raiders
    |> Option.map (fun seen -> seen.Raid |> List.map (fun h -> h.Id), seen.Tick)

[<Tests>]
let raidMemoryTests =
    testList
        "a raider stays its room's raid until seen gone (#569)"
        [
            test "a raider seen is remembered through the blind ticks after" {
                let raider = kiterAt { X = 25; Y = 25 } 1500

                Expect.equal
                    (recallThrough [ watched 100 [ raider ]; dark 101; dark 108 ] |> recalledIn)
                    (Some([ "h-1" ], 100))
                    "dark, the room holds the raid as last seen"
            }

            test "a look that finds the room empty of raiders forgets it" {
                let scout =
                    { hostileIn "W1N2" { X = 20; Y = 20 } [ Move ] with
                        Id = "s-1"
                    }

                Expect.isNone
                    (recallThrough
                        [ watched 100 [ kiterAt { X = 25; Y = 25 } 1500 ]; watched 108 [ scout ] ]
                     |> recalledIn)
                    "seen gone, gone: the scout left there is no raid"

                Expect.isNone
                    (recallThrough [ watched 100 [ scout ] ] |> recalledIn)
                    "and a room no raider was seen in holds nothing"
            }

            test "a raider seen fallen, or standing in another room, is gone from it" {
                let raider = kiterAt { X = 1; Y = 22 } 1500

                Expect.isNone
                    (recallThrough [ watched 100 [ raider ]; dark 101 |> withFallen [ "h-1" ] ]
                     |> recalledIn)
                    "its tombstone lies there"

                Expect.isNone
                    (recallThrough
                        [
                            watched 100 [ raider ]
                            dark 101
                            |> withWestSeen
                                [
                                    { raider with
                                        Pos = RoomPos.at "W2N2" { X = 48; Y = 22 }
                                    }
                                ]
                        ]
                     |> recalledIn)
                    "it stands across the exit"
            }

            test "the memory ends with the raider's life" {
                let raider = kiterAt { X = 25; Y = 25 } 30

                Expect.isSome
                    (recallThrough [ watched 100 [ raider ]; dark 129 ] |> recalledIn)
                    "a tick of life left, it is remembered"

                Expect.isNone
                    (recallThrough [ watched 100 [ raider ]; dark 130 ] |> recalledIn)
                    "its thirty ticks run out, it is not"
            }
        ]

[<Tests>]
let exitHoldTests =
    testList
        "the exit a raid left by"
        [
            test "a threat leaving by the west exit is held at the run it left by" {
                let seen =
                    World.watchExits
                        Tuning.defaults
                        Map.empty
                        (watched 100 [ kiterAt { X = 1; Y = 22 } 1500 ])

                Expect.equal
                    (watchOf seen)
                    (Some(
                        ExitWatch.Seen
                            [
                                {
                                    Id = "h-1"
                                    At = { X = 1; Y = 22 }
                                    Dies = 1600
                                }
                            ]
                    ))
                    "while it stands there the world remembers where, and when it dies"

                let gone = World.watchExits Tuning.defaults seen.ExitWatches (watched 101 [])

                Expect.equal
                    (watchOf gone)
                    (Some(
                        ExitWatch.Held
                            {
                                Run = [ for y in 20..24 -> { X = 0; Y = y } ]
                                Exit = { X = 0; Y = 22 }
                                Until = 101 + Tuning.defaults.ExitHoldTicks
                            }
                    ))
                    "the tick vision finds it gone, the whole west run beside it is held"
            }

            test "the hold ends at ExitHoldTicks, and at the threat's remembered death if sooner" {
                let holdAfter ttl =
                    watchThrough [ watched 100 [ kiterAt { X = 1; Y = 22 } ttl ]; watched 101 [] ]

                let long = holdAfter 1500
                let until = 101 + Tuning.defaults.ExitHoldTicks

                Expect.isSome
                    (World.watchExits Tuning.defaults long.ExitWatches (watched (until - 1) [])
                     |> watchOf)
                    "the hold stands to its last tick"

                Expect.isNone
                    (World.watchExits Tuning.defaults long.ExitWatches (watched until []) |> watchOf)
                    "and ends at ExitHoldTicks"

                Expect.isSome
                    (World.watchExits Tuning.defaults long.ExitWatches (dark 150) |> watchOf)
                    "out of sight it keeps its clock"

                Expect.equal
                    (holdAfter 30 |> holdOf |> Option.map (fun hold -> hold.Until))
                    (Some 130)
                    "a threat with thirty ticks to live is held thirty ticks from its last sighting"
            }

            test
                "the deadline is the death of the threat whose exit is held, not the longest-lived's" {
                // Two vanish together: the one beside the west run left by it with
                // thirty ticks to live; the one in mid-room, with 1,500, died there.
                let held =
                    watchThrough
                        [
                            watched
                                100
                                [ kiterAt { X = 1; Y = 22 } 30; kiterAt { X = 25; Y = 25 } 1500 ]
                            watched 101 []
                        ]

                Expect.equal
                    (holdOf held |> Option.map (fun hold -> hold.Until))
                    (Some 130)
                    "held until the leaver's own death"
            }

            test "a sighting carried through darkness starts no hold when vision returns" {
                Expect.isNone
                    (watchThrough
                        [ watched 100 [ kiterAt { X = 1; Y = 22 } 1500 ]; dark 101; watched 102 [] ]
                     |> watchOf)
                    "the room was dark on the full tick before: where the threat went is not known"
            }

            test "a threat leaving by the north exit near the west corner is held at the north run" {
                let held =
                    watchThrough
                        [
                            watchedOn cornerBorder 100 [ kiterAt { X = 1; Y = 1 } 1500 ]
                            watchedOn cornerBorder 101 []
                        ]

                Expect.equal
                    (holdOf held |> Option.map (fun hold -> hold.Run))
                    (Some [ for x in 1..5 -> { X = x; Y = 0 } ])
                    "(1,0) is a step from (1,1) and (0,2) a diagonal: the north run is the nearer"
            }

            test "a threat that vanishes away from every exit died, and nothing is held" {
                Expect.isNone
                    (watchThrough
                        [ watched 100 [ kiterAt { X = 25; Y = 22 } 1500 ]; watched 101 [] ]
                     |> watchOf)
                    "no exit is a step from (25,22)"

                Expect.isNone
                    (watchThrough [ watched 100 [ kiterAt { X = 1; Y = 30 } 1500 ]; watched 101 [] ]
                     |> watchOf)
                    "nor from (1,30), whose border is wall"
            }

            test "a threat that died beside the exit, its tombstone lying there, holds nothing" {
                let lastSeen = watched 100 [ kiterAt { X = 1; Y = 22 } 1500 ]

                Expect.isSome
                    (watchThrough [ lastSeen; watched 101 [] ] |> holdOf)
                    "the premise: gone from beside the run with no tombstone, it left"

                Expect.isNone
                    (watchThrough [ lastSeen; watched 101 [] |> withFallen [ "h-1" ] ] |> watchOf)
                    "with its tombstone in the room it died a step from the exit"

                Expect.isSome
                    (watchThrough [ lastSeen; watched 101 [] |> withFallen [ "h-other" ] ] |> holdOf)
                    "another creep's tombstone says nothing of this one"
            }

            test
                "a threat that vanished beside the exit is held only if the room across could hold it" {
                let lastSeen = watched 100 [ kiterAt { X = 1; Y = 22 } 1500 ]

                Expect.isSome
                    (watchThrough
                        [
                            lastSeen
                            watched 101 [] |> withWestSeen [ kiterAt { X = 49; Y = 22 } 1400 ]
                        ]
                     |> holdOf)
                    "seen across the border, it stepped onto the exit and was carried over"

                Expect.isNone
                    (watchThrough [ lastSeen; watched 101 [] |> withWestSeen [] ] |> watchOf)
                    "the room across seen and it in none: it died, it did not cross"

                let onRing = watched 100 [ kiterAt { X = 0; Y = 22 } 1500 ]

                Expect.isSome
                    (watchThrough [ onRing; watched 101 [] ] |> holdOf)
                    "the premise: last seen on the exit tile itself and gone, it crossed"

                Expect.isNone
                    (watchThrough [ onRing; watched 101 [] |> withWestSeen [] ] |> watchOf)
                    "last seen on the ring, the room across seen and it not there: it died"
            }

            test "W17S26 (#561): Beam715 dead at 1,6 beside the west run holds no exit" {
                let beam =
                    { kiterAt { X = 1; Y = 6 } 800 with
                        Id = "beam715"
                    }

                let lastSeen = watchedOn w17s26Border 986951 [ beam ]
                let next = watchedOn w17s26Border 986952 []

                Expect.equal
                    (watchThrough [ lastSeen; next ] |> holdOf |> Option.map _.Exit)
                    (Some { X = 0; Y = 6 })
                    "the premise: the watch read the vanished body as leaving by 0,6"

                Expect.isNone
                    (watchThrough [ lastSeen; next |> withFallen [ "beam715" ] ] |> watchOf)
                    "its tombstone at 1,6 says it died: no hold, no guard sent to the corpse"
            }

            test "the threat re-entering puts the ground back on its ring" {
                let held =
                    watchThrough [ watched 100 [ kiterAt { X = 1; Y = 22 } 1500 ]; watched 101 [] ]

                Expect.equal
                    (World.watchExits
                        Tuning.defaults
                        held.ExitWatches
                        (watched 102 [ kiterAt { X = 0; Y = 22 } 1400 ])
                     |> watchOf)
                    (Some(
                        ExitWatch.Seen
                            [
                                {
                                    Id = "h-1"
                                    At = { X = 0; Y = 22 }
                                    Dies = 1502
                                }
                            ]
                    ))
                    "seen again, the world remembers the threat and holds nothing"

                let ringOf colony =
                    Threats.ringIn (threatsOf colony (Atlas.ofView colony)) "W1N2"

                let back = declaredRaid raiders |> westField

                Expect.equal
                    (ringOf (heldWest back))
                    (ringOf back)
                    "and a Threat standing in the room is the Guard's ground whatever is held"

                Expect.isFalse
                    (ringOf back |> Set.exists (fun tile -> tile.X = 1))
                    "which is its own ring, nowhere near the run"
            }

            test
                "the raid gone by the west exit leaves the Guard on the tiles beside that run, and the guard kept" {
                let quiet =
                    declaredRaid [] |> westField |> withGuards [ guard "g-1", { X = 1; Y = 44 } ]

                let held = Map.ofList [ "g-1", taskId (Guard "W1N2") ]

                let releasesOf colony =
                    (decide colony held Set.empty None).Verdicts
                    |> List.choose (function
                        | Verdict.Released(creep, task, reason) -> Some(creep, task, reason)
                        | _ -> None)

                Expect.equal
                    (releasesOf quiet)
                    [ "g-1", taskId (Guard "W1N2"), ReleaseReason.TaskGone ]
                    "the premise: with nothing held the Guard goes with the raid"

                let holding = heldWest quiet

                Expect.isEmpty (releasesOf holding) "with the exit held the guard keeps its Guard"

                Expect.equal
                    ((decide holding held Set.empty None).Assignments |> Map.tryFind "g-1")
                    (Some(taskId (Guard "W1N2")))
                    "and holds it"

                Expect.equal
                    (Threats.ringIn (threatsOf holding (Atlas.ofView holding)) "W1N2")
                    ([ for y in 41..48 -> { X = 1; Y = y } ]
                     |> List.map (RoomPos.at "W1N2")
                     |> Set.ofList)
                    "and its ground is the tiles a step inside the run"
            }

            test "a hold with no guard alive pools no Guard" {
                // The hold keeps a living guard; it is no reason to send one.
                Expect.isNone
                    (entryFor (Guard "W1N2") (pooledOf (declaredRaid [] |> westField |> heldWest)))
                    "nobody holds the room's Guard, so none is pooled"
            }

            test "a hold whose ground comes out empty is no hold, and the guard goes" {
                // Without the field the terrain lays nothing walkable beside the
                // run: the Guard is not kept on an empty ring, which would send
                // the guard to the room's sources instead.
                let bare =
                    declaredRaid [] |> withGuards [ guard "g-1", { X = 1; Y = 44 } ] |> heldWest

                let held = Map.ofList [ "g-1", taskId (Guard "W1N2") ]

                Expect.isEmpty
                    (Threats.ringIn (threatsOf bare (Atlas.ofView bare)) "W1N2")
                    "the premise: nothing walkable beside the run"

                Expect.equal
                    ((decide bare held Set.empty None).Verdicts
                     |> List.choose (function
                         | Verdict.Released(creep, task, reason) -> Some(creep, task, reason)
                         | _ -> None))
                    [ "g-1", taskId (Guard "W1N2"), ReleaseReason.TaskGone ]
                    "the Guard goes as with nothing held"
            }

            test "exit tiles themselves are never in the ground" {
                let holding = declaredRaid [] |> westField |> heldWest
                let ground = Threats.ringIn (threatsOf holding (Atlas.ofView holding)) "W1N2"

                Expect.isNonEmpty ground "the premise: the hold has ground"

                Expect.isFalse
                    (ground |> Set.exists (fun tile -> tile.X = 0))
                    "and none of it is an exit tile the terrain lays, which would carry the guard across"
            }
        ]

/// A ranger body as the ranger row casts it at an 800 bank.
let private ranger name =
    creepWith name 0 0 (bodyFor Bodies.rangerPattern 800)

/// Torque404 as W18S25 saw it (#510): 17 MOVE and 3 ATTACK destroyed, 14
/// ATTACK and one MOVE alive under the whole body's weight.
let private crippledAt pos =
    { hostileIn
          "W1N2"
          pos
          (List.replicate 17 Move
           @ List.replicate 3 Attack
           @ [ Move ]
           @ List.replicate 14 Attack) with
        Hits = 1_454
    }

/// A heavy melee slower than a ranger on plain and on road alike: twelve
/// ATTACK on five MOVE, 360 a swing, which the one guard block a 600 bank
/// cannot even buy loses to. Ten on five would keep a ranger's pace on road.
let private plodderAt pos =
    hostileIn "W1N2" pos (List.replicate 5 Move @ List.replicate 12 Attack)

/// The same blows on a MOVE a part: as quick as the ranger.
let private runnerAt pos =
    hostileIn "W1N2" pos (List.replicate 10 Move @ List.replicate 10 Attack)

/// Crunchy047 as W17S25's door saw it (live t972,544): Torque404's shape,
/// Trepidimous's.
let private crunchyAt pos =
    { crippledAt pos with
        Id = "Crunchy047"
        Owner = "Trepidimous"
    }

/// Isolate093 beside it: eighteen parts at 180 hits, the two HEAL at its
/// tail all that still acts.
let private dyingHealerAt pos =
    { hostileIn "W1N2" pos (List.replicate 9 Move @ List.replicate 9 Heal) with
        Id = "Isolate093"
        Owner = "Trepidimous"
        Hits = 180
    }

/// A whole healer of the same side: twelve HEAL on twelve MOVE.
let private strongHealerAt pos =
    { hostileIn "W1N2" pos (List.replicate 12 Move @ List.replicate 12 Heal) with
        Id = "medic"
        Owner = "Trepidimous"
    }

/// Wraith403 as W17S26 saw it (live t972,544): its RANGED_ATTACK and a MOVE
/// shot off, MOVE, HEAL and MOVE acting on 266 hits, its own HEAL the only
/// one reaching it.
let private wraithAt room pos =
    { hostileIn room pos [ Move; RangedAttack; Move; Heal; Move ] with
        Id = "Wraith403"
        Owner = "Trepidimous"
        Hits = 266
    }

/// The raided field with a one-wide dead end off its east side, (31..36, 44):
/// a pocket a ranger kited into is pinned in.
let private withDeadEnd (colony: ColonyView) =
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Spatial =
            colony.Spatial
            |> withNeighbour
                "W1N2"
                { outpost with
                    Terrain =
                        TerrainGrid.ofList
                            [
                                for x in 20..30 do
                                    for y in 41..48 -> { X = x; Y = y }, Plain
                                for x in 31..36 -> { X = x; Y = 44 }, Plain
                            ]
                }
    }

let private groundOf colony =
    Threats.guardGroundIn (threatsOf colony (Atlas.ofView colony)) "W1N2"
    |> Option.defaultValue Set.empty

let private rangeTo (pos: Pos) (tile: RoomPos) = range pos (RoomPos.pos tile)

[<Tests>]
let kiteTests =
    testList
        "a melee that cannot close on a ranged body (#510)"
        [
            test
                "a crippled melee threatens a step and a swing about it, and the same body on its legs the room's margin" {
                let at = { X = 25; Y = 44 }

                let reachOf colony =
                    Threats.reachIn (threatsOf colony (Atlas.ofView colony)) "W1N2"

                Expect.equal
                    (reachOf (declaredRaid [ crippledAt at ]))
                    (set
                        [
                            for dx in -2 .. 2 do
                                for dy in -2 .. 2 -> { X = 25 + dx; Y = 44 + dy }
                        ])
                    "with its fatigue spent it steps once and swings, then waits seventeen ticks for the next step"

                Expect.isTrue
                    (reachOf (declaredRaid [ { crippledAt at with Hits = 3_500 } ])
                     |> Set.contains { X = 25; Y = 41 })
                    "the whole body walks, so its Reach keeps the margin"
            }

            // A hostile's strength is its live parts and the parts the healing
            // reaching it restores in the reaction window (#531), replacing the
            // Brief's S7 rule 1 (any healer within three read it on paper).
            test
                "a cripple beside a dying healer is read near its live parts, beside a whole one near its paper" {
                let at = { X = 25; Y = 44 }
                let crunchy = crunchyAt at

                let reachOf colony =
                    Threats.reachIn (threatsOf colony (Atlas.ofView colony)) "W1N2"

                let weak = [ crunchy; dyingHealerAt { X = 26; Y = 44 } ]
                let strong = [ crunchy; strongHealerAt { X = 26; Y = 44 } ]

                Expect.isTrue
                    (HostileInfo.stationary weak crunchy)
                    "two HEAL restore too little in the window: it still cannot walk"

                Expect.equal
                    (reachOf (declaredRaid weak) |> Set.filter (fun tile -> range tile at <= 2))
                    (reachOf (declaredRaid [ crunchy ]))
                    "so it threatens a step and a swing about it, as alone"

                Expect.equal
                    (HostileInfo.effectiveParts strong crunchy)
                    crunchy.Body
                    "twelve HEAL restore every part within the window: its paper"

                Expect.isFalse (HostileInfo.stationary strong crunchy) "and it walks"
            }

            test "beside a dying healer a ranger out-shooting its two HEAL kites the cripple down" {
                let crunchy = crunchyAt { X = 25; Y = 44 }
                let healer = dyingHealerAt { X = 26; Y = 44 }

                // Two ranger blocks, 40 a tick against the healer's 24.
                let colony =
                    { declaredRaid [ crunchy; healer ] with
                        Bank = bank 1400 1400
                    }
                    |> withGuards
                        [
                            creepWith "r-1" 0 0 (bodyFor Bodies.rangerPattern 1400),
                            { X = 22; Y = 44 }
                        ]

                Expect.equal (Facts.answer colony "W1N2") Answer.Kite "a ranger kites it down"

                let result = decide colony Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "r-1" result.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "and the ranger takes the room"

                Expect.isTrue
                    (groundOf colony
                     |> Set.exists (fun tile -> rangeTo (RoomPos.pos crunchy.Pos) tile <= 3))
                    "on ground in range of the cripple, not kited off the room"
            }

            // A part's boost multiplies what it does, as the
            // engine's `calcBodyEffectiveness` and `_applyDamage` do.
            test
                "a T3 6T22R10M12H is priced at its boosts: ×4 its shot, heal and MOVE, its TOUGH taking 0.3" {
                let plain = hostileIn "W1N2" { X = 25; Y = 25 } kalgenBody
                let t3 = boostedAt Tier3 plain

                let shot h =
                    HostileInfo.effectivePower [ h ] h RangedAttack

                Expect.equal (shot plain, shot t3) (22, 88) "22 RANGED_ATTACK shoot as 88"

                Expect.equal
                    (HostileInfo.healing [ plain ] plain, HostileInfo.healing [ t3 ] t3)
                    (144, 576)
                    "12 HEAL heal 576 a tick, not 144"

                let plainPace h =
                    Gait.ticks Gait.plain (HostileInfo.gait [ h ] h)

                Expect.equal
                    (plainPace plain, plainPace t3)
                    (Some 4, Some 1)
                    "10 MOVE under 40 weighted parts: a tile in four ticks on plain, boosted in one"

                Expect.equal
                    (HostileInfo.effectiveHits plain, HostileInfo.effectiveHits t3)
                    (5_000, 6_400)
                    "its 600 hits of TOUGH cost 2,000 damage"

                Expect.equal
                    (HostileInfo.damageTaken t3 600, HostileInfo.damageTaken t3 3_000)
                    (180, 1_600)
                    "a 600 tower shot takes 180; 3,000 takes the TOUGH's 600 and 1,000 behind it"

                Expect.equal
                    (HostileInfo.damageFor t3 576)
                    1_920
                    "so a tick of its heal costs 1,920 of our damage to undo"

                // 3,600 a tick: 2,200 through the TOUGH and 576 back leave
                // 3,376; then 3,600 in full leaves 352; the third kills.
                Expect.equal
                    ([ 2; 3 ] |> List.map (HostileInfo.killedWithin t3 (fun _ -> 3_600) 576))
                    [ false; true ]
                    "under 3,600 a tick its TOUGH is paid for once: dead on the third tick"

                let worn = { t3 with Hits = 4_400 }

                Expect.equal
                    (HostileInfo.damageTaken worn 600, HostileInfo.effectiveHits worn)
                    (600, 4_400)
                    "with its TOUGH shot off, a shot lands in full"
            }

            test "a body whose gun is shot off and heals itself is unarmed within a short window" {
                let wraith = wraithAt "W1N2" { X = 25; Y = 44 }

                Expect.equal
                    (Engine.liveParts wraith.Body wraith.Hits)
                    [ Move; Heal; Move ]
                    "the premise: MOVE, HEAL, MOVE acting"

                Expect.isFalse
                    (HostileInfo.partsWithin 2 [ wraith ] wraith |> List.contains RangedAttack)
                    "two ticks of its own 12 a tick leave its gun off"

                Expect.isTrue
                    (HostileInfo.partsWithin 3 [ wraith ] wraith |> List.contains RangedAttack)
                    "the third brings it back"
            }

            // A heal acts on one target a tick (#531): a side's healing is
            // spent once, on the body short of the most hits in its reach.
            test "one healer's heal restores the most hurt of two cripples, never both" {
                let crunchy = crunchyAt { X = 25; Y = 44 }

                let lighter =
                    { crunchyAt { X = 27; Y = 44 } with
                        Id = "lighter"
                        Hits = 2_500
                    }

                let medic = strongHealerAt { X = 26; Y = 44 }
                let side = [ crunchy; lighter; medic ]

                Expect.equal
                    (HostileInfo.effectiveParts side crunchy)
                    crunchy.Body
                    "the most hurt takes the whole heal: its paper"

                Expect.equal
                    (HostileInfo.effectiveParts side lighter)
                    (Engine.liveParts lighter.Body lighter.Hits)
                    "the other none: its live parts"
            }

            test
                "a healer whose HEAL is shot off heals nothing and is no healer, whatever comes back" {
                // HEAL at the head: 100 hits leave the MOVE at the tail alone.
                let worn =
                    { hostileIn "W1N2" { X = 25; Y = 44 } [ Heal; Heal; Move ] with
                        Id = "worn"
                        Owner = "Trepidimous"
                        Hits = 100
                    }

                // A whole healer beside it restores its HEAL inside the window.
                let side = [ worn; strongHealerAt { X = 26; Y = 44 } ]

                Expect.isTrue
                    (HostileInfo.effectiveParts side worn |> List.contains Heal)
                    "the premise: its HEAL comes back in the window"

                Expect.equal (HostileInfo.healing side worn) 0 "it heals nothing now"

                Expect.isFalse
                    (HostileInfo.heals side worn)
                    "and is read as no healer by the same parts"
            }

            // The kite ground off the maps (movement S3, which retired its
            // step-and-swing cut and `holdReach`; `stepAndSwing` stays for the
            // defender's line, S4's): the cripple, rested, takes one
            // step before its seventeen-tick wait, so a tile at two is struck
            // the tick after and the ring at three is what is left.
            test "a ranger is matched to kill it from range three, and the melee guard is not sent" {
                let at = { X = 25; Y = 44 }

                let colony =
                    declaredRaid [ crippledAt at ]
                    |> withGuards
                        [ ranger "r-1", { X = 22; Y = 47 }; guard "g-1", { X = 20; Y = 41 } ]

                let result = decide colony Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "r-1" result.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the ranger takes the room's Guard"

                Expect.notEqual
                    (Map.tryFind "g-1" result.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the guard it out-swings is never walked into its reach"

                Expect.contains
                    result.Intents
                    (RangedAttackCreep("r-1", "h-1"))
                    "and shoots from where it stands"

                Expect.equal
                    (guardQuotaOf colony)
                    (Some 0)
                    "nor does the guard row cast a melee body for it"

                let ground = groundOf colony

                Expect.isNonEmpty ground "the premise: the Guard has ground"

                Expect.isTrue
                    (ground |> Set.forall (fun tile -> rangeTo at tile >= 3 && rangeTo at tile <= 3))
                    "its ground is in range of the melee and out of its step and swing"
            }

            // Movement S3 flipped the S0 pin here ("the kite ground is the same
            // for a lamed, tired ranger as for a quick one"): the room's ground
            // is still one set, but which of its tiles a ranger may step onto
            // is its own walk's (`Threats.kiteUnsafeFor`, #524): a tile the
            // melee strikes before this ranger, its legs, its parts shot off
            // and its fatigue counted, could be on it and gone.
            test "a lamed, tired ranger may not step onto a kite tile a quick one may" {
                let at = { X = 25; Y = 44 }
                let from = { X = 21; Y = 47 }
                let ring = { X = 22; Y = 47 }
                let quick = ranger "r-1"

                let lamed =
                    { quick with
                        Body = Map.add Move 1 quick.Body
                        Fatigue = 20
                    }

                let unsafeWith body =
                    let colony = declaredRaid [ plodderAt at ] |> withGuards [ body, from ]

                    let atlas = Atlas.ofView colony

                    match Threats.kiteUnsafeFor (threatsOf colony atlas) atlas body None with
                    | Some(unsafe, _) -> unsafe (ring.X * Engine.roomSide + ring.Y)
                    | None -> failtest "the premise: its walk reads the plodder's map"

                Expect.isFalse
                    (unsafeWith quick)
                    "the quick ranger is on the ring at three and off it before the plodder's step and swing"

                Expect.isTrue
                    (unsafeWith lamed)
                    "one MOVE acting under its shot-off weight and twenty fatigue to pay: struck first"
            }

            test "the speed and not the paper picks the target" {
                let withRanger hostile =
                    declaredRaid [ hostile ] |> withGuards [ ranger "r-1", { X = 22; Y = 48 } ]

                let assigned colony =
                    (decide colony Map.empty Set.empty None).Assignments |> Map.tryFind "r-1"

                Expect.equal
                    (assigned (withRanger (plodderAt { X = 25; Y = 44 })))
                    (Some(taskId (Guard "W1N2")))
                    "360 a swing, behind the ranger on road and plain, is kited"

                Expect.notEqual
                    (assigned (withRanger (runnerAt { X = 25; Y = 44 })))
                    (Some(taskId (Guard "W1N2")))
                    "the same blows on legs as quick as the ranger's are no chase"

                Expect.notEqual
                    (assigned (
                        withRanger (
                            hostileIn
                                "W1N2"
                                { X = 25; Y = 44 }
                                (List.replicate 5 Move @ List.replicate 10 Attack)
                        )
                    ))
                    (Some(taskId (Guard "W1N2")))
                    "ten ATTACK on five MOVE is slower on plain but keeps the ranger's pace on a road"
            }

            test "the kite ground keeps out of a dead end the melee can pin the ranger in" {
                let at = { X = 28; Y = 44 }
                let colony = declaredRaid [ plodderAt at ] |> withDeadEnd
                let ground = groundOf colony

                Expect.isNonEmpty ground "the premise: there is open ground to kite on"

                Expect.isFalse
                    (ground |> Set.exists (fun tile -> tile.X > 30))
                    "the corridor's mouth is three from the melee, and a step down it is a step it cannot come back up"

                Expect.isTrue
                    (ground |> Set.forall (fun tile -> rangeTo at tile >= 3))
                    "and nothing in it is in the melee's step and swing"
            }

            // A kiter's exit camp, read by its own timing since movement S3
            // (`Threats.kiteUnsafeFor`): a step and a swing about the held exit,
            // a step more for each tick past the first the tile holds the body.
            // A rested ranger on plain keeps the S0 pin's two.
            test
                "the chase crosses the border at an exit three or more from the one the target left by" {
                // The raid left the home by (25,0) and waits across it; the ranger
                // follows it into W1N2 by another exit tile.
                let home =
                    declaredRaid [ plodderAt { X = 25; Y = 44 } ]
                    |> withBodyAtHome (ranger "r-1") { X = 25; Y = 3 }

                let wide =
                    { home with
                        Spatial =
                            home.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    Terrain =
                                        TerrainGrid.ofList
                                            [
                                                for x in 15..35 do
                                                    for y in 1..9 -> { X = x; Y = y }, Plain
                                            ]
                                })
                        ExitHolds =
                            Map.ofList
                                [
                                    "W1N1",
                                    {
                                        Run = [ for x in 1..48 -> { X = x; Y = 0 } ]
                                        Exit = { X = 25; Y = 0 }
                                        Until = home.Time + 50
                                    }
                                ]
                    }

                let result = decide wide Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "r-1" result.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the premise: the ranger chases the target into the outpost"

                let step =
                    moveIntents result.Intents |> List.tryFind (fst >> (=) "r-1") |> Option.map snd

                Expect.isSome step "it walks"

                Expect.isFalse
                    (List.contains step.Value [ TopLeft; Top; TopRight ])
                    "and never onto a tile within two of the exit the target left by"
            }
        ]

/// The raided outpost floored out to its borders, and the colony harassing
/// W1N3 beyond it, its ranger standing quiet there on the enemy's source.
let private harassingBeyond hostiles =
    let colony = declaredRaid hostiles
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    let floor =
        TerrainGrid.ofList
            [
                for x in 1..48 do
                    for y in 1..48 -> { X = x; Y = y }, Plain
            ]

    { colony with
        Creeps = colony.Creeps @ [ ranger "r-1" ]
        Spatial =
            { colony.Spatial with
                Borders = colony.Spatial.Borders |> Map.add "W1N3" plainRing
            }
            |> withNeighbour "W1N2" { outpost with Terrain = floor }
            |> withNeighbour
                "W1N3"
                { RoomLayer.empty with
                    Terrain = floor
                    CreepPositions = Map.ofList [ "r-1", { X = 25; Y = 45 } ]
                }
    }
    |> casting
        {
            RoomName = "W1N3"
            Enemy = "Trepidimous"
            Stand = RoomPos.at "W1N3" { X = 25; Y = 44 }
            Controller = RoomPos.at "W1N3" { X = 30; Y = 30 }
            Via = []
            Blocks = None
        }

[<Tests>]
let chaseCallTests =
    testList
        "a quiet ranger called to a chase (#510)"
        [
            test
                "a ranger holding a quiet room's Guard is let go for a chase within two rooms, and takes it" {
                let held = Map.ofList [ "r-1", taskId (Guard "W1N3") ]

                let quiet = decide (harassingBeyond []) held Set.empty None

                Expect.equal
                    (Map.tryFind "r-1" quiet.Assignments)
                    (Some(taskId (Guard "W1N3")))
                    "the premise: with nothing to chase it keeps its own Guard"

                let called =
                    decide (harassingBeyond [ crippledAt { X = 25; Y = 10 } ]) held Set.empty None

                Expect.contains
                    called.Verdicts
                    (Verdict.Released("r-1", taskId (Guard "W1N3"), ReleaseReason.Called))
                    "a crippled melee a walk away calls it off its quiet ground"

                Expect.equal
                    (Map.tryFind "r-1" called.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "and it takes the chase"
            }

            test "a chase already held calls nobody, whichever order the holders are walked in" {
                // r-1 is walked before r-9, which holds the chase: read against the
                // ones kept so far, the slot looked free to r-1 every tick, and it
                // was let go and took its own Guard back, tick after tick.
                let colony =
                    harassingBeyond [ crippledAt { X = 25; Y = 10 } ]
                    |> standingIn "W1N2" ("r-9", { X = 25; Y = 14 })

                let colony =
                    { colony with
                        Creeps = colony.Creeps @ [ ranger "r-9" ]
                    }

                let held = Map.ofList [ "r-1", taskId (Guard "W1N3"); "r-9", taskId (Guard "W1N2") ]

                let result = decide colony held Set.empty None

                Expect.isFalse
                    (result.Verdicts
                     |> List.exists (function
                         | Verdict.Released _ -> true
                         | _ -> false))
                    "nobody is released"

                Expect.equal
                    (Map.tryFind "r-1" result.Assignments, Map.tryFind "r-9" result.Assignments)
                    (Some(taskId (Guard "W1N3")), Some(taskId (Guard "W1N2")))
                    "and each keeps what it held"
            }
        ]

[<Tests>]
let transitStrikeTests =
    testList
        "a Guard's holder walking through another room (#563)"
        [
            // Live t986,014 (W17S26): a ranger holding W17S25's Guard walked
            // past Trepidimous' melee three off and struck it nothing but a
            // mass attack's 2, its kill order standing in the Guard's room.
            test "strikes by the weapon reflex what it may strike and reaches there" {
                // W1N3, a room that is none of ours: the body stands at
                // (25,25), a raider of the declared enemy at `pos`.
                let held raid body pos =
                    let colony = declaredRaid raid
                    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

                    let floor =
                        TerrainGrid.ofList
                            [
                                for x in 1..48 do
                                    for y in 1..48 -> { X = x; Y = y }, Plain
                            ]

                    let colony =
                        { colony with
                            Creeps = colony.Creeps @ [ body ]
                            Hostiles =
                                raid
                                @ [
                                    { hostileIn "W1N3" pos [ Move; Carry ] with
                                        Id = "passer"
                                        Owner = "Trepidimous"
                                    }
                                ]
                            Spatial =
                                { colony.Spatial with
                                    Borders = colony.Spatial.Borders |> Map.add "W1N3" plainRing
                                }
                                |> withNeighbour "W1N2" { outpost with Terrain = floor }
                                |> withNeighbour
                                    "W1N3"
                                    { RoomLayer.empty with
                                        Terrain = floor
                                        CreepPositions =
                                            Map.ofList [ body.Name, { X = 25; Y = 25 } ]
                                    }
                        }

                    decide colony (Map.ofList [ body.Name, taskId (Guard "W1N2") ]) Set.empty None

                // The ranger answers a plodder's raid, the melee guard a
                // smallMelee's.
                let shot = held [ plodderAt { X = 25; Y = 44 } ] (ranger "r-1") { X = 25; Y = 27 }

                Expect.equal
                    (Map.tryFind "r-1" shot.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the premise: the ranger holds the outpost's Guard on its way"

                Expect.contains
                    shot.Intents
                    (RangedAttackCreep("r-1", "passer"))
                    "and shoots the declared enemy two off on its way"

                let swung = held raiders (guard "g-1") { X = 25; Y = 26 }

                Expect.equal
                    (Map.tryFind "g-1" swung.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the premise: the melee guard holds it too"

                Expect.contains
                    swung.Intents
                    (AttackCreep("g-1", "passer"))
                    "and swings at the one beside it"
            }
        ]

/// The raided field widened to x 15..35, y 35..48, room for a ranger to come
/// at the melee from the north.
let private withField (colony: ColonyView) =
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Spatial =
            colony.Spatial
            |> withNeighbour
                "W1N2"
                { outpost with
                    Terrain =
                        TerrainGrid.ofList
                            [
                                for x in 15..35 do
                                    for y in 35..48 -> { X = x; Y = y }, Plain
                            ]
                }
    }

/// Where one step in a direction lands.
let private stepped (pos: Pos) (direction: Direction) =
    let dx, dy =
        match direction with
        | Top -> 0, -1
        | TopRight -> 1, -1
        | Right -> 1, 0
        | BottomRight -> 1, 1
        | Bottom -> 0, 1
        | BottomLeft -> -1, 1
        | Left -> -1, 0
        | TopLeft -> -1, -1

    { X = pos.X + dx; Y = pos.Y + dy }

[<Tests>]
let kiteStepTests =
    testList
        "a kiting ranger's step (#510)"
        [
            // A kiter walks round what a melee strikes before it could be on it
            // and gone (movement S3, `Threats.kiteUnsafeFor`), not its Reach:
            // against the plodder that is the S0 pin's step and swing.
            test
                "it walks onto the ring three from the melee, inside its Reach, and shoots from there" {
                let melee = { X = 25; Y = 44 }
                let from = { X = 24; Y = 40 }

                let at pos =
                    declaredRaid [ plodderAt melee ]
                    |> withField
                    |> withGuards [ ranger "r-1", pos ]

                let first = decide (at from) Map.empty Set.empty None

                let step =
                    moveIntents first.Intents
                    |> List.tryFind (fst >> (=) "r-1")
                    |> Option.map (snd >> stepped from)

                Expect.equal
                    (step |> Option.map (range melee))
                    (Some 3)
                    "the Reach reaches the ring; only a step and a swing about the melee is kept off"

                let held = Map.ofList [ "r-1", taskId (Guard "W1N2") ]
                let second = decide (at step.Value) held Set.empty None

                Expect.contains
                    second.Intents
                    (RangedAttackCreep("r-1", "h-1"))
                    "and from the ring it shoots"
            }

            // Movement S3 review: the arbitration shoves a body only onto its own
            // candidates, so a kiter on its ring at three offers none a melee
            // strikes before it is off again: a higher-rank chain never pushes
            // it to two.
            test "a kiter on its ring offers the arbitration no tile in the melee's step and swing" {
                let melee = { X = 25; Y = 44 }
                let from = { X = 25; Y = 41 }

                let colony =
                    declaredRaid [ plodderAt melee ]
                    |> withField
                    |> withGuards [ ranger "r-1", from ]

                let atlas = Atlas.ofView colony
                let threats = threatsOf colony atlas

                let movement =
                    movementOf
                        colony
                        atlas
                        threats
                        []
                        (Map.ofList [ "r-1", Guard "W1N2" ])
                        Map.empty
                        Set.empty

                match movement.Intents |> List.tryFind (fun intent -> intent.Creep = "r-1") with
                | Some intent ->
                    Expect.equal
                        (List.tryHead intent.Candidates)
                        (Some(RoomPos.at "W1N2" from))
                        "the premise: it stays"

                    Expect.all
                        intent.Candidates
                        (fun tile -> range melee (RoomPos.pos tile) >= 3)
                        "and is offered nothing within two of the melee"
                | None -> failtest "it has an intent"
            }

            // Movement S3 (#524): a ring tile is one this ranger is off again
            // before the melee strikes it. Swamp holds it five ticks, past the
            // plodder's fourth: the swamp in front is walked round to a plain
            // ring tile it is on in two ticks.
            test "it walks round the swamp on the ring to a plain ring tile" {
                let melee = { X = 25; Y = 44 }
                let from = { X = 25; Y = 40 }
                let swamp = [ for x in 24..26 -> { X = x; Y = 41 } ]
                let colony = declaredRaid [ plodderAt melee ] |> withField
                let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

                let colony =
                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withNeighbour
                                "W1N2"
                                { outpost with
                                    Terrain =
                                        TerrainGrid.ofList
                                            [
                                                for x in 15..35 do
                                                    for y in 35..48 ->
                                                        let tile = { X = x; Y = y }

                                                        tile,
                                                        (if List.contains tile swamp then
                                                             Swamp
                                                         else
                                                             Plain)
                                            ]
                                }
                    }
                    |> withGuards [ ranger "r-1", from ]

                let step =
                    moveIntents (decideOn colony).Intents
                    |> List.tryFind (fst >> (=) "r-1")
                    |> Option.map (snd >> stepped from)

                Expect.isSome step "it walks"

                Expect.isFalse
                    (List.contains step.Value swamp)
                    "never onto the swamp three off the melee"

                Expect.isGreaterThan
                    (range melee step.Value)
                    (Engine.meleeRange + 1)
                    "nor into its step and swing"
            }

            // Movement S3 retired `glueStep`'s `struck` (a second melee beside
            // the target's tile): the full tick reads the maps
            // (`Emitter.glueKeeps`), the tile kept where it is struck no harder
            // by the tick after than where the guard stands.
            test "a melee guard keeps to its target's tile unless the maps strike it harder there" {
                let target = { X = 25; Y = 44 }
                let from = { X = 24; Y = 44 }
                let body = guard "g-1"

                let keeps second =
                    let colony =
                        declaredRaid (plodderAt target :: second)
                        |> withField
                        |> withGuards [ body, from ]

                    let atlas = Atlas.ofView colony

                    Emitter.glueKeeps
                        (threatsOf colony atlas)
                        body
                        (RoomPos.at "W1N2" from)
                        (RoomPos.at "W1N2" target)

                Expect.isTrue (keeps []) "alone, its target strikes both tiles alike"

                let beyond =
                    { plodderAt { X = 27; Y = 44 } with
                        Id = "h-2"
                    }

                Expect.isFalse
                    (keeps [ beyond ])
                    "a second melee a step from the target's tile and two from the guard's strikes it there first"
            }
        ]

[<Tests>]
let shutChaseTests =
    testList
        "an outpost shut on a raid a ranger kites down (#510)"
        [
            test "pools the chase for a ranger and nothing else, dark or seen" {
                let shut hostiles =
                    { raidedOutpost hostiles with
                        ChasedOutposts = Set.singleton "W1N2"
                    }
                    |> withGuards
                        [ ranger "r-1", { X = 22; Y = 47 }; guard "g-1", { X = 20; Y = 41 } ]

                let dark = decide (shut []) Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "r-1" dark.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "out of sight the ranger takes the chase"

                Expect.isNone
                    (Map.tryFind "g-1" dark.Assignments)
                    "and the melee guard is sent nowhere"

                let seen = decide (shut [ plodderAt { X = 25; Y = 44 } ]) Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "r-1" seen.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "in sight it holds it"

                Expect.contains seen.Intents (RangedAttackCreep("r-1", "h-1")) "and shoots"
            }

            test
                "a raid the stand-down shuts that the colony's own row would win is the ranger's chase all the same: it shuts the economy, not the kill (#569)" {
                // Four ATTACK on one MOVE: the guard row wins it, and a ranger
                // holds its range on it, on road too.
                let shut =
                    { raidedOutpost
                          [
                              hostileIn
                                  "W1N2"
                                  { X = 25; Y = 42 }
                                  [ Attack; Attack; Attack; Attack; Move ]
                          ] with
                        RaidedOutposts = Set.singleton "W1N2"
                        Bank = { Available = 1_300; Capacity = 1_300 }
                    }
                    |> withGuards [ ranger "r-1", { X = 22; Y = 47 } ]

                Expect.isTrue
                    (Facts.rowAnswer shut "W1N2").Wins
                    "the guard row the bank buys would win it"

                // The stand-down withholds no row from the verdict: the room
                // is the row's Win, and its kill the chase.
                Expect.equal
                    (Facts.answer shut "W1N2")
                    Answer.Win
                    "and the room's verdict is the row's Win"

                let decision = decide shut Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "r-1" decision.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "but the room is shut to the row's economy, so its kill is the chase, and the ranger takes its Guard"
            }

            test
                "a raid the stand-down shuts that the colony's own guard row wins, with no ranger to chase it, is the guard's kill all the same (#571)" {
                let shut =
                    { raidedOutpost
                          [
                              hostileIn
                                  "W1N2"
                                  { X = 25; Y = 42 }
                                  [ Attack; Attack; Attack; Attack; Move ]
                          ] with
                        RaidedOutposts = Set.singleton "W1N2"
                        Bank = { Available = 1_300; Capacity = 1_300 }
                    }
                    |> withGuards [ guard "g-1", { X = 22; Y = 47 } ]

                Expect.isTrue
                    (Facts.rowAnswer shut "W1N2").Wins
                    "the premise: the guard row the bank buys wins it"

                let decision = decide shut Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "g-1" decision.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the stand-down shuts the economy, never the kill: the guard takes the room's Guard"

                // Each body on its own fitness, not the row's: beside it, a
                // two-ATTACK guard that loses this raid alone.
                let weak = creepWith "g-weak" 0 0 [ Attack; Attack; Move; Move ]
                let both = shut |> withGuards [ weak, { X = 20; Y = 47 } ]
                let atlas = Atlas.ofView both
                let brief = Threats.briefOf (threatsOf both atlas) "W1N2"

                Expect.isTrue (brief.Admits(guard "g-1")) "the guard that wins alone is admitted"

                Expect.isFalse
                    (brief.Admits weak)
                    "and one that loses the raid alone is not, though its row wins"
            }

            test
                "a raid the stand-down shuts that the colony's guard row wins, with no fighter of ours anywhere, casts the guard that hunts it (#571)" {
                let shut =
                    { raidedOutpost
                          [
                              hostileIn
                                  "W1N2"
                                  { X = 25; Y = 42 }
                                  [ Attack; Attack; Attack; Attack; Move ]
                          ] with
                        RaidedOutposts = Set.singleton "W1N2"
                        Bank = { Available = 5_600; Capacity = 5_600 }
                    }

                Expect.equal (Facts.answer shut "W1N2") Answer.Win "the premise: the row wins it"

                let brief = Threats.briefOf (threatsOf shut (Atlas.ofView shut)) "W1N2"

                Expect.equal
                    (brief.Asks |> List.map (fun ask -> ask.Row))
                    [ CastRow.Guard ]
                    "the stand-down withholds no cast: the chase asks the guard row"

                Expect.exists
                    (spawnIntents (decide shut Map.empty Set.empty None).Intents)
                    (fun (_, _, name) -> name.StartsWith "guard-")
                    "and the colony casts the guard"
            }

            test
                "the chase out of sight is the row's: this tick's look elsewhere in the world does not drop it (#534)" {
                let shut =
                    { raidedOutpost [] with
                        ChasedOutposts = Set.singleton "W1N2"
                    }
                    |> withGuards [ ranger "r-1", { X = 22; Y = 47 } ]

                let observed =
                    { shut with
                        World =
                            { WorldView.empty with
                                Rooms =
                                    Map.ofList
                                        [
                                            "W1N2",
                                            {
                                                Hostiles = []
                                                HostileFatigue = Map.empty
                                                InvaderCores = []
                                                Towers = []
                                                OurTowers = []
                                                Fallen = []
                                                Control = neutralRoom
                                            }
                                        ]
                            }
                    }

                Expect.equal
                    (Map.tryFind "r-1" (decide shut Map.empty Set.empty None).Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the premise: out of sight the ranger takes the chase"

                // An observer every other tick would otherwise toggle the Task:
                // the look that finds it empty ends the chase in the row
                // (`ObserveOutpostTests`), and the gate reads only the row.
                Expect.equal
                    (Map.tryFind "r-1" (decide observed Map.empty Set.empty None).Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "with the world looking in, the row still says chase, and the ranger keeps it"
            }
        ]

/// Raptor865 as W17S26 saw it (#516): five WORK on three MOVE, no weapon,
/// a player's body digging our outpost.
let private diggerAt room pos =
    { hostileIn room pos (List.replicate 5 Work @ List.replicate 3 Move) with
        Owner = "Trepidimous"
    }

/// The digger on its seat beside the outpost's rock at (25,40) (#550).
let private seatedDigger =
    { diggerAt "W1N2" { X = 24; Y = 41 } with
        Id = "d-1"
    }

/// Gambit229 as W17S26 saw it (#550): an empty 23C23M hauler, at the pace of
/// every fighting row of ours.
let private haulerOf id pos =
    { hostileIn "W1N2" pos (List.replicate 23 Carry @ List.replicate 23 Move) with
        Id = id
        Owner = "Trepidimous"
    }

/// Infinity425 as W17S26 saw it (#550): a 3M3C claimer reserving against us.
let private claimerOf id pos =
    { hostileIn "W1N2" pos (List.replicate 3 Move @ List.replicate 3 BodyPart.Claim) with
        Id = id
        Owner = "Trepidimous"
    }

/// The outpost floored out to its borders, and beyond it W1N3, the home of
/// a child this colony raises, its controller at (25,25) and its resident
/// ranger `r-1` on the ring: the home borders the outpost and not ours.
let private raisingBeyond hostiles =
    let colony = declaredRaid hostiles
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    let floor =
        TerrainGrid.ofList
            [
                for x in 1..48 do
                    for y in 1..48 -> { X = x; Y = y }, Plain
            ]

    { colony with
        Creeps = colony.Creeps @ [ ranger "r-1" ]
        RoomControl = Map.add "W1N3" ownedRoom colony.RoomControl
        Declared = [ "W1N1"; "W1N3" ]
        Borrowed =
            { BorrowedWork.empty with
                Rooms = [ "W1N3" ]
            }
        Stages = Map.add "W1N3" Nursery colony.Stages
        Spatial =
            { colony.Spatial with
                Borders = colony.Spatial.Borders |> Map.add "W1N3" plainRing
                TargetKinds = colony.Spatial.TargetKinds |> Map.add "ctl-child" Controller
            }
            |> withNeighbour "W1N2" { outpost with Terrain = floor }
            |> withNeighbour
                "W1N3"
                { RoomLayer.empty with
                    Terrain = floor
                    TargetPositions = Map.ofList [ "ctl-child", { X = 25; Y = 25 } ]
                    Obstacles = Set.singleton { X = 25; Y = 25 }
                    CreepPositions = Map.ofList [ "r-1", { X = 25; Y = 26 } ]
                }
    }

let private residentHeld = Map.ofList [ "r-1", taskId (Guard "W1N3") ]

/// The outpost floored out to its borders, and beyond it W1N3, a second
/// room this colony mines, its source at (25,25): outpost A and outpost B
/// (#528), the guards of ours standing in A on the tiles the case names.
let private secondOutpost hostiles (ours: (CreepInfo * Pos) list) =
    let colony = declaredRaid hostiles |> withGuards ours
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    let floor =
        TerrainGrid.ofList
            [
                for x in 1..48 do
                    for y in 1..48 -> { X = x; Y = y }, Plain
            ]

    { colony with
        Sources = source "src-b" :: colony.Sources
        Spatial =
            { colony.Spatial with
                Borders = colony.Spatial.Borders |> Map.add "W1N3" plainRing
                TargetKinds = colony.Spatial.TargetKinds |> Map.add "src-b" Source
            }
            |> withNeighbour "W1N2" { outpost with Terrain = floor }
            |> withNeighbour
                "W1N3"
                { RoomLayer.empty with
                    Terrain = floor
                    TargetPositions = Map.ofList [ "src-b", { X = 25; Y = 25 } ]
                    Obstacles = Set.singleton { X = 25; Y = 25 }
                }
    }

/// Virtual458 as W18S25 saw it (#528): a declared enemy's 17 ATTACK on 17
/// MOVE, shot to 74 hits, every ATTACK and all but one MOVE gone. It strikes
/// nothing back.
let private disarmedAt owner room pos =
    { hostileIn room pos (List.replicate 17 Attack @ List.replicate 17 Move) with
        Owner = owner
        Hits = 74
    }

[<Tests>]
let trespassTests =
    testList
        "a player's unarmed worker in a room we mine (#516)"
        [
            test "an unarmed 5W3M player body in our outpost pools one kill Task, one rung up" {
                let pool = pooledOf (declaredRaid [ diggerAt "W1N2" { X = 25; Y = 44 } ])

                let entry = entryFor (Guard "W1N2") pool

                Expect.isSome entry "the outpost's Guard is pooled for it"

                Expect.equal
                    (entry |> Option.map (fun e -> e.Capacity))
                    (Some(Capacity.fighters 1))
                    "for one body"

                let raidGuard =
                    pooledOf (declaredRaid [ hostileIn "W1N2" { X = 25; Y = 42 } smallMelee ])
                    |> entryFor (Guard "W1N2")

                Expect.equal
                    (entry |> Option.map (fun e -> e.Priority))
                    (Some(safetyRank OneRungUp))
                    "a rung over the tier, a quiet holder's Guard"

                Expect.isLessThan
                    (raidGuard |> Option.map (fun e -> e.Priority) |> Option.defaultValue 0)
                    (entry |> Option.map (fun e -> e.Priority) |> Option.defaultValue 0)
                    "and under a raided outpost's Guard: defence first (#528)"

                Expect.isSome
                    (pooledOf (declaredRaid [ diggerAt "W1N1" { X = 25; Y = 5 } ])
                     |> entryFor (Guard "W1N1"))
                    "and in the home"
            }

            test "a 23C23M player hauler in our outpost pools it too: it carries our energy off" {
                let hauler =
                    { hostileIn
                          "W1N2"
                          { X = 25; Y = 44 }
                          (List.replicate 23 Carry @ List.replicate 23 Move) with
                        Owner = "Trepidimous"
                    }

                Expect.isSome
                    (pooledOf (declaredRaid [ hauler ]) |> entryFor (Guard "W1N2"))
                    "the outpost's Guard is pooled for it"
            }

            test
                "a hauler two from our container is waited for at the container, shot in range, and never followed out" {
                let container = { X = 25; Y = 41 }

                let haulerIn room pos =
                    { hostileIn room pos (List.replicate 23 Carry @ List.replicate 23 Move) with
                        Owner = "Trepidimous"
                    }

                let waiting =
                    declaredRaid [ haulerIn "W1N2" { X = 27; Y = 43 } ]
                    |> withGuards [ ranger "r-9", { X = 25; Y = 47 } ]

                let ground = groundOf waiting

                Expect.isNonEmpty ground "the premise: the kill has ground"

                Expect.isTrue
                    (ground
                     |> Set.forall (fun tile ->
                         rangeTo container tile <= 2 && rangeTo container tile > 0))
                    "its ground is about the container, off it, and not on the hauler's heels"

                Expect.equal
                    (Map.tryFind "r-9" (decideOn waiting).Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the ranger takes it"

                let inRange =
                    declaredRaid [ haulerIn "W1N2" { X = 27; Y = 43 } ]
                    |> withGuards [ ranger "r-9", { X = 26; Y = 45 } ]

                Expect.contains
                    (decideOn inRange).Intents
                    (RangedAttackCreep("r-9", "h-1"))
                    "and shoots it within three"

                let gone =
                    decide
                        (declaredRaid [ haulerIn "W1N3" { X = 25; Y = 48 } ]
                         |> withGuards [ ranger "r-9", { X = 25; Y = 47 } ])
                        (Map.ofList [ "r-9", taskId (Guard "W1N2") ])
                        Set.empty
                        None

                Expect.notEqual
                    (Map.tryFind "r-9" gone.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "out of the room it is no kill of ours"
            }

            test
                "a hauler is waited for at the store nearest it, and beside itself away from every store" {
                let haulerAt pos =
                    { hostileIn "W1N2" pos (List.replicate 23 Carry @ List.replicate 23 Move) with
                        Owner = "Trepidimous"
                    }

                let withSecond colony =
                    { colony with
                        Spatial =
                            { colony.Spatial with
                                TargetKinds =
                                    colony.Spatial.TargetKinds
                                    |> Map.add "can-two" (Structure BuiltKind.Container)
                            }
                    }
                    |> withOutpostLayer "W1N2" (fun layer ->
                        { layer with
                            TargetPositions =
                                Map.add "can-two" { X = 29; Y = 47 } layer.TargetPositions
                        })

                let near = groundOf (declaredRaid [ haulerAt { X = 27; Y = 43 } ] |> withSecond)

                Expect.isNonEmpty near "the premise: the kill has ground"

                Expect.isTrue
                    (near |> Set.forall (fun tile -> rangeTo { X = 25; Y = 41 } tile <= 2))
                    "about the container two from it, and not the one four off"

                let away = { X = 21; Y = 48 }
                let far = groundOf (declaredRaid [ haulerAt away ])

                Expect.isNonEmpty far "the premise: the kill has ground"

                Expect.isTrue
                    (far |> Set.forall (fun tile -> rangeTo away tile = 1))
                    "seven from the one store it draws from none, and is the ground's own"
            }

            // Live W17S26 t979,593-980,050 (#550): a hauler parked four from the
            // container laid its store ring under the whole kill, and the guard
            // standing on it waited ~450 ticks beside a digger and a claimer.
            test
                "beside a trespasser the guard catches a hauler is not waited for at its store (#550)" {
                let hauler = haulerOf "a-haul" { X = 29; Y = 44 }
                let beside (pos: Pos) tile = rangeTo pos tile = 1

                let besideAny bodies tile =
                    bodies |> List.exists (fun h -> beside (RoomPos.pos h.Pos) tile)

                for label, rooted in
                    [
                        // The outpost's controller stands at (30,48).
                        "a claimer at the controller", claimerOf "y-claim" { X = 29; Y = 47 }
                        "a digger on its seat", seatedDigger
                        "a digger slower than the guard",
                        { diggerAt "W1N2" { X = 22; Y = 46 } with
                            Id = "d-2"
                        }
                    ] do
                    let ground = groundOf (declaredRaid [ hauler; rooted ])

                    Expect.isNonEmpty ground $"the premise: beside {label} the kill has ground"

                    Expect.isTrue
                        (ground |> Set.forall (besideAny [ hauler; rooted ]))
                        $"beside {label}, every tile of it is beside a trespasser"

                    Expect.isTrue
                        (ground |> Set.exists (beside (RoomPos.pos hauler.Pos)))
                        $"the hauler's own among them, beside {label}"

                let crossing = claimerOf "y-claim" { X = 22; Y = 46 }

                Expect.isTrue
                    (groundOf (declaredRaid [ hauler; crossing ])
                     |> Set.exists (besideAny [ hauler; crossing ] >> not))
                    "a claimer crossing at the guard's pace roots nothing: the hauler's store ring stands"
            }

            test "a claimer heads the kill order among trespassers, behind the raid (#550)" {
                let hauler = haulerOf "a-haul" { X = 27; Y = 46 }
                let claimer = claimerOf "y-claim" { X = 29; Y = 47 }

                let raider =
                    { hostileIn "W1N2" { X = 20; Y = 44 } smallMelee with
                        Id = "z-raid"
                    }

                let colony =
                    declaredRaid [ hauler; claimer ]
                    |> withGuards [ guard "g-1", { X = 28; Y = 46 } ]

                let result = decideOn colony

                Expect.equal
                    (Map.tryFind "g-1" result.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the premise: the guard holds the kill"

                Expect.contains
                    result.Intents
                    (AttackCreep("g-1", "y-claim"))
                    "beside both, it swings at the claimer, the hauler nearer the Post"

                let raided = declaredRaid [ hauler; claimer; raider ]
                let threats = threatsOf raided (Atlas.ofView raided)

                Expect.equal
                    ((Threats.briefOf threats "W1N2").Targets.Order
                        "W1N2"
                        (fun _ -> 0)
                        [ hauler; claimer; raider ]
                     |> List.map (fun h -> h.Id))
                    [ "z-raid"; "y-claim"; "a-haul" ]
                    "the raid first, then the claimer, then the rest"
            }

            test
                "each trespasser is answered by a row that can strike it: the guard the rooted and the slow, the ranger the rest (#550)" {
                let hauler = haulerOf "a-haul" { X = 29; Y = 44 }

                // The asks and the guards' cap, as a colony whose creeps hold
                // these Tasks reads them.
                let readHolding (holding: string list) colony =
                    let threats =
                        threatsOfHeld
                            colony
                            (Atlas.ofView colony)
                            (heldTaskFacts
                                colony
                                (holding
                                 |> List.map (fun name -> name, taskId (Guard "W1N2"))
                                 |> Map.ofList))

                    let brief = Threats.briefOf threats "W1N2"

                    brief.Asks
                    |> List.map (fun ask -> ask.Row, ask.Wanted threats)
                    |> List.filter (fun (_, wanted) -> wanted > 0),
                    Map.tryFind (CapScope.Row CastRow.Guard) (brief.Capacity threats).Caps

                let asksOf colony = fst (readHolding [] colony)

                let lending colony =
                    colony |> withGuards [ ranger "r-9", { X = 28; Y = 47 } ]

                Expect.equal
                    (asksOf (declaredRaid [ hauler; seatedDigger ]))
                    [ CastRow.Guard, 2; CastRow.Ranger, 1 ]
                    "the digger asks the guard row, the hauler at its pace the ranger's, and the guard's too while no ranger holds the kill"

                Expect.equal
                    (asksOf
                        { declaredRaid [ hauler ] with
                            Bank = bank 550 550
                        })
                    [ CastRow.Guard, 1 ]
                    "with no ranger block castable and none to lend, the guard's alone"

                Expect.equal
                    (readHolding [ "r-9" ] (declaredRaid [ hauler; seatedDigger ] |> lending))
                    ([ CastRow.Guard, 1; CastRow.Ranger, 1 ], Some 1)
                    "a ranger holding the kill takes the hauler off the guard row"

                Expect.equal
                    (readHolding
                        []
                        (declaredRaid [ hauler ] |> withBodyAtHome (ranger "r-9") atSpawn))
                    ([ CastRow.Guard, 1; CastRow.Ranger, 1 ], Some 1)
                    "a lendable ranger elsewhere takes nothing off the guard row"

                // The ranger's lend reads its home's quiet and where it stands,
                // which may flip tick to tick; the guard's seat reads neither.
                Expect.equal
                    (snd (readHolding [] (declaredRaid [ hauler ] |> lending)))
                    (snd (readHolding [] (declaredRaid [ hauler ])))
                    "the guard's cap is one with a ranger to lend or none"

                let haulers = [ for i in 1..4 -> haulerOf $"haul-{i}" { X = 20 + i; Y = 44 } ]

                Expect.equal
                    (asksOf (declaredRaid haulers))
                    [ CastRow.Guard, Engine.guardCap; CastRow.Ranger, Engine.guardCap ]
                    "four haulers buy no more than a raid's room of bodies a row"

                let holders held colony =
                    (decide colony held Set.empty None).Assignments
                    |> Map.filter (fun _ task -> task = taskId (Guard "W1N2"))
                    |> Map.keys
                    |> Set.ofSeq

                Expect.equal
                    (holders
                        Map.empty
                        (declaredRaid [ hauler; seatedDigger ]
                         |> withGuards
                             [ guard "g-1", { X = 27; Y = 47 }; ranger "r-9", { X = 28; Y = 47 } ]))
                    (Set.ofList [ "g-1"; "r-9" ])
                    "a guard and a ranger hold the kill together"

                Expect.equal
                    (holders
                        (Map.ofList [ "r-9", taskId (Guard "W1N2") ])
                        (declaredRaid [ hauler; seatedDigger ]
                         |> withGuards
                             [
                                 guard "g-1", { X = 27; Y = 47 }
                                 guard "g-2", { X = 26; Y = 47 }
                                 ranger "r-9", { X = 28; Y = 47 }
                             ])
                     |> Set.count)
                    2
                    "beside the ranger holding it, a second guard is no answer to the hauler"
            }

            test "a body on the exit row of the room beyond keeps the kill across the bounce" {
                let colony = raisingBeyond [ diggerAt "W1N3" { X = 25; Y = 49 } ]
                let ground = groundOf colony

                Expect.isSome
                    (pooledOf colony |> entryFor (Guard "W1N2"))
                    "the outpost's Guard is pooled for the body the exit carries in"

                Expect.isTrue
                    (not (Set.isEmpty ground)
                     && ground |> Set.forall (fun tile -> tile.Y = 1 && abs (tile.X - 25) <= 1))
                    "beside where it lands, off the exit"
            }

            test
                "defence first: a fresh ranger takes the raided child's Guard, not a trespass at home" {
                let raid =
                    { hostileIn "W1N3" { X = 40; Y = 40 } smallMelee with
                        Id = "h-2"
                    }

                let colony =
                    raisingBeyond [ diggerAt "W1N1" { X = 25; Y = 5 }; raid ]
                    |> withBodyAtHome (ranger "r-5") atSpawn

                let result = decide colony residentHeld Set.empty None

                Expect.equal
                    (Map.tryFind "r-5" result.Assignments)
                    (Some(taskId (Guard "W1N3")))
                    "the raid is answered first"

                let quiet =
                    decide
                        (raisingBeyond [ diggerAt "W1N1" { X = 25; Y = 5 } ]
                         |> withBodyAtHome (ranger "r-5") atSpawn)
                        residentHeld
                        Set.empty
                        None

                Expect.equal
                    (Map.tryFind "r-5" quiet.Assignments)
                    (Some(taskId (Guard "W1N1")))
                    "the premise: with no raid it takes the trespass"
            }

            test "a neutral player's hauler in our outpost pools nothing" {
                let giaco =
                    { hostileIn
                          "W1N2"
                          { X = 25; Y = 44 }
                          (List.replicate 10 Carry @ List.replicate 10 Move) with
                        Owner = "giaco"
                    }

                Expect.isNone
                    (pooledOf (declaredRaid [ giaco ]) |> entryFor (Guard "W1N2"))
                    "only a declared enemy trespasses"
            }

            test "an ally's body, a scout and an Invader's digger pool none" {
                let guardIn colony =
                    pooledOf colony
                    |> List.exists (fun e ->
                        match e.Task with
                        | Guard _ -> true
                        | _ -> false)

                Expect.isFalse
                    (guardIn
                        { declaredRaid [] with
                            Allies = [ diggerAt "W1N2" { X = 25; Y = 44 } ]
                        })
                    "an ally's digger is no hostile"

                Expect.isFalse
                    (guardIn (declaredRaid [ hostileIn "W1N2" { X = 25; Y = 44 } [ Move ] ]))
                    "a scout digs nothing"

                Expect.isFalse
                    (guardIn (
                        declaredRaid
                            [
                                { diggerAt "W1N2" { X = 25; Y = 44 } with
                                    Owner = "Invader"
                                }
                            ]
                    ))
                    "an Invader's body is the NPC's, no player's"
            }

            test "a melee guard may take it and swings from beside it, and no row casts for it" {
                let at = { X = 25; Y = 44 }

                let colony =
                    declaredRaid [ diggerAt "W1N2" at ]
                    |> withGuards [ guard "g-1", { X = 24; Y = 45 } ]

                let result = decideOn colony

                Expect.equal
                    (Map.tryFind "g-1" result.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the guard takes the kill"

                Expect.contains (attacksOf result.Intents) ("g-1", "h-1") "and swings"

                Expect.equal (guardQuotaOf colony) (Some 0) "no guard is cast for it"

                Expect.equal
                    (rowOf "ranger" colony |> Option.map (fun row -> row.Quota))
                    (Some 0)
                    "nor a ranger"
            }

            test "nobody qualifying, nothing takes it" {
                let colony = declaredRaid [ diggerAt "W1N2" { X = 25; Y = 44 } ]

                Expect.isFalse
                    ((decideOn colony).Assignments
                     |> Map.exists (fun _ tid -> tid = taskId (Guard "W1N2")))
                    "the anchor and the haulers are no fighters"
            }

            test "a ranger shoots it from three" {
                let colony =
                    declaredRaid [ diggerAt "W1N2" { X = 25; Y = 44 } ]
                    |> withGuards [ ranger "r-9", { X = 25; Y = 47 } ]

                let result = decideOn colony

                Expect.equal
                    (Map.tryFind "r-9" result.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the ranger takes the kill"

                Expect.contains result.Intents (RangedAttackCreep("r-9", "h-1")) "and shoots"
            }

            test
                "a resident of the bordering home takes it while home is quiet, and walks back once it dies" {
                let called =
                    decide
                        (raisingBeyond [ diggerAt "W1N2" { X = 25; Y = 10 } ])
                        residentHeld
                        Set.empty
                        None

                Expect.contains
                    called.Verdicts
                    (Verdict.Released("r-1", taskId (Guard "W1N3"), ReleaseReason.Called))
                    "the resident is let go of its quiet ring"

                Expect.equal
                    (Map.tryFind "r-1" called.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "and takes the kill"

                let after =
                    raisingBeyond []
                    |> standingIn "W1N2" ("r-1", { X = 25; Y = 12 })
                    |> withOutpostLayer "W1N3" (fun layer ->
                        { layer with
                            CreepPositions = Map.empty
                        })

                let back = decide after (Map.ofList [ "r-1", taskId (Guard "W1N2") ]) Set.empty None

                Expect.equal
                    (Map.tryFind "r-1" back.Assignments)
                    (Some(taskId (Guard "W1N3")))
                    "with the trespasser dead it holds its home's ring again"
            }

            test "a resident is not sent to a room beside our home and not its own" {
                let result =
                    decide
                        (raisingBeyond [ diggerAt "W1N1" { X = 25; Y = 5 } ])
                        residentHeld
                        Set.empty
                        None

                Expect.equal
                    (Map.tryFind "r-1" result.Assignments)
                    (Some(taskId (Guard "W1N3")))
                    "W1N1 does not border W1N3"
            }

            test
                "an armed body at home keeps the resident home, and recalls it from the kill at once" {
                let raid =
                    { hostileIn "W1N3" { X = 40; Y = 40 } smallMelee with
                        Id = "h-2"
                    }

                let kept =
                    decide
                        (raisingBeyond [ diggerAt "W1N2" { X = 25; Y = 10 }; raid ])
                        residentHeld
                        Set.empty
                        None

                Expect.equal
                    (Map.tryFind "r-1" kept.Assignments)
                    (Some(taskId (Guard "W1N3")))
                    "a raid in its home keeps it there"

                let recalled =
                    decide
                        (raisingBeyond [ diggerAt "W1N2" { X = 25; Y = 10 }; raid ]
                         |> standingIn "W1N2" ("r-1", { X = 25; Y = 12 })
                         |> withOutpostLayer "W1N3" (fun layer ->
                             { layer with
                                 CreepPositions = Map.empty
                             }))
                        (Map.ofList [ "r-1", taskId (Guard "W1N2") ])
                        Set.empty
                        None

                Expect.contains
                    recalled.Verdicts
                    (Verdict.Released(
                        "r-1",
                        taskId (Guard "W1N2"),
                        ReleaseReason.Rejected RejectReason.Inapplicable
                    ))
                    "a raid arriving there lets it go of the kill, still pooled"

                Expect.equal
                    (Map.tryFind "r-1" recalled.Assignments)
                    (Some(taskId (Guard "W1N3")))
                    "and it holds its home again"
            }

            test
                "a raid in outpost A withholds no trespass kill in B from a guard A leaves spare (#528)" {
                let raid = hostileIn "W1N2" { X = 25; Y = 42 } smallMelee

                let digger =
                    { diggerAt "W1N3" { X = 25; Y = 20 } with
                        Id = "h-2"
                    }

                let colony =
                    secondOutpost
                        [ raid; digger ]
                        [ guard "g-1", beside; guard "g-2", { X = 25; Y = 30 } ]

                let result =
                    decide colony (Map.ofList [ "g-1", taskId (Guard "W1N2") ]) Set.empty None

                Expect.equal
                    (Map.tryFind "g-1" result.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the raid's guard holds the raid"

                Expect.equal
                    (Map.tryFind "g-2" result.Assignments)
                    (Some(taskId (Guard "W1N3")))
                    "and the spare guard takes B's trespasser"
            }

            test "defence first: a lone guard takes outpost A's raid over B's trespasser (#528)" {
                let raid = hostileIn "W1N2" { X = 25; Y = 42 } smallMelee

                let digger =
                    { diggerAt "W1N3" { X = 25; Y = 20 } with
                        Id = "h-2"
                    }

                let colony = secondOutpost [ raid; digger ] [ guard "g-1", { X = 25; Y = 30 } ]

                Expect.equal
                    (Map.tryFind "g-1" (decideOn colony).Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the raid's Guard ranks over the trespass's kill"
            }

            test "a declared enemy's body shot disarmed in our outpost is a trespasser (#528)" {
                let colony =
                    declaredRaid [ disarmedAt "Trepidimous" "W1N2" { X = 25; Y = 44 } ]
                    |> withGuards [ guard "g-1", { X = 25; Y = 47 } ]

                let atlas = Atlas.ofView colony

                Expect.equal
                    ((Threats.briefOf (threatsOf colony atlas) "W1N2").Duty)
                    (Duty.Guard BriefKind.Trespass)
                    "its Guard is the trespass's kill"

                Expect.isSome (pooledOf colony |> entryFor (Guard "W1N2")) "pooled"

                Expect.equal
                    (Map.tryFind "g-1" (decideOn colony).Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "and taken"
            }

            test "a guard three off its trespasser aims at it, for the light ticks to swing (#528)" {
                let colony =
                    declaredRaid [ diggerAt "W1N2" { X = 25; Y = 44 } ]
                    |> withGuards [ guard "g-1", { X = 25; Y = 47 } ]

                let result = decideOn colony

                Expect.equal
                    (Map.tryFind "g-1" result.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the premise: it holds the kill"

                Expect.isFalse
                    (List.contains (AttackCreep("g-1", "h-1")) result.Intents)
                    "out of reach, no swing this tick"

                Expect.equal result.Aims [ AttackCreep("g-1", "h-1") ] "but its aim is recorded"

                Expect.isEmpty (decideOn (declaredRaid [])).Aims "a quiet colony aims at nothing"
            }

            // #535 made the disarmed clause owner-agnostic (any non-ally's): a
            // cripple left in a room we mine is a fight's to finish, whoever's.
            test
                "a neutral's disarmed body is a trespasser too, an ally's and a bare enemy scout are not (#528, #535)" {
                Expect.isSome
                    (pooledOf (declaredRaid [ disarmedAt "giaco" "W1N2" { X = 25; Y = 44 } ])
                     |> entryFor (Guard "W1N2"))
                    "a neutral's cripple in our outpost"

                Expect.isNone
                    (pooledOf (declaredRaid [ disarmedAt "Odiodin" "W1N2" { X = 25; Y = 44 } ])
                     |> entryFor (Guard "W1N2"))
                    "never an ally's"

                Expect.isNone
                    (pooledOf (
                        declaredRaid
                            [
                                { hostileIn "W1N2" { X = 25; Y = 44 } [ Move ] with
                                    Owner = "Trepidimous"
                                }
                            ]
                     )
                     |> entryFor (Guard "W1N2"))
                    "a scout carried no weapon to lose"
            }
        ]

[<Tests>]
let residentChaseTests =
    testList
        "a resident lent to a chase beside its home (#516)"
        [
            test
                "the W18S25 shape: a crippled melee next door calls the quiet resident, and a raid at home recalls it" {
                let called =
                    decide
                        (raisingBeyond [ crippledAt { X = 25; Y = 10 } ])
                        residentHeld
                        Set.empty
                        None

                Expect.contains
                    called.Verdicts
                    (Verdict.Released("r-1", taskId (Guard "W1N3"), ReleaseReason.Called))
                    "the resident is let go of its quiet ring"

                Expect.equal
                    (Map.tryFind "r-1" called.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "and takes the chase"

                let raid =
                    { hostileIn "W1N3" { X = 40; Y = 40 } smallMelee with
                        Id = "h-2"
                    }

                let recalled =
                    decide
                        (raisingBeyond [ crippledAt { X = 25; Y = 10 }; raid ]
                         |> standingIn "W1N2" ("r-1", { X = 25; Y = 14 })
                         |> withOutpostLayer "W1N3" (fun layer ->
                             { layer with
                                 CreepPositions = Map.empty
                             }))
                        (Map.ofList [ "r-1", taskId (Guard "W1N2") ])
                        Set.empty
                        None

                Expect.equal
                    (Map.tryFind "r-1" recalled.Assignments)
                    (Some(taskId (Guard "W1N3")))
                    "a raid at home calls it back off the chase"
            }

            test
                "a healer bouncing on the home's exit keeps the resident home on the tick it is across" {
                // #552, live t981,3xx: the resident was called to W18S25 each
                // tick the pair stood across the border from W17S25, and
                // released `task-gone` the next.
                let bouncing =
                    { raisingBeyond [ crippledAt { X = 25; Y = 10 } ] with
                        RecalledHealers =
                            [
                                { hostileIn "W1N3" { X = 25; Y = 49 } [ Heal; Move ] with
                                    Id = "med-1"
                                    Owner = "Trepidimous"
                                }
                            ]
                    }

                let held = decide bouncing residentHeld Set.empty None

                Expect.equal
                    (Map.tryFind "r-1" held.Assignments)
                    (Some(taskId (Guard "W1N3")))
                    "the home is not quiet: no lend"
            }
        ]

/// `raisingBeyond` with W1N4 beyond the child's home: the child's outpost,
/// projected by this colony as a transit room for its resident (#517) — no
/// furniture of it, its ground and bodies alone.
let private watchingBeyond hostiles =
    let colony = raisingBeyond hostiles

    let floor =
        TerrainGrid.ofList
            [
                for x in 1..48 do
                    for y in 1..48 -> { X = x; Y = y }, Plain
            ]

    { colony with
        Borrowed =
            { colony.Borrowed with
                ChildOutposts = [ "W1N4" ]
            }
        Crossed = Set.add "W1N4" colony.Crossed
        Spatial =
            { colony.Spatial with
                Borders = colony.Spatial.Borders |> Map.add "W1N4" plainRing
            }
            |> withNeighbour "W1N4" { RoomLayer.empty with Terrain = floor }
    }

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
let private judgeOverFleet hops held (colonies: Facts.Judged list) =
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
    |> Facts.judge hops held

/// The mother's view and the child's with the world's verdicts priced over
/// both (`Facts.judge`), a crossing a room apart.
let private judgedBoth (mother: ColonyView) (child: ColonyView) =
    let crossings (home: string) (room: string) =
        RoomName.offsetOf home room
        |> Option.map (fun (dx, dy) -> max (abs dx) (abs dy))

    let deciding view : Facts.Judged =
        {
            Cut = view
            Owns = Set.empty
            Decides = true
        }

    match judgeOverFleet crossings Map.empty [ deciding mother; deciding child ] with
    | [ mother; child ] -> mother, child
    | _ -> failwith "two views in, two out"

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

            test "a melee guard of ours is not sent across the child's border for it" {
                let colony = guardedBeyond [ diggerAt "W1N4" { X = 25; Y = 42 } ]

                Expect.isSome
                    (pooledOf colony |> entryFor (Guard "W1N4"))
                    "the premise: the kill is pooled"

                Expect.notEqual
                    (Map.tryFind "g-1" (decideOn colony).Assignments)
                    (Some(taskId (Guard "W1N4")))
                    "the child's outpost is the child's guard's, and her resident's"
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
                    (guardBlocksBeat mother "W1N4" (guardBlocksReach mother))
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
                    (guardBlocksBeat (child mother) "W1N4" (guardBlocksReach (child mother)))
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
                "a raid only the child's ranger and the mother's resident win together is one Win, cast for by one colony and fought by both" {
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

                Expect.equal
                    (Facts.verdictOf judged "W1N4").Answerer
                    (Some "W1N3")
                    "answered by one colony: the child, a crossing from it where the mother is three"

                Expect.isEmpty (shut judged) "and the child stands nothing down"

                // The world layer's S4: both colonies decide off the judged
                // views. Each colony the `Win` counts pools the room's Guard
                // and sends its own body, so neither sends half the answer;
                // only the answerer casts for it.
                let judgedMother, judgedChild = judgedBoth mother child
                let held = Map.ofList [ "r-1", taskId (Guard "W1N3") ]

                Expect.isSome
                    (pooledOf judgedChild |> entryFor (Guard "W1N4"))
                    "the child, the answerer, pools guard:W1N4"

                Expect.isSome
                    (pooledOf judgedMother |> entryFor (Guard "W1N4"))
                    "the mother, whose resident it counts, pools it too"

                let briefIn (view: ColonyView) =
                    Threats.briefOf (threatsOf view (Atlas.ofView view)) "W1N4"

                Expect.equal
                    [
                        Threats.answers judgedChild (briefIn judgedChild)
                        Threats.answers judgedMother (briefIn judgedMother)
                    ]
                    [ true; false ]
                    "and only the child casts for it"

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
            // S3 the residents are priced in the mother's view alone, by her
            // rule (`Facts.judge`).
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
                // outpost, and W2N3.
                let mother hostiles =
                    { watchingBeyond hostiles with
                        Bank = bank 5600 5600
                    }
                    |> fun colony ->
                        { colony with
                            Creeps =
                                (colony.Creeps |> List.filter (fun creep -> creep.Name <> "r-1"))
                                @ [ resident "r-1"; resident "r-2" ]
                        }
                    |> standingIn "W1N3" ("r-2", { X = 27; Y = 26 })

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

                Expect.isNone (Facts.verdictOf judged "W1N4").Answerer "answered by nobody"

                Expect.equal (shutOf judged) (Set.singleton "W1N4") "so the child stands it down"
            }

            // The world layer's S4: the answerer's binding never strips a
            // room of the colony whose own Guard it is. The child's row loses
            // her outpost, the mother's resident beside it wins it alone, and
            // the mother answers; the child still pools her outpost's Guard
            // and casts for it, or nobody of hers would guard the room her
            // miners keep working.
            test
                "a neighbour's ranger winning the child's outpost alone leaves the child pooling and casting for it" {
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
                    (Facts.verdictOf child "W1N4").Answerer
                    (Some "W1N1")
                    "the premise: the mother's resident wins it alone, and she answers"

                let briefIn (view: ColonyView) =
                    Threats.briefOf (threatsOf view (Atlas.ofView view)) "W1N4"

                Expect.isSome
                    (pooledOf child |> entryFor (Guard "W1N4"))
                    "the child pools her outpost's Guard"

                Expect.isTrue
                    (Threats.answers child (briefIn child))
                    "and casts for it, whoever answers"

                Expect.isSome
                    (pooledOf mother |> entryFor (Guard "W1N4"))
                    "the mother, the answerer, pools it for her resident"
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

                let crossings (home: string) (room: string) =
                    RoomName.offsetOf home room
                    |> Option.map (fun (dx, dy) -> max (abs dx) (abs dy))

                let _, child =
                    match
                        judgeOverFleet
                            crossings
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
                    (Facts.answer child "W1N4", (Facts.verdictOf child "W1N4").Answerer)
                    (Answer.Win, Some "W1N3")
                    "the Win binds the child, whose row casts for her own room, shut or not"

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

                let crossings (home: string) (room: string) =
                    RoomName.offsetOf home room
                    |> Option.map (fun (dx, dy) -> max (abs dx) (abs dy))

                let judgedWith (mother: ColonyView) (child: ColonyView) =
                    match
                        judgeOverFleet
                            crossings
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
                            (fun _ _ -> None)
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
                let mother =
                    (raisingBeyond [])
                    |> fun colony ->
                        { colony with
                            Creeps =
                                (colony.Creeps |> List.filter (fun creep -> creep.Name <> "r-1"))
                                @ [ smallResident "r-1" ]
                        }

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
                "a neighbour's ranger beside a child's home it does not work leaves the mother's resident outmatched" {
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
                        (fun _ _ -> None)
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
                        (fun _ _ -> None)
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
            // and its garrison's Guard, two Tasks (#552), so no answerer binds
            // it: the mother keeps her garrison's Guard though the child's
            // rangers alone are the fewest that win.
            test
                "a raid at a raised child's home her rangers win binds no answerer: the mother keeps her garrison's Guard" {
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
                // beyond it, W2N3 on its other side.
                let mother others =
                    { watchingBeyond (harasser :: others) with
                        Bank = bank 5600 5600
                    }
                    |> fun colony ->
                        { colony with
                            Creeps =
                                (colony.Creeps |> List.filter (fun creep -> creep.Name <> "r-1"))
                                @ [ resident "r-1"; resident "r-2" ]
                        }
                    |> standingIn "W1N3" ("r-2", { X = 27; Y = 26 })

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
                                @ [ resident "r-1"; resident "r-2" ]
                        }
                    |> standingIn "W1N3" ("r-2", { X = 27; Y = 26 })

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
                                @ [ resident "r-1"; resident "r-2" ]
                        }
                    |> standingIn "W1N3" ("r-2", { X = 27; Y = 26 })

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

/// The tiles beside one tile of W1N2, the tile itself left out.
let private besideIn (at: Pos) =
    [
        for dx in -1 .. 1 do
            for dy in -1 .. 1 do
                if dx <> 0 || dy <> 0 then
                    RoomPos.at "W1N2" { X = at.X + dx; Y = at.Y + dy }
    ]
    |> Set.ofList

// Characterisation (the Brief's S0): each outpost Guard flavour's pooled rank and
// capacity, the body it admits and refuses, its ground, its act and its row,
// pinned as today's code answers so the Brief stages can show they keep it.
// The trespass's kill is pinned whole by `trespassTests`, the chase's ground,
// act and takers by `kiteTests` and `shutChaseTests`.
[<Tests>]
let guardFlavourTests =
    testList
        "one outpost's Guard, flavour by flavour"
        [
            test
                "a raid: the guard row's body on the raider's ring, two rungs up, the room's own count" {
                let colony =
                    declaredRaid raiders
                    |> withGuards [ guard "g-1", beside; ranger "r-1", { X = 21; Y = 47 } ]

                Expect.equal
                    (pooledHolding [] (Guard "W1N2") colony)
                    (Some(safetyRank TwoRungsUp, Capacity.fighters 1))
                    "one Guard over every kill (#528), for the one body the raid wants"

                Expect.equal
                    (Threats.guardGroundIn (threatsOf colony (Atlas.ofView colony)) "W1N2")
                    (Some(besideIn { X = 25; Y = 42 }))
                    "its ground is the raider's ring"

                let decision = decideOn colony

                Expect.equal
                    (Map.tryFind "g-1" decision.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the guard row's body takes it"

                let alone = declaredRaid raiders |> withGuards [ ranger "r-1", { X = 21; Y = 47 } ]

                Expect.notEqual
                    ((decideOn alone).Assignments |> Map.tryFind "r-1")
                    (Some(taskId (Guard "W1N2")))
                    "a ranger is refused an outpost's Guard, the only body there"

                Expect.contains decision.Intents (AttackCreep("g-1", "h-1")) "and the guard swings"
                Expect.equal (guardQuotaOf colony) (Some 1) "the guard row counts the room's one"
            }

            test "a held exit: kept for the guard holding it, on the tier, and no row hires for it" {
                let colony =
                    declaredRaid []
                    |> westField
                    |> withGuards
                        [ guard "g-1", { X = 1; Y = 44 }; ranger "r-1", { X = 3; Y = 46 } ]
                    |> heldWest

                let held = [ "g-1", Guard "W1N2" ]

                Expect.equal
                    (pooledHolding held (Guard "W1N2") colony)
                    (Some(safetyRank OnTheTier, Capacity.fighters 1))
                    "pooled on the tier while its guard holds it"

                Expect.isNone
                    (pooledHolding [] (Guard "W1N2") colony)
                    "and not for a guard that does not hold it yet"

                let decision =
                    decideFrom
                        (held |> List.map (fun (name, task) -> name, taskId task) |> Map.ofList)
                        colony

                let rangerHeld =
                    declaredRaid []
                    |> westField
                    |> withGuards [ ranger "r-1", { X = 1; Y = 44 } ]
                    |> heldWest
                    |> decideFrom (Map.ofList [ "r-1", taskId (Guard "W1N2") ])

                Expect.notEqual
                    (Map.tryFind "r-1" rangerHeld.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "a ranger holding the hold, the only body there, is let go of it"

                Expect.isFalse
                    (decision.Intents
                     |> List.exists (function
                         | AttackCreep("g-1", _) -> true
                         | _ -> false))
                    "nothing in the room to swing at"

                Expect.equal (guardQuotaOf colony) (Some 0) "the guard row hires none"
            }

            test "a chase: one ranger, a raid's two rungs up in sight, and no row casts for it" {
                let colony =
                    declaredRaid [ crippledAt { X = 25; Y = 44 } ]
                    |> withGuards
                        [ ranger "r-1", { X = 22; Y = 47 }; guard "g-1", { X = 20; Y = 41 } ]

                Expect.equal
                    (pooledHolding [] (Guard "W1N2") colony)
                    (Some(safetyRank TwoRungsUp, Capacity.fighters 1))
                    "one kill, over a trespass's (#528)"

                Expect.equal
                    (rowOf "ranger" colony |> Option.map (fun row -> row.Quota))
                    (Some 0)
                    "the ranger row casts nobody for it"

                Expect.equal (guardQuotaOf colony) (Some 0) "nor the guard row"
            }

            // The Brief's S7, rule 1: a hostile no heal reaches is read live. This
            // was the quirk "a raider whose weapons are shot off keeps its Reach
            // and its Guard, priced on its paper", where the stand-down already
            // read it live and reopened the room.
            // #535: no raid's Guard any more, but a cripple left in a room we
            // mine is a trespass's kill, one rung up, not a raid's two.
            test
                "a raider whose weapons are shot off, alone, has no Reach and no raid's Guard: it is a trespass kill" {
                let disarmed =
                    { hostileIn "W1N2" { X = 25; Y = 42 } smallMelee with
                        Id = "disarmed"
                        Hits = 50
                    }

                Expect.equal
                    (Engine.liveParts disarmed.Body disarmed.Hits)
                    [ Move ]
                    "the premise: its last MOVE alone still acts"

                let colony =
                    { declaredRaid [ disarmed ] with
                        Bank = bank 800 800
                    }
                    |> withGuards
                        [ ranger "r-1", { X = 22; Y = 47 }; guard "g-1", { X = 20; Y = 41 } ]

                Expect.isEmpty
                    (Threats.reachIn (threatsOf colony (Atlas.ofView colony)) "W1N2")
                    "no Reach"

                Expect.equal
                    (pooledHolding [] (Guard "W1N2") colony |> Option.map fst)
                    (Some(safetyRank OneRungUp))
                    "its Guard is a kill's, one rung up"

                Expect.equal
                    ((Threats.briefOf (threatsOf colony (Atlas.ofView colony)) "W1N2").Duty)
                    (Duty.Guard BriefKind.Trespass)
                    "a trespass's"

                let shut =
                    Observe.RaidState.empty
                    |> Observe.foldRaids
                        Observe.capEpisodes
                        Set.empty
                        colony
                        (Planner.outpostFactsOf colony (Atlas.ofView colony)).Declared
                    |> Observe.standDown Tuning.defaults (colony.Time + 1)
                    |> fun gate -> gate.Shut

                Expect.isEmpty shut "and it stands nothing down"
            }

            // The Brief's S7, rule 1: healing reaches it, so its parts come back
            // and it is read on paper, whole.
            test "the same raider beside its side's healer is still a threat, priced whole" {
                let disarmed =
                    { hostileIn "W1N2" { X = 25; Y = 42 } smallMelee with
                        Id = "disarmed"
                        Hits = 50
                    }

                let medic =
                    { hostileIn "W1N2" { X = 27; Y = 43 } smallHealer with
                        Id = "medic"
                    }

                let colony =
                    { declaredRaid [ disarmed; medic ] with
                        Bank = bank 800 800
                    }
                    |> withGuards [ guard "g-1", { X = 20; Y = 41 } ]

                Expect.isNonEmpty
                    (Threats.reachIn (threatsOf colony (Atlas.ofView colony)) "W1N2")
                    "it keeps a Reach"

                Expect.isTrue
                    (HostileInfo.armed colony.Hostiles disarmed)
                    "and is armed, its ATTACK and RANGED_ATTACK healed back"

                Expect.isSome (pooledHolding [] (Guard "W1N2") colony) "the room's Guard is pooled"
            }
        ]

/// One ingredient of a room's Guard, laid into `westField`'s W1N2 on tiles
/// apart from every other's.
type private Flavour =
    | Raid
    | Core
    | Hold
    | Trespasser
    | Resident
    | Harassed
    | ChaseSeen
    | ChaseDark

let private withFlavour (colony: ColonyView) flavour =
    match flavour with
    | Raid ->
        { colony with
            Hostiles =
                colony.Hostiles
                @ [
                    { hostileIn "W1N2" { X = 20; Y = 44 } smallMelee with
                        Id = "raider"
                    }
                ]
        }
    | Core ->
        { colony with
            InvaderCores =
                [
                    {
                        Id = "core-W1N2"
                        RoomName = "W1N2"
                        Tile = { X = 15; Y = 44 }
                        CollapseTick = None
                        Level = 0
                    }
                ]
        }
    | Hold -> heldWest colony
    | Trespasser ->
        { colony with
            Hostiles =
                colony.Hostiles
                @ [
                    { hostileIn
                          "W1N2"
                          { X = 10; Y = 47 }
                          (List.replicate 23 Carry @ List.replicate 23 Move) with
                        Id = "hauler"
                        Owner = "Shibdib"
                    }
                ]
        }
    | Resident ->
        { colony with
            Errands =
                [
                    { reactorErrand with
                        Target = "reactor-1", RoomPos.at "W1N2" { X = 6; Y = 46 }
                    }
                ]
        }
    | Harassed ->
        colony
        |> casting
            {
                RoomName = "W1N2"
                Enemy = "Trepidimous"
                Stand = RoomPos.at "W1N2" { X = 12; Y = 42 }
                Controller = RoomPos.at "W1N2" { X = 30; Y = 48 }
                Via = []
                Blocks = None
            }
    | ChaseSeen ->
        { colony with
            Hostiles =
                colony.Hostiles
                @ [
                    { plodderAt { X = 25; Y = 46 } with
                        Id = "plodder"
                    }
                ]
        }
    | ChaseDark ->
        { colony with
            ChasedOutposts = Set.add "W1N2" colony.ChasedOutposts
        }

/// `westField`'s outpost with a guard and a ranger of ours standing in it,
/// apart from every flavour's tiles.
let private precedenceBasis =
    { declaredRaid [] with
        // An RCL5 bank: two ranger blocks, which win the smallMelee the
        // verdict (the Brief's S7) weighs them against, and two guard blocks,
        // which lose the plodder, so it stays a chase.
        Bank = bank 1800 1800
    }
    |> westField
    |> withGuards [ guard "g-1", { X = 3; Y = 43 }; ranger "r-1", { X = 28; Y = 42 } ]

/// One room's Guard Brief as a colony whose creeps hold these Tasks reads it.
let private heldBriefOf (holding: (string * Task) list) (room: string) (colony: ColonyView) =
    let atlas = Atlas.ofView colony

    let held =
        heldTaskFacts
            colony
            (holding |> List.map (fun (name, task) -> name, taskId task) |> Map.ofList)

    Threats.briefOf (threatsOfHeld colony atlas held) room

/// What one mix of flavours answers for a room's Guard.
type private GuardAnswer =
    {
        /// The flavours whose ground alone the mix's ground equals.
        Ground: Flavour list
        /// The `Threats` fields whose entry for the room is that ground.
        From: string list
        /// Its rank and capacity, where the Planner pools it.
        Pooled: (int * Capacity) option
        /// Who holds it after the tick.
        Takers: string list
    }

[<Tests>]
let guardPrecedenceTests =
    testList
        "which flavour answers one room's Guard"
        [
            // Characterisation (the Brief's S0): each flavour's own ground, in
            // tiles, so the mixes below can name a winner by its ground alone
            // and a change to that ground cannot hide behind the comparison.
            test "each flavour's own ground" {
                let groundOf flavour =
                    let colony = withFlavour precedenceBasis flavour
                    Threats.guardGroundIn (threatsOf colony (Atlas.ofView colony)) "W1N2"

                let tiles (pairs: (int * int) list) =
                    pairs
                    |> List.map (fun (x, y) -> RoomPos.at "W1N2" { X = x; Y = y })
                    |> Set.ofList

                Expect.equal
                    ([ Raid; Core; Hold; Trespasser; Resident; Harassed; ChaseSeen; ChaseDark ]
                     |> List.map (fun flavour -> flavour, groundOf flavour))
                    [
                        Raid, Some(besideIn { X = 20; Y = 44 })
                        Core, Some(besideIn { X = 15; Y = 44 })
                        Hold, Some(tiles [ for y in 41..48 -> 1, y ])
                        // No store within five: the tiles beside the hauler.
                        Trespasser, Some(besideIn { X = 10; Y = 47 })
                        Resident, Some(besideIn { X = 6; Y = 46 })
                        // Nobody at work: the Stand's seats, the Stand among them.
                        Harassed,
                        Some(
                            Set.add
                                (RoomPos.at "W1N2" { X = 12; Y = 42 })
                                (besideIn { X = 12; Y = 42 })
                        )
                        // Three from the plodder, less the corners by the field's
                        // south edge, where too little room is left to back off.
                        // Movement S3 reads the escape room off the maps within
                        // the horizon (two), not a three-step flood: (28,47),
                        // two off the field's east edge, backs onto as many
                        // tiles as (22,47) does, and is kept.
                        ChaseSeen,
                        Some(
                            tiles (
                                [ for y in 43..47 -> 22, y ]
                                @ [ for x in 23..28 -> x, 43 ]
                                @ [ for y in 44..47 -> 28, y ]
                            )
                        )
                        ChaseDark, None
                    ]
                    "each alone"
            }

            // What each flavour's holder walks round on its way (`Brief.Avoid`):
            // a chase's ranger what a melee map strikes before it could be on
            // it and gone (movement S3: the kite, which retired the step and
            // swing about each melee), a trespass's killer the whole Reach,
            // every other the runner's Reach.
            test "each flavour's walk-round arm" {
                let avoidOf flavour =
                    let colony = withFlavour precedenceBasis flavour
                    (Threats.briefOf (threatsOf colony (Atlas.ofView colony)) "W1N2").Avoid

                Expect.equal
                    ([ Raid; Core; Hold; Trespasser; Resident; Harassed; ChaseSeen; ChaseDark ]
                     |> List.map (fun flavour -> flavour, avoidOf flavour))
                    [
                        Raid, Avoid.Runner
                        Core, Avoid.Runner
                        Hold, Avoid.Runner
                        Trespasser, Avoid.Reach
                        Resident, Avoid.Runner
                        Harassed, Avoid.Runner
                        ChaseSeen, Avoid.Kite
                        ChaseDark, Avoid.Kite
                    ]
                    "each alone"
            }

            // Characterisation (the Brief's S0): `guard:<room>` is one Task id over
            // every flavour, and which one answers is the precedence `threatsHeldBy`
            // writes across its exclusion lists and `guardGroundIn`'s order. Each
            // row lays its flavours into one room beside a guard holding its
            // Guard and a ranger, and reads whether the Planner pools the Guard,
            // its ground (named by the flavour whose ground alone it equals, and
            // by the `Threats` field that supplies it), its rank and capacity, and
            // who holds it after the tick. The ground is read as that colony
            // reads it, the guard holding the Guard (#542: a held exit's claim
            // pools only for its holder).
            test "the ground, rank, capacity and taker of each mix of flavours" {
                let held = [ "g-1", Guard "W1N2" ]

                let read flavours =
                    let colony = List.fold withFlavour precedenceBasis flavours
                    let atlas = Atlas.ofView colony

                    let threats =
                        threatsOfHeld
                            colony
                            atlas
                            (heldTaskFacts
                                colony
                                (held
                                 |> List.map (fun (name, task) -> name, taskId task)
                                 |> Map.ofList))

                    let ground = Threats.guardGroundIn threats "W1N2"

                    let groundAlone flavour =
                        let alone = withFlavour precedenceBasis flavour
                        Threats.guardGroundIn (threatsOf alone (Atlas.ofView alone)) "W1N2"

                    let answering =
                        flavours
                        |> List.filter (fun flavour ->
                            Option.isSome ground && groundAlone flavour = ground)

                    let supplying =
                        [
                            "Kite", (Threats.briefOf threats "W1N2").Laid.Kite
                            "ResidentRing", (Threats.briefOf threats "W1N2").Laid.Resident
                            "HarassRing", (Threats.briefOf threats "W1N2").Laid.Harass
                            "Trespass", (Threats.briefOf threats "W1N2").Laid.Trespass
                            "Ring", (Threats.briefOf threats "W1N2").Laid.Ring
                        ]
                        |> List.filter (fun (_, field) -> Option.isSome ground && field = ground)
                        |> List.map fst

                    let takers =
                        (decideFrom
                            (held |> List.map (fun (name, task) -> name, taskId task) |> Map.ofList)
                            colony)
                            .Assignments
                        |> Map.filter (fun _ tid -> tid = taskId (Guard "W1N2"))
                        |> Map.keys
                        |> List.ofSeq

                    {
                        Ground = answering
                        From = supplying
                        Pooled = pooledHolding held (Guard "W1N2") colony
                        Takers = takers
                    }

                // The residents and a relief each.
                let garrison = Capacity.fighters (2 * precedenceBasis.Tuning.RangerResidents)
                let one = Capacity.fighters 1
                let relieved = one |> Capacity.relieving

                let hauled =
                    Capacity.fighters 2
                    |> Capacity.capping (CapScope.Row CastRow.Guard) 1
                    |> Capacity.capping (CapScope.Row CastRow.Ranger) 2

                // The guard's seat at a core, and one beside it for the
                // ranger standing in the room (#558).
                let cored =
                    Capacity.fighters 2
                    |> Capacity.capping (CapScope.Row CastRow.Guard) 1
                    |> Capacity.capping (CapScope.Row CastRow.Ranger) 1

                let row ground from rung capacity takers =
                    {
                        Ground = ground
                        From = from
                        Pooled = Some(safetyRank rung, capacity)
                        Takers = takers
                    }

                // Laid but not pooled: an errand room with no ore to burn keeps
                // no garrison (#420), its ring laid all the same.
                let unpooled ground from =
                    {
                        Ground = ground
                        From = from
                        Pooled = None
                        Takers = []
                    }

                let rows =
                    [
                        // Each alone. A raid's Guard two rungs up, over every
                        // kill (#528); a kill one, over a quiet holder's.
                        [ Raid ], row [ Raid ] [ "Ring" ] TwoRungsUp one [ "g-1" ]
                        // Moved on purpose (#558): the ranger standing in the
                        // room takes a seat beside the guard's.
                        [ Core ], row [ Core ] [ "Ring" ] OnTheTier cored [ "g-1"; "r-1" ]
                        [ Hold ], row [ Hold ] [ "Ring" ] OnTheTier one [ "g-1" ]
                        // Moved on purpose (#550): the hauler, at the guard's
                        // pace, asks a ranger seat beside the guard's, which
                        // stays until a ranger holds the kill: r-1 takes it.
                        [ Trespasser ],
                        row [ Trespasser ] [ "Trespass" ] OneRungUp hauled [ "g-1"; "r-1" ]
                        [ Resident ], unpooled [ Resident ] [ "ResidentRing" ]
                        [ Harassed ], row [ Harassed ] [ "HarassRing" ] OnTheTier relieved [ "r-1" ]
                        // #535's hold rule: the guard holding the Guard keeps it
                        // while it can strike a target there, whatever the kind.
                        [ ChaseSeen ], row [ ChaseSeen ] [ "Kite" ] TwoRungsUp one [ "g-1" ]
                        // Out of sight a chase has no ground; the pool walks it in.
                        [ ChaseDark ], row [] [] OneRungUp one [ "r-1" ]
                        // The raid before what it shares the room with.
                        [ Raid; Hold ], row [ Raid ] [ "Ring" ] TwoRungsUp one [ "g-1" ]
                        [ Raid; Core ], row [ Raid ] [ "Ring" ] TwoRungsUp one [ "g-1" ]
                        [ Raid; Trespasser ], row [ Raid ] [ "Ring" ] TwoRungsUp one [ "g-1" ]
                        // In a ranger room a raid is kited, on ground of its own;
                        // a harassment room's raid is the enemy's own, on the tier.
                        [ Raid; Resident ], row [] [ "Kite" ] TwoRungsUp garrison [ "g-1"; "r-1" ]
                        [ Raid; Harassed ], row [] [ "Kite" ] OnTheTier relieved [ "g-1" ]
                        // The held exit before the trespasser, the trespasser
                        // before the core. Moved on purpose (#558): the
                        // trespasser, which can step away, is struck before
                        // the core, which stands (it was the core's).
                        [ Hold; Core ], row [ Hold ] [ "Ring" ] OnTheTier one [ "g-1" ]
                        [ Hold; Trespasser ], row [ Hold ] [ "Ring" ] OnTheTier one [ "g-1" ]
                        [ Core; Trespasser ],
                        row [ Trespasser ] [ "Trespass" ] OneRungUp hauled [ "g-1"; "r-1" ]
                        [ Hold; Core; Trespasser ], row [ Hold ] [ "Ring" ] OnTheTier one [ "g-1" ]
                        // A ranger room's declared ground before all three,
                        // where it pools. Moved by #542: an errand with no ore
                        // pools nothing, so the held exit its guard holds is the
                        // room's (it was the resident's, pooled for nobody).
                        [ Resident; Hold ], row [ Hold ] [ "Ring" ] OnTheTier one [ "g-1" ]
                        // The core pools the Guard, and the garrison takes it
                        // on the Reactor's ring, the core unhit.
                        [ Resident; Core ],
                        row [ Resident ] [ "ResidentRing" ] OnTheTier garrison [ "r-1" ]
                        // The guard holding it holds the trespass's fight (#535).
                        [ Resident; Trespasser ],
                        row [ Resident ] [ "ResidentRing" ] OnTheTier garrison [ "g-1"; "r-1" ]
                        [ Harassed; Hold ],
                        row [ Harassed ] [ "HarassRing" ] OnTheTier relieved [ "r-1" ]
                        [ Harassed; Core ],
                        row [ Harassed ] [ "HarassRing" ] OnTheTier relieved [ "r-1" ]
                        [ Harassed; Trespasser ],
                        row [ Harassed ] [ "HarassRing" ] OnTheTier relieved [ "g-1" ]
                        // Quirk, kept: the resident's ground and cap before the
                        // harassment's, but the cap's count is the row ask's,
                        // which asks as the harassment: one ranger, not the
                        // garrison, plus the reliefs.
                        [ Resident; Harassed ],
                        row
                            [ Resident ]
                            [ "ResidentRing" ]
                            OnTheTier
                            (Capacity.fighters (1 + precedenceBasis.Tuning.RangerResidents))
                            [ "r-1" ]
                        // A chase in sight kites before the core, the held exit
                        // and the trespasser; beside a raid that can close, no chase.
                        [ ChaseSeen; Core ], row [ ChaseSeen ] [ "Kite" ] TwoRungsUp one [ "g-1" ]
                        [ ChaseSeen; Hold ], row [ ChaseSeen ] [ "Kite" ] TwoRungsUp one [ "g-1" ]
                        [ ChaseSeen; Trespasser ],
                        row [ ChaseSeen ] [ "Kite" ] TwoRungsUp one [ "g-1" ]
                        [ ChaseSeen; Raid ],
                        row [] [ "Ring" ] TwoRungsUp (Capacity.fighters 2) [ "g-1" ]
                        // Quirk, kept: a chase out of sight over a held exit takes
                        // the melee guard's range-1 exit ground, a rung up.
                        [ ChaseDark; Hold ], row [ Hold ] [ "Ring" ] OneRungUp one [ "r-1" ]
                        [ ChaseDark; Core ], row [ Core ] [ "Ring" ] OneRungUp one [ "r-1" ]
                        // Shut on a chase on the record, a raid that can close seen
                        // there after all. The Brief's S7, rule 2: its verdict is the
                        // guard's win, so it is the raid's Guard and the guard
                        // takes it (it was pooled as a kill nobody took).
                        [ ChaseDark; Raid; Hold ], row [ Raid ] [ "Ring" ] TwoRungsUp one [ "g-1" ]
                        // And a chase out of sight shuts the trespasser's kill out.
                        [ ChaseDark; Trespasser ], row [] [] OneRungUp one [ "g-1" ]
                    ]

                Expect.equal
                    (rows |> List.map (fun (flavours, _) -> flavours, read flavours))
                    rows
                    "each mix's answer"
            }

            // Quirk, kept (the Brief's S0): the two orders that disagree, named.
            test
                "quirk: a resident room that is harassed too caps its Guard at one ranger and the reliefs" {
                let colony = List.fold withFlavour precedenceBasis [ Resident; Harassed ]

                Expect.equal
                    (pooledHolding [] (Guard "W1N2") colony)
                    (Some(
                        safetyRank OnTheTier,
                        Capacity.fighters (1 + precedenceBasis.Tuning.RangerResidents)
                    ))
                    "the cap's arm is the resident's, its count the row ask's harassment one"
            }

            test
                "quirk: a chase out of sight over a held exit holds the exit's range-1 ground, a rung up" {
                let colony = List.fold withFlavour precedenceBasis [ ChaseDark; Hold ]

                Expect.equal
                    (Threats.guardGroundIn (threatsOf colony (Atlas.ofView colony)) "W1N2")
                    (Some([ for y in 41..48 -> RoomPos.at "W1N2" { X = 1; Y = y } ] |> Set.ofList))
                    "the melee guard's ground a step inside the run, which a kiter keeps off"

                Expect.equal
                    (pooledHolding [ "g-1", Guard "W1N2" ] (Guard "W1N2") colony)
                    (Some(safetyRank OneRungUp, Capacity.fighters 1))
                    "pooled as the chase's kill"

                Expect.equal
                    ((decideFrom (Map.ofList [ "g-1", taskId (Guard "W1N2") ]) colony).Assignments
                     |> Map.tryFind "r-1")
                    (Some(taskId (Guard "W1N2")))
                    "and the ranger takes it"
            }

            // Quirk, kept (the Brief's S3): `Planner.coredOutpostsOf` pools a
            // declared outpost's core whatever kind claims the room first, so an
            // errand room with no ore to burn, pooled for nothing else, is
            // pooled for its core and held on the Reactor's ring.
            test "quirk: an unfuelled errand room with a core pools its Guard as the resident's" {
                let colony = List.fold withFlavour precedenceBasis [ Resident; Core ]
                let threats = threatsOf colony (Atlas.ofView colony)

                Expect.equal
                    ((Threats.briefOf threats "W1N2").Duty, Threats.guardGroundIn threats "W1N2")
                    (Duty.Guard BriefKind.Resident, Some(besideIn { X = 6; Y = 46 }))
                    "read as the resident's, on the Reactor's ring"

                Expect.equal
                    (pooledHolding [] (Guard "W1N2") (withFlavour precedenceBasis Resident))
                    None
                    "the errand alone pools nothing"

                Expect.isSome
                    (pooledHolding [] (Guard "W1N2") colony)
                    "the core beside it pools the Guard"
            }

            // The Brief's S3: a chase leaves its room open to the ring kinds alone.
            // Each chase mix's `Held` and `Ring`, so a hold or a core laid under
            // a raid cannot hide behind the kite ground the Guard reads.
            test "a chase's room keeps the ring a raid, a held exit or a core lays there" {
                let laid flavours =
                    let colony = List.fold withFlavour precedenceBasis flavours
                    let threats = threatsOf colony (Atlas.ofView colony)

                    List.contains BriefKind.ExitHold (Threats.briefOf threats "W1N2").Claims,
                    (Threats.briefOf threats "W1N2").Laid.Ring

                let ringOf flavours = snd (laid flavours)

                Expect.equal
                    ([
                        [ ChaseSeen; Hold ]
                        [ ChaseSeen; Core ]
                        [ ChaseDark; Hold ]
                        [ ChaseDark; Core ]
                        [ ChaseDark; Raid; Hold ]
                        [ ChaseDark; Trespasser ]
                     ]
                     |> List.map (fun flavours -> flavours, laid flavours))
                    [
                        [ ChaseSeen; Hold ], (false, ringOf [ ChaseSeen ])
                        [ ChaseSeen; Core ], (false, ringOf [ ChaseSeen ])
                        [ ChaseDark; Hold ], (true, ringOf [ Hold ])
                        [ ChaseDark; Core ], (false, ringOf [ Core ])
                        [ ChaseDark; Raid; Hold ], (false, ringOf [ Raid ])
                        [ ChaseDark; Trespasser ], (false, None)
                    ]
                    "each chase mix's held flag and ring"

                Expect.isSome (ringOf [ ChaseSeen ]) "the chase in sight stands in a raid's ring"
            }

            test "the order a room's flavour is read in lists every kind once" {
                let cases =
                    Reflection.FSharpType.GetUnionCases typeof<BriefKind>
                    |> Array.map (fun case -> case.Name)
                    |> Set.ofArray

                Expect.equal
                    (BriefKind.order |> List.map string |> List.sort)
                    (Set.toList cases)
                    "every BriefKind case, none twice"
            }

            // The Brief's S2: each room's flavour is classified once, and every
            // Guard reader dispatches on it. The mixes the table above pins,
            // by the one kind each is read as.
            test "each mix of flavours is read as one kind" {
                let kindOf flavours =
                    let colony = List.fold withFlavour precedenceBasis flavours
                    (Threats.briefOf (threatsOf colony (Atlas.ofView colony)) "W1N2").Duty

                let rows =
                    [
                        [ Raid ], BriefKind.Outpost
                        [ Core ], BriefKind.Core
                        [ Hold ], BriefKind.ExitHold
                        [ Trespasser ], BriefKind.Trespass
                        [ Resident ], BriefKind.Resident
                        [ Harassed ], BriefKind.Harass
                        [ ChaseSeen ], BriefKind.Chase
                        [ ChaseDark ], BriefKind.Chase
                        [ Raid; Hold ], BriefKind.Outpost
                        [ Raid; Core ], BriefKind.Outpost
                        [ Raid; Trespasser ], BriefKind.Outpost
                        [ Raid; Resident ], BriefKind.Resident
                        [ Raid; Harassed ], BriefKind.Harass
                        // Kept by #558: the core guards the room, so the hold
                        // pools, and a held exit is defence, ahead of a kill
                        // (#528), its target the raid that left by it.
                        [ Hold; Core; Trespasser ], BriefKind.ExitHold
                        // #542: nobody holds the exit, so its claim pools
                        // nothing and the trespass's kill is the room's.
                        [ Hold; Trespasser ], BriefKind.Trespass
                        // Moved by #558: the trespass's kill before the core.
                        [ Core; Trespasser ], BriefKind.Trespass
                        [ Resident; Core ], BriefKind.Resident
                        [ Harassed; Trespasser ], BriefKind.Harass
                        [ Resident; Harassed ], BriefKind.Resident
                        [ ChaseSeen; Core ], BriefKind.Chase
                        [ ChaseSeen; Raid ], BriefKind.Outpost
                        [ ChaseDark; Hold ], BriefKind.Chase
                        [ ChaseDark; Trespasser ], BriefKind.Chase
                    ]

                Expect.equal
                    (rows |> List.map (fun (flavours, _) -> flavours, kindOf flavours))
                    (rows |> List.map (fun (flavours, kind) -> flavours, Duty.Guard kind))
                    "each mix's kind"
            }

            // #542 (live t976,867-976,972, Specter836 in W17S26): a kind
            // claims a room only where it pools a Task this tick; precedence
            // decides among those claims alone.
            test
                "a held exit nobody holds claims nothing, so the trespasser beside it is the room's kill" {
                let colony = List.fold withFlavour precedenceBasis [ Hold; Trespasser ]

                Expect.equal
                    (heldBriefOf [] "W1N2" colony).Duty
                    (Duty.Guard BriefKind.Trespass)
                    "the trespass's Guard"

                // Moved on purpose (#550): the flavour's hauler, at the guard's
                // pace, asks the ranger row too, and the guard's until a
                // ranger holds the kill.
                Expect.equal
                    (pooledHolding [] (Guard "W1N2") colony)
                    (Some(
                        safetyRank OneRungUp,
                        Capacity.fighters 2
                        |> Capacity.capping (CapScope.Row CastRow.Guard) 1
                        |> Capacity.capping (CapScope.Row CastRow.Ranger) 2
                    ))
                    "pooled as its kill"
            }

            // #542, the same rule in the home: no Guard is pooled for a raid
            // there (the home's is the Defend), so the raid's claim gives way
            // to the digger's kill, where before it pooled nothing.
            test "a raid in the home pools no Guard, so the trespasser beside it is the home's kill" {
                let colony =
                    declaredRaid
                        [
                            diggerAt "W1N1" { X = 25; Y = 5 }
                            { hostileIn "W1N1" { X = 25; Y = 8 } smallMelee with
                                Id = "h-2"
                            }
                        ]

                Expect.equal
                    (heldBriefOf [] "W1N1" colony).Claims
                    [ BriefKind.Trespass; BriefKind.Outpost ]
                    "the kill first, the raid's claim after it"

                Expect.equal
                    (pooledHolding [] (Guard "W1N1") colony)
                    (Some(safetyRank OneRungUp, Capacity.fighters 1))
                    "pooled as the kill"
            }

            test "a held exit a guard holds keeps the room over the trespasser" {
                let colony = List.fold withFlavour precedenceBasis [ Hold; Trespasser ]
                let holding = [ "g-1", Guard "W1N2" ]

                Expect.equal
                    (heldBriefOf holding "W1N2" colony).Duty
                    (Duty.Guard BriefKind.ExitHold)
                    "the held exit's Guard"

                Expect.equal
                    (pooledHolding holding (Guard "W1N2") colony)
                    (Some(safetyRank OnTheTier, Capacity.fighters 1))
                    "pooled on the tier for its holder"
            }
        ]

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

                // Two guard blocks out-trade it.
                let colony =
                    { declaredRaid [ synapse ] with
                        Bank = bank 1500 1500
                    }
                    |> withGuards [ guard "g-1", { X = 20; Y = 41 } ]

                Expect.isTrue
                    (guardBlocksBeat colony "W1N2" (guardBlocksReach colony))
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
                    brief.Duty
                    (Duty.Guard BriefKind.Trespass)
                    "the premise: a trespass's kill"

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
                let reach = Threats.reachIn (threatsOf colony (Atlas.ofView colony)) "W1N2"

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

                Expect.isFalse
                    (steps |> List.exists (fun tile -> Set.contains tile reach))
                    "and its step keeps out of the Reach"
            }
        ]

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
                    [ CastRow.Ranger ]
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
                    [ CastRow.Guard ]
                    "a raid that closes on the guard is still the guard's"

                let poor =
                    { declaredRaid invasion with
                        Bank = bank 550 550
                    }

                Expect.equal
                    ((Threats.briefOf (threatsOf poor (Atlas.ofView poor)) "W1N2").Asks
                     |> List.map (fun ask -> ask.Row))
                    [ CastRow.Guard ]
                    "a bank that casts no ranger block is never asked for one (#546): the guard's"
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
                    [ CastRow.Ranger ]
                    "the guard's swing reaches no Invader shooter, so the ranger row is asked"

                Expect.isFalse (brief.Admits(guard "g-1")) "and a melee guard is not sent into it"

                let mixedOnly =
                    { declaredRaid (List.take 1 npc) with
                        Bank = bank 5600 5600
                    }

                Expect.equal
                    ((Threats.briefOf (threatsOf mixedOnly (Atlas.ofView mixedOnly)) "W1N2").Asks
                     |> List.map (fun ask -> ask.Row))
                    [ CastRow.Guard ]
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

                let colony =
                    declaredRaid [ head; near ] |> withGuards [ guard "g-1", { X = 28; Y = 45 } ]

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
                // A smallMelee beside a smallHealer: one 800-bank guard loses it
                // (`guard`'s own worked example), two win. The bank now buys a
                // guard that wins alone, so the room is the guard row's `Win`;
                // the two held were cast smaller.
                let raid = raiders @ healers 1

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

                // On the walk in, strung out as one file between the rally
                // ground and the crossing, off both: launched, not pulled back
                // (#491's rule for the squads).
                let walking =
                    declared raid
                    |> withBodyAtHome (guard "g-1") { X = 25; Y = 7 }
                    |> withBodyAtHome (guard "g-2") { X = 25; Y = 9 }

                Expect.isEmpty
                    (heldBriefOf holding "W1N2" walking).Stances
                    "holders strung out on the walk in are not pulled back"
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
