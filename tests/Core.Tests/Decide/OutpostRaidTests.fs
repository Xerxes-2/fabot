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
                    (Some(ExitWatch.Seen [ { At = { X = 1; Y = 22 }; Dies = 1600 } ]))
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

            test "the threat re-entering puts the ground back on its ring" {
                let held =
                    watchThrough [ watched 100 [ kiterAt { X = 1; Y = 22 } 1500 ]; watched 101 [] ]

                Expect.equal
                    (World.watchExits
                        Tuning.defaults
                        held.ExitWatches
                        (watched 102 [ kiterAt { X = 0; Y = 22 } 1400 ])
                     |> watchOf)
                    (Some(ExitWatch.Seen [ { At = { X = 0; Y = 22 }; Dies = 1502 } ]))
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
        ]

/// Raptor865 as W17S26 saw it (#516): five WORK on three MOVE, no weapon,
/// a player's body digging our outpost.
let private diggerAt room pos =
    { hostileIn room pos (List.replicate 5 Work @ List.replicate 3 Move) with
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
                    ((Threats.briefOf (threatsOf colony atlas) "W1N2").Kind)
                    BriefKind.Trespass
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

            test "a neutral's disarmed body and a bare enemy scout pool none (#528)" {
                Expect.isNone
                    (pooledOf (declaredRaid [ disarmedAt "giaco" "W1N2" { X = 25; Y = 44 } ])
                     |> entryFor (Guard "W1N2"))
                    "only a declared enemy's"

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
                        (Planner.outpostFactsOf colony).Declared
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

                Expect.equal
                    (Facts.answer mother "W1N4")
                    Facts.Answer.Win
                    "answered by the residents"

                let held = Map.ofList [ "r-1", taskId (Guard "W1N3"); "r-2", taskId (Guard "W1N3") ]

                let called = decide mother held Set.empty None

                Expect.isTrue
                    ([ "r-1"; "r-2" ]
                     |> List.exists (fun name ->
                         Map.tryFind name called.Assignments = Some(taskId (Guard "W1N4"))))
                    "a resident takes the harasser"

                // The child's view: her home W1N1, her outpost W1N2, the
                // mother's residents standing at home (`ColonyView.Lendable`).
                let child lendable =
                    { declaredRaid [ harasser "W1N2" ] with
                        Bank = bank 850 850
                        Lendable =
                            if lendable then
                                [
                                    resident "r-1", RoomPos.at "W1N1" { X = 25; Y = 26 }
                                    resident "r-2", RoomPos.at "W1N1" { X = 27; Y = 26 }
                                ]
                            else
                                []
                    }

                Expect.isFalse
                    (guardBlocksBeat (child true) "W1N2" (guardBlocksReach (child true)))
                    "the premise: the child's guard row loses it"

                Expect.equal
                    (Facts.answer (child true) "W1N2")
                    Facts.Answer.Win
                    "the child counts her mother's residents"

                Expect.isEmpty (shut (child true)) "and does not stand the outpost down"
                Expect.equal (guardQuotaOf (child true)) (Some 0) "nor casts a guard for it"

                Expect.equal
                    (shut (child false))
                    (Set.singleton "W1N2")
                    "with no resident at home it is stood down"
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
            test "a raider whose weapons are shot off, alone, has no Reach and no Guard" {
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

                Expect.isNone (pooledHolding [] (Guard "W1N2") colony) "no Guard is pooled for it"

                let decision = decideOn colony

                Expect.equal
                    (Map.tryFind "r-1" decision.Assignments, Map.tryFind "g-1" decision.Assignments)
                    (None, None)
                    "and neither body is sent to it"
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
                        ChaseSeen,
                        Some(
                            tiles (
                                [ for y in 43..47 -> 22, y ]
                                @ [ for x in 23..28 -> x, 43 ]
                                @ [ for y in 44..46 -> 28, y ]
                            )
                        )
                        ChaseDark, None
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
            // who holds it after the tick.
            test "the ground, rank, capacity and taker of each mix of flavours" {
                let held = [ "g-1", Guard "W1N2" ]

                let read flavours =
                    let colony = List.fold withFlavour precedenceBasis flavours
                    let atlas = Atlas.ofView colony
                    let threats = threatsOf colony atlas
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
                        [ Core ], row [ Core ] [ "Ring" ] OnTheTier one [ "g-1" ]
                        [ Hold ], row [ Hold ] [ "Ring" ] OnTheTier one [ "g-1" ]
                        [ Trespasser ], row [ Trespasser ] [ "Trespass" ] OneRungUp one [ "g-1" ]
                        [ Resident ], unpooled [ Resident ] [ "ResidentRing" ]
                        [ Harassed ], row [ Harassed ] [ "HarassRing" ] OnTheTier relieved [ "r-1" ]
                        [ ChaseSeen ], row [ ChaseSeen ] [ "Kite" ] TwoRungsUp one [ "r-1" ]
                        // Out of sight a chase has no ground; the pool walks it in.
                        [ ChaseDark ], row [] [] OneRungUp one [ "r-1" ]
                        // The raid before what it shares the room with.
                        [ Raid; Hold ], row [ Raid ] [ "Ring" ] TwoRungsUp one [ "g-1" ]
                        [ Raid; Core ], row [ Raid ] [ "Ring" ] TwoRungsUp one [ "g-1" ]
                        [ Raid; Trespasser ], row [ Raid ] [ "Ring" ] TwoRungsUp one [ "g-1" ]
                        // In a ranger room a raid is kited, on ground of its own;
                        // a harassment room's raid is the enemy's own, on the tier.
                        [ Raid; Resident ], row [] [ "Kite" ] TwoRungsUp garrison [ "r-1" ]
                        [ Raid; Harassed ], row [] [ "Kite" ] OnTheTier relieved [ "r-1" ]
                        // The held exit before the core, either before the trespasser.
                        [ Hold; Core ], row [ Hold ] [ "Ring" ] OnTheTier one [ "g-1" ]
                        [ Hold; Trespasser ], row [ Hold ] [ "Ring" ] OnTheTier one [ "g-1" ]
                        [ Core; Trespasser ], row [ Core ] [ "Ring" ] OnTheTier one [ "g-1" ]
                        [ Hold; Core; Trespasser ], row [ Hold ] [ "Ring" ] OnTheTier one [ "g-1" ]
                        // A ranger room's declared ground before all three. The
                        // hold and the trespass pool nothing there; the core
                        // pools the Guard, and the garrison takes it on the
                        // Reactor's ring, the core unhit.
                        [ Resident; Hold ], unpooled [ Resident ] [ "ResidentRing" ]
                        [ Resident; Core ],
                        row [ Resident ] [ "ResidentRing" ] OnTheTier garrison [ "r-1" ]
                        [ Resident; Trespasser ], unpooled [ Resident ] [ "ResidentRing" ]
                        [ Harassed; Hold ],
                        row [ Harassed ] [ "HarassRing" ] OnTheTier relieved [ "r-1" ]
                        [ Harassed; Core ],
                        row [ Harassed ] [ "HarassRing" ] OnTheTier relieved [ "r-1" ]
                        [ Harassed; Trespasser ],
                        row [ Harassed ] [ "HarassRing" ] OnTheTier relieved [ "r-1" ]
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
                        [ ChaseSeen; Core ], row [ ChaseSeen ] [ "Kite" ] TwoRungsUp one [ "r-1" ]
                        [ ChaseSeen; Hold ], row [ ChaseSeen ] [ "Kite" ] TwoRungsUp one [ "r-1" ]
                        [ ChaseSeen; Trespasser ],
                        row [ ChaseSeen ] [ "Kite" ] TwoRungsUp one [ "r-1" ]
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
                        [ ChaseDark; Trespasser ], row [] [] OneRungUp one [ "r-1" ]
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
                    ((Threats.briefOf threats "W1N2").Kind, Threats.guardGroundIn threats "W1N2")
                    (BriefKind.Resident, Some(besideIn { X = 6; Y = 46 }))
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
                    (Threats.briefOf (threatsOf colony (Atlas.ofView colony)) "W1N2").Kind

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
                        [ Hold; Core; Trespasser ], BriefKind.ExitHold
                        [ Core; Trespasser ], BriefKind.Core
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
                    rows
                    "each mix's kind"
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
/// spans (21..29, 0..2), our guard `g-1` at (20,1) just west of it.
let private trespassPastAReach digger =
    beyond
        [
            diggerAt "W1N2" digger
            { hostileIn "W1N3" { X = 25; Y = 47 } smallMelee with
                Id = "h-2"
            }
        ]
    |> withGuards [ guard "g-1", { X = 20; Y = 1 } ]

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
                // A raider beyond the border reaches (21..29, 0..2) of the
                // outpost; the straight walk east from (20,1) to the digger
                // past it crosses it.
                let colony = trespassPastAReach { X = 32; Y = 1 }
                let reach = Threats.reachIn (threatsOf colony (Atlas.ofView colony)) "W1N2"

                Expect.contains
                    reach
                    { X = 21; Y = 1 }
                    "the premise: the Reach lies across the walk"

                let decision = decideOn colony

                Expect.equal
                    (Map.tryFind "g-1" decision.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the guard takes the kill"

                let steps =
                    moveIntents decision.Intents
                    |> List.filter (fst >> (=) "g-1")
                    |> List.map (snd >> stepFrom { X = 20; Y = 1 })

                Expect.equal steps [ { X = 20; Y = 2 } ] "it steps down, under the Reach"

                // The light tick replays the plan's second tile from its first.
                Expect.equal
                    (Map.tryFind "g-1" decision.Steps)
                    (Some(RoomPos.at "W1N2" { X = 20; Y = 2 }, RoomPos.at "W1N2" { X = 21; Y = 3 }))
                    "its step plan is the step it issued, and the next one round"
            }

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
                    |> List.map (snd >> stepFrom { X = 20; Y = 1 })

                Expect.isEmpty steps "it waits rather than step in"
                Expect.isNone (Map.tryFind "g-1" decision.Steps) "and plans no step"

                Expect.isFalse
                    (steps |> List.exists (fun tile -> Set.contains tile reach))
                    "and its step keeps out of the Reach"
            }
        ]
