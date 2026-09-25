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

                // The Safety tier's priority has no rung: nothing ever asks how its
                // two Tasks order.
                Expect.equal
                    (raided |> entryFor (Guard "W1N2") |> Option.map (fun e -> e.Priority))
                    (raided |> entryFor Flee |> Option.map (fun e -> e.Priority))
                    "and it ranks exactly where Flee does — Safety, no rung"
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

                Expect.isEmpty
                    (moveIntentsFor "w-home" raided.Intents)
                    "so nothing walks it across the Seam"
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
