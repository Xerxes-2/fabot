/// A raided outpost's Guard: who holds it, an idle or blind guard, the Task
/// across the Seam, and which flavour answers one room's Guard.
module Fabot.Core.Tests.Decide.OutpostGuardTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.OutpostFixtures

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

            // The world lane's FightMatch: two colonies pooling
            // one Guard capped at one body are matched in one pass, the
            // Task's holders counted whichever colony's, so the body that
            // lands first takes it and no other colony's body doubles it.
            test
                "one match over two colonies' fighters seats a Guard's one body, the one that lands first" {
                let sizing =
                    {
                        AnchorPostCaps = Map.empty
                        ReserverClaims = []
                        MinerWorkPerMove = 1
                        GuardBlocks = 1
                        RangerBlocks = 1
                        MinerQuota = 0
                        OreMinerQuota = 0
                        CourierQuota = 0
                        UpgraderWork = None
                    }

                let side (colony: ColonyView) =
                    let atlas = Atlas.ofView colony
                    let threats = threatsOf colony atlas
                    let pool = planPool colony atlas threats [] (planTasksOn colony threats)

                    let context: MatchContext =
                        {
                            View = colony
                            Atlas = atlas
                            Sizing = sizing
                            Threats = threats
                            Pool = pool
                            PooledElsewhere = false
                        }

                    context,
                    {
                        Bodies = colony.Creeps |> List.filter isFighterBody
                        Offered =
                            pool
                            |> List.filter (fun pooled ->
                                match pooled.Task with
                                | Flee
                                | Guard _
                                | Defend _ -> true
                                | _ -> false)
                    }

                let near = declaredRaid raiders |> withGuards [ guard "g-near", beside ]
                let far = declaredRaid raiders |> withBodyAtHome (guard "g-far") atSpawn

                Expect.equal
                    (entryFor (Guard "W1N2") (pooledOf near)
                     |> Option.bind (fun entry -> Capacity.capOf CapScope.Fighters entry.Capacity))
                    (Some 1)
                    "the premise: one body"

                let holders (sides: (MatchContext * MatchShare) list) =
                    let outcome =
                        matchAcross
                            (List.map fst sides)
                            []
                            Map.empty
                            [ sides |> List.mapi (fun k (_, share) -> k, share) ]
                            Map.empty
                            Set.empty

                    outcome.Assigned
                    |> Map.filter (fun _ tid -> tid = taskId (Guard "W1N2"))
                    |> Map.keys
                    |> List.ofSeq

                Expect.equal
                    (holders [ side far ])
                    [ "g-far" ]
                    "alone, the far colony's body takes it"

                Expect.equal
                    (holders [ side far; side near ])
                    [ "g-near" ]
                    "together, the near one, once"

                Expect.equal
                    (holders [ side near; side far ])
                    [ "g-near" ]
                    "whichever colony is read first"
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
                        // Moved on purpose (#615 U3): #535's hold rule kept the
                        // guard holding the Guard while it could strike a target
                        // there; one fitness answer takes and keeps it now, and
                        // one guard block loses the plodder's swing, so it is let
                        // go and the ranger that kites the plodder down takes it.
                        [ ChaseSeen ], row [ ChaseSeen ] [ "Kite" ] TwoRungsUp one [ "r-1" ]
                        // Out of sight a chase has no ground; the pool walks it in.
                        [ ChaseDark ], row [] [] OneRungUp one [ "r-1" ]
                        // The raid before what it shares the room with.
                        [ Raid; Hold ], row [ Raid ] [ "Ring" ] TwoRungsUp one [ "g-1" ]
                        [ Raid; Core ], row [ Raid ] [ "Ring" ] TwoRungsUp one [ "g-1" ]
                        [ Raid; Trespasser ], row [ Raid ] [ "Ring" ] TwoRungsUp one [ "g-1" ]
                        // In a ranger room a raid is kited, on ground of its own;
                        // a harassment room's raid is the enemy's own. Moved on
                        // purpose (#609): one our side wins is the raid's rung,
                        // two up, so a ranger on a quiet Guard is sent to it.
                        [ Raid; Resident ], row [] [ "Kite" ] TwoRungsUp garrison [ "g-1"; "r-1" ]
                        [ Raid; Harassed ], row [] [ "Kite" ] TwoRungsUp relieved [ "g-1" ]
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
                        // and the trespasser. Moved on purpose (#615 U2): beside
                        // a raid that can close, the ranger row trades with the
                        // smallMelee at the standoff, the plodder kept off and
                        // striking nothing (the smallMelee's gun still strikes
                        // 10 from three), so its win is the ranger's chase:
                        // kite ground, one seat (it was the guard's ring, two).
                        // Moved on purpose (#615 U3), as [ ChaseSeen ]: the ranger
                        // takes the chase the guard no longer fits; beside the
                        // smallMelee, which closes on the one-block ranger, neither
                        // fits, and nobody holds it (the row casts the ranger that
                        // wins it).
                        [ ChaseSeen; Core ], row [ ChaseSeen ] [ "Kite" ] TwoRungsUp one [ "r-1" ]
                        [ ChaseSeen; Hold ], row [ ChaseSeen ] [ "Kite" ] TwoRungsUp one [ "r-1" ]
                        [ ChaseSeen; Trespasser ],
                        row [ ChaseSeen ] [ "Kite" ] TwoRungsUp one [ "r-1" ]
                        [ ChaseSeen; Raid ], row [] [ "Kite" ] TwoRungsUp one []
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
