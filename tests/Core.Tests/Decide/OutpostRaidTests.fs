/// An outpost with a hostile standing in it: the Guard that answers, and
/// the container that switches the room back (ADR 0056).
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
                // What a raid costs an outpost today, before ADR 0056's
                // guard row exists to answer it: the [[hauler unit]]s run,
                // the [[anchor]] cannot and stays, and every Task worked
                // from ground inside the [[reach]] is inapplicable to
                // everyone standing there.
                //
                // Pairwise against the same fixture with no hostile in it,
                // one fact apart: the quiet tick is the yardstick, so a
                // difference below is the raid's and can be nothing else.
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
                    (Some(taskId (Withdraw "can-out")), Some(taskId (Withdraw "can-out")))
                    "and the crew empties the container that Post feeds"

                Expect.equal
                    (Map.tryFind "h-out1" raided, Map.tryFind "h-out2" raided)
                    (Some(taskId Flee), Some(taskId Flee))
                    "both haulers drop that haul for Flee: the Safety tier outranks every other"

                // Neither Flee nor work: Flee is inapplicable to a
                // work-heavy body (ADR 0033), and every Seat of its rock —
                // all three of them, the row of field under it — lies
                // inside the Reach, so the one row that cannot run is left
                // holding nothing on the tile it is being killed on. That is
                // the hole ADR 0056 casts a guard into, pinned here as the
                // behaviour that decision is measured against.
                Expect.equal
                    (Map.tryFind "a-out" raided)
                    None
                    "and the Anchor is matched to nothing at all: it neither runs nor digs"
            }

            test "the garrison's Harvest is released Threatened rather than simply lost" {
                // The other half of the same tick, read off the Verdicts
                // with the assignment the quiet tick made already held: a
                // Task whose whole Work Area is in a Reach is released
                // under its own reason, so an operator reading the
                // transition log tells a raid from a Task that vanished
                // (ADR 0033). Every Seat of `src-out` — the three tiles of
                // the row under it — is inside this raid's Reach, and the
                // container the haul reads is on one of them.
                let releasesOf hostiles =
                    let held =
                        Map.ofList
                            [
                                "a-out", taskId (Harvest "src-out")
                                "h-out1", taskId (Withdraw "can-out")
                            ]

                    (decide (raidedOutpost hostiles) held Set.empty None).Verdicts
                    |> List.choose (function
                        | Verdict.Released(creep, task, reason) -> Some(creep, task, reason)
                        | _ -> None)

                Expect.isEmpty (releasesOf []) "the premise: a quiet tick releases nobody"

                Expect.equal
                    (releasesOf raiders)
                    [
                        "a-out", taskId (Harvest "src-out"), ReleaseReason.Threatened
                        "h-out1", taskId (Withdraw "can-out"), ReleaseReason.Threatened
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
                // ADR 0056 decision 2 at the Planner's seam: one Task per
                // declared [[outpost]] a [[threat]] stands in, keyed on the
                // **room** — `guard:W1N2` and never the invader's id, so the
                // 2% multi-creep raid pools one Task and not five. Pairwise
                // against the same geometry with nothing in it: what moves
                // between the two calls is the raid.
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

                // The Safety tier's [[priority]] with no rung of its own: the
                // two Tasks of that tier carry one number, and nothing ever
                // asks how they order.
                Expect.equal
                    (raided |> entryFor (Guard "W1N2") |> Option.map (fun e -> e.Priority))
                    (raided |> entryFor Flee |> Option.map (fun e -> e.Priority))
                    "and it ranks exactly where Flee does — Safety, no rung"
            }

            test "the cap is the room's own quota, one guard and then two" {
                // The [[capacity]] is decision 2's "one number computed once
                // and read as both the row's quota and the Task's cap": the
                // Fighter share is `guardsWanted` for that room, so the bodies
                // the cascade hires are the bodies the Task admits. Read
                // pairwise off the raid alone — a lone `smallMelee` against the
                // same raid carrying two healers, whose 120 out-heals the 90
                // one `guardPattern` block deals (#272), with one guard of
                // ours standing in both readings so that what moves between
                // them is the raid and nothing of ours.
                let capOf colony =
                    pooledOf colony
                    |> entryFor (Guard "W1N2")
                    |> Option.map (fun entry -> entry.Capacity.Fighters)

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
                // Acceptance, and the hole ADR 0056 was written to fill: the
                // room the [[hauler unit]]s run out of and the [[anchor]] is
                // killed in now has one body whose Task is the fight. Pairwise
                // against the same guard standing in the same room with nothing
                // to fight, which is matched to nothing at all — a guard is
                // applicable to no other work in the pool.
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

                // Off the raid's ground entirely — the y = 48 row this
                // fixture's own [[flee]] cases run onto, so the body is
                // outside the [[reach]] and four tiles from the invader.
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
                // "Between the invader and the [[anchor]]" said in this
                // colony's vocabulary (ADR 0056): the Threat nearest a [[post]]
                // of that room, ties by id — the engine's own `findAttack.js`
                // chases the closest hostile by path, so the guard on that
                // invader's ring is between it and everything behind it.
                //
                // Two invaders, both within range 1 of the guard at (26,43):
                // `inv-2` at (25,42) stands a tile from the Post at (25,41),
                // `inv-1` at (27,44) three tiles from it — and the ids run the
                // other way, so a target chosen by id alone would name `inv-1`
                // in every reading below.
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
                // The same room the tick before its container stands: no
                // container, no Post, and so nothing to stand in front of.
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

                // **The range gate is on the candidates, not on the pick.**
                // (28,44) is a ring tile of `inv-1` and three from `inv-2`, so
                // the Threat nearest the Post is out of reach and the other one
                // is beside the body dealing 40 a tick. Ordered the ADR's
                // sentence literally — nearest the Post, then filtered by range
                // — the guard would swing at nothing here for as long as it
                // stood, which is not what "30 a part is paid at range 1" is a
                // reason for.
                Expect.equal
                    (attacksFrom { X = 28; Y = 44 } posted)
                    [ "g-1", "inv-1" ]
                    "out of reach of the Post's own invader, the guard hits the one it can reach"
            }

            test "the cap refuses the second Fighter while the room wants one" {
                // The [[capacity]] counted at the Matcher (ADR 0052 decision
                // 6): the room wants one guard this tick, so the second body
                // standing in the same ring is refused by the number the
                // Planner set — named, so a verbose reading tells "the room is
                // full" from "this body cannot fight".
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
                // #272, ADR 0056 decision 1 as amended: the count is a
                // function of the **raid** — the healing per tick against the
                // damage of the body the row would cast — so it cannot retract
                // on the arrival of the body it asked for. Read on the
                // reproduction that found it. Priced against the guards
                // standing there, the same two-healer raid admitted two while
                // one guard stood and one the tick the second arrived, and the
                // arriving body was `CapacityFull`-evicted onto a Flee whose
                // safe set is this same room: it never left, so its own damage
                // held the count at one and the colony had bought 750 energy
                // of body that never issues an `AttackCreep`, for as long as
                // it lived. Pairwise against the case above, whose raid heals
                // nothing and where the second body is refused for good. The
                // damage the healing is measured against is one block of the
                // row's own body, a constant, so this holds at whatever the
                // fixture banks.
                let capOf colony =
                    pooledOf colony
                    |> entryFor (Guard "W1N2")
                    |> Option.map (fun entry -> entry.Capacity.Fighters)

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
                // The other half of the [[capacity]] sentence — `Fighter -> the
                // room's quota, every other class 0` — read where it is asked
                // first: the body gate (ADR 0056's applicability clause) shuts
                // every body with no ATTACK part out before the number is ever
                // counted, so neither the crowd that runs from a raid nor the
                // [[anchor]] that cannot run can be matched into it. Both
                // classes, because both stand in this room and the acceptance
                // names both: a [[hauler unit]] is a `Carrier`, the Anchor a
                // `Heavy`, and the `Fighters` share admits neither.
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
                // **The body every guard really is.** The row hires at the
                // spawn (ADR 0056 decision 1), so the guard the colony buys
                // begins a room and a [[seam]] away from the ring it was bought
                // for, and decision 2 gives it "no movement of its own — the
                // mover walks it into the Work Area like any other Task". That
                // is a claim about the *price*: a Work Area filed under the
                // raided room and priced over the creep's own room alone would
                // reject this body `Unreachable` on every tick of its 1,500,
                // and its non-decaying `Living` would suppress the next cast —
                // 750 energy standing at the oven while the outpost is emptied.
                //
                // Pairwise against the same body inside the room, which is what
                // every other case here stands: what moves between the two
                // readings is the border, and the answer must not.
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

                // The other gate that could refuse a body a border away, read
                // off the same tick, and the ordering ADR 0056 decision 3
                // names: the cross-room threat reading (#147) sits *beneath*
                // the Safety tier, so the ring the raid laid is not read as
                // ground the raid took. A rule that judged the tier by its
                // ground would call every Guard threatened on every tick one
                // existed, and the gate that sends a body into the fight would
                // be the one thing keeping it out.
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
                // The Task is a per-tick fact read off vision, exactly as the
                // row's quota is (ADR 0056): the tick nothing armed is standing
                // in that outpost the Guard leaves the pool, and its holder is
                // released under the reason that says the work itself is gone
                // rather than that a raid took its ground.
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
                // **The acceptance ADR 0056 decision 3 is written for**, read
                // off one verbose scoring — the one place the whole pool is
                // judged for one body, so the exemption and the rule it is an
                // exemption from are the same tick's answers.
                //
                // ADR 0033 takes every Reach out of every Work Area at
                // applicability, and this raid's Reach covers the rock's three
                // Seats and the container's ground with them. A Guard's Work
                // Area is *made* of Reach tiles — the range-1 ring of the very
                // Threat that laid them — so under that gate unamended it would
                // be inapplicable to everyone on every tick it existed, and the
                // one Task the row buys a body for would be the one Task no
                // body could ever hold. So the subtraction is skipped for the
                // **Safety tier**, both of whose areas are derived off the
                // tick's `Threats` rather than off a target's surroundings.
                //
                // Flee is the other half of that tier and is refused here for
                // the reason beside it (decision 3's first clause): the two are
                // disjoint by [[body class]], so this one body sees one Task
                // rejected for every gate the pool has and exactly one left.
                let colony = declaredRaid raiders |> withGuards [ guard "g-1", beside ]
                let decision = decide colony Map.empty (Set.singleton "g-1") None
                let rejections = rejectionsFor "g-1" decision.Verdicts |> Option.defaultValue []

                Expect.contains
                    rejections
                    (taskId (Harvest "src-out"), RejectReason.Threatened)
                    "the rock's every Seat is in the Reach, so its Harvest is gone for this body"

                Expect.contains
                    rejections
                    (taskId (Withdraw "can-out"), RejectReason.Threatened)
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

/// The two-room shape #147 was filed on: a body of ours **at home** and a Task
/// whose ground is a raided room across the [[seam]]. ADR 0056 decides it
/// rather than merely touching it — the reading below is the one that must sit
/// beneath the Safety tier, or it lands as the bug decision 3's exemption
/// exists to prevent. The guard half of that ordering is pinned where the
/// guard's own crossing already is, in `guardTaskTests` above: the same colony,
/// the same raid and the same tile, read for `Threatened` beside `Unreachable`.
[<Tests>]
let crossSeamThreatTests =
    testList
        "a Task across the Seam in a raided room"
        [
            test "the home worker is not sent into a raid its own crew is running out of" {
                // **#147, reproduced and fixed.** ADR 0033 makes a Task whose
                // whole Work Area lies in a Reach inapplicable to *everyone*,
                // and the word never reached a body standing in another room:
                // `threatened` read the creep-relative Work Area, which is
                // empty across a border by construction (ADR 0041), and an
                // empty area is not "threatened" but unplaceable. So on the
                // very tick this outpost's crew was fleeing off the rock's
                // Seats, a worker at home was matched to that rock and walked
                // toward the invader standing on it — a wasted crossing ending
                // in `NoneApplicable` in a room under attack.
                //
                // Pairwise on the raid and on nothing else: the same worker on
                // the same tile beside the same spawn, one hostile apart.
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
                    (Some(taskId (Withdraw "can-out")))
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
            // Read against the fleet, one body at a time: a colony standing
            // exactly at its target casts nothing, and the same colony one
            // body short casts one — so a target that moved by n shows up as
            // n bodies and cannot hide inside a spawn's one-cast-a-tick
            // limit.
            let casts colony fleet =
                spawnIntents
                    (decide { colony with Creeps = fleet } Map.empty Set.empty None).Intents

            let short fleet =
                List.truncate (List.length fleet - 1) fleet

            test "an outpost rock with nothing built on it moves no row of the target" {
                // ADR 0042's exclusion, read forward rather than backward:
                // the room is projected, held by us and its rock is pooled
                // for Harvest, and still the colony hires exactly the fleet
                // it hired without it. Until a container stands, an outpost
                // is invisible to every quota.
                //
                // Pairwise, one rival at a time: the two colonies differ in
                // the outpost rock and in nothing else.
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
                // The switch itself (ADR 0042). One tick's difference — a
                // container standing on the outpost rock's one Seat — and
                // the colony hires six more bodies: the Anchor for the
                // Post the container makes, the one hauler its own round
                // trip adds to the colony's rounded-once pool (ADR 0049),
                // and the four workers the rock's own output feeds once
                // those rows are amortized.
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
                // ADR 0042 pins the outpost's Anchor on the *same* row as
                // the home room's, walked to its Post by travel cost like
                // any other body — no remote-miner row, no second sizing
                // rule. So the proof is a swap at a fixed headcount: one
                // body short of the target the colony casts a worker while
                // both Anchors stand, and the same eleven bodies with the
                // outpost's Anchor spelled as a worker cast an Anchor
                // instead. Only a row gap can move between the two, because
                // the deficit is one either way.
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
                // The pool's half of the switch. A rock is pooled for
                // Harvest whichever room it stands in and whether or not it
                // is posted (ADR 0041) — one pool, one ranking — so the
                // container standing adds no Task by standing. It adds one
                // the tick it holds energy, and that Task is a Withdraw on
                // the container itself: nothing here becomes a Refill
                // target, because an outpost's container is no upgrade
                // buffer of a controller a room away (ADR 0010) — the join
                // that answers that is pinned by `roomLayerTests`, on a
                // fixture that has a controller to be wrong about.
                //
                // Standing is not the container's only way into the pool,
                // and the ticket's own Trap names the other: a container is
                // a repairable kind, so once its hits fall under half its
                // max `hungryStructures` pools a cross-room `Repair` for it
                // beside this Withdraw. That is no conflict with ADR 0010 —
                // `isHungry` judges every structure against its own kind's
                // whole line, so an outpost container's decay drags no home
                // container's line with it — and nothing here is decayed.
                Expect.equal
                    (planTasks switchPosted noThreats)
                    (planTasks switchUnposted noThreats)
                    "an empty container standing changes no Task in the pool"

                let stocked =
                    { switchPosted with
                        Spatial =
                            { switchPosted.Spatial with
                                Stores = Map.add "can-out" 500 switchPosted.Spatial.Stores
                            }
                    }

                Expect.equal
                    (List.except (planTasks switchPosted noThreats) (planTasks stocked noThreats))
                    [ Withdraw "can-out" ]
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
                    (List.except (planTasks switchPosted noThreats) (planTasks decayed noThreats))
                    [ Repair "can-out" ]
                    "and decayed it adds exactly one more, its own Repair across the Seam"
            }
        ]
