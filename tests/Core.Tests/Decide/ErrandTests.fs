/// The [[errand]] as `decide` sees it (ADR 0060 decision 1, ADR 0057 decision
/// 5): the one Task a declared controller-less room offers, who may hold it,
/// what caps it, and the act it fires — which is fired on a tick the target is
/// **not ours** and on no other. The projection half of an errand — what a
/// colony may carry of that room at all — is `ViewTests`' and stays there; this
/// is the work the declaration buys, so it lives in `Decide`'s own suite beside
/// the outposts' rather than in the view's.
module Fabot.Core.Tests.Decide.ErrandTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests.Decide.Fixtures

/// The errand these cases run and the tiles they name are `Fixtures`' own
/// (#318) — one spelling for this suite and the reserver row's, which reads the
/// same declaration for what it costs. The room is a neighbour of the fixtures'
/// home, so the chain is one crossing and nothing here is about the walk: the
/// live errand is three crossings out and `RoomSeamTests` prices it.
let private errandRoom = reactorErrand.RoomName
let private reactor = reactorId
let private ringTile = reactorRing

/// A CLAIM body of ours: `[Claim; Move]`, 650 energy, the one block the
/// reserver row casts and so the one the re-claimer is (ADR 0057 decision 5,
/// ADR 0006 — a second pattern row would be the same block under a second
/// name).
let private claimer name =
    creepWith name 0 0 [ BodyPart.Claim; Move ]

/// The shared declaration with the given bodies standing on the given tiles of
/// the errand room, and the owner entry the act is gated on: `None` leaves it
/// out altogether, which is what a gapped relay reads and what the act treats
/// as *not ours* (ADR 0004).
let private errandColony owner creeps (colony: ColonyView) =
    colony |> withReactorErrand |> withReactorOwner owner |> standingInErrand creeps

/// The fixtures' home room with nothing of its own to offer: no controller, no
/// refillable with room, no source placed — so the pool a case reads is the
/// errand's and the comparison is pairwise (the orchestration note: a pool
/// holding three rivals proves nothing about the two that lost).
let private bareHome =
    { bareRespawn with
        Sources = []
        Controller = None
        Refillables = []
        Spatial = openRoom 6
        RoomControl = homeControl
    }

/// The Reclaims this tick's pool offers.
let private reclaimsOf (colony: ColonyView) =
    planTasksOn colony noThreats
    |> List.filter (function
        | Reclaim _ -> true
        | _ -> false)

/// The Task **id** one named body holds this tick, which is what the assignment
/// table is keyed in. `holds` below is what a case should reach for: it takes
/// the Task itself, so a case names the Task and never spells the string.
let private assignedTask name (colony: ColonyView) =
    let { Assignments = assignments } = decideOn colony
    Map.tryFind name assignments

/// The Task id of one named body's assignment, for the two cases that compare
/// against a Task rather than reading one back.
let private holds name task (colony: ColonyView) =
    assignedTask name colony = Some(taskId task)

/// The `ClaimReactor` Intents one tick emits.
let private reclaimIntents (colony: ColonyView) =
    let { Intents = intents } = decideOn colony

    intents
    |> List.choose (function
        | ClaimReactor(name, id) -> Some(name, id)
        | _ -> None)

[<Tests>]
let errandTaskTests =
    testList
        "the errand's own Task: one per declaration, held by a CLAIM body, one at a time"
        [
            test "one Reclaim per declared errand, named for the target the declaration names" {
                // ADR 0060 decision 1's narrowing said in the pool: the errand
                // room's one target is work and nothing else in that room is,
                // so the Task is pooled off the **declaration** and never off a
                // kind census — which is what keeps `Errand.place`'s kind-less
                // target enumerable by no pool that sweeps a kind.
                let colony = bareHome |> errandColony (Some Ownership.Rival) []

                Expect.equal
                    (reclaimsOf colony)
                    [ Reclaim reactor ]
                    "the declared reactor's Reclaim, and one of it"

                Expect.isFalse
                    (Map.containsKey reactor colony.Spatial.TargetKinds)
                    "the premise: the target is classified by nothing, so no kind sweep found it"
            }

            test "a colony that declares no errand pools no Reclaim" {
                // The other half of the same rule: the Task is the
                // declaration's, so a colony without one offers none however
                // much of that room it can see.
                let colony =
                    bareHome
                    |> errandColony (Some Ownership.Rival) []
                    |> fun c -> { c with Errands = [] }

                Expect.isEmpty
                    (reclaimsOf colony)
                    "no declaration, no Task — the room's vision buys nothing"
            }

            test "a CLAIM body may hold it and a worker may not" {
                // Part arithmetic and nothing else (ADR 0006, ADR 0057
                // decision 5): `claimReactor` is a CLAIM part's act, and a
                // re-claimer carries nothing and asks for no energy state.
                // Pairwise: one rival at a time, and the pool holds one Task.
                let withClaimer =
                    bareHome |> errandColony (Some Ownership.Rival) [ claimer "rc", ringTile ]

                let withWorker =
                    bareHome |> errandColony (Some Ownership.Rival) [ worker "wk" 0 50, ringTile ]

                Expect.isTrue
                    (withClaimer |> holds "rc" (Reclaim reactor))
                    "the CLAIM body takes it"

                Expect.isFalse
                    (withWorker |> holds "wk" (Reclaim reactor))
                    "and the worker standing on the very same tile does not"
            }

            test "one body at a time: the relay is never a garrison of two" {
                // ADR 0057 decision 5's capacity, and the Reserve's own
                // argument one room further out: the flag is taken by one
                // touch of one CLAIM part, so a second body beside it buys
                // nothing at all.
                let colony =
                    bareHome
                    |> errandColony
                        (Some Ownership.Rival)
                        [ claimer "rc", ringTile; claimer "rc2", { X = 24; Y = 43 } ]

                let held =
                    [ "rc"; "rc2" ]
                    |> List.filter (fun name -> colony |> holds name (Reclaim reactor))

                Expect.equal
                    (List.length held)
                    1
                    "one holder, whichever of the two travel cost picks"
            }

            test "the seat is handed over at death, which is what the cap counted at arrival means" {
                // **The relay's actual handover, pinned because the row was
                // nearly shipped with a knob asserting the opposite** (#318).
                // `Reclaim`'s capacity is one and ADR 0026 counts a holder
                // against a candidate at that *candidate's arrival*, so the
                // incumbent blocks the relief exactly while it would still be
                // alive when the relief lands. The relief is admitted — and so
                // walks at all, an unassigned body having no Work Area to be
                // walked to — only once the incumbent can no longer outlive its
                // walk, and lands as the incumbent dies.
                //
                // The two do hold the Task together for that last stretch, and
                // that is the cap doing its job rather than leaking: a cap
                // counted at arrival admits two bodies whose stays do not
                // overlap. What it never admits is two bodies *standing* there,
                // which is the "relay and never a garrison of two" the case
                // above pins.
                //
                // Three plain tiles between the relief and the ring, so the
                // walk is three ticks for a one-fatigue-part body, and the
                // incumbent's life is swept across it. That is the whole shape
                // of the live relay at a fiftieth of the distance, and it is
                // why the overlap knob #318 nearly shipped was withdrawn
                // rather than tuned: leading the incumbent further casts the relief
                // earlier, this line does not move, and the extra ticks are
                // spent beside the spawn holding no Task at all.
                let relayAt life =
                    let colony =
                        bareHome
                        |> errandColony
                            (Some Ownership.Rival)
                            [
                                claimer "rc" |> withLife life, ringTile
                                claimer "relief", { X = 25; Y = 40 }
                            ]

                    [ "rc"; "relief" ]
                    |> List.filter (fun name -> colony |> holds name (Reclaim reactor))

                Expect.equal
                    (relayAt 3)
                    [ "rc" ]
                    "with three ticks left the incumbent still outlives the relief's three-tick walk, so the relief is offered nothing and stands where it is"

                Expect.equal
                    (relayAt 2)
                    [ "rc"; "relief" ]
                    "one tick under it the relief is let in beside the incumbent — which is the handover, and it lands as the incumbent dies"
            }
        ]

[<Tests>]
let errandActTests =
    testList
        "the act: claimReactor on a tick the reactor is not ours, and on no other"
        [
            test "a rival holds it, so the body standing on the ring takes it back" {
                // The board this row was cut for (ADR 0060 decision 3): W15S25
                // is `Odiodin`'s, any Thorium delivered scores for him, and
                // `claimReactor` has no cooldown and no ownership precondition
                // — so the claim is the **first** act of the programme and
                // fires against a standing owner.
                let colony =
                    bareHome |> errandColony (Some Ownership.Rival) [ claimer "rc", ringTile ]

                Expect.equal
                    (reclaimIntents colony)
                    [ "rc", reactor ]
                    "the act is issued against the rival's ownership"
            }

            test "it is ours already, so the body stands there and says nothing" {
                // "Every other tick the body stands there and says nothing,
                // which is what resident means" (ADR 0057 decision 5). The act
                // is withheld and the Task is not: the body keeps its Reclaim,
                // holds the tile and goes on being the colony's only eye on the
                // room.
                let colony =
                    bareHome |> errandColony (Some Ownership.Ours) [ claimer "rc", ringTile ]

                Expect.isEmpty (reclaimIntents colony) "no act on a tick the flag is already ours"

                Expect.isTrue
                    (colony |> holds "rc" (Reclaim reactor))
                    "and the Task is still held, which is the whole of standing guard"
            }

            test "nobody holds it, which is not ours either" {
                // An unowned reactor consumes nothing and scores for nobody,
                // and it is still a flag we do not have: the engine's
                // `claimReactor` checks no ownership at all, so taking an empty
                // one costs the same act as taking a rival's.
                let colony =
                    bareHome |> errandColony (Some Ownership.Unowned) [ claimer "rc", ringTile ]

                Expect.equal
                    (reclaimIntents colony)
                    [ "rc", reactor ]
                    "an unowned reactor is claimed on the same terms"
            }

            test "no owner entry at all is not ours: the act fires the tick vision arrives" {
                // ADR 0004's per-entry absence, read the safe way round. The
                // relay is the colony's only vision of the room, so the tick a
                // body lands is the first tick there is an answer — and a
                // withheld act on a missing fact would leave the flag with
                // whoever planted it until the *next* tick.
                let colony = bareHome |> errandColony None [ claimer "rc", ringTile ]

                Expect.equal
                    (reclaimIntents colony)
                    [ "rc", reactor ]
                    "absence reads as not-ours, and the act is issued"
            }

            test "two tiles off is not adjacent, so nothing is issued" {
                // `claimReactor` is a Chebyshev-1 act (`creep.claimReactor.js`,
                // verified against `mod-season5` da59118), which is the range
                // the Work Area is the ring for: a body still walking in emits
                // no act it would be refused for.
                let colony =
                    bareHome
                    |> errandColony (Some Ownership.Rival) [ claimer "rc", { X = 25; Y = 42 } ]

                Expect.isEmpty (reclaimIntents colony) "the act waits for the ring"
            }
        ]
