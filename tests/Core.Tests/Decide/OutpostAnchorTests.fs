/// An Anchor between two outposts, and the heavy pin across a border
/// (ADR 0048).
module Fabot.Core.Tests.Decide.OutpostAnchorTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.OutpostFixtures

[<Tests>]
let twoOutpostAnchorTests =
    testList
        "an Anchor between two outposts"
        [
            test "an unposted outpost rock is not a rival, however near it stands" {
                // The live failure: the colony's one container stood in the
                // north outpost, the Anchor its Post hired was born in the
                // spawn room, and it walked *west* — to a room with no
                // container at all — because ADR 0020's bare-Seat fallback
                // made those Seats reachable and travel cost had nothing
                // left to say but "nearer".
                //
                // The fix is geometric and not a rank or a quota: the west
                // rock's Work Area for this body is empty, so the Task has
                // no travel cost and never enters the pool. The factor
                // therefore reads `only-candidate` rather than
                // `travel-cost`, which is the whole claim — the near rock
                // is not a rival the far one beat, it is not a candidate.
                Expect.equal
                    (anchorMatch (twoRockColony []))
                    (Some(taskId (Harvest "src-north"), MatchFactor.OnlyCandidate))
                    "the posted rock a room and thirty-eight tiles away is the only Task there is"

                let { Intents = intents } = decide (twoRockColony []) Map.empty Set.empty None

                Expect.equal
                    (moveIntents intents)
                    [ "anchor", Right ]
                    "and the Anchor walks back up the west arm toward the northern Seam"

                Expect.isEmpty
                    (actionIntents intents)
                    "digging nothing on the way: it may not act on a target a room away"
            }

            test "a container standing on the west rock makes it a rival again, and it wins" {
                // The other half, and the only reading under which the case
                // above says anything: nothing here refuses an outpost, or
                // ranks a near room behind a far one. Put a container on the
                // west rock's Seat and that rock is posted, its Work Area is
                // that Post, and travel cost — the one comparison left
                // between two feeding-tier Harvests — sends the Anchor to
                // the near one exactly as it always did.
                Expect.equal
                    (anchorMatch (twoRockColony [ "cont-west", { X = 46; Y = 26 } ]))
                    (Some(taskId (Harvest "src-west"), MatchFactor.TravelCost))
                    "two posted rocks are two candidates, and the near one is cheaper"

                let { Intents = intents } =
                    decide
                        (twoRockColony [ "cont-west", { X = 46; Y = 26 } ])
                        Map.empty
                        Set.empty
                        None

                Expect.equal
                    (moveIntents intents)
                    [ "anchor", Left ]
                    "and the same body walks the other way, two tiles to the western Seam"
            }
        ]

[<Tests>]
let heavyPinAcrossTests =
    testList
        "heavy pin across a border"
        [
            test "an empty window at home does not pull an Anchor out of its outpost" {
                // The live tick #193 reports, in two rooms: an Anchor
                // stepped off its container by a hauler, its own rock
                // fifty ticks from restocking, and a home Post whose rock
                // is dry too. ADR 0025 dispatched it because an Anchor's
                // walk covers any wait; ADR 0048 keeps it where it is,
                // because it is in digging range of the rock it was hired
                // for and there is nothing at the far end of that crossing
                // it can do sooner.
                let colony = twoPostWindowColony 20 30 (anchor "a1" 0 50)
                let remembered = Map.ofList [ "a1", taskId (Harvest "src-out") ]

                let {
                        Assignments = assignments
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decide colony remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Kept("a1", taskId (Harvest "src-out")))
                    "the outpost's Post is held through its window, not surrendered"

                Expect.isEmpty
                    (harvesters assignments "src-home")
                    "and the home Post is nobody's to cross a border for"

                Expect.equal
                    (moveIntentsFor "a1" intents)
                    [ MoveCreep("a1", Top) ]
                    "and the one step it takes is back onto its own Post, away from the Seam"
            }

            test "the same geometry really does dispatch a light body home" {
                // The premise of the case above, and the pairwise half of
                // ADR 0048: nothing here is unreachable, mis-posted or out
                // of the pool. A worker on the very tile the Anchor stands
                // on is released from the outpost rock it is beside and
                // crosses the Seam for the home one, because thirty-six
                // tiles at a tick a tile cover twenty ticks of waiting —
                // which is ADR 0025 exactly as it was written.
                let colony = twoPostWindowColony 20 30 (worker "w" 0 50)
                let remembered = Map.ofList [ "w", taskId (Harvest "src-out") ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide colony remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "w",
                        taskId (Harvest "src-out"),
                        ReleaseReason.TooEarly(0, 30)
                    ))
                    "a light body beside a dry rock is released as it always was"

                Expect.equal
                    (harvesters assignments "src-home")
                    [ "w" ]
                    "and the walk home covers the wait, so it sets out"
            }
        ]
