/// An Anchor between two outposts, and the heavy pin across a border.
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
                // The live failure: the colony's one container stood in the north
                // outpost and the Anchor its Post hired walked west, to a room with no
                // container, because the bare-Seat fallback made those Seats reachable.
                // The west rock's Work Area for this body is empty, so its Task never
                // enters the pool: the factor reads `only-candidate`, not `travel-cost`.
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
                // The other half: a container on the west rock's Seat posts it, and
                // travel cost sends the Anchor to the near one as it always did.
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
                // Live tick #193: an Anchor stepped off its container, its rock fifty
                // ticks from restocking, the home rock dry too. It stays, in digging
                // range of the rock it was hired for.
                let colony = twoPostWindowColony 20 30 (anchor "a1" 0 50)
                let remembered = Map.ofList [ "a1", taskId (Harvest "src-out") ]

                let {
                        Assignments = assignments
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decideFrom remembered colony

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
                // The pairwise half: a light body on the same tile is released and
                // crosses the Seam, because thirty-six tiles cover twenty ticks of waiting.
                let colony = twoPostWindowColony 20 30 (worker "w" 0 50)
                let remembered = Map.ofList [ "w", taskId (Harvest "src-out") ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decideFrom remembered colony

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "w",
                        taskId (Harvest "src-out"),
                        ReleaseReason.Rejected(RejectReason.TooEarly(0, 30))
                    ))
                    "a light body beside a dry rock is released as it always was"

                Expect.equal
                    (harvesters assignments "src-home")
                    [ "w" ]
                    "and the walk home covers the wait, so it sets out"
            }
        ]
