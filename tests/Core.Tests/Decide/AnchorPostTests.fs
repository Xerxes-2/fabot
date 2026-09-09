/// A Post's occupancy, what its garrison digs, and the Work ceiling its
/// source saturates at (ADR 0021, ADR 0051).
module Fabot.Core.Tests.Decide.AnchorPostTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.AnchorFixtures

[<Tests>]
let postGarrisonTests =
    testList
        "a manned Post is never vacant"
        [
            test "a heavy body standing on the one Post holds it while holding nothing" {
                // #269, and the older half of it — the mechanism predates
                // #258's widening. `Capacity.Garrisons` counted the Post's
                // *holders*, so a rock whose garrison happened to hold no
                // Task this tick read as an empty Post to every heavy body
                // in the colony, and the Matcher walks its candidates in
                // view order: the body ninety-six ticks of lane away is
                // offered the Post first and takes it, and the body already
                // standing on it is told `none-free` and moves off. The cap
                // reads the tiles now, so the census answers where a body
                // *is* rather than what it was assigned last tick.
                let colony =
                    pinnedCrowd
                        0
                        [
                            anchor "a1" 0 50, { X = 35; Y = 10 }
                            anchor "g1" 0 50, { X = 11; Y = 10 }
                        ]

                let {
                        Assignments = assignments
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decideOn colony

                Expect.contains
                    verdicts
                    (Verdict.Unassigned("a1", IdleReason.NoneFree))
                    "the Post is manned, and a body standing on one is what mans it"

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "g1" ]
                    "so the rock goes to the body already on its Post"

                Expect.isEmpty
                    (moveIntentsFor "a1" intents)
                    "and nothing crosses the room for a tile that is taken"
            }

            test "one tile off the Post it holds nothing, and the walk is offered" {
                // The pairwise rival, one tile apart: what the census reads
                // is the Post itself and not the ground around it (ADR
                // 0024). The same body on (11,11) is beside the Post rather
                // than on it, the rock reads vacant, and the distant Anchor
                // is dispatched exactly as it was before #269 — which is
                // also what keeps the bumped-garrison window of ADR 0048
                // from locking the rock against its own successor.
                let colony =
                    pinnedCrowd
                        0
                        [
                            anchor "a1" 0 50, { X = 35; Y = 10 }
                            anchor "g1" 0 50, { X = 11; Y = 11 }
                        ]

                let {
                        Assignments = assignments
                        Intents = intents
                    } =
                    decideOn colony

                Expect.equal
                    (harvesters assignments "src-a")
                    [ "a1" ]
                    "a Post with nobody standing on it is a Post the cap admits"

                Expect.isNonEmpty (moveIntentsFor "a1" intents) "and the body offered it sets out"
            }

            test "the garrison of a bare Dual Seat holds its Post through an Upgrade" {
                // The live shape #269 was filed on, and the window ADR 0025's
                // gate names and declines to cure. A drained rock releases
                // its Dual Seat Anchor `too-early` — the empty-window
                // reprieve subtracts a bare Dual Seat (ADR 0048) — and the
                // controller is two tiles away, so the released body spends
                // the window upgrading from the very tile it will dig from
                // in sixty ticks. Counting Harvest's holders alone, the Post
                // read vacant for those sixty ticks and a second Anchor
                // twenty tiles down the lane was dispatched onto it.
                let colony =
                    dualSeatLaneColony
                        60
                        [
                            anchor "a1" 50 10, { X = 11; Y = 10 }
                            anchor "a2" 0 50, { X = 31; Y = 9 }
                        ]

                let remembered = Map.ofList [ "a1", taskId (Harvest "src-a") ]

                let {
                        Assignments = assignments
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decide colony remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("a1", taskId (Harvest "src-a"), ReleaseReason.TooEarly(0, 60)))
                    "the bare Dual Seat carries no empty-window reprieve"

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "so it spends the window on the controller two tiles away"

                Expect.contains
                    verdicts
                    (Verdict.Unassigned("a2", IdleReason.NoneFree))
                    "and the Post it is standing on is not vacant for holding something else"

                Expect.isEmpty (harvesters assignments "src-a") "the drained rock waits"

                Expect.isEmpty
                    (moveIntentsFor "a2" intents)
                    "and nothing walks twenty tiles onto an occupied tile"
            }

            test "an expiring garrison still hands its Post on" {
                // The half of ADR 0026 the widened census must not eat. The
                // discount is for an incumbent that will be **dead** when
                // the candidate arrives, and the tile census takes it at
                // arrival like every other holder: a garrison with ten
                // ticks left against a walk of ninety-six is not standing
                // there when the successor lands, so the Post reads vacant
                // and the succession the row cast for goes through. Pairwise
                // against the first case above, one field apart.
                let colony =
                    pinnedCrowd
                        0
                        [
                            anchor "a1" 0 50, { X = 35; Y = 10 }
                            anchor "g1" 0 50 |> withLife 10, { X = 11; Y = 10 }
                        ]

                let { Assignments = assignments } = decideOn colony

                Expect.equal
                    (harvesters assignments "src-a" |> List.sort)
                    [ "a1"; "g1" ]
                    "two Anchors against one Post for the lead's duration is the succession"
            }

            test "a rock with a Post to spare admits a second garrison" {
                // The union, and the reason the widened census is not a sum
                // (#269). On a standing container the garrison holds the
                // Harvest it is standing on — ADR 0024's overflow reprieve
                // keeps it applicable through a full store — so the holder
                // list and the tile census name the same body. Added, that
                // body would spend both of this rock's Posts and the second
                // would read full while it stands empty; unioned, it counts
                // once and the second Post hires.
                let colony =
                    twoPostCrowd
                        [
                            anchor "a1" 0 50, { X = 35; Y = 10 }
                            anchor "g1" 0 50, { X = 11; Y = 10 }
                        ]

                let remembered = Map.ofList [ "g1", taskId (Harvest "src-a") ]

                let { Assignments = assignments } = decide colony remembered Set.empty None

                Expect.equal
                    (harvesters assignments "src-a" |> List.sort)
                    [ "a1"; "g1" ]
                    "one body on one of two Posts is one garrison, not two"
            }

            test "both Posts manned, the third heavy body is refused" {
                // The pairwise rival of the case above, one body apart: the
                // widened census still counts, and a rock whose every Post
                // carries a standing heavy body is full whatever those
                // bodies hold.
                let colony =
                    twoPostCrowd
                        [
                            anchor "a1" 0 50, { X = 35; Y = 10 }
                            anchor "g1" 0 50, { X = 11; Y = 10 }
                            anchor "g2" 0 50, { X = 9; Y = 10 }
                        ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decideOn colony

                Expect.contains
                    verdicts
                    (Verdict.Unassigned("a1", IdleReason.NoneFree))
                    "two Posts, two garrisons standing on them, and no third slot"

                Expect.equal
                    (harvesters assignments "src-a" |> List.sort)
                    [ "g1"; "g2" ]
                    "and the rock is worked by the bodies already on it"
            }
        ]

[<Tests>]
let anchorDigTests =
    testList
        "a Post is worth what its garrison digs"
        [
            test "at a 300 bank the rock's rate is a ceiling nothing reaches" {
                // #208's live defect, pinned at the fixture that always
                // held it. `heldRateOf` prices this owned room's rocks at
                // ten a tick, and the Anchor row's cast at a 300 bank is
                // `2W/1C/1M`, which digs four: a Post yields what the body
                // garrisoning it takes out of the rock, and the rate is
                // only the ceiling on that. Read at the rate the colony
                // counted 20 a tick, hired 19 workers and 3 haulers, and
                // stood 12 of them idle beside a spawn already full.
                //
                // Two readings of one number, and both move together
                // (ADR 0042): the income base is 8 a tick, so the worker
                // row is ceil((8 × 1500 − 900) / 1500) = 8, and the hauler
                // row ships 8 rather than 20 — ceil((24 + 24) × 4 / 200) =
                // one body where the rate hired three.
                Expect.equal
                    (quotaOf incomeColony)
                    1
                    "the two Posts ship what two 2W bodies dig, not what the room would pay"

                Expect.isEmpty
                    (spawnIntents
                        (decide
                            { incomeColony with
                                Creeps = incomeFleet
                            }
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "and eight workers, not eighteen, are the whole of the row"

                match
                    spawnIntents
                        (decide
                            { incomeColony with
                                Creeps = List.truncate (List.length incomeFleet - 1) incomeFleet
                            }
                            Map.empty
                            Set.empty
                            None)
                            .Intents
                with
                | [ (_, _, creepName) ] ->
                    Expect.stringStarts
                        creepName
                        "worker-"
                        "and it is tight: one body short and the colony casts a worker"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "under that ceiling the room's own rate moves nothing" {
                // The cap, pinned as a cap: one input moves — who holds
                // the spawn room — and at a bank whose Anchor digs four
                // the answer does not, because four is under the neutral
                // five as well as under the held ten. A rule that took the
                // room's rate, or the smaller of the two only sometimes,
                // would part these two colonies here.
                let neutralised =
                    { incomeColony with
                        Creeps = incomeFleet
                        RoomControl = homeControl |> Map.map (fun _ _ -> neutralRoom)
                    }

                Expect.equal
                    (quotaOf neutralised)
                    (quotaOf incomeColony)
                    "the same haul: what the garrison digs is what either room's rock ships"

                Expect.isEmpty
                    (spawnIntents (decideOn neutralised).Intents)
                    "and the same fleet is the whole target, held or not"
            }

            test "at an 1800 bank the cast outruns the rock and the rate is the answer again" {
                // The other half of the pair, and the reason the rule is
                // `min` and not a discount: the same geometry at a bank
                // whose Anchor row casts six Work digs twelve a tick, over
                // the ten an owned rock pays and over the five a neutral
                // one does — so the ceiling binds, the rate is the answer,
                // and neutralising the room moves the target by a body
                // where at 300 it moved nothing.
                //
                // Sized to the neutral target — 2 Anchors of three Work
                // each, 1 hauler, and ceil((10 × 1500 − 1,400 − 1,800) /
                // (9 × 1500)) = 1 worker — so the neutral colony has no
                // gap and the owned one does.
                let neutralised =
                    { richestIncomeColony with
                        Creeps = richestIncomeFleet 1
                        RoomControl = homeControl |> Map.map (fun _ _ -> neutralRoom)
                    }

                Expect.isEmpty
                    (spawnIntents (decideOn neutralised).Intents)
                    "the premise: at five a tick these four are the whole target"

                Expect.isNonEmpty
                    (spawnIntents
                        (decide
                            { neutralised with
                                RoomControl = homeControl
                            }
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "owned, the same rocks are worth ten each and the fleet is a body short"
            }
        ]

[<Tests>]
let anchorWorkCapTests =
    testList
        "the Anchor row's Work ceiling"
        [
            test "the same rock caps the Anchor row at six Work reserved and three unreserved" {
                // ADR 0021's rule, ADR 0042's number: the ceiling is a
                // source's saturation plus one spare, and a source under no
                // reservation regenerates 1,500 over 300 ticks instead of
                // 3,000. Five Work saturate the held rock and two the
                // neutral one, so the ceilings are six and three — and the
                // 1,300 bank standing behind both would buy twelve.
                //
                // One rock, one field, one fleet: only who holds W1N2 moves
                // between the two calls.
                Expect.equal
                    (anchorCastBy (anchorCapColony false [ "W1N2", reservedRoom true 4000 ]))
                    sixWork
                    "reserved, the rock gives ten a tick and the row buys the six Work that dig it"

                Expect.equal
                    (anchorCastBy (anchorCapColony false [ "W1N2", neutralRoom ]))
                    threeWork
                    "unreserved it gives five, and three Work drain it as fast as it fills"
            }

            test "a neutral outpost Post does not shrink the ceiling the home room asks for" {
                // The direction the pairing is wrong in, pinned pairwise
                // against the case above: the same neutral W1N2, the same
                // rock, the same field — the colony's own two Posts are the
                // only thing added, and the fleet is one worker, so all
                // three Posts stand empty and the row is three bodies short.
                //
                // Every cast this tick is bought under the **dearest
                // vacancy's** rock (ADR 0053), because a cast is a body and
                // not a posting: travel cost pins the finished body on
                // whichever Post is nearest once it is alive (ADR 0021's own
                // rejection of sizing by the Post), so with several
                // vacancies open the colony cannot steer any of these
                // bodies and buys every one of them for the dearer rock.
                // Under-sizing an Anchor for a held rock loses four energy a
                // tick for the body's whole life; over-sizing one for a
                // neutral rock wastes 300 energy once in 1,500 ticks and
                // still digs everything the rock has.
                //
                // Two spawns and a 1,300 bank buy exactly one of the three:
                // 700 for a home Post's six Work, and the 600 left cannot
                // pay for a second six-Work body — so the second spawn
                // yields the seat (ADR 0050) rather than spending 400 on the
                // neutral Post's `3W/1C/1M`. Which is the whole of why the
                // ceiling is the dearest vacancy's and not each vacancy's
                // own: both of this colony's held Posts are a few tiles from
                // the spawns and the neutral one is a Seam away, so a body
                // bought for the outpost's hole lands on a held rock and
                // digs six where the rock gives ten.
                Expect.equal
                    (anchorCastsBy (anchorCapColony true [ "W1N2", neutralRoom ]))
                    [ sixWork ]
                    "the home room's held rock keeps its own replacement at six Work, and the neutral rock beside it buys nothing"
            }

            test "the colony's own room is capped exactly where it always was" {
                // The regression ADR 0042 promises: "unchanged as a rule and
                // changed as a number", and the colony's own number does not
                // move. Owned, with no outpost in the projection at all —
                // the case every existing Anchor test is written on, read
                // here for the ceiling alone.
                Expect.equal
                    (anchorCastBy
                        { incomeColony with
                            Bank = bank 1300 1300
                            Creeps = [ worker "w1" 0 50 ]
                        })
                    sixWork
                    "two held Posts and a 1,300 bank: the six-Work Anchor of ADR 0021"
            }

            test "a Post the colony cannot price this tick leaves the ceiling where it was" {
                // ADR 0004, entry by entry, and the same separation the
                // source rate keeps: unpriceable is not half. W1N2 carries
                // no control entry here, so nobody knows who holds it —
                // the rock contributes no saturation to the fold rather
                // than the neutral one, and a fold with nothing priceable
                // in it answers the held ceiling, which is the largest the
                // rule gives and the safe direction to be wrong in.
                //
                // Pinned strictly against the neutral case above: seen and
                // held by nobody the same rock casts three Work.
                Expect.equal
                    (anchorCastBy (anchorCapColony false []))
                    sixWork
                    "a rock nobody can price caps nothing, and the row keeps the held ceiling"
            }

            test "the row is charged the body it would cast, not the held one" {
                // The other half of #132's landing note — "the price the row
                // is charged must be the body the row is cast at" — and the
                // half no cast body can show: `workforceTarget` deducts the
                // Anchor row's replacement cost from the income before the
                // surplus is divided into worker places (ADR 0012, ADR
                // 0042), so charging six Work for a row that casts three
                // hires an upgrade mouth fewer than the income really feeds.
                //
                // Read as the income base's cases are read, pairwise across
                // one fleet: three Anchors and nineteen workers is the whole
                // of what this colony's 15 energy a tick pays for, so the
                // tick casts nothing; one worker short of it, the row that
                // is short is the worker row and the tick says so. Charged
                // at the held ceiling the target is 21 instead of 22, and
                // the fleet of 21 below has no gap at all.
                let casts workers =
                    spawnIntents
                        (decide (anchorChargeColony false workers) Map.empty Set.empty None).Intents
                    |> List.map (fun (_, _, name) -> name)

                Expect.isEmpty
                    (casts 19)
                    "three Anchors and nineteen workers: the income base is spent and the tick casts nothing"

                match casts 18 with
                | [ creepName ] ->
                    Expect.stringStarts
                        creepName
                        "worker-"
                        "one short of it the worker row is short, which the held charge would not have hired"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "the charge is one body a Post and not the quota times one ceiling" {
                // **ADR 0053's other half.** The test above is written on a
                // colony whose Posts agree — three neutral rocks, one
                // ceiling between them — where a quota times that ceiling
                // and a sum over the Posts are the same number. This is the
                // colony they part on: the same three neutral rocks with
                // the colony's own two held Posts kept beside them, five
                // Posts over two rates. Post by Post the row is charged
                // 2 x 700 + 3 x 400 = 2,600; the quota times its richest
                // ceiling charges 5 x 700 = 3,500, and the 900 between them
                // is an upgrade mouth the income really feeds.
                //
                // At a 1,600 bank because the target is an integer: the
                // surplus is divided into worker places of a whole body's
                // Work drain over a lifetime (ADR 0037), which is 12,000
                // energy here, and 900 moves the target only where it
                // straddles one. It does here — 11 against the 10 the
                // aggregate charge answers — and at 1,400, the bank the
                // test above is written at, it does not.
                //
                // Pairwise on the outpost room's reservation alone, which
                // is what makes the reading a pairing and not a number:
                // held, all five Posts saturate at six Work, the two
                // readings are the same sum by construction, and the target
                // is 12. The arms differ by more than the charge — a held
                // rock also pays twice the income — and it is the neutral
                // arm that carries the discrimination.
                let target held =
                    let colony = anchorChargeColony true 3

                    { colony with
                        Bank = bank 1600 1600
                        RoomControl =
                            Map.add
                                "W1N2"
                                (if held then reservedRoom true 5000 else neutralRoom)
                                colony.RoomControl
                    }
                    |> fun colony -> (decideOn colony).Quotas.Target

                Expect.equal
                    (target false)
                    11
                    "three neutral Posts charged at their own three Work, beside two held ones charged at six"

                Expect.equal
                    (target true)
                    12
                    "and where every Post saturates alike the sum over them is the quota times the one ceiling"
            }
        ]
