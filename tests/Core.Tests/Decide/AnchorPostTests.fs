/// A Post's occupancy, what its garrison digs, and the Work ceiling its
/// source saturates at.
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
                // `CapScope.Garrisons` once counted the Post's holders, so a
                // garrison holding no Task this tick read as an empty Post,
                // and the Matcher walks candidates in view order: the body
                // ninety-six ticks away was offered the Post first. The cap
                // reads the tiles.
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
                // The pairwise rival, one tile apart: the census reads the
                // Post itself, not the ground around it.
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

            test "an expiring garrison still hands its Post on" {
                // The discount is for an incumbent dead when the candidate
                // arrives: ten ticks left against a walk of ninety-six, so
                // the Post reads vacant and the succession goes through.
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

        ]

[<Tests>]
let anchorDigTests =
    testList
        "a Post is worth what its garrison digs"
        [
            test "at a 300 bank the rock's rate is a ceiling nothing reaches" {
                // `heldRateOf` prices this owned room's rocks at ten a
                // tick, and the Anchor row's cast at a 300 bank is
                // `2W/1C/1M`, which digs four: the rate is only the ceiling.
                // Read at the rate the colony counted 20 a tick, hired 19
                // workers and 3 haulers, and stood 12 idle. The income base
                // is 8 a tick, so the worker row is
                // ceil((8 × 1500 − 900) / 1500) = 8, and the hauler row is
                // ceil((24 + 24) × 4 / 200) = one body where the rate hired
                // three.
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
                // One input moves — who holds the spawn room — and at a bank
                // whose Anchor digs four the answer does not: four is under
                // the neutral five as well as the held ten.
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
                // At a bank whose Anchor row casts six Work it digs twelve a
                // tick, over the owned rock's ten and the neutral five, so
                // the ceiling binds and neutralising the room moves the
                // target by a body. Sized to the neutral target — 2 Anchors
                // of three Work, 1 hauler, and
                // ceil((10 × 1500 − 1,400 − 1,800) / (9 × 1500)) = 1 worker.
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
                // The ceiling is a source's saturation plus one spare, and a
                // source under no reservation regenerates 1,500 over 300
                // ticks instead of 3,000: five Work saturate the held rock
                // and two the neutral one. The 1,300 bank would buy twelve.
                // Only who holds W1N2 moves between the two calls.
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
                // The same neutral W1N2 with the colony's own two Posts
                // added and a fleet of one worker: three Posts empty. Every
                // cast is bought under the dearest vacancy's rock, since
                // travel cost pins the finished body on whichever Post is
                // nearest. Two spawns and a 1,300 bank buy exactly one: 700
                // for six Work, and the 600 left cannot pay a second, so the
                // second spawn yields the seat rather than spend 400 on the
                // neutral Post's `3W/1C/1M`.
                Expect.equal
                    (anchorCastsBy (anchorCapColony true [ "W1N2", neutralRoom ]))
                    [ sixWork ]
                    "the home room's held rock keeps its own replacement at six Work, and the neutral rock beside it buys nothing"
            }

            test "the colony's own room is capped exactly where it always was" {
                // Owned, with no outpost in the projection: the shape every
                // existing Anchor test is written on, read for the ceiling
                // alone.
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
                // Unpriceable is not half: W1N2 carries no control entry, so
                // the rock contributes no saturation to the fold, and a fold
                // with nothing priceable answers the held ceiling — the
                // largest, and the safe direction to be wrong in.
                Expect.equal
                    (anchorCastBy (anchorCapColony false []))
                    sixWork
                    "a rock nobody can price caps nothing, and the row keeps the held ceiling"
            }

            test "the row is charged the body it would cast, not the held one" {
                // `workforceTarget` deducts the Anchor row's replacement cost
                // from income before the surplus is divided into worker
                // places, so charging six Work for a row that casts three
                // hires one mouth fewer than the income feeds. Three Anchors
                // and nineteen workers spend this colony's 15 a tick;
                // charged at the held ceiling the target is 21, not 22.
                let casts workers =
                    spawnIntents (decideOn (anchorChargeColony false workers)).Intents
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
                // Five Posts over two rates: Post by Post the row is charged
                // 2 × 700 + 3 × 400 = 2,600; the quota times its richest
                // ceiling charges 5 × 700 = 3,500. At a 1,600 bank a worker
                // place is 12,000 of lifetime drain and the 900 straddles
                // one: 11 against 10. Held, all five saturate at six Work
                // and the two readings agree at 12.
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
