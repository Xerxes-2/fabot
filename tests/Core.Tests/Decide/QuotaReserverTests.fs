/// The Reserver row and its lead (ADR 0042).
module Fabot.Core.Tests.Decide.QuotaReserverTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.QuotaFixtures

[<Tests>]
let reserverLeadTests =
    testList
        "the reserver row's lead"
        [
            test "a CLAIM body's lead is the reserver row's, not the generalist's" {
                // `patternOf` reads a living body back to the row it was
                // cast from (ADR 0006), and the row is what sizes the
                // replacement a lead prices (ADR 0026). A `[Claim; Move]`
                // body has neither Work nor Carry, so before ADR 0042's row
                // existed it fell through to the generalist and was priced
                // as one.
                //
                // The two arithmetics, at this colony's 1,300 bank: the
                // reserver row casts `[2Claim;2Move]`, four parts, 12 ticks
                // in the spawner, and its two Move carry its two fatigue
                // parts over a plain tile in the walk's one-tick floor — 3
                // ticks for the three steps, a lead of 15. The generalist
                // row at the same bank is twenty parts: 60 ticks in the
                // spawner and the same 3 of walking, a lead of 63. Every
                // life between the two is where the rows disagree.
                let casts life =
                    spawnIntents (decide (leadColony life) Map.empty Set.empty None).Intents

                Expect.isEmpty
                    (casts 30)
                    "at 30 ticks the reserver still counts; read off the generalist row it would not"

                Expect.isEmpty (casts 16) "one tick outside its own row's lead it still counts"

                match casts 15 with
                | [ (_, body, creepName) ] ->
                    Expect.stringStarts
                        creepName
                        "worker-"
                        "at its lead the colony is one short — and casts a generalist, this room declaring no outpost for the reserver row to hire against"

                    Expect.isFalse
                        (List.contains BodyPart.Claim body)
                        "the row that replaces a reserver is the reserver row's quota, and here it is zero"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }
        ]

[<Tests>]
let reserverRowTests =
    testList
        "the reserver row"
        [
            test "a declared outpost hires one reserver, posted or not" {
                // The one quota the container switch does *not* gate
                // (#131's correction comment): gating it deadlocks the
                // chain, because a container site needs vision, vision
                // needs a creep in the room, and this is the only creep
                // with a reason to go. ADR 0042's Considered Options is the
                // authority its Consequences clause contradicts — it
                // rejected "mine first, reserve later" precisely so the
                // reservation is standing before the first hauler is sized.
                //
                // Pairwise on the one structure — same room, same rock,
                // same controller, same reservation — because that is the
                // only input that moves.
                let castsWith posted =
                    let fleet = surplusFleet (if posted then 3 else 2)

                    reserverColony [ northOutpost posted ] fleet [ "W1N2", reservedRoom true 5000 ]
                    |> fun colony -> decide colony Map.empty Set.empty None
                    |> fun result -> spawnIntents result.Intents

                for posted in [ true; false ] do
                    match castsWith posted with
                    | [ (_, body, creepName) ] ->
                        Expect.stringStarts
                            creepName
                            "reserver-"
                            $"the one cast is the reserver row's (posted: %b{posted})"

                        Expect.equal body oneBlock "and its body holds a CLAIM part"
                    | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "a candidate colony hires one body, and it is one block" {
                // ADR 0047's casting clause, and the two things it says at
                // once. The row is *this* row — a claimer is a
                // `[Claim; Move]` body like a reserver, so it is cast, led
                // and amortized where reservers are, and `patternOf` reads
                // one back as the other. And the room hires **one** body,
                // not two: its controller carries a Claim and no Reserve,
                // so a reserver hired for it would arrive at a controller
                // with no Task on it and stand there for its whole
                // 600-tick life.
                //
                // The block count is where the two *demands* are told
                // apart. A reservation sitting 4,000 ticks below its cap
                // asks for seven blocks and takes what the 1,800 bank
                // affords, which is two; a claim is one act by one CLAIM
                // part and asks for one block whatever the reservation has
                // done. Pairwise on the declaration alone — same room, same
                // rock, same reservation, same fleet, same bank.
                //
                // The demand is not the cast: this room's own demand is the
                // whole list here, so the two coincide. The test below is
                // where they come apart.
                let castsWith declared =
                    let colony =
                        reserverColony
                            [ northOutpost false ]
                            (surplusFleet 2)
                            [ "W1N2", reservedRoom true 1000 ]

                    { colony with
                        Declared =
                            if declared then
                                [ SpatialInfo.homeName colony.Spatial; "W1N2" ]
                            else
                                []
                    }
                    |> fun colony -> decide colony Map.empty Set.empty None
                    |> fun result -> reserverCasts result.Intents

                Expect.equal
                    (castsWith false)
                    [ twoBlocks ]
                    "undeclared, the room is an outpost and its lapsed reservation buys the bank's body"

                Expect.equal
                    (castsWith true)
                    [ oneBlock ]
                    "declared a colony, the same room hires one body of one block, and no reserver beside it"
            }

            test "a claimer beside a slipping reservation is cast at the row's largest demand" {
                // The other half of ADR 0047's casting clause, and the half
                // a one-outpost fixture cannot show: the claim's *entry* is
                // one block, but every body this row casts this tick is
                // sized at the largest demand in the list. Which controller
                // a finished CLAIM body ends up holding is the Matcher's,
                // priced by travel cost alone and knowing nothing about
                // which demand paid for which body, so a claimer cast at
                // one block could land on the reservation that has slipped
                // and freeze that room for its whole 600-tick life. The
                // over-buy is the safe direction, and it is the same one
                // `reserverClaimsOf` takes for two slipping reservations.
                //
                // Pairwise on the second outpost alone: the candidate is
                // the same room under the same declaration with the same
                // reservation in both, and what moves is whether an unheld
                // outpost stands beside it.
                let castsWith slippingNeighbour =
                    let outposts =
                        if slippingNeighbour then
                            [ northOutpost false; westOutpost false ]
                        else
                            [ northOutpost false ]

                    let control =
                        [ "W1N2", reservedRoom true 5000 ]
                        @ if slippingNeighbour then [ "W2N2", neutralRoom ] else []

                    let colony = reserverColony outposts (surplusFleet 2) control

                    { colony with
                        Declared = [ SpatialInfo.homeName colony.Spatial; "W1N2" ]
                    }
                    |> fun colony -> decide colony Map.empty Set.empty None
                    |> fun result -> reserverCasts result.Intents

                Expect.equal
                    (castsWith false)
                    [ oneBlock ]
                    "the candidate alone: its one block is the whole list, so the claimer is one block"

                Expect.equal
                    (castsWith true)
                    [ twoBlocks; twoBlocks ]
                    "beside an unheld outpost the whole row is cast at that room's deficit, the claimer with it"

                // And those two bodies are one reserver and one claimer
                // rather than two reservers: the candidate's controller
                // carries the Claim and no Reserve, so one of the two
                // over-bought bodies is the one that will touch a
                // controller once.
                let declared =
                    let colony =
                        reserverColony
                            [ northOutpost false; westOutpost false ]
                            (surplusFleet 2)
                            [ "W1N2", reservedRoom true 5000; "W2N2", neutralRoom ]

                    { colony with
                        Declared = [ SpatialInfo.homeName colony.Spatial; "W1N2" ]
                    }

                Expect.equal
                    (planTasks declared noThreats
                     |> List.filter (function
                         | Reserve _
                         | Claim _ -> true
                         | _ -> false))
                    [ Reserve "ctrl-W2N2"; Claim "ctrl-W1N2" ]
                    "the row's two demands are the unheld outpost's Reserve and the candidate's Claim"
            }

            test "two declared outposts hire two reservers, one apiece" {
                // Never one rover: `[4Claim;4Move]` is 2,600 energy and so
                // an RCL7 body, and two outposts diagonal to each other
                // share no exit — a rover would spend its 600-tick life
                // crossing the home room (ADR 0042).
                let colony =
                    reserverColony
                        [ northOutpost true; westOutpost true ]
                        (surplusFleet 4)
                        [ "W1N2", reservedRoom true 5000; "W2N2", reservedRoom true 5000 ]

                Expect.equal
                    (reserverCasts (decide colony Map.empty Set.empty None).Intents)
                    [ oneBlock; oneBlock ]
                    "one body per declared outpost, and the four idle spawns cast no third"

                let half =
                    reserverColony
                        [ northOutpost true; westOutpost false ]
                        (surplusFleet 3)
                        [ "W1N2", reservedRoom true 5000; "W2N2", reservedRoom true 5000 ]

                Expect.equal
                    (reserverCasts (decide half Map.empty Set.empty None).Intents)
                    [ oneBlock; oneBlock ]
                    "and the one still waiting for its container is hired for just the same"
            }

            test "a bank that cannot buy one block hires no reserver and still casts" {
                // The row's floor body is 650 — larger than every other
                // row's, and larger than the whole bank below RCL3. Being
                // first in the cascade, a gap it can never fill would stop
                // every row under it forever: `planned` counts intents, so
                // an uncast head row leaves every idle spawn asking for the
                // head row again, in the home room included. So a bank that
                // cannot afford the floor hires the row not at all.
                let intentsAt capacity =
                    let colony =
                        reserverColony
                            [ northOutpost false ]
                            [ worker "w1" 0 50 ]
                            [ "W1N2", neutralRoom ]

                    { colony with
                        Bank = bank capacity capacity
                    }
                    |> fun colony -> decide colony Map.empty Set.empty None
                    |> fun result -> result.Intents

                Expect.isEmpty
                    (reserverCasts (intentsAt 550))
                    "at an RCL2 bank of 550 the row has no quota at all"

                match spawnIntents (intentsAt 550) with
                | (_, _, creepName) :: _ ->
                    Expect.stringStarts
                        creepName
                        "anchor-"
                        "and the row under it casts rather than the colony freezing"
                | [] -> failtest "expected the rows under the reserver to cast"

                Expect.equal
                    (reserverCasts (intentsAt 650))
                    [ oneBlock ]
                    "one block's worth of capacity is where the row starts hiring"
            }

            test "a living reserver fills the quota; one inside its lead does not" {
                // The quota counts bodies and not rooms (#130): which
                // controller each body ends up holding is the Reserve
                // Task's one-holder-per-controller capacity, so a reserver
                // still walking to its outpost already fills the row's
                // place. Its succession is ADR 0026's existing path and
                // nothing new: inside its lead it leaves the count, and the
                // replacement is cast while it still holds the reservation.
                let colonyWith reservers =
                    reserverColony
                        [ northOutpost true ]
                        (surplusFleet 3 @ reservers)
                        [ "W1N2", reservedRoom true 5000 ]

                let placed life =
                    let colony = colonyWith [ reserver "r1" |> withLife life ]

                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "r1", { X = 22; Y = 10 } ]
                                })
                    }

                Expect.isEmpty
                    (reserverCasts (decide (placed 1500) Map.empty Set.empty None).Intents)
                    "a reserver with a life ahead of it is the row's one body"

                Expect.equal
                    (reserverCasts (decide (placed 5) Map.empty Set.empty None).Intents)
                    [ oneBlock ]
                    "inside its lead it is already outside the count, so the successor is cast"
            }

            test "the reserver row casts in front of the Anchor, hauler and worker rows" {
                // ADR 0042's ordering: the other three rows spend income
                // and this one decides whether the income is five a tick or
                // ten across every source of an outpost at once — and it is
                // the cheapest body on the table, so the row it displaces
                // for a tick waits on 650 energy.
                //
                // The fleet is short in every row at once: two Anchors
                // against three Posts, no hauler against the one-body
                // quota the two home containers come to at this bank (ADR
                // 0049), and a whole-fleet deficit under all of it. Four
                // idle spawns cast one body each, so the whole order is
                // readable in one tick.
                // The generalist in the fleet is the supply floor's
                // premise and not this case's (ADR 0050): two Anchors are
                // two Carry parts and still nothing that can refill an
                // extension, so without it the row cast first would be the
                // floor's carrier rather than the reservation's.
                let colony =
                    reserverColony
                        [ northOutpost true ]
                        [ anchor "a1" 0 50; anchor "a2" 0 50; worker "w1" 0 50 ]
                        [ "W1N2", reservedRoom true 5000 ]

                match spawnIntents (decide colony Map.empty Set.empty None).Intents with
                | [ (_, firstBody, firstName)
                    (_, _, secondName)
                    (_, _, thirdName)
                    (_, _, fourthName) ] ->
                    Expect.stringStarts firstName "reserver-" "the reservation is cast for first"
                    Expect.equal firstBody oneBlock "at the deficit's own body, not the bank's"
                    Expect.stringStarts secondName "anchor-" "then the empty Post"
                    Expect.stringStarts thirdName "hauler-" "then the throughput quota"
                    Expect.stringStarts fourthName "worker-" "and the generalist last"
                | other -> failtest $"expected exactly four SpawnCreep intents, got %A{other}"
            }

            test "the body grows by a CLAIM part for every 600 ticks the reservation has lost" {
                // ADR 0042's one rule, quota and sizing in the same
                // expression: `ceil((5000 − ticks held) / 600)` CLAIM
                // parts. No state between ticks — the deficit is read off
                // the reservation itself, so the row shrinks to its floor
                // in steady state and comes back bigger on its own the
                // tick a reservation has slipped.
                let castFor held =
                    reserverColony
                        [ northOutpost true ]
                        (surplusFleet 3)
                        [ "W1N2", reservedRoom true held ]
                    |> fun colony -> decide colony Map.empty Set.empty None
                    |> fun result -> reserverCasts result.Intents

                Expect.equal (castFor 5000) [ oneBlock ] "at the cap the deficit is zero: the floor"

                Expect.equal
                    (castFor 4400)
                    [ oneBlock ]
                    "600 ticks lost is one part: one CLAIM holds a reservation up through a whole CLAIM life"

                Expect.equal (castFor 4399) [ twoBlocks ] "the 601st lost tick is the second part"
                Expect.equal (castFor 3800) [ twoBlocks ] "and 1,200 lost is still the second"
            }

            test "the bank truncates the deficit, and the deficit truncates the bank" {
                // The two halves of the sizing rule, each shown cutting the
                // other off. ADR 0042 refuses the bank *as the rule*: at
                // RCL6 a 2,300 bank would buy a third CLAIM for a
                // reservation that caps at 5,000 anyway — which is why the
                // pair below is read at 2,300 and not at today's 1,800,
                // where the two rules agree.
                let castAt capacity held =
                    let colony =
                        reserverColony
                            [ northOutpost true ]
                            (surplusFleet 3)
                            [ "W1N2", reservedRoom true held ]

                    { colony with
                        Bank = bank 8000 capacity
                    }
                    |> fun colony -> decide colony Map.empty Set.empty None
                    |> fun result -> reserverCasts result.Intents

                Expect.equal
                    (castAt 2300 5000)
                    [ oneBlock ]
                    "a full reservation asks for one block, and the RCL6 bank's three do not overrule it"

                Expect.equal
                    (castAt 2300 0)
                    [ List.replicate 3 BodyPart.Claim @ List.replicate 3 Move ]
                    "a reservation on the floor asks for nine parts and gets the three the bank buys"

                Expect.equal
                    (castAt 8000 0)
                    [ List.replicate 9 BodyPart.Claim @ List.replicate 9 Move ]
                    "at a bank that affords them, the deficit's own nine"
            }

            test "a reservation another player holds leaves this colony holding nothing" {
                // Pairwise, one holder at a time: the same room, the same
                // ticks on the same controller, and only whose it is moves.
                // The colony's hold starts at zero under a rival's
                // reservation, exactly as that room's sources stay at the
                // neutral rate — a hold somebody else owns is not one this
                // row can measure its deficit from.
                //
                // Read at a bank that affords the whole deficit and not at
                // the live 1,800, where two parts and nine both truncate to
                // two and the pair could not tell the `Ours` filter from
                // its absence.
                let castWith control =
                    let colony = reserverColony [ northOutpost true ] (surplusFleet 3) control

                    { colony with Bank = bank 8000 8000 }
                    |> fun colony -> decide colony Map.empty Set.empty None
                    |> fun result -> reserverCasts result.Intents

                let nineBlocks = List.replicate 9 BodyPart.Claim @ List.replicate 9 Move

                Expect.equal
                    (castWith [ "W1N2", reservedRoom true 4000 ])
                    [ twoBlocks ]
                    "1,000 ticks lost of ours is two parts"

                Expect.equal
                    (castWith [ "W1N2", reservedRoom false 4000 ])
                    [ nineBlocks ]
                    "the same 4,000 in a rival's name is a deficit of the whole 5,000: nine parts"

                Expect.equal
                    (castWith [])
                    [ nineBlocks ]
                    "and no reservation at all is that same whole deficit"
            }

            test "every cast this tick carries the largest outstanding demand" {
                // The row casts bodies and the Matcher pairs them to
                // controllers, by travel cost alone and knowing nothing
                // about a deficit (#130). So a demand read room by room
                // would land the *nearer* room's small body on the room
                // that has slipped, and a controller held by one CLAIM
                // against the engine's one tick of decay is frozen where it
                // stands for that body's whole 600-tick life. The row
                // over-buys instead — at most one block per cast, and only
                // while two demands differ.
                let colony =
                    reserverColony
                        [ northOutpost false; westOutpost false ]
                        (surplusFleet 2)
                        [ "W1N2", reservedRoom true 5000; "W2N2", reservedRoom true 2000 ]

                let fiveBlocks = List.replicate 5 BodyPart.Claim @ List.replicate 5 Move

                Expect.equal
                    (reserverCasts
                        (decide { colony with Bank = bank 8000 8000 } Map.empty Set.empty None)
                            .Intents)
                    [ fiveBlocks; fiveBlocks ]
                    "the room standing at its cap is cast the five blocks the slipped room asked for"
            }

            test "the reserver row is an addend of the target, amortized over a CLAIM life" {
                // The row's two effects on the Workforce target (ADR 0042),
                // both read off one boundary: it adds a place of its own —
                // a CLAIM body is a creep, and a fleet counting it as a
                // generalist would hire an upgrade mouth fewer — and its
                // replacement cost is deducted from the income base like
                // the Anchor and hauler rows'. Unlike theirs it is spread
                // over a **CLAIM body's own 600 ticks** rather than the
                // 1,500 the rest of the sum is written in: ADR 0042 prices
                // this row at 2.17 energy a tick, and over 1,500 it would
                // read as 0.87.
                //
                // The bank is 8,000 so the deficit's whole nine blocks are
                // affordable and the difference is a worker place wide.
                // W1N2 is seen and held by nobody: its rock is worth five,
                // and its reservation is on the floor, so the row asks for
                // its largest body against its smallest income.
                //
                // Income 10 + 10 + 5 = 25 a tick over 1,500 = 37,500.
                // Amortization: 3 Anchors × 700 = 2,100, one hauler ×
                // 2,400 — the two home containers' demands summed and
                // rounded once (ADR 0049) — and one 9-block reserver at
                // 5,850 spread over 600 and re-scaled onto 1,500 = 14,625
                // — 19,125 in all. The surplus 18,375 over a 16-Work
                // body's drain × 1,500 = 24,000 rounds up to one worker
                // (ADR 0037). Charged over 1,500 instead, the same row
                // would leave 27,150 and hire two.
                let fleetOf workers =
                    [ for i in 1..3 -> anchor $"a{i}" 0 50 ]
                    @ [ hauler "h1" 0 100 ]
                    @ [ reserver "r1" ]
                    @ [ for i in 1..workers -> worker $"w{i}" 0 50 ]

                let atFleet workers =
                    let colony =
                        reserverColony
                            [ northOutpost true ]
                            (fleetOf workers)
                            [ "W1N2", neutralRoom ]

                    decide { colony with Bank = bank 40000 8000 } Map.empty Set.empty None

                Expect.equal
                    (atFleet 1).Memo.HaulerQuota
                    1
                    "the premise: the two home containers come to one body at this bank"

                Expect.isEmpty
                    (spawnIntents (atFleet 1).Intents)
                    "3 Anchors + 1 hauler + 1 reserver + 1 worker is the whole target: six"

                match spawnIntents (atFleet 0).Intents with
                | [ (_, _, creepName) ] ->
                    Expect.stringStarts
                        creepName
                        "worker-"
                        "one body short, the generalist row fills the remainder"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "a declared outpost this colony owns hires no reserver at all" {
                // The tick a declared outpost is claimed, its controller
                // stops being reservable: the engine answers
                // ERR_INVALID_TARGET to `reserveController` on a room
                // anybody owns (#181). An owned room carries no reservation
                // for the same reason, so the deficit read off one is the
                // whole 5,000 and this row would cast a 650-energy body at
                // it every 600 ticks forever, each one walking over to fail
                // for its whole life.
                //
                // Pairwise on the one fact — same room, same rock, same
                // absent reservation, same fleet, same bank — because
                // ownership is the only input that moves. Read at a bank
                // that affords the whole nine-block deficit, so the
                // neutral half is a body the truncation could not have
                // produced by accident.
                let castsUnder control =
                    let colony =
                        reserverColony [ northOutpost false ] (surplusFleet 2) [ "W1N2", control ]

                    { colony with Bank = bank 8000 8000 }
                    |> fun colony -> decide colony Map.empty Set.empty None
                    |> fun result -> reserverCasts result.Intents

                Expect.equal
                    (castsUnder neutralRoom)
                    [ List.replicate 9 BodyPart.Claim @ List.replicate 9 Move ]
                    "a room nobody holds is the whole 5,000 of deficit: nine parts"

                Expect.isEmpty
                    (castsUnder ownedRoom)
                    "the same room, owned by this colony, is no longer a room to reserve"
            }
        ]
