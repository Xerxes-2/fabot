/// ADR-0042: the Reserver row and its lead.
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
                // A `[Claim; Move]` body has neither Work nor Carry, so read off the
                // generalist row it prices as one. At the 1,300 bank the reserver row
                // casts `[2Claim;2Move]`: four parts, 12 in the spawner, 3 of walking,
                // a lead of 15. The generalist row is twenty parts, a lead of 63.
                // Every life between is where the rows disagree.
                let casts life =
                    spawnIntents (decideOn (leadColony life)).Intents

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
                // The one quota the container switch does not gate: gating it
                // deadlocks the chain, because a container site needs vision, vision
                // needs a creep in the room, and this is the only creep with a reason
                // to go (#131). Pairwise on the one structure alone.
                let castsWith posted =
                    let fleet = surplusFleet (if posted then 3 else 2)

                    reserverColony [ northOutpost posted ] fleet [ "W1N2", reservedRoom true 5000 ]
                    |> fun colony -> decideOn colony
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
                // A claimer is a `[Claim; Move]` body like a reserver, so it is cast,
                // led and amortized where reservers are, and the room hires one body,
                // not two: its controller carries a Claim and no Reserve, so a reserver
                // hired for it would stand at a Task-less controller for its whole
                // 600-tick life. The block count tells the demands apart: a reservation
                // 4,000 ticks below its cap asks for seven blocks and takes the two the
                // 1,800 bank affords; a claim asks for one whatever the reservation has
                // done. Pairwise on the declaration alone. This room's own demand is
                // the whole list here, so demand and cast coincide; the test below is
                // where they part.
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
                    |> fun colony -> decideOn colony
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
                // The half a one-outpost fixture cannot show: the claim's entry is one
                // block, but every body cast this tick is sized at the largest demand
                // in the list. Which controller a finished CLAIM body holds is the
                // Matcher's, priced by travel cost alone, so a claimer cast at one
                // block could land on the slipped reservation and freeze that room for
                // 600 ticks. The over-buy is the safe direction, the same one
                // `reserverClaimsOf` takes for two slipping reservations. Pairwise on
                // the second outpost alone.
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
                    |> fun colony -> decideOn colony
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
                    (planTasksOn declared noThreats
                     |> List.filter (function
                         | Reserve _
                         | Claim _ -> true
                         | _ -> false))
                    [ Reserve "ctrl-W2N2"; Claim "ctrl-W1N2" ]
                    "the row's two demands are the unheld outpost's Reserve and the candidate's Claim"
            }

            test "two declared outposts hire two reservers, one apiece" {
                // Never one rover: `[4Claim;4Move]` is 2,600 energy and so an RCL7
                // body, and two outposts diagonal to each other share no exit, so a
                // rover would spend its 600-tick life crossing the home room.
                let colony =
                    reserverColony
                        [ northOutpost true; westOutpost true ]
                        (surplusFleet 4)
                        [ "W1N2", reservedRoom true 5000; "W2N2", reservedRoom true 5000 ]

                Expect.equal
                    (reserverCasts (decideOn colony).Intents)
                    [ oneBlock; oneBlock ]
                    "one body per declared outpost, and the four idle spawns cast no third"

                let half =
                    reserverColony
                        [ northOutpost true; westOutpost false ]
                        (surplusFleet 3)
                        [ "W1N2", reservedRoom true 5000; "W2N2", reservedRoom true 5000 ]

                Expect.equal
                    (reserverCasts (decideOn half).Intents)
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
                    |> fun colony -> decideOn colony
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

            // A guarded room's seat waits for its guard (#375): the reservation is
            // at its cap, so W1N2 wants one block, and while the guard row hired
            // for that room has a gap the seat is withheld. Standing or in the
            // oven, the guard gives it back.
            test
                "a guarded room's reserver seat is withheld until its guard stands or is in the oven" {
                let quotaOf colony =
                    rowOf "reserver" colony |> Option.map (fun row -> row.Quota)

                Expect.equal
                    (quotaOf (guardColony [] []))
                    (Some 1)
                    "the premise: the quiet room hires its one reserver"

                let raided = guardColony [ hostileIn "W1N2" raidTile smallMelee ] []

                Expect.equal (quotaOf raided) (Some 0) "raided and unguarded, the seat is withheld"

                Expect.equal
                    (quotaOf (
                        guardColony
                            [ hostileIn "W1N2" raidTile smallMelee ]
                            [ guard "g-1", outpostSeat ]
                    ))
                    (Some 1)
                    "a guard standing in the room gives the seat back"

                Expect.equal
                    (quotaOf
                        { raided with
                            Casting =
                                [
                                    {
                                        Name = "guard-1-spawn-1"
                                        Body = bodyFor guardPattern 800
                                    }
                                ]
                        })
                    (Some 1)
                    "and so does one in the oven: the seat waits on the row, not on the walk"
            }

            test "a living reserver fills the quota; one inside its lead does not" {
                // The quota counts bodies and not rooms (#130): which controller each
                // body holds is the Reserve Task's one-holder-per-controller capacity,
                // so a reserver still walking to its outpost already fills the row's
                // place. Inside its lead it leaves the count, and the replacement is
                // cast while it still holds the reservation.
                let colonyWith reservers =
                    reserverColony
                        [ northOutpost true ]
                        (surplusFleet 3 @ reservers)
                        [ "W1N2", reservedRoom true 5000 ]

                let placed life =
                    let colony = colonyWith [ reserver "r1" |> withLife life ]

                    { colony with
                        Spatial = colony.Spatial |> withCreepsAt [ "r1", { X = 22; Y = 10 } ]
                    }

                Expect.isEmpty
                    (reserverCasts (decideOn (placed 1500)).Intents)
                    "a reserver with a life ahead of it is the row's one body"

                Expect.equal
                    (reserverCasts (decideOn (placed 5)).Intents)
                    [ oneBlock ]
                    "inside its lead it is already outside the count, so the successor is cast"
            }

            test "the reserver row casts in front of the Anchor, hauler and worker rows" {
                // The other three rows spend income and this one decides whether the
                // income is five a tick or ten, and it is the cheapest body on the
                // table. The fleet is short in every row at once: two Anchors against
                // three Posts, no hauler against the one-body quota the two home
                // containers come to at this bank, and a whole-fleet deficit under all
                // of it. Four idle spawns cast one body each, so the whole order is
                // readable in one tick. The generalist in the fleet is the supply
                // floor's premise: two Anchors are two Carry parts and nothing that
                // can refill an extension, so without it the row cast first would be
                // the floor's carrier.
                let colony =
                    reserverColony
                        [ northOutpost true ]
                        [ anchor "a1" 0 50; anchor "a2" 0 50; worker "w1" 0 50 ]
                        [ "W1N2", reservedRoom true 5000 ]

                match spawnIntents (decideOn colony).Intents with
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
                // Quota and sizing in one expression: `ceil((5000 − ticks held) / 600)`
                // CLAIM parts, read off the reservation itself with no state between
                // ticks, so the row shrinks to its floor in steady state and comes back
                // bigger the tick a reservation has slipped.
                let castFor held =
                    reserverColony
                        [ northOutpost true ]
                        (surplusFleet 3)
                        [ "W1N2", reservedRoom true held ]
                    |> fun colony -> decideOn colony
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
                // The two halves of the sizing rule, each shown cutting the other off.
                // At RCL6 a 2,300 bank would buy a third CLAIM for a reservation that
                // caps at 5,000 anyway, which is why the pair is read at 2,300 and not
                // at today's 1,800, where the two rules agree.
                let castAt capacity held =
                    let colony =
                        reserverColony
                            [ northOutpost true ]
                            (surplusFleet 3)
                            [ "W1N2", reservedRoom true held ]

                    { colony with
                        Bank = bank 8000 capacity
                    }
                    |> fun colony -> decideOn colony
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
                // Since #333 that zero is not what the row *acts* on: the
                // room leaves the row's list altogether, because the engine
                // refuses `reserveController` on a controller somebody else
                // holds and the nine parts this used to buy were nine parts
                // of refusal. What the deficit rule still has to get right
                // is the case below it — no reservation at all — and the
                // case above, our own hold, which is where the `Ours` read
                // is now the only reading that can be taken at all.
                //
                // Read at a bank that affords the whole deficit and not at
                // the live 1,800, where two parts and nine both truncate to
                // two and the pair could not tell a full deficit from a
                // slipped one.
                let castWith control =
                    let colony = reserverColony [ northOutpost true ] (surplusFleet 3) control

                    { colony with Bank = bank 8000 8000 }
                    |> fun colony -> decideOn colony
                    |> fun result -> reserverCasts result.Intents

                let nineBlocks = List.replicate 9 BodyPart.Claim @ List.replicate 9 Move

                Expect.equal
                    (castWith [ "W1N2", reservedRoom true 4000 ])
                    [ twoBlocks ]
                    "1,000 ticks lost of ours is two parts"

                Expect.isEmpty
                    (castWith [ "W1N2", reservedRoom false 4000 ])
                    "the same 4,000 in a rival's name hires nobody at all: the act is refused (#333)"

                // Both holders, because the predicate under this is
                // `heldByOther` and a `= Rival` version of it would pass
                // every other assertion in this file while leaving W12S27's
                // own holder — the NPC Invader, whose core is long gone —
                // buying a body every 600 ticks.
                Expect.isEmpty
                    (castWith [ "W1N2", coreReservedRoom 4000 ])
                    "and the Invader's hold reads alike: it is the holder the ticket was filed on"

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
                    (reserverCasts (decideOn { colony with Bank = bank 8000 8000 }).Intents)
                    [ fiveBlocks; fiveBlocks ]
                    "the room standing at its cap is cast the five blocks the slipped room asked for"
            }

            test "the reserver row is an addend of the target, amortized over a CLAIM life" {
                // The row's two effects on the Workforce target, both read off one
                // boundary: it adds a place of its own, and its replacement cost is
                // deducted from the income base, spread over a CLAIM body's own 600
                // ticks rather than the 1,500 the rest of the sum is written in (2.17
                // a tick; over 1,500 it would read 0.87).
                //
                // The bank is 8,000 so the deficit's whole nine blocks are affordable
                // and the difference is a worker place wide. W1N2 is seen and held by
                // nobody: its rock is worth five and its reservation is on the floor.
                //
                // Income 10 + 10 + 5 = 25 a tick over 1,500 = 37,500. Amortization:
                // 3 Anchors × 700 = 2,100, one hauler × 2,400, and one 9-block reserver
                // at 5,850 spread over 600 and re-scaled onto 1,500 = 14,625: 19,125
                // in all. The surplus 18,375 over a 16-Work body's drain × 1,500 =
                // 24,000 rounds up to one worker. Charged over 1,500 instead, the same
                // row would leave 27,150 and hire two.
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

                    decideOn { colony with Bank = bank 40000 8000 }

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
                    |> fun colony -> decideOn colony
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

[<Tests>]
let reclaimerRowTests =
    testList
        "the re-claimer: the reserver row's third face"
        [
            test "a declared errand hires one CLAIM body of one block, and the bank gate is 650" {
                // The ticket's start condition: cast the tick the colony affords 650
                // and a chain exists, with no extractor, no road and no banked Thorium,
                // because the flag on W15S25 is a rival's already and every Thorium
                // delivered under it scores for him. One block: the reactor is taken by
                // one touch of one CLAIM part, and the engine checks no ownership and
                // runs no cooldown, so a second block buys a body that holds the flag
                // no faster. Pairwise on the declaration alone, then on the bank alone,
                // which is where the 650 is.
                let castsAt capacity errand =
                    let colony = reserverColony [] (surplusFleet 2) []

                    (if errand then
                         withReactorErrand colony |> withBurningReactor
                     else
                         colony)
                    |> fun colony ->
                        { colony with
                            Bank = bank capacity capacity
                        }
                    |> fun colony -> reserverCasts (decideOn colony).Intents

                Expect.isEmpty
                    (castsAt 650 false)
                    "the premise: with no outpost and no errand this colony hires no CLAIM body at all"

                Expect.equal
                    (castsAt 650 true)
                    [ oneBlock ]
                    "the errand is the whole hire, and one block is the whole of it"

                Expect.isEmpty
                    (castsAt 600 true)
                    "fifty short of a block, the row hires nobody and yields the tick (ADR 0050)"
            }

            test "the relief is cast while the incumbent still stands, and the overlap is the knob" {
                // A relay and never a garrison of two, overlapping rather than gapping
                // because the body out there is the colony's only vision of the room
                // and the only thing holding its flag. The threshold is the lead plus
                // the overlap, and both terms are real since #379: this fixture's chain
                // used to be unpriceable, which zeroed the lead's walk.
                let colonyAt overlap life =
                    let incumbent = reserver "rc" |> withLife life

                    let colony =
                        reserverColony [] (surplusFleet 2 @ [ incumbent ]) []
                        |> withReactorErrand
                        |> withBurningReactor

                    { colony with
                        Tuning =
                            { colony.Tuning with
                                ReclaimerOverlap = overlap
                            }
                    }
                    |> standingIn reactorErrand.RoomName [ incumbent, reactorRing ]

                let atLife overlap life =
                    reserverCasts (decideOn (colonyAt overlap life)).Intents

                // The threshold is the **lead plus the overlap**, and since
                // #379 the lead has a real walk in it: the shared fixture used
                // to price this crossing at `None`, which zeroed the walk term
                // and left this case measuring the knob against nothing. Read
                // off the Atlas rather than written down, so it stays pinned to
                // the walk the colony prices and not to a number that moves
                // with the floor under it.
                let lead =
                    let colony = colonyAt 25 25
                    let atlas = Atlas.ofView colony

                    let spawn =
                        match SpatialInfo.placementOf colony.Spatial "spawn-1" with
                        | Some tile -> RoomPos.pos tile
                        | None -> failtest "the fixture stands a spawn"

                    match
                        Atlas.castWalkTicks
                            atlas
                            oneBlock
                            spawn
                            (RoomPos.at reactorErrand.RoomName reactorRing)
                    with
                    | Some walk -> Engine.spawnTicksPerPart * List.length oneBlock + walk
                    | None ->
                        failtest
                            "the errand's crossing is priceable since #379, or this case shows nothing"

                Expect.isGreaterThan
                    lead
                    (Engine.spawnTicksPerPart * List.length oneBlock)
                    "the premise: the lead has a **walk** in it and not an oven alone, which is what #379 bought this case"

                Expect.isEmpty
                    (atLife 25 (lead + 26))
                    "one tick above the lead plus the overlap, the incumbent is the row's one body"

                Expect.equal
                    (atLife 25 (lead + 25))
                    [ oneBlock ]
                    "at it the relief is cast while the incumbent still holds the flag"

                Expect.isEmpty
                    (atLife 10 (lead + 25))
                    "and a shorter overlap leaves the same body counted: the knob is the term that moved"
            }

            test "a body at home is not led by the errand's overlap" {
                // The overlap is a property of the seat the body is handing
                // over, not of every CLAIM body produced by the shared row.
                let atRoom room tile =
                    let incumbent = reserver "rc" |> withLife 25

                    reserverColony [] (surplusFleet 2 @ [ incumbent ]) []
                    |> withReactorErrand
                    |> withBurningReactor
                    |> standingIn room [ incumbent, tile ]
                    |> fun colony -> reserverCasts (decideOn colony).Intents

                let home = SpatialInfo.homeName (reserverColony [] (surplusFleet 2) []).Spatial

                Expect.isEmpty
                    (atRoom home { X = 22; Y = 10 })
                    "at home, twenty-five ticks of life is outside this body's ordinary lead"

                Expect.equal
                    (atRoom reactorErrand.RoomName reactorRing)
                    [ oneBlock ]
                    "out on the errand, the same body is already outside the count"
            }

        ]

[<Tests>]
let reclaimerChargeTests =
    testList
        "the re-claimer is an addend of the target and a term of the surplus"
        [
            test "the seat is added to the target, and its replacement is deducted from the income" {
                // The re-claimer is hired off a fact about the ground and earns no
                // energy, so it is an addend of the workforce target like the four rows
                // beside it and a term of `surplusOverLifetime`, unlike the guard's "an
                // addend, charged nowhere else" (#304). Both halves are visible, each
                // at its own bank, because a worker unit is several CLAIM bodies wide:
                //
                // - at the RCL5 bank of 1,800 the charge falls inside a generalist's
                //   rounding and the addend is what moves: the target rises by one;
                // - at 1,300 it crosses one, and the charge is what moves: a generalist
                //   is retired to pay for the body, so the target does not rise.
                //
                // The charge is `reserverCost`'s own expression: the errand's entry
                // joins `reserverClaimsOf`'s list, so addend and charge are one number
                // (why `RowSizing` carries it), scaled onto a CLAIM body's 600-tick
                // life.
                //
                // The errand room also keeps a garrison of two rangers (#414, #419),
                // two more addends at both banks and charged nowhere else: hence 8
                // and 9 where the seat alone was 6 and 7.
                let atBank capacity =
                    let plain = reserverColony [] (surplusFleet 2) []

                    let plain =
                        { plain with
                            Bank = bank 40000 capacity
                        }

                    let quotas colony = (decideOn colony).Quotas

                    let workerRow colony =
                        (quotas colony).Rows
                        |> List.tryFind (fun row -> row.Row = "worker")
                        |> Option.map (fun row -> row.Quota)

                    (quotas plain).Target,
                    workerRow plain,
                    (quotas (plain |> withReactorErrand |> withBurningReactor)).Target,
                    workerRow (plain |> withReactorErrand |> withBurningReactor)

                Expect.equal
                    (atBank 1800)
                    (5, Some 2, 8, Some 2)
                    "at 1,800 the seat is a place of its own and the generalist row is unmoved"

                Expect.equal
                    (atBank 1300)
                    (7, Some 4, 9, Some 3)
                    "at 1,300 the same seat costs a generalist, so the target rises by the garrison alone"
            }
        ]
