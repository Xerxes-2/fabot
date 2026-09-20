/// ADR-0046: the upgrader row, its lead and its quota, and the colony's
/// own cast the inputs are read off.
module Fabot.Core.Tests.Decide.QuotaUpgraderTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.QuotaFixtures

[<Tests>]
let upgraderLeadTests =
    testList
        "the upgrader row's lead"
        [
            test "a standing body's lead is the upgrader row's, not the generalist's" {
                // `11W/1C/11M` has Work at Move and a Carry nine short of parity, so
                // read off the generalist row it prices as one. At the 1,800 bank the
                // upgrader row's lead is 72 (23 parts, 69 in the spawner, 3 of walking);
                // the generalist's is 84 (27 parts). Every life between is where the
                // rows disagree.
                let upgraderShape = bodyFor upgraderPattern 1800

                Expect.isEmpty
                    (leadCasts upgraderShape 80)
                    "at 80 ticks the standing body still counts; read off the generalist row it would not"

                Expect.isEmpty
                    (leadCasts upgraderShape 73)
                    "one tick outside its own row's lead it still counts"

                Expect.hasLength
                    (leadCasts upgraderShape 72)
                    1
                    "at its own row's lead the colony is one short and casts"

                // The generalist at the same tile, one rival at a time: the
                // same colony, the same life, a body of the row this one
                // used to be read as.
                Expect.hasLength
                    (leadCasts (workerBodyFor 1800) 80)
                    1
                    "the generalist at 80 is inside its own longer lead, which is what 80 was chosen to show"
            }

            test "the row that replaces a standing body is not the upgrader row" {
                // The lead is priced off the row and the replacement is hired off the
                // quota. With no controller container the upgrader quota is zero, so
                // the body the colony is short of comes from the generalist row.
                match leadCasts (bodyFor upgraderPattern 1800) 72 with
                | [ (_, body, creepName) ] ->
                    Expect.stringStarts
                        creepName
                        "worker-"
                        "with no buffer standing the upgrader row's quota is zero, so the deficit hires the generalist"

                    Expect.equal
                        body
                        (workerBodyFor 1800)
                        "and it is the generalist's own body, sized at the bank"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "the Anchor row wins the read over the upgrader row" {
                // `6W/1C/1M` answers to both descriptions, and the Anchor row is read
                // first. Eight parts is 24 in the spawner plus six ticks a step on one
                // Move: a lead of 42; read as an upgrader it would be 72.
                Expect.isEmpty
                    (leadCasts (bodyFor anchorPattern 1800) 50)
                    "at 50 the Anchor is outside its own row's lead; read as an upgrader it would not be"

                Expect.hasLength
                    (leadCasts (bodyFor anchorPattern 1800) 42)
                    1
                    "and at 42 it is inside it"
            }
        ]

[<Tests>]
let upgraderQuotaTests =
    testList
        "the upgrader row's quota"
        [
            let casts colony fleet =
                spawnIntents (decideOn { colony with Creeps = fleet }).Intents

            test "a built buffer hires the standing row out of the surplus" {
                // 30,000 in over a life from two posted sources, less 2 × 700 of Anchor
                // and 1,800 of hauler, is 26,800 of surplus; one standing body's eleven
                // Work drinks 16,500 of it, so the quota is one.
                match buffered 0 0 with
                | [ (_, body, name) ] ->
                    Expect.stringStarts name "upgrader-" "the surplus hires the standing row"

                    Expect.equal
                        body
                        (List.replicate 11 Work @ [ Carry ] @ List.replicate 11 Move)
                        "11W/1C/11M, the row's own cast at the RCL5 bank"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test
                "the quota is the whole bodies the surplus buys, and the remainder is the generalist row's" {
                // Only one of the two rows hired out of one surplus may round up, and
                // it is the smaller body's. 26,800 over 16,500 is one and a half: one
                // is hired, and the remainder less that body's 1,700 replacement is
                // 8,600, which rounds up against a 13,500 drain to one worker.
                Expect.isEmpty
                    (buffered 1 1)
                    "one standing body and one generalist are the whole of what this surplus hires"

                Expect.stringStarts
                    (castName (buffered 0 1))
                    "upgrader-"
                    "one short of the standing row, the gap is the buffer's"

                Expect.stringStarts
                    (castName (buffered 1 0))
                    "worker-"
                    "and one short of the generalist, the remainder hires it"

                Expect.isEmpty
                    (buffered 2 1)
                    "a second standing body is over the quota, and it holds the generalist row down rather than casting beside it"
            }

            // The stock's own half of the row (#385). The colony below is the
            // same 1,800-bank, two-source one every case above uses: income
            // buys one standing body and the floor is `UpgradeStockBodies` ×
            // 1,800 = 36,000.
            test "a bank above the floor buys a second standing body the income cannot" {
                // Live, this rule's absence was 849,766 energy standing in
                // W13S28's Storage that had not moved by one unit in 365 ticks,
                // while the colony put 14.1 e/t into its controller and a
                // neighbour on the same rocks and no stock at all put 30.6.
                let banked =
                    stocking 200_000 (upgraderColony (withBuffer (Structure BuiltKind.Container)))


                Expect.stringStarts
                    (castName (
                        spawnIntents
                            (decideOn
                                { banked with
                                    Creeps = upgraderFleet 1 1
                                })
                                .Intents
                    ))
                    "upgrader-"
                    "the income's one body is standing and the stock buys the second"
            }

            test "a bank under the floor buys nothing" {
                // 30,000 is short of the 36,000 the floor keeps back, and a
                // floor that is not whole is not a floor: the colony must be
                // able to re-cast itself before it spends a unit on upgrading.
                let thin =
                    stocking 30_000 (upgraderColony (withBuffer (Structure BuiltKind.Container)))

                Expect.isEmpty
                    (spawnIntents (decideOn { thin with Creeps = upgraderFleet 1 1 }).Intents)
                    "one standing body and one generalist are still the whole of what this colony hires"
            }

            test "the stock may double the row and no more" {
                // 200,000 over the floor is eleven bodies' worth of drink, and
                // the cap is the row the income itself buys — one. A second
                // mouth is about what one buffer refilled by one hauler's spare
                // loads can feed, and each tick re-decides as the stock falls.
                let rich =
                    stocking 1_000_000 (upgraderColony (withBuffer (Structure BuiltKind.Container)))

                Expect.isEmpty
                    (spawnIntents (decideOn { rich with Creeps = upgraderFleet 2 1 }).Intents)
                    "two standing bodies are the whole of what a stock of any size buys beside this income"
            }

            test "a bank buys nothing where the row itself is illegal" {
                // The stock's half must repeat the row's gate: a colony whose buffer is
                // still a site would otherwise cast a standing body against its bank,
                // which reads `NoneApplicable` for its whole life.
                let pending =
                    stocking 200_000 (upgraderColony (withBuffer (Site BuiltKind.Container)))

                Expect.stringStarts
                    (castName (casts pending (upgraderFleet 0 0)))
                    "worker-"
                    "a container site is no buffer however much is banked behind it"

                Expect.isEmpty
                    (casts pending (upgraderFleet 0 2))
                    "and the bank moves the Workforce target by nothing at all"
            }

            test "the building is charged before the mouth, as it is before the body" {
                // The same subtraction the worker row's backlog term makes: 200,000
                // buys a mouth on its own (200,000 − 36,000 of floor over an 18,200
                // body) and does not once a site is owed 150,000 of it.
                let building =
                    stocking 200_000 (upgraderColony (withBuffer (Structure BuiltKind.Container)))
                    |> owing [ 150_000 ]

                // The site itself puts a Build in the pool, so the generalist
                // row is what this colony hires next — the point is only that
                // the standing row is not.
                Expect.stringStarts
                    (castName (casts building (upgraderFleet 1 1)))
                    "worker-"
                    "the site is covered first and the stock's mouth is what goes"
            }

            test "the worker row is not charged for a body the stock bought" {
                // The trap this split exists for: `workforceTarget` hires the
                // generalist row out of the surplus the upgrade row leaves, and
                // a mouth the stock bought ate no surplus. Charged for it, the
                // worker row would be taken away twice — once at the Storage
                // the energy came from and once in the arithmetic here.
                let banked =
                    stocking 200_000 (upgraderColony (withBuffer (Structure BuiltKind.Container)))

                Expect.equal
                    (targetOf banked)
                    (targetOf (upgraderColony (withBuffer (Structure BuiltKind.Container))) + 1)
                    "the target grows by the stock's one body and the generalist row is left where it was"
            }

            test "a surplus of two and a half standing bodies hires two" {
                // A third posted source: 45,000 in less 2,100 of Anchor and the hauler
                // body is over two whole standing bodies, and the quota is the
                // quotient's whole part, never a cap of one.
                let richer =
                    thirdSource (upgraderColony (withBuffer (Structure BuiltKind.Container)))

                let richFleet = upgraderFleetAt 1800 3

                Expect.stringStarts
                    (castName (casts richer (richFleet 1 1)))
                    "upgrader-"
                    "one standing body in and the surplus still buys a whole second"

                Expect.isEmpty
                    (casts richer (richFleet 2 1))
                    "and the second is where it stops: the half body left over is not a third hire"
            }

            test "a surplus short of one whole standing body hires none of the row" {
                // One posted source: 15,000 in less 700 of Anchor and the hauler body
                // is about three quarters of the 16,500 one standing body drinks.
                // Rounded down the row is empty and the surplus is the generalist's.
                let lean = oneSource (upgraderColony (withBuffer (Structure BuiltKind.Container)))

                // The standing row is empty in both readings, so the fleet
                // is the one Anchor this room's one Post hires, its hauler
                // and the generalists.
                let leanFleet = upgraderFleetAt 1800 1 0

                Expect.stringStarts
                    (castName (buffered 0 0))
                    "upgrader-"
                    "the premise: at two posted sources this same buffer hires the standing row"

                Expect.stringStarts
                    (castName (casts lean (leanFleet 0)))
                    "worker-"
                    "at one it does not, and the surplus that cannot carry a standing body is the generalist's"

                Expect.isEmpty
                    (casts lean (leanFleet 1))
                    "and that one generalist is the whole of what the half-income colony hires"
            }

            test "a buffer still under construction hires none, and the worker row is unmoved" {
                // A site is no buffer: the quota is zero and the surplus goes to the
                // generalist row, ceil((26,800 − 0) / (9 × 1,500)) = 2 workers.
                let pending = upgraderColony (withBuffer (Site BuiltKind.Container))

                Expect.stringStarts
                    (castName (casts pending (upgraderFleet 0 0)))
                    "worker-"
                    "a container site is no buffer, and the row it does not hire is the standing one"

                Expect.isEmpty
                    (casts pending (upgraderFleet 0 2))
                    "and two generalists are the whole of what the surplus feeds"

                Expect.stringStarts
                    (castName (casts pending (upgraderFleet 0 1)))
                    "worker-"
                    "one short of those two, the gap is a worker's"

                // The third reading: no container at the controller at all answers the
                // site case's count. A pending container is not half a buffer.
                Expect.isEmpty
                    (casts (upgraderColony upgraderRoom) (upgraderFleet 0 2))
                    "with no container at the controller the same two generalists are the target"
            }

            test "the standing row is cast after the hauler row and before the generalist" {
                // The ground rows produce the surplus, so they are cast first; the
                // generalist spends it at nine Work against eleven, so it is cast last.
                // The fleets below differ from the fleet above in one body each.
                Expect.stringStarts
                    (castName (buffered 0 0))
                    "upgrader-"
                    "the premise: with every ground row manned the standing row is next"

                let noAnchor = upgraderFleet 0 0 |> List.filter (fun creep -> creep.Name <> "a2")

                Expect.stringStarts
                    (castName (
                        casts (upgraderColony (withBuffer (Structure BuiltKind.Container))) noAnchor
                    ))
                    "anchor-"
                    "an empty Post is filled before the buffer is manned"

                // One generalist stands in this fleet: with the hauler filtered out
                // two Anchors alone can refill no extension, so the cast would be the
                // supply floor's carrier, which at this bank is the hauler row's body
                // under another name.
                let noHauler =
                    upgraderFleetAt 1800 2 0 1 |> List.filter (fun creep -> creep.Name <> "h1")

                Expect.stringStarts
                    (castName (
                        casts (upgraderColony (withBuffer (Structure BuiltKind.Container))) noHauler
                    ))
                    "hauler-"
                    "and so is an unshipped round trip"

                Expect.stringStarts
                    (castName (buffered 1 0))
                    "worker-"
                    "with the quota's one standing, what is left of the target is the generalist's"
            }

            test "a declared outpost's reserver is cast before the standing row" {
                // The reserver decides whether an outpost's sources are worth five a
                // tick or ten, and the upgrader spends what they bring in.
                let declared =
                    withDeclaredOutpost (
                        upgraderColony (withBuffer (Structure BuiltKind.Container))
                    )

                Expect.stringStarts
                    (castName (casts declared (upgraderFleet 0 0)))
                    "reserver-"
                    "the row that doubles the income is cast before the row that spends it"

                let withReserver =
                    upgraderFleet 0 0
                    @ [ creepWith "r1" 0 50 [ BodyPart.Claim; BodyPart.Claim; Move; Move ] ]

                Expect.stringStarts
                    (castName (casts declared withReserver))
                    "upgrader-"
                    "and with it standing the next gap is the buffer's"
            }

            test "the worker row's floor is two while a Build stands in the pool and one otherwise" {
                // Without the floor a colony beside a rich buffer would run no body
                // that may build or repair. This fixture's income term is one, so what
                // is pinned here is the step from one to two a site buys; the lower
                // half is pinned at the poorer bank further down.
                Expect.stringStarts
                    (castName (buffered 1 0))
                    "worker-"
                    "the remainder and the floor agree at one, and the colony is one short of it"

                Expect.isEmpty (buffered 1 1) "and with a quiet pool it stops at one"

                let building =
                    withBuildSite (upgraderColony (withBuffer (Structure BuiltKind.Container)))

                Expect.stringStarts
                    (castName (casts building (upgraderFleet 1 1)))
                    "worker-"
                    "a site in the pool raises the floor to two, and the same fleet is one short"

                Expect.isEmpty
                    (casts building (upgraderFleet 1 2))
                    "and stops there: the floor is two, not a body per site"
            }

            test "no surplus hires no standing body, buffer or no buffer" {
                // The surplus is what the posted sources bring in less the ground
                // rows' amortization, so a colony with no posted source has none: the
                // container is a precondition of the quota, never its cause. With no
                // Post there is no amortization either, so the surplus is exactly zero;
                // the quota's `|> max 0` covers the case this fixture cannot reach.
                //
                // The generalist below is the colony floor of two, not the row floor:
                // with no Post there is no Anchor and no hauler, so the sum is one body
                // at most. The row floor is pinned at the RCL4 bank below.
                let unposted =
                    { upgraderColony (withBuffer (Structure BuiltKind.Container)) with
                        Sources = []
                        Spatial =
                            withBuffer (Structure BuiltKind.Container)
                            |> fun room ->
                                { room with
                                    TargetKinds =
                                        room.TargetKinds |> Map.remove "src-a" |> Map.remove "src-b"
                                }
                    }

                Expect.stringStarts
                    (castName (buffered 0 0))
                    "upgrader-"
                    "the premise: this buffer hires the row when there is income to hire it out of"

                // One living body and not none, so the cast is read
                // through the cascade rather than through the disaster
                // fallback, which answers the generalist row whatever
                // asked (`castFromBank`).
                Expect.stringStarts
                    (castName (casts unposted [ worker "w1" 0 50 ]))
                    "worker-"
                    "with the same buffer standing and nothing coming in, the gap is the floor's"

                Expect.isEmpty
                    (casts unposted [ worker "w1" 0 50; worker "w2" 0 50 ])
                    "and the colony floor is where it stops, with no surplus to hire a standing body"
            }

            // The same room and the same standing buffer at a poorer bank,
            // which is the one knob that moves what the row's own cast is
            // (`bodyFor upgraderPattern`) and, with it, the drink the
            // surplus is divided by. The levels are the banks' real ones:
            // 1,300 is RCL4's, 800 RCL3's ten extensions, 750 the same
            // room one extension short of them, 550 RCL2's five.
            let atBank capacity level =
                { upgraderColony (withBuffer (Structure BuiltKind.Container)) with
                    Bank = bank capacity capacity
                }
                |> withLevel level

            test
                "the standing row's own replacement can eat the whole remainder, and the floor hires the generalist then" {
                // One posted source at the RCL4 bank: 15,000 in less 700 of Anchor
                // and 1,200 of hauler is 13,100; the row's `8W/1C/8M` drinks 12,000,
                // so the quota is one and 1,100 is left, under the 1,250 that body
                // costs to replace, so the income term is zero. The target is four,
                // clear of the colony floor of two: what is read is the row floor.
                let quiet = oneSource (atBank 1300 4)

                Expect.stringStarts
                    (castName (casts quiet (upgraderFleetAt 1300 1 1 0)))
                    "worker-"
                    "the remainder feeds no generalist mouth, and the floor hires one all the same"

                Expect.isEmpty
                    (casts quiet (upgraderFleetAt 1300 1 1 1))
                    "and with a quiet pool the floor is one, so that generalist is where it stops"
            }

            test
                "the standing row's replacement is charged before the generalist row divides the remainder" {
                // The standing bodies' own replacement is charged before the commuting
                // row divides. One posted source at the RCL3 bank: 15,000 in less 700
                // of Anchor and 750 of hauler is 13,550; `5W/1C/5M` drinks 7,500, so
                // the quota is one and 6,050 is left. Charged the 800 replacement it is
                // 5,250, one generalist against a 6,000 drain; uncharged it would be two.
                let poorer = oneSource (atBank 800 3)

                Expect.stringStarts
                    (castName (casts poorer (upgraderFleetAt 800 1 1 0)))
                    "worker-"
                    "5,250 of remainder against a 6,000 drain is one generalist"

                Expect.isEmpty
                    (casts poorer (upgraderFleetAt 800 1 1 1))
                    "and it is one and not two: the 6,050 that would have bought a second is the standing body's replacement"
            }

            test "a bank whose own cast is no standing body hires none of the row" {
                // The row is counted by `patternOf` off the parts, so at a bank where
                // the sizing rule's own cast reads back to the generalist the quota
                // hires nobody. 800 against 750 at RCL3, because 800 is where one Carry
                // against `floor((capacity - 50) / 150)` Work reaches four Work.
                // Two haulers stand because at an 800 bank this room's two containers
                // ask for two bodies, whose gap would be cast ahead of the upgrader's.
                Expect.stringStarts
                    (castName (casts (atBank 800 3) (upgraderFleet 0 0 @ [ hauler "h2" 0 100 ])))
                    "upgrader-"
                    "at the 800 bank the row's own cast is a standing body, so the surplus hires it"

                Expect.stringStarts
                    (castName (casts (atBank 750 3) (upgraderFleet 0 0 @ [ hauler "h2" 0 100 ])))
                    "worker-"
                    "fifty energy poorer the same cast is `4W/1C/4M` and the row is not hired"

                // The RCL2 bank's hauler body carries 300 where the 800 bank's carries
                // 500, so the two containers ask for two bodies there and one here.
                Expect.stringStarts
                    (castName (casts (atBank 550 2) (upgraderFleet 0 0 @ [ hauler "h2" 0 100 ])))
                    "worker-"
                    "and at the RCL2 bank, where the cast is `3W/1C/3M`, the surplus is the generalist's"
            }

            test "the poor band's hire is bounded: a row it cannot count is a row it does not hire" {
                // The failure the gate above exists to prevent, pinned as
                // the trace that would have caught it: hiring at a bank
                // whose cast `patternOf` reads back to the generalist
                // leaves `upgraderGap` at the full quota every tick,
                // however many of that very body are alive — a fresh
                // `3W/1C/3M` cast for ever, ahead of the whole-fleet
                // deficit that is the only gate on the generalist row.
                //
                // So the count of casts must stop growing with the living
                // count. Both Posts are manned and both round trips
                // shipped, so what is left is the two surplus rows.
                let living n =
                    [ anchor "a1" 0 50; anchor "a2" 0 50; hauler "h1" 0 100; hauler "h2" 0 100 ]
                    @ [ for i in 1..n -> creepWith $"u{i}" 0 50 (bodyFor upgraderPattern 550) ]

                for n in [ 0; 1; 3; 10 ] do
                    Expect.isFalse
                        (casts (atBank 550 2) (living n)
                         |> List.exists (fun (_, _, name: string) -> name.StartsWith "upgrader-"))
                        $"no upgrader is cast at the RCL2 bank with {n} of that body alive"

                // And the colony settles: those bodies are generalists by
                // the ratio and the whole-fleet deficit counts them, so the
                // spawn goes quiet instead of casting into a gap that never
                // closes.
                Expect.isEmpty
                    (casts (atBank 550 2) (living 10))
                    "ten of them fill the target, and nothing is cast at all"
            }
        ]

[<Tests>]
let quotaInputTests =
    testList
        "the quota inputs read this colony's own cast"
        [
            test "the hauler row prices its haul to the buffer as well as to the spawn" {
                // This colony's sinks are the spawn cluster and the controller's
                // buffer, each priced at its own round trip. Pairwise on the
                // controller's position; the third number in each row is the same
                // colony with the buffer out of the census. Live (#216 R4), a buffer at
                // zero beside a container overflowing 2,000 with 1,859 on the ground
                // and seven mini workers walking fifty tiles was the shape.
                let withoutBuffer (colony: ColonyView) =
                    { colony with
                        Spatial =
                            { colony.Spatial with
                                TargetKinds = Map.remove "can-buf" colony.Spatial.TargetKinds
                            }
                    }

                Expect.equal
                    (quotaOf (sinkLaneColony 150 28))
                    1
                    "a controller beside the spawn: one hauler covers both sinks"

                Expect.equal
                    (quotaOf (withoutBuffer (sinkLaneColony 150 28)))
                    1
                    "and the spawn alone answers the same, because the two legs are one trip"

                Expect.equal
                    (quotaOf (sinkLaneColony 150 40))
                    2
                    "a controller across the room is a second haul, priced at its own trip"

                Expect.equal
                    (quotaOf (withoutBuffer (sinkLaneColony 150 40)))
                    1
                    "and the spawn alone still answers one, which is the body the buffer never got"
            }

            test "a nearer sink lowers nothing: the row is sized to the dearest sink it reaches" {
                // Under a mean this read two: a buffer one tile from the container
                // pulled the long haul to the cluster down. Live (W13S28, 2026-09-07)
                // the near sink filled in a trip, and the mean hired a body short while
                // both containers stood full. The row is priced at the dearest
                // reachable sink, so a nearer sink never lowers the count.
                let clusterAcrossTheRoom (colony: ColonyView) =
                    let away = { X = 45; Y = 25 }

                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    TargetPositions = Map.add "spawn-1" away layer.TargetPositions
                                    Obstacles =
                                        layer.Obstacles
                                        |> Set.remove { X = 25; Y = 25 }
                                        |> Set.add away
                                })
                    }

                let colony = clusterAcrossTheRoom (sinkLaneColony 150 24)

                let withoutBuffer =
                    { colony with
                        Spatial =
                            { colony.Spatial with
                                TargetKinds = Map.remove "can-buf" colony.Spatial.TargetKinds
                            }
                    }

                Expect.equal
                    (quotaOf withoutBuffer)
                    3
                    "priced to the spawn alone the long haul asks for three bodies"

                Expect.equal
                    (quotaOf colony)
                    3
                    "and with a buffer one tile off the container it still asks for three: the cluster across the room is the sink the flow runs to"
            }

            test "the capture the scope note was written from hires the hauler it was missing" {
                // W13S28 at RCL3 on an 800 bank, the room whose north container held
                // 2,000 with 1,859 decaying beside it while one hauler ran to the
                // spawn. `RoomFixtures.colonyAt` furnishes the captured room as its
                // Layout would, so the legs are real terrain. The numbers are the
                // room's answer: what must not move is that the buffer is worth a body.
                let colony = RoomFixtures.colonyAt (RoomFixtures.load "W13S28") 3 800

                let spawnOnly =
                    { colony with
                        Spatial =
                            { colony.Spatial with
                                TargetKinds = Map.remove "cont-ctrl" colony.Spatial.TargetKinds
                            }
                    }

                Expect.equal
                    (quotaOf spawnOnly)
                    1
                    "priced to the spawn alone the row hires one, which is what the live colony had"

                Expect.equal
                    (quotaOf colony)
                    2
                    "and priced to the buffer as well it hires the second, which is the whole of the fix"
            }

            test "the ferry is hired for a bootstrapping child and for no other stage" {
                // A mother hauls her stock into a child still being raised and stops
                // the tick it is `Independent`. Pairwise on the child's stage, at the
                // one load the rule was derived at.
                let lending stage =
                    ferryMother stage |> tunedBy (fun t -> { t with FerryLoads = 1 })

                Expect.equal (quotaOf (lending Bootstrapping)) 1 "one ferry body for the child"

                Expect.equal
                    (quotaOf (lending Independent))
                    0
                    "and none once the child feeds itself"

                Expect.equal
                    (quotaOf (lending Nursery))
                    0
                    "nor for a nursery, which has no buffer to fill and no mouth to drink it"

                // The shipped default is the derived one: the Refill that spends a
                // ferried load stands beside this term, so the body hired has a Task.
                Expect.equal
                    (quotaOf (ferryMother Bootstrapping))
                    1
                    "and the shipped default lends the one body the rule was derived at"

                // What a mother lends is written down, never derived from how much the
                // child could absorb.
                Expect.equal
                    (quotaOf (
                        ferryMother Bootstrapping |> tunedBy (fun t -> { t with FerryLoads = 2 })
                    ))
                    2
                    "and `Tuning.FerryLoads` is the whole of how much she lends"
            }

            test "the ferry's sink is the mother's to fill and never to draw" {
                // The lend is energy going one way: the buffer is a Refill target of
                // hers while the child is raised, and the Withdraw is denied her, or
                // her hauler would take the load it just carried straight back out.
                let poolFor stage =
                    planTasksOn (ferryMother stage) noThreats

                let lending = poolFor Bootstrapping

                Expect.isTrue
                    (List.contains (Refill("can-child", Energy)) lending)
                    "the child's buffer is a sink of hers while she is raising it"

                Expect.isFalse
                    (List.contains (Withdraw("can-child", Energy)) lending)
                    "and never an intake, however much stands in it"

                let grown = poolFor Independent

                Expect.isFalse
                    (List.contains (Refill("can-child", Energy)) grown)
                    "an independent child feeds itself, which is what the stage means"

                // The denial does not ride on the lend: `Borrowed.Rooms` also holds
                // the nursery she is raising and the child she has lost, so a Withdraw
                // denied only where a Refill is pooled would make those stores plain
                // intakes of hers, the inverse of the lend.
                Expect.isFalse
                    (List.contains (Withdraw("can-child", Energy)) (poolFor Nursery))
                    "a nursery's store is not hers to draw either"

                Expect.isFalse
                    (List.contains (Withdraw("can-child", Energy)) grown)
                    "nor a grown child's: no store of a child's is ever her intake"

                // The lend is bounded by capacity, never tier: `Tuning.FerryLoads` is
                // the same number the hauler row was raised by, so the quota and the
                // pool cannot disagree.
                let bound =
                    poolOn (ferryMother Bootstrapping)
                    |> List.tryPick (fun entry ->
                        if entry.Task = Refill("can-child", Energy) then
                            Some(Capacity.capOf CapScope.Everyone entry.Capacity)
                        else
                            None)

                Expect.equal
                    bound
                    (Some(Some Tuning.defaults.FerryLoads))
                    "the pool carries the lend's bound, and it is the tuned one"
            }

            test "a hungry ferry sink opens the mother's stock" {
                // The stock gate reads "some Refill target other than the Storage has
                // free capacity", and a bootstrapping child's buffer is one. Uncounted,
                // the ferry's Refill on the buffer's deep tier is reached only once
                // every home sink is full, which is exactly the state that leaves the
                // stock shut: the body hired for the lend had a sink and no intake.
                // This fixture is that state: no Refillables, no home buffer, a stocked
                // Storage.
                let stocked stage =
                    let mother = ferryMother stage

                    { mother with
                        Spatial =
                            { mother.Spatial with
                                Stores = Map.add "storage-1" 240000 mother.Spatial.Stores
                            }
                    }

                let lending = planTasksOn (stocked Bootstrapping) noThreats

                Expect.isTrue
                    (List.contains (Refill("can-child", Energy)) lending)
                    "the lend is the one hungry sink she has"

                Expect.isTrue
                    (List.contains (Withdraw("storage-1", Energy)) lending)
                    "so the stock it is priced from is drawable"

                // The control: with no lend there is no sink, and the stock is not
                // opened against its own Refill.
                Expect.isFalse
                    (List.contains
                        (Withdraw("storage-1", Energy))
                        (planTasksOn (stocked Independent) noThreats))
                    "and with nothing to feed, the stock stays shut"
            }

            test "a mother with no stock ferries nothing" {
                // The stock is the only energy a mother holds that her own rows are
                // not already hired against.
                let stockless =
                    let mother =
                        ferryMother Bootstrapping |> tunedBy (fun t -> { t with FerryLoads = 1 })

                    { mother with
                        Spatial =
                            { mother.Spatial with
                                TargetKinds = Map.remove "storage-1" mother.Spatial.TargetKinds
                            }
                    }

                Expect.equal (quotaOf stockless) 0 "no Storage, no ferry"
            }

            test "the upgrader row's divisor carries the body as well as its drink" {
                // The surplus is 71,500 by construction (`upgraderBandColony`): the
                // drain alone divides it into four bodies, the drain plus the body it
                // is spent on into three. Four would drink 66,000 and cost 6,800 to
                // replace, 1,300 over the income, out of the Storage every tick.
                Expect.equal
                    (castRows (decideOn (upgraderBandColony 2)).Intents)
                    [ "upgrader" ]
                    "two upgraders standing and the row is a body short"

                Expect.equal
                    (castRows (decideOn (upgraderBandColony 3)).Intents)
                    [ "worker" ]
                    "three, and the row is full: the surplus pays for three mouths and three bodies"

                Expect.isEmpty
                    (castRows (decideOn (upgraderBandColony 4)).Intents)
                    "and four is over every row's quota, so nothing is cast at all"
            }

            test "a lead is priced at the body its row will cast, not the largest it could" {
                // An Anchor is expiring when its life is at or under the ticks its
                // replacement needs to stand where it stands. Pairwise on the
                // reservation of the room its one Post stands in: held, the cast is
                // `6W/1C/1M`, eight parts, 24 in the spawner and a slow walk, and a
                // twenty-tick-old Anchor is inside that lead; unheld the rock gives
                // five, the cast is `3W/1C/1M`, and it is not. Priced at `bodyFor`'s
                // held ceiling both arms read "expiring" and the successor stood
                // beside the spawn reading its own Post as full (`IdleReason.NoneFree`).
                Expect.equal
                    (castRows (decideOn (outpostPostColony true 20)).Intents)
                    [ "anchor" ]
                    "a held rock: the row casts an eight-part body and twenty ticks is inside its lead"

                Expect.equal
                    (castRows (decideOn (outpostPostColony false 20)).Intents)
                    [ "worker" ]
                    "an unheld one: the row casts five parts, the lead is shorter, and the incumbent still counts"
            }

            test "a vacant Post is cast into with a body sized for its own rock" {
                // `anchorWorkCapOf` used to fold every posted source into one
                // colony-wide `List.max` with the home room in it at the held rate, so
                // a lapsed outpost was garrisoned by `6W/1C/1M` against a rock giving
                // five (#158). What pairs a body to a rock is the vacancy it fills: one
                // empty Post seats one rock. Pairwise on the outpost's reservation.
                Expect.equal
                    (anchorCastsBy (pairedPostColony false 1500 20))
                    [ threeWork ]
                    "the vacancy is on a rock nobody holds: three Work drain it as fast as it fills"

                Expect.equal
                    (anchorCastsBy (pairedPostColony true 1500 20))
                    [ sixWork ]
                    "reserved, the same vacancy is worth ten a tick and the row buys the six Work that dig it"
            }

            test "an ordinary home succession is not sized off an outpost's neutral rock" {
                // The vacancy is judged at arrival, not by who is standing where: the
                // home Post's expiring Anchor is still standing on it and will be dead
                // before a replacement arrives, so that Post is the vacancy and its own
                // held rock sizes the successor. Read off who is standing, the home
                // replacement would be cast at three Work against a rock giving ten.
                // Pairwise against the arm above on which garrison is expiring.
                Expect.equal
                    (anchorCastsBy (pairedPostColony false 40 1500))
                    [ sixWork ]
                    "the home room's own Post is the vacancy, and its rock gives ten whatever the outpost pays"

                // With the outpost's Post genuinely empty a standing read would find
                // that hole alone and cast the home replacement at three Work; above,
                // with both garrisoned, it found none and answered six for the wrong
                // reason.
                Expect.equal
                    (anchorCastsBy (pairedPostColony false 40 1500 |> withoutOutpostGarrison))
                    [ sixWork ]
                    "an expiring incumbent's own Post is a vacancy even with a neutral one standing open beside it"
            }

            test
                "a bank short of the dearest vacancy casts no Anchor rather than the cheapest one's body" {
                // A per-vacancy seat list would let the spawn take the first seat its
                // bank pays for: both Posts vacant, the held rock at 700 and the
                // neutral at 400, and 600 available buys the 400, a three-Work Anchor
                // born beside the home spawn and pinned to the held Post by travel
                // cost, digging six a tick where the rock gives ten. Sized at the
                // dearest vacancy the row yields the tick instead. Pairwise on the bank.
                let atBank available =
                    let colony = pairedPostColony false 20 20

                    { colony with
                        Bank = bank available 1800
                    }
                    |> anchorCastsBy

                Expect.equal
                    (atBank 600)
                    []
                    "600 buys neither the held Post's body nor a cheaper one for a rock this cast cannot be steered to"

                Expect.equal
                    (atBank 1800)
                    [ sixWork ]
                    "and the same two vacancies at a bank that can pay buy the dearer of them"
            }

            test "a lead on a Post is priced at the body that Post will be cast" {
                // A lead is what the successor needs to stand where this creep stands,
                // so it is the Post's body and not the row's largest. Same 40 ticks on
                // the same garrison a Seam away: held, `6W/1C/1M` has a lead of 66 and
                // 40 is expiring; unheld, `3W/1C/1M` has a lead of 36 and 40 still
                // counts. Priced at the colony's richest ceiling both arms would read
                // 66 and cast 30 ticks early.
                Expect.equal
                    (castRows (decideOn (pairedPostColony true 1500 40)).Intents)
                    [ "anchor" ]
                    "a held rock: the successor is eight parts and forty ticks is inside its lead"

                Expect.equal
                    (castRows (decideOn (pairedPostColony false 1500 40)).Intents)
                    [ "worker" ]
                    "an unheld one: the successor is five parts, the lead is shorter, and the incumbent still counts"
            }

            test "a rival's room hires no reserver on the tick it is first seen held" {
                // The stand-down gate reads the previous tick's raid log, so on the
                // tick a room is first seen held it is still in the scan set and reads
                // the whole 5,000-tick deficit: the row cast 1,300 of reserver at a
                // controller the engine would refuse (#184). Closed with the same-tick
                // fact. Pairwise on the room's owner alone.
                let castsIn control =
                    reserverColony [ northOutpost false ] (surplusFleet 2) [ "W1N2", control ]
                    |> fun colony -> decideOn colony
                    |> fun result -> reserverCasts result.Intents

                Expect.equal
                    (castsIn neutralRoom)
                    [ twoBlocks ]
                    "a room nobody holds is the outpost this row exists for"

                Expect.isEmpty
                    (castsIn rivalRoom)
                    "and a room somebody else owns hires nobody: the engine refuses a reservation there"
            }

            test "a body in the oven fills its row's gap: two idle spawns cast one Anchor" {
                // A creep still spawning is in no `Creeps` list, so every row's living
                // count read past it. With two spawns, spawn one casts an Anchor at T
                // and spawn two casts a second for the same Post at T+1, because the
                // spawn's own `IsSpawning` only stops one spawn double-casting (#156).
                // Pairwise on the oven alone.
                let colony casting =
                    { quotaColony 15 2 300 with
                        Creeps = [ worker "w1" 0 50 ]
                        Casting = casting
                    }

                Expect.equal
                    (castRows (decideOn (colony [])).Intents |> List.head)
                    "anchor"
                    "an empty Post and nothing bought for it: the Anchor row is a body short"

                Expect.isFalse
                    (List.contains
                        "anchor"
                        (castRows
                            (decide
                                (colony
                                    [
                                        {
                                            Name = "anchor-14-Spawn1"
                                            Body = [ Work; Work; Carry; Move ]
                                        }
                                    ])
                                Map.empty
                                Set.empty
                                None)
                                .Intents))
                    "and with one already in an oven the row is full: the second spawn buys something else"
            }

            test "the oven's body is read back to the row that bought it" {
                // The counting rule that makes the case above safe: a
                // gestating body fills the gap of the row `patternOfCast`
                // reads it into, and of no other. A hauler in the oven
                // leaves the Anchor row exactly as short as it was — which
                // is what keeps this from being a blanket "one body in
                // flight suppresses one cast".
                let colony casting =
                    { quotaColony 15 2 300 with
                        Creeps = [ worker "w1" 0 50 ]
                        Casting = casting
                    }

                Expect.equal
                    (castRows
                        (decideOn (
                            colony
                                [
                                    {
                                        Name = "hauler-14-Spawn1"
                                        Body = [ Carry; Carry; Move ]
                                    }
                                ]
                        ))
                            .Intents
                     |> List.head)
                    "anchor"
                    "a hauler in the oven pays off no Anchor gap"
            }
        ]
