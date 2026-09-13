/// The [[miner]] row (ADR 0057 decision 2): the store-less Work body that
/// stands over the mineral container and digs the season's Thorium — its sizing
/// rule, the quota that opens and closes it, the cut that tells it from the
/// [[anchor]], and its place in the casting cascade.
module Fabot.Core.Tests.Decide.QuotaMinerTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.QuotaFixtures

/// The quota of one row of a colony's cascade, and None for a row that is not
/// in it at all — the two answers a pairwise case has to be able to tell apart.
let private quotaOfRow name colony =
    rowOf name colony |> Option.map (fun row -> row.Quota)

/// The colony at a bank it can actually cast a miner out of: the row is priced
/// at capacity like every row but the supply floor, so a 300 bank refuses the
/// cast without touching the quota.
let private richMine capacity =
    { mineColony with
        Bank = bank capacity capacity
    }

/// The mine colony with a **scanned neighbour** beside it carrying its own
/// deposit, its own extractor and its own container — the room owned by
/// somebody else, which is the whole of what tells the two apart (#261).
/// Geometry identical to ours on purpose: nothing in the shape of the three
/// facts says whose they are.
let private rivalMine =
    { mineColony with
        RoomControl = Map.add "W1N2" rivalRoom mineColony.RoomControl
        Spatial =
            { mineColony.Spatial with
                TargetKinds =
                    mineColony.Spatial.TargetKinds
                    |> Map.add "min-r" Mineral
                    |> Map.add "ext-r" (Structure BuiltKind.Extractor)
                    |> Map.add "can-r" (Structure BuiltKind.Container)
                Thorium = Map.add "min-r" 22_000 mineColony.Spatial.Thorium
                Cooldowns = Map.add "ext-r" 0 mineColony.Spatial.Cooldowns
            }
            |> withNeighbour
                "W1N2"
                { RoomLayer.empty with
                    Terrain =
                        Map.ofList
                            [
                                for x in 8..12 ->
                                    { X = x; Y = 10 }, (if x = 10 then Wall else Plain)
                            ]
                    TargetPositions =
                        Map.ofList [ "min-r", minePos; "ext-r", minePos; "can-r", minePost ]
                }
    }

[<Tests>]
let minerBodyTests =
    testList
        "the miner row's body"
        [
            test "the miner row buys Work up to twenty, with one Move per five" {
                // ADR 0057 decision 2's own two banks, which are the two the
                // colony will actually stand at: RCL5's 1,800 and RCL6's 2,300.
                // The Move is bought for a single commute and not for ADR
                // 0003's fatigue parity — the body walks to one tile once and
                // then never leaves it — so the ratio is the row's own and the
                // remainder is spent on Work.
                Expect.equal
                    (bodyFor minerPattern 1800)
                    (List.replicate 16 Work @ List.replicate 4 Move)
                    "1,800 buys sixteen Work and the four Move that carry them, to the last energy"

                Expect.equal
                    (bodyCost (bodyFor minerPattern 1800))
                    1800
                    "and it is exactly the bank"

                Expect.equal
                    (bodyFor minerPattern 2300)
                    (List.replicate 20 Work @ List.replicate 4 Move)
                    "2,300 buys the twenty the ceiling allows, and the 100 left over stays banked"
            }

            test "the miner row stops at twenty Work however rich the bank" {
                // The ceiling is the bank's and never the engine's part cap —
                // twenty Work and four Move is twenty-four parts of fifty.
                // `WORK / 6` a tick against a reactor that eats one Thorium a
                // tick means mining was never the bottleneck, and past twenty
                // the deposit only ends sooner.
                Expect.equal
                    (bodyFor minerPattern 12900)
                    (List.replicate 20 Work @ List.replicate 4 Move)
                    "the richest bank casts the same body the 2,300 one does"
            }

            test "the miner row never casts below its block, and never carries" {
                Expect.equal
                    (bodyFor minerPattern 300)
                    [ Work; Work; Move ]
                    "two Work keep the body readable as a miner (Work > Move, and no Carry)"

                Expect.equal
                    (bodyFor minerPattern 550)
                    [ Work; Work; Work; Work; Work; Move ]
                    "a whole group of five Work and the one Move that carries them"

                for capacity in [ 300; 550; 1300; 1800; 2300; 12900 ] do
                    Expect.equal
                        (bodyFor minerPattern capacity |> List.filter ((=) Carry))
                        []
                        $"no bank buys the row a Carry part ({capacity})"
            }

            test "the Work each Move carries is the colony's own number" {
                // `Tuning.MinerWorkPerMove` pairwise, one field apart and at
                // the bank ADR 0057 derived it at: the ratio is a knob a human
                // turns in a commit, not a fact of the server, so the row's
                // sizing rule has to read the colony's own and not a constant.
                let bodyAt perMove =
                    sizedBodyFor
                        { largestSizing with
                            MinerWorkPerMove = perMove
                        }
                        minerPattern
                        1800

                Expect.equal
                    (bodyAt 5)
                    (List.replicate 16 Work @ List.replicate 4 Move)
                    "five Work a Move is sixteen Work at 1,800"

                Expect.equal
                    (bodyAt 3)
                    (List.replicate 15 Work @ List.replicate 5 Move)
                    "three Work a Move spends a hundred of the same bank on a fifth Move instead"
            }
        ]

[<Tests>]
let minerQuotaTests =
    testList
        "the miner row's quota"
        [
            test "a stocked deposit under a standing extractor hires one miner" {
                Expect.equal
                    (quotaOfRow "miner" mineColony)
                    (Some 1)
                    "one body per diggable deposit"
            }

            test "an extractor that is still a site hires nobody" {
                // Pairwise against the case above, one projection entry apart:
                // an extractor under construction extracts nothing —
                // `harvest.js` refuses a mineral with no extractor standing on
                // it — and a miner cast against a site stands idle for a life.
                Expect.equal
                    (quotaOfRow "miner" (mineColony |> withExtractorSite))
                    (Some 0)
                    "a site is 0"
            }

            test "a deposit the mod has deleted hires nobody" {
                // The stop condition ADR 0057 decision 7 declines to write: the
                // mod removes an exhausted Thorium deposit outright, so the
                // target leaves the projection and the row reads 0 off the same
                // fact that gave it 1.
                Expect.equal
                    (quotaOfRow "miner" (mineColony |> withDepositGone))
                    (Some 0)
                    "a deposit that is gone is 0"
            }

            test "a deposit standing at zero hires nobody either" {
                // Belt to the braces above: the amount is read as well as the
                // kind, so a projection that ever carried a deposit at zero
                // does not hire a body for it for 1,500 ticks.
                let empty =
                    { mineColony with
                        Spatial =
                            { mineColony.Spatial with
                                Thorium = Map.add "min-a" 0 mineColony.Spatial.Thorium
                            }
                    }

                Expect.equal (quotaOfRow "miner" empty) (Some 0) "an empty deposit is 0"
            }

            test "the miner row is an addend of the Workforce target" {
                // The guard row's own argument said again (ADR 0057 decision
                // 2): the row produces no energy at all, so no term of the
                // surplus answers for it, and a miner left out of the target
                // would have the deficit read the body it is alive as one of
                // the generalists the income already paid for — quietly
                // retiring a worker for the whole of its life.
                // Read under a Workforce floor of one, which is the premise:
                // `Tuning.MinWorkforce` is two, and a colony this small stands
                // on the floor either side of the pair, where a difference of
                // one addend is invisible.
                let targetOf colony =
                    (decideOn
                        { colony with
                            Tuning = { colony.Tuning with MinWorkforce = 1 }
                        })
                        .Quotas.Target

                Expect.equal
                    (targetOf mineColony - targetOf (mineColony |> withExtractorSite))
                    1
                    "the tick the extractor stands the target grows by exactly one"
            }
        ]

[<Tests>]
let minerCastTests =
    testList
        "the miner row's cast"
        [
            test "a miner fills the miner row's Living and no other row's" {
                // The row is read back off the parts like every other (ADR
                // 0006), and Work-heavy-with-no-Carry is the one cut no other
                // row of this colony makes. Without the arm a `[20 Work; 4
                // Move]` is Work-heavy and falls through to the **anchor** row,
                // filling a quota counted off the [[post]]s and retiring a
                // garrison from a rock for 1,500 ticks. Pairwise, one body
                // apart.
                let livingOf colony =
                    (decideOn colony).Quotas.Rows |> List.map (fun row -> row.Row, row.Living)

                let bare = livingOf mineColony

                let standing =
                    livingOf
                        { mineColony with
                            Creeps = [ miner "m1" ]
                            Spatial = mineColony.Spatial |> withCreepsAt [ "m1", minePost ]
                        }

                Expect.equal
                    (bare |> List.map fst)
                    (standing |> List.map fst)
                    "the premise: the same rows either side"

                Expect.equal
                    (List.zip bare standing
                     |> List.filter (fun ((_, before), (_, after)) -> before <> after)
                     |> List.map (fun ((row, before), (_, after)) -> row, before, after))
                    [ "miner", 0, 1 ]
                    "one body arrives and exactly one row's Living moves — the miner's"
            }

            test "the cascade casts the miner behind the hauler and ahead of the upgrader" {
                // The row is hired off a fact about the ground like the three
                // rows in front of it, and produces no energy at all, which is
                // what keeps it behind them; the season's whole score rides on
                // it, which is what puts it ahead of the rows that spend the
                // surplus.
                Expect.equal
                    ((decideOn (richMine 1800)).Quotas.Rows |> List.map (fun row -> row.Row))
                    [ "guard"; "reserver"; "anchor"; "hauler"; "miner"; "upgrader"; "worker" ]
                    "the reported order is the casting order"
            }

            test "the row casts the body its bank affords, named for the row" {
                // The supply floor stands in front of every row (ADR 0050), so
                // the colony is given a carrier that pays it off before the
                // miner's own seat is reached.
                let colony =
                    { richMine 1800 with
                        Creeps = [ hauler "h1" 0 200 ]
                        Spatial = mineColony.Spatial |> withCreepsAt [ "h1", { X = 14; Y = 10 } ]
                    }

                let casts = spawnIntents (decideOn colony).Intents

                Expect.contains
                    (casts |> List.map (fun (_, body, name: string) -> name.Split('-').[0], body))
                    ("miner", List.replicate 16 Work @ List.replicate 4 Move)
                    "the seat the miner row was owed is filled with the miner row's own body"
            }

            test "a colony with no deposit casts no miner and reports the row at zero" {
                Expect.equal
                    (quotaOfRow "miner" (richMine 1800 |> withDepositGone))
                    (Some 0)
                    "the row is reported and empty"

                Expect.isEmpty
                    (castRows (decideOn (richMine 1800 |> withDepositGone)).Intents
                     |> List.filter ((=) "miner"))
                    "and nothing is cast for it"
            }
        ]

[<Tests>]
let minerGroundTests =
    testList
        "the miner row's standing room"
        [
            test "a deposit whose container is not standing hires nobody" {
                // #261: the row's gate was the **extractor** and the [[work
                // area]]'s was the **container**, and between them the colony
                // bought a 2,200-energy body with nowhere to work. The Layout
                // emits both 5,000-point sites in one tick, so which finishes
                // first is the builder's accident, and any tick the mine
                // container is destroyed under a standing extractor re-opens
                // the window. Pairwise against the stocked case above, one
                // projection entry apart.
                Expect.equal
                    (quotaOfRow "miner" (mineColony |> withoutMineContainer))
                    (Some 0)
                    "no container, no mine Post, no body"

                Expect.equal
                    (Atlas.postsOf (Atlas.ofView (mineColony |> withoutMineContainer)) "min-a")
                    Set.empty
                    "the premise: the row and the ground read one census"
            }

            test "a neighbour's deposit is dug by nobody and pooled for nobody" {
                // #261: `FIND_MINERALS` and `FIND_STRUCTURES` carry every
                // owner's, so a scanned neighbour arrives with its own deposit,
                // its own extractor and its own container, and nothing in the
                // shape of the three says who built them. The engine does:
                // `harvest` refuses a mineral whose extractor is somebody
                // else's, which is one `ERR_NOT_OWNER` a tick for a whole life.
                // The room is the honest join — an extractor needs an **owned**
                // RCL6 room — so the deposit next door is worth exactly the
                // zero a room with no deposit in it is.
                Expect.equal (quotaOfRow "miner" rivalMine) (Some 1) "our deposit and not theirs"

                Expect.equal
                    (planTasks rivalMine noThreats
                     |> List.choose (function
                         | Harvest rock -> Some rock
                         | _ -> None))
                    [ "min-a" ]
                    "and only ours is a Task at all"
            }

            test "a light body is offered no tile of a deposit and no slot on it" {
                // #261: a `[16 Work; 4 Move]` miner loses Work parts head-first
                // to damage, and at `[3 Work; 4 Move]` it is no longer
                // Work-heavy — it still answers the Emitter's mineral arm, a
                // Work part and no Carry. ADR 0051's complement rule would then
                // hand it the deposit's Seats **less** the mine Post, steering
                // it deliberately off the container onto bare ground, where a
                // store-less dig drops the Thorium and the pile bleeds
                // `ceil(amount / 1000)` a tick with nothing in the colony to
                // pick it up. A deposit has no half the garrison is not
                // draining, so it has no light body's tile and no light body's
                // slot.
                let damaged = creepWith "m1" 0 0 [ Work; Work; Work; Move; Move; Move; Move ]

                let colony =
                    { mineColony with
                        Creeps = [ damaged ]
                        Spatial = mineColony.Spatial |> withCreepsAt [ "m1", { X = 9; Y = 10 } ]
                    }

                Expect.isFalse
                    (Atlas.workHeavy (Atlas.ofView colony) "m1")
                    "the premise: three Work under four Move is a light body"

                Expect.equal
                    (Atlas.workAreaFor (Atlas.ofView colony) "m1" (Harvest "min-a"))
                    Set.empty
                    "the deposit offers it no tile"

                Expect.equal
                    (poolOn colony
                     |> List.tryPick (fun entry ->
                         if entry.Task = Harvest "min-a" then
                             Some(Capacity.capOf CapScope.Commuters entry.Capacity)
                         else
                             None))
                    (Some(Some 0))
                    "and no Commuter slot to stand in"

                Expect.equal
                    (Map.tryFind "m1" (decideOn colony).Assignments)
                    None
                    "so it takes nothing"
            }
        ]
