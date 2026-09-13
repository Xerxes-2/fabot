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

/// The mine with an **income** beside it: the corridor run out to x = 4 with a
/// source walled in at either end and a built container on each one's only Seat
/// — two [[post]]s, so both rocks are income the surplus counts (ADR 0042) — at
/// a 550 bank. `mineColony` carries no source at all on purpose, so this is the
/// one colony in the suite whose surplus has a number for the miner's charge to
/// come out of (#304).
///
/// **Neither source container may stand on a [[seat]] of the deposit**, and
/// that is why the west source is at x = 4 rather than at the corridor's old
/// mouth: `Atlas.postsOf` reads a mine [[post]] as (any mineral's Seats) ∩ (any
/// container tile in the room), so an energy container on (9,10) — which is
/// `min-a`'s other Seat — is a second mine Post holding no Thorium, and the
/// fixture carrying this charge's whole justification would be a colony whose
/// deposit has a standing place that ages nobody. The gate itself is #261's and
/// is filed as #312; what is fixed here is the fixture.
let private earningMine =
    { mineColony with
        Bank = bank 550 550
        Sources = [ source "src-a"; source "src-b" ]
        Spatial =
            mineColony.Spatial
            |> withHome (fun layer ->
                { layer with
                    Terrain =
                        layer.Terrain
                        |> Map.add { X = 4; Y = 10 } Wall
                        |> Map.add { X = 5; Y = 10 } Plain
                        |> Map.add { X = 6; Y = 10 } Plain
                        |> Map.add { X = 7; Y = 10 } Plain
                        |> Map.add { X = 20; Y = 10 } Wall
                })
            |> withTargets
                [
                    "src-a", { X = 4; Y = 10 }, Source
                    "can-src", { X = 5; Y = 10 }, Structure BuiltKind.Container
                    "src-b", { X = 20; Y = 10 }, Source
                    "can-srb", { X = 19; Y = 10 }, Structure BuiltKind.Container
                ]
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
                // ADR 0057 decision 2: a miner left out of the target would
                // have the deficit read the body it is alive as one of the
                // generalists the income already paid for — quietly retiring a
                // worker for the whole of its life.
                // Read under a Workforce floor of one, which is the premise:
                // `Tuning.MinWorkforce` is two, and a colony this small stands
                // on the floor either side of the pair, where a difference of
                // one addend is invisible.
                // `mineColony` has no source at all, so its income is zero and
                // the surplus the miner is now charged against (#304, below)
                // is at or under zero either side of the pair: what the addend
                // costs the *generalist* row is the case beneath this one, and
                // it is a fact about a colony with an income rather than about
                // the addend.
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

            test "the miner row's replacement is deducted from the surplus, three bodies of it" {
                // #304: the row is an addend of the target **and** a term of
                // the amortization, which is the reserver, anchor and hauler
                // rows' own shape — every row hired off a fact about the
                // ground has its replacement settled before the surplus has a
                // number, and the two rows hired out of the surplus itself are
                // charged inside `workforceTarget`. A miner charged nowhere is
                // an upgrade mouth's worth of income the colony sells twice for
                // as long as a deposit lasts.
                //
                // **Three bodies over one worker's life**, scaled exactly as
                // the reserver's 600-tick body is scaled onto the 1,500 this
                // sum is written in: the miner stands *on* the mineral
                // container, `thorium.js` takes `floor(log10 store.T)` off the
                // `ageTime` of everything on that tile every tick, and a
                // container the haul keeps inside the 100..999 band therefore
                // burns `Tuning.MineContactAgeing` (3) ticks of the body's life
                // a tick (ADR 0057's Consequences, where the programme's
                // ~220,000 energy is priced off that same 500-tick life and its
                // twenty-six bodies).
                //
                // The number is read from **both sides**, because one worker
                // unit is about six miner bodies wide and a single income
                // therefore pins a band and not a number: at the 550 bank a
                // charge of two bodies still hires the eighth generalist, and
                // at the 500 bank a charge of four retires one the third does
                // not. Neither bank can do both — a worker unit is wider than
                // four miner bodies, so only one rounding boundary ever falls
                // inside the range — which is why this case is two colonies.
                let ageing ticks (colony: ColonyView) =
                    { colony with
                        Tuning =
                            { colony.Tuning with
                                MineContactAgeing = ticks
                            }
                    }

                // The fixture's own guard, because the charge is priced per
                // mine [[post]] and `Atlas.postsOf` intersects the deposit's
                // Seats with **every** container tile in the room: take the
                // mineral container away and the row must go to nothing. It
                // read 1 while an energy container stood on the deposit's other
                // Seat, which is the gate filed as #312.
                Expect.equal
                    (quotaOfRow "miner" (earningMine |> withoutMineContainer))
                    (Some 0)
                    "no source container of this colony stands on a Seat of the deposit"

                // The 550 bank, from below. Two posted rocks leave 22,550 of
                // surplus and a generalist's two Work drink 3,000 over a life,
                // which is eight of them; three miner bodies at 550 are 1,650,
                // leaving 20,900 — seven. Two would have left 21,450 and hired
                // the eighth.
                Expect.equal
                    (quotaOfRow "worker" (earningMine |> withExtractorSite))
                    (Some 8)
                    "the premise: with no miner to pay for, the whole income is the generalist row's"

                Expect.equal
                    (quotaOfRow "worker" earningMine)
                    (Some 7)
                    "the tick the extractor stands one generalist is retired to pay for the miner"

                Expect.equal
                    (quotaOfRow "worker" (earningMine |> ageing 2))
                    (Some 8)
                    "and two bodies of charge would not have retired it — the row is charged more than twice"

                // The 500 bank, from above: the same colony one cast smaller,
                // where the rounding boundary sits between the third body and
                // the fourth. This is also `MineContactAgeing`'s own pairwise
                // — a Tuning field arrives with one (ADR 0057's Consequences)
                // — and it is the reading that says the charge is *three* and
                // not merely "at least three".
                let leanMine = { earningMine with Bank = bank 500 500 }

                Expect.equal
                    (quotaOfRow "worker" leanMine)
                    (Some 8)
                    "three bodies of charge leave the eighth generalist standing at this bank"

                Expect.equal
                    (quotaOfRow "worker" (leanMine |> ageing 4))
                    (Some 7)
                    "a fourth body of charge would retire it, which is what makes the three a number"
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

[<Tests>]
let mineHaulTests =
    testList
        "the mine's haul"
        [
            test "the mineral container is one more term in the hauler row's demand sum" {
                // ADR 0057 decision 3: the leg is worked by the existing
                // [[hauler unit]] row and no new one, so what grows is a row's
                // quota and not the cascade. The term is the mineral
                // [[container]]'s round trip to the [[storage]] times the
                // [[miner]]'s own rate — `Work / 6` Thorium a tick, the
                // extractor's cooldown being five and the intent pass running
                // before the object pass — which is the `output × trip` shape
                // every source container's line already has, in the other
                // resource.
                //
                // Pairwise on the container alone: the same colony with the mine
                // container not yet standing has no mine and asks for nothing.
                let demandOf colony = (decideOn colony).Quotas.HaulerDemand

                let withMine = demandOf mineHaulColony
                let without = demandOf (mineHaulColony |> withoutMineContainer)

                Expect.isEmpty
                    without
                    "the premise: no source containers here, so the mine is the whole sum"

                Expect.equal (List.length withMine) 1 "and with it standing there is one line"

                let row = List.head withMine

                Expect.equal
                    row.Container
                    (RoomPos.at (SpatialInfo.homeName mineHaulColony.Spatial) minePost)
                    "the line is the mineral container's own tile"

                Expect.equal
                    row.Output
                    (partCountIn (bodyFor minerPattern mineHaulColony.Bank.Capacity) Work)
                    "priced at the row's cast at this bank, one Thorium a Work part a dig"

                Expect.equal
                    (row.Sinks |> List.map (fun sink -> sink.Kind))
                    [ "storage" ]
                    "the Storage and never the three energy sinks: it is the store nothing stands on"

                Expect.isTrue
                    (row.Demand > 0 && row.Sinks |> List.forall (fun sink -> sink.Trip.IsSome))
                    "a priced trip is a demand, and the trip is the one the Atlas answers"
            }

            test "a mine the colony cannot bank to asks for no haul at all" {
                // ADR 0004 at this term: the Storage is the one sink, so a
                // colony with none standing prices nothing here — which is the
                // same tick the Thorium pair is not pooled either, the Refill
                // needing a Storage with room. Pairwise against the case above,
                // one structure apart.
                Expect.isEmpty
                    (decideOn mineColony).Quotas.HaulerDemand
                    "no Storage, no leg, no term"
            }

            test "a mine that cannot be dug asks for no haul either" {
                // #262: the term prices the [[miner]]'s output, so it must read
                // the same facts the miner row's own quota puts a body at 0 for
                // (ADR 0057 decision 2). The container alone bought a carrier
                // for a mine producing nothing — and both halves are ordinary
                // rather than hypothetical: the Layout emits the extractor and
                // the container as two 5,000-point sites in one tick and which
                // finishes first is the builder's accident, which is the RCL6
                // build window this leg lands in.
                //
                // Pairwise on one fact at a time against the case above.
                let demandOf colony = (decideOn colony).Quotas.HaulerDemand

                // The deposit read out but still in the projection — the tick
                // before the mod's `postProcessObject` deletes it.
                let runOut =
                    { mineHaulColony with
                        Spatial =
                            { mineHaulColony.Spatial with
                                Thorium = Map.add "min-a" 0 mineHaulColony.Spatial.Thorium
                            }
                    }

                Expect.equal
                    (List.length (demandOf mineHaulColony))
                    1
                    "the premise: a standing extractor over a full deposit is one line"

                Expect.isEmpty
                    (demandOf (mineHaulColony |> withExtractorSite))
                    "an extractor under construction extracts nothing, so nothing is carried"

                Expect.isEmpty
                    (demandOf runOut)
                    "and a deposit with nothing left in it is a mine that is over"
            }
        ]
