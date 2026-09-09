/// The Guard row (ADR 0056) and the supply floor in front of every row
/// (ADR 0050).
module Fabot.Core.Tests.Decide.QuotaGuardTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.QuotaFixtures

[<Tests>]
let guardRowTests =
    testList
        "the guard row"
        [
            test "a clear outpost hires none, and one armed hostile in it hires one" {
                // ADR 0056's first two banks of the count rule, pairwise on
                // one fixture: the row is **0** for the whole of a colony's
                // ordinary life, and 1 the tick a [[threat]] is seen standing
                // in a declared outpost. Nothing is pre-cast and nothing is
                // remembered — the quota is a per-tick fact read off vision.
                Expect.equal
                    (guardQuotaOf (guardColony [] []))
                    (Some 0)
                    "the premise: a quiet outpost is no reason to buy a body"

                Expect.equal
                    (guardQuotaOf (guardColony [ hostileIn "W1N2" raidTile smallMelee ] []))
                    (Some 1)
                    "and the lone smallMelee nine raids in ten arrive as hires exactly one"
            }

            test "a hostile that reaches nothing is no reason to hire" {
                // The gate is ADR 0033's [[threat]] and never "a hostile": a
                // `smallHealer` carries neither ATTACK nor RANGED_ATTACK, so
                // it takes no ground, kills nothing and buys no body — even
                // though its HEAL parts are exactly what the count rule
                // prices once something armed *is* standing beside it.
                Expect.equal
                    (guardQuotaOf (guardColony [ hostileIn "W1N2" raidTile smallHealer ] []))
                    (Some 0)
                    "a lone healer is a hostile the raid log records and no threat at all"
            }

            test "a raid at home hires no guard" {
                // This row is the [[outpost]]'s and nothing else (ADR 0056):
                // a raid in the home room is the [[keep]]'s business (ADR
                // 0034), and the home room is not a declared outpost. Read at
                // (8,9), far enough from the spawn at (20,10) that the spawn
                // hold is not what is answering — a held tick derives no
                // quotas at all and this case would pass on the wrong reason.
                let athome =
                    { guardColony [] [] with
                        Hostiles = [ hostileIn "W1N1" { X = 8; Y = 9 } smallMelee ]
                    }

                Expect.equal
                    (guardQuotaOf athome)
                    (Some 0)
                    "the same raid that hires one in the outpost hires none at home"

                Expect.isSome
                    (rowOf "guard" athome)
                    "and the cascade ran: the row is written down, it is simply zero"
            }

            test "a melee guard cannot count self-heal toward surviving a healer-backed raid" {
                let raid healers = guardColony (raidOf healers) []

                Expect.equal
                    (guardQuotaOf (raid 0))
                    (Some 1)
                    "90 damage kills the lone melee before its 40 damage kills one block"

                Expect.equal
                    (guardQuotaOf (raid 1))
                    (Some 2)
                    "30 net damage needs 34 ticks, but without self-heal the block dies in 25"

                Expect.equal
                    (guardQuotaOf (raid 2))
                    (Some 2)
                    "120 healing exceeds one block's damage and still asks for two"
            }

            test "two attackers and no healer at all buy the second guard" {
                // #280, on the raid that found it: W13S29 took a `smallMelee`
                // and a three-RANGED invader together — seventy a tick between
                // them and nothing healing either. The rule this amends read
                // the raid's *healing* against one block's ninety, saw none,
                // and asked for one, which loses: a block dies in fifteen
                // ticks under seventy without self-heal, and
                // needs twenty-three to chew two thousand hits.
                //
                // What the count compares now is the two clocks. Pairwise on
                // the second attacker alone, which is the only thing that
                // moves between the readings.
                let ranged =
                    { hostileIn "W1N2" raidTile smallRanged with
                        Id = "ranged-1"
                    }

                Expect.equal
                    (guardQuotaOf (guardColony (raidOf 0 @ [ ranged ]) []))
                    (Some 2)
                    "two armed bodies out-live one block, so the room buys the second"

                Expect.equal
                    (guardQuotaOf (guardColony (raidOf 0) []))
                    (Some 1)
                    "and the lone smallMelee it is drawn from still buys one"
            }

            test "the count reads the raid and never our own answer to it" {
                // #272, and the amendment's whole point. Priced against the
                // guards *standing* in the room, the number was not monotone:
                // 2 with one guard up and 1 the tick the second arrived, so
                // the reinforcement the escalation had just bought was
                // `CapacityFull`-evicted on arrival — onto a Flee whose safe
                // set is that same room, so it never left, never swung, and
                // held the count at 1 for as long as it lived. 750 energy for
                // a body that does nothing, on exactly the two-healer raid the
                // ADR buys it for. So the damage term is one block of the
                // row's own body and nothing that stands, and the same raid
                // answers the same number with none, one and two guards of
                // ours in the room. Read at the colony's own live 1,800 bank,
                // where a survivor cast at a poorer bank is exactly the body
                // that must not veto its own reinforcement.
                let standing healers ours = guardColony (raidOf healers) ours

                let standing2 = standing 2

                Expect.equal
                    (guardQuotaOf (standing2 []))
                    (Some 2)
                    "the tick the raid is seen, before anything of ours has arrived"

                Expect.equal
                    (guardQuotaOf (standing2 [ guard "g-1", outpostSeat ]))
                    (Some 2)
                    "the tick the first guard stands, which used to be the only tick this read 2"

                Expect.equal
                    (guardQuotaOf (standing2 [ guard "g-1", outpostSeat; guard "g-2", secondSeat ]))
                    (Some 2)
                    "and the tick the second stands beside it, which used to retract to 1"

                Expect.equal
                    (guardQuotaOf (standing 0 [ guard "g-1", outpostSeat ]))
                    (guardQuotaOf (standing 0 []))
                    "and the below-threshold reading is invariant the same way: a lone melee is one guard, before and after ours arrives"

                Expect.equal
                    (guardQuotaOf (standing 0 [ guard "g-1", outpostSeat ]))
                    (Some 1)
                    "a lone melee still needs only one guard"

                Expect.equal
                    (guardCasts (decide (standing2 []) Map.empty Set.empty None).Intents
                     |> List.length)
                    2
                    "so the escalation is bought on the tick the raid is seen, an oven earlier than a rule reading our own bodies could"
            }

            test "the count is capped at two per outpost" {
                // The bound decision 4 rests on: two guards die, 1,500
                // energy is spent, and the outpost falls back to ADR 0043's
                // [[stand-down]] rather than feeding an unbounded stream of
                // bodies into a raid we are losing. Four healers is 240
                // against one block's 90 and still asks for two.
                let raid healers = guardColony (raidOf healers) []

                Expect.equal
                    (guardQuotaOf (raid 2))
                    (guardQuotaOf (raid 4))
                    "twice the healing asks for the same two bodies"

                Expect.equal (guardQuotaOf (raid 4)) (Some 2) "and two is the cap"
            }

            test
                "the damage the raid is priced against is one block, so the bank does not move the count" {
                // The other half of "the count reads the raid" (#272): the
                // damage term is **one `guardPattern` block**, a constant of
                // the row, and not the whole body this bank would cast. The
                // whole body grows with the bank while the row's `Living`
                // counts the body that is *standing*, so a guard cast at a
                // poorer bank would veto its own reinforcement — 120 of
                // healing against a two-block 180 reads 1 while a 90-damage
                // survivor holds the row's `Living` at 1 and nothing is cast.
                // It would also take ADR 0056 decision 1's own two-healer case
                // (120 ≥ 90 → 2) out of reach at every bank above 1,300, this
                // colony's live 1,800 included. So the same raid answers the
                // same number across the decision's whole bank table.
                let raid capacity =
                    guardColony (raidOf 2) [ guard "g-1", outpostSeat ] |> banked capacity

                for capacity in [ 800; 1300; 1800; 2300 ] do
                    Expect.equal
                        (guardQuotaOf (raid capacity))
                        (Some 2)
                        $"120 healed against one block's 90 hires the second at a {capacity} bank too"
            }

            test "the count is summed over the declared outposts" {
                // Two rooms, so "one guard per raided outpost" can be told
                // apart from "one guard". The second outpost is unposted, and
                // that is deliberate: the row is hired off a *declaration*
                // and a [[threat]], never off a standing container — an
                // outpost whose crew is being killed before it can build one
                // is the case that most needs the body.
                let twoOutposts hostiles =
                    let colony =
                        reserverColony
                            [ northOutpost true; westOutpost false ]
                            (surplusFleet 3)
                            [ "W1N2", reservedRoom true 5000; "W2N2", reservedRoom true 5000 ]

                    { colony with Hostiles = hostiles }

                Expect.equal
                    (guardQuotaOf (twoOutposts [ hostileIn "W1N2" raidTile smallMelee ]))
                    (Some 1)
                    "the premise: one raided outpost of the two hires one"

                Expect.equal
                    (guardQuotaOf (
                        twoOutposts
                            [
                                hostileIn "W1N2" raidTile smallMelee
                                { hostileIn "W2N2" { X = 20; Y = 41 } smallMelee with
                                    Id = "h-2"
                                }
                            ]
                    ))
                    (Some 2)
                    "and a raid in each hires one apiece"
            }

            test "the healing the second guard is priced against is the raided room's own" {
                // The conjunct that keeps the count room-local, in the
                // codebase whose first hazard is room aliasing (ADR 0041): the
                // healing term filters the raid by the room the [[threat]]
                // stands in, and without it two healers forty tiles away in
                // another outpost would price a fight they are not in.
                // Pairwise, one room apart — the same melee, the same two
                // healers, and only the healers' room moving. The other
                // outpost holds no armed hostile of its own, so it is no
                // guarded outpost and adds nothing to the sum from either
                // side.
                let twoOutposts healerRoom healerTile =
                    let raid =
                        hostileIn "W1N2" raidTile smallMelee
                        :: [
                            for i in 1..2 ->
                                { hostileIn healerRoom healerTile smallHealer with
                                    Id = $"heal-{i}"
                                }
                        ]

                    let colony =
                        reserverColony
                            [ northOutpost true; westOutpost false ]
                            (surplusFleet 3)
                            [ "W1N2", reservedRoom true 5000; "W2N2", reservedRoom true 5000 ]

                    { colony with Hostiles = raid }

                Expect.equal
                    (guardQuotaOf (twoOutposts "W1N2" raidTile))
                    (Some 2)
                    "the premise: 120 healed in the raided room against the 90 one block deals hires the second"

                Expect.equal
                    (guardQuotaOf (twoOutposts "W2N2" westSeat))
                    (Some 1)
                    "the same two healers standing in the other outpost price nothing here: this room's raid heals nothing"
            }

            test "the row is cast in front of the reserver and reads its own body back" {
                // The cascade slot (ADR 0056): behind the [[supply floor]]
                // and in front of the [[reserver]]. Both gaps are open on
                // this tick — the reservation is at its cap, so the reserver
                // row wants one block — and the colony's four idle spawns
                // draw the seats in order, so the *first* cast says which row
                // was asked first. Pairwise against the quiet tick, where the
                // reserver is the head of the cascade exactly as ADR 0042
                // left it.
                let castNames colony =
                    spawnIntents (decideOn colony).Intents
                    |> List.map (fun (_, _, name: string) -> name.Split('-').[0])

                Expect.equal
                    (castNames (guardColony [] []) |> List.truncate 1)
                    [ "reserver" ]
                    "the premise: with nothing to fight, the reserver is the head of the cascade"

                Expect.equal
                    (castNames (guardColony [ hostileIn "W1N2" raidTile smallMelee ] [])
                     |> List.truncate 2)
                    [ "guard"; "reserver" ]
                    "and a raid puts the guard in front of it, without displacing it"
            }

            test "the guard the row casts is the block the bank buys" {
                // The cast itself and not the quota: at the live RCL5 bank
                // the row buys two whole blocks, which is the body every
                // damage number above is written in.
                Expect.equal
                    (guardCasts
                        (decide
                            (guardColony [ hostileIn "W1N2" raidTile smallMelee ] [])
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    [ bodyFor guardPattern 1800 ]
                    "one cast, at the 1,800 bank `reserverColony` holds"
            }

            test "a bank that cannot afford a block yields the tick" {
                // ADR 0050 through the new row: 750 is more than a 300 bank
                // holds, so the row casts nothing and does not hold the
                // cascade for the rows behind it — a colony this small has
                // ADR 0043's stand-down and nothing else. Pairwise against
                // 800, the first bank that can pay for the row at all, with
                // the same raid standing in the same room.
                let raided = guardColony [ hostileIn "W1N2" raidTile smallMelee ] []

                let castsAt available capacity =
                    { raided with
                        Bank = bank available capacity
                    }
                    |> fun colony -> decideOn colony
                    |> fun result -> guardCasts result.Intents

                Expect.equal
                    (castsAt 800 800)
                    [ bodyFor guardPattern 800 ]
                    "the premise: at 800 the row buys its one block"

                Expect.isEmpty (castsAt 300 300) "at 300 it buys nothing and yields the tick"

                Expect.equal
                    (guardQuotaOf { raided with Bank = bank 300 300 })
                    (Some 1)
                    "and the quota is unmoved: what the poor bank refuses is the cast, not the row"
            }

            test "a guard fills the guard row's Living and no other row's" {
                // The row is read back off the parts like every other (ADR
                // 0006), and an ATTACK part is the one cut no other row of
                // this colony makes. Without the arm a `[T; A×3; M×5; H]`
                // has neither Work nor Carry and falls through to the
                // **generalist**, so a raid would quietly retire a worker for
                // the guard's whole 1,500-tick life. Pairwise, one body
                // apart.
                let livingOf colony =
                    (decideOn colony).Quotas.Rows |> List.map (fun row -> row.Row, row.Living)

                let quiet = livingOf (guardColony [] [])
                let standing = livingOf (guardColony [] [ guard "g-1", outpostSeat ])

                Expect.equal
                    (quiet |> List.map fst)
                    (standing |> List.map fst)
                    "the premise: the same rows either side"

                Expect.equal
                    (List.zip quiet standing
                     |> List.filter (fun ((_, before), (_, after)) -> before <> after)
                     |> List.map (fun ((row, before), (_, after)) -> row, before, after))
                    [ "guard", 0, 1 ]
                    "one body arrives and exactly one row's Living moves — the guard's"
            }

            test "an idle survivor keeps the row filled, so the next raid casts nothing" {
                // ADR 0056's "no decay", read at the seam it is about: a
                // guard that outlived its raid is pooled no work, stands
                // idle, and goes on counting in the row's `Living` — so a
                // second raid inside its 1,500 ticks buys nothing and waits
                // no thirty ticks of oven for a body the colony already
                // owns.
                let raid = [ hostileIn "W1N2" raidTile smallMelee ]

                Expect.equal
                    (guardCasts (decide (guardColony raid []) Map.empty Set.empty None).Intents
                     |> List.length)
                    1
                    "the premise: with nothing standing, the raid casts one"

                Expect.isEmpty
                    (guardCasts
                        (decide
                            (guardColony raid [ guard "g-1", outpostSeat ])
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "with the survivor standing, the same raid casts none"
            }

            test "a guard classifies Fighter, and no other row's body does" {
                // The [[body class]] ladder's new head (ADR 0056), read the
                // only way it is readable today: `Fighter` answers no
                // differently from `Carrier` in every [[capacity]] scope
                // written so far — `(=) Heavy`, `(<>) Heavy`, `(=) Standing`
                // and "neither Heavy nor Standing" — so the Guard Task's
                // `Fighter -> quota` is the first cap that will tell them
                // apart, and until it lands no fixture at the `decide` seam
                // can. Pinned here rather than left to that ticket, because
                // what it is guarding against is the guard falling back into
                // `Carrier` beside the [[hauler unit]]s, which is silent.
                //
                // Both halves of one claim, so both are asserted over one
                // fleet: the guard is a Fighter, and every other row's body
                // at the same bank is the class it was before the arm
                // existed.
                let bodies =
                    [
                        "guard", bodyFor guardPattern 800
                        "anchor", bodyFor anchorPattern 800
                        "upgrader", bodyFor upgraderPattern 800
                        "hauler", bodyFor haulerPattern 800
                        "reserver", bodyFor reserverPattern 800
                        "worker", bodyFor workerPattern 800
                    ]

                let colony =
                    { incomeColony with
                        Creeps = bodies |> List.map (fun (name, body) -> creepWith name 0 50 body)
                    }

                let atlas = Atlas.ofView colony

                Expect.equal
                    (colony.Creeps
                     |> List.map (fun creep -> creep.Name, bodyClassOf colony.Tuning atlas creep))
                    [
                        "guard", Fighter
                        "anchor", Heavy
                        "upgrader", Standing
                        "hauler", Carrier
                        "reserver", Carrier
                        "worker", Light
                    ]
                    "one row's body classifies Fighter and it is the guard's"
            }
        ]

[<Tests>]
let supplyFloorTests =
    testList
        "the supply floor, and a row that cannot be afforded"
        [
            test "#203: 361 in the bank, two Anchors, and the carrier is cast before every row" {
                // ADR 0050's floor, at the reading it was written from.
                // The head of the cascade wants 1,300 and the bank holds
                // 361; every row under it prices at capacity — hauler
                // 1,800, upgrader 1,750, worker 1,800 — and the one row
                // cheap enough to buy with a broken bank is the Anchor's
                // 700, whose gap is zero because the two bodies that made
                // the deadlock are Anchors. So falling through the cascade
                // alone still casts nothing: the floor is the half that
                // moves.
                match spawnIntents (decideOn deadlockColony).Intents with
                | [ (_, body, creepName) ] ->
                    Expect.stringStarts
                        creepName
                        "hauler-"
                        "the row that can refill an extension is cast before every other"

                    Expect.equal
                        body
                        [ Carry; Carry; Carry; Carry; Move; Move ]
                        "sized from what is banked right now — 361 buys two blocks — and never from \
                         the 1,800 capacity, which is the price the deadlock is made of"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "an Anchor's lone Carry does not answer the gate" {
                // The counterexample the gate is written against, pairwise
                // against the case above on the reading and not on the
                // fleet: same colony, same bank. `6W/1C/1M` holds a Carry
                // part and a Move part, so a floor gated on "no body with a
                // Carry that can move" is a floor two Anchors hold down for
                // ever — and #203 reproduces itself unchanged with the rule
                // in place. The gate is `Refill`'s own conjunction beside
                // `Withdraw`'s: a Carry, no standing-body ratio, no more
                // Work than Move.
                Expect.isTrue
                    (deadlockColony.Creeps
                     |> List.forall (fun creep -> Map.containsKey Carry creep.Body))
                    "the premise: every body in this fleet carries a Carry part"

                Expect.isNonEmpty
                    (spawnIntents (decideOn deadlockColony).Intents)
                    "and the colony still hires a carrier, because none of them can refill one"
            }

            test "a full bank does not disarm the floor: the carrier is bought first" {
                // Pairwise with the #203 case on the bank alone — the same
                // two Anchors, the same two declared outposts, and 1,800 of
                // 1,800 banked, a bank every row below can pay for. The
                // floor is armed by the *absence* of a body that can put
                // energy into an extension and by nothing else (ADR 0050):
                // firing it only on a short bank was considered and
                // rejected, because it re-opens the cheapest failure the
                // incident showed — the reserver row takes 1,300 first and
                // the carrier is hired out of what is left on the next
                // tick, which is the losing race the live colony ran when
                // the manual hauler filled the bank to 1,900 and two
                // reserver casts took 2,600 back out of it inside 64 ticks.
                let full =
                    { deadlockColony with
                        Bank = bank 1800 1800
                    }

                match spawnIntents (decideOn full).Intents with
                | [ (_, body, creepName) ] ->
                    Expect.stringStarts
                        creepName
                        "hauler-"
                        "the body the bank depends on is cast before the bodies that depend on the \
                         bank, however full the bank is"

                    Expect.equal
                        body
                        (List.replicate 24 Carry @ List.replicate 12 Move)
                        "sized from what is banked right now, which at a full bank is the whole \
                         1,800 the capacity would have bought"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "one living hauler switches the floor off and the cascade is unchanged" {
                // The floor is a floor and not a new head row: with one
                // body alive that can draw from a store and deliver into
                // an extension, the bank is fillable again and the head of
                // the cascade is the reserver row's, exactly as ADR 0042
                // orders it.
                let withHauler =
                    { deadlockColony with
                        Creeps = hauler "h1" 0 100 :: deadlockColony.Creeps
                        Bank = bank 1800 1800
                    }

                match spawnIntents (decideOn withHauler).Intents with
                | [ (_, _, creepName) ] ->
                    Expect.stringStarts
                        creepName
                        "reserver-"
                        "with the bank fillable the declared outposts' row is first again"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "the empty colony's disaster fallback is untouched" {
                // ADR 0006's fallback is the floor's ancestor and not its
                // casualty: a colony with no creep at all still casts the
                // minimal worker unit from what is banked, because
                // time-to-first-creep outranks every row including this
                // one. Same colony, same 361, and only the fleet moves.
                match
                    spawnIntents
                        (decide { deadlockColony with Creeps = [] } Map.empty Set.empty None)
                            .Intents
                with
                | [ (_, body, creepName) ] ->
                    Expect.stringStarts
                        creepName
                        "worker-"
                        "time-to-first-creep still outranks the row that asked"

                    Expect.equal body [ Work; Carry; Move ] "and it is still the worker unit"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "a row the bank cannot pay for yields the tick to the row below it" {
                // ADR 0050's other half, read where the floor is disarmed:
                // the fleet holds four haulers, so nothing here is the
                // supply floor. One outpost declared and unheld is a
                // reserver gap of one at `[2Claim;2Move]` = 1,300; one
                // Anchor against the home room's two Posts is an Anchor gap
                // of one at `6W/1C/1M` = 700; every other row is over
                // quota.
                //
                // Pairwise on the bank alone, and the second reading is why
                // this is not "skip the reserver": at 1,300 the head row is
                // affordable and it is cast, on the very next tick a filled
                // extension would give it.
                let castsAt available =
                    let colony = reserverColony [ northOutpost false ] (surplusFleet 1) []

                    spawnIntents
                        (decide
                            { colony with
                                Bank = bank available 1800
                            }
                            Map.empty
                            Set.empty
                            None)
                            .Intents

                match castsAt 700 with
                | [ (_, body, creepName) ] ->
                    Expect.stringStarts
                        creepName
                        "anchor-"
                        "the head row is 600 short, so the empty Post below it is filled instead"

                    Expect.equal
                        body
                        [ Work; Work; Work; Work; Work; Work; Carry; Move ]
                        "and at the Anchor row's own capacity-sized body, unchanged by the fall"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"

                match castsAt 1300 with
                | [ (_, _, creepName) ] ->
                    Expect.stringStarts
                        creepName
                        "reserver-"
                        "and the tick the bank affords it, the same head row is first again"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }
        ]
