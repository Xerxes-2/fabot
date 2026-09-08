/// A source's output, the room that holds it, and the deficit that gates
/// the worker row alone.
module Fabot.Core.Tests.Decide.QuotaSourceTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.QuotaFixtures

[<Tests>]
let sourceOutputTests =
    testList
        "a source's output and the room that holds it"
        [
            // Ten is the *reserved* rate (ADR 0042). The colony that holds
            // W1N2 counts ten energy a tick from its rock and the colony
            // that does not counts five, and five over a 1,500-tick
            // lifetime is two worker places at this bank's Work drain of
            // three. So the fleet below is sized to the *unreserved*
            // target: the unreserved colony has no gap to cast into and
            // the reserved one does, which is a difference no shared cap
            // and no one-body-per-spawn limit can hide.
            //
            // Unreserved the target is 3 Anchors — the outpost's Post
            // hires one since #129 — + 2 haulers +
            // ceil(((20 + 5) × 1500 − 3000) / 4500) = 8 workers = 13;
            // reserved it is 10 workers and 15. The amortization is three
            // Anchors at 600 and two haulers at 600, every one of them the
            // body this 600 bank casts.
            let unreservedWorkers = 8

            test "the same outpost source is worth twice as much reserved" {
                Expect.isEmpty
                    (spawnIntents
                        (decide
                            { midIncomeColony with
                                Creeps = incomeFleetOf 7
                            }
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "the premise: without the outpost this bank's whole target is 2 Anchors, \
                     2 haulers and 7 workers, and `incomeFleetOf` spells it a worker count at \
                     a time"

                Expect.isNonEmpty
                    (Atlas.postsOf
                        (Atlas.ofView (postedOutpostColony unreservedWorkers []))
                        "src-out")
                    "the premise: the container standing on its Seat makes the rock a Post"

                Expect.isEmpty
                    (spawnIntents
                        (decide
                            (postedOutpostColony unreservedWorkers [ "W1N2", neutralRoom ])
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "unreserved, the rock is worth five a tick and the fleet already matches"

                Expect.isNonEmpty
                    (spawnIntents
                        (decide
                            (postedOutpostColony
                                unreservedWorkers
                                [ "W1N2", reservedRoom true 4000 ])
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "reserved, the same rock is worth ten and the colony hires against it"
            }

            test "a reservation another player holds doubles nothing of ours" {
                // Pairwise, one rival at a time: the same room, the same
                // rock, the same reservation standing on the same
                // controller — only whose it is moves. The engine pays
                // ten a tick in a room a rival holds as readily as in one
                // we hold (docs/research/remote-mining.md §1.1); the
                // colony prices it at five anyway, because a room
                // somebody else holds is one it is withdrawing from.
                Expect.isEmpty
                    (spawnIntents
                        (decide
                            (postedOutpostColony
                                unreservedWorkers
                                [ "W1N2", reservedRoom false 4000 ])
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "another player's reservation prices the rock exactly as none at all does"

                // Five and specifically not nothing. The assertion above
                // is sized to the neutral target, so it would hold just
                // as well if a rival's reservation made the rock
                // *unpriceable* — the answer ADR 0004 reserves for a room
                // with no vision. This fleet is the blind target below,
                // which the neutral rate outgrows and the blind one does
                // not, so the branch is pinned strictly between the two.
                Expect.isNonEmpty
                    (spawnIntents
                        (decide
                            (postedOutpostColony 6 [ "W1N2", reservedRoom false 4000 ])
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "a rival's reservation prices the rock at five and hires, not at nothing"
            }

            test "a room another player owns doubles nothing of ours either" {
                // The other half of "somebody else holds it", and the one
                // the projection could not tell from an unowned room until
                // #133: a rival's *ownership*. ADR 0043 withdraws from
                // either half — since #165 the owned half is the latch and
                // the reserved half a clock — so either has to be a fact the
                // ColonyView can state, and stating it must not accidentally
                // read as a hold of ours, which is what this pins.
                //
                // Pairwise against the neutral room, one rival at a time:
                // same room, same rock, same container, same fleet. The
                // engine pays ten a tick in a room a rival owns exactly as
                // in one we own (`sources/tick.js` switches on
                // `roomController.user || roomController.reservation`); the
                // colony prices it at five for the same reason it prices a
                // rival's reservation at five.
                Expect.isEmpty
                    (spawnIntents
                        (decide
                            (postedOutpostColony unreservedWorkers [ "W1N2", rivalRoom ])
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "a rival's ownership prices the rock exactly as nobody's does"

                // Five and specifically not ten, which is the failure a
                // three-state owner exists to make unrepresentable: read
                // as "owned, therefore held", the same rock would be worth
                // ten and this fleet would be five worker places short.
                // The one input that moves between this and the assertion
                // above is whose the controller is.
                Expect.isNonEmpty
                    (spawnIntents
                        (decide
                            (postedOutpostColony unreservedWorkers [ "W1N2", ownedRoom ])
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "owned by us the same rock is worth ten, so the fleet above is the neutral one"

                // Five and specifically not nothing, the same strict
                // bracket the reservation case is pinned in: sized to the
                // blind target, the neutral rate hires and unpriceable
                // does not.
                Expect.isNonEmpty
                    (spawnIntents
                        (decide
                            (postedOutpostColony 6 [ "W1N2", rivalRoom ])
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "a rival's ownership prices the rock at five and hires, not at nothing"
            }

            test "the NPC's reservation prices like a rival's and is not the same fact" {
                // The third holder (ADR 0043). A level-0 invader core
                // `attackController`s the room it expanded into and holds
                // the reservation itself — the measured core two rooms
                // from W12S27 does exactly this
                // (docs/research/remote-mining.md §8.4) — and that
                // reservation is the *only* readable deadline it has,
                // because a level-0 core carries no collapse timer.
                //
                // ADR 0043 reads different answers off the NPC's hold and a
                // player's: the NPC's is the clock a core's stand-down runs
                // to under the fallback floor (#136), a player's the clock
                // its own stand-down runs to with no floor at all (#165). So
                // the two must price the same and must stay tellable apart.
                // Pricing first, pairwise against the rival's reservation,
                // one input at a time.
                let priced control =
                    spawnIntents
                        (decide
                            (postedOutpostColony unreservedWorkers [ "W1N2", control ])
                            Map.empty
                            Set.empty
                            None)
                            .Intents

                Expect.equal
                    (priced (coreReservedRoom 4000))
                    (priced (reservedRoom false 4000))
                    "the NPC's reservation prices the rock exactly as a rival's does"

                Expect.isEmpty
                    (priced (coreReservedRoom 4000))
                    "and that price is five, not the held ten"

                Expect.isNonEmpty
                    (priced (reservedRoom true 4000))
                    "held by us the same rock is worth ten, so the fleet above is the neutral one"

                // And tellable apart, which is the whole reason the holder
                // is a closed three-state rather than a flag. A ColonyView
                // that answered both with one "not ours" would hand the
                // gate ADR 0043 describes an input on which no correct
                // answer exists: the NPC's hold read as a rival's shuts an
                // outpost for the life of the colony, and a rival's read
                // as the NPC's walks back into a room somebody else holds.
                let holderOf (control: RoomControlInfo) =
                    control.Reservation |> Option.map (fun held -> held.Holder)

                Expect.notEqual
                    (holderOf (coreReservedRoom 4000))
                    (holderOf (reservedRoom false 4000))
                    "the NPC's hold and a rival's are two facts, not one"

                Expect.notEqual
                    (holderOf (coreReservedRoom 4000))
                    (holderOf (reservedRoom true 4000))
                    "and neither of them is ours"
            }

            test "an outpost the colony cannot see this tick prices no source" {
                // ADR 0004, entry by entry: who holds a room we cannot look
                // into is not a fact this tick, so the source is
                // unpriceable and enters no quota. Unpriceable is not
                // half — half is what a room we *can* see and nobody holds
                // is worth, and the pair below is what separates the two.
                //
                // What is blind here is the *control* entry alone, which is
                // the one input this test moves. The fixture's container
                // still stands in the projection, so its Post is still in
                // the Anchor row and the fleet still carries `a-out` — live
                // the two arrive and vanish together, because the shell
                // gates the structure census and the control entry on the
                // same `seen` list.
                let blind = postedOutpostColony 6 []

                Expect.isEmpty
                    (spawnIntents (decide blind Map.empty Set.empty None).Intents)
                    "no entry for W1N2: the rock's output prices at nothing and the fleet still matches"

                Expect.isNonEmpty
                    (spawnIntents
                        (decide
                            { blind with
                                RoomControl = Map.add "W1N2" neutralRoom blind.RoomControl
                            }
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "seen and held by nobody, the same rock is worth five and hires"
            }

            test "the colony's own room is priced on its owner, not on a reservation" {
                // The trap #116's prose walks into and ADR 0042's rule does
                // not: taken as "reserved, or half", the spawn room — which
                // is owned and which nothing reserves — would price both its
                // sources at five, halving the income base and the hauler
                // quota together. The engine gives a room with an owner the
                // same 3,000 a cycle it gives a reserved one.
                //
                // Sized to the halved target so the direction is
                // readable, and the hauler row halves with the output it
                // ships: 2 Anchors + 1 hauler, whose amortization is
                // 2 × 400 + 1 × 600 = 1,400, + ceil((10 × 1500 − 1,400) /
                // 4500) = 4 workers = 7. Held it would be 2 + 2 + 7 = 11,
                // which is what the second half reads.
                //
                // The Anchors are 400 and not 600 because a neutral rock
                // lowers the row's own ceiling as well as its output (ADR
                // 0021 as ADR 0042 narrows it): three Work saturate a rock
                // giving five, and this bank would otherwise buy five.
                let halved =
                    { midIncomeColony with
                        Creeps = incomeFleetRows 1 4
                        RoomControl = homeControl |> Map.map (fun _ _ -> neutralRoom)
                    }

                Expect.isEmpty
                    (spawnIntents (decide halved Map.empty Set.empty None).Intents)
                    "the premise: a neutral spawn room's whole target is these seven"

                Expect.isNonEmpty
                    (spawnIntents
                        (decide
                            { halved with
                                RoomControl = homeControl
                            }
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    "owned, the same two sources are worth ten each and the fleet is four short"
            }

            test "the hauler quota prices each container at its own source's output" {
                // The quota's other reader (ADR 0042), read here on the
                // colony's own room: it folds every projected room's
                // containers and prices each at *that* container's
                // source, so moving the rate under the home room moves
                // the home containers' half of it and nothing else. The
                // outpost half is `outpostHaulTests`, on a fixture with a
                // Seam to cross. The two containers' demands are summed and
                // rounded once (ADR 0049): ceil((24 + 24) × 10 / 400) is
                // two haulers for the pair and ceil((24 + 24) × 5 / 400)
                // is one.
                Expect.equal (quotaOf midIncomeColony) 2 "the premise: the reserved rate hires two"

                Expect.equal
                    (quotaOf
                        { midIncomeColony with
                            RoomControl = homeControl |> Map.map (fun _ _ -> neutralRoom)
                        })
                    1
                    "half the output is half the haul, and the pool is a body lighter"

                Expect.equal
                    (quotaOf
                        { midIncomeColony with
                            RoomControl = Map.empty
                        })
                    0
                    "a container whose source's room prices nothing hires nobody (ADR 0004)"
            }

            test "a quota memoised while the room was held is not handed back when it lapses" {
                // ADR 0017's stated failure mode, at the seam that would
                // ship it: the hauler quota rides the census memo, and
                // since ADR 0042 it reads who holds the room — a per-tick
                // vision fact, not a census one. `Main.fs` keeps the memo
                // in heap and hands `decide` last tick's every tick, so a
                // signature blind to the rate would recall three haulers
                // for a room now worth half, and would size the worker
                // row off that amortization too. Every census input here
                // is byte-identical between the two views: the
                // reservation is the only thing that moved.
                let lapsed =
                    { midIncomeColony with
                        Creeps = incomeFleetRows 1 4
                        RoomControl = homeControl |> Map.map (fun _ _ -> neutralRoom)
                    }

                let previous = (decide midIncomeColony Map.empty Set.empty None).Memo

                Expect.equal
                    previous.HaulerQuota
                    2
                    "the premise: held, the two home containers hire two"

                let recalled = decide lapsed Map.empty Set.empty (Some previous)
                let fresh = decide lapsed Map.empty Set.empty None

                Expect.equal
                    recalled.Memo.HaulerQuota
                    fresh.Memo.HaulerQuota
                    "the stale memo recomputes to the fresh quota: half the output, half the haul"

                Expect.equal
                    (spawnIntents recalled.Intents)
                    (spawnIntents fresh.Intents)
                    "so the fleet standing at the halved target casts nothing it does not need"
            }
        ]

[<Tests>]
let rowGapTests =
    testList
        "the deficit gates the worker row alone"
        [
            // Read as the switch's own tests are, one body at a time off
            // the one idle spawn `switchHome` stands: a tick casts at most
            // one body, so the list this returns is either empty or names
            // the row whose gap was answered first.
            let casts colony fleet =
                spawnIntents
                    (decide { colony with Creeps = fleet } Map.empty Set.empty None).Intents

            // The premise every case below rests on, asserted where it is
            // used rather than assumed: at `switchUnposted`'s target of
            // six a fleet of twelve is far over, and one body fewer is
            // still over — so nothing that follows can be the ordinary
            // deficit hiring.
            let overTarget colony fleet =
                Expect.isEmpty
                    (casts colony (List.truncate (List.length fleet - 1) fleet))
                    "the premise: a body short of this fleet the colony is still over target"

            test "the tick a source unposts, the home room's empty Post is cast for anyway" {
                // #154's reproduction, and the reason the gate moved. The
                // colony loses vision of its outpost for one tick: the
                // source there unposts, and its Anchor place, its haul and
                // its income share leave the target together (ADR 0042,
                // ADR 0004), dropping it under the living count. The home
                // room's Post is empty across both ticks and is a fact
                // about the ground either way — gated on the deficit it
                // went unfilled until ordinary deaths had paid off the
                // whole twelve-body overshoot, and the colony cast
                // nothing at all, in its own room included, in the
                // meantime.
                //
                // Pairwise, one rival at a time: the two fleets differ in
                // the two Anchors' bodies and in nothing else.
                Expect.isEmpty
                    (casts switchUnposted switchFleet)
                    "with every row manned the same twelve cast nothing"

                overTarget switchUnposted switchFleet

                match casts switchUnposted unmannedPosts with
                | [ (_, body, name) ] ->
                    Expect.stringStarts
                        name
                        "anchor-"
                        "the empty Post is filled from the Anchor row"

                    Expect.equal
                        body
                        [ Work; Work; Carry; Move ]
                        "and sized to the bank exactly as that row always is"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "a standing container's hauler gap is filled under the target too" {
                // The same rule on the row beside it (ADR 0012): a source
                // container standing wants its round trip shipped whatever
                // the headcount is, and the tick the target fell the
                // container did not stop standing. Both Anchors stay alive
                // here, so the Anchor row has no gap and the hauler row is
                // the only rival the cast can come from.
                Expect.isEmpty
                    (casts switchUnposted switchFleet)
                    "with every row manned the same twelve cast nothing"

                overTarget switchUnposted switchFleet

                match casts switchUnposted unshippedFleet with
                | [ (_, _, name) ] ->
                    Expect.stringStarts
                        name
                        "hauler-"
                        "the home container's own round trip hires it"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "the worker row is the one the deficit is the quota of, and it still stops" {
                // The half of the gate that does not move (ADR 0012): the
                // worker row's quota *is* whatever the target has left over
                // once the specialist rows are counted, so with nothing
                // left over it hires nobody however far the fleet has
                // overshot. Pairwise against the same fleet under a target
                // that reaches it — one room's vision richer, where those
                // twelve are the target — and one body short there is a
                // worker.
                Expect.isEmpty
                    (casts switchUnposted switchFleet)
                    "six over target, every row manned, and no generalist"

                overTarget switchUnposted switchFleet

                match casts switchPosted (List.truncate 11 switchFleet) with
                | [ (_, _, name) ] ->
                    Expect.stringStarts
                        name
                        "worker-"
                        "posted, the target reaches the fleet and the remainder is the worker row's"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "a row standing over its quota still holds the worker row down" {
                // What the deficit is and is not (ADR 0012). It gates the
                // worker row; it is not that row's own gap, and the
                // difference shows the tick a specialist row stands over
                // quota. Under `switchUnposted` the Anchor row wants one
                // and the hauler row one: a fleet of two Anchors, two
                // haulers and two workers is six bodies exactly at the
                // target, two of them surplus specialists, and the worker
                // row is two short of its own quota of four. The surplus
                // holds it there — #154 moves the specialist rows off the
                // deficit and deliberately leaves this half of the gate
                // standing.
                //
                // Pairwise against the same target with the specialist
                // rows at quota, where the whole-fleet gap and the worker
                // row's own gap coincide and one body short is a worker.
                let overSpecialised =
                    switchFleet
                    |> List.filter (fun creep ->
                        not (List.contains creep.Name [ for i in 3..8 -> $"w{i}" ]))

                Expect.hasLength
                    overSpecialised
                    6
                    "the premise: six bodies, standing exactly at the target"

                Expect.isEmpty
                    (casts switchUnposted overSpecialised)
                    "two surplus specialists, and the worker row hires none of its two missing"

                match casts switchUnposted (List.truncate 5 switchHomeFleet) with
                | [ (_, _, name) ] ->
                    Expect.stringStarts
                        name
                        "worker-"
                        "with every specialist row at quota the same shortfall is hired"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test
                "the doorstep hold still comes first: an empty Post is no reason to cast into a Reach" {
                // ADR 0033's gate is asked before anything is priced and
                // this ticket does not move it (#154). The row gap that
                // now outlives a negative deficit is exactly the case that
                // could have walked past it — the hold is the outer
                // question, the deficit an inner one.
                let hot =
                    switchUnposted |> facing [ hostileAt "h-1" { X = 25; Y = 13 } [ Attack; Move ] ]

                Expect.isNonEmpty
                    (casts switchUnposted unmannedPosts)
                    "the premise: quiet, the empty Post is cast for"

                Expect.isEmpty
                    (casts hot unmannedPosts)
                    "and under fire the same empty Post casts nothing"
            }
        ]
