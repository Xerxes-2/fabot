/// The upgrader row, its lead and its quota (ADR 0046), and the colony's
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
                // `patternOf` reads a living body back to the row it was
                // cast from (ADR 0006), and the row is what sizes the
                // replacement a lead prices (ADR 0026). A `11W/1C/11M` body
                // has Work at Move and a Carry nine times short of parity,
                // so before ADR 0046's row existed it fell through to the
                // generalist and was priced as one.
                //
                // The two arithmetics at this colony's 1,800 bank: the
                // upgrader row casts twenty-three parts, 69 ticks in the
                // spawner, and its eleven Move carry eleven fatigue parts
                // — an empty Carry rides free — over a plain tile in the
                // walk's one-tick floor, 3 ticks for the three steps, a
                // lead of 72. The generalist row at the same bank is nine
                // whole units, twenty-seven parts: 81 ticks in the spawner
                // and the same 3 of walking, a lead of 84. Every life
                // between the two is where the rows disagree.
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
                // The lead is priced off the row and the *replacement* is
                // hired off the quota, and the two are separate readings
                // (ADR 0026 beside ADR 0046). This colony has no controller
                // container, so the upgrader row's quota is zero however
                // many standing bodies stand in it (#187, `upgraderQuota`),
                // and the body the colony is one short of is hired from the
                // generalist row's remainder. A `SpawnCreep` named
                // "upgrader-" here would mean the quota had stopped reading
                // the buffer.
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
                // `6W/1C/1M` answers to both descriptions — more Work than
                // Move *and* a standing body — and it is the Anchor row
                // that cast it, so that is the row it is read back to and
                // the lead it is priced at. The arms are ordered and the
                // order is the rule (ADR 0021 over ADR 0046).
                //
                // Eight parts is 24 ticks in the spawner, and six fatigue
                // parts on one Move is six ticks a plain step: a lead of
                // 42. Read as an upgrader it would be 72, so a life of 50
                // sits between the two rows and casts under one of them
                // only.
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
                // ADR 0046's whole arithmetic at the live bank, and the
                // numbers are the ADR's own, less the hauler body ADR 0049
                // took off the row. Twenty a tick from two posted sources
                // over a 1,500-tick life is 30,000; the two rows hired off
                // the ground cost 2 × 700 of Anchor and 1 × 1,800 of
                // hauler, so the surplus is 26,800 and one standing body's
                // eleven Work drinks 16,500 of it — a quota of one, the
                // whole bodies the surplus buys and no part of one (#195,
                // pinned below).
                //
                // The body is the row's sizing rule and not the
                // generalist's: eleven Work, one Carry, eleven Move for
                // 1,700 of the 1,800, where `9W/9C/9M` buys nine Work out
                // of the same bank.
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
                // #195, amending ADR 0046: two rows are hired out of one
                // surplus and only one of them may round up. ADR 0037
                // admits an oversell bounded by *one body's* lifetime
                // drain, paid out of stock rather than income, and two
                // rows rounding up against the same number sell that bound
                // twice — so the rounding stays with the smaller body,
                // which is the generalist's.
                //
                // 26,800 of surplus over 16,500 of drink is one and a half
                // standing bodies. One is hired; the remainder — 10,300 of
                // drink less the 1,700 that body costs to replace, so
                // 8,600 — is the generalist row's, and its own rounding up
                // (ADR 0037) turns it against a 13,500 drain into one
                // worker, which is the number ADR 0046's own #195 bullet
                // hands on. Rounded up here
                // instead it was two standing bodies drinking 33,000
                // against 26,800 of surplus — twenty-two a tick against
                // twenty of income, the difference made up out of the
                // storage for the whole of both lives (#187's finding).
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

            test "a surplus of two and a half standing bodies hires two" {
                // The pairwise on the surplus alone, one rival at a time:
                // the same room, the same 1,800 bank, the same
                // `11W/1C/11M`, and a third posted source. 45,000 in over
                // a lifetime less 2,100 of Anchor and the hauler row's
                // body is a surplus of rather over two whole standing
                // bodies, and the quota is the two — the floor is the
                // *quotient's* whole part and never a cap of one.
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
                // The other half of the same pairwise, and the case the
                // old rounding got most wrong: one posted source instead
                // of two. 15,000 in less 700 of Anchor and the hauler's
                // body is a surplus of about three quarters of the 16,500
                // one standing body drinks. Rounded up that hired a whole
                // eleven-Work body against three quarters of the income to
                // feed it; rounded down the row is empty and the surplus
                // goes to the generalist row, which is where it went
                // before the buffer stood.
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
                // The gate, pairwise: the same room, the same bank, the
                // same two Posts, and the buffer a site instead of a
                // structure. A row hired against a promise would stand
                // beside a hole with nothing to withdraw from, so the
                // quota is zero and the surplus goes back to the
                // generalist row — ceil((26,800 − 0) / (9 × 1,500)) = 2
                // workers, which is the count this colony hired before the
                // row existed.
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

                // The third reading of the same tile, and the one the
                // fixture existed as before this ticket: no container at
                // the controller at all. The worker count is the site
                // case's, which is what "the buffer is the switch" means
                // — a pending container is not half a buffer.
                Expect.isEmpty
                    (casts (upgraderColony upgraderRoom) (upgraderFleet 0 2))
                    "with no container at the controller the same two generalists are the target"
            }

            test "the standing row is cast after the hauler row and before the generalist" {
                // The cascade's new rung (ADR 0046, #154's shape): the
                // three rows hired off the ground produce the surplus this
                // one spends, so they are cast first; the generalist
                // spends the same surplus at nine Work against eleven, so
                // it is cast last. Pairwise, one rival at a time — the
                // fleets below differ from the fleet above in one body
                // each.
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

                // One generalist stands in this fleet where the others
                // have none, and it is the supply floor's premise rather
                // than this reading's (ADR 0050): with the hauler filtered
                // out the fleet would be two Anchors, which can refill no
                // extension, so the cast read below would be the floor's
                // carrier — and at this bank the floor's body and the
                // hauler row's are the same body, so the name would not
                // say which row answered.
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
                // The head of the cascade keeps its place (ADR 0042): the
                // reserver decides whether an outpost's sources are worth
                // five a tick or ten, and the upgrader spends what they
                // bring in. Pairwise on the one body — the same colony
                // with the reserver alive casts the standing row.
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
                // ADR 0046's floor, pairwise on the pool alone: the same
                // colony, the same fleet, one construction site between
                // the two readings. Without the floor a colony beside a
                // rich buffer would run no body that may build or repair
                // at all — a standing body is shut out of all three
                // deliveries (ADR 0046) and the hauler row has no Work
                // part.
                //
                // This fixture's income term is one since #195: the
                // remainder the standing row leaves feeds one generalist
                // mouth, so what these readings pin is the floor's *upper*
                // half — the step from one to two that a site in the pool
                // buys. The lower half, the one otherwise, is pinned at
                // the poorer bank further down ("the standing row's own
                // replacement can eat the whole remainder"), where the
                // income term really is zero and the floor is the only
                // thing hiring a body that may build.
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
                // ADR 0046's trap: the surplus is what the posted sources
                // bring in less the ground rows' amortization, so a colony
                // with no posted source has none to divide. The buffer
                // stands, the controller is there, and the row is still
                // not hired — the container is a *precondition* of the
                // quota and never its cause.
                //
                // A colony with no Post has no amortization either, so the
                // surplus here is exactly zero rather than negative and it
                // is the division's own answer that is being read: the
                // quota's `|> max 0` is the floor under the case this
                // fixture cannot reach, an amortization above income by
                // more than one whole body's lifetime drain (integer
                // division truncates toward zero, so a smaller shortfall
                // answers 0 unaided).
                //
                // The generalist below is *not* the worker row's floor
                // being read: with no Post there is no Anchor and no
                // hauler either, so the sum is one body at most and
                // `max minWorkforce` (ADR 0012) answers two whatever the
                // floor is. ADR 0046's floor is pinned where it is
                // separable — at the RCL4 bank below, where the ground
                // rows carry the sum clear of two on their own.
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
                // ADR 0046's floor at its lower half, separable at last
                // (#195). Rounding down leaves a remainder, but the
                // remainder is bounded by one whole drink and the standing
                // row's own replacement is charged against it before the
                // commuting row divides — so a surplus landing just past a
                // whole multiple of the drink leaves the generalist row
                // asking for nothing, and only the floor keeps a body that
                // may Build or Repair in the colony.
                //
                // One posted source at the RCL4 bank is that colony:
                // 15,000 in, less 700 of Anchor and 1,200 of hauler, is
                // 13,100 of surplus; the row's `8W/1C/8M` drinks 12,000 of
                // it, so the quota is one and 1,100 is left — and the
                // 1,250 that body costs to replace is more than the whole
                // of it, so the income term is zero. The target is one
                // Anchor, one hauler, one standing body and the floor's
                // one generalist, which is four and clear of ADR 0012's
                // colony floor of two: what is read here is ADR 0046's
                // row floor and nothing else.
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
                // The amortization term ADR 0046 asks `workforceTarget` to
                // grow, which was inert while the quota rounded up and
                // bites since #195: what the commuting row divides is the
                // remainder *less* the standing bodies' own replacement,
                // or the colony hires an upgrade mouth out of energy that
                // replacement is already spending.
                //
                // One posted source at the RCL3 bank is where the charge
                // is the whole of the answer: 15,000 in, less 700 of
                // Anchor and 750 of hauler, is 13,550; the row's
                // `5W/1C/5M` drinks 7,500, so the quota is one and 6,050
                // is left. Charged the 800 that body costs to replace it
                // is 5,250, and ADR 0037's rounding turns that against a
                // 6,000 drain into one generalist. Uncharged it would be
                // 6,050 — over the drain, and a second generalist.
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
                // The gate's other half (#187, ADR 0046's amended
                // Consequences): the row is *counted* by `patternOf`, off
                // the parts, so at a bank where the sizing rule's own cast
                // is read back to the generalist the quota hires nobody.
                // Pairwise on the bank alone — 800 against 750 at the same
                // RCL3 — because 800 is where one Carry against
                // `floor((capacity - 50) / 150)` Work reaches four Work to
                // the Carry.
                // Two haulers standing, as the RCL2 arm below has: the
                // hauler row is priced at the dearest sink it reaches, and
                // at an 800 bank this room's two containers ask for two
                // bodies, whose gap would be cast ahead of the upgrader's.
                Expect.stringStarts
                    (castName (casts (atBank 800 3) (upgraderFleet 0 0 @ [ hauler "h2" 0 100 ])))
                    "upgrader-"
                    "at the 800 bank the row's own cast is a standing body, so the surplus hires it"

                Expect.stringStarts
                    (castName (casts (atBank 750 3) (upgraderFleet 0 0 @ [ hauler "h2" 0 100 ])))
                    "worker-"
                    "fifty energy poorer the same cast is `4W/1C/4M` and the row is not hired"

                // The RCL2 bank's hauler body carries 300 where the 800
                // bank's carries 500, so the two containers' summed demand
                // comes to two bodies there and one here (ADR 0049); the
                // reading is the row *under* the hauler's, so its own gap
                // is filled before the case is read.
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
                // #216 R4's scope note, on the geometry that cost W13S28 a
                // thousand ticks: this colony's sinks are the
                // spawn/extension cluster **and** the controller's
                // [[buffer]], each priced at its own round trip and the
                // flow spread over both. Pairwise on the controller's
                // position alone — the same rock, the same container, the
                // same spawn, the same bank.
                //
                // The third number in each row is the same colony with the
                // buffer taken out of the census, which is the sink set as
                // it was before R4: beside the spawn the buffer's own leg
                // is the cluster's and the answer does not move, and
                // fifteen tiles further off it is most of the colony's haul
                // and the row hires for it. That is the live shape — a
                // buffer at zero, a container overflowing 2,000 with 1,859
                // on the ground beside it, and seven mini workers walking
                // fifty tiles for what one hauler was never hired to bring.
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
                // The mirror of the case above. Under R4's mean this read
                // *two*: the average was a claim about proportion, and a
                // buffer one tile from the container pulled the long haul
                // to the cluster down. Live (W13S28, 2026-09-07) the near
                // sink was the spawn cluster, which fills in a trip, so the
                // flow that ran all day was the far one and the mean hired
                // a body short while both containers stood full. The row is
                // priced at the dearest reachable sink now, so a nearer sink
                // never lowers the count.
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
                // The ticket's own pairwise, on the room it names: W13S28
                // at RCL3 on an 800 bank, the child whose north container
                // held 2,000 with 1,859 decaying beside it while one hauler
                // ran to the spawn. `RoomFixtures.colonyAt` furnishes the
                // captured room as its Layout would have left it — a
                // container on each source's Seat and the upgrade buffer
                // beside the controller — so the legs here are the room's
                // real terrain and not a lane drawn to make a point.
                //
                // Pairwise on the sink set alone, which is the only thing
                // R4 moved: the same room, the same bank, the same fleet,
                // with the buffer in the census and then out of it (the
                // rule as it stood before R4). The numbers are the room's
                // answer rather than a chosen value — if the fixture's own
                // furniture moves they move with it, and what must not
                // move is that the buffer is worth a body here.
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
                // #222's quota half (ADR 0052 decision 7). A mother hauls
                // her stock into a child that is still being raised and
                // stops the tick it is `Independent`, which is what the
                // stage means. Pairwise on the child's stage alone: the
                // same rooms, the same Storage, the same declaration and
                // the same borrowing list — each of them at the one load
                // the rule was derived at, which is now the shipped default
                // too (#216 R5 landed the Refill half).
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

                // The shipped number **is** the derived one since #216 R5:
                // the Refill that spends a ferried load stands beside this
                // term now, pooled for the very tile the round trip above
                // was priced to, so the body hired here has a Task and the
                // two halves of the split feature ship together.
                Expect.equal
                    (quotaOf (ferryMother Bootstrapping))
                    1
                    "and the shipped default lends the one body the rule was derived at"

                // The cap is what makes the borrowing an exception rather
                // than a second economy (ADR 0052 decision 7): what a
                // mother lends is written down, never derived from how much
                // the child could absorb.
                Expect.equal
                    (quotaOf (
                        ferryMother Bootstrapping |> tunedBy (fun t -> { t with FerryLoads = 2 })
                    ))
                    2
                    "and `Tuning.FerryLoads` is the whole of how much she lends"
            }

            test "the ferry's sink is the mother's to fill and never to draw" {
                // #222's pool half (ADR 0052 decision 7), pairwise on the
                // child's [[stage]]. The whole of the lend is energy going
                // one way: the buffer is a Refill target of hers while the
                // child is being raised, and the Withdraw the same store
                // would otherwise carry is denied her — left in, her hauler
                // would take the load it just carried across the Seam
                // straight back out again, the ADR 0019 cycle over a
                // border, with the child's own upgraders drinking against
                // her.
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

                // And the denial does **not** ride on the lend. The two
                // sets are not the same set: the ferry lends to a
                // `Bootstrapping` child alone, while `Borrowed.Rooms` also
                // holds the [[nursery]] she is raising and the child she has
                // lost (#221) — so a Withdraw denied only where a Refill is
                // pooled would make those rooms' stores plain Feeding-tier
                // intakes of hers, which is the cross-Seam drain ADR 0047
                // refuses and the exact inverse of the lend. Pairwise on
                // the stage, over the same store the case above pools the
                // Refill for.
                Expect.isFalse
                    (List.contains (Withdraw("can-child", Energy)) (poolFor Nursery))
                    "a nursery's store is not hers to draw either"

                Expect.isFalse
                    (List.contains (Withdraw("can-child", Energy)) grown)
                    "nor a grown child's: no store of a child's is ever her intake"

                // What bounds the lend is the capacity and never the tier
                // (ADR 0052 decision 6): the Refill sits on the buffer's own
                // deep tier like her own, and `Tuning.FerryLoads` is the
                // whole of how many bodies may cross for it — the same
                // number the hauler row was raised by, so the quota and the
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
                // ADR 0023's gate reads "some Refill target **other than
                // the Storage** has free capacity", and a bootstrapping
                // child's buffer is exactly one (#222). Without it counted,
                // the two conditions were close to mutually exclusive: the
                // ferry's Refill sits on the buffer's own deep tier, so a
                // load reaches it only once the spawn, the extensions and
                // the home buffer are full — which is precisely the state
                // that leaves `refills` and `containerRefills` empty — and
                // the body `haulerQuota` hires for the lend, priced on the
                // round trip from this Storage to that buffer, had a sink
                // and no intake at all.
                //
                // This fixture is that state by construction: no
                // Refillables, no home buffer, and a stocked Storage.
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

                // The pairwise control on the one fact that decides it: the
                // child's stage. With no lend there is no sink at all, and
                // the gate shuts exactly as it always did — the stock is
                // not opened against its own Refill (ADR 0023).
                Expect.isFalse
                    (List.contains
                        (Withdraw("storage-1", Energy))
                        (planTasksOn (stocked Independent) noThreats))
                    "and with nothing to feed, the stock stays shut"
            }

            test "a mother with no stock ferries nothing" {
                // The other half of "priced from her Storage": the stock is
                // the only energy a mother holds that her own rows are not
                // already hired against (ADR 0023), so a colony without one
                // has nothing to send whatever stage its child stands at.
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
                // #200, the ADR 0046 correction, read at the fleet. The
                // surplus is 71,500 by construction (`upgraderBandColony`),
                // which the drain alone divides into **four** bodies and
                // the drain plus the body it is spent on into **three**.
                //
                // Read one body at a time, the way the container switch is:
                // the colony standing at the quota casts nothing of this
                // row and one body short casts one, so a quota that had
                // stayed at four would show as a fourth upgrader here.
                //
                // What the fourth cost is the double sale #195 fixed, one
                // order of magnitude smaller: four bodies drink 66,000 and
                // cost 6,800 to replace, 1,300 over an income of 71,500,
                // with the difference coming out of the Storage every tick
                // of both lives.
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
                // #158's second half, at the seam a lead is observable
                // from: an Anchor is expiring when its life is at or under
                // the ticks its replacement needs to stand where it stands
                // (ADR 0026), and an expiring one leaves its row's count so
                // the successor is cast while it still works.
                //
                // Pairwise on the reservation of the room its one Post
                // stands in, which is the only input that moves. Held, the
                // row's ceiling is six Work and its cast is `6W/1C/1M` —
                // eight parts, 24 ticks in the spawner and a slow walk out
                // — and a twenty-tick-old Anchor is inside that lead. Under
                // no reservation the rock gives five, the ceiling is three,
                // the cast is `3W/1C/1M` — five parts and a faster walk —
                // and the same Anchor is not.
                //
                // Before #158 the lead was `bodyFor`'s answer, which is the
                // held ceiling's six Work whatever the room pays: both
                // arms answered "expiring", the successor was cast some
                // nine ticks plus three quarters of a walk early, and ADR
                // 0024's arrival-priced Post capacity counted the incumbent
                // as the holder — the fresh Anchor standing beside the
                // spawn reading its own Post as full, which is the
                // `IdleReason.NoneFree` ADR 0026 names as the symptom.
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
                // **ADR 0053's first half**, and the shape #158 filed:
                // `anchorWorkCapOf` folded every posted source into one
                // colony-wide `List.max`, and the colony's own owned room
                // is in that fold at the held rate — so the ceiling was six
                // Work in every state a colony with one posted home source
                // can reach, and an outpost whose reservation had lapsed
                // went on being garrisoned by `6W/1C/1M` against a rock
                // giving five. Twelve a tick bought for a rock that gives
                // five, for the whole of a 1,500-tick life.
                //
                // What pairs a body to a rock without giving a cast a role
                // (ADR 0021, ADR 0006) is the **vacancy** it is filling:
                // the row is casting into one empty Post, that Post seats
                // one rock, and the rock's rate is a fact of the
                // projection.
                //
                // Pairwise on the outpost's reservation alone — the same
                // two Posts, the same two garrisons, the same 1,800 bank,
                // and the outpost's Anchor the expiring one in both arms.
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
                // Trap (i) of #158, which is why the vacancy has to be
                // judged at **arrival** (ADR 0026) rather than by who is
                // standing where. The colony's own Post is garrisoned by an
                // expiring Anchor — it is still standing on it, and will be
                // dead before a replacement could arrive — so that Post is
                // the vacancy and its own held rock sizes the successor.
                //
                // Read off who is standing instead, the home Post would
                // read as taken, the row would fall through to the
                // outpost's Post as the only free one, and the home room's
                // replacement would be cast at three Work against a rock
                // giving ten: four energy a tick lost for a whole life,
                // which is the error ADR 0042's fold existed to prevent and
                // the reason it could not simply be narrowed.
                //
                // Pairwise against the arm above, on which of the two
                // garrisons is expiring — the outpost stands unreserved in
                // both, so the colony's cheapest rock is the neutral one in
                // both.
                Expect.equal
                    (anchorCastsBy (pairedPostColony false 40 1500))
                    [ sixWork ]
                    "the home room's own Post is the vacancy, and its rock gives ten whatever the outpost pays"

                // And again with the outpost's Post standing genuinely
                // empty, which is the arm that makes the reading a
                // *judgement* rather than a coincidence: with both Posts
                // garrisoned above, a standing read finds no free Post at
                // all and falls back to the richest ceiling, answering six
                // Work for the wrong reason. Here it would find the
                // outpost's hole and only that one, and cast the home
                // room's replacement at three Work against a rock giving
                // ten.
                Expect.equal
                    (anchorCastsBy (pairedPostColony false 40 1500 |> withoutOutpostGarrison))
                    [ sixWork ]
                    "an expiring incumbent's own Post is a vacancy even with a neutral one standing open beside it"
            }

            test
                "a bank short of the dearest vacancy casts no Anchor rather than the cheapest one's body" {
                // The hole a per-vacancy seat list opens in the cascade
                // (ADR 0053's rejected option, ADR 0050's step-over): a
                // spawn takes the first seat its bank can pay for and steps
                // over the ones it cannot, so seats sized richest-first at
                // *different* prices are cheapest-first at every bank
                // between two of them.
                //
                // Both garrisons expiring, so both Posts read vacant — the
                // held home rock at six Work and the neutral outpost's at
                // three — and 600 available against an 1,800 capacity. The
                // dearest vacancy's body costs 700 and the cheapest's 400.
                // Sized one seat per vacancy, this tick buys the 400: a
                // three-Work Anchor born beside the home spawn, a few tiles
                // from the held Post it will be pinned to by travel cost
                // and a Seam from the neutral one it was bought for, digging
                // six a tick where the rock gives ten for the whole of a
                // 1,500-tick life. Sized at the dearest vacancy the row
                // yields the tick instead and buys the six Work the tick
                // the bank holds 700.
                //
                // Pairwise on the bank alone, against the same fixture at
                // its own 1,800.
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
                // #158's second half where ADR 0053 puts it: a [[lead]] is
                // what the successor needs to stand **where this creep
                // stands** (ADR 0026), and where an Anchor stands is its
                // own Post — so the successor is that Post's body and not
                // the row's largest, nor the colony's richest.
                //
                // Pairwise on the outpost's reservation alone, with the
                // same 40 ticks left on the same garrison standing on the
                // same tile a Seam away. Held, its successor is `6W/1C/1M`:
                // eight parts, 24 ticks in the spawner and a slow crossing,
                // a lead of 66 — so at 40 it is expiring and the row casts.
                // Unheld, its successor is `3W/1C/1M`: five parts, 15 ticks
                // and a faster body, a lead of 36 — and at 40 it still
                // counts, so the tick's body goes to the generalist row.
                //
                // The colony's own held Post stands beside it in both arms
                // and moves neither answer, which is the half a lead read
                // off the colony's richest ceiling would get wrong: it
                // would price both arms at 66 and cast a successor 30 ticks
                // early, to stand beside the spawn reading its own Post as
                // full (`IdleReason.NoneFree`, ADR 0026).
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
                // #184. The [[stand-down]] withdraws from a room somebody
                // else has taken (ADR 0043), but the gate reads the
                // *previous* tick's [[raid log]] — so on the tick the
                // colony first sees the room held it is still in the scan
                // set, carries no reservation of ours and reads the whole
                // 5,000-tick deficit. The row cast the bank's largest
                // reserver body at it, 1,300 energy for a creep the engine
                // would refuse at the controller.
                //
                // Pairwise on the room's owner alone — the same
                // declaration, the same rock, the same fleet, the same
                // bank — and closed with the same-tick fact rather than
                // with a gate that arrives a tick late.
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
                // #156. A creep still spawning is in no `Creeps` list — it
                // cannot act, cannot be matched and holds no tile — so
                // every row's living count read straight past it. With one
                // spawn that was harmless, because the spawn casting the
                // body is busy; with two it is not: spawn one casts an
                // Anchor for the empty Post at tick T, and at T+1 the gap
                // is still one, spawn one is still busy, and spawn two
                // casts a second Anchor for the same Post.
                //
                // ADR 0026 rejected counting a gestating body and named the
                // reason that has since expired — "the deficit already
                // stops double-casting through the spawn's own
                // `IsSpawning`" — which was true of one spawn and of no
                // other number of them.
                //
                // Pairwise on the oven alone: the same colony, the same
                // fleet, the same Post, with and without the body already
                // bought for it.
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
                                (colony [ [ Work; Work; Carry; Move ] ])
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
                    (castRows (decideOn (colony [ [ Carry; Carry; Move ] ])).Intents |> List.head)
                    "anchor"
                    "a hauler in the oven pays off no Anchor gap"
            }
        ]
