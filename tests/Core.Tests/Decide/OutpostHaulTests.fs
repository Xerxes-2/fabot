/// The outpost's source container, what it adds to the hauler quota, and
/// the Anchor's lead over it.
module Fabot.Core.Tests.Decide.OutpostHaulTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.OutpostFixtures

[<Tests>]
let outpostContainerTests =
    testList
        "the outpost's source container"
        [
            test "the site lands on the Seat whose walk out to the Seam is shortest" {
                // ADR 0042's own rule, at the seam it is decided on: an
                // outpost has no spawn for a trunk to anchor on, so the
                // pick is anchored on the Seam instead. Measured as a walk
                // and never as a range — the two disagree on this floor by
                // construction, and the Seat the range would pick is the
                // one three swamp tiles from the border.
                let colony =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround "W1N2" detourGround [ "src-out", outpostSource, Source ]

                Expect.equal
                    (Atlas.seatTilesOf (Atlas.ofView colony) "src-out" |> RoomPos.inRoom "W1N2")
                    (Set.ofList [ { X = 10; Y = 45 }; { X = 11; Y = 43 } ])
                    "the premise: the rock has two Seats, and the nearer one to the border is (10,45)"

                Expect.equal
                    (containerSites colony)
                    [ "W1N2", { X = 11; Y = 43 } ]
                    "the farther Seat wins, because the ground between it and the Seam is cheaper"
            }

            test "the Intent carries the outpost's own room, never the colony's" {
                // The trap the Layout would have walked into: a placement
                // Intent has always carried a room name, and `planLayout`
                // stamps the one room it plans onto every site it emits, so
                // an outpost pick routed through that path would drop a
                // 5,000-energy container on the *home* room's tile of the
                // same coordinates. (11,43) is a real coordinate in both
                // rooms and this asserts which one is named.
                let colony =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround "W1N2" detourGround [ "src-out", outpostSource, Source ]

                // Asserted as the whole list and never with `Expect.all`,
                // which is vacuously true of the empty one: a rule that
                // planned nothing would pass the room-stamping case it
                // exists to pin.
                Expect.equal
                    (containerSites colony)
                    [ "W1N2", { X = 11; Y = 43 } ]
                    "the one site this rule places names the room its source stands in"
            }

            test "a room the colony cannot see this tick is planned nothing" {
                // ADR 0004 entry by entry, the same reading `sourceOutputOf`
                // gives the same rock: with no vision the container census
                // of that room is empty because nobody looked, not because
                // nothing stands there, and an absence is not an answer.
                // The Intent would also be one the Executor can only report
                // as `ActorMissing` — `Game.rooms` holds the seen rooms
                // alone — so a rule that fired here would file an upstream
                // bug against itself once a tick per rock, for ever.
                let seen =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround "W1N2" detourGround [ "src-out", outpostSource, Source ]

                Expect.isNonEmpty
                    (containerSites seen)
                    "the premise: seen, this rock is planned a container"

                Expect.isEmpty
                    (containerSites
                        { seen with
                            RoomControl = Map.remove "W1N2" seen.RoomControl
                        })
                    "and the same tick with the room unseen plans nothing at all"
            }

            test "a source with one Seat is the same rule with one candidate" {
                // W13S28's `16,7` is a single-Seat rock, and "the shortest"
                // has to answer where there is nothing to be shorter than.
                let ground =
                    [
                        { X = 10; Y = 45 }, Swamp
                        for y in 46..48 do
                            { X = 10; Y = y }, Plain
                    ]

                let colony =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround "W1N2" ground [ "src-out", outpostSource, Source ]

                Expect.equal
                    (containerSites colony)
                    [ "W1N2", { X = 10; Y = 45 } ]
                    "the one Seat there is, priced and picked like any other"
            }

            test "Seats that price alike fall to the lowest (X, Y), as every tie here does" {
                // W12S27's `16,45` has three Seats and all three are swamp,
                // so they can price identically — and a plan that answered
                // a different one of them on different ticks would not be
                // one (ADR 0011's determinism). Three swamp Seats over one
                // plain apron, so the three walks are equal by construction
                // and only the tie-break separates them.
                //
                // It also pins the subtraction the walk is measured with:
                // the Seat's own swamp step is charged to whatever walks
                // *in* to it, so three swamp Seats over identical ground
                // tie rather than each carrying five ticks of their own.
                let ground =
                    [
                        { X = 9; Y = 45 }, Swamp
                        { X = 10; Y = 45 }, Swamp
                        { X = 11; Y = 45 }, Swamp
                        for x in 8..12 do
                            for y in 46..48 do
                                { X = x; Y = y }, Plain
                    ]

                let colony =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround "W1N2" ground [ "src-out", outpostSource, Source ]

                Expect.equal
                    (containerSites colony)
                    [ "W1N2", { X = 9; Y = 45 } ]
                    "the three Seats tie, and the lowest X answers"
            }

            test "a Seat's own terrain is not charged to it: the walk is the ground beyond it" {
                // The convention every walk in this colony is measured by
                // (ADR 0029): a walk charges the tiles a creep steps onto
                // and never the tile it already stands on. Here it decides
                // the pick. Two Seats over one symmetric plain apron, so
                // the ground beyond them is identical and only their own
                // terrain differs — the swamp one first in (X, Y) order. A
                // rule that charged a Seat for standing on it would price
                // the swamp Seat five ticks dearer and pick the plain one;
                // this rule ties them and lets the tie-break answer, which
                // is right because whoever hauls from that container starts
                // on it and never pays to arrive.
                let ground =
                    [
                        { X = 9; Y = 45 }, Swamp
                        { X = 11; Y = 45 }, Plain
                        for x in 8..12 do
                            for y in 46..48 do
                                { X = x; Y = y }, Plain
                    ]

                let colony =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround "W1N2" ground [ "src-out", outpostSource, Source ]

                Expect.equal
                    (Atlas.seatTilesOf (Atlas.ofView colony) "src-out" |> RoomPos.inRoom "W1N2")
                    (Set.ofList [ { X = 9; Y = 45 }; { X = 11; Y = 45 } ])
                    "the premise: two Seats, one swamp and one plain, over the same apron"

                Expect.equal
                    (containerSites colony)
                    [ "W1N2", { X = 9; Y = 45 } ]
                    "the swamp Seat is no dearer than the plain one, so the tie-break answers"
            }

            test "a container already serving the source is planned for no second one" {
                // ADR 0040 holds here as it does at home, and by target
                // rather than by tile: the thing serving the rock is on
                // (10,45), which is not the tile the plan picked, and the
                // rock is served all the same. Standing and pending both,
                // because the plan asks whether another must be built and a
                // site going up answers that.
                let served kind =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround
                        "W1N2"
                        detourGround
                        [ "src-out", outpostSource, Source; "con-out", { X = 10; Y = 45 }, kind ]

                Expect.isEmpty
                    (containerSites (served (Structure BuiltKind.Container)))
                    "a container standing within range 1 of the rock, on a Seat the plan did not pick"

                Expect.isEmpty
                    (containerSites (served (Site BuiltKind.Container)))
                    "and a site pending there, which is a container already being built"
            }

            test "a Seat another kind's site already holds is no candidate at all" {
                // #244, live in W13S29: the human paved the outpost by hand
                // and his road sites landed on the two Seats this rule had
                // picked, (28,6) and (15,28). The engine takes one
                // construction site per tile, so the Executor asked for the
                // container on a taken tile and was answered
                // ERR_INVALID_TARGET once a tick, for ever — and with no
                // container the rock is no Post, hires no Anchor and enters
                // no income quota (ADR 0042), behind a road two workers
                // finish at the surplus tier. So the pick moves to the next
                // cheapest Seat rather than waiting on a site nobody
                // promised to build.
                let siteOn tile =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround
                        "W1N2"
                        detourGround
                        [ "src-out", outpostSource, Source; "road-out", tile, Site BuiltKind.Road ]

                Expect.equal
                    (containerSites (siteOn { X = 11; Y = 43 }))
                    [ "W1N2", { X = 10; Y = 45 } ]
                    "the Seat the walk picked is taken, so the dearer Seat takes the container"

                Expect.equal
                    (containerSites (siteOn { X = 10; Y = 45 }))
                    [ "W1N2", { X = 11; Y = 43 } ]
                    "and a site on the Seat that lost moves nothing: one tile is subtracted, not a source"
            }

            test "a road that already stands is no obstruction, and is the best tile there is" {
                // The half the clause must not subtract. One construction
                // site per tile is the whole of the engine's rule: a
                // *finished* structure holds no site, and a container on a
                // paved Seat is the tile this rule would have chosen anyway,
                // since the hauler that draws it arrives over the road. A
                // built road prices the tile too, so it is laid in both
                // pieces the shell lays one in (`paved`).
                let colony =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround
                        "W1N2"
                        detourGround
                        [
                            "src-out", outpostSource, Source
                            "road-out", { X = 11; Y = 43 }, Structure BuiltKind.Road
                        ]
                    |> paved "W1N2" [ { X = 11; Y = 43 } ]

                Expect.equal
                    (containerSites colony)
                    [ "W1N2", { X = 11; Y = 43 } ]
                    "the pick is unmoved by a road that has finished going up on it"
            }

            test "a source whose every Seat is taken plans nothing and waits" {
                // Waiting is the answer and it is not a self-clearing one.
                // The colony has no vocabulary for cancelling a human's
                // site and asking the engine for a refusal once a tick is
                // not a plan — but nothing here promises the Seat comes
                // back either: a road site in an outpost is a plain
                // Surplus Build with no home rung and outside the
                // builders' budget, which is what "an ordinary outpost
                // site keeps its travel cost" above pins, so the human's
                // site is the only thing that ends this. Pinned as it
                // really is: no Intent, this tick or any other.
                let bothTaken =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround
                        "W1N2"
                        detourGround
                        [
                            "src-out", outpostSource, Source
                            "road-a", { X = 11; Y = 43 }, Site BuiltKind.Road
                            "road-b", { X = 10; Y = 45 }, Site BuiltKind.Road
                        ]

                Expect.isEmpty
                    (containerSites bothTaken)
                    "both Seats hold a site, so this rock is planned no container this tick"
            }

            test "a home container on the pick's coordinates defers nothing" {
                // The room-blind census this rule would have inherited: a
                // `Pos` carries no room (ADR 0041), so a census unioning
                // both rooms' container tiles would read the home room's
                // container as serving an outpost rock fifty tiles away —
                // and would then defer the outpost's container forever,
                // leaving the room with no switch to close (ADR 0042).
                let colony =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround "W1N2" detourGround [ "src-out", outpostSource, Source ]
                    |> withTarget "con-home" { X = 11; Y = 43 } (Structure BuiltKind.Container)

                Expect.equal
                    (containerSites colony)
                    [ "W1N2", { X = 11; Y = 43 } ]
                    "the outpost's rock is unserved: what stands on those coordinates stands at home"
            }

            test "the home room's Layout is not moved by an outpost joining the projection" {
                // ADR 0042: "The outpost gets a container and nothing
                // else. No roads, and no Layout." This rule runs beside the
                // Layout and never inside it, so a colony that gains an
                // outpost plans the same home room it planned without one —
                // the same clustered picks, the same trunks, the same
                // containers, the same footings — and gains exactly one
                // site, in the other room.
                let alone = trunkColony 4

                let withOutpost =
                    alone
                    |> withOutpostGround "W1N2" detourGround [ "src-out", outpostSource, Source ]

                let atHome colony =
                    let { Intents = intents } = decideOn colony

                    placementIntents intents |> List.filter (fun (room, _, _) -> room = "W1N1")

                Expect.isNonEmpty (atHome alone) "the premise: this room has a Layout to move"

                Expect.equal
                    (atHome withOutpost)
                    (atHome alone)
                    "every home site the Layout placed, unmoved and in its own order"

                Expect.equal
                    (containerSites withOutpost |> List.filter (fun (room, _) -> room <> "W1N1"))
                    [ "W1N2", { X = 11; Y = 43 } ]
                    "and the one site the outpost gained is the container, in the outpost"
            }

            test "a room home shares no border with is planned nothing" {
                // Total (ADR 0004): the Seam is read out of the two room
                // names, and two rooms four sectors apart have no band —
                // so the walk that anchors the pick has no anchor, and an
                // unpriceable rule plans nothing rather than planning
                // arbitrarily. W5N5 is not W1N1's neighbour.
                let colony =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround "W5N5" detourGround [ "src-out", outpostSource, Source ]

                Expect.isEmpty
                    (containerSites colony)
                    "no band to price a Seat against, so no Seat is picked"
            }
        ]

[<Tests>]
let outpostHaulTests =
    testList
        "the outpost's container in the hauler quota"
        [
            test "an outpost container hires haul capacity, priced at its own room's rate" {
                // ADR 0042's hauler half, which #127 could not reach: the
                // quota folds every projected room's containers, and the
                // round trip it prices this one at is the Seam join
                // (`Atlas.haulRoundTripTicks`), 51 ticks over this
                // corridor. At the 600 bank the hauler row carries 400, so
                // held the rock ships ten a tick and hires ceil(51 x 10 /
                // 400) = 2, and unheld it ships five and hires 1.
                //
                // Pairwise, one rival at a time: the two colonies differ in
                // who holds W1N2 and in nothing else.
                let held control =
                    quotaOf (haulHome |> withHaulOutpost (Some control))

                Expect.equal
                    (quotaOf haulHome)
                    0
                    "the premise: without the outpost there is no haul"

                // The rate is read off the demand and not off the quota: since
                // #279 a haul that crosses a Seam is floored at two bodies, so
                // both rocks hire two and the quota can no longer see which of
                // them ships ten a tick. What the rate moves is the sum the
                // quota divides, and that is what this case is about.
                let shipped control =
                    haulDemandOf (haulHome |> withHaulOutpost (Some control))

                Expect.equal
                    (shipped (reservedRoom true 4000))
                    510
                    "reserved, the rock ships ten a tick"

                Expect.equal
                    (shipped neutralRoom)
                    255
                    "held by nobody it ships five, and the sum the quota divides is halved with it"

                Expect.equal
                    (held (reservedRoom true 4000))
                    2
                    "and either way the crossing is never one body's to lose (#279)"

                Expect.equal
                    (held neutralRoom)
                    2
                    "including the neutral rock, whose overflow decays the same"

                // #279's floor, and the premise it rests on: a crossing worth
                // half a load or more is never one body's to lose. What a full
                // container at home does is wait; what a full one out here does
                // is drop the Anchor's next fifty on the floor to decay, forty
                // tiles from the replacement. Live it was 1,170 energy-ticks
                // against a 1,200 load — one body at 97.5% of itself.
                Expect.isTrue
                    (shipped (reservedRoom true 4000) * 2 >= 400)
                    "the premise: the crossing is worth half a 400 load or more"

                Expect.equal
                    (held ownedRoom)
                    (held (reservedRoom true 4000))
                    "owned or reserved by us is one rate, as the engine pays it"

                Expect.equal
                    (quotaOf (haulHome |> withHaulOutpost None))
                    0
                    "and a room the colony cannot see prices no rock at all (ADR 0004)"
            }

            test "two containers across the same Seam are one sum, rounded once" {
                // Acceptance criterion 1's own geometry (#194): the
                // arithmetic case `haulRoundingTests` carries has no Seam
                // in it, so until here nothing pooled *two* cross-room
                // terms and a join that mispriced the second one would
                // have moved no number in the suite.
                //
                // Both rocks are held, so each ships ten a tick, and the
                // 600 bank's hauler carries 400 — a body per 40 ticks of
                // round trip. The two crossings below come to 51 and 63,
                // so the colony's haul is 2.85 bodies and hires three; a
                // ceiling apiece bought 1.275 → 2 and 1.575 → 2 and
                // hired four.
                let colony =
                    haulHome
                    |> withHaulOutpost (Some(reservedRoom true 4000))
                    |> withSecondHaulContainer { X = 33; Y = 44 }

                let atlas = Atlas.ofView colony

                // The body the quota itself divides by — this fixture's
                // 600 bank, not `haulRoundingBody`'s 300 (#208). Road
                // parity holds at every size so the ticks are the same
                // either way, and asserting them off the row's own cast is
                // what keeps the premise and the quota one arithmetic.
                let roundTrip from =
                    Atlas.haulRoundTripTicks
                        atlas
                        (bodyFor haulerPattern 600)
                        (RoomPos.at "W1N2" from)
                        (RoomPos.at "W1N1" { X = 25; Y = 10 })

                Expect.equal
                    (roundTrip { X = 25; Y = 41 })
                    (Some 51)
                    "the premise: the near container's Seam crossing"

                Expect.equal
                    (roundTrip { X = 33; Y = 44 })
                    (Some 63)
                    "the premise: and the branch container's, eight steps out along the branch"

                Expect.equal (quotaOf colony) 3 "the two crossings are summed and rounded once"
            }

            test "one crossing container's longer haul moves the pair by a body" {
                // The pairwise half of the criterion, on the shape it
                // names: the branch container's round trip goes from 63 to
                // 72 ticks, which is 1.575 of a body to 1.8 — its own
                // ceiling is two either way, so under the old rule the
                // move was invisible. Pooled, the colony goes from 2.85 to
                // 3.075 and hires the body, because the fraction the haul
                // grew by is now spent rather than already bought.
                let atTile tile =
                    haulHome
                    |> withHaulOutpost (Some(reservedRoom true 4000))
                    |> withSecondHaulContainer tile

                Expect.equal
                    (quotaOf (atTile { X = 33; Y = 44 }))
                    3
                    "the premise: eight steps down the branch hires three"

                Expect.equal
                    (quotaOf (atTile { X = 36; Y = 44 }))
                    4
                    "three steps further out is one body more"
            }

            test "a container in a room the projection does not carry hires nobody" {
                // ADR 0004 at the fold's own edge: the container is in the
                // kind census, the colony holds the room, and the
                // projection places neither the container nor its rock —
                // so there is no tile to flood from and no room to flood
                // over. Nothing, and never the home room's arithmetic run
                // over an outpost's coordinates.
                let seen = haulHome |> withHaulOutpost (Some(reservedRoom true 4000))

                let unprojected =
                    { seen with
                        Spatial =
                            { seen.Spatial with
                                Rooms = Map.remove "W1N2" seen.Spatial.Rooms
                            }
                    }

                Expect.equal (quotaOf seen) 2 "the premise: projected, the container hires two"
                Expect.equal (quotaOf unprojected) 0 "unprojected, the same census hires none"
            }

            test "a quota memoised while the outpost was held is not handed back when it lapses" {
                // #127's memo case, in the room it was written for. The
                // hauler quota rides the census memo (ADR 0017) and now
                // reads a *second* room's held rate, so the census
                // signature had to widen to sign every projected room's —
                // and this is what the widening buys. Every census input
                // below is byte-identical between the two views: the
                // reservation is the only thing that moved.
                let lapsed = haulHome |> withHaulOutpost (Some neutralRoom)

                let previous =
                    (decide
                        (haulHome |> withHaulOutpost (Some(reservedRoom true 4000)))
                        Map.empty
                        Set.empty
                        None)
                        .Memo

                // Read off the demand and not the quota: since #279 both rates
                // hire two bodies across a Seam, so the quota can no longer
                // tell a recomputed answer from a handed-back one. The sum the
                // quota divides still halves with the rate, and that is what
                // the memo either recomputes or wrongly keeps.
                let shipped (memo: PlanMemo) =
                    memo.HaulerDemand |> List.sumBy (fun row -> row.Demand)

                Expect.equal
                    (shipped previous)
                    510
                    "the premise: held, the container ships ten a tick"

                let recalled = decide lapsed Map.empty Set.empty (Some previous)
                let fresh = decideOn lapsed

                Expect.equal (shipped fresh.Memo) 255 "the premise: lapsed, it ships five"

                Expect.equal
                    (shipped recalled.Memo)
                    (shipped fresh.Memo)
                    "the stale memo recomputes rather than handing back the held rate's haul"

                Expect.equal
                    (spawnIntents recalled.Intents)
                    (spawnIntents fresh.Intents)
                    "so the fleet the colony casts is the one the halved haul asked for"
            }

            test
                "the quota memoised before an outpost container stood is not handed back once it does" {
                // The other half of the widening, and the one the rate
                // above cannot reach: the census entry itself. The
                // reservation case moves `held`, which is signed per room;
                // this one moves nothing but whether `can-out` stands in
                // W1N2, which only the *standing* census spanning every
                // projected room can see. Joined against the home layer
                // alone the two views sign one string, so the colony
                // would recall the container-less nothing for ever and ADR
                // 0042's switch would never fire.
                let standing = haulHome |> withHaulOutpost (Some(reservedRoom true 4000))
                let before = standing |> beforeHaulContainer

                let previous = (decideOn before).Memo

                Expect.equal
                    previous.HaulerQuota
                    0
                    "the premise: with no container on the Seat there is no haul to hire for"

                let recalled = decide standing Map.empty Set.empty (Some previous)
                let fresh = decideOn standing

                Expect.equal fresh.Memo.HaulerQuota 2 "the premise: standing, it hires two"

                Expect.equal
                    recalled.Memo.HaulerQuota
                    fresh.Memo.HaulerQuota
                    "the container standing up moves the signature, so the quota is recomputed"

                Expect.equal
                    (spawnIntents recalled.Intents)
                    (spawnIntents fresh.Intents)
                    "and the fleet the colony casts is the one the new haul asked for"
            }
        ]

[<Tests>]
let outpostSuccessionTests =
    testList
        "an outpost's Anchor and its lead"
        [
            test "an Anchor a room away is expiring, and its replacement is cast before it dies" {
                // The reproduction #153 opens on. Until it, a lead was
                // priced off the home room's flood alone, so a creep the
                // home room did not place answered 0 and was never expiring
                // — an outpost's garrison held its Post to the last tick,
                // its successor was cast only once it was dead, and the Post
                // stood empty for the cast plus the crossing in every
                // 1,500-tick life while the workforce target went on hiring
                // against the source's nominal output (ADR 0042).
                //
                // Priced over the border the lead is countable a tile at a
                // time. The Anchor row at this 300 bank is two Work over a
                // Carry and a Move (`anchorBodyFor`), so twelve ticks in the
                // spawner and four cost units — two ticks — a plain step.
                // The replacement is born on (25,9), walks eight tiles up to
                // (25,1), steps onto the exit at (25,0), is moved to (25,49)
                // for nothing, steps off onto (25,48) and walks seven more
                // down to the container at (25,41): sixteen tiles of ground
                // at two ticks each, plus the plain exit's own two — 34 of
                // walking and a lead of 46.
                Expect.equal
                    (castNames (outpostSuccession 1500))
                    (castNames (outpostSuccession 47))
                    "one tick outside its lead the garrison still counts, exactly as a fresh one does"

                match castNames (outpostSuccession 47) with
                | [ name ] ->
                    Expect.stringStarts name "hauler-" "the premise: the Anchor row is filled"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"

                match castNames (outpostSuccession 46) with
                | [ name ] ->
                    Expect.stringStarts
                        name
                        "anchor-"
                        "at its lead the outpost's row is short and the successor is cast"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"

                match castNames (outpostSuccession 1) with
                | [ name ] ->
                    Expect.stringStarts
                        name
                        "anchor-"
                        "and a tick from death it is being replaced, not mourned"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "an outpost no crossing reaches leads nobody, however little life is left" {
                // Total (ADR 0004) at the seam the border is joined on: with
                // no ring in the projection the two rooms share no Seam
                // band, so there is no walk to price and no lead — and a
                // lead of 0 leaves the garrison counted living to its last
                // tick, which is the answer unpriceable geometry has always
                // had. Never an arbitrary number, and never the home room's
                // arithmetic run over an outpost's coordinates.
                let unbordered life =
                    let colony = outpostSuccession life

                    { colony with
                        Spatial =
                            { colony.Spatial with
                                Borders = Map.empty
                            }
                    }

                Expect.equal
                    (castNames (unbordered 1))
                    (castNames (unbordered 1500))
                    "the same colony casts the same body whether the garrison is dying or fresh"

                // A worker and not a hauler, because the same missing band
                // leaves the container's round trip unpriceable and its
                // haul unhired (ADR 0004, `outpostHaulTests`). What this
                // case reads is the row it is *not*: the Anchor row is
                // filled, so the garrison is still counted living.
                match castNames (unbordered 1) with
                | [ name ] ->
                    Expect.stringStarts name "worker-" "and the Anchor row reads as filled"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }
        ]
